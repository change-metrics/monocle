# MIT License
# Copyright (c) 2019 Fabien Boucher

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


import logging
import requests
from datetime import datetime
from time import sleep


name = 'github_crawler'
help = 'Github Crawler to fetch PRs events'


def init_crawler_args_parser(parser):
    parser.add_argument(
        '--token', help='A Github API token',
        required=True)
    parser.add_argument(
        '--org', help='The Github organization to fetch PR events',
        required=True)
    parser.add_argument(
        '--updated-since', help='Acts on PRs updated since')
    parser.add_argument(
        '--id', help='Get one PR (for debug purpose)')
    parser.add_argument(
        '--repository',
        help='Only used with --id (for debug purpose)')


class PRsFetcher(object):

    log = logging.getLogger("monocle.PRsFetcher")

    def __init__(self, gql, host, org, bulk_size=25):
        self.gql = gql
        self.size = bulk_size
        self.host = host
        self.org = org
        self.events_map = {
            'ClosedEvent': 'ChangeClosedEvent',
            'PullRequestReview': 'ChangeReviewedEvent',
        }
        self.pr_query = '''
          id
          updatedAt
          createdAt
          mergedAt
          closedAt
          title
          state
          number
          mergeable
          labels (first: 100){
            edges {
              node {
                name
              }
            }
          }
          assignees (first: 100){
            edges {
              node {
                login
              }
            }
          }
          comments (first: 100){
            edges {
              node {
                id
                createdAt
                author {
                  login
                }
              }
            }
          }
          timelineItems (first: 100 itemTypes: [CLOSED_EVENT, PULL_REQUEST_REVIEW]) {
            edges {
              node {
                __typename
                ... on ClosedEvent {
                  id
                  createdAt
                  actor {
                    login
                  }
                }
                ... on PullRequestReview {
                  id
                  createdAt
                  state
                  author {
                    login
                  }
                }
              }
            }
          }
          author {
            login
          }
          mergedBy {
            login
          }
          repository {
            owner {
              login
            }
            name
          }
        '''

    def _getPage(self, kwargs, prs):
        # Note: usage of the default sort on created field because
        # sort on the updated field does not return well ordered PRs
        qdata = '''{
          search(query: "org:%(org)s is:pr sort:created updated:>%(updated_since)s created:<%(created_before)s" type: ISSUE first: %(size)s %(after)s) {
            issueCount
            pageInfo {
              hasNextPage endCursor
            }
            edges {
              node {
                ... on PullRequest {
                    %(pr_query)s
                }
              }
            }
          }
        }'''
        data = self.gql.query(qdata % kwargs)
        if not kwargs['total_prs_count']:
            kwargs['total_prs_count'] = data['data']['search']['issueCount']
            self.log.info("Total PRs to fetch: %s" % kwargs['total_prs_count'])
        for pr in data['data']['search']['edges']:
            prs.append(pr['node'])
        pageInfo = data['data']['search']['pageInfo']
        if pageInfo['hasNextPage']:
            kwargs['after'] = 'after: "%s"' % pageInfo['endCursor']
            return True
        else:
            return False

    def get(self, updated_since):
        prs = []
        kwargs = {
            'pr_query': self.pr_query,
            'org': self.org,
            'updated_since': updated_since,
            'after': '',
            'created_before': datetime.now().strftime("%Y-%m-%dT%H:%M:%SZ"),
            'total_prs_count': 0,
            'size': self.size
        }

        while True:
            self.log.info('Running request %s' % dict(
                [(k, v) for k, v in kwargs.items() if k != 'pr_query']))
            try:
                hnp = self._getPage(kwargs, prs)
            except requests.exceptions.ConnectionError:
                self.log.exception(
                    "Error connecting to the Github API - retrying in 5s ...")
                sleep(5)
                continue
            self.log.info("%s PRs fetched" % len(prs))
            if not hnp:
                if (len(prs) < kwargs['total_prs_count'] and
                        len(prs) % self.size == 0):
                    kwargs['created_before'] = prs[-1]['createdAt']
                    kwargs['after'] = ''
                    continue
                break
        return prs

    def get_one(self, org, repository, number):
        qdata = '''{
          repository(owner: "%(org)s", name:"%(repository)s") {
            pullRequest(number: %(number)s) {
              %(pr_query)s
            }
          }
        }
        '''
        kwargs = {
            'pr_query': self.pr_query,
            'org': org,
            'repository': repository,
            'number': number
        }
        return self.gql.query(qdata % kwargs)

    def extract_objects(self, prs):
        def timedelta(start, end):
            format = "%Y-%m-%dT%H:%M:%SZ"
            start = datetime.strptime(start, format)
            end = datetime.strptime(end, format)
            return int((start - end).total_seconds())

        def extract_pr_objects(pr):
            objects = []
            change = {}
            change['type'] = 'Change'
            change['id'] = pr['id']
            change['number'] = pr['number']
            change['repository_prefix'] = pr['repository']['owner']['login']
            change['repository_fullname'] = "%s/%s" % (
                pr['repository']['owner']['login'], pr['repository']['name'])
            change['repository_shortname'] = pr['repository']['name']
            change['author'] = pr['author']['login']
            change['title'] = pr['title']
            if pr['mergedBy']:
                change['merged_by'] = pr['mergedBy']['login']
            else:
                change['merged_by'] = None
            change['updated_at'] = pr['updatedAt']
            change['created_at'] = pr['createdAt']
            change['merged_at'] = pr['mergedAt']
            change['closed_at'] = pr['closedAt']
            change['state'] = pr['state']
            if pr['state'] in ('CLOSED', 'MERGED'):
                change['duration'] = timedelta(
                    change['closed_at'], change['created_at'])
            change['mergeable'] = pr['mergeable']
            change['labels'] = tuple(map(
                lambda n: n['node']['name'], pr['labels']['edges']))
            change['assignees'] = tuple(map(
                lambda n: n['node']['login'], pr['assignees']['edges']))
            objects.append(change)
            objects.append({
                'type': 'ChangeCreatedEvent',
                'id': 'CCE' + pr['id'],
                'created_at': pr['createdAt'],
                'author': pr['author']['login'],
                'repository_prefix': pr['repository']['owner']['login'],
                'repository_fullname': "%s/%s" % (
                    pr['repository']['owner']['login'],
                    pr['repository']['name']),
                'repository_shortname': pr['repository']['name'],
                'number': pr['number'],
            })
            for comment in pr['comments']['edges']:
                _comment = comment['node']
                objects.append(
                    {
                        'type': 'ChangeCommentedEvent',
                        'id': _comment['id'],
                        'created_at': _comment['createdAt'],
                        'author': _comment['author']['login'],
                        'repository_prefix': pr['repository']['owner'][
                            'login'],
                        'repository_fullname': "%s/%s" % (
                            pr['repository']['owner']['login'],
                            pr['repository']['name']),
                        'repository_shortname': pr['repository']['name'],
                        'number': pr['number'],
                        'on_author': pr['author']['login'],
                    }
                )
            for timelineitem in pr['timelineItems']['edges']:
                _timelineitem = timelineitem['node']
                obj = {
                    'type': self.events_map[_timelineitem['__typename']],
                    'id': _timelineitem['id'],
                    'created_at': _timelineitem['createdAt'],
                    'author': (
                        _timelineitem.get('actor', {}).get('login') or
                        _timelineitem.get('author', {}).get('login')
                    ),
                    'repository_prefix': pr['repository']['owner']['login'],
                    'repository_fullname': "%s/%s" % (
                        pr['repository']['owner']['login'],
                        pr['repository']['name']),
                    'repository_shortname': pr['repository']['name'],
                    'number': pr['number'],
                    'on_author': pr['author']['login'],
                }
                if 'state' in _timelineitem:
                    obj['approval'] = _timelineitem['state']
                objects.append(obj)
            return objects

        objects = []
        for pr in prs:
            try:
                objects.extend(extract_pr_objects(pr))
            except Exception:
                self.log.exception("Unable to extract PR data: %s" % pr)
        return objects
