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
from dataclasses import dataclass


name = 'github_crawler'
help = 'Github Crawler to fetch PRs events'


@dataclass
class GithubCrawlerArgs(object):
    updated_since: str
    loop_delay: int
    command: str
    index: str
    org: str
    repository: str
    base_url: str
    token: str


class PRsFetcher(object):

    log = logging.getLogger(__name__)

    def __init__(self, gql, base_url, org, repository, bulk_size=25):
        self.gql = gql
        self.size = bulk_size
        self.base_url = base_url
        self.org = org
        self.repository = repository
        self.events_map = {
            'ClosedEvent': 'ChangeAbandonedEvent',
            'PullRequestReview': 'ChangeReviewedEvent',
            'HeadRefForcePushedEvent': 'ChangeCommitForcePushedEvent',
        }
        self.pr_query = '''
          id
          updatedAt
          createdAt
          mergedAt
          closedAt
          additions
          deletions
          changedFiles
          title
          headRefName
          baseRefName
          bodyText
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
          commits (first: 100){
            edges {
              node {
                commit {
                  oid
                  pushedDate
                  author {
                    user {
                      login
                    }
                  }
                }
              }
            }
          }
          files (first: 100){
            edges {
              node {
                additions
                deletions
                path
              }
            }
          }
          timelineItems (first: 100 itemTypes: [
                CLOSED_EVENT,
                PULL_REQUEST_REVIEW,
                HEAD_REF_FORCE_PUSHED_EVENT]) {
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
                ... on HeadRefForcePushedEvent {
                  id
                  createdAt
                  actor {
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
        '''  # noqa: E501

    def _getPage(self, kwargs, prs):
        # Note: usage of the default sort on created field because
        # sort on the updated field does not return well ordered PRs
        scope = "org:%(org)s" % kwargs
        if kwargs.get('repository'):
            scope = "repo:%(org)s/%(repository)s" % kwargs
        kwargs['scope'] = scope
        qdata = '''{
          search(
              query: "%(scope)s is:pr archived:false sort:created updated:>%(updated_since)s created:<%(created_before)s"
              type: ISSUE first: %(size)s %(after)s) {
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
        }'''  # noqa: E501
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
            'repository': self.repository,
            'updated_since': updated_since,
            'after': '',
            'created_before': datetime.now().strftime("%Y-%m-%dT%H:%M:%SZ"),
            'total_prs_count': 0,
            'size': self.size,
        }

        while True:
            self.log.info(
                'Running request %s'
                % dict([(k, v) for k, v in kwargs.items() if k != 'pr_query'])
            )
            try:
                hnp = self._getPage(kwargs, prs)
            except requests.exceptions.ConnectionError:
                self.log.exception(
                    "Error connecting to the Github API - retrying in 5s ..."
                )
                sleep(5)
                continue
            self.log.info("%s PRs fetched" % len(prs))
            if not hnp:
                if len(prs) < kwargs['total_prs_count'] and len(prs) % self.size == 0:
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
            'number': number,
        }
        raw = self.gql.query(qdata % kwargs)['data']['repository']['pullRequest']
        return (raw, self.extract_objects([raw]))

    def extract_objects(self, prs):
        def timedelta(start, end):
            format = "%Y-%m-%dT%H:%M:%SZ"
            start = datetime.strptime(start, format)
            end = datetime.strptime(end, format)
            return int((start - end).total_seconds())

        def insert_change_attributes(obj, change):
            obj.update(
                {
                    'repository_prefix': change['repository_prefix'],
                    'repository_fullname': change['repository_fullname'],
                    'repository_shortname': change['repository_shortname'],
                    'number': change['number'],
                    'change_id': change['change_id'],
                    'url': change['url'],
                    'on_author': change['author'],
                    'on_created_at': change['created_at'],
                }
            )

        def extract_pr_objects(pr):
            objects = []
            change = {}
            change['type'] = 'Change'
            change['id'] = pr['id']
            change['number'] = pr['number']
            change['repository_prefix'] = pr['repository']['owner']['login']
            change['repository_fullname'] = "%s/%s" % (
                pr['repository']['owner']['login'],
                pr['repository']['name'],
            )
            change['repository_shortname'] = pr['repository']['name']
            change['change_id'] = "%s@%s" % (
                change['repository_fullname'].replace('/', '@'),
                change['number'],
            )
            change['url'] = '%s/%s/pull/%s' % (
                self.base_url,
                change['repository_fullname'],
                change['number'],
            )
            change['author'] = pr['author']['login']
            change['branch'] = pr['headRefName']
            change['target_branch'] = pr['baseRefName']
            change['title'] = pr['title']
            change['text'] = pr['bodyText']
            change['additions'] = pr['additions']
            change['deletions'] = pr['deletions']
            change['changed_files_count'] = pr['changedFiles']
            change["changed_files"] = [
                {
                    "additions": fd['node']["additions"],
                    "deletions": fd['node']["deletions"],
                    "path": fd['node']["path"],
                }
                for fd in pr["files"]["edges"]
            ]
            change['commit_count'] = len(pr['commits']['edges'])
            if pr['mergedBy']:
                change['merged_by'] = pr['mergedBy']['login']
            else:
                change['merged_by'] = None
            change['updated_at'] = pr['updatedAt']
            change['created_at'] = pr['createdAt']
            change['merged_at'] = pr['mergedAt']
            change['closed_at'] = pr['closedAt']
            # A closed PR is an unmerged closed PR
            change['state'] = pr['state']
            if pr['state'] in ('CLOSED', 'MERGED'):
                change['duration'] = timedelta(
                    change['closed_at'], change['created_at']
                )
            change['mergeable'] = pr['mergeable']
            change['labels'] = list(
                map(lambda n: n['node']['name'], pr['labels']['edges'])
            )
            change['assignees'] = list(
                map(lambda n: n['node']['login'], pr['assignees']['edges'])
            )
            objects.append(change)
            obj = {
                'type': 'ChangeCreatedEvent',
                'id': 'CCE' + change['id'],
                'created_at': change['created_at'],
                'author': change['author'],
            }
            insert_change_attributes(obj, change)
            objects.append(obj)
            for comment in pr['comments']['edges']:
                _comment = comment['node']
                obj = {
                    'type': 'ChangeCommentedEvent',
                    'id': _comment['id'],
                    'created_at': _comment['createdAt'],
                    'author': _comment['author']['login'],
                }
                insert_change_attributes(obj, change)
                objects.append(obj)
            for timelineitem in pr['timelineItems']['edges']:
                _timelineitem = timelineitem['node']
                obj = {
                    'type': self.events_map[_timelineitem['__typename']],
                    'id': _timelineitem['id'],
                    'created_at': _timelineitem['createdAt'],
                    'author': (
                        _timelineitem.get('actor', {}).get('login')
                        or _timelineitem.get('author', {}).get('login')
                    ),
                }
                insert_change_attributes(obj, change)
                if 'state' in _timelineitem:
                    obj['approval'] = _timelineitem['state']
                if obj['type'] == 'ChangeAbandonedEvent':
                    if change['state'] == 'MERGED':
                        obj['type'] = 'ChangeMergedEvent'
                        obj['author'] = change['merged_by']
                objects.append(obj)
            for commit in pr['commits']['edges']:
                _commit = commit['node']['commit']
                obj = {
                    'type': 'ChangeCommitPushedEvent',
                    'id': _commit['oid'],
                    # Seems the first PR's commit get a date with Node value
                    # So make sense to set the same createdçat date then the
                    # change
                    'created_at': _commit.get('pushedDate', change['created_at']),
                }
                if _commit['author'].get('user'):
                    obj['author'] = _commit['author']['user'].get('login')
                insert_change_attributes(obj, change)
                objects.append(obj)
            return objects

        objects = []
        for pr in prs:
            try:
                objects.extend(extract_pr_objects(pr))
            except Exception:
                self.log.exception("Unable to extract PR data: %s" % pr)
        return objects


if __name__ == '__main__':
    import os
    import argparse
    import json
    from pprint import pprint
    from monocle.github import graphql

    parser = argparse.ArgumentParser(prog='pullrequest')

    parser.add_argument('--loglevel', help='logging level', default='INFO')
    parser.add_argument('--token', help='A Github personal token', required=True)
    parser.add_argument('--org', help='A Github organization', required=True)
    parser.add_argument(
        '--repository', help='The repository within the organization', required=True
    )
    parser.add_argument('--id', help='The pull request id', required=True)
    parser.add_argument(
        '--output-dir', help='Store the dump in this directory',
    )

    args = parser.parse_args()

    logging.basicConfig(level=getattr(logging, args.loglevel.upper()),)
    prf = PRsFetcher(
        graphql.GithubGraphQLQuery(args.token),
        'https://github.com',
        args.org,
        args.repository,
    )
    data = prf.get_one(args.org, args.repository, args.id)
    if not args.output_dir:
        pprint(data)
    else:
        basename = "github.com-%s-%s-%s" % (args.org, args.repository, args.id)
        basepath = os.path.join(args.output_dir, basename)
        json.dump(data[0], open(basepath + '_raw.json', 'w'), indent=2)
        json.dump(data[1], open(basepath + '_extracted.json', 'w'), indent=2)
