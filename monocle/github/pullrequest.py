# Monocle.
# Copyright (C) 2019-2020 Monocle authors
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

import sys
import logging
from datetime import datetime
from dataclasses import dataclass
from pprint import pprint

from monocle.github.graphql import RequestTimeout
from monocle.utils import dbdate_to_datetime

MAX_TRY = 3
MAX_BULK_SIZE = 100
REDUCE = 2
AUGMENT = 1.1

name = 'github_crawler'
help = 'Github Crawler to fetch PRs events'


class ExtractPRIssue(Exception):
    def __init__(self, excpt, pr, idx=-1):
        self.excpt = excpt
        self.pr = pr
        self.idx = idx


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

    def __init__(self, gql, base_url, org, repository):
        self.gql = gql
        self.size = MAX_BULK_SIZE
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
          reviewDecision
          number
          mergeable
          isDraft
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
          %s
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
        self.pr_commits = '''
          commits (first: 100){
            edges {
              node {
                commit {
                  oid
                  pushedDate
                  authoredDate
                  committedDate
                  additions
                  deletions
                  message
                  author {
                    user {
                      login
                    }
                  }
                  committer {
                    user {
                      login
                    }
                  }
                }
              }
            }
          }
        '''  # noqa: E501

    def _getPage(self, kwargs, prs):
        scope = "org:%(org)s" % kwargs
        if kwargs.get('repository'):
            scope = "repo:%(org)s/%(repository)s" % kwargs
        kwargs['scope'] = scope
        qdata = '''{
          repository(owner: "%(org)s", name:"%(repository)s") {
            pullRequests(
              first: %(size)s
              %(after)s
              orderBy: { field: UPDATED_AT, direction: DESC }
            ) {
              totalCount
              pageInfo {
                hasNextPage endCursor
              }
              edges {
                node {
                  %(pr_query)s
                }
              }
            }
          }
        }'''  # noqa: E501
        data = self.gql.query(qdata % kwargs)
        if not kwargs['total_prs_count']:
            kwargs['total_prs_count'] = data['data']['repository']['pullRequests'][
                'totalCount'
            ]
            self.log.info(
                "Total PRs: %s but will fetch until we reached a PR"
                "updated at date < %s"
                % (kwargs['total_prs_count'], kwargs['updated_since'])
            )
        if 'data' not in data:
            self.log.error('No data collected: %s' % data)
            return False
        edges = data['data']['repository']['pullRequests']['edges']
        for pr in edges:
            prs.append(pr['node'])
        # We sort to mitigate this
        # https://github.community/t5/GitHub-API-Development-and/apiv4-pullrequests-listing-broken-ordering/m-p/59439#M4968
        oldest_update = sorted(
            [dbdate_to_datetime(pr['node']['updatedAt']) for pr in edges], reverse=True
        )[-1]
        logging.info("page oldest updated at date is %s" % oldest_update)
        if oldest_update < kwargs['updated_since']:
            # The crawler reached a page where the oldest updated PR
            # is oldest than the configured limit
            return False
        pageInfo = data['data']['repository']['pullRequests']['pageInfo']
        if pageInfo['hasNextPage']:
            kwargs['after'] = 'after: "%s"' % pageInfo['endCursor']
            return True
        else:
            return False

    def get(self, updated_since):
        prs = []
        kwargs = {
            'pr_query': self.pr_query % self.pr_commits,
            'org': self.org,
            'repository': self.repository,
            'updated_since': datetime.strptime(updated_since, "%Y-%m-%d"),
            'after': '',
            'total_prs_count': 0,
            'size': self.size,
        }
        one = 0
        get_commits = True
        while True:
            self.log.info(
                'Running request %s'
                % dict([(k, v) for k, v in kwargs.items() if k != 'pr_query'])
            )
            try:
                hnp = self._getPage(kwargs, prs)
                if kwargs['size'] == 1:
                    pr = prs[0]
                    self.log.info('PR %s' % pr)
                kwargs['size'] = min(MAX_BULK_SIZE, int(kwargs['size'] * AUGMENT) + 1)
                one = 0
                if not get_commits:
                    self.log.info('Will get full commits on next query.')
                    kwargs['pr_query'] = self.pr_query % self.pr_commits
                    get_commits = True
            except RequestTimeout:
                kwargs['size'] = max(1, kwargs['size'] // REDUCE)
                if kwargs['size'] == 1:
                    one += 1
                    if one == MAX_TRY - 1:
                        self.log.info(
                            '%d timeouts in a raw for one pr, retrying without commits.'
                            % (MAX_TRY - 1)
                        )
                        get_commits = False
                        kwargs['pr_query'] = self.pr_query % ''
                    elif one >= MAX_TRY:
                        self.log.info(
                            '%d timeouts in a raw for one pr, giving up.' % MAX_TRY
                        )
                        raise
                continue
            self.log.info("%s PRs fetched" % len(prs))
            if not hnp:
                break
        return prs

    def get_one(self, org, repository, number):
        def _dumper(raw, prefix=None):
            pprint(raw)

        qdata = '''{
          repository(owner: "%(org)s", name:"%(repository)s") {
            pullRequest(number: %(number)s) {
              %(pr_query)s
            }
          }
        }
        '''
        kwargs = {
            'pr_query': self.pr_query % self.pr_commits,
            'org': org,
            'repository': repository,
            'number': number,
        }
        raw = self.gql.query(qdata % kwargs)['data']['repository']['pullRequest']
        return (raw, self.extract_objects([raw], _dumper))

    def extract_objects(self, prs, dumper=None):
        def get_login(data):
            if data and 'login' in data and data['login']:
                return data['login']
            return 'ghost'

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
                    'branch': change['branch'],
                    'target_branch': change['target_branch'],
                    'number': change['number'],
                    'change_id': change['change_id'],
                    'url': change['url'],
                    'on_author': change['author'],
                    'on_created_at': change['created_at'],
                    'changed_files': [
                        {'path': cf['path']} for cf in change['changed_files']
                    ],
                }
            )

        def extract_pr_objects(pr):
            objects = []
            change = {}
            change['type'] = 'Change'
            change['id'] = pr['id']
            change['draft'] = pr['isDraft']
            change['number'] = pr['number']
            change['repository_prefix'] = get_login(pr['repository']['owner'])
            change['repository_fullname'] = "%s/%s" % (
                get_login(pr['repository']['owner']),
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
            if 'commits' not in pr:
                pr['commits'] = {'edges': []}
            change['author'] = get_login(pr['author'])
            change['branch'] = pr['headRefName']
            change['target_branch'] = pr['baseRefName']
            change['title'] = pr['title']
            change['text'] = pr['bodyText']
            change['additions'] = pr['additions']
            change['deletions'] = pr['deletions']
            change['approval'] = [pr['reviewDecision']]
            change['changed_files_count'] = pr['changedFiles']
            if pr["files"] and "edges" in pr["files"]:
                change["changed_files"] = [
                    {
                        "additions": fd['node']["additions"],
                        "deletions": fd['node']["deletions"],
                        "path": fd['node']["path"],
                    }
                    for fd in pr["files"]["edges"]
                ]
            else:
                change["changed_files"] = []
            change['commits'] = []
            change['commit_count'] = len(pr['commits']['edges'])
            if pr['mergedBy']:
                change['merged_by'] = get_login(pr['mergedBy'])
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
                map(lambda n: get_login(n['node']), pr['assignees']['edges'])
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
                    'author': get_login(_comment['author']),
                }
                insert_change_attributes(obj, change)
                objects.append(obj)
            for timelineitem in pr['timelineItems']['edges']:
                _timelineitem = timelineitem['node']
                _author = _timelineitem.get('actor', {}) or _timelineitem.get(
                    'author', {}
                )
                if not _author:
                    _author = {'login': 'ghost'}
                obj = {
                    'type': self.events_map[_timelineitem['__typename']],
                    'id': _timelineitem['id'],
                    'created_at': _timelineitem['createdAt'],
                    'author': _author.get('login'),
                }
                insert_change_attributes(obj, change)
                if 'state' in _timelineitem:
                    obj['approval'] = _timelineitem['state']
                if obj['type'] == 'ChangeAbandonedEvent':
                    if change['state'] == 'MERGED':
                        obj['type'] = 'ChangeMergedEvent'
                        obj['author'] = change['merged_by']
                objects.append(obj)
            # Here we don't use the PullRequestCommit timeline event because
            # it does not provide more data than the current list of commits
            # of the pull request
            for commit in pr['commits']['edges']:
                if not commit['node']:
                    continue
                _commit = commit['node']['commit']
                obj = {
                    'type': 'ChangeCommitPushedEvent',
                    'id': _commit['oid'],
                    # Seems the first PR's commit get a date with Node value
                    # So make sense to set the same created_at date as the
                    # change
                    'created_at': _commit.get('pushedDate', change['created_at']),
                }
                if _commit['committer'].get('user'):
                    obj['author'] = get_login(_commit['committer']['user'])
                insert_change_attributes(obj, change)
                objects.append(obj)
            # Now attach a commits list to the change
            for commit in pr['commits']['edges']:
                if not commit['node']:
                    continue
                _commit = commit['node']['commit']
                obj = {
                    'sha': _commit['oid'],
                    'authored_at': _commit['authoredDate'],
                    'committed_at': _commit['committedDate'],
                    'additions': _commit['additions'],
                    'deletions': _commit['deletions'],
                    'title': _commit['message'],
                }
                for k in ('author', 'committer'):
                    if _commit[k].get('user'):
                        obj[k] = get_login(_commit[k]['user'])
                change['commits'].append(obj)
            return objects

        objects = []
        for pr in prs:
            try:
                objects.extend(extract_pr_objects(pr))
            except Exception:
                self.log.exception('Unable to extract PR')
                if dumper:
                    dumper(pr, 'github_')
        return objects


if __name__ == '__main__':
    import os
    import json
    import argparse
    from monocle.github import graphql

    parser = argparse.ArgumentParser(prog='pullrequest')

    parser.add_argument('--crawler', help='run crawler', action='store_true')
    parser.add_argument('--updated-since', help='stop date for the crawler')
    parser.add_argument('--loglevel', help='logging level', default='INFO')
    parser.add_argument('--token', help='A Github personal token', required=True)
    parser.add_argument('--org', help='A Github organization', required=True)
    parser.add_argument(
        '--repository', help='The repository within the organization', required=True
    )
    parser.add_argument('--id', help='The pull request id')
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
    if args.crawler:
        if not args.updated_since:
            print("please provide --update-since arg")
            sys.exit(1)
        prs = prf.get(args.updated_since)
        pprint(prs)
    else:
        if not args.id:
            print("please provide --id arg")
            sys.exit(1)
        data = prf.get_one(args.org, args.repository, args.id)
        if not args.output_dir:
            pprint(data)
        else:
            basename = "github.com-%s-%s-%s" % (args.org, args.repository, args.id)
            basepath = os.path.join(args.output_dir, basename)
            json.dump(data[0], open(basepath + '_raw.json', 'w'), indent=2)
            json.dump(data[1], open(basepath + '_extracted.json', 'w'), indent=2)
