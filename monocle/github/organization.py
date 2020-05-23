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


import logging


class RepositoriesFetcher(object):
    def __init__(self, gql):
        self.gql = gql
        self.qdata = '''{
          organization(login: "%(login)s") {
            repositories(isFork: false first: 100) {
              totalCount
              pageInfo {
                hasNextPage endCursor
              }
              edges {
              node {
                name
                nameWithOwner
                isArchived
              }
            }}
          }
        }'''

    def get(self, login):
        repositories = []
        kwargs = {'login': login, 'after': ''}

        def _getPage(kwargs):
            data = self.gql.query(self.qdata % kwargs)
            for repository in data['data']['organization']['repositories']['edges']:
                # That's curious but the API return some forked repos
                # even isFork is at false. We can detect the bogus result
                # with nameWithOwner.
                # So here let's remove the bogus ones !
                if not repository['node']['nameWithOwner'].startswith(login + '/'):
                    continue
                if repository['node']['isArchived']:
                    continue
                repositories.append(repository['node'])
            pageInfo = data['data']['organization']['repositories']['pageInfo']
            if pageInfo['hasNextPage']:
                kwargs['after'] = 'after: "%s"' % pageInfo['endCursor']
                return True
            else:
                return False

        while True:
            hnp = _getPage(kwargs)
            if not hnp:
                break
        return repositories


if __name__ == '__main__':
    import argparse
    from pprint import pprint
    from monocle.github import graphql

    parser = argparse.ArgumentParser(prog='organization')

    parser.add_argument('--loglevel', help='logging level', default='INFO')
    parser.add_argument('--token', help='A Github personal token', required=True)
    parser.add_argument('--org', help='A Github organization', required=True)

    args = parser.parse_args()

    logging.basicConfig(level=getattr(logging, args.loglevel.upper()))
    rf = RepositoriesFetcher(graphql.GithubGraphQLQuery(args.token))
    data = rf.get(args.org)
    pprint(data)
