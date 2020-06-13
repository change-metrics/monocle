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
import argparse
import os
import sys
import yaml

from pprint import pprint

from jsonschema import validate

from monocle import utils
from monocle.db.db import ELmonocleDB
from monocle.db.db import UnknownQueryException
from monocle.github import pullrequest
from monocle.github import application
from monocle.github import organization
from monocle.github import graphql
from monocle.gerrit import review
from monocle.crawler import Crawler, Runner, GroupCrawler
from monocle import config


def main():
    parser = argparse.ArgumentParser(prog='monocle')
    parser.add_argument('--loglevel', help='logging level', default='INFO')
    parser.add_argument(
        '--elastic-timeout',
        help='Elasticsearch connection retry timeout',
        default=10,
        type=int,
    )
    parser.add_argument(
        '--elastic-conn', help='Elasticsearch connection info', default='localhost:9200'
    )
    subparsers = parser.add_subparsers(
        title='Subcommands', description='valid subcommands', dest="command"
    )

    parser_crawler = subparsers.add_parser('crawler', help='Threaded crawlers pool')
    parser_crawler.add_argument(
        '--config', help='Configuration file of the crawlers pool', required=True
    )

    parser_dbmanage = subparsers.add_parser('dbmanage', help='Database manager')
    parser_dbmanage.add_argument(
        '--delete-repository', help='Delete events related to a repository (regexp)',
    )
    parser_dbmanage.add_argument(
        '--delete-index', help='Delete the index', action='store_true',
    )
    parser_dbmanage.add_argument(
        '--index', help='The Elastisearch index name', required=True
    )

    parser_dbquery = subparsers.add_parser(
        'dbquery', help='Run an existsing query on stored events'
    )
    parser_dbquery.add_argument(
        '--index', help='The Elastisearch index name', required=True
    )
    parser_dbquery.add_argument('--name', help='The query name', required=True)
    parser_dbquery.add_argument(
        '--repository', help='Scope to events of repositories (regexp)', required=True
    )
    parser_dbquery.add_argument(
        '--target-branch', help='Scope to events of a target branches (regexp)'
    )
    parser_dbquery.add_argument('--gte', help='Scope to events created after date')
    parser_dbquery.add_argument('--lte', help='Scope to events created before date')
    parser_dbquery.add_argument(
        '--on_cc_gte', help='Scope to events related to changes created after date'
    )
    parser_dbquery.add_argument(
        '--on_cc_lte', help='Scope to events related to changes created before date'
    )
    parser_dbquery.add_argument(
        '--ec-same-date',
        help='Scope to events related to changes created during the '
        'same date bondaries defined by gte/lte arguments',
        action='store_true',
    )
    parser_dbquery.add_argument(
        '--type', help='Scope to events types list (comma separated)'
    )
    parser_dbquery.add_argument(
        '--files', help='Scope to changes containing this file regexp'
    )
    parser_dbquery.add_argument(
        '--state',
        help='Scope to changes having this state',
        choices=['OPEN', 'CLOSED', 'MERGED'],
    )
    parser_dbquery.add_argument(
        '--change-ids', help='Scope to change ids (comma separated)'
    )
    parser_dbquery.add_argument('--authors', help='Scope to authors (comma separated)')
    parser_dbquery.add_argument(
        '--approvals', help='Scope to objects with approvals (comma separated)'
    )
    parser_dbquery.add_argument(
        '--exclude-approvals', help='Approvals exclude list (comma separated)'
    )
    parser_dbquery.add_argument(
        '--size', help='Return maximum of size results', default=10
    )
    parser_dbquery.add_argument(
        '--from', help='Starting index of the elements to retrieve', default=0
    )
    parser_dbquery.add_argument(
        '--exclude-authors', help='Authors exclude list (comma separated)'
    )
    parser_dbquery.add_argument(
        '--tests-included',
        help='Scope to changes containing tests',
        action='store_true',
    )
    parser_dbquery.add_argument(
        '--has-issue-tracker-links',
        help='Scope to changes containing an issue tracker link',
        choices=['generic', 'github.com', 'altassian.net'],
    )

    args = parser.parse_args()

    logging.basicConfig(
        level=getattr(logging, args.loglevel.upper()),
        format="%(asctime)s - %(name)s - %(thread)d - %(threadName)s - "
        + "%(levelname)s - %(message)s",
    )
    log = logging.getLogger(__name__)

    if not args.command:
        parser.print_usage()
        return 1

    if args.command == "crawler":
        realpath = os.path.expanduser(args.config)
        if not os.path.isfile(realpath):
            log.error('Unable to access config: %s' % realpath)
            sys.exit(1)
        configdata = yaml.safe_load(open(realpath).read())
        validate(instance=configdata, schema=config.schema)
        tpool = []
        group = {}
        app = None
        if os.getenv('APP_ID') and os.getenv('APP_KEY_PATH'):
            app = application.get_app(os.getenv('APP_ID'), os.getenv('APP_KEY_PATH'))
        for tenant in configdata['tenants']:
            for crawler_item in tenant['crawler'].get('github_orgs', []):
                tg = pullrequest.TokenGetter(
                    crawler_item['name'], crawler_item.get('token'), app
                )
                c_args = pullrequest.GithubCrawlerArgs(
                    command='github_crawler',
                    org=crawler_item['name'],
                    updated_since=crawler_item['updated_since'],
                    loop_delay=tenant['crawler']['loop_delay'],
                    repository=crawler_item.get('repository'),
                    base_url=crawler_item['base_url'],
                    token_getter=tg,
                    db=ELmonocleDB(
                        elastic_conn=args.elastic_conn,
                        index=tenant['index'],
                        timeout=args.elastic_timeout,
                    ),
                )
                gid = crawler_item.get('token')
                if not gid:
                    if app:
                        # No token, if we have a app then get the token from the app
                        gid = app.get_token(org=crawler_item['name'])
                    else:
                        log.info('Skip crawler because no token: %s' % c_args)
                        continue
                if gid not in group:
                    group[gid] = GroupCrawler()
                    tpool.append(group[gid])
                if c_args.repository:
                    repositories = [c_args.repository]
                else:
                    log.info('Discovering repositories in %s ...' % c_args.org)
                    # No repository specified for that organization so
                    # try to discover all of them
                    rf = organization.RepositoriesFetcher(
                        graphql.GithubGraphQLQuery(token_getter=tg)
                    )
                    repos = rf.get(c_args.org)
                    repositories = [
                        repo['name'] for repo in repos if not repo['isArchived']
                    ]
                    log.info(
                        'Found %s repositories in %s ...'
                        % (len(repositories), c_args.org)
                    )
                for repository in repositories:
                    c_args.repository = repository
                    group[gid].add_crawler(Runner(c_args))
            for crawler_item in tenant['crawler'].get('gerrit_repositories', []):
                c_args = review.GerritCrawlerArgs(
                    command='gerrit_crawler',
                    repository=crawler_item['name'],
                    updated_since=crawler_item['updated_since'],
                    loop_delay=tenant['crawler']['loop_delay'],
                    base_url=crawler_item['base_url'],
                    insecure=crawler_item.get('insecure', False),
                    login=crawler_item.get('login'),
                    password=crawler_item.get('password'),
                    db=ELmonocleDB(
                        elastic_conn=args.elastic_conn,
                        index=tenant['index'],
                        timeout=args.elastic_timeout,
                    ),
                )
                tpool.append(Crawler(c_args))
        log.info('%d configured threads' % len(tpool))
        for cthread in tpool:
            cthread.start()

    if args.command == "dbmanage":
        db = ELmonocleDB(elastic_conn=args.elastic_conn, index=args.index)
        if args.delete_repository:
            db.delete_repository(args.delete_repository)
        if args.delete_index:
            db.delete_index()

    if args.command == "dbquery":
        db = ELmonocleDB(elastic_conn=args.elastic_conn, index=args.index)
        params = utils.set_params(args)
        try:
            ret = db.run_named_query(args.name, args.repository.lstrip('^'), params)
        except UnknownQueryException as err:
            log.error('Unable to run query: %s' % err)
            sys.exit(1)
        pprint(ret)


if __name__ == '__main__':
    main()
