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
        '--delete-repository',
        help='Delete events related to a repository (regexp)',
        required=True,
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
    parser_dbquery.add_argument('--approval', help='Scope to events with approval')
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
        format="%(asctime)s - %(name)s - %(threadName)s - "
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
        for tenant in configdata['tenants']:
            for crawler_item in tenant['crawler'].get('github_orgs', []):
                c_args = pullrequest.GithubCrawlerArgs(
                    command='github_crawler',
                    index=tenant['index'],
                    org=crawler_item['name'],
                    updated_since=crawler_item['updated_since'],
                    loop_delay=tenant['crawler']['loop_delay'],
                    token=crawler_item['token'],
                    repository=crawler_item.get('repository'),
                    base_url=crawler_item['base_url'],
                )
                log.info('args=%s' % c_args)
                if crawler_item['token'] not in group:
                    group[crawler_item['token']] = GroupCrawler()
                    tpool.append(group[crawler_item['token']])
                group[crawler_item['token']].add_crawler(
                    Runner(
                        c_args,
                        elastic_conn=args.elastic_conn,
                        elastic_timeout=args.elastic_timeout,
                    )
                )
            for crawler_item in tenant['crawler'].get('gerrit_repositories', []):
                c_args = review.GerritCrawlerArgs(
                    command='gerrit_crawler',
                    index=tenant['index'],
                    repository=crawler_item['name'],
                    updated_since=crawler_item['updated_since'],
                    loop_delay=tenant['crawler']['loop_delay'],
                    base_url=crawler_item['base_url'],
                )
                tpool.append(
                    Crawler(
                        c_args,
                        elastic_conn=args.elastic_conn,
                        elastic_timeout=args.elastic_timeout,
                    )
                )
        log.info('%d configured threads' % len(tpool))
        for cthread in tpool:
            cthread.start()

    if args.command == "dbmanage":
        if args.delete_repository:
            db = ELmonocleDB(elastic_conn=args.elastic_conn, index=args.index)
            db.delete_repository(args.delete_repository)

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
