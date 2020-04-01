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
import argparse
import os
import yaml

from pprint import pprint

from monocle import utils
from monocle.db.db import ELmonocleDB
from monocle.github import pullrequest
from monocle.gerrit import review
from monocle.crawler import Crawler

from monocle import projects
from jsonschema import validate


def main():
    parser = argparse.ArgumentParser(prog='monocle')
    parser.add_argument(
        '--loglevel', help='logging level',
        default='INFO')
    parser.add_argument(
        '--elastic-timeout', help='Elasticsearch connection retry timeout',
        default=10, type=int)
    parser.add_argument(
        '--elastic-conn', help='Elasticsearch connection info',
        default='localhost:9200')
    subparsers = parser.add_subparsers(title='Subcommands',
                                       description='valid subcommands',
                                       dest="command")

    parser_crawler = subparsers.add_parser(
        'crawler', help='Threaded crawlers pool')
    parser_crawler.add_argument(
        '--config',
        help='Configuration file of the crawlers pool',
        required=True)

    parser_dbmanage = subparsers.add_parser(
        'dbmanage', help='Database manager')
    parser_dbmanage.add_argument(
        '--delete-repository',
        help='Delete events related to a repository (regexp)',
        required=True)

    parser_dbquery = subparsers.add_parser(
        'dbquery', help='Run an existsing query on stored events')
    parser_dbquery.add_argument(
        '--interval', help='Histogram interval',
        default="3h")
    parser_dbquery.add_argument(
        '--name', help='The query name',
        required=True)
    parser_dbquery.add_argument(
        '--repository', help='Scope to events of a repository (regexp)',
        required=True)
    parser_dbquery.add_argument(
        '--gte', help='Scope to events created after date')
    parser_dbquery.add_argument(
        '--lte', help='Scope to events created before date')
    parser_dbquery.add_argument(
        '--on_cc_gte',
        help='Scope to events related to changes created after date')
    parser_dbquery.add_argument(
        '--on_cc_lte',
        help='Scope to events related to changes created before date')
    parser_dbquery.add_argument(
        '--ec-same-date',
        help='Scope to events related to changes created during the '
        'same date bondaries defined by gte/lte arguments',
        action='store_true')
    parser_dbquery.add_argument(
        '--type', help='Scope to events types list (comma separated)')
    parser_dbquery.add_argument(
        '--authors', help='Scope to authors (comma separated)')
    parser_dbquery.add_argument(
        '--approval', help='Scope to events with approval')
    parser_dbquery.add_argument(
        '--size', help='Return maximum of size results',
        default=10)
    parser_dbquery.add_argument(
        '--exclude-authors', help='Authors exclude list (comma separated)')

    args = parser.parse_args()

    logging.basicConfig(
        level=getattr(logging, args.loglevel.upper()),
        format="%(asctime)s - %(name)s - %(threadName)s - " +
               "%(levelname)s - %(message)s")

    if not args.command:
        parser.print_usage()
        return 1

    if args.command == "crawler":
        realpath = os.path.expanduser(args.config)
        if not os.path.isfile(realpath):
            print('Unable to access config: %s' % realpath)
        config = yaml.safe_load(open(realpath).read())
        validate(
            instance=config,
            schema=projects.schema)
        tpool = []
        for project in config['projects']:
            for crawler_item in project['crawler'].get(
                    'github_orgs', []):
                c_args = pullrequest.GithubCrawlerArgs(
                    command='github_crawler',
                    org=crawler_item['name'],
                    updated_since=crawler_item['updated_since'],
                    loop_delay=project['crawler']['loop_delay'],
                    token=crawler_item['token'],
                    base_url=crawler_item['base_url'])
                tpool.append(Crawler(c_args, elastic_conn=args.elastic_conn,
                                     elastic_timeout=args.elastic_timeout))
            for crawler_item in project['crawler'].get(
                    'gerrit_repositories', []):
                c_args = review.GerritCrawlerArgs(
                    command='gerrit_crawler',
                    repository=crawler_item['name'],
                    updated_since=crawler_item['updated_since'],
                    loop_delay=project['crawler']['loop_delay'],
                    base_url=crawler_item['base_url'])
                tpool.append(Crawler(c_args, elastic_conn=args.elastic_conn,
                                     elastic_timeout=args.elastic_timeout))
        for cthread in tpool:
            cthread.start()

    if args.command == "dbmanage":
        if args.delete_repository:
            db = ELmonocleDB()
            db.delete_repository(args.delete_repository)

    if args.command == "dbquery":
        db = ELmonocleDB()
        params = utils.set_params(args)
        ret = db.run_named_query(
            args.name,
            args.repository.lstrip('^'),
            params)
        pprint(ret)


if __name__ == '__main__':
    main()
