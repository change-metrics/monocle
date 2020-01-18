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

from time import sleep
from datetime import datetime

from monocle import utils
from monocle.db.db import ELmonocleDB
from monocle.github.graphql import GithubGraphQLQuery
from monocle.github import pullrequest
from monocle.gerrit import review


class MonocleCrawler():

    log = logging.getLogger("monocle.Crawler")

    def __init__(self, args):
        self.updated_since = args.updated_since
        self.loop_delay = int(args.loop_delay)
        self.get_one = getattr(args, 'id', None)
        self.db = ELmonocleDB()
        if args.command == 'github_crawler':
            self.get_one_rep = getattr(args, 'repository', None)
            self.org = args.org
            self.repository_el_re = args.org.lstrip('^')
            self.prf = pullrequest.PRsFetcher(
                GithubGraphQLQuery(args.token),
                args.host, args.org)
        elif args.command == 'gerrit_crawler':
            self.repository_el_re = args.repository.lstrip('^')
            self.prf = review.ReviewesFetcher(
                args.host, args.repository)

    def get_last_updated_date(self):
        change = self.db.get_last_updated(self.repository_el_re)
        if not change:
            return (
                self.updated_since or
                datetime.now().strftime("%Y-%m-%dT%H:%M:%SZ"))
        else:
            logging.info(
                "Most recent change date in the database for %s is %s" % (
                    self.repository_el_re, change['updated_at']))
            return change['updated_at']

    def run_step(self):
        updated_since = self.get_last_updated_date()
        prs = self.prf.get(updated_since)
        objects = self.prf.extract_objects(prs)
        if objects:
            self.log.info("%s objects will be updated in the database" % len(
                objects))
            self.db.update(objects)

    def run(self):
        if self.get_one:
            if not self.get_one_rep:
                print("The --repository argument must be given")
            else:
                print(self.prf.get_one(
                    self.org, self.get_one_rep,
                    self.get_one))
        else:
            while True:
                self.run_step()
                self.log.info("Waiting %s seconds before next fetch ..." % (
                    self.loop_delay))
                sleep(self.loop_delay)


def main():
    parser = argparse.ArgumentParser(prog='monocle')
    parser.add_argument(
        '--loglevel', help='logging level', default='INFO')
    subparsers = parser.add_subparsers(title='Subcommands',
                                       description='valid subcommands',
                                       dest="command")

    for crawler_driver in (pullrequest, review):
        parser_crawler = subparsers.add_parser(
            crawler_driver.name, help=crawler_driver.help)
        parser_crawler.add_argument(
            '--loop-delay', help='Request last updated events every N secs',
            default=900)
        parser_crawler.add_argument(
            '--host', help='Base url of the code review server',
            required=True)
        crawler_driver.init_crawler_args_parser(parser_crawler)

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
        '--type', help='Scope to events types list (comma separated)')
    parser_dbquery.add_argument(
        '--author', help='Scope to events author')
    parser_dbquery.add_argument(
        '--approval', help='Scope to events with approval')
    parser_dbquery.add_argument(
        '--size', help='Return maximum of size results',
        default=10)
    parser_dbquery.add_argument(
        '--exclude-authors', help='Authors exclude list (comma separated)')

    args = parser.parse_args()

    logging.basicConfig(
        level=getattr(logging, args.loglevel.upper()))

    if args.command.endswith("_crawler"):
        crawler = MonocleCrawler(args)
        crawler.run()

    if args.command == "dbmanage":
        if args.delete_repository:
            db = ELmonocleDB()
            db.delete_repository(args.delete_repository)

    print(args)
    if args.command == "dbquery":
        db = ELmonocleDB()
        if args.gte:
            args.gte = utils.date_to_epoch_ml(args.gte)
        if args.lte:
            args.lte = utils.date_to_epoch_ml(args.lte)
        if args.exclude_authors:
            args.exclude_authors = args.exclude_authors.strip().split(',')
        if args.type:
            args.type = args.type.strip().split(',')
        params = {
            'gte': args.gte,
            'lte': args.lte,
            'etype': args.type,
            'author': args.author,
            'interval': args.interval,
            'approval': args.approval,
            'size': args.size,
            'exclude_authors': args.exclude_authors
        }
        ret = db.run_named_query(
            args.name,
            args.repository.lstrip('^'),
            params)
        print(ret)


if __name__ == '__main__':
    main()
