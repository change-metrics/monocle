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

from monocle.db.db import ELmonocleDB
from monocle.github.graphql import GithubGraphQLQuery
from monocle.github.pullrequest import PRsFetcher


class MonocleCrawler():

    log = logging.getLogger("monocle.Crawler")

    def __init__(self, org, updated_since, token, loop_delay):
        self.org = org
        self.updated_since = updated_since
        self.loop_delay = loop_delay
        self.db = ELmonocleDB()
        self.prf = PRsFetcher(GithubGraphQLQuery(token))

    def get_last_updated_date(self, org):
        pr = self.db.get_last_updated(org)
        if not pr:
            return (
                self.updated_since or
                datetime.now().strftime("%Y-%m-%dT%H:%M:%SZ"))
        else:
            return pr['updated_at']

    def run_step(self):
        updated_since = self.get_last_updated_date(self.org)
        prs = self.prf.get(self.org, updated_since)
        objects = self.prf.extract_objects(prs)
        if objects:
            self.log.info("%s objects will be updated in the database" % len(
                objects))
            self.db.update(objects)

    def run(self):
        while True:
            self.run_step()
            self.log.info("Waiting %s seconds before next fetch ..." % (
                self.loop_delay))
            sleep(self.loop_delay)


def date_to_epoch_ml(datestr):
    return int(datetime.strptime(
        datestr, "%Y-%m-%d").timestamp() * 1000)


def main():
    parser = argparse.ArgumentParser(prog='monocle')
    parser.add_argument(
        '--loglevel', help='logging level', default='INFO')
    subparsers = parser.add_subparsers(title='subcommands',
                                       description='valid subcommands',
                                       dest="command")

    parser_crawler = subparsers.add_parser(
        'crawler', help='Crawler to fetch PRs events')
    parser_crawler.add_argument(
        '--token', help='A Github API token',
        required=True)
    parser_crawler.add_argument(
        '--org', help='The Github organization to fetch PR events',
        required=True)
    parser_crawler.add_argument(
        '--updated-since', help='Acts on PRs updated since')
    parser_crawler.add_argument(
        '--loop-delay', help='Request PRs events every N secs',
        default=900)

    parser_db = subparsers.add_parser(
        'database', help='Database manager')
    parser_db.add_argument(
        '--delete-org', help='Delete PRs event related to an org',
        required=True)

    parser_fetcher = subparsers.add_parser(
        'fetch', help='Fetch PullRequest from GraphQL')
    parser_fetcher.add_argument(
        '--token', help='A Github API token',
        required=True)
    parser_fetcher.add_argument(
        '--org', help='The Github organization to fetch the PR from',
        required=True)
    parser_fetcher.add_argument(
        '--repository', help='The PR repository within the organization',
        required=True)
    parser_fetcher.add_argument(
        '--id', help='The PR id within the repository',
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
        '--org', help='Scope to events of an organization',
        required=True)
    parser_dbquery.add_argument(
        '--gte', help='Scope to events created after date')
    parser_dbquery.add_argument(
        '--lte', help='Scope to events created before date')
    parser_dbquery.add_argument(
        '--type', help='Scope to events type')

    args = parser.parse_args()

    logging.basicConfig(
        level=getattr(logging, args.loglevel.upper()))

    if args.command == "crawler":
        crawler = MonocleCrawler(
            args.org, args.updated_since,
            args.token, int(args.loop_delay))
        crawler.run()

    if args.command == "database":
        if args.delete_org:
            db = ELmonocleDB()
            db.delete_org(args.delete_org)

    if args.command == "fetch":
        prf = PRsFetcher(GithubGraphQLQuery(args.token))
        pr = prf.get_pr(args.org, args.repository, args.id)
        print(pr)

    if args.command == "dbquery":
        db = ELmonocleDB()
        if args.gte:
            args.gte = date_to_epoch_ml(args.gte)
        if args.lte:
            args.lte = date_to_epoch_ml(args.lte)
        ret = db.run_named_query(
            args.name,
            args.org, args.gte, args.lte, args.type,
            interval=args.interval)
        print(ret)


if __name__ == '__main__':
    main()
