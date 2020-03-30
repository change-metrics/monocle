# MIT License
# Copyright (c) 2020 Fabien Boucher

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
from time import sleep
from datetime import datetime
from threading import Thread
from pprint import pprint

from monocle.github.graphql import GithubGraphQLQuery
from monocle.db.db import ELmonocleDB
from monocle.github import pullrequest
from monocle.gerrit import review


class Crawler(Thread):

    log = logging.getLogger(__name__)

    def __init__(self, args):
        super().__init__()
        self.updated_since = args.updated_since
        self.loop_delay = int(args.loop_delay)
        self.get_one = getattr(args, 'id', None)
        self.db = ELmonocleDB()
        if args.command == 'github_crawler':
            self.get_one_rep = getattr(args, 'repository', None)
            self.org = args.org
            self.repository_el_re = args.org.lstrip('^') + '.*'
            self.prf = pullrequest.PRsFetcher(
                GithubGraphQLQuery(args.token),
                args.base_url, args.org)
        elif args.command == 'gerrit_crawler':
            self.repository_el_re = args.repository.lstrip('^')
            self.prf = review.ReviewesFetcher(
                args.base_url, args.repository)
        self.setName(self.repository_el_re)

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
                pprint(self.prf.get_one(
                    self.org, self.get_one_rep,
                    self.get_one))
        else:
            while True:
                self.run_step()
                self.log.info("Waiting %s seconds before next fetch ..." % (
                    self.loop_delay))
                sleep(self.loop_delay)
