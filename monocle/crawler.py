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

import json
import logging
import os
import tempfile
from time import sleep
from threading import Thread

from monocle.github.graphql import GithubGraphQLQuery
from monocle.github import pullrequest
from monocle.gerrit import review
from monocle.utils import utcnow

log = logging.getLogger(__name__)

DUMP_DIR = "/var/lib/crawler"


class Runner(object):
    def __init__(self, args):
        super().__init__()
        self.updated_since = args.updated_since
        self.dump_dir = DUMP_DIR if os.path.isdir(DUMP_DIR) else None
        self.loop_delay = int(args.loop_delay)
        self.db = args.db
        if args.command == "github_crawler":
            if args.repository:
                self.repository_el_re = "%s/%s" % (
                    args.org.lstrip("^"),
                    args.repository.lstrip("^"),
                )
            else:
                self.repository_el_re = args.org.lstrip("^") + "/.*"
            self.prf = pullrequest.PRsFetcher(
                GithubGraphQLQuery(token_getter=args.token_getter),
                args.base_url,
                args.org,
                args.repository,
            )
        elif args.command == "gerrit_crawler":
            self.repository_el_re = args.repository.lstrip("^")
            self.prf = review.ReviewesFetcher(
                args.base_url,
                args.repository,
                args.insecure,
                login=args.login,
                password=args.password,
            )

    def get_last_updated_date(self):
        change = self.db.get_last_updated(self.repository_el_re)
        if not change:
            return self.updated_since or utcnow().strftime("%Y-%m-%dT%H:%M:%SZ")
        else:
            log.info(
                "Most recent change date in the database for %s is %s"
                % (self.repository_el_re, change["updated_at"])
            )
            return change["updated_at"]

    def run_step(self):
        def dump_data(data, prefix=None):
            try:
                if self.dump_dir:
                    tmpfile = tempfile.NamedTemporaryFile(
                        dir=self.dump_dir,
                        prefix=prefix,
                        suffix=".json",
                        mode="w",
                        delete=False,
                    )
                    json.dump(data, tmpfile)
                    tmpfile.close()
                    log.info("Data dumped to %s" % tmpfile.name)
                    return tmpfile.name
            except Exception:
                log.exception("Unable to dump data")
            return None

        updated_since = self.get_last_updated_date()
        try:
            prs = self.prf.get(updated_since)
        except Exception:
            log.exception("Unable to get PR data")
            return
        objects = self.prf.extract_objects(prs, dump_data)
        if objects:
            log.info("%d objects will be updated in the database" % len(objects))
            self.db.update(objects)


class Crawler(Thread, Runner):
    def __init__(self, args):
        Runner.__init__(self, args)
        Thread.__init__(self)

    def run(self):
        self.setName(self.repository_el_re)
        while True:
            self.run_step()
            log.info("Waiting %s seconds before next fetch ..." % (self.loop_delay))
            sleep(self.loop_delay)


class GroupCrawler(Thread):
    def __init__(self):
        super().__init__()
        self.crawlers = []

    def add_crawler(self, crawler):
        self.crawlers.append(crawler)

    def run(self):
        while True:
            for crawler in self.crawlers:
                self.setName(crawler.repository_el_re)
                crawler.run_step()
            if self.crawlers:
                delay = self.crawlers[0].loop_delay
            else:
                delay = 300
            log.info("Waiting %s seconds before next fetch ..." % delay)
            sleep(delay)
