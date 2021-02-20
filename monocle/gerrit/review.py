# Monocle.
# Copyright (C) 2019-2021 Monocle authors
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
import requests
from requests.auth import HTTPBasicAuth
from datetime import datetime
import json
import re

from dacite import from_dict

from dataclasses import dataclass
from typing import List, Union, Optional, Callable

from monocle import utils
from monocle.db.db import (
    Change,
    Event,
    File,
    SimpleFile,
    Commit,
    change_or_event_to_dict,
)
from monocle.basecrawler import BaseCrawler, RawChange, create_ident


name = "gerrit_crawler"
help = "Gerrit Crawler to fetch Reviews events"


@dataclass
class GerritCrawlerArgs(object):
    updated_since: str
    loop_delay: int
    command: str
    base_url: str
    repository: str
    db: object
    insecure: bool
    login: str
    password: str
    prefix: str


class ReviewesFetcher(BaseCrawler):

    log = logging.getLogger(__name__)

    def __init__(
        self,
        base_url,
        repository_prefix,
        insecure=False,
        login=None,
        password=None,
        prefix=None,
    ):
        self.base_url = base_url
        self.repository_prefix = repository_prefix
        self.insecure = insecure
        self.status_map = {"NEW": "OPEN", "MERGED": "MERGED", "ABANDONED": "CLOSED"}
        self.message_re = re.compile(r"Patch Set \d+:( [^ ]+[+-]\d+)?\n\n.+")
        self.approval_re = re.compile(r"Patch Set \d+:(?P<approval> [^ ]+[+-]\d+)\n.*")
        self.auth = None
        if login:
            self.auth = HTTPBasicAuth(login, password)
        self.prefix = prefix

    def convert_date_for_db(self, str_date):
        cdate = datetime.strptime(str_date[:-10], "%Y-%m-%d %H:%M:%S").strftime(
            "%Y-%m-%dT%H:%M:%SZ"
        )
        return cdate

    def get(
        self, updated_since: str, change_id: Optional[str] = None
    ) -> List[RawChange]:
        if not change_id:
            request_params = "?q=after:%s+project:%s" % (
                utils.is8601_to_dt(updated_since).strftime("%Y-%m-%d"),
                self.repository_prefix,
            )
        else:
            request_params = "?q=change:%s" % change_id
        for option in [
            "MESSAGES",
            "DETAILED_ACCOUNTS",
            "DETAILED_LABELS",
            "CURRENT_REVISION",
            "CURRENT_FILES",
            "CURRENT_COMMIT",
        ]:
            request_params += "&o=%s" % option
        count = 100
        start_after = 0
        reviews = []
        while True:
            urlpath = (
                self.base_url
                + "/changes/"
                + request_params
                + "&n=%s&start=%s" % (count, start_after)
            )
            self.log.info("query: %s" % urlpath)
            try:
                response = requests.get(
                    urlpath, verify=not self.insecure, auth=self.auth
                )
                response.raise_for_status()
            except Exception:
                self.log.exception("Unable to process the Gerrit query request")
                break
            _reviewes = json.loads(response.text[4:])
            if _reviewes:
                reviews.extend(_reviewes)
                self.log.info("read %s reviews from the api" % len(reviews))
                if reviews[-1].get("_more_changes"):
                    start_after = len(reviews)
                else:
                    break
            else:
                break
        if self.prefix:
            for review in reviews:
                review["project"] = self.prefix + review["project"]
        return reviews

    def extract_objects(
        self,
        changes: List[RawChange],
        dumper: Callable,
    ) -> List[Union[Change, Event]]:
        def timedelta(start, end):
            start = utils.is8601_to_dt(start)
            end = utils.is8601_to_dt(end)
            return int((start - end).total_seconds())

        def insert_change_attributes(obj, change):
            obj.update(
                {
                    "repository_prefix": change["repository_prefix"],
                    "repository_fullname": change["repository_fullname"],
                    "repository_shortname": change["repository_shortname"],
                    "branch": change["branch"],
                    "target_branch": change["target_branch"],
                    "number": change["number"],
                    "change_id": change["change_id"],
                    "url": change["url"],
                    "on_author": change["author"],
                    "on_created_at": change["created_at"],
                    "changed_files": [
                        SimpleFile(path=cf.path) for cf in change["changed_files"]
                    ],
                }
            )

        def extract_pr_objects(review) -> List[Union[Change, Event]]:
            objects: List[Union[Change, Event]] = []

            def get_pfs(name):
                fullname = name
                if self.prefix:
                    name = name.replace(self.prefix, "")
                prefix = name.split("/")[0]
                if self.prefix:
                    prefix = self.prefix + prefix
                shortname = "/".join(name.split("/")[1:]) if "/" in name else name
                return prefix, fullname, shortname

            repository_prefix, repository_fullname, repository_shortname = get_pfs(
                review["project"]
            )

            url = "%s/%s" % (self.base_url, review["_number"])

            change = {
                "_type": "Change",
                "_id": review["id"],
                "draft": False,
                "number": review["_number"],
                "target_branch": review["branch"],
                "branch": review["branch"],
                "repository_prefix": repository_prefix,
                "repository_fullname": repository_fullname,
                "repository_shortname": repository_shortname,
                "url": url,
                "author": create_ident(
                    url,
                    "%s/%s"
                    % (review["owner"].get("name"), review["owner"]["_account_id"]),
                ),
                "title": review["subject"],
                "updated_at": self.convert_date_for_db(review["updated"]),
                "created_at": self.convert_date_for_db(review["created"]),
                "merged_at": (
                    self.convert_date_for_db(review.get("submitted"))
                    if review.get("submitted")
                    else self.convert_date_for_db(review["updated"])
                ),
                "self_merged": None,
                "merged_by": None,
                "approval": [],
                "state": self.status_map[review["status"]],
                # Note(fbo): Gerrit labels must be handled as Review
                "labels": [],
                # Note(fbo): Only one assignee possible by review on Gerrit
                "assignees": (
                    [
                        create_ident(
                            url,
                            "%s/%s"
                            % (
                                review["assignee"].get("name"),
                                review["assignee"]["_account_id"],
                            ),
                        )
                    ]
                    if review.get("assignee")
                    else []
                ),
                "additions": review["insertions"],
                "deletions": review["deletions"],
                "commits": [],
                # Gerrit review is one commit by review
                "commit_count": 1,
                "changed_files_count": len(
                    list(review["revisions"].values())[0]["files"].keys()
                ),
                "changed_files": [
                    File(
                        additions=details.get("lines_inserted", 0),
                        deletions=details.get("lines_deleted", 0),
                        path=path,
                    )
                    for path, details in list(review["revisions"].values())[0][
                        "files"
                    ].items()
                ],
                "text": list(review["revisions"].values())[0]["commit"]["message"],
            }
            change["change_id"] = "%s@%s" % (
                change["repository_fullname"].replace("/", "@"),
                change["number"],
            )

            # Extract commit data
            _commit = list(review["revisions"].values())[0]
            obj = {}
            obj["sha"] = list(review["revisions"].keys())[0]
            obj["title"] = _commit["commit"]["subject"]
            obj["additions"] = change["additions"]
            obj["deletions"] = change["deletions"]
            obj["authored_at"] = self.convert_date_for_db(
                _commit["commit"]["author"]["date"]
            )
            obj["committed_at"] = self.convert_date_for_db(
                _commit["commit"]["committer"]["date"]
            )
            obj["author"] = change["author"]
            obj["committer"] = create_ident(
                url,
                "%s/%s"
                % (
                    _commit["uploader"].get("name"),
                    _commit["uploader"]["_account_id"],
                ),
            )
            change["commits"].append(from_dict(data_class=Commit, data=obj))

            # Note(fbo): Gerrit 3.x does not return the mergeable status
            mergeable = review.get("mergeable")
            if mergeable is True:
                change["mergeable"] = "MERGEABLE"
            elif mergeable is False:
                change["mergeable"] = "CONFLICTING"
            else:
                change["mergeable"] = "UNKNOWN"

            if change["state"] == "CLOSED":
                # CLOSED means abandoned in that context
                # use updated_at date as closed_at
                change["closed_at"] = change["updated_at"]
            if change["state"] == "MERGED":
                change["closed_at"] = change["merged_at"]
            if change["state"] in ("CLOSED", "MERGED"):
                change["duration"] = timedelta(
                    change["closed_at"], change["created_at"]
                )
            if change["state"] == "MERGED":
                if "submitter" in review:
                    # Gerrit 2.x seems to not have that submitter attribute
                    change["merged_by"] = create_ident(
                        url,
                        "%s/%s"
                        % (
                            review["submitter"].get("name"),
                            review["submitter"]["_account_id"],
                        ),
                    )
                    change["self_merged"] = (
                        change["merged_by"].uid == change["author"].uid
                    )
            for label in review["labels"]:
                for _review in review["labels"][label].get("all", []):
                    # If the date field exists then it means a review label
                    # has been set by someone
                    if "date" in _review and "value" in _review:
                        change["approval"].append(
                            "%s%s"
                            % (
                                label,
                                (
                                    "+%s" % _review["value"]
                                    if not str(_review["value"]).startswith("-")
                                    else _review["value"]
                                ),
                            )
                        )
            objects.append(from_dict(data_class=Change, data=change))

            obj = {
                "_type": "ChangeCreatedEvent",
                "_id": "CCE" + change["_id"],
                "created_at": change["created_at"],
                "author": change["author"],
            }
            insert_change_attributes(obj, change)
            objects.append(from_dict(data_class=Event, data=obj))

            if change["state"] in ("MERGED", "CLOSED"):
                obj = {
                    "_type": "ChangeMergedEvent"
                    if change["state"] == "MERGED"
                    else "ChangeAbandonedEvent",
                    "_id": "CCLE" + change["_id"],
                    "created_at": change["closed_at"],
                    # Gerrit does not tell about closed_by so here
                    # let's set None except if merged_by
                    # is set (Gerrit 3.x tells about the author of a merge)
                    "author": change.get("merged_by"),
                }
                insert_change_attributes(obj, change)
                objects.append(from_dict(data_class=Event, data=obj))

            for comment in review["messages"]:
                if comment["message"].startswith("Uploaded patch set "):
                    obj = {
                        "_type": "ChangeCommitPushedEvent",
                        "_id": comment["id"],
                        "created_at": self.convert_date_for_db(comment["date"]),
                        "author": create_ident(
                            url,
                            "%s/%s"
                            % (
                                comment["author"].get("name"),
                                comment["author"]["_account_id"],
                            ),
                        ),
                    }
                    insert_change_attributes(obj, change)
                    objects.append(from_dict(data_class=Event, data=obj))
                    continue

                # Here we apply a regexp to ensure the message contains a message
                # body. Inline comments match as well because they add in the message
                # body the string '(X comments)'.
                # Gerrit reports votes as comments, this regexp not match if
                # the message only match a vote such as Code-Review+1 w/o further comments
                if self.message_re.match(comment["message"]):
                    obj = {
                        "_type": "ChangeCommentedEvent",
                        "_id": comment["id"],
                        "created_at": self.convert_date_for_db(comment["date"]),
                        "author": create_ident(
                            url,
                            "%s/%s"
                            % (
                                comment["author"].get("name"),
                                comment["author"]["_account_id"],
                            ),
                        ),
                    }
                    insert_change_attributes(obj, change)
                    objects.append(from_dict(data_class=Event, data=obj))

                approval_match = self.approval_re.match(comment["message"])
                if approval_match:
                    obj = {
                        "_type": "ChangeReviewedEvent",
                        "_id": "approval_%s" % comment["id"],
                        "created_at": self.convert_date_for_db(comment["date"]),
                        "approval": [
                            approval_match.groupdict().get("approval").strip()
                        ],
                        "author": create_ident(
                            url,
                            "%s/%s"
                            % (
                                comment["author"].get("name"),
                                comment["author"]["_account_id"],
                            ),
                        ),
                    }
                    insert_change_attributes(obj, change)
                    objects.append(from_dict(data_class=Event, data=obj))

            return objects

        objects: List[Union[Change, Event]] = []
        for review in changes:
            try:
                objects.extend(extract_pr_objects(review))
            except Exception:
                self.log.exception("Unable to extract Review data: %s" % review)
                dumper(review, "gerrit_")
        return objects


if __name__ == "__main__":
    import argparse
    import os
    from pprint import pprint

    parser = argparse.ArgumentParser(prog="review")

    parser.add_argument("--loglevel", help="logging level", default="INFO")
    parser.add_argument("--base-url", help="A Gerrit server", required=True)
    parser.add_argument("--repository", help="The repository name", required=True)
    parser.add_argument("--id", help="The review change id", required=True)
    parser.add_argument("--output-dir", help="Store the dump in this directory")
    parser.add_argument(
        "--insecure", help="Bypass the HTTP X509 verification", action="store_true"
    )
    parser.add_argument("--login", help="Login to use to authenticate")
    parser.add_argument("--password", help="Password to use to authenticate")
    parser.add_argument("--prefix", help="Project prefix to prepend")

    args = parser.parse_args()

    # Hosts list
    # https://gerrit-review.googlesource.com gerrit
    # https://review.opendev.org zuul/zuul
    # https://softwarefactory-project.io/r software-factory/sf-config

    def _dumper(raw, prefix=None):
        pprint(raw)

    rf = ReviewesFetcher(
        args.base_url,
        args.repository,
        insecure=args.insecure,
        login=args.login,
        password=args.password,
        prefix=args.prefix,
    )
    review = rf.get("2020-01-01 00:00:00", args.id)
    objs = rf.extract_objects(review, _dumper)
    if not args.output_dir:
        pprint([review[0], objs])
    else:
        basename = "%s-%s-%s" % (
            args.base_url.replace("/", ("_")).replace(":", ""),
            args.repository.replace("/", "_"),
            args.id,
        )
        basepath = os.path.join(args.output_dir, basename)
        json.dump(review[0], open(basepath + "_raw.json", "w"), indent=2)
        json.dump(
            [change_or_event_to_dict(o) for o in objs],
            open(basepath + "_extracted.json", "w"),
            indent=2,
        )
