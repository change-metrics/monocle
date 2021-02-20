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
from time import sleep

from typing import Optional, List, Dict, Any, Union, Callable
from dacite import from_dict

from monocle.github.graphql import RequestTimeout
from monocle.github import application
from monocle.utils import is8601_to_dt
from monocle.db.db import (
    Change,
    Event,
    File,
    SimpleFile,
    Commit,
    change_or_event_to_dict,
)
from monocle.basecrawler import BaseCrawler, RawChange, create_ident

MAX_TRY = 3
MAX_BULK_SIZE = 100
REDUCE = 2
AUGMENT = 1.1

name = "github_crawler"
help = "Github Crawler to fetch PRs events"


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
    org: str
    repository: str
    base_url: str
    token_getter: object
    db: object


class PRsFetcher(BaseCrawler):

    log = logging.getLogger(__name__)

    def __init__(self, gql, base_url, org, repository):
        self.gql = gql
        self.size = MAX_BULK_SIZE
        self.base_url = base_url
        self.org = org
        self.repository = repository
        self.events_map = {
            "ClosedEvent": "ChangeAbandonedEvent",
            "PullRequestReview": "ChangeReviewedEvent",
            "HeadRefForcePushedEvent": "ChangeCommitForcePushedEvent",
        }

    def get_pr_query(self, include_commits=True):
        commits = """
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
        """
        force_push_event = "HEAD_REF_FORCE_PUSHED_EVENT"
        force_push_event_item = """
                ... on HeadRefForcePushedEvent {
                  id
                  createdAt
                  actor {
                    login
                  }
                }
        """
        pr_query = """
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
          commits (first: 100){
            totalCount
            %(commits)s
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
                %(force_push_event)s
                ]) {
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
                %(force_push_event_item)s
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
        """  # noqa: E501
        if include_commits and not self.gql.token_getter.can_read_commit():
            self.log.info(
                "Disable commits fetching for org %s due to unsufficent ACLs" % self.org
            )
            include_commits = False
        query_params = {
            "commits": commits if include_commits else "",
            "force_push_event": force_push_event if include_commits else "",
            "force_push_event_item": force_push_event_item if include_commits else "",
        }
        return pr_query % query_params

    def _getPage(self, kwargs, prs):
        qdata = """{
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
        }"""  # noqa: E501
        data = self.gql.query(qdata % kwargs)
        if "data" not in data:
            self.log.error("No data collected: %s" % data)
            if "message" in data and "wait a few minutes" in data["message"]:
                self.log.info("sleeping 2 mn")
                sleep(120)
            else:
                self.log.info("sleeping 20 s")
                sleep(20)
            return None
        if not kwargs["total_prs_count"]:
            kwargs["total_prs_count"] = data["data"]["repository"]["pullRequests"][
                "totalCount"
            ]
            self.log.info(
                "Total PRs: %s but will fetch until we reached a PR "
                "updated at date < %s"
                % (kwargs["total_prs_count"], kwargs["updated_since"])
            )
            if kwargs["total_prs_count"] == 0:
                return False
        edges = data["data"]["repository"]["pullRequests"]["edges"]
        for pr in edges:
            prs.append(pr["node"])
        # We sort to mitigate this
        # https://github.community/t5/GitHub-API-Development-and/apiv4-pullrequests-listing-broken-ordering/m-p/59439#M4968
        oldest_update = sorted(
            [is8601_to_dt(pr["node"]["updatedAt"]) for pr in edges], reverse=True
        )[-1]
        logging.info("page oldest updated at date is %s" % oldest_update)
        if oldest_update < kwargs["updated_since"]:
            # The crawler reached a page where the oldest updated PR
            # is oldest than the configured limit
            return False
        pageInfo = data["data"]["repository"]["pullRequests"]["pageInfo"]
        if pageInfo["hasNextPage"]:
            kwargs["after"] = 'after: "%s"' % pageInfo["endCursor"]
            return True
        else:
            return False

    def get(
        self, updated_since: str, change_id: Optional[str] = None
    ) -> List[RawChange]:
        prs: List[RawChange] = []
        updated_since = is8601_to_dt(updated_since)
        get_commits = True
        kwargs = {
            "pr_query": self.get_pr_query(include_commits=get_commits),
            "org": self.org,
            "repository": self.repository,
            "updated_since": updated_since,
            "after": "",
            "total_prs_count": 0,
            "size": self.size,
        }
        one = 0
        while True:
            self.log.info(
                "Running request %s"
                % dict([(k, v) for k, v in kwargs.items() if k != "pr_query"])
            )
            try:
                hnp = self._getPage(kwargs, prs)
                if kwargs["size"] == 1:
                    self.log.debug("Getting this PR, with page size 1: %s" % prs[0])
                kwargs["size"] = min(MAX_BULK_SIZE, int(kwargs["size"] * AUGMENT) + 1)
                one = 0
                if not get_commits:
                    self.log.info("Will get full commits on next query.")
                    kwargs["pr_query"] = self.get_pr_query(include_commits=get_commits)
                    get_commits = True
            except RequestTimeout:
                kwargs["size"] = max(1, kwargs["size"] // REDUCE)
                if kwargs["size"] == 1:
                    one += 1
                    if one == MAX_TRY - 1:
                        self.log.info(
                            "%d timeouts in a raw for one pr, retrying without commits."
                            % (MAX_TRY - 1)
                        )
                        get_commits = False
                        kwargs["pr_query"] = self.get_pr_query(
                            include_commits=get_commits
                        )
                    elif one >= MAX_TRY:
                        self.log.info(
                            "%d timeouts in a raw for one pr, giving up." % MAX_TRY
                        )
                        raise
                continue
            self.log.info("%s PRs fetched" % len(prs))
            if hnp is False:
                break
        return prs

    def get_one(self, org, repository, number):
        def _dumper(raw, prefix=None):
            pprint(raw)

        qdata = """{
          repository(owner: "%(org)s", name:"%(repository)s") {
            pullRequest(number: %(number)s) {
              %(pr_query)s
            }
          }
        }
        """
        kwargs = {
            "pr_query": self.get_pr_query(),
            "org": org,
            "repository": repository,
            "number": number,
        }
        raw = self.gql.query(qdata % kwargs)["data"]["repository"]["pullRequest"]
        return (raw, self.extract_objects([raw], _dumper))

    def extract_objects(
        self, changes: List[RawChange], dumper: Callable
    ) -> List[Union[Change, Event]]:
        def get_login(data: Union[None, Dict[str, str]]) -> str:
            if data and "login" in data and data["login"]:
                return data["login"]
            return "ghost"

        def timedelta(start, end):
            format = "%Y-%m-%dT%H:%M:%SZ"
            start = datetime.strptime(start, format)
            end = datetime.strptime(end, format)
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
                    "changed_files": list(
                        map(lambda x: SimpleFile(path=x.path), change["changed_files"])
                    ),
                }
            )

        def extract_pr_objects(pr: Dict) -> List[Union[Change, Event]]:
            if "commits" not in pr:
                pr["commits"] = {"edges": []}
            if "edges" not in pr["commits"]:
                pr["commits"]["edges"] = []

            objects: List[Union[Change, Event]] = []

            change: Dict[str, Any] = {}
            change["_type"] = "Change"
            change["_id"] = pr["id"]
            change["draft"] = pr["isDraft"]
            change["number"] = pr["number"]
            change["repository_prefix"] = get_login(pr["repository"]["owner"])
            change["repository_fullname"] = "%s/%s" % (
                get_login(pr["repository"]["owner"]),
                pr["repository"]["name"],
            )
            change["repository_shortname"] = pr["repository"]["name"]
            change["change_id"] = "%s@%s" % (
                change["repository_fullname"].replace("/", "@"),
                change["number"],
            )
            url = "%s/%s/pull/%s" % (
                self.base_url,
                change["repository_fullname"],
                change["number"],
            )
            change["url"] = url
            change["author"] = create_ident(url, get_login(pr["author"]))
            change["branch"] = pr["headRefName"]
            change["target_branch"] = pr["baseRefName"]
            change["self_merged"] = None
            change["title"] = pr["title"]
            change["text"] = pr["bodyText"]
            change["additions"] = pr["additions"]
            change["deletions"] = pr["deletions"]
            change["approval"] = (
                [pr["reviewDecision"]] if pr["reviewDecision"] else None
            )
            change["changed_files_count"] = pr["changedFiles"]
            change["changed_files"] = []
            if pr["files"] and "edges" in pr["files"]:
                for fd in pr["files"]["edges"]:
                    change["changed_files"].append(
                        File(
                            additions=fd["node"]["additions"],
                            deletions=fd["node"]["deletions"],
                            path=fd["node"]["path"],
                        )
                    )
            if pr["mergedBy"]:
                change["merged_by"] = create_ident(url, get_login(pr["mergedBy"]))
                change["self_merged"] = change["merged_by"].uid == change["author"].uid
            else:
                change["merged_by"] = None
            change["updated_at"] = pr["updatedAt"]
            change["created_at"] = pr["createdAt"]
            change["merged_at"] = pr["mergedAt"]
            change["closed_at"] = pr["closedAt"]
            # A closed PR is an unmerged closed PR
            change["state"] = pr["state"]
            if pr["state"] in ("CLOSED", "MERGED"):
                change["duration"] = timedelta(
                    change.get("closed_at") or change.get("updated_at"),
                    change["created_at"],
                )
            change["mergeable"] = pr["mergeable"]
            change["labels"] = list(
                map(lambda n: n["node"]["name"], pr["labels"]["edges"])
            )
            change["assignees"] = list(
                map(
                    lambda n: create_ident(url, get_login(n["node"])),
                    pr["assignees"]["edges"],
                )
            )
            change["commits"] = []
            commits = [c for c in pr["commits"]["edges"] if c["node"]]
            for commit in commits:
                _commit = commit["node"]["commit"]
                obj = {
                    "sha": _commit["oid"],
                    "authored_at": _commit["authoredDate"],
                    "committed_at": _commit["committedDate"],
                    "additions": _commit["additions"],
                    "deletions": _commit["deletions"],
                    "title": _commit["message"],
                }
                for k in ("author", "committer"):
                    obj[k] = create_ident(url, get_login(_commit[k].get("user")))
                change["commits"].append(from_dict(data_class=Commit, data=obj))

            if pr["commits"].get("totalCount") is not None:
                change["commit_count"] = int(pr["commits"].get("totalCount"))
            else:
                change["commit_count"] = len(commits)

            objects.append(from_dict(data_class=Change, data=change))

            obj = {
                "_type": "ChangeCreatedEvent",
                "_id": "CCE" + change["_id"],
                "created_at": change["created_at"],
                "author": change["author"],
            }
            insert_change_attributes(obj, change)
            objects.append(from_dict(data_class=Event, data=obj))

            for comment in pr["comments"]["edges"]:
                _comment = comment["node"]
                if not _comment:
                    continue
                obj = {
                    "_type": "ChangeCommentedEvent",
                    "_id": _comment["id"],
                    "created_at": _comment["createdAt"],
                    "author": create_ident(url, get_login(_comment["author"])),
                }
                insert_change_attributes(obj, change)
                objects.append(from_dict(data_class=Event, data=obj))

            for timelineitem in pr["timelineItems"]["edges"]:
                _timelineitem = timelineitem["node"]
                _author = _timelineitem.get("actor", {}) or _timelineitem.get(
                    "author", {}
                )
                if not _author:
                    _author = {"login": "ghost"}
                obj = {
                    "_type": self.events_map[_timelineitem["__typename"]],
                    "_id": _timelineitem["id"],
                    "created_at": _timelineitem["createdAt"],
                    "author": create_ident(url, _author.get("login")),
                }
                insert_change_attributes(obj, change)
                if "state" in _timelineitem:
                    obj["approval"] = [_timelineitem["state"]]
                if obj["_type"] == "ChangeAbandonedEvent":
                    if change["state"] == "MERGED":
                        obj["_type"] = "ChangeMergedEvent"
                        obj["author"] = change["merged_by"]
                objects.append(from_dict(data_class=Event, data=obj))

            # Here we don't use the PullRequestCommit timeline event because
            # it does not provide more data than the current list of commits
            # of the pull request
            for commit in commits:
                _commit = commit["node"]["commit"]
                obj = {
                    "_type": "ChangeCommitPushedEvent",
                    "_id": _commit["oid"],
                    # Seems the first PR's commit get a date with None value
                    # So make sense to set the same created_at date as the
                    # change
                    "author": create_ident(
                        url, get_login(_commit["committer"].get("user"))
                    ),
                    "created_at": _commit.get("pushedDate") or change["created_at"],
                }
                insert_change_attributes(obj, change)
                objects.append(from_dict(data_class=Event, data=obj))

            return objects

        objects: List[Union[Change, Event]] = []
        for pr in changes:
            try:
                objects.extend(extract_pr_objects(pr))
            except Exception:
                self.log.exception("Unable to extract PR")
                dumper(pr, "github_")
        return objects


class TokenGetter:
    def __init__(
        self,
        org,
        token: Optional[str] = None,
        app: Optional[application.MonocleGithubApp] = None,
    ) -> None:
        self.org = org
        self.token = token
        self.app = app

    def get_token(self):
        if self.token:
            return self.token, {"contents": "read"}
        elif self.app:
            return self.app.get_token(self.org), self.app.get_permissions(self.org)
        else:
            raise RuntimeError("TokenGetter need a Token or a GithubApp")

    def can_read_commit(self) -> bool:
        return "contents" in self.get_token()[1].keys()


if __name__ == "__main__":
    import os
    import json
    import argparse
    from monocle.github import graphql

    parser = argparse.ArgumentParser(prog="pullrequest")

    parser.add_argument("--crawler", help="run crawler", action="store_true")
    parser.add_argument(
        "--updated-since", help="stop date for the crawler (YYYY-mm-dd)"
    )
    parser.add_argument("--loglevel", help="logging level", default="INFO")
    parser.add_argument("--token", help="A Github personal token")
    parser.add_argument("--org", help="A Github organization", required=True)
    parser.add_argument("--app-id", help="The Github app-id")
    parser.add_argument("--app-key-path", help="A Github app key path")
    parser.add_argument(
        "--repository", help="The repository within the organization", required=True
    )
    parser.add_argument("--id", help="The pull request id")
    parser.add_argument(
        "--output-dir",
        help="Store the dump in this directory",
    )

    args = parser.parse_args()

    logging.basicConfig(
        level=getattr(logging, args.loglevel.upper()),
        format="%(asctime)s - %(name)s - %(threadName)s - "
        + "%(levelname)s - %(message)s",
    )

    app = None
    if args.app_id and args.app_key_path:
        app = application.get_app(args.app_id, args.app_key_path)
    tg = TokenGetter(args.org, args.token, app)

    prf = PRsFetcher(
        graphql.GithubGraphQLQuery(token_getter=tg),
        "https://github.com",
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
            json.dump(data[0], open(basepath + "_raw.json", "w"), indent=2)
            json.dump(
                [change_or_event_to_dict(o) for o in data[1]],
                open(basepath + "_extracted.json", "w"),
                indent=2,
            )
