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
from dataclasses import dataclass
from time import sleep
from typing import Callable, List, Optional, Union

from monocle.basecrawler import BaseCrawler, RawChange
from monocle.db.db import (
    Change,
    Commit,
    Event,
    File,
    SimpleFile,
    change_or_event_to_dict,
)
from monocle.gitlab.graphql import GitlabGraphQLQuery, RequestTimeout
from monocle.ident import IdentsConfig, create_ident
from monocle.utils import is8601_to_dt

name = "gitlab_crawler"
help = "GitLab Crawler to fetch MRs events"

MAX_TRY = 3
MAX_BULK_SIZE = 100
REDUCE = 2
AUGMENT = 1.1


class TokenGetter:
    def __init__(
        self,
        url,
        group,
        token: Optional[str] = None,
    ) -> None:
        self.url = url
        self.group = group
        self.token = token

    def get_token(self):
        if self.token:
            return self.token, {"contents": "read"}
        else:
            raise RuntimeError("TokenGetter need a Token")

    def can_read_commit(self) -> bool:
        return "contents" in self.get_token()[1].keys()


@dataclass
class GitLabCrawlerArgs(object):
    updated_since: str
    loop_delay: int
    command: str
    url: str
    group: str
    repository: str
    db: object
    idents_config: IdentsConfig


class MRsFetcher(BaseCrawler):

    log = logging.getLogger(__name__)

    def __init__(
        self,
        gql: GitlabGraphQLQuery,
        url: str,
        group: str,
        idents_config: IdentsConfig,
        repository: str,
    ):
        self.gql = gql
        self.size = MAX_BULK_SIZE
        self.url = url
        self.group = group
        self.repository = repository
        self.idents_config = idents_config

    def get_mr_query(self, include_commits=True):
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
        mr_query = """
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
                f"Disable commits fetching for group {self.group} due to unsufficent ACLs"
            )
            include_commits = False
        query_params = {
            "commits": commits if include_commits else "",
            "force_push_event": force_push_event if include_commits else "",
            "force_push_event_item": force_push_event_item if include_commits else "",
        }
        return mr_query % query_params

    def _getPage(self, kwargs, mrs):
        qdata = """{
        repository(owner: "%(group)s", name:"%(repository)s") {
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
                %(mr_query)s
              }
            }
          }
        }
      }"""  # noqa: E501
        data = self.gql.query(qdata % kwargs)
        if "data" not in data:
            self.log.error("No data collected: %s" % data)
            if "message" in data and "wait a few minutes" in data["message"]:
                self.log.info("sleeping 2 min")
                sleep(120)
            else:
                self.log.info("sleeping 20 s")
                sleep(20)
            return None
        if not kwargs["total_mrs_count"]:
            kwargs["total_mrs_count"] = data["data"]["repository"]["pullRequests"][
                "totalCount"
            ]
            self.log.info(
                f'Total MRs: {kwargs["total_mrs_count"]} but will fetch until we reached a MR '
                f'updated at date < {kwargs["updated_since"]}'
            )
            if kwargs["total_mrs_count"] == 0:
                return False
        edges = data["data"]["repository"]["pullRequests"]["edges"]
        for mr in edges:
            mrs.append(mr["node"])
        oldest_update = sorted(
            [is8601_to_dt(mr["node"]["updatedAt"]) for mr in edges],
        )[0]
        logging.info("page oldest updated at date is %s" % oldest_update)
        if oldest_update < kwargs["updated_since"]:
            # The crawler reached a page where the oldest updated MR
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
        mrs: List[RawChange] = []
        updated_since = is8601_to_dt(updated_since)
        get_commits = True
        kwargs = {
            "mr_query": self.get_mr_query(include_commits=get_commits),
            "url": self.url,
            "group": self.group,
            "repository": self.repository,
            "updated_since": updated_since,
            "after": "",
            "total_mrs_count": 0,
            "size": self.size,
        }
        one = 0
        while True:
            self.log.info(
                "Running request %s"
                % dict([(k, v) for k, v in kwargs.items() if k != "mr_query"])
            )
            try:
                hnp = self._getPage(kwargs, mrs)
                if kwargs["size"] == 1:
                    self.log.debug(f"Getting this MR, with page size 1: {mrs[0]}")
                kwargs["size"] = min(MAX_BULK_SIZE, int(kwargs["size"] * AUGMENT) + 1)
                one = 0
                if not get_commits:
                    self.log.info("Will get full commits on next query.")
                    kwargs["mr_query"] = self.get_mr_query(include_commits=get_commits)
                    get_commits = True
            except RequestTimeout:
                kwargs["size"] = max(1, kwargs["size"] // REDUCE)
                if kwargs["size"] == 1:
                    one += 1
                    if one == MAX_TRY - 1:
                        self.log.info(
                            "%d timeouts in a raw for one mr, retrying without commits."
                            % (MAX_TRY - 1)
                        )
                        get_commits = False
                        kwargs["mr_query"] = self.get_mr_query(
                            include_commits=get_commits
                        )
                    elif one >= MAX_TRY:
                        self.log.info(
                            "%d timeouts in a raw for one mr, giving up." % MAX_TRY
                        )
                        raise
                continue
            self.log.info(f"{len(mrs)} MRs fetched")
            if hnp is False:
                break
        return mrs

    def get_one(self, url, group, repository, number):
        def _dumper(raw, prefix=None):
            pprint(raw)

        qdata = """{
          repository(owner: "%(group)s", name:"%(repository)s") {
            mergeRequest(number: %(number)s) {
              %(mr_query)s
            }
          }
        }
        """
        kwargs = {
            "mr_query": self.get_mr_query(),
            "url": url,
            "group": group,
            "repository": repository,
            "number": number,
        }
        raw = self.gql.query(qdata % kwargs)["data"]["repository"]["pullRequest"]
        return (raw, self.extract_objects([raw], _dumper))

    def extract_objects(
        self,
        changes: List[RawChange],
        dumper: Callable,
    ) -> List[Union[Change, Event]]:
        #
        # Here convert raw changes to the Change or Event dataclasses.
        #
        # These objects will be indexed into the DB by the caller.
        # It is mandatory that an object keep the same id during its live;
        # for instance each time the Pull Request X is extracted by this function
        # (because it has been updated) its computed id must remane the same.
        # Then the objects will updated in the DB and will not concidered as a
        # new object.
        #
        # Change and Event's attributes type must be follow (see: db.py)
        #
        objects: List[Union[Change, Event]] = []
        url = "https://dummy.com/change/id"
        dummy_change = Change(
            _id="123",
            _type="Change",
            number=1,
            change_id="group/dummyrepo/1",
            title="A dummy change",
            text="Body text",
            url=url,
            commit_count=1,
            additions=10,
            deletions=0,
            changed_files_count=1,
            changed_files=[File(additions=10, deletions=0, path="README.md")],
            commits=[
                Commit(
                    sha="12345",
                    author=create_ident(url, "John", []),
                    committer=create_ident(url, "John", []),
                    authored_at="2020-04-11T07:01:15Z",
                    committed_at="2020-04-11T07:00:15Z",
                    additions=10,
                    deletions=0,
                    title="A dummy commit",
                )
            ],
            repository_prefix="group",
            repository_fullname="group/dummyrepo",
            repository_shortname="dummyrepo",
            author=create_ident(url, "John", []),
            committer=create_ident(url, "John", []),
            merged_by=create_ident(url, "Zuul", []),
            branch="dummy-feature",
            target_branch="main",
            created_at="2020-04-11T07:00:15Z",
            merged_at="2020-04-12T07:00:15Z",
            updated_at="2020-04-12T07:00:15Z",
            closed_at="2020-04-12T07:00:15Z",
            state="MERGED",
            duration=3600,
            mergeable=None,
            labels=None,
            assignees=None,
            approval=None,
            draft=None,
            self_merged=False,
        )
        dummy_event = Event(
            _id="cce_1",
            _type="ChangeCreatedEvent",
            created_at="2020-04-11T07:00:15Z",
            author=create_ident(url, "John", []),
            repository_prefix="group",
            repository_fullname="group/dummyrepo",
            repository_shortname="dummyrepo",
            url="https://dummy.com/change/id",
            branch="dummy-feature",
            target_branch="main",
            number=1,
            change_id="group/dummyrepo/1",
            on_author=create_ident(url, "John", []),
            on_created_at="2020-04-11T07:00:15Z",
            changed_files=[SimpleFile(path="README.md")],
            approval=None,
        )
        objects.extend([dummy_change, dummy_event])
        return objects


if __name__ == "__main__":
    import argparse
    import json
    import os
    import sys
    from pprint import pprint

    from monocle.gitlab import graphql

    parser = argparse.ArgumentParser(prog="mergerequest")

    parser.add_argument("--crawler", help="run crawler", action="store_true")
    parser.add_argument(
        "--updated-since", help="stop date for the crawler (YYYY-mm-dd)"
    )
    parser.add_argument("--loglevel", help="logging level", default="INFO")
    parser.add_argument(
        "--url",
        help="GitLab service url",
        default="https://gitlab.com",
    )
    parser.add_argument(
        "--group",
        help="GitLab group",
        required=True,
    )
    parser.add_argument("--repository", help="The repository or regexp", required=True)
    parser.add_argument("--id", help="The merge request id")

    args = parser.parse_args()

    logging.basicConfig(
        level=getattr(logging, args.loglevel.upper()),
        format="%(asctime)s - %(name)s - %(threadName)s - "
        + "%(levelname)s - %(message)s",
    )

    tg = TokenGetter(args.url, args.group, args.token)

    mrf = MRsFetcher(
        graphql.GitlabGraphQLQuery(token_getter=tg),
        url=args.url,
        group=args.group,
        idents_config=[],
        repository=args.repository,
    )
    if args.crawler:
        if not args.updated_since:
            print("please provide --update-since arg")
            sys.exit(1)
        mrs = mrf.get(args.updated_since)
        pprint(mrs)
    else:
        if not args.id:
            print("please provide --id arg")
            sys.exit(1)
        data = mrf.get_one(args.url, args.group, args.repository, args.id)
        if not args.output_dir:
            pprint(data)
        else:
            basename = f"{args.url}-{args.group}-{args.repository}-{args.id}"
            basepath = os.path.join(args.output_dir, basename)
            json.dump(data[0], open(basepath + "_raw.json", "w"), indent=2)
            json.dump(
                [change_or_event_to_dict(o) for o in data[1]],
                open(basepath + "_extracted.json", "w"),
                indent=2,
            )
