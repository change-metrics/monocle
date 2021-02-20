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
from typing import List, Union, Optional, Callable

from monocle.db.db import (
    Change,
    Event,
    File,
    SimpleFile,
    Commit,
)
from monocle.basecrawler import BaseCrawler, RawChange, create_ident

name = "dummy_crawler"
help = "Dummy crawler to demo a crawler for a dummy code review service"


@dataclass
class DummyCrawlerArgs(object):
    updated_since: str
    loop_delay: int
    command: str
    base_url: str
    repository: str
    db: object


class DummyChangeFetcher(BaseCrawler):

    log = logging.getLogger(__name__)

    def __init__(
        self,
        base_url,
        repository_prefix,
    ):
        self.base_url = base_url
        self.repository_prefix = repository_prefix

    def get(
        self, updated_since: str, change_id: Optional[str] = None
    ) -> List[RawChange]:
        #
        # Here perform API calls to the Code Review service in order to
        # fetch the changes details (PR details).
        #
        # This function must be able to request from the API only changes
        # updated since <updated_since>. A change update occurs when a
        # comment, a status, or whatever attribute of a change is changed.
        # Usually a field such as "last_updated" is return by the API.
        #
        # This function is going to be called periodically by the caller.
        #
        # Optionaly this function should be able to request only a specific
        # change (if change_id is specified). This attribute is supposed
        # to be used in a dev or debug context of a crawler.
        #
        changes: List[RawChange] = []
        return changes

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
            change_id="org/dummyrepo/1",
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
                    author=create_ident(url, "John"),
                    committer=create_ident(url, "John"),
                    authored_at="2020-04-11T07:01:15Z",
                    committed_at="2020-04-11T07:00:15Z",
                    additions=10,
                    deletions=0,
                    title="A dummy commit",
                )
            ],
            repository_prefix="org",
            repository_fullname="org/dummyrepo",
            repository_shortname="dummyrepo",
            author=create_ident(url, "John"),
            committer=create_ident(url, "John"),
            merged_by=create_ident(url, "Zuul"),
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
            author=create_ident(url, "John"),
            repository_prefix="org",
            repository_fullname="org/dummyrepo",
            repository_shortname="dummyrepo",
            url="https://dummy.com/change/id",
            branch="dummy-feature",
            target_branch="main",
            number=1,
            change_id="org/dummyrepo/1",
            on_author=create_ident(url, "John"),
            on_created_at="2020-04-11T07:00:15Z",
            changed_files=[SimpleFile(path="README.md")],
            approval=None,
        )
        objects.extend([dummy_change, dummy_event])
        return objects


if __name__ == "__main__":
    # The main function is for dev and debug purpose only. This is where
    # you should focus first when developing a crawler. Once this is fonctional
    # (you are able to create Change and Event object from raw changes coming from
    # service API and the get function can crawler all updated changes from the
    # <updated_since> attribute) then congratulation 95% of the work is done, now
    # you can move forward by integrating your new crawler with the Runner
    # (see: crawler.py)

    import sys
    import argparse
    from pprint import pprint

    parser = argparse.ArgumentParser(prog="dummy")

    parser.add_argument("--crawler", help="run crawler", action="store_true")
    parser.add_argument(
        "--updated-since", help="stop date for the crawler (YYYY-mm-dd)"
    )
    parser.add_argument("--loglevel", help="logging level", default="INFO")
    parser.add_argument("--repository", help="The repository or regexp", required=True)
    parser.add_argument("--id", help="The change id")

    args = parser.parse_args()

    logging.basicConfig(
        level=getattr(logging, args.loglevel.upper()),
        format="%(asctime)s - %(name)s - %(threadName)s - "
        + "%(levelname)s - %(message)s",
    )

    cf = DummyChangeFetcher(
        "https://code-review.service.com/",
        args.repository,
    )
    if args.crawler and args.updated_since:
        changes = cf.get(args.updated_since, None)
    elif args.id and not args.crawler:
        changes = cf.get("", args.id)
    else:
        print("Missing arguments")
        sys.exit(-1)

    objs = cf.extract_objects(changes, lambda x, y: None)
    pprint([changes, objs])
