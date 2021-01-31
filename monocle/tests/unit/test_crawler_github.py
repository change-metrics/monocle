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

from pathlib import Path
import unittest
from deepdiff import DeepDiff

from typing import List, Union

from monocle.db.db import Change, Event, change_or_event_to_dict
from monocle.github import pullrequest

from .common import load_change
from .common import DiffException
from .common import DATASETS
from .common import load_dataset


class TestGithubCrawler(unittest.TestCase):
    def setUp(self):
        logging.basicConfig(
            level=logging.DEBUG,
            format="%(asctime)s - %(name)s - " + "%(levelname)s - %(message)s",
        )
        self.log = logging.getLogger(__name__)
        # log to stderr
        self.log.addHandler(logging.StreamHandler())

    def extract_and_compare(self, name: str) -> None:
        input_pr, xtrd_ref = load_change(name)

        pr_fetcher = pullrequest.PRsFetcher(None, "https://github.com", None, None)
        xtrd: List[Union[Change, Event]] = pr_fetcher.extract_objects(
            [input_pr], lambda x, y: None
        )

        ddiff = DeepDiff(
            xtrd_ref,
            [change_or_event_to_dict(x) for x in xtrd],
            ignore_order=True,
        )
        if ddiff:
            raise DiffException(ddiff)

    def test_extract_and_compare_pr1(self):
        """
        Github crawler extracts github.com-morucci-monocle-70
        """
        self.extract_and_compare("github.com-morucci-monocle-70")

    def test_extract_and_compare_pr2(self):
        """
        Github crawler extracts github.com-wazo-platform-wazo-ansible-76
        """
        self.extract_and_compare("github.com-wazo-platform-wazo-ansible-76")

    def test_load_buggy(self):
        """
        Github crawler extracts buggy prs
        """
        pr_fetcher = pullrequest.PRsFetcher(None, "https://github.com", None, None)
        datasets_dir = Path(DATASETS)
        for fn in datasets_dir.glob("github_*.json"):
            self.log.info("Loading buggy PR from %s " % fn)
            dataset = load_dataset(fn)
            xtrd = pr_fetcher.extract_objects([dataset], lambda x, y: None)
            self.assertNotEqual(xtrd, [])
