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

from pathlib import Path
import unittest
from deepdiff import DeepDiff

from monocle.gerrit import review

from .common import load_change
from .common import DiffException
from .common import DATASETS
from .common import load_dataset


class TestGerritCrawler(unittest.TestCase):
    def extract_and_compare(self, base_url, name):
        input_review, xtrd_ref = load_change(name)

        rf = review.ReviewesFetcher(base_url, None)
        xtrd = rf.extract_objects([input_review], None)

        ddiff = DeepDiff(xtrd_ref, xtrd, ignore_order=True)
        if ddiff:
            raise DiffException(ddiff)

    def test_extract_and_compare_review1(self):
        """
        Gerrit crawler extracts https:__gerrit-review.googlesource.com-gerrit-246332
        """
        self.extract_and_compare(
            "https://gerrit-review.googlesource.com",
            "https:__gerrit-review.googlesource.com-gerrit-246332",
        )

    def test_load_buggy(self):
        """
        Gerrit crawler extracts buggy reviews
        """
        rf = review.ReviewesFetcher("https://gerrit.org", None)
        datasets_dir = Path(DATASETS)
        for fn in datasets_dir.glob("gerrit_*.json"):
            dataset = load_dataset(fn)
            xtrd = rf.extract_objects([dataset], None)
            self.assertNotEqual(xtrd, [])
