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


import unittest
from deepdiff import DeepDiff

from monocle.gerrit import review

from .common import load_change
from .common import DiffException


class TestGerritCrawler(unittest.TestCase):
    def extract_and_compare(self, base_url, name):
        input_review, xtrd_ref = load_change(name)

        rf = review.ReviewesFetcher(base_url, None)
        xtrd = rf.extract_objects([input_review])

        ddiff = DeepDiff(xtrd_ref, xtrd, ignore_order=True)
        if ddiff:
            raise DiffException(ddiff)

    def test_extract_and_compare_review1(self):
        """
        Gerrit crawler extracts https:__gerrit-review.googlesource.com-gerrit-246332
        """
        self.extract_and_compare(
            'https://gerrit-review.googlesource.com',
            'https:__gerrit-review.googlesource.com-gerrit-246332',
        )
