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

from monocle.github import pullrequest

from .common import load_change
from .common import DiffException


class TestGithubCrawler(unittest.TestCase):
    def extract_and_compare(self, name):
        input_pr, xtrd_ref = load_change(name)

        pr_fetcher = pullrequest.PRsFetcher(None, 'https://github.com', None, None)
        xtrd = pr_fetcher.extract_objects([input_pr], print)

        ddiff = DeepDiff(xtrd_ref, xtrd, ignore_order=True)
        if ddiff:
            raise DiffException(ddiff)

    def test_extract_and_compare_pr1(self):
        """
        Github crawler extracts github.com-morucci-monocle-70
        """
        self.extract_and_compare('github.com-morucci-monocle-70')

    def test_extract_and_compare_pr2(self):
        """
        Github crawler extracts github.com-wazo-platform-wazo-ansible-76
        """
        self.extract_and_compare('github.com-wazo-platform-wazo-ansible-76')
