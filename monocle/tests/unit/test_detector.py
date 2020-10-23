# Monocle.
# Copyright (C) 2020 Monocle authors
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


import unittest

from deepdiff import DeepDiff
from .common import DiffException

from monocle.utils import Detector


class TestDetector(unittest.TestCase):
    def test_generic_issue_tracker_links(self):
        """
        Test Detector tracker links: generic
        """
        d = Detector()

        change = {
            "title": "A text PR",
            "text": "This PR fix the issue https://bugs.demo.net/1749",
        }

        # Test #1 style
        d.issue_tracker_extract_links(change)
        self.assertTrue(change["has_issue_tracker_links"])
        expected = [["https://bugs.demo.net/1749", "https://bugs.demo.net/1749"]]
        ddiff = DeepDiff(change["issue_tracker_links"], expected)
        if ddiff:
            raise DiffException(ddiff)

    def test_github_issue_tracker_links(self):
        """
        Test Detector tracker links: GitHub.com
        """
        d = Detector()

        change = {
            "repository_prefix": "change-metrics",
            "repository_shortname": "monocle",
            "title": "A text PR",
            "text": "This PR fix the issue #1",
        }

        # Test #1 style
        d.issue_tracker_extract_links(change)
        self.assertTrue(change["has_issue_tracker_links"])
        expected = [["#1", "https://github.com/change-metrics/monocle/issues/1"]]
        ddiff = DeepDiff(change["issue_tracker_links"], expected)
        if ddiff:
            raise DiffException(ddiff)

        change["title"] = "A PR that fix #1"
        change["text"] = "But also fix #2\n and #31"

        d.issue_tracker_extract_links(change)
        self.assertTrue(change["has_issue_tracker_links"])
        expected = [
            ["#1", "https://github.com/change-metrics/monocle/issues/1"],
            ["#2", "https://github.com/change-metrics/monocle/issues/2"],
            ["#31", "https://github.com/change-metrics/monocle/issues/31"],
        ]
        ddiff = DeepDiff(change["issue_tracker_links"], expected)
        if ddiff:
            raise DiffException(ddiff)

        # Test full issue url style
        change["title"] = "A test PR"
        change["text"] = "This fix https://github.com/change-metrics/monocle/issues/1"
        d.issue_tracker_extract_links(change)
        expected = [
            [
                "https://github.com/change-metrics/monocle/issues/1",
                "https://github.com/change-metrics/monocle/issues/1",
            ]
        ]
        ddiff = DeepDiff(change["issue_tracker_links"], expected)
        if ddiff:
            raise DiffException(ddiff)

        # Test full issue url style
        change["text"] = "This fix change-metrics/cross-repo#12"
        d.issue_tracker_extract_links(change)
        expected = [
            [
                "change-metrics/cross-repo#12",
                "https://github.com/change-metrics/cross-repo/issues/12",
            ]
        ]
        ddiff = DeepDiff(change["issue_tracker_links"], expected)
        if ddiff:
            raise DiffException(ddiff)

        # Test GH-42 style
        change["text"] = "This fix GH-12"
        d.issue_tracker_extract_links(change)
        expected = [["GH-12", "https://github.com/change-metrics/monocle/issues/12"]]
        ddiff = DeepDiff(change["issue_tracker_links"], expected)
        if ddiff:
            raise DiffException(ddiff)

    def test_altassian_net_issue_tracker_links(self):
        """
        Test Detector tracker links: altassian.net
        """
        d = Detector()

        change = {
            "repository_prefix": "change-metrics",
            "repository_shortname": "monocle",
            "title": "A text PR",
            "text": "This PR fix the issue https://yoyo.atlassian.net/browse/YOYO-1749",
        }

        # Test #1 style
        d.issue_tracker_extract_links(change)
        self.assertTrue(change["has_issue_tracker_links"])
        expected = [
            [
                "https://yoyo.atlassian.net/browse/YOYO-1749",
                "https://yoyo.atlassian.net/browse/YOYO-1749",
            ]
        ]
        ddiff = DeepDiff(change["issue_tracker_links"], expected)
        if ddiff:
            raise DiffException(ddiff)
