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
import unittest
from datetime import datetime

from deepdiff import DeepDiff

from .common import index_dataset
from .common import DiffException
from .common import get_db_cnx

from monocle.db.db import UnknownQueryException
from monocle.db import queries
from monocle.utils import set_params
from monocle.config import ProjectDefinition
from monocle.task_data import OrphanTaskDataForEL, TaskData


class TestQueries(unittest.TestCase):

    index = "monocle-unittest"
    datasets = [
        "objects/unit_repo1.json",
        "objects/unit_repo2.json",
    ]

    otds = [
        OrphanTaskDataForEL(
            _id="https://bugtracker.domain.dom/123",
            task_data=TaskData(
                crawler_name="mycrawler",
                updated_at=datetime.strptime(
                    "2020-01-01T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"
                ),
                change_url="https://tests.com/unit/repo1/pull/1",
                ttype=["BUG", "CLIENT_IMPACT"],
                tid="123",
                url="https://bugtracker.domain.dom/123",
                title="It does not work",
                priority="HIGH",
            ),
        ),
        OrphanTaskDataForEL(
            _id="https://bugtracker.domain.dom/124",
            task_data=TaskData(
                crawler_name="mycrawler",
                updated_at=datetime.strptime(
                    "2020-01-02T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"
                ),
                change_url="https://tests.com/unit/repo1/pull/1",
                ttype=["FutureFeature"],
                tid="124",
                url="https://bugtracker.domain.dom/124",
                title="It does not work",
                priority="MEDIUM",
            ),
        ),
        OrphanTaskDataForEL(
            _id="https://bugtracker.domain.dom/125",
            task_data=TaskData(
                crawler_name="mycrawler",
                updated_at=datetime.strptime(
                    "2020-01-03T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"
                ),
                change_url="https://tests.com/unit/repo2/pull/2",
                ttype=["BUG", "DOC"],
                tid="125",
                url="https://bugtracker.domain.dom/125",
                title="It does not work",
                priority="LOW",
            ),
        ),
    ]

    @classmethod
    def setUpClass(cls):
        logging.basicConfig(
            level=logging.DEBUG,
            format="%(asctime)s - %(name)s - " + "%(levelname)s - %(message)s",
        )
        log = logging.getLogger(__name__)
        # log to stderr
        log.addHandler(logging.StreamHandler())
        cls.eldb = get_db_cnx(cls.index, "monocle.test.")
        for dataset in cls.datasets:
            index_dataset(cls.eldb, dataset)
        cls.eldb.update_task_data(cls.otds)
        cls.eldb.update_change_and_events_with_orphan_tds(
            {
                "https://tests.com/unit/repo1/pull/1": ["c1", "c1_e2"],
                "https://tests.com/unit/repo2/pull/2": ["c2"],
                "https://tests.com/unit/repo2/pull/3": ["c3"],
            }
        )

    @classmethod
    def tearDownClass(cls):
        cls.eldb.es.indices.delete(index=cls.eldb.prefix + cls.index)

    def test_unknown_query(self):
        """
        Test unknown query exception
        """
        params = set_params({})
        self.assertRaises(
            UnknownQueryException,
            self.eldb.run_named_query,
            "unknown",
            "unit/repo1",
            params,
        )

    def test_all_queries(self):
        """
        Test all public queries
        """
        failing = []
        for query in queries.public_queries:
            params = set_params({})
            ret = self.eldb.run_named_query(query, "unit/repo1", params)
            if (
                not isinstance(ret, dict)
                and not isinstance(ret, list)
                and not isinstance(ret, tuple)
                and not isinstance(ret, int)
            ):
                failing.append((query, ret))
        self.assertEqual(failing, [])

    def test_scan(self):
        """
        Test internal query: _scan
        """
        params = set_params({})
        ret = queries._scan(self.eldb.es, self.eldb.index, "unit/repo1", params)
        ids = [obj["id"] for obj in ret]
        expected = ["c1_e1", "c1_e2", "c1_e3", "c1_e4", "c1_e5"]
        self.assertCountEqual(ids, expected)

    def test_first_created_event(self):
        """
        Test internal query: _first_created_event
        """
        params = set_params({})
        ret = queries._first_created_event(
            self.eldb.es, self.eldb.index, "unit/repo1", params
        )
        self.assertEqual(ret, "2020-01-01T00:00:00Z")

    def test_events_top(self):
        """
        Test internal query: _events_top
        """
        params = set_params({})
        ret = queries._events_top(
            self.eldb.es, self.eldb.index, "unit/repo1", "type", params
        )
        expected = {
            "count_avg": 1.25,
            "count_median": 1.0,
            "items": [
                {"doc_count": 2, "key": "ChangeReviewedEvent"},
                {"doc_count": 1, "key": "ChangeCommentedEvent"},
                {"doc_count": 1, "key": "ChangeCreatedEvent"},
                {"doc_count": 1, "key": "ChangeMergedEvent"},
            ],
            "total": 4,
            "total_hits": 5,
        }
        ddiff = DeepDiff(ret, expected)
        if ddiff:
            raise DiffException(ddiff)

    def test_count_events(self):
        """
        Test query: count_events
        """
        params = set_params({})
        ret = self.eldb.run_named_query("count_events", "unit/repo1", params)
        self.assertEqual(ret, 5)

    def test_count_authors(self):
        """
        Test query: count_authors
        """
        params = set_params({})
        ret = self.eldb.run_named_query("count_authors", "unit/repo1", params)
        self.assertEqual(ret, 2)

        params = set_params({"type": "ChangeCreatedEvent"})
        ret = self.eldb.run_named_query("count_authors", "unit/repo1", params)
        self.assertEqual(ret, 1)

    def test_events_histo(self):
        """
        Test query: events_histo
        """
        params = set_params({"gte": "2020-01-01", "lte": "2020-01-02"})
        ret = self.eldb.run_named_query("events_histo", "unit/repo1", params)
        expected = (
            [
                {"doc_count": 4, "key": 1577836800000, "key_as_string": "2020-01-01"},
                {"doc_count": 1, "key": 1577923200000, "key_as_string": "2020-01-02"},
            ],
            2.5,
        )
        ddiff = DeepDiff(ret, expected)
        if ddiff:
            raise DiffException(ddiff)

    def test_authors_histo(self):
        """
        Test query: authors_histo
        """
        params = set_params({"gte": "2020-01-01", "lte": "2020-01-02"})
        ret = self.eldb.run_named_query("authors_histo", "unit/repo1", params)
        expected = {
            "avg_authors": 1.5,
            "buckets": [
                {
                    "authors": ["jane", "john"],
                    "doc_count": 2,
                    "key": 1577836800000,
                    "key_as_string": "2020-01-01",
                },
                {
                    "authors": ["jane"],
                    "doc_count": 1,
                    "key": 1577923200000,
                    "key_as_string": "2020-01-02",
                },
            ],
            "total_authors": 2,
        }
        ddiff = DeepDiff(ret, expected)
        if ddiff:
            raise DiffException(ddiff)

    def test_events_top_authors(self):
        """
        Test query: events_top_authors
        """
        params = set_params({})
        ret = self.eldb.run_named_query("events_top_authors", "unit/repo1", params)
        expected = {
            "count_avg": 2.5,
            "count_median": 2.5,
            "items": [{"doc_count": 3, "key": "jane"}, {"doc_count": 2, "key": "john"}],
            "total": 2,
            "total_hits": 5,
        }
        ddiff = DeepDiff(ret, expected)
        if ddiff:
            raise DiffException(ddiff)

    def test_repos_top_merged(self):
        """
        Test query: repos_top_merged
        """
        params = set_params({"state": "MERGED"})
        ret = self.eldb.run_named_query("repos_top", "unit/repo[12]", params)
        expected = {
            "items": [
                {"key": "unit/repo2", "doc_count": 2},
                {"key": "unit/repo1", "doc_count": 1},
            ],
            "count_avg": 1.5,
            "count_median": 1.5,
            "total": 2,
            "total_hits": 3,
        }
        ddiff = DeepDiff(ret, expected)
        if ddiff:
            raise DiffException(ddiff)

    def test_project_param(self):
        """
        Test project param: last_changes
        """
        params = set_params({"project": "mytestproject"})
        params["_project_defs"] = [
            ProjectDefinition(
                name="mytestproject",
                repository_regex=None,
                branch_regex=None,
                file_regex=r".*backend.py",
            )
        ]
        ret = self.eldb.run_named_query("last_changes", ".*", params)
        self.assertEqual(ret["total"], 1, ret)

    def test_files_param(self):
        """
        Test files param: last_changes
        """
        params = set_params({"files": r".*backend.py"})
        ret = self.eldb.run_named_query("last_changes", ".*", params)
        self.assertEqual(ret["total"], 1, ret)

    def test_state_param(self):
        """
        Test files param: changes_and_events
        """
        params = set_params({"state": "MERGED"})
        ret = self.eldb.run_named_query("changes_and_events", "unit/repo[12]", params)
        self.assertEqual(ret["total"], 3, ret)

    def test_approvals_param(self):
        """
        Test approvals param: changes_and_events
        """
        params = set_params({"approvals": "Code-Review+2", "gte": "2020-01-01"})
        ret = self.eldb.run_named_query("changes_and_events", "unit/repo[12]", params)
        self.assertEqual(ret["total"], 2, ret)
        self.assertCountEqual([item["id"] for item in ret["items"]], ["c1", "c1_e4"])

        params = set_params(
            {"approvals": "CHANGES_REQUESTED,APPROVED", "gte": "2020-01-01"}
        )
        ret = self.eldb.run_named_query("changes_and_events", "unit/repo[12]", params)
        self.assertEqual(ret["total"], 4, ret)
        self.assertCountEqual(
            [item["id"] for item in ret["items"]], ["c2", "c2_e4", "c3", "c3_e2"]
        )

    def test_task_params(self):
        """
        Test task related params
        """
        params = set_params({"task_priority": "HIGH"})
        ret = self.eldb.run_named_query("last_changes", ".*", params)
        self.assertEqual(ret["total"], 1, ret)

        params = set_params({"task_priority": "HIGH,MEDIUM,LOW"})
        ret = self.eldb.run_named_query("last_changes", ".*", params)
        self.assertEqual(ret["total"], 2, ret)

        params = set_params({"task_type": "BUG"})
        ret = self.eldb.run_named_query("last_changes", ".*", params)
        self.assertEqual(ret["total"], 2, ret)

        params = set_params({"task_type": "BUG,CLIENT_IMPACT"})
        ret = self.eldb.run_named_query("last_changes", ".*", params)
        self.assertEqual(ret["total"], 2, ret)

        params = set_params({"task_priority": "LOW", "task_type": "BUG,CLIENT_IMPACT"})
        ret = self.eldb.run_named_query("last_changes", ".*", params)
        self.assertEqual(ret["total"], 1, ret)

        params = set_params({"task_priority": "HIGH"})
        ret = self.eldb.run_named_query("changes_and_events", ".*", params)
        self.assertEqual(ret["total"], 2, ret)
        self.assertListEqual([o["id"] for o in ret["items"]], ["c1", "c1_e2"])

    def test_exclude_approvals_param(self):
        """
        Test exclude_approvals param: last_changes
        """
        params = set_params({"exclude_approvals": "Verified-1", "gte": "2020-01-01"})
        ret = self.eldb.run_named_query("last_changes", "unit/repo1", params)
        self.assertEqual(ret["total"], 0, ret)

        params = set_params(
            {
                "approvals": "Code-Review+2",
                "exclude_approvals": "Verified-1",
                "gte": "2020-01-01",
            }
        )
        ret = self.eldb.run_named_query("last_changes", "unit/repo1", params)
        self.assertEqual(ret["total"], 0, ret)

    def test_get_indices(self):
        """
        Test get_indices
        """
        ret = self.eldb.get_indices()
        self.assertEqual(ret, [self.index])

    def test_branch_param(self):
        """
        Test branch param: last_changes
        """
        params = set_params({"state": "MERGED", "target_branch": "maintainance"})
        ret = self.eldb.run_named_query("last_changes", "unit/repo[12]", params)
        self.assertEqual(ret["total"], 0, ret)
        params = set_params({"target_branch": "master"})
        ret = self.eldb.run_named_query("changes_and_events", "unit/repo[12]", params)
        ret2 = self.eldb.run_named_query(
            "changes_and_events", "unit/repo[12]", set_params({})
        )
        self.assertEqual(ret["total"], ret2["total"])

    def test_change_and_events(self):
        """
        Test change_and_events query
        """
        params = set_params({})
        ret = self.eldb.run_named_query("changes_and_events", "unit/repo1", params)
        self.assertEqual(ret["total"], 6)
        change = [c for c in ret["items"] if c["type"] == "Change"][0]
        self.assertTrue(change["tests_included"])
        self.assertTrue(change["has_issue_tracker_links"])
        self.assertListEqual(
            change["issue_tracker_links"][0],
            ["#42", "https://github.com/unit/repo1/issues/42"],
        )

    def test_last_changes(self):
        """
        Test last_changes query
        """
        params = set_params({"state": "OPEN"})
        ret = self.eldb.run_named_query("last_changes", "unit/repo[12]", params)
        self.assertEqual(ret["total"], 1)
        self.assertFalse(ret["items"][0]["tests_included"])

        params = set_params({"state": "MERGED"})
        ret = self.eldb.run_named_query("last_changes", "unit/repo[12]", params)
        self.assertEqual(ret["total"], 3)
        for change in ret["items"]:
            self.assertIn("tests_included", list(change.keys()))

    def test_self_merged_param(self):
        params = set_params({"state": "MERGED", "self_merged": True})
        ret = self.eldb.run_named_query("last_changes", "unit/repo[12]", params)
        self.assertEqual(ret["total"], 1)
        self.assertEqual(ret["items"][0]["author"], ret["items"][0]["merged_by"])

    def test_tests_included_param(self):
        """
        Test tests_included param: last_changes
        """
        params = set_params({"tests_included": True})
        ret = self.eldb.run_named_query("last_changes", "unit/repo[12]", params)
        self.assertEqual(ret["total"], 1, ret)
        params = set_params({})
        ret = self.eldb.run_named_query("last_changes", "unit/repo[12]", params)
        self.assertEqual(ret["total"], 4, ret)

    def test_has_issue_tracker_links_param(self):
        """
        Test has_issue_tracker_links param: last_changes
        """
        params = set_params({"has_issue_tracker_links": "github.com"})
        ret = self.eldb.run_named_query("last_changes", "unit/repo[12]", params)
        self.assertEqual(ret["total"], 1, ret)
        params = set_params({})
        ret = self.eldb.run_named_query("last_changes", "unit/repo[12]", params)
        self.assertEqual(ret["total"], 4, ret)

    def test_changes_lifecycle_stats(self):
        """
        Test changes_lifecycle_stats query
        """
        params = set_params({"gte": "2020-01-01", "lte": "2020-01-03"})
        ret = self.eldb.run_named_query("changes_lifecycle_stats", ".*", params)
        expected = {
            "ChangeCommitForcePushedEvent": {"authors_count": 0, "events_count": 0},
            "ChangeCommitPushedEvent": {"authors_count": 1, "events_count": 1},
            "ChangeCreatedEvent": {"authors_count": 2, "events_count": 2},
            "abandoned": 0,
            "self_merged": 0,
            "commits": 1.0,
            "duration": 86400.0,
            "duration_variability": 0.0,
            "histos": {
                "ChangeAbandonedEvent": (
                    [
                        {
                            "doc_count": 0,
                            "key": 1577836800000,
                            "key_as_string": "2020-01-01",
                        },
                        {
                            "doc_count": 0,
                            "key": 1577923200000,
                            "key_as_string": "2020-01-02",
                        },
                        {
                            "doc_count": 0,
                            "key": 1578009600000,
                            "key_as_string": "2020-01-03",
                        },
                    ],
                    0,
                ),
                "ChangeCommitForcePushedEvent": (
                    [
                        {
                            "doc_count": 0,
                            "key": 1577836800000,
                            "key_as_string": "2020-01-01",
                        },
                        {
                            "doc_count": 0,
                            "key": 1577923200000,
                            "key_as_string": "2020-01-02",
                        },
                        {
                            "doc_count": 0,
                            "key": 1578009600000,
                            "key_as_string": "2020-01-03",
                        },
                    ],
                    0,
                ),
                "ChangeCommitPushedEvent": (
                    [
                        {
                            "doc_count": 0,
                            "key": 1577836800000,
                            "key_as_string": "2020-01-01",
                        },
                        {
                            "doc_count": 0,
                            "key": 1577923200000,
                            "key_as_string": "2020-01-02",
                        },
                        {
                            "doc_count": 1,
                            "key": 1578009600000,
                            "key_as_string": "2020-01-03",
                        },
                    ],
                    0.3333333333333333,
                ),
                "ChangeCreatedEvent": (
                    [
                        {
                            "doc_count": 1,
                            "key": 1577836800000,
                            "key_as_string": "2020-01-01",
                        },
                        {
                            "doc_count": 0,
                            "key": 1577923200000,
                            "key_as_string": "2020-01-02",
                        },
                        {
                            "doc_count": 1,
                            "key": 1578009600000,
                            "key_as_string": "2020-01-03",
                        },
                    ],
                    0.6666666666666666,
                ),
                "ChangeMergedEvent": (
                    [
                        {
                            "doc_count": 0,
                            "key": 1577836800000,
                            "key_as_string": "2020-01-01",
                        },
                        {
                            "doc_count": 1,
                            "key": 1577923200000,
                            "key_as_string": "2020-01-02",
                        },
                        {
                            "doc_count": 0,
                            "key": 1578009600000,
                            "key_as_string": "2020-01-03",
                        },
                    ],
                    0.3333333333333333,
                ),
            },
            "merged": 1,
            "opened": 1,
            "ratios": {
                "abandoned/created": 0.0,
                "iterations/created": 1.5,
                "merged/created": 50.0,
                "self_merged/created": 0.0,
            },
            "tests": 50.0,
        }

        ddiff = DeepDiff(ret, expected)
        if ddiff:
            raise DiffException(ddiff)

        params = set_params(
            {"gte": "2020-01-01", "lte": "2020-01-03", "authors": "john,jane"}
        )
        ret = self.eldb.run_named_query("changes_lifecycle_stats", ".*", params)
        ddiff = DeepDiff(ret, expected)
        if ddiff:
            raise DiffException(ddiff)

        params = set_params(
            {"gte": "2020-01-01", "lte": "2020-01-03", "authors": "john"}
        )
        ret = self.eldb.run_named_query("changes_lifecycle_stats", ".*", params)
        expected = {
            "ChangeCommitForcePushedEvent": {"authors_count": 0, "events_count": 0},
            "ChangeCommitPushedEvent": {"authors_count": 0, "events_count": 0},
            "ChangeCreatedEvent": {"authors_count": 1, "events_count": 1},
            "abandoned": 0,
            "self_merged": 0,
            "commits": 1.0,
            "duration": 86400.0,
            "duration_variability": 0.0,
            "histos": {
                "ChangeAbandonedEvent": (
                    [
                        {
                            "doc_count": 0,
                            "key": 1577836800000,
                            "key_as_string": "2020-01-01",
                        },
                        {
                            "doc_count": 0,
                            "key": 1577923200000,
                            "key_as_string": "2020-01-02",
                        },
                        {
                            "doc_count": 0,
                            "key": 1578009600000,
                            "key_as_string": "2020-01-03",
                        },
                    ],
                    0,
                ),
                "ChangeCommitForcePushedEvent": (
                    [
                        {
                            "doc_count": 0,
                            "key": 1577836800000,
                            "key_as_string": "2020-01-01",
                        },
                        {
                            "doc_count": 0,
                            "key": 1577923200000,
                            "key_as_string": "2020-01-02",
                        },
                        {
                            "doc_count": 0,
                            "key": 1578009600000,
                            "key_as_string": "2020-01-03",
                        },
                    ],
                    0,
                ),
                "ChangeCommitPushedEvent": (
                    [
                        {
                            "doc_count": 0,
                            "key": 1577836800000,
                            "key_as_string": "2020-01-01",
                        },
                        {
                            "doc_count": 0,
                            "key": 1577923200000,
                            "key_as_string": "2020-01-02",
                        },
                        {
                            "doc_count": 0,
                            "key": 1578009600000,
                            "key_as_string": "2020-01-03",
                        },
                    ],
                    0,
                ),
                "ChangeCreatedEvent": (
                    [
                        {
                            "doc_count": 1,
                            "key": 1577836800000,
                            "key_as_string": "2020-01-01",
                        },
                        {
                            "doc_count": 0,
                            "key": 1577923200000,
                            "key_as_string": "2020-01-02",
                        },
                        {
                            "doc_count": 0,
                            "key": 1578009600000,
                            "key_as_string": "2020-01-03",
                        },
                    ],
                    0.3333333333333333,
                ),
                "ChangeMergedEvent": (
                    [
                        {
                            "doc_count": 0,
                            "key": 1577836800000,
                            "key_as_string": "2020-01-01",
                        },
                        {
                            "doc_count": 1,
                            "key": 1577923200000,
                            "key_as_string": "2020-01-02",
                        },
                        {
                            "doc_count": 0,
                            "key": 1578009600000,
                            "key_as_string": "2020-01-03",
                        },
                    ],
                    0.3333333333333333,
                ),
            },
            "merged": 1,
            "opened": 0,
            "ratios": {
                "abandoned/created": 0.0,
                "iterations/created": 1.0,
                "merged/created": 100.0,
                "self_merged/created": 0.0,
            },
            "tests": 100.0,
        }

        ddiff = DeepDiff(ret, expected)
        if ddiff:
            raise DiffException(ddiff)

    def test_most_active_authors_stats(self):
        """
        Test query: most_active_authors_stats
        """
        params = set_params({})
        ret = self.eldb.run_named_query("most_active_authors_stats", ".*", params)
        expected = {
            "ChangeCommentedEvent": {
                "count_avg": 1,
                "count_median": 1.0,
                "items": [
                    {"doc_count": 1, "key": "jane"},
                    {"doc_count": 1, "key": "steve"},
                ],
                "total": 2,
                "total_hits": 2,
            },
            "ChangeCreatedEvent": {
                "count_avg": 1.3333333333333333,
                "count_median": 1,
                "items": [
                    {"doc_count": 2, "key": "jane"},
                    {"doc_count": 1, "key": "john"},
                    {"doc_count": 1, "key": "steve"},
                ],
                "total": 3,
                "total_hits": 4,
            },
            "ChangeMergedEvent": {
                "count_avg": 1,
                "count_median": 1,
                "items": [
                    {"doc_count": 1, "key": "jane"},
                    {"doc_count": 1, "key": "john"},
                    {"doc_count": 1, "key": "steve"},
                ],
                "total": 3,
                "total_hits": 3,
            },
            "ChangeReviewedEvent": {
                "count_avg": 1.3333333333333333,
                "count_median": 1,
                "items": [
                    {"doc_count": 2, "key": "john"},
                    {"doc_count": 1, "key": "jane"},
                    {"doc_count": 1, "key": "steve"},
                ],
                "total": 3,
                "total_hits": 4,
            },
        }

        ddiff = DeepDiff(ret, expected)
        if ddiff:
            raise DiffException(ddiff)

        params = set_params({"authors": "jane"})
        ret = self.eldb.run_named_query("most_active_authors_stats", ".*", params)
        expected = {
            "ChangeCommentedEvent": {
                "count_avg": 1,
                "count_median": 1,
                "items": [{"doc_count": 1, "key": "jane"}],
                "total": 1,
                "total_hits": 1,
            },
            "ChangeCreatedEvent": {
                "count_avg": 2,
                "count_median": 2,
                "items": [{"doc_count": 2, "key": "jane"}],
                "total": 1,
                "total_hits": 2,
            },
            "ChangeMergedEvent": {
                "count_avg": 1,
                "count_median": 1,
                "items": [{"doc_count": 1, "key": "jane"}],
                "total": 1,
                "total_hits": 1,
            },
            "ChangeReviewedEvent": {
                "count_avg": 1,
                "count_median": 1,
                "items": [{"doc_count": 1, "key": "jane"}],
                "total": 1,
                "total_hits": 1,
            },
        }

        ddiff = DeepDiff(ret, expected)
        if ddiff:
            raise DiffException(ddiff)

    def test_repos_summary(self):
        """
        Test query: repos_summary
        """
        params = set_params({})
        ret = self.eldb.run_named_query("repos_summary", ".*", params)
        expected = {
            "summary": {
                "unit/repo1": {
                    "changes": 1,
                    "changes_abandoned": 0,
                    "changes_merged": 1,
                    "changes_open": 0,
                },
                "unit/repo2": {
                    "changes": 3,
                    "changes_abandoned": 0,
                    "changes_merged": 2,
                    "changes_open": 1,
                },
            }
        }

        ddiff = DeepDiff(ret, expected)
        if ddiff:
            raise DiffException(ddiff)
