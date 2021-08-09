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

from .common import index_dataset
from .common import get_db_cnx

from monocle.db.db import UnknownQueryException
from monocle.db import queries
from monocle.utils import set_params
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
                score=50,
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
                score=10,
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

    def test_first_created_event(self):
        """
        Test internal query: _first_created_event
        """
        params = set_params({})
        ret = queries._first_created_event(
            self.eldb.es, self.eldb.index, "unit/repo1", params
        )
        self.assertEqual(ret, "2020-01-01T00:00:00Z")

    def test_get_indices(self):
        """
        Test get_indices
        """
        ret = self.eldb.get_indices()
        self.assertEqual(ret, [self.index])

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
