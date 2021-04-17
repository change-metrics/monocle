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

import unittest
from datetime import datetime

from .common import index_dataset, get_db_cnx

from monocle.task_data import (
    OrphanTaskDataForEL,
    TaskData,
    AdoptedTaskDataForEL,
    AdoptedTaskData,
)


class TestQueries(unittest.TestCase):

    index = "unittest"
    datasets = [
        "objects/unit_repo1.json",
        "objects/unit_repo2.json",
    ]

    def setUp(self):
        self.db = get_db_cnx(self.index, "monocle.test.1.")
        for dataset in self.datasets:
            index_dataset(self.db, dataset)
        self.otds = [
            OrphanTaskDataForEL(
                _id="https://bugtracker.domain.dom/123",
                task_data=TaskData(
                    crawler_name="mycrawler",
                    updated_at=datetime.strptime(
                        "2020-01-01T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"
                    ),
                    change_url="https://tests.com/unit/repo1/pull/1",
                    issue_type=["BUG"],
                    issue_id="123",
                    issue_url="https://bugtracker.domain.dom/123",
                    issue_title="It does not work",
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
                    issue_type=["BUG"],
                    issue_id="124",
                    issue_url="https://bugtracker.domain.dom/124",
                    issue_title="It does not work",
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
                    issue_type=["BUG"],
                    issue_id="125",
                    issue_url="https://bugtracker.domain.dom/125",
                    issue_title="It does not work",
                ),
            ),
        ]

    def tearDown(self):
        self.db.es.indices.delete(index=self.db.prefix + self.index)

    def test_get_orphan_tds_by_change_url(self):

        self.db.update_task_data(self.otds)
        mtds = self.db.get_orphan_tds_by_change_urls(
            [
                "https://tests.com/unit/repo1/pull/1",
                "https://tests.com/unit/repo2/pull/2",
                "https://tests.com/unit/repomissing/pull/2",
            ]
        )
        self.assertEqual(len(mtds), 3)

        self.db.update_task_data(
            [
                AdoptedTaskDataForEL(
                    _id="https://bugtracker.domain.dom/125",
                    task_data=AdoptedTaskData(_adopted=True),
                )
            ]
        )
        mtds = self.db.get_orphan_tds_by_change_urls(
            [
                "https://tests.com/unit/repo1/pull/1",
                "https://tests.com/unit/repo2/pull/2",
                "https://tests.com/unit/repomissing/pull/2",
            ]
        )
        self.assertEqual(len(mtds), 2)

    def test_get_orphan_tds_and_declare_adoption(self):
        self.db.update_task_data(self.otds)
        adopted_tds = self.db.get_orphan_tds_and_declare_adpotion(
            [
                "https://tests.com/unit/repo1/pull/1",
                "https://tests.com/unit/repo2/pull/2",
                "https://tests.com/unit/repomissing/pull/2",
            ]
        )
        self.assertEqual(len(adopted_tds), 3)
        adopted_tds = self.db.get_orphan_tds_and_declare_adpotion(
            [
                "https://tests.com/unit/repo1/pull/1",
                "https://tests.com/unit/repo2/pull/2",
                "https://tests.com/unit/repomissing/pull/2",
            ]
        )
        self.assertEqual(len(adopted_tds), 0)

    def test_update_changes_with_orphan_tds(self):
        self.otds.append(
            OrphanTaskDataForEL(
                _id="https://bugtracker.domain.dom/126",
                task_data=TaskData(
                    crawler_name="mycrawler",
                    updated_at=datetime.strptime(
                        "2020-01-04T00:00:00Z", "%Y-%m-%dT%H:%M:%SZ"
                    ),
                    change_url="https://tests.com/unit/repomissing/pull/1",
                    issue_type=["BUG"],
                    issue_id="126",
                    issue_url="https://bugtracker.domain.dom/126",
                    issue_title="It does not work",
                ),
            ),
        )
        self.db.update_task_data(self.otds)
        self.db.update_changes_with_orphan_tds(
            {
                "https://tests.com/unit/repo1/pull/1": "c1",
                "https://tests.com/unit/repo2/pull/2": "c2",
                "https://tests.com/unit/repo2/pull/3": "c3",
            }
        )
        changes = self.db.get_changes_by_url(
            [
                "https://tests.com/unit/repo1/pull/1",
                "https://tests.com/unit/repo2/pull/2",
                "https://tests.com/unit/repo2/pull/3",
                "https://tests.com/unit/repomissing/pull/1",
            ],
            size=100,
        )
        self.assertEqual(len(changes), 3)
        r1p1 = [c for c in changes if c["url"].endswith("repo1/pull/1")][0]
        r2p2 = [c for c in changes if c["url"].endswith("repo2/pull/2")][0]
        r2p3 = [c for c in changes if c["url"].endswith("repo2/pull/3")][0]

        # Ensure Tasks data are assign to the right changes
        self.assertEqual(len(r1p1["tasks_data"]), 2)
        self.assertEqual(len(r2p2["tasks_data"]), 1)
        self.assertEqual(len(r2p3.get("tasks_data", [])), 0)

        # Ensure no more orphan Task remain in the DB
        otds = self.db.get_orphan_tds_by_change_urls(
            [
                "https://tests.com/unit/repo1/pull/1",
                "https://tests.com/unit/repo2/pull/2",
                "https://tests.com/unit/repo2/pull/3",
                "https://tests.com/unit/repomissing/pull/1",
            ]
        )
        self.assertEqual(len(otds), 1)
