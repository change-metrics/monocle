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

from deepdiff import DeepDiff

from .common import index_dataset
from .common import DiffException

from monocle.db.db import ELmonocleDB
from monocle.db.db import UnknownQueryException
from monocle.db import queries
from monocle.utils import set_params


class TestQueries(unittest.TestCase):

    index = 'monocle-unittest'
    datasets = [
        'objects/unit_repo1.json',
        'objects/unit_repo2.json',
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
        cls.eldb = ELmonocleDB(index=cls.index, prefix='monocle.test.')
        for dataset in cls.datasets:
            index_dataset(cls.eldb, dataset)

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
            'unknown',
            'unit/repo1',
            params,
        )

    def test_all_queries(self):
        """
        Test all public queries
        """
        failing = []
        for query in queries.public_queries:
            params = set_params({})
            ret = self.eldb.run_named_query(query, 'unit/repo1', params)
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
        ret = queries._scan(self.eldb.es, self.eldb.index, 'unit/repo1', params)
        ids = [obj['id'] for obj in ret]
        expected = ['c1_e1', 'c1_e2', 'c1_e3']
        self.assertListEqual(ids, expected)

    def test_first_created_event(self):
        """
        Test internal query: _first_created_event
        """
        params = set_params({})
        ret = queries._first_created_event(
            self.eldb.es, self.eldb.index, 'unit/repo1', params
        )
        self.assertEqual(ret, "2020-01-01T00:00:00Z")

    def test_events_top(self):
        """
        Test internal query: _events_top
        """
        params = set_params({})
        ret = queries._events_top(
            self.eldb.es, self.eldb.index, 'unit/repo1', 'type', params
        )
        expected = {
            'items': [
                {'key': 'ChangeCommentedEvent', 'doc_count': 1},
                {'key': 'ChangeCreatedEvent', 'doc_count': 1},
                {'key': 'ChangeMergedEvent', 'doc_count': 1},
            ],
            'count_avg': 1,
            'count_median': 1,
            'total': 3,
            'total_hits': 3,
        }
        ddiff = DeepDiff(ret, expected)
        if ddiff:
            raise DiffException(ddiff)

    def test_count_events(self):
        """
        Test query: count_events
        """
        params = set_params({})
        ret = self.eldb.run_named_query('count_events', 'unit/repo1', params)
        self.assertEqual(ret, 3)

    def test_count_authors(self):
        """
        Test query: count_authors
        """
        params = set_params({})
        ret = self.eldb.run_named_query('count_authors', 'unit/repo1', params)
        self.assertEqual(ret, 2)

        params = set_params({'type': 'ChangeCreatedEvent'})
        ret = self.eldb.run_named_query('count_authors', 'unit/repo1', params)
        self.assertEqual(ret, 1)

    def test_events_histo(self):
        """
        Test query: events_histo
        """
        params = set_params({'gte': '2020-01-01', 'lte': '2020-01-02'})
        ret = self.eldb.run_named_query('events_histo', 'unit/repo1', params)
        expected = (
            [
                {'key_as_string': '2019-12-31', 'key': 1577750400000, 'doc_count': 0},
                {'key_as_string': '2020-01-01', 'key': 1577836800000, 'doc_count': 2},
                {'key_as_string': '2020-01-02', 'key': 1577923200000, 'doc_count': 1},
            ],
            1.0,
        )
        ddiff = DeepDiff(ret, expected)
        if ddiff:
            raise DiffException(ddiff)

    def test_events_top_authors(self):
        """
        Test query: events_top_authors
        """
        params = set_params({})
        ret = self.eldb.run_named_query('events_top_authors', 'unit/repo1', params)
        expected = {
            'items': [{'key': 'jane', 'doc_count': 2}, {'key': 'john', 'doc_count': 1}],
            'count_avg': 1.5,
            'count_median': 1.5,
            'total': 2,
            'total_hits': 3,
        }
        ddiff = DeepDiff(ret, expected)
        if ddiff:
            raise DiffException(ddiff)

    def test_repos_top_merged(self):
        """
        Test query: repos_top_merged
        """
        params = set_params({})
        ret = self.eldb.run_named_query('repos_top_merged', 'unit/repo[12]', params)
        expected = {
            'items': [
                {'key': 'unit/repo2', 'doc_count': 2},
                {'key': 'unit/repo1', 'doc_count': 1},
            ],
            'count_avg': 1.5,
            'count_median': 1.5,
            'total': 2,
            'total_hits': 3,
        }
        ddiff = DeepDiff(ret, expected)
        if ddiff:
            raise DiffException(ddiff)

    def test_files_param(self):
        """
        Test files param: last_merged_changes
        """
        params = set_params({'files': r'.*backend.py'})
        ret = self.eldb.run_named_query('last_merged_changes', 'unit/repo[12]', params)
        self.assertEqual(ret['total'], 1, ret)

    def test_state_param(self):
        """
        Test files param: changes_and_events
        """
        params = set_params({'state': 'MERGED'})
        ret = self.eldb.run_named_query('changes_and_events', 'unit/repo[12]', params)
        self.assertEqual(ret['total'], 3, ret)

    def test_get_indices(self):
        """
        Test get_indices
        """
        ret = self.eldb.get_indices()
        self.assertEqual(ret, [self.index])
