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
        cls.eldb = ELmonocleDB(index=cls.index)
        for dataset in cls.datasets:
            index_dataset(cls.index, dataset)

    @classmethod
    def tearDownClass(cls):
        cls.eldb.es.indices.delete(index=cls.index, ignore=[400, 404])

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
        params = set_params(
            {'interval': '1d', 'gte': '2020-01-01', 'lte': '2020-01-03'}
        )
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
