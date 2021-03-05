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

import logging
import os
import unittest
from flask import json
from monocle import webapp
from monocle import config
from monocle.db.db import ELmonocleDB

from .common import index_dataset


class TestWebAPI(unittest.TestCase):
    prefix = "monocle.test."
    index1 = "monocle-unittest-1"
    index2 = "monocle-unittest-2"
    datasets = ["objects/unit_repo1.json"]

    @classmethod
    def setUpClass(cls):
        logging.basicConfig(
            level=logging.DEBUG,
            format="%(asctime)s - %(name)s - " + "%(levelname)s - %(message)s",
        )
        log = logging.getLogger(__name__)
        # log to stderr
        log.addHandler(logging.StreamHandler())
        for index in (cls.index1, cls.index2):
            cls.eldb = ELmonocleDB(index=index, prefix=cls.prefix)
            for dataset in cls.datasets:
                index_dataset(cls.eldb, dataset)

    @classmethod
    def tearDownClass(cls):
        for index in (cls.index1, cls.index2):
            cls.eldb.es.indices.delete(index=cls.eldb.prefix + index)

    def setUp(self):
        webapp.CHANGE_PREFIX = self.prefix
        webapp.app.config["TESTING"] = True
        self.client = webapp.app.test_client()
        config_data = {
            "tenants": [
                {
                    # Private index
                    "index": self.index1,
                    "users": ["jane", "john"],
                },
                {
                    # Public index
                    "index": self.index2
                },
            ]
        }
        webapp.indexes_acl = config.build_index_acl(config_data)

    def test_health(self):
        "Test health endpoint"
        resp = self.client.get("/api/0/health")
        for key in ("hostname", "status", "timestamp"):
            self.assertIn(key, json.loads(resp.data))

    def test_get_indices(self):
        "Test indices endpoint"
        resp = self.client.get("/api/0/indices")
        self.assertListEqual(["monocle-unittest-2"], json.loads(resp.data))

    def test_get_indices_with_acl(self):
        "Test indices endpoint with acl"
        with self.client.session_transaction() as sess:
            sess["username"] = "jane"
        resp = self.client.get("/api/0/indices")
        self.assertListEqual(
            ["monocle-unittest-1", "monocle-unittest-2"], json.loads(resp.data)
        )

    def test_query(self):
        "Test we can run query via the api"
        resp = self.client.get(
            "/api/0/query/count_events?index=%s&repository=.*" % self.index2
        )
        self.assertEqual(5, json.loads(resp.data))

    def test_query_with_acl(self):
        "Test we can run query via the api with acl"
        resp = self.client.get(
            "/api/0/query/count_events?index=%s&repository=.*" % self.index1
        )
        self.assertEqual(403, resp.status_code)
        with self.client.session_transaction() as sess:
            sess["username"] = "jane"
        resp = self.client.get(
            "/api/0/query/count_events?index=%s&repository=.*" % self.index1
        )
        self.assertEqual(5, json.loads(resp.data))
        resp = self.client.get(
            "/api/0/query/count_events?index=%s&repository=.*" % self.index2
        )
        self.assertEqual(5, json.loads(resp.data))
        with self.client.session_transaction() as sess:
            sess["remote_user"] = "jane"
        resp = self.client.get(
            "/api/0/query/count_events?index=%s&repository=.*" % self.index1,
            headers={"REMOTE_USER": "jane"},
        )
        self.assertEqual(5, json.loads(resp.data))

    def test_whoami(self):
        "Test whoami method"
        resp = self.client.get("/api/0/whoami")
        self.assertEqual(503, resp.status_code)
        os.environ["CLIENT_ID"] = "test"
        resp = self.client.get("/api/0/whoami", headers={"REMOTE_USER": "jane"})
        self.assertEqual("jane", json.loads(resp.data))

        with self.client.session_transaction() as sess:
            sess["username"] = "jane"
        resp = self.client.get("/api/0/whoami")
        self.assertEqual("jane", json.loads(resp.data))
