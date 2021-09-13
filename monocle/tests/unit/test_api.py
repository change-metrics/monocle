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

import os
import unittest
from flask import json

from monocle import env
from monocle import webapp
from monocle import config

from .common import index_dataset, get_db_cnx


class TestWebAPI(unittest.TestCase):
    prefix = "monocle.test.1."
    index1 = "unittest-1"
    index2 = "unittest-2"
    datasets = ["objects/unit_repo1.json"]

    def tearDown(self):
        for index in (self.index1, self.index2):
            self.eldb.es.indices.delete(index=self.eldb.prefix + index)

    def setUp(self):
        for index in (self.index1, self.index2):
            self.eldb = get_db_cnx(index, self.prefix)
            if index in (self.index1, self.index2):
                for dataset in self.datasets:
                    index_dataset(self.eldb, dataset)
        webapp.CHANGE_PREFIX = self.prefix
        webapp.app.config["TESTING"] = True
        self.client = webapp.app.test_client()
        self.apikey = "1a2b3c4d5e"
        config_data = {
            "workspaces": [
                {
                    "index": self.index1,
                },
                {
                    "index": self.index2,
                    "task_crawlers": [
                        {
                            "name": "myttcrawler",
                            "api_key": self.apikey,
                            "updated_since": "2020-01-01",
                        }
                    ],
                },
            ]
        }
        env.indexes_task_crawlers = config.build_index_task_crawlers(config_data)

    def test_health(self):
        "Test health endpoint"
        resp = self.client.get("/api/0/health")
        for key in ("hostname", "status", "timestamp"):
            self.assertIn(key, json.loads(resp.data))

    def test_get_indices(self):
        "Test indices endpoint"
        resp = self.client.get("/api/0/indices")
        self.assertListEqual(["unittest-1", "unittest-2"], json.loads(resp.data))

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

    def check_APIErr_msg(self, message, resp):
        err = json.loads(resp.data)
        self.assertTrue(err["message"].startswith(message))
