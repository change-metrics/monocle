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
import tempfile
import unittest
import yaml
from flask import json
from monocle import webapp
from monocle import config
from monocle.db.db import ELmonocleDB

from .common import index_dataset


class TestWebAPI(unittest.TestCase):
    prefix = "monocle.test.1."
    index1 = "monocle-unittest-1"
    index2 = "monocle-unittest-2"
    index3 = "monocle-unittest-3"
    datasets = ["objects/unit_repo1.json"]
    datasets2 = ["objects/unit_repo2.json"]

    @classmethod
    def setUpClass(cls):
        logging.basicConfig(
            level=logging.DEBUG,
            format="%(asctime)s - %(name)s - " + "%(levelname)s - %(message)s",
        )
        log = logging.getLogger(__name__)
        # log to stderr
        log.addHandler(logging.StreamHandler())
        for index in (cls.index1, cls.index2, cls.index3):
            cls.eldb = ELmonocleDB(
                index=index,
                prefix=cls.prefix,
                user=os.getenv("ELASTIC_USER", None),
                password=os.getenv("ELASTIC_PASSWORD", None),
                use_ssl=os.getenv("ELASTIC_USE_SSL", False),
                verify_certs=os.getenv("ELASTIC_INSECURE", None),
                ssl_show_warn=os.getenv("ELASTIC_SSL_SHOW_WARN", None),
            )
            if index in (cls.index1, cls.index2):
                for dataset in cls.datasets:
                    index_dataset(cls.eldb, dataset)
            if index in (cls.index3,):
                for dataset in cls.datasets2:
                    index_dataset(cls.eldb, dataset)

    @classmethod
    def tearDownClass(cls):
        for index in (cls.index1, cls.index2, cls.index3):
            cls.eldb.es.indices.delete(index=cls.eldb.prefix + index)

    def setUp(self):
        webapp.CHANGE_PREFIX = self.prefix
        webapp.app.config["TESTING"] = True
        self.client = webapp.app.test_client()
        self.apikey = "1a2b3c4d5e"
        config_data = {
            "tenants": [
                {
                    # Private index
                    "index": self.index1,
                    "users": ["jane", "john"],
                },
                {
                    # Public index
                    "index": self.index2,
                    "task_tracker_crawlers": [
                        {
                            "name": "myttcrawler",
                            "api_key": self.apikey,
                            "updated_since": "2020-01-01",
                        }
                    ],
                },
                {
                    "index": self.index3,
                    "task_tracker_crawlers": [
                        {
                            "name": "myttcrawler",
                            "api_key": self.apikey,
                            "updated_since": "2020-01-01",
                        },
                        {
                            "name": "myttcrawler2",
                            "api_key": self.apikey,
                            "updated_since": "2020-01-01",
                        },
                    ],
                },
            ]
        }
        webapp.indexes_acl = config.build_index_acl(config_data)
        webapp.indexes_task_tracker_crawlers = config.build_index_task_tracker_crawlers(
            config_data
        )

    def test_health(self):
        "Test health endpoint"
        resp = self.client.get("/api/0/health")
        for key in ("hostname", "status", "timestamp"):
            self.assertIn(key, json.loads(resp.data))

    def test_get_indices(self):
        "Test indices endpoint"
        resp = self.client.get("/api/0/indices")
        self.assertListEqual(
            ["monocle-unittest-2", "monocle-unittest-3"], json.loads(resp.data)
        )

    def test_get_indices_with_acl(self):
        "Test indices endpoint with acl"
        with self.client.session_transaction() as sess:
            sess["username"] = "jane"
        resp = self.client.get("/api/0/indices")
        self.assertListEqual(
            ["monocle-unittest-1", "monocle-unittest-2", "monocle-unittest-3"],
            json.loads(resp.data),
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

    def test_config_project_def(self):
        "Test we can get project definitions from config file"
        config_example = """
---
tenants:
  - index: %s
    crawler:
      loop_delay: 600
      gerrit_repositories:
        - name: "^openstack/.*"
          updated_since: "2021-01-01"
          base_url: https://review.opendev.org/
    projects:
      - name: %s
        repositories_regex: "test1/somerepo1"
      - name: %s
        repositories_regex: "test2/test"
        """ % (
            self.index2,
            self.index2,
            self.index2,
        )
        with tempfile.NamedTemporaryFile() as fp:
            with open(fp.name, "w") as f:
                f.write(config_example)
            webapp.project_defs = config.build_project_definitions(
                yaml.safe_load(open(fp.name))
            )
            resp = self.client.get("/api/0/projects?index=%s" % self.index2)
            self.assertEqual(2, len(json.loads(resp.data)))

    def test_config_project_def_no_cfg_provided(self):
        "Test we can get project definitions without config file"
        # NOTE: previous tests were changing the configuration file,
        # so if the index2 name is last set, Monocle will return HTTP conde
        # 200, instead of 404.
        fake_index = "fake-index"
        resp = self.client.get("/api/0/projects?index=%s" % fake_index)
        self.assertEqual(404, resp.status_code)

    def test_tracker_data_amend(self):
        "Test tracker_data_amend endpoint"
        # First try some faulty requests
        resp = self.client.post(
            "/api/0/amend/tracker_data?index=%s" % self.index2, json=""
        )
        self.assertEqual(400, resp.status_code)
        self.assertEqual(
            "No crawler name or/and apikey provided",
            resp.data.decode(),
        )

        resp = self.client.post(
            "/api/0/amend/tracker_data?index=%s&apikey=badkey" % self.index2, json=""
        )
        self.assertEqual(400, resp.status_code)
        self.assertEqual("No crawler name or/and apikey provided", resp.data.decode())

        resp = self.client.post(
            "/api/0/amend/tracker_data?index=%s&apikey=badkey&name=myttcrawler"
            % self.index2,
            json="",
        )
        self.assertEqual(403, resp.status_code)
        self.assertEqual("Not authorized", resp.data.decode())

        url = "/api/0/amend/tracker_data?index=%s&apikey=%s&name=%s" % (
            self.index2,
            self.apikey,
            "myttcrawler",
        )

        resp = self.client.post(url, json="data")
        self.assertEqual(400, resp.status_code)
        self.assertEqual("Input data is not a List", resp.data.decode())

        resp = self.client.post(
            url,
            json=list(range(webapp.INPUT_TRACKER_DATA_LIMIT + 1)),
        )
        self.assertEqual(400, resp.status_code)
        self.assertTrue(resp.data.decode().startswith("Input data List over limit"))

        resp = self.client.post(
            url,
            json=[{"do": "you", "eat": "that"}],
        )
        self.assertEqual(400, resp.status_code)
        self.assertEqual(
            "Unable to extract input data due to: 'crawler_name'",
            resp.data.decode(),
        )

        # Now test a working workflow
        resp = self.client.get(
            "/api/0/query/changes?index=%s&repository=.*&change_ids=unit@repo1@1"
            % self.index2
        )
        orig = json.loads(resp.data)["items"][0]
        self.assertNotIn("tracker_data", orig)
        # Do a first post of tracker_data
        tracker_data = [
            {
                "crawler_name": "myttcrawler",
                "updated_at": "2021-04-09T12:00:00",
                "change_url": "https://tests.com/unit/repo1/pull/1",
                "issue_type": "RFE",
                "issue_id": "1234",
                "issue_url": "https://issue-tracker.domain.com/1234",
                "issue_title": "Implement feature XYZ",
            }
        ]
        resp = self.client.post(url, json=tracker_data)
        self.assertEqual(200, resp.status_code)
        webapp.cache.delete_memoized(webapp.do_query)
        resp = self.client.get(
            "/api/0/query/changes?index=%s&repository=.*&change_ids=unit@repo1@1"
            % self.index2
        )
        new = json.loads(resp.data)["items"][0]
        self.assertIn("tracker_data", new)
        # Attempt a new post with an updated task
        tracker_data = [
            {
                "crawler_name": "myttcrawler",
                "updated_at": "2021-04-09T13:00:00",
                "change_url": "https://tests.com/unit/repo1/pull/1",
                "issue_type": "RFE",
                "issue_id": "1234",
                "issue_url": "https://issue-tracker.domain.com/1234",
                "issue_title": "Implement feature XYZ",
            },
            {
                "crawler_name": "myttcrawler",
                "updated_at": "2021-04-09T12:00:00",
                "change_url": "https://tests.com/unit/repo1/pull/1",
                "issue_type": "RFE",
                "issue_id": "1235",
                "issue_url": "https://issue-tracker.domain.com/1235",
                "issue_title": "Implement feature XYZ",
            },
        ]
        resp = self.client.post(url, json=tracker_data)
        self.assertEqual(200, resp.status_code)
        webapp.cache.delete_memoized(webapp.do_query)
        resp = self.client.get(
            "/api/0/query/changes?index=%s&repository=.*&change_ids=unit@repo1@1"
            % self.index2
        )
        new = json.loads(resp.data)["items"][0]
        self.assertIn("tracker_data", new)
        std = [(td["issue_url"], td["updated_at"]) for td in new["tracker_data"]]
        self.assertListEqual(
            [
                ("https://issue-tracker.domain.com/1234", "2021-04-09T13:00:00"),
                ("https://issue-tracker.domain.com/1235", "2021-04-09T12:00:00"),
            ],
            std,
        )

    def test_get_task_tracker_updated_since_date(self):
        "Test get_task_tracker_updated_since_date endpoint"
        url = "/api/0/amend/tracker_data?index=%s&apikey=%s&name=%s" % (
            self.index3,
            self.apikey,
            "myttcrawler",
        )
        tracker_data = [
            {
                "crawler_name": "myttcrawler",
                "updated_at": "2021-04-09T13:00:00",
                "change_url": "https://tests.com/unit/repo2/pull/2",
                "issue_type": "RFE",
                "issue_id": "1234",
                "issue_url": "https://issue-tracker.domain.com/1234",
                "issue_title": "Implement feature XYZ",
            },
            {
                "crawler_name": "myttcrawler",
                "updated_at": "2021-04-09T12:00:00",
                "change_url": "https://tests.com/unit/repo2/pull/2",
                "issue_type": "RFE",
                "issue_id": "1235",
                "issue_url": "https://issue-tracker.domain.com/1235",
                "issue_title": "Implement feature XYZ",
            },
            {
                "crawler_name": "myttcrawler",
                "updated_at": "2021-04-09T14:00:00",
                "change_url": "https://tests.com/unit/repo2/pull/3",
                "issue_type": "RFE",
                "issue_id": "1236",
                "issue_url": "https://issue-tracker.domain.com/1236",
                "issue_title": "Implement feature XYZ",
            },
            {
                "crawler_name": "myttcrawler",
                "updated_at": "2021-04-09T16:00:00",
                "change_url": "https://tests.com/unit/repo2/pull/3",
                "issue_type": "RFE",
                "issue_id": "1237",
                "issue_url": "https://issue-tracker.domain.com/1237",
                "issue_title": "Implement feature XYZ",
            },
        ]
        resp = self.client.post(url, json=tracker_data)
        self.assertEqual(200, resp.status_code)
        # Ensure we get the most recent date for the updated_at date
        # of a task tracker task
        resp = self.client.get(
            "/api/0/task_tracker/updated_since_date?index=%s&name=myttcrawler"
            % self.index3
        )
        self.assertEqual(json.loads(resp.data), "2021-04-09T16:00:00")
        # Now let's call the endpoint with an crawler that never sent data
        # The default updated_since date from the configuration must be returned
        resp = self.client.get(
            "/api/0/task_tracker/updated_since_date?index=%s&name=myttcrawler2"
            % self.index3
        )
        self.assertEqual(json.loads(resp.data), "2020-01-01T00:00:00")
        # Now let's see if the called get 404 in case of unknown crawler
        resp = self.client.get(
            "/api/0/task_tracker/updated_since_date?index=%s&name=myttcrawler3"
            % self.index3
        )
        self.assertEqual(404, resp.status_code)
