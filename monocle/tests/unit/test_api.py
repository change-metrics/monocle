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
import tempfile
import time
import unittest
import yaml
from flask import json

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
            "tenants": [
                {
                    # Private index
                    "index": self.index1,
                    "users": ["jane", "john"],
                },
                {
                    # Public index
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
        webapp.indexes_acl = config.build_index_acl(config_data)
        webapp.indexes_task_crawlers = config.build_index_task_crawlers(config_data)

    def test_health(self):
        "Test health endpoint"
        resp = self.client.get("/api/0/health")
        for key in ("hostname", "status", "timestamp"):
            self.assertIn(key, json.loads(resp.data))

    def test_get_indices(self):
        "Test indices endpoint"
        resp = self.client.get("/api/0/indices")
        self.assertListEqual(["unittest-2"], json.loads(resp.data))

    def test_get_indices_with_acl(self):
        "Test indices endpoint with acl"
        with self.client.session_transaction() as sess:
            sess["username"] = "jane"
        resp = self.client.get("/api/0/indices")
        self.assertListEqual(
            ["unittest-1", "unittest-2"],
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

    def check_APIErr_msg(self, message, resp):
        err = json.loads(resp.data)
        self.assertTrue(err["message"].startswith(message))

    def test_task_data_post(self):
        "Test post on task_data endpoint"
        # First try some faulty requests
        resp = self.client.post("/api/0/task_data?index=%s" % self.index2, json="")
        self.assertEqual(404, resp.status_code)
        self.check_APIErr_msg("No crawler name provided", resp)

        resp = self.client.post(
            "/api/0/task_data?index=%s&apikey=badkey" % self.index2, json=""
        )
        self.assertEqual(404, resp.status_code)
        self.check_APIErr_msg("No crawler name provided", resp)

        resp = self.client.post(
            "/api/0/task_data?index=%s&apikey=badkey&name=myttcrawler" % self.index2,
            json="",
        )
        self.assertEqual(403, resp.status_code)
        self.check_APIErr_msg("Not authorized", resp)

        url = "/api/0/task_data?index=%s&apikey=%s&name=%s" % (
            self.index2,
            self.apikey,
            "myttcrawler",
        )

        resp = self.client.post(url, json="data")
        self.assertEqual(400, resp.status_code)
        self.check_APIErr_msg("Input data is not a List", resp)

        resp = self.client.post(
            url,
            json=list(range(webapp.INPUT_TASK_DATA_LIMIT + 1)),
        )
        self.assertEqual(400, resp.status_code)
        self.check_APIErr_msg("Input data List over limit (500 items)", resp)

        resp = self.client.post(
            url,
            json=[{"do": "you", "eat": "that"}],
        )
        self.assertEqual(400, resp.status_code)
        self.check_APIErr_msg(
            (
                "Unable to extract input data due to wrong input format: "
                "Missing mandatory field:",
            ),
            resp,
        )

        # Now test a working workflow
        resp = self.client.get(
            "/api/0/query/changes?index=%s&repository=.*&change_ids=unit@repo1@1"
            % self.index2
        )
        orig = json.loads(resp.data)["items"][0]
        self.assertNotIn("task_data", orig)
        # Do a first post of task_data
        task_data = [
            {
                "updated_at": "2021-04-09T12:00:00Z",
                "change_url": "https://tests.com/unit/repo1/pull/1",
                "issue_type": ["RFE"],
                "issue_id": "1234",
                "issue_url": "https://issue-tracker.domain.com/1234",
                "issue_title": "Implement feature XYZ",
            }
        ]
        resp = self.client.post(url, json=task_data)
        self.assertEqual(200, resp.status_code)
        webapp.cache.delete_memoized(webapp.do_query)
        resp = self.client.get(
            "/api/0/query/changes?index=%s&repository=.*&change_ids=unit@repo1@1"
            % self.index2
        )
        new = json.loads(resp.data)["items"][0]
        self.assertIn("tasks_data", new)
        # Check if crawler metadata have been updated
        resp = self.client.get(
            "/api/0/task_data?index=%s&name=%s&details=true"
            % (self.index2, "myttcrawler")
        )
        c_metadata_1 = json.loads(resp.data)
        self.assertEqual(c_metadata_1["total_docs_posted"], 1)
        self.assertEqual(c_metadata_1["total_changes_updated"], 1)
        self.assertEqual(c_metadata_1["total_orphans_updated"], 0)
        # Sleep 1s to ensure the next post will get and updated last_post_at date
        # as we have a second granularity
        time.sleep(1)
        # Attempt a new post with an updated task
        task_data = [
            {
                "updated_at": "2021-04-09T13:00:00Z",
                "change_url": "https://tests.com/unit/repo1/pull/1",
                "issue_type": ["RFE", "Needed"],
                "issue_id": "1234",
                "issue_url": "https://issue-tracker.domain.com/1234",
                "issue_title": "Implement feature XYZ",
            },
            {
                "updated_at": "2021-04-09T12:00:00Z",
                "change_url": "https://tests.com/unit/repo1/pull/1",
                "issue_type": ["RFE"],
                "issue_id": "1235",
                "issue_url": "https://issue-tracker.domain.com/1235",
                "issue_title": "Implement feature XYZ",
            },
            {
                "updated_at": "2021-04-09T15:00:00Z",
                "change_url": "https://tests.com/unit/repomissing/pull/1",
                "issue_type": ["RFE"],
                "issue_id": "1235",
                "issue_url": "https://issue-tracker.domain.com/421235",
                "issue_title": "Implement feature XYZ",
            },
        ]
        resp = self.client.post(url, json=task_data)
        self.assertEqual(200, resp.status_code)
        webapp.cache.delete_memoized(webapp.do_query)
        resp = self.client.get(
            "/api/0/query/changes?index=%s&repository=.*&change_ids=unit@repo1@1"
            % self.index2
        )
        new = json.loads(resp.data)["items"][0]
        self.assertIn("tasks_data", new)
        std = [
            (td["issue_url"], td["updated_at"], td["issue_type"])
            for td in new["tasks_data"]
        ]
        self.assertListEqual(
            [
                (
                    "https://issue-tracker.domain.com/1234",
                    "2021-04-09T13:00:00",
                    ["RFE", "Needed"],
                ),
                (
                    "https://issue-tracker.domain.com/1235",
                    "2021-04-09T12:00:00",
                    ["RFE"],
                ),
            ],
            std,
        )
        # Check if crawler metadata have been updated
        resp = self.client.get(
            "/api/0/task_data?index=%s&name=%s&details=true"
            % (self.index2, "myttcrawler")
        )
        c_metadata_2 = json.loads(resp.data)
        self.assertNotEqual(c_metadata_1["last_post_at"], c_metadata_2["last_post_at"])
        self.assertEqual(c_metadata_2["total_docs_posted"], 4)
        self.assertEqual(c_metadata_2["total_changes_updated"], 2)
        self.assertEqual(c_metadata_2["total_orphans_updated"], 1)

    def test_task_data_commit(self):
        "Test task_data_commit endpoint"
        posturl = "/api/0/task_data/commit?index=%s&apikey=%s&name=%s" % (
            self.index2,
            self.apikey,
            "myttcrawler",
        )
        geturl = "/api/0/task_data?index=%s&name=%s" % (
            self.index2,
            "myttcrawler",
        )
        # No previous commit data - return the default date
        resp = self.client.get(geturl)
        self.assertEqual(200, resp.status_code)
        commit_date = json.loads(resp.data)
        self.assertEqual(commit_date, "2020-01-01T00:00:00Z")
        # Set a commit date and check we can retrieve it
        input_date = "2020-01-01T00:10:00Z"
        resp = self.client.post(posturl, json=input_date)
        self.assertEqual(200, resp.status_code)
        resp = self.client.get(geturl)
        self.assertEqual(200, resp.status_code)
        commit_date = json.loads(resp.data)
        self.assertEqual(commit_date, input_date)
        # Set a new commit date and check we can retrieve it
        input_date = "2020-01-01T01:00:00Z"
        resp = self.client.post(posturl, json=input_date)
        resp = self.client.get(geturl)
        commit_date = json.loads(resp.data)
        self.assertEqual(commit_date, input_date)
