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

    def test_config_project_def(self):
        "Test get project definitions"
        config_example = """
---
workspaces:
  - index: testindex
    crawler: []
    projects:
      - name: projectdef1
        repository_regex: "test1/somerepo1"
        branch_regex: "master"
      - name: projectdef2
        repository_regex: "test2/test"
  - index: testindex2
    crawler: []
        """
        with tempfile.NamedTemporaryFile() as fp:
            with open(fp.name, "w") as f:
                f.write(config_example)
            env.project_defs = config.build_project_definitions(
                yaml.safe_load(open(fp.name))
            )
            # First try with a non existing index
            resp = self.client.post("/api/1/get_projects", json=dict(index="missing"))
            self.assertEqual(200, resp.status_code)
            # Now fetch projects definition of testindex
            resp = self.client.post("/api/1/get_projects", json=dict(index="testindex"))
            self.assertEqual(200, resp.status_code)
            projects = json.loads(resp.data)["projects"]
            self.assertEqual(2, len(projects))
            self.assertListEqual(
                projects,
                [
                    {
                        "branch_regex": "master",
                        "name": "projectdef1",
                        "repository_regex": "test1/somerepo1",
                    },
                    {
                        "name": "projectdef2",
                        "repository_regex": "test2/test",
                    },
                ],
            )
            p_names = [p["name"] for p in json.loads(resp.data)["projects"]]
            self.assertListEqual(p_names, ["projectdef1", "projectdef2"])
            # Now fetch projects definition of testindex
            resp = self.client.post(
                "/api/1/get_projects", json=dict(index="testindex2")
            )
            self.assertEqual(200, resp.status_code)
            self.assertEqual(0, len(json.loads(resp.data).get("projects", [])))

    def check_APIErr_msg(self, message, resp):
        err = json.loads(resp.data)
        self.assertTrue(err["message"].startswith(message))

    def test_task_data_post(self):
        "Test post on task_data endpoint"
        # First try some faulty requests
        posturl = "/api/1/task_data_add"
        postdata = dict(index=self.index2)

        def _post(body):
            return json.loads(self.client.post(posturl, json=body).data)

        resp = _post(dict(index=self.index2))
        self.assertEqual("UnknownCrawler", resp.get("error"))

        resp = _post(dict(index=self.index2, crawler="myttcrawler", apikey="badkey"))
        self.assertEqual("UnknownApiKey", resp.get("error"))

        postdata = dict(index=self.index2, apikey=self.apikey, crawler="myttcrawler")
        resp = _post(postdata)
        self.assertEqual("AddFailed", resp.get("error"))

        # TODO: handle parse error in webapi
        # postdata.update(dict(
        #    items=list(range(webapp.INPUT_TASK_DATA_LIMIT + 1)),
        # ))
        # resp = _post(postdata)
        # self.assertEqual("AddFailed", resp.get("error"))

        # postdata.update(dict(
        #     items=[{"do": "you", "eat": "that"}]
        # ))
        # resp = _post(postdata)

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
                "ttype": ["RFE"],
                "tid": "1234",
                "url": "https://issue-tracker.domain.com/1234",
                "title": "Implement feature XYZ",
            }
        ]
        postdata.update(dict(items=task_data))
        resp = _post(postdata)
        print("HERE", resp)
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
        self.assertEqual(c_metadata_1["total_change_events_updated"], 5)
        self.assertEqual(c_metadata_1["total_orphans_updated"], 0)
        # Sleep 1s to ensure the next post will get and updated last_post_at date
        # as we have a second granularity
        time.sleep(1)
        # Attempt a new post with an updated task
        task_data = [
            {
                "updated_at": "2021-04-09T13:00:00Z",
                "change_url": "https://tests.com/unit/repo1/pull/1",
                "ttype": ["RFE", "Needed"],
                "tid": "1234",
                "url": "https://issue-tracker.domain.com/1234",
                "title": "Implement feature XYZ",
            },
            {
                "updated_at": "2021-04-09T12:00:00Z",
                "change_url": "https://tests.com/unit/repo1/pull/1",
                "ttype": ["RFE"],
                "tid": "1235",
                "url": "https://issue-tracker.domain.com/1235",
                "title": "Implement feature XYZ",
            },
            {
                "updated_at": "2021-04-09T15:00:00Z",
                "change_url": "https://tests.com/unit/repomissing/pull/1",
                "ttype": ["RFE"],
                "tid": "1235",
                "url": "https://issue-tracker.domain.com/421235",
                "title": "Implement feature XYZ",
            },
        ]
        postdata.update(dict(items=task_data))
        resp = _post(postdata)
        webapp.cache.delete_memoized(webapp.do_query)
        resp = self.client.get(
            "/api/0/query/changes_and_events?index=%s&repository=.*&change_ids=unit@repo1@1"
            % self.index2
        )
        new_objs = json.loads(resp.data)["items"]
        for new_obj in new_objs:
            self.assertIn("tasks_data", new_obj)
            std = [
                (td["url"], td["updated_at"], td["ttype"])
                for td in new_obj["tasks_data"]
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
        self.assertEqual(c_metadata_2["total_change_events_updated"], 10)
        self.assertEqual(c_metadata_2["total_orphans_updated"], 1)

    def test_task_data_commit(self):
        "Test task_data_commit endpoint"
        posturl = "/api/1/task_data_commit"
        postdata = dict(
            index=self.index2,
            apikey=self.apikey,
            crawler="myttcrawler",
        )
        geturl = "/api/1/task_data_get_last_updated"
        getdata = dict(
            index=self.index2,
            crawler="myttcrawler",
        )
        # No previous commit data - return the default date
        resp = self.client.post(geturl, json=getdata)
        self.assertEqual(200, resp.status_code)
        commit_date = json.loads(resp.data)["timestamp"]
        self.assertEqual(commit_date, "2020-01-01T00:00:00Z")
        # Set a commit date and check we can retrieve it
        input_date = "2020-01-01T00:10:00Z"
        postdata["timestamp"] = input_date
        resp = self.client.post(posturl, json=postdata)
        self.assertEqual(200, resp.status_code)
        resp = self.client.post(geturl, json=getdata)
        self.assertEqual(200, resp.status_code)
        commit_date = json.loads(resp.data)["timestamp"]
        self.assertEqual(commit_date, input_date)
        # Set a new commit date and check we can retrieve it
        new_date = "2020-01-01T01:00:00Z"
        postdata["timestamp"] = new_date
        resp = self.client.post(posturl, json=postdata)
        resp = self.client.post(geturl, json=getdata)
        commit_date = json.loads(resp.data)["timestamp"]
        self.assertEqual(commit_date, new_date)
