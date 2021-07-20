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

import unittest

import yaml

from monocle import config


legacy_config_sample_yaml = """
---
tenants:
  - index: default
    task_crawlers:
      - name: crawler
        updated_since: "2020-01-01"
        api_key: 1a2b3c4d5e
    users:
      - john
      - jane
    crawler:
      loop_delay: 300
      github_orgs:
        - name: git
          updated_since: "2020-01-01"
          token: "123"
          base_url: https://github.com
        - name: bitcoin
          updated_since: "2020-01-01"
          token: "123"
          base_url: https://github.com
  - index: tenant1
    idents:
      - ident: john
        aliases:
          - github.com/john
          - github.domain.org/john
          - review.opendev.org/John Doe/12345
    crawler:
      loop_delay: 300
      github_orgs:
        - name: docker
          updated_since: "2020-01-01"
          token: "123"
          base_url: https://github.com
        - name: tekton
          updated_since: "2020-01-01"
          token: "123"
          base_url: https://github.com
      gerrit_repositories:
        - name: ^soft.*
          updated_since: "2020-01-01"
          base_url: https://softwarefactory-project.io/r
        - name: ^rpms/.*
          updated_since: "2020-01-01"
          base_url: https://softwarefactory-project.io/r
          insecure: true
          login: fabien
          password: secure
          prefix: namespace/
    projects:
      - name: infra
        repository_regex: "config|infra|openstack/.*"
        branch_regex: "master|devel"
      - name: infra-doc
        repository_regex: "config|infra|openstack/.*"
        branch_regex: "master|devel"
        file_regex: "doc[s]/.*"
"""

config_sample_yaml = """
---
workspaces:
  - index: default
    users:
      - john
      - jane
    crawlers_api_key: "1a2b3c4d5e"
    crawlers:
      - name: github-git
        update_since: "2020-01-01"
        provider:
          github_token: "123"
          github_organization: git

      - name: github-bitcoin
        update_since: "2020-01-01"
        provider:
          github_token: "123"
          github_organization: bitcoin

      - name: crawler
        update_since: "2020-01-01"
        provider: TaskDataProvider

  - index: tenant1
    crawlers_api_key: "CHANGE_ME"
    crawlers:
      - name: github-docker
        update_since: "2020-01-01"
        provider:
          github_token: "123"
          github_organization: docker

      - name: github-tekton
        update_since: "2020-01-01"
        provider:
          github_token: "123"
          github_organization: tekton

      - name: gerrit-1
        update_since: "2020-01-01"
        provider:
          gerrit_url: https://softwarefactory-project.io/r
          gerrit_repositories:
            - ^soft.*

      - name: gerrit-2
        update_since: "2020-01-01"
        provider:
          gerrit_url: https://softwarefactory-project.io/r
          gerrit_url_insecure: true
          gerrit_login: fabien
          gerrit_password: secure
          gerrit_prefix: namespace/
          gerrit_repositories:
            - ^rpms/.*

    idents:
      - ident: john
        aliases:
          - github.com/john
          - github.domain.org/john
          - review.opendev.org/John Doe/12345
    projects:
      - name: infra
        repository_regex: "config|infra|openstack/.*"
        branch_regex: "master|devel"
      - name: infra-doc
        repository_regex: "config|infra|openstack/.*"
        branch_regex: "master|devel"
        file_regex: "doc[s]/.*"
"""


class TestSchemas(unittest.TestCase):
    maxDiff = None

    def test_config_schema(self):
        config.validate(yaml.safe_load(legacy_config_sample_yaml), config.schema)
        config.validate(yaml.safe_load(config_sample_yaml), config.schema)

    def test_config_upgrade(self):
        """test adapting legacy_config into config"""
        adapted_config = config.loadUpgrade(legacy_config_sample_yaml)
        self.assertEqual(adapted_config, yaml.safe_load(config_sample_yaml))

    def test_config_downgrade(self):
        """test adapting config into legacy_config"""
        new_config = config.load(config_sample_yaml)
        self.assertEqual(new_config, yaml.safe_load(legacy_config_sample_yaml))

    def test_get_ident_config(self):
        idents_config = config.get_idents_config(
            yaml.safe_load(legacy_config_sample_yaml), "tenant1"
        )
        self.assertEqual(len(idents_config), 1)
        self.assertEqual(idents_config[0].ident, "john")
        self.assertListEqual(
            idents_config[0].aliases,
            [
                "github.com/john",
                "github.domain.org/john",
                "review.opendev.org/John Doe/12345",
            ],
        )
        idents_config = config.get_idents_config(
            yaml.safe_load(legacy_config_sample_yaml), "default"
        )
        self.assertEqual(len(idents_config), 0)
