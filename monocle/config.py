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

from dataclasses import dataclass
from typing import Dict, List
from jsonschema import validate as schema_validate
from jsonschema import draft7_format_checker

from monocle.ident import IdentsConfig, ident_from_config
from monocle.task_data import TaskTrackerCrawler, createTaskTrackerCrawler

schema = {
    "$schema": "http://json-schema.org/draft-07/schema#",
    "definitions": {
        "github_organization": {
            "$id": "http://monocle/github_org.schema.json",
            "title": "Github organization",
            "description": "A github organization description for the crawler",
            "type": "object",
            "required": ["name", "updated_since", "base_url"],
            "properties": {
                "name": {"description": "The organization name", "type": "string"},
                "repository": {
                    "description": "The repository name within the organization",
                    "type": "string",
                },
                "updated_since": {
                    "description": "The change updated since date (YYYY-mm-dd)",
                    "type": "string",
                    "format": "date",
                },
                "base_url": {
                    "description": "Base url of the Github instance",
                    "type": "string",
                },
                "token": {
                    "description": "The API token to access the API",
                    "type": "string",
                },
            },
        },
        "gerrit_repository": {
            "$id": "http://monocle/gerrit_repository.schema.json",
            "title": "Gerrit repository",
            "description": "A gerrit repository description for the crawler",
            "type": "object",
            "required": ["name", "updated_since", "base_url"],
            "properties": {
                "name": {
                    "description": "The repository name or regexp",
                    "type": "string",
                },
                "updated_since": {
                    "description": "The change updated since date (YYYY-mm-dd)",
                    "type": "string",
                    "format": "date",
                },
                "base_url": {
                    "description": "Base url of the Gerrit instance",
                    "type": "string",
                },
                "prefix": {
                    "description": "Repository name prefix in case of name collision",
                    "type": "string",
                },
                "insecure": {
                    "description": "Set to true to bypass the HTTP X509 certificate verification",
                    "type": "boolean",
                },
                "login": {
                    "description": "Login to use to authenticate on Gerrit",
                    "type": "string",
                },
                "password": {
                    "description": "Password to use to authenticate on Gerrit",
                    "type": "string",
                },
            },
        },
    },
    "type": "object",
    "required": ["tenants"],
    "properties": {
        "tenants": {
            "type": "array",
            "items": {
                "type": "object",
                "required": ["index", "crawler"],
                "properties": {
                    "index": {
                        "type": "string",
                        "description": "Elasticsearch index name",
                    },
                    "users": {
                        "description": "User authorized to see and access the index",
                        "type": "array",
                        "items": {"type": "string"},
                    },
                    "idents": {
                        "description": "Identity aliases",
                        "type": "array",
                        "items": {
                            "type": "object",
                            "required": ["ident", "aliases"],
                            "properties": {
                                "ident": {"type": "string"},
                                "aliases": {
                                    "type": "array",
                                    "items": {"type": "string"},
                                },
                            },
                        },
                    },
                    "task_tracker_crawlers": {
                        "description": "Task tracker crawlers authorized to act on that index",
                        "type": "array",
                        "items": {
                            "type": "object",
                            "required": ["name", "api_key", "updated_since"],
                            "properties": {
                                "name": {"type": "string"},
                                "updated_since": {"type": "string", "format": "date"},
                                "api_key": {"type": "string"},
                            },
                        },
                    },
                    "crawler": {
                        "type": "object",
                        "properties": {
                            "loop_delay": {"type": "integer"},
                            "github_orgs": {
                                "type": "array",
                                "items": {"$ref": "#/definitions/github_organization"},
                            },
                            "gerrit_repositories": {
                                "type": "array",
                                "items": {"$ref": "#/definitions/gerrit_repository"},
                            },
                        },
                    },
                    "projects": {
                        "description": "Project definition",
                        "type": "array",
                        "items": {
                            "type": "object",
                            "properties": {
                                "name": {"type": "string"},
                                "repositories_regex": {"type": "string"},
                            },
                        },
                    },
                },
            },
        }
    },
}

config_sample_yaml = """
---
tenants:
  - index: default
    task_tracker_crawlers:
      - name: crawler
        updated_since: "2020-01-01"
        api_key: 1a2b3c4d5e
    users:
      - john
      - jane
    crawler:
      loop_delay: 10
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
      loop_delay: 10
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
        repositories_regex: "^config|infra$|openstack/.*"
"""


def validate(data, schema):
    schema_validate(
        instance=data,
        schema=schema,
        format_checker=draft7_format_checker,
    )


class Username(str):
    pass


@dataclass
class ProjectDefinition(str):
    name: str
    repositories_regex: str


def build_index_acl(config: dict) -> Dict[str, List[Username]]:
    indexes_acl: Dict[str, List[Username]] = {}
    for tenant in config["tenants"]:
        if "users" not in tenant.keys():
            indexes_acl[tenant["index"]] = []
        else:
            indexes_acl[tenant["index"]] = tenant["users"]
    return indexes_acl


def build_index_task_tracker_crawlers(
    config: dict,
) -> Dict[str, List[TaskTrackerCrawler]]:
    ret = {}
    for tenant in config["tenants"]:
        if "task_tracker_crawlers" in tenant.keys():
            ret[tenant["index"]] = [
                createTaskTrackerCrawler(entry)
                for entry in tenant["task_tracker_crawlers"]
            ]
    return ret


def is_public_index(indexes_acl: Dict[str, List[Username]], index_name: str) -> bool:
    if not indexes_acl.get(index_name, []):
        return True
    else:
        return False


def get_authorized_users(
    indexes_acl: Dict[str, List[Username]], index_name: str
) -> List[Username]:
    return indexes_acl.get(index_name, [])


def get_idents_config(config: dict, index_name: str) -> IdentsConfig:
    matches = list(
        filter(lambda tenant: tenant["index"] == index_name, config["tenants"])
    )
    if len(matches) > 0:
        return list(map(ident_from_config, matches[0].get("idents", [])))
    else:
        return []


def build_project_definitions(config: dict) -> Dict[str, List[ProjectDefinition]]:
    indexes_project_def: Dict[str, List[ProjectDefinition]] = {}
    for tenant in config["tenants"]:
        if "projects" not in tenant.keys():
            indexes_project_def[tenant["index"]] = []
        else:
            indexes_project_def[tenant["index"]] = tenant["projects"]
    return indexes_project_def
