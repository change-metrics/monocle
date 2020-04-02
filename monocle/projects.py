# MIT License
# Copyright (c) 2019 Fabien Boucher

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


import yaml
from jsonschema import validate

schema = {
    "$schema": "http://json-schema.org/draft-07/schema#",
    "definitions": {
        "github_organization": {
            "$id": "http://monocle/github_org.schema.json",
            "title": "Github organization",
            "description": "A github organization description for the crawler",
            "type": "object",
            "required": ["name", "updated_since", "base_url", "token"],
            "properties": {
                "name": {"description": "The organization name", "type": "string"},
                "repository": {
                    "description": "The repository name within the organization",
                    "type": "string",
                },
                "updated_since": {
                    "description": "The change updated since date (YYYY-mm-dd)",
                    "type": "string",
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
                },
                "base_url": {
                    "description": "Base url of the Gerrit instance",
                    "type": "string",
                },
            },
        },
    },
    "type": "object",
    "required": ["projects"],
    "properties": {
        "projects": {
            "type": "array",
            "items": {
                "type": "object",
                "required": ["name", "crawler"],
                "properties": {
                    "name": {"type": "string"},
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
                },
            },
        }
    },
}

projects_sample_yaml = """
---
projects:
  - name: project1
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
  - name: project2
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
"""


def test(self, path):
    self.path = path
    validate(instance=yaml.safe_load(self.projects_sample_yaml), schema=self.schema)


if __name__ == "__main__":
    test()
