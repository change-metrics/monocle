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
from jsonschema import validate
from deepdiff import DeepDiff
from .common import DiffException

from monocle import projects


class TestSchemas(unittest.TestCase):
    def test_projects_schema(self):
        validate(
            instance=yaml.safe_load(projects.projects_sample_yaml),
            schema=projects.schema,
        )

    def test_indexes_acl(self):
        indexes_acl = projects.build_index_acl(
            yaml.safe_load(projects.projects_sample_yaml)
        )
        expected = {'default': ['john', 'jane'], 'tenant1': []}
        ddiff = DeepDiff(indexes_acl, expected)
        if ddiff:
            raise DiffException(ddiff)

        self.assertTrue(projects.is_public_index(indexes_acl, 'tenant1'))
        self.assertFalse(projects.is_public_index(indexes_acl, 'default'))

        users = projects.get_authorized_users(indexes_acl, 'default')
        self.assertListEqual(users, ['john', 'jane'])
