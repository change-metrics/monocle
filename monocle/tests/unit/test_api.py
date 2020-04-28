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
from flask import json
from monocle import webapp


class TestWebAPI(unittest.TestCase):
    def setUp(self):
        webapp.app.config['TESTING'] = True
        self.client = webapp.app.test_client()

    def test_webapp(self):
        "Test indices endpoint"
        resp = self.client.get('/api/0/indices')
        self.assertListEqual(["monocle"], json.loads(resp.data))
