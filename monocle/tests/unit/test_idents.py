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

from monocle import ident


class TestIdent(unittest.TestCase):
    def test_create_ident(self):
        idents_config = [
            ident.IdentConfig(
                ident="john", aliases=["github.com/john", "github.corp.org/john"]
            ),
            ident.IdentConfig(
                ident="jane",
                aliases=[
                    "github.com/jane",
                    "github.corp.org/janette",
                    "review.opendev.org/Jane Doe/12345",
                ],
            ),
        ]

        created_ident = ident.create_ident("https://github.com", "john", idents_config)
        self.assertEqual(created_ident.uid, "github.com/john")
        self.assertEqual(created_ident.muid, "john")

        created_ident = ident.create_ident("https://github.com", "fabio", idents_config)
        self.assertEqual(created_ident.uid, "github.com/fabio")
        self.assertEqual(created_ident.muid, "fabio")

        created_ident = ident.create_ident(
            "https://review.opendev.org", "Jane Doe/12345", idents_config
        )
        self.assertEqual(created_ident.uid, "review.opendev.org/Jane Doe/12345")
        self.assertEqual(created_ident.muid, "jane")

        created_ident = ident.create_ident(
            "https://github.corp.org", "janette", idents_config
        )
        self.assertEqual(created_ident.uid, "github.corp.org/janette")
        self.assertEqual(created_ident.muid, "jane")
