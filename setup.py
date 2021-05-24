#!/usr/bin/env python

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

from setuptools import setup

setup(
    name="monocle",
    version="0.9.0",
    packages=[
        "monocle",
        "monocle.db",
        "monocle.gerrit",
        "monocle.github",
        "monocle.messages",
        "monocle.migrate",
    ],
    entry_points={
        "console_scripts": ["monocle=monocle.main:main", "webapi=monocle.webapp:main"]
    },
)
