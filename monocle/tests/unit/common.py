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

import os
import json
import pprint

FIXTURES_DIR = os.path.join(os.path.dirname(os.path.realpath(__file__)), "fixtures")
DATASETS = os.path.join(FIXTURES_DIR, "datasets")


def load_dataset(name):
    with open(os.path.join(DATASETS, name)) as fd:
        data = json.load(fd)
    return data


def load_change(name):
    input_pr = load_dataset(name + "_raw.json")
    xtrd_ref = load_dataset(name + "_extracted.json")
    return input_pr, xtrd_ref


def index_dataset(eldb, name):
    data = load_dataset(name)
    eldb.update(data)


class DiffException(Exception):
    def __init__(self, message):
        super().__init__()
        self.message = pprint.pformat(message)

    def __str__(self):
        return "\n\n" + self.message
