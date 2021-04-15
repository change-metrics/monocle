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

from typing import Tuple, Dict, List, Any

from monocle.db.db import dict_to_change_or_event, ELmonocleDB

FIXTURES_DIR = os.path.join(os.path.dirname(os.path.realpath(__file__)), "fixtures")
DATASETS = os.path.join(FIXTURES_DIR, "datasets")


def get_db_cnx(index: str, prefix: str) -> ELmonocleDB:
    return ELmonocleDB(
        index=index,
        prefix=prefix,
        user=os.getenv("ELASTIC_USER", None),
        password=os.getenv("ELASTIC_PASSWORD", None),
        use_ssl=os.getenv("ELASTIC_USE_SSL", False),
        verify_certs=os.getenv("ELASTIC_INSECURE", None),
        ssl_show_warn=os.getenv("ELASTIC_SSL_SHOW_WARN", None),
    )


def load_dataset(name: str) -> Any:
    with open(os.path.join(DATASETS, name)) as fd:
        data = json.load(fd)
    return data


def load_change(name: str) -> Tuple[Dict, List[Dict]]:
    input_pr = load_dataset(name + "_raw.json")
    xtrd_ref = load_dataset(name + "_extracted.json")
    return input_pr, xtrd_ref


def index_dataset(eldb, name) -> None:
    _data: List[Dict] = load_dataset(name)
    data = [dict_to_change_or_event(x) for x in _data]
    eldb.update(data)


class DiffException(Exception):
    def __init__(self, message):
        super().__init__()
        self.message = pprint.pformat(message)

    def __str__(self):
        return "\n\n" + self.message
