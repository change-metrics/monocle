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

from monocle.db.db import ELmonocleDB
from monocle.db.db import dict_to_change_or_event


class NotAvailableException(Exception):
    pass


def self_merge(elastic_conn, index) -> None:
    to_update = []
    bulk_size = 500

    def update_change(change):
        if "self_merged" not in change.keys():
            if "merged_by" not in change:
                # Here we fix the missing field that can happen with the Gerrit crawler
                change["merged_by"] = None
            if change["merged_by"]:
                change["self_merged"] = change["merged_by"] == change["author"]
            else:
                change["self_merged"] = None
            return True

    client = ELmonocleDB(elastic_conn, index)
    for _obj in client.iter_index():
        obj = _obj["_source"]
        if obj["type"] == "Change":
            updated = update_change(obj)
            if updated:
                to_update.append(dict_to_change_or_event(obj))
            if len(to_update) == bulk_size:
                print("Updating %s changes ..." % bulk_size)
                client.update(to_update)
                to_update = []


def run_migrate(name, elastic_conn, index):
    if name not in processes:
        raise NotAvailableException()
    processes[name](elastic_conn, index)


processes = {"self-merge": self_merge}
