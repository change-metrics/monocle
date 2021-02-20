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
from monocle.basecrawler import create_ident_dict
from monocle import utils

from typing import Dict, List, Optional


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


def missing_url_gerrit_events(elastic_conn, index) -> None:
    bulk_size = 500
    client = ELmonocleDB(elastic_conn, index, previous_schema=True)
    url_cache: Dict[str, str] = {}

    def get_change_url(obj: Dict) -> str:
        change_id = obj["change_id"]
        if change_id in url_cache:
            return url_cache[change_id]
        # Set a large size to avoid pagination, we do not expect more than 10000
        # objects (change + events) for a given change
        params = {"change_ids": [change_id], "size": 10000, "from": 0}
        result = client.run_named_query("changes_and_events", ".*", params=params)
        changes = list(filter(lambda obj: obj["type"] == "Change", result["items"]))
        if len(changes) != 1:
            raise RuntimeError("Wrong unicity for change %s" % change_id)
        change = changes[0]
        url = change["url"]
        url_cache[change_id] = url
        return url

    def bulk_update(to_update: List) -> List:
        print("Updating %s events ..." % len(to_update))
        client.update(to_update)
        return []

    to_update = []
    for _obj in client.iter_index():
        obj = _obj["_source"]
        if obj["type"] in utils.get_events_list() and "url" not in obj.keys():
            url = get_change_url(obj)
            obj["url"] = url
            to_update.append(dict_to_change_or_event(obj))
            if len(to_update) == bulk_size:
                to_update = bulk_update(to_update)

    bulk_update(to_update)


def to_idents(elastic_conn, index) -> None:
    bulk_size = 500
    client = ELmonocleDB(elastic_conn, index, previous_schema=True)
    client2 = ELmonocleDB(elastic_conn, index)

    def bulk_update(to_update: List) -> List:
        print("Updating %s objects ..." % len(to_update))
        client2.update(to_update)
        return []

    def update_ident(obj: Dict) -> Dict:

        url = obj["url"]

        def to_ident(value: Optional[str]) -> Optional[Dict]:
            if value:
                return create_ident_dict(url, value)
            return None

        if obj["type"] == "Change":
            obj["author"] = to_ident(obj["author"])
            obj["committer"] = to_ident(obj.get("committer"))
            obj["merged_by"] = to_ident(obj.get("merged_by"))
            obj["assignees"] = list(map(to_ident, obj.get("assignees", [])))
            for commit in obj.get("commits", []):
                commit["author"] = to_ident(commit["author"])
                commit["committer"] = to_ident(commit["committer"])
        else:
            obj["author"] = to_ident(obj.get("author"))
            obj["on_author"] = to_ident(obj.get("on_author"))

        return obj

    to_update = []
    for _obj in client.iter_index():
        d = update_ident(_obj["_source"])
        obj = dict_to_change_or_event(d)
        to_update.append(obj)
        if len(to_update) == bulk_size:
            to_update = bulk_update(to_update)

    bulk_update(to_update)


def run_migrate(name, elastic_conn, index):
    if name not in processes:
        raise NotAvailableException()
    processes[name](elastic_conn, index)


processes = {
    "self-merge": self_merge,
    "missing-url-gerrit-events": missing_url_gerrit_events,
    "to_idents": to_idents,
}
