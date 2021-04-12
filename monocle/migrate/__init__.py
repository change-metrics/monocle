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

from urllib.parse import urlparse

from monocle.db.db import ELmonocleDB
from monocle.db.db import dict_to_change_or_event
from monocle.ident import prefix, create_muid_from_uid
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


def string_ident_to_ident(elastic_conn, index) -> None:
    bulk_size = 7500
    client = ELmonocleDB(elastic_conn, index, previous_schema=True)
    client2 = ELmonocleDB(elastic_conn, index)
    changes_url_lookup: Dict[str, str] = {}
    to_update: List = []
    need_url_update: List[Dict] = []
    total_objects_updated = 0

    def bulk_update(to_update: List) -> None:
        client2.update(to_update)

    def update_changes_url_lookup(objs: List[Dict]) -> None:
        change_ids = [o["change_id"] for o in objs]
        change_ids = list(set(change_ids))
        change_ids = [_id for _id in change_ids if _id not in changes_url_lookup]
        print("Updating change_url_lookup for %s changes ..." % len(change_ids))
        params = {"change_ids": change_ids, "size": 10000, "from": 0}
        result = client.run_named_query("changes", ".*", params=params)
        changes = result["items"]
        for change in changes:
            changes_url_lookup[change["change_id"]] = change["url"]
        print("%s entries in changes_url_lookup" % len(changes_url_lookup))

    def update_ident(obj: Dict) -> Dict:

        url = obj["url"]

        def update_approval_type(approval):
            if isinstance(approval, str):
                ret = [approval]
            else:
                ret = approval
            return [r for r in ret if r is not None]

        def create_ident_dict(url: str, uid: str) -> Dict:
            domain = urlparse(url).netloc
            uid = prefix(domain, uid)
            return {
                "uid": uid,
                "muid": create_muid_from_uid(uid),
            }

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
                # Also fix commit's author that might be not exists
                if "author" not in commit.keys():
                    commit["author"] = obj["author"]
                else:
                    commit["author"] = to_ident(commit["author"])
                # Also fix commit's committer that might be not exists
                if "committer" not in commit.keys():
                    commit["committer"] = commit["author"]
                else:
                    commit["committer"] = to_ident(commit["committer"])
        else:
            obj["author"] = to_ident(obj.get("author"))
            obj["on_author"] = to_ident(obj.get("on_author"))
            # Also fix missing created_at date on ChangeCommitPushedEvent
            if obj["type"] == "ChangeCommitPushedEvent" and obj["created_at"] is None:
                obj["created_at"] = obj["on_created_at"]
        # Also fix approval format if needed
        if obj.get("approval"):
            obj["approval"] = update_approval_type(obj["approval"])

        return obj

    def proceed():
        if need_url_update:
            update_changes_url_lookup(need_url_update)
        for o in to_update:
            if o in need_url_update:
                if o["change_id"] in changes_url_lookup:
                    o["url"] = changes_url_lookup[o["change_id"]]
                else:
                    print("Warning - unable to find change %s" % o["change_id"])
                    o["url"] = "https://undefined"
        updated = list(map(update_ident, to_update))
        print("Updating %s objects ..." % len(to_update))
        bulk_update(list(map(dict_to_change_or_event, updated)))

    for _obj in client.iter_index():
        obj = _obj["_source"]
        if obj["type"] in utils.get_events_list() and "url" not in obj.keys():
            need_url_update.append(obj)
        if obj["type"] in utils.get_events_list() + ["Change"]:
            to_update.append(obj)

        if len(to_update) == bulk_size:
            proceed()
            total_objects_updated += len(to_update)
            print("Total objects updated: %s" % total_objects_updated)
            need_url_update = []
            to_update = []

    proceed()
    total_objects_updated += len(to_update)
    print("Total objects updated: %s" % total_objects_updated)


def run_migrate(name, elastic_conn, index):
    if name not in processes:
        raise NotAvailableException()
    processes[name](elastic_conn, index)


processes = {"self-merge": self_merge, "from-0.8-to-last-stable": string_ident_to_ident}
