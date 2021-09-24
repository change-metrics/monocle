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

import logging
import socket
import time
from datetime import datetime
from os import environ

from typing import List, Optional, Literal, Dict, Union, Tuple
from dataclasses import dataclass, asdict

from dacite import from_dict

from elasticsearch.helpers import bulk, BulkIndexError
from elasticsearch.helpers import scan
from elasticsearch.client import Elasticsearch
from elasticsearch.exceptions import NotFoundError
from elasticsearch.helpers import scan as scanner

from monocle.db import queries
from monocle.ident import Ident, IdentsConfig, create_muid
from monocle.utils import get_events_list
from monocle.task_data import (
    TaskDataForEL,
    OrphanTaskDataForEL,
    AdoptedTaskData,
    AdoptedTaskDataForEL,
    createELTaskData,
)


CHANGE_PREFIX = "monocle.changes.1."
PREV_CHANGE_PREFIX = "monocle.changes."


class UnknownQueryException(Exception):
    pass


InvalidIndexError = NotFoundError


@dataclass
class File:
    additions: int
    deletions: int
    path: str


@dataclass
class SimpleFile:
    path: str


@dataclass
class Commit:
    sha: str
    author: Ident
    committer: Ident
    authored_at: str  # eg. 2020-04-11T07:01:15Z
    committed_at: str  # eg. 2020-04-11T07:01:15Z
    additions: int
    deletions: int
    title: str


# Must be splitted in two record type
# Change and Event
@dataclass
class Change:
    """A Change record"""

    _id: str
    _type: Literal["Change"]
    number: int
    change_id: str
    title: Optional[str]
    text: Optional[str]
    url: str
    commit_count: Optional[int]
    additions: Optional[int]
    deletions: Optional[int]
    changed_files_count: Optional[int]
    changed_files: List[File]
    commits: Optional[List[Commit]]
    repository_prefix: str
    repository_fullname: str
    repository_shortname: str
    author: Ident
    committer: Optional[Ident]
    merged_by: Optional[Ident]
    branch: str
    target_branch: str
    created_at: str  # eg. 2020-04-11T07:01:15Z
    merged_at: Optional[str]  # eg. 2020-04-11T07:01:15Z
    updated_at: Optional[str]  # eg. 2020-04-11T07:01:15Z
    closed_at: Optional[str]  # eg. 2020-04-11T07:01:15Z
    state: Optional[Literal["OPEN", "CLOSED", "MERGED"]]
    duration: Optional[int]
    mergeable: Optional[str]
    labels: Optional[List[str]]
    assignees: Optional[List[Ident]]
    approval: Optional[List[str]]
    draft: Optional[bool]
    self_merged: Optional[bool]


@dataclass
class Event:
    """An Event record"""

    _id: str
    _type: Literal[
        "ChangeCreatedEvent",
        "ChangeCommentedEvent",
        "ChangeAbandonedEvent",
        "ChangeReviewedEvent",
        "ChangeCommitForcePushedEvent",
        "ChangeCommitPushedEvent",
        "ChangeMergedEvent",
    ]
    created_at: str  # eg. 2020-04-11T07:01:15Z
    author: Optional[Ident]  # ChangeMergedEvent on Gerrit can have an optional author
    repository_prefix: str
    repository_fullname: str
    repository_shortname: str
    branch: str
    target_branch: str
    number: int
    change_id: str
    url: str
    on_author: Optional[Ident]
    on_created_at: Optional[str]  # eg. 2020-04-11T07:01:15Z
    changed_files: List[SimpleFile]
    approval: Optional[List[str]]


def change_or_event_to_dict(change: Union[Change, Event]) -> Dict:
    d = asdict(change)
    for k1, k2 in (("id", "_id"), ("type", "_type")):
        d[k1] = d[k2]
        del d[k2]
    # Remove None attributes
    return dict([(k, v) for k, v in d.items() if v is not None])


def dict_to_change_or_event(d: Dict) -> Union[Change, Event]:
    _type = d["type"]
    for k1, k2 in (("id", "_id"), ("type", "_type")):
        d[k2] = d[k1]
        del d[k1]
    if _type == "Change":
        return from_dict(data_class=Change, data=d)
    elif _type in get_events_list():
        return from_dict(data_class=Event, data=d)
    else:
        raise Exception("Unknown DB item id: %s" % _type)


class ELmonocleDB:

    log = logging.getLogger("monocle.ELmonocleDB")

    def __init__(
        self,
        elastic_conn=environ.get("ELASTIC_CONN", "localhost:9200"),
        index=None,
        timeout=10,
        prefix=CHANGE_PREFIX,
        create=True,
        previous_schema=False,
        idents_config: Optional[IdentsConfig] = None,
        user=None,
        password=None,
        use_ssl=None,
        verify_certs=None,
        ssl_show_warn=None,
    ) -> None:
        host, port = elastic_conn.split(":")
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        ip = socket.gethostbyname(host)
        self.log.info("ES IP is %s" % ip)
        self.log.info("ES prefix is %s" % prefix)

        elastic_conn = [
            {
                "host": host,
                "port": port,
            }
        ]

        if use_ssl:
            elastic_conn[0]["use_ssl"] = use_ssl

        if not verify_certs:
            elastic_conn[0]["verify_certs"] = verify_certs

        if not ssl_show_warn:
            elastic_conn[0]["ssl_show_warn"] = ssl_show_warn

        if user and password:
            elastic_conn[0]["http_auth"] = "%s:%s" % (user, password)

        while True:
            try:
                s.connect((ip, int(port)))
                s.shutdown(2)
                s.close()
                break
            except Exception as excpt:
                self.log.info(
                    "Unable to connect to %s: %s. Sleeping for %ds."
                    % (elastic_conn, excpt, timeout)
                )
                time.sleep(timeout)

        self.log.info("Connecting to ES server at %s" % elastic_conn)
        self.es = Elasticsearch(elastic_conn)
        self.log.info(self.es.info())

        if previous_schema:
            self.prefix = PREV_CHANGE_PREFIX
        else:
            self.prefix = prefix

        if not index:
            self.log.info("No index provided")
            return

        self.idents_config = idents_config or []

        self.index = "{}{}".format(self.prefix, index)
        self.log.info("Using ES index %s" % self.index)
        self.new_fields = {
            "crawler_metadata": {
                "properties": {
                    "last_commit_at": {
                        "type": "date",
                        "format": "date_time_no_millis",
                    },
                    "last_post_at": {
                        "type": "date",
                        "format": "date_time_no_millis",
                    },
                    "total_docs_posted": {"type": "integer"},
                    "total_changes_updated": {"type": "integer"},
                    "total_change_events_updated": {"type": "integer"},
                    "total_orphans_updated": {"type": "integer"},
                }
            },
            "tasks_data": {
                "properties": {
                    "tid": {"type": "keyword"},
                    "ttype": {"type": "keyword"},
                    "crawler_name": {"type": "keyword"},
                    "updated_at": {"type": "date", "format": "date_time_no_millis"},
                    "change_url": {"type": "keyword"},
                    "severity": {"type": "keyword"},
                    "priority": {"type": "keyword"},
                    "score": {"type": "integer"},
                    "url": {"type": "keyword"},
                    "title": {
                        "type": "text",
                        "fields": {
                            "keyword": {"type": "keyword", "ignore_above": 8191}
                        },
                    },
                    "_adopted": {"type": "boolean"},
                }
            },
        }
        self.mapping = {
            "id": {"type": "keyword"},
            "type": {"type": "keyword"},
            "number": {"type": "keyword"},
            "change_id": {"type": "keyword"},
            "title": {
                "type": "text",
                "fields": {"keyword": {"type": "keyword", "ignore_above": 8191}},
            },
            "text": {
                "type": "text",
                "fields": {"keyword": {"type": "keyword", "ignore_above": 8191}},
            },
            "url": {"type": "keyword"},
            "commit_count": {"type": "integer"},
            "additions": {"type": "integer"},
            "deletions": {"type": "integer"},
            "changed_files_count": {"type": "integer"},
            "changed_files": {
                "properties": {
                    "additions": {"type": "integer"},
                    "deletions": {"type": "integer"},
                    "path": {"type": "keyword"},
                }
            },
            "commits": {
                "properties": {
                    "sha": {"type": "keyword"},
                    "author": {
                        "properties": {
                            "uid": {"type": "keyword"},
                            "muid": {"type": "keyword"},
                        }
                    },
                    "committer": {
                        "properties": {
                            "uid": {"type": "keyword"},
                            "muid": {"type": "keyword"},
                        }
                    },
                    "authored_at": {
                        "type": "date",
                        "format": "date_time_no_millis",
                    },
                    "committed_at": {
                        "type": "date",
                        "format": "date_time_no_millis",
                    },
                    "additions": {"type": "integer"},
                    "deletions": {"type": "integer"},
                    "title": {"type": "text"},
                }
            },
            "repository_prefix": {"type": "keyword"},
            "repository_fullname": {"type": "keyword"},
            "repository_shortname": {"type": "keyword"},
            "author": {
                "properties": {
                    "uid": {"type": "keyword"},
                    "muid": {"type": "keyword"},
                }
            },
            "on_author": {
                "properties": {
                    "uid": {"type": "keyword"},
                    "muid": {"type": "keyword"},
                }
            },
            "committer": {
                "properties": {
                    "uid": {"type": "keyword"},
                    "muid": {"type": "keyword"},
                }
            },
            "merged_by": {
                "properties": {
                    "uid": {"type": "keyword"},
                    "muid": {"type": "keyword"},
                }
            },
            "branch": {"type": "keyword"},
            "target_branch": {"type": "keyword"},
            "created_at": {"type": "date", "format": "date_time_no_millis"},
            "on_created_at": {"type": "date", "format": "date_time_no_millis"},
            "merged_at": {"type": "date", "format": "date_time_no_millis"},
            "updated_at": {"type": "date", "format": "date_time_no_millis"},
            "closed_at": {"type": "date", "format": "date_time_no_millis"},
            "state": {"type": "keyword"},
            "duration": {"type": "integer"},
            "mergeable": {"type": "keyword"},
            "labels": {"type": "keyword"},
            "assignees": {
                "type": "nested",
                "properties": {
                    "uid": {"type": "keyword"},
                    "muid": {"type": "keyword"},
                },
            },
            "approval": {"type": "keyword"},
            "draft": {"type": "boolean"},
            "self_merged": {"type": "boolean"},
        }
        self.mapping.update(self.new_fields)
        self.ic = self.es.indices
        if create:
            try:
                self.ic.get(index=self.index)
            except NotFoundError:
                settings = {"mappings": {"properties": self.mapping}}
                self.ic.create(index=self.index, body=settings)
            # The authors_histo is failing on some context with this error when the
            # time slice is large: Must be less than or equal to: [10000] but was [10001]. ()This limit can be
            # set by changing the [search.max_buckets] cluster level)
            # This is an attempt to mitigate the issue
            cluster_settings = {"transient": {"search.max_buckets": 100000}}
            self.es.cluster.put_settings(body=cluster_settings)
            # Check current index properties and add new filed index properties
            current_index_properties = (
                self.ic.get_mapping(index=self.index)
                .get(self.index, {})
                .get("mappings", {})
                .get("properties", {})
            )
            for new_field_name, new_field_props in self.new_fields.items():
                if new_field_name not in current_index_properties:
                    body = {"properties": {new_field_name: new_field_props}}
                    self.ic.put_mapping(index=self.index, body=body)

    def update(self, source_it: List[Union[Change, Event]]) -> None:
        def gen(it):
            for _source in it:
                source = change_or_event_to_dict(_source)
                d = {}
                d["_index"] = self.index
                d["_op_type"] = "update"
                d["_id"] = source["id"]
                d["doc"] = source
                d["doc_as_upsert"] = True
                yield d

        bulk(self.es, gen(source_it))
        self.es.indices.refresh(index=self.index)

    def update_task_data(
        self,
        source_it: Union[
            List[TaskDataForEL],
            List[OrphanTaskDataForEL],
            List[AdoptedTaskDataForEL],
        ],
    ) -> Optional[BulkIndexError]:
        def gen(it):
            for _source in it:
                d = {}
                d["_index"] = self.index
                d["_op_type"] = "update"
                d["_id"] = _source._id
                d["doc"] = {}
                d["doc"].update({"id": _source._id})
                if isinstance(_source, TaskDataForEL):
                    d["doc"].update(
                        {"tasks_data": [asdict(td) for td in _source.tasks_data]}
                    )
                if isinstance(_source, OrphanTaskDataForEL):
                    d["doc"].update({"tasks_data": asdict(_source.task_data)})
                    d["doc"]["type"] = "OrphanTaskData"
                if isinstance(_source, AdoptedTaskDataForEL):
                    d["doc"].update({"tasks_data": asdict(_source.task_data)})
                d["doc_as_upsert"] = True
                yield d

        ret = None
        try:
            bulk(self.es, gen(source_it))
        except BulkIndexError as err:
            ret = err
        self.es.indices.refresh(index=self.index)
        return ret

    def compute_crawler_id_by_name(self, name, _type):
        return "crawler/%s/%s" % (_type, name)

    def get_task_crawler_metadata(self, name: str) -> Dict:
        try:
            # mypy fails with `Too many positional arguments for "get" of "Elasticsearch"`
            # and that does not seems true
            ret = self.es.get(  # type: ignore
                self.index, self.compute_crawler_id_by_name(name, "tasks_crawler")
            )
            return ret["_source"]["crawler_metadata"]
        except Exception:
            return {}

    def set_task_crawler_metadata(
        self, name: str, commit_date: datetime = None, push_infos: Dict = None
    ):
        metadata = {}
        if commit_date:
            metadata.update({"last_commit_at": commit_date})
        if push_infos:
            prev_metadata = self.get_task_crawler_metadata(name)
            metadata.update(
                {
                    "last_post_at": push_infos["last_post_at"],
                    "total_docs_posted": prev_metadata.get("total_docs_posted", 0)
                    + push_infos["total_docs_posted"],
                    "total_changes_updated": prev_metadata.get(
                        "total_changes_updated", 0
                    )
                    + push_infos["total_changes_updated"],
                    "total_change_events_updated": prev_metadata.get(
                        "total_change_events_updated", 0
                    )
                    + push_infos["total_change_events_updated"],
                    "total_orphans_updated": prev_metadata.get(
                        "total_orphans_updated", 0
                    )
                    + push_infos["total_orphans_updated"],
                }
            )
        body = {
            "doc": {"type": "TaskCrawlerDataCommit", "crawler_metadata": metadata},
            "doc_as_upsert": True,
        }
        ret = None
        try:
            # mypy fails with `Too many positional arguments for "update" of "Elasticsearch"`
            # and that does not seems true
            self.es.update(  # type: ignore
                self.index,
                self.compute_crawler_id_by_name(name, "tasks_crawler"),
                body=body,
            )
            self.es.indices.refresh(index=self.index)
        except Exception as err:
            ret = err
        return ret

    def delete_index(self):
        self.log.info("Deleting index: %s" % self.index)
        self.ic.delete(index=self.index)

    def delete_repository(self, repository_fullname):
        params = {"index": self.index}
        body = {
            "query": {
                "bool": {
                    "filter": {
                        "regexp": {
                            "repository_fullname": {"value": repository_fullname}
                        }
                    }
                }
            }
        }
        params["body"] = body
        self.es.delete_by_query(**params)
        self.es.indices.refresh(index=self.index)

    def get_last_updated(self, repository_fullname):
        params = {"index": self.index}
        body = {
            "sort": [{"updated_at": {"order": "desc"}}],
            "query": {
                "bool": {
                    "filter": [
                        {"term": {"type": "Change"}},
                        {
                            "regexp": {
                                "repository_fullname": {"value": repository_fullname}
                            }
                        },
                    ]
                }
            },
        }
        params["body"] = body
        try:
            res = self.es.search(**params)
        except Exception:
            return []
        ret = [r["_source"] for r in res["hits"]["hits"]]
        if not ret:
            return []
        return ret[0]

    def get_changes_by_url(self, change_urls, size):
        params = {
            "index": self.index,
            "body": {
                "size": size,
                "query": {
                    "bool": {
                        "filter": [
                            {"term": {"type": "Change"}},
                            {"terms": {"url": change_urls}},
                        ]
                    }
                },
            },
        }
        try:
            res = self.es.search(**params)
        except Exception:
            return []
        return [r["_source"] for r in res["hits"]["hits"]]

    def get_change_events_by_url(self, change_urls):
        qfilter = {
            "bool": {
                "filter": [
                    {"terms": {"type": get_events_list()}},
                    {"terms": {"url": change_urls}},
                ]
            }
        }
        body = {
            "query": qfilter,
        }
        scanner_params = {"index": self.index, "query": body}
        data = scanner(self.es, **scanner_params)
        return [d["_source"] for d in data]

    def get_orphan_tds_by_change_urls(self, change_urls):
        assert len(change_urls) <= 50
        size = 5000  # Asumming not more that 100 TD data relataed to a change
        params = {
            "index": self.index,
            "body": {
                "size": size,
                "query": {
                    "bool": {
                        "must_not": {"exists": {"field": "tasks_data._adopted"}},
                        "filter": [
                            {"term": {"type": "OrphanTaskData"}},
                            {"terms": {"tasks_data.change_url": change_urls}},
                        ],
                    }
                },
            },
        }
        try:
            res = self.es.search(**params)
        except Exception:
            return []
        return [r["_source"] for r in res["hits"]["hits"]]

    def get_orphan_tds_and_declare_adpotion(self, changes_url):
        assert len(changes_url) <= 50
        tds = self.get_orphan_tds_by_change_urls(changes_url)
        if tds:
            adopted_tds = [
                AdoptedTaskDataForEL(
                    _id=td["id"],
                    task_data=AdoptedTaskData(_adopted=True),
                )
                for td in tds
            ]
            self.update_task_data(adopted_tds)
        return tds

    def update_change_and_events_with_orphan_tds(self, mapping: Dict[str, List[str]]):
        change_urls = list(mapping.keys())
        while change_urls:
            change_urls_to_process = change_urls[:50]
            change_urls = change_urls[50:]
            tds = self.get_orphan_tds_and_declare_adpotion(change_urls_to_process)
            # Group tds in buckets by change_url
            _map: Dict[str, List] = dict()
            for td in tds:
                _map.setdefault(td["tasks_data"]["change_url"], []).append(
                    td["tasks_data"]
                )
            # Create update docs to attach tds to matching changes
            to_update = []
            for change_url, tds in _map.items():
                for _id in mapping[change_url]:
                    to_update.append(
                        TaskDataForEL(
                            _id=_id,
                            tasks_data=createELTaskData(tds),
                        )
                    )
            self.update_task_data(to_update)

    def run_named_query(self, name, *args, **kwargs):
        if name not in queries.public_queries:
            raise UnknownQueryException("Unknown query: %s" % name)
        return getattr(queries, name)(self.es, self.index, *args, **kwargs)

    def get_indices(self):
        return [
            ind.replace(self.prefix, "")
            for ind in self.es.indices.get(self.prefix + "*")
        ]

    def iter_index(self):
        body = {"query": {"match_all": {}}}
        return scan(self.es, query=body, index=self.index, size=5000)

    def update_idents(self) -> None:

        import json

        bulk_size = 7500

        def get_obj_hash(obj: Dict) -> int:
            obj_json = json.dumps(obj, sort_keys=True)
            return hash(obj_json)

        def update_ident(dict_ident: Dict) -> Dict:
            dict_ident["muid"] = create_muid(dict_ident["uid"], self.idents_config)
            return dict_ident

        def _update_idents(obj: Dict) -> Tuple[Optional[Union[Change, Event]], bool]:

            prev_hash = get_obj_hash(obj)

            if obj["type"] == "Change":
                obj["author"] = update_ident(obj["author"])
                if "committer" in obj:
                    obj["committer"] = update_ident(obj["committer"])
                if "merged_by" in obj:
                    obj["merged_by"] = update_ident(obj["merged_by"])
                if "assignees" in obj:
                    obj["assignees"] = list(map(update_ident, obj["assignees"]))
                if "commits" in obj:
                    for commit in obj["commits"]:
                        commit["author"] = update_ident(commit["author"])
                        commit["committer"] = update_ident(commit["committer"])
            if obj["type"] in get_events_list():
                if "author" in obj:
                    obj["author"] = update_ident(obj["author"])
                if "on_author" in obj:
                    obj["on_author"] = update_ident(obj["on_author"])
            updated = not prev_hash == get_obj_hash(obj)
            if updated:
                return dict_to_change_or_event(obj), True
            else:
                return None, False

        def bulk_update(to_update: List) -> List:
            print("Updating %s objects ..." % len(to_update))
            self.update(to_update)
            return []

        to_update = []
        total_read = 0
        for _obj in self.iter_index():
            total_read += 1
            if total_read % bulk_size == 0:
                print("%s objects read from the database" % total_read)
            obj = _obj["_source"]
            obj, updated = _update_idents(obj)
            if updated:
                to_update.append(obj)
            if len(to_update) == bulk_size:
                to_update = bulk_update(to_update)

        bulk_update(to_update)
