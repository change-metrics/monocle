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

from typing import List, Optional, Literal, Dict, Union
from dataclasses import dataclass, asdict

from dacite import from_dict

from elasticsearch.helpers import bulk
from elasticsearch.helpers import scan
from elasticsearch import client
from elasticsearch.exceptions import NotFoundError

from monocle.db import queries
from monocle.utils import get_events_list

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
class Ident:
    uid: str
    muid: str


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
        elastic_conn="localhost:9200",
        index=None,
        timeout=10,
        prefix=CHANGE_PREFIX,
        create=True,
        previous_schema=False,
    ):
        host, port = elastic_conn.split(":")
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        ip = socket.gethostbyname(host)
        self.log.info("ES IP is %s" % ip)
        self.log.info("ES prefix is %s" % prefix)

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
        self.es = client.Elasticsearch(elastic_conn)
        self.log.info(self.es.info())

        if previous_schema:
            self.prefix = PREV_CHANGE_PREFIX
        else:
            self.prefix = prefix

        if not index:
            self.log.info("No index provided")
            return

        self.index = "{}{}".format(self.prefix, index)
        self.log.info("Using ES index %s" % self.index)
        self.mapping = {
            "properties": {
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
        }
        settings = {"mappings": self.mapping}
        self.ic = client.IndicesClient(self.es)
        if create:
            self.ic.create(index=self.index, ignore=400, body=settings)
        # The authors_histo is failing on some context with this error when the
        # time slice is large: Must be less than or equal to: [10000] but was [10001]. ()This limit can be
        # set by changing the [search.max_buckets] cluster level)
        # This is an attempt to mitigate the issue
        cluster_settings = {"transient": {"search.max_buckets": 100000}}
        self.es.cluster.put_settings(body=cluster_settings)

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

    def run_named_query(self, name, *args, **kwargs):
        # Here we set gte and lte if not provided by user
        # especially to be able to set the histogram extended_bounds
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
        return scan(self.es, query=body, index=self.index)
