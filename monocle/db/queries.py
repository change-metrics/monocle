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

import statistics
from copy import deepcopy
from monocle.utils import is8601_to_dt
from monocle.utils import enhance_changes
from monocle.utils import Detector
from monocle.utils import utcnow
from monocle.utils import get_events_list
from monocle.config import get_project_by_name

from elasticsearch.helpers import scan as scanner
from elasticsearch.exceptions import NotFoundError

log = logging.getLogger(__name__)

public_queries = (
    "count_events",
    "count_authors",
    "count_opened_changes",
    "events_top_authors",
    "changes_top_approval",
    "changes_top_commented",
    "changes_top_reviewed",
    "approvals_top",
    "change_merged_count_by_duration",
    "last_changes",
    "last_state_changed_changes",
    "oldest_open_changes",
    "changes_and_events",
    "changes",
    "changes_by_file_map",
    "authors_by_file_map",
)


def ensure_gte_lte(es, index, repository_fullname, params):
    if not params.get("gte"):
        first_created_event = _first_created_event(
            es, index, repository_fullname, params
        )
        if first_created_event:
            params["gte"] = int(is8601_to_dt(first_created_event).timestamp() * 1000)
        else:
            # There is probably nothing in the db that match the query
            params["gte"] = None
    if not params.get("lte"):
        params["lte"] = int(utcnow().timestamp() * 1000)


def generate_events_filter(params, qfilter):
    gte = params.get("gte")
    lte = params.get("lte")
    on_cc_gte = params.get("on_cc_gte")
    on_cc_lte = params.get("on_cc_lte")
    ec_same_date = params.get("ec_same_date")

    on_created_at_range = {"on_created_at": {"format": "epoch_millis"}}
    if ec_same_date:
        on_cc_gte = gte
        on_cc_lte = lte
    if on_cc_gte or ec_same_date:
        on_created_at_range["on_created_at"]["gte"] = on_cc_gte
    if on_cc_lte or ec_same_date:
        on_created_at_range["on_created_at"]["lte"] = on_cc_lte
    qfilter.append({"range": on_created_at_range})


def generate_changes_filter(params, qfilter):
    state = params.get("state")
    tests_included = params.get("tests_included")
    self_merged = params.get("self_merged")
    has_issue_tracker_links = params.get("has_issue_tracker_links")
    if state:
        qfilter.append({"terms": {"state": state}})
    if tests_included:
        qfilter.append(
            {"regexp": {"changed_files.path": {"value": Detector.tests_regexp}}}
        )
    if has_issue_tracker_links:
        value = Detector().get_issue_tracker_regexp(style=has_issue_tracker_links)
        if value:
            qfilter.append(
                {
                    "bool": {
                        "should": [
                            {"regexp": {"text.keyword": {"value": value}}},
                            {"regexp": {"title.keyword": {"value": value}}},
                        ]
                    }
                }
            )
    if self_merged:
        qfilter.append({"term": {"self_merged": True}})


def generate_filter(es, index, repository_fullname, params, ensure_time_range=True):
    if ensure_time_range:
        ensure_gte_lte(es, index, repository_fullname, params)
    gte = params.get("gte")
    lte = params.get("lte")
    # The type is mandatory
    etype = params["etype"]
    authors = params.get("authors")
    on_authors = params.get("on_authors")
    approvals = params.get("approvals")
    exclude_approvals = params.get("exclude_approvals")
    exclude_authors = params.get("exclude_authors")
    created_at_range = {"created_at": {"format": "epoch_millis"}}
    change_ids = params.get("change_ids")
    target_branch = params.get("target_branch")
    task_priority = params.get("task_priority")
    task_severity = params.get("task_severity")
    task_score = params.get("task_score")
    task_type = params.get("task_type")
    files = params.get("files")
    project = params.get("project")
    project_defs = params.get("_project_defs")
    if project and project_defs:
        project_def = get_project_by_name(project, project_defs)
        if project_def:
            repository_fullname = project_def.repository_regex or repository_fullname
            target_branch = project_def.branch_regex or target_branch
            files = project_def.file_regex or files
    if gte:
        created_at_range["created_at"]["gte"] = gte
    if lte:
        created_at_range["created_at"]["lte"] = lte
    qfilter = [
        {"regexp": {"repository_fullname": {"value": repository_fullname}}},
        {"range": created_at_range},
    ]
    qfilter.append({"terms": {"type": etype}})
    if authors:
        qfilter.append({"terms": {"author.muid": authors}})
    if on_authors:
        qfilter.append({"terms": {"on_author.muid": on_authors}})
    if change_ids:
        qfilter.append({"terms": {"change_id": change_ids}})
    if target_branch:
        qfilter.append({"regexp": {"target_branch": {"value": target_branch}}})
    if "Change" in params["etype"]:
        generate_changes_filter(params, qfilter)
    else:
        generate_events_filter(params, qfilter)
    if files:
        qfilter.append({"regexp": {"changed_files.path": files}})
    if approvals:
        qfilter.append({"terms": {"approval": approvals}})
    if task_priority:
        qfilter.append({"terms": {"tasks_data.priority": task_priority}})
    if task_severity:
        qfilter.append({"terms": {"tasks_data.severity": task_severity}})
    if task_type:
        qfilter.append({"terms": {"tasks_data.ttype": task_type}})
    if task_score:
        if task_score[0] == "==":
            qfilter.append(
                {
                    "range": {
                        "tasks_data.score": {
                            "gte": task_score[1],
                            "lte": task_score[1],
                        }
                    }
                }
            )
        else:
            qfilter.append(
                {"range": {"tasks_data.score": {task_score[0]: task_score[1]}}}
            )

    must_not = []
    if exclude_authors:
        must_not.append({"terms": {"author.muid": exclude_authors}})
        must_not.append({"terms": {"on_author.muid": exclude_authors}})
    if exclude_approvals:
        must_not.append({"terms": {"approval": exclude_approvals}})

    ret = {"bool": {"filter": qfilter, "must_not": must_not}}
    log.debug("query EL filter: %s" % ret)
    return ret


def totalc(val):
    if isinstance(val, dict) and "value" in val:
        return val["value"]
    return val


def switch_to_on_authors(params):
    if params.get("authors"):
        # We want the events happening on changes authored by the selected authors
        params["on_authors"] = params.get("authors")
        del params["authors"]
        if "exclude_authors" in params:
            # We don't want to exclude any events authors in that context
            del params["exclude_authors"]


def run_query(es, index, body):
    search_params = {"index": index, "body": body}
    try:
        log.debug('run_query "%s"' % search_params)
        res = es.search(**search_params)
    except NotFoundError:
        raise
    except Exception:
        log.exception('Unable to run query: "%s"' % search_params)
        return []
    return res


def _scan(es, index, repository_fullname, params):
    body = {
        # "_source": "change_id",
        "_source": params.get("field", []),
        "query": generate_filter(es, index, repository_fullname, params),
    }
    scanner_params = {"index": index, "query": body}
    data = scanner(es, **scanner_params)
    ret = []
    size = params.get("size")
    for d in data:
        if size and len(ret) == size:
            break
        ret.append(d["_source"])
    return ret


def _first_created_event(es, index, repository_fullname, params):
    body = {
        "sort": [{"created_at": {"order": "asc"}}],
        "query": generate_filter(
            es, index, repository_fullname, params, ensure_time_range=False
        ),
    }
    data = run_query(es, index, body)
    data = [r["_source"] for r in data["hits"]["hits"]]
    if data:
        return data[0]["created_at"]


def count_events(es, index, repository_fullname, params):
    body = {"query": generate_filter(es, index, repository_fullname, params)}
    count_params = {"index": index}
    count_params["body"] = body
    res = es.count(**count_params)
    return res["count"]


def count_authors(es, index, repository_fullname, params):
    body = {
        "aggs": {
            "agg1": {
                "cardinality": {"field": "author.muid", "precision_threshold": 3000}
            }
        },
        "size": 0,
        "query": generate_filter(es, index, repository_fullname, params),
    }
    data = run_query(es, index, body)
    return data["aggregations"]["agg1"]["value"]


def _events_top(es, index, repository_fullname, field, params):
    body = {
        "aggs": {
            "agg1": {
                "terms": {"field": field, "size": 1000, "order": {"_count": "desc"}}
            }
        },
        "size": 0,
        "query": generate_filter(es, index, repository_fullname, params),
    }
    data = run_query(es, index, body)
    count_series = [b["doc_count"] for b in data["aggregations"]["agg1"]["buckets"]]
    count_avg = statistics.mean(count_series) if count_series else 0
    count_median = statistics.median(sorted(count_series)) if count_series else 0
    _from = params["from"]
    _to = params["from"] + params["size"]
    buckets = data["aggregations"]["agg1"]["buckets"]
    return {
        "items": buckets[_from:_to],
        "count_avg": count_avg,
        "count_median": count_median,
        "total": len(buckets),
        "total_hits": totalc(data["hits"]["total"]),
    }


def events_top_authors(es, index, repository_fullname, params):
    params = deepcopy(params)
    return _events_top(es, index, repository_fullname, "author.muid", params)


# TODO(fbo): add tests for queries below
def changes_top_approval(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["etype"] = ("ChangeReviewedEvent",)
    return _events_top(es, index, repository_fullname, "approval", params)


def changes_top_commented(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["etype"] = ("ChangeCommentedEvent",)
    return _events_top(es, index, repository_fullname, "change_id", params)


def changes_top_reviewed(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["etype"] = ("ChangeReviewedEvent",)
    return _events_top(es, index, repository_fullname, "change_id", params)


def approvals_top(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["etype"] = ("Change",)
    return _events_top(es, index, repository_fullname, "approval", params)


def change_merged_count_by_duration(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["etype"] = ("Change",)
    params["state"] = ("MERGED",)
    body = {
        "aggs": {
            "agg1": {
                "range": {
                    "field": "duration",
                    "ranges": [
                        {"to": 24 * 3600},
                        {"from": 24 * 3600 + 1, "to": 7 * 24 * 3600},
                        {"from": 7 * 24 * 3600 + 1, "to": 31 * 24 * 3600},
                        {"from": 31 * 24 * 3600 + 1},
                    ],
                    "keyed": True,
                }
            }
        },
        "size": 0,
        "query": generate_filter(es, index, repository_fullname, params),
    }
    data = run_query(es, index, body)
    return data["aggregations"]["agg1"]["buckets"]


def change_merged_stats_duration(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["etype"] = ("Change",)
    params["state"] = ("MERGED",)
    body = {
        "aggs": {
            "avg": {"avg": {"field": "duration"}},
            "variability": {"median_absolute_deviation": {"field": "duration"}},
        },
        "size": 0,
        "docvalue_fields": [{"field": "created_at", "format": "date_time"}],
        "query": generate_filter(es, index, repository_fullname, params),
    }
    data = run_query(es, index, body)
    return {
        "avg": data["aggregations"]["avg"]["value"],
        "variability": data["aggregations"]["variability"]["value"],
    }


def change_merged_avg_commits(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["etype"] = ("Change",)
    params["state"] = ("MERGED",)
    body = {
        "aggs": {"agg1": {"avg": {"field": "commit_count"}}},
        "size": 0,
        "docvalue_fields": [{"field": "created_at", "format": "date_time"}],
        "query": generate_filter(es, index, repository_fullname, params),
    }
    data = run_query(es, index, body)
    return data["aggregations"]["agg1"]["value"]


def changes_with_tests_ratio(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["etype"] = ("Change",)
    all = count_events(es, index, repository_fullname, params)
    if all == 0:
        return 0
    params["tests_included"] = True
    tests = count_events(es, index, repository_fullname, params)
    return round(tests / all * 100, 1)


def count_opened_changes(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["etype"] = ("Change",)
    params["state"] = ("OPEN",)
    return count_events(es, index, repository_fullname, params)


def last_changes(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["etype"] = ("Change",)
    body = {
        "sort": [{"updated_at": {"order": "desc"}}],
        "size": params["size"],
        "from": params["from"],
        "query": generate_filter(es, index, repository_fullname, params),
    }
    data = run_query(es, index, body)
    changes = [r["_source"] for r in data["hits"]["hits"]]
    changes = enhance_changes(changes)
    return {"items": changes, "total": totalc(data["hits"]["total"])}


def last_merged_changes(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["state"] = ("MERGED",)
    return last_changes(es, index, repository_fullname, params)


def last_opened_changes(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["state"] = ("OPEN",)
    return last_changes(es, index, repository_fullname, params)


def last_state_changed_changes(es, index, repository_fullname, params):
    return {
        "merged_changes": last_merged_changes(es, index, repository_fullname, params),
        "opened_changes": last_opened_changes(es, index, repository_fullname, params),
    }


def oldest_open_changes(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["etype"] = ("Change",)
    params["state"] = ("OPEN",)
    body = {
        "sort": [{"created_at": {"order": "asc"}}],
        "size": params["size"],
        "from": params["from"],
        "query": generate_filter(es, index, repository_fullname, params),
    }
    data = run_query(es, index, body)
    changes = [r["_source"] for r in data["hits"]["hits"]]
    changes = enhance_changes(changes)
    return {"items": changes, "total": totalc(data["hits"]["total"])}


def changes_and_events(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["etype"] = [
        "Change",
    ] + get_events_list()
    body = {
        "sort": [{"created_at": {"order": "asc"}}],
        "size": params["size"],
        "from": params["from"],
        "query": generate_filter(es, index, repository_fullname, params),
    }
    data = run_query(es, index, body)
    changes = [r["_source"] for r in data["hits"]["hits"]]
    changes = enhance_changes(changes)
    return {"items": changes, "total": totalc(data["hits"]["total"])}


def changes(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["etype"] = ["Change"]
    body = {
        "sort": [{"created_at": {"order": "asc"}}],
        "size": params["size"],
        "from": params["from"],
        "query": generate_filter(es, index, repository_fullname, params),
    }
    data = run_query(es, index, body)
    changes = [r["_source"] for r in data["hits"]["hits"]]
    changes = enhance_changes(changes)
    return {"items": changes, "total": totalc(data["hits"]["total"])}


def changes_by_file_map(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["etype"] = ("Change",)
    body = {
        "size": 1000,
        "query": generate_filter(es, index, repository_fullname, params),
    }
    data = run_query(es, index, body)
    changes = [r["_source"] for r in data["hits"]["hits"]]
    files = {}
    for change in changes:
        for f in change["changed_files"]:
            key = "{}:{}".format(change["repository_fullname"], f["path"])
            files[key] = files.get(key, 0) + 1
    return {"changes": files}


def authors_by_file_map(es, index, repository_fullname, params):
    params = deepcopy(params)
    params["etype"] = ("Change",)
    body = {
        "size": 1000,
        "query": generate_filter(es, index, repository_fullname, params),
    }
    data = run_query(es, index, body)
    changes = [r["_source"] for r in data["hits"]["hits"]]
    authors = {}
    for change in changes:
        for f in change["changed_files"]:
            key = "{}:{}".format(change["repository_fullname"], f["path"])
            try:
                authors[key].add(change["author"]["muid"])
            except KeyError:
                authors[key] = set()
                authors[key].add(change["author"]["muid"])
    return {"authors": authors}
