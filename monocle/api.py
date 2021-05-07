# Monocle.
# Copyright (C) 2019-2021 Monocle authors
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

from datetime import datetime
from typing import Any, List

from google.protobuf.timestamp_pb2 import Timestamp
from monocle.messages.config_pb2 import (
    GetProjectsRequest,
    GetProjectsResponse,
)
from monocle.messages.search_pb2 import (
    SearchSuggestionsRequest,
    SearchSuggestionsResponse,
)
from monocle.messages.task_data_pb2 import (
    TaskDataCommitRequest,
    TaskDataCommitResponse,
    TaskDataGetLastUpdatedRequest,
    TaskDataGetLastUpdatedResponse,
    AddRequest,
    AddResponse,
)
import monocle.messages.task_data_pb2 as TD
from monocle import env

from monocle.webapp import create_db_connection, INPUT_TASK_DATA_LIMIT
from monocle.task_data import (
    createELTaskData,
    TaskDataForEL,
    OrphanTaskDataForEL,
    toTaskData,
)

from elasticsearch.exceptions import NotFoundError


def config_get_projects(request: GetProjectsRequest) -> GetProjectsResponse:
    project_defs = env.project_defs
    return GetProjectsResponse(projects=project_defs.get(request.index, []))


def check_crawler_request(index, name, api_key):
    crawlers = env.indexes_task_crawlers.get(index)
    if crawlers is None:
        return (True, TD.UnknownIndex)
    configs = [crawler for crawler in crawlers if crawler.name == name]
    if not configs:
        return (True, TD.UnknownCrawler)
    config = configs[0]
    if api_key is not None and api_key != config.api_key:
        return (True, TD.UnknownApiKey)
    return (False, config)


def task_data_commit(request: TaskDataCommitRequest) -> TaskDataCommitResponse:
    (error, result) = check_crawler_request(
        request.index, request.crawler, request.apikey
    )
    if error:
        return TaskDataCommitResponse(error=result)
    db = create_db_connection(request.index)
    input_date = request.timestamp.ToDatetime()
    if db.set_task_crawler_metadata(request.crawler, input_date):
        return TaskDataCommitResponse(error=TD.CommitDateInferiorThanPrevious)
    return TaskDataCommitResponse(timestamp=request.timestamp)


def task_data_get_last_updated(
    request: TaskDataGetLastUpdatedRequest,
) -> TaskDataGetLastUpdatedResponse:
    (error, result) = check_crawler_request(request.index, request.crawler, None)
    if error:
        # Note: here we are abusing the fact that TaskDataGetLastUpdatedError
        # is a strict subset of TaskDataCommitRequest
        return TaskDataGetLastUpdatedResponse(error=result)
    db = create_db_connection(request.index)
    metadata = db.get_task_crawler_metadata(result.name)
    # TODO(add details to the protobuf description)
    # if "details" in request.args and request.args.get("details") == "true":
    #    return jsonify(metadata)
    timestamp = Timestamp()
    if not metadata.get("last_commit_at"):
        timestamp.FromDatetime(result.updated_since)
    else:
        timestamp.FromJsonString(metadata["last_commit_at"] + "Z")
    return TaskDataGetLastUpdatedResponse(timestamp=timestamp)


def task_data_add(request: AddRequest) -> AddResponse:
    (error, result) = check_crawler_request(
        request.index, request.crawler, request.apikey
    )
    if error:
        return AddResponse(error=result)
    if not (0 < len(request.items) <= INPUT_TASK_DATA_LIMIT):
        return AddResponse(error=TD.AddFailed)
    extracted_data = request.items
    crawler_config = result
    index = request.index
    # Find changes in EL ids that match urls
    change_urls = [e.change_url for e in extracted_data]
    db = create_db_connection(index)
    mc = db.get_changes_by_url(change_urls, INPUT_TASK_DATA_LIMIT)
    me = db.get_change_events_by_url(change_urls)
    mc = dict(
        [
            (
                r["url"],
                {
                    "id": r["id"],
                    "td": createELTaskData(r.get("tasks_data", [])),
                },
            )
            for r in mc
        ]
    )
    # Prepare input data set
    update_docs: Any = []
    for input_task_data in extracted_data:
        td = toTaskData(request.crawler, input_task_data)
        if input_task_data.change_url in mc:
            # First check if a td match the input one
            prev_td = [
                td
                for td in mc[input_task_data.change_url]["td"]
                if td.url == input_task_data.url
            ]
            if len(prev_td) > 1:
                raise RuntimeError("Multiple td match in previous td")
            # Remove the previous outdated one if any
            if prev_td:
                mc[input_task_data.change_url]["td"].remove(prev_td[0])
            # Add the new one to the list
            mc[input_task_data.change_url]["td"].append(td)
        else:
            update_docs.append(
                OrphanTaskDataForEL(_id=input_task_data.url, task_data=td)
            )
    total_orphans_to_update = len(update_docs)
    for _mc in mc.values():
        update_docs.append(
            TaskDataForEL(
                _id=_mc["id"],
                tasks_data=_mc["td"],
            )
        )
    total_changes_to_update = len(update_docs) - total_orphans_to_update
    for _me in me:
        update_docs.append(
            TaskDataForEL(_id=_me["id"], tasks_data=mc[_me["url"]]["td"])
        )
    total_change_events_to_update = (
        len(update_docs) - total_orphans_to_update - total_changes_to_update
    )
    # Now insert the data
    err = db.update_task_data(source_it=update_docs)
    # https://github.com/elastic/elasticsearch-py/blob/f4447bf996bdee47a0eb4c736bd39dea20a4486e/elasticsearch/helpers/actions.py#L177
    if err:
        return AddResponse(error=TD.AddFailed)
    db.set_task_crawler_metadata(
        crawler_config.name,
        push_infos={
            "last_post_at": datetime.utcnow().replace(microsecond=0),
            "total_docs_posted": len(extracted_data),
            "total_changes_updated": total_changes_to_update,
            "total_change_events_updated": total_change_events_to_update,
            "total_orphans_updated": total_orphans_to_update,
        },
    )
    return AddResponse()


def get_top_terms(db, field: str) -> List[str]:
    body = {
        "size": 0,
        "aggs": {
            "top_terms": {
                "terms": {"field": field, "size": 100, "order": {"_key": "asc"}}
            }
        },
        "query": {"bool": {"filter": [{"term": {"type": "Change"}}]}},
    }

    search_params = {"index": db.index, "body": body}
    try:
        res = db.es.search(**search_params)
    except NotFoundError:
        return []
    return [b["key"] for b in res["aggregations"]["top_terms"]["buckets"]]


def search_suggestions(request: SearchSuggestionsRequest) -> SearchSuggestionsResponse:
    db = create_db_connection(request.index)
    return SearchSuggestionsResponse(
        task_types=get_top_terms(db, "tasks_data.ttype"),
        authors=get_top_terms(db, "author.muid"),
        approvals=get_top_terms(db, "approval"),
    )
