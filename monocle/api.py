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

from monocle.messages.config_pb2 import (
    GetProjectsRequest,
    GetProjectsResponse,
)
from monocle.messages.task_data_pb2 import (
    TaskDataCommitRequest,
    TaskDataCommitResponse,
)
import monocle.messages.task_data_pb2 as TD
from monocle import env

from monocle.webapp import create_db_connection


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
