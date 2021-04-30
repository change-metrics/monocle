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
from monocle import env


def config_get_projects(request: GetProjectsRequest) -> GetProjectsResponse:
    project_defs = env.project_defs
    return GetProjectsResponse(projects=project_defs.get(request.index, []))
