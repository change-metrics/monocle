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

from typing import List, Optional
from dataclasses import dataclass

from dacite import from_dict


@dataclass
class TrackerData:
    issue_type: str
    severity: Optional[str]
    priority: Optional[str]
    score: Optional[int]
    issue_id: str
    issue_url: str
    issue_title: str


@dataclass
class InputTrackerData:
    _id: str
    tracker_data: TrackerData


def extract_data(data: List) -> List[InputTrackerData]:
    return [from_dict(data_class=InputTrackerData, data=d) for d in data]
