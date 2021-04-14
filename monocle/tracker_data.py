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

from typing import List, Optional, Dict
from dataclasses import dataclass
from datetime import datetime


@dataclass(frozen=True)
class TrackerData:
    crawler_name: str
    updated_at: datetime
    change_url: str
    issue_type: str
    issue_id: str
    issue_url: str
    issue_title: str
    severity: Optional[str]
    priority: Optional[str]
    score: Optional[int]


InputTrackerData = List[TrackerData]


@dataclass
class TaskTrackerCrawler:
    name: str
    api_key: str
    updated_since: datetime


def createInputTrackerData(data: List, crawler_name: str) -> InputTrackerData:
    def createTrackerData(td: Dict) -> TrackerData:
        return TrackerData(
            crawler_name=crawler_name,
            updated_at=datetime.strptime(td["updated_at"], "%Y-%m-%dT%H:%M:%S"),
            change_url=td["change_url"],
            issue_type=td["issue_type"],
            issue_id=td["issue_id"],
            issue_url=td["issue_url"],
            issue_title=td["issue_title"],
            severity=td.get("severity"),
            priority=td.get("priority"),
            score=td.get("score"),
        )

    return [createTrackerData(td) for td in data]


def createELTrackerData(data: List) -> InputTrackerData:
    def createTrackerData(td: Dict) -> TrackerData:
        return TrackerData(
            crawler_name=td["crawler_name"],
            updated_at=td["updated_at"],
            change_url=td["change_url"],
            issue_type=td["issue_type"],
            issue_id=td["issue_id"],
            issue_url=td["issue_url"],
            issue_title=td["issue_title"],
            severity=td.get("severity"),
            priority=td.get("priority"),
            score=td.get("score"),
        )

    return [createTrackerData(td) for td in data]


def createTaskTrackerCrawler(raw: Dict) -> TaskTrackerCrawler:
    return TaskTrackerCrawler(
        name=raw["name"],
        api_key=raw["api_key"],
        updated_since=datetime.strptime(raw["updated_since"], "%Y-%m-%d"),
    )


@dataclass
class TrackerDataForEL:
    _id: str
    tracker_data: InputTrackerData


@dataclass
class OrphanTrackerDataForEL(TrackerDataForEL):
    pass
