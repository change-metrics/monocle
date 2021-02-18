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


from typing import Any, List, Dict, Union, Optional
from collections.abc import Callable
from monocle.db.db import Change, Event
from urllib.parse import urlparse

RawChange = Dict[str, Any]


class BaseCrawler(object):
    def get(self, updated_since: str, change_id: Optional[str]) -> List[RawChange]:
        raise NotImplementedError

    def extract_objects(
        self, changes: List[RawChange], dumper: Callable
    ) -> List[Union[Change, Event]]:
        raise NotImplementedError


def prefix_ident_with_domain(obj: Union[Change, Event]) -> Union[Change, Event]:
    """Ensure that idents is prefixed with the domain of the code review system"""
    domain = urlparse(obj.url).netloc

    def prefix(name: str) -> str:
        return domain + "/" + name

    if isinstance(obj, Change):
        obj.author = prefix(obj.author)
        if obj.committer:
            obj.committer = prefix(obj.committer)
        if obj.merged_by:
            obj.merged_by = prefix(obj.merged_by)
        if obj.assignees:
            map(prefix, obj.assignees)
        if obj.commits:
            for commit in obj.commits:
                commit.author = prefix(commit.author)
                commit.committer = prefix(commit.committer)
    else:
        if obj.author:
            obj.author = prefix(obj.author)
        if obj.on_author:
            obj.on_author = prefix(obj.on_author)
    return obj
