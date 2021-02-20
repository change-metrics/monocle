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
from monocle.db.db import Change, Event, Ident
from urllib.parse import urlparse

RawChange = Dict[str, Any]


class BaseCrawler(object):
    def get(self, updated_since: str, change_id: Optional[str]) -> List[RawChange]:
        raise NotImplementedError

    def extract_objects(
        self, changes: List[RawChange], dumper: Callable
    ) -> List[Union[Change, Event]]:
        raise NotImplementedError


def prefix(domain: str, name: str) -> str:
    if not name.startswith("%s/" % domain):
        return domain + "/" + name
    else:
        return name


def create_muid_from_uid(uid: str) -> str:
    """This is a temporary implementation
    The muid is the Monocle Uniq ID of an ident. But for now it
    uses a placeolder. Later the muid will be used to prettify
    the ident in the UI with Full Name gathered from a local DB.
    """
    elms = uid.split("/")
    if len(elms) == 3 or len(elms) == 2:
        muid = "/".join(elms[1:])
    else:
        raise RuntimeError("Wrong ident uid format")
    return muid


def create_ident_dict(url: str, uid: str) -> Dict:
    domain = urlparse(url).netloc
    uid = prefix(domain, uid)
    return {"uid": uid, "muid": create_muid_from_uid(uid)}


def create_ident(url: str, uid: str) -> Ident:
    domain = urlparse(url).netloc
    uid = prefix(domain, uid)
    return Ident(uid=uid, muid=create_muid_from_uid(uid))
