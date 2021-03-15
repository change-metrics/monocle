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


from typing import Dict, List, Optional
from dataclasses import dataclass
from urllib.parse import urlparse


@dataclass
class Ident:
    uid: str
    muid: str


@dataclass
class IdentConfig:
    ident: str
    aliases: List[str]


IdentsConfig = List[IdentConfig]


def ident_from_config(ident: Dict) -> IdentConfig:
    return IdentConfig(ident["ident"], ident["aliases"])


def prefix(domain: str, name: str) -> str:
    if not name.startswith("%s/" % domain):
        return domain + "/" + name
    else:
        return name


def create_muid_from_uid(uid: str) -> str:
    """Fallback implementation where the muid is built based on the uid"""
    elms = uid.split("/")
    if len(elms) == 3 or len(elms) == 2:
        muid = "/".join(elms[1:])
    else:
        raise RuntimeError("Wrong ident uid format")
    return muid


def get_muid_from_config(uid: str, idents_config: IdentsConfig) -> Optional[str]:
    """Implementation where the muid is built from the provided configuration"""
    matches = list(filter(lambda ident: uid in ident.aliases, idents_config))
    # Only keep the first match. An uid should not by part of multiple idents
    if len(matches) > 0:
        return matches[0].ident
    else:
        return None


def create_muid(uid: str, idents_config: IdentsConfig) -> str:
    return get_muid_from_config(uid, idents_config) or create_muid_from_uid(uid)


def create_ident(url: str, uid: str, idents_config: IdentsConfig) -> Ident:
    domain = urlparse(url).netloc
    uid = prefix(domain, uid)
    return Ident(
        uid=uid,
        muid=create_muid(uid, idents_config),
    )
