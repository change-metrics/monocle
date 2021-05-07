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

import re
import copy
import iso8601
from argparse import Namespace
from datetime import date
from datetime import timezone
from datetime import datetime
from datetime import timedelta
from urllib.parse import urlparse, urlunparse

from typing import List, Dict, Union


def utcnow():
    return datetime.now(timezone.utc)


def date_to_epoch_ml(datestr):
    if not datestr:
        return None
    tst = date.fromisoformat(datestr).timetuple()
    return int(
        datetime(tst.tm_year, tst.tm_mon, tst.tm_mday, tzinfo=timezone.utc).timestamp()
        * 1000
    )


def end_of_day_to_epoch_ml(datestr):
    if not datestr:
        return None
    delta = int(timedelta(days=1).total_seconds() - 1) * 1000
    return date_to_epoch_ml(datestr) + delta


def is8601_to_dt(datestr):
    return iso8601.parse_date(datestr)


def float_trunc(f, n=2):
    return float(int(f * 10 ** n)) / 10 ** n


class Detector(object):
    tests_regexp = ".*[Tt]est.*"
    tests_re = re.compile(tests_regexp)

    issue_tracker_links = {
        "generic": [
            {
                "regexp": "|".join(
                    [
                        r"https?:\/\/.*issue.*",
                        r"https?:\/\/.*bug.*",
                        r"https?:\/\/.*jira.*",
                    ]
                ),
                "rewrite": None,
            }
        ],
        # https://help.github.com/en/github/writing-on-github/autolinked-references-and-urls
        "github.com": [
            {
                "regexp": r" #[0-9]+",
                "rewrite": {
                    "from": re.compile(r" #(?P<id>[0-9]+)"),
                    "to": "https://github.com/%(repository_prefix)s/%(repository_shortname)s/issues/%%(id)s",
                },
            },
            {
                "regexp": r"[^/ :]+\/[^/]+#[0-9]+",
                "rewrite": {
                    "from": re.compile(
                        r"(?P<org>[^/ :]+)\/(?P<repo>[^/]+)#(?P<id>[0-9]+)"
                    ),
                    "to": "https://github.com/%%(org)s/%%(repo)s/issues/%%(id)s",
                },
            },
            {
                "regexp": r"GH-[1-9]+",
                "rewrite": {
                    "from": re.compile(r"GH-(?P<id>[1-9]+)"),
                    "to": "https://github.com/%(repository_prefix)s/%(repository_shortname)s/issues/%%(id)s",
                },
            },
        ],
        "altassian.net": [
            {"regexp": r"https?:\/\/.+.atlassian.net\/browse\/.*", "rewrite": None}
        ],
    }

    def is_tests_included(self, change):
        for file in change["changed_files"]:
            if self.tests_re.match(file["path"]):
                return True
        return False

    def get_issue_tracker_regexp(self, style="generic"):
        regexps = []
        for reg in self.issue_tracker_links.get(style, []):
            regexps.append(".*%s.*" % reg["regexp"].replace("#", r"\#"))
        regexp = "|".join(regexps)
        return regexp

    def issue_match_and_rewrite(self, change, field, reg):
        store = change["issue_tracker_links"]
        r = re.compile(reg["regexp"])
        matches = r.findall(change[field])
        for match in matches:
            if not reg["rewrite"]:
                # This is already a link. Do not rewrite
                store.append([match, match])
            else:
                m = reg["rewrite"]["from"].match(match)
                if m:
                    # Format rewrite with change attributes
                    rewrite = reg["rewrite"]["to"] % change
                    # Format rewrite with matched attributes
                    rewrite = rewrite % m.groupdict()
                    store.append([match.strip(), rewrite])

    def issue_tracker_extract_links(self, change):
        change["issue_tracker_links"] = []
        for tracker_regs in self.issue_tracker_links.values():
            for reg in tracker_regs:
                for field in ("title", "text"):
                    self.issue_match_and_rewrite(change, field, reg)
        change["has_issue_tracker_links"] = (
            True if change["issue_tracker_links"] else False
        )

    def remove_plus_0_approvals(self, change: dict) -> dict:
        _change = copy.deepcopy(change)
        _change["approval"] = [
            approval
            for approval in change.get("approval", [])
            if approval and not approval.endswith("+0")
        ]
        return _change

    def enhance(self, change):
        if change["type"] == "Change":
            if self.is_tests_included(change):
                change["tests_included"] = True
            else:
                change["tests_included"] = False
            self.issue_tracker_extract_links(change)
            change = self.remove_plus_0_approvals(change)
        return change


def enhance_changes(changes):
    detector = Detector()
    changes = list(map(detector.enhance, changes))
    return changes


def strip_url(url: str) -> str:
    """
    Remove extra '/' in an url path
    Previous version of Monocle did not ensure
    """
    p = urlparse(url)
    np = "/".join(filter(lambda x: x, p.path.split("/")))
    return urlunparse(p._replace(path=np))


def get_events_list() -> List[str]:
    return [
        "ChangeCreatedEvent",
        "ChangeAbandonedEvent",
        "ChangeMergedEvent",
        "ChangeCommentedEvent",
        "ChangeReviewedEvent",
        "ChangeCommitPushedEvent",
        "ChangeCommitForcePushedEvent",
    ]


def set_params(input: Union[Dict, Namespace]) -> Dict:
    def getter(attr, default):
        if isinstance(input, dict):
            return input.get(attr, default)
        else:
            return getattr(input, attr, default) or default

    params = {}

    params["gte"] = date_to_epoch_ml(getter("gte", None))
    params["lte"] = end_of_day_to_epoch_ml(getter("lte", None))
    params["on_cc_gte"] = date_to_epoch_ml(getter("on_cc_gte", None))
    params["on_cc_lte"] = end_of_day_to_epoch_ml(getter("on_cc_gte", None))

    params["etype"] = getter("type", ",".join(get_events_list())).split(",")

    for e in ("ec_same_date", "tests_included", "self_merged"):
        params[e] = getter(e, False)

    params["size"] = int(getter("size", 10))
    params["from"] = int(getter("from", 0))

    for e in (
        "exclude_authors",
        "authors",
        "approvals",
        "has_issue_tracker_links",
        "exclude_approvals",
        "files",
        "state",
        "change_ids",
        "target_branch",
        "task_type",
        "task_priority",
        "task_severity",
        "project",
    ):
        params[e] = getter(e, None)

    for sp in (
        "state",
        "change_ids",
        "exclude_authors",
        "authors",
        "approvals",
        "exclude_approvals",
        "task_priority",
        "task_severity",
        "task_type",
    ):
        if params.get(sp):
            params[sp] = params[sp].split(",")
    return params
