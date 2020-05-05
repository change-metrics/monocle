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
from datetime import datetime

events_list = [
    'ChangeCreatedEvent',
    'ChangeAbandonEvent',
    'ChangeMergedEvent',
    'ChangeCommentedEvent',
    'ChangeReviewedEvent',
    'ChangeCommitPushedEvent',
    'ChangeCommitForcePushedEvent',
]


def date_to_epoch_ml(datestr):
    if not datestr:
        return None
    return int(datetime.strptime(datestr, "%Y-%m-%d").timestamp() * 1000)


def end_of_day_to_epoch_ml(datestr):
    if not datestr:
        return None
    return int(
        datetime.strptime(datestr + ' 23:59:59', "%Y-%m-%d %H:%M:%S").timestamp() * 1000
    )


def dbdate_to_datetime(datestr):
    return datetime.strptime(datestr, "%Y-%m-%dT%H:%M:%SZ")


def float_trunc(f, n=2):
    return float(int(f * 10 ** n)) / 10 ** n


class Detector(object):
    tests_regexp = ".*[Tt]est.*"
    tests_re = re.compile(tests_regexp)

    def is_tests_included(self, change):
        for file in change['changed_files']:
            if self.tests_re.match(file['path']):
                return True
        return False

    def enhance(self, change):
        if change['type'] == 'Change':
            if self.is_tests_included(change):
                change['tests_included'] = True
            else:
                change['tests_included'] = False
        return change


def enhance_changes(changes):
    detector = Detector()
    changes = list(map(detector.enhance, changes))
    return changes


def set_params(input):
    def getter(attr, default):
        if isinstance(input, dict):
            return input.get(attr, default)
        else:
            return getattr(input, attr, default) or default

    params = {}
    params['gte'] = date_to_epoch_ml(getter('gte', None))
    params['lte'] = end_of_day_to_epoch_ml(getter('lte', None))
    params['on_cc_gte'] = date_to_epoch_ml(getter('on_cc_gte', None))
    params['on_cc_lte'] = end_of_day_to_epoch_ml(getter('on_cc_gte', None))
    params['ec_same_date'] = getter('ec_same_date', False)
    params['etype'] = getter('type', ','.join(events_list)).split(',')
    params['exclude_authors'] = getter('exclude_authors', None)
    params['authors'] = getter('authors', None)
    params['approval'] = getter('approval', None)
    params['size'] = int(getter('size', 10))
    params['from'] = int(getter('from', 0))
    params['files'] = getter('files', None)
    params['state'] = getter('state', None)
    params['tests_included'] = getter('tests_included', False)
    params['change_ids'] = getter('change_ids', None)
    params['target_branch'] = getter('target_branch', None)
    if params['change_ids']:
        params['change_ids'] = params['change_ids'].split(',')
    if params['exclude_authors']:
        params['exclude_authors'] = params['exclude_authors'].split(',')
    if params['authors']:
        params['authors'] = params['authors'].split(',')
    return params
