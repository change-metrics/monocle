# MIT License
# Copyright (c) 2019 Fabien Boucher

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


from datetime import datetime

events_list = [
    'ChangeCreatedEvent',
    'ChangeAbandonEvent',
    'ChangeMergedEvent',
    'ChangeCommentedEvent',
    'ChangeReviewedEvent',
]


class ExlusiveParametersException(Exception):
    def __init__(self, param1, param2):
        self.param1 = param1
        self.param2 = param2

    def __str__(self):
        return "%s and %s are mutually exclusive" % (self.param1, self.param2)


def date_to_epoch_ml(datestr):
    if not datestr:
        return None
    return int(datetime.strptime(datestr, "%Y-%m-%d").timestamp() * 1000)


def dbdate_to_datetime(datestr):
    return datetime.strptime(datestr, "%Y-%m-%dT%H:%M:%SZ")


def float_trunc(f, n=2):
    return float(int(f * 10 ** n)) / 10 ** n


def set_params(input):
    def getter(attr, default):
        if isinstance(input, dict):
            return input.get(attr, default)
        else:
            return getattr(input, attr, default) or default

    params = {}
    params['gte'] = date_to_epoch_ml(getter('gte', None))
    params['lte'] = date_to_epoch_ml(getter('lte', None))
    params['on_cc_gte'] = date_to_epoch_ml(getter('on_cc_gte', None))
    params['on_cc_lte'] = date_to_epoch_ml(getter('on_cc_gte', None))
    params['ec_same_date'] = getter('ec_same_date', False)
    params['etype'] = getter('type', ','.join(events_list)).split(',')
    params['exclude_authors'] = getter('exclude_authors', None)
    params['authors'] = getter('authors', None)
    params['interval'] = getter('interval', '3h')
    params['approval'] = getter('approval', None)
    params['size'] = int(getter('size', 10))
    if params['exclude_authors'] and params['authors']:
        raise ExlusiveParametersException("excluse_authors", "authors")
    params['from'] = int(getter('from', 0))
    if params['exclude_authors']:
        params['exclude_authors'] = params['exclude_authors'].split(',')
    if params['authors']:
        params['authors'] = params['authors'].split(',')
    return params
