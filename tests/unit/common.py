# MIT License
# Copyright (c) 2020 Fabien Boucher

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

import os
import json
import pprint

from monocle.db.db import ELmonocleDB

FIXTURES_DIR = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'fixtures')
DATASETS = os.path.join(FIXTURES_DIR, 'datasets')


def load_dataset(name):
    with open(os.path.join(DATASETS, name)) as fd:
        data = json.load(fd)
    return data


def load_change(name):
    input_pr = load_dataset(name + '_raw.json')
    xtrd_ref = load_dataset(name + '_extracted.json')
    return input_pr, xtrd_ref


def index_dataset(index, name):
    data = load_dataset(name)
    eldb = ELmonocleDB(index=index)
    eldb.update(data)


class DiffException(Exception):
    def __init__(self, message):
        super().__init__()
        self.message = pprint.pformat(message)

    def __str__(self):
        return '\n\n' + self.message
