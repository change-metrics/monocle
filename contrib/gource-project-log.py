#!/usr/bin/env python

'''Generate log lines for the activity of the merged PR following the
gource format.

Usage:

$ ./gource-project-log.py | sort -n > log
$ gource log
'''

import argparse
import datetime
import sys

from monocle import utils
from monocle.db.db import ELmonocleDB

# Gource custom log format from https://github.com/acaudwell/Gource :
#
# timestamp - A unix timestamp of when the update occured.
# username  - The name of the user who made the update.
# type      - initial for the update type - (A)dded, (M)odified or (D)eleted.
# file      - Path of the file updated.
# colour    - A colour for the file in hex (FFFFFF) format. Optional.


def main():
    parser_dbquery = argparse.ArgumentParser(prog=sys.argv[0])
    parser_dbquery.add_argument(
        '--repository', help='Scope to events of a repository (regexp)', default=r'.*'
    )
    parser_dbquery.add_argument('--gte', help='Scope to events created after date')
    parser_dbquery.add_argument('--lte', help='Scope to events created before date')
    parser_dbquery.add_argument(
        '--size', help='Return maximum of size results', default=1000
    )
    parser_dbquery.add_argument(
        '--exclude-authors', help='Authors exclude list (comma separated)'
    )
    args = parser_dbquery.parse_args()

    db = ELmonocleDB()
    params = utils.set_params(args)
    data = db.run_named_query(
        'last_merged_changes', args.repository.lstrip('^'), params
    )

    lte_time = (
        datetime.datetime.strptime(args.lte, '%Y-%m-%d') + datetime.timedelta(days=1)
        if args.lte
        else None
    )

    title = {}

    for entry in data:
        # example: 2020-02-24T19:05:13Z
        created_time = datetime.datetime.strptime(
            entry['created_at'], '%Y-%m-%dT%H:%M:%SZ'
        )
        merge_time = datetime.datetime.strptime(
            entry['merged_at'], '%Y-%m-%dT%H:%M:%SZ'
        )
        if lte_time and merge_time > lte_time:
            continue

        print(
            '%.0f|%s|A|/%s/%s|'
            % (
                created_time.timestamp(),
                entry['author'],
                entry['repository_fullname'],
                entry['title'],
            )
        )
        print(
            '%.0f|%s|M|/%s/%s|'
            % (
                merge_time.timestamp(),
                entry['author'],
                entry['repository_fullname'],
                entry['title'],
            )
        )
        title[entry['repository_fullname_and_number']] = entry['title']

    params['etype'] = ("ChangeCommentedEvent",)
    data = db.run_named_query('_scan', args.repository.lstrip('^'), params)

    for entry in data:
        # example: 2020-02-24T19:05:13Z
        created_time = datetime.datetime.strptime(
            entry['created_at'], '%Y-%m-%dT%H:%M:%SZ'
        )
        try:
            print(
                '%.0f|%s|M|/%s/%s|'
                % (
                    created_time.timestamp(),
                    entry['author'],
                    entry['repository_fullname'],
                    title[entry['repository_fullname_and_number']],
                )
            )
        except KeyError:
            print(
                '%s not merged' % entry['repository_fullname_and_number'],
                file=sys.stderr,
            )


if __name__ == "__main__":
    main()

# gource-project-log.py ends here
