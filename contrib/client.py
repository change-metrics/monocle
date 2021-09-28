# Copyright (C) 2021 Monocle authors
# SPDX-License-Identifier: AGPL-3.0-or-later

# An example client to query the api

import argparse
import monocle.webapi as M


def usage():
    parser = argparse.ArgumentParser()
    parser.add_argument("--url")
    parser.add_argument("workspace")
    parser.add_argument("query")
    return parser.parse_args()


def main():
    args = usage()

    # Query workspaces:
    print(M.config_get_workspaces(args.url, M.GetWorkspacesRequest()))

    # Query changes:
    query = M.QueryRequest(
        index=args.workspace, query=args.query, query_type=M.QueryRequest.QUERY_CHANGE
    )
    resp = M.search_query(args.url, query)
    if resp.error.message:
        print(resp)
    else:
        for change in resp.changes.changes:
            print(change.url)


if __name__ == "__main__":
    main()
