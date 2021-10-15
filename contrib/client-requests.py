# Copyright (C) 2021 Monocle authors
# SPDX-License-Identifier: AGPL-3.0-or-later

# An example client to query the Monocle api using requests
# Checkout the available endpoints in
#  https://github.com/change-metrics/monocle/blob/master/doc/openapi.yaml

import argparse
import requests
import json


def usage():
    parser = argparse.ArgumentParser()
    parser.add_argument("--url")
    parser.add_argument("action", choices=["workspaces", "query", "add-td"])
    parser.add_argument("params", nargs="*")
    return parser.parse_args()


args = usage()

# Query workspaces:
if args.action == "workspaces":
    # Empty request requires an empty object body
    print(requests.post(args.url + "/api/2/get_workspaces", json={}).json())

# Query changes:
elif args.action == "query":
    try:
        (workspace, query) = tuple(args.params)
    except:
        print("usage: query workspace query")
        exit(1)
    query = dict(index=workspace, query=query, query_type="QUERY_CHANGE")
    resp = requests.post(args.url + "/api/2/search/query", json=query).json()
    if resp.get("error"):
        print(resp)
    else:
        for change in resp["changes"]["changes"]:
            print(change["url"], change["title"])

# Add task data to a change
elif args.action == "add-td":
    try:
        (workspace, crawler, apikey, td) = tuple(args.params)
    except:
        print("usage: add-td workspace crawler apikey json")
        exit(1)
    request = dict(
        index=workspace,
        crawler=crawler,
        apikey=apikey,
        entity=dict(td_name=crawler),
        task_datas=[json.loads(td)],
    )
    resp = requests.post(args.url + "/api/2/crawler/add", json=request).json()
    print(resp)
