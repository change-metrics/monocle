#!/bin/sh -e
# Copyright (C) 2021 Monocle authors
# SPDX-License-Identifier: AGPL-3.0-or-later

export $(cat .secrets)

if ! test -d .venv; then
    python3 -mvenv .venv
    ./.venv/bin/pip install --upgrade pip
    ./.venv/bin/pip install -r requirements.txt
fi

if ! test -f .venv/bin/monocle; then
    ./.venv/bin/python3 setup.py install
fi

PORT=${1:-9200}
exec monocle --elastic-conn localhost:$PORT crawler --config etc/config.yaml
