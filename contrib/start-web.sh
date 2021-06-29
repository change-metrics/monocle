#!/bin/sh -e
# Copyright (C) 2021 Monocle authors
# SPDX-License-Identifier: AGPL-3.0-or-later

cd web

if ! test -d node_modules; then
    npm install
fi

exec npm start
