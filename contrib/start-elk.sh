#!/bin/sh -e
# Copyright (C) 2021 Monocle authors
# SPDX-License-Identifier: AGPL-3.0-or-later

IMAGE=$(awk '/docker.elastic/ { print $2 }' docker-compose.yml* | head -n 1 | sed 's/"//g')

PORT=${1:-9200}

exec podman run -it --rm --name elastic               \
     --env discovery.type=single-node                 \
     --publish "${PORT}:9200/tcp"                     \
     -v "$(pwd)/data:/usr/share/elasticsearch/data:Z" \
     "${IMAGE}"
