#!/bin/bash

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


####### USAGE #######


# This script is an helper for the developer to ease building a development
# environment using podman instead of docker.
#
# Build container images using the "build" parameter
# Create container using the "create" parameter
# Start container using the "start" parameter
# -> firefox http://localhost:3000
#
# crawler and api container use a bind the monocle python module, so
# you can hack the crawler and api code and simply restart the containers with
# podman restart monocle_api monocle_crawler
#
# To hack the WEB UI (after a complete run with "start") run
# the script with the "start-web-dev" parameter.

set -x

export PUBLIC_ADDRESS=localhost

if [ "$1" == "build" ]; then
    podman build -t monocle_web -f web/Dockerfile web
    podman build -t monocle_backend -f Dockerfile .
fi

if [ "$1" == "create" ]; then

    podman pod create -p 9200:9200 -p 9876:9876 -p 3000:3000 -n monocle

    podman create --name=monocle_elastic \
               --pod monocle \
               -e ES_JAVA_OPTS="-Xms512m -Xmx512m" \
               -e discovery.type="single-node" \
               --ulimit nofile=65535:65535 \
               -v ./data:/usr/share/elasticsearch/data:Z \
               docker.elastic.co/elasticsearch/elasticsearch:7.10.1

    podman create --name=monocle_api \
               --pod monocle \
               -e CONFIG=/etc/monocle/config.yaml \
               -e ELASTIC_CONN=monocle_elastic:9200 \
               -e ALLOW_ORIGIN=http://$PUBLIC_ADDRESS:3000 \
               -e WEB_URL=http://$PUBLIC_ADDRESS:3000 \
               --add-host monocle_elastic:127.0.0.1 \
               -v $PWD/etc:/etc/monocle:Z \
               -v $PWD/monocle:/code/monocle:Z \
               -it \
               monocle_backend uwsgi --http :9876 --manage-script-name --mount /app=monocle.webapp:app

    podman create --name=monocle_crawler \
               --pod monocle \
               --add-host monocle_elastic:127.0.0.1 \
               -v $PWD/etc:/etc/monocle:Z \
               -v $PWD/dump:/var/lib/crawler:Z \
               -it \
               -v $PWD/monocle:/code/monocle:Z \
               monocle_backend monocle --elastic-conn monocle_elastic:9200 crawler --config /etc/monocle/config.yaml

    podman create --name=monocle_web \
               --pod monocle \
               --add-host monocle_api:127.0.0.1 \
               --add-host monocle_elastic:127.0.0.1 \
               monocle_web bash -c "set -x; sed -i -e \"s@__API_URL__@http://$PUBLIC_ADDRESS:9876@\" build/index.html; sed -i -e \"s@__TITLE__@Monocle Dev Podman deployment@\" build/index.html; serve -s /app/ -l 3000"
fi

if [ "$1" == "rm" ]; then
    podman pod rm monocle
fi

if [ "$1" == "start" ]; then
    podman pod start monocle
    # Re-attempt to start monocle_crawler - sometime race condition failure
    podman start monocle_crawler
fi

if [ "$1" == "stop" ]; then
    podman pod stop monocle
fi

if [ "$1" == "start-web-dev" ]; then
    podman stop monocle_web
    podman stop monocle_api
    podman rm monocle_api
    podman create --name=monocle_api \
               --pod monocle \
               -e CONFIG=/etc/monocle/config.yaml \
               -e ELASTIC_CONN=monocle_elastic:9200 \
               -e ALLOW_ORIGIN=http://$PUBLIC_ADDRESS:3001 \
               -e WEB_URL=http://$PUBLIC_ADDRESS:3001 \
               --add-host monocle_elastic:127.0.0.1 \
               -v $PWD/etc:/etc/monocle:Z \
               -v $PWD/monocle:/usr/local/lib/python3.9/site-packages/monocle:Z \
               -it \
               monocle_backend uwsgi --http :9876 --manage-script-name --mount /app=monocle.webapp:app
    podman start monocle_api
    cd web && npm install && PORT=3001 npm start
fi
