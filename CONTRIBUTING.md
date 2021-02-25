# Contributing

## Understanding the design choices

Follow the [Architectural Decision Records](doc/adr/index.md) to
understand the choices made by the project.

## To deploy from source code

To use `docker-compose` from source code, use the
`docker-compose.yml.dev` file instead of the image based one:

```ShellSession
$ ln -sf docker-compose.yml.dev docker-compose.yml
```

## To run the tests

Tests rely on the Elasticsearch service so first you need to ensure the Elasticsearch container is running.
You can run the "docker-compose up -d" or you can only run the Elasticsearch container by running "docker-compose start elastic".

If you don't have docker-compose, you can start the elasticsearch service with podman by running this command:

```ShellSession
$ podman run --env discovery.type=single-node --publish 9200:9200/tcp --name elastic --rm $(awk '/docker.elastic/ { print $2 }' docker-compose.yml* | head -n 1 | sed 's/"//g')
```

Then the tests can be executed using:

```ShellSession
$ tox
```

## Reloading code

This section explains how you can hack the Monocle code. The idea is to use
the docker deployment to avoid complex development methods.

After changes, simply run the following command. docker-compose will
rebuild and re-spawn the changed containers:

```ShellSession
$ docker-compose up -d --build
```

### auto-reloading the web UI code

To have your code automatically reloaded on each change, just run it
outside the container:

```ShellSession
$ docker-compose stop web
$ cd web
$ npm install
$ npm start
```

## Git hooks

Before submitting a [Pull Request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request-from-a-fork),
make sure to configure the git hooks locally to avoid proposing broken code
or not well formatted code.

### pre-push

To be sure to push correct branches, you have to configure the
`pre-push` git hook by creating `.git/hooks/pre-push` with the
following content:

```Shell
#!/bin/bash

exec ./contrib/pre-push "$@"
```

and making it executable with `chmod +x .git/hooks/pre-push`.

### pre-commit

To be sure to have correctly formatted code, enable the `pre-commit`
git hook to reformat your code by creating `.git/hooks/pre-commit`
with the following content:

```Shell
#!/bin/bash

exec ./contrib/pre-commit "$@"
```

and making it executable with `chmod +x .git/hooks/pre-commit`.

## Contributing a new driver

Please refer to the file `dummy/change.py` that contain a Dummy driver and
some comments to help you get started.

## Use podman commands for start Monocle

If you don't want to use docker-compose or podman-compose tools for
deploy Monocle, you are able to run script for start services:

```Shell
# Set public IP address
PUBLIC_ADDRESS='<YOUR IP ADDRESS>'

# build dev images:
podman build -t monocle_web -f web/Dockerfile web
podman build -t monocle_crawler -f ./Dockerfile .
podman build -t monocle_api -f ./Dockerfile .

cat << EOF > deploy-monocle.sh
podman run --name=monocle_elastic_1 \
           -d \
           -e ES_JAVA_OPTS="-Xms512m -Xmx512m" \
           -e discovery.type="single-node" \
           -v ./data:/usr/share/elasticsearch/data:Z \
           -p 9200:9200 \
           --expose 9200 \
           --ulimit nofile=65535:65535 \
           --healthcheck-command 'curl --silent --fail localhost:9200/_cluster/health' \
           --healthcheck-interval 30s \
           --healthcheck-timeout 30s \
           --healthcheck-retries 3 \
           docker.elastic.co/elasticsearch/elasticsearch:7.10.1

podman run --name=monocle_api_1 \
           --network host \
           --env-file .env \
           -d \
           -e CONFIG=/etc/monocle/config.yaml \
           -e ELASTIC_CONN=elastic:9200 \
           -e ALLOW_ORIGIN=http://$PUBLIC_ADDRESS:3000 \
           -e CLIENT_ID= \
           -e CLIENT_SECRET= \
           -e REDIRECT_URL=http://$PUBLIC_ADDRESS:9876/api/0/authorize \
           -e WEB_URL=http://$PUBLIC_ADDRESS:3000 \
           -v ./etc:/etc/monocle:Z \
           --add-host elastic:127.0.0.1 \
           --add-host monocle_elastic_1:127.0.0.1 \
           -p 9876:9876 \
           -p 9877:9877 \
           --expose 9876-9877 \
           --healthcheck-command 'python -c "import requests,sys; r=requests.get('"'"'http://localhost:9876/api/0/health'"'"'); print(r.text); sys.exit(1) if r.status_code!=2
00 else sys.exit(0)"' \
           monocle_api sh -c 'uwsgi --uid guest --gid nogroup --http :9876 --socket :9877 --manage-script-name --mount /app=monocle.webapp:app'

podman run --name=monocle_crawler_1 \
           --env-file .env \
           -d \
           -e APP_ID= \
           -e APP_KEY_PATH=/etc/monocle/app_key.rsa \
           -v ./etc:/etc/monocle:Z \
           -v ./dump:/var/lib/crawler:Z \
           --add-host elastic:127.0.0.1 \
           --add-host monocle_elastic_1:127.0.0.1 \
           monocle_crawler sh -c 'monocle --elastic-conn elastic:9200 crawler --config /etc/monocle/config.yaml'

podman run --name=monocle_web_1 \
           --env-file .env \
           -d \
           -e REACT_APP_API_URL=http://$PUBLIC_ADDRESS:9876 \
           -e REACT_APP_TITLE='' \
           -p 3000:3000 \
           --add-host api:127.0.0.1 \
           --add-host monocle_api_1:127.0.0.1 \
           --add-host elastic:127.0.0.1 \
           --add-host monocle_elastic_1:127.0.0.1 \
           monocle_web
EOF
sh deploy-monocle.sh
```
