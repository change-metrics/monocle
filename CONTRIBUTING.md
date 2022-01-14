# Contributing

This document provides some instructions to get started with Monocle development. It covers topics
such as running tests, running services w/o docker-compose or running the codegen.

## Understanding the design choices

Follow the [Architectural Decision Records](doc/adr/index.md) to understand the choices made by the project.

## Deploy from source code

### Running the services manually

This section describes how to start the Monocle services directly on your host without using containers.
This can be used to better understand how the system works and to enable fast reload of local changes.

#### Requirements

These requirements are for a Fedora-based system. Please adapt them to your own OS if needed.

```ShellSession
sudo dnf install -y nginx podman nodejs git ghc cabal-install zlib-devel python3-virtualenv python3-devel openssl-devel gcc
```

If needed, it is possible to install a distro-agnostic Haskell environment using

```ShellSession
curl -sSf https://get-ghcup.haskell.org | sh
```

#### HTTP gateway (nginx)

The Monocle WebAPP and the API endpoints are served though NGINX.

Copy this configuration to `/etc/nginx/conf.d/monocle.conf`

```
server {
  listen 8081;

  gzip on;
  gzip_min_length 1000;
  gzip_types text/plain text/xml application/javascript text/css;
  client_max_body_size 1024M;

  location /api/2/ {
    proxy_pass http://localhost:9879/;
    proxy_http_version 1.1;
  }

  location /auth {
    proxy_pass http://localhost:9879/auth;
    proxy_http_version 1.1;
  }

  # Forward the rest to the node development server
  location / {
    proxy_pass http://localhost:3000;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
    proxy_set_header Host $host;
    proxy_cache_bypass $http_upgrade;
  }
}
```

Then ensure NGINX service is started/reloaded on your system.

#### ElasticSearch

Monocle relies on ElasticSearch as backend to store Changes events.

```ShellSession
./contrib/start-elk.sh 9200
```

#### API

To start the Monocle API run the following commands.

To reload modules when code is updated then type `:reload`.

In addition you could run `ghcid -c 'cabal repl monocle'` in another terminal, then on every save in your IDE you'll see a list of the errors and warnings from ghc.

You might need to restart the repl and/or ghcid when new dependencies are
added in the `monocle.cabal` file.

```ShellSession
export $(cat .secrets)
cd haskell
cabal repl monocle
λ> import Monocle.Api
λ> run 9879 "http://localhost:9200" "../etc/config.yaml"
```

#### Web

The Monocle React WebAPP (hot reload is enabled).

```ShellSession
./contrib/start-web.sh
firefox http://localhost:3000
```

#### Start crawlers process

Run this commands to start the Macroscope which is the new Monocle crawler system.

```ShellSession
export $(cat .secrets)
cd haskell
cabal repl monocle
λ> import Macroscope.Worker
λ> import Macroscope.Main
λ> import Monocle.Client (withClient)
λ> withClient "http://127.0.0.1:8081" Nothing $ \client -> runMacroscope True "../etc/config.yaml" 30 client
```

### Running the services manually using NIX

This section describes how to start the Monocle services directly on your host using nix.

Note that the commands below can be started in emacs buffer using:

```ShellSession
nix-shell --command launch-monocle-with-emacs
```

#### HTTP gateway (nginx)

```ShellSession
nix-shell --command nginx-start
```

#### ElasticSearch

```ShellSession
nix-shell --command elk-start
```

#### API

```ShellSession
nix-shell --command monocle-api2-start
λ> import Monocle.Api
λ> run 19875 "http://localhost:19200" "../etc/config.yaml"
```

In another terminal you could run ghcid with

```ShellSession
nix-shell --command monocle-ghcid
```

#### Web

```ShellSession
nix-shell --command monocle-web-start
firefox http://localhost:13000
```

#### Start crawlers process

```ShellSession
nix-shell --command monocle-api2-start
λ> import Macroscope.Worker
λ> import Macroscope.Main
λ> import Monocle.Client (withClient)
λ> withClient "http://localhost:18080" Nothing $ \client -> runMacroscope 19001 "../etc/config.yaml" client
```

## Contributing a new driver

There is no specific documentation to cover that topic yet but the source code of
the [GitLab driver](monocle/haskell/src/Lentille/GitLab/MergeRequests.hs) might be a good
source of knowledge to hack on a new crawler.

## Running tests

Tests rely on the Elasticsearch service so first you need to ensure the ElasticSearch is running on your system. To start the service use the script `contrib/start-elk.sh` or the
related nix-shell command.

### On the Haskell code base

Tests can be executed using:

```ShellSession
export ELASTIC_URL=http://localhost:9200
cd haskell
cabal test
```

Doctest can be executed using:

```ShellSession
# Ensure doctest-0.20. is installed
cabal install doctest --overwrite-policy=always
# Run doctest through cabal
cabal repl --with-ghc=doctest
```

Or using ghcid to automatically run the test when the code changes:

```ShellSession
ghcid --test 'Monocle.Test.Spec.main'
```

Similarly the api can be automatically restarted:

```ShellSession
ghcid --test 'Monocle.Api.run 19875 "http://localhost:19200" "../etc/config.yaml"'
```

## Update API (protobuf)

The APIs are defined using protobuf. To change them, first you need to update the
protobuf definitions present in the [./protos/ folder](./protos). Then you need to update
the api and web client by running the protoc command using the Makefile:

```ShellSession
$ make codegen
```
