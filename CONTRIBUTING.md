# Contributing

This document provides some instructions to get started with Monocle development. It covers topics
such as running tests, running services or running the codegen.

## Understanding the design choices

Follow the [Architectural Decision Records](doc/adr/index.md) to understand the choices made by the project.

## Deploy from source code

See [Instructions from the README.md](README.md#checkout-the-code) to checkout the code and prepare
the .secret file.

According to the [README.md](README.md#installation), the recommended way to deploy Monocle is via
Docker compose, however the containerized deployment does not fit well for building a development environment.
Furthermore, deploying from source can be used to better understand how Monocle components interact together.

The recommended ways to deploy and hack the source is via nix. Nix gives you the same environment as a
production build along with developer tools. However the project could still be built with ghcup+cabal
using the [cabal-override.project file](README.md#build-using-cabal).

To install nix, follow the instructions [here](https://nixos.org/), or from the [manual](https://nixos.org/manual/nix/stable/installation/installing-binary.html).

You can configure the project [cachix](https://cachix.org) binary cache with this command: `nix shell nixpkgs#cachix --command cachix use change-metrics`.

The first "nix develop" command might takes long as data must be fetched from the nix cache.

### Running the services

This section describes how to start the Monocle services directly on your host.

#### Start ElasticSearch

```ShellSession
just elastic
```

#### Start the Monocle API

The API serves the web UI and the Monocle API. The web App must be built first by running:

```ShellSession
just build-web
```

Then, ensure you have set the Monocle config file `config.yaml` (see [README.md](README.md#configuration)) and run:

```ShellSession
just repl
λ> import Monocle.Main
λ> run $ defaultApiConfig 8080 "http://localhost:19200" "etc/config.yaml"
```

… or by running the executable:

```ShellSession
just api
```

> Make sure to setup your .env file, e.g. with:
> (echo CRAWLERS_API_KEY=secret; echo MONOCLE_CONFIG=./etc/config.yaml) > .env

The Monocle UI should be accessible:

```ShellSession
firefox http://localhost:8080
```

#### Start the Monocle crawler process

```ShellSession
just repl
λ> Monocle.Client.withClient "http://localhost:8080" Nothing $ \client -> Macroscope.Main.runMacroscope 19001 "etc/config.yaml" client
```

… or by running the executable:

```ShellSession
just crawler
```

### Start the CLI

Display the CLI help:

```
nix run . -- --help
```

For instance running a crawler:

```
nix run . -- lentille github-projects --url https://api.github.com/graphql --token <a-valid-token> --organization change-metrics
```

### Build the documentation

Build and read the code documentation:

```
just docs
```

## nix-develop

The nix develop shell provides development tooling such as:

- Haskell Language Server
- Fourmolu
- Hlint

Then one could simply run its code editor from the `nix develop` shell and benefit
the tooling available in the PATH:

```ShellSession
monocle $ nix develop
[nix(monocle)]$ code .
```

You might need to install the right Haskell plugin for your editor.

## Extra toolings

### Run ghcid

ghcid automatically re-compiles the code when a haskell file change and display compilation
errors and warnings.

```ShellSession
just ghcid
```

### Run hoogle

Hoogle generates the documentation from the Monocle source code.

```ShellSession
just hoogle-monocle
```

> To avoid building monocle, you can start hoogle with only the dependencies using `just hoogle`

You can access the generated documentation on port 8081.

## Running tests

Tests rely on the Elasticsearch service so first you need to ensure that the ElasticSearch is running on your system.
Ensure the service is started by running: `nix develop --command elasticsearch-start`

Run linters (fourmolu and hlint) with:

```ShellSession
just ci-fast
```

When the linters fail, you can fix the issue automatically with:

```ShellSession
just fmt
```

Run the full test suite with:

```ShellSession
just ci
```

Run a single test (on failure, tasty provides a pattern to re-run a single test):

```ShellSession
just test "PATTERN"
```

## Start the web development server

Start the web dev server (hot-reload):

```ShellSession
just web
firefox http://localhost:13000
```

If the command fails with `Error: package bs-parse not found or built`, you can try running this command: `rm -Rf web/lib web/node_modules`

If the API is not running locally, sets the `MONOCLE_PUBLIC_URL=http://monocle-api:8080` environment before running `monocle-web-start`

## Automatic restart of the API

The api can be automatically restarted when code change:

```ShellSession
export MONOCLE_CONFIG=./etc/config.yaml
export MONOCLE_ELASTIC_URL=http://localhost:19200
export MONOCLE_PUBLIC_URL=http://localhost:8080
export CRAWLERS_API_KEY=$(uuidgen)
ghcid --set ":set args api" --test 'CLI.main'
```

## Run Kibana to introspect Elasticsearch indices


Start Kibana with:

```ShellSession
just kibana
```

Then access http://localhost:5601

## Fake data provisionning

Provisonning fake data (only fake changes are supported) can be done using the repl:

```
just repl
λ> Monocle.Backend.Provisioner.runProvisioner "etc/config.yaml" "http://localhost:19200" "demo-fake-data" 300
```

Prior to run the provisonner, add the *demo-fake-data* workspace in the config file with an
empty *crawlers* list, then start the monocle API.


## Run the codegen (protobuf)

The APIs are defined using protobuf. To change them, first you need to update the
protobuf definitions present in the [./schemas/monocle folder](./schemas/monocle). Then you need to update
the api and web client by running the protoc command using the Makefile:

```ShellSession
just codegen
```

## Create a monocle build

This produces a monocle binary:

```
$(nix build . --print-out-paths)/bin/monocle --help
```

## Build the container image

```
podman load < $(nix build .#containerMonocle)
podman build -t quay.io/change-metrics/monocle:latest .
```

## Build using cabal

Despite that nix is the recommended way to build Monocle from source, using cabal should
be possible using the command below:

```
cabal build --project-file=cabal-override.project
```

Please open an issue if this fails to build.

## Contributing a new driver

There is no specific documentation to cover that topic yet but the source code of
the [GitLab driver](src/Lentille/GitLab/MergeRequests.hs) might be a good
source of knowledge to hack on a new crawler.

## Monitoring containers

```ShellSession
podman load < $(nix build .#containerPrometheus)
podman load < $(nix build .#containerGrafana)
```

Test the containers:

```ShellSession
podman run --network host -v prom-data:/var/lib/prometheus:Z -e API_TARGET=localhost:8080 --rm quay.io/change-metrics/monocle-prometheus:latest
podman run -it --rm --network host quay.io/change-metrics/monocle-grafana:latest
```

## Example query

Add a crawler error:

```ShellSession
curl -X POST -d '{"index": "monocle", "crawler": "demo", "apikey": "secret", "entity": {"project_name": "neutron"}, "errors": [{"created_at": "2023-12-22T10:11:12Z"}]}' -H "Content-type: application/json" localhost:8080/api/2/crawler/add
```

Get crawler errors:

```ShellSession
curl -X POST -d '{"index": "monocle"}' -H "Content-type: application/json" localhost:8080/api/2/crawler/errors
```

## Debug by dumping every stacktrace

When the service fails with an obscure `NonEmpty.fromList: empty list`, run the following commands to get the full stacktrace:

```ShellSession
cabal --ghc-options="-fprof-auto" --enable-executable-profiling --enable-profiling --enable-library-profiling -O0 run exe:monocle -- api +RTS -xc -RTS
```

Note that this also shows legitimate exceptions that are correctly caught, but hopefully you should see something like:

```
*** Exception (reporting due to +RTS -xc): (THUNK_1_0), stack trace:
  GHC.IsList.CAF
  --> evaluated by: Monocle.Backend.Index.getChangesByURL,
  called from Monocle.Backend.Index.taskDataAdd,
  called ...
NonEmpty.fromList: empty list
```
