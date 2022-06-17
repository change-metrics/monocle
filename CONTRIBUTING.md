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

Furthermore, deploying from source can be used to better understand how Monocle components interact
together.

Below are the two recommended ways to deploy Monocle from source:

- [By deploying the tool chain on your system](#running-the-services-manually-on-your-host)
- [By deploying the tool chain via NIX](#running-the-services-manually-using-nix)

### Running the services manually on your host

#### Requirements

These requirements are for a Fedora based system. Please adapt them to your own OS if needed.

Please note that the GHC version available on your OS might not fit the Monocle build requirements. Please
ensure the GHC version into [monocle.cabal](./haskell/monocle.cabal) (line: "tested-with:") is the same version
than the version available on your OS.

Run the following commands **as non-root user**:

```ShellSession
sudo dnf install -y podman nodejs git ghc cabal-install zlib-devel python3-virtualenv python3-devel openssl-devel gcc
```

Alternatively, or if the GHC version of your OS does not match the requirement, the Haskell tool chain
can be deployed using a *distro-agnostic* way via [ghc-up](https://www.haskell.org/ghcup):


```ShellSession
curl -sSf https://get-ghcup.haskell.org | sh
```

If the above command fails read the output from more information, usually there are missing dependencies.
Then logout and login again, for the new configurations to be loaded.

#### ElasticSearch

Monocle relies on ElasticSearch as backend to store Changes events.

```ShellSession
./contrib/start-elk.sh 9200
```
Make sure data directory has writing permissions.

#### WebUI

The API serves the webui from `/usr/share/monocle/webapp` or the `web/build` directory.
Build the UI first:

```ShellSession
cd web
npm install
npm build
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
λ> import Monocle.Main
λ> run $ defaultApiConfig 9879 "http://localhost:9200" "../etc/config.yaml"
```

#### WebUI hot reload

The Monocle React WebAPP hot reload service can be started:

```ShellSession
cd web
npm install
REACT_APP_API_URL=http://localhost:8080 npm start
firefox http://localhost:3000
```
If you are running this on non-local machine, set the `REACT_APP_API_URL=http://< machine ip | FQDN >:8080`. FQDN must be known in your network.

#### Start crawlers process

Run this commands to start the Macroscope which is the new Monocle crawler system.

```ShellSession
export $(cat .secrets)
cd haskell
cabal repl monocle
λ> import Macroscope.Worker
λ> import Macroscope.Main
λ> import Monocle.Client (withClient)
λ> withClient "http://127.0.0.1:8080" Nothing $ \client -> runMacroscope 19001 "../etc/config.yaml" client
```

### Running the services manually using NIX

This section describes how to start the Monocle services directly on your host using nix.

If you have not installed nix-shell follow the instructions [here](https://nixos.org/), or from the [manual](https://nixos.org/manual/nix/stable/installation/installing-binary.html).

You can configure the project [cachix](https://cachix.org) binary cache with this command: `nix-shell -p cachix --command "cachix use change-metrics"`.


#### ElasticSearch

```ShellSession
nix-shell --command elasticsearch-start
```

#### Web

```ShellSession
nix-shell --command monocle-web-start
firefox http://localhost:13000
```

If the command fails with `Error: package bs-parse not found or built`, you can try running this command: `rm -Rf web/lib web/node_modules`

#### API

```ShellSession
nix-shell --command monocle-repl
λ> import Monocle.Main
λ> run $ defaultApiConfig 8080 "http://localhost:19200" "../etc/config.yaml"
```

#### Start crawlers process

```ShellSession
nix-shell --command monocle-repl
λ> import Macroscope.Worker
λ> import Macroscope.Main
λ> import Monocle.Client (withClient)
λ> withClient "http://localhost:8080" Nothing $ \client -> runMacroscope 19001 "../etc/config.yaml" client
```

#### Run ghcid

```ShellSession
nix-shell --command monocle-ghcid
```

## Contributing a new driver

There is no specific documentation to cover that topic yet but the source code of
the [GitLab driver](haskell/src/Lentille/GitLab/MergeRequests.hs) might be a good
source of knowledge to hack on a new crawler.

## Running tests

Tests rely on the Elasticsearch service so first you need to ensure the ElasticSearch is running on your system. To start the service use the script `contrib/start-elk.sh` or the related nix-shell command.

### On the Haskell code base

Tests can be executed using:

```ShellSession
export ELASTIC_URL=http://localhost:9200
cd haskell
cabal test
```

Run a single test with:

```ShellSession
cabal test --test-option=-p --test-option='/Test get metrics/'
```

Run linters with:

```ShellSession
nix-shell --command monocle-fast-ci-run
```

When the linters fail, you can fix the issue automatically with:

```ShellSession
nix-shell --command monocle-reformat-run
```

Doctest can be executed using:

```ShellSession
# Ensure doctest-0.20. is installed
cabal install doctest --overwrite-policy=always
# Run doctest through cabal
cabal repl --with-ghc=doctest --ghc-options=-Wno-unused-packages
```

Or using ghcid to automatically run the test when the code changes:

```ShellSession
ghcid --test 'Tests.main'
```

Similarly the api can be automatically restarted:

```ShellSession
ghcid --test 'Monocle.Main.run 8080 "http://localhost:19200" "../etc/config.yaml"'
```

### Update API (protobuf)

The APIs are defined using protobuf. To change them, first you need to update the
protobuf definitions present in the [./protos/ folder](./protos). Then you need to update
the api and web client by running the protoc command using the Makefile:

```ShellSession
$ make codegen
```
