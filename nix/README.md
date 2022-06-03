Monocle with nix
================

The [default.nix](./default.nix) contains nix expression to setup
reproducible builds and deployments.

Get started by running the `nix-shell` command at the root of the project:

```ShellSession
monocle $ nix-shell
[nix-shell:monocle]$ make
```

Starts the hoogle search engine:

```ShellSession
nix-shell --command "hoogle server -p 8080 --local --haskell"
```

Build the project:

```
export MONOCLE_COMMIT=$(git show HEAD --format="format:%H" -q)
$(nix-build --attr monocle)/bin/monocle-api --help
podman load < $(nix-build --attr containerBackend)
```

Build the monitoring containers:

```ShellSession
podman load < $(nix-build --attr containerPrometheus)
podman load < $(nix-build --attr containerGrafana)
```

Test the containers:

```ShellSession
podman run --network host -v prom-data:/var/lib/prometheus:Z -e API_TARGET=localhost:8080 --rm quay.io/change-metrics/monocle-prometheus:latest
podman run -it --rm --network host quay.io/change-metrics/monocle-grafana:latest
```
