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
