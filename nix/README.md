Monocle with nix
================

> Install nix with https://nixos.org/manual/nix/unstable/installation/installing-binary.html

The [default.nix](./default.nix) file contains useful nix expressions
to work with monocle. Here are some examples usage:

## Test with nix-shell

At the root of the project run the `nix-shell` command. Using the
default shell.nix file, the command creates a bash session with
all the tools needed to develop monocle:

- `nginx-start` to start the web gateway
- `elk-start` to start the database service
- `monocle-web-start` to start the web app using hot-reload service
- `monocle-api-start` to start a repl with the api modules

Check the [CONTRIBUTING.md](../CONTRIBUTING.md) documentation for
service documentations.

## Inspect the derivations

A package in nix is called a derivation corresponding to a node in
the store. The default.nix file is a set of derivations that can be
accessed using the `--attr name` argument.

For example to inspect the derivation for the python requirements:

```ShellSession
$ nix-instantiate --attr python-req ./default.nix
[print store paths ending with '.drv']

$ nix-store -q -R $(nix-instantiate --attr python-req ./default.nix)
[print a list of the dependencies]
```

To operate on the derivation output only (without its build requirements),
use the `nix path-info` command. Combine the commands like this:

```ShellSession
$ nix path-info $(nix-instantiate --attr python-req ./default.nix)
[print store path directory containing the runtime]

$ nix-store -q --tree $(nix path-info $(nix-instantiate --attr python-req ./default.nix))
[print a tree of the runtime dependencies]
```

## Install

To use a command outside of the nix-shell, install it globally with `nix-env`:

```ShellSession
$ nix-env -i /nix/store/path...
[install the derivation in ~/.nix-profile/]

$ nix-env -e '.*elk.*'
[remove elk from profile]
```

## Copy

Copy a derivation between hosts with `nix-copy-closure`:

```ShellSession
localhost$ nix-copy-closure --from ssh://server /nix/store/path...
```

For example to copy the services:

```ShellSession
server$ nix path-info $(nix-instantiate --attr services-req ./default.nix) > ~/monocle-services.drvs

localhost$ nix-copy-closure --from ssh://server $(ssh server cat monocle-services.drvs)
localhost$ nix-env -i $(ssh server cat monocle-services.drvs)
```
