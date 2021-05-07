Monocle with nix
================

The [default.nix](./default.nix) contains nix expression to setup
reproducible builds and deployments.

- codegen-shell: a shell with the protobuf codegen requirements
- codegen-container: a container image with the codegen-shell

Get started by running the `nix-shell` command at the root of the project:

```ShellSession
monocle $ nix-shell
[nix-shell:monocle]$ make
```
