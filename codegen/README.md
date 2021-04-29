# Monocle codegen

This tool provides a simple codegen process to convert service defined
in protobuf into api stubs to manage json encoding/decoding for:

- Python Flask
- ReScript Axios

To build the tool:

```ShellSession
# Install the toolchain, (make sure to get cabal --version >= 3.0.0.0)
dnf install -y ghc cabal-install zlib-devel git

# Update repository
cabal update

# Generate code
cabal repl
```
