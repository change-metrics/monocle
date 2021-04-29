# Monocle codegen

This tool provides a simple codegen process to create data types
from protobuf to python and rescript.

To build the tool:

```ShellSession
# Install the toolchain, (make sure to get cabal --version >= 3.0.0.0)
dnf install -y ghc cabal-install zlib-devel git

# Update repository
cabal update

# Generate code
cabal repl
```
