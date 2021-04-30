# Monocle codegen

This tool provides a simple codegen process to create HTTP API
endpoint to handle JSON encoding for the protobuf service definition:

- Python Flask routes.
- Javascript Axios client.

To build the tool:

```ShellSession
# Install the toolchain, (make sure to get cabal --version >= 3.0.0.0)
dnf install -y ghc cabal-install zlib-devel git

# Update repository
cabal update

# Generate code
cabal repl
```

Read the [monocle protobuf tutorial](../doc/tutorial/protobuf.md) to learn
more about the code generation process.
