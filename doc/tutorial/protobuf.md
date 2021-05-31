How to update the Monocle API defined with Protobuf
===================================================

This tutorial teachs you how to use the protobuf definition to create a new API named `GetInfo`
You can read the [Choice of Protobuf Decision Record](../adr/0010-choice-of-protobuf.md) to learn more about the design.

- In Monocle, we use protobuf *messages* to define the API inputs and outputs.
- Then we use the protobuf *JSON* encoding to exchange those messages over HTTP.
- Finally, we use protobuf *services* along with the `google.api.http` package to define the API path location.

We'll do each step by hand and we'll see how it works in practice for Monocle.

# Install the requirements

Get the codegen container by running: `podman pull changemetrics/monocle_codegen`

Or build the image locally using:

```ShellSession
$ TMPDIR=/tmp/ podman build -f Dockerfile-codegen -t changemetrics/monocle_codegen .
```

# Create the Info message

In a file named `./protos/monocle/info.proto` you add this content:

```protobuf
syntax = "proto3";
package monocle_info;
option go_package = "monocle/info";

message Info {
  string version = 1;
  string title = 2;
}
```

The Info message contains two fields: `version` and `title`.

> Learn more about protobuf syntax with the [proto3 guide](https://developers.google.com/protocol-buffers/docs/proto3).

Then you generate python bindings with this command:

```ShellSession
$ podman run -it -v $(pwd):/data:z --rm changemetrics/monocle_codegen \
    protoc -I=./protos/ --python_out=./monocle --mypy_out=./monocle \
      monocle/info.proto
```

... and you can use the message using a python REPL:

```ShellSession
$ ./.tox/py3/bin/python
>>> import monocle.monocle.info_pb2 as info_pb2
>>> info_message = info_pb2.Info(version="0.9", title="My Monocle")
>>> info_message
version: "0.9"
title: "My Monocle"
```

Then you generate javascript bindings with this command:

```ShellSession
$ podman run -it -v $(pwd):/data:z --rm changemetrics/monocle_codegen \
    ocaml-protoc -bs -ml_out web/src/messages protos/monocle/info.proto
```

... and  you can use the message using a javascript REPL:

```ShellSession
$ (cd web; node)
> const info_pb2 = require('./src/messages/info_types.bs.js')
> info_pb2.default_info("0.9", "My Monocle")
{ version: '0.9', title: 'My Monocle' }
```

# Encode the Info message in JSON

To exchange the message between the api and the client, we use the protobuf JSON encoding.

With the python REPL:

```ShellSession
>>> from google.protobuf import json_format as pbjson

# Encode
>>> pbjson.MessageToJson(info_message)
'{\n  "version": "0.9",\n  "title": "My Monocle"\n}'
>>> open("info_message.json", "w").write(_)

# Decode
>>> pbjson.Parse('{"version":"0.9"}', info_pb2.Info())
version: "0.9"
```

With the javascript REPL:

```ShellSession
> const info_pbjson = require('./src/messages/info_bs.bs.js')
> const info_message = require('../info_message.json')

# Decode
> info_pbjson.decode_info(info_message)
{ version: '0.9', title: 'My Monocle' }

# Encode
> JSON.stringify(info_pbjson.encode_info({version: "0.9"}))
'{"version":"0.9"}'
```

Upstream libraries documentation is available at:

- https://googleapis.dev/python/protobuf/latest/google/protobuf/json_format.html
- https://github.com/mransan/bs-protobuf-demo

> The protobuf JSON encoding mapping documentation: [pbjson](https://developers.google.com/protocol-buffers/docs/proto3#json).

# Define the HTTP service

To define how to exchange this message, you add a Request and Response messages at the end of `info.proto` file:

```protobuf
message InfoRequest {
}

message InfoResponse {
  Info info = 1;
}
```

Then add the new service to the `http.proto` file in the `protos/monocle` folder:

```protobuf
import "monocle/task_data.proto";

service Info {
  // Return the list of projects
  rpc GetInfo(monocle_info.InfoRequest) returns (monocle_info.InfoResponse) {
    option (google.api.http) = {
      post: "/api/1/info"
      body: "*"
    };
  }
}
```

... and run this command to update the OpenAPI definition:

```ShellSession
$ podman run -it -v $(pwd):/data:z --rm changemetrics/monocle_codegen \
    protoc -I=./protos/ -I/usr/src/ \
        --openapi_out=./doc/ \
        monocle/http.proto
```

> Notice that we always define inputs, even when they are empty, and we only use the POST verb.

# Implement the API in Monocle

We would like to avoid doing the above manually, thus Monocle provide a codegen utility
to automate this process in three steps:

- 1. Write protobuf definitions in the `protos/` folder, (and add newly created files to the Makefile `MESSAGES` list)
- 2. Generate the code by running `make codegen-with-container`
- 3. Implement the API, e.g. in `monocle/api.py` and open the `web/src/components/WebApi.res` module to use it.

> You can check the generated api stub in `monocle/webapi.py`
