* Status:
* Deciders:
* Date: 2021-04-22
* Issue: https://github.com/change-metrics/monocle/issues/346

## Context and Problem Statement

* The current API implementations is spread across multiple modules (such as webapp, config, db and query) without a formal definition of the inputs/outputs. This induces inconsistencies and we foresee technical difficulties to maintain and evole the service.
* We would like to use an interface description language to define the API between the various component (such as between the webapi and webclients, worker and database).
* How do we define and maintain the interfaces?

## Considered Options

* OpenAPI Swagger
* Protobuf
* Thrift

## Decision Outcome

Chosen option: "Protobuf", because it comes out best (see below).

### Positive Consequences

- We need to start defining interfaces outside of their implementations.
- We need to use a simpler HTTP api (e.g. the path for the action function name and the body for its input), instead of encoding the inputs with a mix of path, querystring and body components.
  For example, use `POST /get_projects BODY { "index": "name" }` instead of `GET /projects?index=name`.

### Negative Consequences

- The team needs to learn a new language.
- Follow-up decisions required for the actual implementation.

## Pros and Cons of the Options

### OpenAPI

Zuul example: https://opendev.org/zuul/zuul/raw/branch/master/web/public/openapi.yaml (generated webclient: https://zuul.opendev.org/openapi ).

* Good, because it defines common HTTP APIs.
* Bad, because it is hard to maintain.

### Protobuf

Google APIs example: https://github.com/googleapis/googleapis.
ETCD APIs [model kv.proto](https://github.com/etcd-io/etcd/blob/bad0b4d5131629f0d7fe3d572cb7953548f54afd/api/mvccpb/kv.proto) and [rpc.proto](https://github.com/etcd-io/etcd/blob/bad0b4d5131629f0d7fe3d572cb7953548f54afd/api/etcdserverpb/rpc.proto).

Proof of concept for the upcoming filter sugestion api of Monocle:

```protobuf
// monocle.proto
// Copyright: (c) 2021 Monocle authors
// SPDX-License-Identifier: AGPL-3.0-only

syntax = "proto3";

import "google/api/annotations.proto";
option go_package = "monocle/config";

message ProjectDefinition {
  string name = 1;
  string repository_regex = 2;
  string branch_regex = 3;
  string file_regex = 4;
}

message GetProjectsRequest {
  string index = 1;
}

message GetProjectsResponse {
  repeated ProjectDefinition projects = 1;
}

service Monocle {
    rpc GetProjects(GetProjectsRequest) returns (GetProjectsResponse) {
      option (google.api.http) = {
        post: "/api/1/get_projects"
        body: "*"
    };
  }
}
```

```ShellSession
# Generate openapi from protobuf definition
protoc search.proto -I/home/tdecacqu/src/github.com/googleapis/googleapis/ -I. --openapi_out=.
```

```yaml
# monocle-openapi.yaml
openapi: 3.0.3
info:
    title: Monocle
    version: 0.0.1
paths:
    /api/1/get_projects:
        post:
            operationId: Monocle_GetProjects
            requestBody:
                content:
                    application/json:
                        schema:
                            $ref: '#/components/schemas/GetProjectsRequest'
                required: true
            responses:
                "200":
                    description: OK
                    content:
                        application/json:
                            schema:
                                $ref: '#/components/schemas/GetProjectsResponse'
components:
    schemas:
        GetProjectsRequest:
            properties:
                index:
                    type: string
        GetProjectsResponse:
            properties:
                projects:
                    type: array
                    items:
                        $ref: '#/components/schemas/ProjectDefinition'
        ProjectDefinition:
            properties:
                name:
                    type: string
                repository_regex:
                    type: string
                branch_regex:
                    type: string
                file_regex:
                    type: string
```

* Good, because it is a stable standard.
* Good, because it enables high quality code generation for client and server.
* Bad, because it requires custom tooling to work with.

### Thrift

* Good, it has many languages implementations.
* Good, it can describes function error with exception.
* Bad, the JSON serialization is not human readable.

## Links

- [gRPC vs REST: Understanding gRPC, OpenAPI and REST and when to use them in API design](https://cloud.google.com/blog/products/api-management/understanding-grpc-openapi-and-rest-and-when-to-use-them)

- [OpenAPI spec](https://spec.openapis.org/oas/v3.1.0)
- [Swagger codegen](https://github.com/swagger-api/swagger-codegen#overview)
- [Protobuf overview](https://developers.google.com/protocol-buffers/docs/overview)
- [Protobuf python tutorial](https://developers.google.com/protocol-buffers/docs/pythontutorial)
- [Protobuf javascript tutorial](https://developers.google.com/protocol-buffers/docs/reference/javascript-generated)
- [Thrift tutorials](https://thrift-tutorial.readthedocs.io/)
- [Thrift types](https://thrift.apache.org/docs/types.html)
