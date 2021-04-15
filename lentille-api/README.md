# WIP: lentille-api

Run api:

```ShellSession
$ cabal run lentille-api 3000
```

Create reason binding:

```ShellSession
$ cabal run --flags=+with-reason lentille-api-reason-codegen
```

Create openapi definition:

```ShellSession
$ cabal run --flags=+with-openapi lentille-api 3000
$ curl localhost:3000/swagger.json | json-to-dhall | dhall-to-yaml
```
