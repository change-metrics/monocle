-- # Monocle.
-- # Copyright (C) 2021 Monocle authors
-- #
-- # This program is free software: you can redistribute it and/or modify
-- # it under the terms of the GNU Affero General Public License as published
-- # by the Free Software Foundation, either version 3 of the License, or
-- # (at your option) any later version.
-- #
-- # This program is distributed in the hope that it will be useful,
-- # but WITHOUT ANY WARRANTY; without even the implied warranty of
-- # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- # GNU Affero General Public License for more details.
-- #
-- # You should have received a copy of the GNU Affero General Public License
-- # along with this program.  If not, see <https://www.gnu.org/licenses/>.
let Compose =
      https://raw.githubusercontent.com/sbdchd/dhall-docker-compose/master/compose/v3/package.dhall

let monocleImage =
      \(name : Text) ->
        "quay.io/change-metrics/monocle_${name}:\${COMPOSE_MONOCLE_VERSION:-latest}"

let buildContext =
      \(name : Text) ->
        { context = "."
        , dockerfile = "Dockerfile-${name}"
        , args =
            let -- todo: add default upstream
                default =
                  Compose.ListOrDict.Dict
                    ([] : List { mapKey : Text, mapValue : Text })

            in  default
        }

let envFile = Some (Compose.StringOrList.String ".secrets")

let mkEnvDefault =
      \(env-var-name : Text) ->
      \(default-value : Text) ->
        "\${" ++ env-var-name ++ ":-" ++ default-value ++ "}"

let mkPort =
      \(env-var-prefix : Text) ->
      \(default-port : Natural) ->
      \(container-port : Natural) ->
        Compose.StringOrNumber.String
          (     mkEnvDefault "COMPOSE_MONOCLE_${env-var-prefix}_ADDR" "0.0.0.0"
            ++  ":"
            ++  mkEnvDefault
                  "COMPOSE_MONOCLE_${env-var-prefix}_PORT"
                  (Natural/show default-port)
            ++  ":${Natural/show container-port}"
          )

let mkHealthCheck =
      \(command : Text) ->
        Compose.Healthcheck::{
        , test = Some (Compose.StringOrList.String command)
        , retries = Some 6
        , timeout = Some "60s"
        }

let createElasticService =
      \(dev : Bool) ->
        let service =
              { image = Some
                  "docker.elastic.co/elasticsearch/elasticsearch:7.10.1"
              , healthcheck = Some
                  ( mkHealthCheck
                      "curl --silent --fail localhost:9200/_cluster/health || exit 1"
                  )
              , ulimits = Some
                  ( toMap
                      { nofile =
                          Compose.Ulimits.Object { hard = 65535, soft = 65535 }
                      }
                  )
              , volumes = Some [ "./data:/usr/share/elasticsearch/data:Z" ]
              , environment = Some
                  ( Compose.ListOrDict.Dict
                      [ { mapKey = "ES_JAVA_OPTS"
                        , mapValue =
                            "-Xms\${COMPOSE_ES_XMS:-512m} -Xmx\${COMPOSE_ES_XMX:-512m}"
                        }
                      , { mapKey = "discovery.type", mapValue = "single-node" }
                      ]
                  )
              }

        in  if    dev
            then  Compose.Service::(     service
                                     //  { ports = Some
                                           [ mkPort "ELASTIC" 9200 9200 ]
                                         }
                                   )
            else  Compose.Service::(     service
                                     //  { restart = Some "unless-stopped"
                                         , expose = Some
                                           [ Compose.StringOrNumber.Number 9200
                                           ]
                                         }
                                   )

let apiBuildContext =
      Compose.Build.Object
        { context = "."
        , dockerfile = "Dockerfile-api"
        , args =
            Compose.ListOrDict.Dict
              [ { mapKey = "MONOCLE_COMMIT"
                , mapValue = mkEnvDefault "MONOCLE_COMMIT" "HEAD"
                }
              ]
        }

let createApiService =
      \(dev : Bool) ->
        let service =
              { healthcheck = Some
                  ( mkHealthCheck
                      "curl --silent --fail localhost:8080/health || exit 1"
                  )
              , depends_on = Some [ "elastic" ]
              , command = Some (Compose.StringOrList.String "monocle api")
              , volumes = Some [ "./etc:/etc/monocle:z" ]
              , env_file = envFile
              , ports = Some [ mkPort "API" 8080 8080 ]
              , environment = Some
                  ( Compose.ListOrDict.Dict
                      [ { mapKey = "MONOCLE_CONFIG"
                        , mapValue = "/etc/monocle/config.yaml"
                        }
                      , { mapKey = "MONOCLE_ELASTIC_URL"
                        , mapValue = "http://elastic:9200"
                        }
                      , { mapKey = "MONOCLE_WEBAPP_PATH"
                        , mapValue = "/usr/share/monocle/webapp/"
                        }
                      , { mapKey = "MONOCLE_API_PORT", mapValue = "8080" }
                      , { mapKey = "MONOCLE_PUBLIC_URL"
                        , mapValue =
                            "\${COMPOSE_MONOCLE_PUBLIC_URL:-http://localhost:8080}"
                        }
                      , { mapKey = "MONOCLE_WEBAPP_TITLE"
                        , mapValue = "\${COMPOSE_MONOCLE_WEBAPP_TITLE:-Monocle}"
                        }
                      ]
                  )
              }

        in  if    dev
            then  Compose.Service::(service // { build = Some apiBuildContext })
            else  Compose.Service::(     service
                                     //  { image = Some (monocleImage "api")
                                         , restart = Some "unless-stopped"
                                         }
                                   )

let createCrawlerService =
      \(dev : Bool) ->
        let service =
              { depends_on = Some [ "api" ]
              , command = Some (Compose.StringOrList.String "monocle crawler")
              , healthcheck = Some
                  ( mkHealthCheck
                      "curl --silent --fail localhost:9001/health || exit 1"
                  )
              , volumes = Some [ "./etc:/etc/monocle:z" ]
              , env_file = envFile
              , environment = Some
                  ( Compose.ListOrDict.Dict
                      [ { mapKey = "MONOCLE_CONFIG"
                        , mapValue = "/etc/monocle/config.yaml"
                        }
                      , { mapKey = "MONOCLE_PUBLIC_URL"
                        , mapValue = "http://api:8080"
                        }
                      ]
                  )
              }

        in  if    dev
            then  Compose.Service::(service // { build = Some apiBuildContext })
            else  Compose.Service::(     service
                                     //  { image = Some (monocleImage "api")
                                         , restart = Some "unless-stopped"
                                         }
                                   )

let createServices =
      \(dev : Bool) ->
        toMap
          { api = createApiService dev
          , crawler = createCrawlerService dev
          , elastic = createElasticService dev
          }

in  { img = Compose.Config::{ services = Some (createServices False) }
    , dev = Compose.Config::{ services = Some (createServices True) }
    }
