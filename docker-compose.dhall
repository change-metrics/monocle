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
        "quay.io/change-metrics/monocle_${name}:\${MONOCLE_VERSION:-latest}"

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
          (     mkEnvDefault "MONOCLE_${env-var-prefix}_ADDR" "0.0.0.0"
            ++  ":"
            ++  mkEnvDefault
                  "MONOCLE_${env-var-prefix}_PORT"
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
                        , mapValue = "-Xms\${ES_XMS:-512m} -Xmx\${ES_XMX:-512m}"
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
                      "python -c \"import requests,sys; r=requests.post('http://localhost:9898/api/2/health', json={}); print(r.text); sys.exit(1) if r.status_code!=200 else sys.exit(0)\""
                  )
              , depends_on = Some [ "elastic" ]
              , command = Some
                  (Compose.StringOrList.String "monocle-api --port 9898")
              , volumes = Some [ "./etc:/etc/monocle:z" ]
              , env_file = envFile
              , environment = Some
                  ( Compose.ListOrDict.Dict
                      [ { mapKey = "CONFIG"
                        , mapValue = "/etc/monocle/config.yaml"
                        }
                      , { mapKey = "ELASTIC_CONN", mapValue = "elastic:9200" }
                      ]
                  )
              }

        in  if    dev
            then  Compose.Service::(     service
                                     //  { build = Some apiBuildContext
                                         , ports = Some
                                           [ mkPort "API" 9898 9898 ]
                                         }
                                   )
            else  Compose.Service::(     service
                                     //  { image = Some (monocleImage "api")
                                         , expose = Some
                                           [ Compose.StringOrNumber.Number 9898
                                           ]
                                         , restart = Some "unless-stopped"
                                         }
                                   )

let createCrawlerService =
      \(dev : Bool) ->
        let service =
              { depends_on = Some [ "api" ]
              , command = Some (Compose.StringOrList.String "macroscope")
              , healthcheck = Some
                  ( mkHealthCheck
                      "curl --silent --fail localhost:9001/health || exit 1"
                  )
              , volumes = Some [ "./etc:/etc/monocle:z" ]
              , env_file = envFile
              , environment = Some
                  ( Compose.ListOrDict.Dict
                      [ { mapKey = "CONFIG"
                        , mapValue = "/etc/monocle/config.yaml"
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

let createCrawlerLegacyService =
      \(dev : Bool) ->
        let service =
              { depends_on = Some [ "elastic" ]
              , command = Some
                  ( Compose.StringOrList.String
                      "monocle --elastic-conn elastic:9200 crawler --config /etc/monocle/config.yaml"
                  )
              , env_file = envFile
              , volumes = Some
                [ "./etc:/etc/monocle:z", "./dump:/var/lib/crawler:Z" ]
              }

        in  if    dev
            then  Compose.Service::(     service
                                     //  { build = Some
                                             (Compose.Build.String ".")
                                         }
                                   )
            else  Compose.Service::(     service
                                     //  { image = Some (monocleImage "backend")
                                         , restart = Some "unless-stopped"
                                         }
                                   )

let createWebService =
      \(dev : Bool) ->
        let service =
              { depends_on = Some [ "api" ]
              , ports = Some [ mkPort "WEB" 8080 8080 ]
              , volumes = Some [ "./web/conf:/etc/nginx/conf.d:z" ]
              , environment = Some
                  ( Compose.ListOrDict.Dict
                      [ { mapKey = "REACT_APP_API_URL"
                        , mapValue =
                            "\${MONOCLE_PUBLIC_URL:-http://localhost:8080}"
                        }
                      , { mapKey = "REACT_APP_TITLE"
                        , mapValue = "\${MONOCLE_TITLE}"
                        }
                      ]
                  )
              }

        let -- podman v3.1.0 doesn't seem to work with build directory
            -- adding a build context and a dockerfile next to the compose file is a working combo
            build =
              buildContext "web" // { context = "web" }

        in  if    dev
            then  Compose.Service::(     service
                                     //  { build = Some
                                             (Compose.Build.Object build)
                                         }
                                   )
            else  Compose.Service::(     service
                                     //  { image = Some (monocleImage "web")
                                         , restart = Some "unless-stopped"
                                         }
                                   )

let createServices =
      \(dev : Bool) ->
        toMap
          { api = createApiService dev
          , web = createWebService dev
          , crawler-legacy = createCrawlerLegacyService dev
          , crawler = createCrawlerService dev
          , elastic = createElasticService dev
          }

in  { img = Compose.Config::{ services = Some (createServices False) }
    , dev = Compose.Config::{ services = Some (createServices True) }
    }
