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

let monocleImage = "changemetrics/monocle_backend:\${MONOCLE_VERSION:-latest}"

let monocleWebImage = "changemetrics/monocle_web:\${MONOCLE_VERSION:-latest}"

let createElasticService =
      \(dev : Bool) ->
        let service =
              { image = Some
                  "docker.elastic.co/elasticsearch/elasticsearch:7.10.1"
              , healthcheck = Some Compose.Healthcheck::{
                , test = Some
                    ( Compose.StringOrList.String
                        "curl --silent --fail localhost:9200/_cluster/health || exit 1"
                    )
                }
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
                                           [ Compose.StringOrNumber.String
                                               "\${MONOCLE_ELASTIC_ADDR:-0.0.0.0}:9200:9200"
                                           ]
                                         }
                                   )
            else  Compose.Service::(     service
                                     //  { restart = Some "unless-stopped"
                                         , expose = Some
                                           [ Compose.StringOrNumber.Number 9200
                                           ]
                                         }
                                   )

let createApiService =
      \(dev : Bool) ->
        let service =
              { ports = Some
                [ Compose.StringOrNumber.String
                    "\${MONOCLE_API_ADDR:-0.0.0.0}:9876:9876"
                , Compose.StringOrNumber.String
                    "\${MONOCLE_API_ADDR:-0.0.0.0}:9877:9877"
                ]
              , healthcheck = Some Compose.Healthcheck::{
                , test = Some
                    ( Compose.StringOrList.String
                        "python -c \"import requests,sys; r=requests.get('http://localhost:9876/api/0/health'); print(r.text); sys.exit(1) if r.status_code!=200 else sys.exit(0)\""
                    )
                }
              , depends_on = Some [ "elastic" ]
              , command = Some
                  ( Compose.StringOrList.String
                      "uwsgi --http :9876 --socket :9877 --manage-script-name --mount /app=monocle.webapp:app"
                  )
              , volumes = Some [ "./etc:/etc/monocle:z" ]
              , environment = Some
                  ( Compose.ListOrDict.Dict
                      [ { mapKey = "CONFIG"
                        , mapValue = "/etc/monocle/config.yaml"
                        }
                      , { mapKey = "ELASTIC_CONN", mapValue = "elastic:9200" }
                      , { mapKey = "ALLOW_ORIGIN"
                        , mapValue = "\${MONOCLE_URL:-http://localhost:3000}"
                        }
                      , { mapKey = "CLIENT_ID"
                        , mapValue = "\${GITHUB_CLIENT_ID:-}"
                        }
                      , { mapKey = "CLIENT_SECRET"
                        , mapValue = "\${GITHUB_CLIENT_SECRET:-}"
                        }
                      , { mapKey = "REDIRECT_URL"
                        , mapValue =
                            "\${MONOCLE_API_URL:-http://localhost:9876}/api/0/authorize"
                        }
                      , { mapKey = "WEB_URL"
                        , mapValue = "\${MONOCLE_URL:-http://localhost:3000}"
                        }
                      ]
                  )
              }

        in  if    dev
            then  Compose.Service::(     service
                                     //  { build = Some
                                             (Compose.Build.String ".")
                                         }
                                   )
            else  Compose.Service::(     service
                                     //  { image = Some monocleImage
                                         , restart = Some "unless-stopped"
                                         }
                                   )

let createCrawlerService =
      \(dev : Bool) ->
        let service =
              { depends_on = Some [ "elastic" ]
              , command = Some
                  ( Compose.StringOrList.String
                      "monocle --elastic-conn elastic:9200 crawler --config /etc/monocle/config.yaml"
                  )
              , volumes = Some
                [ "./etc:/etc/monocle:z", "./dump:/var/lib/crawler:Z" ]
              , environment = Some
                  ( Compose.ListOrDict.Dict
                      [ { mapKey = "APP_ID", mapValue = "\${GITHUB_APP_ID:-}" }
                      , { mapKey = "APP_KEY_PATH"
                        , mapValue =
                            "\${GITHUB_APP_KEY_PATH:-/etc/monocle/app_key.rsa}"
                        }
                      ]
                  )
              }

        in  if    dev
            then  Compose.Service::(     service
                                     //  { build = Some
                                             (Compose.Build.String ".")
                                         }
                                   )
            else  Compose.Service::(     service
                                     //  { image = Some monocleImage
                                         , restart = Some "unless-stopped"
                                         }
                                   )

let createWebService =
      \(dev : Bool) ->
        let service =
              { depends_on = Some [ "api" ]
              , ports = Some
                [ Compose.StringOrNumber.String
                    "\${MONOCLE_WEB_ADDR:-0.0.0.0}:3000:3000"
                ]
              , environment = Some
                  ( Compose.ListOrDict.Dict
                      [ { mapKey = "REACT_APP_API_URL"
                        , mapValue =
                            "\${MONOCLE_API_URL:-http://localhost:9876}"
                        }
                      , { mapKey = "REACT_APP_TITLE"
                        , mapValue = "\${MONOCLE_TITLE:-}"
                        }
                      ]
                  )
              }

        in  if    dev
            then  Compose.Service::(     service
                                     //  { build = Some
                                             (Compose.Build.String "web")
                                         }
                                   )
            else  Compose.Service::(     service
                                     //  { image = Some monocleWebImage
                                         , restart = Some "unless-stopped"
                                         }
                                   )

let createServices =
      \(dev : Bool) ->
        toMap
          { api = createApiService dev
          , web = createWebService dev
          , crawler = createCrawlerService dev
          , elastic = createElasticService dev
          }

in  { img = Compose.Config::{ services = Some (createServices False) }
    , dev = Compose.Config::{ services = Some (createServices True) }
    }
