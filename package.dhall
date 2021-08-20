let Prelude =
        env:DHALL_PRELUDE
      ? https://prelude.dhall-lang.org/v17.0.0/package.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e

let Kubernetes =
        env:DHALL_KUBERNETES
      ? https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/v5.0.0/1.19/package.dhall sha256:1ba3b2108e8f38427f649f336e21f08f20d825c91b3ac64033be8c98783345d2

let DevelConfig =
      { image = "registry.access.redhat.com/ubi8/ubi-minimal"
      , volumesMounts =
        [ Kubernetes.VolumeMount::{ mountPath = "/nix", name = "nix" }
        , Kubernetes.VolumeMount::{ mountPath = "/src", name = "src" }
        ]
      , volumes =
        [ Kubernetes.Volume::{
          , hostPath = Some Kubernetes.HostPathVolumeSource::{ path = "/nix" }
          , name = "nix"
          }
        , Kubernetes.Volume::{
          , hostPath = Some Kubernetes.HostPathVolumeSource::{ path = "/src/" }
          , name = "src"
          }
        ]
      }

in  \(dev : Bool) ->
      let default-paths =
            { nginx = ""
            , elastic = ""
            , web = ""
            , api = ""
            , setup = "sleep infinity"
            , api-test = ""
            }

      let Paths = ./data/nix-paths.dhall ? default-paths

      let mkDeployment =
            \(name : Text) ->
            \(command : Text) ->
            \(containerPort : Natural) ->
            \(env : Optional (List Kubernetes.EnvVar.Type)) ->
              let metadata =
                    Kubernetes.ObjectMeta::{
                    , labels = Some
                      [ { mapKey = "app.kubernetes.io/component"
                        , mapValue = name
                        }
                      , { mapKey = "app.kubernetes.io/name", mapValue = name }
                      , { mapKey = "app.kubernetes.io/part-of"
                        , mapValue = "monocle"
                        }
                      ]
                    , name = Some name
                    }

              let selector =
                    Some
                      [ { mapKey = "app.kubernetes.io/name", mapValue = name } ]

              let service =
                    Kubernetes.Resource.Service
                      Kubernetes.Service::{
                      , metadata
                      , spec = Some Kubernetes.ServiceSpec::{
                        , ports = Some
                          [ Kubernetes.ServicePort::{
                            , name = Some "web"
                            , port = containerPort
                            , targetPort = Some
                                ( < Int : Natural | String : Text >.Int
                                    containerPort
                                )
                            }
                          ]
                        , selector
                        }
                      }

              let deployment =
                    Kubernetes.Resource.Deployment
                      Kubernetes.Deployment::{
                      , metadata
                      , spec = Some Kubernetes.DeploymentSpec::{
                        , replicas = Some 1
                        , selector = Kubernetes.LabelSelector::{
                          , matchLabels = selector
                          }
                        , template = Kubernetes.PodTemplateSpec::{
                          , metadata
                          , spec = Some Kubernetes.PodSpec::{
                            , automountServiceAccountToken = Some False
                            , containers =
                              [ Kubernetes.Container::{
                                , image = Some DevelConfig.image
                                , imagePullPolicy = Some "IfNotPresent"
                                , command = Some [ command ]
                                , env
                                , name
                                , securityContext = Some Kubernetes.SecurityContext::{
                                  , runAsUser = Some 1000
                                  , runAsNonRoot = Some True
                                  }
                                , ports = Some
                                  [ Kubernetes.ContainerPort::{
                                    , containerPort
                                    , name = Some name
                                    }
                                  ]
                                , volumeMounts = Some DevelConfig.volumesMounts
                                }
                              ]
                            , volumes = Some DevelConfig.volumes
                            }
                          }
                        }
                      }

              in  [ service, deployment ]

      let ingress =
            Kubernetes.Ingress::{
            , metadata = Kubernetes.ObjectMeta::{
              , name = Some "monocle-ingress"
              }
            , spec = Some Kubernetes.IngressSpec::{
              , rules = Some
                [ Kubernetes.IngressRule::{
                  , http = Some Kubernetes.HTTPIngressRuleValue::{
                    , paths =
                      [ Kubernetes.HTTPIngressPath::{
                        , backend = Kubernetes.IngressBackend::{
                          , service = Some Kubernetes.IngressServiceBackend::{
                            , name = "nginx"
                            , port = Some Kubernetes.ServiceBackendPort::{
                              , number = Some 8080
                              }
                            }
                          }
                        , path = Some "/"
                        , pathType = Some "Prefix"
                        }
                      ]
                    }
                  }
                ]
              }
            }

      let no-env = None (List Kubernetes.EnvVar.Type)

      let to-env =
            \(map : Prelude.Map.Type Text Text) ->
              Some
                ( Prelude.List.map
                    (Prelude.Map.Entry Text Text)
                    Kubernetes.EnvVar.Type
                    ( \(entry : Prelude.Map.Entry Text Text) ->
                        Kubernetes.EnvVar::{
                        , name = entry.mapKey
                        , value = Some entry.mapValue
                        }
                    )
                    map
                )

      let components =
            { nginx =
                mkDeployment
                  "nginx"
                  Paths.nginx
                  8080
                  ( to-env
                      ( toMap
                          { MONOCLE_WEB_HOST = "web", MONOCLE_API_HOST = "api" }
                      )
                  )
            , elastic =
                mkDeployment
                  "elastic"
                  Paths.elastic
                  9200
                  ( to-env
                      ( toMap
                          { `discovery.type` = "single-node"
                          , ES_JAVA_OPTS = "-Xms512m -Xmx512m"
                          }
                      )
                  )
            , web = mkDeployment "web" Paths.web 3000 no-env
            , api =
                mkDeployment
                  "api"
                  Paths.api
                  9000
                  (to-env (toMap { ELASTIC_HOST = "elastic" }))
            }

      let resources =
              components.elastic
            # components.api
            # components.web
            # components.nginx
            # [ Kubernetes.Resource.Ingress ingress ]

      let mkJob =
            \(name : Text) ->
            \(command : Text) ->
              Kubernetes.Job::{
              , metadata = Kubernetes.ObjectMeta::{
                , name = Some "monocle-${name}"
                }
              , spec = Some Kubernetes.JobSpec::{
                , backoffLimit = Some 0
                , template = Kubernetes.PodTemplateSpec::{
                  , metadata = Kubernetes.ObjectMeta::{
                    , name = Some "monocle-${name}"
                    }
                  , spec = Some Kubernetes.PodSpec::{
                    , containers =
                      [ Kubernetes.Container::{
                        , command = Some [ command ]
                        , image = Some DevelConfig.image
                        , name = "monocle-${name}"
                        , volumeMounts = Some DevelConfig.volumesMounts
                        , securityContext = Some Kubernetes.SecurityContext::{
                          , runAsUser = Some 1000
                          , runAsNonRoot = Some True
                          }
                        }
                      ]
                    , volumes = Some DevelConfig.volumes
                    , restartPolicy = Some "Never"
                    }
                  }
                }
              }

      let setup = mkJob "setup" Paths.setup

      let test = mkJob "api-test" Paths.api-test

      in  { setup, test, resources }
