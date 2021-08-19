let Prelude = env:DHALL_PRELUDE

let Kubernetes = env:DHALL_KUBERNETES

let DevelConfig =
      { image = "registry.access.redhat.com/ubi8/ubi-minimal"
      , volumesMounts =
        [ Kubernetes.VolumeMount::{ mountPath = "/nix", name = "nix" }
        , Kubernetes.VolumeMount::{ mountPath = "/src", name = "src" }
        , Kubernetes.VolumeMount::{ mountPath = "/cabal", name = "cabal" }
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
        , Kubernetes.Volume::{
          , hostPath = Some Kubernetes.HostPathVolumeSource::{ path = "/cabal" }
          , name = "cabal"
          }
        ]
      }

in  \(dev : Bool) ->
      let default-paths =
            let PWD = env:PWD as Text

            in  { web = "${PWD}/monoclectl start-web"
                , api = "${PWD}/monoclectl start-api"
                , api-repl = "${PWD}/monoclectl start-api-repl"
                }

      let Paths = ./data/nix-paths.dhall ? default-paths

      let mkDeployment =
            \(name : Text) ->
            \(command : Text) ->
            \(containerPort : Natural) ->
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
                          , containers =
                            [ Kubernetes.Container::{
                              , image = Some DevelConfig.image
                              , command = Some [ command ]
                              , name
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

              in  { service, deployment }

      let components =
            { web = mkDeployment "web" Paths.web 3000
            , api = mkDeployment "api" Paths.api 9000
            }

      in  [ Kubernetes.Resource.Service components.web.service
          , Kubernetes.Resource.Deployment components.web.deployment
          ]
