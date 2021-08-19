let Prelude = env:DHALL_PRELUDE

let Kubernetes = env:DHALL_KUBERNETES

let default-paths =
      let PWD = env:PWD as Text

      in  { web = "${PWD}/monoclectl start-web"
          , api = "${PWD}/monoclectl start-api"
          }

let Paths = ./data/nix-paths.dhall ? default-paths

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

              in  Kubernetes.Deployment::{
                  , metadata
                  , spec = Some Kubernetes.DeploymentSpec::{
                    , replicas = Some 1
                    , selector = Kubernetes.LabelSelector::{
                      , matchLabels = Some
                        [ { mapKey = "app.kubernetes.io/name", mapValue = name }
                        ]
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

      let services =
            { web = mkDeployment "web" Paths.web 8080
            , api = mkDeployment "api" Paths.api 9000
            }

      in  services.web
