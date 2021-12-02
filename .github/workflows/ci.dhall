let mk = ./mkCI.dhall

in  mk.make
      "change-metrics"
      (   mk.elastic-steps
        # [ mk.GithubActions.Step::{
            , name = Some "Build the env"
            , run = Some "nix-build --no-out-link --attr monocle-light.env"
            }
          , mk.GithubActions.Step::{
            , name = Some "Run Test"
            , run = Some
                "nix-build --no-out-link --attr ci --arg elasticsearch-port 9200"
            }
          ]
      )
