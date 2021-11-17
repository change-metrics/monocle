let mk = ./mkCI.dhall

in  mk.make
      "change-metrics"
      [ mk.GithubActions.Step::{
        , name = Some "Run Test"
        , run = Some "nix-build --no-out-link --attr ci"
        }
      ]
