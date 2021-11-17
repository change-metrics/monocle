-- Re-usable github action for cachix
let GithubActions =
      https://raw.githubusercontent.com/regadas/github-actions-dhall/master/package.dhall sha256:66b276bb67cca4cfcfd1027da45857cc8d53e75ea98433b15dade1e1e1ec22c8

in  { GithubActions
    , make =
        \(cache-name : Text) ->
        \(steps : List GithubActions.Step.Type) ->
          let boot =
                \(name : Text) ->
                  [ GithubActions.Step::{
                    , uses = Some "actions/checkout@v2.4.0"
                    , `with` = Some (toMap { submodules = "true" })
                    }
                  , GithubActions.Step::{
                    , uses = Some "cachix/install-nix-action@v15"
                    , `with` = Some
                        (toMap { nix_path = "nixpkgs=channel:nixos-unstable" })
                    }
                  , GithubActions.Step::{
                    , uses = Some "cachix/cachix-action@v10"
                    , `with` = Some
                        ( toMap
                            { name
                            , authToken = "\${{ secrets.CACHIX_AUTH_TOKEN }}"
                            }
                        )
                    }
                  ]

          in  GithubActions.Workflow::{
              , name = "Nix"
              , on = GithubActions.On::{
                , pull_request = Some GithubActions.PullRequest::{=}
                , push = Some GithubActions.Push::{=}
                , release = Some GithubActions.Release::{=}
                }
              , jobs = toMap
                  { tests = GithubActions.Job::{
                    , name = Some "tests"
                    , runs-on = GithubActions.RunsOn.Type.ubuntu-latest
                    , steps = boot cache-name # steps
                    }
                  }
              }
    }
