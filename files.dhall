{- Render github action file using:
dhall-to-yaml --output .github/workflows/ci.yaml <<< '(./files.dhall).gh-workflow'
-}
let GithubActions =
        ~/src/github.com/regadas/github-actions-dhall/package.dhall
      ? https://raw.githubusercontent.com/regadas/github-actions-dhall/master/package.dhall sha256:8efe6772e27f99ed3a9201b4e45c68eeaaf82c349e70d36fbe89185a324f6519

let python-ver = "3.7"

let setup-es =
      [ GithubActions.steps.checkout
      , GithubActions.Step::{
        , name = Some "Configure sysctl limits"
        , run = Some
            ''
            sudo swapoff -a
            sudo sysctl -w vm.swappiness=1
            sudo sysctl -w fs.file-max=262144
            sudo sysctl -w vm.max_map_count=262144
            ''
        }
      , GithubActions.steps.actions/elasticsearch-run
          { stack-version = "7.7.1", nodes = None Natural }
      ]

let setup-python =
      [ GithubActions.Step::{
        , name = Some ("Setup Python " ++ python-ver)
        , uses = Some "actions/setup-python@v1"
        , with = Some (toMap { python-version = python-ver })
        }
      , GithubActions.Step::{
        , name = Some "Install dependencies"
        , run = Some
            ''
            python3 -m pip install --upgrade pip
            python3 -m pip install tox
            ''
        }
      ]

let run-tox =
      [ GithubActions.Step::{ name = Some "Test with tox", run = Some "tox" } ]

let gh-workflow =
      GithubActions.Workflow::{
      , name = "Monocle CI"
      , on = GithubActions.On::{
        , push = Some GithubActions.Push::{=}
        , pull_request = Some GithubActions.PullRequest::{=}
        }
      , jobs = toMap
          { integration-job = GithubActions.Job::{
            , runs-on = GithubActions.RunsOn.Type.ubuntu-latest
            , steps = setup-es # setup-python # run-tox
            }
          }
      }

in  { gh-workflow = gh-workflow }
