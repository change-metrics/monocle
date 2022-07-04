let mk = ./mkCI.dhall

in  { Nix =
        mk.makeNix
          "change-metrics"
          (   mk.elastic-steps
            # [ mk.GithubActions.Step::{
                , name = Some "Build the env"
                , run = Some "nix-build --no-out-link --attr monocle-light.env"
                }
              , mk.GithubActions.Step::{
                , name = Some "Run Test"
                , run = Some
                    ( let command =
                            "env MONOCLE_ELASTIC_URL=http://localhost:9200 monocle-ci-run"

                      in  "cd haskell; nix-shell --pure --attr ci-shell ../nix/default.nix --command '${command}'"
                    )
                }
              ]
          )
    , Web =
        mk.makeNPM
          [ mk.GithubActions.Step::{
            , name = Some "Install npm packages"
            , run = Some
                "sudo apt-get install git; cd web; npm install --legacy-peer-deps"
            }
          , mk.GithubActions.Step::{
            , name = Some "Run NPM tests"
            , run = Some "cd web ; npm run test"
            }
          , mk.GithubActions.Step::{
            , name = Some "Run NPM format"
            , run = Some "cd web ; npm run format"
            }
          ]
    , Docker = mk.makeCompose mk.validate-compose-steps
    , Publish-Master-Image =
        mk.makePublishMaster
          (   mk.validate-compose-steps
            # [ mk.GithubActions.Step::{
                , name = Some "Login on quay.io"
                , run = Some
                    "docker login -u \"\${{ secrets.QUAYIO_USERNAME }}\" -p \"\${{ secrets.QUAYIO_PASSWORD }}\" quay.io"
                }
              , mk.GithubActions.Step::{
                , name = Some "Publish images to quay.io"
                , run = Some "docker push quay.io/change-metrics/monocle:latest"
                }
              ]
          )
    }
