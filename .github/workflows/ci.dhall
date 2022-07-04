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
    , Docker =
        mk.makeCompose
          [ mk.GithubActions.Step::{
            , name = Some "Set write permission for data directory"
            , run = Some "chmod o+w data"
            }
          , mk.GithubActions.Step::{
            , name = Some "Create a config.yaml file"
            , run = Some
                ''
                cat > etc/config.yaml << EOL
                ---
                workspaces:
                  - name: monocle
                    crawlers:
                      - name: github-tekton
                        update_since: "2020-01-01"
                        provider:
                          github_organization: tekton
                EOL      
                ''
            }
          , mk.GithubActions.Step::{
            , name = Some "Create a secret.yaml file"
            , run = Some
                ''
                cat > .secrets << EOF
                CRAWLERS_API_KEY=secret
                GITHUB_TOKEN=123
                EOF
                ''
            }
          , mk.GithubActions.Step::{
            , name = Some "Build docker image"
            , run = Some
                "docker build -t quay.io/change-metrics/monocle:latest ."
            }
          , mk.GithubActions.Step::{
            , name = Some "Start Monocle compose"
            , run = Some "docker-compose up -d"
            }
          , mk.GithubActions.Step::{
            , name = Some "Wait for services to start"
            , run = Some "sleep 45"
            }
          , mk.GithubActions.Step::{
            , name = Some "Display docker-compose ps"
            , run = Some "docker-compose ps"
            }
          , mk.GithubActions.Step::{
            , name = Some "Display docker-compose logs"
            , run = Some "docker-compose logs"
            }
          , mk.GithubActions.Step::{
            , name = Some "Check services are running"
            , run = Some "test -z \"\$(sudo docker-compose ps -a | grep Exit)\""
            }
          , mk.GithubActions.Step::{
            , name = Some "Check api service through nginx"
            , run = Some
                "curl -s --fail -H 'Content-type: application/json' http://localhost:8080/api/2/get_workspaces -d '{}' | grep 'workspaces'"
            }
          , mk.GithubActions.Step::{
            , name = Some "Check web service to fetch web app"
            , run = Some
                "curl -s http://localhost:8080/index.html | grep 'window.document.title'"
            }
          ]
    }
