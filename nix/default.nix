{ elasticsearch-port ? 19200 }:
let
  # pin the upstream nixpkgs
  nixpkgsPath = fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/4d60081494259c0785f7e228518fee74e0792c1b.tar.gz";
    sha256 = "sha256:15vxvzy9sxsnnxn53w2n44vklv7irzxvqv8xj9dn78z9zwl17jhq";
  };
  nixpkgsSrc = (import nixpkgsPath);

  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore.nix";
    # put the latest commit sha of gitignore Nix library here:
    rev = "211907489e9f198594c0eb0ca9256a1949c9d412";
    # use what nix suggests in the mismatch message here:
    sha256 = "sha256-qHu3uZ/o9jBHiA3MEKHJ06k7w4heOhA+4HCSIvflRxo=";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;

  monocleHaskellSrc = gitignoreSource ../haskell/.;

  # update haskell dependencies
  compilerVersion = "8107";
  compiler = "ghc" + compilerVersion;

  morpheus-graphql-src = pkgs.fetchFromGitHub {
    owner = "morpheusgraphql";
    repo = "morpheus-graphql";
    rev = "0.19.0";
    sha256 = "sha256-YY4TDvkFuPxeAPSJurklYzEn3rO6feToiKR4Ii0iLQc=";
  };

  mk-morpheus-lib = hpPrev: name:
    (hpPrev.callCabal2nix "morpheus-graphql-${name}"
      "${morpheus-graphql-src}/morpheus-graphql-${name}" { });

  overlays = [
    (final: prev: {
      myHaskellPackages = prev.haskell.packages.${compiler}.override {
        overrides = hpFinal: hpPrev: {
          # Unbreak proto3-suite
          range-set-list = pkgs.haskell.lib.dontCheck
            (pkgs.haskell.lib.overrideCabal hpPrev.range-set-list {
              broken = false;
            });
          proto3-suite = let
            src = builtins.fetchGit {
              url = "https://github.com/awakesecurity/proto3-suite";
              ref = "master";
              rev = "9720ec14e1979e687b77a18e094b81625cc537ae";
            };
          in pkgs.haskell.lib.dontCheck
          (hpPrev.callCabal2nix "proto3-suite" src { });

          text-time = (pkgs.haskell.lib.overrideCabal hpPrev.text-time {
            broken = false;
            src = builtins.fetchGit {
              url = "https://github.com/klangner/text-time.git";
              ref = "master";
              rev = "33bffc43fde3fc57d0a6e3cb9f4f60fca2a8af6e";
            };
          });

          json-syntax = pkgs.haskell.lib.dontCheck
            (pkgs.haskell.lib.overrideCabal hpPrev.json-syntax {
              broken = false;
            });

          bloodhound = pkgs.haskell.lib.overrideCabal hpPrev.bloodhound {
            version = "0.18.0.0";
            sha256 = "1dmmvpcmylnwwlw8p30azd9wfa4fk18fd13jnb1gx4wjs8jcwy7p";
            broken = false;
          };

          bugzilla-redhat =
            pkgs.haskell.lib.overrideCabal hpPrev.bugzilla-redhat {
              version = "1.0.0";
              sha256 = "sha256-nUITDj5l7e/d4sEyfSpok1Isoy1AIeIad+Fp4QeQJb0=";
              broken = false;
            };

          morpheus-graphql-tests = mk-morpheus-lib hpPrev "tests";
          morpheus-graphql-core = mk-morpheus-lib hpPrev "core";
          morpheus-graphql-code-gen = mk-morpheus-lib hpPrev "code-gen";
          morpheus-graphql-client = mk-morpheus-lib hpPrev "client";

          gerrit = let
            src = builtins.fetchGit {
              url =
                "https://softwarefactory-project.io/r/software-factory/gerrit-haskell";
              ref = "master";
              rev = "7ba07ed5c9da867bd566d114eb2962d7f4b90cf5";
            };
          in pkgs.haskell.lib.dontCheck (hpPrev.callCabal2nix "gerrit" src { });

          relude = hpPrev.relude_1_0_0_1;
          fakedata = hpPrev.fakedata_1_0_2;

          monocle =
            (hpPrev.callCabal2nix "monocle" monocleHaskellSrc { }).overrideAttrs
            (_: { MONOCLE_COMMIT = builtins.getEnv "MONOCLE_COMMIT"; });

          monocle-codegen =
            hpPrev.callCabal2nix "monocle-codegen" monocleHaskellSrc { };
        };
      };

    })
  ];

  # create the main package set without options
  pkgs = nixpkgsSrc {
    inherit overlays;
    system = "x86_64-linux";
  };
  pkgsNonFree = nixpkgsSrc { config.allowUnfree = true; };

  # manually adds build dependencies for benchmark that are not managed by cabal2nix
  addCriterion = drv:
    pkgs.haskell.lib.addBuildDepends drv ([ pkgs.myHaskellPackages.criterion ]);

  # local devel env
  nginx-port = 18080;
  monocle-port = 19875;
  web-port = 13000;
  prom-port = 19090;
  grafana-port = 19030;

  # script helpers to setup service starting environment
  headers = ''
    #!${pkgs.dash}/bin/dash -e
    if ! test -z "$DEBUG"; then set -x; fi
    export PATH=${pkgs.coreutils}/bin:${pkgs.gnused}:$PATH
  '';

  # script helpers to create application home on the hosts
  mkHome = name: ''
    mkdir -p ${name} 2> /dev/null || {
      echo "${name}: creating"
      /bin/sudo mkdir -m 0700 ${name}
      /bin/sudo chown $(id -u) ${name}
    }
    cd ${name}
  '';

in rec {
  # DB
  info = pkgs.lib.splitString "-" pkgs.stdenv.hostPlatform.system;
  arch = pkgs.lib.elemAt info 0;
  plat = pkgs.lib.elemAt info 1;
  elasticsearch = pkgsNonFree.elasticsearch7.overrideAttrs (old: rec {
    version = "7.10.1";
    name = "elasticsearch-${version}";
    src = pkgs.fetchurl {
      url =
        "https://artifacts.elastic.co/downloads/elasticsearch/${name}-${plat}-${arch}.tar.gz";
      sha256 = "1r62afmpmwyxifr4kjlannj44zbh67gdcch5czh4fllv459ajf7f";
    };
  });
  elasticsearch-home = "/var/lib/elasticsearch";
  elasticsearchConf = pkgs.writeTextFile {
    name = "elasticsearch.yml";
    text = ''
      cluster.name: monocle
      http.port: ${toString elasticsearch-port}
      discovery.type: single-node
      network.host: 0.0.0.0
      cluster.routing.allocation.disk.threshold_enabled: false
    '';
  };
  elasticsearchStart = pkgs.writeScriptBin "elasticsearch-start" ''
    ${headers}

    # todo: only set max_map_count when necessary
    ${pkgs.sudo}/bin/sudo sysctl -w vm.max_map_count=262144 || true

    ${mkHome elasticsearch-home}
    export ES_HOME=${elasticsearch-home}
    mkdir -p $ES_HOME/logs $ES_HOME/data
    ${pkgs.rsync}/bin/rsync -a ${elasticsearch}/config/ $ES_HOME/config/
    ln -sf ${elasticsearch}/modules/ $ES_HOME/
    find $ES_HOME -type f | xargs chmod 0600
    find $ES_HOME -type d | xargs chmod 0700
    cat ${elasticsearchConf} > $ES_HOME/config/elasticsearch.yml
    exec ${elasticsearch}/bin/elasticsearch
  '';

  # Prometheus
  promConf = pkgs.writeTextFile {
    name = "prometheus.yml";
    text = ''
      global:
        evaluation_interval: "1m"
        scrape_interval: "1m"
        scrape_timeout: "10s"
      scrape_configs:
        - job_name: api
          static_configs:
            - targets:
                - API_TARGET
        - job_name: crawler
          static_configs:
            - targets:
                - CRAWLER_TARGET
    '';
  };
  prom-home = "/var/lib/prometheus";
  promStart = pkgs.writeScriptBin "prometheus-start" ''
    ${headers}

    # config from env
    API_TARGET=''${API_TARGET:-localhost:${toString monocle-port}}
    CRAWLER_TARGET=''${CRAWLER_TARGET:-localhost:9001}
    LISTEN=''${PROMETHEUS_TARGET:-0.0.0.0:${toString prom-port}}

    echo "Starting $LISTEN: prometheus for $API_TARGET api and $CRAWLER_TARGET crawler"

    # boot
    ${mkHome prom-home}
    cat ${promConf} | \
      sed -e "s/API_TARGET/$API_TARGET/" -e "s/CRAWLER_TARGET/$CRAWLER_TARGET/" > config.yml
    cd ${prom-home}
    exec ${pkgs.prometheus}/bin/prometheus --config.file=config.yml --web.listen-address="$LISTEN"
  '';

  promContainer = pkgs.dockerTools.buildLayeredImage {
    name = "quay.io/change-metrics/monocle-prometheus";
    tag = "latest";
    # created = "now";
    config = {
      Entrypoint = [ "${promStart}/bin/prometheus-start" ];
      Volumes = { "${prom-home}" = { }; };
    };
  };

  grafana-home = "/var/lib/grafana";
  grafanaPromDS = pkgs.writeTextFile {
    name = "prometheus.yml";
    text = ''
      apiVersion: 1
      datasources:
      - name: Prometheus
        type: prometheus
        url: PROMETHEUS_URL
    '';
  };
  grafanaDashboards = pkgs.writeTextFile {
    name = "grafana-dashboards-provider.yml";
    text = ''
      apiVersion: 1
      providers:
      - name: dashboards
        type: file
        updateIntervalSeconds: 30
        options:
          path: ${grafana-home}/dashboards
          foldersFromFilesStructure: true
    '';
  };
  grafanaConf = pkgs.writeTextFile {
    name = "grafana.ini";
    text = ''
      [security]
      admin_user = admin
      admin_password = monocle

      [server]
      http_port = ${toString grafana-port}

      [dashboards]
      default_home_dashboard_path = ${grafana-home}/dashboards/monocle.json

      [auth.anonymous]
      enabled = true
      org_role = Viewer
      hide_version = true

      [plugin.grafana-image-renderer]
      rendering_ignore_https_errors = true
    '';
  };

  dhall-grafana = pkgs.dhallPackages.dhall-grafana;

  grafanaConfig = pkgs.runCommand "build-grafana-config" { } ''
    echo Building grafana config
    export XDG_CACHE_HOME=/tmp/dhall-home
    mkdir -p $XDG_CACHE_HOME/dhall

    export DHALL_PRELUDE=${pkgs.dhallPackages.Prelude}/binary.dhall
    export DHALL_GRAFANA=${dhall-grafana}/binary.dhall
    for pkg in ${pkgs.dhallPackages.Prelude} ${dhall-grafana}; do
        for cache in $pkg/.cache/dhall/*; do
            ln -sf $cache $XDG_CACHE_HOME/dhall/
        done
    done

    mkdir $out
    ${pkgs.dhall-json}/bin/dhall-to-json  \
      --file ${../conf/grafana-dashboard.dhall} \
      --output $out/monocle.json
  '';

  grafanaStart = pkgs.writeScriptBin "grafana-start" ''
    ${headers}

    # config from env
    ADMIN_PASSWORD=''${GRAFANA_PASS:-monocle}
    PROMETHEUS_URL=''${PROMETHEUS_URL:-http://localhost:${toString prom-port}}
    echo "Starting 0.0.0.0:${
      toString grafana-port
    }: grafana for $PROMETHEUS_URL prometheus"

    # boot
    ${mkHome grafana-home}
    GRAFANA_BASE=${pkgs.grafana}
    ${pkgs.rsync}/bin/rsync --exclude /public/ -r $GRAFANA_BASE/share/grafana/ ${grafana-home}/
    ln -sf  $GRAFANA_BASE/share/grafana/public/ ${grafana-home}/
    find ${grafana-home} -type f | xargs chmod 0600
    find ${grafana-home} -type d | xargs chmod 0700
    mkdir -p ${grafana-home}/dashboards
    cat ${grafanaConfig}/monocle.json > ${grafana-home}/dashboards/monocle.json
    cd ${grafana-home}
    cat ${grafanaDashboards} > conf/provisioning/dashboards/dashboard.yaml
    cat ${grafanaPromDS} | sed -e "s|PROMETHEUS_URL|$PROMETHEUS_URL|" > conf/provisioning/datasources/prometheus.yaml
    cat ${grafanaConf} | sed -e "s/admin_password = monocle/admin_password = $ADMIN_PASSWORD/" > grafana.ini
    exec $GRAFANA_BASE/bin/grafana-server -config ./grafana.ini
  '';

  grafanaContainer = pkgs.dockerTools.buildLayeredImage {
    name = "quay.io/change-metrics/monocle-grafana";
    tag = "latest";
    # created = "now";
    contents = [ pkgs.coreutils pkgs.gnused pkgs.findutils ];
    config = {
      Entrypoint = [ "${grafanaStart}/bin/grafana-start" ];
      Volumes = { "${grafana-home}" = { }; };
    };
  };

  # WEB
  nginx-home = "/tmp/nginx-home";
  nginxConf = pkgs.writeTextFile {
    name = "nginx.conf";
    text = ''
      error_log /dev/stdout info;
      pid ${nginx-home}/nginx.pid;

      events {
          worker_connections 1024;
      }

      http {
        access_log /dev/stdout;
        client_body_temp_path ${nginx-home}/client-body;
        proxy_temp_path ${nginx-home}/proxy;
        proxy_cache_path ${nginx-home}/cache keys_zone=one:10m;
        fastcgi_temp_path ${nginx-home}/fastcgi;
        fastcgi_cache_path ${nginx-home}/fcache keys_zone=one1:10m;
        uwsgi_temp_path ${nginx-home}/uwsgi;
        scgi_temp_path ${nginx-home}/scgi;
        client_max_body_size 1024M;
        server {
          listen ${toString nginx-port} default_server;
          proxy_cache one;

          gzip on;
          gzip_min_length 1000;
          gzip_types text/plain text/xml application/javascript text/css;

          location /api/2/ {
             proxy_pass http://localhost:${toString monocle-port}/;
             proxy_http_version 1.1;
          }

          location /auth {
              proxy_pass http://localhost:${toString monocle-port}/auth;
              proxy_http_version 1.1;
          }

          # Forward the rest to the node development server
          location / {
              proxy_pass http://localhost:${toString web-port};
              proxy_http_version 1.1;
              proxy_set_header Upgrade $http_upgrade;
              proxy_set_header Connection "upgrade";
              proxy_set_header Host $host;
              proxy_cache_bypass $http_upgrade;
          }
        }
      }
    '';
  };
  nginxStart = pkgs.writeScriptBin "nginx-start" ''
    #!/bin/sh
    set -ex
    mkdir -p ${nginx-home};
    exec ${pkgs.nginx}/bin/nginx -c ${nginxConf} -p ${nginx-home}/ -g "daemon off;"
  '';

  monocleReplStart = pkgs.writeScriptBin "monocle-repl" ''
    #!/bin/sh
    set -ex
    export $(cat .secrets)
    cd haskell; cabal repl monocle
  '';

  monocleGhcid = pkgs.writeScriptBin "monocle-ghcid" ''
    #!/bin/sh
    set -ex
    cd haskell; ${pkgs.ghcid}/bin/ghcid -c "cabal repl monocle" $*
  '';

  monocleWebStart = pkgs.writeScriptBin "monocle-web-start" ''
    #!/bin/sh
    set -ex
    cd web

    if ! test -d node_modules; then
        ${pkgs.nodejs}/bin/npm install
    fi

    export WEB_PORT=${toString web-port}
    export REACT_APP_API_URL=http://localhost:${toString nginx-port}
    export REACT_APP_TITLE="Monocle Dev"
    exec ${pkgs.nodejs}/bin/npm start
  '';

  monocleEmacsLauncher = pkgs.writeTextFile {
    name = "monocle.el";
    text = ''
      ;;; monocle.el --- Functions to operate Monocle

      ;; This file is not part of GNU Emacs.

      ;;; Code:

      ;; Start a process in a buffer with ansi colors
      (require 'comint)
      (defun start-worker-process (name program &rest args)
        (let ((buffer-name (concat "*" name "*")))
          (message "Starting %s %s" buffer-name program)
          (let ((*buffer* (get-buffer-create buffer-name)))
            (if (get-buffer-process *buffer*)
                (message "Process already running!")
              (with-current-buffer *buffer*
                (let ((*proc* (apply 'start-process name buffer-name program args)))
                  (ansi-color-for-comint-mode-on)
                  (comint-mode)
                  (set-process-filter *proc* 'comint-output-filter))))
            (switch-to-buffer-other-window *buffer*))))

      (defun monocle-startp (name command)
        (start-worker-process (concat "monocle-" name) (concat command "/bin/" name "-start")))

      (defun monocle-start ()
        (monocle-startp "elasticsearch" "${elasticsearchStart}" )
        (monocle-startp "nginx" "${nginxStart}" )
        (monocle-startp "prometheus" "${promStart}" )
        (monocle-startp "grafana" "${grafanaStart}" )
        (monocle-startp "monocle-api" "${monocleReplStart}" )
        (monocle-startp "monocle-web" "${monocleWebStart}" ))

      (monocle-start)
    '';
  };

  monocleEmacsStart = pkgs.writeScriptBin "launch-monocle-with-emacs" ''
    #!/bin/sh
    set -ex
    ${pkgs.emacs-nox}/bin/emacs --quick --load ${monocleEmacsLauncher}
  '';

  services-req = [
    elasticsearchStart
    nginxStart
    promStart
    grafanaStart
    monocleReplStart
    monocleWebStart
    monocleEmacsStart
    monocleGhcid
  ];

  # define the base requirements
  base-req = [ pkgs.bashInteractive pkgs.coreutils pkgs.gnumake ];
  codegen-req = [ pkgs.protobuf pkgs.ocamlPackages.ocaml-protoc pkgs.glibc ]
    ++ base-req;

  # haskell dependencies for codegen
  hsPkgs = pkgs.myHaskellPackages;

  hs-req = [ hsPkgs.cabal-install hsPkgs.ormolu hsPkgs.proto3-suite pkgs.zlib ];

  # define javascript requirements
  javascript-req = [ pkgs.nodejs ];

  # define openapi requirements
  gnostic = pkgs.buildGoModule rec {
    pname = "gnostic";
    version = "0.5.5";
    patches = [
      (pkgs.fetchpatch {
        url =
          "https://github.com/TristanCacqueray/gnostic/commit/d0c924cdb08c0c0667cca033841807cfc84776f2.patch";
        sha256 = "15wb2fr5iqy2zygl3svzyq72w9gpqsb7whmwqy43l3hph28bmvwb";
      })
    ];
    src = pkgs.fetchFromGitHub {
      owner = "google";
      repo = "gnostic";
      rev = "v${version}";
      sha256 = "065yqwlk9swgd13d4rvdaixq2dfsdbyha8k7k5j52982c2xz5c07";
    };
    doCheck = false;
    # nativeBuildInputs = [ pkgs.protobuf ];
    vendorSha256 = "1r0gjhv513174pbqf399vsrpx6zsmdlj48pzc5qh15k62ihy0h68";
    subPackages = [ "./apps/protoc-gen-openapi" ];
  };
  protobuf-src = pkgs.protobuf.src;
  googleapis-src = pkgs.fetchFromGitHub {
    owner = "googleapis";
    repo = "googleapis";
    rev = "94a788e91f0f10db7d3ca38d3503d6eecefffab8";
    sha256 = "03ky469sk0gkndxs4v8civ6x70mnnihgzaaqj51rr1xc14h40qss";
  };
  go-req = [ gnostic ];
  doc-req = [ pkgs.plantuml ];

  # all requirement
  all-req = codegen-req ++ hs-req ++ javascript-req ++ go-req ++ doc-req;

  # containers
  containerPrometheus = promContainer;
  containerGrafana = grafanaContainer;
  containerBackend = pkgs.dockerTools.buildLayeredImage {
    name = "quay.io/change-metrics/monocle-backend";
    tag = "latest";
    # created = "now";
    contents = [ (pkgs.haskell.lib.justStaticExecutables hsPkgs.monocle) ];

  };

  monocle-light =
    # Disable profiling, haddock and test to speedup the build
    (pkgs.haskell.lib.disableLibraryProfiling
      (pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck hsPkgs.monocle))
      # Enable the ci flag to fail on warning
    ).overrideAttrs (_:
      # Set dhall env variable to avoid warning
      {
        XDG_CACHE_HOME = "/tmp";
      });

  mk-ci = name: cmd:
    pkgs.runCommand "monocle-${name}" {
      # Set local to avoid utf-8 invalid byte sequence errors
      LC_ALL = "en_US.UTF-8";
      LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      XDG_CACHE_HOME = "/tmp";
      PATH = "${pkgs.coreutils}/bin:${pkgs.findutils}/bin:${pkgs.gnugrep}/bin";
    } ''
      # Create temporary home
      export HOME=$(mktemp -d)

      # Copy the source so that doctest can write temp files
      mkdir -p $HOME/monocle-src
      cd $HOME/monocle-src
      cp -r -p ${monocleHaskellSrc}/* .

      ${cmd}

      touch $out
    '';

  cabal-setup = ghcEnv: ''
    export GHC_DRV=$(cat ${ghcEnv})
    export PATH=$GHC_DRV/bin:${pkgs.cabal-install}/bin:${hsPkgs.doctest20}/bin:$PATH
    export NIX_GHCPKG=$GHC_DRV/bin/ghc-pkg
    export NIX_GHC=$GHC_DRV/bin/ghc
    export NIX_GHC_LIBDIR=$($NIX_GHC --print-libdir)
    export NIX_GHC_DOCDIR="$GHC_DRV/share/doc/ghc/html"
    mkdir -p $HOME/.cabal
    touch $HOME/.cabal/config
    ghc --version
    cabal --version
    doctest --version
  '';

  # Here we use monocle.env to get a ghc with monocle dependencies, but without monocle
  # since we are going to build it in the ci command.
  cabal-setup-monocle = cabal-setup monocle-light.env;

  ci-commands = ''
    set -e
    echo "[+] Building the project"
    cabal build --enable-tests --flags=ci -O0

    echo "[+] Running the tests"
    cabal test --enable-tests --flags=ci -O0 --test-show-details=direct

    echo "[+] Running doctests"
    cabal repl --with-ghc=doctest

    # cabal haddock
    # cabal sdist
    # cabal check
    # cabal install --installdir=/tmp --overwrite-policy=always'}}

    ${lightCI}
  '';

  # A script to be used in ci with nix-shell so that the build can access the elasticsearch service
  ci-run = pkgs.writeScriptBin "monocle-ci-run" ''
    #!/bin/sh
    ${ci-commands}
  '';

  ci-shell = hsPkgs.shellFor {
    packages = p: [ p.monocle ];
    buildInputs = [ hsPkgs.cabal-install hsPkgs.doctest_0_20_0 ci-run ];
  };

  ci = mk-ci "ci" ''
    echo "[+] Setup local ghc shell"
    ${cabal-setup-monocle}

    ${ci-commands}
  '';

  ci-light = mk-ci "ci-light" lightCI;

  lightCI = ''
    echo "[+] Running hlint"
    ${pkgs.hlint}/bin/hlint -XQuasiQuotes src/

    echo "[+] Checking ormolu syntax"
    ${pkgs.ormolu}/bin/ormolu                                 \
      -o -XPatternSynonyms -o -XTypeApplications --mode check \
      $(find src/ -name "*.hs")
  '';

  # dontCheck because doctests are not working...
  monocle = pkgs.haskell.lib.dontCheck hsPkgs.monocle;

  services = pkgs.stdenv.mkDerivation {
    name = "monocle-services";
    buildInputs = base-req ++ services-req;
  };
  shell = hsPkgs.shellFor {
    packages = p: [ (addCriterion p.monocle) p.monocle-codegen ];

    buildInputs = with pkgs.myHaskellPackages;
      [ hlint ghcid haskell-language-server doctest_0_20_0 ] ++ all-req
      ++ services-req ++ [ ci-run ];

    withHoogle = true;

    shellHook = ''
      export PROTOC_FLAGS="-I ${googleapis-src}/ -I ${protobuf-src}/src"
      export PROTOBUF_SRC=${protobuf-src}/src
      export NIX_PATH=nixpkgs=${nixpkgsPath}
      export ELASTIC_URL=http://localhost:19200
    '';
  };
  inherit pkgs;
}
