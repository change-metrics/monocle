let
  # pin the upstream nixpkgs
  nixpkgsPath = fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/9fc2cddf24ad1819f17174cbae47789294ea6dc4.tar.gz";
    sha256 = "058l6ry119mkg7pwmm7z4rl1721w0zigklskq48xb5lmgig4l332";
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

  # update haskell dependencies
  compilerVersion = "8104";
  compiler = "ghc" + compilerVersion;
  overlays = [
    (final: prev: {
      myHaskellPackages = prev.haskell.packages.${compiler}.override {
        overrides = hpFinal: hpPrev: {
          # Unbreak proto3-suite
          range-set-list = pkgs.haskell.lib.dontCheck
            (pkgs.haskell.lib.overrideCabal hpPrev.range-set-list {
              broken = false;
            });
          proto3-suite = pkgs.haskell.lib.dontCheck hpPrev.proto3-suite;

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

          # relude>1 featuer exposed modules
          relude = pkgs.haskell.lib.overrideCabal hpPrev.relude {
            version = "1.0.0.1";
            sha256 = "0cw9a1gfvias4hr36ywdizhysnzbzxy20fb3jwmqmgjy40lzxp2g";
          };

          # bloodhound needs a new release, use current master for now
          bloodhound = pkgs.haskell.lib.overrideCabal hpPrev.bloodhound {
            src = pkgs.fetchFromGitHub {
              owner = "bitemyapp";
              repo = "bloodhound";
              rev = "4775ebb759fe1b7cb5f880e4a41044b2363d98af";
              sha256 = "00wzaj4slvdxanm0krbc6mfn96mi5c6hhd3sywd3gq5m2ff59ggn";
            };
            broken = false;
          };

          gerrit = let
            src = builtins.fetchGit {
              url = "https://softwarefactory-project.io/r/software-factory/gerrit-haskell";
              ref = "master";
              rev = "406435cef0a6175ff36d7c1fc9955d328683caae";
            };
          in pkgs.haskell.lib.dontCheck (hpPrev.callCabal2nix "gerrit" src { });

          # use a more recent version that is not broken in nix
          fakedata = let
            fakedataSrc = builtins.fetchGit {
              url = "https://github.com/fakedata-haskell/fakedata.git";
              rev = "8ede9a9dbf1325df0295883eab59e74108729a28";
              submodules = true;
            };
          in pkgs.haskell.lib.dontCheck
          (hpPrev.callCabal2nix "fakedata" fakedataSrc { });

          monocle =
            hpPrev.callCabal2nix "monocle" (gitignoreSource ../haskell) { };

          monocle-codegen =
            hpPrev.callCabal2nix "monocle-codegen" (gitignoreSource ../codegen)
            { };
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

  # local devel env
  elk-port = 19200;
  nginx-port = 18080;
  monocle-port = 19876;
  monocle2-port = 19875;
  web-port = 13000;
  prom-port = 19090;
  grafana-port = 19030;

  # DB
  info = pkgs.lib.splitString "-" pkgs.stdenv.hostPlatform.system;
  arch = pkgs.lib.elemAt info 0;
  plat = pkgs.lib.elemAt info 1;
  elk = pkgsNonFree.elasticsearch7.overrideAttrs (old: rec {
    version = "7.10.1";
    name = "elasticsearch-${version}";
    src = pkgs.fetchurl {
      url =
        "https://artifacts.elastic.co/downloads/elasticsearch/${name}-${plat}-${arch}.tar.gz";
      sha256 = "1r62afmpmwyxifr4kjlannj44zbh67gdcch5czh4fllv459ajf7f";
    };
  });
  elk-home = "/tmp/es-home";
  elkConf = pkgs.writeTextFile {
    name = "elasticsearch.yml";
    text = ''
      cluster.name: monocle
      http.port: ${toString elk-port}
      discovery.type: single-node
      network.host: 0.0.0.0
      cluster.routing.allocation.disk.threshold_enabled: false
    '';
  };
  elkStart = pkgs.writeScriptBin "elk-start" ''
    #!/bin/sh
    # todo: only set max_map_count when necessary
    ${pkgs.sudo}/bin/sudo sysctl -w vm.max_map_count=262144
    set -ex
    export ES_HOME=${elk-home}
    mkdir -p $ES_HOME/logs $ES_HOME/data
    ${pkgs.rsync}/bin/rsync -a ${elk}/config/ $ES_HOME/config/
    ln -sf ${elk}/modules/ $ES_HOME/
    find $ES_HOME -type f | xargs chmod 0600
    find $ES_HOME -type d | xargs chmod 0700
    cat ${elkConf} > $ES_HOME/config/elasticsearch.yml
    exec ${elk}/bin/elasticsearch -p $ES_HOME/pid
  '';
  elkStop = pkgs.writeScriptBin "elk-stop" ''
    #!/bin/sh
    kill $(cat /tmp/es-home/pid)
  '';
  elkDestroy = pkgs.writeScriptBin "elk-destroy" ''
    #!/bin/sh
    set -x
    [ -f ${elk-home}/pid ] && (${elkStop}/bin/elkstop; sleep 5)
    rm -Rf ${elk-home}/
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
                - localhost:${toString monocle2-port}
    '';
  };
  promStart = pkgs.writeScriptBin "prometheus-start" ''
    #!/bin/sh
    exec ${pkgs.prometheus}/bin/prometheus \
      --config.file=${promConf}            \
      --web.listen-address="0.0.0.0:${toString prom-port}"
  '';

  grafana-home = "/tmp/grafana-home";
  grafanaPromDS = pkgs.writeTextFile {
    name = "prometheus.yml";
    text = ''
      apiVersion: 1
      datasources:
      - name: Prometheus
        type: prometheus
        access: direct
        url: http://localhost:${toString prom-port}
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

      [plugin.grafana-image-renderer]
      rendering_ignore_https_errors = true
    '';
  };
  grafanaStart = pkgs.writeScriptBin "grafana-start" ''
    #!/bin/sh -ex
    mkdir -p ${grafana-home}/dashboards
    ${pkgs.rsync}/bin/rsync -a ${pkgs.grafana}/share/grafana/ ${grafana-home}/
    find ${grafana-home} -type f | xargs chmod 0600
    find ${grafana-home} -type d | xargs chmod 0700
    ${pkgs.dhall-json}/bin/dhall-to-json  \
      --file conf/grafana-dashboard.dhall \
      --output ${grafana-home}/dashboards/monocle.json
    cd ${grafana-home}
    cat ${grafanaDashboards} > conf/provisioning/dashboards/dashboard.yaml
    cat ${grafanaPromDS} > conf/provisioning/datasources/prometheus.yaml
    exec ${pkgs.grafana}/bin/grafana-server -config ${grafanaConf}
  '';

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
             proxy_pass http://localhost:${toString monocle2-port}/;
             proxy_http_version 1.1;
          }

          location /api/ {
              proxy_pass http://localhost:${toString monocle-port}/api/;
              proxy_http_version 1.1;
          }

          location /auth {
              proxy_pass http://localhost:${toString monocle2-port}/auth;
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

  monocle-home = "/tmp/monocle-home";
  monocleApiStart = pkgs.writeScriptBin "monocle-api-start" ''
    #!/bin/sh
    set -ex
    export $(cat .secrets)
    if ! test -d ${monocle-home}; then
        ${pkgs.python3}/bin/python -mvenv ${monocle-home}
        ${monocle-home}/bin/pip install --upgrade pip
        ${monocle-home}/bin/pip install -r requirements.txt
    fi

    if ! test -f ${monocle-home}/bin/monocle; then
        ${monocle-home}/bin/python3 setup.py install
    fi

    export ELASTIC_CONN="localhost:${toString elk-port}"
    exec ${monocle-home}/bin/uwsgi --http ":${
      toString monocle-port
    }" --manage-script-name --mount /app=monocle.webapp:app
  '';

  monocleApi2Start = pkgs.writeScriptBin "monocle-api2-start" ''
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

  monocleCrawlersLegacy = pkgs.writeScriptBin "monocle-crawlers-legacy-start" ''
    #!/bin/sh
    set -ex
    export $(cat .secrets)
    if ! test -d ${monocle-home}; then
        ${pkgs.python3}/bin/python -mvenv ${monocle-home}
        ${monocle-home}/bin/pip install --upgrade pip
        ${monocle-home}/bin/pip install -r requirements.txt
    fi

    if ! test -f ${monocle-home}/bin/monocle; then
        ${monocle-home}/bin/python3 setup.py install
    fi

    exec ${monocle-home}/bin/monocle --elastic-conn "localhost:${
      toString elk-port
    }" crawler --config etc/config.yaml
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
        (monocle-startp "elk" "${elkStart}" )
        (monocle-startp "nginx" "${nginxStart}" )
        (monocle-startp "prometheus" "${promStart}" )
        (monocle-startp "grafana" "${grafanaStart}" )
        (monocle-startp "monocle-api" "${monocleApiStart}" )
        (monocle-startp "monocle-api2" "${monocleApi2Start}" )
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
    elkStart
    nginxStart
    promStart
    grafanaStart
    monocleApiStart
    monocleApi2Start
    monocleWebStart
    monocleCrawlersLegacy
    monocleEmacsStart
    monocleGhcid
  ];

  # define the base requirements
  base-req = [ pkgs.bashInteractive pkgs.coreutils pkgs.gnumake ];
  codegen-req = [ pkgs.protobuf pkgs.ocamlPackages.ocaml-protoc pkgs.glibc ]
    ++ base-req;

  # haskell dependencies for codegen
  hsPkgs = pkgs.myHaskellPackages;
  easyHlsSrc = pkgs.fetchFromGitHub {
    owner = "jkachmar";
    repo = "easy-hls-nix";
    rev = "a332d37c59fdcc9e44907bf3f48cf20b6d275ef4";
    sha256 = "1zwgg8qd33411c9rdlz1x7qv65pbw80snlvadifm4bm4avpkjhnk";
  };
  easyHls = pkgs.callPackage easyHlsSrc { ghcVersions = [ "8.10.4" ]; };

  hs-req = [ hsPkgs.cabal-install hsPkgs.ormolu hsPkgs.proto3-suite pkgs.zlib ];

  # define python requirements
  python-req = [ pkgs.python39Packages.mypy-protobuf pkgs.black ];

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
  all-req = codegen-req ++ hs-req ++ python-req ++ javascript-req ++ go-req
    ++ doc-req;

in rec {
  services = pkgs.stdenv.mkDerivation {
    name = "monocle-services";
    buildInputs = base-req ++ services-req;
  };
  shell = hsPkgs.shellFor {
    packages = p: [ p.monocle p.monocle-codegen ];

    buildInputs = with pkgs.myHaskellPackages;
      [ hlint ghcid easyHls.nixosDrv ] ++ all-req ++ services-req;

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
