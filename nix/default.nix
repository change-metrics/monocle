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

          monocle =
            hpPrev.callCabal2nix "monocle" (gitignoreSource ../haskell) { };
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

  script-headers = ''
    #!${pkgs.bash}/bin/bash -ex
  '';

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
      http.port: ELASTIC_PORT
      discovery.type: single-node
      network.host: 0.0.0.0
      cluster.routing.allocation.disk.threshold_enabled: false
    '';
  };
  elkStart = pkgs.writeScriptBin "elk-start" ''
    ${script-headers}
    source ${../monoclectl}

    # todo: only set max_map_count when necessary
    ${pkgs.sudo}/bin/sudo sysctl -w vm.max_map_count=262144 || true

    # look for data dir
    if test -d /src/data; then
      DATA=/src/data
    else
      DATA=${builtins.toString ./../data}
    fi

    # setup standalone ES_HOME
    export ES_HOME=${elk-home}
    mkdir -p $ES_HOME/logs
    ${pkgs.rsync}/bin/rsync -a ${elk}/config/ $ES_HOME/config/
    ln -sf ${elk}/modules/ $ES_HOME/
    ln -sf $DATA $ES_HOME/data
    export PATH=$PATH:${pkgs.findutils}/bin
    find $ES_HOME -type f | xargs chmod 0600
    find $ES_HOME -type d | xargs chmod 0700
    cat ${elkConf} | sed "s/ELASTIC_PORT/$ELASTIC_PORT/" > $ES_HOME/config/elasticsearch.yml

    # start the service
    exec ${elk}/bin/elasticsearch -p $ES_HOME/pid
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
                - localhost:MONOCLE_API_PORT
    '';
  };
  promStart = pkgs.writeScriptBin "prometheus-start" ''
    ${script-headers}
    source ${../monoclectl}
    cat ${promConf} | sed "s/MONOCLE_API_PORT/$MONOCLE_API_PORT/" > data/prometheus.yml
    exec ${pkgs.prometheus}/bin/prometheus \
      --config.file=${promConf}            \
      --web.listen-address="0.0.0.0:$PROM_PORT
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
        url: http://localhost:PROM_PORT
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
      http_port = GRAFANA_PORT

      [plugin.grafana-image-renderer]
      rendering_ignore_https_errors = true
    '';
  };
  grafanaStart = pkgs.writeScriptBin "grafana-start" ''
    ${script-headers}
    source ${../monoclectl}

    # setup standalone grafana
    mkdir -p ${grafana-home}/dashboards
    ${pkgs.rsync}/bin/rsync -a ${pkgs.grafana}/share/grafana/ ${grafana-home}/
    find ${grafana-home} -type f | xargs chmod 0600
    find ${grafana-home} -type d | xargs chmod 0700
    ${pkgs.dhall-json}/bin/dhall-to-json  \
      --file ${../conf/grafana-dashboard.dhall} \
      --output ${grafana-home}/dashboards/monocle.json
    cd ${grafana-home}
    cat ${grafanaDashboards} > conf/provisioning/dashboards/dashboard.yaml
    cat ${grafanaPromDS} | sed "s/PROM_PORT/$PROM_PORT/" > conf/provisioning/datasources/prometheus.yaml
    cat ${grafanaConf} | sed "s/GRAFANA_PORT/$GRAFANA_PORT/" > conf/grafana.ini
    exec ${pkgs.grafana}/bin/grafana-server -config conf/grafana.ini
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
          listen NGINX_PORT default_server;
          proxy_cache one;

          gzip on;
          gzip_min_length 1000;
          gzip_types text/plain text/xml application/javascript text/css;

          location /api/2/ {
             proxy_pass http://localhost:MONOCLE_API_PORT/;
             proxy_http_version 1.1;
          }

          location /api/ {
              proxy_pass http://localhost:MONOCLE_LEGACY_PORT/api/;
              proxy_http_version 1.1;
          }

          location /auth {
              proxy_pass http://localhost:MONOCLE_API_PORT/auth;
              proxy_http_version 1.1;
          }

          # Forward the rest to the node development server
          location / {
              proxy_pass http://localhost:MONOCLE_WEB_PORT;
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
    ${script-headers}
    source ${../monoclectl}

    # setup standalone nginx
    mkdir -p ${nginx-home};
    cat ${nginxConf} | sed \
      -e "s/NGINX_PORT/8080/" \
      -e "s/MONOCLE_API_PORT/$MONOCLE_API_PORT/" \
      -e "s/MONOCLE_LEGACY_PORT/$MONOCLE_LEGACY_PORT/" \
      -e "s/MONOCLE_WEB_PORT/$MONOCLE_WEB_PORT/" \
      > ${nginx-home}/nginx.conf
    exec ${pkgs.nginx}/bin/nginx -c ${nginx-home}/nginx.conf -p ${nginx-home}/ -g "daemon off;"
  '';

  monocleApiLegacyStart = pkgs.writeScriptBin "monocle-api-legacy-start" ''
    ${script-headers};
    export PATH=${pkgs.python3}/bin:$PATH
    exec ./contrib/start-apiv1.sh
  '';
  monocleCrawlersLegacyStart =
    pkgs.writeScriptBin "monocle-crawlers-legacy-start" ''
      ${script-headers}
      export PATH=${pkgs.python3}/bin:$PATH
      exec ./contrib/start-crawlers-legacy.sh
    '';

  # Here we use a `shellFor` to pull in the monocle requirements
  monocleReq = pkgs.myHaskellPackages.shellFor {
    packages = p: [ p.monocle ];
    buildInputs = with pkgs.myHaskellPackages; [ cabal-install hlint ghcid ];
  };

  monocleApiStart = pkgs.writeScriptBin "monocle-api-start" ''
    ${script-headers}
    # Setup the requirements env
    export PATH=$(cat ${monocleReq} | sed -e 's| |/bin:|g' -e 's|$|/bin|'):$PATH
    ${../monoclectl} start-api
  '';

  monocleWebStart = pkgs.writeScriptBin "monocle-web-start" ''
    ${script-headers}
    export PATH=$PATH:${pkgs.nodejs}/bin
    export REACT_APP_TITLE="Monocle Dev"
    ${../monoclectl} start-web
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
        (monocle-startp "monocle-api-legacy" "${monocleApiLegacyStart}" )
        (monocle-startp "monocle-web" "${monocleWebStart}" ))

      (monocle-start)
    '';
  };

  monocleEmacsStart = pkgs.writeScriptBin "launch-monocle-with-emacs" ''
    #!/bin/sh
    set -ex
    emacs --quick --load ${monocleEmacsLauncher}
  '';

  # define the base requirements
  base-req = [ pkgs.bashInteractive pkgs.coreutils pkgs.gnumake ];
  codegen-req = [ pkgs.protobuf pkgs.ocamlPackages.ocaml-protoc pkgs.glibc ]
    ++ base-req;

  # haskell dependencies for codegen
  hsPkgs = pkgs.myHaskellPackages;
  ghc = hsPkgs.ghcWithPackages (p: with p; [ casing language-protobuf relude ]);
  hs-req =
    [ ghc hsPkgs.cabal-install hsPkgs.ormolu hsPkgs.proto3-suite pkgs.zlib ];

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

  kindConf = pkgs.writeTextFile {
    name = "kind.yml";
    text = ''
      kind: Cluster
      apiVersion:  kind.x-k8s.io/v1alpha4
      nodes:
      - role: control-plane
        kubeadmConfigPatches:
        - |
          kind: InitConfiguration
          nodeRegistration:
            kubeletExtraArgs:
              node-labels: "ingress-ready=true"
        extraPortMappings:
        - containerPort: 80
          hostPort: 80
      - role: worker
        extraMounts:
        - hostPath: HOME/.cabal
          containerPath: /cabal
        - hostPath: ./
          containerPath: /src
        - hostPath: /nix
          containerPath: /nix
    '';
  };
  kindIngress = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/kubernetes/ingress-nginx/main/deploy/static/provider/kind/deploy.yaml";
    sha256 = "1ck3df2mjvr7xikzb4hcq4czazqz3m1084pxyxf0cw9ha6bqxkxz";
  };

  dhall-kubernetes = pkgs.dhallPackages.buildDhallGitHubPackage {
    name = "dhall-kubernetes";
    githubBase = "github.com";
    owner = "dhall-lang";
    repo = "dhall-kubernetes";
    rev = "v5.0.0";
    fetchSubmodules = false;
    sha256 = "0irqv44nh6fp3nyal48rzp5ir0y82r897aaw2nnc4yrfh9rd8w0y";
    directory = "1.19";
    file = "package.dhall";
    source = false;
    document = false;
    dependencies = [ ];
  };

in rec {
  kind-start = pkgs.writeScriptBin "kind-start" ''
    #!/bin/sh -e
    export PATH=${pkgs.kind}/bin:$PATH
    cat ${kindConf} | sed "s|HOME|$HOME|" > data/kind.yaml

    if [ "$1" == "stop" ]; then
      kind delete cluster
    else
      kind create cluster --config data/kind.yaml
      kubectl apply -f ${kindIngress}
    fi
  '';
  kube-req = [ kind-start pkgs.kubectl ];
  dhall-req = [ pkgs.dhall pkgs.dhall-json ];

  monoclectl-shell = pkgs.stdenv.mkDerivation {
    name = "monoclectl-shell";
    buildInputs = kube-req ++ services-req ++ dhall-req;
    shellHook = ''
      ROOT=${builtins.toString ./..}
      cd $ROOT
      echo '{ web = "${monocleWebStart}/bin/monocle-web-start", api = "${monocleApiStart}/bin/monocle-api-start" }' | dhall > data/nix-paths.dhall
      export DHALL_PRELUDE=${pkgs.dhallPackages.Prelude}/binary.dhall
      export DHALL_KUBERNETES=${dhall-kubernetes}/binary.dhall
      export XDG_CACHE_HOME=/tmp/ops-home/
      mkdir -p $XDG_CACHE_HOME/dhall
      for pkg in ${pkgs.dhallPackages.Prelude} ${dhall-kubernetes}; do
        for cache in $pkg/.cache/dhall/*; do
            ln -sf $cache $XDG_CACHE_HOME/dhall/
        done
      done
      alias monoclectl=${../monoclectl}
      echo "Welcome to monoclectl, "
    '';
  };
  elk-start = elkStart;
  monocle-api-start = monocleApiStart;

  python-req = [ pkgs.python39Packages.mypy-protobuf pkgs.black ];
  javascript-req = [ pkgs.nodejs ];
  go-req = [ gnostic ];
  services-req = [
    elkStart
    nginxStart
    promStart
    grafanaStart
    monocleApiStart
    monocleApiLegacyStart
    monocleWebStart
    monocleCrawlersLegacyStart
    monocleEmacsStart
  ];

  # all requirement
  all-req = codegen-req ++ hs-req ++ python-req ++ javascript-req ++ go-req;

  codegen-shell = pkgs.stdenv.mkDerivation {
    name = "monocle-codegen-shell";
    buildInputs = all-req ++ services-req;
    shellHook = ''
      export PROTOC_FLAGS="-I ${googleapis-src}/ -I ${protobuf-src}/src"
      export PROTOBUF_SRC=${protobuf-src}/src
      export NIX_PATH=nixpkgs=${nixpkgsPath}
      eval $(egrep ^export ${ghc}/bin/ghc)
    '';
  };

  services = pkgs.stdenv.mkDerivation {
    name = "monocle-services";
    buildInputs = base-req ++ services-req;
  };
  shell = codegen-shell;
  inherit pkgs;
}
