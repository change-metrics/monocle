{ elasticsearch-port ? 19200, nixpkgsPath ? (fetchTarball {
  url =
    # Keep in sync with haskell/flake.nix
    "https://github.com/NixOS/nixpkgs/archive/ed014c27f4d0ca772fb57d3b8985b772b0503bbd.tar.gz";
  sha256 = "sha256-Jxyn3uXFr5LdZNNiippI/obtLXAVBM18uVfiKVP4j9Q=";
}) }:
let
  # pin the upstream nixpkgs
  nixpkgsSrc = import nixpkgsPath;

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
  compilerVersion = "922";
  compiler = "ghc" + compilerVersion;

  mk-morpheus-lib = hpPrev: name:
    let
      morpheus-graphql-src = pkgs.fetchFromGitHub {
        owner = "morpheusgraphql";
        repo = "morpheus-graphql";
        # Fix for bytestring-0.11, proposed in https://github.com/morpheusgraphql/morpheus-graphql/pull/723
        rev = "df3a4b0d11b53de3ddf3b43966e0242877541e50";
        sha256 = "sha256-rrDWIYmY9J9iPI/lSuflyyxapAXyuHbLP86awH33mzo=";
      };
    in (hpPrev.callCabal2nix "morpheus-graphql-${name}"
      "${morpheus-graphql-src}/morpheus-graphql-${name}" { });

  mk-servant-lib = hpPrev: name:
    let
      # Use direct source because nixpkgs somehow can't fetch
      servant-src = builtins.fetchGit {
        url = "https://github.com/haskell-servant/servant";
        ref = "master";
        rev = "c19ed0fb925fbe62365adcaf286c00c497adf8fb";
      };
    in (hpPrev.callCabal2nix "sevant${name}" "${servant-src}/servant${name}"
      { });

  overlays = [
    (final: prev: {
      myHaskellPackages = prev.haskell.packages.${compiler}.override {
        overrides = hpFinal: hpPrev: {
          # For proto3-suite:
          range-set-list = pkgs.haskell.lib.dontCheck
            (pkgs.haskell.lib.overrideCabal hpPrev.range-set-list {
              broken = false;
            });
          data-diverse = pkgs.haskell.lib.dontCheck
            (pkgs.haskell.lib.overrideCabal hpPrev.data-diverse {
              broken = false;
            });
          # Test suite hang
          servant-auth-server = pkgs.haskell.lib.dontCheck
            (pkgs.haskell.lib.overrideCabal hpPrev.servant-auth-server {
              broken = false;
            });
          # HEAD is needed for bytestring-0.11 and ghc-9.2 base
          proto3-suite = let
            src = builtins.fetchGit {
              url = "https://github.com/awakesecurity/proto3-suite";
              ref = "master";
              rev = "5456b633ba7283ff11adcd457744b54ebdd28a37";
            };
            base = pkgs.haskell.lib.dontCheck
              (hpPrev.callCabal2nix "proto3-suite" src { });
          in pkgs.haskell.lib.disableCabalFlag base "swagger";

          # HEAD is needed for bytestring-0.11
          proto3-wire = let
            src = builtins.fetchGit {
              url = "https://github.com/awakesecurity/proto3-wire";
              ref = "master";
              rev = "a5ed1a3bff0816cc247a1058232f3ed8a6f1e873";
            };
          in pkgs.haskell.lib.dontCheck
          (hpPrev.callCabal2nix "proto3-wire" src { });

          oidc-client = pkgs.haskell.lib.overrideCabal hpPrev.oidc-client {
            version = "0.6.1.0";
            sha256 = "sha256-PtzYvIs6gXO40JHMElNH/i+kxMSZephJMkkJTGCzEkI=";
          };

          jose-jwt = (pkgs.haskell.lib.overrideCabal hpPrev.jose-jwt {
            broken = false;
          });

          # Hackage version does not build without a bump
          text-time = (pkgs.haskell.lib.overrideCabal hpPrev.text-time {
            broken = false;
            src = builtins.fetchGit {
              url = "https://github.com/klangner/text-time.git";
              ref = "master";
              rev = "1ff65c2c8845e3fdd99900054f0596818a95c316";
            };
          });

          # ghc-9.2 fix proposed in https://github.com/byteverse/json-syntax/pull/11
          # and https://github.com/byteverse/json-syntax/pull/13
          json-syntax = let
            src = builtins.fetchGit {
              url = "https://github.com/TristanCacqueray/json-syntax";
              ref = "ghc-9.2-integration";
              rev = "f54325b2c601b962cfb078c2295b09a0780d7313";
            };
          in (pkgs.haskell.lib.dontCheck
            (hpPrev.callCabal2nix "bytebuild" src { }));

          # ghc-9.2 fix proposed in: https://github.com/byteverse/bytesmith/pull/22
          bytesmith = let
            src = builtins.fetchGit {
              url = "https://github.com/teto/bytesmith";
              ref = "ghc92";
              rev = "dfdf6922e118932ba7d671e05a5b65998349cee8";
            };
          in (hpPrev.callCabal2nix "bytesmith" src { });

          # Master version for ghc-9.2
          bytebuild = let
            src = builtins.fetchGit {
              url = "https://github.com/byteverse/bytebuild";
              ref = "master";
              rev = "dde5a9b07d03f5a9c33eba6d63ca0140ab6f406e";
            };
          in (pkgs.haskell.lib.dontCheck
            (hpPrev.callCabal2nix "bytebuild" src { }));

          # ghc-9.2 fix proposed: https://github.com/andrewthad/scientific-notation/pull/4
          scientific-notation = let
            src = builtins.fetchGit {
              url = "https://github.com/andrewthad/scientific-notation";
              ref = "master";
              rev = "0076fe9ed83e70ce068962fbd1b49d475af7a7f2";
            };
          in (pkgs.haskell.lib.dontCheck
            (hpPrev.callCabal2nix "scientific-notation" src { }));

          # nixpkgs somehow doesn't fetch, use direct src
          servant = mk-servant-lib hpPrev "";
          servant-foreign = mk-servant-lib hpPrev "-foreign";
          servant-server = mk-servant-lib hpPrev "-server";

          # nixpkgs version are broken, use direct src
          morpheus-graphql-tests = mk-morpheus-lib hpPrev "tests";
          morpheus-graphql-core = mk-morpheus-lib hpPrev "core";
          morpheus-graphql-code-gen = mk-morpheus-lib hpPrev "code-gen";
          morpheus-graphql-client = mk-morpheus-lib hpPrev "client";

          gerrit = let
            src = builtins.fetchGit {
              url =
                "https://softwarefactory-project.io/r/software-factory/gerrit-haskell";
              ref = "refs/changes/41/24541/1";
              rev = "c0c337bccb35e7d94e6125ab034cfcc9efe68476";
            };
          in pkgs.haskell.lib.dontCheck (hpPrev.callCabal2nix "gerrit" src { });

          # Relude needs head for ghc-9.2
          relude = (pkgs.haskell.lib.overrideCabal hpPrev.relude {
            broken = false;
            src = builtins.fetchGit {
              url = "https://github.com/kowainik/relude";
              ref = "main";
              rev = "fcb32257a37e34a48f70ddd55424394de4bb025c";
            };
          });

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
  pkgsNonFree = nixpkgsSrc {
    system = "x86_64-linux";
    config.allowUnfree = true;
  };

  # manually adds build dependencies for benchmark that are not managed by cabal2nix
  addCriterion = drv:
    pkgs.haskell.lib.addBuildDepends drv ([ pkgs.myHaskellPackages.criterion ]);

  # local devel env
  monocle-port = 8080;
  web-port = 3000;
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
    contents = [ pkgs.coreutils pkgs.gnused pkgs.findutils ];
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

  monocleReplStart = pkgs.writeScriptBin "monocle-repl" ''
    #!/bin/sh
    set -ex
    if [ -f .secrets ]; then
      export $(cat .secrets)
      cd haskell
    else
      export $(cat ../.secrets)
    fi
    cabal repl --build-depends pretty-simple monocle
  '';

  monocleGhcid = pkgs.writeScriptBin "monocle-ghcid" ''
    #!/bin/sh
    set -x
    cd haskell 2> /dev/null; ${pkgs.ghcid}/bin/ghcid -c "cabal repl monocle" $*
  '';

  monocleWebStart = pkgs.writeScriptBin "monocle-web-start" ''
    #!/bin/sh
    set -ex
    cd web

    if ! test -d node_modules; then
        ${pkgs.nodejs}/bin/npm install
    fi

    export WEB_PORT=${toString web-port}
    export REACT_APP_API_URL=http://localhost:${toString monocle-port}
    export REACT_APP_TITLE="Monocle Dev"
    exec ${pkgs.nodejs}/bin/npm start
  '';

  services-req = [
    elasticsearchStart
    promStart
    grafanaStart
    monocleReplStart
    monocleWebStart
    monocleGhcid
  ];

  # define the base requirements
  base-req = [ pkgs.bashInteractive pkgs.coreutils pkgs.gnumake ];
  codegen-req = [ pkgs.protobuf pkgs.ocamlPackages.ocaml-protoc pkgs.glibc ]
    ++ base-req;

  # haskell dependencies for codegen
  hsPkgs = pkgs.myHaskellPackages;

  hs-req = [ pkgs.cabal-install pkgs.ormolu hsPkgs.proto3-suite pkgs.zlib ];

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

  # Helper function to create helper script.
  mkRun = name: commands:
    pkgs.writeScriptBin "monocle-${name}-run" ''
      #!/bin/sh -e
      # Start from the project root
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)

      ${commands}
    '';

  ci-commands = ''
    cd haskell;

    echo "[+] Building the project"
    cabal build --enable-tests --flags=ci -O0

    echo "[+] Running the tests"
    cabal test --enable-tests --flags=ci -O0 --test-show-details=direct

    echo "[+] Running doctests"
    export PATH=${hsPkgs.doctest_0_20_0}/bin:$PATH
    cabal repl --with-ghc=doctest --ghc-options=-Wno-unused-packages

    # cabal haddock
    # cabal sdist
    # cabal check
    # cabal install --installdir=/tmp --overwrite-policy=always'}}

    cd ..;
    ${fast-ci-commands}
  '';

  # A script to be used in ci with nix-shell
  ci-run = mkRun "ci" ci-commands;

  ci-shell = hsPkgs.shellFor {
    packages = p: [ p.monocle ];
    buildInputs = [ pkgs.cabal-install ci-run ];
  };

  hlint = args: "${pkgs.hlint}/bin/hlint -XQuasiQuotes ${args} haskell/src/";
  ormolu = mode: ''
    ${pkgs.ormolu}/bin/ormolu                                 \
      -o -XPatternSynonyms -o -XTypeApplications -o -XImportQualifiedPost --mode ${mode} \
      $(find haskell/src/ -name "*.hs")
  '';
  nixfmt = mode: "${pkgs.nixfmt}/bin/nixfmt ./nix/default.nix";

  fast-ci-commands = ''
    echo "[+] Running hlint"
    ${hlint ""}

    echo "[+] Checking ormolu syntax"
    ${ormolu "check"}

    echo "[+] Checking nixfmt syntax"
    ${nixfmt "--check"}
  '';

  fast-ci-run = mkRun "fast-ci" fast-ci-commands;

  reformat-run = mkRun "reformat" ''
    # This needs apply-refact, but it doesn't build in our package set and that would requires pulling an extra ghc.
    # Let's try again next time we bump nixpkgs.
    # echo "[+] Apply hlint suggestions"
    # ${hlint ''--refactor --refactor-options="-i"''}

    echo "[+] Reformat with ormolu"
    ${ormolu "inplace"}

    echo "[+] Reformat with nixfmt"
    ${nixfmt ""}
  '';

  # dontCheck because doctests are not working...
  monocle = pkgs.haskell.lib.dontCheck hsPkgs.monocle;

  services = pkgs.stdenv.mkDerivation {
    name = "monocle-services";
    buildInputs = base-req ++ services-req;
  };
  shell = hsPkgs.shellFor {
    packages = p: [
      (addCriterion p.monocle)
      p.monocle-codegen
      p.pretty-simple
    ];

    buildInputs = with pkgs.myHaskellPackages;
      [ pkgs.hlint pkgs.ghcid pkgs.haskell-language-server doctest_0_20_0 ]
      ++ all-req ++ services-req ++ [ ci-run fast-ci-run reformat-run ];

    withHoogle = false;

    shellHook = ''
      export PROTOC_FLAGS="-I ${googleapis-src}/ -I ${protobuf-src}/src"
      export PROTOBUF_SRC=${protobuf-src}/src
      export NIX_PATH=nixpkgs=${nixpkgsPath}
      export MONOCLE_ELASTIC_URL=http://localhost:19200
    '';
  };
  inherit pkgs;
}
