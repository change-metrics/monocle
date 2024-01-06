{ elasticsearch-port ? 19200, nixpkgsPath, hspkgs, self }:
let
  nixpkgsSrc = import nixpkgsPath;

  rev = if self ? rev then
    self.rev
  else
    throw "Refusing to build from a dirty Git tree!";

  src = pkgs.lib.cleanSourceWith {
    src = self; # The original, unfiltered source
    filter = path: type:
      type == "directory" || (pkgs.lib.hasSuffix ".cabal" path)
      || (pkgs.lib.hasSuffix ".hs" path) || (pkgs.lib.hasSuffix ".dhall" path)
      || (pkgs.lib.hasSuffix ".json" path) || (pkgs.lib.hasSuffix ".yaml" path)
      || (pkgs.lib.hasSuffix "LICENSE" path)
      || (pkgs.lib.hasSuffix ".graphql" path);

  };

  # Add monocle and patch broken dependency to the haskell package set
  haskellExtend = hpFinal: hpPrev: {
    monocle = hpPrev.callCabal2nix "monocle" src { };

    # upgrade to bloodhound 0.20 needs some work
    bloodhound = pkgs.haskell.lib.overrideCabal hpPrev.bloodhound {
      version = "0.19.1.0";
      sha256 = "sha256-QEN1wOLLUEsDKAbgz8ex0wfK/duNytvRYclwkBj/1G0=";
    };

    # relax bound for doctest, ghc-prim, primitive, template-haskell, text and transformers
    proto3-wire = pkgs.haskell.lib.doJailbreak hpPrev.proto3-wire;
    # proto3-suite needs HEAD for swagger fix
    proto3-suite = let
      src = pkgs.fetchFromGitHub {
        owner = "awakesecurity";
        repo = "proto3-suite";
        # https://github.com/awakesecurity/proto3-suite/pull/239
        rev = "6b6245fe8526a1f9fd64472bf1218bd7fdea9960";
        sha256 = "sha256-XYGeQJ2EXDnezI8NfhI+R3t1k31PKyfg9doOKp5FsCk=";
      };
      pkg = hpPrev.callCabal2nix "proto3-suite" src { };
    in pkgs.lib.pipe pkg [
      pkgs.haskell.lib.compose.doJailbreak
      pkgs.haskell.lib.compose.dontCheck
      (pkgs.haskell.lib.compose.disableCabalFlag "swagger")
      (pkgs.haskell.lib.compose.disableCabalFlag "large-records")
    ];
  };

  # create the main package set without options
  pkgs = nixpkgsSrc { system = "x86_64-linux"; };
  pkgsNonFree = nixpkgsSrc {
    system = "x86_64-linux";
    config.allowUnfree = true;
  };
  # final haskell set, see: https://github.com/NixOS/nixpkgs/issues/25887
  hsPkgs = hspkgs.hspkgs.extend haskellExtend;

  # manually adds build dependencies for benchmark and codegen that are not managed by cabal2nix
  addExtraDeps = drv:
    pkgs.haskell.lib.addBuildDepends drv ([
      hsPkgs.criterion
      hsPkgs.casing
      hsPkgs.language-protobuf
    ]);

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
      mkdir -m 0700 ${name}
      chown $(id -u) ${name}
    }
    cd ${name}
  '';

in rec {
  # Overlay
  hExtend = haskellExtend;

  # DB
  elk7Version = "7.17.5";
  info = pkgs.lib.splitString "-" pkgs.stdenv.hostPlatform.system;
  arch = pkgs.lib.elemAt info 0;
  plat = pkgs.lib.elemAt info 1;
  elasticsearch = pkgsNonFree.elasticsearch7.overrideAttrs (old: rec {
    version = elk7Version;
    name = "elasticsearch-${version}";
    src = pkgs.fetchurl {
      url =
        "https://artifacts.elastic.co/downloads/elasticsearch/${name}-${plat}-${arch}.tar.gz";
      sha256 = "ocz3CJFf+diThmocrgSnhWW/fjuRLLyCxwUKl3Cm7WA=";
    };
  });
  elasticsearch-home = "~/.local/share/monocle/elasticsearch-home";
  elasticsearchConf = pkgs.writeTextFile {
    name = "elasticsearch.yml";
    text = ''
      cluster.name: monocle
      http.port: ${toString elasticsearch-port}
      discovery.type: single-node
      network.host: 0.0.0.0
      cluster.routing.allocation.disk.threshold_enabled: false
      ingest.geoip.downloader.enabled: false
    '';
  };
  elasticsearchStart = pkgs.writeScriptBin "elasticsearch-start" ''
    ${headers}

    # todo: only set max_map_count when necessary
    ${pkgs.sudo}/bin/sudo sysctl -w vm.max_map_count=262144 || true

    ${mkHome elasticsearch-home}
    export ES_HOME=${elasticsearch-home}
    mkdir -p $ES_HOME/logs $ES_HOME/data $ES_HOME/modules $ES_HOME/plugins
    ${pkgs.rsync}/bin/rsync -a ${elasticsearch}/config/ $ES_HOME/config/
    ${pkgs.rsync}/bin/rsync -a ${elasticsearch}/modules/ $ES_HOME/modules/
    find $ES_HOME -type f | xargs chmod 0600
    find $ES_HOME -type d | xargs chmod 0700
    find $ES_HOME/modules -type f | xargs chmod 0700
    cat ${elasticsearchConf} > $ES_HOME/config/elasticsearch.yml
    exec ${elasticsearch}/bin/elasticsearch
  '';
  # DB Companion
  kibana = pkgsNonFree.kibana7.overrideAttrs (old: rec {
    version = elk7Version;
    name = "kibana-${version}";
    src = pkgs.fetchurl {
      url =
        "https://artifacts.elastic.co/downloads/kibana/${name}-${plat}-${arch}.tar.gz";
      sha256 = "Oobbs3es1AegTFmzG2ln+iuJaRZrONamzDQOXKRF2Tk=";
    };
  });
  kibana-home = "~/.local/share/monocle/kibana-home";
  kibanaConf = pkgs.writeTextFile {
    name = "kibana.yml";
    text = ''
      path.data: ${kibana-home}/data
      elasticsearch.hosts: [ "http://localhost:${toString elasticsearch-port}" ]
    '';
  };
  kibanaStart = pkgs.writeScriptBin "kibana-start" ''
    ${headers}

    ${mkHome kibana-home}

    mkdir -p ${kibana-home}/config
    mkdir -p ${kibana-home}/data

    cat ${kibanaConf} > ${kibana-home}/config/kibana.yml

    export KBN_PATH_CONF=${kibana-home}/config
    export DATA_PATH=${kibana-home}/data
    exec ${kibana}/bin/kibana
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
  prom-home = "~/.local/share/monocle/prometheus-home";
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

  grafana-home = "~/.local/share/monocle/grafana-home";
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
    export $(cat .secrets)
    cabal repl --build-depends pretty-simple monocle
  '';

  monocleGhcid = pkgs.writeScriptBin "monocle-ghcid" ''
    #!/bin/sh
    set -x
    ${hspkgs.ghcid}/bin/ghcid -c "cabal repl monocle" $*
  '';

  monocleWebStart = pkgs.writeScriptBin "monocle-web-start" ''
    #!/bin/sh
    set -ex
    cd web

    if ! test -d node_modules; then
        ${pkgs.nodejs}/bin/npm install
    fi

    export WEB_PORT=${toString web-port}
    if [ -z "$MONOCLE_PUBLIC_URL" ]; then
      export REACT_APP_API_URL="http://localhost:${toString monocle-port}"
    else
      export REACT_APP_API_URL=$MONOCLE_PUBLIC_URL
    fi
    export REACT_APP_TITLE="Monocle Dev"
    exec ${pkgs.nodejs}/bin/npm start
  '';

  services-req = [
    kibanaStart
    elasticsearchStart
    monocleReplStart
    monocleWebStart
    monocleGhcid
  ];

  # define the base requirements
  base-req = [ pkgs.bashInteractive hspkgs.coreutils pkgs.gnumake ];
  codegen-req = [ pkgs.protobuf pkgs.ocamlPackages.ocaml-protoc ] ++ base-req;

  hs-req = [
    # Here we pull the executable from the global pkgs, not the haskell packages
    hspkgs.cabal-install
    hspkgs.fourmolu
    # Here we pull the proto3-suite executable from the haskell packages, not the global pkgs
    hsPkgs.proto3-suite
    hspkgs.zlib
    hspkgs.weeder
  ];

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
    echo "[+] Building the project"
    cabal build --enable-tests --flags=ci -O0

    echo "[+] Running the tests"
    cabal test --enable-tests --flags=ci -O0 --test-show-details=direct

    echo "[+] Running doctests"
    export PATH=${hsPkgs.doctest}/bin:$PATH
    cabal repl --with-ghc=doctest --ghc-options=-Wno-unused-packages

    # cabal haddock
    # cabal sdist
    # cabal check
    # cabal install --installdir=/tmp --overwrite-policy=always'}}

    ${fast-ci-commands}
  '';

  # The monocle-ci-run script for ci validation.
  ci-run = mkRun "ci" ci-commands;

  ci-shell = hsPkgs.shellFor {
    packages = p: [ p.monocle ];
    buildInputs = [ hspkgs.cabal-install ci-run ];
  };

  hlint = args: "${hspkgs.hlint}/bin/hlint -XQuasiQuotes ${args} src/";
  fourmolu = mode: "${hspkgs.fourmolu}/bin/fourmolu --mode ${mode} src/";

  nixfmt = mode: "${pkgs.nixfmt}/bin/nixfmt ./nix/default.nix";

  fast-ci-commands = ''
    echo "[+] Running hlint"
    ${hlint ""}

    echo "[+] Checking fourmolu syntax"
    ${fourmolu "check"}

    echo "[+] Checking nixfmt syntax"
    ${nixfmt "--check"}
  '';

  fast-ci-run = mkRun "fast-ci" fast-ci-commands;

  reformat-run = mkRun "reformat" ''
    echo "[+] Apply hlint suggestions"
    find src/ -name "*hs" -exec ${hspkgs.hlint}/bin/hlint -XQuasiQuotes --refactor --refactor-options="-i" {} \;

    echo "[+] Reformat with fourmolu"
    ${fourmolu "inplace"}

    echo "[+] Reformat with nixfmt"
    ${nixfmt ""}
  '';

  monocle = hsPkgs.monocle;

  monocle-exe = pkgs.haskell.lib.justStaticExecutables
    (hsPkgs.monocle.overrideAttrs (_: { MONOCLE_COMMIT = rev; }));

  monocle-wrapper = pkgs.writeScriptBin "monocle" ''
    #!/usr/bin/sh -e
    # Use fakeroot to avoid `No user exists for uid` error
    env LD_PRELOAD=${pkgs.fakeroot}/lib/libfakeroot.so ${monocle-exe}/bin/monocle $*
  '';

  containerMonocle = let
    # Container user info
    user = "monocle";
    home = "var/lib/${user}";

    # Create a passwd entry so that openssh can find the .ssh config
    createPasswd =
      "mkdir etc; echo ${user}:x:0:0:monocle:/${home}:/bin/bash >> etc/passwd";

    # Ensure the home directory is r/w for any uid
    rwHome = "mkdir -p -m 1777 ${home}";

    # Ensure /bin/sh (docker healthcheck assumes /bin/sh)
    binSh = "ln -s /usr/bin/sh bin/sh";

  in pkgs.dockerTools.buildLayeredImage {
    name = "quay.io/change-metrics/monocle-exe";
    contents = [ monocle-wrapper ];
    extraCommands = "${createPasswd} && ${rwHome} && ${binSh}";
    tag = "latest";
    created = "now";
    config = {
      USER = "1000";
      Env = [ "HOME=/${home}" ];
    };
    # To update, run: nix run github:TristanCacqueray/nixpkgs/skopeo-fix#nix-prefetch-docker -- -c nix-prefetch-docker --image-name registry.access.redhat.com/ubi8/ubi --image-tag 8.8-1067
    fromImage = pkgs.dockerTools.pullImage {
      imageName = "registry.access.redhat.com/ubi8/ubi";
      imageDigest =
        "sha256:269e9753043a4066af12649e921c6ad3201702fda5b2652f7a4aa010c2ed4c1a";
      sha256 = "0wc566pph59mwn1dyw9h06lmfzc4x2p665lxffplpgqc10cr3w2c";
      finalImageName = "registry.access.redhat.com/ubi8/ubi";
      finalImageTag = "8.8-1067";
    };
  };

  services = pkgs.stdenv.mkDerivation {
    name = "monocle-services";
    buildInputs = base-req ++ services-req;
  };
  shell = hsPkgs.shellFor {
    packages = p: [ (addExtraDeps p.monocle) p.pretty-simple ];

    buildInputs = [
      hspkgs.hlint
      hspkgs.apply-refact
      hspkgs.ghcid
      hspkgs.haskell-language-server
      hsPkgs.doctest
    ] ++ all-req ++ services-req ++ [ ci-run fast-ci-run reformat-run ];

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
