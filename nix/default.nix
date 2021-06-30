let
  # pin the upstream nixpkgs
  nixpkgsPath = fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/8d0340aee5caac3807c58ad7fa4ebdbbdd9134d6.tar.gz";
    sha256 = "0r00azbz64fz8yylm8x37imnrsm5cdzshd5ma8gwfwjyw166n3r1";
  };
  nixpkgsSrc = (import nixpkgsPath);

  # create the main package set without options
  pkgs = nixpkgsSrc { };
  pkgsNonFree = nixpkgsSrc { config.allowUnfree = true; };

  # local devel env
  elk-port = 19200;
  nginx-port = 18080;
  monocle-port = 19876;
  monocle2-port = 19875;
  web-port = 13000;

  # DB
  elk = pkgsNonFree.elasticsearch7;
  elk-home = "/tmp/es-home";
  # elk-port = 9200;
  elkConf = pkgs.writeTextFile {
    name = "elasticsearch.yml";
    text = ''
      cluster.name: monocle
      http.port: ${toString elk-port}
      discovery.type: single-node
      network.host: 0.0.0.0
    '';
  };
  elkStart = pkgs.writeScriptBin "elk-start" ''
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
  elkStop = pkgs.writeScriptBin "elk-stop" "kill $(cat /tmp/es-home/pid)";
  elkDestroy = pkgs.writeScriptBin "elk-destroy" ''
    set -x
    [ -f ${elk-home}/pid ] && (${elkStop}/bin/elkstop; sleep 5)
    rm -Rf ${elk-home}/
  '';

  # WEB
  nginx-home = "/tmp/nginx-home";
  nginxConf = pkgs.writeTextFile {
    name = "nginx.conf";
    text = ''
      error_log /dev/stdout info;

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
    set -ex
    mkdir -p ${nginx-home};
    exec ${pkgs.nginx}/bin/nginx -c ${nginxConf} -p ${nginx-home}/ -g "daemon off;"
  '';

  monocle-home = "/tmp/monocle-home";
  monocleApiStart = pkgs.writeScriptBin "monocle-api-start" ''
    set -ex
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
    set -ex
    cd haskell; cabal repl
  '';

  monocleWebStart = pkgs.writeScriptBin "monocle-web-start" ''
    set -ex
    cd web

    if ! test -d node_modules; then
        ${pkgs.nodejs}/bin/npm install
    fi

    export WEB_PORT=${toString web-port}
    exec ${pkgs.nodejs}/bin/npm start
  '';

  services-req =
    [ elkStart nginxStart monocleApiStart monocleApi2Start monocleWebStart ];

  # define the base requirements
  base-req = [ pkgs.bashInteractive pkgs.coreutils pkgs.gnumake ];
  codegen-req = [ pkgs.protobuf pkgs.ocamlPackages.ocaml-protoc pkgs.glibc ]
    ++ base-req;

  # define the haskell toolchain
  grpc-haskell-src = pkgs.fetchFromGitHub {
    owner = "awakesecurity";
    repo = "gRPC-haskell";
    rev = "d821e58c2b72f127ce5b74b69dac8cf3d7f558ad";
    sha256 = "1ms6v58rznkqk4807n9yr888lf0bbn7p7a9mjwmbdckc1pa1gxdv";
  };
  grpc-overlay = (import "${grpc-haskell-src}/release.nix").overlay;
  grpc-nixpkgs = (import "${grpc-haskell-src}/nixpkgs.nix");
  grpc-pkgs = grpc-nixpkgs {
    overlays = [ grpc-overlay ];
    config = { allowBroken = true; };
  };
  hsPkgs = grpc-pkgs.haskellPackages;
  ghc = hsPkgs.ghcWithPackages (p: [ p.language-protobuf p.relude ]);
  hs-req =
    [ ghc hsPkgs.cabal-install hsPkgs.ormolu hsPkgs.proto3-suite pkgs.zlib ];

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

  # all requirement
  all-req = codegen-req ++ hs-req ++ python-req ++ javascript-req ++ go-req;

in rec {
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
  # Build using: TMPDIR=/tmp/podman podman load < $(nix-build --attr codegen-container)
  codegen-container = pkgs.dockerTools.buildImage {
    name = " changemetrics/monocle_codegen";
    contents = all-req;
    config = {
      Cmd = [ "/bin/make" ];
      WorkingDir = "/data";
    };
    tag = "latest";
  };
  services = pkgs.stdenv.mkDerivation {
    name = "monocle-services";
    buildInputs = base-req ++ services-req;
  };
  shell = codegen-shell;
}
