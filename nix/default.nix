let
  # pin the upstream nixpkgs
  nixpkgsSrc = (import (fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/8d0340aee5caac3807c58ad7fa4ebdbbdd9134d6.tar.gz";
    sha256 = "0r00azbz64fz8yylm8x37imnrsm5cdzshd5ma8gwfwjyw166n3r1";
  }));

  # create the main package set without options
  pkgs = nixpkgsSrc { };

  # define the base requirements
  codegen-req = [
    pkgs.bashInteractive
    pkgs.coreutils
    pkgs.gnumake
    pkgs.protobuf
    pkgs.ocamlPackages.ocaml-protoc
    pkgs.glibc
  ];

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
  hs-req = [ ghc hsPkgs.cabal-install hsPkgs.ormolu hsPkgs.proto3-suite ];

  # define python requirements
  python-req = [ pkgs.python39Packages.mypy-protobuf pkgs.black ];

  # define javascript requirements
  javascript-req = [ pkgs.nodejs ];

  # define openapi requirements
  gnostic = pkgs.buildGoModule rec {
    pname = "gnostic";
    version = "0.5.5";
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
    buildInputs = all-req;
    shellHook = ''
      export PROTOC_FLAGS="-I ${googleapis-src}/ -I ${protobuf-src}/src"
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
  shell = codegen-shell;
}
