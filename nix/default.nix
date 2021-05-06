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
  hsPkgs = pkgs.haskellPackages;
  ghc = hsPkgs.ghcWithPackages (p: [ p.language-protobuf p.relude ]);
  hs-req = [ ghc hsPkgs.cabal-install ];

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
  go-req = [ gnostic ];

  # all requirement
  all-req = codegen-req ++ hs-req ++ python-req ++ javascript-req ++ go-req;

in rec {
  codegen-shell = pkgs.stdenv.mkDerivation {
    name = "monocle-codegen-shell";
    buildInputs = all-req;
    shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
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
