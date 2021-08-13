{
  description = "The monocle application";

  nixConfig.bash-prompt = "[nix]λ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        config = { };

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
                  sha256 =
                    "0cw9a1gfvias4hr36ywdizhysnzbzxy20fb3jwmqmgjy40lzxp2g";
                };

                # bloodhound needs a new release, use current master for now
                bloodhound = pkgs.haskell.lib.overrideCabal hpPrev.bloodhound {
                  src = pkgs.fetchFromGitHub {
                    owner = "bitemyapp";
                    repo = "bloodhound";
                    rev = "4775ebb759fe1b7cb5f880e4a41044b2363d98af";
                    sha256 =
                      "00wzaj4slvdxanm0krbc6mfn96mi5c6hhd3sywd3gq5m2ff59ggn";
                  };
                  broken = false;
                };

                monocle = hpPrev.callCabal2nix "monocle" ./. { };
              };
            };

          })
        ];

        pkgs = import nixpkgs { inherit config overlays system; };
        easy-hls-src = pkgs.fetchFromGitHub {
          owner = "jkachmar";
          repo = "easy-hls-nix";
          rev = "a332d37c59fdcc9e44907bf3f48cf20b6d275ef4";
          sha256 = "1zwgg8qd33411c9rdlz1x7qv65pbw80snlvadifm4bm4avpkjhnk";
        };
        easy-hls =
          pkgs.callPackage easy-hls-src { ghcVersions = [ "8.10.4" ]; };

      in rec {
        defaultPackage = packages.monocle;

        packages = with pkgs.myHaskellPackages; { inherit monocle; };

        devShell = pkgs.myHaskellPackages.shellFor {
          packages = p: [ p.monocle ];

          buildInputs = with pkgs.myHaskellPackages; [
            cabal-install
            hlint
            ghcid
            easy-hls.nixosDrv
          ];

          withHoogle = false;
        };
      });
}
