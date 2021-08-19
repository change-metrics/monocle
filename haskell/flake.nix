{
  description = "The monocle application";

  nixConfig.bash-prompt = "[nix]λ ";

  inputs = { };

  outputs = { self, nixpkgs }:
    let
      pkgs = (import ../nix/default.nix).pkgs;
      easy-hls-src = pkgs.fetchFromGitHub {
        owner = "jkachmar";
        repo = "easy-hls-nix";
        rev = "a332d37c59fdcc9e44907bf3f48cf20b6d275ef4";
        sha256 = "1zwgg8qd33411c9rdlz1x7qv65pbw80snlvadifm4bm4avpkjhnk";
      };
      easy-hls = pkgs.callPackage easy-hls-src { ghcVersions = [ "8.10.4" ]; };

    in rec {
      defaultPackage.x86_64-linux = packages.monocle;

      packages = with pkgs.myHaskellPackages; { inherit monocle; };

      devShell.x86_64-linux = pkgs.myHaskellPackages.shellFor {
        packages = p: [ p.monocle ];

        buildInputs = with pkgs.myHaskellPackages; [
          cabal-install
          hlint
          ghcid
          easy-hls.nixosDrv
        ];

        withHoogle = false;
      };
    };
}
