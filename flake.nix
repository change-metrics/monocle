{
  description = "Monocle";
  nixConfig.bash-prompt = "[nix(monocle)] ";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/ed014c27f4d0ca772fb57d3b8985b772b0503bbd";
    hspkgs.url =
      "github:podenv/hspkgs/c97192fe75d7f9df9fae97b0840cf0ed777e23ce";
  };

  outputs = { self, nixpkgs, hspkgs }:
    let
      legacy = import ./nix/default.nix {
        nixpkgsPath = nixpkgs;
        hspkgs = hspkgs.pkgs;
        self = self;
      };
    in {
      devShell."x86_64-linux" = legacy.shell;
      devShells."x86_64-linux".ci = legacy.ci-shell;
      packages."x86_64-linux".default = legacy.monocle-exe;
      packages."x86_64-linux".env = legacy.monocle-light.env;
      packages."x86_64-linux".containerBackend = legacy.containerBackend;
      packages."x86_64-linux".containerGrafana = legacy.containerGrafana;
      packages."x86_64-linux".containerPrometheus = legacy.containerPrometheus;
      apps."x86_64-linux".default = {
        type = "app";
        program = "${legacy.monocle-exe}/bin/monocle";
      };
    };
}
