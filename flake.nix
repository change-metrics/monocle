{
  description = "Monocle";
  nixConfig.bash-prompt = "[nix(monocle)] ";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/ed014c27f4d0ca772fb57d3b8985b772b0503bbd";
  };

  outputs = { self, nixpkgs }:
    let
      legacy = import ./nix/default.nix {
        nixpkgsPath = nixpkgs;
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
