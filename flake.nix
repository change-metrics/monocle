# Note: [Nix Flake Usage]:
#
# The flake location can be defined as:
#   `github:change-metrics/monocle`, or `path:/absolute/path`, or `.`
#
# Package output can be accessed using their #name:
#   nix show-derivation '.#containerMonocle'
#
# For non packages, the full qualified name can be used:
#   nix show derivation '.#devShells."x86_64-linux".ci'
#
# Inspect context using nix-tree, for example:
#   nix run github:utdemir/nix-tree -- --derivation .#devShell."x86_64-linux"
{
  description = "Monocle";
  nixConfig.bash-prompt = "[nix(monocle)] ";

  nixConfig = {
    extra-substituters = "https://change-metrics.cachix.org";
    extra-trusted-public-keys =
      "change-metrics.cachix.org-1:dCe8jx9vptiF6DCdZ5y2QouvDsxgFRZnbHowhPnS4C0=";
  };

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/d3780c92e64472e8f9aa54f7bbb0dd4483b98303";
  };

  outputs = { self, nixpkgs }:
    let
      legacy = import ./nix/default.nix {
        nixpkgsPath = nixpkgs;
        self = self;
      };
    in {
      haskellExtend = legacy.hExtend;
      devShell."x86_64-linux" = legacy.shell;
      devShells."x86_64-linux".hoogle-monocle = legacy.hoogle-monocle;
      devShells."x86_64-linux".hoogle = legacy.hoogle;
      devShells."x86_64-linux".ci = legacy.ci-shell;
      devShells."x86_64-linux".monitoring = legacy.monitoring-shell;
      packages."x86_64-linux".default = legacy.monocle-exe;
      packages."x86_64-linux".env = legacy.monocle-light.env;
      packages."x86_64-linux".containerMonocle = legacy.containerMonocle;
      apps."x86_64-linux".default = {
        type = "app";
        program = "${legacy.monocle-exe}/bin/monocle";
      };
      apps."x86_64-linux".prometheus = {
        type = "app";
        program = "${legacy.promStart}/bin/prometheus-start";
      };
      apps."x86_64-linux".grafana = {
        type = "app";
        program = "${legacy.grafanaStart}/bin/grafana-start";
      };
    };
}
