{
  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/ed014c27f4d0ca772fb57d3b8985b772b0503bbd";
  };

  outputs = { self, nixpkgs }:
    let legacy = import ../nix/default.nix { nixpkgsPath = nixpkgs; };
    in { devShell."x86_64-linux" = legacy.shell; };
}
