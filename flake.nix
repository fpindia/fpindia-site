# This is a modular Nix flake
# Learn about it here: https://zero-to-flakes.com/
{
  description = "Ema template app";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { self', inputs', pkgs, lib, ... }: {
        # "haskellProjects" comes from https://github.com/srid/haskell-flake
        haskellProjects.default = {
          devShell.tools = hp: {
            inherit (pkgs)
              treefmt
              nixpkgs-fmt
              foreman
              git;
            inherit (hp)
              cabal-fmt
              fourmolu
              tailwind;
          };
          # Want to override Haskell dependencies?
          # See https://zero-to-flakes.com/haskell-flake/dependency
        };
        packages.default = self'.packages.fpindia-site;
        apps.tailwind-run.program = "${lib.getExe pkgs.haskellPackages.tailwind}";
      };
    };
}
