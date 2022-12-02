{
  description = "Ema template app";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = { self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { self', config, inputs', pkgs, lib, ... }: {
        # "haskellProjects" comes from https://github.com/srid/haskell-flake
        haskellProjects.default = {
          packages.fpindia-site.root = ./.;
          buildTools = hp: {
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
          overrides = self: super: { };
        };
        packages.default = config.packages.fpindia-site;
        apps.tailwind-run.program = "${lib.getExe pkgs.haskellPackages.tailwind}";
      };
    };
}
