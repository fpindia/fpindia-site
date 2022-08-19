{
  description = "Ema template app";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";

    # Haskell overrides
    ema.url = "github:srid/ema/0.8.0.0";
    ema.flake = false;
    tailwind-haskell.url = "github:srid/tailwind-haskell/master";
    tailwind-haskell.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { self', inputs', pkgs, ... }: {
        # "haskellProjects" comes from https://github.com/srid/haskell-flake
        haskellProjects.default = {
          root = ./.;
          buildTools = hp: {
            inherit (pkgs)
              treefmt
              nixpkgs-fmt
              foreman
              git;
            inherit (hp)
              cabal-fmt
              fourmolu;
            inherit (inputs'.tailwind-haskell.packages)
              tailwind;
          };
          source-overrides = {
            inherit (inputs)
              ema;
          };
          overrides = self: super: with pkgs.haskell.lib; {
            ema = dontCheck super.ema;
            inherit (inputs'.tailwind-haskell.packages)
              tailwind;
          };
        };
        apps.tailwind-run.program = "${inputs'.tailwind-haskell.packages.tailwind}/bin/tailwind-run";
      };
    };
}
