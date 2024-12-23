{ }:
let
  nixpkgsSets = import ./.ci/nixpkgs.nix;
  inherit (nixpkgsSets) nixos1809 nixos2003 unstable;
  inherit (nixos2003) lib;
  inherit (nixos2003.haskell.lib) doJailbreak dontCheck;
  sharedOverrides = self: super: {
    monad-logger = self.callHackageDirect
      { pkg = "monad-logger";
        ver = "0.3.36";
        sha256 = "0ba1liqvmwjcyz3smp9fh2px1kvz8zzbwcafm0armhwazlys1qh1";
      } {};
  };
  ghcs = rec {
    ghc865 = nixos2003.haskell.packages.ghc865.override {
      overrides = sharedOverrides;
    };
    ghc884 = nixos2003.haskell.packages.ghc884.override {
      overrides = sharedOverrides;
    };
    ghc8102 = unstable.haskell.packages.ghc8102.override {
      overrides = sharedOverrides;
    };
  };
in
  lib.mapAttrs (_: ghc: ghc.callCabal2nix "monad-logger-extras" ./. {}) ghcs
