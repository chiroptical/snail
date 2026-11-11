{ pkgs }:
pkgs.haskell.packages.ghc910.extend (
  final: prev: {
    snail = final.callCabal2nix "snail" ./. { };
  }
)
