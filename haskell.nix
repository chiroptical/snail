{ pkgs }:
pkgs.haskell.packages.ghc910.extend (
  final: prev: {
    snail-example = final.callCabal2nix "snail-example" ./example { };
    snail = final.callCabal2nix "snail" ./. { };
  }
)
