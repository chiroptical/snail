{ pkgs }:
pkgs.haskell.packages.ghc910.extend (
  final: prev: {
    snail-bench = pkgs.haskell.lib.overrideCabal (final.callCabal2nix "snail-bench" ./benchmark { }) {
      # Keep the shared test fixtures accessible through benchmark/fixtures.
      src = ./.;
      postUnpack = ''sourceRoot="$sourceRoot/benchmark"'';
    };
    snail-example = final.callCabal2nix "snail-example" ./example { };
    snail = final.callCabal2nix "snail" ./. { };
  }
)
