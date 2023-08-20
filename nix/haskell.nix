{pkgs, ...}:
# let
#   haskell-text = pkgs.callPackage ./haskell-text.nix {};
#in
  pkgs.haskell.packages.ghc94.extend (final: prev: {
  })
