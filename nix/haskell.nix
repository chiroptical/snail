{pkgs, ...}:
let
  haskell-text = pkgs.callPackage ./haskell-text.nix {};
in
  pkgs.haskell.packages.ghc924.extend (final: prev: {
    "text" = final.callCabal2nix "text" haskell-text {};
  })
