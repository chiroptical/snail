{ pkgs }:
let
  haskell = pkgs.callPackage ./haskell.nix { };
in
haskell.snail
