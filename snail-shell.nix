{pkgs, ...}: let
  haskell = pkgs.callPackage ./nix/haskell.nix {};
in
  haskell.callCabal2nix "snail-shell" ./. {}
