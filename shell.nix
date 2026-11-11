{ pkgs, nixFormatter }:
let
  haskell = pkgs.callPackage ./haskell.nix { };
in
haskell.shellFor {
  packages = p: [ p.snail ];
  nativeBuildInputs = [
    haskell.cabal-install
    haskell.fourmolu
    haskell.ghcid
    haskell.haskell-language-server
    haskell.hlint
    haskell.hpack
    nixFormatter
  ];
  withHoogle = true;
}
