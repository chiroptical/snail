{
  pkgs,
  ...
}:
pkgs.mkShell {
  inputsFrom = [
    (import ./snail-shell.nix pkgs).env
  ];
  buildInputs = with pkgs; [
    haskell-language-server
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.hlint
    haskellPackages.hpack
    haskellPackages.fourmolu
    alejandra
  ];
  withHoogle = true;
  # required for 'make test' hedgehog output
  LANG = "en_US.utf8";
}
