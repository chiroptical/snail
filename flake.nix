{
  description = "snail-shell";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    # self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux" "aarch64-darwin"] (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowBroken = true;
      };
      snail-shell = pkgs.callPackage ./snail-shell.nix {};
    in {
      devShell = import ./shell.nix {
        inherit pkgs;
      };
      defaultPackage = snail-shell;
      packages = flake-utils.lib.flattenTree {
       inherit snail-shell;
      };
    });
}
