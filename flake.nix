{
  description = "An S-expression parser and abstract syntax tree";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs =
    { self, nixpkgs }:
    let
      supportedSystems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixFormatter =
        pkgs:
        pkgs.nixfmt-tree.override {
          settings.tree-root-file = "flake.nix";
        };
    in
    {
      devShells = forAllSystems (system: {
        default = nixpkgs.legacyPackages.${system}.callPackage ./shell.nix {
          nixFormatter = self.formatter.${system};
        };
      });

      formatter = forAllSystems (system: nixFormatter nixpkgs.legacyPackages.${system});

      packages = forAllSystems (
        system:
        let
          snail = import ./snail.nix { pkgs = nixpkgs.legacyPackages.${system}; };
        in
        {
          inherit snail;
          default = snail;
        }
      );

      apps = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          haskell = pkgs.callPackage ./haskell.nix { };
          example = {
            type = "app";
            program = "${haskell.snail-example}/bin/snail";
          };
          optimize = pkg: pkgs.haskell.lib.appendConfigureFlag pkg "--enable-optimization=2";
          benchmark = optimize (haskell.snail-bench.override { snail = optimize haskell.snail; });
        in
        {
          default = example;
          snail = example;
          benchmark = {
            type = "app";
            program = "${benchmark}/bin/snail-bench";
          };
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          test = pkgs.haskell.lib.doCheck self.packages.${system}.snail;
          format =
            pkgs.runCommand "snail-format"
              {
                nativeBuildInputs = [
                  pkgs.findutils
                  pkgs.gnumake
                  pkgs.haskellPackages.fourmolu
                  self.formatter.${system}
                ];
              }
              ''
                cp -R ${self} source
                chmod -R u+w source
                cd source
                make format-check
                touch "$out"
              '';
        }
      );
    };
}
