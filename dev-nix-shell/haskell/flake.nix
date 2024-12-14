{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Align with GHC version from stack.yaml resolver
        hPkgs = pkgs.haskell.packages.ghc946;
      in
        {
          packages.default = pkgs.buildEnv {
            name = "symbolize-haskell";
            buildInputs = [
              pkgs.zlib
              pkgs.haskell.compiler.ghc946
              hPkgs.stack
            ];
            paths = [];
          };
        }
    );
}
