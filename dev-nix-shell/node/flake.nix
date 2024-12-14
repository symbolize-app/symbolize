{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          (final: prev: {
            node-packages = prev.callPackage ./node-packages/default.nix {
              pkgs = pkgs;
              system = system;
              nodejs = pkgs.nodejs_22;
            };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; };
        node-loader = pkgs.writeShellScriptBin "node-loader" ''
          node --experimental-loader @symbolize/dev-node-loader/index.js $@
        '';
      in
        {
          packages.default = pkgs.buildEnv {
            name = "symbolize-node";
            buildInputs = [
              pkgs.nodejs_22
              pkgs.nodejs_22.pkgs.node2nix
              pkgs.node-packages.pnpm
              node-loader
            ];
            paths = [];
          };
        }
    );
}
