{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { nixpkgs, flake-utils, rust-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };
      in
        {
          packages.default = pkgs.buildEnv {
            name = "symbolize-rust";
            buildInputs = [
              pkgs.openssl
              (pkgs.rust-bin.nightly."2023-12-09".default.override {
                extensions = [
                  "rust-src"
                  "rust-analyzer"
                ];
              })
            ];
            paths = [];
          };
        }
    );
}
