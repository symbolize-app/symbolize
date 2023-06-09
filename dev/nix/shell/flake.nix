{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-node.url = "github:NixOS/nixpkgs/d1c3fea7ecbed758168787fe4e4a3157e52bc808";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { nixpkgs, nixpkgs-node, flake-utils, rust-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };
        pkgs-node = import nixpkgs-node { inherit system; };
      in
        {
          devShells.default = pkgs.mkShell {
            buildInputs = [
              pkgs-node.nodejs-17_x
              pkgs-node.nodejs-17_x.pkgs.pnpm
              (pkgs-node.linkFarm "pnpm" [ { name = "bin/pnpm"; path = "${pkgs-node.nodejs-17_x.pkgs.pnpm}/lib/node_modules/pnpm/bin/pnpm.cjs"; } ] )
              pkgs.rust-bin.stable."1.57.0".default
            ];

            shellHook = ''
              echo "node $(node --version)"
              echo "pnpm v$(pnpm --version)"
              rustc --version
              cargo --version
            '';
          };
        }
    );
}
