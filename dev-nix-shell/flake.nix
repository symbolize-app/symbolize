{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { nixpkgs, flake-utils, rust-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          (import ./curl.nix)
          (import ./dasel.nix)
          (import ./dbmate.nix)
          (import ./easyrsa.nix)
          (import ./fd.nix)
          (import ./git.nix)
          (import ./haskell.nix)
          (import ./node.nix)
          (import ./overmind.nix)
          (import ./rust.nix { inherit rust-overlay; })
          (import ./sqlfluff.nix)
          (import ./sqlite.nix)
          (import ./task.nix)
          (import ./unzip.nix)
          (import ./watchman.nix)
          (import ./woff2.nix)
        ];
        pkgs = import nixpkgs { inherit system overlays; };
      in
        {
          devShells.default = pkgs.mkShell {
            packages = [
              pkgs.symbolize-curl
              pkgs.symbolize-dasel
              pkgs.symbolize-dbmate
              pkgs.symbolize-easyrsa
              pkgs.symbolize-fd
              pkgs.symbolize-git
              pkgs.symbolize-haskell
              pkgs.symbolize-node
              pkgs.symbolize-overmind
              pkgs.symbolize-rust
              pkgs.symbolize-sqlfluff
              pkgs.symbolize-sqlite
              pkgs.symbolize-task
              pkgs.symbolize-unzip
              pkgs.symbolize-watchman
              pkgs.symbolize-woff2
            ];

            shellHook = ''
              export PATH=${pkgs.symbolize-node}/lib/node_modules/.bin:$PATH
              cabal --version | head -n 1
              cargo --version
              curl --version | head -n 1
              dasel --version
              dbmate --version
              echo $(easyrsa --version | head -n 4)
              fd --version
              ghc --version
              git --version
              echo "gt $(gt --version)"
              echo "node $(node --version)"
              echo "pnpm v$(pnpm --version)"
              overmind --version
              rustc --version
              sqlfluff --version
              echo "sqlite3 v$(sqlite3 --version)"
              task --version
              echo $(unzip -v | head -n 1)
              echo "watchman $(watchman --version)"
            '';
          };
        }
    );
}
