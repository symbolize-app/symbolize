{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    dprint.url = "path:./dprint";
    haskell.url = "path:./haskell";
    node.url = "path:./node";
    rust.url = "path:./rust";
  };

  outputs = { nixpkgs, flake-utils, dprint, haskell, node, rust, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
        {
          devShells.default = pkgs.mkShell {
            packages = [
              dprint.packages.${system}.default
              haskell.packages.${system}.default
              node.packages.${system}.default
              rust.packages.${system}.default
            ];

            shellHook = ''
              dprint --version
              ghc --version
              echo "stack $(stack --version)"
              echo "node $(node --version)"
              echo "pnpm v$(pnpm --version)"
              rustc --version
              cargo --version
            '';
          };
        }
    );
}
