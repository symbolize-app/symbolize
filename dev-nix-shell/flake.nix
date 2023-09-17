{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    dprint.url = "path:./dprint";
    node.url = "path:./node";
    rust.url = "path:./rust";
  };

  outputs = { nixpkgs, flake-utils, dprint, node, rust, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
        {
          devShells.default = pkgs.mkShell {
            packages = [
              dprint.packages.${system}.default
              node.packages.${system}.default
              rust.packages.${system}.default
            ];

            shellHook = ''
              dprint --version
              echo "node $(node --version)"
              echo "pnpm v$(pnpm --version)"
              rustc --version
              cargo --version
            '';
          };
        }
    );
}
