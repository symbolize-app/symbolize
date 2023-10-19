{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    curl.url = "path:./curl";
    dasel.url = "path:./dasel";
    dprint.url = "path:./dprint";
    fd.url = "path:./fd";
    haskell.url = "path:./haskell";
    node.url = "path:./node";
    rust.url = "path:./rust";
    task.url = "path:./task";
  };

  outputs = { nixpkgs, flake-utils, curl, dasel, dprint, fd, haskell, node, rust, task, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
        {
          devShells.default = pkgs.mkShell {
            inputsFrom = [
              curl.packages.${system}.default
              dasel.packages.${system}.default
              dprint.packages.${system}.default
              fd.packages.${system}.default
              haskell.packages.${system}.default
              node.packages.${system}.default
              rust.packages.${system}.default
              task.packages.${system}.default
            ];

            nativeBuildInputs = [
              pkgs.pkg-config
            ];

            shellHook = ''
              curl --version | head -n 1
              dasel --version
              dprint --version
              fd --version
              ghc --version
              echo "stack $(stack --version)"
              echo "node $(node --version)"
              echo "pnpm v$(pnpm --version)"
              rustc --version
              cargo --version
              task --version
            '';
          };
        }
    );
}
