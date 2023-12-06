{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    curl.url = "path:./curl";
    dasel.url = "path:./dasel";
    dbmate.url = "path:./dbmate";
    fd.url = "path:./fd";
    haskell.url = "path:./haskell";
    node.url = "path:./node";
    rust.url = "path:./rust";
    sqlfluff.url = "path:./sqlfluff";
    sqlite.url = "path:./sqlite";
    task.url = "path:./task";
  };

  outputs = { nixpkgs, flake-utils, curl, dasel, dbmate, fd, haskell, node, rust, sqlfluff, sqlite, task, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
        {
          devShells.default = pkgs.mkShell {
            inputsFrom = [
              curl.packages.${system}.default
              dasel.packages.${system}.default
              fd.packages.${system}.default
              haskell.packages.${system}.default
              node.packages.${system}.default
              rust.packages.${system}.default
              sqlfluff.packages.${system}.default
              sqlite.packages.${system}.default
              task.packages.${system}.default
            ];

            nativeBuildInputs = [
              pkgs.pkg-config
            ];
            
            packages = [
              dbmate.packages.${system}.default
            ];

            shellHook = ''
              curl --version | head -n 1
              dasel --version
              dbmate --version
              fd --version
              ghc --version
              echo "stack $(stack --version)"
              echo "node $(node --version)"
              echo "pnpm v$(pnpm --version)"
              rustc --version
              cargo --version
              sqlfluff --version
              echo "sqlite3 v$(sqlite3 --version)"
              task --version
            '';
          };
        }
    );
}
