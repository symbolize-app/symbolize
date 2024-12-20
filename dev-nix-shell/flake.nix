{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    curl.url = "path:./curl";
    dasel.url = "path:./dasel";
    dbmate.url = "path:./dbmate";
    easyrsa.url = "path:./easyrsa";
    fd.url = "path:./fd";
    git.url = "path:./git";
    haskell.url = "path:./haskell";
    node.url = "path:./node";
    overmind.url = "path:./overmind";
    rust.url = "path:./rust";
    sqlfluff.url = "path:./sqlfluff";
    sqlite.url = "path:./sqlite";
    task.url = "path:./task";
    unzip.url = "path:./unzip";
    watchman.url = "path:./watchman";
    woff2.url = "path:./woff2";
  };

  outputs = { nixpkgs, flake-utils, curl, dasel, dbmate, easyrsa, fd, git, haskell, node, overmind, rust, sqlfluff, sqlite, task, unzip, watchman, woff2, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
        {
          devShells.default = pkgs.mkShell {
            inputsFrom = [
              curl.packages.${system}.default
              dasel.packages.${system}.default
              easyrsa.packages.${system}.default
              fd.packages.${system}.default
              git.packages.${system}.default
              haskell.packages.${system}.default
              node.packages.${system}.default
              overmind.packages.${system}.default
              rust.packages.${system}.default
              sqlfluff.packages.${system}.default
              sqlite.packages.${system}.default
              task.packages.${system}.default
              unzip.packages.${system}.default
              watchman.packages.${system}.default
              woff2.packages.${system}.default
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
              echo $(easyrsa --version | head -n 4)
              fd --version
              ghc --version
              git --version
              echo "gt $(gt --version)"
              echo "stack $(stack --version)"
              echo "node $(node --version)"
              echo "pnpm v$(pnpm --version)"
              overmind --version
              rustc --version
              cargo --version
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
