{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/d1c3fea7ecbed758168787fe4e4a3157e52bc808";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
        {
          devShells.default = pkgs.mkShell {
            buildInputs = [
              pkgs.nodejs-17_x
              pkgs.nodejs-17_x.pkgs.pnpm
              (pkgs.linkFarm "pnpm" [ { name = "bin/pnpm"; path = "${pkgs.nodejs-17_x.pkgs.pnpm}/lib/node_modules/pnpm/bin/pnpm.cjs"; } ] )
            ];

            shellHook = ''
              echo "node $(node --version)"
              echo "pnpm v$(pnpm --version)"
            '';
          };
        }
    );
}
