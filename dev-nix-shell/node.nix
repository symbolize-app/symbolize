final: prev:

{
  symbolize-node-packages = final.callPackage ./node-packages/default.nix {
    nodejs = final.nodejs_22;
  };

  symbolize-node-loader = final.writeShellScriptBin "node-loader" ''
    node --experimental-loader @symbolize/dev-node-loader/index.js "$@"
  '';

  symbolize-node = final.symlinkJoin {
    name = "symbolize-node";
    paths = [
      final.nodejs_22
      final.nodejs_22.pkgs.node2nix
      final.symbolize-node-packages."@withgraphite/graphite-cli"
      final.symbolize-node-packages.pnpm
      final.symbolize-node-loader
    ];
  };
}

