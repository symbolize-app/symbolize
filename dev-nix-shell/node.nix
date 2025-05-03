final: prev:

{
  node-packages = final.callPackage ./node-packages/default.nix {
    nodejs = final.nodejs_22;
  };

  node-loader = final.writeShellScriptBin "node-loader" ''
    node --experimental-loader @symbolize/dev-node-loader/index.js $@
  '';

  symbolize-node = final.symlinkJoin {
    name = "symbolize-node";
    paths = [
      final.nodejs_22
      final.nodejs_22.pkgs.node2nix
      final.node-packages."@withgraphite/graphite-cli"
      final.node-packages.pnpm
      final.node-loader
    ];
  };
}

