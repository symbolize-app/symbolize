final: prev:

{
  symbolize-node-packages = final.callPackage ./node-packages/default.nix {
    nodejs = final.nodejs_22;
  };

  symbolize-node = final.symlinkJoin {
    name = "symbolize-node";
    paths = [
      final.nodejs_22
      final.nodejs_22.pkgs.node2nix
      final.symbolize-node-packages."@withgraphite/graphite-cli-1.6.1"
      final.symbolize-node-packages."pnpm-10.10.0"
      final.symbolize-node-packages."puppeteer-25.9.0"
    ];
  };
}
