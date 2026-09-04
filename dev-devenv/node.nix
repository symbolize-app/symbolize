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
      final.symbolize-node-packages."eslint-8.56.0"
      final.symbolize-node-packages."prettier-3.2.5"
    ];
  };
}
