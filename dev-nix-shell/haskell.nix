final: prev:

# Align with stack.yaml
{
  symbolize-haskell = final.symlinkJoin {
    name = "symbolize-haskell";
    paths = [
      final.zlib
      final.haskell.compiler.ghc946
      final.haskell.packages.ghc946.stack
    ];
  };
}
