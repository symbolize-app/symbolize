final: prev:

{
  symbolize-haskell = final.symlinkJoin {
    name = "symbolize-haskell";
    paths = [
      final.zlib
      final.haskell.compiler.ghc98
      final.haskell.packages.ghc98.cabal-install
      final.haskell.packages.ghc98.haskell-language-server
      final.haskell.packages.ghc98.hlint
      final.haskell.packages.ghc98.hpack
      final.haskell.packages.ghc98.implicit-hie
      final.haskell.packages.ghc98.ormolu
    ];
  };
}
