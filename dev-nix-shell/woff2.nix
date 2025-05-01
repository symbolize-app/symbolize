final: prev:

{
  symbolize-woff2 = final.symlinkJoin {
    name = "symbolize-woff2";
    paths = [
      final.woff2
    ];
  };
}
