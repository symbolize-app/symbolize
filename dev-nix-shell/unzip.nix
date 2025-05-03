final: prev:

{
  symbolize-unzip = final.symlinkJoin {
    name = "symbolize-unzip";
    paths = [
      final.unzip
    ];
  };
}
