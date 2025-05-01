final: prev:

{
  symbolize-curl = final.symlinkJoin {
    name = "symbolize-curl";
    paths = [
      final.curl
    ];
  };
}
 