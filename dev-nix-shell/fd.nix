final: prev:

{
  symbolize-fd = final.symlinkJoin {
    name = "symbolize-fd";
    paths = [
      final.fd
    ];
  };
}
