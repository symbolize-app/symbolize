final: prev:

{
  symbolize-dasel = final.symlinkJoin {
    name = "symbolize-dasel";
    paths = [
      final.dasel
    ];
  };
}
 