final: prev:

{
  symbolize-overmind = final.symlinkJoin {
    name = "symbolize-overmind";
    paths = [
      final.overmind
    ];
  };
}
