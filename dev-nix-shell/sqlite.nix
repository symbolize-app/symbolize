final: prev:

{
  symbolize-sqlite = final.symlinkJoin {
    name = "symbolize-sqlite";
    paths = [
      final.sqlite-interactive
    ];
  };
}
