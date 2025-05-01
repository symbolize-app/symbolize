final: prev:

{
  symbolize-watchman = final.symlinkJoin {
    name = "symbolize-watchman";
    paths = [
      final.watchman
    ];
  };
}
