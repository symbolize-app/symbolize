final: prev:

{
  symbolize-git = final.symlinkJoin {
    name = "symbolize-git";
    paths = [
      final.git
    ];
  };
}
