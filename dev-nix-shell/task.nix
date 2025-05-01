final: prev:

{
  symbolize-task = final.symlinkJoin {
    name = "symbolize-task";
    paths = [
      final.go-task
    ];
  };
}
