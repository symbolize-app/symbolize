final: prev:

{
  symbolize-sqlfluff = final.symlinkJoin {
    name = "symbolize-sqlfluff";
    paths = [
      final.sqlfluff
    ];
  };
}
