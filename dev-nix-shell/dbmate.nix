final: prev:

{
  symbolize-dbmate = final.symlinkJoin {
    name = "symbolize-dbmate";
    paths = [
      final.dbmate
    ];
  };
}
 