final: prev:

{
  symbolize-easyrsa = final.symlinkJoin {
    name = "symbolize-easyrsa";
    paths = [
      final.easyrsa
    ];
  };
}
 