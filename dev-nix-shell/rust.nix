{ rust-overlay }: final: prev:

(import rust-overlay final prev)
//
{
  symbolize-rust = final.symlinkJoin {
    name = "symbolize-rust";
    paths = [
      final.openssl
      (final.rust-bin.nightly."2025-04-28".default.override {
        extensions = [
          "rust-src"
          "rust-analyzer"
        ];
      })
    ];
  };
}
