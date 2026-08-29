{ pkgs, ... }:

{
  packages = with pkgs; [
    curl
    dasel
    dbmate
    easyrsa
    fd
    git
    symbolize-haskell
    symbolize-node
    overmind
    symbolize-rust
    sqlfluff
    sqlite-interactive
    go-task
    unzip
    watchman
    woff2
  ];
}
