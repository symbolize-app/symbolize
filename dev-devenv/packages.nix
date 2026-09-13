{ inputs, pkgs, ... }:

{
  packages = with pkgs; [
    buck2
    curl
    chromium
    dbmate
    easyrsa
    fd
    gcc
    gnumake
    go
    git
    nodePackages.node-gyp
    perl
    pkg-config
    python3
    tcl
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
    inputs.nixpkgs-gleam.legacyPackages.${pkgs.system}.gleam
  ];
}
