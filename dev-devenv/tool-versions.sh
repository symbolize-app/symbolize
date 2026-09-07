#!/usr/bin/env bash

cabal --version | head -n 1
echo "cargo-deny $(cargo-deny --version)"
echo "chromium $(chromium --version)"
cargo --version
curl --version | head -n 1
dbmate --version
echo $(easyrsa --version | head -n 4)
fd --version
gleam --version
ghc --version
git --version
echo "node $(node --version)"
echo "eslint $(eslint --version)"
echo "prettier $(prettier --version)"
overmind --version
rustc --version
sqlfluff --version
echo "sqlite3 v$(sqlite3 --version)"
task --version
echo $(unzip -v | head -n 1)
echo "watchman $(watchman --version)"
