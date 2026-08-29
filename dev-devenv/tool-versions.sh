#!/usr/bin/env bash

cabal --version | head -n 1
cargo --version
curl --version | head -n 1
dasel --version
dbmate --version
echo $(easyrsa --version | head -n 4)
fd --version
ghc --version
git --version
echo "gt $(gt --version)"
echo "node $(node --version)"
echo "pnpm v$(pnpm --version)"
overmind --version
rustc --version
sqlfluff --version
echo "sqlite3 v$(sqlite3 --version)"
task --version
echo $(unzip -v | head -n 1)
echo "watchman $(watchman --version)"
