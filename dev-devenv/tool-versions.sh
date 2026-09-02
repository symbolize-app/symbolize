#!/usr/bin/env bash

cabal --version | head -n 1
echo "chromium $(chromium --version)"
cargo --version
curl --version | head -n 1
dbmate --version
echo $(easyrsa --version | head -n 4)
fd --version
gleam --version
ghc --version
git --version
echo "gt $(gt --version)"
echo "node $(node --version)"
echo "pnpm v$(pnpm --version)"
echo "puppeteer $(node -e 'console.log(require(require.resolve("puppeteer/package.json")).version)')"
overmind --version
rustc --version
sqlfluff --version
echo "sqlite3 v$(sqlite3 --version)"
task --version
echo $(unzip -v | head -n 1)
echo "watchman $(watchman --version)"
