watchman__server: watchman --unix-listener-path="$(realpath build/watchman-unix-listener)" --pidfile="$(realpath build/watchman-pid)" --statefile="$(realpath build/watchman-state)" --foreground
rust__build__clippy: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --mode rust -- bash -c 'buck2 build -m debug //dev-sim //dev-watchman-client //svc-gateway-host-run //svc-search-host-read //dev-sim:test //dev-watchman-client:test //lib-hex-rs:test //svc-gateway-host-run:test //svc-search-host-read:test && buck2 bxl -m debug dev_buck//clippy.bxl:check'
node__esbuild__build: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --mode javascript -- buck2 build -m debug //svc-gateway-host-run
node__test: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --mode gleam -- bash -c 'buck2 test -m debug $(buck2 uquery "kind(\"gleam_test\", //...)")'
node__eslint__lint__check: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --mode javascript -- eslint --config dev-eslint/index.json --cache --cache-location build/eslint-cache --ignore-path .gitignore --max-warnings 0 --ext js,cjs,mjs
haskell__build: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --mode haskell -- buck2 build -m debug //dev-gen:symbolize-dev-gen-exe //dev-gen:symbolize-dev-gen-test
haskell__lint: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --mode haskell -- buck2 build //dev-gen:lint
dev-gen__test: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --target //dev-gen:test




dev-sim__test: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --target //dev-sim:test
dev-watchman-client__test: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --target //dev-watchman-client:test
lib-hex-rs__test: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --target //lib-hex-rs:test
svc-gateway-host-run__test: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --target //svc-gateway-host-run:test
svc-gateway-host-run__run: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --target //svc-gateway-host-run --restart
svc-search-host-read__test: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --target //svc-search-host-read:test
svc-search-host-read__run: buck2 run -m $TASK_WATCHMAN_CLIENT_MODE //dev-watchman-client -- --target //svc-search-host-read --restart
