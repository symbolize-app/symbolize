{ config, pkgs, ... }:

{
  env = {
    EASYRSA_PKI = "${config.devenv.root}/.pki";
    EASYRSA_NO_PASS = "1";

    NODE_OPTIONS_PRODUCTION = "--unhandled-rejections strict";
    NODE_OPTIONS = "--require ${config.devenv.root}/dev-node-suppress/index.cjs --experimental-json-modules --unhandled-rejections strict";

    DATABASE_URL = "sqlite:svc-gateway-host-store/build/manifest.sqlite3";
    DBMATE_MIGRATIONS_DIR = "svc-gateway-host-store/migrate";
    DBMATE_SCHEMA_FILE = "svc-gateway-host-store/schema.sql";
    DBMATE_MIGRATIONS_TABLE = "migration";

    CARGO_HOME = "${config.devenv.root}/.cargo";
    CARGO_BUILD_TARGET_DIR = "build/target";
    RUST_BACKTRACE = "1";

    LC_ALL = "C.UTF-8";

    OVERMIND_SOCKET = "build/overmind.socket";

    TASK_CARGO_BUILD_MODE = "release";
    TASK_GEN_MODE = "release";
    TASK_WATCHMAN_CLIENT_MODE = "release";
  };

  scripts."devenv-tool-versions" = {
    exec = ./tool-versions.sh;
    description = "Print the versions of the development tools";
  };

  enterShell = ''
    if [ -f "$DEVENV_ROOT/.env" ]; then
      set -a
      . "$DEVENV_ROOT/.env"
      set +a
    fi

    export PATH="${config.devenv.root}/node_modules/.bin:${pkgs.symbolize-node}/lib/node_modules/.bin:$PATH"
    export PATH="$CARGO_HOME/bin:$PATH"

    task --silent tmpfs:link-build-dirs

    echo "Run devenv-tool-versions to list development tool versions."
  '';
}
