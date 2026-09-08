{ config, pkgs, ... }:

{
  env = {
    EASYRSA_PKI = "${config.devenv.root}/.pki";
    EASYRSA_NO_PASS = "1";

    NODE_OPTIONS_PRODUCTION = "--unhandled-rejections strict";
    NODE_OPTIONS = "--experimental-json-modules --unhandled-rejections strict --import ${config.devenv.root}/dev-node-loader/register.mjs";
    ESBUILD_BINARY_PATH = "${config.devenv.root}/build/vendor/esbuild/esbuild";
    BETTER_SQLITE3_BINDING = "${config.devenv.root}/build/vendor/better-sqlite3/better_sqlite3.node";
    SYMBOLIZE_CHROMIUM_EXECUTABLE = "${pkgs.chromium}/bin/chromium";

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
    unset NODE_PATH

    if [ -f "$DEVENV_ROOT/.env" ]; then
      set -a
      . "$DEVENV_ROOT/.env"
      set +a
    fi

    symbolize_clean_path=""
    old_ifs="$IFS"
    IFS=:
    for symbolize_path_entry in $PATH; do
      case "$symbolize_path_entry" in
        */symbolize-node/*|*-symbolize-node/*) ;;
        *)
          if [ -n "$symbolize_clean_path" ]; then
            symbolize_clean_path="$symbolize_clean_path:$symbolize_path_entry"
          else
            symbolize_clean_path="$symbolize_path_entry"
          fi
          ;;
      esac
    done
    IFS="$old_ifs"
    export PATH="${pkgs.symbolize-node}/bin:${pkgs.symbolize-node}/lib/node_modules/.bin:$symbolize_clean_path"

    echo "Run devenv-tool-versions to list development tool versions."
  '';
}
