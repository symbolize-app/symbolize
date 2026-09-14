load(":gleam.bzl", "GleamPackageInfo")

def _esbuild_manifest_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output("manifest.sqlite3")

    dev_esbuild_info = ctx.attrs.dev_esbuild[GleamPackageInfo]
    runner = ctx.actions.write("run.mjs", "import { main } from './symbolize_dev_esbuild/dev_esbuild_main.mjs'; main();\n")
    tool_transitive = dict(dev_esbuild_info.transitive_deps)
    tool_transitive[dev_esbuild_info.package_name] = dev_esbuild_info.output_dir
    tool_transitive["prelude.mjs"] = ctx.attrs.prelude[DefaultInfo].default_outputs[0]
    tool_transitive["run.mjs"] = runner
    tool_dir = ctx.actions.symlinked_dir("__tool", tool_transitive)
    main_script = cmd_args(tool_dir, "/run.mjs", delimiter = "")

    guest_info = ctx.attrs.guest[GleamPackageInfo]
    guest_transitive = dict(guest_info.transitive_deps)
    guest_transitive[guest_info.package_name] = guest_info.output_dir

    schema_file = ctx.attrs.schema[DefaultInfo].default_outputs[0]

    cmd = cmd_args(
        "node",
        "--preserve-symlinks",
        "--preserve-symlinks-main",
        main_script,
        "--mode",
        ctx.attrs.mode,
        "--database",
        out.as_output(),
        "--schema",
        schema_file,
        "--migrations",
        ctx.attrs.migrations_dir,
        "--query",
        ctx.attrs.query_dir,
        "--guest-dir",
        ctx.attrs.guest_dir,
        "--outbase",
        ".",
        "--esbuild-bin",
        ctx.attrs.esbuild_bin[DefaultInfo].default_outputs[0],
        "--better-sqlite3-binding",
        ctx.attrs.better_sqlite3[DefaultInfo].default_outputs[0],
    )

    for pkg_name, pkg_out in guest_transitive.items():
        cmd.add("--package", cmd_args(pkg_name, "=", pkg_out, delimiter = ""))

    cmd.add("--package", cmd_args("prelude=", ctx.attrs.prelude[DefaultInfo].default_outputs[0], delimiter = ""))

    guest_out = guest_info.output_dir
    cmd.add("--classic-entry", cmd_args(guest_out, "/svc_gateway_guest_run_service_worker_main.mjs", delimiter = ""))
    cmd.add("--module-entry", cmd_args(guest_out, "/svc_gateway_guest_run_dedicated_worker_main.mjs", delimiter = ""))
    cmd.add("--module-entry", cmd_args(guest_out, "/svc_gateway_guest_run_main.mjs", delimiter = ""))
    cmd.add("--module-entry", cmd_args(guest_out, "/svc_gateway_guest_run_register.mjs", delimiter = ""))
    if ctx.attrs.mode == "development":
        cmd.add("--module-entry", cmd_args(guest_out, "/svc_gateway_guest_run_development.mjs", delimiter = ""))

    for asset in ctx.attrs.copy_entries:
        cmd.add("--copy-entry", asset)

    cmd.add(cmd_args(hidden = ctx.attrs.assets))
    cmd.add(cmd_args(hidden = [dep[DefaultInfo].default_outputs[0] for dep in ctx.attrs.migration_deps]))
    cmd.add(cmd_args(hidden = [dep[DefaultInfo].default_outputs[0] for dep in ctx.attrs.query_deps]))

    ctx.actions.run(
        cmd,
        category = "dev_esbuild",
        identifier = ctx.attrs.mode,
    )

    return [
        DefaultInfo(default_output = out),
    ]

_esbuild_manifest = rule(
    impl = _esbuild_manifest_impl,
    attrs = {
        "assets": attrs.list(attrs.source(), default = []),
        "better_sqlite3": attrs.dep(default = "vendor//better-sqlite3-11.1.2:better-sqlite3"),
        "copy_entries": attrs.list(attrs.source(), default = []),
        "dev_esbuild": attrs.dep(default = "//dev-esbuild:dev-esbuild"),
        "esbuild_bin": attrs.dep(default = "vendor//esbuild-0.19.5:esbuild"),
        "guest": attrs.dep(default = "//svc-gateway-guest-run:svc-gateway-guest-run"),
        "guest_dir": attrs.string(default = "svc-gateway-guest-run"),
        "migration_deps": attrs.list(attrs.dep(), default = []),
        "migrations_dir": attrs.string(default = "svc-gateway-host-store/migrate"),
        "mode": attrs.string(default = "development"),
        "prelude": attrs.dep(default = "dev_buck//:prelude"),
        "query_deps": attrs.list(attrs.dep(), default = []),
        "query_dir": attrs.string(default = "dev-esbuild/query"),
        "schema": attrs.dep(default = "//svc-gateway-host-store:schema.sql"),
    },
)

_DEFAULT_MIGRATIONS = [
    "//svc-gateway-host-store:migrate/20231203011147_pragma_wal.sql",
    "//svc-gateway-host-store:migrate/20231203011148_create_version_table.sql",
    "//svc-gateway-host-store:migrate/20231204175648_create_content_table.sql",
    "//svc-gateway-host-store:migrate/20231204175649_create_path_table.sql",
]

_DEFAULT_QUERIES = [
    "//dev-esbuild:query/begin_transaction.sql",
    "//dev-esbuild:query/commit_transaction.sql",
    "//dev-esbuild:query/insert_path.sql",
    "//dev-esbuild:query/insert_version.sql",
    "//dev-esbuild:query/pragma_foreign_key.sql",
    "//dev-esbuild:query/pragma_wal_autocheckpoint.sql",
    "//dev-esbuild:query/pragma_wal_checkpoint.sql",
    "//dev-esbuild:query/rollback_transaction.sql",
    "//dev-esbuild:query/update_content_compressed.sql",
    "//dev-esbuild:query/upsert_content.sql",
]

def esbuild_manifest(
    name,
    mode = "development",
    guest = ":svc-gateway-guest-run",
    dev_esbuild = "//dev-esbuild:dev-esbuild",
    esbuild_bin = "vendor//esbuild-0.19.5:esbuild",
    better_sqlite3 = "vendor//better-sqlite3-11.1.2:better-sqlite3",
    schema = "//svc-gateway-host-store:schema.sql",
    migration_deps = _DEFAULT_MIGRATIONS,
    query_deps = _DEFAULT_QUERIES,
    visibility = ["PUBLIC"],
):
    copy_entries = [
        ".font/literata-italic.woff2",
        ".font/literata.woff2",
        "init.html",
    ]
    assets = native.glob([
        ".font/*",
        "*.html",
        "*.css",
        "*.txt",
    ])

    _esbuild_manifest(
        name = name,
        mode = mode,
        guest = guest,
        dev_esbuild = dev_esbuild,
        esbuild_bin = esbuild_bin,
        better_sqlite3 = better_sqlite3,
        schema = schema,
        copy_entries = copy_entries,
        assets = assets,
        migration_deps = migration_deps,
        query_deps = query_deps,
        visibility = visibility,
    )
