load("@prelude//rules.bzl", _alias = "alias")
load(":format.bzl", _format_check = "format_check", _gleam_format_diff = "gleam_format_diff")

GleamPackageInfo = provider(fields = [

    "package_name",
    "output_dir",
    "transitive_deps",
])

def _gleam_package_impl(ctx: AnalysisContext) -> list[Provider]:
    out_dir = ctx.actions.declare_output(ctx.attrs.package_name)

    transitive_deps = {}
    for dep in ctx.attrs.deps:
        if GleamPackageInfo in dep:
            info = dep[GleamPackageInfo]
            transitive_deps[info.package_name] = info.output_dir
            for k, v in info.transitive_deps.items():
                transitive_deps[k] = v

    lib_dir = ctx.actions.symlinked_dir("__lib", transitive_deps)

    pkg_dir = ctx.attrs.package_dir
    if not pkg_dir:
        if ctx.label.cell == "root":
            pkg_dir = ctx.label.package
        else:
            pkg_dir = ctx.label.cell + "/" + ctx.label.package

    warnings_out = ctx.actions.declare_output("warnings.log")

    wrapper = ctx.actions.declare_output("gleam_compile_wrapper.sh")
    wrapper_lines = [
        "#!/usr/bin/env bash",
        "set -euo pipefail",
        'WARNINGS_OUT="$1"',
        "shift",
        "set +e",
        '"$@" 2> "$WARNINGS_OUT"',
        "status=$?",
        'if [ -s "$WARNINGS_OUT" ]; then',
        '  cat "$WARNINGS_OUT" >&2',
        "fi",
        "exit $status",
    ]
    ctx.actions.write(wrapper, "\n".join(wrapper_lines) + "\n", is_executable = True)

    cmd = cmd_args(
        wrapper,
        warnings_out.as_output(),
        "gleam",
        "compile-package",
        "--target=javascript",
        "--package",
        pkg_dir,
        "--out",
        out_dir.as_output(),
        "--lib",
        lib_dir,
        "--javascript-prelude",
        ctx.attrs.javascript_prelude,
        hidden = ctx.attrs.srcs,
    )

    ctx.actions.run(
        cmd,
        category = "gleam_compile",
        identifier = ctx.attrs.package_name,
    )

    all_transitive = dict(transitive_deps)
    all_transitive[ctx.attrs.package_name] = out_dir

    return [
        DefaultInfo(
            default_output = out_dir,
            sub_targets = {
                "warnings": [DefaultInfo(default_output = warnings_out)],
            },
        ),
        GleamPackageInfo(
            package_name = ctx.attrs.package_name,
            output_dir = out_dir,
            transitive_deps = all_transitive,
        ),
    ]

_gleam_package = rule(
    impl = _gleam_package_impl,
    attrs = {
        "deps": attrs.list(attrs.dep(), default = []),
        "javascript_prelude": attrs.string(default = "../prelude.mjs"),
        "package_dir": attrs.string(default = ""),
        "package_name": attrs.string(),
        "srcs": attrs.list(attrs.source(), default = []),
    },
)

def _gleam_lint_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output("lint.ok")
    warnings_file = ctx.attrs.package[DefaultInfo].sub_targets["warnings"][DefaultInfo].default_outputs[0]

    script = ctx.actions.declare_output("check_gleam_warnings.sh")
    script_lines = [
        "#!/usr/bin/env bash",
        "set -euo pipefail",
        'WARNINGS="$1"',
        'OUT="$2"',
        'if [ -s "$WARNINGS" ]; then',
        '  if grep -q "warning:" "$WARNINGS"; then',
        '    echo "Gleam compiler warning(s) detected:" >&2',
        '    cat "$WARNINGS" >&2',
        '    exit 1',
        '  fi',
        'fi',
        'touch "$OUT"',
    ]
    ctx.actions.write(script, "\n".join(script_lines) + "\n", is_executable = True)

    cmd = cmd_args([script, warnings_file, out.as_output()])
    ctx.actions.run(cmd, category = "gleam_lint")

    return [DefaultInfo(default_output = out)]

gleam_lint = rule(
    impl = _gleam_lint_impl,
    attrs = {
        "package": attrs.dep(),
    },
)


def gleam_package(
    name,
    package_name = None,
    srcs = None,
    deps = [],
    package_dir = "",
    javascript_prelude = "../prelude.mjs",
    test = True,
    test_module = None,
    visibility = ["PUBLIC"],
):
    actual_pkg_name = package_name or name.replace("-", "_")
    actual_srcs = srcs
    if actual_srcs == None:
        actual_srcs = native.glob([
            "src/**/*.gleam",
            "src/**/*.mjs",
            "test/**/*.gleam",
            "test/**/*.mjs",
            "gleam.toml",
        ])

    _gleam_package(
        name = name,
        package_name = actual_pkg_name,
        package_dir = package_dir,
        srcs = actual_srcs,
        deps = deps,
        javascript_prelude = javascript_prelude,
        visibility = visibility,
    )

    if test and not native.rule_exists("test"):
        resolved_module = test_module
        if resolved_module == None:
            test_mains = [
                f[4:-6]
                for f in native.glob(["src/*test_main.gleam"])
                if "browser" not in f
            ]
            if len(test_mains) == 1:
                resolved_module = test_mains[0]
            elif len(test_mains) > 1:
                primary = [m for m in test_mains if m.endswith("_test_main") and not m.endswith("_data_test_main")]
                if primary:
                    resolved_module = primary[0]
                else:
                    resolved_module = test_mains[0]

        if resolved_module:
            gleam_test(
                name = "test",
                package = ":" + name,
                module = resolved_module,
                visibility = visibility,
            )

    gleam_srcs = native.glob(["src/**/*.gleam", "test/**/*.gleam"])

    if not native.rule_exists("format-diff"):
        _gleam_format_diff(
            name = "format-diff",
            srcs = gleam_srcs,
            visibility = visibility,
        )

    if not native.rule_exists("format-check"):
        _format_check(
            name = "format-check",
            diff = ":format-diff",
            visibility = visibility,
        )

    if not native.rule_exists("lint"):
        gleam_lint(
            name = "lint",
            package = ":" + name,
            visibility = visibility,
        )

    if not native.rule_exists("clippy"):
        _alias(
            name = "clippy",
            actual = ":lint",
            visibility = visibility,
        )

    if not native.rule_exists("check"):
        _alias(
            name = "check",
            actual = ":" + name,
            visibility = visibility,
        )



def _gleam_prelude_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output("prelude.mjs")
    ctx.actions.run(
        cmd_args(
            "sh",
            "-c",
            cmd_args("gleam export javascript-prelude > \"$1\"", "--", out.as_output()),
        ),
        category = "gleam_prelude",
    )
    return [DefaultInfo(default_output = out)]

gleam_prelude = rule(
    impl = _gleam_prelude_impl,
    attrs = {},
)

def _gleam_test_impl(ctx: AnalysisContext) -> list[Provider]:
    pkg_info = ctx.attrs.package[GleamPackageInfo]
    test_env = dict(pkg_info.transitive_deps)
    test_env[pkg_info.package_name] = pkg_info.output_dir
    test_env["prelude.mjs"] = ctx.attrs.prelude[DefaultInfo].default_outputs[0]

    test_env["register.mjs"] = ctx.attrs.loader_register[DefaultInfo].default_outputs[0]
    test_env["hooks.mjs"] = ctx.attrs.loader_hooks[DefaultInfo].default_outputs[0]

    runner_content = 'import { main } from "./' + pkg_info.package_name + "/" + ctx.attrs.module + '.mjs";\nmain();\n'
    runner_mjs = ctx.actions.write("run_test.mjs", runner_content)
    test_env["run_test.mjs"] = runner_mjs

    test_dir = ctx.actions.symlinked_dir("__test_dir", test_env)

    script = ctx.actions.declare_output("run_test.sh")
    script_lines = [
        "#!/usr/bin/env bash",
        "set -euo pipefail",
        'TEST_DIR="$1"',
        'shift',
        'cd "$TEST_DIR"',
        'if [ -z "${DEVENV_ROOT:-}" ]; then',
        '  p="$PWD"',
        '  while [ "$p" != "/" ] && [ ! -f "$p/.buckconfig" ]; do',
        '    p="$(dirname "$p")"',
        '  done',
        '  if [ -f "$p/.buckconfig" ]; then',
        '    export DEVENV_ROOT="$p"',
        '  fi',
        'fi',
        'exec node --preserve-symlinks --preserve-symlinks-main --import ./register.mjs run_test.mjs "$@"',
    ]
    ctx.actions.write(script, "\n".join(script_lines) + "\n", is_executable = True)

    cmd = cmd_args(script, test_dir)

    return [
        DefaultInfo(default_output = script),
        RunInfo(args = cmd),
        ExternalRunnerTestInfo(
            type = "gleam",
            command = [script, test_dir],
        ),
    ]

_gleam_test = rule(
    impl = _gleam_test_impl,
    attrs = {
        "loader_hooks": attrs.dep(default = "@root//dev-node-loader:hooks.mjs"),
        "loader_register": attrs.dep(default = "@root//dev-node-loader:register.mjs"),
        "module": attrs.string(),
        "package": attrs.dep(),
        "prelude": attrs.dep(default = "dev_buck//:prelude"),
    },
)

def gleam_test(
    name,
    package,
    module,
    visibility = ["PUBLIC"],
):
    _gleam_test(
        name = name,
        package = package,
        module = module,
        visibility = visibility,
    )

