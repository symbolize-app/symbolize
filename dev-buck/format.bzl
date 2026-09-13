def _rust_format_diff_impl(ctx: AnalysisContext) -> list[Provider]:
    diff_out = ctx.actions.declare_output("format.diff")

    diff_generator = ctx.actions.declare_output("generate_diff.sh")
    script_lines = [
        "#!/usr/bin/env bash",
        "set -euo pipefail",
        "CONFIG=\"$1\"",
        "DIFF_OUT=\"$2\"",
        "shift 2",
        ": > \"$DIFF_OUT\"",
        "for src in \"$@\"; do",
        "  formatted=$(rustfmt --config-path \"$CONFIG\" < \"$src\")",
        "  diff -u -L \"a/$src\" -L \"b/$src\" \"$src\" - <<< \"$formatted\" >> \"$DIFF_OUT\" || [ $? -eq 1 ]",
        "done",
    ]
    ctx.actions.write(diff_generator, "\n".join(script_lines) + "\n", is_executable = True)

    gen_cmd = cmd_args([
        diff_generator,
        ctx.attrs.config,
        diff_out.as_output(),
    ])
    for s in ctx.attrs.srcs:
        gen_cmd.add(s)

    ctx.actions.run(
        gen_cmd,
        category = "rustfmt_diff",
    )

    return [DefaultInfo(default_output = diff_out)]

rust_format_diff = rule(
    impl = _rust_format_diff_impl,
    attrs = {
        "srcs": attrs.list(attrs.source()),
        "config": attrs.source(default = "root//:rustfmt.toml"),
    },
)

def _rust_format_check_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output("format-check.ok")
    diff = ctx.attrs.diff[DefaultInfo].default_outputs[0]

    script = ctx.actions.declare_output("run_format_check.sh")
    script_lines = [
        "#!/usr/bin/env bash",
        "set -euo pipefail",
        "DIFF=\"$1\"",
        "OUT=\"$2\"",
        "if [ -s \"$DIFF\" ]; then",
        "  cat \"$DIFF\"",
        "  exit 1",
        "fi",
        "touch \"$OUT\"",
    ]
    ctx.actions.write(script, "\n".join(script_lines) + "\n", is_executable = True)

    cmd = cmd_args([script, diff, out.as_output()])

    ctx.actions.run(
        cmd,
        category = "rustfmt_check",
    )

    return [DefaultInfo(default_output = out)]

rust_format_check = rule(
    impl = _rust_format_check_impl,
    attrs = {
        "diff": attrs.dep(),
    },
)

