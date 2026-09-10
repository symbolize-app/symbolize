def _rust_clippy_check_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output("clippy-check.ok")

    script = ctx.actions.declare_output("run_clippy_check.sh")
    script_lines = [
        "#!/usr/bin/env bash",
        "set -euo pipefail",
        "OUT=\"$1\"",
        "shift",
        "failed=0",
        "for diag in \"$@\"; do",
        "  if [ -s \"$diag\" ]; then",
        "    cat \"$diag\"",
        "    failed=1",
        "  fi",
        "done",
        "if [ \"$failed\" -ne 0 ]; then",
        "  exit 1",
        "fi",
        "touch \"$OUT\"",
    ]
    ctx.actions.write(script, "\n".join(script_lines) + "\n", is_executable = True)

    cmd = cmd_args([script, out.as_output()])
    for dep in ctx.attrs.targets:
        for artifact in dep[DefaultInfo].default_outputs:
            cmd.add(artifact)

    ctx.actions.run(
        cmd,
        category = "clippy_check",
    )

    return [DefaultInfo(default_output = out)]

rust_clippy_check = rule(
    impl = _rust_clippy_check_impl,
    attrs = {
        "targets": attrs.list(attrs.dep(), default = []),
    },
)
