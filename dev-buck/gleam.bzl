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

    cmd = cmd_args(
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
        DefaultInfo(default_output = out_dir),
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

def gleam_package(
    name,
    package_name = None,
    srcs = None,
    deps = [],
    package_dir = "",
    javascript_prelude = "../prelude.mjs",
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
