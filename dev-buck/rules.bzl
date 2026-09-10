load("@prelude//rules.bzl", _alias = "alias", _rust_binary = "rust_binary", _rust_library = "rust_library", _rust_test = "rust_test")
load(":clippy.bzl", _rust_clippy_check = "rust_clippy_check")
load(":format.bzl", _rust_format_check = "rust_format_check", _rust_format_diff = "rust_format_diff")

_DEFAULT_RUSTC_FLAGS = [
    "-A",
    "clippy::single_component_path_imports",
]

_MODE_RUSTC_FLAGS = select({
    "dev_buck//mode:is_debug": [
        "-C",
        "opt-level=0",
    ],
    "dev_buck//mode:is_release": [
        "-C",
        "opt-level=3",
        "-C",
        "codegen-units=1",
    ],
})

def _default_env(name, extra_env = {}):
    env = {
        "CARGO_PKG_NAME": name,
        "CARGO_PKG_VERSION": "0.1.0",
    }
    env.update(extra_env)
    return env

# Canonical rust_library wrapper for first-party Symbolize crates.
# Strictly enforces repository invariants by construction:
# - Crate root is always src/mod.rs
# - Edition is always 2021
# - Sources are always globbed from src/**/*.rs
# - Crate name is always derived from the target name (replacing '-' with '_')
# - Visibility is always PUBLIC
# - Companion test target is always named :test
# - Companion format-check target is always named :format-check
# - Companion format-diff target is always named :format-diff
# - Companion clippy target is always named :clippy
def rust_library(
    name,
    deps = [],
    test_deps = [],
    features = [],
    test_features = [],
    rustc_flags = [],
    env = {},
    test = True,
):
    crate = name.replace("-", "_")
    crate_root = "src/mod.rs"
    edition = "2021"
    srcs = native.glob(["src/**/*.rs", "src/**/*.sql", "src/**/*.txt"])

    effective_rustc_flags = rustc_flags + _DEFAULT_RUSTC_FLAGS + _MODE_RUSTC_FLAGS
    effective_env = _default_env(name, env)

    _rust_library(
        name = name,
        crate = crate,
        crate_root = crate_root,
        edition = edition,
        srcs = srcs,
        deps = deps,
        features = features,
        rustc_flags = effective_rustc_flags,
        env = effective_env,
        visibility = ["PUBLIC"],
    )

    if test and not native.rule_exists("test"):
        _rust_test(
            name = "test",
            crate = crate,
            crate_root = crate_root,
            edition = edition,
            srcs = srcs,
            deps = deps + test_deps,
            features = features + test_features,
            rustc_flags = effective_rustc_flags,
            env = effective_env,
        )

    rs_srcs = [s for s in srcs if s.endswith(".rs")]

    if not native.rule_exists("format-diff"):
        _rust_format_diff(
            name = "format-diff",
            srcs = rs_srcs,
            visibility = ["PUBLIC"],
        )

    if not native.rule_exists("format-check"):
        _rust_format_check(
            name = "format-check",
            diff = ":format-diff",
            visibility = ["PUBLIC"],
        )

    if not native.rule_exists("clippy"):
        clippy_targets = [":{}[clippy.txt]".format(name)]
        if test and native.rule_exists("test"):
            clippy_targets.append(":test[clippy.txt]")

        _rust_clippy_check(
            name = "clippy",
            targets = clippy_targets,
            visibility = ["PUBLIC"],
        )

# Canonical rust_binary wrapper for first-party Symbolize crates and tools.
# Invariants enforced:
# - Default crate root resolves to src/main.rs or src/mod.rs
# - Edition is always 2021
# - Sources are always globbed from src/**/*.rs
# - Visibility is always PUBLIC
# - Automatically depends on companion library in same package if present
# - Creates convenient package-name alias if target name differs from directory
# - Equipped with companion :format-check, :format-diff, and :clippy targets
def rust_binary(
    name,
    crate_root = None,
    deps = [],
    test_deps = [],
    features = [],
    test_features = [],
    rustc_flags = [],
    env = {},
    lib = None,
    test = True,
    format = True,
    clippy = True,
):
    crate = name.replace("-", "_")
    if crate_root == None:
        if native.glob(["src/main.rs"]):
            crate_root = "src/main.rs"
        else:
            crate_root = "src/mod.rs"

    edition = "2021"
    srcs = native.glob(["src/**/*.rs", "src/**/*.sql", "src/**/*.txt"])
    effective_rustc_flags = rustc_flags + _DEFAULT_RUSTC_FLAGS + _MODE_RUSTC_FLAGS
    effective_env = _default_env(name, env)

    effective_deps = list(deps)
    if lib:
        if lib not in effective_deps:
            effective_deps.append(lib)
    elif lib == None:
        pkg = native.package_name()
        potential_libs = [":" + pkg, ":lib", ":symbolize-" + pkg]
        for plib in potential_libs:
            target_name = plib.lstrip(":")
            if target_name != name and native.rule_exists(target_name):
                if plib not in effective_deps:
                    effective_deps.append(plib)
                break

    _rust_binary(
        name = name,
        crate_root = crate_root,
        edition = edition,
        srcs = srcs,
        deps = effective_deps,
        features = features,
        rustc_flags = effective_rustc_flags,
        env = effective_env,
        visibility = ["PUBLIC"],
    )

    # Convenience alias: if name is symbolize-<pkg> and target <pkg> does not exist, create alias
    pkg_basename = native.package_name().split("/")[-1]
    if name != pkg_basename and not native.rule_exists(pkg_basename):
        _alias(
            name = pkg_basename,
            actual = ":" + name,
            visibility = ["PUBLIC"],
        )

    if test and not native.rule_exists("test"):
        _rust_test(
            name = "test",
            crate = crate,
            crate_root = crate_root,
            edition = edition,
            srcs = srcs,
            deps = effective_deps + test_deps,
            features = features + test_features,
            rustc_flags = effective_rustc_flags,
            env = effective_env,
        )

    rs_srcs = [s for s in srcs if s.endswith(".rs")]

    if format and not native.rule_exists("format-diff"):
        _rust_format_diff(
            name = "format-diff",
            srcs = rs_srcs,
            visibility = ["PUBLIC"],
        )

    if format and not native.rule_exists("format-check"):
        _rust_format_check(
            name = "format-check",
            diff = ":format-diff",
            visibility = ["PUBLIC"],
        )

    if clippy:
        clippy_target_name = "clippy" if not native.rule_exists("clippy") else "{}-clippy".format(name)
        clippy_targets = [":{}[clippy.txt]".format(name)]
        if test and native.rule_exists("test"):
            clippy_targets.append(":test[clippy.txt]")

        _rust_clippy_check(
            name = clippy_target_name,
            targets = clippy_targets,
            visibility = ["PUBLIC"],
        )




