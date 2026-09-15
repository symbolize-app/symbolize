load("@prelude//rules.bzl", "export_file", "test_suite")
load("//:workspace.bzl", "rust_members")

export_file(
    name = "rustfmt.toml",
    src = "rustfmt.toml",
    visibility = ["PUBLIC"],
)

test_suite(
    name = "test",
    tests = [f"//{pkg}:test" for pkg in rust_members],
    visibility = ["PUBLIC"],
)


