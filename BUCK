load("@prelude//rules.bzl", "export_file", "test_suite")
load("//:workspace.bzl", "members")

export_file(
    name = "rustfmt.toml",
    src = "rustfmt.toml",
    visibility = ["PUBLIC"],
)

test_suite(
    name = "test",
    tests = [f"//{pkg}:test" for pkg in members],
    visibility = ["PUBLIC"],
)


