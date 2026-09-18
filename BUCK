load("@prelude//rules.bzl", "export_file", "test_suite")
load("//:workspace.bzl", "gleam_members", "haskell_members", "rust_members")

export_file(
    name = "rustfmt.toml",
    src = "rustfmt.toml",
    visibility = ["PUBLIC"],
)

_browser_only_gleam = [
    "dev-browser-test",
    "svc-auth-guest-read",
    "svc-auth-guest-view",
]

test_suite(
    name = "test",
    tests = [
        f"//{pkg}:test"
        for pkg in rust_members + haskell_members + [
            pkg
            for pkg in gleam_members
            if pkg not in _browser_only_gleam
        ]
    ],
    visibility = ["PUBLIC"],
)
