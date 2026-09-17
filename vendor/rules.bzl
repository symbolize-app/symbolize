load("@prelude//rules.bzl", _alias = "alias", _cxx_library = "cxx_library", _genrule = "genrule", _rust_library = "rust_library")

VENDOR_RUSTC_FLAGS = [
    "--cap-lints=allow",
]

def rust_library(name, rustc_flags = [], **kwargs):
    if "visibility" in kwargs:
        fail("Cannot override 'visibility' in vendor rust_library; all vendored crates are PUBLIC.")

    _rust_library(
        name = name,
        rustc_flags = VENDOR_RUSTC_FLAGS + rustc_flags,
        visibility = ["PUBLIC"],
        **kwargs,
    )

def alias(name, actual):
    _alias(
        name = name,
        actual = actual,
        visibility = ["PUBLIC"],
    )

def cxx_library(name, **kwargs):
    _cxx_library(
        name = name,
        visibility = ["PUBLIC"],
        **kwargs,
    )

def genrule(name, **kwargs):
    _genrule(
        name = name,
        visibility = ["PUBLIC"],
        **kwargs,
    )

load("@dev_buck//:gleam.bzl", _gleam_package = "gleam_package")

def gleam_package(name, **kwargs):
    _gleam_package(
        name = name,
        visibility = ["PUBLIC"],
        **kwargs,
    )

load("@dev_buck//:haskell.bzl", _haskell_boot_package = "haskell_boot_package", _haskell_library = "haskell_library")

def haskell_library(name, **kwargs):
    _haskell_library(
        name = name,
        visibility = ["PUBLIC"],
        **kwargs,
    )

def haskell_boot_package(name, **kwargs):
    _haskell_boot_package(
        name = name,
        visibility = ["PUBLIC"],
        **kwargs,
    )


