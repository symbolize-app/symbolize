# Code style

General notes:

- Use absolute imports for everything
- Use un-prefixed imports sparingly
- Use a nested object for specialized context
- Use "init" name for builders that acquire resources

## Rust

Basics controlled by clippy and rustfmt.

Other details:

- Use a builder instead of letting a constructor become compilcated
- Use `mod.rs` to "index" all source files

## Gleam

Basics controlled by the Gleam formatter and compiler.

Other details:

- Prefer records for data and `..default` updates for optional fields.
- Keep type nesting shallow and use common types across APIs.
- Use opaque types only for semantic boundaries that need representation hiding.
- Keep Web API and JavaScript interop at explicit FFI boundaries.

## CSS

Reset includes:

- Browser or element-specific simplification
- Settings for jank-free preload (fonts, sizing)
- Line box grid setup

In-app root includes:

- Application-level details not needed for preload
