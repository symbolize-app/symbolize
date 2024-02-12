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

## TypeScript

Basics controlled by ESLint and Prettier.

Other details:

- Don't put logic in a constructor
  - If needed, introduce a static builder method
- Hide implementation details inside class or interface methods
- Make most data readonly
- Use classes over plain interfaces
