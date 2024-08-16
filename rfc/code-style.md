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

### Classes

- Use interfaces instead of object literal type aliases
- Use class instances instead of object literals
- Only export class types (except errors)
- Don't export class types for purely-implementation classes
- Export an optionally-async constructor function with the same name as the class
- Use a readonly type alias if the class has mutable fields
- Don't use class inheritance

## CSS

Reset includes:

- Browser or element-specific simplification
- Settings for jank-free preload (fonts, sizing)
- Line box grid setup

In-app root includes:

- Application-level details not needed for preload
