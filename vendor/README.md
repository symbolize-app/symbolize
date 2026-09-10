# Vendor snapshots

This directory contains the transitive closure of checked-out snapshots of
upstream Git repositories that Symbolize depends on but that are not provided
by Devenv. Gleam dependencies and build adapters consume direct paths into
these snapshots. Rust dependencies define local Buck2 targets and are exposed
through unversioned aliases in `vendor/BUCK`; nothing is installed by a package
manager or linked through a vendor workspace.

Each immediate child directory represents one upstream repository:

```text
vendor/
└── <name>-<version-or-commit-date>/
    ├── .vendor.toml
    └── <upstream repository files>
```

## Dependency closure

`vendor/` must contain every non-Devenv upstream dependency needed by the
vendored roots, recursively, as direct immediate child directories. Transitive
dependencies (for example, libraries required by TypeScript sources such as
`rxjs`, `urlpattern-polyfill`, and `parsel-js`) must not be bundled into
consumers or stubbed with hand-maintained adapter packages; each must exist as
its own distinct, versioned child directory under `vendor/`. A dependency must
not rely on an undeclared package-manager installation or on a dependency that
exists only elsewhere in the developer environment.

Dependencies supplied by Devenv are excluded from the closure. For example,
ESLint and Prettier belong to Devenv and should not be duplicated here. Node
built-ins and other platform facilities are also not vendored.

Every snapshot in the closure follows the same naming, metadata, and
automatic-agent-file rules, whether it is a direct dependency or a deeper
transitive dependency.

All third-party projects are kept as pure Git checkouts.

### Revendoring nested dependencies and single sources of truth

When an upstream dependency vendors or bundles its own copy of an underlying
third-party library (for example, `libsqlite3-sys` bundling SQLite amalgamations
and SQLCipher, or Node packages bundling C/C++ libraries in internal directories),
**never keep their nested copy**.

Instead:
1. **Revendor at root**: Revendor the underlying dependency directly at the top
   level of `vendor/` as its own distinct, versioned upstream Git checkout
   (e.g., `vendor/sqlite3-3.46.0/`).
2. **Prune nested duplicates**: Purge the nested vendor directories and bundled
   amalgamations from the consumer's snapshot.
3. **Wire to the canonical target**: Point the consumer's build configuration
   directly to the top-level vendored target (e.g., `deps = ["//sqlite3-3.46.0:sqlite3"]`
   in Buck2, or staging the build artifact dynamically in Taskfile tasks).

This practice establishes a strict single source of truth across all language
ecosystems in the monorepo (e.g., Rust's `rusqlite` and Node's `better-sqlite3` both
consume the exact same SQLite 3.46.0 source), prevents version drift, eliminates
hundreds of thousands of lines of duplicate code from git history, and guarantees
uniform license auditing.

### Dynamic synthesis: leveraging Devenv + Buck2 for special build steps

Vendored dependencies must not contain committed build artifacts, pre-computed
amalgamations, generated assembly routines, or pre-built binaries. When an upstream
project requires code generation, macro expansions, or pre-compilation steps,
leverage **Devenv** and **Buck2** in tandem:

- **Devenv supplies ambient toolchains**: Devenv provides the necessary hermetic
  compilers, interpreters, and build tools (such as `gcc`, `clang`, `rustc`, `go`,
  `tclsh`, `perl`, `python`, `node-gyp`) in the ambient developer environment. Buck2
  rules rely on this ambient PATH rather than hardcoding Nix store hashes or
  downloading external binaries.
- **Buck2 orchestrates dynamic synthesis**: Buck2 `genrule` targets execute the
  upstream generation scripts (for example, generating SQLite's `sqlite3.c`
  amalgamation using upstream `Makefile.linux-gcc` + `tclsh`, or running upstream
  Perl scripts to synthesize `ring`'s architecture-specific assembly).
- **Hermetic build consumption**: Generated source files and compiled libraries
  (`cxx_library`, `rust_library`) are tracked as rule outputs in `buck-out/` and
  consumed directly by downstream build targets or staged into build directories
  when required by native addons.

## Directory names

Snapshot directories must identify both the upstream name and the snapshot
version:

```text
<name>-<version-or-commit-date>
```

Use the upstream package or repository name for `<name>`, making it safe for a
directory name when necessary. For scoped package names, omit the leading `@`
and replace `/` with `-`; for example, `@scope/package` becomes
`scope-package`.

Use the published package version when one exists:

```text
vendor/example-library-1.2.3/
```

For a repository snapshot without a package version, use the selected commit's
date in `YYYYMMDD` form:

```text
vendor/example-repository-20260904/
```

The date is for identification only and is not necessarily unique. If two
snapshots would have the same directory name, append a short commit SHA. The
exact commit remains authoritative in `.vendor.toml`.

## Snapshot metadata

Every snapshot must have a `.vendor.toml` file at its root. It records the
upstream repository and the exact commit used for the snapshot:

```toml
url = "https://github.com/example/project"
ref = "0123456789abcdef0123456789abcdef01234567"
```

`url` is the upstream Git repository URL. `ref` must resolve to the commit
that was checked out; use a full commit SHA whenever possible.

Code that uses the snapshot should reference its versioned directory directly.
For example, a Gleam package dependency may use:

```toml
[dependencies]
example_library = { path = "../vendor/example-library-1.2.3" }
```

JavaScript or other build tooling should follow the same principle and import
or read files from the versioned snapshot path when needed.

All Gleam project dependencies must use local path specifications. The
repository's vendor check rejects registry dependencies and locked manifest
entries whose source is not `local`. The shared `gleam_stdlib` dependency is
vendored as `gleam_stdlib-1.0.5`.

## Rust and Buck2 snapshots

Rust library dependencies use the same snapshot layout, but their source of
truth is the upstream Git repository and the full commit recorded in
`.vendor.toml`, not the crates.io archive or a package registry cache. `.vendor.toml`
strictly permits only `url` and `ref`:

```toml
url = "https://github.com/example/project"
ref = "0123456789abcdef0123456789abcdef01234567"
```

Each vendored Rust package defines its build rule in a local `BUCK` file using
the `vendor_rust_library` or `rust_library` rules from `vendor/rules.bzl` or
`//:rules.bzl`. `dev-gen` scans these vendored `BUCK` files to synthesize
unversioned target aliases in `vendor/BUCK` (e.g. `alias(name = "tokio", actual = "//vendor/tokio-1.39.2/tokio:tokio")`),
allowing first-party crates to depend on unversioned targets like `vendor//:tokio`
and `vendor//:serde`. When multiple snapshot versions of a package are retained,
they are explicitly tracked in `vendor/dup.toml` with consumer lists and justifications.

Cargo package-manager installations, root manifests, and lockfiles are intentionally absent.
All compilation, testing, and formatting run through Buck2. `task vendor:check` validates
Git metadata, directory naming conventions, duplicate justifications, and closure integrity,
while `task vendor:license:check` executes a pure Haskell SPDX license auditor across all snapshots.

## Node and TypeScript snapshots

Node and TypeScript library dependencies follow the same direct-path snapshot
principles without package managers, root `package.json`, or `node_modules`.
Every upstream JavaScript/TypeScript library is kept as a pure Git checkout in
an immediate child directory of `vendor/`.

`vendor/node.json` serves as the single source of truth for bare module
specifier remapping across the repository:

```json
{
  "version": 1,
  "imports": {
    "puppeteer": "vendor/puppeteer-25.9.0/packages/puppeteer/src/puppeteer.ts",
    "rxjs": "vendor/rxjs-7.8.2/src/index.ts",
    "urlpattern-polyfill": "vendor/urlpattern-polyfill-10.0.0/src/url-pattern.ts",
    "parsel-js": "vendor/parsel-js-1.2.3/parsel.ts",
    "better-sqlite3": "vendor/better-sqlite3-11.1.2/lib/index.js",
    "esbuild": "build/vendor/esbuild/main.js"
  }
}
```

Execution and bundling consume this table directly:
- **Node runtime execution**: Node 22 executes tests and scripts directly via
  `dev-node-loader` (registered via `NODE_OPTIONS`), which intercepts bare
  specifiers using `node.json`, transpiles TypeScript files on the fly, and
  resolves extensionless TypeScript imports across vendored packages.
- **Frontend bundling**: `dev-esbuild` reads `vendor/node.json` directly to
  populate esbuild aliases during bundling.
- **No wrapper packages**: Owned glue packages and handwritten `index.js` shim
  directories (such as `dev-vendor/`) are strictly prohibited. Upstream libraries
  are consumed directly from their source entrypoints or legitimate build
  artifacts.
- **Native build artifacts**: Platform-specific native outputs (such as
  `better_sqlite3.node` compiled via `node-gyp` or the `esbuild` Go binary)
  belong strictly in `build/vendor/` and are built by tasks in
  `dev-task/vendor.yml`.

## Buck2 target resolution

Buck2 projects consume vendored dependencies through the `vendor//` cell,
providing direct, hermetic compilation without shadow lockfiles, translation
layers, or package-manager caches.

`vendor/BUCK` serves as the single source of truth for unversioned target
aliases across the repository, acting as the exact equivalent of
`vendor/node.json` for Node module specifiers:

```python
alias(
    name = "anyhow",
    actual = "//anyhow-1.0.79:anyhow",
    visibility = ["PUBLIC"],
)

alias(
    name = "tokio",
    actual = "//tokio-1.38.0:tokio",
    visibility = ["PUBLIC"],
)
```

Consumers throughout the repository depend directly on these canonical,
unversioned aliases:

```python
rust_library(
    name = "symbolize-lib-hex",
    ...
    deps = [
        "vendor//:anyhow",
    ],
)
```

This establishes a clear dependency architecture:
- **Zero version coupling**: First-party packages never hardcode snapshot
  version numbers in their dependencies. Upgrading an upstream crate only
  requires repointing its single alias in `vendor/BUCK`.
- **Intra-vendor functional rules**: Vendored crates load `load("//:rules.bzl", "rust_library")`
  directly from the `vendor//` cell root. This encapsulates baseline compilation
  flags (`--cap-lints=allow`) and defaults visibility to `PUBLIC` without
  introducing ambient directory inheritance or `PACKAGE` files.
- **Explicit feature declaration**: Vendored crates declare their active feature
  set explicitly using the native `features = [...]` attribute, reflecting the
  monorepo-unified feature requirements of the codebase.

## Rust crate ingestion and update workflow

In Symbolize's Buck2 monorepo architecture, all third-party Rust dependencies are vendored directly in `vendor/` and built natively through the `vendor//` cell. Builds do not access the network, do not invoke Cargo, and do not consult or require lockfiles at build time; Git history provides the immutable, bit-for-bit cryptographic lock on all dependencies.

### Step-by-Step Ingestion & Update Procedure

When adding a new third-party crate or updating an existing one:

1. **Import the Upstream Repository Snapshot**:
   - Check out the upstream repository at the desired release tag or commit.
   - Place the snapshot in an immediate child directory named `vendor/<package-name>-<version-or-date>/`.
   - Remove upstream `.git`, `.gitattributes`, and automatic-agent instruction files (`AGENTS.md`, `CLAUDE.md`, etc.).
   - Create `.vendor.toml` at the snapshot root specifying `url` and `ref` (full commit SHA).

2. **Automate BUCK Alias Registration**:
   - Run:
     ```bash
     task gen
     ```
     (or `task vendor:rust:gen`).
   - This executes `dev-gen` (Haskell generator), which:
     - Scans vendored crates and libraries in `vendor/**/BUCK`.
     - Generates canonical, unversioned aliases in [`vendor/BUCK`](file:///home/geecko/code/symbolize/vendor/BUCK) (e.g. `alias(name = "tokio", actual = "//tokio-1.40.0:tokio")`).
     - Automatically normalizes hyphenated crate names to both hyphenated and underscored aliases (e.g. `serde-json` and `serde_json`).

3. **Audit Third-Party Licenses**:
   - Run:
     ```bash
     task vendor:license:check
     ```
     (or `task security`).
   - The Haskell license auditor in `dev-gen` inspects every package under `vendor/` across all languages and frameworks, evaluating full SPDX expressions against the approved open-source license list (MIT, Apache-2.0, BSD-2-Clause, BSD-3-Clause, ISC, CC0-1.0, Unicode-3.0, Zlib, etc.).

4. **Handle C, Assembly, or Code Generation Special Cases**:
   - Dependencies with native C, assembly, or code-generation steps (such as `ring` or `libsqlite3-sys`) must not rely on committed generated files or arbitrary Cargo `build.rs` execution.
   - Revendor any nested third-party dependencies (such as SQLite) at the top level of `vendor/` and prune the nested copies.
   - Leverage Devenv to provide any required ambient toolchains (`perl`, `tclsh`, etc.).
   - Define Buck2 `genrule` targets to dynamically synthesize required sources (such as assembly files or amalgamations) and build native libraries using `cxx_library`.
   - Wire consumer libraries directly to these dynamically synthesized targets.

5. **Consume Canonical Aliases**:
   - In first-party `BUCK` files, add `vendor//:<crate-name>` to `deps`.
   - Never reference versioned snapshot directories directly from first-party crates.

6. **Validate Hermetic Compilation**:
   - Verify that all builds, tests, clippy checks, and formatting pass with an explicit mode modifier (e.g. `-m debug` or `-m release`):
     ```bash
     buck2 build -m debug //...
     buck2 test -m debug //...
     buck2 bxl -m debug dev_buck//clippy.bxl:check
     buck2 bxl -m debug dev_buck//format.bxl:check
     ```

## Duplicate dependency ledger

Duplicate snapshot directories in `vendor/` are strictly prohibited unless
truly exceptional circumstances require major rework to resolve. Duplicates
must be actively resolved and normalized whenever feasible (such as upgrading
consumers, updating callers to compatible releases, pruning obsolete flags,
or consolidating shared dependencies). Retaining multiple snapshots of the
same upstream repository is never an accepted convenience or permanent steady
state.

Normal dependency maintenance is **never** exceptional. Routine version
differences across transitive dependencies, packaging churn (such as Win32 raw
C bindings across generational release tags), or obsolete execution models
dragged in by unused compatibility flags must be resolved directly rather than
tolerated as duplicates.

"Exceptional circumstances requiring major rework" is strictly defined as:
1. Two versions that are essentially different crates under the same name
   (such as a total paradigm shift in macro syntax, type representation, or
   design between generational versions, e.g. `bitflags` 1.x vs 2.x);
2. Updating to a new API would change fundamental architectural code
   structures across the consumer; or
3. Resolving the duplicate would require deep, pervasive changes across a large
   portion of the vendor closure where no compatible upstream releases or
   straightforward migrations exist.

`dup.toml` records the repository identities for which the closure contains
more than one snapshot directory, together with the concrete technical blockers
explaining why they have not been normalized.

The ledger covers the immediate snapshot directories in `vendor/` (each named
`<name>-<version-or-commit-date>`). It tracks upstream repository identities,
not individual Cargo packages or internal architecture target subcrates within
a multi-crate monorepo snapshot. It does not reinterpret package-manager
lockfiles retained inside an upstream repository snapshot as part of that
repository's source history; those lockfiles are not inputs to Symbolize's
direct-path resolution.

The file uses this schema:

```toml
version = 1

# When all snapshot identities are normalized, no [[duplicate]] entries exist.
# If an exceptional circumstance requires retaining multiple snapshots:
# [[duplicate]]
# name = "example-library"
# snapshots = ["example-library-1.2.3", "example-library-2.0.0"]
# consumers = [
#   "example-tool-4.0.0: ^1.2.0",
#   "other-tool-7.0.0: ^2.0.0",
# ]
# justification = "The consumers require fundamentally incompatible macro architectures, and upgrading either consumer requires pervasive rewrites across the closure."
```

An unresolved `[[duplicate]]` entry must name every retained snapshot directory
and its consumers, and its `justification` must articulate a concrete technical
blocker requiring major rework. Placeholders such as “minor vendored source
changes needed” or boilerplate justifications are strictly rejected.

The ledger is verified by `task vendor:check` against invalid entries: every
entry must correspond to an actual set of duplicated snapshot directories in
`vendor/`, list all and only those retained directories, and contain no entries
for single snapshots, non-existent directories, or internal subcrates. If all
snapshots are normalized, `dup.toml` must still exist declaring `version = 1`
with an empty ledger.

## Requirements

- Keep one upstream repository per immediate child directory.
- Include the upstream name and package version or commit date in every
  snapshot directory name.
- Keep `.vendor.toml` alongside the snapshot it describes.
- Do not preserve upstream `.gitattributes` files; their rules would affect
  future additions and checkouts within the vendored subtree.
- Keep tracked upstream files even when an upstream `.gitignore` hides them;
  the vendor check rejects any files left untracked below `vendor/`.
- Duplicate snapshot directories are prohibited unless truly exceptional
  circumstances require major rework to normalize; any retained duplicates must
  be justified in `dup.toml`.
- Do not put package-manager installations or Devenv state in this directory.
- No generated artifacts, ad-hoc wrappers, or monolithic runtime bundles are
  permitted in `vendor/`. All snapshots must be pure upstream Git checkouts. Build
  output for native or platform-specific tools belongs strictly in `build/vendor/`.
  First-party glue adapter packages and owned `index.js` shim files are strictly
  prohibited.
- Revendor any third-party dependencies bundled inside an upstream snapshot as
  independent top-level snapshots in `vendor/`, and delete the nested copies.
- Do not commit generated build artifacts, amalgamations, or assembly to `vendor/`.
  Leverage Devenv for ambient toolchains and Buck2 for dynamic build-time synthesis.
- Expose every active vendored crate as an unversioned alias in `vendor/BUCK`.
  First-party packages must depend on canonical `vendor//:<name>` targets rather
  than coupling to versioned snapshot directory names.
- Vendored subdirectories must not contain automatic-agent instruction files,
  including `AGENTS.md`, `CLAUDE.md`, or equivalent files with other names.

Devenv configuration and dependencies remain in `devenv.*` and
`dev-devenv/`; they do not belong in `vendor/`.

## Updating a snapshot

To update a vendored dependency, select the upstream Git commit, export that
commit without its `.git` directory into a new versioned child directory, and
write its full commit SHA and upstream URL to `.vendor.toml`. Recalculate the
root's transitive dependency closure and add any missing upstream dependencies.
Ensure `BUCK` build files are present in newly vendored crates, run `dev-gen` to
regenerate `vendor/BUCK` aliases, and update `vendor/dup.toml` if retaining multiple
versions of a package. Update module mappings in `vendor/node.json` as needed, then
run `task vendor:check` and the full repository check `task c`.

Native outputs are rebuilt for the current platform from the exact snapshots:
esbuild is compiled with Go, and better-sqlite3 is compiled with node-gyp.
Puppeteer executes directly from TypeScript source via `dev-node-loader` and the
Chromium executable provided by Devenv; it does not download a browser or resolve
a package from `node_modules`.
