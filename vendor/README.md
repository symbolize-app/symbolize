# Vendor snapshots

This directory contains the transitive closure of checked-out snapshots of
upstream Git repositories that Symbolize depends on but that are not provided
by Devenv. Gleam dependencies and build adapters consume direct paths into
these snapshots. Cargo dependencies retain their upstream manifests and are
redirected by the root workspace's `[patch.crates-io]` table; nothing is
installed by a package manager or linked through a vendor workspace.

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

An upstream project that distributes a generated source unit may be represented
by that exact source unit instead of a second copy inside each consumer. The
SQLite amalgamation is kept this way in `sqlite3-3.46.0`; consumers stage or
reference those shared files from their own build adapters. Its upstream Git
ref and SQLite source identifier are recorded in `.vendor.toml`.

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

## Cargo snapshots

Rust library dependencies use the same snapshot layout, but their source of
truth is the upstream Git repository and the full commit recorded in
`.vendor.toml`, not the crates.io archive or Cargo's local cache. `.vendor.toml`
strictly permits only `url` and `ref`:

```toml
url = "https://github.com/example/project"
ref = "0123456789abcdef0123456789abcdef01234567"
```

The root workspace keeps its normal Cargo version constraints, and the root
`[patch.crates-io]` table maps every active `(package name, version)` identity
to a local path in these snapshots. Vendored Cargo manifests retain their
upstream registry dependency declarations; Cargo applies the root patches
when resolving the active graph. Multiple versions use unique patch aliases
with `package = "..."`. The Cargo lockfile contains no registry or Git package
sources. The outer workspace excludes `vendor/` because a monorepo snapshot
may contain several package roots and multiple retained versions of a package.

Cargo package-manager installations and fetch tasks are intentionally absent.
Development binaries such as `cargo-deny` are supplied by Devenv, while
library sources remain in this directory. `task vendor:check` validates the
Git metadata, package mappings, root patch table, local Cargo paths, and
lockfile closure.

## Node and TypeScript snapshots

Node and TypeScript library dependencies follow the same direct-path snapshot
principles without package managers, root `package.json`, or `node_modules`.
Every upstream JavaScript/TypeScript library is kept as a pure Git checkout in
an immediate child directory of `vendor/`.

`vendor/node.json` serves as the single source of truth for bare module
specifier remapping across the repository, acting as the exact equivalent of
Cargo's `[patch.crates-io]` table:

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
- Vendored subdirectories must not contain automatic-agent instruction files,
  including `AGENTS.md`, `CLAUDE.md`, or equivalent files with other names.

Devenv configuration and dependencies remain in `devenv.*` and
`dev-devenv/`; they do not belong in `vendor/`.

## Updating a snapshot

To update a vendored dependency, select the upstream Git commit, export that
commit without its `.git` directory into a new versioned child directory, and
write its full commit SHA and upstream URL to `.vendor.toml`. Recalculate the
root's transitive dependency closure and regenerate the root
`[patch.crates-io]` table; do not rewrite upstream Cargo manifests merely to
point at neighboring snapshots. Add every newly required upstream repository
unless it is supplied by Devenv. Update module mappings in `vendor/node.json`
and build tasks as needed, then run `task vendor:check` and the full repository check.

Native outputs are rebuilt for the current platform from the exact snapshots:
esbuild is compiled with Go, and better-sqlite3 is compiled with node-gyp.
Puppeteer executes directly from TypeScript source via `dev-node-loader` and the
Chromium executable provided by Devenv; it does not download a browser or resolve
a package from `node_modules`.
