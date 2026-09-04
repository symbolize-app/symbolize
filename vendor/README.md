# Vendor snapshots

This directory contains the transitive closure of checked-out snapshots of
upstream Git repositories that Symbolize depends on but that are not provided
by Devenv. Vendored dependencies are consumed by direct paths into these
snapshots; they are not installed by a package manager or linked through a
workspace.

Each immediate child directory represents one upstream repository:

```text
vendor/
└── <name>-<version-or-commit-date>/
    ├── .vendor.toml
    ├── <upstream repository files>
    └── optional runtime or build adapters
```

## Dependency closure

`vendor/` must contain every non-Devenv upstream dependency needed by the
vendored roots, recursively. A dependency must not rely on an undeclared
package-manager installation or on a dependency that exists only elsewhere in
the developer environment.

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

## Duplicate dependency ledger

`dup.toml` records the dependency identities for which the closure contains
more than one snapshot, together with the reason they have not been
normalized.

The ledger covers the immediate snapshot directories in `vendor/`. It does
not reinterpret package-manager lockfiles that are retained inside an
upstream repository snapshot as part of that repository's source history;
those lockfiles are not inputs to Symbolize's direct-path resolution.

The file uses this schema:

```toml
version = 1

[[duplicate]]
name = "example-library"
snapshots = ["example-library-1.2.3", "example-library-2.0.0"]
consumers = [
  "example-tool-4.0.0: ^1.2.0",
  "other-tool-7.0.0: ^2.0.0",
]
justification = "The consumers require incompatible APIs, and neither can be upgraded in this closure."
```

An unresolved `[[duplicate]]` entry must name every retained snapshot and its
consumers, and its `justification` must identify a concrete technical blocker
such as incompatible API or behavior, ABI/platform requirements, or an
unavailable compatible upstream release. A statement such as “minor vendored
source changes needed” is not sufficient. If no duplicate snapshots remain,
`dup.toml` must still exist with `version = 1` and no `[[duplicate]]` records.

## Requirements

- Keep one upstream repository per immediate child directory.
- Include the upstream name and package version or commit date in every
  snapshot directory name.
- Keep `.vendor.toml` alongside the snapshot it describes.
- Keep `dup.toml` up to date whenever adding, removing, or collapsing a
  snapshot with the same dependency identity as another snapshot.
- Do not put package-manager installations or Devenv state in this directory.
- A generated artifact is allowed only when it is a deliberate, documented
  part of the direct-path adapter. For example,
  `puppeteer-25.9.0/runtime/puppeteer.mjs` is the tested Node runtime bundle
  for Puppeteer; its `.vendor.toml` records the exact upstream source release.
  Build output for native or platform-specific tools belongs in `build/`, not
  in `vendor/`.
- Vendored subdirectories must not contain automatic-agent instruction files,
  including `AGENTS.md`, `CLAUDE.md`, or equivalent files with other names.

Devenv configuration and dependencies remain in `devenv.*` and
`dev-devenv/`; they do not belong in `vendor/`.

## Updating a snapshot

To update a vendored dependency, select the upstream Git commit, export that
commit without its `.git` directory into a new versioned child directory, and
write its full commit SHA and upstream URL to `.vendor.toml`. Recalculate the
root's transitive dependency closure and add every newly required upstream
repository unless it is supplied by Devenv. Update direct adapters and build
tasks as needed, then run `task vendor:check` and the full repository check.

Native outputs are rebuilt for the current platform from the exact snapshots:
esbuild is compiled with Go, and better-sqlite3 is compiled with node-gyp.
Puppeteer uses the committed runtime bundle and the Chromium executable
provided by Devenv; it does not download a browser or resolve a package from
`node_modules`.
