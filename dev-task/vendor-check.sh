#!/usr/bin/env bash
set -euo pipefail

vendor_root=${DEVENV_ROOT:-.}/vendor

test -d "$vendor_root"
repository_root=$(cd "$vendor_root/.." && pwd)

expected_dirs=(
  ansi-regex-6.3.0
  ansi-styles-6.2.3
  better-sqlite3-11.1.2
  chromium-bidi-17.0.2
  cliui-9.0.1
  devtools-protocol-0.0.1666840
  esbuild-0.19.5
  escalade-3.2.0
  get-caller-file-2.0.5
  get-east-asian-width-1.6.0
  gleam_stdlib-1.0.5
  lilconfig-3.1.3
  mitt-3.0.1
  modern-tar-0.8.4
  puppeteer-25.9.0
  sqlite3-3.46.0
  string-width-8.2.2
  strip-ansi-7.2.0
  webdriver-bidi-protocol-0.4.2
  wrap-ansi-9.0.2
  ws-8.21.3
  y18n-5.0.8
  yargs-18.1.0
  yargs-parser-22.0.0
  zod-3.25.76
)

test -f "$vendor_root/dup.toml"
python3 - "$vendor_root" <<'PY'
import sys
import re
import tomllib
from collections import defaultdict
from pathlib import Path

vendor_root = Path(sys.argv[1])
ledger = tomllib.loads((vendor_root / "dup.toml").read_text())
if ledger.get("version") != 1:
    raise SystemExit("vendor/dup.toml must declare version = 1")

allowed_top_keys = {"version", "duplicate"}
extra_top_keys = set(ledger.keys()) - allowed_top_keys
if extra_top_keys:
    raise SystemExit(f"vendor/dup.toml contains invalid top-level keys: {', '.join(sorted(extra_top_keys))}")

snapshots = {}
snapshot_groups = defaultdict(list)
dir_pattern = re.compile(r"^(.+?)-(?:\d.*)$")

for directory in vendor_root.iterdir():
    if not directory.is_dir():
        continue
    metadata_path = directory / ".vendor.toml"
    if not metadata_path.is_file():
        raise SystemExit(f"{directory} is missing .vendor.toml")
    metadata = tomllib.loads(metadata_path.read_text())
    if set(metadata.keys()) != {"url", "ref"}:
        raise SystemExit(f"{metadata_path} must contain only url and ref")
    url = metadata.get("url")
    if not isinstance(url, str) or not url:
        raise SystemExit(f"{metadata_path} must contain a non-empty url")
    ref = metadata.get("ref")
    if not isinstance(ref, str) or not re.fullmatch(r"[0-9a-f]{40}", ref):
        raise SystemExit(f"{metadata_path} must contain a full 40-character Git ref")
    snapshots[directory.name] = url

    m = dir_pattern.match(directory.name)
    if not m:
        raise SystemExit(f"{directory.name} does not match '<name>-<version>' format")
    snapshot_groups[m.group(1)].append(directory.name)

actual_duplicate_groups = {
    name: sorted(dirs) for name, dirs in snapshot_groups.items() if len(dirs) > 1
}

duplicates = ledger.get("duplicate", [])
if not isinstance(duplicates, list):
    raise SystemExit("vendor/dup.toml [[duplicate]] must be an array of tables")

ledger_names = []
for entry in duplicates:
    if not isinstance(entry, dict):
        raise SystemExit("vendor/dup.toml [[duplicate]] entry must be a table")
    required = ("name", "snapshots", "consumers", "justification")
    missing = [key for key in required if not entry.get(key)]
    if missing:
        raise SystemExit(f"duplicate entry {entry.get('name', '<unnamed>')} is missing required fields: {', '.join(missing)}")
    
    invalid_keys = set(entry.keys()) - set(required)
    if invalid_keys:
        raise SystemExit(f"duplicate entry '{entry.get('name')}' contains invalid keys: {', '.join(sorted(invalid_keys))}")
    
    name = entry["name"]
    if not isinstance(name, str) or not name:
        raise SystemExit("duplicate entry name must be a non-empty string")
    if name in ledger_names:
        raise SystemExit(f"duplicate entry '{name}' declared more than once in vendor/dup.toml")
    if name not in actual_duplicate_groups:
        raise SystemExit(f"invalid duplicate entry '{name}': does not correspond to any duplicated snapshot directory in vendor/")
    
    listed = entry["snapshots"]
    if not isinstance(listed, list) or len(listed) < 2 or len(set(listed)) != len(listed):
        raise SystemExit(f"invalid duplicate entry '{name}': snapshots must be a list of at least two unique snapshot directory names")
    if any(s not in snapshots for s in listed):
        raise SystemExit(f"invalid duplicate entry '{name}': lists a snapshot not present in vendor/")
    if set(listed) != set(actual_duplicate_groups[name]):
        raise SystemExit(f"invalid duplicate entry '{name}': snapshots list {listed} does not match actual retained snapshots {actual_duplicate_groups[name]}")
    
    consumers = entry["consumers"]
    if not isinstance(consumers, list) or not consumers:
        raise SystemExit(f"invalid duplicate entry '{name}': consumers must be a non-empty list of consumer strings")
    if not all(isinstance(c, str) and c.strip() for c in consumers):
        raise SystemExit(f"invalid duplicate entry '{name}': consumer entries must be non-empty strings")
    
    justification = entry["justification"]
    if not isinstance(justification, str) or len(justification.strip()) < 20:
        raise SystemExit(f"invalid duplicate entry '{name}': justification is missing or too brief")
    if "minor vendored source changes needed" in justification.lower():
        raise SystemExit(f"invalid duplicate entry '{name}': uses a non-justification placeholder")
    
    ledger_names.append(name)

missing_entries = set(actual_duplicate_groups.keys()) - set(ledger_names)
if missing_entries:
    raise SystemExit(f"vendor/dup.toml is missing entries for duplicated snapshots: {', '.join(sorted(missing_entries))}")
PY

python3 - "$vendor_root" <<'PY'
import sys
import tomllib
from pathlib import Path

vendor_root = Path(sys.argv[1])
repository_root = vendor_root.parent
ignored_parts = {".git", ".tmp", "build", "node_modules"}
errors = []

for path in repository_root.rglob("gleam.toml"):
    if ignored_parts.intersection(path.parts):
        continue
    config = tomllib.loads(path.read_text())
    for section_name in ("dependencies", "dev-dependencies"):
        dependencies = config.get(section_name, {})
        if not isinstance(dependencies, dict):
            errors.append(f"{path}: [{section_name}] must be a table")
            continue
        for name, specification in dependencies.items():
            if isinstance(specification, str):
                errors.append(
                    f"{path}: {section_name}.{name} uses a registry version constraint"
                )
            elif not isinstance(specification, dict) or not isinstance(
                specification.get("path"), str
            ):
                errors.append(
                    f"{path}: {section_name}.{name} must use a local path dependency"
                )

for path in repository_root.rglob("manifest.toml"):
    if ignored_parts.intersection(path.parts):
        continue
    manifest = tomllib.loads(path.read_text())
    for package in manifest.get("packages", []):
        if package.get("source") != "local":
            errors.append(
                f"{path}: {package.get('name', '<unnamed>')} has non-local source "
                f"{package.get('source', '<missing>')}"
            )

if errors:
    raise SystemExit("Gleam dependencies must all be local paths:\n" + "\n".join(errors))
PY

python3 - "$vendor_root" <<'PY'
import json
import re
import subprocess
import sys
import tomllib
from pathlib import Path

vendor_root = Path(sys.argv[1]).resolve()
repository_root = vendor_root.parent
root_manifest_path = repository_root / "Cargo.toml"
root_manifest = tomllib.loads(root_manifest_path.read_text())
errors = []

def dependency_tables(manifest):
    for section in ("dependencies", "dev-dependencies", "build-dependencies"):
        yield section, manifest.get(section, {})
    for target_name, target in manifest.get("target", {}).items():
        if not isinstance(target, dict):
            continue
        for section in ("dependencies", "dev-dependencies", "build-dependencies"):
            yield f"{target_name}.{section}", target.get(section, {})

workspace_dependencies = root_manifest.get("workspace", {}).get("dependencies", {})
for name, specification in workspace_dependencies.items():
    if not isinstance(specification, dict):
        errors.append(
            f"{root_manifest_path}: workspace.dependencies.{name} must use a version constraint or local path"
        )
        continue
    if any(key in specification for key in ("git", "registry")):
        errors.append(f"{root_manifest_path}: workspace.dependencies.{name} has a non-local source")
        continue
    if isinstance(specification.get("path"), str):
        resolved = (repository_root / specification["path"]).resolve()
        if not (resolved / "Cargo.toml").is_file() or resolved.is_relative_to(vendor_root):
            errors.append(
                f"{root_manifest_path}: workspace.dependencies.{name} must not directly reference a vendored package"
            )
    elif "version" not in specification:
        errors.append(
            f"{root_manifest_path}: workspace.dependencies.{name} must use a version constraint or local path"
        )

patch_table = root_manifest.get("patch", {}).get("crates-io", {})
if not isinstance(patch_table, dict):
    errors.append(f"{root_manifest_path}: [patch.crates-io] must be a table")
    patch_table = {}

patch_packages = {}
for alias, specification in patch_table.items():
    if not isinstance(specification, dict):
        errors.append(f"{root_manifest_path}: patch.crates-io.{alias} must use a local path")
        continue
    if any(key in specification for key in ("git", "registry")):
        errors.append(f"{root_manifest_path}: patch.crates-io.{alias} has a non-local source")
        continue
    patch_path = specification.get("path")
    if not isinstance(patch_path, str):
        errors.append(f"{root_manifest_path}: patch.crates-io.{alias} must use a local path")
        continue
    resolved = (repository_root / patch_path).resolve()
    manifest = resolved / "Cargo.toml"
    if not resolved.is_relative_to(vendor_root) or not manifest.is_file():
        errors.append(f"{root_manifest_path}: patch.crates-io.{alias} must point into vendor")
        continue
    package = tomllib.loads(manifest.read_text()).get("package", {})
    identity = (package.get("name"), package.get("version"))
    if not all(isinstance(part, str) for part in identity):
        errors.append(f"{root_manifest_path}: patch.crates-io.{alias} points to an invalid Cargo package")
        continue
    if "package" in specification and specification["package"] != identity[0]:
        errors.append(f"{root_manifest_path}: patch.crates-io.{alias} has a mismatched package name")
    if identity in patch_packages:
        errors.append(f"{root_manifest_path}: patch.crates-io maps {identity} more than once")
    patch_packages[identity] = manifest.resolve()

metadata_process = subprocess.run(
    ["cargo", "metadata", "--locked", "--offline", "--format-version=1"],
    cwd=repository_root,
    check=False,
    capture_output=True,
    text=True,
)
if metadata_process.returncode != 0:
    raise SystemExit(
        "Cargo metadata must resolve the locked offline local closure:\n"
        + metadata_process.stderr
    )
metadata = json.loads(metadata_process.stdout)

def check_manifest(path, package_name, allow_version_dependencies=False):
    manifest = tomllib.loads(path.read_text())
    for section, dependencies in dependency_tables(manifest):
        if not isinstance(dependencies, dict):
            errors.append(f"{path}: [{section}] must be a table")
            continue
        for name, specification in dependencies.items():
            if not isinstance(specification, dict):
                if allow_version_dependencies and isinstance(specification, str):
                    continue
                errors.append(f"{path}: {section}.{name} must use a local path dependency")
                continue
            if any(key in specification for key in ("git", "registry")):
                errors.append(f"{path}: {section}.{name} has a non-local source")
                continue
            if specification.get("workspace") is True:
                if name not in workspace_dependencies:
                    errors.append(f"{path}: {section}.{name} references an unknown workspace dependency")
                continue
            dependency_path = specification.get("path")
            if not isinstance(dependency_path, str):
                if allow_version_dependencies and "version" in specification:
                    continue
                errors.append(f"{path}: {section}.{name} must use a local path dependency")
                continue
            resolved = (path.parent / dependency_path).resolve()
            if not resolved.is_relative_to(repository_root) or not (resolved / "Cargo.toml").is_file():
                errors.append(f"{path}: {section}.{name} points outside the local Cargo closure")

active_vendored_manifests = set()
for package in metadata["packages"]:
    path = Path(package["manifest_path"]).resolve()
    if path.is_relative_to(vendor_root):
        active_vendored_manifests.add(path)
        check_manifest(path, package["name"], allow_version_dependencies=True)
    elif path.parent == repository_root or path.parent in {
        (repository_root / member).resolve() for member in root_manifest["workspace"]["members"]
    }:
        check_manifest(path, package["name"])
    else:
        errors.append(f"{path}: Cargo package is outside the repository or vendor closure")

lock = tomllib.loads((repository_root / "Cargo.lock").read_text())
lock_packages = {(p["name"], p["version"]): p for p in lock.get("package", [])}
if any(p.get("source", "").startswith(("registry+", "git+")) for p in lock.get("package", [])):
    errors.append("Cargo.lock contains a registry or Git package source")

metadata_packages = {
    (package["name"], package["version"]): Path(package["manifest_path"]).resolve()
    for package in metadata["packages"]
}
if any(package.get("source") for package in metadata["packages"]):
    errors.append("cargo metadata contains a registry or Git package source")
if set(lock_packages) != set(metadata_packages):
    errors.append("Cargo.lock and cargo metadata do not describe the same package identities")

active_patch_packages = {
    identity: manifest
    for identity, manifest in metadata_packages.items()
    if manifest.is_relative_to(vendor_root)
}
if set(patch_packages) != set(active_patch_packages):
    errors.append("[patch.crates-io] must contain exactly the active vendored Cargo packages")
for identity, manifest in active_patch_packages.items():
    if patch_packages.get(identity) != manifest:
        errors.append(f"[patch.crates-io] does not point {identity} to its active vendor manifest")

if errors:
    raise SystemExit("Cargo dependency sources must resolve through local root patches:\n" + "\n".join(errors))
PY

for name in "${expected_dirs[@]}"; do
  dir="$vendor_root/$name"
  test -d "$dir"
  test -f "$dir/.vendor.toml"
  ref=$(sed -nE 's/^ref = "([0-9a-f]{40})"$/\1/p' "$dir/.vendor.toml")
  test "$ref" != ""
done

while IFS= read -r path; do
  case "$path" in
    */.git|*/.git/*|*/node_modules|*/node_modules/*) exit 1 ;;
  esac
done < <(find "$vendor_root" -type d \( -name .git -o -name node_modules \) -print)

if test -n "$(find "$vendor_root" -type f -name .gitattributes -print -quit)"; then
  echo "vendored snapshots must not contain .gitattributes files" >&2
  exit 1
fi

untracked_vendor_files=$(
  {
    git -C "$repository_root" ls-files -o --exclude-standard -- vendor
    git -C "$repository_root" ls-files -o --ignored --exclude-standard -- vendor
  } | sort -u
)
if test -n "$untracked_vendor_files"; then
  echo "vendor contains untracked files; remove generated output or add tracked upstream files:" >&2
  printf '%s\n' "$untracked_vendor_files" >&2
  exit 1
fi

if test -n "$(find "$vendor_root" \( -name AGENTS.md -o -name CLAUDE.md -o -name CODEX.md -o -name SKILL.md -o -name .agents -o -name .claude \) -print -quit)"; then
  exit 1
fi

test -f "$vendor_root/puppeteer-25.9.0/runtime/puppeteer.mjs"
test -f "$vendor_root/gleam_stdlib-1.0.5/gleam.toml"
test -f "$vendor_root/gleam_stdlib-1.0.5/src/gleam/io.gleam"
test -f "$vendor_root/gleam_stdlib-1.0.5/src/gleam_stdlib.mjs"
test -f "$vendor_root/sqlite3-3.46.0/sqlite3.c"
test -f "$vendor_root/sqlite3-3.46.0/sqlite3.h"
test -f "$vendor_root/sqlite3-3.46.0/sqlite3ext.h"
vendor_snapshot_count=$(find "$vendor_root" -mindepth 2 -maxdepth 2 -name .vendor.toml -print | wc -l)
echo "vendor closure: $vendor_snapshot_count snapshots"
