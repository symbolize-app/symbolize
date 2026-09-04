#!/usr/bin/env bash
set -euo pipefail

vendor_root=${DEVENV_ROOT:-.}/vendor

test -d "$vendor_root"

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
import tomllib
from collections import defaultdict
from pathlib import Path

vendor_root = Path(sys.argv[1])
ledger = tomllib.loads((vendor_root / "dup.toml").read_text())
if ledger.get("version") != 1:
    raise SystemExit("vendor/dup.toml must declare version = 1")

snapshots = {}
by_url = defaultdict(list)
for directory in vendor_root.iterdir():
    if not directory.is_dir():
        continue
    metadata_path = directory / ".vendor.toml"
    if not metadata_path.is_file():
        continue
    metadata = tomllib.loads(metadata_path.read_text())
    url = metadata.get("url")
    if not isinstance(url, str) or not url:
        raise SystemExit(f"{metadata_path} must contain a non-empty url")
    snapshots[directory.name] = url
    by_url[url].append(directory.name)

duplicates = ledger.get("duplicate", [])
actual_duplicate_sets = {
    frozenset(names) for names in by_url.values() if len(names) > 1
}
ledger_duplicate_sets = []
for entry in duplicates:
    required = ("name", "snapshots", "consumers", "justification")
    missing = [key for key in required if not entry.get(key)]
    if missing:
        raise SystemExit(f"duplicate entry {entry.get('name', '<unnamed>')} is missing: {', '.join(missing)}")
    if "minor vendored source changes needed" in entry["justification"].lower():
        raise SystemExit(f"duplicate entry {entry['name']} uses a non-justification placeholder")
    listed = entry["snapshots"]
    if len(listed) < 2 or len(set(listed)) != len(listed):
        raise SystemExit(f"duplicate entry {entry['name']} must list at least two distinct snapshots")
    if any(snapshot not in snapshots for snapshot in listed):
        raise SystemExit(f"duplicate entry {entry['name']} lists a snapshot outside the vendor closure")
    urls = {snapshots[snapshot] for snapshot in listed}
    if len(urls) != 1:
        raise SystemExit(f"duplicate entry {entry['name']} must list snapshots from one upstream URL")
    listed_set = frozenset(listed)
    if listed_set not in actual_duplicate_sets:
        raise SystemExit(f"duplicate entry {entry['name']} does not match a duplicated upstream URL")
    ledger_duplicate_sets.append(listed_set)

if len(ledger_duplicate_sets) != len(set(ledger_duplicate_sets)) or set(ledger_duplicate_sets) != actual_duplicate_sets:
    raise SystemExit("vendor/dup.toml must contain exactly one entry for every duplicated upstream URL")
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

for name in "${expected_dirs[@]}"; do
  dir="$vendor_root/$name"
  test -d "$dir"
  test -f "$dir/.vendor.toml"
  ref=$(sed -nE 's/^ref = "([0-9a-f]{40})"$/\1/p' "$dir/.vendor.toml")
  test "$ref" != ""
done

while IFS= read -r dir; do
  name=$(basename "$dir")
  found=false
  for expected in "${expected_dirs[@]}"; do
    if test "$name" = "$expected"; then
      found=true
      break
    fi
  done
  test "$found" = true
done < <(find "$vendor_root" -mindepth 1 -maxdepth 1 -type d -print)

while IFS= read -r path; do
  case "$path" in
    */.git|*/.git/*|*/node_modules|*/node_modules/*) exit 1 ;;
  esac
done < <(find "$vendor_root" -type d \( -name .git -o -name node_modules \) -print)

while IFS= read -r path; do
  case "$(basename "$path")" in
    AGENTS.md|CLAUDE.md|CODEX.md|SKILL.md|.agents|.claude) exit 1 ;;
  esac
done < <(find "$vendor_root" \( -type f -o -type d \) -print)

test -f "$vendor_root/puppeteer-25.9.0/runtime/puppeteer.mjs"
test -f "$vendor_root/gleam_stdlib-1.0.5/gleam.toml"
test -f "$vendor_root/gleam_stdlib-1.0.5/src/gleam/io.gleam"
test -f "$vendor_root/gleam_stdlib-1.0.5/src/gleam_stdlib.mjs"
test -f "$vendor_root/sqlite3-3.46.0/sqlite3.c"
test -f "$vendor_root/sqlite3-3.46.0/sqlite3.h"
test -f "$vendor_root/sqlite3-3.46.0/sqlite3ext.h"
echo "vendor closure: ${#expected_dirs[@]} snapshots"
