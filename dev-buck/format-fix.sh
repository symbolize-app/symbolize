#!/usr/bin/env bash
set -euo pipefail

diff_paths=$(buck2 bxl -m debug dev_buck//format.bxl:diff "$@")

applied=0
for diff_file in $diff_paths; do
  if [ -s "$diff_file" ]; then
    git apply "$diff_file"
    applied=$((applied + 1))
  fi
done

if [ "$applied" -gt 0 ]; then
  echo "Applied format diffs to $applied crate(s)."
fi
