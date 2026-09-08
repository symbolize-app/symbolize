#!/usr/bin/env bash
set -euo pipefail

root=${DEVENV_ROOT:-.}
mkdir -p "$root/build/vendor/puppeteer"

python3 - "$root" <<'PY'
import sys, subprocess, json
from pathlib import Path

root = Path(sys.argv[1]).resolve()
esbuild_bin = root / "build/vendor/esbuild/esbuild"
injected_ts = root / "vendor/puppeteer-25.9.0/packages/puppeteer-core/src/injected/injected.ts"
tmpl_path = root / "vendor/puppeteer-25.9.0/packages/puppeteer-core/src/templates/injected.ts.tmpl"
out_path = root / "build/vendor/puppeteer/injected.js"

code = subprocess.check_output([
    str(esbuild_bin),
    str(injected_ts),
    "--bundle",
    "--format=cjs",
    "--target=chrome125,firefox125",
    "--minify",
    "--legal-comments=none",
]).decode("utf-8")

tmpl = tmpl_path.read_text(encoding="utf-8")
out_path.write_text(tmpl.replace("SOURCE_CODE", json.dumps(code)), encoding="utf-8")
PY
