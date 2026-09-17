#!/usr/bin/env python3
"""
Runs `ghc -M` to generate module dependencies and converts the output to a JSON DAG.
Usage:
  ghc_dep_scan.py --out deps.json -- [ghc args...]
"""

import json
import os
import subprocess
import sys
import tempfile

def main():
    if "--out" not in sys.argv:
        print("Usage: ghc_dep_scan.py --out <output.json> -- [ghc args...]", file=sys.stderr)
        sys.exit(1)

    out_idx = sys.argv.index("--out")
    out_file = sys.argv[out_idx + 1]

    if "--" in sys.argv:
        sep_idx = sys.argv.index("--")
        ghc_args = sys.argv[sep_idx + 1:]
    else:
        ghc_args = sys.argv[out_idx + 2:]

    extra_inc = []
    for arg in ghc_args:
        if arg.endswith(".hs") or arg.endswith(".lhs"):
            parent = os.path.dirname(arg)
            if parent and f"-I{parent}" not in extra_inc:
                extra_inc.append(f"-I{parent}")

    with tempfile.NamedTemporaryFile(mode="w+", delete=False, suffix=".mk") as tmp:
        tmp_path = tmp.name

    try:
        cmd = ["ghc", "-M", "-dep-makefile", tmp_path] + extra_inc + ghc_args
        res = subprocess.run(cmd, capture_output=True, text=True)
        if res.returncode != 0:
            sys.stderr.write(res.stdout)
            sys.stderr.write(res.stderr)
            sys.exit(res.returncode)

        with open(tmp_path, "r", encoding="utf-8") as f:
            content = f.read()

        nodes = {}
        for line in content.splitlines():
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            if ":" not in line:
                continue
            target, prereq = [x.strip() for x in line.split(":", 1)]
            target_mod = target[:-2] if target.endswith(".o") else target
            if target_mod not in nodes:
                nodes[target_mod] = {"src": None, "deps": []}
            if prereq.endswith(".hs") or prereq.endswith(".lhs"):
                nodes[target_mod]["src"] = prereq
            elif prereq.endswith(".hi"):
                dep_mod = prereq[:-3]
                if dep_mod not in nodes[target_mod]["deps"]:
                    nodes[target_mod]["deps"].append(dep_mod)

        # Filter out nodes without source file (if any)
        result = {k: v for k, v in nodes.items() if v["src"]}

        with open(out_file, "w", encoding="utf-8") as f:
            json.dump(result, f, indent=2)

    finally:
        if os.path.exists(tmp_path):
            os.remove(tmp_path)

if __name__ == "__main__":
    main()
