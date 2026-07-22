#!/usr/bin/env python3
from __future__ import annotations

import argparse
import subprocess
import sys
from pathlib import Path


FRONTEND_ROOT = Path(__file__).resolve().parents[1]
PROVENANCE_ROOT = FRONTEND_ROOT / "env"
if str(PROVENANCE_ROOT) not in sys.path:
    sys.path.insert(0, str(PROVENANCE_ROOT))

from artifact_provenance import write_frontend_build_manifest


def _git(repo_root: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", "-C", str(repo_root), *args],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    return result.stdout.strip()


def main() -> int:
    parser = argparse.ArgumentParser(description="Write a provenance manifest for a compiled Frontend DUT")
    parser.add_argument("--repo-root", type=Path, required=True)
    parser.add_argument("--build-root", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--build-config", required=True)
    parser.add_argument("--build-command", default="")
    args = parser.parse_args()

    repo_root = args.repo_root.resolve()
    source_sha = _git(repo_root, "rev-parse", "HEAD")
    source_tree_dirty = bool(_git(repo_root, "status", "--porcelain=v1", "--untracked-files=all"))
    manifest = write_frontend_build_manifest(
        args.output,
        build_root=args.build_root,
        dut_source_sha=source_sha,
        source_tree_dirty=source_tree_dirty,
        build_config=args.build_config,
        build_command=args.build_command,
    )
    print(
        "[frontend] build manifest: "
        f"{args.output} source_sha={manifest['dut_source_sha']} dirty={manifest['source_tree_dirty']}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
