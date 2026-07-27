#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import os
import subprocess
import sys
from pathlib import Path


FRONTEND_ROOT = Path(__file__).resolve().parents[1]
PROVENANCE_ROOT = FRONTEND_ROOT / "env"
if str(PROVENANCE_ROOT) not in sys.path:
    sys.path.insert(0, str(PROVENANCE_ROOT))

from artifact_provenance import _OBSERVABILITY_SOURCE_ALLOWLIST, write_frontend_build_manifest


def _git(repo_root: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", "-C", str(repo_root), *args],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    return result.stdout.strip()


def _git_bytes(repo_root: Path, *args: str) -> bytes:
    result = subprocess.run(
        ["git", "-C", str(repo_root), *args],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    return bytes(result.stdout)


def main() -> int:
    parser = argparse.ArgumentParser(description="Write a provenance manifest for a compiled Frontend DUT")
    parser.add_argument("--repo-root", type=Path, required=True)
    parser.add_argument("--build-root", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--sim", required=True, choices=("verilator", "vcs"))
    parser.add_argument("--build-config", required=True)
    parser.add_argument("--build-command", default="")
    parser.add_argument(
        "--dut-source-sha",
        default="",
        help="Frozen design SHA to bind to the DUT; defaults to FRONTEND_DUT_SOURCE_SHA or HEAD",
    )
    parser.add_argument(
        "--design-baseline-sha",
        default="",
        help="Optional semantic design baseline SHA (defaults to --dut-source-sha)",
    )
    args = parser.parse_args()

    repo_root = args.repo_root.resolve()
    implementation_sha = _git(repo_root, "rev-parse", "HEAD")
    source_sha = (
        str(args.dut_source_sha).strip()
        or os.environ.get("FRONTEND_DUT_SOURCE_SHA", "").strip()
        or implementation_sha
    )
    baseline_sha = str(args.design_baseline_sha).strip() or source_sha
    # Refuse an accidental typo or a non-commit override.  The build remains
    # tied to a real frozen revision even when observability-only files are
    # committed after that revision.
    _git(repo_root, "cat-file", "-e", f"{source_sha}^{{commit}}")
    _git(repo_root, "cat-file", "-e", f"{baseline_sha}^{{commit}}")
    delta_files = [
        item
        for item in _git(repo_root, "diff", "--name-only", f"{source_sha}..{implementation_sha}", "--", "src/main/scala").splitlines()
        if item.strip()
    ]
    unexpected_delta = sorted(set(delta_files) - set(_OBSERVABILITY_SOURCE_ALLOWLIST))
    source_override = source_sha != implementation_sha
    if source_override and unexpected_delta:
        raise RuntimeError(
            "source SHA override contains non-observability design files: "
            + ",".join(unexpected_delta)
        )
    delta = _git_bytes(
        repo_root,
        "diff",
        "--binary",
        f"{source_sha}..{implementation_sha}",
        "--",
        *delta_files,
    ) if delta_files else b""
    source_tree_dirty = bool(_git(repo_root, "status", "--porcelain=v1", "--untracked-files=all"))
    manifest = write_frontend_build_manifest(
        args.output,
        build_root=args.build_root,
        dut_source_sha=source_sha,
        source_tree_dirty=source_tree_dirty,
        build_config=args.build_config,
        build_command=args.build_command,
        simulator=args.sim,
        metadata={
            "implementation_sha": implementation_sha,
            "design_baseline_sha": baseline_sha,
            "source_sha_override": source_override,
            "source_delta_sha256": hashlib.sha256(delta).hexdigest(),
            "source_delta_files": delta_files,
            "source_delta_policy": "observability_only" if source_override else "none",
        },
    )
    print(
        "[frontend] build manifest: "
        f"{args.output} sim={manifest['simulator']} source_sha={manifest['dut_source_sha']} "
        f"dirty={manifest['source_tree_dirty']}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
