#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import os
import subprocess
import sys
from pathlib import Path


FRONTEND_ROOT = Path(__file__).resolve().parents[1]
PROVENANCE_ROOT = FRONTEND_ROOT / "env" / "runtime"
if str(PROVENANCE_ROOT) not in sys.path:
    sys.path.insert(0, str(PROVENANCE_ROOT))

from artifact_provenance import _OBSERVABILITY_SOURCE_ALLOWLIST, write_frontend_build_manifest


# These paths can change the generated Frontend DUT or the toolchain inputs
# used to build it. Verification-side edits are tracked independently by the
# funcov registry/sampler/testcase hashes and do not require a DUT rebuild.
_DUT_INPUT_PATHS = (
    "Makefile",
    "build.sc",
    "build.mill",
    "build.sbt",
    "mill",
    ".mill-version",
    "project/",
    "src/main/scala/",
    "src/main/resources/",
    "rocket-chip/",
    "hardfloat/",
    "XSCore/",
    "coupledL2/",
    "utility/",
)


def _worktree_git_context(repo_root: Path) -> tuple[Path, Path, Path] | None:
    """Return (common git dir, worktree index, admin dir) for a linked worktree."""
    gitfile = repo_root / ".git"
    if not gitfile.is_file():
        return None
    first_line = gitfile.read_text(encoding="utf-8").splitlines()[0].strip()
    if not first_line.startswith("gitdir:"):
        return None
    admin_dir = Path(first_line.split(":", 1)[1].strip())
    if not admin_dir.is_absolute():
        admin_dir = (repo_root / admin_dir).resolve()
    commondir_file = admin_dir / "commondir"
    if commondir_file.is_file():
        common_dir = Path(commondir_file.read_text(encoding="utf-8").strip())
        if not common_dir.is_absolute():
            common_dir = (admin_dir / common_dir).resolve()
    else:
        common_dir = admin_dir
    return common_dir, admin_dir / "index", admin_dir


def _git_invocation(repo_root: Path) -> tuple[list[str], dict[str, str] | None]:
    context = _worktree_git_context(repo_root)
    if context is None:
        return ["git"], None
    common_dir, index_file, _ = context
    env = os.environ.copy()
    env["GIT_INDEX_FILE"] = str(index_file)
    return ["git", "--git-dir", str(common_dir), "--work-tree", str(repo_root)], env


def _worktree_head(repo_root: Path) -> str:
    context = _worktree_git_context(repo_root)
    if context is None:
        return _git(repo_root, "rev-parse", "HEAD")
    _, _, admin_dir = context
    head = (admin_dir / "HEAD").read_text(encoding="utf-8").strip()
    if head.startswith("ref: "):
        return _git(repo_root, "rev-parse", head[5:].strip())
    return _git(repo_root, "rev-parse", head)


def _git(repo_root: Path, *args: str) -> str:
    command, env = _git_invocation(repo_root)
    result = subprocess.run(
        [*command, *args],
        cwd=str(repo_root),
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        env=env,
    )
    return result.stdout.strip()


def _git_bytes(repo_root: Path, *args: str) -> bytes:
    command, env = _git_invocation(repo_root)
    result = subprocess.run(
        [*command, *args],
        cwd=str(repo_root),
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=env,
    )
    return bytes(result.stdout)


def _is_dut_input_path(path: str) -> bool:
    normalized = path.strip().lstrip("./")
    return any(
        normalized == prefix or normalized.startswith(prefix)
        for prefix in _DUT_INPUT_PATHS
    )


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
    implementation_sha = _worktree_head(repo_root)
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
    # Generated/downloaded files inside submodules, such as the local NEMU
    # executable under ready-to-run, are not part of the superproject source
    # baseline and must not invalidate a commit-pinned DUT manifest.
    status_output = _git(
        repo_root,
        "status",
        # Use the unversioned spelling for compatibility with older Git
        # releases that do not recognize the versioned form.
        "--porcelain",
        "--untracked-files=all",
        "--ignore-submodules=untracked",
    )
    status_entries = []
    for line in status_output.splitlines():
        path = line[3:].split(" -> ", 1)[-1] if len(line) >= 4 else ""
        # Build/run outputs are audit data, not source inputs to the DUT.
        if path.startswith("data/runs/") or path.startswith("build-frontend/"):
            continue
        if _is_dut_input_path(path):
            status_entries.append(line)
    source_tree_dirty = bool(status_entries)
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
