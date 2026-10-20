#!/usr/bin/env python3
"""Build the latest XS-MLVP Picker C++ toolchain in an ignored directory."""

from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
from pathlib import Path


PICKER_REPOSITORY = "https://github.com/XS-MLVP/picker.git"


class BootstrapError(RuntimeError):
    pass


def run(command: list[str], **kwargs) -> str:
    result = subprocess.run(command, check=True, text=True, **kwargs)
    return result.stdout.strip() if result.stdout else ""


def checkout_latest(root: Path, label: str) -> str:
    if not (root / ".git").is_dir():
        raise BootstrapError(f"{label} destination is not a git checkout: {root}")
    dirty = run(["git", "status", "--porcelain"], cwd=root, capture_output=True)
    if dirty:
        raise BootstrapError(f"refusing to overwrite a dirty {label} checkout: {root}")
    remote_head = run(
        ["git", "ls-remote", "--symref", "origin", "HEAD"],
        cwd=root,
        capture_output=True,
    ).splitlines()
    if not remote_head or not remote_head[0].startswith("ref: refs/heads/"):
        raise BootstrapError(f"cannot determine the default branch for {label}")
    branch = remote_head[0].removeprefix("ref: refs/heads/").split()[0]
    run(["git", "fetch", "origin", "--prune"], cwd=root)
    run(["git", "checkout", "--detach", f"origin/{branch}"], cwd=root)
    return run(["git", "rev-parse", "HEAD"], cwd=root, capture_output=True)


def ensure_checkout(root: Path) -> None:
    if not root.exists():
        root.parent.mkdir(parents=True, exist_ok=True)
        run(["git", "clone", "--filter=blob:none", PICKER_REPOSITORY, str(root)])
    checkout_latest(root, "Picker")


def local_swig(tool_root: Path, environment: dict[str, str]) -> Path:
    system_swig = shutil.which("swig")
    if system_swig:
        return Path(system_swig)

    apt_get = shutil.which("apt-get")
    dpkg_deb = shutil.which("dpkg-deb")
    if not apt_get or not dpkg_deb:
        raise BootstrapError(
            "Picker requires SWIG at configure time; install it or provide it on PATH"
        )
    package_dir = tool_root / "packages"
    extract_dir = tool_root / "swig-root"
    package_dir.mkdir(parents=True, exist_ok=True)
    if not extract_dir.exists():
        run([apt_get, "download", "swig"], cwd=package_dir)
        packages = sorted(package_dir.glob("swig_*.deb"))
        if not packages:
            raise BootstrapError("apt-get downloaded no swig package")
        extract_dir.mkdir(parents=True)
        run([dpkg_deb, "-x", str(packages[-1]), str(extract_dir)])
    candidates = sorted((extract_dir / "usr/bin").glob("swig*"))
    executables = [path for path in candidates if path.is_file() and os.access(path, os.X_OK)]
    libraries = sorted(extract_dir.glob("usr/share/swig*/swig.swg"))
    if not executables or not libraries:
        raise BootstrapError("locally extracted SWIG package is incomplete")
    environment["SWIG_LIB"] = str(libraries[-1].parent)
    return executables[-1]


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--metadata", type=Path, required=True)
    parser.add_argument("--jobs", type=int, default=8)
    args = parser.parse_args()
    try:
        if args.jobs < 1:
            raise BootstrapError("--jobs must be positive")
        root = args.root.resolve()
        ensure_checkout(root)
        environment = os.environ.copy()
        environment["BUILD_XSPCOMM_SWIG"] = ""
        swig = local_swig(root.parent, environment)
        run(["make", "init"], cwd=root, env=environment)
        xcomm = root / "dependence/xcomm"
        checkout_latest(xcomm, "xcomm")
        run(
            [
                "make",
                "build",
                f"NPROC={args.jobs}",
                f"ARGS=-DSWIG_EXECUTABLE={swig}",
            ],
            cwd=root,
            env=environment,
        )
        picker = root / "build/bin/picker"
        picker_commit = run(["git", "rev-parse", "HEAD"], cwd=root, capture_output=True)
        xcomm_commit = run(["git", "rev-parse", "HEAD"], cwd=xcomm, capture_output=True)
        version = run([str(picker), "--version"], capture_output=True)
        metadata = {
            "schema_version": 1,
            "repository": PICKER_REPOSITORY,
            "commit": picker_commit,
            "xcomm_commit": xcomm_commit,
            "picker": str(picker),
            "version": version.splitlines()[-1],
            "swig": str(swig),
            "jobs": args.jobs,
        }
        args.metadata.parent.mkdir(parents=True, exist_ok=True)
        args.metadata.write_text(json.dumps(metadata, indent=2, sort_keys=True) + "\n")
    except (BootstrapError, OSError, subprocess.CalledProcessError) as error:
        print(f"bootstrap_picker.py: error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
