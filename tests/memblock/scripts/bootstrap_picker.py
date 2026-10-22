#!/usr/bin/env python3
"""Build the locked XS-MLVP Picker C++ toolchain in an ignored directory."""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path


DEFAULT_LOCK = Path(__file__).resolve().parents[1] / "toolchain.lock.json"
COMMIT_PATTERN = re.compile(r"^[0-9a-f]{40}$")


class BootstrapError(RuntimeError):
    pass


def run(command: list[str], **kwargs) -> str:
    result = subprocess.run(command, check=True, text=True, **kwargs)
    return result.stdout.strip() if result.stdout else ""


def load_toolchain(path: Path = DEFAULT_LOCK) -> dict[str, dict[str, object]]:
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise BootstrapError(f"cannot read toolchain lock {path}: {error}") from error
    if not isinstance(document, dict):
        raise BootstrapError("toolchain lock must contain a JSON object")
    if document.get("schema_version") != 1:
        raise BootstrapError("unsupported toolchain lock schema")
    tools: dict[str, dict[str, object]] = {}
    for name in ("picker", "xcomm", "toffee", "toffee_test"):
        entry = document.get(name)
        if not isinstance(entry, dict):
            raise BootstrapError(f"toolchain lock has no {name} entry")
        for field in ("repository", "ref", "commit"):
            if not isinstance(entry.get(field), str) or not entry[field]:
                raise BootstrapError(f"toolchain lock {name}.{field} is invalid")
        if not COMMIT_PATTERN.fullmatch(entry["commit"]):
            raise BootstrapError(f"toolchain lock {name}.commit is not a full SHA-1")
        tools[name] = entry
    return tools


def checkout_revision(
    root: Path,
    label: str,
    ref: str,
    expected_commit: str | None = None,
) -> str:
    if not (root / ".git").is_dir():
        raise BootstrapError(f"{label} destination is not a git checkout: {root}")
    dirty = run(["git", "status", "--porcelain"], cwd=root, capture_output=True)
    if dirty:
        raise BootstrapError(f"refusing to overwrite a dirty {label} checkout: {root}")
    run(["git", "fetch", "origin", "--tags", "--prune"], cwd=root)
    commit = run(
        ["git", "rev-parse", "--verify", f"{ref}^{{commit}}"],
        cwd=root,
        capture_output=True,
    )
    if expected_commit and commit != expected_commit:
        raise BootstrapError(
            f"{label} ref {ref} resolves to {commit}, expected {expected_commit}"
        )
    run(["git", "checkout", "--detach", commit], cwd=root)
    return commit


def ensure_checkout(
    root: Path,
    label: str,
    repository: str,
    ref: str,
    expected_commit: str | None,
) -> str:
    if not root.exists():
        root.parent.mkdir(parents=True, exist_ok=True)
        run(["git", "clone", "--filter=blob:none", repository, str(root)])
    return checkout_revision(root, label, ref, expected_commit)


def recorded_revisions(metadata: Path) -> tuple[str, str]:
    try:
        document = json.loads(metadata.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError):
        return "", ""
    if not isinstance(document, dict):
        return "", ""
    picker = document.get("commit")
    xcomm = document.get("xcomm_commit")
    return (
        picker if isinstance(picker, str) else "",
        xcomm if isinstance(xcomm, str) else "",
    )


def write_if_changed(path: Path, contents: str) -> bool:
    try:
        if path.read_text(encoding="utf-8") == contents:
            return False
    except (OSError, UnicodeDecodeError):
        pass
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(contents, encoding="utf-8")
    return True


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
    parser.add_argument("--lock", type=Path, default=DEFAULT_LOCK)
    parser.add_argument("--picker-ref")
    parser.add_argument("--xcomm-ref")
    parser.add_argument("--jobs", type=int, default=8)
    args = parser.parse_args()
    try:
        if args.jobs < 1:
            raise BootstrapError("--jobs must be positive")
        tools = load_toolchain(args.lock)
        picker_spec = tools["picker"]
        xcomm_spec = tools["xcomm"]
        root = args.root.resolve()
        picker_ref = args.picker_ref or str(picker_spec["ref"])
        xcomm_ref = args.xcomm_ref or str(xcomm_spec["ref"])
        picker_commit = ensure_checkout(
            root,
            "Picker",
            str(picker_spec["repository"]),
            picker_ref,
            None if args.picker_ref else str(picker_spec["commit"]),
        )
        environment = os.environ.copy()
        environment["BUILD_XSPCOMM_SWIG"] = ""
        swig = local_swig(root.parent, environment)
        run(["make", "init"], cwd=root, env=environment)
        xcomm = root / "dependence/xcomm"
        xcomm_commit = checkout_revision(
            xcomm,
            "xcomm",
            xcomm_ref,
            None if args.xcomm_ref else str(xcomm_spec["commit"]),
        )
        if recorded_revisions(args.metadata) != (picker_commit, xcomm_commit):
            shutil.rmtree(root / "build", ignore_errors=True)
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
        version = run([str(picker), "--version"], capture_output=True)
        metadata = {
            "schema_version": 2,
            "repository": str(picker_spec["repository"]),
            "ref": picker_ref,
            "commit": picker_commit,
            "xcomm_repository": str(xcomm_spec["repository"]),
            "xcomm_ref": xcomm_ref,
            "xcomm_commit": xcomm_commit,
            "picker": str(picker),
            "version": version.splitlines()[-1],
            "swig": str(swig),
            "jobs": args.jobs,
        }
        write_if_changed(
            args.metadata,
            json.dumps(metadata, indent=2, sort_keys=True) + "\n",
        )
    except (BootstrapError, OSError, subprocess.CalledProcessError) as error:
        print(f"bootstrap_picker.py: error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
