#!/usr/bin/env python3
"""Copy a Toffee DUT package into an immutable verification runtime directory."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import sys
from pathlib import Path
from typing import Any


REQUIRED_PACKAGE_FILES = ("__init__.py", "libUT_MemBlock.py")


class ToffeeRuntimeError(RuntimeError):
    pass


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def file_record(path: Path) -> dict[str, Any]:
    return {
        "path": str(path),
        "size": path.stat().st_size,
        "sha256": sha256(path),
    }


def remove_tree(path: Path) -> None:
    for root, directories, files in os.walk(path, topdown=False):
        for name in files:
            (Path(root) / name).chmod(0o644)
        for name in directories:
            directory = Path(root) / name
            directory.chmod(0o755)
        Path(root).chmod(0o755)
    path.chmod(0o755)
    shutil.rmtree(path)


def harden_package(path: Path) -> None:
    for root, directories, files in os.walk(path):
        for name in files:
            file_path = Path(root) / name
            mode = 0o555 if file_path.suffix == ".so" else 0o444
            file_path.chmod(mode)
        for name in directories:
            (Path(root) / name).chmod(0o555)
    path.chmod(0o555)


def harden_file(path: Path) -> None:
    path.chmod(0o444)


def freeze(
    dut_package: Path,
    project_source: Path,
    port_manifest: Path,
    environment_metadata: Path,
    rtl_metadata: Path,
    toolchain_lock: Path,
    project: Path,
    output: Path,
) -> Path:
    dut_package = dut_package.resolve()
    for required in REQUIRED_PACKAGE_FILES:
        if not (dut_package / required).is_file():
            raise ToffeeRuntimeError(f"Toffee DUT package is missing {required}: {dut_package}")

    if output.exists():
        remove_tree(output)
    package_output = output / "picker" / dut_package.name
    package_output.parent.mkdir(parents=True)
    shutil.copytree(dut_package, package_output)
    project_source = project_source.resolve()
    if not project_source.is_dir():
        raise ToffeeRuntimeError(f"Toffee project source is missing: {project_source}")
    source_output = output / "python"
    shutil.copytree(
        project_source,
        source_output,
        ignore=shutil.ignore_patterns("__pycache__", "*.pyc", "*.egg-info"),
    )
    port_manifest = port_manifest.resolve()
    if not port_manifest.is_file():
        raise ToffeeRuntimeError(f"port manifest is missing: {port_manifest}")
    port_output = output / "config/expected_ports.json"
    port_output.parent.mkdir(parents=True)
    shutil.copy2(port_manifest, port_output)

    environment_output = output / "environment.json"
    rtl_output = output / "rtl.json"
    lock_output = output / "toolchain.lock.json"
    shutil.copy2(environment_metadata, environment_output)
    shutil.copy2(rtl_metadata, rtl_output)
    shutil.copy2(toolchain_lock, lock_output)

    runtime_files = sorted(
        path
        for root in (package_output, source_output, port_output.parent)
        for path in root.rglob("*")
        if path.is_file()
    )
    runtime_files.extend([environment_output, rtl_output, lock_output])
    runtime_files.sort()
    environment = json.loads(environment_output.read_text(encoding="utf-8"))
    manifest = {
        "schema_version": 1,
        "profile": "toffee",
        "kind": "python-picker-dut",
        "package_name": dut_package.name,
        "package_root": str(package_output.parent),
        "source_root": str(source_output),
        "project_source": str(project_source),
        "port_manifest": file_record(port_output),
        "project": str(project.resolve()),
        "environment": environment,
        "rtl_metadata": file_record(rtl_output),
        "toolchain_lock": file_record(lock_output),
        "artifacts": [file_record(path) for path in runtime_files],
    }
    metadata_path = output / "runtime.json"
    temporary = metadata_path.with_suffix(".json.tmp")
    temporary.write_text(
        json.dumps(manifest, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    temporary.replace(metadata_path)
    harden_package(package_output)
    package_output.parent.chmod(0o555)
    harden_package(source_output)
    harden_package(port_output.parent)
    for sidecar in (environment_output, rtl_output, lock_output, metadata_path):
        harden_file(sidecar)
    output.chmod(0o555)
    return metadata_path


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dut-package", type=Path, required=True)
    parser.add_argument("--project-source", type=Path, required=True)
    parser.add_argument("--port-manifest", type=Path, required=True)
    parser.add_argument("--environment", type=Path, required=True)
    parser.add_argument("--rtl-metadata", type=Path, required=True)
    parser.add_argument("--toolchain-lock", type=Path, required=True)
    parser.add_argument("--project", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    try:
        print(
            freeze(
                args.dut_package,
                args.project_source,
                args.port_manifest,
                args.environment,
                args.rtl_metadata,
                args.toolchain_lock,
                args.project,
                args.output,
            )
        )
    except (OSError, json.JSONDecodeError, ToffeeRuntimeError) as error:
        print(f"freeze_toffee_runtime.py: error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
