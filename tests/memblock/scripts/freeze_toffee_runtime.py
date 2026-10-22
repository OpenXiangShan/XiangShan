#!/usr/bin/env python3
"""Copy a Toffee DUT package into an immutable verification runtime directory."""

from __future__ import annotations

import argparse
import ctypes
import errno
import hashlib
import json
import os
import shutil
import sys
import tempfile
from pathlib import Path
from typing import Any


REQUIRED_PACKAGE_FILES = ("__init__.py", "libUT_MemBlock.py")
AT_FDCWD = -100
RENAME_EXCHANGE = 2


class ToffeeRuntimeError(RuntimeError):
    pass


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def file_record(path: Path, root: Path) -> dict[str, Any]:
    return {
        "path": path.relative_to(root).as_posix(),
        "size": path.stat().st_size,
        "sha256": sha256(path),
    }


def tree_sha256(root: Path) -> str:
    digest = hashlib.sha256()
    for path in sorted(item for item in root.rglob("*") if item.is_file()):
        digest.update(path.relative_to(root).as_posix().encode("utf-8"))
        digest.update(b"\0")
        digest.update(path.read_bytes())
        digest.update(b"\0")
    return digest.hexdigest()


def runtime_identity(records: list[dict[str, Any]]) -> str:
    canonical = json.dumps(
        sorted(records, key=lambda record: str(record["path"])),
        separators=(",", ":"),
        sort_keys=True,
    )
    return hashlib.sha256(canonical.encode("utf-8")).hexdigest()


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


def exchange_directories(left: Path, right: Path) -> bool:
    """Atomically exchange sibling directories when the platform supports it."""

    libc = ctypes.CDLL(None, use_errno=True)
    renameat2 = getattr(libc, "renameat2", None)
    if renameat2 is None:
        return False
    renameat2.argtypes = (
        ctypes.c_int,
        ctypes.c_char_p,
        ctypes.c_int,
        ctypes.c_char_p,
        ctypes.c_uint,
    )
    renameat2.restype = ctypes.c_int
    result = renameat2(
        AT_FDCWD,
        os.fsencode(left),
        AT_FDCWD,
        os.fsencode(right),
        RENAME_EXCHANGE,
    )
    if result == 0:
        return True
    error_number = ctypes.get_errno()
    if error_number in (errno.ENOSYS, errno.EINVAL, errno.EOPNOTSUPP, errno.EXDEV):
        return False
    raise OSError(error_number, os.strerror(error_number), str(left))


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

    project_source = project_source.resolve()
    if not project_source.is_dir():
        raise ToffeeRuntimeError(f"Toffee project source is missing: {project_source}")
    port_manifest = port_manifest.resolve()
    if not port_manifest.is_file():
        raise ToffeeRuntimeError(f"port manifest is missing: {port_manifest}")
    for label, path in (
        ("environment metadata", environment_metadata),
        ("RTL metadata", rtl_metadata),
        ("toolchain lock", toolchain_lock),
    ):
        if not path.is_file():
            raise ToffeeRuntimeError(f"{label} is missing: {path}")

    output = output.resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    staging = Path(
        tempfile.mkdtemp(prefix=f".{output.name}.staging-", dir=output.parent)
    )
    try:
        package_output = staging / "picker" / dut_package.name
        package_output.parent.mkdir(parents=True)
        shutil.copytree(
            dut_package,
            package_output,
            ignore=shutil.ignore_patterns("__pycache__", "*.pyc", "*.pyo"),
        )
        source_output = staging / "python"
        shutil.copytree(
            project_source,
            source_output,
            ignore=shutil.ignore_patterns(
                "__pycache__",
                "*.pyc",
                "*.pyo",
                "*.egg-info",
            ),
        )
        port_output = staging / "config/expected_ports.json"
        port_output.parent.mkdir(parents=True)
        shutil.copy2(port_manifest, port_output)

        environment_output = staging / "environment.json"
        rtl_output = staging / "rtl.json"
        lock_output = staging / "toolchain.lock.json"
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
        artifacts = [file_record(path, staging) for path in runtime_files]
        environment = json.loads(environment_output.read_text(encoding="utf-8"))
        manifest = {
            "schema_version": 2,
            "profile": "toffee",
            "kind": "python-picker-dut",
            "package_name": dut_package.name,
            "package_root": package_output.parent.relative_to(staging).as_posix(),
            "source_root": source_output.relative_to(staging).as_posix(),
            "source_identity": tree_sha256(source_output),
            "port_manifest": file_record(port_output, staging),
            "project_name": project.resolve().name,
            "environment": environment,
            "rtl_metadata": file_record(rtl_output, staging),
            "toolchain_lock": file_record(lock_output, staging),
            "runtime_identity": runtime_identity(artifacts),
            "artifacts": artifacts,
        }
        metadata_path = staging / "runtime.json"
        metadata_path.write_text(
            json.dumps(manifest, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )
        harden_package(package_output)
        package_output.parent.chmod(0o555)
        harden_package(source_output)
        harden_package(port_output.parent)
        for sidecar in (environment_output, rtl_output, lock_output, metadata_path):
            harden_file(sidecar)
        staging.chmod(0o555)

        if output.exists():
            if not exchange_directories(staging, output):
                raise ToffeeRuntimeError(
                    "atomic directory exchange is unavailable; refusing to "
                    f"replace existing runtime {output}"
                )
            previous = staging
            staging = output
            remove_tree(previous)
        else:
            staging.rename(output)
            staging = output
        return output / "runtime.json"
    finally:
        if staging.exists() and staging != output:
            remove_tree(staging)


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
