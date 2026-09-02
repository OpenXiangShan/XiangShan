#!/usr/bin/env python3
"""Freeze and describe the MemBlock simulator's mutable runtime artifacts."""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import json
import os
import re
import shutil
import stat
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any


class FreezeError(RuntimeError):
    pass


ARTIFACT_NAMES = {
    "binary": "memblock_sim",
    "model": "libUTMemBlock.so",
    "xspcomm": "libxspcomm.so.0.0.1",
}


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def artifact_entry(role: str, path: Path, relative_path: str) -> dict[str, Any]:
    return {
        "role": role,
        "path": relative_path,
        "sha256": sha256(path),
        "size": path.stat().st_size,
    }


def resolved_dependencies(binary: Path, library_directory: Path) -> list[dict[str, Any]]:
    environment = os.environ.copy()
    previous = environment.get("LD_LIBRARY_PATH")
    environment["LD_LIBRARY_PATH"] = (
        str(library_directory)
        if not previous
        else str(library_directory) + os.pathsep + previous
    )
    completed = subprocess.run(
        ["ldd", str(binary)],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        env=environment,
    )
    if completed.returncode != 0:
        raise FreezeError(f"ldd failed for {binary}: {completed.stdout.strip()}")

    dependencies: list[dict[str, Any]] = []
    for raw_line in completed.stdout.splitlines():
        line = raw_line.strip()
        if not line or line.startswith("linux-vdso.so"):
            continue
        match = re.match(r"(\S+)\s+=>\s+(\S+)\s+\(0x[0-9a-fA-F]+\)$", line)
        if match:
            soname, resolved = match.groups()
            if resolved == "not":
                raise FreezeError(f"unresolved dependency: {line}")
            path = Path(resolved).resolve()
        else:
            match = re.match(r"(/\S+)\s+\(0x[0-9a-fA-F]+\)$", line)
            if not match:
                raise FreezeError(f"cannot parse ldd output: {line}")
            path = Path(match.group(1)).resolve()
            soname = path.name
        if not path.is_file():
            raise FreezeError(f"resolved dependency is not a file: {path}")
        dependencies.append(
            {
                "soname": soname,
                "path": str(path),
                "sha256": sha256(path),
                "size": path.stat().st_size,
            }
        )
    return sorted(dependencies, key=lambda entry: (entry["soname"], entry["path"]))


def write_json_atomic(path: Path, document: dict[str, Any]) -> None:
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=path.name + ".", suffix=".tmp", dir=path.parent
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            json.dump(document, stream, indent=2, sort_keys=True)
            stream.write("\n")
        os.chmod(temporary, stat.S_IRUSR | stat.S_IRGRP | stat.S_IROTH)
        os.replace(temporary, path)
    finally:
        if temporary.exists():
            temporary.unlink()


def freeze(
    binary: Path,
    model: Path,
    xspcomm: Path,
    output: Path,
) -> Path:
    sources = {"binary": binary, "model": model, "xspcomm": xspcomm}
    for role, source in sources.items():
        source = source.resolve()
        if not source.is_file():
            raise FreezeError(f"{role} source is not a file: {source}")
        sources[role] = source

    output.mkdir(parents=True, exist_ok=True)
    artifacts: list[dict[str, Any]] = []
    for role, filename in ARTIFACT_NAMES.items():
        destination = output / filename
        descriptor, temporary_name = tempfile.mkstemp(
            prefix=filename + ".", suffix=".tmp", dir=output
        )
        os.close(descriptor)
        temporary = Path(temporary_name)
        try:
            shutil.copyfile(sources[role], temporary)
            mode = 0o555 if role == "binary" else 0o444
            os.chmod(temporary, mode)
            os.replace(temporary, destination)
        finally:
            if temporary.exists():
                temporary.unlink()
        artifacts.append(artifact_entry(role, destination, filename))

    frozen_binary = output / ARTIFACT_NAMES["binary"]
    dependencies = resolved_dependencies(frozen_binary, output.resolve())
    resolved_by_soname = {
        entry["soname"]: Path(entry["path"]).resolve() for entry in dependencies
    }
    expected_resolution = {
        "libUTMemBlock.so": (output / ARTIFACT_NAMES["model"]).resolve(),
        "libxspcomm.so.0.0.1": (output / ARTIFACT_NAMES["xspcomm"]).resolve(),
    }
    for soname, expected in expected_resolution.items():
        if resolved_by_soname.get(soname) != expected:
            raise FreezeError(
                f"{soname} resolves to {resolved_by_soname.get(soname)}, expected {expected}"
            )

    document = {
        "schema_version": 1,
        "created_at": dt.datetime.now(dt.timezone.utc).isoformat(),
        "artifacts": sorted(artifacts, key=lambda entry: entry["role"]),
        "external_dependencies": [
            entry
            for entry in dependencies
            if Path(entry["path"]).parent != output.resolve()
        ],
        "source_paths": {
            role: str(source) for role, source in sorted(sources.items())
        },
    }
    metadata = output / "runtime.json"
    write_json_atomic(metadata, document)
    return metadata


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", type=Path, required=True)
    parser.add_argument("--model", type=Path, required=True)
    parser.add_argument("--xspcomm", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()

    try:
        metadata = freeze(args.binary, args.model, args.xspcomm, args.output.resolve())
    except (OSError, FreezeError) as error:
        print(f"freeze_runtime.py: error: {error}", file=sys.stderr)
        return 2
    print(metadata)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
