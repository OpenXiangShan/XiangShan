from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
from typing import Any


_VALIDATED_ROOTS: set[str] = set()


class RuntimeIntegrityError(RuntimeError):
    pass


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _metadata_path(package_root: Path) -> Path:
    configured = os.environ.get("MEMBLOCK_TOFFEE_RUNTIME_METADATA")
    if configured:
        return Path(configured)
    return package_root.parent / "runtime.json"


def validate_runtime(package_root: Path) -> dict[str, Any]:
    package_root = package_root.resolve()
    key = str(package_root)
    if key in _VALIDATED_ROOTS:
        return {}
    metadata_path = _metadata_path(package_root)
    if not os.environ.get("MEMBLOCK_TOFFEE_RUNTIME_METADATA") and not metadata_path.is_file() and os.environ.get("MEMBLOCK_TOFFEE_ALLOW_UNFROZEN") == "1":
        return {}
    if not metadata_path.is_file():
        raise RuntimeIntegrityError(f"runtime metadata is missing: {metadata_path}")
    try:
        document = json.loads(metadata_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise RuntimeIntegrityError(f"cannot read runtime metadata: {error}") from error
    if document.get("profile") != "toffee":
        raise RuntimeIntegrityError("runtime profile is not Toffee")
    if Path(document.get("package_root", "")).resolve() != package_root:
        raise RuntimeIntegrityError("runtime package root does not match metadata")
    runtime_root = metadata_path.resolve().parent
    artifacts = document.get("artifacts")
    if not isinstance(artifacts, list) or not artifacts:
        raise RuntimeIntegrityError("runtime metadata has no artifacts")
    for record in artifacts:
        if not isinstance(record, dict):
            raise RuntimeIntegrityError("runtime artifact record is invalid")
        path = Path(record.get("path", ""))
        try:
            path.resolve().relative_to(runtime_root)
        except (OSError, ValueError) as error:
            raise RuntimeIntegrityError(f"artifact escapes runtime root: {path}") from error
        if not path.is_file():
            raise RuntimeIntegrityError(f"runtime artifact is missing: {path}")
        if path.stat().st_size != record.get("size"):
            raise RuntimeIntegrityError(f"runtime artifact size changed: {path}")
        if _sha256(path) != record.get("sha256"):
            raise RuntimeIntegrityError(f"runtime artifact hash changed: {path}")
    _VALIDATED_ROOTS.add(key)
    return document
