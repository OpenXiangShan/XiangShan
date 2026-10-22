from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
from typing import Any


_VALIDATED_ROOTS: dict[str, dict[str, Any]] = {}
_SCHEMA_VERSION = 2


class RuntimeIntegrityError(RuntimeError):
    pass


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _tree_sha256(root: Path) -> str:
    digest = hashlib.sha256()
    for path in sorted(item for item in root.rglob("*") if item.is_file()):
        relative = path.relative_to(root).as_posix()
        digest.update(relative.encode("utf-8"))
        digest.update(b"\0")
        with path.open("rb") as handle:
            for chunk in iter(lambda: handle.read(1024 * 1024), b""):
                digest.update(chunk)
        digest.update(b"\0")
    return digest.hexdigest()


def _runtime_identity(artifacts: list[dict[str, Any]]) -> str:
    canonical = json.dumps(
        sorted(
            (
                {
                    "path": str(record["path"]),
                    "size": int(record["size"]),
                    "sha256": str(record["sha256"]),
                }
                for record in artifacts
            ),
            key=lambda record: record["path"],
        ),
        separators=(",", ":"),
        sort_keys=True,
    )
    return hashlib.sha256(canonical.encode("utf-8")).hexdigest()


def _metadata_path(package_root: Path) -> Path:
    configured = os.environ.get("MEMBLOCK_TOFFEE_RUNTIME_METADATA")
    if configured:
        return Path(configured)
    return package_root.parent / "runtime.json"


def _load_document(metadata_path: Path) -> dict[str, Any]:
    try:
        document = json.loads(metadata_path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise RuntimeIntegrityError(f"cannot read runtime metadata: {error}") from error
    if not isinstance(document, dict):
        raise RuntimeIntegrityError("runtime metadata must be a JSON object")
    return document


def _resolve_member(runtime_root: Path, value: object, label: str) -> Path:
    if not isinstance(value, str) or not value:
        raise RuntimeIntegrityError(f"runtime {label} is missing")
    relative = Path(value)
    if relative.is_absolute() or ".." in relative.parts:
        raise RuntimeIntegrityError(f"runtime {label} must be a relative path")
    resolved = (runtime_root / relative).resolve()
    try:
        resolved.relative_to(runtime_root)
    except ValueError as error:
        raise RuntimeIntegrityError(f"runtime {label} escapes runtime root") from error
    return resolved


def validate_runtime(package_root: Path) -> dict[str, Any]:
    package_root = package_root.resolve()
    metadata_path = _metadata_path(package_root).resolve()
    key = f"{package_root}\0{metadata_path}"
    if key in _VALIDATED_ROOTS:
        return _VALIDATED_ROOTS[key]
    if not os.environ.get("MEMBLOCK_TOFFEE_RUNTIME_METADATA") and not metadata_path.is_file() and os.environ.get("MEMBLOCK_TOFFEE_ALLOW_UNFROZEN") == "1":
        return {}
    if not metadata_path.is_file():
        raise RuntimeIntegrityError(f"runtime metadata is missing: {metadata_path}")
    document = _load_document(metadata_path)
    if document.get("schema_version") != _SCHEMA_VERSION:
        raise RuntimeIntegrityError("runtime metadata schema is unsupported")
    if document.get("profile") != "toffee":
        raise RuntimeIntegrityError("runtime profile is not Toffee")
    runtime_root = metadata_path.resolve().parent
    metadata_package_root = _resolve_member(
        runtime_root,
        document.get("package_root"),
        "package root",
    )
    if metadata_package_root != package_root:
        raise RuntimeIntegrityError("runtime package root does not match metadata")
    artifacts = document.get("artifacts")
    if not isinstance(artifacts, list) or not artifacts:
        raise RuntimeIntegrityError("runtime metadata has no artifacts")
    validated_artifacts: list[dict[str, Any]] = []
    for record in artifacts:
        if not isinstance(record, dict) or set(record) != {"path", "size", "sha256"}:
            raise RuntimeIntegrityError("runtime artifact record is invalid")
        path = _resolve_member(runtime_root, record["path"], "artifact path")
        if not path.is_file():
            raise RuntimeIntegrityError(f"runtime artifact is missing: {path}")
        if path.stat().st_size != record.get("size"):
            raise RuntimeIntegrityError(f"runtime artifact size changed: {path}")
        if _sha256(path) != record.get("sha256"):
            raise RuntimeIntegrityError(f"runtime artifact hash changed: {path}")
        validated_artifacts.append(record)
    if document.get("runtime_identity") != _runtime_identity(validated_artifacts):
        raise RuntimeIntegrityError("runtime artifact identity does not match metadata")
    package_relative = package_root.relative_to(runtime_root).as_posix()
    package_prefix = "" if package_relative == "." else f"{package_relative}/"
    declared_package_files = {
        str(record["path"])
        for record in validated_artifacts
        if str(record["path"]).startswith(package_prefix)
    }
    actual_package_files = {
        path.relative_to(runtime_root).as_posix()
        for path in package_root.rglob("*")
        if path.is_file()
    }
    undeclared = sorted(actual_package_files - declared_package_files)
    if undeclared:
        raise RuntimeIntegrityError(
            "runtime package contains undeclared files: " + ", ".join(undeclared)
        )
    source_root = _resolve_member(
        runtime_root,
        document.get("source_root"),
        "source root",
    )
    if not source_root.is_dir():
        raise RuntimeIntegrityError("runtime source root is missing")
    if _tree_sha256(source_root) != document.get("source_identity"):
        raise RuntimeIntegrityError("runtime source identity changed")
    _VALIDATED_ROOTS[key] = document
    return document
