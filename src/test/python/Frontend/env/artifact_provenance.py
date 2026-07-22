from __future__ import annotations

import hashlib
import json
import re
from datetime import datetime, timezone
from functools import lru_cache
from pathlib import Path
from typing import Any


BUILD_MANIFEST_SCHEMA_VERSION = 1
BUILD_HASH_FIELDS = (
    "dut_build_sha256",
    "dut_python_extension_sha256",
    "generated_rtl_sha256",
    "signal_contract_sha256",
)

_GIT_OBJECT_ID_RE = re.compile(r"(?:[0-9a-fA-F]{40}|[0-9a-fA-F]{64})\Z")
_SHA256_RE = re.compile(r"[0-9a-fA-F]{64}\Z")


@lru_cache(maxsize=None)
def _file_sha256_version(path_text: str, mtime_ns: int, size: int) -> str:
    path = Path(path_text)
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def file_sha256(path: Path) -> str:
    path = Path(path)
    try:
        stat = path.stat()
    except OSError:
        return "unavailable"
    if not path.is_file():
        return "unavailable"
    return _file_sha256_version(str(path.resolve()), stat.st_mtime_ns, stat.st_size)


def rtl_tree_sha256(rtl_dir: Path) -> str:
    rtl_dir = Path(rtl_dir)
    paths = sorted(path for path in rtl_dir.glob("*.sv") if path.is_file())
    if not paths:
        return "unavailable"
    digest = hashlib.sha256()
    for path in paths:
        relative = path.relative_to(rtl_dir).as_posix().encode("utf-8")
        digest.update(len(relative).to_bytes(8, "big"))
        digest.update(relative)
        digest.update(bytes.fromhex(file_sha256(path)))
    return digest.hexdigest()


def current_frontend_build_hashes(build_root: Path) -> dict[str, str]:
    build_root = Path(build_root)
    pylib = build_root / "pylib" / "Frontend"
    return {
        "dut_build_sha256": file_sha256(pylib / "libUTFrontend.so"),
        "dut_python_extension_sha256": file_sha256(pylib / "_UT_Frontend.so"),
        "generated_rtl_sha256": rtl_tree_sha256(build_root / "rtl"),
        "signal_contract_sha256": file_sha256(pylib / "Frontend_offset.yaml"),
    }


def write_frontend_build_manifest(
    output_path: Path,
    *,
    build_root: Path,
    dut_source_sha: str,
    source_tree_dirty: bool,
    build_config: str,
    build_command: str,
) -> dict[str, Any]:
    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    manifest = {
        "schema_version": BUILD_MANIFEST_SCHEMA_VERSION,
        "dut_source_sha": str(dut_source_sha).strip() or "unavailable",
        "source_tree_dirty": bool(source_tree_dirty),
        "build_config": str(build_config).strip() or "frontend-default",
        "build_command": str(build_command).strip(),
        "created_at_utc": datetime.now(timezone.utc).isoformat(),
        "artifacts": current_frontend_build_hashes(build_root),
    }
    temporary = output_path.with_name(f".{output_path.name}.tmp")
    temporary.write_text(json.dumps(manifest, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    temporary.replace(output_path)
    return manifest


def load_frontend_build_manifest(build_root: Path, manifest_path: Path | None = None) -> dict[str, Any]:
    build_root = Path(build_root)
    path = Path(manifest_path) if manifest_path is not None else build_root / "frontend_build_manifest.json"
    current_hashes = current_frontend_build_hashes(build_root)
    result: dict[str, Any] = {
        **current_hashes,
        "dut_source_sha": "unavailable",
        "build_config": "frontend-default",
        "build_manifest_status": "missing",
        "build_manifest_sha256": file_sha256(path),
        "build_manifest_reasons": [],
    }
    if not path.is_file():
        result["build_manifest_reasons"] = ["manifest_not_found"]
        return result

    try:
        manifest = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        result["build_manifest_status"] = "invalid"
        result["build_manifest_reasons"] = [f"manifest_read_error:{type(exc).__name__}"]
        return result

    if not isinstance(manifest, dict):
        result["build_manifest_status"] = "invalid"
        result["build_manifest_reasons"] = ["manifest_root_not_object"]
        return result

    reasons: list[str] = []
    if manifest.get("schema_version") != BUILD_MANIFEST_SCHEMA_VERSION:
        reasons.append("manifest_schema_mismatch")
    source_sha = str(manifest.get("dut_source_sha") or "").strip()
    if not source_sha or source_sha in {"unknown", "unavailable"}:
        reasons.append("missing_dut_source_sha")
    elif _GIT_OBJECT_ID_RE.fullmatch(source_sha) is None:
        reasons.append("invalid_dut_source_sha")
    if bool(manifest.get("source_tree_dirty", True)):
        reasons.append("source_tree_dirty")

    recorded_hashes = manifest.get("artifacts")
    if not isinstance(recorded_hashes, dict):
        reasons.append("manifest_artifacts_not_object")
        recorded_hashes = {}
    for field in BUILD_HASH_FIELDS:
        actual = current_hashes[field]
        recorded = str(recorded_hashes.get(field) or "").strip()
        if actual == "unavailable":
            reasons.append(f"missing_build_artifact:{field}")
        elif _SHA256_RE.fullmatch(recorded) is None:
            reasons.append(f"invalid_build_hash:{field}")
        elif recorded != actual:
            reasons.append(f"build_hash_mismatch:{field}")

    result["build_config"] = str(manifest.get("build_config") or "frontend-default").strip()
    result["build_manifest_status"] = "valid" if not reasons else "invalid"
    result["build_manifest_reasons"] = reasons
    if not reasons:
        result["dut_source_sha"] = source_sha
    return result
