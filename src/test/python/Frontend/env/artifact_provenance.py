from __future__ import annotations

import hashlib
import json
import re
import subprocess
from datetime import datetime, timezone
from functools import lru_cache
from pathlib import Path
from typing import Any


BUILD_MANIFEST_SCHEMA_VERSION = 2
BUILD_HASH_FIELDS = (
    "dut_build_sha256",
    "dut_python_extension_sha256",
    "generated_rtl_sha256",
    "signal_contract_sha256",
)

_GIT_OBJECT_ID_RE = re.compile(r"(?:[0-9a-fA-F]{40}|[0-9a-fA-F]{64})\Z")
_SHA256_RE = re.compile(r"[0-9a-fA-F]{64}\Z")
_OBSERVABILITY_SOURCE_ALLOWLIST = (
    "src/main/scala/xiangshan/frontend/icache/ICacheMainPipe.scala",
    "src/main/scala/xiangshan/frontend/ifu/Ifu.scala",
)


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


def _git_output(repo_root: Path, *args: str) -> bytes:
    result = subprocess.run(
        ["git", "-C", str(repo_root), *args],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    return bytes(result.stdout)


def write_frontend_build_manifest(
    output_path: Path,
    *,
    build_root: Path,
    dut_source_sha: str,
    source_tree_dirty: bool,
    build_config: str,
    build_command: str,
    metadata: dict[str, Any] | None = None,
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
    # Direct callers get a complete, non-overridden provenance shape as well;
    # the Makefile writer replaces these values with the checked git delta.
    manifest.update(
        {
            "implementation_sha": str(dut_source_sha).strip() or "unavailable",
            "design_baseline_sha": str(dut_source_sha).strip() or "unavailable",
            "source_sha_override": False,
            "source_delta_sha256": hashlib.sha256(b"").hexdigest(),
            "source_delta_files": [],
            "source_delta_policy": "none",
        }
    )
    if metadata:
        manifest.update({str(key): value for key, value in metadata.items()})
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
        "implementation_sha": "unavailable",
        "design_baseline_sha": "unavailable",
        "source_sha_override": False,
        "source_delta_sha256": "unavailable",
        "source_delta_files": [],
        "source_delta_policy": "unavailable",
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
    required_metadata = (
        "implementation_sha",
        "design_baseline_sha",
        "source_sha_override",
        "source_delta_sha256",
        "source_delta_files",
        "source_delta_policy",
    )
    for key in required_metadata:
        if key not in manifest:
            reasons.append(f"missing_manifest_metadata:{key}")

    implementation_sha = str(manifest.get("implementation_sha") or "").strip()
    if implementation_sha and _GIT_OBJECT_ID_RE.fullmatch(implementation_sha) is None:
        reasons.append("invalid_implementation_sha")
    design_baseline_sha = str(manifest.get("design_baseline_sha") or "").strip()
    if design_baseline_sha and _GIT_OBJECT_ID_RE.fullmatch(design_baseline_sha) is None:
        reasons.append("invalid_design_baseline_sha")
    source_delta_sha256 = str(manifest.get("source_delta_sha256") or "").strip()
    if source_delta_sha256 and _SHA256_RE.fullmatch(source_delta_sha256) is None:
        reasons.append("invalid_source_delta_sha256")
    source_delta_files = manifest.get("source_delta_files", [])
    if not isinstance(source_delta_files, list) or any(
        not isinstance(item, str) or not item.strip() for item in source_delta_files
    ):
        reasons.append("invalid_source_delta_files")
        source_delta_files = []
    source_delta_policy = str(manifest.get("source_delta_policy") or "").strip()
    source_sha_override_value = manifest.get("source_sha_override")
    source_sha_override = source_sha_override_value if isinstance(source_sha_override_value, bool) else False
    if "source_sha_override" in manifest and not isinstance(source_sha_override_value, bool):
        reasons.append("invalid_source_sha_override")
    if source_sha_override:
        if not implementation_sha:
            reasons.append("missing_implementation_sha_for_override")
        if source_delta_sha256 == "":
            reasons.append("missing_source_delta_sha256_for_override")
        if source_delta_policy != "observability_only":
            reasons.append("source_delta_policy_not_allowlisted")
        unexpected = sorted(set(source_delta_files) - set(_OBSERVABILITY_SOURCE_ALLOWLIST))
        if unexpected:
            reasons.append("source_delta_file_not_allowlisted:" + ",".join(unexpected))
        if source_sha == implementation_sha:
            reasons.append("source_sha_override_without_source_delta")
        repo_root = build_root.resolve().parent
        if not (repo_root / ".git").exists():
            reasons.append("source_delta_runtime_unavailable")
        else:
            try:
                runtime_head = _git_output(repo_root, "rev-parse", "HEAD").decode("ascii").strip()
                if runtime_head.lower() != implementation_sha.lower():
                    reasons.append("implementation_sha_runtime_mismatch")
                actual_files = [
                    item
                    for item in _git_output(
                        repo_root,
                        "diff",
                        "--name-only",
                        f"{source_sha}..{implementation_sha}",
                        "--",
                        "src/main/scala",
                    ).decode("utf-8").splitlines()
                    if item.strip()
                ]
                if actual_files != source_delta_files:
                    reasons.append("source_delta_files_runtime_mismatch")
                actual_delta = (
                    _git_output(
                        repo_root,
                        "diff",
                        "--binary",
                        f"{source_sha}..{implementation_sha}",
                        "--",
                        *actual_files,
                    )
                    if actual_files
                    else b""
                )
                if hashlib.sha256(actual_delta).hexdigest() != source_delta_sha256:
                    reasons.append("source_delta_hash_runtime_mismatch")
            except (OSError, UnicodeError, subprocess.CalledProcessError):
                reasons.append("source_delta_runtime_check_failed")
    else:
        if implementation_sha and source_sha and implementation_sha != source_sha:
            reasons.append("implementation_sha_diff_without_override")
        if source_delta_policy != "none":
            reasons.append("source_delta_policy_requires_override")
        if source_delta_files:
            reasons.append("source_delta_files_without_override")
        if source_delta_sha256 and source_delta_sha256 != hashlib.sha256(b"").hexdigest():
            reasons.append("source_delta_hash_without_override")

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
    result["implementation_sha"] = implementation_sha or "unavailable"
    result["design_baseline_sha"] = design_baseline_sha or source_sha or "unavailable"
    result["source_sha_override"] = source_sha_override
    result["source_delta_sha256"] = source_delta_sha256 or "unavailable"
    result["source_delta_files"] = list(source_delta_files)
    result["source_delta_policy"] = source_delta_policy or "unavailable"
    result["build_manifest_status"] = "valid" if not reasons else "invalid"
    result["build_manifest_reasons"] = reasons
    if not reasons:
        result["dut_source_sha"] = source_sha
    return result
