import hashlib
import json
import shutil
from pathlib import Path

import pytest

from memblock_toffee import runtime


def write_runtime(root: Path, content: bytes) -> tuple[Path, Path]:
    runtime_root = root / "runtime"
    package = runtime_root / "picker/MemBlock"
    package.mkdir(parents=True)
    artifact = package / "__init__.py"
    artifact.write_bytes(content)
    source = runtime_root / "python/memblock_toffee"
    source.mkdir(parents=True)
    source_file = source / "__init__.py"
    source_file.write_text("VALUE = 1\n", encoding="utf-8")
    records = [
        runtime_record(path, runtime_root)
        for path in (artifact, source_file)
    ]
    metadata = runtime_root / "runtime.json"
    metadata.write_text(
        json.dumps(
            {
                "schema_version": 2,
                "profile": "toffee",
                "package_root": "picker",
                "source_root": "python",
                "source_identity": tree_hash(source.parent),
                "runtime_identity": runtime_identity(records),
                "artifacts": records,
            }
        ),
        encoding="utf-8",
    )
    return package.parent, metadata


def runtime_record(path: Path, root: Path) -> dict[str, object]:
    content = path.read_bytes()
    return {
        "path": path.relative_to(root).as_posix(),
        "size": len(content),
        "sha256": hashlib.sha256(content).hexdigest(),
    }


def tree_hash(root: Path) -> str:
    digest = hashlib.sha256()
    for path in sorted(item for item in root.rglob("*") if item.is_file()):
        digest.update(path.relative_to(root).as_posix().encode("utf-8"))
        digest.update(b"\0")
        digest.update(path.read_bytes())
        digest.update(b"\0")
    return digest.hexdigest()


def runtime_identity(records: list[dict[str, object]]) -> str:
    canonical = json.dumps(
        sorted(records, key=lambda record: str(record["path"])),
        separators=(",", ":"),
        sort_keys=True,
    )
    return hashlib.sha256(canonical.encode("utf-8")).hexdigest()


def test_runtime_integrity_validation(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    package, metadata = write_runtime(tmp_path, b"original")
    monkeypatch.delenv("MEMBLOCK_TOFFEE_RUNTIME_METADATA", raising=False)
    runtime._VALIDATED_ROOTS.clear()
    document = runtime.validate_runtime(package)
    assert document["profile"] == "toffee"
    assert runtime.validate_runtime(package) is document


def test_runtime_integrity_rejects_mutation(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    package, metadata = write_runtime(tmp_path, b"original")
    monkeypatch.setenv("MEMBLOCK_TOFFEE_RUNTIME_METADATA", str(metadata))
    runtime._VALIDATED_ROOTS.clear()
    (package / "MemBlock/__init__.py").write_bytes(b"changed")
    with pytest.raises(runtime.RuntimeIntegrityError, match="size changed"):
        runtime.validate_runtime(package)


def test_runtime_integrity_rejects_sidecar_mutation(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    package, _ = write_runtime(tmp_path, b"original")
    sidecar = tmp_path / "runtime/rtl.json"
    sidecar.write_text("original\n", encoding="utf-8")
    metadata = tmp_path / "runtime/runtime.json"
    document = json.loads(metadata.read_text(encoding="utf-8"))
    document["artifacts"].append(
        {
            "path": "rtl.json",
            "size": sidecar.stat().st_size,
            "sha256": hashlib.sha256(sidecar.read_bytes()).hexdigest(),
        }
    )
    metadata.write_text(json.dumps(document), encoding="utf-8")
    monkeypatch.delenv("MEMBLOCK_TOFFEE_RUNTIME_METADATA", raising=False)
    runtime._VALIDATED_ROOTS.clear()
    sidecar.write_text("changed\n", encoding="utf-8")
    with pytest.raises(runtime.RuntimeIntegrityError, match="sidecar|size changed"):
        runtime.validate_runtime(package)


def test_runtime_validation_survives_relocation(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    package, metadata = write_runtime(tmp_path, b"original")
    relocated_root = tmp_path / "relocated"
    shutil.copytree(metadata.parent, relocated_root)
    relocated_package = relocated_root / "picker"
    monkeypatch.delenv("MEMBLOCK_TOFFEE_RUNTIME_METADATA", raising=False)
    runtime._VALIDATED_ROOTS.clear()
    document = runtime.validate_runtime(relocated_package)
    assert document["package_root"] == "picker"


def test_runtime_rejects_extra_source_files(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    package, metadata = write_runtime(tmp_path, b"original")
    source_file = tmp_path / "runtime/python/memblock_toffee/injected.py"
    source_file.write_text("INJECTED = True\n", encoding="utf-8")
    monkeypatch.setenv("MEMBLOCK_TOFFEE_RUNTIME_METADATA", str(metadata))
    runtime._VALIDATED_ROOTS.clear()
    with pytest.raises(runtime.RuntimeIntegrityError, match="source identity"):
        runtime.validate_runtime(package)


def test_runtime_rejects_undeclared_package_files(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    package, metadata = write_runtime(tmp_path, b"original")
    injected = package / "MemBlock/injected.py"
    injected.write_text("INJECTED = True\n", encoding="utf-8")
    monkeypatch.setenv("MEMBLOCK_TOFFEE_RUNTIME_METADATA", str(metadata))
    runtime._VALIDATED_ROOTS.clear()
    with pytest.raises(runtime.RuntimeIntegrityError, match="undeclared files"):
        runtime.validate_runtime(package)


def test_runtime_rejects_absolute_artifact_paths(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    package, metadata = write_runtime(tmp_path, b"original")
    document = json.loads(metadata.read_text(encoding="utf-8"))
    document["artifacts"][0]["path"] = str(package / "MemBlock/__init__.py")
    metadata.write_text(json.dumps(document), encoding="utf-8")
    monkeypatch.setenv("MEMBLOCK_TOFFEE_RUNTIME_METADATA", str(metadata))
    runtime._VALIDATED_ROOTS.clear()
    with pytest.raises(runtime.RuntimeIntegrityError, match="relative path"):
        runtime.validate_runtime(package)
