import hashlib
import json
from pathlib import Path

import pytest

from memblock_toffee import runtime


def write_runtime(root: Path, content: bytes) -> tuple[Path, Path]:
    package = root / "runtime/picker/MemBlock"
    package.mkdir(parents=True)
    artifact = package / "__init__.py"
    artifact.write_bytes(content)
    metadata = root / "runtime/runtime.json"
    metadata.write_text(
        json.dumps(
            {
                "profile": "toffee",
                "package_root": str(package),
                "artifacts": [
                    {
                        "path": str(artifact),
                        "size": len(content),
                        "sha256": hashlib.sha256(content).hexdigest(),
                    }
                ],
            }
        ),
        encoding="utf-8",
    )
    return package, metadata


def test_runtime_integrity_validation(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    package, metadata = write_runtime(tmp_path, b"original")
    monkeypatch.setenv("MEMBLOCK_TOFFEE_RUNTIME_METADATA", str(metadata))
    runtime._VALIDATED_ROOTS.clear()
    document = runtime.validate_runtime(package)
    assert document["profile"] == "toffee"


def test_runtime_integrity_rejects_mutation(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    package, metadata = write_runtime(tmp_path, b"original")
    monkeypatch.setenv("MEMBLOCK_TOFFEE_RUNTIME_METADATA", str(metadata))
    runtime._VALIDATED_ROOTS.clear()
    (package / "__init__.py").write_bytes(b"changed")
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
            "path": str(sidecar),
            "size": sidecar.stat().st_size,
            "sha256": hashlib.sha256(sidecar.read_bytes()).hexdigest(),
        }
    )
    metadata.write_text(json.dumps(document), encoding="utf-8")
    monkeypatch.setenv("MEMBLOCK_TOFFEE_RUNTIME_METADATA", str(metadata))
    runtime._VALIDATED_ROOTS.clear()
    sidecar.write_text("changed\n", encoding="utf-8")
    with pytest.raises(runtime.RuntimeIntegrityError, match="sidecar|size changed"):
        runtime.validate_runtime(package)
