from __future__ import annotations

import hashlib
import json
from pathlib import Path

import pytest

from env.artifact_provenance import (
    load_frontend_build_manifest,
    write_frontend_build_manifest,
)
from env.functional_coverage import CoverageBinDef, FunctionalCoverageRecorder
from tools import backannotate_funcov


_SOURCE_SHA = "1" * 40


def _write_build(build_root: Path, simulator: str = "verilator") -> tuple[Path, dict]:
    pylib = build_root / f"pylib-{simulator}" / "Frontend"
    rtl = build_root / "rtl"
    pylib.mkdir(parents=True)
    rtl.mkdir(parents=True)
    (pylib / "libUTFrontend.so").write_bytes(b"dut-model")
    (pylib / "_UT_Frontend.so").write_bytes(b"python-extension")
    (pylib / "Frontend_offset.yaml").write_text("signal: 1\n", encoding="utf-8")
    (rtl / "Frontend.sv").write_text("module Frontend; endmodule\n", encoding="utf-8")

    manifest_path = (build_root / f"frontend_build_manifest.{simulator}.json").resolve()
    write_frontend_build_manifest(
        manifest_path,
        build_root=build_root,
        dut_source_sha=_SOURCE_SHA,
        source_tree_dirty=False,
        build_config="DefaultConfig:E.b:1:systemverilog:fst",
        build_command="make frontend",
        simulator=simulator,
    )
    runtime = load_frontend_build_manifest(build_root, manifest_path, simulator=simulator)
    assert runtime["build_manifest_status"] == "valid", runtime["build_manifest_reasons"]
    return manifest_path, runtime


def _resign(provenance: dict) -> None:
    provenance["compatibility_signature"] = backannotate_funcov._json_sha256(
        {
            field: provenance[field]
            for field in backannotate_funcov._COMPATIBILITY_FIELDS
        }
    )


def _eligible_artifact(tmp_path: Path, simulator: str = "verilator") -> tuple[dict, Path, Path]:
    build_root = tmp_path / "build-frontend"
    manifest_path, runtime = _write_build(build_root, simulator)
    run_root = (tmp_path / "run").resolve()
    (run_root / "logs").mkdir(parents=True)
    (run_root / "funcov").mkdir()
    (run_root / "coverage").mkdir()
    (run_root / "waveforms").mkdir()
    testcase_path = run_root / "test_runtime_manifest.py"
    testcase_path.write_text("def test_runtime_manifest(): pass\n", encoding="utf-8")
    case_log_path = run_root / "logs" / "case.log"
    case_log_path.write_text("", encoding="utf-8")
    funcov_path = run_root / "funcov" / "case.funcov.json"
    funcov_path.write_text("{}\n", encoding="utf-8")
    dat_path = run_root / "coverage" / "case.dat"
    dat_path.write_text("C 'a' 1\n", encoding="utf-8")
    waveform_path = run_root / "waveforms" / "case.fst"
    waveform_path.write_bytes(b"fst")

    definitions = [{"bin_id": "BIN-501", "bin_name": "runtime_manifest"}]
    provenance = {
        "simulator": runtime["simulator"],
        "dut_source_sha": runtime["dut_source_sha"],
        "implementation_sha": runtime["implementation_sha"],
        "design_baseline_sha": runtime["design_baseline_sha"],
        "source_sha_override": runtime["source_sha_override"],
        "source_delta_sha256": runtime["source_delta_sha256"],
        "source_delta_files": runtime["source_delta_files"],
        "source_delta_policy": runtime["source_delta_policy"],
        "dut_build_sha256": runtime["dut_build_sha256"],
        "dut_python_extension_sha256": runtime["dut_python_extension_sha256"],
        "generated_rtl_sha256": runtime["generated_rtl_sha256"],
        "registry_sha256": backannotate_funcov._file_sha256(
            backannotate_funcov._CANONICAL_REGISTRY
        ),
        "definitions_sha256": backannotate_funcov._json_sha256(definitions),
        "sampler_sha256": backannotate_funcov._current_sampler_sha256(),
        "signal_contract_sha256": runtime["signal_contract_sha256"],
        "build_manifest_path": str(manifest_path),
        "build_manifest_sha256": runtime["build_manifest_sha256"],
        "build_manifest_status": runtime["build_manifest_status"],
        "build_manifest_reasons": runtime["build_manifest_reasons"],
        "build_config": runtime["build_config"],
        "toolchain": "python-unit",
    }
    _resign(provenance)
    raw = {
        "artifact_schema_version": 2,
        "testcase_name": "test_runtime_manifest",
        "artifact_tag": "runtime_manifest",
        "source_csv": str(backannotate_funcov._CANONICAL_REGISTRY.resolve()),
        "waveform_path": str(waveform_path),
        "line_coverage_path": str(dat_path),
        "coverage_targets": {"bin_ids": ["BIN-501"]},
        "provenance": provenance,
        "definitions": definitions,
        "hits": {},
        "stats": {"monitor": {"cycles_total": 1, "error_count": 0}},
        "errors": [],
        "checker": {"status": "pass", "error_count": 0, "errors": []},
        "run": {
            "run_id": "unit-runtime-manifest",
            "testcase_nodeid": "test_runtime_manifest.py::test_runtime_manifest",
            "testcase_path": str(testcase_path),
            "testcase_sha256": backannotate_funcov._file_sha256(testcase_path),
            "run_command": "pytest test_runtime_manifest.py",
            "artifact_root": str(run_root),
            "case_log_path": str(case_log_path),
            "funcov_path": str(funcov_path),
            "pytest_outcome": "pass",
            "exit_code": 0,
            "checker": {"status": "pass", "error_count": 0, "errors": []},
            "seed": 7,
            "seeds": {"test": 7, "backend": 8, "icache": 9, "ptw": 10},
        },
    }
    return raw, manifest_path, build_root


def test_funcov_artifact_records_absolute_manifest_path(tmp_path, monkeypatch):
    seen: dict[str, Path] = {}
    empty_delta_sha = hashlib.sha256(b"").hexdigest()

    def fake_load(build_root: Path, manifest_path: Path, **_kwargs) -> dict:
        seen["manifest_path"] = manifest_path
        return {
            "dut_source_sha": _SOURCE_SHA,
            "implementation_sha": _SOURCE_SHA,
            "design_baseline_sha": _SOURCE_SHA,
            "source_sha_override": False,
            "source_delta_sha256": empty_delta_sha,
            "source_delta_files": [],
            "source_delta_policy": "none",
            "dut_build_sha256": "2" * 64,
            "dut_python_extension_sha256": "3" * 64,
            "generated_rtl_sha256": "4" * 64,
            "signal_contract_sha256": "5" * 64,
            "build_config": "unit",
            "build_manifest_status": "valid",
            "build_manifest_sha256": "6" * 64,
            "build_manifest_reasons": [],
        }

    monkeypatch.chdir(tmp_path)
    monkeypatch.setenv("TB_DUT_BUILD_MANIFEST", "relative/build-manifest.json")
    monkeypatch.setattr("env.functional_coverage.load_frontend_build_manifest", fake_load)
    definition = CoverageBinDef(
        bin_id="BIN-999",
        stage="unit",
        coverage_type="unit",
        coverage_group="unit",
        coverpoint="manifest",
        bin_name="absolute_path",
        mapped_path="unit",
        sample_event="unit",
        observe_object="unit",
        hit_rule="unit",
        priority="P2",
        suggested_testcase="unit",
    )
    recorder = FunctionalCoverageRecorder(
        [definition],
        testcase_name="unit",
        artifact_tag="unit",
        output_dir=tmp_path / "funcov",
    )
    raw_path = Path(recorder.write_artifacts()["raw_path"])
    provenance = json.loads(raw_path.read_text(encoding="utf-8"))["provenance"]

    assert seen["manifest_path"].is_absolute()
    assert Path(provenance["build_manifest_path"]).is_absolute()
    assert provenance["build_manifest_path"] == str(seen["manifest_path"])


def test_backannotation_runtime_manifest_gate_accepts_unchanged_build(tmp_path):
    raw, _manifest_path, _build_root = _eligible_artifact(tmp_path)

    gate = backannotate_funcov.evaluate_artifact(raw)

    assert gate == {"kind": "dut", "eligible": True, "reasons": []}


def test_backannotation_runtime_manifest_gate_accepts_vcs_build(tmp_path):
    raw, _manifest_path, _build_root = _eligible_artifact(tmp_path, "vcs")

    gate = backannotate_funcov.evaluate_artifact(raw)

    assert gate == {"kind": "dut", "eligible": True, "reasons": []}


@pytest.mark.parametrize(
    ("field", "replacement"),
    [
        ("build_manifest_sha256", "0" * 64),
        ("dut_build_sha256", "0" * 64),
        ("dut_python_extension_sha256", "0" * 64),
        ("generated_rtl_sha256", "0" * 64),
        ("signal_contract_sha256", "0" * 64),
        ("dut_source_sha", "2" * 40),
        ("design_baseline_sha", "2" * 40),
        ("implementation_sha", "2" * 40),
        ("source_sha_override", True),
        ("source_delta_sha256", "0" * 64),
        ("source_delta_files", ["src/main/scala/changed.scala"]),
        ("source_delta_policy", "observability_only"),
        ("build_config", "different-config"),
    ],
)
def test_backannotation_runtime_manifest_gate_rejects_provenance_drift(
    tmp_path, field, replacement
):
    raw, _manifest_path, _build_root = _eligible_artifact(tmp_path)
    raw["provenance"][field] = replacement
    _resign(raw["provenance"])

    gate = backannotate_funcov.evaluate_artifact(raw)

    assert not gate["eligible"]
    assert f"build_manifest_runtime_mismatch:{field}" in gate["reasons"]


def test_backannotation_runtime_manifest_gate_rejects_missing_or_relative_path(tmp_path):
    raw, manifest_path, _build_root = _eligible_artifact(tmp_path)
    del raw["provenance"]["build_manifest_path"]
    missing = backannotate_funcov.evaluate_artifact(raw)
    assert "missing_provenance:build_manifest_path" in missing["reasons"]

    raw["provenance"]["build_manifest_path"] = manifest_path.name
    relative = backannotate_funcov.evaluate_artifact(raw)
    assert "invalid_provenance:build_manifest_path" in relative["reasons"]


def test_backannotation_runtime_manifest_gate_rejects_manifest_tamper(tmp_path):
    raw, manifest_path, _build_root = _eligible_artifact(tmp_path)
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    manifest["build_config"] = "tampered-build-config-with-different-size"
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

    gate = backannotate_funcov.evaluate_artifact(raw)

    assert not gate["eligible"]
    assert "build_manifest_runtime_mismatch:build_manifest_sha256" in gate["reasons"]
    assert "build_manifest_runtime_mismatch:build_config" in gate["reasons"]


def test_backannotation_runtime_manifest_gate_rejects_deleted_manifest(tmp_path):
    raw, manifest_path, _build_root = _eligible_artifact(tmp_path)
    manifest_path.unlink()

    gate = backannotate_funcov.evaluate_artifact(raw)

    assert not gate["eligible"]
    assert "build_manifest_runtime:missing" in gate["reasons"]
    assert "build_manifest_runtime:manifest_not_found" in gate["reasons"]


def test_backannotation_runtime_manifest_gate_rejects_build_artifact_tamper(tmp_path):
    raw, _manifest_path, build_root = _eligible_artifact(tmp_path)
    (build_root / "pylib-verilator" / "Frontend" / "libUTFrontend.so").write_bytes(
        b"tampered-dut-model"
    )

    gate = backannotate_funcov.evaluate_artifact(raw)

    assert not gate["eligible"]
    assert "build_manifest_runtime:invalid" in gate["reasons"]
    assert "build_manifest_runtime:build_hash_mismatch:dut_build_sha256" in gate["reasons"]
    assert "build_manifest_runtime_mismatch:dut_build_sha256" in gate["reasons"]


@pytest.mark.parametrize("simulator", ("verilator", "vcs"))
def test_build_manifest_binds_the_selected_simulator(tmp_path, simulator):
    build_root = tmp_path / "build-frontend"
    manifest_path, runtime = _write_build(build_root, simulator)

    assert manifest_path.name == f"frontend_build_manifest.{simulator}.json"
    assert runtime["build_manifest_status"] == "valid"


def test_build_manifest_rejects_simulator_mismatch(tmp_path):
    build_root = tmp_path / "build-frontend"
    manifest_path, _runtime = _write_build(build_root, "vcs")

    mismatched = load_frontend_build_manifest(
        build_root,
        manifest_path,
        simulator="verilator",
        pylib_dir=build_root / "pylib-vcs" / "Frontend",
    )

    assert mismatched["build_manifest_status"] == "invalid"
    assert "manifest_simulator_mismatch" in mismatched["build_manifest_reasons"]
