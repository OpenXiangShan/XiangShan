"""Pure Python native report gates; fixtures are synthetic, never DUT/corpus."""
import hashlib
import json

import pytest

from .funcov_compare import build_report, load_artifact
from .native_funcov_adapter import _json_sha


def _write(path, payload):
    path.write_text(json.dumps(payload))
    return hashlib.sha256(path.read_bytes()).hexdigest()


@pytest.fixture
def native(tmp_path):
    execution = {"seeds": {"test": 12, "backend": 34}, "run_command": "test-only"}
    for kind in ("asm", "bin", "trace"):
        p = tmp_path / kind
        p.write_bytes(b"synthetic input")
        execution[kind + "_path"] = str(p)
        execution[kind + "_sha256"] = hashlib.sha256(p.read_bytes()).hexdigest()
    raw = {"groups": [{"name": "g", "points": [{"name": "p", "dynamic_bin": False,
            "bins": [{"name": "hit", "hints": 2}, {"name": "miss", "hints": 0}]}]}],
           "bin_num_total": 2,
           "run_metadata": {"run_id": "case", "outcome": "passed", "exit_code": 0,
                            "checker": {"status": "pass", "error_count": 0, "errors": []},
                            "execution": execution}}
    definitions = [{"bin_id": "test-" + name, "coverage_group": "g", "coverpoint": "p",
                    "bin_name": name} for name in ("hit", "miss")]
    provenance = {k: "a" * 64 for k in (
        "compatibility_signature", "build_manifest_sha256", "dut_build_sha256",
        "dut_python_extension_sha256", "generated_rtl_sha256", "signal_contract_sha256",
        "registry_sha256", "sampler_sha256", "verification_env_sha256")}
    provenance.update(dut_source_sha="b" * 40, definitions_sha256=_json_sha(definitions),
                      build_manifest_status="valid")
    contract = {"definitions": definitions, "provenance": provenance,
                "minimum_dynamic_instructions": 5000}
    timeline = {"scenario_id": "case", "program_map_binary_sha256": execution["bin_sha256"],
                "timing_plan": {"timing_seed": 34}, "execution_status": "REALIZED",
                "dropped_timeline_events": 0,
                "details": {"monitor_errors": 0, "translation_oracle_error_count": 0,
                            "translation_oracle_errors": [], "golden_entries": 6526,
                            "golden_cursor": 6526}}
    return tmp_path, raw, contract, timeline


def _load(native):
    root, raw, contract, timeline = native
    artifact = root / "toffee.funcov.json"
    binding = {"schema_version": 1, "artifact_sha256": _write(artifact, raw), "run_id": "case"}
    for key, filename, payload in (("frozen_contract", "frozen.json", contract),
                                    ("scenario_timeline", "timeline.json", timeline)):
        binding[key] = {"path": filename, "sha256": _write(root / filename, payload)}
    sidecar = root / "coord-sidecar.json"
    pin = _write(sidecar, binding)
    return load_artifact(artifact, native_sidecar=sidecar, native_sidecar_sha256=pin)


def test_unbound_native_preserves_counts_and_never_becomes_zero_hit_pass(native):
    root, raw, _, _ = native
    artifact = root / "toffee.funcov.json"
    _write(artifact, raw)
    before = artifact.read_bytes()
    result = load_artifact(artifact)
    assert not result.eligible and "native_frozen_sidecar_missing" in result.eligibility_reason
    assert result.native_bin_count == 2 and result.native_hit_bin_count == 1
    assert not result.definition_bin_ids and not result.hit_bin_ids
    report = build_report([result], [], minimum_qualified_runs=1)
    assert report["random"]["union_hit_bins"] is None
    assert not report["qualification"]["pass"]
    assert report["ineligible_random_artifacts"][0]["native_hit_bin_count"] == 1
    assert artifact.read_bytes() == before


def test_bound_completed_native_maps_full_identity(native):
    result = _load(native)
    assert result.eligible, result.eligibility_reason
    assert result.hit_bin_ids == {"test-hit"}
    assert result.definition_bin_ids == {"test-hit", "test-miss"}
    assert result.seed_pair == (12, 34)


@pytest.mark.parametrize("field", ["outcome", "exit_code", "checker", "run_id"])
def test_native_missing_runtime_health_fails_closed(native, field):
    del native[1]["run_metadata"][field]
    assert not _load(native).eligible


def test_checker_fail_zero_errors_is_still_failure(native):
    native[1]["run_metadata"]["checker"]["status"] = "fail"
    result = _load(native)
    assert not result.eligible and "native_checker_not_proved_clean" in result.eligibility_reason


@pytest.mark.parametrize("field", ["monitor_errors", "translation_oracle_error_count",
                                   "translation_oracle_errors", "golden_cursor", "golden_entries"])
def test_native_missing_timeline_health_fails_closed(native, field):
    del native[3]["details"][field]
    assert not _load(native).eligible


@pytest.mark.parametrize("change", ["short", "incomplete", "failed", "dropped", "identity", "timing",
                                    "mapping", "duplicate", "hints", "count", "input", "definitions",
                                    "sampler", "manifest", "bool_health", "no_seeds"])
def test_native_rejects_incomplete_or_incompatible_evidence(native, change):
    _, raw, contract, timeline = native
    if change == "short": timeline["details"].update(golden_entries=4999, golden_cursor=4999)
    elif change == "incomplete": timeline["details"]["golden_cursor"] -= 1
    elif change == "failed": timeline["execution_status"] = "FAIL"
    elif change == "dropped": timeline["dropped_timeline_events"] = 1
    elif change == "identity": timeline["scenario_id"] = "other"
    elif change == "timing": timeline["timing_plan"]["timing_seed"] += 1
    elif change == "mapping": contract["definitions"][0]["coverpoint"] = "wrong"
    elif change == "duplicate": raw["groups"] *= 2
    elif change == "hints": raw["groups"][0]["points"][0]["bins"][0]["hints"] = True
    elif change == "count": raw["bin_num_total"] = 3
    elif change == "input": raw["run_metadata"]["execution"]["trace_sha256"] = "wrong"
    elif change == "definitions": contract["provenance"]["definitions_sha256"] = "wrong"
    elif change == "sampler": del contract["provenance"]["sampler_sha256"]
    elif change == "manifest": contract["provenance"]["build_manifest_status"] = "invalid"
    elif change == "bool_health": timeline["details"]["monitor_errors"] = False
    elif change == "no_seeds": del raw["run_metadata"]["execution"]["seeds"]
    assert not _load(native).eligible


def test_native_sidecar_pin_and_bound_files_cannot_change(native):
    assert _load(native).eligible
    root = native[0]
    artifact, sidecar = root / "toffee.funcov.json", root / "coord-sidecar.json"
    pin = hashlib.sha256(sidecar.read_bytes()).hexdigest()
    assert not load_artifact(artifact, native_sidecar=sidecar).eligible
    original = artifact.read_bytes()
    artifact.write_bytes(original + b" ")
    assert not load_artifact(artifact, native_sidecar=sidecar, native_sidecar_sha256=pin).eligible
    artifact.write_bytes(original)
    (root / "timeline.json").write_text("{}")
    assert not load_artifact(artifact, native_sidecar=sidecar, native_sidecar_sha256=pin).eligible
    sidecar.write_text("{}")
    assert not load_artifact(artifact, native_sidecar=sidecar, native_sidecar_sha256=pin).eligible


@pytest.mark.parametrize("field", ["provenance", "definitions"])
def test_malformed_frozen_contract_is_ineligible(native, field):
    native[2][field] = [] if field == "provenance" else None
    assert not _load(native).eligible
