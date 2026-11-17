import hashlib
import json
from pathlib import Path

from .timing_replay_compare import build_report, write_json_exclusive


def _write(path, payload):
    path.write_text(json.dumps(payload, sort_keys=True) + "\n", encoding="ascii")
    return path


def _sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _qualification(tmp_path, label, hits, *, binding=None):
    artifact = _write(
        tmp_path / f"{label}.funcov.json",
        {
            "provenance": {
                "compatibility_signature": "contract",
                "definitions_sha256": "definitions",
            },
            "definitions": [{"bin_id": "BIN-1"}, {"bin_id": "BIN-2"}],
            "hits": {bin_id: {"bin_id": bin_id, "hits": 1} for bin_id in hits},
            "coverage_targets": {"bin_ids": []},
        },
    )
    integrity = {"status": "proved"}
    if binding is not None:
        integrity["inherited_input_binding"] = {
            "status": "proved",
            "sha256": _sha(binding),
        }
    return _write(
        tmp_path / f"{label}.qualification.json",
        {
            "scenario_id": "case",
            "replay_artifact_qualification": {"pass": True},
            "dut_pass": {"pass": True},
            "recorded_compatibility_signature": "contract",
            "input_integrity_review": integrity,
            "inputs": {"artifact": {"path": str(artifact), "sha256": _sha(artifact)}},
        },
    )


def _timing_audit(tmp_path, *, dynamic_delta=0, candidate_kind="timing_only"):
    scenario = {"only_timing_action_changed": candidate_kind == "timing_only"}
    if candidate_kind == "pbmt_nc_stall":
        scenario["only_pbmt_nc_attribute_and_timing_changed"] = True
    return _write(
        tmp_path / "timing.json",
        {
            "candidate_kind": candidate_kind,
            "qualification": {
                "pass": True,
                "status": "qualified",
                "input_qualification": True,
                "nemu_replay_required": False,
                "dut_replay_completed": False,
                "coverage_hit_claimed": False,
            },
            "cases": [
                {
                    "name": "case",
                    "pass": True,
                    "reasons": [],
                    "metrics": {
                        "cfg_edge_delta": 0,
                        "dynamic_instruction_delta": dynamic_delta,
                        "repeated_instruction_delta": 0,
                        "scope": "identical inherited program and NEMU trace",
                        "static_instruction_delta": 0,
                    },
                    "program_identity": {"source_and_generator_unchanged": True},
                    "scenario": scenario,
                }
            ],
        },
    )


def test_same_contract_timing_comparison_reports_raw_delta_only(tmp_path):
    timing = _timing_audit(tmp_path)
    original = _qualification(tmp_path, "original", ["BIN-1"])
    candidate = _qualification(tmp_path, "candidate", ["BIN-1", "BIN-2"], binding=timing)

    report = build_report(
        original_qualification_path=original,
        candidate_qualification_path=candidate,
        timing_candidate_audit_path=timing,
        case_name="case",
    )

    assert report["qualification"]["pass"] is True
    assert report["raw_coverage_comparison"]["candidate_only_raw_bin_ids"] == ["BIN-2"]
    assert report["raw_coverage_comparison"]["trusted_coverage_hit_claimed"] is False


def test_program_delta_or_contract_mismatch_fails_closed(tmp_path):
    timing = _timing_audit(tmp_path, dynamic_delta=1)
    original = _qualification(tmp_path, "original", ["BIN-1"])
    candidate = _qualification(tmp_path, "candidate", ["BIN-1"], binding=timing)
    artifact_path = json.loads(candidate.read_text())["inputs"]["artifact"]["path"]
    artifact = json.loads(open(artifact_path, encoding="utf-8").read())
    artifact["provenance"]["compatibility_signature"] = "other"
    _write(__import__("pathlib").Path(artifact_path), artifact)
    candidate_payload = json.loads(candidate.read_text())
    candidate_payload["inputs"]["artifact"]["sha256"] = _sha(
        __import__("pathlib").Path(artifact_path)
    )
    _write(candidate, candidate_payload)

    report = build_report(
        original_qualification_path=original,
        candidate_qualification_path=candidate,
        timing_candidate_audit_path=timing,
        case_name="case",
    )

    assert report["qualification"]["pass"] is False
    assert "timing_candidate_program_delta_not_zero" in report["qualification"]["reasons"]
    assert "compatibility_signature_mismatch" in report["qualification"]["reasons"]


def test_pbmt_nc_stall_delta_is_reviewed_but_does_not_weaken_contract_gate(tmp_path):
    timing = _timing_audit(tmp_path, candidate_kind="pbmt_nc_stall")
    original = _qualification(tmp_path, "original", ["BIN-1"])
    candidate = _qualification(
        tmp_path, "candidate", ["BIN-1", "BIN-2"], binding=timing
    )

    report = build_report(
        original_qualification_path=original,
        candidate_qualification_path=candidate,
        timing_candidate_audit_path=timing,
        case_name="case",
    )

    assert report["candidate_kind"] == "pbmt_nc_stall"
    assert report["qualification"]["pass"] is True
    assert report["raw_coverage_comparison"]["candidate_only_raw_bin_ids"] == [
        "BIN-2"
    ]

    artifact_path = Path(json.loads(candidate.read_text())["inputs"]["artifact"]["path"])
    artifact = json.loads(artifact_path.read_text())
    artifact["provenance"]["compatibility_signature"] = "new-contract"
    _write(artifact_path, artifact)
    candidate_payload = json.loads(candidate.read_text())
    candidate_payload["recorded_compatibility_signature"] = "new-contract"
    candidate_payload["inputs"]["artifact"]["sha256"] = _sha(artifact_path)
    _write(candidate, candidate_payload)

    rejected = build_report(
        original_qualification_path=original,
        candidate_qualification_path=candidate,
        timing_candidate_audit_path=timing,
        case_name="case",
    )
    assert rejected["qualification"]["pass"] is False
    assert "compatibility_signature_mismatch" in rejected["qualification"]["reasons"]


def test_exclusive_writer_refuses_overwrite(tmp_path):
    output = tmp_path / "report.json"
    write_json_exclusive(output, {"value": 1})
    try:
        write_json_exclusive(output, {"value": 2})
    except FileExistsError:
        pass
    else:
        raise AssertionError("existing report was overwritten")
