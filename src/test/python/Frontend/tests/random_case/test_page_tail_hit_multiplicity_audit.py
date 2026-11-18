from __future__ import annotations

import pytest

from .page_tail_hit_multiplicity_audit import (
    _event_count,
    _replay_binding_reasons,
    _target_hits,
)


def test_target_hits_extracts_only_frozen_bins_and_retained_cycles():
    artifact = {
        "hits": {
            "a": {
                "bin_id": "BIN-417",
                "hits": 11,
                "first_cycle": 10,
                "last_cycle": 20,
                "evidence": [
                    {"request_cycle": 12},
                    {"cycle": 19},
                    {"request_cycle": 12},
                ],
            },
            "ignored": {"bin_id": "BIN-1", "hits": 99},
        }
    }
    assert _target_hits(artifact) == {
        "BIN-417": {
            "hit_key": "a",
            "hit_count": 11,
            "first_cycle": 10,
            "last_cycle": 20,
            "retained_evidence_sample_count": 3,
            "retained_evidence_cycles": [12, 19],
        }
    }


def test_event_count_requires_exactly_one_record():
    analysis = {"events": [{"event": "trace.cross_page_instruction", "count": 11}]}
    assert _event_count(analysis, "trace.cross_page_instruction") == 11
    with pytest.raises(ValueError, match="expected one event count"):
        _event_count({"events": []}, "trace.cross_page_instruction")


def _binding_inputs() -> dict:
    full = {
        "name": "full-case",
        "program_seed": 11,
        "timing_seed": 22,
        "source": {"sha256": "source"},
        "binary": {"sha256": "binary"},
        "trace": {"sha256": "trace"},
        "program_map": {"sha256": "map"},
    }
    artifact = {
        "run": {
            "asm_sha256": "source",
            "bin_sha256": "binary",
            "trace_sha256": "trace",
            "seed": 11,
            "seeds": {"backend": 22},
        },
        "provenance": {"compatibility_signature": "contract"},
    }
    qualification = {
        "inputs": {"artifact": {"sha256": "artifact"}},
        "replay_artifact_qualification": {"pass": True},
        "dut_pass": {"pass": True},
        "recorded_compatibility_signature": "contract",
        "input_integrity_review": {
            "status": "proved",
            "failures": [],
            "campaign": {"sha256": "campaign", "case_name": "full-case"},
            "files": {
                name: {
                    "pass": True,
                    "expected_sha256": sha,
                    "actual_sha256": sha,
                    "recorded_sha256": sha,
                }
                for name, sha in (
                    ("source", "source"),
                    ("binary", "binary"),
                    ("trace", "trace"),
                    ("program_map", "map"),
                )
            },
        },
    }
    target_audit = {
        "coverage_contract": {"compatibility_signature": "contract"}
    }
    return {
        "full": full,
        "full_campaign_sha256": "campaign",
        "artifact": artifact,
        "artifact_sha256": "artifact",
        "qualification": qualification,
        "target_audit": target_audit,
    }


def test_replay_binding_accepts_exact_full_campaign_and_dut_inputs():
    assert _replay_binding_reasons(**_binding_inputs()) == []


def test_replay_binding_fails_closed_on_program_or_qualification_drift():
    inputs = _binding_inputs()
    inputs["artifact"]["run"]["asm_sha256"] = "different-source"
    inputs["qualification"]["input_integrity_review"]["campaign"][
        "sha256"
    ] = "different-campaign"
    inputs["qualification"]["input_integrity_review"]["files"]["trace"][
        "actual_sha256"
    ] = "different-trace"
    reasons = _replay_binding_reasons(**inputs)
    assert "full_DUT_source_sha256_mismatch" in reasons
    assert "qualification_campaign_hash_mismatch" in reasons
    assert "qualification_trace_hash_mismatch" in reasons
