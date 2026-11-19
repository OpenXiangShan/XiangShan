import hashlib
import json
from pathlib import Path

import pytest

from . import translation_replay_audit
from .translation_replay_audit import (
    _input_integrity_review,
    _post_replay_audit_only_paths,
    _replay_provenance_review,
    _scenario_review,
    audit_translation_replay,
)


def _write_inherited_input_fixture(tmp_path):
    paths = {
        name: tmp_path / filename
        for name, filename in {
            "source": "case.S",
            "binary": "case.bin",
            "trace": "case.trace.jsonl",
            "program_map": "case.program_map.json",
            "scenario": "case.scenario_spec.json",
        }.items()
    }
    for label, path in paths.items():
        path.write_text(f"{label}\n", encoding="utf-8")

    digest = lambda path: hashlib.sha256(path.read_bytes()).hexdigest()
    campaign = tmp_path / "campaign.json"
    campaign.write_text(
        json.dumps(
            {
                "cases": [
                    {
                        "name": "candidate",
                        "source_path": str(paths["source"]),
                        "source_sha256": digest(paths["source"]),
                        "scenario_spec_path": str(paths["scenario"]),
                        "scenario_spec_sha256": digest(paths["scenario"]),
                    }
                ]
            }
        ),
        encoding="utf-8",
    )

    def evidence(path):
        sha = digest(path)
        return {
            "path": str(path),
            "actual_sha256": sha,
            "recorded_sha256": sha,
            "pass": True,
        }

    binding = tmp_path / "timing-candidate-audit.json"
    binding.write_text(
        json.dumps(
            {
                "qualification": {
                    "pass": True,
                    "status": "qualified",
                    "input_qualification": True,
                    "nemu_replay_required": False,
                    "dut_replay_completed": False,
                    "coverage_hit_claimed": False,
                },
                "inputs": {
                    "candidate_campaign": {
                        "path": str(campaign),
                        "sha256": digest(campaign),
                    }
                },
                "cases": [
                    {
                        "name": "candidate",
                        "pass": True,
                        "reasons": [],
                        "inherited_nemu_inputs": {
                            "binary": evidence(paths["binary"]),
                            "trace": evidence(paths["trace"]),
                            "program_map": evidence(paths["program_map"]),
                        },
                    }
                ],
            }
        ),
        encoding="utf-8",
    )
    artifact = {
        "run": {
            "asm_path": str(paths["source"]),
            "asm_sha256": digest(paths["source"]),
            "bin_path": str(paths["binary"]),
            "bin_sha256": digest(paths["binary"]),
            "trace_path": str(paths["trace"]),
            "trace_sha256": digest(paths["trace"]),
        }
    }
    return paths, campaign, binding, artifact


def _evidence():
    provenance = {key: "fixed" for key in (
        "dut_source_sha", "dut_build_sha256", "generated_rtl_sha256", "signal_contract_sha256",
    )}
    artifact = {
        "provenance": provenance,
        "checker": {"status": "pass"}, "outcome": {"status": "passed"},
        "stats": {
            "translation_oracle": {"records": [], "errors": [], "error_count": 0},
            "backend": {"ftq_entries_pending": 0, "pending_resolves": 0, "pending_events": 0},
        },
    }
    timeline = {"execution_status": "REALIZED", "dropped_timeline_events": 0,
                "details": {"golden_cursor": 6000, "golden_entries": 6000}}
    return artifact, timeline, {"dut_source_sha": "fixed", "artifacts": provenance}


def test_passing_trace_does_not_prove_speculative_exception_ownership():
    artifact, timeline, manifest = _evidence()
    record = {"kind": "speculative_cfvec_exception", "cycle": 100, "pc": 0}
    artifact["stats"]["translation_oracle"]["records"].append(record)
    report = audit_translation_replay(artifact, timeline, manifest)
    assert report["review_reasons"] == ["speculative_exceptions_require_causal_review"]
    assert report["speculative_exception_causal_review"]["status"] == "unproved"


def test_drain_and_dut_changes_remain_visible_even_when_checker_passes():
    artifact, timeline, manifest = _evidence()
    artifact["stats"]["backend"]["ftq_entries_pending"] = 2
    manifest["dut_source_sha"] = "new"
    report = audit_translation_replay(artifact, timeline, manifest)
    assert report["review_reasons"] == ["current_dut_baseline_not_established"]
    assert report["baseline_differences"]["dut_source_sha"] == {"recorded": "fixed", "current": "new"}
    assert report["completion_review"]["full_backend_drain_claimed"] is False


def test_missing_evidence_is_not_a_clean_audit():
    report = audit_translation_replay({}, {}, {})
    assert report["review_status"] == "requires_review"
    assert "translation_oracle_evidence_missing" in report["review_reasons"]
    assert "timeline_missing_or_dropped_events" in report["review_reasons"]


def test_clean_selected_checks_do_not_grant_coverage_eligibility():
    report = audit_translation_replay(*_evidence())
    assert report["review_reasons"] == []
    assert report["review_status"] == "no_flags_in_selected_checks"
    assert "eligible" not in report
    assert report["replay_artifact_qualification"]["pass"] is False
    assert report["coverage_hit_eligibility"]["eligible"] is False


def test_scenario_review_accounts_for_independent_uncache_stall_action():
    backend = {
        "action_id": "backend-backpressure-000",
        "kind": "backend_can_accept_low",
        "status": "REALIZED",
        "confirmed_low_cycles": 3,
        "duration_cycles": 3,
    }
    uncache = {
        "action_id": "uncache-a-stall-000",
        "kind": "uncache_a_ready_low",
        "status": "REALIZED",
        "confirmed_low_cycles": 8,
        "duration_cycles": 8,
    }
    timeline = {
        "execution_status": "REALIZED",
        "dropped_timeline_events": 0,
        "timing_plan": {
            "maximum_backpressure_windows": 1,
            "uncache_a_stall_cycles": 8,
        },
        "actions": [backend, uncache],
        "timeline": [
            {"type": event_type, "payload": action}
            for action in (backend, uncache)
            for event_type in ("scenario.action_start", "scenario.action_complete")
        ],
    }

    review = _scenario_review(timeline)

    assert review["status"] == "proved"
    assert review["expected_action_count"] == 2
    assert review["action_kind_counts"] == {
        "backend_can_accept_low": 1,
        "uncache_a_ready_low": 1,
    }


def test_input_integrity_accepts_hash_bound_inherited_nemu_inputs(tmp_path):
    paths, campaign, binding, artifact = _write_inherited_input_fixture(tmp_path)

    report = _input_integrity_review(
        artifact,
        program_map_path=paths["program_map"],
        campaign_path=campaign,
        case_name="candidate",
        inherited_input_audit_path=binding,
    )

    assert report["status"] == "proved"
    assert report["failures"] == []
    assert report["inherited_input_binding"]["status"] == "proved"
    assert all(item["pass"] for item in report["files"].values())


def test_input_integrity_accepts_identical_archived_campaign_copy(tmp_path):
    paths, campaign, binding, artifact = _write_inherited_input_fixture(tmp_path)
    archived_campaign = tmp_path / "candidate-campaign.json"
    archived_campaign.write_bytes(campaign.read_bytes())

    report = _input_integrity_review(
        artifact,
        program_map_path=paths["program_map"],
        campaign_path=archived_campaign,
        case_name="candidate",
        inherited_input_audit_path=binding,
    )

    assert report["status"] == "proved"
    assert report["inherited_input_binding"]["candidate_campaign_recorded_path"] == str(
        campaign
    )
    assert (
        report["inherited_input_binding"]["candidate_campaign_recorded_actual_sha256"]
        == hashlib.sha256(archived_campaign.read_bytes()).hexdigest()
    )


def test_input_integrity_rejects_changed_inherited_binary(tmp_path):
    paths, campaign, binding, artifact = _write_inherited_input_fixture(tmp_path)
    paths["binary"].write_text("changed\n", encoding="utf-8")

    report = _input_integrity_review(
        artifact,
        program_map_path=paths["program_map"],
        campaign_path=campaign,
        case_name="candidate",
        inherited_input_audit_path=binding,
    )

    assert report["status"] == "unproved"
    assert "inherited_binary_binding_invalid" in report["failures"]


def test_input_integrity_accepts_relocated_program_map_with_only_source_path_delta(tmp_path):
    paths, campaign, binding, artifact = _write_inherited_input_fixture(tmp_path)
    digest = lambda path: hashlib.sha256(path.read_bytes()).hexdigest()
    original_source = paths["source"]
    replay_source = tmp_path / "replay-copy.S"
    replay_source.write_bytes(original_source.read_bytes())
    canonical_map = {
        "binary_sha256": digest(paths["binary"]),
        "binary_text_verified": True,
        "instructions": [{"address": 0x80000000, "text": "addi zero, zero, 0"}],
        "source_path": str(original_source),
    }
    replay_map = {**canonical_map, "source_path": str(replay_source)}
    paths["program_map"].write_text(json.dumps(canonical_map), encoding="utf-8")
    observed_map = tmp_path / "replay.program_map.json"
    observed_map.write_text(json.dumps(replay_map), encoding="utf-8")
    binding_payload = json.loads(binding.read_text(encoding="utf-8"))
    inherited_map = binding_payload["cases"][0]["inherited_nemu_inputs"]["program_map"]
    inherited_map.update({
        "actual_sha256": digest(paths["program_map"]),
        "recorded_sha256": digest(paths["program_map"]),
        "pass": True,
    })
    binding.write_text(json.dumps(binding_payload), encoding="utf-8")

    report = _input_integrity_review(
        artifact,
        program_map_path=observed_map,
        campaign_path=campaign,
        case_name="candidate",
        inherited_input_audit_path=binding,
    )

    program_map = report["files"]["program_map"]
    assert report["status"] == "proved"
    assert program_map["byte_exact"] is False
    assert program_map["pass"] is True
    assert program_map["path_metadata_equivalence"] == {
        "expected_program_map_path": str(paths["program_map"]),
        "observed_program_map_path": str(observed_map),
        "ignored_metadata_fields": ["source_path"],
        "source_sha256": digest(original_source),
        "binary_sha256": digest(paths["binary"]),
        "pass": True,
        "source_paths_bound": True,
        "binary_bound": True,
        "semantic_json_equal": True,
        "reason": "path_metadata_only",
    }


def test_input_integrity_rejects_relocated_program_map_with_semantic_delta(tmp_path):
    paths, campaign, binding, artifact = _write_inherited_input_fixture(tmp_path)
    digest = lambda path: hashlib.sha256(path.read_bytes()).hexdigest()
    replay_source = tmp_path / "replay-copy.S"
    replay_source.write_bytes(paths["source"].read_bytes())
    canonical_map = {
        "binary_sha256": digest(paths["binary"]),
        "binary_text_verified": True,
        "instructions": [{"address": 0x80000000, "text": "addi zero, zero, 0"}],
        "source_path": str(paths["source"]),
    }
    paths["program_map"].write_text(json.dumps(canonical_map), encoding="utf-8")
    observed_map = tmp_path / "replay.program_map.json"
    observed_map.write_text(
        json.dumps({
            **canonical_map,
            "source_path": str(replay_source),
            "instructions": [{"address": 0x80000000, "text": "jal zero, 0"}],
        }),
        encoding="utf-8",
    )
    binding_payload = json.loads(binding.read_text(encoding="utf-8"))
    inherited_map = binding_payload["cases"][0]["inherited_nemu_inputs"]["program_map"]
    inherited_map.update({
        "actual_sha256": digest(paths["program_map"]),
        "recorded_sha256": digest(paths["program_map"]),
        "pass": True,
    })
    binding.write_text(json.dumps(binding_payload), encoding="utf-8")

    report = _input_integrity_review(
        artifact,
        program_map_path=observed_map,
        campaign_path=campaign,
        case_name="candidate",
        inherited_input_audit_path=binding,
    )

    program_map = report["files"]["program_map"]
    assert report["status"] == "unproved"
    assert "program_map" in report["failures"]
    assert program_map["path_metadata_equivalence"]["semantic_json_equal"] is False
    assert program_map["pass"] is False


def test_native_exception_is_proved_by_ftq_generation_and_older_redirect():
    artifact, timeline, manifest = _evidence()
    record = {
        "kind": "speculative_cfvec_exception",
        "cycle": 20,
        "pc": 0,
        "folded_pc": 0,
        "fault": "instruction_page_fault",
        "cross_page": False,
    }
    artifact["stats"]["translation_oracle"]["records"].append(record)
    artifact["stats"]["monitor"] = {"error_count": 0, "exception_mark_count": 1}
    artifact["coverage_targets"] = {"bin_ids": []}
    timeline.update({
        "program_address_mapping": {
            "valid": True,
            "pages": [{"va": 0x80000000, "pa": 0x80400000}],
        },
        "program_map_binary_sha256": "bin",
        "timing_plan": {"maximum_backpressure_windows": 0},
        "actions": [],
        "timeline": [
            {
                "cycle": 12,
                "type": "audit.native_ftq_write",
                "payload": {"ftq_idx_raw": 11, "start_pc_addr_raw": 0},
            },
            {
                "cycle": 20,
                "type": "audit.native_cfvec_exception",
                "payload": {
                    "ftq_flag": 0,
                    "ftq_value": 11,
                    "ftq_offset": 0,
                    "is_rvc": 1,
                    "folded_pc": 0,
                    "can_accept": 1,
                    "slot": 0,
                    "exception_bits": {"1": 0, "2": 0, "12": 1, "19": 0, "20": 0},
                },
            },
            {
                "cycle": 20,
                "type": "translation.speculative_cfvec_exception",
                "payload": record,
            },
            {
                "cycle": 27,
                "type": "audit.native_redirect",
                "payload": {"ftq_flag": 0, "ftq_value": 10},
            },
        ],
    })
    program_map = {
        "binary_text_verified": True,
        "binary_sha256": "bin",
        "summary": {"static_instruction_count": 1},
    }
    artifact["run"] = {"bin_sha256": "bin"}
    log = (
        "run until golden complete: cursor=6000 entries=6000 cycles=100 "
        "completion_pending_work=0 backend_pending_work=2"
    )
    report = audit_translation_replay(
        artifact, timeline, manifest, program_map, case_log_text=log
    )
    assert report["review_reasons"] == []
    assert report["speculative_exception_causal_review"]["status"] == "proved"
    assert report["speculative_exception_causal_review"]["classification_counts"] == {
        "later_older_redirect_flush": 1
    }
    assert report["completion_review"]["status"] == "proved"
    assert report["replay_artifact_qualification"]["pass"] is True
    assert report["coverage_hit_eligibility"] == {
        "eligible": False,
        "reason": "diagnostic_run_has_no_declared_target_bins",
        "declared_target_bin_ids": [],
        "raw_observed_hit_count": 0,
    }


def _provenance_review_for_changed_path(tmp_path, changed_path: Path) -> dict:
    manifest_before = tmp_path / "manifest.before.json"
    manifest_after = tmp_path / "manifest.after.json"
    environment_before = tmp_path / "environment.before.diff"
    environment_after = tmp_path / "environment.after.diff"
    python_hashes = tmp_path / "python-inputs.before.sha256"
    manifest_before.write_text("same manifest\n")
    manifest_after.write_text("same manifest\n")
    environment_before.write_text("same worktree\n")
    environment_after.write_text("same worktree\n")
    python_hashes.write_text(f"{'0' * 64}  {changed_path.resolve()}\n")
    return _replay_provenance_review(
        manifest_before=manifest_before,
        manifest_after=manifest_after,
        environment_before=environment_before,
        environment_after=environment_after,
        python_hashes=python_hashes,
    )


def test_post_replay_audit_only_allowlist_is_explicit_and_complete():
    assert {Path(path).name for path in _post_replay_audit_only_paths()} == {
        "translation_replay_audit.py",
        "test_translation_replay_audit.py",
        "target_hit_audit.py",
        "test_target_hit_audit.py",
        "candidate_offline_compare.py",
        "test_candidate_offline_compare.py",
        "pa_response_boundary_audit.py",
        "test_pa_response_boundary_audit.py",
        "timing_replay_compare.py",
        "test_timing_replay_compare.py",
    }


@pytest.fixture
def isolated_audit_dir(tmp_path, monkeypatch):
    audit_dir = tmp_path / "random_case"
    audit_dir.mkdir()
    monkeypatch.setattr(translation_replay_audit, "__file__", str(audit_dir / "translation_replay_audit.py"))
    return audit_dir


@pytest.mark.parametrize(
    "basename", sorted(Path(path).name for path in _post_replay_audit_only_paths())
)
def test_post_replay_audit_tool_delta_preserves_provenance(tmp_path, isolated_audit_dir, basename):
    changed_path = isolated_audit_dir / basename
    changed_path.write_text("# changed offline audit tool\n", encoding="utf-8")
    report = _provenance_review_for_changed_path(tmp_path, changed_path)
    assert report["status"] == "proved"
    assert report["post_replay_audit_only_deltas"] == [str(changed_path)]
    assert report["execution_relevant_python_deltas"] == []


def test_missing_audit_only_input_still_fails_provenance(tmp_path, isolated_audit_dir):
    missing_path = isolated_audit_dir / "target_hit_audit.py"
    report = _provenance_review_for_changed_path(tmp_path, missing_path)
    assert report["status"] == "unproved"
    assert report["python_inputs_missing"] == [str(missing_path)]
    assert report["post_replay_audit_only_deltas"] == []


@pytest.mark.parametrize(
    "runtime_basename",
    (
        "generate_random_asm.py",
        "scenario_coordinator.py",
        "trusted_funcov.py",
        "test_random_bin_trace_dut.py",
    ),
)
def test_runtime_delta_cannot_be_hidden_by_audit_only_allowlist(
    tmp_path, isolated_audit_dir, runtime_basename
):
    runtime_path = isolated_audit_dir / runtime_basename
    runtime_path.write_text("# changed replay runtime component\n", encoding="utf-8")
    report = _provenance_review_for_changed_path(tmp_path, runtime_path)
    assert report["status"] == "unproved"
    assert report["post_replay_audit_only_deltas"] == []
    assert report["execution_relevant_python_deltas"] == [str(runtime_path)]
