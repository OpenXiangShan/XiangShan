from __future__ import annotations

import json
from pathlib import Path

import pytest

from .test_candidate_offline_compare import _input_record, _sha256, _write_json

from .page_tail_repetition_sweep import (
    _expected_attribute_plan,
    _parse_candidate,
    _uniform_value,
    _variant_summary,
    build_report,
)


def _case(ordinal: int, dynamic: int, repeated: int) -> dict:
    return {
        "ordinal": ordinal,
        "offline_check": {"pass": True, "reasons": []},
        "dynamic_instruction_count": dynamic,
        "delta_from_original": {
            "static_program_map_instructions": 3,
            "text_span_bytes": 8170,
            "dynamic_instructions": 27,
            "unique_pcs": 7,
            "unique_cfg_edges": 6,
            "repeated_dynamic_instruction_proxy": repeated,
        },
    }


def test_variant_summary_requires_uniform_values_before_reporting_them():
    cases = [_case(0, 7081, 20), _case(1, 6808, 20)]
    summary = _variant_summary(1, cases)
    assert summary["all_cases_comparable"] is True
    assert summary["minimum_dynamic_instruction_count"] == 6808
    assert summary["maximum_dynamic_instruction_count"] == 7081
    assert summary["uniform_deltas_from_original"] == {
        "static_program_map_instructions": 3,
        "text_span_bytes": 8170,
        "dynamic_instructions": 27,
        "unique_pcs": 7,
        "unique_cfg_edges": 6,
        "repeated_dynamic_instruction_proxy": 20,
    }
    assert summary["incremental_density"]["unique_pc_fraction"] == 7 / 27
    assert summary["incremental_density"]["repeated_pc_proxy_fraction"] == 20 / 27

    cases[1]["delta_from_original"]["dynamic_instructions"] = 28
    assert _uniform_value(cases, ("delta_from_original", "dynamic_instructions")) is None


def test_candidate_argument_binds_repetition_to_manifest_path():
    repetitions, path = _parse_candidate("0x4=retained/campaign.json")
    assert repetitions == 4
    assert str(path) == "retained/campaign.json"


def test_attribute_plan_preserves_rvc_direction_while_pinning_rvi_pair():
    assert _expected_attribute_plan(0) == (
        "page_tail_rvc_nc_to_cacheable_rvi_nc_pair",
        [1, 0, 1, 0, 0],
    )
    assert _expected_attribute_plan(1) == (
        "page_tail_rvc_cacheable_to_nc_rvi_nc_pair",
        [0, 1, 0, 0, 0],
    )


@pytest.mark.parametrize("trace_count,complete", [(5000, True), (1, True), (5000, False)])
def test_sweep_recomputes_trace_floor_and_checks_original_completion(tmp_path, trace_count, complete):
    original = _input_record(tmp_path, "original", directed_pattern="none")
    candidate = _input_record(tmp_path, "candidate", directed_pattern="page_tail_pair_sparse", candidate=True)
    for record in (original, candidate):
        spec_path = Path(record["scenario_spec_path"])
        scenario = json.loads(spec_path.read_text())
        scenario["family"] = "translation_timing_cross"
        parameters = scenario["parameters"]
        parameters["generator_parameters"]["family"] = scenario["family"]
        record["spec"] = dict(parameters["generator_parameters"])
        if record is candidate:
            record["spec"]["page_tail_probe_repetitions"] = 1
            parameters["pattern_reuse"] = {
                "attribute_mode": "rvi_nc_pair", "probe_repetitions": 1,
                "probe_repetitions_decoupled_from_program_iterations": True,
            }
            profile, pattern = _expected_attribute_plan(0)
            parameters["translation"] = {"attribute_plan": {"profile": profile, "pma_pattern": pattern}}
            config = {"status": "valid", "binary_sha256": record["bin_sha256"]}
            record["configuration_preflight"] = config
            record["configuration_preflight_path"] = str(_write_json(tmp_path / "config.json", config))
            trace_path = Path(record["trace_path"])
            trace_path.write_text("".join(
                json.dumps({"index": index, "pc": "0x80000000", "size": 4}) + "\n"
                for index in range(trace_count)
            ))
            record["trace_sha256"] = _sha256(trace_path)
            record["dynamic_trace_instructions"] = 5000  # Campaign claim can disagree with the trace.
        _write_json(spec_path, scenario)
        record["scenario_spec_sha256"] = _sha256(spec_path)
    family = "translation_timing_cross"
    base = {"status": "pass", "master_seed": 17, "timing_master_seed": 29,
            "families": [family], "family_case_counts": {family: 1},
            "planned_case_count": 1, "repo_head": "historical-fixture"}
    original_path = _write_json(tmp_path / "original.json", {
        **base, "execution": {"complete": complete}, "cases": [original]})
    candidate_path = _write_json(tmp_path / "candidate.json", {
        **base, "execution": {"complete": True}, "cases": [candidate],
        "parameters": {"directed_pattern": "page_tail_pair_sparse",
                       "page_tail_attribute_mode": "rvi_nc_pair", "page_tail_probe_repetitions": 1}})
    report = build_report(original_path, {1: candidate_path})
    passed = trace_count == 5000 and complete
    assert report["offline_check"]["pass"] is passed
    assert report["selection"]["lowest_comparable_repetition_count"] == (1 if passed else None)
    assert report["qualification"]["pass"] is False
    assert report["canonical_hit_eligible"] is False
    if trace_count < 5000:
        assert "pr01:case_0:minimum_dynamic_instruction_count_not_met" in report["offline_check"]["reasons"]
    if not complete:
        assert "original_campaign_not_complete_pass" in report["offline_check"]["reasons"]
