from __future__ import annotations

import json

import pytest

from .program_map import analyze_trace


def _analyze(tmp_path, entries, instructions):
    trace = tmp_path / "trace.jsonl"
    trace.write_text("".join(json.dumps({"index": i, **entry}) + "\n" for i, entry in enumerate(entries)))
    program_map = {"binary_sha256": "fixture", "instructions": instructions}
    return analyze_trace(trace, program_map)


def test_diversity_distinguishes_repetition_from_new_branch_outcomes(tmp_path):
    result = _analyze(tmp_path, [
        {"pc": "0x1000", "taken": True}, {"pc": "0x2000"},
        {"pc": "0x1000", "taken": True}, {"pc": "0x2000"},
        {"pc": "0x1000", "taken": False}, {"pc": "0x1004"},
    ], [{"pc": 0x1000, "size": 4, "conditional_branch": True},
        {"pc": 0x1004, "size": 4}, {"pc": 0x2000, "size": 2}])
    d = result["program_diversity"]
    assert d["dynamic_instruction_count"] == 6
    assert d["unique_pc_count"] == 3
    assert d["max_pc_visit_count"] == 3
    assert d["dynamic_control_flow_edges"] == 3
    assert d["unique_control_flow_edges"] == 2
    assert d["conditional_branch_sites_with_both_outcomes"] == 1
    assert d["branch_outcomes"] == [{"pc": "0x1000", "taken": 2, "not_taken": 1}]
    assert d["instruction_start_boundary_transitions"]["page"] == 4


def test_indirect_target_diversity_uses_next_executed_pc_not_declared_target(tmp_path):
    result = _analyze(tmp_path, [
        {"pc": "0x1000", "target_pc": "0xbad"}, {"pc": "0x1020"},
        {"pc": "0x1000", "target_pc": "0xbad"}, {"pc": "0x1040"},
        {"pc": "0x1000", "target_pc": "0xbad"},
    ], [{"pc": 0x1000, "size": 4, "indirect_jump": True},
        {"pc": 0x1020, "size": 2}, {"pc": 0x1040, "size": 4}])
    d = result["program_diversity"]
    assert d["indirect_sites_with_observed_successor"] == 1
    assert d["indirect_sites_with_multiple_targets"] == 1
    assert d["indirect_targets"][0]["targets"] == [
        {"pc": "0x1020", "count": 1}, {"pc": "0x1040", "count": 1},
    ]
    assert d["dynamic_control_flow_edges"] == 2


def test_missing_map_entry_cannot_invent_a_control_flow_edge(tmp_path):
    result = _analyze(tmp_path, [
        {"pc": "0x1000"}, {"pc": "0x2000"}, {"pc": "0x3000"},
    ], [{"pc": 0x1000, "size": 4, "indirect_jump": True}, {"pc": 0x3000, "size": 4}])
    assert result["missing_program_map_pc_count"] == 1
    assert result["program_diversity"]["unique_control_flow_edges"] == 0


def test_empty_trace_has_no_diversity_observations(tmp_path):
    result = _analyze(tmp_path, [], [])
    assert result["program_diversity"]["max_pc_visit_count"] == 0
    assert result["program_diversity"]["indirect_targets"] == []


def test_analyzer_rejects_overlapping_external_program_map(tmp_path):
    with pytest.raises(ValueError, match="overlap"):
        _analyze(
            tmp_path,
            [{"pc": "0x1000"}],
            [{"pc": 0x1000, "size": 4}, {"pc": 0x1002, "size": 2}],
        )


def test_analyzer_rejects_trace_size_mismatch(tmp_path):
    with pytest.raises(ValueError, match="trace instruction size mismatch"):
        _analyze(
            tmp_path,
            [{"pc": "0x1000", "size": 2}],
            [{"pc": 0x1000, "size": 4}],
        )


def test_target_region_diversity_distinguishes_local_targets_and_page_cross(tmp_path):
    instructions = [
        {"pc": 0x1000, "size": 4, "indirect_jump": True},
        {"pc": 0x2000, "size": 4, "indirect_jump": True},
        *({"pc": pc, "size": 4} for pc in (0x1020, 0x1040, 0x1060, 0x1080, 0x3000)),
    ]
    entries = []
    for target in (0x1020, 0x1040, 0x1060, 0x1080):
        entries.extend([{"pc": "0x1000"}, {"pc": hex(target)}])
    # One far target crosses a page, but is not a multi-page target set.
    entries.extend([{"pc": "0x2000"}, {"pc": "0x3000"}])
    d = _analyze(tmp_path, entries, instructions)["program_diversity"]
    regions = d["indirect_target_region_diversity"]
    assert regions["cacheline"]["sites_with_targets_in_multiple_regions"] == 1
    assert regions["page"] == {
        "sites_with_targets_in_multiple_regions": 0,
        "sites_with_target_outside_source_region": 1,
    }
    entries.extend([{"pc": "0x2000"}, {"pc": "0x1080"}])
    regions = _analyze(tmp_path, entries, instructions)["program_diversity"]["indirect_target_region_diversity"]
    assert regions["page"]["sites_with_targets_in_multiple_regions"] == 1


def test_four_target_event_requires_actual_successors(tmp_path):
    instructions = [{"pc": 0x1000, "size": 4, "indirect_jump": True}]
    entries = []
    for target in (0x1020, 0x1040, 0x1060, 0x1080):
        entries.extend([{"pc": "0x1000"}, {"pc": hex(target)}])
        instructions.append({"pc": target, "size": 4})
    result = _analyze(tmp_path, entries, instructions)
    events = {e["event"]: e["count"] for e in result["events"]}
    assert events["trace.indirect_four_target_site"] == 1
    assert events["trace.indirect_four_target_page_site"] == 0
    result = _analyze(tmp_path, entries[:-2], instructions)
    events = {e["event"]: e["count"] for e in result["events"]}
    assert events["trace.indirect_four_target_site"] == 0


def test_four_page_event_requires_four_observed_target_pages(tmp_path):
    instructions = [{"pc": 0x1000, "size": 4, "indirect_jump": True}]
    entries = []
    for target in (0x2000, 0x3000, 0x4000, 0x5000):
        entries.extend([{"pc": "0x1000"}, {"pc": hex(target)}])
        instructions.append({"pc": target, "size": 4})
    events = {e["event"]: e["count"] for e in _analyze(tmp_path, entries, instructions)["events"]}
    assert events["trace.indirect_four_target_page_site"] == 1
    events = {e["event"]: e["count"] for e in _analyze(tmp_path, entries[:-2], instructions)["events"]}
    assert events["trace.indirect_four_target_page_site"] == 0


def test_taken_cj_offset_ring_counts_distinct_executed_offsets(tmp_path):
    instructions = []
    entries = []
    for offset in range(16):
        pc = 0x1000 + offset * 0x40 + offset * 2
        instructions.append(
            {
                "pc": pc,
                "size": 2,
                "mnemonic": "c.j",
                "direct_jump": True,
                "episode_kind": "taken_cj_offset_ring",
                "episode_parameters": {"halfword_offset": offset},
            }
        )
        entries.append({"pc": hex(pc)})
    result = _analyze(tmp_path, entries, instructions)
    events = {event["event"]: event["count"] for event in result["events"]}
    assert events["trace.taken_cj_offset_ring"] == 16
    assert events["trace.taken_cj_offset_transition"] == 15
    assert result["program_diversity"]["taken_cj_offset_ring_offsets"] == list(range(16))
    assert result["program_diversity"]["taken_cj_offset_ring_sequence"] == list(range(16))

    result = _analyze(tmp_path, entries[:-1], instructions)
    events = {event["event"]: event["count"] for event in result["events"]}
    assert events["trace.taken_cj_offset_ring"] == 15
    assert events["trace.taken_cj_offset_transition"] == 14
    assert result["program_diversity"]["taken_cj_offset_ring_offsets"] == list(range(15))
