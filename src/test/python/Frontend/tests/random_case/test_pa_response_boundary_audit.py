import json

import pytest

from .pa_response_boundary_audit import build_report, write_json_exclusive


def _fixture(tmp_path, *, alias=False):
    timeline = {
        "program_address_mapping": {
            "valid": True,
            "pages": [{"va": 0x80000000, "pa": 0x80400000}],
            "policy": "configured",
        },
        "timeline": [
            {
                "type": "handshake.icache_d",
                "cycle": 1,
                "payload": {"address": 0x80400FE0, "beat_idx": 0},
            }
        ],
        "actions": [],
        "dropped_timeline_events": 0,
        "program_map_binary_sha256": "bin",
        "selected_boundary_lines": ["0x80000fc0"],
        "timing_plan": {"maximum_backpressure_windows": 8},
    }
    if alias:
        timeline["program_address_mapping"]["pages"].append({"va": 0x80001000, "pa": 0x80400000})
    timeline_path = tmp_path / "timeline.json"
    map_path = tmp_path / "map.json"
    timeline_path.write_text(json.dumps(timeline))
    map_path.write_text(
        json.dumps(
            {
                "binary_sha256": "bin",
                "binary_text_verified": True,
                "binary_base": 0x80000000,
                "text_size_bytes": 0x1000,
                "instructions": [
                    {
                        "pc": 0x80000FE0,
                        "size": 4,
                        "encoding": "13000000",
                        "fetch_block_offset": 0,
                        "cacheline_offset": 32,
                        "page_offset": 0xFE0,
                        "episode_id": "boundary",
                        "crosses_fetch_block": False,
                        "crosses_cacheline": False,
                        "crosses_page": False,
                        "conditional_branch": True,
                    }
                ],
                "summary": {"static_instruction_count": 1},
            }
        )
    )
    return timeline_path, map_path


def test_audit_proves_mapping_but_not_exact_delivery(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    report = build_report(timeline, program_map)
    assert report["qualification"]["pass"] is True
    assert report["response_summary"]["mapping_status_counts"] == {"mapped": 1}
    record = report["records"][0]
    assert record["attribution_scope"] == "cacheline"
    assert record["exact_instruction_attribution"] is False
    assert record["exact_address_instruction_pcs"] == [0x80000FE0]


def test_cross_cacheline_instruction_is_indexed_on_both_lines(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    timeline_payload = json.loads(timeline.read_text())
    timeline_payload["timeline"][0]["payload"]["address"] = 0x80400040
    timeline.write_text(json.dumps(timeline_payload))
    map_payload = json.loads(program_map.read_text())
    map_payload["instructions"] = [
        {
            "pc": 0x8000003E,
            "size": 4,
            "encoding": "13000000",
            "fetch_block_offset": 30,
            "cacheline_offset": 62,
            "page_offset": 62,
            "episode_id": "cross-line",
            "crosses_fetch_block": True,
            "crosses_cacheline": True,
            "crosses_page": False,
        }
    ]
    program_map.write_text(json.dumps(map_payload))
    report = build_report(timeline, program_map)
    record = report["records"][0]
    assert record["cacheline_address"] == 0x80000040
    assert record["boundary_instruction_pcs"] == [0x8000003E]
    assert record["exact_address_instruction_pcs"] == [0x8000003E]


def test_audit_rejects_physical_alias(tmp_path):
    timeline, program_map = _fixture(tmp_path, alias=True)
    report = build_report(timeline, program_map)
    assert report["qualification"]["pass"] is False
    assert "response_mapping_ambiguous_alias=1" in report["qualification"]["reasons"]


@pytest.mark.parametrize(
    "page, reason",
    [
        ({"va": 0x80000001, "pa": 0x80400000}, "mapping_page_0_va_not_page_aligned"),
        ({"va": 0x80000000, "pa": 0x80400001}, "mapping_page_0_pa_not_page_aligned"),
        ({"va": -0x1000, "pa": 0x80400000}, "mapping_page_0_negative_address"),
        ({"va": 0x80000000}, "mapping_page_0_invalid_address"),
    ],
)
def test_audit_rejects_invalid_page_mapping(tmp_path, page, reason):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(timeline.read_text())
    payload["program_address_mapping"]["pages"] = [page]
    timeline.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["mapping"]["valid"] is False
    assert reason in report["qualification"]["reasons"]


def test_audit_rejects_explicitly_unusable_page_mapping(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(timeline.read_text())
    payload["program_address_mapping"]["pages"][0]["ok"] = False
    timeline.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert "mapping_page_0_not_usable" in report["qualification"]["reasons"]


def test_audit_rejects_virtual_page_conflict(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(timeline.read_text())
    payload["program_address_mapping"]["pages"].append(
        {"va": 0x80000000, "pa": 0x80500000}
    )
    timeline.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert "mapping_virtual_page_conflict:0x80000000" in report["qualification"]["reasons"]


def test_audit_rejects_duplicate_page_entries(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(timeline.read_text())
    payload["program_address_mapping"]["pages"].append(
        {"va": 0x80000000, "pa": 0x80400000}
    )
    timeline.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["qualification"]["pass"] is False
    assert "mapping_virtual_page_duplicate:0x80000000" in report["qualification"]["reasons"]
    assert "mapping_physical_page_duplicate:0x80400000" in report["qualification"]["reasons"]


def test_audit_accepts_distinct_multi_page_mapping(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(timeline.read_text())
    payload["program_address_mapping"]["pages"].append(
        {"va": 0x80001000, "pa": 0x80401000}
    )
    timeline.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["mapping"]["valid"] is True
    assert report["qualification"]["pass"] is True


def test_audit_rejects_program_map_binary_mismatch(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(program_map.read_text())
    payload["binary_sha256"] = "other-bin"
    program_map.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["program_map_binary_binding"]["pass"] is False
    assert report["qualification"]["pass"] is False
    assert "program_map_binary_binding_invalid" in report["qualification"]["reasons"]


def test_audit_rejects_overlapping_program_map_instructions(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(program_map.read_text())
    payload["instructions"].append(
        {
            "pc": 0x80000FE2,
            "size": 4,
            "crosses_fetch_block": False,
            "crosses_cacheline": False,
            "crosses_page": False,
        }
    )
    payload["summary"]["static_instruction_count"] = 2
    program_map.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["program_map_layout"]["pass"] is False
    assert "program_map_instruction_1_overlaps_previous:0x80000fe2" in report["qualification"]["reasons"]


def test_audit_rejects_false_spatial_boundary_flag(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(program_map.read_text())
    payload["instructions"][0]["crosses_page"] = True
    program_map.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["program_map_layout"]["pass"] is False
    assert "program_map_instruction_0_crosses_page_mismatch" in report["qualification"]["reasons"]


@pytest.mark.parametrize(
    "field, value, reason",
    [
        ("fetch_block_offset", 1, "program_map_instruction_0_fetch_block_offset_mismatch"),
        ("cacheline_offset", 1, "program_map_instruction_0_cacheline_offset_mismatch"),
        ("page_offset", 1, "program_map_instruction_0_page_offset_mismatch"),
        ("encoding", "0013", "program_map_instruction_0_encoding_size_mismatch"),
    ],
)
def test_audit_rejects_inconsistent_program_map_instruction_fields(
    tmp_path, field, value, reason
):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(program_map.read_text())
    payload["instructions"][0][field] = value
    program_map.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["program_map_layout"]["pass"] is False
    assert reason in report["qualification"]["reasons"]


def test_audit_rejects_instruction_outside_declared_text_span(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(program_map.read_text())
    payload["text_size_bytes"] = 4
    program_map.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert "program_map_instruction_0_outside_text_span" in report["qualification"]["reasons"]


def test_audit_accepts_top_level_action_attribution(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(timeline.read_text())
    payload["actions"] = [
        {
            "action_id": "a0",
            "trigger_event": "handshake.icache_d",
            "mapping_status": "mapped",
            "physical_address": 0x80400FC0,
            "program_address_candidates": [0x80000FC0],
            "episode_ids": ["boundary"],
            "cacheline_address": 0x80000FC0,
            "selected_boundary_line": True,
            "episode_ids": ["boundary"],
        }
    ]
    timeline.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["qualification"]["pass"] is True


def test_audit_rejects_action_mapping_claim_without_physical_evidence(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(timeline.read_text())
    payload["actions"] = [{"action_id": "a0", "mapping_status": "mapped"}]
    timeline.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["qualification"]["pass"] is False
    assert "action_not_mapped:a0:recompute" in report["qualification"]["reasons"]


def test_audit_rejects_action_cacheline_or_episode_mismatch(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(timeline.read_text())
    payload["actions"] = [
        {
            "action_id": "a0",
            "mapping_status": "mapped",
            "physical_address": 0x80400FC0,
            "program_address_candidates": [0x80000FC0],
            "cacheline_address": 0x80000F80,
            "episode_ids": ["wrong"],
        }
    ]
    timeline.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["qualification"]["pass"] is False
    assert "action_not_mapped:a0:cacheline_mismatch" in report["qualification"]["reasons"]


def test_audit_rejects_action_selected_boundary_mismatch(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(timeline.read_text())
    payload["actions"] = [
        {
            "action_id": "a0",
            "mapping_status": "mapped",
            "physical_address": 0x80400FC0,
            "program_address_candidates": [0x80000FC0],
            "cacheline_address": 0x80000FC0,
            "episode_ids": ["boundary"],
            "selected_boundary_line": False,
        }
    ]
    timeline.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["qualification"]["pass"] is False
    assert "action_not_mapped:a0:selected_boundary_mismatch" in report["qualification"]["reasons"]


def test_audit_rejects_selected_line_without_program_boundary(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(timeline.read_text())
    payload["selected_boundary_lines"] = ["0x80000000"]
    timeline.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["selected_boundary_lines"]["valid"] is False
    assert (
        "selected_boundary_line_without_program_boundary:0x80000000"
        in report["qualification"]["reasons"]
    )


def test_audit_rejects_empty_response_evidence(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(timeline.read_text())
    payload["timeline"] = []
    timeline.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["qualification"] == {
        "pass": False,
        "reasons": ["response_events_missing"],
        "status": "not_proved",
        "exact_instruction_delivery_claimed": False,
    }


@pytest.mark.parametrize(
    "payload, expected_reason",
    [
        (None, "payload_not_object"),
        ({}, "address_missing"),
        ({"address": "not-an-address"}, "address_invalid"),
        ({"address": -1}, "address_invalid"),
        ({"address": True}, "address_invalid"),
    ],
)
def test_audit_rejects_malformed_response_event(tmp_path, payload, expected_reason):
    timeline, program_map = _fixture(tmp_path)
    data = json.loads(timeline.read_text())
    data["timeline"][0]["payload"] = payload
    timeline.write_text(json.dumps(data))
    report = build_report(timeline, program_map)
    assert report["response_summary"]["observed_response_event_count"] == 1
    assert report["response_summary"]["malformed_response_event_count"] == 1
    assert report["qualification"]["pass"] is False
    assert report["qualification"]["status"] == "not_proved"
    assert "malformed_response_events=1" in report["qualification"]["reasons"]
    assert report["malformed_response_events"][0]["reason"] == expected_reason


def test_audit_rejects_invalid_action_physical_address_without_crashing(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(timeline.read_text())
    payload["actions"] = [
        {
            "action_id": "a0",
            "mapping_status": "mapped",
            "physical_address": "not-an-address",
        }
    ]
    timeline.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["qualification"]["pass"] is False
    assert "action_not_mapped:a0:recompute" in report["qualification"]["reasons"]
    assert report["actions"][0]["recomputed_mapping_status"] == "missing"


def test_audit_rejects_invalid_action_boundary_pcs_without_crashing(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(timeline.read_text())
    payload["actions"] = [
        {
            "action_id": "a0",
            "mapping_status": "mapped",
            "physical_address": 0x80400FC0,
            "program_address_candidates": [0x80000FC0],
            "cacheline_address": 0x80000FC0,
            "episode_ids": ["boundary"],
            "selected_boundary_line": True,
            "boundary_instruction_pcs": ["not-an-address"],
        }
    ]
    timeline.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["qualification"]["pass"] is False
    assert "action_not_mapped:a0:invalid_boundary_pcs" in report["qualification"]["reasons"]


def test_dropped_timeline_cannot_report_proved_status(tmp_path):
    timeline, program_map = _fixture(tmp_path)
    payload = json.loads(timeline.read_text())
    payload["dropped_timeline_events"] = 1
    timeline.write_text(json.dumps(payload))
    report = build_report(timeline, program_map)
    assert report["qualification"]["pass"] is False
    assert report["qualification"]["status"] == "not_proved"
    assert report["qualification"]["reasons"] == ["timeline_dropped_events"]


def test_audit_sidecar_writer_is_exclusive(tmp_path):
    output = tmp_path / "audit.json"
    write_json_exclusive(output, {"version": 1})
    with pytest.raises(FileExistsError):
        write_json_exclusive(output, {"version": 2})
