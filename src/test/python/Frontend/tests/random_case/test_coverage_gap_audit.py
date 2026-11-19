from __future__ import annotations

from .coverage_gap_audit import build_gap_inventory


def test_gap_inventory_separates_unhit_bins_from_shadow_quarantine() -> None:
    definitions = [
        {
            "bin_id": "BIN-001",
            "coverage_group": "ifu_instr_compact",
            "coverpoint": "layout",
            "bin_name": "contiguous",
            "suggested_testcase": "compact_case",
        },
        {
            "bin_id": "BIN-002",
            "coverage_group": "icache_missunit_fencei",
            "coverpoint": "fence",
            "bin_name": "response",
        },
        {
            "bin_id": "BIN-003",
            "coverage_group": "ifu_redirect",
            "coverpoint": "redirect",
            "bin_name": "recovery",
        },
    ]
    report = {
        "coverage": {
            "raw_union_bin_ids": ["BIN-001", "BIN-003"],
            "trusted_union_bin_ids": ["BIN-003"],
        },
        "quarantine": {
            "reasons_by_bin_id": {"BIN-001": ["compact_module_shadow_first"]},
            "rules": [
                {
                    "rule_id": "compact_module_shadow_first",
                    "source_path": "env/funcov/compact_funcov.py",
                }
            ],
        },
    }

    inventory = build_gap_inventory(definitions, report)
    by_id = {entry["bin_id"]: entry for entry in inventory["bins"]}

    assert by_id["BIN-001"]["classification"] == "raw_hit_quarantined"
    assert by_id["BIN-001"]["next_action"] == "repair_semantic_sampler_then_replay"
    assert by_id["BIN-001"]["sampler_source_paths"] == ["env/funcov/compact_funcov.py"]
    assert by_id["BIN-002"]["classification"] == "unhit"
    assert by_id["BIN-002"]["next_action"] == "add_icache_timing_or_maintenance_scenario"
    assert by_id["BIN-003"]["classification"] == "trusted_hit"
    assert inventory["summary"]["classification_counts"] == {
        "raw_hit_quarantined": 1,
        "trusted_hit": 1,
        "unhit": 1,
    }
