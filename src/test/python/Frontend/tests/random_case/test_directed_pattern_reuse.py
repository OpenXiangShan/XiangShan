import json
from pathlib import Path

import pytest

from .generate_random_asm import CaseSpec, generate_case
from .program_map import analyze_trace
from .scenario_profiles import build_scenario_spec
from .testpoint_catalog import build_catalog, DEFAULT_SOURCE


def test_page_tail_pattern_is_opt_in_replayable_and_keeps_both_widths():
    original = CaseSpec(family="translation_timing_cross", seed=12, blocks=48)
    spec = CaseSpec(family=original.family, seed=12, blocks=48, directed_pattern="page_tail_pair")
    source, record = generate_case(spec)
    assert source == generate_case(spec)[0]
    assert spec.name == original.name + "_ptp"
    assert "cfg_page_rvc_probe" not in generate_case(original)[0]
    assert "cfg_page_rvc_probe:\n    c.nop" in source
    assert "jal ra, cfg_page_rvc_probe" in source
    assert "jal ra, cfg_page_tail_probe" in source
    scenario = build_scenario_spec(record, timing_seed=81, testpoint_catalog=build_catalog(DEFAULT_SOURCE))
    assert scenario.parameters["translation"]["attribute_plan"]["pma_pattern"] == [1, 0]
    assert scenario.parameters["translation"]["attribute_plan"]["pbmt"] == 0
    requirements = {r.event: r.min_count for r in scenario.required_events}
    assert requirements["trace.page_tail_rvc_fallthrough"] == spec.iterations
    assert requirements["trace.cross_page_instruction"] == spec.iterations
    assert scenario.to_dict()["coverage_attribution"]["single_target_bin"] is False
    reuse = scenario.parameters["pattern_reuse"]
    assert reuse["pattern_id"] == "page_tail_pair"
    assert len(reuse["source_tests"]) == 3
    assert reuse["source_sha256"] == "aee2f098ba49639a05cbc3e8498000b3ea80a2228c57ee03cda04fbc7d118309"
    evidence = reuse["reviewed_historical_dut_evidence"]
    assert evidence["admission_scope"] == "historical_diagnostic_only"
    assert evidence["canonical_hit_eligible"] is False
    assert evidence["dut_source_sha"] == "8724911fd6d94c1b953b6ab646584845da9c7fc5"
    assert evidence["artifacts"][0]["declared_bin_hits"] == [
        "BIN-416", "BIN-1107", "BIN-1117",
    ]
    assert evidence["artifacts"][2]["declared_bin_hits"] == []
    assert "rvi_resend_uses_next_page_attributes" in reuse["not_proven_by_program_trace"]

    _, reverse_record = generate_case(CaseSpec(
        family=original.family, seed=12, ordinal=1, blocks=48, directed_pattern="page_tail_pair",
    ))
    reverse = build_scenario_spec(reverse_record, timing_seed=81, testpoint_catalog=build_catalog(DEFAULT_SOURCE))
    assert reverse.parameters["translation"]["attribute_plan"]["pma_pattern"] == [0, 1]


def test_pattern_rejects_unsupported_family_and_keeps_original_attributes():
    with pytest.raises(ValueError, match="requires translation"):
        generate_case(CaseSpec(family="cfg_layout_mix", seed=1, directed_pattern="page_tail_pair"))
    _, record = generate_case(CaseSpec(family="translation_timing_cross", seed=12, blocks=48))
    scenario = build_scenario_spec(record, timing_seed=81, testpoint_catalog=build_catalog(DEFAULT_SOURCE))
    assert scenario.parameters["translation"]["attribute_plan"]["pma_pattern"] == [1]
    assert "pattern_reuse" not in scenario.parameters


def test_dense_page_tail_pair_executes_probe_padding_without_replacing_ptp():
    dense = CaseSpec(
        family="translation_timing_cross", seed=12, blocks=48,
        directed_pattern="page_tail_pair_dense",
    )
    source, record = generate_case(dense)
    assert dense.name.endswith("_ptpd")
    assert "cfg_page_rvc_probe:\n    .rept 2047" in source
    scenario = build_scenario_spec(record, timing_seed=81, testpoint_catalog=build_catalog(DEFAULT_SOURCE))
    requirements = {r.event: r.min_count for r in scenario.required_events}
    assert requirements["trace.page_tail_rvc_fallthrough"] == dense.iterations
    assert scenario.parameters["pattern_reuse"]["pattern_id"] == "page_tail_pair_dense"


def test_sparse_page_tail_pair_uses_nonexecuted_address_holes():
    sparse = CaseSpec(
        family="translation_timing_cross", seed=12, blocks=48,
        directed_pattern="page_tail_pair_sparse",
    )
    source, record = generate_case(sparse)
    assert sparse.name.endswith("_ptps")
    assert source.count("    .zero 4094") == 2
    assert "    .fill 2047, 2, 0x0001" not in source
    assert "cfg_page_rvc_probe:\n    c.nop" in source
    assert record["logical_static_instructions"] < generate_case(
        CaseSpec(
            family="translation_timing_cross", seed=12, blocks=48,
            directed_pattern="page_tail_pair",
        )
    )[1]["logical_static_instructions"]
    scenario = build_scenario_spec(
        record, timing_seed=81, testpoint_catalog=build_catalog(DEFAULT_SOURCE)
    )
    requirements = {r.event: r.min_count for r in scenario.required_events}
    assert requirements["trace.page_tail_rvc_fallthrough"] == sparse.iterations
    assert requirements["trace.cross_page_instruction"] == sparse.iterations
    assert scenario.parameters["pattern_reuse"]["pattern_id"] == "page_tail_pair_sparse"
    sparse_episodes = {
        episode["kind"]: episode for episode in record["episodes"]
        if episode["kind"] in {"page_tail_rvc", "page_crossing_rvi"}
    }
    assert {episode["parameters"]["padding_kind"] for episode in sparse_episodes.values()} == {
        "zero_hole"
    }


def test_sparse_page_tail_probe_repetitions_are_decoupled_from_program_length():
    reduced = CaseSpec(
        family="translation_timing_cross",
        seed=12,
        blocks=48,
        iterations=10,
        directed_pattern="page_tail_pair_sparse",
        page_tail_probe_repetitions=2,
    )
    source, record = generate_case(reduced)
    scenario = build_scenario_spec(
        record, timing_seed=81, testpoint_catalog=build_catalog(DEFAULT_SOURCE)
    )

    assert reduced.name.endswith("_ptps_pr02")
    assert "li t6, 2" in source
    assert "bltu t6, s0, cfg_page_pattern_skip" in source
    assert source.count("jal ra, cfg_page_rvc_probe") == 1
    requirements = {r.event: r.min_count for r in scenario.required_events}
    assert requirements["trace.page_tail_rvc_fallthrough"] == 2
    assert requirements["trace.cross_page_instruction"] == 2
    reuse = scenario.parameters["pattern_reuse"]
    assert reuse["probe_repetitions"] == 2
    assert reuse["probe_repetitions_decoupled_from_program_iterations"] is True
    rvc_episode = next(e for e in record["episodes"] if e["kind"] == "page_tail_rvc")
    assert rvc_episode["parameters"]["repetitions"] == 2

    for repetitions in (0, 10, 11):
        with pytest.raises(ValueError, match="must be in"):
            generate_case(
                CaseSpec(
                    family="translation_timing_cross",
                    seed=12,
                    blocks=48,
                    iterations=10,
                    directed_pattern="page_tail_pair_sparse",
                    page_tail_probe_repetitions=repetitions,
                )
            )
    with pytest.raises(ValueError, match="requires a page-tail pattern"):
        generate_case(
            CaseSpec(
                family="translation_timing_cross",
                seed=12,
                blocks=48,
                page_tail_probe_repetitions=2,
            )
        )


def test_sparse_rvi_nc_pair_changes_only_the_translation_attribute_pressure():
    sparse = CaseSpec(
        family="translation_timing_cross",
        seed=12,
        blocks=48,
        directed_pattern="page_tail_pair_sparse",
    )
    source, record = generate_case(sparse)
    scenario = build_scenario_spec(
        record,
        timing_seed=81,
        testpoint_catalog=build_catalog(DEFAULT_SOURCE),
        page_tail_attribute_mode="rvi_nc_pair",
    )

    assert source == generate_case(sparse)[0]
    assert scenario.parameters["pattern_reuse"]["attribute_mode"] == "rvi_nc_pair"
    plan = scenario.parameters["translation"]["attribute_plan"]
    assert plan == {
        "schema_version": 1,
        "profile": "page_tail_rvc_nc_to_cacheable_rvi_nc_pair",
        "pbmt": 0,
        "pbmte": 0,
        "pma_pattern": [1, 0, 1, 0, 0],
    }

    _, reverse_record = generate_case(
        CaseSpec(
            family="translation_timing_cross",
            seed=12,
            ordinal=1,
            blocks=48,
            directed_pattern="page_tail_pair_sparse",
        )
    )
    reverse = build_scenario_spec(
        reverse_record,
        timing_seed=81,
        testpoint_catalog=build_catalog(DEFAULT_SOURCE),
        page_tail_attribute_mode="rvi_nc_pair",
    )
    assert reverse.parameters["translation"]["attribute_plan"] == {
        "schema_version": 1,
        "profile": "page_tail_rvc_cacheable_to_nc_rvi_nc_pair",
        "pbmt": 0,
        "pbmte": 0,
        "pma_pattern": [0, 1, 0, 0, 0],
    }

    with pytest.raises(ValueError, match="requires a page-tail pattern"):
        build_scenario_spec(
            generate_case(
                CaseSpec(family="translation_timing_cross", seed=12, blocks=48)
            )[1],
            timing_seed=81,
            testpoint_catalog=build_catalog(DEFAULT_SOURCE),
            page_tail_attribute_mode="rvi_nc_pair",
        )


def test_sparse_rvi_pbmt_nc_pair_preserves_rvc_contrast_and_targets_rvi_pages():
    _, record = generate_case(
        CaseSpec(
            family="translation_timing_cross",
            seed=12,
            blocks=48,
            directed_pattern="page_tail_pair_sparse",
        )
    )
    scenario = build_scenario_spec(
        record,
        timing_seed=81,
        testpoint_catalog=build_catalog(DEFAULT_SOURCE),
        page_tail_attribute_mode="rvi_pbmt_nc_pair",
    )

    assert scenario.parameters["pattern_reuse"]["attribute_mode"] == "rvi_pbmt_nc_pair"
    assert scenario.parameters["translation"]["attribute_plan"] == {
        "schema_version": 1,
        "profile": "page_tail_rvc_pma_nc_rvi_pbmt_nc_pair",
        "pbmt": 0,
        "pbmte": 1,
        "pma_pattern": [1, 0, 1, 1, 1],
        "pbmt_pattern": [0, 0, 0, 1, 1],
    }


def test_uncache_a_stall_reuse_is_timing_only_and_profile_scoped():
    sparse = CaseSpec(
        family="translation_timing_cross",
        seed=12,
        blocks=48,
        directed_pattern="page_tail_pair_sparse",
    )
    source, record = generate_case(sparse)
    catalog = build_catalog(DEFAULT_SOURCE)
    control = build_scenario_spec(
        record,
        timing_seed=81,
        testpoint_catalog=catalog,
        page_tail_attribute_mode="rvi_pbmt_nc_pair",
    )
    candidate = build_scenario_spec(
        record,
        timing_seed=81,
        testpoint_catalog=catalog,
        page_tail_attribute_mode="rvi_pbmt_nc_pair",
        uncache_a_stall_cycles=8,
    )

    assert source == generate_case(sparse)[0]
    assert candidate.parameters["generator_parameters"] == control.parameters[
        "generator_parameters"
    ]
    assert candidate.parameters["translation"] == control.parameters["translation"]
    assert "uncache_a_stall_cycles" not in control.parameters["timing_plan"]
    assert candidate.parameters["timing_plan"]["uncache_a_stall_cycles"] == 8
    assert candidate.parameters["uncache_a_stall_reuse"]["policy"].endswith(
        "REALIZED is not a coverage HIT"
    )
    required = {
        item.event: item.min_count for item in candidate.required_events
    }
    assert required["dut.uncache_a_stall_cycle"] == 8
    assert required["dut.uncache_a_stall_release"] == 1

    with pytest.raises(ValueError, match="uncache A stall requires"):
        build_scenario_spec(
            record,
            timing_seed=81,
            testpoint_catalog=catalog,
            page_tail_attribute_mode="alternating",
            uncache_a_stall_cycles=8,
        )
    with pytest.raises(ValueError, match="rvi_pbmt_nc_pair"):
        build_scenario_spec(
            record,
            timing_seed=81,
            testpoint_catalog=catalog,
            page_tail_attribute_mode="rvi_nc_pair",
            uncache_a_stall_cycles=8,
        )


def test_page_tail_replay_script_keeps_sparse_variant_contract():
    """Inspect closed replay semantics as text; never run its check/run branches."""
    import re

    script = _historical_wrapper_fixture("replay_page_tail_candidates.sh")
    source = script.read_text(encoding="utf-8")
    assert "variant=${PAGE_TAIL_VARIANT:-ptp}" in source
    assert (
        "campaign_root=/tmp/frontend-random-ptps-seed0-20260921-n2p5s7CQ/campaign"
        in source
    )
    assert (
        "campaign_root=/tmp/frontend-random-ptps-rvi-nc-alternating-20260921-GvSfdHtj/campaign"
        in source
    )
    assert (
        "campaign_root=/tmp/frontend-random-ptpsr-pr01-sweep-20260921-a1"
        in source
    )
    assert "suffix=ptps" in source
    assert "suffix=ptps_pr01" in source
    assert "expected_attribute_mode=rvi_nc_pair" in source
    assert "expected_probe_repetitions=1" in source
    assert "target_bins='BIN-417 BIN-1120 BIN-1121'" in source
    assert (
        "expected_compatibility_signature=${DUT_COMPATIBILITY_SIGNATURE:-"
        "5a758b0571b506ea7df5f650bf98b32eb336e7fc24fb60b2e29b0bfcddcbf552}"
        in source
    )

    # These are historical variant/selection rules, not current admission evidence.
    variants = {
        match.group(1): match.group(2)
        for match in re.finditer(r"^  (ptp|ptpd|ptps|ptpsr|ptpsr1)\)\n(.*?)^    ;;", source, re.M | re.S)
    }
    assert set(variants) == {"ptp", "ptpd", "ptps", "ptpsr", "ptpsr1"}
    assert "suffix=ptps\n" in variants["ptps"]
    assert "suffix=ptps\n" in variants["ptpsr"]
    assert "expected_attribute_mode=rvi_nc_pair" in variants["ptpsr"]
    assert "suffix=ptps_pr01" in variants["ptpsr1"]
    assert "expected_probe_repetitions=1" in variants["ptpsr1"]
    assert "expected_pma_pattern='[1,0,1,0,0]'" in variants["ptpsr1"]
    assert 'if [[ -n "$case_index_override" && "$variant" != ptpsr1 ]]; then' in source
    assert 'if [[ -n "$case_index_override" && "$case_index_override" != 1 ]]; then' in source
    assert 'if [[ "$variant" != ptps && "$variant" != ptpsr && "$variant" != ptpsr1 ]]; then' in source
    assert '(( index >= 0 && index < ${#names[@]} )) || {' in source
    assert 'Candidate index $index is unavailable for PAGE_TAIL_VARIANT=$variant' in source

    # The old seed1 exception required an explicit ptpsr1 override and index=1.
    seed1 = source.split('if [[ "$variant" == ptpsr1 && "$case_index_override" == 1 ]]; then', 1)[1].split("\nelse\n", 1)[0]
    assert "campaign_root=/tmp/frontend-random-page-tail-native-replay-J9tiAVby" in seed1
    assert 'campaign="$campaign_root/candidate-campaign.json"' in seed1
    assert 'names=(fe_random_translation_timing_cross_sa4bbc3009c8cfca2_o001_"$suffix")' in seed1
    assert "selected_seed_index=1" in seed1
    assert "expected_attribute_profile=page_tail_rvc_cacheable_to_nc_rvi_nc_pair" in seed1
    assert "expected_pma_pattern='[0,1,0,0,0]'" in seed1
    assert '[[ "$index" == 1 ]] || {' in source
    assert "Explicit seed1 entry requires check/run index 1" in source
    assert "program_seeds=(4823640145504506223 11870295650258975906)" in source
    assert "timing_seeds=(12181526746822571996 11807444488609381302)" in source
    assert '[[ "$scenario_program_seed" == "${program_seeds[candidate_index]}" ]] || exit 1' in source
    assert '[[ "$scenario_timing_seed" == "${timing_seeds[candidate_index]}" ]] || exit 1' in source

    # Historical check validated all input hashes and manifest before any slot/run.
    assert "for kind in source bin trace program_map scenario_spec; do" in source
    assert '[[ "${actual%% *}" == "$expected" ]] || {' in source
    assert 'if build["build_manifest_sha256"] != expected_manifest_sha:' in source
    assert 'raise SystemExit("chosen DUT manifest hash mismatch")' in source
    assert 'if signature != expected_signature:' in source
    check_exit = source.index('[[ "$mode" == run ]] || exit 0')
    slot_gate = source.index('[[ "${TB_DUT_SLOT_RELEASED:-0}" == 1 ]]')
    pipeline = source.index('bash "$frontend/scripts/run_bin_trace_pipeline.sh"')
    assert source.index('chosen DUT manifest hash mismatch') < check_exit < slot_gate < pipeline


def test_historical_wrapper_fixtures_are_archives_without_active_entries():
    directory = Path(__file__).parent / "fixtures" / "historical"
    manifest = json.loads((directory / "manifest.json").read_text())
    assert "Historical text fixtures only" in manifest["scope"]
    for entry in manifest["fixtures"].values():
        fixture = _historical_wrapper_fixture(entry["original_filename"])
        assert fixture.parent == directory
        assert not (Path(__file__).parent / entry["original_filename"]).exists()


def _historical_wrapper_fixture(name):
    """Review archived text only; these closed replays are not current entries."""
    import hashlib
    import json
    import stat

    directory = Path(__file__).parent / "fixtures" / "historical"
    fixture = directory / (name + ".txt")
    manifest = json.loads((directory / "manifest.json").read_text())
    entry = manifest["fixtures"][fixture.name]
    assert entry["original_filename"] == name
    assert entry["executable_entry"] is False
    assert fixture.suffix == ".txt"
    assert not fixture.stat().st_mode & (stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
    assert hashlib.sha256(fixture.read_bytes()).hexdigest() == entry["sha256"]
    return fixture


def test_uncache_a_stall_replay_script_freezes_candidate_and_slot_contract():
    script = _historical_wrapper_fixture("replay_uncache_a_stall_candidate.sh")
    source = script.read_text(encoding="utf-8")

    assert (
        "candidate_root=/tmp/frontend-random-ptpsr-pbmt-nc-stall8-input-5Aaiy1y1"
        in source
    )
    assert (
        "expected_campaign_sha="
        "5061856fc1b3af874d887b17beae6b57c5b3a94bc4f1946a07490f1cb0ebf324"
        in source
    )
    assert (
        "expected_timing_audit_sha="
        "c0c3a7a64246f64b201da0c3d848409c9f6537658f83c31359623c5b97c9fd91"
        in source
    )
    assert "target_bins='BIN-1095 BIN-1096 BIN-1129 BIN-1130'" in source
    assert '[[ "${TB_DUT_SLOT_RELEASED:-0}" == 1 ]]' in source
    assert "export TB_SKIP_NEMU=1 TB_RUN_DUT=1" in source
    assert (
        "expected_compatibility_signature=${DUT_COMPATIBILITY_SIGNATURE:-"
        "4d9422de77523df36b98918a9c279514c9335581d4ddec09f180385e2c79f508}"
        in source
    )
    assert 'required_events.get("dut.uncache_a_stall_cycle") == 8' in source
    assert 'required_events.get("dut.uncache_a_stall_release") == 1' in source


def test_uncache_a_stall_post_run_audit_is_offline_and_complete():
    script = _historical_wrapper_fixture("audit_uncache_a_stall_replay.sh")
    source = script.read_text(encoding="utf-8")

    assert "translation_replay_audit.py" in source
    assert "--inherited-input-audit" in source
    assert "pa_response_boundary_audit.py" in source
    assert "timing_replay_compare.py" in source
    assert "target_hit_audit.py" in source
    assert (
        "expected_campaign_sha="
        "5061856fc1b3af874d887b17beae6b57c5b3a94bc4f1946a07490f1cb0ebf324"
        in source
    )
    assert (
        "expected_timing_audit_sha="
        "c0c3a7a64246f64b201da0c3d848409c9f6537658f83c31359623c5b97c9fd91"
        in source
    )
    assert "comparison_status=$?" in source
    assert source.index("target_hit_audit.py") > source.index("comparison_status=$?")
    assert "TB_RUN_DUT" not in source
    assert "run_bin_trace_pipeline.sh" not in source
    assert "pytest" not in source
    assert "nemu" not in source.lower()


def test_taken_cj_offset_ring_is_reusable_and_requires_all_executed_offsets():
    original = CaseSpec(family="cfg_layout_mix", seed=12, blocks=48)
    ring = CaseSpec(
        family="cfg_layout_mix",
        seed=12,
        blocks=48,
        directed_pattern="taken_cj_offset_ring",
    )
    source, record = generate_case(ring)
    assert source == generate_case(ring)[0]
    assert ring.name == original.name + "_cjor"
    assert "cfg_cj_offset_00:" not in generate_case(original)[0]
    for offset in range(16):
        label = f"cfg_cj_offset_{offset:02d}:"
        target = f"cfg_cj_offset_{offset + 1:02d}" if offset < 15 else "cfg_cj_ring_finish"
        assert label in source
        assert f"c.j {target}" in source
    episodes = [
        episode for episode in record["episodes"]
        if episode["kind"] == "taken_cj_offset_ring"
    ]
    assert [episode["parameters"]["halfword_offset"] for episode in episodes] == list(range(16))
    scenario = build_scenario_spec(
        record,
        timing_seed=81,
        testpoint_catalog=build_catalog(DEFAULT_SOURCE),
    )
    requirements = {requirement.event: requirement.min_count for requirement in scenario.required_events}
    assert requirements["trace.taken_cj_offset_ring"] == 16
    reuse = scenario.parameters["pattern_reuse"]
    assert reuse["pattern_id"] == "taken_cj_offset_ring"
    assert reuse["source_tests"] == ["test_ifu_owner_taken_cfi_covers_all_end_offsets"]
    assert "BIN-974_current_DUT_hit" in reuse["not_proven_by_program_trace"]


def test_taken_cj_offset_ring_rejects_non_cfg_family():
    with pytest.raises(ValueError, match="requires a CFG or translation family"):
        generate_case(
            CaseSpec(
                family="ras_indirect_history",
                seed=12,
                directed_pattern="taken_cj_offset_ring",
            )
        )


def test_taken_cj_offset_ring_permutation_varies_sequence_not_offset_set():
    specs = [
        CaseSpec(
            family="translation_timing_cross",
            seed=seed,
            ordinal=ordinal,
            blocks=48,
            directed_pattern="taken_cj_offset_ring_permuted",
        )
        for ordinal, seed in enumerate((12, 13))
    ]
    observed_orders = []
    for spec in specs:
        source, record = generate_case(spec)
        assert source == generate_case(spec)[0]
        assert spec.name.endswith("_cjorp")
        episodes = [
            episode for episode in record["episodes"]
            if episode["kind"] == "taken_cj_offset_ring"
        ]
        ordered = sorted(
            episodes,
            key=lambda episode: episode["parameters"]["execution_position"],
        )
        offsets = [episode["parameters"]["halfword_offset"] for episode in ordered]
        observed_orders.append(offsets)
        assert sorted(offsets) == list(range(16))
        assert offsets != list(range(16))
        assert f"jal zero, cfg_cj_offset_{offsets[0]:02d}" in source
        for current, successor in zip(ordered, ordered[1:]):
            current_offset = current["parameters"]["halfword_offset"]
            next_offset = successor["parameters"]["halfword_offset"]
            assert current["parameters"]["successor_offset"] == next_offset
            assert f"cfg_cj_offset_{current_offset:02d}:" in source
            assert f"c.j cfg_cj_offset_{next_offset:02d}" in source
        assert ordered[-1]["parameters"]["successor_offset"] is None
        scenario = build_scenario_spec(
            record,
            timing_seed=81,
            testpoint_catalog=build_catalog(DEFAULT_SOURCE),
        )
        requirements = {
            requirement.event: requirement.min_count
            for requirement in scenario.required_events
        }
        assert requirements["trace.taken_cj_offset_ring"] == 16
        assert requirements["trace.taken_cj_offset_transition"] == 15
        assert scenario.parameters["pattern_reuse"]["pattern_id"] == (
            "taken_cj_offset_ring_permuted"
        )
    assert observed_orders[0] != observed_orders[1]


@pytest.mark.parametrize("size, successor, expected", [(2, 0x2000, 1), (4, 0x2002, 0), (2, 0x3000, 0)])
def test_rvc_page_fallthrough_requires_actual_consecutive_pc(tmp_path, size, successor, expected):
    trace = tmp_path / "trace.jsonl"
    trace.write_text(json.dumps({"pc": "0x1ffe"}) + "\n" + json.dumps({"pc": hex(successor)}) + "\n")
    report = analyze_trace(trace, {"binary_sha256": "test", "instructions": [
        {"pc": 0x1FFE, "size": size}, {"pc": successor, "size": 4},
    ]})
    events = {e["event"]: e["count"] for e in report["events"]}
    assert events.get("trace.page_tail_rvc_fallthrough", 0) == expected
