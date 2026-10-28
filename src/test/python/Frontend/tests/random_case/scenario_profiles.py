from __future__ import annotations

from dataclasses import replace
from typing import Any

try:
    from .scenario_contract import EventRequirement, ScenarioSpec
    from .scenario_timing import TimingPlan
    from .scenario_testpoint_mapping import FAMILY_TESTPOINT_IDS, REVIEWED_TESTPOINT_MOVES, _resolve_testpoint_ids
    from .translation_attributes import (
        PBMT_NC,
        PBMT_PMA,
        TranslationAttributePlan,
        translation_attribute_plan,
    )
except ImportError:
    from scenario_contract import EventRequirement, ScenarioSpec
    from scenario_timing import TimingPlan
    from scenario_testpoint_mapping import FAMILY_TESTPOINT_IDS, REVIEWED_TESTPOINT_MOVES, _resolve_testpoint_ids
    from translation_attributes import (
        PBMT_NC,
        PBMT_PMA,
        TranslationAttributePlan,
        translation_attribute_plan,
    )


PROFILE_SCHEMA_VERSION = 1
PAGE_TAIL_ATTRIBUTE_MODES = ("alternating", "rvi_nc_pair", "rvi_pbmt_nc_pair")


def _page_tail_attribute_plan(
    ordinal: int,
    mode: str,
) -> TranslationAttributePlan:
    if mode == "alternating":
        return translation_attribute_plan(4 + int(ordinal) % 2)
    if mode == "rvi_nc_pair":
        # The sparse probes occupy the tails of virtual pages 1 and 3.  Keep
        # the RVC probe's direction alternating by ordinal, then make both
        # pages of the RVI probe non-cacheable so the second half must return
        # through InstrUncache rather than switching to ICache.
        if int(ordinal) % 2 == 0:
            profile = "page_tail_rvc_nc_to_cacheable_rvi_nc_pair"
            pattern = (True, False, True, False, False)
        else:
            profile = "page_tail_rvc_cacheable_to_nc_rvi_nc_pair"
            pattern = (False, True, False, False, False)
        return TranslationAttributePlan(profile, PBMT_PMA, 0, pattern)
    if mode == "rvi_pbmt_nc_pair":
        # The RVI probe and its next page occupy slots three and four. Keep
        # them cacheable in PMA, then select InstrUncache through PBMT.NC.
        # The RVC leg retains its alternating PMA contrast.
        if int(ordinal) % 2 == 0:
            profile = "page_tail_rvc_pma_nc_rvi_pbmt_nc_pair"
            pma_pattern = (True, False, True, True, True)
        else:
            profile = "page_tail_rvc_cacheable_rvi_pbmt_nc_pair"
            pma_pattern = (False, True, False, True, True)
        return TranslationAttributePlan(
            profile,
            PBMT_PMA,
            1,
            pma_pattern,
            (PBMT_PMA, PBMT_PMA, PBMT_PMA, PBMT_NC, PBMT_NC),
        )
    raise ValueError(f"unsupported page-tail attribute mode: {mode}")

FAMILY_RISK_TAGS: dict[str, tuple[str, ...]] = {
    "cfg_layout_mix": (
        "control_flow_history",
        "rvi_rvc_mix",
        "taken_not_taken_transition",
        "direct_indirect_target_mix",
        "fetch_block_boundary",
        "cacheline_boundary",
        "page_boundary",
        "predictor_revisit",
    ),
    "ras_indirect_history": (
        "ras_nested_call_return",
        "x1_x5_link_registers",
        "indirect_target_history",
        "rvi_rvc_mix",
        "fetch_block_boundary",
        "cacheline_boundary",
        "page_boundary",
        "predictor_revisit",
    ),
    "ibuffer_pressure_cross": (
        "ibuffer_occupancy",
        "long_backend_backpressure",
        "response_during_stall",
        "redirect_during_stall",
        "rvi_rvc_mix",
        "fetch_block_boundary",
        "cacheline_boundary",
        "page_boundary",
    ),
    "translation_timing_cross": (
        "translation_context_switch",
        "ptw_latency_and_ready",
        "sfence_ptw_intersection",
        "pmp_pma_attribute_cross",
        "sector_tlb",
        "superpage_translation",
        "translation_cacheable_uncache_cross",
        "rvi_rvc_mix",
        "fetch_block_boundary",
        "cacheline_boundary",
        "page_boundary",
    ),
}

COMMON_REQUIRED_EVENTS = (
    EventRequirement(event="trace.committed_instruction", min_count=5000, evidence_stage="program"),
    EventRequirement(event="trace.rvi_instruction", evidence_stage="program"),
    EventRequirement(event="trace.rvc_instruction", evidence_stage="program"),
    EventRequirement(event="trace.conditional_branch", evidence_stage="program"),
    EventRequirement(event="trace.branch_taken", evidence_stage="program"),
    EventRequirement(event="trace.branch_not_taken", evidence_stage="program"),
    EventRequirement(event="trace.direct_jump", evidence_stage="program"),
    EventRequirement(event="trace.indirect_jump", evidence_stage="program"),
    EventRequirement(event="trace.cross_fetch_block_instruction", evidence_stage="program"),
    EventRequirement(event="trace.cross_page_instruction", evidence_stage="program"),
)

DUT_REQUIRED_EVENTS = (
    EventRequirement(event="dut.icache_request", evidence_stage="dut"),
    EventRequirement(event="dut.icache_response", evidence_stage="dut"),
    EventRequirement(event="dut.backend_redirect", evidence_stage="dut"),
    EventRequirement(event="dut.backend_backpressure_window", evidence_stage="dut"),
    EventRequirement(
        event="dut.icache_response_x_backend_backpressure",
        evidence_stage="dut",
    ),
)

FAMILY_REQUIRED_EVENTS: dict[str, tuple[EventRequirement, ...]] = {
    "cfg_layout_mix": (
        *COMMON_REQUIRED_EVENTS,
        EventRequirement(event="trace.call", evidence_stage="program"),
        EventRequirement(event="trace.return", evidence_stage="program"),
        *DUT_REQUIRED_EVENTS,
    ),
    "ras_indirect_history": (
        *COMMON_REQUIRED_EVENTS,
        EventRequirement(event="trace.call", evidence_stage="program"),
        EventRequirement(event="trace.return", evidence_stage="program"),
        EventRequirement(event="trace.max_call_depth", min_count=2, evidence_stage="program"),
        *DUT_REQUIRED_EVENTS,
    ),
    "ibuffer_pressure_cross": (
        *COMMON_REQUIRED_EVENTS,
        EventRequirement(event="trace.call", evidence_stage="program"),
        EventRequirement(event="trace.return", evidence_stage="program"),
        *DUT_REQUIRED_EVENTS,
        EventRequirement(
            event="dut.ibuffer_full_x_backend_backpressure",
            evidence_stage="dut",
        ),
        EventRequirement(
            event="dut.redirect_x_backend_backpressure",
            evidence_stage="dut",
        ),
    ),
    "translation_timing_cross": (
        *COMMON_REQUIRED_EVENTS,
        EventRequirement(event="trace.call", evidence_stage="program"),
        EventRequirement(event="trace.return", evidence_stage="program"),
        # Translation may resolve to either the cacheable ICache path or the
        # PMA non-cacheable path.  The coordinator records both under the
        # path-independent fetch events; path-specific events remain
        # diagnostic evidence in the timeline.
        EventRequirement(event="dut.fetch_request", evidence_stage="dut"),
        EventRequirement(event="dut.fetch_response", evidence_stage="dut"),
        EventRequirement(event="dut.ptw_request", evidence_stage="dut"),
        EventRequirement(event="dut.ptw_response", evidence_stage="dut"),
    ),
}


def _catalog_index(catalog: dict[str, Any]) -> dict[str, dict[str, Any]]:
    return {
        str(item["testpoint_id"]): item
        for item in catalog.get("testpoints", [])
        if isinstance(item, dict) and item.get("testpoint_id")
    }


def build_scenario_spec(
    case_record: dict[str, Any],
    *,
    timing_seed: int,
    testpoint_catalog: dict[str, Any],
    page_tail_attribute_mode: str = "alternating",
    uncache_a_stall_cycles: int = 0,
) -> ScenarioSpec:
    spec = case_record["spec"]
    family = str(spec["family"])
    if family not in FAMILY_TESTPOINT_IDS:
        raise ValueError(f"no random scenario profile for family: {family}")

    index = _catalog_index(testpoint_catalog)
    testpoint_ids = _resolve_testpoint_ids(FAMILY_TESTPOINT_IDS[family], index)
    missing = [testpoint_id for testpoint_id in testpoint_ids if testpoint_id not in index]
    if missing:
        raise ValueError(
            "canonical testpoint catalog no longer contains the reviewed random profile IDs: "
            + ", ".join(missing)
        )

    intent = [
        {
            "testpoint_id": testpoint_id,
            "source_row": index[testpoint_id]["source_row"],
            "path_text": index[testpoint_id]["path_text"],
            "status_at_generation": index[testpoint_id]["status"],
            "coverage_refs_at_generation": index[testpoint_id]["coverage_refs"],
        }
        for testpoint_id in testpoint_ids
    ]
    required_events = FAMILY_REQUIRED_EVENTS[family]
    reuse_pair = spec.get("directed_pattern", "none") in {
        "page_tail_pair", "page_tail_pair_dense", "page_tail_pair_sparse",
    }
    reuse_cj_ring = spec.get("directed_pattern", "none") in {
        "taken_cj_offset_ring", "taken_cj_offset_ring_permuted",
    }
    reuse_cj_ring_permuted = (
        spec.get("directed_pattern", "none") == "taken_cj_offset_ring_permuted"
    )
    if page_tail_attribute_mode not in PAGE_TAIL_ATTRIBUTE_MODES:
        raise ValueError(
            f"unsupported page-tail attribute mode: {page_tail_attribute_mode}"
        )
    if page_tail_attribute_mode != "alternating" and not reuse_pair:
        raise ValueError(
            "non-default page-tail attribute mode requires a page-tail pattern"
        )
    uncache_a_stall_cycles = int(uncache_a_stall_cycles)
    if not 0 <= uncache_a_stall_cycles <= 1024:
        raise ValueError("uncache A stall cycles must be in [0, 1024]")
    if uncache_a_stall_cycles and (
        family != "translation_timing_cross"
        or not reuse_pair
        or page_tail_attribute_mode != "rvi_pbmt_nc_pair"
    ):
        raise ValueError(
            "uncache A stall requires translation_timing_cross, a page-tail "
            "pattern, and page_tail_attribute_mode=rvi_pbmt_nc_pair"
        )
    if reuse_pair:
        page_tail_probe_repetitions = int(
            spec.get("page_tail_probe_repetitions")
            if spec.get("page_tail_probe_repetitions") is not None
            else spec["iterations"]
        )
        required_events += (
            EventRequirement(event="trace.page_tail_rvc_fallthrough",
                             min_count=page_tail_probe_repetitions, evidence_stage="program"),
            EventRequirement(event="trace.cross_page_instruction",
                             min_count=page_tail_probe_repetitions, evidence_stage="program"),
        )
    if uncache_a_stall_cycles:
        required_events += (
            EventRequirement(
                event="dut.uncache_a_stall_cycle",
                min_count=uncache_a_stall_cycles,
                evidence_stage="dut",
            ),
            EventRequirement(
                event="dut.uncache_a_stall_release",
                evidence_stage="dut",
            ),
        )
    if reuse_cj_ring:
        required_events += (
            EventRequirement(
                event="trace.taken_cj_offset_ring",
                min_count=16,
                evidence_stage="program",
            ),
        )
    if reuse_cj_ring_permuted:
        required_events += (
            EventRequirement(
                event="trace.taken_cj_offset_transition",
                min_count=15,
                evidence_stage="program",
            ),
        )
    if spec.get("indirect_target_mode", "fixed") in {"history4", "history4_page"}:
        target_sites = sum(e.get("kind") == "jalr" for e in case_record.get("episodes", []))
        required_events += (
            EventRequirement(
                event="trace.indirect_four_target_site",
                min_count=max(1, target_sites),
                evidence_stage="program",
            ),
        )
        if spec.get("indirect_target_mode") == "history4_page":
            required_events += (
                EventRequirement(
                    event="trace.indirect_four_target_page_site",
                    min_count=max(1, target_sites),
                    evidence_stage="program",
                ),
            )
    if spec.get("branch_history_mode", "loop_mask") == "site8":
        scheduled = sum(e.get("parameters", {}).get("branch_history_mode") == "site8"
                        for e in case_record.get("episodes", []))
        if not scheduled:
            raise ValueError("site8 requires actual scheduled branch episodes")
        required_events += (EventRequirement(event="trace.site8_branch_sites_with_both_successors",
                                             min_count=scheduled, evidence_stage="program"),)
    if spec.get("ras_return_mode", "private") == "shared_leaf":
        required_events += (EventRequirement(event="trace.shared_return_distinct_successors",
                                             min_count=int(spec["blocks"]), evidence_stage="program"),)
    scenario = ScenarioSpec(
        scenario_id=str(case_record["name"]),
        family=family,
        ordinal=int(spec["ordinal"]),
        program_seed=int(spec["seed"]),
        timing_seed=int(timing_seed),
        testpoint_ids=testpoint_ids,
        risk_tags=FAMILY_RISK_TAGS[family],
        required_events=required_events,
        parameters={
            "profile_schema_version": PROFILE_SCHEMA_VERSION,
            "testpoint_catalog_sha256": str(testpoint_catalog["source_sha256"]),
            "testpoint_intent": intent,
            "generator_parameters": dict(spec),
            **({"pattern_reuse": {
                "schema_version": 1,
                "pattern_id": str(spec.get("directed_pattern")),
                "attribute_mode": page_tail_attribute_mode,
                "probe_repetitions": int(
                    spec.get("page_tail_probe_repetitions")
                    if spec.get("page_tail_probe_repetitions") is not None
                    else spec["iterations"]
                ),
                "probe_repetitions_decoupled_from_program_iterations": (
                    spec.get("page_tail_probe_repetitions") is not None
                ),
                "source_tests": [
                    "test_uncache_page_tail_rvc_does_not_fetch_next_page_before_delivery",
                    "test_uncache_page_tail_rvi_need_resend_rechecks_next_page",
                    "test_uncache_sv39_cross_page_rvi_uses_second_page_pma_path",
                ],
                "source_path": "tests/py/zhaoxinran/uncache/test_instr_uncache_port_boundaries.py",
                "source_sha256": "aee2f098ba49639a05cbc3e8498000b3ea80a2228c57ee03cda04fbc7d118309",
                "supporting_source_path": "tests/py/support/uncache_scenarios.py",
                "supporting_source_sha256": "782eb5faa4b7c2bca12d73bc2d150ddbbe6143a7e5e8ea9e4bc4d09fa599b620",
                "reviewed_historical_dut_evidence": {
                    "admission_scope": "historical_diagnostic_only",
                    "canonical_hit_eligible": False,
                    "review_date": "2026-09-18",
                    "policy": (
                        "recipe provenance only; the random candidate inherits neither "
                        "directed PASS nor directed coverage HIT"
                    ),
                    "dut_source_sha": "8724911fd6d94c1b953b6ab646584845da9c7fc5",
                    "build_manifest_sha256": "9b839c2bcd54001783025402ef18a2a3899865b789589eb514b393021c8e16d0",
                    "compatibility_signature": "dc16010948f136bd339219e1955be759d8966d868030996399149cbde39706cb",
                    "artifacts": [
                        {
                            "test": "test_uncache_page_tail_rvc_does_not_fetch_next_page_before_delivery",
                            "sha256": "2a73e49b8395e2803325e575c96ae59508ef9ae1afd611f0dc16cdffff1fbfdf",
                            "declared_bin_hits": ["BIN-416", "BIN-1107", "BIN-1117"],
                        },
                        {
                            "test": "test_uncache_page_tail_rvi_need_resend_rechecks_next_page",
                            "sha256": "5228bbab6cb0c3b3d13aa6706ee97f974ff23b978f27733e5e43b5823aabed73",
                            "declared_bin_hits": [
                                "BIN-417", "BIN-1101", "BIN-1115", "BIN-1116",
                                "BIN-1119", "BIN-1120", "BIN-1121",
                            ],
                        },
                        {
                            "test": "test_uncache_sv39_cross_page_rvi_uses_second_page_pma_path",
                            "sha256": "010ef6462a18e0b31a69fa58b20cdd3da15575047baeba4b548f09aa3bf4ef9f",
                            "declared_testpoint_ids": ["ATP-090"],
                            "declared_bin_hits": [],
                        },
                    ],
                },
                "reused_conditions": ["page_offset_4094", "rvc_rvi_width_contrast", "adjacent_page_pma_change"],
                "not_proven_by_program_trace": ["no_resend_before_rvc_delivery", "rvi_resend_uses_next_page_attributes"],
                "policy": "adapted_stimulus; no directed HIT inheritance; preserve independent golden checker",
            }} if reuse_pair else ({"pattern_reuse": {
                "schema_version": 1,
                "pattern_id": str(spec.get("directed_pattern")),
                "source_tests": ["test_ifu_owner_taken_cfi_covers_all_end_offsets"],
                "source_path": "tests/py/jiabowen/test_ifu_owner_taken_offsets_v3_dut.py",
                "source_sha256": "0628d2a27361c4e7075e93729432994c8978564b4c1ff2472fe15d702143639a",
                "reused_conditions": [
                    "taken_c_j",
                    "64_byte_block",
                    "halfword_offsets_0_to_15",
                    *(["seeded_offset_execution_order"] if reuse_cj_ring_permuted else []),
                ],
                "not_proven_by_program_trace": [
                    "predchecker_taken_owner_attribution",
                    "BIN-974_current_DUT_hit",
                ],
                "policy": "adapted_stimulus; no directed HIT inheritance; preserve independent golden checker",
            }} if reuse_cj_ring else {})),
            **({
                "uncache_a_stall_reuse": {
                    "schema_version": 1,
                    "stall_cycles": uncache_a_stall_cycles,
                    "source_test": "test_pbmt_nc_tl_a_backpressure_holds_user_attributes",
                    "source_path": "tests/py/jiabowen/test_ifu_instr_uncache_attribute_stability_v3_dut.py",
                    "source_sha256": "50730970d546507ec4e38bb6e9b511c1b04871226e86f93e04dfeeca1fa752b7",
                    "reused_conditions": [
                        "real_tl_a_valid_trigger",
                        "ready_low_after_valid",
                        "address_and_user_attributes_stable",
                        "release_requires_same_request_fire",
                    ],
                    "not_proven_by_scenario_action": [
                        "BIN-1095_current_DUT_hit",
                        "BIN-1096_current_DUT_hit",
                        "BIN-1129_current_DUT_hit",
                        "BIN-1130_current_DUT_hit",
                    ],
                    "policy": "adapted timing stimulus; REALIZED is not a coverage HIT",
                }
            } if uncache_a_stall_cycles else {}),
            "episode_count": len(case_record.get("episodes") or []),
            "translation": (
                {
                    "schema_version": 1,
                    "randomizer_seed": int(timing_seed) ^ 0x51ED2705,
                    "ordinal": int(spec["ordinal"]),
                    "avoid_bare_no_stage": True,
                    "payload_source": "DUT_BIN_BYTES",
                    "virtual_base": 0x80000000,
                    "physical_base": 0x80400000,
                    "guest_physical_base": 0x80600000,
                    "attribute_plan": (
                        _page_tail_attribute_plan(
                            int(spec["ordinal"]), page_tail_attribute_mode
                        )
                        if reuse_pair
                        else translation_attribute_plan(int(spec["ordinal"]))
                    ).to_dict(),
                    **({"permission_layout": "exact_napot_v1"}
                       if spec.get("indirect_target_mode") == "history4_page" else {}),
                }
                if family == "translation_timing_cross"
                else None
            ),
            "timing_plan": replace(
                TimingPlan.from_seed(
                    timing_seed,
                    profile=(
                        "ibuffer_pressure_cross"
                        if family == "ibuffer_pressure_cross"
                        else "translation_timing_cross"
                        if family == "translation_timing_cross"
                        else "default"
                    ),
                ),
                uncache_a_stall_cycles=uncache_a_stall_cycles,
            ).to_dict(),
        },
    )
    scenario.validate()
    return scenario
