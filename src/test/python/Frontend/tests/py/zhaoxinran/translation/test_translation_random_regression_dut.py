from __future__ import annotations

import hashlib
import os
from dataclasses import asdict

import pytest

from env.sequences import (
    TranslationPmpPmaEntry,
    TranslationScenario,
    TranslationScenarioBuilder,
    TranslationScenarioPhase,
    TranslationScenarioRandomizer,
    TranslationScenarioSequence,
)
from env.support import PmpPmaConfig, record_scenario


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"


def _read_int(name: str, default: str) -> int:
    return int(os.getenv(name, default), 0)


def _normal_cross_page_scenarios() -> tuple[TranslationScenario, ...]:
    payload = b"\x13\x00\x00\x00" * 512
    return tuple(
        TranslationScenario(
            scenario_id=f"translation-normal-cross-page-{mode}",
            va=0x8020_0F00,
            pa=0x8040_0F00,
            payload=payload,
            page_count=2,
            mode=mode,
            pmp_entries=(
                TranslationPmpPmaEntry(
                    "pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True), 0x8040_0000, size=0x2000
                ),
            ),
            pma_entries=(
                TranslationPmpPmaEntry(
                    "pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_0000, size=0x2000
                ),
            ),
        )
        for mode in ("sv39", "sv48")
    )


def _translation_complete(env) -> bool:
    active = env.translation_oracle.get_active()
    if active is None:
        return False
    expected_ptw_requests = {
        (int(request["vpn"]), int(request["s2xlate"]), int(request["get_gpa"]))
        for request in active["expected_ptw_requests"]
    }
    if active["expected_path"] == "fault":
        return bool(
            active["fault_seen"]
            and expected_ptw_requests.issubset(active["responded_ptw_request_keys"])
        )
    expected_cfvec_pages = {
        int(page)
        for page, outcome in zip(
            active["selected_pages"], active["expected_page_outcomes"]
        )
        if bool(outcome.get("ok", False))
    }
    return bool(
        expected_ptw_requests.issubset(active["responded_ptw_request_keys"])
        and len(active["expected_fetches"]) == len(active["observed_fetch_pas"])
        and expected_cfvec_pages.issubset(active["observed_normal_cfvec_pages"])
        and int(active["observed_normal_cfvec_count"]) >= 2
    )


def _fault_probe_pages(state) -> tuple[int, ...] | None:
    if state.expected_page_outcomes[0]["ok"]:
        return None

    base_vpn = int(state.scenario.va) >> 12
    pages = {0}
    for probe in state.scenario.permission_probes:
        first_page = (int(probe.va) >> 12) - base_vpn
        last_page = ((int(probe.va) + max(1, int(probe.size)) - 1) >> 12) - base_vpn
        pages.update(
            page
            for page in range(first_page, last_page + 1)
            if 0 <= page < len(state.expected_page_outcomes)
        )
    return tuple(sorted(pages))


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_translation_constrained_random_stream_dut(env) -> None:
    seed = _read_int("TB_TRANSLATION_RANDOM_SEED", "0x5a390001")
    count = _read_int("TB_TRANSLATION_RANDOM_COUNT", "6")
    start_ordinal = _read_int("TB_TRANSLATION_RANDOM_START_ORDINAL", "0")
    if count < 1:
        raise ValueError("TB_TRANSLATION_RANDOM_COUNT must be positive")
    if start_ordinal < 0:
        raise ValueError("TB_TRANSLATION_RANDOM_START_ORDINAL must be non-negative")

    phase_results = []
    for ordinal in range(start_ordinal, start_ordinal + count):
        generated = TranslationScenarioRandomizer(seed).next(ordinal)
        scenario = generated.scenario
        kind = TranslationScenarioRandomizer.kind_for_ordinal(ordinal)
        scenario_parameters = {
            "scenario_id": scenario.scenario_id,
            "mode": scenario.mode,
            "stage2_mode": scenario.stage2_mode,
            "s2xlate": scenario.s2xlate,
            "va": scenario.va,
            "pa": scenario.pa,
            "gpa": scenario.gpa,
            "page_count": scenario.page_count,
            "declared_expected_path": scenario.expected_path,
            "declared_expected_result": scenario.expected_result,
            "payload_sha256": hashlib.sha256(scenario.payload).hexdigest(),
            "s1_pte": asdict(scenario.s1_pte),
            "s2_pte": asdict(scenario.s2_pte),
            "s1_sector_lanes": [asdict(lane) for lane in scenario.s1_sector_lanes],
            "response_faults": {
                "s1_pf": scenario.s1_pf,
                "s1_af": scenario.s1_af,
                "s2_gpf": scenario.s2_gpf,
                "s2_gaf": scenario.s2_gaf,
            },
            "privilege": {
                "imode": scenario.priv_imode,
                "virt": scenario.priv_virt,
            },
            "pmp_entries": [asdict(entry) for entry in scenario.pmp_entries],
            "pma_entries": [asdict(entry) for entry in scenario.pma_entries],
            "permission_probes": [
                asdict(probe) for probe in scenario.permission_probes
            ],
            "ptw_response_latency": scenario.ptw_response_latency,
            "ptw_response_latency_max": scenario.ptw_response_latency_max,
            "ptw_response_seed": scenario.ptw_response_seed,
            "ptw_req_ready_strategy": scenario.ptw_req_ready_strategy,
            "ptw_req_ready_probability": scenario.ptw_req_ready_probability,
            "ptw_req_ready_high_cycles": scenario.ptw_req_ready_high_cycles,
            "ptw_req_ready_low_cycles": scenario.ptw_req_ready_low_cycles,
        }
        translation_enabled = str(scenario.mode).lower() != "bare" or int(scenario.s2xlate) != 0
        env.translation_oracle.disarm()
        env.initialize(reset_vector=scenario.va, bare_mode=not translation_enabled)
        prepared: dict[str, object] = {}

        def arm_before_reset_release() -> None:
            state = TranslationScenarioBuilder(env).build(scenario)
            env.monitor.clear()
            env.monitor.set_expected_pc(scenario.va)
            env.translation_oracle.clear()
            env.arm_translation_scenario(state, page_indexes=_fault_probe_pages(state))
            prepared["state"] = state

        env.reset(before_release=arm_before_reset_release)
        state = prepared["state"]
        scenario_parameters.update(
            {
                "oracle_expected_outcome": state.expected_outcome,
                "oracle_expected_page_outcomes": list(state.expected_page_outcomes),
            }
        )
        record_scenario(
            env,
            f"zhaoxinran/translation/{kind}",
            base_seed=seed,
            seed=seed,
            ordinal=ordinal,
            parameters=scenario_parameters,
        )
        for _ in range(12000):
            if _translation_complete(env) or env.translation_oracle.get_stats()["errors"]:
                break
            env.step(1)
        env.assert_translation_scenario()
        phase_results.append(
            {
                "kind": "phase",
                "scenario_id": scenario.scenario_id,
                "state": state,
            }
        )

    assert len(phase_results) == count
    assert [record["scenario_id"] for record in phase_results] == [
        f"translation-random-s{seed}-n{ordinal}" for ordinal in range(start_ordinal, start_ordinal + count)
    ]
    assert not env.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_translation_normal_cross_page_stream_dut(env) -> None:
    for scenario in _normal_cross_page_scenarios():
        sequence = TranslationScenarioSequence(actions=(TranslationScenarioPhase(scenario=scenario),))
        sequence.initialize_first_phase(env)
        phase_results = [record for record in sequence.run(env) if record["kind"] == "phase"]

        assert len(phase_results) == 1
        state = phase_results[0]["state"]
        assert all(outcome["ok"] and outcome["expected_path"] == "cacheable" for outcome in state.expected_page_outcomes)
        observed_pcs = [int(observation.pc) for observation in env.monitor.observations]
        for outcome in state.expected_page_outcomes:
            page_va = int(outcome["va"])
            assert any(page_va <= pc < page_va + 0x1000 for pc in observed_pcs), (
                f"{scenario.scenario_id} did not deliver a normal cfVec from VA page 0x{page_va:x}"
            )

    assert not env.get_errors()
