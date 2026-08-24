from __future__ import annotations

import os

import pytest

from env.sequences import (
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationScenario,
    TranslationScenarioPhase,
    TranslationScenarioSequence,
)
from env.support import PmpPmaConfig


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_VA = 0x8020_0F00
_GPA = 0x8060_0F00
_PA = 0x8040_0F00
_PAGE_SIZE = 0x1000
_PAYLOAD = b"\x13\x00\x00\x00" * 512


def _physical_permissions() -> tuple[tuple[TranslationPmpPmaEntry, ...], tuple[TranslationPmpPmaEntry, ...]]:
    return (
        (
            TranslationPmpPmaEntry(
                "pmp",
                0,
                PmpPmaConfig(match="napot", read=True, execute=True),
                _PA & ~(_PAGE_SIZE - 1),
                size=0x2000,
            ),
        ),
        (
            TranslationPmpPmaEntry(
                "pma",
                0,
                PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True),
                _PA & ~(_PAGE_SIZE - 1),
                size=0x2000,
            ),
        ),
    )


def _scenario(scenario_id: str, *, with_physical_permissions: bool = True, **kwargs) -> TranslationScenario:
    pmp_entries, pma_entries = _physical_permissions() if with_physical_permissions else ((), ())
    return TranslationScenario(
        scenario_id=scenario_id,
        va=_VA,
        pa=_PA,
        payload=_PAYLOAD,
        page_count=2,
        pmp_entries=pmp_entries,
        pma_entries=pma_entries,
        **kwargs,
    )


_TIMING_CASES = (
    pytest.param(
        _scenario(
            "translation-ptw-only-stage2-latency",
            mode="bare",
            stage2_mode="sv39",
            s2xlate=2,
            priv_virt=1,
            ptw_response_latency=2,
            ptw_response_latency_max=7,
            ptw_response_seed=0x5A390031,
        ),
        None,
        id="only-stage2-latency",
    ),
    pytest.param(
        _scenario(
            "translation-ptw-all-stage-periodic-ready",
            mode="sv39",
            stage2_mode="sv39",
            s2xlate=3,
            gpa=_GPA,
            s1_pte=TranslationPte(asid=5, vmid=7),
            s2_pte=TranslationPte(vmid=7),
            vsatp_asid=5,
            hgatp_vmid=7,
            priv_virt=1,
            ptw_req_ready_strategy="periodic",
            ptw_req_ready_high_cycles=1,
            ptw_req_ready_low_cycles=2,
        ),
        None,
        id="all-stage-periodic-ready",
    ),
    pytest.param(
        _scenario(
            "translation-ptw-only-stage2-gpf-latency",
            mode="bare",
            stage2_mode="sv39",
            s2xlate=2,
            priv_virt=1,
            s2_gpf=1,
            expected_path="fault",
            expected_result="guest_fault",
            with_physical_permissions=False,
            ptw_response_latency=4,
            ptw_response_latency_max=7,
            ptw_response_seed=0x5A390032,
        ),
        "instruction_guest_page_fault",
        id="only-stage2-gpf-latency",
    ),
    pytest.param(
        _scenario(
            "translation-ptw-all-stage-gaf-periodic-ready",
            mode="sv39",
            stage2_mode="sv39",
            s2xlate=3,
            gpa=_GPA,
            s1_pte=TranslationPte(asid=5, vmid=7),
            s2_pte=TranslationPte(vmid=7),
            vsatp_asid=5,
            hgatp_vmid=7,
            priv_virt=1,
            s2_gaf=1,
            expected_path="fault",
            expected_result="access_fault",
            with_physical_permissions=False,
            ptw_req_ready_strategy="periodic",
            ptw_req_ready_high_cycles=1,
            ptw_req_ready_low_cycles=2,
        ),
        "instruction_access_fault",
        id="all-stage-gaf-periodic-ready",
    ),
)


@pytest.mark.parametrize("scenario,expected_fault", _TIMING_CASES)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_ptw_timing_by_translation_stage(
    env,
    scenario: TranslationScenario,
    expected_fault: str | None,
) -> None:
    sequence = TranslationScenarioSequence(
        actions=(
            TranslationScenarioPhase(
                scenario=scenario,
                page_indexes=(0,) if expected_fault is not None else None,
            ),
        )
    )
    sequence.initialize_first_phase(env)
    phases = [record for record in sequence.run(env) if record["kind"] == "phase"]

    assert len(phases) == 1
    state = phases[0]["state"]
    if expected_fault is None:
        assert all(outcome["ok"] for outcome in state.expected_page_outcomes)
    else:
        assert state.expected_page_outcomes[0]["outcome"] == expected_fault
        assert env.monitor.exception_mark_count > 0
        observations_after_fault = len(env.monitor.observations)
        env.step(64)
        assert len(env.monitor.observations) == observations_after_fault
    assert not env.get_errors()
