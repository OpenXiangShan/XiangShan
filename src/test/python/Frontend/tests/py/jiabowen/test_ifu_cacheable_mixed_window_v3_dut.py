from __future__ import annotations

import os

import pytest

from env.support import PmpPmaConfig
from env.funcov.py.ifu.cacheable_pipeline_funcov import _UPSTREAM_SIGNALS
from tests.py.jiabowen import test_two_fetch_directed_flow_dut as two_fetch
from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BIN_ID = "BIN-904"


def _entry(kind: str, index: int, addr: int, *, cacheable: bool | None = None):
    config = PmpPmaConfig(
        match="napot",
        read=True,
        write=True,
        execute=True,
        **({} if cacheable is None else {"cacheable": bool(cacheable)}),
    )
    return uncache.TranslationPmpPmaEntry(
        kind=kind,
        index=index,
        config=config,
        addr=addr,
        size=uncache._SV39_PAGE_SIZE,
    )


def _upstream_snapshot(recorder) -> dict[str, int | None]:
    snapshot: dict[str, int | None] = {}
    for key, candidates in _UPSTREAM_SIGNALS.items():
        value = recorder._read_first_dut_signal(recorder.env.dut, candidates)
        snapshot[str(key)] = None if value is None else int(value)
    return snapshot


@pytest.mark.funcov_bins(_BIN_ID)
@pytest.mark.skipif(
    not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration"
)
@pytest.mark.xfail(
    strict=True,
    reason=(
        "current V3 review sentinel: PMA-transition traffic has not produced "
        "a checker-eligible BIN-904 MainPipe acceptance"
    ),
)
def test_two_fetch_mmio_window_suppresses_second_icache_response(env) -> None:
    """A trained dual-fetch candidate becomes single after a real PMA update."""
    mapping_va = uncache._NORMAL_BASE
    mapping_pa = uncache._NORMAL_PHYS_BASE
    start_va = mapping_va + two_fetch._SECOND_TAKEN_LOOP_OFFSET
    page = mapping_pa & ~(uncache._SV39_PAGE_SIZE - 1)
    payload = two_fetch._second_block_taken_loop()

    uncache._initialize_sv39_fetch(env, reset_vector=start_va)
    scenario = uncache.TranslationScenario(
        scenario_id="bin-904-train-cacheable-then-pma-uncache-two-fetch",
        va=mapping_va,
        pa=mapping_pa,
        payload=payload,
        expected_path="cacheable",
        expected_result="normal",
        pmp_entries=(
            _entry("pmp", 0, page),
        ),
        pma_entries=(
            _entry("pma", 0, page, cacheable=True),
        ),
    )
    state = uncache.TranslationScenarioBuilder(env).build(scenario)
    assert state.expected_page_outcomes[0]["expected_path"] == "cacheable"

    env.arm_translation_scenario(state)
    uncache._force_redirect_to(env, start_va)

    recorder = env.functional_coverage
    for _ in range(12000):
        env.step(1)
        if recorder.key_hit("two_fetch_ftq_eligibility", "eligible_dual"):
            break
    assert recorder.key_hit("two_fetch_ftq_eligibility", "eligible_dual"), {
        "reason": "cacheable warmup never formed an FTQ dual-fetch candidate",
        "icache": env.icache_agent.get_stats(),
        "backend": env.backend_model.get_stats(),
        "monitor_errors": env.monitor.get_errors(),
    }

    # This is the architectural distributed-CSR path, not an internal force.
    env.write_pma_entry(
        0,
        PmpPmaConfig(
            match="napot", read=True, write=True, execute=True, cacheable=False
        ),
        page,
        size=uncache._SV39_PAGE_SIZE,
        settle_cycles=4,
    )
    env.uncache_agent.configure(latency=4, mmio_latency=4)
    uncache._force_redirect_to(env, start_va)

    definition = recorder.definition_by_bin_id[_BIN_ID]
    target_key = definition.key
    upstream_observations: list[dict[str, int | None]] = []
    for _ in range(12000):
        env.step(1)
        upstream = _upstream_snapshot(recorder)
        if any(
            upstream[key] == 1
            for key in (
                "mainpipe_fire",
                "second_requested",
                "second_waylookup_valid",
                "first_mmio",
                "second_mmio",
            )
        ):
            upstream_observations.append(
                {"cycle": int(env.current_cycle), **upstream}
            )
            upstream_observations = upstream_observations[-32:]
        if recorder.key_hit(
            definition.coverage_group,
            definition.bin_name,
            coverpoint=definition.coverpoint,
        ):
            break

    assert recorder.key_hit(
        definition.coverage_group,
        definition.bin_name,
        coverpoint=definition.coverpoint,
    ), {
        "reason": "no same-page MMIO/uncache MainPipe dual-request window was observed",
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
        "ptw": env.ptw_agent.get_stats(),
        "monitor_errors": env.monitor.get_errors(),
        "upstream_observations": upstream_observations,
    }
    hit = recorder.hits[target_key]
    evidence = hit.evidence[-1]
    assert evidence["first_mmio"] == 1
    assert evidence["second_mmio"] == 1
    assert evidence["real_two_fetch"] == 0
    assert evidence["output_second_valid"] == 0
    assert evidence["illegal_mixed_response_emitted"] is False
    assert not env.monitor.get_errors()
