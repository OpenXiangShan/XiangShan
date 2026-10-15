"""Directed DUT closure for ICache MainPipe partial functional coverage.

Scenario preparation and transaction observations are owned by local support
modules and use the shared Frontend environment interfaces.
"""

from __future__ import annotations

from env.runtime.frontend_init import (
    FrontendInitConfig,
    initialize_frontend,
)

import os
from collections.abc import (
    Callable,
)

import pytest
from tests.py.icache.directed_dut.support import (
    cycle_limit as _cycle_limit,
    wait_until,
    completed_dual_transactions as _completed_dual_transactions,
    register_mainpipe_observer as _register_mainpipe_observer,
    register_mainpipe_observer,
    make_cacheable_init_config,
    make_dual_refill_init_config,
    make_sv39_init_config,
    make_pbmt_redirect_init_config,
)

from tests.py.icache.directed_dut.programs import (
    CACHEABLE_BASE as _BASE,
)

from dataclasses import (
    replace,
)
from env.sequences import (
    TranslationPmpPmaEntry,
    TranslationScenario,
    TranslationScenarioBuilder,
    TranslationSectorLane,
    TranslationScenarioPhase,
    TranslationScenarioSequence,
)
from env.support import (
    PmpPmaConfig,
    record_scenario,
    scenario_rng,
)
from tests.py.support.uncache_scenarios import (
    _ADDI_X0_X0_0,
    _CNOP,
    _FETCH_BLOCK_SIZE,
    _NORMAL_ALT_BASE,
    _NORMAL_BASE,
    _NORMAL_PHYS_BASE,
    _SV39_PAGE_SIZE,
    _UNCACHE_BEAT_BYTES,
    _configure_exec_attrs_16k,
    _force_redirect_to,
    _prepare_sv39_dual_nc_cacheable_stream,
    _pulse_sfence,
    _wait_for_observed_pc,
    _wait_for_observed_pc_sequence,
    _wait_for_request_addr,
    _wait_for_resp_count,
)


pytestmark = pytest.mark.skipif(
    os.getenv("TB_ENABLE_DUT_TESTS") != "1",
    reason="requires compiled DUT",
)


def _run_until(env, predicate: Callable[[], bool], *, max_cycles: int, label: str) -> None:
    wait_until(
        env,
        predicate,
        max_cycles=max_cycles,
        label=label,
        diagnostics=lambda: {
            "icache": env.icache_agent.get_stats(),
            "backend": env.backend_model.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        },
    )


def _wait_hit(env, group: str, bin_name: str, *, max_cycles: int = 6000) -> None:
    _run_until(
        env,
        lambda: env.functional_coverage.key_hit(group, bin_name),
        max_cycles=max_cycles,
        label=f"{group}.{bin_name}",
    )


# Protection flows used only by this test module.


def _randomize_translation_timing(
    env, scenario: TranslationScenario, *, latency_min: int = 1, latency_max: int = 8
) -> TranslationScenario:
    scenario_key = f"zhaoxinran/uncache/translation/{scenario.scenario_id}"
    base_seed, seed, rng = scenario_rng(scenario_key)
    latency = rng.randint(int(latency_min), int(latency_max))
    randomized = replace(scenario, ptw_response_latency=latency, ptw_response_seed=seed)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "scenario_id": scenario.scenario_id,
            "va": scenario.va,
            "pa": scenario.pa,
            "page_count": scenario.page_count,
            "latency": latency,
            "expected_path": scenario.expected_path,
            "expected_result": scenario.expected_result,
        },
    )
    return randomized


def _wait_for_icache_req_count(env, count: int, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if int(env.icache_agent.get_stats().get("req_count", 0)) >= int(count):
            return True
        env.step(1)
    return int(env.icache_agent.get_stats().get("req_count", 0)) >= int(count)


def _first_observed_index(env, pc: int) -> int:
    for idx, obs in enumerate(env.monitor.observations):
        if int(obs.pc) == int(pc):
            return int(idx)
    return -1


def _assert_observed_instrs(env, expected_block: list[tuple[int, int, bool]]) -> None:
    observed_by_pc = {
        int(obs.pc): obs
        for obs in env.monitor.observations
        if any((int(obs.pc) == int(pc) for pc, _, _ in expected_block))
    }
    for pc, instr, is_rvc in expected_block:
        obs = observed_by_pc[int(pc)]
        assert int(obs.instr) == int(instr), {
            "pc": hex(int(pc)),
            "expected_instr": hex(int(instr)),
            "observed_instr": hex(int(obs.instr)),
        }
        assert bool(obs.is_rvc) == bool(is_rvc), {
            "pc": hex(int(pc)),
            "expected_is_rvc": bool(is_rvc),
            "observed_is_rvc": bool(obs.is_rvc),
        }


def run_cross_page_pma_path(env):
    cross_page_va = _NORMAL_BASE + _SV39_PAGE_SIZE - 2
    cross_page_pa = _NORMAL_PHYS_BASE + _SV39_PAGE_SIZE - 2
    next_page_pa = _NORMAL_PHYS_BASE + _SV39_PAGE_SIZE
    payload = (
        int(_ADDI_X0_X0_0).to_bytes(4, "little") + int(_CNOP).to_bytes(2, "little") * 64
    )
    initialize_frontend(
        env, make_sv39_init_config(env.config, reset_vector=cross_page_va)
    )
    scenario = TranslationScenario(
        scenario_id="atp-090-cross-page-pma-dut",
        va=cross_page_va,
        pa=cross_page_pa,
        payload=payload,
        page_count=2,
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_PHYS_BASE,
                size=_SV39_PAGE_SIZE,
            ),
            TranslationPmpPmaEntry(
                kind="pmp",
                index=1,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=next_page_pa,
                size=_SV39_PAGE_SIZE,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(
                    match="napot",
                    read=True,
                    write=True,
                    execute=True,
                    cacheable=True,
                    atomic=True,
                ),
                addr=_NORMAL_PHYS_BASE,
                size=_SV39_PAGE_SIZE,
            ),
            TranslationPmpPmaEntry(
                kind="pma",
                index=1,
                config=PmpPmaConfig(
                    match="napot", read=True, write=True, execute=True, cacheable=False
                ),
                addr=next_page_pa,
                size=_SV39_PAGE_SIZE,
            ),
        ),
    )
    scenario = _randomize_translation_timing(env, scenario)
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(cross_page_va)
    env.arm_translation_scenario(state)
    _force_redirect_to(env, cross_page_va)
    assert _wait_for_request_addr(env, next_page_pa, max_cycles=12000), {
        "ptw": env.ptw_agent.get_stats(),
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    assert _wait_for_observed_pc(env, cross_page_va, max_cycles=12000), {
        "ptw": env.ptw_agent.get_stats(),
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
        "observed": [
            (int(obs.pc), int(obs.instr), bool(obs.is_rvc))
            for obs in env.monitor.observations[-16:]
        ],
    }
    observed = next(
        (obs for obs in env.monitor.observations if int(obs.pc) == cross_page_va)
    )
    icache_records = env.icache_agent.get_stats()["request_records"]
    uncache_stats = env.uncache_agent.get_stats()
    oracle_stats = env.assert_translation_scenario()
    assert any(
        (int(record["address"]) == cross_page_pa & ~63 for record in icache_records)
    ), icache_records
    assert next_page_pa in uncache_stats["request_addrs"], uncache_stats
    assert int(observed.instr) == _ADDI_X0_X0_0
    assert not bool(observed.is_rvc)
    assert env.monitor.exception_mark_count == 0
    assert oracle_stats["error_count"] == 0
    assert not env.monitor.get_errors()


def run_pmp_execute_denied(env):
    initialize_frontend(
        env, make_sv39_init_config(env.config, reset_vector=_NORMAL_BASE)
    )
    scenario = TranslationScenario(
        scenario_id="atp-076-sv39-pmp-execute-denied-dut",
        va=_NORMAL_BASE,
        pa=_NORMAL_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 32,
        expected_path="fault",
        expected_result="access_fault",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(
                    match="napot", read=True, write=True, execute=False
                ),
                addr=_NORMAL_PHYS_BASE,
                size=4096,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(
                    match="napot",
                    read=True,
                    write=True,
                    execute=True,
                    cacheable=True,
                    atomic=True,
                ),
                addr=_NORMAL_PHYS_BASE,
                size=4096,
            ),
        ),
    )
    scenario = _randomize_translation_timing(env, scenario)
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _force_redirect_to(env, scenario.va)
    for _ in range(6000):
        env.step(1)
        active = env.translation_oracle.get_active()
        if active is not None and active["fault_seen"]:
            break
    env.step(32)
    assert int(env.ptw_agent.get_stats().get("resp_count", 0)) >= 1
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


def run_sector_lane_refill(env):
    initialize_frontend(
        env, make_sv39_init_config(env.config, reset_vector=_NORMAL_BASE)
    )
    scenario = TranslationScenario(
        scenario_id="sv39-sector-lane-reuse-dut",
        va=_NORMAL_BASE,
        pa=_NORMAL_PHYS_BASE,
        payload=int(_CNOP).to_bytes(2, "little") * 2049,
        page_count=2,
        s1_sector_lanes=(
            TranslationSectorLane(lane=1, ppn=(_NORMAL_PHYS_BASE >> 12) + 1),
        ),
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_PHYS_BASE,
                size=4096,
            ),
            TranslationPmpPmaEntry(
                kind="pmp",
                index=1,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=_NORMAL_PHYS_BASE + 4096,
                size=4096,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(
                    match="napot",
                    read=True,
                    write=True,
                    execute=True,
                    cacheable=True,
                    atomic=True,
                ),
                addr=_NORMAL_PHYS_BASE,
                size=4096,
            ),
            TranslationPmpPmaEntry(
                kind="pma",
                index=1,
                config=PmpPmaConfig(
                    match="napot",
                    read=True,
                    write=True,
                    execute=True,
                    cacheable=True,
                    atomic=True,
                ),
                addr=_NORMAL_PHYS_BASE + 4096,
                size=4096,
            ),
        ),
    )
    scenario = _randomize_translation_timing(env, scenario)
    result = TranslationScenarioSequence(
        actions=(
            TranslationScenarioPhase(scenario=scenario, page_indexes=(0,)),
            TranslationScenarioPhase(
                reuse_previous=True, page_indexes=(1,), expect_ptw=False
            ),
        )
    ).run(env)
    assert [item["kind"] for item in result] == ["phase", "phase"]
    assert int(env.ptw_agent.get_stats()["req_count"]) == 1
    assert any(
        (
            int(record["address"]) == _NORMAL_PHYS_BASE + 4096 & ~63
            for record in env.icache_agent.get_stats()["request_records"]
        )
    )
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


def run_cacheable_pending_redirect_to_pbmt_nc(
    env, *, uncache_latency: int = 16, icache_latency: int = 64, agent_seed: int = 4
):
    nc_expected, cacheable_pcs = _prepare_sv39_dual_nc_cacheable_stream(env)
    nc_pcs = [pc for pc, _, _ in nc_expected]
    initialize_frontend(
        env,
        make_pbmt_redirect_init_config(
            env.config,
            reset_vector=_NORMAL_ALT_BASE,
            uncache_latency=uncache_latency,
            icache_latency=icache_latency,
            agent_seed=agent_seed,
        ),
    )
    _configure_exec_attrs_16k(env, base_addr=2147483648)
    _force_redirect_to(env, _NORMAL_ALT_BASE)
    assert _wait_for_icache_req_count(
        env, 2, max_cycles=12000
    ), env.icache_agent.get_stats()
    assert int(env.icache_agent.get_stats().get("pending", 0)) > 0
    _pulse_sfence(env, addr=_NORMAL_ALT_BASE, rs1=1, rs2=0)
    assert int(env.icache_agent.get_stats().get("pending", 0)) > 0
    _force_redirect_to(env, _NORMAL_BASE)
    assert _wait_for_request_addr(
        env, _NORMAL_PHYS_BASE, max_cycles=6000
    ), env.uncache_agent.get_stats()
    assert _wait_for_resp_count(
        env, _FETCH_BLOCK_SIZE // _UNCACHE_BEAT_BYTES, max_cycles=12000
    ), {
        "uncache": env.uncache_agent.get_stats(),
        "icache": env.icache_agent.get_stats(),
    }
    assert _wait_for_observed_pc_sequence(env, nc_pcs, max_cycles=12000), {
        "observed": [
            (int(obs.pc), int(obs.instr), bool(obs.is_rvc))
            for obs in env.monitor.observations[-40:]
        ],
        "uncache": env.uncache_agent.get_stats(),
        "icache": env.icache_agent.get_stats(),
    }
    _assert_observed_instrs(env, nc_expected)
    env.step(128)
    uncache_stats = env.uncache_agent.get_stats()
    icache_stats = env.icache_agent.get_stats()
    assert all(
        (
            _NORMAL_PHYS_BASE + _UNCACHE_BEAT_BYTES * idx
            in uncache_stats.get("request_addrs", [])
            for idx in range(_FETCH_BLOCK_SIZE // _UNCACHE_BEAT_BYTES)
        )
    ), uncache_stats
    assert (
        int(uncache_stats.get("req_count", 0))
        >= _FETCH_BLOCK_SIZE // _UNCACHE_BEAT_BYTES
    )
    assert int(icache_stats.get("req_count", 0)) >= 2, icache_stats
    recovery_index = _first_observed_index(env, nc_pcs[0])
    assert recovery_index >= 0
    assert not any(
        (
            int(obs.pc) in set(cacheable_pcs)
            for obs in list(env.monitor.observations)[recovery_index:]
        )
    )
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)
@pytest.mark.parametrize(
    "fault,refill_bin",
    [
        ({"corrupt": 1, "denied": 0, "beat": 0}, "error_first_beat_no_sram_write"),
        ({"corrupt": 1, "denied": 1, "beat": 1}, "error_second_beat_no_sram_write"),
    ],
    ids=("first-beat-corrupt", "second-beat-denied"),
)
@pytest.mark.funcov_bins("BIN-624", "BIN-636", "BIN-715", "BIN-1009")
def test_tc_icache_mainpipe_fault_refill_dut(env, fault, refill_bin) -> None:
    env.icache_agent.inject_response_fault_at(_BASE, **fault)
    register_mainpipe_observer(env)
    initialize_frontend(env, make_cacheable_init_config(env.config, _BASE, latency=12))

    _wait_hit(env, "icache_mainpipe_s1_refill", "corrupt_refill_saved")
    _wait_hit(env, "icache_mainpipe_s1_protection", "tl_error_to_exception")
    _wait_hit(env, "icache_missunit_refill", refill_bin)
    stats = env.icache_agent.get_stats()
    assert int(stats["corrupt_resp_count"]) >= 1
    if int(fault["denied"]):
        assert int(stats["denied_resp_count"]) >= 1
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)
@pytest.mark.funcov_bins("BIN-626")
def test_tc_icache_mainpipe_selective_refill_dut(env) -> None:
    samples = _register_mainpipe_observer(env)
    initialize_frontend(env, make_dual_refill_init_config(env.config, seed=9508))

    _run_until(
        env,
        lambda: any(
            item["pattern"] == "hit_hit" for item in _completed_dual_transactions(samples)
        ),
        max_cycles=_cycle_limit("TB_ICACHE_MAINPIPE_SELECTIVE_WARM_WAIT", 3000),
        label="resident dual hit training",
    )
    fencei = getattr(env.dut, "io_fencei", None)
    assert fencei is not None, {"missing_dut_signal": "io_fencei"}
    fencei.value = 1
    env.step(1)
    fencei.value = 0

    _wait_hit(
        env,
        "icache_mainpipe_s1_refill",
        "refill_request_line_selective",
        max_cycles=_cycle_limit("TB_ICACHE_MAINPIPE_SELECTIVE_REFILL_WAIT", 6000),
    )
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)
@pytest.mark.funcov_bins("BIN-623")
def test_tc_icache_mainpipe_nonmatching_refill_dut(env) -> None:
    run_sector_lane_refill(env)
    _wait_hit(
        env,
        "icache_mainpipe_s1_refill",
        "nonmatching_refill_ignored",
        max_cycles=1,
    )
    assert int(env.icache_agent.get_stats().get("resp_line_count", 0)) >= 1
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)
@pytest.mark.funcov_bins("BIN-634")
def test_tc_icache_mainpipe_pmp_fault_dut(env) -> None:
    run_pmp_execute_denied(env)
    _wait_hit(env, "icache_mainpipe_s1_protection", "pmp_exception_suppresses_miss")
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)
@pytest.mark.funcov_bins("BIN-635")
def test_tc_icache_mainpipe_pma_mmio_suppress_dut(env) -> None:
    run_cross_page_pma_path(env)
    _wait_hit(env, "icache_mainpipe_s1_protection", "pmp_mmio_suppresses_refill")
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) >= 1
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)
@pytest.mark.funcov_bins("BIN-638")
def test_tc_icache_mainpipe_uncache_suppress_dut(env) -> None:
    run_cacheable_pending_redirect_to_pbmt_nc(env)
    _wait_hit(env, "icache_mainpipe_s1_protection", "pbmt_uncache_suppresses_refill")
    assert not env.monitor.get_errors()
