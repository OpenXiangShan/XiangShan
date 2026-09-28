from __future__ import annotations

import os

import pytest

from env.funcov.py.ifu import mmio_nc_owner_funcov as owner_funcov
from env.support import record_scenario, scenario_rng
from tests.py.support import uncache_scenarios as uncache

_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"


def _capture_post_drive_snapshot(cycle: int, env) -> dict[str, int | None]:
    sample = owner_funcov._snapshot(env.functional_coverage, env.dut)
    sample.update(
        tl_a_valid=int(env.uncache_if.a_valid.value),
        tl_a_ready=int(env.uncache_if.a_ready.value),
        tl_a_addr=int(env.uncache_if.a_bits_address.value),
        tl_d_valid=int(env.uncache_if.d_valid.value),
    )
    return {"cycle": int(cycle), **sample}


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_send_req_a_fire_enters_wait_resp_without_duplicate_request(env):
    scenario_key = "zhaoxinran/mmio/state-edge/a-fire-enters-wait-resp"
    base_seed, seed, rng = scenario_rng(scenario_key)
    latency = rng.randint(1, 16)
    env.uncache_agent.configure(latency=latency, mmio_latency=latency)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "latency": latency,
            "expected_path": "a_fire_enters_wait_resp_once",
        },
    )
    uncache._prepare_mmio_cnop_stream(env)
    env.uncache_agent.set_a_ready(0)
    snapshots: list[dict[str, int | None]] = []

    def capture(cycle: int, active_env) -> None:
        sample = _capture_post_drive_snapshot(cycle, active_env)
        snapshots.append(sample)

    env.register_cycle_observer(capture)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_a_valid_addr(env, uncache._MMIO_BASE, max_cycles=8000)
    stalled = [
        sample
        for sample in snapshots
        if sample["tl_a_valid"] == 1
        and sample["tl_a_ready"] == 0
    ]
    assert stalled, {"snapshots": snapshots[-32:]}

    env.uncache_agent.set_a_ready(None)
    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE, max_cycles=8000)
    assert uncache._wait_for_uncache_resp(env, max_cycles=8000)

    transitions = [
        sample
        for sample in snapshots
        if sample["tl_d_valid"] == 1 and sample["tl_a_valid"] == 0
    ]
    assert transitions, {"snapshots": snapshots[-32:]}
    wait_resp_cycle = min(
        sample["cycle"] for sample in snapshots if sample["tl_d_valid"] == 1
    )
    first_d_cycle = min(
        sample["cycle"] for sample in snapshots if sample["tl_d_valid"] == 1
    )
    assert not any(
        wait_resp_cycle <= sample["cycle"] <= first_d_cycle
        and sample["tl_a_valid"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-32:]}
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_send_req_with_ibuffer_ready_drives_tl_a(env):
    scenario_key = "zhaoxinran/mmio/state-edge/ibuffer-ready-drives-tl-a"
    base_seed, seed, rng = scenario_rng(scenario_key)
    latency = rng.randint(1, 16)
    env.uncache_agent.configure(latency=latency, mmio_latency=latency)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "latency": latency,
            "expected_path": "ibuffer_ready_drives_tl_a",
        },
    )
    uncache._prepare_mmio_cnop_stream(env)
    snapshots: list[dict[str, int | None]] = []

    def capture(cycle: int, active_env) -> None:
        sample = _capture_post_drive_snapshot(cycle, active_env)
        snapshots.append(sample)

    env.register_cycle_observer(capture)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_a_valid_addr(env, uncache._MMIO_BASE, max_cycles=8000)
    assert any(
        sample["ifu_stall"] == 0
        and sample["tl_a_valid"] == 1
        and sample["tl_a_ready"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-32:]}
    assert uncache._wait_for_uncache_resp(env, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, uncache._MMIO_BASE, max_cycles=8000)
    assert not env.monitor.get_errors()
