from __future__ import annotations

import os

import pytest
from env.support import record_scenario, scenario_rng

from tests.py.support import uncache_scenarios as uncache
from tests.py.zhaoxinran.support.mmio import configure_random_mmio_latency, prepare_mmio_jalr_stream

_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"


_JALR_X0_X1_0 = 0x00008067


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_jalr_return_is_delivered_as_indirect_control_flow(env):
    scenario_key = "zhaoxinran/mmio/control-flow/jalr-return"
    base_seed, seed, rng = scenario_rng(scenario_key)
    latency = configure_random_mmio_latency(env, rng)
    pc = prepare_mmio_jalr_stream(env, rng, _JALR_X0_X1_0)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "pc": pc,
            "latency": latency,
            "expected_path": "indirect_control_flow",
        },
    )
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    assert uncache._wait_for_request_addr(env, pc)
    assert uncache._wait_for_observed_pc(env, pc, max_cycles=8000)
    observed = next(item for item in env.monitor.observations if int(item.pc) == pc)
    assert int(observed.instr) == _JALR_X0_X1_0
    assert not bool(observed.is_rvc)
    assert env.branch_checker.get_stats()["by_type"]["jump_indirect"] >= 1
    assert not env.monitor.get_errors()
