from __future__ import annotations

import os

import pytest
from env.core.transactions import ProgramImage
from env.sequences import LoadProgramSequence
from env.support import record_scenario, scenario_rng

from tests.py.support import uncache_scenarios as uncache

_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"


_JALR_X0_X1_0 = 0x00008067


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_jalr_return_is_delivered_as_indirect_control_flow(env):
    scenario_key = "zhaoxinran/mmio/control-flow/jalr-return"
    base_seed, seed, rng = scenario_rng(scenario_key)
    latency = rng.randint(1, 16)
    env.uncache_agent.configure(latency=latency, mmio_latency=latency)
    payload = int(_JALR_X0_X1_0).to_bytes(4, "little")
    payload += int(uncache._CNOP).to_bytes(2, "little") * 128
    env.memory.mmio_ranges.append((uncache._MMIO_BASE, uncache._MMIO_BASE + len(payload)))
    LoadProgramSequence(
        image=ProgramImage(payload=payload, base_addr=uncache._MMIO_BASE),
        step_cycles=0,
    ).run(env)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "pc": uncache._MMIO_BASE,
            "latency": latency,
            "expected_path": "indirect_control_flow",
        },
    )
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE)
    assert uncache._wait_for_observed_pc(env, uncache._MMIO_BASE, max_cycles=8000)
    observed = next(
        item for item in env.monitor.observations if int(item.pc) == uncache._MMIO_BASE
    )
    assert int(observed.instr) == _JALR_X0_X1_0
    assert not bool(observed.is_rvc)
    assert env.branch_checker.get_stats()["by_type"]["jump_indirect"] >= 1
    assert not env.monitor.get_errors()
