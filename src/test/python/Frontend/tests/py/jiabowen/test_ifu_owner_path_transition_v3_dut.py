"""Real-DUT coverage for the complete sequential IFU path transition cross."""

from __future__ import annotations

import os

import pytest

from env.core.transactions import ProgramImage
from env.sequences import LoadProgramSequence
from env.support import PmpPmaConfig

from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_CACHEABLE_VA = uncache._NORMAL_ALT_BASE
_NC_VA = uncache._NORMAL_BASE
_MMIO_VA = uncache._MMIO_BASE
_MMIO_PA = uncache._MMIO_BASE
_PAGE_SIZE = uncache._SV39_PAGE_SIZE
_CACHEABLE_REGION_BASE = 0x8000_0000
_PAYLOAD = int(uncache._CNOP).to_bytes(2, "little") * 256


def _prepare_three_path_stream(env) -> None:
    uncache._prepare_sv39_dual_nc_cacheable_stream(env)
    env.memory.mmio_ranges.append((_MMIO_PA, _MMIO_PA + len(_PAYLOAD)))
    env.page_table.map_page(
        _MMIO_VA >> 12,
        _MMIO_PA >> 12,
        v=1,
        r=1,
        x=1,
        pbmt=uncache._PBMT_IO,
    )
    LoadProgramSequence(
        image=ProgramImage(payload=_PAYLOAD, base_addr=_MMIO_PA),
        step_cycles=0,
    ).run(env)


def _wait_for_pc(env, pc: int, *, start_index: int = 0, max_cycles: int = 8000) -> None:
    for _ in range(int(max_cycles)):
        if any(int(obs.pc) == int(pc) for obs in env.monitor.observations[int(start_index) :]):
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": "path did not deliver expected PC",
            "pc": hex(int(pc)),
            "observed_tail": [hex(int(obs.pc)) for obs in env.monitor.observations[-32:]],
            "icache": env.icache_agent.get_stats(),
            "uncache": env.uncache_agent.get_stats(),
        }
    )


@pytest.mark.funcov_bins("BIN-957")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_ifu_owner_sequential_cacheable_nc_mmio_transitions(env) -> None:
    _prepare_three_path_stream(env)
    uncache._initialize_sv39_fetch(env, reset_vector=_NC_VA)
    uncache._configure_exec_attrs_16k(env, base_addr=_CACHEABLE_REGION_BASE)
    env.write_pmp_entry(
        1,
        PmpPmaConfig(match="napot", read=True, write=True, execute=True),
        _MMIO_PA,
        size=_PAGE_SIZE,
        settle_cycles=4,
    )
    env.write_pma_entry(
        1,
        PmpPmaConfig(
            match="napot",
            read=True,
            write=True,
            execute=True,
            cacheable=False,
        ),
        _MMIO_PA,
        size=_PAGE_SIZE,
        settle_cycles=4,
    )
    env.icache_agent.configure(hit_latency=8, miss_latency=8, miss_rate=0.0, seed=0x957)
    env.uncache_agent.configure(latency=8, mmio_latency=8)

    _wait_for_pc(env, _NC_VA)

    start_index = len(env.monitor.observations)
    uncache._force_redirect_to(env, _CACHEABLE_VA)
    _wait_for_pc(env, _CACHEABLE_VA, start_index=start_index)

    start_index = len(env.monitor.observations)
    uncache._force_redirect_to(env, _MMIO_VA)
    _wait_for_pc(env, _MMIO_VA, start_index=start_index)

    start_index = len(env.monitor.observations)
    uncache._force_redirect_to(env, _NC_VA)
    _wait_for_pc(env, _NC_VA, start_index=start_index)

    start_index = len(env.monitor.observations)
    uncache._force_redirect_to(env, _CACHEABLE_VA)
    _wait_for_pc(env, _CACHEABLE_VA, start_index=start_index)

    start_index = len(env.monitor.observations)
    uncache._force_redirect_to(env, _NC_VA)
    _wait_for_pc(env, _NC_VA, start_index=start_index)

    start_index = len(env.monitor.observations)
    uncache._force_redirect_to(env, _MMIO_VA)
    _wait_for_pc(env, _MMIO_VA, start_index=start_index)

    owner_state = getattr(env.functional_coverage, "_ifu_mmio_nc_owner_state", {})
    assert env.functional_coverage.key_hit(
        "ifu_v3_pipeline_owner_model", "owner_leaf_059"
    ), {
        "seen": sorted(owner_state.get("nc_seen", ())),
        "previous_path": owner_state.get("previous_path"),
        "path_transition_observations": owner_state.get("path_transition_observations"),
    }
    assert not env.monitor.get_errors()
