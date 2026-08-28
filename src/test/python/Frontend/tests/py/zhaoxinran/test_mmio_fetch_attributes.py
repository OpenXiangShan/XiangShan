from __future__ import annotations

import pytest

from env.funcov.py.ifu import mmio_nc_owner_funcov as owner_funcov
from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_pbmt_nc_non_mmio_enters_uncache_send_path(env):
    expected_block, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        pbmt=uncache._PBMT_NC,
    )
    uncache._initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    uncache._configure_exec_pmp_4k(env, base_addr=mapping.paddr)
    uncache._configure_exec_cacheable_pma_4k(env, base_addr=mapping.paddr)
    uncache._force_redirect_to(env, mapping.vaddr)

    assert uncache._wait_for_uncache_req(env, max_cycles=6000) > 0
    assert uncache._wait_for_uncache_resp(env, max_cycles=6000) > 0
    assert mapping.paddr in env.uncache_agent.get_stats().get("request_addrs", [])
    assert uncache._wait_for_observed_pc(env, mapping.vaddr, max_cycles=12000)
    assert any(int(obs.pc) == mapping.vaddr for obs in env.monitor.observations)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_pmp_mmio_with_pbmt_nc_waits_for_mmio_commit_order(env):
    _expected_block, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        pbmt=uncache._PBMT_NC,
    )
    uncache._initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    uncache._configure_exec_pmp_4k(env, base_addr=mapping.paddr)
    uncache._configure_exec_mmio_pma_4k(env, base_addr=mapping.paddr)
    env.backend_model.set_can_accept(0)
    snapshots: list[dict[str, int | None]] = []

    def capture(cycle: int, active_env) -> None:
        snapshots.append(
            {
                "cycle": int(cycle),
                **owner_funcov._snapshot(
                    active_env.functional_coverage, active_env.dut
                ),
            }
        )

    env.register_cycle_observer(capture)
    uncache._force_redirect_to(env, mapping.vaddr)

    assert uncache._wait_for_uncache_req(env, max_cycles=6000) > 0
    assert uncache._wait_for_uncache_resp(env, max_cycles=6000) > 0
    req_before_release = int(env.uncache_agent.get_stats().get("req_count", 0))
    env.step(64)
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == req_before_release

    env.backend_model.set_can_accept(1)
    assert uncache._wait_for_uncache_req(env, max_cycles=6000) > req_before_release
    assert any(
        sample["s2_valid"] == 1
        and sample["s2_pmp_mmio"] == 1
        and sample["s2_pbmt"] == uncache._PBMT_NC
        and sample["is_first"] == 0
        and sample["backend_commit"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}
    assert uncache._wait_for_observed_pc(env, mapping.vaddr, max_cycles=12000)
    assert not env.monitor.get_errors()
