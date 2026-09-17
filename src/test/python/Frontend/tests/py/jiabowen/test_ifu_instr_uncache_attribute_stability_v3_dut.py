from __future__ import annotations

import os

import pytest

from env.funcov.py.ifu import mmio_nc_owner_funcov as owner
from tests.py.support import uncache_scenarios as uncache


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_GROUP = "ifu_instruncache_owner_v3"
_USER_PORTS = {
    "tl_a_mem_back_type_mm": "auto_inner_instrUncache_client_out_a_bits_user_memBackType_MM",
    "tl_a_mem_page_type_nc": "auto_inner_instrUncache_client_out_a_bits_user_memPageType_NC",
}


@pytest.mark.funcov_bins("BIN-1095", "BIN-1096", "BIN-1129", "BIN-1130")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_pbmt_nc_tl_a_backpressure_holds_user_attributes(env) -> None:
    expected_block, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
    )
    uncache._initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    uncache._configure_exec_pmp_4k(env, base_addr=mapping.paddr)
    uncache._configure_exec_cacheable_pma_4k(env, base_addr=mapping.paddr)
    env.backend_model.set_can_accept(0)
    env.uncache_agent.set_a_ready(0)
    uncache._force_redirect_to(env, mapping.vaddr)

    assert uncache._wait_for_ptw_resp(env, max_cycles=6000), env.ptw_agent.get_stats()
    assert uncache._wait_for_uncache_a_valid_addr(
        env, mapping.paddr, max_cycles=6000
    ), {
        "mapping": mapping,
        "ptw": env.ptw_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }

    _check_stalled_user_attributes(env, paddr=mapping.paddr, expected=(1, 1))
    assert uncache._wait_for_observed_pc(env, expected_block[0][0], max_cycles=12000)
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1095", "BIN-1096", "BIN-1129", "BIN-1130")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_pma_mmio_tl_a_backpressure_holds_user_attributes(env) -> None:
    uncache._prepare_mmio_cnop_stream(env)
    uncache._initialize_mmio_fetch(env)
    assert uncache._wait_for_uncache_req(env) > 0
    assert uncache._wait_for_uncache_resp(env) > 0
    env.uncache_agent.set_a_ready(0)
    for _ in range(256):
        if int(env.uncache_if.a_valid.value) == 1:
            break
        env.step(1)
    assert int(env.uncache_if.a_valid.value) == 1
    paddr = int(env.uncache_if.a_bits_address.value)
    env.backend_model.set_can_accept(0)
    snapshot = owner._snapshot(env.functional_coverage, env.dut)
    assert snapshot["s2_instr_pc"] is not None
    expected_pc = int(snapshot["s2_instr_pc"]) << 1
    before_observations = len(env.monitor.observations)
    _check_stalled_user_attributes(env, paddr=paddr, expected=(0, 0))
    for _ in range(12000):
        if any(item.pc == expected_pc for item in env.monitor.observations[before_observations:]):
            break
        env.step(1)
    assert any(item.pc == expected_pc for item in env.monitor.observations[before_observations:])
    assert not env.monitor.get_errors()


def _check_stalled_user_attributes(env, *, paddr: int, expected: tuple[int, int]) -> None:
    before = dict(env.uncache_agent.get_stats())

    first_snapshot = owner._snapshot(env.functional_coverage, env.dut)
    if (
        first_snapshot["tl_a_mem_back_type_mm"] is None
        or first_snapshot["tl_a_mem_page_type_nc"] is None
    ):
        env.functional_coverage.risk_observations.append(
            {
                "cycle": int(env.current_cycle),
                "risk": "instruncache_tl_a_user_attributes_unavailable",
                "tl_a_valid": first_snapshot["tl_a_valid"],
                "tl_a_ready": first_snapshot["tl_a_ready"],
                "tl_a_addr": first_snapshot["tl_a_addr"],
                "tl_a_mem_back_type_mm": first_snapshot[
                    "tl_a_mem_back_type_mm"
                ],
                "tl_a_mem_page_type_nc": first_snapshot[
                    "tl_a_mem_page_type_nc"
                ],
            }
        )
        env.uncache_agent.set_a_ready(None)
        pytest.skip(
            "current standalone Frontend DUT prunes InstrUncache TL-A user "
            "attributes; use a signal contract that retains MemBackTypeMM "
            "and MemPageTypeNC"
        )

    samples = []
    for _ in range(8):
        snapshot = owner._snapshot(env.functional_coverage, env.dut)
        # Independent public-port canary: the sampler alias must resolve to
        # the actual TL payload in this same pre-step phase.
        for field, path in _USER_PORTS.items():
            signal = getattr(env.dut, path, None)
            assert signal is not None, {"missing_user_port": path}
            assert snapshot[field] == int(signal.value), {"path": path, "snapshot": snapshot}
        if snapshot["tl_a_valid"] == 1 and snapshot["tl_a_ready"] == 0:
            samples.append(
                {
                    "cycle": int(env.current_cycle),
                    "addr": snapshot["tl_a_addr"],
                    "mem_back_type_mm": snapshot["tl_a_mem_back_type_mm"],
                    "mem_page_type_nc": snapshot["tl_a_mem_page_type_nc"],
                }
            )
        env.step(1)

    assert len(samples) == 8, samples
    assert {sample["addr"] for sample in samples} == {paddr}, samples
    assert {sample["mem_back_type_mm"] for sample in samples} == {expected[0]}, samples
    assert {sample["mem_page_type_nc"] for sample in samples} == {expected[1]}, samples
    assert int(env.uncache_agent.get_stats()["req_count"]) == int(before["req_count"])
    for leaf in (2, 3, 36, 37):
        assert env.functional_coverage.key_hit(_GROUP, f"instruncache_leaf_{leaf:03d}")

    env.uncache_agent.set_a_ready(None)
    assert uncache._wait_for_request_addr(
        env, paddr, max_cycles=6000
    ), env.uncache_agent.get_stats()
    assert uncache._wait_for_resp_count(
        env, int(before["resp_count"]) + 1, max_cycles=6000
    ), env.uncache_agent.get_stats()
    env.backend_model.set_can_accept(1)
    env.logger.info(
        "TL-A user attribute canary: expected=%s actual_paths=%s held_samples=%s",
        expected, _USER_PORTS, samples,
    )
    assert not env.monitor.get_errors()
