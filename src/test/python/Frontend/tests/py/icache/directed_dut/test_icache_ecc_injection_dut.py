"""Corrupt real SRAM parity; refetch through FTQ and require actual ECC coverage."""

import os

import pytest

from tests.py.icache.directed_dut.test_icache_lowrisk_gap_closure_dut import (
    _drive_soft_prefetch,
    _initialize_cacheable_stream,
)


pytestmark = pytest.mark.skipif(
    os.getenv("TB_ENABLE_DUT_TESTS") != "1",
    reason="requires compiled DUT",
)

@pytest.mark.parametrize("kind,bin_name", [
    pytest.param("meta", "meta_code_mismatch_single_way", marks=pytest.mark.funcov_bins("BIN-641")),
    pytest.param("data", "data_ecc_selected_valid_sram_bank", marks=pytest.mark.funcov_bins("BIN-642")),
    pytest.param("multiway", "meta_multiway_hit", marks=pytest.mark.funcov_bins("BIN-770")),
    pytest.param("unselected", "data_ecc_unselected_bank_ignored", marks=pytest.mark.funcov_bins("BIN-773")),
])
def test_icache_sram_ecc_refetch(env, kind, bin_name):
    base = target = 0x80040000
    _initialize_cacheable_stream(env, base, latency=8)
    env.load_program((0x13).to_bytes(4, "little") * 1024, target)
    env.backend_model.set_can_accept(0)
    agent = env.icache_ecc_agent
    beu_samples = []

    def observe_error(cycle, active_env):
        beu_valid = getattr(active_env.dut, "io_error_ecc_error_valid", None)
        beu_addr = getattr(active_env.dut, "io_error_ecc_error_bits", None)
        assert beu_valid is not None and beu_addr is not None
        if int(beu_valid.value) == 1:
            beu_samples.append({
                "cycle": cycle,
                "paddr": int(beu_addr.value),
            })

    env.register_cycle_observer(observe_error)
    try:
        # Fetch the target once so tag/valid/data are written by the real refill path.
        agent.wait_resident(target, max_cycles=4096)
        env.step(16)
        if kind == "multiway":
            second = target + 0x4000
            env.load_program((0x13).to_bytes(4, "little") * 16, second)
            _drive_soft_prefetch(env, [second])
            dest_way = agent.wait_resident(second)
            env.step(8)
            mutation = agent.clone_meta_to_second_way(target, dest_way=dest_way)
        elif kind == "meta":
            mutation = agent.inject_meta_ecc(target)
        else:
            mutation = agent.inject_data_ecc(target, bank=1 if kind == "unselected" else 0)
        env.step(2)
        agent.verify_persisted(mutation)
        injection_cycle = int(env.current_cycle)
        bin_id = {
            "meta": "BIN-641",
            "data": "BIN-642",
            "multiway": "BIN-770",
            "unselected": "BIN-773",
        }[kind]
        hits_before = env.functional_coverage.hit_count_by_bin_id(bin_id)
        # Redirect flushes cached WayLookup metadata but preserves the SRAM fault.
        assert not env.monitor.get_errors()
        env.monitor.clear()
        env.monitor.set_expected_pc(target)
        env.backend_model.inject_redirect(target, "ctrl_redirect", delay_cycles=0)
        env.backend_model.set_can_accept(1)
        for _ in range(2048):
            if env.functional_coverage.hit_count_by_bin_id(bin_id) > hits_before:
                break
            env.step(1)
        else:
            raise AssertionError({"missing_new_ecc_hit": bin_name, "hits_before": hits_before})
        detail = env.functional_coverage.hit_detail_by_bin_id(bin_id)
        assert detail is not None and detail["evidence"], {"missing_ecc_evidence": bin_id}
        s2_evidence = detail["evidence"][-1]
        if kind == "unselected":
            env.step(4)
            assert not [s for s in beu_samples if s["cycle"] >= injection_cycle], beu_samples
            assert not env.get_errors()
            return
        assert s2_evidence["s2_corrupt"][0] == 1, s2_evidence
        assert s2_evidence["error_valid"] == 1, s2_evidence
        for _ in range(12):
            if any(s["cycle"] >= injection_cycle and s["paddr"] == target for s in beu_samples):
                break
            env.step(1)
        else:
            raise AssertionError({
                "reason": "ECC error did not propagate from MainPipe to BEU",
                "beu_errors": beu_samples[-8:],
                "coverage": detail,
            })
        assert not env.monitor.get_errors()
    finally:
        agent.restore_all()
        env.backend_model.set_can_accept(1)
