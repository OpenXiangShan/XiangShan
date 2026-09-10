"""Corrupt real SRAM parity; refetch through FTQ and require actual ECC coverage."""

import os

import pytest

from env.funcov.py.icache.icache_mainpipe_funcov import _S2_CORRUPT
from tests.py.ruierhan.test_icache_lowrisk_gap_closure_dut import (
    _wait_funcov_hit, _drive_soft_prefetch,
    _initialize_cacheable_stream,
)


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires compiled DUT")
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
    error_samples = []

    def observe_error(cycle, active_env):
        reader = active_env.functional_coverage._read_first_dut_signal
        main = agent.ROOT + "mainPipe."
        candidates = {
            "error_valid": (main + "io_error_valid", main + "__Vtogcov__io_error_valid"),
            "corrupt": _S2_CORRUPT[0],
            "paddr": (main + "io_error_bits_paddr",),
        }
        values = {key: reader(active_env.dut, names) for key, names in candidates.items()}
        missing = {key: candidates[key] for key, value in values.items() if value is None}
        assert not missing, {"missing_ecc_observations": missing}
        if values["error_valid"] == 1:
            error_samples.append({
                "cycle": cycle,
                "corrupt": values["corrupt"],
                "paddr": values["paddr"],
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
        # Redirect flushes cached WayLookup metadata but preserves the SRAM fault.
        assert not env.monitor.get_errors()
        env.monitor.clear()
        env.monitor.set_expected_pc(target)
        env.backend_model.inject_redirect(target, "ctrl_redirect", delay_cycles=0)
        env.backend_model.set_can_accept(1)
        _wait_funcov_hit(env, "icache_mainpipe_s2_ecc", bin_name, max_cycles=2048)
        if kind == "unselected":
            env.step(4)
            assert not [s for s in error_samples if s["cycle"] >= injection_cycle], error_samples
            assert not env.get_errors()
            return
        assert any(s["cycle"] >= injection_cycle and s["corrupt"] == 1
                   and s["paddr"] == target for s in error_samples), error_samples
        # Frontend registers the ICache error twice before the BEU-facing port.
        for _ in range(8):
            valid = getattr(env.dut, "io_error_ecc_error_valid", None)
            address = getattr(env.dut, "io_error_ecc_error_bits", None)
            if valid is not None and address is not None and int(valid.value) == 1:
                assert int(address.value) == target
                break
            env.step(1)
        else:
            raise AssertionError("ECC did not reach the BEU-facing Frontend error port")
        assert not env.monitor.get_errors()
    finally:
        agent.restore_all()
        env.backend_model.set_can_accept(1)
