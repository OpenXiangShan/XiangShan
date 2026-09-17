"""Regression tests for optimized-port aliases, unknowns, and cycle semantics."""

import pytest

from env.funcov.py.icache import icache_hitmiss_funcov as hitmiss
from env.funcov.py.icache import icache_mainpipe_funcov as mainpipe
from env.funcov.py.icache import icache_prefetchpipe_funcov as prefetch
from env.funcov.py.icache import icache_waylookup_funcov as waylookup
from env.funcov.py.icache.signal_contract import half_aligned_cross_line, validate_target_probes
from tests.py.ruierhan.test_icache_functional_coverage import _Recorder, _hit


@pytest.mark.parametrize("module,key,path", [
    (mainpipe, "pmp_instr", "Frontend_top.Frontend.inner_icache.__Vtogcov__io_pmp_0_resp_instr"),
    (hitmiss, "pmp_instr", "Frontend_top.Frontend.inner_icache.__Vtogcov__io_pmp_0_resp_instr"),
    (mainpipe, "pmp_mmio", "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_bits_info_0_icacheMeta_pmpMmio"),
    (mainpipe, "toifu_maybe_rvc_map", "Frontend_top.Frontend._inner_icache_io_toIfu_req_bits_maybeRvcMap"),
    (waylookup, "fencei", "Frontend_top.Frontend.inner_icache_io_fencei_REG"),
    (hitmiss, "fencei", "Frontend_top.Frontend.inner_icache_io_fencei_REG"),
    (prefetch, "s1_flush", "Frontend_top.Frontend.inner_icache.__Vtogcov__io_itlbFlushPipe"),
    (prefetch, "itlb_req_valid", "Frontend_top.Frontend.inner_icache.__Vtogcov__io_itlb_req_valid"),
    (prefetch, "itlb_resp_miss", "Frontend_top.Frontend.inner_icache.__Vtogcov__io_itlb_resp_bits_miss"),
])
def test_icache_exported_alias_preserves_unknown_zero_and_one(module, key, path):
    recorder = _Recorder()
    read = prefetch._read_prefetch if module is prefetch else module._read
    assert read(recorder, key) is None
    for value in (0, 1):
        recorder.env.dut.set(path, value)
        assert read(recorder, key) == value


def test_icache_pmp_main_port_does_not_read_prefetch_port():
    recorder = _Recorder()
    recorder.env.dut.set("Frontend_top.Frontend.inner_icache.__Vtogcov__io_pmp_1_resp_instr", 1)
    assert mainpipe._read(recorder, "pmp_instr") is None
    assert hitmiss._read(recorder, "pmp_instr") is None


@pytest.mark.parametrize("top,registered,expected", [(1, 0, True), (0, 1, False), (0, None, False)])
def test_icache_waylookup_uses_registered_fencei(top, registered, expected):
    recorder = _Recorder()
    for key, value in {"write0_valid": 1, "write0_ready": 1, "write0_exception": 0, "flush": 0}.items():
        recorder.set_waylookup_key(key, value)
    recorder.env.dut.set("Frontend_top.io_fencei", top)
    if registered is not None:
        recorder.env.dut.set("Frontend_top.Frontend.inner_icache_io_fencei_REG", registered)
    waylookup.sample_icache_waylookup_coverage(recorder, recorder.env, 1)
    assert _hit(recorder, "icache_waylookup_queue", "entry_fields") == expected


def test_icache_prefetch_flush_includes_bpu_without_global_flush():
    recorder = _Recorder()
    recorder.set_prefetch_key("global_flush", 0)
    recorder.env.dut.set("Frontend_top.Frontend.inner_icache.__Vtogcov__io_itlbFlushPipe", 1)
    assert prefetch._read_prefetch(recorder, "s1_flush") == 1
    assert prefetch._read_prefetch(recorder, "itlb_flush") == 1


@pytest.mark.parametrize("start,end,expected", [(0, 31, 0), (16, 15, 0), (16, 16, 1), (None, 16, None), (16, None, None)])
def test_icache_cross_line_uses_pruned_address_and_preserves_unknown(start, end, expected):
    assert half_aligned_cross_line(start, end) == expected


def test_icache_target_probe_contract_fails_only_for_relevant_missing_signals():
    recorder = _Recorder()
    recorder.coverage_targets = {"bin_ids": ["BIN-608"]}
    with pytest.raises(AssertionError, match="main_pmp_instr"):
        validate_target_probes(recorder)
    recorder.env.dut.set("Frontend_top.Frontend.inner_icache.__Vtogcov__io_pmp_0_resp_instr", 0)
    validate_target_probes(recorder)
    recorder.coverage_targets = {"bin_ids": ["BIN-422"]}
    validate_target_probes(recorder)
