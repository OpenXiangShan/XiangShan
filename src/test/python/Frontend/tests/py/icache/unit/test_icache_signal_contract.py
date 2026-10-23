"""Regression tests for optimized-port aliases, unknowns, and cycle semantics."""

import pytest

from env.funcov.py.icache import icache_hitmiss_funcov as hitmiss
from env.funcov.py.icache import icache_mainpipe_funcov as mainpipe
from env.funcov.py.icache import icache_prefetchpipe_funcov as prefetch
from env.funcov.py.icache import icache_waylookup_funcov as waylookup
from env.funcov.py.icache.signal_contract import (
    MAIN_EXCEPTIONS,
    TO_IFU_EXCEPTIONS,
    half_aligned_cross_line,
    validate_target_probes,
)
from tests.py.icache.unit.test_icache_functional_coverage import _Recorder, _hit


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


def test_mainpipe_exception_probes_preserve_request_identity_and_tl_boundary():
    recorder = _Recorder()
    main = "Frontend_top.Frontend.inner_icache.mainPipe."
    output = "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_bits_info_"
    recorder.env.dut.set(main + "s1_exception_value", 7)
    assert mainpipe._read(recorder, "exception") is None
    recorder.env.dut.set(main + "s1_exception_0_value", 0)
    assert mainpipe._read(recorder, "req1_exception") is None
    recorder.env.dut.set(main + "s1_exception_1_value", 3)
    recorder.env.dut.set(output + "0_icacheMeta_exception_value", 2)
    recorder.env.dut.set(output + "1_icacheMeta_exception_value", 3)

    assert mainpipe._read(recorder, "exception") == 0
    assert mainpipe._read(recorder, "req1_exception") == 3
    assert tuple(recorder._read_first_dut_signal(recorder.env.dut, names)
                 for names in MAIN_EXCEPTIONS) == (0, 3)
    assert tuple(recorder._read_first_dut_signal(recorder.env.dut, names)
                 for names in TO_IFU_EXCEPTIONS) == (2, 3)


@pytest.mark.parametrize("bin_id", [613, 616, 635, 638])
def test_mainpipe_target_requires_real_request_exception_probes(bin_id):
    recorder = _Recorder()
    recorder.coverage_targets = {"bin_ids": [f"BIN-{bin_id}"]}
    for key in ("pmp_mmio", "toifu_ready"):
        recorder.set_key(key, 0)
    recorder.env.dut.set("Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_valid", 0)
    recorder.env.dut.set(mainpipe._MAIN + "s1_exception_value", 0)
    with pytest.raises(AssertionError, match="main_exception_0"):
        validate_target_probes(recorder)
    recorder.set_key("exception", 0)
    if bin_id in (613, 616):
        with pytest.raises(AssertionError, match="main_exception_1"):
            validate_target_probes(recorder)
        recorder.set_key("req1_exception", 0)
    validate_target_probes(recorder)


@pytest.mark.parametrize("req1_valid,exception0,exception1,expected", [
    (0, 0, None, True),
    (0, 0, 3, True),
    (1, 0, 0, True),
    (1, 0, 3, False),
    (1, 0, None, False),
    (1, 3, 0, False),
    (0, None, 0, False),
    (None, 0, 0, False),
])
def test_mainpipe_hit_classification_checks_only_valid_requests(
    req1_valid, exception0, exception1, expected
):
    snapshot = {
        "s1_valid": 1, "s1_flush": 0, "pmp_mmio": 0, "is_mmio": 0,
        "itlb_exception": 0, "exception": exception0, "req1_exception": exception1,
        "req1_valid": req1_valid, "cross0": 0, "cross1": 0,
        "should": (0, 0, 0, 0),
    }
    hits = (1, 1, 1, 1)
    assert mainpipe._pure_cache_hit(snapshot, hits, (0, 0, 0, 0)) == expected
    assert mainpipe._all_valid_lines_cache_hit(snapshot, hits) == expected
