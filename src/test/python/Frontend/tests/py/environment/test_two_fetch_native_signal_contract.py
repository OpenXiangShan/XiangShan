"""Runtime admission checks for the two-fetch sampler's current interfaces."""

import os

import pytest

from env.funcov.py.ftq.two_fetch_funcov import _TWO_FETCH_SIGNALS


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires compiled DUT")
def test_two_fetch_current_native_aliases_are_readable(env):
    recorder = env.functional_coverage
    keys = (
        "bpu_s3_flush", "bpu_s3_flush_ptr_flag", "bpu_s3_flush_ptr_value",
        "prefetch_case", "ifu_valid", "ifu_ready", "ifu_req1_valid",
        "ifu_req0_size", "ifu_first_taken", "ifu_req0_ftq_flag",
        "ifu_req0_ftq_value", "ifu_req1_ftq_flag", "ifu_req1_ftq_value",
        "ifu_req0_start", "ifu_req1_start", "main_s1_exception0",
        "main_s1_exception1",
    )
    for _ in range(16):
        env.step(1)
        values = {
            key: recorder._read_first_dut_signal(env.dut, _TWO_FETCH_SIGNALS[key])
            for key in keys
        }
        assert all(value is not None for value in values.values()), values
        offset = recorder._read_first_dut_signal(
            env.dut,
            ("Frontend_top.Frontend.inner_icache.mainPipe.s1_maybeRvcAlignInfo_takenCfiOffset_0",),
        )
        assert offset is not None
        assert values["ifu_req0_size"] == ((offset + 1) & 0x3F)


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires compiled DUT")
def test_ftq_ingress_native_aliases_match_live_sources(env):
    recorder = env.functional_coverage
    source_by_key = {
        'ftq_valid': 'Frontend_top.Frontend.inner_icache.mainPipe.io_fromFtq_valid',
        'ftq_ready': 'Frontend_top.Frontend.inner_icache.mainPipe.io_fromFtq_ready',
        'ftq_req1_valid': 'Frontend_top.Frontend.inner_icache.mainPipe.io_fromFtq_bits_req_1_valid',
        'ftq_req0_start': 'Frontend_top.Frontend.inner_icache.mainPipe.io_fromFtq_bits_req_0_vAddr_0_addr',
        'ftq_req1_start': 'Frontend_top.Frontend.inner_icache.mainPipe.io_fromFtq_bits_req_1_vAddr_0_addr',
        'ftq_req0_ftq_flag': 'Frontend_top.Frontend.inner_ftq.fetchPtr_ptrs_0_flag',
        'ftq_req0_ftq_value': 'Frontend_top.Frontend.inner_ftq.fetchPtr_ptrs_0_value',
        'ftq_req1_ftq_flag': 'Frontend_top.Frontend.inner_ftq.fetchPtr_ptrs_1_flag',
        'ftq_req1_ftq_value': 'Frontend_top.Frontend.inner_ftq.fetchPtr_ptrs_1_value',
        'ftq_req0_end': 'Frontend_top.Frontend.inner_icache.mainPipe.io_fromFtq_bits_req_0_endPosition',
        'ftq_req1_end': 'Frontend_top.Frontend.inner_icache.mainPipe.io_fromFtq_bits_req_1_endPosition',
        'ftq_req0_exception': 'Frontend_top.Frontend.inner_icache.mainPipe.io_fromFtq_bits_req_0_hasBackendException',
    }
    for _ in range(16):
        env.step(1)
        for key, source in source_by_key.items():
            native = recorder._read_first_dut_signal(env.dut, _TWO_FETCH_SIGNALS[key])
            live_source = recorder._read_first_dut_signal(env.dut, (source,))
            assert native is not None and live_source is not None, key
            assert native == live_source, key


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires compiled DUT")
def test_prefetch_and_real_two_native_aliases_match_live_sources(env):
    recorder = env.functional_coverage
    sources = {
        "prefetch_valid": "Frontend_top.Frontend.inner_ftq.prefetchQueue.io_deq_valid_0",
        "prefetch_ready": "Frontend_top.Frontend.inner_icache.prefetcher.io_fromFtq_ready",
        "way_real_two": "Frontend_top.Frontend.inner_icache.dataArray.io_read_req_bits_1_valid",
    }
    for _ in range(16):
        env.step(1)
        for key, source in sources.items():
            native = recorder._read_first_dut_signal(env.dut, _TWO_FETCH_SIGNALS[key])
            live_source = recorder._read_first_dut_signal(env.dut, (source,))
            assert native is not None and live_source is not None, key
            assert native == live_source, key
