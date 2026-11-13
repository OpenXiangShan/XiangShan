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
