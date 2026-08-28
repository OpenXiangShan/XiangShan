from __future__ import annotations

import pytest

from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_page_tail_rvi_preserves_half_pc_data_and_state(env):
    uncache._prepare_cross_page_rvi_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=16)
    samples = uncache._register_prev_half_rvi_observer(env)
    uncache._initialize_mmio_fetch(env, reset_vector=uncache._CROSS_PAGE_PC)

    first_beat = uncache._CROSS_PAGE_PC & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    next_page = uncache._MMIO_BASE + uncache._SV39_PAGE_SIZE
    expected_half_data = uncache._ADDI_X0_X0_0 & 0xFFFF
    expected_half_pc = uncache._CROSS_PAGE_PC >> 1

    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=8000)
    assert uncache._wait_for_request_addr(env, next_page, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, uncache._CROSS_PAGE_PC, max_cycles=8000)

    first_page_resend = [
        sample
        for sample in samples
        if sample["last_request_addr"] == first_beat and int(sample["need_resend"]) == 1
    ]
    assert first_page_resend, {"samples": samples[-16:]}

    saved_half = [
        sample
        for sample in samples
        if sample["last_request_addr"] == first_beat
        and int(sample["s1"]) == 1
        and int(sample["s1_data"]) == expected_half_data
        and int(sample["s1_pc"]) == expected_half_pc
    ]
    assert saved_half, {"samples": samples[-16:]}

    second_page_pending = uncache._pending_uncache_samples(samples, next_page)
    assert second_page_pending, {"samples": samples[-16:]}
    assert all(
        int(sample["s2"]) == 1
        and int(sample["s2_data"]) == expected_half_data
        and int(sample["s2_pc"]) == expected_half_pc
        for sample in second_page_pending
    ), {"second_page_pending": second_page_pending}
    assert not env.monitor.get_errors()
