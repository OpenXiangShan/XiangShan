from __future__ import annotations

import pytest

from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


@pytest.mark.funcov_bins("BIN-1012")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_tl_a_stall_holds_request_context(env):
    uncache._prepare_mmio_cnop_stream(env)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_req(env) > 0
    assert uncache._wait_for_uncache_resp(env) > 0

    env.uncache_agent.set_a_ready(0)
    for _ in range(256):
        env.step(1)
        if int(env.uncache_if.a_valid.value) == 1:
            break

    req_before = int(env.uncache_agent.get_stats().get("req_count", 0))
    assert int(env.uncache_if.a_ready.value) == 0
    assert int(env.uncache_if.a_valid.value) == 1
    stalled_addr = int(env.uncache_if.a_bits_address.value)

    env.step(8)
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == req_before
    assert int(env.uncache_if.a_valid.value) == 1
    assert int(env.uncache_if.a_bits_address.value) == stalled_addr
    assert env.functional_coverage.key_hit("ifu_mmio_tl_a_stall", "stable_until_accept")

    env.uncache_agent.set_a_ready(None)
    assert uncache._wait_for_uncache_req_delta(env, 1)
    assert not env.monitor.get_errors()
