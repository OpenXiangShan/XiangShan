from __future__ import annotations

import os

import pytest


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_ICACHE_CONTROL_BASE = 0x38022080


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_full_frontend_icache_control_read_returns_reset_state(env) -> None:
    # The control node's synchronized reset releases after the top-level reset.
    env.step(4)
    env.icache_control_agent.request(
        opcode=4,
        address=_ICACHE_CONTROL_BASE,
        source=3,
    )
    for _ in range(64):
        env.step(1)
        if env.icache_control_agent.get_stats()["response_count"] == 1:
            break

    stats = env.icache_control_agent.get_stats()
    assert stats["accepted_request_count"] == 1
    assert stats["response_count"] == 1
    assert stats["outstanding"] is False
    response = stats["responses"][0]
    assert response["opcode"] == 1
    assert response["size"] == 3
    assert response["source"] == 3
    assert response["data"] & 1 == 1
