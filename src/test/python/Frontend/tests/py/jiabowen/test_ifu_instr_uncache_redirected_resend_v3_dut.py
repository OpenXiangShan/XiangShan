from __future__ import annotations

import pytest

from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


_OWNER_GROUP = "ifu_instruncache_owner_v3"
_OWNER_LEAF = "instruncache_leaf_011"
_BLOCKING_RISKS = {
    "ifu_instruncache_redirected_resend_old_identity_leak",
    "ifu_instruncache_redirected_resend_second_d_fault",
    "ifu_instruncache_redirected_resend_timeout",
}


def _owner_state(env) -> dict:
    return env.functional_coverage._ifu_instr_uncache_owner_state


def _wait_for_second_a_fire(env, *, max_cycles: int = 4000) -> dict | None:
    for _ in range(int(max_cycles)):
        cross_8b = _owner_state(env).get("cross_8b_pending")
        if cross_8b is not None and cross_8b.get("second_a_fire"):
            return dict(cross_8b)
        env.step(1)
    return None


def _wait_for_redirected_resend(env, *, max_cycles: int = 128) -> dict | None:
    for _ in range(int(max_cycles)):
        pending = _owner_state(env).get("redirected_resend_pending")
        if pending is not None:
            return dict(pending)
        env.step(1)
    return None


@pytest.mark.funcov_bins("BIN-1104")
@pytest.mark.skipif(
    not uncache._RUN_DUT,
    reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration",
)
def test_cross_8b_resend_redirect_completes_without_old_delivery(env):
    uncache._prepare_cross_beat_rvi_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=96)
    uncache._initialize_mmio_fetch(env, reset_vector=uncache._CROSS_BEAT_PC)

    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE)
    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE + 8)
    cross_8b = _wait_for_second_a_fire(env)
    assert cross_8b is not None, {
        "owner_state": _owner_state(env),
        "uncache": env.uncache_agent.get_stats(),
    }
    assert tuple(cross_8b["old_identity"])[2] == (uncache._CROSS_BEAT_PC >> 1)
    assert (uncache._MMIO_BASE + 8) not in env.uncache_agent.get_stats().get(
        "response_addrs", []
    )

    recovery_pc = uncache._MMIO_BASE + 0x40
    uncache._force_redirect_to(env, recovery_pc)
    redirected = _wait_for_redirected_resend(env)
    assert redirected is not None, {
        "owner_state": _owner_state(env),
        "uncache": env.uncache_agent.get_stats(),
    }
    redirect_cycle = int(redirected["redirect_cycle"])
    assert redirected["saw_second_a_fire"] is True
    assert redirected["saw_second_d"] is False

    for _ in range(4000):
        env.step(1)
        if env.functional_coverage.key_hit(_OWNER_GROUP, _OWNER_LEAF):
            break

    stats = env.uncache_agent.get_stats()
    risks = [
        item
        for item in env.functional_coverage.risk_observations
        if item.get("event") in _BLOCKING_RISKS
        and int(item.get("cycle", -1)) >= redirect_cycle
    ]
    assert env.functional_coverage.key_hit(
        _OWNER_GROUP, _OWNER_LEAF
    ), {
        "owner_state": _owner_state(env),
        "uncache": stats,
        "blocking_risks": risks,
        "monitor_errors": env.monitor.get_errors(),
    }
    assert (uncache._MMIO_BASE + 8) in stats.get("response_addrs", [])
    assert uncache._wait_for_observed_pc(env, recovery_pc, max_cycles=4000)
    assert not any(
        int(observation.pc) == uncache._CROSS_BEAT_PC
        for observation in env.monitor.observations
    )
    assert not risks
    assert not env.monitor.get_errors()
