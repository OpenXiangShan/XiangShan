"""Reset must discard in-flight external responses before the next DUT epoch."""

from types import SimpleNamespace

from env.core.frontend_env import FrontendEnv
from env.runtime.dut_factory import FakeDUTFrontend


def test_reset_discards_external_responses_and_pauses_ptw_checkers(monkeypatch):
    dut = FakeDUTFrontend()
    env = FrontendEnv(dut, register_callbacks=False)
    env.uncache_agent.configure(latency=4)
    env.ptw_agent.configure(latency=4, response_source="model")
    env.uncache_agent.pending.append(SimpleNamespace(ready_cycle=200))
    env.ptw_agent.pending.append(SimpleNamespace(ready_cycle=200))
    env.ptw_agent.active_resp = SimpleNamespace(
        resp={}, snapshot=SimpleNamespace(sequence_id=1, cycle=99), driven_source="model",
        ready_cycle=99, vpn=0, s2xlate=0, get_gpa=0,
    )
    env.ptw_agent.last_drive_expectation = {"resp": {}}
    env.uncache_agent.resp_count = 2
    env.ptw_agent.resp_count = 3
    env.uncache_if.d_valid.value = 1
    env.uncache_if.a_valid.value = 1
    env.uncache_if.a_bits_address.value = 0x80000000
    env.ptw_if.req_0_ready.value = 1
    env.ptw_if.req_0_valid.value = 1
    env.ptw_if.req_0_bits_vpn.value = 0x80000
    env.ptw_if.resp_valid.value = 1

    checked = []
    monkeypatch.setattr(env.ptw_full_ppn_checker, "on_clock_edge", lambda cycle: checked.append(("ppn", cycle)))
    monkeypatch.setattr(env.ptw_resp_input_checker, "on_clock_edge", lambda cycle: checked.append(("input", cycle)))

    dut.reset.value = 1
    env._on_clock_edge(100)
    env._on_clock_edge(101)

    assert not env.uncache_agent.pending
    assert not env.ptw_agent.pending
    assert env.ptw_agent.active_resp is None
    assert env.ptw_agent.last_drive_expectation is None
    assert env.uncache_if.a_ready.value == env.uncache_if.d_valid.value == 0
    assert env.ptw_if.req_0_ready.value == env.ptw_if.resp_valid.value == 0
    assert env.uncache_agent.req_count == env.ptw_agent.req_count == 0
    assert env.uncache_agent.resp_count == 2
    assert env.ptw_agent.resp_count == 3
    assert env.uncache_agent.latency == env.ptw_agent.latency_min == 4
    assert checked == []

    dut.reset.value = 0
    env._on_clock_edge(102)
    assert checked == [("ppn", 102), ("input", 102)]
    assert env.uncache_agent.req_count == env.ptw_agent.req_count == 1
