from __future__ import annotations

import pytest

from env.checkers.ptw_checker import PTWFullPpnChecker, PTWRespInputChecker
from env.core.checkers import PTWFullPpnChecker as LegacyPTWFullPpnChecker
from env.core.checkers import PTWRespInputChecker as LegacyPTWRespInputChecker
from env.core.frontend_env import FrontendEnv
from env.runtime.dut_factory import FakeDUTFrontend


def test_ptw_checkers_keep_legacy_imports() -> None:
    assert LegacyPTWFullPpnChecker is PTWFullPpnChecker
    assert LegacyPTWRespInputChecker is PTWRespInputChecker


def _response() -> dict:
    return {
        "s2xlate": 1,
        "s1_entry_v": 1,
        "s1_entry_ppn": 0x123,
        "s1_entry_level": 0,
        "s1_entry_perm_x": 1,
        "s1_pf": 0,
        "s1_af": 0,
        "s2_entry_ppn": 0,
        "s2_gpf": 0,
    }


def _checker(*, cycle: int, ready: int, response: dict, events: list) -> PTWRespInputChecker:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    env.ptw_if.resp_valid.value = 1
    env.ptw_if.resp_ready.value = int(ready)
    for name, value in _response().items():
        getattr(env.ptw_if, f"resp_bits_{name}").value = value
    env.ptw_agent.last_drive_expectation = {"resp": dict(response)}
    env.monitor.capture_cycle_observation(cycle)
    checker = env.ptw_resp_input_checker
    checker.set_event_sink(events.append)
    return checker


def test_ptw_response_checker_checks_ready_handshake_snapshot() -> None:
    events: list[dict] = []
    checker = _checker(cycle=17, ready=1, response=_response(), events=events)

    checker.on_clock_edge(17)

    assert checker.get_stats() == {
        "checked_count": 1,
        "error_count": 0,
        "last_error": None,
    }
    assert events == []


def test_ptw_response_checker_checks_blocked_response_snapshot() -> None:
    events: list[dict] = []
    checker = _checker(cycle=18, ready=0, response=_response(), events=events)

    checker.on_clock_edge(18)

    assert checker.get_stats()["checked_count"] == 1
    assert checker.get_stats()["error_count"] == 0


def test_ptw_response_checker_reports_payload_mismatch_as_error() -> None:
    events: list[dict] = []
    response = _response()
    response["s1_entry_ppn"] = 0x456
    checker = _checker(cycle=19, ready=1, response=response, events=events)

    with pytest.raises(AssertionError, match="PTW response input mismatch cycle=19 cursor=None"):
        checker.on_clock_edge(19)

    assert checker.get_stats()["checked_count"] == 1
    assert checker.get_stats()["error_count"] == 1
    assert checker.get_stats()["last_error"] == {
        "cycle": 19,
        "reason": "ptw_resp_input_mismatch",
        "details": {"s1_entry_ppn": {"expected": 0x456, "got": 0x123}},
    }
    assert events[0]["level"] == "ERROR"


def test_ptw_response_checker_catches_array_payload_mismatch() -> None:
    events: list[dict] = []
    response = _response()
    response["s1_ppn_low"] = [0, 0, 0, 7, 0, 0, 0, 0]
    checker = _checker(cycle=20, ready=0, response=response, events=events)

    with pytest.raises(AssertionError, match="PTW response input mismatch cycle=20"):
        checker.on_clock_edge(20)

    assert checker.get_stats()["last_error"]["details"] == {
        "s1_ppn_low[3]": {"expected": 7, "got": 0}
    }


@pytest.mark.parametrize("checker_name", ["ptw_full_ppn_checker", "ptw_resp_input_checker"])
def test_ptw_checker_requires_current_monitor_snapshot(checker_name: str) -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    checker = getattr(env, checker_name)

    with pytest.raises(AssertionError, match="current FrontendMonitor snapshot"):
        checker.on_clock_edge(21)

    env.monitor.capture_cycle_observation(20)
    with pytest.raises(AssertionError, match="current FrontendMonitor snapshot"):
        checker.on_clock_edge(21)


@pytest.mark.parametrize("ready", [0, 1])
def test_ptw_agent_response_is_checked_after_handshake_decision(ready: int) -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    env.ptw_agent.configure(latency=1, response_source="model")
    env.ptw_if.req_0_valid.value = 1
    env.ptw_if.req_0_bits_vpn.value = 0x80001
    env.ptw_agent.on_clock_edge(10, env.monitor.capture_ptw_agent_inputs(10))

    env.ptw_if.req_0_valid.value = 0
    env.ptw_if.resp_ready.value = ready
    env.ptw_agent.on_clock_edge(11, env.monitor.capture_ptw_agent_inputs(11))
    assert (env.ptw_agent.active_resp is None) == bool(ready)
    assert env.ptw_agent.get_last_drive_expectation() is not None

    env.monitor.capture_cycle_observation(11)
    env.ptw_resp_input_checker.on_clock_edge(11)

    assert env.ptw_resp_input_checker.get_stats()["checked_count"] == 1
    assert env.ptw_resp_input_checker.get_stats()["error_count"] == 0


def test_ptw_agent_uses_monitored_request_and_context() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    env.ptw_agent.configure(latency=2, response_source="model")
    env.monitor.ptw_get_gpa_provider = lambda: 1
    env.ptw_if.req_0_valid.value = 1
    env.ptw_if.req_0_bits_vpn.value = 0x80001
    env.csr_ctrl_if.io_tlbCsr_priv_imode.value = 1

    sampled = env.monitor.capture_ptw_agent_inputs(10)
    env.ptw_if.req_0_valid.value = 0
    env.ptw_if.req_0_bits_vpn.value = 0
    env.csr_ctrl_if.io_tlbCsr_priv_imode.value = 3
    env.monitor.ptw_get_gpa_provider = lambda: 0
    env.ptw_agent.on_clock_edge(10, sampled)

    assert env.ptw_agent.get_last_request_expectation() == {
        "cycle": 10, "vpn": 0x80001, "s2xlate": 0, "get_gpa": 1,
    }
    assert env.ptw_agent.pending[0].snapshot.priv_imode == 1
    with pytest.raises(AssertionError, match="stale input snapshot"):
        env.ptw_agent.on_clock_edge(11, sampled)


def test_ptw_agent_uses_monitored_response_ready_and_sfence() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    env.ptw_agent.configure(latency=1, response_source="model")
    env.ptw_if.req_0_valid.value = 1
    env.ptw_if.req_0_bits_vpn.value = 0x80001
    env.ptw_agent.on_clock_edge(10, env.monitor.capture_ptw_agent_inputs(10))

    env.ptw_if.req_0_valid.value = 0
    env.ptw_if.resp_ready.value = 0
    sampled = env.monitor.capture_ptw_agent_inputs(11)
    env.ptw_if.resp_ready.value = 1
    env.ptw_agent.on_clock_edge(11, sampled)
    assert env.ptw_agent.active_resp is not None

    env.ptw_if.sfence_valid.value = 1
    sampled = env.monitor.capture_ptw_agent_inputs(12)
    env.ptw_if.sfence_valid.value = 0
    env.ptw_agent.on_clock_edge(12, sampled)
    assert env.ptw_agent.active_resp is None
    assert env.ptw_agent.sfence_dropped_responses == 1


def test_ptw_nemu_sync_uses_monitored_csr_before_request_context(monkeypatch) -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    env.ptw_agent.response_source = "nemu"
    monkeypatch.setattr(env.ptw_agent, "_build_response", lambda _request: {})
    synced = []

    def sync(**request) -> None:
        synced.append(request["ptw_context"]["satp_mode"])
        env._nemu_satp_override = 0x1234

    env.ptw_agent.set_nemu_sync_hook(sync)
    env.ptw_if.req_0_valid.value = 1
    env.ptw_if.req_0_bits_vpn.value = 0x80001
    env.csr_ctrl_if.io_tlbCsr_satp_mode.value = 8
    sampled = env.monitor.capture_ptw_agent_inputs(10)
    env.csr_ctrl_if.io_tlbCsr_satp_mode.value = 0
    env.ptw_agent.on_clock_edge(10, sampled)

    assert synced == [8]
    assert env.ptw_agent.pending[0].snapshot.extra["satp"] == 0x1234


def test_ptw_full_ppn_checker_uses_observed_response_value() -> None:
    dut = FakeDUTFrontend()
    env = FrontendEnv(dut, register_callbacks=False)
    dut.io_ptw_resp_valid.value = 1
    dut.io_ptw_resp_bits_s1_entry_ppn.value = 0x123
    dut.io_ptw_resp_bits_s1_ppn_low_3.value = 5
    env.ptw_agent.last_drive_expectation = {
        "resp": {
            "s1_entry_v": 1,
            "s1_entry_level": 0,
            "s1_entry_ppn": 0x123,
            "s1_addr_low": 3,
            "s1_ppn_low": [0, 0, 0, 5, 0, 0, 0, 0],
        },
    }

    env.monitor.capture_cycle_observation(20)
    dut.io_ptw_resp_bits_s1_ppn_low_3.value = 0
    env.ptw_full_ppn_checker.on_clock_edge(20)
    assert env.ptw_full_ppn_checker.get_stats()["error_count"] == 0

    env.monitor.capture_cycle_observation(21)
    with pytest.raises(AssertionError, match="PTW full PPN mismatch cycle=21"):
        env.ptw_full_ppn_checker.on_clock_edge(21)
    assert env.ptw_full_ppn_checker.get_stats()["last_error"]["details"] == {
        "full_ppn": 0x123 << 3,
        "expected_full_ppn": (0x123 << 3) | 5,
    }
