from __future__ import annotations

from types import SimpleNamespace

import pytest

from env.bundles.ptw import PTWBundle
from env.core.checkers import PTWRespInputChecker
from env.core.frontend_env import FrontendEnv
from env.runtime.dut_factory import FakeDUTFrontend


class _Signal:
    def __init__(self, value: int = 0) -> None:
        self.value = int(value)


def _ptw_interface() -> SimpleNamespace:
    interface = SimpleNamespace(**{
        name: _Signal() for name in PTWBundle.SIGNAL_BINDINGS if name.startswith("resp_bits_")
    })
    interface.resp_valid = _Signal(1)
    interface.resp_ready = _Signal(1)
    for name in ("s1_ppn_low", "s1_valididx", "s1_pteidx"):
        setattr(interface, f"resp_bits_{name}", [_Signal() for _ in range(8)])
    for name, value in _response().items():
        getattr(interface, f"resp_bits_{name}").value = value
    return interface


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


def _checker(*, ready: int, response: dict, events: list) -> PTWRespInputChecker:
    interface = _ptw_interface()
    interface.resp_ready.value = int(ready)
    env = SimpleNamespace(
        ptw_if=interface,
        ptw_agent=SimpleNamespace(
            # active_resp has already been consumed on a ready handshake.
            get_active_drive_expectation=lambda: None,
            get_last_drive_expectation=lambda: {"resp": dict(response)},
        ),
    )
    checker = PTWRespInputChecker()
    checker.bind_env(env)
    checker.set_event_sink(events.append)
    return checker


def test_ptw_response_checker_checks_ready_handshake_snapshot() -> None:
    events: list[dict] = []
    checker = _checker(ready=1, response=_response(), events=events)

    checker.on_clock_edge(17)

    assert checker.get_stats() == {
        "checked_count": 1,
        "error_count": 0,
        "last_error": None,
    }
    assert events == []


def test_ptw_response_checker_checks_blocked_response_snapshot() -> None:
    events: list[dict] = []
    checker = _checker(ready=0, response=_response(), events=events)

    checker.on_clock_edge(18)

    assert checker.get_stats()["checked_count"] == 1
    assert checker.get_stats()["error_count"] == 0


def test_ptw_response_checker_reports_payload_mismatch_as_error() -> None:
    events: list[dict] = []
    response = _response()
    response["s1_entry_ppn"] = 0x456
    checker = _checker(ready=1, response=response, events=events)

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
    checker = _checker(ready=0, response=response, events=events)

    with pytest.raises(AssertionError, match="PTW response input mismatch cycle=20"):
        checker.on_clock_edge(20)

    assert checker.get_stats()["last_error"]["details"] == {
        "s1_ppn_low[3]": {"expected": 7, "got": 0}
    }


@pytest.mark.parametrize("ready", [0, 1])
def test_ptw_agent_response_is_checked_after_handshake_decision(ready: int) -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    env.ptw_agent.configure(latency=1, response_source="model")
    env.ptw_if.req_0_valid.value = 1
    env.ptw_if.req_0_bits_vpn.value = 0x80001
    env.ptw_agent.on_clock_edge(10)

    env.ptw_if.req_0_valid.value = 0
    env.ptw_if.resp_ready.value = ready
    env.ptw_agent.on_clock_edge(11)
    assert (env.ptw_agent.active_resp is None) == bool(ready)
    assert env.ptw_agent.get_last_drive_expectation() is not None

    env.monitor.capture_cycle_observation(11)
    env.ptw_resp_input_checker.on_clock_edge(11)

    assert env.ptw_resp_input_checker.get_stats()["checked_count"] == 1
    assert env.ptw_resp_input_checker.get_stats()["error_count"] == 0


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
