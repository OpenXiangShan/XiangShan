from __future__ import annotations

from types import SimpleNamespace

import pytest

from env.agents.icache_control_agent import ICacheControlAgent
from env.monitors.frontend_monitor import FrontendMonitor


class _Signal:
    def __init__(self, value: int = 0) -> None:
        self.value = int(value)


def _interface() -> SimpleNamespace:
    return SimpleNamespace(
        a_ready=_Signal(),
        a_valid=_Signal(),
        a_bits_opcode=_Signal(),
        a_bits_size=_Signal(),
        a_bits_source=_Signal(),
        a_bits_address=_Signal(),
        a_bits_mask=_Signal(),
        a_bits_data=_Signal(),
        d_ready=_Signal(),
        d_valid=_Signal(),
        d_bits_opcode=_Signal(),
        d_bits_size=_Signal(),
        d_bits_source=_Signal(),
        d_bits_data=_Signal(),
    )


@pytest.mark.parametrize(
    "request_spec,match",
    [
        ({"opcode": 2, "address": 0x38022080}, "opcode"),
        ({"opcode": 4, "address": 0x38022080, "size": 2}, "size"),
        ({"opcode": 4, "address": 0x38022084}, "aligned"),
        ({"opcode": 4, "address": 0x38022000}, "address"),
        ({"opcode": 4, "address": 0x38022080, "source": 32}, "source"),
        ({"opcode": 0, "address": 0x38022080, "mask": 0x0F}, "mask"),
        ({"opcode": 1, "address": 0x38022080, "mask": 0}, "mask"),
    ],
)
def test_icache_control_agent_rejects_illegal_requests(request_spec: dict, match: str) -> None:
    agent = ICacheControlAgent()

    with pytest.raises(ValueError, match=match):
        agent.request(**request_spec)


def test_icache_control_agent_holds_request_until_handshake_and_checks_read_response() -> None:
    interface = _interface()
    agent = ICacheControlAgent()
    agent.interface = interface
    monitor = FrontendMonitor()
    monitor.bind_icache_control(interface)
    agent.request(opcode=4, address=0x38022080, source=7)

    with pytest.raises(RuntimeError, match="outstanding"):
        agent.request(opcode=4, address=0x38022088)

    agent.on_clock_edge(10, monitor.capture_icache_control(10))
    assert int(interface.a_valid.value) == 1
    assert int(interface.a_bits_opcode.value) == 4
    assert int(interface.a_bits_size.value) == 3
    assert int(interface.a_bits_source.value) == 7
    assert int(interface.a_bits_address.value) == 0x38022080
    assert int(interface.a_bits_mask.value) == 0xFF
    assert agent.get_stats()["accepted_request_count"] == 0

    interface.a_ready.value = 1
    agent.on_clock_edge(11, monitor.capture_icache_control(11))
    assert agent.get_stats()["accepted_request_count"] == 1

    interface.d_valid.value = 1
    interface.d_bits_opcode.value = 1
    interface.d_bits_size.value = 3
    interface.d_bits_source.value = 7
    interface.d_bits_data.value = 0x1234
    snapshot = monitor.capture_icache_control(12)
    interface.d_bits_data.value = 0
    agent.on_clock_edge(12, snapshot)

    stats = agent.get_stats()
    assert stats["response_count"] == 1
    assert stats["outstanding"] is False
    assert stats["responses"] == [
        {
            "cycle": 12,
            "opcode": 1,
            "size": 3,
            "source": 7,
            "data": 0x1234,
        }
    ]


def test_icache_control_agent_rejects_mismatched_or_unsolicited_response() -> None:
    interface = _interface()
    agent = ICacheControlAgent()
    agent.interface = interface
    monitor = FrontendMonitor()
    monitor.bind_icache_control(interface)
    interface.d_valid.value = 1

    with pytest.raises(AssertionError, match="without an outstanding request"):
        agent.on_clock_edge(1, monitor.capture_icache_control(1))

    interface.d_valid.value = 0
    interface.a_ready.value = 1
    agent.request(opcode=0, address=0x38022088, source=3, data=0xAA)
    agent.on_clock_edge(2, monitor.capture_icache_control(2))
    agent.on_clock_edge(3, monitor.capture_icache_control(3))
    interface.d_valid.value = 1
    interface.d_bits_opcode.value = 1
    interface.d_bits_size.value = 3
    interface.d_bits_source.value = 3

    with pytest.raises(AssertionError, match="opcode"):
        agent.on_clock_edge(4, monitor.capture_icache_control(4))

    with pytest.raises(AssertionError, match="stale snapshot"):
        agent.on_clock_edge(5, monitor.icache_control_snapshot)
