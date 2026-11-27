from __future__ import annotations

from types import SimpleNamespace

import pytest

from env.agents.uncache_agent import UncacheAgent
from env.model import MemoryModel
from env.monitors.frontend_monitor import FrontendMonitor


class _Signal:
    def __init__(self, value: int = 0) -> None:
        self.value = int(value)


def _interface(*, mm: int = 0, nc: int = 0) -> SimpleNamespace:
    return SimpleNamespace(
        a_ready=_Signal(1),
        a_valid=_Signal(1),
        a_bits_address=_Signal(0x80000000),
        a_bits_user_mem_back_type_mm=_Signal(mm),
        a_bits_user_mem_page_type_nc=_Signal(nc),
        d_valid=_Signal(),
        d_bits_source=_Signal(),
        d_bits_denied=_Signal(),
        d_bits_data=_Signal(),
        d_bits_corrupt=_Signal(),
    )


def _memory() -> MemoryModel:
    memory = MemoryModel()
    memory.load_bin(bytes(range(32)), 0x80000000)
    return memory


def test_uncache_agent_returns_one_eight_byte_beat_and_records_attributes() -> None:
    interface = _interface(mm=1, nc=1)
    agent = UncacheAgent(_memory())
    agent.interface = interface
    monitor = FrontendMonitor()
    monitor.bind_uncache(interface)
    agent.configure(latency=0)

    agent.on_clock_edge(10, monitor.capture_uncache_request(10))

    assert int(interface.d_bits_data.value) == int.from_bytes(bytes(range(8)), "little")
    assert agent.get_stats()["response_bytes"] == 8
    assert agent.get_stats()["request_records"] == [
        {
            "cycle": 10,
            "address": 0x80000000,
            "mem_back_type_mm": 1,
            "mem_page_type_nc": 1,
        }
    ]


@pytest.mark.parametrize("ready_before", (0, 1))
def test_uncache_agent_uses_monitored_request_snapshot(ready_before: int) -> None:
    interface = _interface(mm=1, nc=1)
    agent = UncacheAgent(_memory())
    agent.interface = interface
    monitor = FrontendMonitor()
    monitor.bind_uncache(interface)

    interface.a_ready.value = ready_before
    agent.set_a_ready(1 - ready_before)
    request = monitor.capture_uncache_request(10)
    interface.a_valid.value = 0
    interface.a_bits_address.value += 8
    interface.a_bits_user_mem_back_type_mm.value = 0
    interface.a_bits_user_mem_page_type_nc.value = 0
    agent.on_clock_edge(10, request)

    expected = [
        {"cycle": 10, "address": 0x80000000, "mem_back_type_mm": 1, "mem_page_type_nc": 1}
    ] if ready_before else []
    assert agent.request_records == expected
    assert interface.a_ready.value == 1 - ready_before
    with pytest.raises(AssertionError, match="stale request snapshot"):
        agent.on_clock_edge(11, request)
