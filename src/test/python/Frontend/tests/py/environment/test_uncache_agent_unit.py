from __future__ import annotations

from types import SimpleNamespace

from env.agents.uncache_agent import UncacheAgent
from env.model import MemoryModel


class _Signal:
    def __init__(self, value: int = 0) -> None:
        self.value = int(value)


def _interface(*, mm: int = 0, nc: int = 0) -> SimpleNamespace:
    return SimpleNamespace(
        a_ready=_Signal(),
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
    agent.configure(latency=0)

    agent.on_clock_edge(10)

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

