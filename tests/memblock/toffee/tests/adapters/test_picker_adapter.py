from __future__ import annotations

from dataclasses import dataclass
from typing import Any

from memblock_toffee.adapters import MemBlockDutAdapter
from memblock_toffee.adapters.memblock import LOGICAL_PINS


class FakeXData:
    Imme = "immediate"

    def __init__(self) -> None:
        self.value = 0
        self.write_mode = None

    def SetWriteMode(self, mode: str) -> None:
        self.write_mode = mode


class FakeRawDut:
    def __init__(self, names: set[str]) -> None:
        self.event = object()
        self.clock_name = None
        self.signals = {name: FakeXData() for name in names}

    def InitClock(self, name: str) -> None:
        self.clock_name = name

    def __getitem__(self, name: str) -> FakeXData:
        return self.signals[name]


@dataclass
class FakePort:
    name: str
    direction: str = "input"
    width: int = 1
    input_policy: str | None = "zero"
    protocol: str = "signal"
    role: str = "value"
    group: str = "test"


def test_adapter_owns_generated_names_and_input_defaults(monkeypatch: Any) -> None:
    ports = [
        FakePort("clock", input_policy="clock", protocol="clock", role="clock"),
        FakePort("reset", input_policy="reset_active_high", protocol="reset"),
        FakePort("forced_one", input_policy="one"),
        FakePort("io_outer_l2_flush_en", direction="output"),
    ]
    monkeypatch.setattr(
        "memblock_toffee.adapters.memblock.load_port_manifest",
        lambda _: {port.name: port for port in ports},
    )
    raw = FakeRawDut({port.name for port in ports} | set(LOGICAL_PINS.values()))
    adapter = MemBlockDutAdapter(raw)
    assert raw.clock_name == "clock"
    assert raw["reset"].value == 0
    assert raw["forced_one"].value == 1
    assert raw["forced_one"].write_mode == FakeXData.Imme
    assert adapter.port_count == 4
