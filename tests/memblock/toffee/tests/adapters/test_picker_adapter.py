from __future__ import annotations

import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import pytest

from memblock_toffee.adapters import MemBlockDutAdapter, load_dut_class
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
    ports_by_name = {
        "clock": FakePort(
            "clock",
            input_policy="clock",
            protocol="clock",
            role="clock",
        ),
        "reset": FakePort(
            "reset",
            input_policy="reset_active_high",
            protocol="reset",
        ),
        "forced_one": FakePort("forced_one", input_policy="one"),
    }
    for generated_name in LOGICAL_PINS.values():
        ports_by_name.setdefault(generated_name, FakePort(generated_name, direction="output"))
    ports = list(ports_by_name.values())
    monkeypatch.setattr(
        "memblock_toffee.adapters.memblock.load_port_manifest",
        lambda _: ports_by_name,
    )
    raw = FakeRawDut(set(ports_by_name))
    adapter = MemBlockDutAdapter(raw)
    assert raw.clock_name == "clock"
    assert raw["reset"].value == 0
    assert raw["forced_one"].value == 1
    assert raw["forced_one"].write_mode == FakeXData.Imme
    assert adapter.port_count == len(ports_by_name)
    raw[LOGICAL_PINS["outer_l2_flush_enable"]].value = 1
    assert adapter.sample("outer_l2_flush_enable") == 1
    with pytest.raises(ValueError, match="not drivable"):
        adapter.drive("outer_l2_flush_enable", 0)
    with pytest.raises(ValueError, match="does not fit"):
        adapter.drive("reset", 2)


def test_dut_loader_rejects_a_module_from_another_runtime(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    package_name = "MemBlockRuntimeIdentityProbe"
    first_root = tmp_path / "first"
    first_package = first_root / package_name
    first_package.mkdir(parents=True)
    (first_package / "__init__.py").write_text(
        "class Probe:\n    pass\n",
        encoding="utf-8",
    )
    second_root = tmp_path / "second"
    second_root.mkdir()
    monkeypatch.setenv("MEMBLOCK_TOFFEE_DUT_PACKAGE", package_name)
    monkeypatch.setenv("MEMBLOCK_TOFFEE_DUT_CLASS", "Probe")
    monkeypatch.setattr(sys, "path", list(sys.path))
    sys.modules.pop(package_name, None)
    try:
        assert load_dut_class(first_root).__name__ == "Probe"
        with pytest.raises(RuntimeError, match="outside the selected runtime"):
            load_dut_class(second_root)
    finally:
        sys.modules.pop(package_name, None)
