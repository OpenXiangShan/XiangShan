from __future__ import annotations

from typing import Iterable, Optional


def _dut(recorder):
    return getattr(getattr(recorder, "env", None), "dut", None)


def _read(recorder, name: str, default: int = 0) -> int:
    dut = _dut(recorder)
    if dut is None:
        return int(default)
    return recorder._read_dut_signal(dut, name, default)


def _read_first(recorder, names: Iterable[str]) -> Optional[int]:
    dut = _dut(recorder)
    if dut is None:
        return None
    return recorder._read_first_dut_signal(dut, names)
