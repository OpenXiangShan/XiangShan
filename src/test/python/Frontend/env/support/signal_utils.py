from collections.abc import Sequence
from typing import Any, Optional


def has_sig(dut: Any, name: str) -> bool:
    return hasattr(dut, name)


def _lookup_pin(dut: Any, name: str) -> Any:
    pin = getattr(dut, name, None)
    if pin is None:
        getter = getattr(dut, "GetInternalSignal", None)
        if callable(getter):
            try:
                pin = getter(name)
            except Exception:
                pin = None
    return pin


def read_signal_value(pin: Any, *, wide: bool = False) -> Optional[int]:
    if pin is None:
        return None
    if wide:
        width = getattr(pin, "W", None)
        if callable(width) and int(width()) > 32:
            raw = pin.GetBytes()
            data = raw if isinstance(raw, (bytes, bytearray)) else bytes(raw)
            return int.from_bytes(data, byteorder="little", signed=False)
    value = getattr(pin, "value", None)
    return None if value is None else int(value)


def try_read_signal(dut: Any, name: str, *, wide: bool = False) -> Optional[int]:
    try:
        return read_signal_value(_lookup_pin(dut, name), wide=wide)
    except Exception:
        return None


def read_first_signal(
    dut: Any,
    names: Sequence[str],
    *,
    wide: bool = False,
    cache: dict | None = None,
) -> Optional[int]:
    """Read ordered aliases; an optional cache stores handles, not values."""
    aliases = tuple(str(name) for name in names)
    if cache is not None and aliases in cache:
        return read_signal_value(cache[aliases], wide=wide)
    for name in aliases:
        try:
            pin = _lookup_pin(dut, name)
            value = read_signal_value(pin, wide=wide)
            if value is not None:
                if cache is not None:
                    cache[aliases] = pin
                return value
        except Exception:
            continue
    if cache is not None:
        cache[aliases] = None
    return None


def get_sig(dut: Any, name: str, default: int = 0) -> int:
    pin = _lookup_pin(dut, name)
    if pin is None:
        return default
    try:
        return int(pin.value)
    except Exception:
        return default


def require_sig(dut: Any, name: str) -> int:
    pin = _lookup_pin(dut, name)
    if pin is None:
        raise AssertionError({"missing_dut_signal": name})
    try:
        value = pin.value
    except Exception as exc:
        raise AssertionError({"unreadable_dut_signal": name}) from exc
    if value is None:
        raise AssertionError({"unreadable_dut_signal": name})
    return int(value)


def read_internal_signal(dut: Any, name: str) -> Optional[int]:
    getter = getattr(dut, "GetInternalSignal", None)
    if not callable(getter):
        return None
    try:
        pin = getter(name)
        return read_signal_value(pin)
    except Exception:
        return None


def set_sig(dut: Any, name: str, value: int) -> bool:
    pin = getattr(dut, name, None)
    if pin is None:
        return False
    pin.value = int(value)
    return True
