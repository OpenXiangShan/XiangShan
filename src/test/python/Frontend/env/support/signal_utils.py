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
        value = getattr(pin, "value", None)
        return None if value is None else int(value)
    except Exception:
        return None


def set_sig(dut: Any, name: str, value: int) -> bool:
    pin = getattr(dut, name, None)
    if pin is None:
        return False
    pin.value = int(value)
    return True
