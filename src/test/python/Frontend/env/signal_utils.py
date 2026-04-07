from typing import Any


def has_sig(dut: Any, name: str) -> bool:
    return hasattr(dut, name)


def get_sig(dut: Any, name: str, default: int = 0) -> int:
    pin = getattr(dut, name, None)
    if pin is None:
        return default
    try:
        return int(pin.value)
    except Exception:
        return default


def set_sig(dut: Any, name: str, value: int) -> bool:
    pin = getattr(dut, name, None)
    if pin is None:
        return False
    pin.value = int(value)
    return True
