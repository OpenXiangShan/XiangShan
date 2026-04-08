from __future__ import annotations

from toffee import Bundle
from toffee.bundle import DummySignal, Signal, SignalList

from .backend_ctrl import BackendCtrlBundle
from .backend_from_ftq import BackendFromFtqBundle
from .backend_observe import BackendObserveBundle
from .clock_reset import ClockResetBundle
from .csr_control import CSRControlBundle
from .frontend_info import FrontendInfoBundle
from .icache import ICacheBundle
from .ptw import PTWBundle
from .uncache import UncacheBundle


def _dut_pin(dut, signal_name: str):
    if dut is None:
        return DummySignal()
    pin = getattr(dut, str(signal_name), None)
    return DummySignal() if pin is None else pin


def bind_bundle_optional(bundle_type: type[Bundle], dut):
    """Bind a bundle onto any DUT-like object with best-effort optional pins."""
    bundle = bundle_type()
    signal_bindings = getattr(bundle_type, "SIGNAL_BINDINGS", {})

    for attr_name in dir(bundle):
        if attr_name.startswith("_"):
            continue
        attr_value = getattr(bundle, attr_name)
        if isinstance(attr_value, Signal):
            setattr(bundle, attr_name, _dut_pin(dut, signal_bindings.get(attr_name, attr_name)))
            continue
        if isinstance(attr_value, SignalList):
            for idx, signal_name in enumerate(attr_value.names):
                attr_value.signals[idx] = _dut_pin(dut, signal_name)
            continue
        if isinstance(attr_value, list):
            for item in attr_value:
                if isinstance(item, SignalList):
                    for idx, signal_name in enumerate(item.names):
                        item.signals[idx] = _dut_pin(dut, signal_name)
    return bundle


__all__ = [
    "BackendCtrlBundle",
    "BackendFromFtqBundle",
    "BackendObserveBundle",
    "CSRControlBundle",
    "ClockResetBundle",
    "FrontendInfoBundle",
    "ICacheBundle",
    "PTWBundle",
    "UncacheBundle",
    "bind_bundle_optional",
]
