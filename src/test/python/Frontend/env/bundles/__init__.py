from __future__ import annotations

from toffee import Bundle
from toffee.bundle import DummySignal, Signal, SignalList

from .backend_ctrl import BackendCtrlBundle
from .backend_from_ftq import BackendFromFtqBundle
from .backend_observe import BackendObserveBundle
from .clock_reset import ClockResetBundle
from .csr_control import CSRControlBundle
from .dft_control import DFTControlBundle
from .frontend_info import FrontendInfoBundle
from .icache import ICacheBundle
from .ptw import PTWBundle
from .uncache import UncacheBundle


def _dut_pin_optional(dut, signal_name: str):
    if dut is None:
        return DummySignal()
    pin = getattr(dut, str(signal_name), None)
    return DummySignal() if pin is None else pin


def _dut_pin_required(dut, signal_name: str, bundle_type: type[Bundle]):
    bundle_name = bundle_type.__name__
    if dut is None:
        raise AttributeError(f"{bundle_name} requires DUT pin '{signal_name}', but dut is None")
    try:
        pin = getattr(dut, str(signal_name))
    except AttributeError as exc:
        raise AttributeError(f"{bundle_name} requires DUT pin '{signal_name}'") from exc
    if pin is None:
        raise AttributeError(f"{bundle_name} requires DUT pin '{signal_name}'")
    return pin


def _bind_bundle_member(value, dut, *, bundle_type: type[Bundle], required: bool) -> None:
    if isinstance(value, SignalList):
        for idx, signal_name in enumerate(value.names):
            value.signals[idx] = (
                _dut_pin_required(dut, signal_name, bundle_type)
                if required
                else _dut_pin_optional(dut, signal_name)
            )
        return
    if isinstance(value, list):
        for item in value:
            _bind_bundle_member(item, dut, bundle_type=bundle_type, required=required)


def _bind_bundle(bundle_type: type[Bundle], dut, *, required: bool):
    bundle = bundle_type()
    signal_bindings = getattr(bundle_type, "SIGNAL_BINDINGS", {})
    optional_attrs = set(getattr(bundle_type, "OPTIONAL_ATTRS", ()))

    for attr_name in dir(bundle):
        if attr_name.startswith("_"):
            continue
        attr_value = getattr(bundle, attr_name)
        if isinstance(attr_value, Signal):
            signal_name = signal_bindings.get(attr_name, attr_name)
            use_required = required and attr_name not in optional_attrs
            setattr(
                bundle,
                attr_name,
                _dut_pin_required(dut, signal_name, bundle_type)
                if use_required
                else _dut_pin_optional(dut, signal_name),
            )
            continue
        use_required = required and attr_name not in optional_attrs
        _bind_bundle_member(attr_value, dut, bundle_type=bundle_type, required=use_required)
    return bundle


def bind_bundle_required(bundle_type: type[Bundle], dut):
    """Bind a bundle onto a DUT-like object and fail if any required pin is missing."""
    return _bind_bundle(bundle_type, dut, required=True)


def bind_bundle_optional(bundle_type: type[Bundle], dut):
    """Bind a bundle onto any DUT-like object with best-effort optional pins."""
    return _bind_bundle(bundle_type, dut, required=False)


__all__ = [
    "BackendCtrlBundle",
    "BackendFromFtqBundle",
    "BackendObserveBundle",
    "CSRControlBundle",
    "ClockResetBundle",
    "DFTControlBundle",
    "FrontendInfoBundle",
    "ICacheBundle",
    "PTWBundle",
    "UncacheBundle",
    "bind_bundle_optional",
    "bind_bundle_required",
]
