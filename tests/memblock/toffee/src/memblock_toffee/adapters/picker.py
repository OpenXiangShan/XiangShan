from __future__ import annotations

from collections.abc import Mapping
from typing import Any


class ToffeePin:
    """Adapt Picker's raw XData objects to Toffee's signal protocol."""

    def __init__(self, xdata: Any, event: Any) -> None:
        self.xdata = xdata
        self.event = event

    @property
    def value(self) -> Any:
        return self.xdata.value

    @value.setter
    def value(self, value: Any) -> None:
        self.xdata.value = value

    def __getattr__(self, name: str) -> Any:
        return getattr(self.xdata, name)


class PickerDutAdapter:
    """Single generated-package adapter used by all Toffee-facing components."""

    def __init__(
        self,
        raw: Any,
        pin_map: Mapping[str, str],
        clock_pin: str,
    ) -> None:
        raw.InitClock(clock_pin)
        self._raw = raw
        self.event = raw.event
        self.pin_map = dict(pin_map)
        for logical_name, generated_name in pin_map.items():
            try:
                signal = raw[generated_name]
            except Exception as error:
                raise KeyError(
                    f"generated DUT has no pin {generated_name!r} for {logical_name!r}"
                ) from error
            setattr(self, logical_name, ToffeePin(signal, self.event))

    def Step(self, cycles: int = 1) -> Any:
        return self._raw.Step(cycles)

    def StepRis(
        self,
        callback: Any,
        args: tuple[Any, ...] = (),
        kwargs: dict[str, Any] | None = None,
    ) -> Any:
        return self._raw.StepRis(callback, args, kwargs or {})

    def StepFal(
        self,
        callback: Any,
        args: tuple[Any, ...] = (),
        kwargs: dict[str, Any] | None = None,
    ) -> Any:
        return self._raw.StepFal(callback, args, kwargs or {})

    def Finish(self) -> Any:
        return self._raw.Finish()

    def refresh_comb(self) -> Any:
        return self._raw.RefreshComb()

    def raw_signal(self, generated_name: str) -> Any:
        return self._raw[generated_name]
