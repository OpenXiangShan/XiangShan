from __future__ import annotations

import os
from logging import Logger, getLogger


logger = getLogger("env.dut_factory")


class FakeSignal:
    def __init__(self, value: int = 0) -> None:
        self.value = int(value)


class FakeDUTFrontend:
    """Fallback DUT used when the compiled Frontend pylib is unavailable."""

    def __init__(self) -> None:
        self._signals = {}
        self._step_ris_callbacks = []
        self._cycle = 0
        self._waveform_paused = 0
        self._frontend_is_fake_dut = True

        self.reset = FakeSignal(1)
        self.clock = FakeSignal(0)

    def __getattr__(self, name: str):
        if name.startswith("__"):
            raise AttributeError(name)
        signal = self._signals.get(name)
        if signal is None:
            signal = FakeSignal(0)
            self._signals[name] = signal
        return signal

    def SetWaveform(self, _path: str) -> None:
        return None

    def SetCoverage(self, _path: str) -> None:
        return None

    def InitClock(self, _name: str) -> None:
        return None

    def StepRis(self, callback) -> None:
        self._step_ris_callbacks.append(callback)

    def Step(self, cycles: int) -> int:
        for _ in range(int(cycles)):
            self._cycle += 1
            for step_callback in list(self._step_ris_callbacks):
                step_callback(self._cycle)
        return self._cycle

    def Finish(self) -> None:
        return None

    def ResumeWaveformDump(self) -> None:
        self._waveform_paused = 0

    def PauseWaveformDump(self) -> None:
        self._waveform_paused = 1

    def FlushWaveform(self) -> None:
        return None

    def WaveformPaused(self) -> int:
        return int(self._waveform_paused)


def create_frontend_dut(tc_name: str = "frontend", dut_logger: Logger | None = None):
    active_logger = dut_logger or logger
    try:
        from Frontend import DUTFrontend
    except ModuleNotFoundError as exc:
        if exc.name != "Frontend":
            raise
        if os.getenv("TB_ENABLE_DUT_TESTS") == "1":
            raise RuntimeError(
                "compiled Frontend DUT not found while TB_ENABLE_DUT_TESTS=1; "
                "run `make frontend` before DUT integration tests"
            ) from exc
        active_logger.warning("compiled Frontend DUT not found; using fallback fake DUT for tc=%s", tc_name)
        return FakeDUTFrontend()
    dut = DUTFrontend()
    setattr(dut, "_frontend_is_fake_dut", False)
    return dut
