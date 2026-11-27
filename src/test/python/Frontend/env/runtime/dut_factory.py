from __future__ import annotations

import os
from collections.abc import Iterable, Mapping
from logging import Logger, getLogger


logger = getLogger("env.dut_factory")


def _vcs_vector_reader(dut):
    from cffi import FFI

    ffi = FFI()
    ffi.cdef("""
        typedef struct { unsigned int aval; unsigned int bval; } s_vpi_vecval;
        typedef struct {
            int format;
            union {
                char *str;
                int integer;
                double real;
                s_vpi_vecval *vector;
                void *misc;
            } value;
        } s_vpi_value;
    """)
    get_value = ffi.cast(
        "void (*)(void *, s_vpi_value *)",
        dut.dut.GetVPIFuncPtr("vpi_get_value"),
    )

    def read(handle, width):
        value = ffi.new("s_vpi_value *")
        value.format = 9  # vpiVectorVal
        get_value(ffi.cast("void *", handle), value)
        # VPI may replace the vector pointer with simulator-owned storage.
        result = 0
        for index in range((width + 31) // 32):
            result |= int(value.value.vector[index].aval) << (index * 32)
        return result & ((1 << width) - 1)

    return read


class _VcsWidePin:
    def __init__(self, pin, read, handle):
        object.__setattr__(self, "_pin", pin)
        object.__setattr__(self, "_read", read)
        object.__setattr__(self, "_handle", handle)

    def __getattr__(self, name):
        return getattr(self._pin, name)

    def __setattr__(self, name, value):
        setattr(self._pin, name, value)

    @property
    def value(self):
        return self._read(self._handle, self._pin.W())

    def U(self):
        return self.value

    def S(self):
        width = self._pin.W()
        value = self.value
        return value - ((value >> (width - 1)) << width)

    def GetBytes(self):
        return self.value.to_bytes((self._pin.W() + 7) // 8, "little")

    def __getitem__(self, key):
        return self._pin[key]

    def __setitem__(self, key, value):
        self._pin[key] = value


def _install_vcs_vpi_name_adapter(dut) -> None:
    if os.getenv("TB_FRONTEND_SIM", "").strip().lower() != "vcs":
        return
    get_internal = getattr(dut, "GetInternalSignal", None)
    if not callable(get_internal):
        return

    wide_pins = {}
    read_vector = None

    def get_vcs_internal_signal(name, *args, **kwargs):
        nonlocal read_vector
        relative_name = str(name)
        if relative_name.startswith("TOP."):
            relative_name = relative_name[len("TOP.") :]
        if relative_name.startswith("Frontend_top."):
            relative_name = relative_name[len("Frontend_top.") :]
        relative_name = relative_name.replace("__Vtogcov__", "")
        pin = get_internal(relative_name, *args, **kwargs)
        width = getattr(pin, "W", None)
        if not callable(width) or width() <= 32 or not pin.IsVPIBinded():
            return pin
        native_name = str(pin.mName).removeprefix("VPI:")
        if native_name not in wide_pins:
            if read_vector is None:
                read_vector = _vcs_vector_reader(dut)
            wide_pins[native_name] = _VcsWidePin(
                pin, read_vector, dut.dut.GetVPIHandleObj(native_name)
            )
        return wide_pins[native_name]

    dut.GetInternalSignal = get_vcs_internal_signal


class FakeSignal:
    def __init__(self, value: int = 0) -> None:
        self.value = int(value)


class FakeDUTFrontend:
    """Permissive fallback DUT used by Python-only unit tests."""

    _is_fake_frontend_dut = True

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

    def GetWaveFormat(self) -> str:
        return "fst"

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


class StrictFakeDUTFrontend(FakeDUTFrontend):
    """DUT-shaped fake that rejects signals outside an explicit inventory."""

    _is_fake_frontend_dut = False

    def __init__(
        self,
        signal_names: Iterable[str],
        initial_values: Mapping[str, int] | None = None,
    ) -> None:
        super().__init__()
        self._frontend_is_fake_dut = False
        known_names = frozenset(str(name) for name in signal_names)
        values = {str(name): int(value) for name, value in (initial_values or {}).items()}
        unknown_values = sorted(set(values) - known_names)
        if unknown_values:
            raise ValueError(f"initial values reference unknown DUT signals: {unknown_values}")
        for name in known_names:
            setattr(self, name, FakeSignal(values.get(name, 0)))

    def __getattr__(self, name: str):
        if name.startswith("__"):
            raise AttributeError(name)
        raise AttributeError(f"unknown DUT signal: {name}")

    def GetInternalSignal(self, _name: str):
        return FakeSignal(0)


def create_frontend_dut(
    tc_name: str = "frontend",
    dut_logger: Logger | None = None,
    vcs_coverage_vdb: str | None = None,
    vcs_coverage_name: str | None = None,
):
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
    dut_args = []
    if (
        os.getenv("TB_FRONTEND_SIM", "").strip().lower() == "vcs"
        and vcs_coverage_vdb
        and vcs_coverage_name
    ):
        dut_args = [
            "Frontend",
            "-cm_dir",
            str(vcs_coverage_vdb),
            "-cm_name",
            str(vcs_coverage_name),
        ]
    dut = DUTFrontend(dut_args) if dut_args else DUTFrontend()
    _install_vcs_vpi_name_adapter(dut)
    setattr(dut, "_is_fake_frontend_dut", False)
    setattr(dut, "_frontend_is_fake_dut", False)
    return dut


def is_fake_frontend_dut(dut) -> bool:
    return bool(
        getattr(
            dut,
            "_is_fake_frontend_dut",
            getattr(dut, "_frontend_is_fake_dut", False),
        )
    )
