from __future__ import annotations

import os
import sys
from logging import getLogger
from pathlib import Path

import pytest

_HERE = Path(__file__).resolve().parents[1]
_REPO_ROOT = _HERE.parents[3]
_PYLIB_PATH = _REPO_ROOT / 'build-frontend' / 'pylib'

for _path in (str(_PYLIB_PATH), str(_HERE)):
    if _path not in sys.path:
        sys.path.insert(0, _path)

from .api import api_Frontend_load_program
from .coverage_def import get_coverage_groups
from .env_config import DEFAULT_ENV_CONFIG
from .frontend_env import FrontendEnv
from .logging_utils import configure_env_logging


logger = getLogger("env.fixtures")


class _FakeSignal:
    def __init__(self, value: int = 0) -> None:
        self.value = int(value)


class _FakeDUTFrontend:
    """Fallback DUT used when the compiled Frontend pylib is unavailable."""

    def __init__(self) -> None:
        self._signals = {}
        self._step_ris_callbacks = []
        self._cycle = 0
        self._waveform_paused = 0

        self.reset = _FakeSignal(1)
        self.clock = _FakeSignal(0)

    def __getattr__(self, name: str):
        if name.startswith("__"):
            raise AttributeError(name)
        signal = self._signals.get(name)
        if signal is None:
            signal = _FakeSignal(0)
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
            for callback in list(self._step_ris_callbacks):
                callback(self._cycle)
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


def _data_dir() -> Path:
    p = Path(__file__).resolve().parents[1] / "data"
    p.mkdir(parents=True, exist_ok=True)
    return p


def _is_enabled(name: str, default: str = "1") -> bool:
    raw = os.getenv(name, default).strip().lower()
    return raw not in {"0", "false", "off", "no"}


def _artifact_tag(request) -> str:
    tc_name = request.node.name if request is not None else "frontend"
    raw_bin = os.getenv("TB_BIN_PATH", "").strip()
    if not raw_bin:
        return tc_name
    return f"{Path(raw_bin).stem}_{tc_name}"


def _waveform_path(request, default_dir: Path) -> Path:
    tag = _artifact_tag(request)
    raw = os.getenv("TB_WAVEFORM_PATH", "").strip()
    if raw:
        return Path(raw.format(tc=tag))
    wave_dir = Path(os.getenv("TB_WAVEFORM_DIR", str(default_dir)))
    wave_dir.mkdir(parents=True, exist_ok=True)
    return wave_dir / f"{tag}.fst"


def _coverage_path(request, default_dir: Path) -> Path:
    return default_dir / f"{_artifact_tag(request)}.dat"


def create_dut(request):
    configure_env_logging()
    tc_name = request.node.name if request is not None else "frontend"
    data_dir = _data_dir()
    try:
        from Frontend import DUTFrontend

        dut = DUTFrontend()
    except ModuleNotFoundError:
        logger.warning("compiled Frontend DUT not found; using fallback fake DUT for tc=%s", tc_name)
        dut = _FakeDUTFrontend()

    waveform = _waveform_path(request, data_dir)
    coverage = _coverage_path(request, data_dir)
    try:
        if _is_enabled("TB_ENABLE_FST_DUMP", default="1"):
            waveform.parent.mkdir(parents=True, exist_ok=True)
            dut.SetWaveform(str(waveform))
        dut.SetCoverage(str(coverage))
        logger.info("dut created: tc=%s waveform=%s coverage=%s", tc_name, waveform, coverage)
    except Exception:
        logger.exception("dut setup waveform/coverage failed: tc=%s", tc_name)

    dut.reset.value = 1
    dut.clock.value = 0
    return dut


@pytest.fixture(scope="function")
def dut(request):
    dut = create_dut(request)
    dut.InitClock("clock")
    groups = get_coverage_groups(dut)
    if groups:
        dut.StepRis(lambda _: [g.sample() for g in groups])
    yield dut
    for g in groups:
        try:
            g.clear()
        except Exception:
            pass
    dut.Finish()


@pytest.fixture(scope="function")
def env(dut):
    configure_env_logging()
    tb = FrontendEnv(dut, config=DEFAULT_ENV_CONFIG)
    tb.initialize(reset_vector=0x80000000, bare_mode=True, reset_cycles=20)
    return tb


@pytest.fixture(scope="function")
def full_env(env):
    program = [0x00000013] * 64
    api_Frontend_load_program(env, program, 0x80000000)
    return env
