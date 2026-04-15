from __future__ import annotations

import logging
import os
import sys
from datetime import datetime
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
from .dut_factory import create_frontend_dut
from .env_config import DEFAULT_ENV_CONFIG
from .frontend_env import FrontendEnv
from .logging_utils import configure_env_logging


logger = getLogger("env.fixtures")


def _data_dir() -> Path:
    p = Path(__file__).resolve().parents[1] / "data"
    p.mkdir(parents=True, exist_ok=True)
    return p


def _artifact_root_dir(request, default_dir: Path) -> Path:
    raw_bin = os.getenv("TB_BIN_PATH", "").strip()
    if not raw_bin:
        return default_dir
    date_dir = default_dir / datetime.now().strftime("%Y%m%d")
    date_dir.mkdir(parents=True, exist_ok=True)
    return date_dir


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
        path = Path(raw.format(tc=tag))
        path.parent.mkdir(parents=True, exist_ok=True)
        return path
    wave_dir = Path(os.getenv("TB_WAVEFORM_DIR", str(_artifact_root_dir(request, default_dir))))
    wave_dir.mkdir(parents=True, exist_ok=True)
    return wave_dir / f"{tag}.fst"


def _coverage_path(request, default_dir: Path) -> Path:
    return default_dir / f"{_artifact_tag(request)}.dat"


def _log_path(request, default_dir: Path) -> Path:
    tag = _artifact_tag(request)
    raw = os.getenv("TB_CASE_LOG_PATH", "").strip()
    if raw:
        path = Path(raw.format(tc=tag))
        path.parent.mkdir(parents=True, exist_ok=True)
        return path
    log_dir = _artifact_root_dir(request, default_dir)
    log_dir.mkdir(parents=True, exist_ok=True)
    return log_dir / f"{tag}.log"


def _attach_case_log_handler(path: Path) -> logging.Handler:
    handler = logging.FileHandler(str(path), mode="w", encoding="utf-8")
    handler.setLevel(logging.DEBUG)
    handler.setFormatter(
        logging.Formatter(
            "%(asctime)s %(levelname)-8s %(name)s:%(filename)s:%(lineno)d %(message)s"
        )
    )
    env_logger = logging.getLogger("env")
    env_logger.addHandler(handler)
    return handler


def create_dut(request):
    configure_env_logging()
    tc_name = request.node.name if request is not None else "frontend"
    data_dir = _data_dir()
    case_log_handler = None
    case_log_path = None
    if os.getenv("TB_BIN_PATH", "").strip():
        case_log_path = _log_path(request, data_dir)
        case_log_handler = _attach_case_log_handler(case_log_path)
    dut = create_frontend_dut(tc_name=tc_name, dut_logger=logger)

    waveform = _waveform_path(request, data_dir)
    coverage = _coverage_path(request, data_dir)
    try:
        if _is_enabled("TB_ENABLE_FST_DUMP", default="1"):
            waveform.parent.mkdir(parents=True, exist_ok=True)
            dut.SetWaveform(str(waveform))
        dut.SetCoverage(str(coverage))
        logger.info(
            "dut created: tc=%s waveform=%s coverage=%s case_log=%s",
            tc_name,
            waveform,
            coverage,
            case_log_path,
        )
    except Exception:
        logger.exception("dut setup waveform/coverage failed: tc=%s", tc_name)

    dut.reset.value = 1
    dut.clock.value = 0
    setattr(dut, "_frontend_case_log_handler", case_log_handler)
    setattr(dut, "_frontend_case_log_path", None if case_log_path is None else str(case_log_path))
    return dut


@pytest.fixture(scope="function")
def dut(request):
    dut = create_dut(request)
    dut.InitClock("clock")
    groups = get_coverage_groups(dut)
    if groups:
        dut.StepRis(lambda _: [g.sample() for g in groups])
    yield dut
    try:
        if hasattr(dut, "FlushWaveform"):
            dut.FlushWaveform()
    except Exception:
        logger.exception("dut waveform flush failed")
    for g in groups:
        try:
            g.clear()
        except Exception:
            pass
    handler = getattr(dut, "_frontend_case_log_handler", None)
    if handler is not None:
        try:
            logging.getLogger("env").removeHandler(handler)
            handler.flush()
            handler.close()
        except Exception:
            logger.exception("dut case log handler teardown failed")
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
