from __future__ import annotations

import logging
import os
import random
import re
import sys
import tempfile
from datetime import datetime
from logging import getLogger
from pathlib import Path

import pytest
from toffee_test.reporter import set_line_coverage

from .pylib import frontend_pylib_path

_HERE = Path(__file__).resolve().parents[1]
_REPO_ROOT = _HERE.parents[3]


def _frontend_pylib_path() -> Path:
    return frontend_pylib_path()


_PYLIB_PATH = _frontend_pylib_path()

for _path in (str(_PYLIB_PATH), str(_HERE)):
    if _path not in sys.path:
        sys.path.insert(0, _path)

from .api import api_Frontend_load_program
from .artifact_provenance import file_sha256
from .dut_factory import create_frontend_dut, is_fake_frontend_dut
from .env_config import DEFAULT_ENV_CONFIG
from .functional_coverage import FunctionalCoverageRecorder, default_pilot_csv_path
from .frontend_env import FrontendEnv
from .logging_utils import configure_env_logging


logger = getLogger("env.fixtures")


def _data_dir() -> Path:
    p = Path(__file__).resolve().parents[1] / "data"
    p.mkdir(parents=True, exist_ok=True)
    return p


_DEFAULT_RUN_ID: str | None = None


def _effective_run_id() -> str:
    global _DEFAULT_RUN_ID
    explicit = os.getenv("TB_RUN_ID", "").strip()
    if explicit:
        return explicit
    if _DEFAULT_RUN_ID is None:
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S_%f")
        _DEFAULT_RUN_ID = f"frontend_pytest_{timestamp}_{os.getpid()}"
    return _DEFAULT_RUN_ID


def _funcov_dir() -> Path:
    raw = os.getenv("TB_FUNCOV_DIR", "").strip()
    if raw:
        p = Path(raw)
    else:
        artifact_root = os.getenv("TB_ARTIFACT_DIR", "").strip()
        if artifact_root:
            p = Path(artifact_root) / "funcov"
        else:
            p = _data_dir() / "runs" / _safe_path_component(_effective_run_id()) / "funcov"
    p.mkdir(parents=True, exist_ok=True)
    return p


def _safe_path_component(value: str) -> str:
    text = re.sub(r"[^A-Za-z0-9_.=-]+", "_", str(value).strip())
    return text.strip("._") or "run"


def _artifact_root_dir(request, default_dir: Path) -> Path:
    raw = os.getenv("TB_ARTIFACT_DIR", "").strip()
    if raw:
        root = Path(raw)
    else:
        root = default_dir / "runs" / _safe_path_component(_effective_run_id())
    root.mkdir(parents=True, exist_ok=True)
    return root


def _split_env_list(raw: str | None) -> list[str]:
    result: list[str] = []
    seen: set[str] = set()
    for token in re.split(r"[\s,;]+", str(raw or "").strip()):
        if not token or token in seen:
            continue
        seen.add(token)
        result.append(token)
    return result


def _flatten_marker_values(values) -> list[str]:
    if isinstance(values, (str, bytes)):
        values = (values,)
    result: list[str] = []
    for value in values:
        if isinstance(value, (list, tuple, set)):
            result.extend(_flatten_marker_values(value))
        else:
            result.extend(_split_env_list(str(value)))
    return result


def _funcov_targets(request) -> dict[str, list[str]]:
    bin_ids = _split_env_list(os.getenv("TB_FUNCOV_TARGET_BINS", ""))
    tp_ids = _split_env_list(os.getenv("TB_FUNCOV_TARGET_TP_IDS", ""))
    testcases = _split_env_list(os.getenv("TB_FUNCOV_TARGET_TESTCASES", ""))
    node = getattr(request, "node", None)
    if node is not None:
        testcases.extend(
            str(value).strip()
            for value in (getattr(node, "originalname", None), getattr(node, "name", None))
            if str(value or "").strip()
        )
        for marker in node.iter_markers("funcov_bins"):
            bin_ids.extend(_flatten_marker_values(marker.args))
            bin_ids.extend(_flatten_marker_values(marker.kwargs.get("bins", [])))
        for marker in node.iter_markers("funcov_tps"):
            tp_ids.extend(_flatten_marker_values(marker.args))
            tp_ids.extend(_flatten_marker_values(marker.kwargs.get("tps", [])))
    raw_bin = os.getenv("TB_BIN_PATH", "").strip()
    if raw_bin:
        testcases.append(Path(raw_bin).stem)
    return {
        "bin_ids": _split_env_list(" ".join(bin_ids)),
        "tp_ids": _split_env_list(" ".join(tp_ids)),
        "testcases": _split_env_list(" ".join(testcases)),
    }


def _is_enabled(name: str, default: str = "1") -> bool:
    raw = os.getenv(name, default).strip().lower()
    return raw not in {"0", "false", "off", "no"}


def _read_int_env(name: str, default: str) -> int:
    raw = os.getenv(name, default).strip()
    try:
        return int(raw, 0)
    except ValueError as exc:
        raise AssertionError(f"{name} must be a valid integer, got: {raw}") from exc


def _test_seed() -> int:
    return _read_int_env("TB_SEED", "1")


def _input_path_metadata(env_name: str) -> tuple[str | None, str]:
    raw = os.getenv(env_name, "").strip()
    if not raw:
        return None, "unavailable"
    path = Path(raw).resolve()
    return str(path), file_sha256(path)


def _artifact_tag(request) -> str:
    tc_name = request.node.name if request is not None else "frontend"
    raw_bin = os.getenv("TB_BIN_PATH", "").strip()
    if not raw_bin:
        return tc_name
    return f"{Path(raw_bin).stem}_{tc_name}"


def _normalize_waveform_format(value: str | None) -> str:
    normalized = "" if value is None else str(value).strip().lower()
    return normalized if normalized in {"fst", "vcd", "fsdb"} else "fst"


def _waveform_format_from_dut(dut) -> str:
    if dut is None or not hasattr(dut, "GetWaveFormat"):
        return "fst"
    try:
        return _normalize_waveform_format(dut.GetWaveFormat())
    except Exception:
        return "fst"


def _waveform_path(request, default_dir: Path, *, waveform_format: str | None = None) -> Path:
    tag = _artifact_tag(request)
    raw = os.getenv("TB_WAVEFORM_PATH", "").strip()
    if raw:
        path = Path(raw.format(tc=tag))
        path.parent.mkdir(parents=True, exist_ok=True)
        return path
    wave_dir = Path(os.getenv("TB_WAVEFORM_DIR", str(_artifact_root_dir(request, default_dir))))
    wave_dir.mkdir(parents=True, exist_ok=True)
    return wave_dir / f"{tag}.{_normalize_waveform_format(waveform_format)}"


def _coverage_path(request, default_dir: Path) -> Path:
    coverage_dir = Path(os.getenv("TB_COVERAGE_DIR", str(_artifact_root_dir(request, default_dir))))
    coverage_dir.mkdir(parents=True, exist_ok=True)
    return coverage_dir / f"{_artifact_tag(request)}.dat"


def _coverage_ignore_path() -> str | None:
    raw = os.getenv("TB_LINE_COVERAGE_IGNORE", "").strip()
    if raw:
        return raw
    path = _HERE / "Frontend.ignore"
    return str(path) if path.is_file() else None


def _coverage_omit_path() -> Path | None:
    raw = os.getenv("TB_LINE_COVERAGE_OMIT", "").strip()
    path = Path(raw) if raw else _HERE / "Frontend.omit"
    return path if path.is_file() else None


def _funcov_run_metadata(request, env) -> dict:
    report = getattr(getattr(request, "node", None), "rep_call", None)
    outcome = str(getattr(report, "outcome", "unknown") or "unknown").lower()
    exit_code = 0 if outcome == "passed" else 1
    try:
        errors = list(env.get_errors())
    except Exception:
        errors = [{"kind": "checker_error_collection_failed"}]
    node = getattr(request, "node", None)
    node_path_raw = getattr(node, "path", None)
    testcase_path = Path(str(node_path_raw)).resolve() if node_path_raw is not None else None
    bin_path, bin_sha256 = _input_path_metadata("TB_BIN_PATH")
    trace_path, trace_sha256 = _input_path_metadata("TB_TRACE_PATH")
    asm_path, asm_sha256 = _input_path_metadata("TB_ASM_PATH")
    seed = _test_seed()
    backend_seed = _read_int_env("TB_BACKEND_RANDOM_SEED", str(seed))
    config = getattr(env, "config", DEFAULT_ENV_CONFIG)
    nodeid = str(getattr(node, "nodeid", "") or "").strip()
    run_command = os.getenv("TB_RUN_COMMAND", "").strip() or f"pytest {nodeid}".strip()
    run_id = _effective_run_id()
    artifact_root_raw = os.getenv("TB_ARTIFACT_DIR", "").strip()
    artifact_root = (
        Path(artifact_root_raw)
        if artifact_root_raw
        else _data_dir() / "runs" / _safe_path_component(run_id)
    ).resolve()
    case_log_path = str(
        getattr(getattr(env, "dut", None), "_frontend_case_log_path", "") or ""
    ).strip()
    return {
        "outcome": outcome,
        "exit_code": exit_code,
        "checker": {
            "status": "pass" if not errors and outcome == "passed" else "fail",
            "error_count": len(errors),
            "errors": errors[:32],
        },
        "run_id": run_id,
        "execution": {
            "testcase_nodeid": nodeid,
            "testcase_path": None if testcase_path is None else str(testcase_path),
            "testcase_sha256": (
                "unavailable" if testcase_path is None else file_sha256(testcase_path)
            ),
            "asm_path": asm_path,
            "asm_sha256": asm_sha256,
            "bin_path": bin_path,
            "bin_sha256": bin_sha256,
            "trace_path": trace_path,
            "trace_sha256": trace_sha256,
            "run_command": run_command,
            "artifact_root": str(artifact_root),
            "case_log_path": case_log_path or None,
            "seed": seed,
            "seeds": {
                "test": seed,
                "backend": backend_seed,
                "icache": int(config.icache.seed),
                "ptw": int(config.ptw.seed),
            },
        },
    }


def _line_ranges(lines: list[int]) -> list[str]:
    if not lines:
        return []
    ranges = []
    start = prev = lines[0]
    for line in lines[1:]:
        if line == prev + 1:
            prev = line
            continue
        ranges.append(str(start) if start == prev else f"{start}-{prev}")
        start = prev = line
    ranges.append(str(start) if start == prev else f"{start}-{prev}")
    return ranges


def _expanded_coverage_ignore_path() -> str | None:
    ignore = _coverage_ignore_path()
    omit = _coverage_omit_path()
    if omit is None:
        return ignore

    patterns = [
        line.strip()
        for line in omit.read_text(encoding="utf-8").splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    ]
    if not patterns:
        return ignore

    regexes = [re.compile(pattern) for pattern in patterns]
    lines = []
    if ignore:
        lines.extend(Path(ignore).read_text(encoding="utf-8").splitlines())
    for source in sorted((_REPO_ROOT / "build-frontend" / "rtl").glob("*.sv")):
        matched = []
        for lineno, text in enumerate(source.read_text(errors="ignore").splitlines(), 1):
            if any(regex.search(text) for regex in regexes):
                matched.append(lineno)
        ranges = _line_ranges(matched)
        if ranges:
            lines.append(f"*/build-frontend/rtl/{source.name}:{','.join(ranges)}")

    tmp = tempfile.NamedTemporaryFile(
        "w", encoding="utf-8", suffix=".ignore", prefix="frontend-line-coverage-", delete=False
    )
    with tmp:
        tmp.write("\n".join(line for line in lines if line))
        tmp.write("\n")
    return tmp.name


def _log_path(request, default_dir: Path) -> Path:
    tag = _artifact_tag(request)
    raw = os.getenv("TB_CASE_LOG_PATH", "").strip()
    if raw:
        path = Path(raw.format(tc=tag))
        path.parent.mkdir(parents=True, exist_ok=True)
        return path
    log_dir = Path(os.getenv("TB_CASE_LOG_DIR", "").strip() or str(_artifact_root_dir(request, default_dir)))
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
    if _is_enabled("TB_ENABLE_CASE_LOG", default="1"):
        case_log_path = _log_path(request, data_dir)
        case_log_handler = _attach_case_log_handler(case_log_path)
    dut = create_frontend_dut(tc_name=tc_name, dut_logger=logger)
    if (
        request is not None
        and _is_enabled("TB_ENABLE_DUT_TESTS", default="0")
        and is_fake_frontend_dut(dut)
    ):
        pytest.skip(
            "compiled Frontend DUT not found; run `make frontend-verilator` "
            "or `make frontend-vcs` to build the selected "
            "build-frontend/pylib-<sim>/Frontend package before enabling "
            "TB_ENABLE_DUT_TESTS=1"
        )
    waveform_format = _waveform_format_from_dut(dut)

    waveform = _waveform_path(request, data_dir, waveform_format=waveform_format)
    coverage = _coverage_path(request, data_dir)
    try:
        if _is_enabled("TB_ENABLE_FST_DUMP", default="1"):
            waveform.parent.mkdir(parents=True, exist_ok=True)
            dut.SetWaveform(str(waveform))
        if _is_enabled("TB_ENABLE_DUT_COVERAGE", default="1"):
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


def _suppress_dut_finalizer_for_batch_run(dut) -> None:
    """Keep VCS from calling $finish when pytest releases a function-scoped DUT."""
    if not _is_enabled("TB_SKIP_DUT_FINISH", default="0"):
        return
    dut_type = type(dut)
    if getattr(dut_type, "_frontend_batch_finalizer_suppressed", False):
        return
    if "__del__" not in vars(dut_type):
        return
    dut_type.__del__ = lambda _dut: None
    dut_type._frontend_batch_finalizer_suppressed = True


@pytest.fixture(scope="function")
def dut(request):
    dut = create_dut(request)
    _suppress_dut_finalizer_for_batch_run(dut)
    coverage = _coverage_path(request, _data_dir())
    dut.InitClock("clock")
    yield dut
    try:
        if hasattr(dut, "FlushWaveform"):
            dut.FlushWaveform()
    except Exception:
        logger.exception("dut waveform flush failed")
    if _is_enabled("TB_ENABLE_TOFFEE_LINE_COVERAGE", default="1") and coverage.is_file():
        ignore = _expanded_coverage_ignore_path()
        set_line_coverage(request, str(coverage), ignore=ignore)
    handler = getattr(dut, "_frontend_case_log_handler", None)
    if handler is not None:
        try:
            logging.getLogger("env").removeHandler(handler)
            handler.flush()
            handler.close()
        except Exception:
            logger.exception("dut case log handler teardown failed")
    if not _is_enabled("TB_SKIP_DUT_FINISH", default="0"):
        dut.Finish()


@pytest.fixture(scope="function")
def env(dut, request):
    configure_env_logging()
    random.seed(_test_seed())
    data_dir = _data_dir()
    funcov_dir = _funcov_dir()
    tag = _artifact_tag(request)
    waveform = _waveform_path(request, data_dir, waveform_format=_waveform_format_from_dut(dut))
    coverage = _coverage_path(request, data_dir)
    recorder = None
    if _is_enabled("TB_ENABLE_FUNCTIONAL_COVERAGE", default="1"):
        targets = _funcov_targets(request)
        recorder = FunctionalCoverageRecorder.from_pilot_csv(
            default_pilot_csv_path(),
            testcase_name=request.node.name if request is not None else "frontend",
            artifact_tag=tag,
            output_dir=funcov_dir,
            waveform_path=waveform,
            line_coverage_path=coverage,
            target_bin_ids=targets["bin_ids"],
            target_tp_ids=targets["tp_ids"],
            target_testcases=targets["testcases"],
        )
    tb = FrontendEnv(dut, event_sink=None if recorder is None else recorder.handle_event, config=DEFAULT_ENV_CONFIG)
    tb.waveform_path = str(waveform)
    tb.line_coverage_path = str(coverage)
    tb.functional_coverage = recorder
    if recorder is not None:
        recorder.attach(tb)
        dut.StepRis(lambda cycle: recorder.on_cycle(cycle, tb))
    tb.initialize(
        reset_vector=_read_int_env("TB_RESET_VECTOR", "0x80000000"),
        bare_mode=True,
        reset_cycles=20,
    )
    yield tb
    if recorder is not None:
        metadata = _funcov_run_metadata(request, tb)
        metadata["execution"]["funcov_path"] = str(recorder.raw_path().resolve())
        recorder.set_run_metadata(
            outcome=metadata["outcome"],
            exit_code=metadata["exit_code"],
            checker=metadata["checker"],
            run_id=metadata["run_id"],
            extra=metadata["execution"],
        )
        recorder.write_artifacts()


@pytest.fixture(scope="function")
def full_env(env):
    program = [0x00000013] * 64
    api_Frontend_load_program(env, program, 0x80000000)
    return env
