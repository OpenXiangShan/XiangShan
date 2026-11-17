from __future__ import annotations

from pathlib import Path

from env.funcov.py.ftq.sampler import _TWO_FETCH_SIGNALS
from env.funcov.py.ifu.cacheable_pipeline_funcov import (
    _SIGNALS as _IFU_CACHEABLE_SIGNALS,
    _native_signal_names,
)
from env.runtime.pylib import frontend_offset_path


_REPO_ROOT = Path(__file__).resolve().parents[7]
_REQUIRED_IFU_KEYS = (
    "req_valid",
    "req_ready",
    "s0_fire",
    "s0_flush",
    "s0_flush_bpu",
    "s1_valid",
    "backend_redirect",
    "ifu_backend_redirect",
    "wb_redirect",
    "bpu_s3_flush",
)
_REQUIRED_TWO_FETCH_KEYS = (
    "bpu_s3_flush_ptr_flag",
    "bpu_s3_flush_ptr_value",
)


def _read(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def _compact(source: str) -> str:
    return " ".join(source.split())


def _inventory_names(source: str) -> set[str]:
    return {
        line[len("  - name: ") :].strip().strip('"')
        for line in source.splitlines()
        if line.startswith("  - name: ")
    }


def _missing_signal_groups(registered: set[str]) -> dict[str, list[str]]:
    required = {
        **{key: _native_signal_names(_IFU_CACHEABLE_SIGNALS[key]) for key in _REQUIRED_IFU_KEYS},
        **{key: _TWO_FETCH_SIGNALS[key] for key in _REQUIRED_TWO_FETCH_KEYS},
    }
    return {
        key: list(candidates)
        for key, candidates in required.items()
        if not any(path in registered for path in candidates)
    }


def test_bin814_contract_reports_missing_native_groups() -> None:
    registered = _inventory_names('  - name: "inner_icache.mainPipe.io_toIfu_req_ready"\n')
    missing = _missing_signal_groups(registered)
    assert "req_ready" not in missing
    assert "req_valid" in missing
    assert all("__Vtogcov__" not in name for name in missing["req_valid"])


def test_bin814_observation_contract_matches_current_dut_inventory() -> None:
    offset = frontend_offset_path()
    assert offset.is_file(), "compile Frontend before running BIN-814 contract tests"
    registered = _inventory_names(_read(offset))
    missing = _missing_signal_groups(registered)

    assert not missing, {"missing_bin814_signal_groups": missing}


def test_matching_bpu_s3_flush_suppresses_icache_response_before_ifu() -> None:
    mainpipe = _compact(
        _read(
            _REPO_ROOT
            / "src/main/scala/xiangshan/frontend/icache/ICacheMainPipe.scala"
        )
    )
    ifu = _compact(
        _read(_REPO_ROOT / "src/main/scala/xiangshan/frontend/ifu/Ifu.scala")
    )
    generated_mainpipe = _compact(
        _read(_REPO_ROOT / "build-frontend/rtl/ICacheMainPipe.sv")
    )

    assert (
        "s1_flush := io.flush || "
        "io.flushFromBpu.shouldFlushByStage3(s1_ftqIdx, s1_valid)"
    ) in mainpipe
    assert (
        "io.toIfu.req.valid := s1_valid && s1_fetchFinish && !s1_flush"
    ) in mainpipe
    assert "private val s0_valid = io.fromICache.req.valid" in ifu
    assert (
        "s0_flushFromBpu := "
        "fromFtq.flushFromBpu.shouldFlushByStage3(s0_fetchBlock(0).ftqIdx, s0_valid)"
    ) in ifu
    assert "assign io_toIfu_req_valid = _s1_fire_T & ~s1_flush;" in generated_mainpipe
