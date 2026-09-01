from __future__ import annotations

import json
from pathlib import Path

from env.funcov.py.ftq.sampler import _TWO_FETCH_SIGNALS
from env.funcov.py.ifu.cacheable_pipeline_funcov import (
    _SIGNALS as _IFU_CACHEABLE_SIGNALS,
)
from env.runtime.pylib import frontend_offset_path


_REPO_ROOT = Path(__file__).resolve().parents[7]
_EXPECTED_IMPLEMENTATION = "1a32a9056d993233fa1bf3a394b16e8a762abf52"
_EXPECTED_DESIGN_BASELINE = "e5c70547f3a966accf20a4b065ec1d8e33443180"
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


def test_bin814_review_is_bound_to_the_current_dut_manifest() -> None:
    manifest_path = _REPO_ROOT / "build-frontend/frontend_build_manifest.verilator.json"
    manifest = json.loads(_read(manifest_path))

    assert manifest["implementation_sha"] == _EXPECTED_IMPLEMENTATION
    assert manifest["dut_source_sha"] == _EXPECTED_IMPLEMENTATION
    assert manifest["design_baseline_sha"] == _EXPECTED_DESIGN_BASELINE
    assert manifest["source_tree_dirty"] is False


def test_bin814_observation_contract_matches_current_dut_inventory() -> None:
    offset = frontend_offset_path()
    assert offset.is_file(), "compile Frontend before running BIN-814 contract tests"
    registered = {
        line[len("  - name: ") :].strip()
        for line in _read(offset).splitlines()
        if line.startswith("  - name: ")
    }
    required = {
        **{key: _IFU_CACHEABLE_SIGNALS[key] for key in _REQUIRED_IFU_KEYS},
        **{key: _TWO_FETCH_SIGNALS[key] for key in _REQUIRED_TWO_FETCH_KEYS},
    }
    missing = {
        key: list(candidates)
        for key, candidates in required.items()
        if not any(path in registered for path in candidates)
    }

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
