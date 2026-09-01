from __future__ import annotations

import re
from pathlib import Path

import pytest

from env.runtime.pylib import frontend_offset_path
from env.funcov.py.icache.icache_waylookup_funcov import (
    _SIGNALS as ICACHE_WAYLOOKUP_SIGNALS,
)
from env.support.bpu_v3_contract import (
    BPU_V3_SIGNAL_GROUPS,
    PREFETCH_DEPTH,
    PREFETCH_DEPTH_SIGNAL_GROUPS,
    BpuV3SignalUnavailable,
    sample_bpu_v3_cycle,
    sample_prefetch_depth_cycle,
)


_REPO_ROOT = Path(__file__).resolve().parents[7]
_FRONTEND = _REPO_ROOT / "src/main/scala/xiangshan/frontend"


def _read(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def _registered_signal_names() -> set[str]:
    offset = frontend_offset_path()
    assert offset.is_file(), "compile Frontend before running BPU V3 signal-contract tests"
    return {
        line[len("  - name: ") :].strip()
        for line in offset.read_text(encoding="utf-8").splitlines()
        if line.startswith("  - name: ")
    }


def test_bpu_v3_negative_canary_signal_contract_matches_dut_inventory() -> None:
    registered = _registered_signal_names()
    required = {**BPU_V3_SIGNAL_GROUPS, **PREFETCH_DEPTH_SIGNAL_GROUPS}
    missing = {
        key: list(candidates)
        for key, candidates in required.items()
        if not any(name in registered for name in candidates)
    }
    assert not missing, {"missing_bpu_v3_signal_groups": missing}


def test_bpu_v3_negative_canary_fails_closed_when_override_probe_is_missing() -> None:
    values = {
        candidates[0]: 0
        for key, candidates in BPU_V3_SIGNAL_GROUPS.items()
        if key != "s3_override"
    }
    values[BPU_V3_SIGNAL_GROUPS["s3_valid"][0]] = 1

    with pytest.raises(BpuV3SignalUnavailable) as exc_info:
        sample_bpu_v3_cycle(values.get)

    assert exc_info.value.key == "s3_override"
    assert exc_info.value.candidates == BPU_V3_SIGNAL_GROUPS["s3_override"]


def test_bpu_v3_negative_canary_records_the_actual_alias_path() -> None:
    values = {candidates[-1]: 0 for candidates in BPU_V3_SIGNAL_GROUPS.values()}
    values[BPU_V3_SIGNAL_GROUPS["s3_valid"][-1]] = 1

    sample = sample_bpu_v3_cycle(values.get)

    assert sample.is_all_not_taken_candidate
    assert sample.s3_override == 0
    assert sample.signal_paths == {
        key: candidates[-1] for key, candidates in BPU_V3_SIGNAL_GROUPS.items()
    }


def test_prefetch_depth_canary_fails_closed_when_occupancy_probe_is_missing() -> None:
    values = {
        candidates[0]: 0
        for key, candidates in PREFETCH_DEPTH_SIGNAL_GROUPS.items()
        if key != "num_valid_entries"
    }

    with pytest.raises(BpuV3SignalUnavailable) as exc_info:
        sample_prefetch_depth_cycle(values.get)

    assert exc_info.value.key == "num_valid_entries"
    assert exc_info.value.candidates == PREFETCH_DEPTH_SIGNAL_GROUPS["num_valid_entries"]


def test_prefetch_depth_canary_fails_closed_when_shared_ready_probe_is_missing() -> None:
    values = {
        candidates[0]: 0
        for key, candidates in PREFETCH_DEPTH_SIGNAL_GROUPS.items()
        if key != "shared_write_ready"
    }

    with pytest.raises(BpuV3SignalUnavailable) as exc_info:
        sample_prefetch_depth_cycle(values.get)

    assert exc_info.value.key == "shared_write_ready"
    assert (
        exc_info.value.candidates
        == PREFETCH_DEPTH_SIGNAL_GROUPS["shared_write_ready"]
    )


def test_prefetch_depth_canary_preserves_pointer_flags_and_actual_paths() -> None:
    values = {
        candidates[-1]: 0 for candidates in PREFETCH_DEPTH_SIGNAL_GROUPS.values()
    }
    values[PREFETCH_DEPTH_SIGNAL_GROUPS["num_valid_entries"][-1]] = PREFETCH_DEPTH
    values[PREFETCH_DEPTH_SIGNAL_GROUPS["read_ptr_flag"][-1]] = 1
    values[PREFETCH_DEPTH_SIGNAL_GROUPS["read_ptr_value"][-1]] = 31
    values[PREFETCH_DEPTH_SIGNAL_GROUPS["write_ptr_flag"][-1]] = 0
    values[PREFETCH_DEPTH_SIGNAL_GROUPS["write_ptr_value"][-1]] = 31

    sample = sample_prefetch_depth_cycle(values.get)

    assert sample.full
    assert not sample.one_slot_left
    assert sample.read_ptr == (1, 31)
    assert sample.write_ptr == (0, 31)
    assert sample.signal_paths == {
        key: candidates[-1]
        for key, candidates in PREFETCH_DEPTH_SIGNAL_GROUPS.items()
    }


def test_prefetch_depth_dual_write_uses_the_rtl_shared_ready() -> None:
    values = {
        candidates[0]: 0 for candidates in PREFETCH_DEPTH_SIGNAL_GROUPS.values()
    }
    values[PREFETCH_DEPTH_SIGNAL_GROUPS["write0_valid"][0]] = 1
    values[PREFETCH_DEPTH_SIGNAL_GROUPS["write1_valid"][0]] = 1
    values[PREFETCH_DEPTH_SIGNAL_GROUPS["shared_write_ready"][0]] = 1

    assert sample_prefetch_depth_cycle(values.get).dual_write_fire

    values[PREFETCH_DEPTH_SIGNAL_GROUPS["shared_write_ready"][0]] = 0
    assert not sample_prefetch_depth_cycle(values.get).dual_write_fire


def test_existing_waylookup_producer_uses_the_rtl_shared_ready() -> None:
    assert ICACHE_WAYLOOKUP_SIGNALS["write0_ready"] == (
        PREFETCH_DEPTH_SIGNAL_GROUPS["shared_write_ready"]
    )
    assert ICACHE_WAYLOOKUP_SIGNALS["write1_ready"] == (
        PREFETCH_DEPTH_SIGNAL_GROUPS["shared_write_ready"]
    )


def test_existing_waylookup_producer_uses_the_current_top_fencei_alias() -> None:
    registered = _registered_signal_names()
    candidates = ICACHE_WAYLOOKUP_SIGNALS["fencei"]

    assert candidates == (
        "Frontend_top.io_fencei",
        "Frontend_top.__Vtogcov__io_fencei",
    )
    assert any(name in registered for name in candidates)


def test_bpu_all_not_taken_differences_are_guarded_by_s3_taken() -> None:
    source = _read(_FRONTEND / "bpu/Bpu.scala")

    assert "val cfiPositionDiff = s3_taken &&" in source
    assert "val attributeDiff   = s3_taken &&" in source
    assert "false.B, // fall-through" in source
    assert "(s3_taken && s3_useRas)" in source
    assert "(s3_taken && s3_useIttage)" in source
    assert "s3_taken                   -> Mux1H" in source


def test_prefetch_depth_is_shared_by_ftq_and_waylookup() -> None:
    parameters = _read(_FRONTEND / "FrontendParameters.scala")
    ftq = _read(_FRONTEND / "ftq/Ftq.scala")
    waylookup = _read(_FRONTEND / "icache/ICacheWayLookup.scala")
    all_frontend_scala = "\n".join(
        _read(path) for path in sorted(_FRONTEND.rglob("*.scala"))
    )

    assert re.search(rf"PrefetchDepth:\s+Int\s*=\s*{PREFETCH_DEPTH}\b", parameters)
    assert "distanceBetween(bpuPtr(0), fetchPtr(0)) < PrefetchDepth.U" in ftq
    assert "CircularQueuePtr[ICacheWayLookupPtr](PrefetchDepth)" in waylookup
    assert "Seq.fill(PrefetchDepth)" in waylookup
    assert "PrefetchDepth.U - numValidEntries" in waylookup
    assert "BpRunAheadDistance" not in all_frontend_scala


def test_mbtb_write_buffer_compare_bits_exclude_nonprediction_fields() -> None:
    source = _read(_FRONTEND / "bpu/mbtb/Bundles.scala")
    match = re.search(
        r"override def compareBits: Option\[UInt\] = \{(?P<body>.*?)\n  \}",
        source,
        re.S,
    )
    assert match is not None
    body = match.group("body")

    assert "entry.attribute.asUInt" in body
    assert "entry.targetLowerBits" in body
    assert "entry.targetCarry" in body
    assert "entry.tag" not in body
    assert "entry.position" not in body
    assert "setIdx" not in body


def test_bpu_target_diff_splits_btb_lower_from_ittage_and_ras_full_target() -> None:
    source = _read(_FRONTEND / "bpu/Bpu.scala")

    assert "_.bits.targetLower =/= s3_s1Prediction.targetLower" in source
    assert "ittage.io.prediction.target =/= s3_s1Prediction.target" in source
    assert "ras.io.topRetAddr =/= s3_s1Prediction.target" in source
    assert "(s3_taken && s3_useRas)    -> s3_rasTargetDiff" in source
    assert "(s3_taken && s3_useIttage) -> s3_ittageTargetDiff" in source
    assert "s3_taken                   -> Mux1H(s3_firstTakenBranchOH, s3_mbtbTargetDiffVec)" in source
