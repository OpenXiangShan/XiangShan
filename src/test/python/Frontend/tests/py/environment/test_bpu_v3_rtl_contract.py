from __future__ import annotations

from dataclasses import replace
import re
from pathlib import Path

import pytest

from env.runtime.pylib import frontend_offset_path
from env.funcov.py.icache.icache_waylookup_funcov import (
    _SIGNALS as ICACHE_WAYLOOKUP_SIGNALS,
)
from env.support.bpu_v3_contract import (
    BPU_TARGET_DIFF_SIGNAL_GROUPS,
    BPU_V3_SIGNAL_GROUPS,
    BTB_TARGET_COMPARE_WIDTH,
    MBTB_WRITE_BUFFER_SIGNAL_GROUPS,
    PREFETCH_DEPTH,
    PREFETCH_DEPTH_SIGNAL_GROUPS,
    BpuTargetDiffCycle,
    BpuV3SignalUnavailable,
    MbtbWriteEntry,
    read_mbtb_write_buffer_dirty,
    sample_bpu_target_diff_cycle,
    sample_bpu_v3_cycle,
    sample_mbtb_write_buffer_events,
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
    required = {
        **BPU_V3_SIGNAL_GROUPS,
        **PREFETCH_DEPTH_SIGNAL_GROUPS,
        **BPU_TARGET_DIFF_SIGNAL_GROUPS,
        **MBTB_WRITE_BUFFER_SIGNAL_GROUPS,
    }
    missing = {
        key: list(candidates)
        for key, candidates in required.items()
        if not any(name in registered for name in candidates)
    }
    assert not missing, {"missing_bpu_v3_signal_groups": missing}


def _target_diff_cycle(**overrides: int) -> BpuTargetDiffCycle:
    defaults = {
        "s3_valid": 1,
        "s3_taken": 1,
        "s1_taken": 1,
        "s1_cfi_position": 3,
        "s1_branch_type": 2,
        "s1_ras_action": 0,
        "s1_target": 0,
        "s3_cfi_position": 3,
        "s3_branch_type": 2,
        "s3_ras_action": 0,
        "s3_target": 0,
        "ittage_hit": 0,
        "s3_override": 0,
        "signal_paths": {},
    }
    defaults.update(overrides)
    return BpuTargetDiffCycle(**defaults)


def _mbtb_header_values(*, use_last_alias: bool = False) -> dict[str, int]:
    values: dict[str, int] = {}
    for key, candidates in MBTB_WRITE_BUFFER_SIGNAL_GROUPS.items():
        if key.endswith(("_write_valid", "_hit_written", "_hit_not_written")):
            values[candidates[-1 if use_last_alias else 0]] = 0
    return values


def _set_group_value(
    values: dict[str, int],
    groups: dict[str, tuple[str, ...]],
    key: str,
    value: int,
    *,
    use_last_alias: bool = False,
) -> None:
    values[groups[key][-1 if use_last_alias else 0]] = int(value)


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


def test_bpu_target_diff_canary_fails_closed_when_selected_target_is_missing() -> None:
    values = {
        candidates[0]: 0
        for key, candidates in BPU_TARGET_DIFF_SIGNAL_GROUPS.items()
        if key != "s3_target"
    }

    with pytest.raises(BpuV3SignalUnavailable) as exc_info:
        sample_bpu_target_diff_cycle(values.get)

    assert exc_info.value.key == "s3_target"
    assert exc_info.value.candidates == BPU_TARGET_DIFF_SIGNAL_GROUPS["s3_target"]


def test_bpu_target_diff_canary_records_actual_alias_paths() -> None:
    values = {
        candidates[-1]: 0 for candidates in BPU_TARGET_DIFF_SIGNAL_GROUPS.values()
    }
    values[BPU_TARGET_DIFF_SIGNAL_GROUPS["s3_valid"][-1]] = 1

    sample = sample_bpu_target_diff_cycle(values.get)

    assert sample.s3_valid == 1
    assert sample.signal_paths == {
        key: candidates[-1]
        for key, candidates in BPU_TARGET_DIFF_SIGNAL_GROUPS.items()
    }


def test_bpu_target_diff_uses_lower_bits_only_for_btb() -> None:
    high_only = _target_diff_cycle(s3_target=1 << BTB_TARGET_COMPARE_WIDTH)
    lower = _target_diff_cycle(s3_target=1)

    assert high_only.target_only_candidate
    assert high_only.target_source == "btb"
    assert high_only.full_target_diff
    assert high_only.btb_high_only_diff
    assert not high_only.target_diff
    assert lower.target_source == "btb"
    assert lower.target_diff


def test_bpu_target_diff_keeps_full_target_for_ittage_and_ras() -> None:
    high_bit = 1 << BTB_TARGET_COMPARE_WIDTH
    ittage = _target_diff_cycle(
        s1_branch_type=3,
        s3_branch_type=3,
        ittage_hit=1,
        s3_target=high_bit,
    )
    ras = replace(ittage, s1_ras_action=1, s3_ras_action=1)
    pop_and_push = replace(ittage, s1_ras_action=3, s3_ras_action=3)

    assert ittage.target_source == "ittage"
    assert ittage.target_diff
    assert ras.target_source == "ras"
    assert ras.target_diff
    assert pop_and_push.target_source == "btb"
    assert pop_and_push.btb_high_only_diff
    assert not pop_and_push.target_diff


def test_mbtb_compare_bits_ignore_identity_but_include_prediction_semantics() -> None:
    stored = MbtbWriteEntry(
        set_idx=3,
        tag=0x1234,
        branch_type=2,
        ras_action=0,
        position=7,
        target_carry=1,
        target_lower=0x34567,
    )
    identity_only = replace(stored, set_idx=4, tag=0x5678, position=9)
    changed_target = replace(stored, target_lower=stored.target_lower ^ 1)
    changed_attribute = replace(stored, ras_action=2)

    assert identity_only.identity != stored.identity
    assert identity_only.compare_bits == stored.compare_bits
    assert changed_target.compare_bits != stored.compare_bits
    assert changed_attribute.compare_bits != stored.compare_bits


def test_mbtb_write_buffer_canary_fails_closed_when_hit_probe_is_missing() -> None:
    values = _mbtb_header_values()
    missing_key = "mbtb_a0_i0_p0_hit_written"
    values.pop(MBTB_WRITE_BUFFER_SIGNAL_GROUPS[missing_key][0])

    with pytest.raises(BpuV3SignalUnavailable) as exc_info:
        sample_mbtb_write_buffer_events(values.get)

    assert exc_info.value.key == missing_key
    assert exc_info.value.candidates == MBTB_WRITE_BUFFER_SIGNAL_GROUPS[missing_key]


def test_mbtb_write_buffer_canary_reads_real_hit_entry_and_aliases() -> None:
    values = _mbtb_header_values(use_last_alias=True)
    groups = MBTB_WRITE_BUFFER_SIGNAL_GROUPS
    set_value = lambda key, value: _set_group_value(
        values,
        groups,
        key,
        value,
        use_last_alias=True,
    )
    set_value("mbtb_a0_i0_p0_write_valid", 1)
    set_value("mbtb_a0_i0_p0_hit_written", 1)
    for row in range(4):
        set_value(f"mbtb_a0_i0_p0_hit_row_{row}", int(row == 2))
        set_value(f"mbtb_a0_i0_p0_hit_index_{row}", int(row == 2))

    fields = {
        "setIdx": 3,
        "entry_tag": 0x1234,
        "entry_attribute_branchType": 2,
        "entry_attribute_rasAction": 0,
        "entry_position": 7,
        "entry_targetCarry_value": 1,
        "entry_targetLowerBits": 0x34567,
    }
    for field, value in fields.items():
        set_value(f"mbtb_a0_i0_p0_input_{field}", value)
        set_value(f"mbtb_a0_i0_r2_e1_{field}", value)
    set_value("mbtb_a0_i0_r2_e1_dirty", 0)

    (event,) = sample_mbtb_write_buffer_events(values.get)
    dirty, dirty_path = read_mbtb_write_buffer_dirty(values.get, event)

    assert (event.align_bank, event.internal_bank, event.port) == (0, 0, 0)
    assert (event.row, event.entry, event.dirty) == (2, 1, 0)
    assert event.identity_matches
    assert not event.semantic_changed
    assert dirty == 0
    assert dirty_path == groups["mbtb_a0_i0_r2_e1_dirty"][-1]
    assert all(path in values for path in event.signal_paths.values())


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


def test_not_cfi_taken_redirect_is_eligible_for_training() -> None:
    source = _read(_FRONTEND / "ifu/Ifu.scala")

    assert (
        "checkerRedirect.bits.invalidTaken || checkerRedirect.bits.notCfiTaken"
        in source
    )
    assert "b.bits.canTrain  := canTrain" in source


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
    bundles = _read(_FRONTEND / "bpu/Bundles.scala")
    parameters = _read(_FRONTEND / "bpu/Parameters.scala")
    ubtb_parameters = _read(_FRONTEND / "bpu/ubtb/Parameters.scala")
    abtb_parameters = _read(_FRONTEND / "bpu/abtb/Parameters.scala")
    mbtb_parameters = _read(_FRONTEND / "bpu/mbtb/Parameters.scala")

    assert "_.bits.targetLower =/= s3_s1Prediction.targetLower" in source
    assert "ittage.io.prediction.target =/= s3_s1Prediction.target" in source
    assert "ras.io.topRetAddr =/= s3_s1Prediction.target" in source
    assert "(s3_taken && s3_useRas)    -> s3_rasTargetDiff" in source
    assert "(s3_taken && s3_useIttage) -> s3_ittageTargetDiff" in source
    assert "s3_taken                   -> Mux1H(s3_firstTakenBranchOH, s3_mbtbTargetDiffVec)" in source
    assert (
        "target(MaxBtbTargetWidth + instOffsetBits - 1, instOffsetBits)"
        in bundles
    )
    assert "def MaxBtbTargetWidth: Int = Seq(" in parameters
    assert re.search(rf"TargetWidth:\s+Int\s*=\s*{BTB_TARGET_COMPARE_WIDTH}\b", ubtb_parameters)
    assert re.search(rf"TargetWidth:\s+Int\s*=\s*{BTB_TARGET_COMPARE_WIDTH}\b", abtb_parameters)
    assert re.search(r"EnableTargetFix:\s+Boolean\s*=\s*true\b", mbtb_parameters)
    assert re.search(r"TargetWidth:\s+Int\s*=\s*20\b", mbtb_parameters)
