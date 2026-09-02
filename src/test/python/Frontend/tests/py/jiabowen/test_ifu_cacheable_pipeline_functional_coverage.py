from pathlib import Path
from types import SimpleNamespace

import pytest

from env.funcov.py.ifu.cacheable_pipeline_funcov import (
    _BIN907_SIGNALS,
    _BIN907_TIMEOUT_CYCLES,
    IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS,
    _LATE_FAULT_SIGNALS,
    _SIGNALS,
    _UPSTREAM_SIGNALS,
    _decode_branch_type,
    _expected_s1_semantic_slots,
    sample_ifu_cacheable_pipeline_coverage,
)
from env.funcov.recorder import FunctionalCoverageRecorder, default_pilot_csv_path


_ICACHE_PREFIX = "Frontend_top.Frontend.inner_icache."
_IFU_PREFIX = "Frontend_top.Frontend.inner_ifu."
_NOP_CACHE_LINE = sum(0x00000013 << (32 * index) for index in range(16))
_RVC_NOP_CACHE_LINE = sum(0x0001 << (16 * index) for index in range(32))


class _Signal:
    def __init__(self, value=0):
        self.value = int(value)


class _FakeDut:
    def set(self, name, value):
        signal = getattr(self, str(name), None)
        if signal is None:
            signal = _Signal()
            setattr(self, str(name), signal)
        signal.value = int(value)


def _make_recorder(tmp_path):
    dut = _FakeDut()
    env = SimpleNamespace(dut=dut)
    recorder = FunctionalCoverageRecorder.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="ifu_cacheable_pipeline_unit",
        artifact_tag="ifu_cacheable_pipeline_unit",
        output_dir=tmp_path,
    )
    recorder.attach(env)
    for candidates in _SIGNALS.values():
        dut.set(candidates[0], 0)
    for candidates in _UPSTREAM_SIGNALS.values():
        dut.set(candidates[0], 0)
    for candidates in _LATE_FAULT_SIGNALS.values():
        dut.set(candidates[0], 0)
    for candidates in _BIN907_SIGNALS.values():
        dut.set(candidates[0], 0)
    for index in range(32):
        dut.set(f"{_IFU_PREFIX}s1_instrEndMask_{index}", 0)
    for stem in (
        "s1_prevIBufEnqPtrDup_dup_0_value",
        "s1_prevIBufEnqPtrDup_dup_1_value",
        "s1_prevEndHalfRviInfo_valid",
        "s1_prevEndHalfRviInfo_bits_data",
        "s1_prevEndHalfRviInfo_bits_pc_addr",
    ):
        dut.set(f"{_IFU_PREFIX}{stem}", 0)
    return recorder, env, dut


def _set_request(
    dut,
    *,
    valid=1,
    ready=1,
    fire=1,
    flush=0,
    ftq0=(0, 3),
    start0=0x40000000,
    taken0=(0, 7),
    size0=8,
    range0=0xFF,
    data0=_NOP_CACHE_LINE,
    maybe_rvc_map=0,
    second=None,
):
    dut.set(_SIGNALS["req_valid"][0], valid)
    dut.set(_SIGNALS["req_ready"][0], ready)
    dut.set(_SIGNALS["s0_fire"][0], fire)
    dut.set(_SIGNALS["s0_flush"][0], flush)
    blocks = [
        {
            "valid": 1,
            "ftqIdx_flag": ftq0[0],
            "ftqIdx_value": ftq0[1],
            "startVAddr_addr": start0,
            "takenCfiOffset_valid": taken0[0],
            "takenCfiOffset_bits": taken0[1],
            "range": range0,
            "size": size0,
            "data": int(data0),
            "maybeRvcMap": int(maybe_rvc_map),
        },
        second
        or {
            "valid": 0,
        },
    ]
    for index, block in enumerate(blocks):
        for field, value in block.items():
            if field in {"data", "maybeRvcMap", "range"}:
                continue
            dut.set(
                f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_info_{index}_{field}",
                value,
            )
        if block.get("valid"):
            data = int(block["data"])
            for local_bank in range(8):
                bank = index * 8 + local_bank
                reg_suffix = "" if bank == 0 else f"_{bank}"
                data_index = 3 * bank + int(bank >= 8)
                valid_suffix = f"_{data_index + 1}"
                dut.set(f"{_ICACHE_PREFIX}mainPipe._s1_data_T{valid_suffix}", 0)
                dut.set(
                    f"{_ICACHE_PREFIX}mainPipe.s1_data_r{reg_suffix}",
                    (data >> (64 * local_bank)) & ((1 << 64) - 1),
                )
    first_range = int(blocks[0].get("range", 0))
    second_range = int(blocks[1].get("range", 0)) if blocks[1].get("valid") else 0
    dut.set(f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_firstRange", first_range)
    dut.set(
        f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_totalRange",
        first_range | second_range,
    )
    dut.set(
        f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_maybeRvcMap",
        int(blocks[0].get("maybeRvcMap", 0)),
    )
    return blocks


def _set_s1(dut, blocks, *, valid=1, ready=1, fire=None, req_is_uncache=0, exception=0):
    dut.set(_SIGNALS["s1_valid"][0], valid)
    dut.set(_SIGNALS["s1_ready"][0], ready)
    dut.set(_SIGNALS["s1_fire"][0], int(valid and ready) if fire is None else fire)
    dut.set(_SIGNALS["s1_req_uncache"][0], req_is_uncache)
    dut.set(_SIGNALS["s1_exception"][0], exception)
    for index, block in enumerate(blocks):
        for field, value in block.items():
            if field in {"data", "maybeRvcMap"}:
                continue
            if field == "range":
                continue
            dut.set(f"{_IFU_PREFIX}s1_fetchBlock_{index}_{field}", value)
        if block.get("valid"):
            stem = "s1_firstICacheData" if index == 0 else "s1_secondICacheData"
            dut.set(f"{_IFU_PREFIX}{stem}", block["data"])
    first_range = int(blocks[0].get("range", 0))
    second_range = int(blocks[1].get("range", 0)) if blocks[1].get("valid") else 0
    dut.set(f"{_IFU_PREFIX}s1_firstRange", first_range)
    dut.set(f"{_IFU_PREFIX}s1_totalRange", first_range | second_range)


def _idle_request(dut):
    dut.set(_SIGNALS["req_valid"][0], 0)
    dut.set(_SIGNALS["req_ready"][0], 1)
    dut.set(_SIGNALS["s0_fire"][0], 0)
    dut.set(_SIGNALS["s0_flush"][0], 0)


def _set_full_rvc_request(dut):
    return _set_request(
        dut,
        size0=32,
        range0=(1 << 32) - 1,
        data0=_RVC_NOP_CACHE_LINE,
        maybe_rvc_map=(1 << 32) - 1,
    )


def _set_bin907_s1(
    dut,
    *,
    ftq=(0, 7),
    exception=3,
    raw_candidate_count=2,
    spec_instr_count=1,
    instr_count=1,
    expose_later_data=True,
    expose_raw_candidate_probe=True,
):
    dut.set(_SIGNALS["s1_valid"][0], 1)
    dut.set(_SIGNALS["s1_ready"][0], 1)
    dut.set(_SIGNALS["s1_fire"][0], 1)
    dut.set(_SIGNALS["s1_flush"][0], 0)
    dut.set(_SIGNALS["s1_req_uncache"][0], 0)
    dut.set(_SIGNALS["s1_exception"][0], exception)
    dut.set(_BIN907_SIGNALS["s1_instr_count"][0], instr_count)
    raw_candidate_count = int(raw_candidate_count)
    assert 0 <= raw_candidate_count <= 32
    raw_candidate_mask = (1 << raw_candidate_count) - 1
    dut.set(_BIN907_SIGNALS["s1_total_range"][0], raw_candidate_mask)
    for index in range(32):
        signal_name = f"{_IFU_PREFIX}s1_instrEndMask_{index}"
        if not expose_raw_candidate_probe and index == 0:
            delattr(dut, signal_name)
        else:
            dut.set(signal_name, (raw_candidate_mask >> index) & 1)
    if expose_later_data:
        dut.set(_BIN907_SIGNALS["s1_later_data"][0], _NOP_CACHE_LINE & 0xFFFFFFFF)
    else:
        delattr(dut, _BIN907_SIGNALS["s1_later_data"][0])
    dut.set(_BIN907_SIGNALS["s1_ftq_flag"][0], ftq[0])
    dut.set(_BIN907_SIGNALS["s1_ftq_value"][0], ftq[1])


def _set_bin907_s2(
    dut,
    *,
    ftq=(0, 7),
    output_ftq=None,
    exception=3,
    slot=2,
    younger_output=False,
):
    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    dut.set(_BIN907_SIGNALS["s2_flush"][0], 0)
    dut.set(_BIN907_SIGNALS["s2_valid"][0], 1)
    dut.set(_BIN907_SIGNALS["s2_ftq_flag"][0], ftq[0])
    dut.set(_BIN907_SIGNALS["s2_ftq_value"][0], ftq[1])
    dut.set(_BIN907_SIGNALS["s2_exception"][0], exception)
    dut.set(_BIN907_SIGNALS["s2_instr_count"][0], 1)
    dut.set(_BIN907_SIGNALS["to_ibuffer_valid"][0], 1)
    dut.set(_BIN907_SIGNALS["to_ibuffer_ready"][0], 1)
    dut.set(_BIN907_SIGNALS["to_ibuffer_exception"][0], exception)
    active_mask = (1 << int(slot)) | (
        (1 << (int(slot) + 1)) if younger_output else 0
    )
    dut.set(_BIN907_SIGNALS["to_ibuffer_enq"][0], active_mask)
    dut.set(_BIN907_SIGNALS["to_ibuffer_valid_mask"][0], active_mask)
    for index in range(35):
        dut.set(
            f"{_IFU_PREFIX}__Vtogcov__io_toIBuffer_bits_exceptionMask_{index}",
            int(index == int(slot)),
        )
    output_ftq = ftq if output_ftq is None else output_ftq
    dut.set(f"{_IFU_PREFIX}io_toIBuffer_bits_ftqPtr_{slot}_flag", output_ftq[0])
    dut.set(f"{_IFU_PREFIX}io_toIBuffer_bits_ftqPtr_{slot}_value", output_ftq[1])


def _set_registered_stage(
    dut,
    stage,
    blocks,
    *,
    branch_type=None,
    expose_cross_flag=True,
    slot=0,
    instr_count=1,
    align_shift=0,
    prev_half_info=None,
):
    prev_half_info = prev_half_info or {"valid": 0, "data": 0, "pc": 0}
    valid_mask = ((1 << int(instr_count)) - 1) << int(slot)
    dut.set(f"{_IFU_PREFIX}{stage}_alignedInstrValid", valid_mask)
    dut.set(f"{_IFU_PREFIX}{stage}_instrCount", int(instr_count))
    for index, block in enumerate(blocks):
        for field, default in (
            ("valid", 0),
            ("ftqIdx_flag", 0),
            ("ftqIdx_value", 0),
            ("startVAddr_addr", 0),
        ):
            dut.set(
                f"{_IFU_PREFIX}{stage}_fetchBlock_{index}_{field}",
                block.get(field, default),
            )
    aggregate = {
        "blocks": blocks,
        "firstRange": int(blocks[0].get("range", 0)),
        "totalRange": int(blocks[0].get("range", 0))
        | int(blocks[1].get("range", 0) if blocks[1].get("valid") else 0),
        "maybeRvcMap": int(blocks[0].get("maybeRvcMap", 0)),
        "req0_data": int(blocks[0]["data"]),
    }
    if blocks[1].get("valid"):
        aggregate["req1_data"] = int(blocks[1]["data"])
    semantics = _expected_s1_semantic_slots(
        aggregate,
        align_shift=int(align_shift),
        prev_half_info=prev_half_info,
    )

    for duplicate in range(2):
        dut.set(
            f"{_IFU_PREFIX}s1_prevIBufEnqPtrDup_dup_{duplicate}_value",
            int(align_shift),
        )
    for stem, value in (
        ("s1_prevEndHalfRviInfo_valid", prev_half_info["valid"]),
        ("s1_prevEndHalfRviInfo_bits_data", prev_half_info["data"]),
        ("s1_prevEndHalfRviInfo_bits_pc_addr", prev_half_info["pc"]),
    ):
        dut.set(f"{_IFU_PREFIX}{stem}", value)
    for active_slot in range(int(slot), int(slot) + int(instr_count)):
        semantic = semantics[active_slot]
        if branch_type is not None:
            semantic = {**semantic, "branch_type": int(branch_type)}
        data_name = (
            f"{_IFU_PREFIX}s1_baseInstrData_{active_slot}"
            if stage == "s1" and active_slot >= 4
            else f"{_IFU_PREFIX}{stage}_alignedInstrVec_{active_slot}_data"
        )
        dut.set(data_name, semantic["data"])
        for stem, value in (
            (f"alignedInstrVec_{active_slot}_isRvc", semantic["is_rvc"]),
            (f"alignedInstrVec_{active_slot}_blockSel", semantic["raw_block_sel"]),
            (f"alignedInstrPcVec_{active_slot}_addr", semantic["pc"]),
            (
                f"alignedPdInfoVec_{active_slot}_brAttribute_branchType",
                semantic["branch_type"],
            ),
        ):
            dut.set(f"{_IFU_PREFIX}{stage}_{stem}", value)
        cross_name = (
            f"{_IFU_PREFIX}{stage}_alignedInstrVec_"
            f"{active_slot}_isCrossBlockInstr"
        )
        if expose_cross_flag:
            dut.set(cross_name, semantic["is_cross_block_instr"])
        if stage == "s1":
            dut.set(
                f"{_IFU_PREFIX}s1_alignedInstrVec_{active_slot}_index",
                semantic["index"],
            )
        else:
            dut.set(
                f"{_IFU_PREFIX}s2_alignedInstrVec_{active_slot}_endOffset",
                semantic["end_offset"],
            )
            dut.set(
                f"{_IFU_PREFIX}s2_alignedJumpOffsetVec_{active_slot}_addr",
                semantic["jump_offset"],
            )
            dut.set(
                f"{_IFU_PREFIX}s2_alignedPdInfoVec_{active_slot}_isRVC",
                semantic["predecode_is_rvc"],
            )
            dut.set(
                f"{_IFU_PREFIX}s2_alignedPdInfoVec_{active_slot}_brAttribute_rasAction",
                semantic["ras_action"],
            )


def _prime_lane34_registered_transaction(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_full_rvc_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(
        dut, "s1", blocks, slot=3, instr_count=32, align_shift=3
    )
    delattr(dut, f"{_IFU_PREFIX}s1_alignedInstrVec_34_isCrossBlockInstr")
    delattr(dut, f"{_IFU_PREFIX}s1_alignedInstrPcVec_34_addr")
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)
    return recorder, env, dut, blocks


def test_bin907_requires_same_transaction_exception_truncation(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_bin907_s1(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    assert recorder._ifu_bin907_pending is not None
    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_009")

    _set_bin907_s2(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_009")
    hit = recorder.hits[recorder.definition_by_bin_id["BIN-907"].key]
    observations = hit.evidence[-1]["observations"]
    assert observations["raw_candidate_count"] == 2
    assert observations["instr_count"] == 1
    assert observations["checks"] == {
        "s2_exception_matches": True,
        "s2_instr_count_is_one": True,
        "output_exception_matches": True,
        "single_enabled_slot": True,
        "single_valid_slot": True,
        "single_active_slot": True,
        "exception_mask_matches_active": True,
        "output_ftq_matches": True,
    }
    assert recorder._raw_dict()["errors"] == []


def test_bin907_missing_later_data_probe_is_visible_and_fail_closed(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_bin907_s1(dut, expose_later_data=False)

    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    assert recorder._ifu_bin907_pending is None
    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_009")
    assert any(
        error.get("event") == "ifu_bin907_probe_unobservable"
        and "s1_later_data" in error.get("missing", [])
        for error in recorder._raw_dict()["errors"]
    )


def test_bin907_requires_multiple_raw_pretruncation_candidates(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_bin907_s1(dut, raw_candidate_count=1)

    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    assert recorder._ifu_bin907_pending is None
    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_009")
    assert recorder._raw_dict()["errors"] == []


def test_bin907_missing_raw_candidate_probe_is_visible_and_fail_closed(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_bin907_s1(dut, expose_raw_candidate_probe=False)

    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    assert recorder._ifu_bin907_pending is None
    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_009")
    assert any(
        error.get("event") == "ifu_bin907_probe_unobservable"
        and "s1_instr_end_mask_0" in error.get("missing", [])
        for error in recorder._raw_dict()["errors"]
    )


@pytest.mark.parametrize(
    ("s2_ftq", "output_ftq", "expected_event"),
    (
        ((0, 8), (0, 7), "ifu_bin907_s2_identity_mismatch"),
        ((0, 7), (0, 8), "ifu_bin907_delivery_mismatch"),
    ),
)
def test_bin907_rejects_identity_mismatch(
    tmp_path, s2_ftq, output_ftq, expected_event
):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_bin907_s1(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _set_bin907_s2(dut, ftq=s2_ftq, output_ftq=output_ftq)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_009")
    assert any(
        error.get("event") == expected_event
        for error in recorder._raw_dict()["errors"]
    )


def test_bin907_rejects_younger_output(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_bin907_s1(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _set_bin907_s2(dut, younger_output=True)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_009")
    assert any(
        error.get("event") == "ifu_bin907_delivery_mismatch"
        for error in recorder._raw_dict()["errors"]
    )


def test_bin907_timeout_is_visible_and_fail_closed(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_bin907_s1(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    dut.set(_BIN907_SIGNALS["s2_valid"][0], 0)
    dut.set(_BIN907_SIGNALS["s2_flush"][0], 0)
    for cycle in range(2, _BIN907_TIMEOUT_CYCLES + 3):
        sample_ifu_cacheable_pipeline_coverage(recorder, env, cycle)

    assert recorder._ifu_bin907_pending is None
    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_009")
    assert any(
        error.get("event") == "ifu_bin907_transaction_timeout"
        for error in recorder._raw_dict()["errors"]
    )


@pytest.mark.parametrize(
    ("cause", "bin_name"),
    (("mmio", "owner_leaf_006"), ("second_itlb", "owner_leaf_010")),
)
def test_upstream_dual_block_suppression_reaches_single_block_output(
    tmp_path, cause, bin_name
):
    recorder, env, dut = _make_recorder(tmp_path)
    for key, value in {
        "mainpipe_fire": 1,
        "second_requested": 1,
        "second_waylookup_valid": 1,
        "first_mmio": 0,
        "second_mmio": int(cause == "mmio"),
        "first_itlb_exception": 0,
        "second_itlb_exception": int(cause == "second_itlb"),
        "first_ftq_flag": 0,
        "first_ftq_value": 11,
        "real_two_fetch": 0,
    }.items():
        dut.set(_UPSTREAM_SIGNALS[key][0], value)

    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", bin_name)

    dut.set(_UPSTREAM_SIGNALS["mainpipe_fire"][0], 0)
    _set_request(dut, ftq0=(0, 11))
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_v3_pipeline_owner_model", bin_name)


def _set_line0_late_fault(
    dut,
    *,
    ready,
    fire,
    flush,
    ftq_value=13,
    ecc_corrupt=1,
    req_is_uncache=0,
    input_exception=0,
    merged_exception=5,
):
    dut.set(_SIGNALS["s1_valid"][0], 1)
    dut.set(_SIGNALS["s1_ready"][0], ready)
    dut.set(_SIGNALS["s1_fire"][0], fire)
    dut.set(_SIGNALS["s1_flush"][0], flush)
    dut.set(_SIGNALS["s1_req_uncache"][0], req_is_uncache)
    dut.set(_LATE_FAULT_SIGNALS["line0_corrupt"][0], ecc_corrupt)
    dut.set(_LATE_FAULT_SIGNALS["line1_corrupt"][0], 0)
    dut.set(_LATE_FAULT_SIGNALS["s1_meta_in_exception"][0], input_exception)
    dut.set(_LATE_FAULT_SIGNALS["s1_merged_exception"][0], merged_exception)
    dut.set(_LATE_FAULT_SIGNALS["s1_ftq_flag"][0], 0)
    dut.set(_LATE_FAULT_SIGNALS["s1_ftq_value"][0], ftq_value)


def test_line0_late_fault_survives_stall_and_binds_one_exception_slot(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_line0_late_fault(dut, ready=0, fire=0, flush=0)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_008")

    _set_line0_late_fault(dut, ready=1, fire=1, flush=0)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    for key, value in {
        "s2_valid": 1,
        "s2_ftq_flag": 0,
        "s2_ftq_value": 13,
        "s2_exception": 5,
        "s2_instr_count": 1,
        "to_ibuffer_valid": 1,
        "to_ibuffer_ready": 1,
        "to_ibuffer_exception": 5,
        "to_ibuffer_enq": 1,
    }.items():
        dut.set(_LATE_FAULT_SIGNALS[key][0], value)
    for slot in range(35):
        dut.set(
            f"{_IFU_PREFIX}__Vtogcov__io_toIBuffer_bits_exceptionMask_{slot}",
            int(slot == 0),
        )
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)

    assert recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_008")


def test_line0_late_fault_flushes_without_s2_delivery(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_line0_late_fault(dut, ready=1, fire=0, flush=1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_flush"][0], 0)
    dut.set(_LATE_FAULT_SIGNALS["s2_valid"][0], 0)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_008")


@pytest.mark.parametrize("req_is_uncache", [0, 1])
def test_exception_code_without_explicit_late_fault_source_does_not_hit(
    tmp_path, req_is_uncache
):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_line0_late_fault(
        dut,
        ready=0,
        fire=0,
        flush=0,
        ecc_corrupt=0,
        req_is_uncache=req_is_uncache,
        input_exception=3,
        merged_exception=3,
    )
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    _set_line0_late_fault(
        dut,
        ready=1,
        fire=1,
        flush=0,
        ecc_corrupt=0,
        req_is_uncache=req_is_uncache,
        input_exception=3,
        merged_exception=3,
    )
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_008")
    assert recorder._ifu_late_fault_stall_pending is None


def test_line0_tl_fault_requires_matching_s0_source_before_s1_stall(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_request(dut, ftq0=(0, 13))
    dut.set(
        f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_info_0_icacheMeta_exception_value",
        3,
    )
    dut.set(_LATE_FAULT_SIGNALS["line0_tl_corrupt"][0], 1)
    dut.set(_LATE_FAULT_SIGNALS["line0_tl_denied"][0], 1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_line0_late_fault(
        dut,
        ready=0,
        fire=0,
        flush=0,
        ecc_corrupt=0,
        input_exception=3,
        merged_exception=3,
    )
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)
    assert recorder._ifu_late_fault_stall_pending is not None
    assert recorder._ifu_late_fault_stall_pending["fault_source"] == "tl_denied"

    _set_line0_late_fault(
        dut,
        ready=1,
        fire=1,
        flush=0,
        ecc_corrupt=0,
        input_exception=3,
        merged_exception=3,
    )
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)
    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    for key, value in {
        "s2_valid": 1,
        "s2_ftq_flag": 0,
        "s2_ftq_value": 13,
        "s2_exception": 3,
        "s2_instr_count": 1,
        "to_ibuffer_valid": 1,
        "to_ibuffer_ready": 1,
        "to_ibuffer_exception": 3,
        "to_ibuffer_enq": 1,
    }.items():
        dut.set(_LATE_FAULT_SIGNALS[key][0], value)
    for slot in range(35):
        dut.set(
            f"{_IFU_PREFIX}__Vtogcov__io_toIBuffer_bits_exceptionMask_{slot}",
            int(slot == 0),
        )
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 4)

    assert recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_008")
    hit = recorder.hits[recorder.definition_by_bin_id["BIN-906"].key]
    observations = hit.evidence[-1]["observations"]
    assert observations["fault_source"] == "tl_denied"
    assert observations["source_signal_paths"] == {
        "line0_tl_corrupt": _LATE_FAULT_SIGNALS["line0_tl_corrupt"][0],
        "line0_tl_denied": _LATE_FAULT_SIGNALS["line0_tl_denied"][0],
    }


def test_missing_line0_tl_source_probe_is_visible_and_fail_closed(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_request(dut, ftq0=(0, 13))
    dut.set(
        f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_info_0_icacheMeta_exception_value",
        3,
    )
    for key in ("line0_tl_corrupt", "line0_tl_denied"):
        for name in _LATE_FAULT_SIGNALS[key]:
            if hasattr(dut, name):
                delattr(dut, name)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_008")
    assert any(
        item.get("event") == "ifu_line0_late_fault_source_unobservable"
        for item in recorder.risk_observations
    )
    assert any(
        item.get("kind") == "FUNCOV_CONTRACT_ERROR"
        and item.get("event") == "ifu_line0_late_fault_source_unobservable"
        for item in recorder.contract_errors
    )


def test_line1_late_fault_records_blocked_attribution_risk(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_line0_late_fault(dut, ready=1, fire=1, flush=0)
    dut.set(_LATE_FAULT_SIGNALS["line0_corrupt"][0], 0)
    dut.set(_LATE_FAULT_SIGNALS["line1_corrupt"][0], 1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_011")
    assert any(
        item.get("event") == "ifu_second_cacheline_late_fault_unattributed"
        and item.get("blocked_bin_id") == "BIN-909"
        for item in recorder.risk_observations
    )


def test_cacheable_single_block_transfer_requires_matching_s1_metadata(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    _idle_request(dut)
    _set_s1(dut, blocks)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_cacheable_ingress", "accepted")
    assert recorder.key_hit("ifu_cacheable_window", "single_block")
    assert recorder.key_hit("ifu_cacheable_metadata", "first_ftq_preserved")
    assert recorder.key_hit("ifu_cacheable_metadata", "not_taken_preserved")
    assert recorder.key_hit("ifu_cacheable_s1", "fire_to_s2")
    assert recorder.key_hit("ifu_cacheable_s1", "source_ftq_address_matched")
    assert recorder.key_hit("ifu_cacheable_s1", "s0_accept_to_s1_valid")
    assert recorder.key_hit("ifu_cacheable_s1", "single_cacheable_path")
    assert recorder.key_hit("ifu_cacheable_s1", "cacheable_no_uncache")
    assert not recorder.key_hit("ifu_cacheable_metadata", "second_ftq_preserved")


def test_recorder_on_cycle_invokes_cacheable_pipeline_sampler(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set("reset", 0)
    _set_request(dut)

    recorder.on_cycle(1, env)

    assert recorder.key_hit("ifu_cacheable_ingress", "accepted")


def test_cacheable_dual_block_transfer_preserves_both_ftq_sources(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    second = {
        "valid": 1,
        "ftqIdx_flag": 0,
        "ftqIdx_value": 4,
        "startVAddr_addr": 0x40000010,
        "takenCfiOffset_valid": 1,
        "takenCfiOffset_bits": 5,
        "range": 0x3F,
        "size": 6,
        "data": 0x55667788,
        "maybeRvcMap": 0x33,
    }
    blocks = _set_request(dut, taken0=(1, 7), second=second)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    _idle_request(dut)
    _set_s1(dut, blocks)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_cacheable_window", "dual_block")
    assert recorder.key_hit("ifu_cacheable_metadata", "first_ftq_preserved")
    assert recorder.key_hit("ifu_cacheable_metadata", "second_ftq_preserved")
    assert not recorder.key_hit("ifu_cacheable_metadata", "not_taken_preserved")
    assert recorder.key_hit("ifu_cacheable_s1", "dual_cacheable_path")


def test_cacheable_metadata_diversity_bins_are_transaction_scoped(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    transactions = [
        (0x40000000, 32, (0, 31), (0, 1)),
        (0x40000001, 16, (1, 7), (0, 2)),
        (0x40000010, 16, (1, 7), (0, 3)),
        (0x40000018, 15, (1, 6), (0, 4)),
    ]
    cycle = 1
    for start, size, taken, ftq in transactions:
        blocks = _set_request(dut, start0=start, size0=size, taken0=taken, ftq0=ftq)
        sample_ifu_cacheable_pipeline_coverage(recorder, env, cycle)
        cycle += 1
        _idle_request(dut)
        _set_s1(dut, blocks)
        sample_ifu_cacheable_pipeline_coverage(recorder, env, cycle)
        cycle += 1

    assert recorder.key_hit("ifu_cacheable_address", "head_mid_tail_seen")
    assert recorder.key_hit("ifu_cacheable_address", "align_2b_4b_seen")
    assert recorder.key_hit("ifu_cacheable_range", "sequential_full_fetch")
    assert recorder.key_hit("ifu_cacheable_metadata", "taken_offset_preserved")
    assert recorder.key_hit("ifu_cacheable_range", "fetch_size_variation")
    assert recorder.key_hit("ifu_cacheable_metadata", "ftq_pointer_progression")


def test_cacheable_cross_cacheline_dual_block_is_distinct(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    second = {
        "valid": 1,
        "ftqIdx_flag": 0,
        "ftqIdx_value": 8,
        "startVAddr_addr": 0x40000020,
        "takenCfiOffset_valid": 1,
        "takenCfiOffset_bits": 7,
        "range": 0xFF,
        "size": 8,
        "data": 0x55667788,
        "maybeRvcMap": 0x33,
    }
    blocks = _set_request(
        dut,
        start0=0x40000018,
        ftq0=(0, 7),
        size0=8,
        taken0=(1, 7),
        second=second,
    )
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    _idle_request(dut)
    _set_s1(dut, blocks)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_cacheable_window", "cross_cacheline_dual_block")


def test_cacheable_ingress_payload_must_stay_stable_while_backpressured(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_request(dut, ready=0, fire=0)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_cacheable_ingress", "backpressured")
    assert recorder.key_hit("ifu_cacheable_ingress", "backpressure_payload_stable")

    dut.set(
        f"{_ICACHE_PREFIX}mainPipe.s1_data_r",
        0xDEADBEEF,
    )
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)
    assert any(
        item["event"] == "ifu_cacheable_backpressure_payload_changed"
        for item in recorder.risk_observations
    )


def test_cacheable_s1_backpressure_holds_icache_response(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut, ready=0, fire=0)
    _set_s1(dut, blocks, ready=0)

    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_cacheable_s1", "response_backpressured_by_s2")


def test_cacheable_s1_metadata_must_stay_stable_while_blocked(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut, valid=0, fire=0)
    _set_s1(dut, blocks, ready=0)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_cacheable_transfer", "s1_payload_stable")


def test_cacheable_back_to_back_and_gapped_transfers_are_distinct(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    first = _set_request(dut, ftq0=(0, 1))
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    second = _set_request(dut, ftq0=(0, 2), start0=0x40000010)
    _set_s1(dut, first)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)
    assert recorder.key_hit("ifu_cacheable_ingress", "back_to_back_accept")

    _idle_request(dut)
    _set_s1(dut, second)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)
    dut.set(_SIGNALS["s1_valid"][0], 0)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 4)

    third = _set_request(dut, ftq0=(0, 7), start0=0x40000040)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 5)
    _idle_request(dut)
    _set_s1(dut, third)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 6)
    assert recorder.key_hit("ifu_cacheable_transfer", "gapped_metadata_isolated")


@pytest.mark.parametrize(
    ("cause", "bin_name"),
    [
        ("backend_redirect", "backend_redirect_blocks"),
        ("wb_redirect", "wb_redirect_blocks"),
        ("bpu_match", "bpu_match_blocks"),
    ],
)
def test_cacheable_flush_causes_block_old_s0_return(tmp_path, cause, bin_name):
    recorder, env, dut = _make_recorder(tmp_path / cause)
    _set_request(dut, ready=1, fire=0, flush=1)
    if cause == "bpu_match":
        dut.set(_SIGNALS["bpu_s3_flush"][0], 1)
        dut.set(_SIGNALS["s0_flush_bpu"][0], 1)
    else:
        dut.set(_SIGNALS[cause][0], 1)

    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_cacheable_flush", bin_name)
    assert recorder.key_hit("ifu_cacheable_flush", "flush_wins_fire")
    assert not recorder.key_hit("ifu_cacheable_ingress", "accepted")


def test_backend_redirect_closes_held_response_on_ifu_routed_cycle(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_request(dut, ready=0, fire=0, flush=0)
    dut.set(_SIGNALS["backend_redirect"][0], 1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    assert not recorder.key_hit("ifu_cacheable_flush", "backend_redirect_blocks")
    _idle_request(dut)
    dut.set(_SIGNALS["backend_redirect"][0], 0)
    dut.set(_SIGNALS["ifu_backend_redirect"][0], 1)
    dut.set(_SIGNALS["s0_flush"][0], 1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_cacheable_flush", "backend_redirect_blocks")
    assert recorder.key_hit("ifu_cacheable_flush", "flush_wins_fire")
    hit = recorder.hits[recorder.definition_by_bin_id["BIN-812"].key]
    evidence = hit.evidence[-1]
    assert evidence["event"] == "ifu_s0_backend_redirect_blocks_aggregate_response"
    assert evidence["redirect_cycle"] == 1
    assert evidence["flush_cycle"] == 2
    assert evidence["req_valid_at_flush"] == 0
    assert not recorder.contract_errors


def test_backend_redirect_does_not_credit_changed_aggregate_response(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_request(dut, ready=0, fire=0, flush=0, ftq0=(0, 3))
    dut.set(_SIGNALS["backend_redirect"][0], 1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _set_request(dut, ready=1, fire=0, flush=1, ftq0=(0, 4))
    dut.set(_SIGNALS["backend_redirect"][0], 0)
    dut.set(_SIGNALS["ifu_backend_redirect"][0], 1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert not recorder.key_hit("ifu_cacheable_flush", "backend_redirect_blocks")
    assert any(
        error["event"] == "ifu_cacheable_backend_flush_source_changed"
        for error in recorder.contract_errors
    )


def test_backend_redirect_missing_ifu_probe_is_contract_error(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    delattr(dut, _SIGNALS["ifu_backend_redirect"][0])
    _set_request(dut, ready=0, fire=0, flush=0)
    dut.set(_SIGNALS["backend_redirect"][0], 1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    dut.set(_SIGNALS["backend_redirect"][0], 0)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert not recorder.key_hit("ifu_cacheable_flush", "backend_redirect_blocks")
    assert any(
        error["event"] == "ifu_cacheable_backend_flush_internal_unobservable"
        for error in recorder.contract_errors
    )


def test_cacheable_nonmatching_bpu_flush_allows_s0_fire(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_request(dut)
    dut.set(_SIGNALS["bpu_s3_flush"][0], 1)
    dut.set(_SIGNALS["s0_flush_bpu"][0], 0)

    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_cacheable_flush", "bpu_miss_allows")
    assert recorder.key_hit("ifu_cacheable_ingress", "accepted")


def test_cacheable_metadata_mismatch_is_diagnostic_not_coverage(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    blocks[0]["ftqIdx_value"] += 1
    _idle_request(dut)
    _set_s1(dut, blocks)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert not recorder.key_hit("ifu_cacheable_window", "single_block")
    assert not recorder.key_hit("ifu_cacheable_metadata", "first_ftq_preserved")
    assert any(
        item["event"] == "ifu_cacheable_s1_metadata_mismatch"
        for item in recorder.risk_observations
    )


def test_cacheable_pending_transfer_is_not_credited_when_s1_flushes(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    _idle_request(dut)
    _set_s1(dut, blocks)
    dut.set(_SIGNALS["s1_flush"][0], 1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert not recorder.key_hit("ifu_cacheable_window", "single_block")
    assert not recorder.key_hit("ifu_cacheable_metadata", "first_ftq_preserved")
    assert any(
        item["event"] == "ifu_cacheable_pending_transfer_flushed"
        for item in recorder.risk_observations
    )


def test_cacheable_aggregate_transaction_keeps_registered_s1_s2_fields(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    second = {
        "valid": 1,
        "ftqIdx_flag": 0,
        "ftqIdx_value": 4,
        "startVAddr_addr": 0x40000008,
        "takenCfiOffset_valid": 0,
        "takenCfiOffset_bits": 7,
        "range": 0xFF00,
        "size": 8,
        "data": 0x55667788,
    }
    blocks = _set_request(dut, second=second)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    dut.set(f"{_IFU_PREFIX}s2_valid_valid", 1)
    dut.set(f"{_IFU_PREFIX}s2_flush", 0)
    _set_registered_stage(dut, "s2", blocks)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)

    passes = [
        item
        for item in recorder.risk_observations
        if item.get("event") == "ifu_s1_s2_registered_transaction_pass"
    ]
    assert len(passes) == 1
    assert passes[0]["s0_cycle"] == 1
    assert passes[0]["s1_cycle"] == 2
    assert passes[0]["s2"]["slots"][0]["effective_owner"] == 0
    assert passes[0]["semantics"]["s2_expected"][0]["end_offset"] == 1
    assert recorder._raw_dict()["errors"] == []


def test_cacheable_s1_alignment_rejects_wrong_independent_index(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks)
    dut.set(f"{_IFU_PREFIX}s1_alignedInstrVec_0_index", 1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder._ifu_cacheable_s1_s2_pending is None
    assert any(
        item.get("event") == "ifu_s1_alignment_semantic_mismatch"
        and item["mismatches"][0]["expected"]["index"] == 0
        and item["mismatches"][0]["observed"]["index"] == 1
        for item in recorder.risk_observations
    )
    raw = recorder._raw_dict()
    assert raw["checker"]["status"] == "fail"
    assert any(
        error.get("kind") == "FUNCOV_CONTRACT_ERROR"
        and error.get("event") == "ifu_s1_alignment_semantic_mismatch"
        for error in raw["errors"]
    )


def test_cacheable_s1_alignment_rejects_wrong_cross_block_stitch(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    first_line = 0x006F << (16 * 31)
    second = {
        "valid": 1,
        "ftqIdx_flag": 0,
        "ftqIdx_value": 4,
        "startVAddr_addr": 0x40000020,
        "takenCfiOffset_valid": 0,
        "takenCfiOffset_bits": 0,
        "range": 0x2,
        "size": 1,
        "data": 0,
    }
    blocks = _set_request(
        dut,
        start0=0x4000001F,
        size0=1,
        range0=0x1,
        taken0=(0, 0),
        data0=first_line,
        maybe_rvc_map=0,
        second=second,
    )
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks)
    dut.set(f"{_IFU_PREFIX}s1_alignedInstrVec_0_data", 0x006F006F)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    mismatches = [
        item
        for item in recorder.risk_observations
        if item.get("event") == "ifu_s1_alignment_semantic_mismatch"
    ]
    assert len(mismatches) == 1
    assert mismatches[0]["mismatches"][0]["expected"]["data"] == 0x0000006F
    assert mismatches[0]["mismatches"][0]["expected"]["effective_owner"] == 1
    assert mismatches[0]["mismatches"][0]["observed"]["data"] == 0x006F006F


def test_cacheable_s1_alignment_rejects_non_compacted_valid_mask(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks, instr_count=3)
    dut.set(f"{_IFU_PREFIX}s1_instrCount", 2)
    dut.set(f"{_IFU_PREFIX}s1_alignedInstrValid", 0b101)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder._ifu_cacheable_s1_s2_pending is None
    mismatches = [
        mismatch
        for item in recorder.risk_observations
        if item.get("event") == "ifu_s1_alignment_semantic_mismatch"
        for mismatch in item.get("mismatches", [])
    ]
    assert any(
        item.get("reason") == "valid_mask_instr_count_or_compaction_mismatch"
        for item in mismatches
    )
    assert any(
        item.get("reason")
        == "registered_valid_slots_do_not_match_compacted_transaction"
        for item in mismatches
    )


def test_cacheable_s1_alignment_missing_index_probe_is_visible(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks)
    delattr(dut, f"{_IFU_PREFIX}s1_alignedInstrVec_0_index")
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder._ifu_cacheable_s1_s2_pending is None
    assert any(
        item.get("event") == "ifu_s1_alignment_probe_unobservable"
        and "s1_alignedInstrVec_0_index" in item.get("missing", [])
        for item in recorder.risk_observations
    )


def test_cacheable_s1_alignment_rejects_predecode_even_if_stage_payload_agrees(
    tmp_path,
):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks, branch_type=2)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder._ifu_cacheable_s1_s2_pending is None
    assert any(
        item.get("event") == "ifu_s1_alignment_semantic_mismatch"
        and item["mismatches"][0]["expected"]["branch_type"] == 0
        and item["mismatches"][0]["observed"]["branch_type"] == 2
        for item in recorder.risk_observations
    )


def test_cacheable_previous_half_rvi_uses_atomic_data_and_pc(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    previous = {"valid": 1, "data": 0x0003, "pc": 0x3FFFFFFF}
    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks, prev_half_info=previous)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    dut.set(f"{_IFU_PREFIX}s2_valid_valid", 1)
    dut.set(f"{_IFU_PREFIX}s2_flush", 0)
    _set_registered_stage(dut, "s2", blocks, prev_half_info=previous)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)

    passes = [
        item
        for item in recorder.risk_observations
        if item.get("event") == "ifu_s1_s2_registered_transaction_pass"
    ]
    assert len(passes) == 1
    semantic = passes[0]["semantics"]["s1"][0]
    assert semantic["data"] == 0x00130003
    assert semantic["pc"] == previous["pc"]


def test_cacheable_s1_s2_transaction_survives_one_valid_hole(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    dut.set(f"{_IFU_PREFIX}s2_valid_valid", 0)
    dut.set(f"{_IFU_PREFIX}s2_flush", 0)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)

    assert recorder._ifu_cacheable_s1_s2_pending is not None
    assert not any(
        item.get("event")
        in {
            "ifu_s1_s2_registered_transaction_pass",
            "ifu_s1_s2_registered_transaction_mismatch",
            "ifu_s1_s2_transaction_timeout",
        }
        for item in recorder.risk_observations
    )

    dut.set(f"{_IFU_PREFIX}s2_valid_valid", 1)
    _set_registered_stage(dut, "s2", blocks)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 4)

    passes = [
        item
        for item in recorder.risk_observations
        if item.get("event") == "ifu_s1_s2_registered_transaction_pass"
    ]
    assert len(passes) == 1
    assert passes[0]["s1_cycle"] == 2
    assert passes[0]["cycle"] == 4


def test_cacheable_s1_s2_flush_is_diagnostic_not_contract_error(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    dut.set(f"{_IFU_PREFIX}s2_valid_valid", 1)
    dut.set(f"{_IFU_PREFIX}s2_flush", 1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)

    assert any(
        item.get("event") == "ifu_s1_s2_transaction_flushed"
        for item in recorder.risk_observations
    )
    assert recorder._raw_dict()["errors"] == []


def test_cacheable_s1_s2_transaction_missing_cross_probe_is_visible(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks, expose_cross_flag=False)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder._ifu_cacheable_s1_s2_pending is None
    assert any(
        item.get("event") == "ifu_s1_s2_transaction_probe_unobservable"
        and item.get("stage") == "s1"
        and any("isCrossBlockInstr" in name for name in item.get("missing", []))
        for item in recorder.risk_observations
    )


def test_cacheable_lane34_structural_cross_and_deferred_s1_pc_pass_at_s2(
    tmp_path,
):
    recorder, env, dut, blocks = _prime_lane34_registered_transaction(tmp_path)

    assert recorder._ifu_cacheable_s1_s2_pending is not None
    pending_slot = recorder._ifu_cacheable_s1_s2_pending["s1"]["slots"][-1]
    assert pending_slot["slot"] == 34
    assert pending_slot["pc"] is None
    assert pending_slot["is_cross_block_instr"] == 0
    assert pending_slot["observation_modes"] == {
        "is_cross_block_instr": "rtl_structural_zero",
        "pc": "deferred_to_s2",
    }

    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    dut.set(f"{_IFU_PREFIX}s2_valid_valid", 1)
    dut.set(f"{_IFU_PREFIX}s2_flush", 0)
    _set_registered_stage(
        dut, "s2", blocks, slot=3, instr_count=32, align_shift=3
    )
    delattr(dut, f"{_IFU_PREFIX}s2_alignedInstrVec_34_isCrossBlockInstr")
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)

    passes = [
        item
        for item in recorder.risk_observations
        if item.get("event") == "ifu_s1_s2_registered_transaction_pass"
    ]
    assert len(passes) == 1
    assert passes[0]["s2"]["slots"][-1]["pc"] == 0x4000001F
    assert passes[0]["s2"]["slots"][-1]["observation_modes"] == {
        "is_cross_block_instr": "rtl_structural_zero"
    }
    assert recorder._raw_dict()["errors"] == []


@pytest.mark.parametrize(
    ("field", "stem"),
    (
        ("cross", "s1_alignedInstrVec_33_isCrossBlockInstr"),
        ("pc", "s1_alignedInstrPcVec_33_addr"),
    ),
)
def test_cacheable_lane33_missing_probe_still_fails_closed(tmp_path, field, stem):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_full_rvc_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(
        dut, "s1", blocks, slot=2, instr_count=32, align_shift=2
    )
    delattr(dut, f"{_IFU_PREFIX}{stem}")
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder._ifu_cacheable_s1_s2_pending is None
    assert any(
        item.get("event") == "ifu_s1_s2_transaction_probe_unobservable"
        and item.get("stage") == "s1"
        and stem in item.get("missing", [])
        for item in recorder.risk_observations
    ), field


def test_cacheable_lane34_fallback_requires_exact_tail_shape(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_full_rvc_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(
        dut, "s1", blocks, slot=3, instr_count=32, align_shift=3
    )
    dut.set(f"{_IFU_PREFIX}s1_instrCount", 31)
    delattr(dut, f"{_IFU_PREFIX}s1_alignedInstrVec_34_isCrossBlockInstr")
    delattr(dut, f"{_IFU_PREFIX}s1_alignedInstrPcVec_34_addr")
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    errors = recorder._raw_dict()["errors"]
    assert any(
        error.get("event") == "ifu_s1_s2_transaction_probe_unobservable"
        and "s1_alignedInstrVec_34_isCrossBlockInstr" in error.get("missing", [])
        and "s1_alignedInstrPcVec_34_addr" in error.get("missing", [])
        for error in errors
    )


@pytest.mark.parametrize("failure", ("missing", "wrong"))
def test_cacheable_lane34_s2_pc_must_be_observed_and_correct(tmp_path, failure):
    recorder, env, dut, blocks = _prime_lane34_registered_transaction(tmp_path)
    assert recorder._ifu_cacheable_s1_s2_pending is not None

    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    dut.set(f"{_IFU_PREFIX}s2_valid_valid", 1)
    dut.set(f"{_IFU_PREFIX}s2_flush", 0)
    _set_registered_stage(
        dut, "s2", blocks, slot=3, instr_count=32, align_shift=3
    )
    delattr(dut, f"{_IFU_PREFIX}s2_alignedInstrVec_34_isCrossBlockInstr")
    pc_stem = f"{_IFU_PREFIX}s2_alignedInstrPcVec_34_addr"
    if failure == "missing":
        delattr(dut, pc_stem)
    else:
        dut.set(pc_stem, 0x40000020)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)

    assert not any(
        item.get("event") == "ifu_s1_s2_registered_transaction_pass"
        for item in recorder.risk_observations
    )
    errors = recorder._raw_dict()["errors"]
    expected_event = (
        "ifu_s1_s2_transaction_probe_unobservable"
        if failure == "missing"
        else "ifu_s2_registered_semantic_mismatch"
    )
    assert any(error.get("event") == expected_event for error in errors)


def test_cacheable_s1_s2_transaction_predecode_mismatch_is_not_a_pass(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    dut.set(f"{_IFU_PREFIX}s2_valid_valid", 1)
    dut.set(f"{_IFU_PREFIX}s2_flush", 0)
    _set_registered_stage(dut, "s2", blocks, branch_type=2)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)

    assert not any(
        item.get("event") == "ifu_s1_s2_registered_transaction_pass"
        for item in recorder.risk_observations
    )
    assert any(
        item.get("event") == "ifu_s1_s2_registered_transaction_mismatch"
        for item in recorder.risk_observations
    )


@pytest.mark.parametrize(
    ("stem", "wrong_value"),
    (("isRVC", 1), ("brAttribute_rasAction", 2)),
)
def test_cacheable_s2_registered_predecode_semantic_mismatch_is_error(
    tmp_path, stem, wrong_value
):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    dut.set(f"{_IFU_PREFIX}s2_valid_valid", 1)
    dut.set(f"{_IFU_PREFIX}s2_flush", 0)
    _set_registered_stage(dut, "s2", blocks)
    dut.set(f"{_IFU_PREFIX}s2_alignedPdInfoVec_0_{stem}", wrong_value)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)

    assert not any(
        item.get("event") == "ifu_s1_s2_registered_transaction_pass"
        for item in recorder.risk_observations
    )
    assert any(
        item.get("event") == "ifu_s2_registered_semantic_mismatch"
        for item in recorder.risk_observations
    )
    assert any(
        error.get("kind") == "FUNCOV_CONTRACT_ERROR"
        and error.get("event") == "ifu_s2_registered_semantic_mismatch"
        for error in recorder._raw_dict()["errors"]
    )


def test_cacheable_s2_missing_predecode_probe_is_visible_error(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    dut.set(f"{_IFU_PREFIX}s2_valid_valid", 1)
    dut.set(f"{_IFU_PREFIX}s2_flush", 0)
    _set_registered_stage(dut, "s2", blocks)
    delattr(
        dut,
        f"{_IFU_PREFIX}s2_alignedPdInfoVec_0_brAttribute_rasAction",
    )
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)

    errors = recorder._raw_dict()["errors"]
    assert any(
        error.get("event") == "ifu_s1_s2_transaction_probe_unobservable"
        and any("rasAction" in name for name in error.get("missing", []))
        for error in errors
    )


def test_cacheable_s1_s2_transaction_uses_current_s1_base_data_alias(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(
        dut, "s1", blocks, slot=1, instr_count=4, align_shift=1
    )
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    dut.set(f"{_IFU_PREFIX}s2_valid_valid", 1)
    dut.set(f"{_IFU_PREFIX}s2_flush", 0)
    _set_registered_stage(
        dut, "s2", blocks, slot=1, instr_count=4, align_shift=1
    )
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)

    passes = [
        item
        for item in recorder.risk_observations
        if item.get("event") == "ifu_s1_s2_registered_transaction_pass"
    ]
    assert len(passes) == 1
    assert passes[0]["s1"]["slots"][3]["slot"] == 4
    assert passes[0]["s1"]["slots"][3]["data"] == 0x00000013


@pytest.mark.parametrize("instruction", (0x8002, 0x9002))
def test_cacheable_predecode_matches_rtl_cebreak_priority(instruction):
    assert _decode_branch_type(instruction) == 0


def test_cacheable_sampler_signals_match_generated_contract():
    root = Path(__file__).resolve().parents[7]
    offset = root / "build-frontend/pylib-verilator/Frontend/Frontend_offset.yaml"
    names = {
        line[len("  - name: ") :].strip()
        for line in offset.read_text(encoding="utf-8").splitlines()
        if line.startswith("  - name: ")
    }
    required = {
        candidates[0]
        for key, candidates in _SIGNALS.items()
        if key not in {"wb_redirect"}
    }
    required |= {
        _SIGNALS["wb_redirect"][0],
        f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_info_0_ftqIdx_flag",
        f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_info_0_startVAddr_addr",
        f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_info_1_valid",
        f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_firstRange",
        f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_totalRange",
        f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_maybeRvcMap",
        f"{_IFU_PREFIX}s1_fetchBlock_0_ftqIdx_flag",
        f"{_IFU_PREFIX}s1_fetchBlock_0_startVAddr_addr",
        f"{_IFU_PREFIX}s1_fetchBlock_1_valid",
        f"{_IFU_PREFIX}s1_firstRange",
        f"{_IFU_PREFIX}s1_totalRange",
        f"{_IFU_PREFIX}s1_firstICacheData",
        f"{_IFU_PREFIX}s1_secondICacheData",
        f"{_IFU_PREFIX}_s1_alignedInstrValid_T",
        f"{_IFU_PREFIX}s1_alignedInstrVec_0_data",
        f"{_IFU_PREFIX}s1_alignedInstrVec_0_isRvc",
        f"{_IFU_PREFIX}s1_alignedInstrVec_0_blockSel",
        f"{_IFU_PREFIX}s1_alignedInstrVec_0_isCrossBlockInstr",
        f"{_IFU_PREFIX}s1_alignedInstrPcVec_0_addr",
        f"{_IFU_PREFIX}s1_alignedPdInfoVec_0_brAttribute_branchType",
        f"{_IFU_PREFIX}s2_alignedInstrValid",
        f"{_IFU_PREFIX}s2_alignedInstrVec_0_data",
        f"{_IFU_PREFIX}s2_alignedInstrVec_0_isRvc",
        f"{_IFU_PREFIX}s2_alignedInstrVec_0_blockSel",
        f"{_IFU_PREFIX}s2_alignedInstrVec_0_isCrossBlockInstr",
        f"{_IFU_PREFIX}s2_alignedInstrPcVec_0_addr",
        f"{_IFU_PREFIX}s2_alignedPdInfoVec_0_brAttribute_branchType",
        f"{_IFU_PREFIX}s1_prevIBufEnqPtrDup_dup_0_value",
        f"{_IFU_PREFIX}s1_prevIBufEnqPtrDup_dup_1_value",
        f"{_IFU_PREFIX}s1_prevEndHalfRviInfo_valid",
        f"{_IFU_PREFIX}s1_prevEndHalfRviInfo_bits_data",
        f"{_IFU_PREFIX}s1_prevEndHalfRviInfo_bits_pc_addr",
        _UPSTREAM_SIGNALS["mainpipe_fire"][0],
        _UPSTREAM_SIGNALS["second_requested"][0],
        _UPSTREAM_SIGNALS["second_waylookup_valid"][0],
        _UPSTREAM_SIGNALS["first_mmio"][0],
        _UPSTREAM_SIGNALS["second_mmio"][0],
        _UPSTREAM_SIGNALS["first_itlb_exception"][1],
        _UPSTREAM_SIGNALS["second_itlb_exception"][1],
        _UPSTREAM_SIGNALS["first_ftq_flag"][1],
        _UPSTREAM_SIGNALS["first_ftq_value"][1],
        _UPSTREAM_SIGNALS["real_two_fetch"][2],
        *(
            candidates[0]
            for candidates in _LATE_FAULT_SIGNALS.values()
        ),
    }
    required |= {
        f"{_IFU_PREFIX}__Vtogcov__io_toIBuffer_bits_exceptionMask_{slot}"
        for slot in range(35)
    }
    required |= {
        f"{_IFU_PREFIX}s1_alignedInstrVec_{slot}_index"
        for slot in range(35)
    }
    required |= {
        f"{_IFU_PREFIX}s2_alignedInstrVec_{slot}_endOffset"
        for slot in range(35)
    }
    required |= {
        f"{_IFU_PREFIX}s2_alignedJumpOffsetVec_{slot}_addr"
        for slot in range(35)
    }
    required |= {
        f"{_IFU_PREFIX}s2_alignedPdInfoVec_{slot}_isRVC"
        for slot in range(35)
    }
    required |= {
        f"{_IFU_PREFIX}s2_alignedPdInfoVec_{slot}_brAttribute_rasAction"
        for slot in range(35)
    }
    required |= {
        f"{_IFU_PREFIX}s1_alignedInstrVec_{slot}_data"
        for slot in range(4)
    }
    required |= {
        f"{_IFU_PREFIX}s1_baseInstrData_{slot}"
        for slot in range(4, 35)
    }
    required |= {
        f"{_ICACHE_PREFIX}mainPipe._s1_data_T_{3 * bank + int(bank >= 8) + 1}"
        for bank in range(16)
    }
    required |= {
        f"{_ICACHE_PREFIX}mainPipe._s1_data_T{'' if bank == 0 else f'_{3 * bank + int(bank >= 8)}'}"
        for bank in range(16)
    }
    required |= {
        f"{_ICACHE_PREFIX}mainPipe.s1_data_r{'' if bank == 0 else f'_{bank}'}"
        for bank in range(16)
    }
    assert required <= names
    assert len(IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS) == 30
