from pathlib import Path
from types import SimpleNamespace

import pytest

from env.funcov.py.ifu.cacheable_pipeline_funcov import (
    IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS,
    _LATE_FAULT_SIGNALS,
    _SIGNALS,
    _UPSTREAM_SIGNALS,
    sample_ifu_cacheable_pipeline_coverage,
)
from env.funcov.recorder import FunctionalCoverageRecorder, default_pilot_csv_path


_ICACHE_PREFIX = "Frontend_top.Frontend.inner_icache."
_IFU_PREFIX = "Frontend_top.Frontend.inner_ifu."


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
            "data": 0x11223344,
            "maybeRvcMap": 0x55,
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


def _set_registered_stage(
    dut,
    stage,
    blocks,
    *,
    branch_type=0,
    expose_cross_flag=True,
    slot=0,
):
    valid_mask = 1 << int(slot)
    dut.set(f"{_IFU_PREFIX}{stage}_alignedInstrValid", valid_mask)
    dut.set(f"{_IFU_PREFIX}{stage}_instrCount", 1)
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
    data_name = (
        f"{_IFU_PREFIX}s1_baseInstrData_{slot}"
        if stage == "s1" and int(slot) >= 4
        else f"{_IFU_PREFIX}{stage}_alignedInstrVec_{slot}_data"
    )
    dut.set(data_name, 0x00000013)
    for stem, value in (
        (f"alignedInstrVec_{slot}_isRvc", 0),
        (f"alignedInstrVec_{slot}_blockSel", 0),
        (f"alignedInstrPcVec_{slot}_addr", blocks[0]["startVAddr_addr"]),
        (f"alignedPdInfoVec_{slot}_brAttribute_branchType", branch_type),
    ):
        dut.set(f"{_IFU_PREFIX}{stage}_{stem}", value)
    cross_name = f"{_IFU_PREFIX}{stage}_alignedInstrVec_{slot}_isCrossBlockInstr"
    if expose_cross_flag:
        dut.set(cross_name, 0)


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


def _set_line0_late_fault(dut, *, ready, fire, flush, ftq_value=13):
    dut.set(_SIGNALS["s1_valid"][0], 1)
    dut.set(_SIGNALS["s1_ready"][0], ready)
    dut.set(_SIGNALS["s1_fire"][0], fire)
    dut.set(_SIGNALS["s1_flush"][0], flush)
    dut.set(_LATE_FAULT_SIGNALS["line0_corrupt"][0], 1)
    dut.set(_LATE_FAULT_SIGNALS["line1_corrupt"][0], 0)
    dut.set(_LATE_FAULT_SIGNALS["s1_meta_in_exception"][0], 0)
    dut.set(_LATE_FAULT_SIGNALS["s1_merged_exception"][0], 5)
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
    _set_registered_stage(dut, "s1", blocks, branch_type=2)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    dut.set(f"{_IFU_PREFIX}s2_valid_valid", 1)
    dut.set(f"{_IFU_PREFIX}s2_flush", 0)
    _set_registered_stage(dut, "s2", blocks, branch_type=2)
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


def test_cacheable_s1_s2_transaction_survives_one_valid_hole(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks, branch_type=2)
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
    _set_registered_stage(dut, "s2", blocks, branch_type=2)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 4)

    passes = [
        item
        for item in recorder.risk_observations
        if item.get("event") == "ifu_s1_s2_registered_transaction_pass"
    ]
    assert len(passes) == 1
    assert passes[0]["s1_cycle"] == 2
    assert passes[0]["cycle"] == 4


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


def test_cacheable_s1_s2_transaction_predecode_mismatch_is_not_a_pass(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks, branch_type=1)
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


def test_cacheable_s1_s2_transaction_uses_current_s1_base_data_alias(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    _idle_request(dut)
    _set_s1(dut, blocks)
    _set_registered_stage(dut, "s1", blocks, slot=4)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    dut.set(f"{_IFU_PREFIX}s2_valid_valid", 1)
    dut.set(f"{_IFU_PREFIX}s2_flush", 0)
    _set_registered_stage(dut, "s2", blocks, slot=4)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)

    passes = [
        item
        for item in recorder.risk_observations
        if item.get("event") == "ifu_s1_s2_registered_transaction_pass"
    ]
    assert len(passes) == 1
    assert passes[0]["s1"]["slots"][0]["slot"] == 4
    assert passes[0]["s1"]["slots"][0]["data"] == 0x00000013


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
