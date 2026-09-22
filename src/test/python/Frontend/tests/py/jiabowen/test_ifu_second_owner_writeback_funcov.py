"""Negative contracts for BIN-947's same-transaction FTQ writeback."""

import pytest

from env.funcov.py.ifu.compact_funcov import _sample_predchecker_v3
from tests.py.jiabowen.test_ifu_compact_functional_coverage import (
    _PREFIX, _make_recorder, _set_predchecker_redirect, _set_predchecker_request,
)


def _drive(recorder, dut, *, raw_owner, cross):
    for stem, value in {"s2_valid_valid": 1, "s2_flush": 0, "s2_useUncacheFetch": 0}.items():
        dut.set(_PREFIX + stem, value)
    for b in range(2):
        for stem, value in dict(valid=1, ftqIdx_flag=0, ftqIdx_value=10 + b,
                                startVAddr_addr=0x40000000 + 32 * b).items():
            dut.set(_PREFIX + f"s2_fetchBlock_{b}_" + stem, value)
            dut.set(_PREFIX + f"wbAlignFetchBlock_{b}_" + stem, value)
    entry = dict(slot=15, branch_type=2, block_sel=raw_owner,
                 is_cross_block_instr=cross, end_offset=0, pc_addr=0x4000001F)
    _set_predchecker_request(dut, [entry])
    _sample_predchecker_v3(recorder, dut, 10)
    pending = recorder._ifu_predchecker_v3_pending
    target = pending["pc_addr"] + pending["jump_offset_addr"]
    _set_predchecker_redirect(dut, pending, target=target)
    for stem, value in {
        "wbValid": 1, "io_toFtq_wbRedirect_valid": 1,
        "io_toFtq_wbRedirect_bits_ftqIdx_flag": 0,
        "io_toFtq_wbRedirect_bits_ftqIdx_value": 11,
        "io_toFtq_wbRedirect_bits_pc": 0x80000040,
        "io_toFtq_wbRedirect_bits_target": target << 1,
        "io_toFtq_wbRedirect_bits_ftqOffset": 0,
        "io_toFtq_wbRedirect_bits_isRVC": 0,
        "io_toFtq_wbRedirect_bits_taken": 1,
        "io_toFtq_wbRedirect_bits_attribute_branchType": 2,
        "io_toFtq_wbRedirect_bits_attribute_rasAction": 0,
    }.items():
        dut.set(_PREFIX + stem, value)


@pytest.mark.parametrize("raw_owner,cross", [(1, 0), (0, 1)])
def test_second_owner_matches_both_raw_second_and_cross_block_rvi(tmp_path, raw_owner, cross):
    recorder, _env, dut, _memory = _make_recorder(tmp_path)
    _drive(recorder, dut, raw_owner=raw_owner, cross=cross)
    _sample_predchecker_v3(recorder, dut, 11)
    assert recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_049")
    detail = recorder.hit_detail_by_bin_id("BIN-947")
    assert detail is not None
    evidence = detail["evidence"][-1]["observations"]
    assert evidence["checkpoint_passed"]
    assert evidence["source_blocks"][1]["ftqIdx_value"] == 11
    assert evidence["writeback"]["io_toFtq_wbRedirect_bits_ftqIdx_value"] == 11
    assert evidence["signal_paths"] and not evidence["missing_probes"]


@pytest.mark.parametrize("field,value", [
    ("io_toFtq_wbRedirect_valid", 0),
    ("wbValid", 0),
    ("io_toFtq_wbRedirect_bits_ftqIdx_value", 10),
    ("io_toFtq_wbRedirect_bits_pc", 0x80000000),
    ("io_toFtq_wbRedirect_bits_target", 0x80000900),
    ("wbAlignFetchBlock_1_ftqIdx_value", 12),
    ("io_toFtq_wbRedirect_bits_ftqIdx_value", None),
])
def test_second_owner_rejects_missing_wrong_or_unwritten_payload(tmp_path, field, value):
    recorder, _env, dut, _memory = _make_recorder(tmp_path)
    _drive(recorder, dut, raw_owner=1, cross=0)
    if value is None:
        delattr(dut, _PREFIX + field)
    else:
        dut.set(_PREFIX + field, value)
    _sample_predchecker_v3(recorder, dut, 11)
    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_049")
    assert any(r.get("event") == "ifu_second_owner_writeback_rejected" for r in recorder.risk_observations)


def test_second_owner_rejects_stale_checker_cycle(tmp_path):
    recorder, _env, dut, _memory = _make_recorder(tmp_path)
    _drive(recorder, dut, raw_owner=0, cross=1)
    _sample_predchecker_v3(recorder, dut, 12)
    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_049")
