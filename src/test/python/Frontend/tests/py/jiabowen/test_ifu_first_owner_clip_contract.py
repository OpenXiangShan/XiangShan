"""BIN-874 producer and independent DUT-checker negative contracts."""

from copy import deepcopy
from pathlib import Path

import pytest

from env.funcov.py.ifu.cfvec_funcov import sample_cfvec_coverage
from tests.py.jiabowen.test_ifu_compact_functional_coverage import (
    _PREFIX, _make_recorder, _set_aligned_slot, _set_ifu_output,
)
from tests.py.jiabowen.test_ifu_first_owner_clip_v3_dut import (
    _accepted_cfvec_slots, _assert_clip_masks, _assert_recovery_prefix,
)


def _setup(tmp_path):
    recorder, env, dut, memory = _make_recorder(tmp_path)
    entries = [(0, 0x80000000, 0x13, 0, 1, 0, 3, 0),
               (1, 0x80000004, 0x13, 0, 3, 0, 3, 0)]
    _set_ifu_output(dut, entries)
    for slot in range(36):
        dut.set(_PREFIX + f"s2_alignedInstrVec_{slot}_valid", 0)
    for slot, entry in enumerate(entries):
        memory.write32(entry[1], entry[2])
        _set_aligned_slot(dut, slot, entry, block_sel=0, branch_type=0)
    _set_aligned_slot(dut, 2, (2, 0x80000040, 0x13, 0, 1, 0, 4, 0),
                      block_sel=1, branch_type=0)
    for block in range(2):
        dut.set(_PREFIX + f"s2_fetchBlock_{block}_valid", 1)
    return recorder, env, dut


def test_first_owner_clipping_marks_once_per_accepted_transaction(tmp_path):
    recorder, env, dut = _setup(tmp_path)
    sample_cfvec_coverage(recorder, env, 10)
    detail = recorder.hit_detail_by_bin_id("BIN-874")
    assert detail is not None
    assert detail["hits"] == 1
    assert detail["first_cycle"] == 10


@pytest.mark.parametrize("stem,value", [
    ("io_toIBuffer_ready", 0), ("io_toIBuffer_valid", 0),
    ("s2_fire", 0), ("s2_useUncacheFetch", 1),
    ("s2_fetchBlock_0_valid", 0), ("s2_fetchBlock_1_valid", 0),
    ("s2_fetchBlock_1_valid", None),
    ("s2_alignedInstrVec_2_valid", 0),
    ("s2_alignedInstrVec_2_blockSel", 0),
    ("s2_alignedInstrVec_2_blockSel", None),
    ("s2_alignedInstrVec_1_isCrossBlockInstr", 1),
    ("s2_alignedInstrVec_1_isCrossBlockInstr", None),
    ("io_toIBuffer_bits_foldpc_1", 123),
    ("io_toIBuffer_bits_enqEnable", 7),
])
def test_clipping_rejects_incomplete_or_leaking_output(tmp_path, stem, value):
    recorder, env, dut = _setup(tmp_path)
    if value is None:
        delattr(dut, _PREFIX + stem)
    else:
        dut.set(_PREFIX + stem, value)
    sample_cfvec_coverage(recorder, env, 10)
    assert not recorder.key_hit("ifu_data_slice", "second_block_suppressed")


@pytest.mark.parametrize("mutation", [
    "positive", "second_enqueued", "first_dropped", "first_invalid",
    "no_second", "cross_owned", "second_before_fault",
])
def test_independent_mask_checker_rejects_bad_checkpoint(mutation):
    slots = [dict(slot=0, owner=0, cross=0), dict(slot=1, owner=0, cross=0),
             dict(slot=2, owner=1, cross=0)]
    values = dict(fixed=3, enq=3, valid=7)
    if mutation == "second_enqueued":
        values.update(fixed=7, enq=7)
    elif mutation == "first_dropped":
        values.update(fixed=1, enq=1)
    elif mutation == "first_invalid":
        values["valid"] = 5
    elif mutation == "no_second":
        slots.pop()
    elif mutation == "cross_owned":
        slots[1]["cross"] = 1
    elif mutation == "second_before_fault":
        slots[0]["owner"] = 1
    if mutation == "positive":
        assert len(_assert_clip_masks(slots, 1, **values)) == 2
    else:
        with pytest.raises(AssertionError):
            _assert_clip_masks(slots, 1, **values)


@pytest.mark.parametrize("mutation", ["positive", "short", "duplicate", "skip", "wrong_bits"])
def test_recovery_checker_does_not_filter_unexpected_delivery(mutation):
    expected = [(0x8000019A, 0x13), (0x8000019E, 0x13), (0x800001A0, 0x13)]
    actual = deepcopy(expected)
    if mutation == "short":
        actual.pop()
    elif mutation == "duplicate":
        actual.insert(1, actual[0])
    elif mutation == "skip":
        actual[1] = (0x800001C0, 0x13)
    elif mutation == "wrong_bits":
        actual[1] = (actual[1][0], 0x6F)
    if mutation == "positive":
        _assert_recovery_prefix(actual, expected)
    else:
        with pytest.raises(AssertionError):
            _assert_recovery_prefix(actual, expected)


@pytest.mark.parametrize("accept,valids,expected", [
    (0, [1] * 8, []),
    (1, [1] * 8, list(range(8))),
    (1, [1, 1, 0, 0, 0, 0, 0, 0], [0, 1]),
    (1, [0] * 8, []),
])
def test_cfvec_presented_valid_is_not_a_transfer_without_acceptance(accept, valids, expected):
    assert _accepted_cfvec_slots(accept, valids) == expected


def test_current_cfvec_generated_abi_uses_decode_accept_not_pruned_ready():
    root = Path(__file__).resolve().parents[7]
    frontend = (root / "build-frontend/rtl/Frontend.sv").read_text()
    ibuffer = (root / "build-frontend/rtl/IBuffer.sv").read_text()
    source = (root / "src/main/scala/xiangshan/frontend/ibuffer/IBuffer.scala").read_text()
    assert "io_backend_toIBuf_decodeCanAccept" in frontend
    assert "io_backend_toIBuf_resumingVType" in frontend
    assert "io_backend_cfVec_0_ready" not in frontend
    assert "io_out_0_ready" not in ibuffer
    assert "assign io_out_0_valid = outputEntries_0_valid;" in ibuffer
    assert "private val decodeCanAccept = io.fromBackend.decodeCanAccept" in source
    assert ".elsewhen(decodeCanAccept)" in source
