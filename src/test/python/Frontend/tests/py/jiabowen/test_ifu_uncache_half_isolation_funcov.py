"""Positive and fail-closed tests for BIN-922's cross-cycle identity."""

import pytest

from env.funcov.py.ifu.compact_funcov import _sample_uncache_half_isolation
from tests.py.jiabowen.test_ifu_compact_functional_coverage import _make_recorder, _PREFIX


def _samples():
    old_pc = 0x80000FFE >> 1
    initial = dict(
        uncacheRedirect_valid=1, uncacheNeedResend=1, uncachePc_addr=old_pc,
        io_fromFtq_redirect_valid=0, wbRedirect_valid=0,
        s2_valid_valid=1, s2_reqIsUncache=1,
    )
    initial["uncacheUnit.__Vtogcov__io_resp_bits_uncacheData"] = 0x313
    saved = dict(uncacheRedirect_valid=0, s0_prevEndIsHalfRvi=1,
                 s1_prevEndHalfRviInfo_bits_pc_addr=old_pc,
                 s1_prevEndHalfRviInfo_bits_data=0x313)
    redirect = dict(
        io_fromFtq_redirect_valid=1, s2_prevEndIsHalfRviInfo_valid=1,
        s2_prevEndIsHalfRviInfo_bits_pc_addr=old_pc,
        s2_prevEndIsHalfRviInfo_bits_data=0x313,
        s2_fetchBlock_0_startVAddr_addr=old_pc + 1,
        s0_flush=1, s1_flush=1, s2_flush=1, io_toIBuffer_valid=0,
        s2_fetchBlock_0_ftqIdx_flag=0, s2_fetchBlock_0_ftqIdx_value=7,
    )
    cleared = dict(io_fromFtq_redirect_valid=0, s0_prevEndIsHalfRvi=0,
                   s1_prevEndHalfRviInfo_bits_pc_addr=0,
                   s1_prevEndHalfRviInfo_bits_data=0, s1_valid=0, s2_valid_valid=0)
    fired = dict(s0_fire=1)
    recovered = dict(s0_fire=0, s1_valid=1, s1_flush=0, s1_reqIsUncache=0,
                     s1_prevEndHalfRviInfo_valid=0, s1_fire=1,
                     s1_prevIBufEnqPtrDup_dup_0_value=0,
                     s1_fetchBlock_0_startVAddr_addr=0x80003000 >> 1,
                     s1_alignedInstrPcVec_0_addr=0x80003000 >> 1,
                     s1_alignedInstrVec_0_data=0x02400393,
                     s1_fetchBlock_0_ftqIdx_flag=0, s1_fetchBlock_0_ftqIdx_value=8)
    return [initial, saved, redirect, cleared, fired, recovered]


def _drive(recorder, dut, samples, *, mutate=None, missing=None, reset_at=None):
    for cycle, fields in enumerate(samples, 1):
        for stem, value in fields.items():
            dut.set(_PREFIX + stem, value)
        if mutate and cycle == mutate[0]:
            dut.set(_PREFIX + mutate[1], mutate[2])
        if missing and cycle == missing[0]:
            delattr(dut, _PREFIX + missing[1])
        if cycle == reset_at:
            recorder._clear_transient_sampling_state()
        _sample_uncache_half_isolation(recorder, dut, cycle)


def test_uncache_half_isolation_matches_saved_pc_data_through_backend_recovery(tmp_path):
    recorder, _env, dut, _memory = _make_recorder(tmp_path)
    _drive(recorder, dut, _samples())
    assert recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_024")
    evidence = recorder.hits[("ifu_v3_pipeline_owner_model", "verified_leaf_event",
                              "owner_leaf_024")].evidence[-1]["observations"]
    assert evidence["half_data"] == 0x313
    assert evidence["source_cycle"] < evidence["redirect_cycle"] < evidence["recovery_cycle"]
    assert evidence["recovery_pc"] == 0x80003000
    assert evidence["signal_paths"]


@pytest.mark.parametrize("mutate", [
    (1, "uncacheNeedResend", 0),
    (2, "s1_prevEndHalfRviInfo_bits_data", 0x13),
    (3, "s2_prevEndIsHalfRviInfo_bits_pc_addr", 0x80000FFC >> 1),
    (3, "s2_fetchBlock_0_startVAddr_addr", 0x80002000 >> 1),
    (3, "io_fromFtq_redirect_valid", 0),
    (4, "s1_prevEndHalfRviInfo_bits_data", 0x313),
    (5, "s0_prevEndIsHalfRvi", 1),
    (5, "wbRedirect_valid", 1),
    (5, "uncacheRedirect_valid", 1),
    (5, "io_fromFtq_redirect_valid", 1),
    (6, "s1_prevEndHalfRviInfo_valid", 1),
    (6, "s1_alignedInstrPcVec_0_addr", 0x80000FFE >> 1),
    (6, "s1_reqIsUncache", 1),
])
def test_uncache_half_isolation_rejects_wrong_or_interrupted_transaction(tmp_path, mutate):
    recorder, _env, dut, _memory = _make_recorder(tmp_path)
    _drive(recorder, dut, _samples(), mutate=mutate)
    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_024")


@pytest.mark.parametrize("missing", [
    (1, "uncachePc_addr"),
    (2, "s1_prevEndHalfRviInfo_bits_data"),
    (3, "s2_prevEndIsHalfRviInfo_valid"),
    (6, "s1_prevEndHalfRviInfo_valid"),
])
def test_uncache_half_isolation_missing_probe_is_visible_and_cannot_hit(tmp_path, missing):
    recorder, _env, dut, _memory = _make_recorder(tmp_path)
    _drive(recorder, dut, _samples(), missing=missing)
    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_024")
    assert any(r.get("reason") == "missing_probe" and r.get("missing") == missing[1]
               for r in recorder.risk_observations)


def test_uncache_half_isolation_reset_discards_pending_transaction(tmp_path):
    recorder, _env, dut, _memory = _make_recorder(tmp_path)
    _drive(recorder, dut, _samples(), reset_at=5)
    assert not recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_024")
