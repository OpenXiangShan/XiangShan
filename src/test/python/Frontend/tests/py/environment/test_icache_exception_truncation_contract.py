"""Before/after count and exact-consumer identity for the denied-refill test."""

from copy import deepcopy

import pytest

from tests.py.jiabowen.test_icache_mainpipe_miss_response import (
    _check_bin907_truncation,
    _is_bin907_truncation_candidate,
)


def _witness(raw=16, lane=0):
    source = {"cycle": 10, "s1_valid": 1, "s1_fire": 1, "s1_flush": 0,
              "s1_req_uncache": 0, "s1_merged_exception": 3,
              "s1_raw_candidate_count": raw, "s1_spec_instr_count": raw,
              "s1_instr_count": 1, "s1_ftq_flag": 0, "s1_ftq_value": 4}
    delivery = {"cycle": 12, "s2_valid": 1, "to_ibuffer_valid": 1,
                "to_ibuffer_ready": 1, "s2_instr_count": 1, "s2_exception": 3,
                "to_ibuffer_exception": 3, "s2_ftq_flag": 0, "s2_ftq_value": 4,
                "to_ibuffer_enq": 1 << lane, "to_ibuffer_valid_mask": 1 << lane,
                "exception_mask": 1 << lane}
    return source, delivery


@pytest.mark.parametrize("raw", [2, 16, 32])
@pytest.mark.parametrize("lane", [0, 17, 35])
def test_speculative_raw_count_is_independent_of_single_effective_delivery(raw, lane):
    source, delivery = _witness(raw, lane)
    _check_bin907_truncation(source, delivery, (0, 4))


@pytest.mark.parametrize("side,field,value", [
    (0, "s1_spec_instr_count", 1), (0, "s1_spec_instr_count", 15),
    (0, "s1_raw_candidate_count", 1), (0, "s1_instr_count", 2),
    (0, "s1_ftq_flag", 1), (0, "s1_ftq_value", 5),
    (0, "s1_valid", 0), (0, "s1_fire", 0), (0, "s1_flush", 1),
    (0, "s1_req_uncache", 1), (0, "s1_merged_exception", 0),
    (1, "cycle", 10), (1, "s2_valid", 0), (1, "to_ibuffer_valid", 0),
    (1, "to_ibuffer_ready", 0), (1, "s2_instr_count", 2),
    (1, "s2_exception", 1), (1, "to_ibuffer_exception", 1),
    (1, "s2_ftq_flag", 1), (1, "s2_ftq_value", 5),
    (1, "to_ibuffer_enq", 0), (1, "to_ibuffer_enq", 3),
    (1, "to_ibuffer_enq", 1 << 36), (1, "to_ibuffer_valid_mask", 3),
    (1, "exception_mask", 0), (1, "exception_mask", 2),
])
def test_wrong_stage_count_or_unconsumed_or_foreign_lane_is_rejected(side, field, value):
    witnesses = list(_witness())
    witnesses[side][field] = value
    with pytest.raises(AssertionError):
        _check_bin907_truncation(*witnesses, (0, 4))


def test_younger_s1_ftq_cannot_replace_original_witness_when_old_s2_delivers():
    old_source, old_delivery = _witness()
    younger = deepcopy(old_source)
    younger.update(cycle=12, s1_ftq_value=5)
    assert _is_bin907_truncation_candidate(old_source, (0, 4))
    assert not _is_bin907_truncation_candidate(younger, (0, 4))
    _check_bin907_truncation(old_source, old_delivery, (0, 4))
    with pytest.raises(AssertionError):
        _check_bin907_truncation(younger, old_delivery, (0, 4))
