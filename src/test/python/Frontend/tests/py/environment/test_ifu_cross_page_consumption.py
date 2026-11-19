"""Cross-page metadata is checked only when its exception lane is consumed."""
from copy import deepcopy
import pytest
from tests.py.jiabowen.test_ifu_exception_metadata_v3_dut import _accepted_cross_page_records


def _record():
    return {"io_toIBuffer_valid": 1, "io_toIBuffer_ready": 1,
            "io_toIBuffer_bits_exceptionCrossPage": 1,
            "io_toIBuffer_bits_exceptionType_value": 1,
            "io_toIBuffer_bits_valid": 0xffffffff,
            "io_toIBuffer_bits_enqEnable": 0x7fffffff,
            "io_toIBuffer_bits_exceptionMask": 0x80000000}


def test_unconsumed_first_window_cannot_replace_later_accepted_exception():
    inactive = _record()
    consumed = deepcopy(inactive)
    consumed["io_toIBuffer_bits_enqEnable"] = 0xffffffff
    before = deepcopy(inactive)
    assert _accepted_cross_page_records([inactive, consumed]) == [consumed]
    assert inactive == before
    assert _accepted_cross_page_records([inactive]) == []


@pytest.mark.parametrize("lane", [0, 31, 34, 35])
def test_consumed_high_or_low_lane_still_requires_full_exception_scope(lane):
    record = _record()
    for field in ("valid", "enqEnable", "exceptionMask"):
        record["io_toIBuffer_bits_" + field] = 1 << lane
    assert _accepted_cross_page_records([record]) == [record]


@pytest.mark.parametrize("field", ["io_toIBuffer_valid", "io_toIBuffer_ready",
                                  "io_toIBuffer_bits_exceptionCrossPage"])
def test_no_acceptance_or_cross_page_flag_is_never_a_fault_witness(field):
    record = _record()
    record[field] = 0
    assert _accepted_cross_page_records([record]) == []


@pytest.mark.parametrize("field,value", [("valid", 1 << 36), ("enqEnable", -1),
                                        ("exceptionMask", 1 << 36), ("exceptionMask", True),
                                        ("valid", 0)])
def test_malformed_or_invalid_enqueue_cannot_be_silently_filtered(field, value):
    record = _record()
    record["io_toIBuffer_bits_" + field] = value
    with pytest.raises(AssertionError):
        _accepted_cross_page_records([record])
