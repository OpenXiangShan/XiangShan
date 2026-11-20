"""Native fromFtq has six index bits; cfVec's wrap bit is separate."""
from types import SimpleNamespace

import pytest

from tests.py.support.mbtb_training_request import decoded_source_registration


@pytest.mark.parametrize("flag", [0, 1])
def test_six_bit_native_registration_keeps_cfvec_wrap_metadata(flag):
    entry = SimpleNamespace(ftq_flag=flag, ftq_value=7, ftq_offset=1, pc=0x80000040)
    model = SimpleNamespace(_ftq_start_pc_cache={7: 0x80000040})
    assert decoded_source_registration(model, entry, {"source_pc": 0x80000040}) == 0x80000040
    assert entry.ftq_flag == flag and entry.ftq_value == 7


@pytest.mark.parametrize("fault", ["missing", "wrong_pc", "wrong_offset", "wrong_index", "invalid_flag", "invented_wrap_key"])
def test_unobserved_or_inconsistent_native_registration_fails(fault):
    entry = SimpleNamespace(ftq_flag=1, ftq_value=7, ftq_offset=1, pc=0x80000040)
    cache = {7: 0x80000040}
    if fault == "missing":
        cache.clear()
    elif fault == "wrong_pc":
        cache[7] += 4
    elif fault == "wrong_offset":
        entry.ftq_offset += 1
    elif fault == "wrong_index":
        entry.ftq_value = 64
    elif fault == "invalid_flag":
        entry.ftq_flag = 2
    else:
        cache = {71: 0x80000040}
    with pytest.raises(AssertionError):
        decoded_source_registration(SimpleNamespace(_ftq_start_pc_cache=cache), entry, {"source_pc": 0x80000040})
