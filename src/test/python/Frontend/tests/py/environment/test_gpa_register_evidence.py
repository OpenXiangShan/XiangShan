from dataclasses import replace
import pytest
from tests.py.support.gpa_register_evidence import GpaCountSnapshot, check_adjacent_gpa_count


def sample(cycle=0, count=0, **kwargs):
    fields = dict(cycle=cycle, count=count, need_gpa=1, ptw_resp_valid=1,
                  ptw_resp_ready=1, ptw_resp_get_gpa=1, need_gpa_wire=0, need_gpa_state_clear=0)
    fields.update(kwargs)
    return GpaCountSnapshot.from_observed(fields)


def test_511_handshakes_then_registered_clear_not_512_idle_cycles():
    for n in range(511):
        evidence = check_adjacent_gpa_count(sample(n, n), sample(n + 1, n + 1))
        assert not evidence["clear_priority"]
    evidence = check_adjacent_gpa_count(sample(511, 511), sample(512, 0, need_gpa=0))
    assert evidence["clear_priority"]
    for n in range(512):
        assert check_adjacent_gpa_count(sample(n, 0, ptw_resp_valid=0), sample(n + 1, 0))["count_after"] == 0


@pytest.mark.parametrize("gate", ["need_gpa", "ptw_resp_valid", "ptw_resp_ready", "ptw_resp_get_gpa"])
def test_all_four_native_qualifiers_required(gate):
    before = sample(count=22, **{gate: 0})
    check_adjacent_gpa_count(before, sample(1, 22))
    with pytest.raises(AssertionError): check_adjacent_gpa_count(before, sample(1, 23))


@pytest.mark.parametrize("clear", ["need_gpa_wire", "need_gpa_state_clear"])
def test_start_and_consumption_or_flush_clear_beat_response_increment(clear):
    check_adjacent_gpa_count(sample(count=100, **{clear: 1}), sample(1, 0))
    with pytest.raises(AssertionError): check_adjacent_gpa_count(sample(count=100, **{clear: 1}), sample(1, 101))


@pytest.mark.parametrize("missing", list(GpaCountSnapshot.__dataclass_fields__))
def test_no_missing_probe_defaults(missing):
    fields = dict(sample().__dict__)
    fields.pop(missing)
    with pytest.raises(KeyError): GpaCountSnapshot.from_observed(fields)


@pytest.mark.parametrize("bad", [-1, 512, None])
def test_counter_width_and_unreadable_rejected(bad):
    with pytest.raises(AssertionError): sample(count=bad)


def test_observation_gap_cannot_hide_extra_handshakes():
    with pytest.raises(AssertionError, match="adjacent"):
        check_adjacent_gpa_count(sample(), sample(512, 0))
