"""Offline checker API for adjacent native TLB counter snapshots, never a driver.

Only certifies the count register equation. It does not certify reachability,
GPA ownership, downstream progress, or any canonical coverage bin.
"""
from dataclasses import dataclass


@dataclass(frozen=True)
class GpaCountSnapshot:
    cycle: int
    need_gpa: int
    count: int
    ptw_resp_valid: int
    ptw_resp_ready: int
    ptw_resp_get_gpa: int
    need_gpa_wire: int
    need_gpa_state_clear: int

    @classmethod
    def from_observed(cls, observed):
        # Required fields have no defaults: inventory/runtime must supply all.
        result = cls(**{key: observed[key] for key in cls.__dataclass_fields__})
        assert type(result.cycle) is int and result.cycle >= 0
        assert type(result.count) is int and 0 <= result.count < 512
        for key in cls.__dataclass_fields__:
            if key not in {"cycle", "count"}:
                assert type(getattr(result, key)) is int and getattr(result, key) in {0, 1}, key
        return result


def check_adjacent_gpa_count(before: GpaCountSnapshot, after: GpaCountSnapshot):
    assert after.cycle == before.cycle + 1, "counter evidence must span adjacent registered boundaries"
    qualifying = bool(before.need_gpa and before.ptw_resp_valid
                      and before.ptw_resp_ready and before.ptw_resp_get_gpa)
    clear = bool(before.need_gpa_wire or before.need_gpa_state_clear or before.count == 511)
    expected = 0 if clear else before.count + int(qualifying)
    assert after.count == expected, {"cycle": before.cycle, "expected_count": expected,
                                    "observed_count": after.count, "qualifying_response": qualifying}
    return {"cycle": before.cycle, "next_cycle": after.cycle, "qualifying_response": qualifying,
            "clear_priority": clear, "count_before": before.count, "count_after": after.count,
            "scope": "counter_register_equation_only", "runtime_qualified": False,
            "canonical_promotion": False}
