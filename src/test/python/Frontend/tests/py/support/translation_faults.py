from __future__ import annotations

import pytest

_FAULT_BITS = {
    "instruction_access_fault": 1,
    "instruction_guest_page_fault": 20,
    "instruction_page_fault": 12,
}

def _read_exception_bit(signal) -> int:
    value = getattr(signal, "value", None)
    return 0 if value is None else int(value)

_CROSS_PAGE_FAULT_CASES = (
    pytest.param(0, "s1_pf", "page_fault", "instruction_page_fault", id="no-stage-page-fault"),
    pytest.param(0, "s1_af", "access_fault", "instruction_access_fault", id="no-stage-access-fault"),
    pytest.param(1, "s1_pf", "page_fault", "instruction_page_fault", id="only-stage1-page-fault"),
    pytest.param(1, "s1_af", "access_fault", "instruction_access_fault", id="only-stage1-access-fault"),
    pytest.param(2, "s2_gpf", "guest_fault", "instruction_guest_page_fault", id="only-stage2-guest-page-fault"),
    pytest.param(2, "s2_gaf", "access_fault", "instruction_access_fault", id="only-stage2-guest-access-fault"),
    pytest.param(3, "s1_pf", "page_fault", "instruction_page_fault", id="all-stage-vs-page-fault"),
    pytest.param(3, "s1_af", "access_fault", "instruction_access_fault", id="all-stage-vs-access-fault"),
    pytest.param(3, "s2_gpf", "guest_fault", "instruction_guest_page_fault", id="all-stage-g-page-fault"),
    pytest.param(3, "s2_gaf", "access_fault", "instruction_access_fault", id="all-stage-g-access-fault"),
)

def _capture_gpaddr_writes(env) -> list[dict]:
    records: list[dict] = []

    def capture(cycle: int, active_env) -> None:
        observe = active_env.backend_observe_if
        if int(observe.gpaddr_mem_wen.value) != 1:
            return
        records.append(
            {
                "cycle": int(cycle),
                "waddr": int(observe.gpaddr_mem_waddr.value),
                "gpaddr": int(observe.gpaddr_mem_gpaddr.value),
                "is_for_vs_nonleaf_pte": int(
                    observe.gpaddr_mem_is_for_vs_nonleaf_pte.value
                ),
            }
        )

    env.register_cycle_observer(capture)
    return records

def _capture_cfvec_deliveries(env) -> list[dict]:
    records: list[dict] = []

    def capture(cycle: int, active_env) -> None:
        observe = active_env.backend_observe_if
        for slot in range(8):
            if int(observe.cfvec_valid[slot].value) != 1:
                continue
            exception_bits = tuple(
                bit
                for bit in range(24)
                if _read_exception_bit(observe.cfvec_exception_vec[slot][bit]) == 1
            )
            records.append(
                {
                    "cycle": int(cycle),
                    "pc": active_env.observed_cfvec_pc(slot),
                    "ftq_flag": int(observe.cfvec_ftq_ptr_flag[slot].value),
                    "ftq_value": int(observe.cfvec_ftq_ptr_value[slot].value),
                    "cross_page": bool(observe.cfvec_cross_page_ipf_fix[slot].value),
                    "exception_bits": exception_bits,
                }
            )

    env.register_cycle_observer(capture)
    return records

def _assert_fault_ftq_identity(
    records: list[dict],
    *,
    pc: int,
    expected_fault: str,
    cross_page: bool,
) -> tuple[int, int]:
    expected_bit = _FAULT_BITS[expected_fault]
    target = [record for record in records if int(record["pc"]) == int(pc)]
    assert target, {"missing_fault_pc": hex(int(pc)), "records": records[-64:]}
    assert all(
        record["exception_bits"] == (expected_bit,)
        and bool(record["cross_page"]) is bool(cross_page)
        for record in target
    ), target
    identities = {(record["ftq_flag"], record["ftq_value"]) for record in target}
    assert len(identities) == 1, target
    return next(iter(identities))
