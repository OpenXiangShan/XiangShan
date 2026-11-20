from __future__ import annotations

import pytest

from env.runtime.frontend_init import FrontendInitConfig, initialize_frontend
from env.sequences import (
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationPtwResponseOverride,
    TranslationScenario,
    TranslationScenarioBuilder,
)
from env.support import PmpPmaConfig, fold_pc, scenario_rng
from tests.py.support.cfvec import iter_valid_cfvec_slots, read_cfvec_exception_bits
from tests.py.support.dut_support import poll_until
from tests.py.support.translation_scenarios import _randomize_ptw_response_timing

CACHEABLE_CROSS_PAGE_VA = 0x8020_0000
CACHEABLE_CROSS_PAGE_PA = 0x8040_0000
CACHEABLE_CROSS_PAGE_GPA = 0x8060_0000
CACHEABLE_CROSS_PAGE_RVI_PC = CACHEABLE_CROSS_PAGE_VA + 0x1000 - 2
_CACHEABLE_CROSS_PAGE_PAYLOAD = (
    b"\x01\x00" * (0x1000 // 2 - 1)
    + b"\x13\x00\x00\x00"
    + b"\x01\x00" * 32
)

_FAULT_BITS = {
    "instruction_access_fault": 1,
    "instruction_guest_page_fault": 20,
    "instruction_page_fault": 12,
}


def poll_for_oracle_fault(env, *, max_cycles: int) -> bool:
    def fault_seen() -> bool:
        active = env.translation_oracle.get_active()
        return active is not None and bool(active.get("fault_seen"))

    return poll_until(env, fault_seen, max_cycles=max_cycles)


def assert_exact_oracle_fault(env, *, pc: int, expected_fault: str) -> None:
    exception_records = [
        record
        for record in env.translation_oracle.get_stats()["records"]
        if record["kind"] == "cfvec_exception"
    ]
    assert any(
        record["fault"] == expected_fault
        and (
            int(record["pc"]) == int(pc)
            or (
                int(record["pc"]) == 0
                and int(record["folded_pc"]) == fold_pc(pc)
            )
        )
        for record in exception_records
    ), exception_records


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
        observe = active_env.backend_from_ifu_if
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
        observe = active_env.backend_cfvec_if
        for slot in iter_valid_cfvec_slots(observe):
            exception_bits = read_cfvec_exception_bits(observe, slot)
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


def cacheable_cross_page_fault_scenario(
    *, s2xlate: int, response_field: str, expected_result: str,
    va: int = CACHEABLE_CROSS_PAGE_VA, pa: int = CACHEABLE_CROSS_PAGE_PA,
) -> TranslationScenario:
    first_page_pa = pa & ~0xFFF
    translation = {
        "mode": "bare" if s2xlate == 2 else "sv39",
        "s2xlate": s2xlate,
        "priv_virt": int(s2xlate != 0),
    }
    if s2xlate == 3:
        translation.update(
            {
                "gpa": CACHEABLE_CROSS_PAGE_GPA,
                "s1_pte": TranslationPte(asid=5, vmid=7),
                "s2_pte": TranslationPte(vmid=7),
                "vsatp_asid": 5,
                "hgatp_vmid": 7,
            }
        )
    return TranslationScenario(
        scenario_id=f"cacheable-cross-page-second-s2xlate-{s2xlate}-{response_field}",
        va=va,
        pa=pa,
        payload=_CACHEABLE_CROSS_PAGE_PAYLOAD,
        page_count=2,
        ptw_response_overrides=(
            TranslationPtwResponseOverride(
                vpn=(va >> 12) + 1,
                s2xlate=s2xlate,
                patch=((response_field, 1),),
            ),
        ),
        expected_path="fault",
        expected_result=expected_result,
        pmp_entries=tuple(
            TranslationPmpPmaEntry(
                kind="pmp", index=page,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=first_page_pa + page * 0x1000, size=0x1000,
            )
            for page in range(2)
        ),
        pma_entries=tuple(
            TranslationPmpPmaEntry(
                kind="pma", index=page,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=True),
                addr=first_page_pa + page * 0x1000, size=0x1000,
            )
            for page in range(2)
        ),
        **translation,
    )


def run_cacheable_cross_page_second_page_fault(
    env, s2xlate: int, response_field: str, expected_result: str, expected_fault: str
) -> None:
    scenario_id = f"cacheable-cross-page-second-s2xlate-{s2xlate}-{response_field}"
    scenario_key = f"zhaoxinran/translation/fault/{scenario_id}"
    addresses = {}
    if s2xlate == 2 and response_field == "s2_gpf":
        _, _, rng = scenario_rng(scenario_key)
        addresses = {
            "va": rng.randrange(0x8020_0000, 0x8028_0000, 0x2000),
            "pa": rng.randrange(0x8040_0000, 0x8048_0000, 0x2000),
        }
    scenario = cacheable_cross_page_fault_scenario(
        s2xlate=s2xlate, response_field=response_field, expected_result=expected_result, **addresses
    )
    initialize_frontend(
        env,
        FrontendInitConfig(reset_vector=scenario.va, bare_mode=False, reset_cycles=20),
    )
    scenario = _randomize_ptw_response_timing(
        env, scenario,
        scenario_key=scenario_key,
        parameters={
            "page_count": scenario.page_count,
            "expected_path": "fault",
            "expected_fault": expected_fault,
        },
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    if s2xlate == 2 and response_field == "s2_gpf":
        assert state.expected_page_outcomes[0]["ok"]
        assert state.expected_page_outcomes[1]["outcome"] == expected_fault
    gpaddr_writes = _capture_gpaddr_writes(env)
    cfvec_deliveries = _capture_cfvec_deliveries(env)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    env.translation_oracle.set_fetch_observation_ready(ready=False)
    env.backend_model.inject_redirect(scenario.va, "ctrl_redirect", delay_cycles=0)

    for _ in range(12000):
        active = env.translation_oracle.get_active()
        if active is not None and (active["fault_seen"] or env.get_errors()):
            break
        env.step(1)

    active = env.translation_oracle.get_active()
    assert int(env.ptw_agent.get_stats().get("response_override_hit_count", 0)) >= 1
    assert active is not None and active.get("fault_seen"), {
        "s2xlate": s2xlate, "response_field": response_field, "active": active,
        "ptw": env.ptw_agent.get_stats(), "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    second_page_vpn = (scenario.va >> 12) + 1
    assert any(
        int(request["vpn"]) == second_page_vpn and int(request["s2xlate"]) == s2xlate
        for request in active["expected_ptw_requests"]
    ), active["expected_ptw_requests"]
    assert active["expected_fault"] == expected_fault
    exception_records = [
        record for record in env.translation_oracle.get_stats()["records"]
        if record["kind"] == "cfvec_exception"
    ]
    assert any(
        int(record["pc"]) == scenario.va + 0x1000 - 2
        and record["fault"] == expected_fault and bool(record["cross_page"])
        for record in exception_records
    ), exception_records
    first_line_pa = scenario.pa & ~0x3F
    icache_records = env.icache_agent.get_stats().get("request_records", [])
    assert any(int(record["address"]) == first_line_pa for record in icache_records), icache_records
    if s2xlate == 2 and response_field == "s2_gpf":
        assert any(
            scenario.va <= int(record["pc"]) < scenario.va + 0x1000 - 2
            and not record["exception_bits"]
            for record in cfvec_deliveries
        ), cfvec_deliveries[-64:]
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == 0
    assert env.monitor.exception_mark_count > 0
    _, ftq_value = _assert_fault_ftq_identity(
        cfvec_deliveries, pc=scenario.va + 0x1000 - 2,
        expected_fault=expected_fault, cross_page=True,
    )
    if expected_fault == "instruction_guest_page_fault":
        assert gpaddr_writes, {"expected_fault": expected_fault}
        assert all(record["waddr"] == ftq_value for record in gpaddr_writes), gpaddr_writes
        if s2xlate == 2 and response_field == "s2_gpf":
            assert any(record["gpaddr"] == (scenario.va & ~0xFFF) + 0x1000 for record in gpaddr_writes), gpaddr_writes
    else:
        assert not gpaddr_writes, {"expected_fault": expected_fault, "gpaddr_writes": gpaddr_writes}
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()
    assert not env.get_errors()
