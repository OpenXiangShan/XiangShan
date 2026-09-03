from __future__ import annotations

import os

import pytest

from env.sequences import (
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationPtwResponseOverride,
    TranslationScenario,
    TranslationScenarioBuilder,
)
from env.support import PmpPmaConfig


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_VA = 0x8020_0F00
_PA = 0x8040_0F00
_GPA = 0x8060_0F00
_PAGE_SIZE = 0x1000
_PAYLOAD = b"\x13\x00\x00\x00" * 512
_CACHEABLE_CROSS_PAGE_VA = 0x8020_0000
_CACHEABLE_CROSS_PAGE_PA = 0x8040_0000
_CACHEABLE_CROSS_PAGE_GPA = 0x8060_0000
_CACHEABLE_CROSS_PAGE_RVI_PC = _CACHEABLE_CROSS_PAGE_VA + _PAGE_SIZE - 2
_CACHEABLE_CROSS_PAGE_PAYLOAD = (
    b"\x01\x00" * (_PAGE_SIZE // 2 - 1)
    + b"\x13\x00\x00\x00"
    + b"\x01\x00" * 32
)
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
                    "pc": int(observe.cfvec_pc[slot].value),
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


def _scenario(scenario_id: str, **kwargs) -> TranslationScenario:
    return TranslationScenario(
        scenario_id=scenario_id,
        va=_VA,
        pa=_PA,
        payload=_PAYLOAD,
        page_count=2,
        expected_path="fault",
        **kwargs,
    )


_FAULT_CASES = (
    pytest.param(
        _scenario(
            "translation-fault-stage1-only-response-page",
            mode="sv39",
            s2xlate=1,
            priv_virt=1,
            s1_pf=1,
            expected_result="page_fault",
        ),
        "instruction_page_fault",
        id="stage1-only-response-page-fault",
    ),
    pytest.param(
        _scenario(
            "translation-fault-stage1-only-response-access",
            mode="sv39",
            s2xlate=1,
            priv_virt=1,
            s1_af=1,
            expected_result="access_fault",
        ),
        "instruction_access_fault",
        id="stage1-only-response-access-fault",
    ),
    pytest.param(
        _scenario(
            "translation-fault-superpage-response-page",
            mode="sv39",
            s1_pte=TranslationPte(level=1),
            s1_pf=1,
            expected_result="page_fault",
        ),
        "instruction_page_fault",
        id="superpage-response-page-fault",
    ),
    pytest.param(
        _scenario(
            "translation-fault-superpage-response-access",
            mode="sv39",
            s1_pte=TranslationPte(level=1),
            s1_af=1,
            expected_result="access_fault",
        ),
        "instruction_access_fault",
        id="superpage-response-access-fault",
    ),
    pytest.param(
        _scenario(
            "translation-fault-stage1-pte-invalid",
            mode="sv39",
            s1_pte=TranslationPte(v=0),
            expected_result="page_fault",
        ),
        "instruction_page_fault",
        id="stage1-pte-invalid-fault",
    ),
    pytest.param(
        _scenario(
            "translation-fault-stage1-pte-accessed-clear",
            mode="sv39",
            s1_pte=TranslationPte(a=0),
            expected_result="page_fault",
        ),
        "instruction_page_fault",
        id="stage1-pte-accessed-clear-fault",
    ),
    pytest.param(
        _scenario(
            "translation-fault-only-stage2-guest-page",
            mode="bare",
            stage2_mode="sv39",
            s2xlate=2,
            priv_virt=1,
            s2_gpf=1,
            expected_result="guest_fault",
        ),
        "instruction_guest_page_fault",
        id="only-stage2-guest-page-fault",
    ),
    pytest.param(
        _scenario(
            "translation-fault-only-stage2-guest-access",
            mode="bare",
            stage2_mode="sv39",
            s2xlate=2,
            priv_virt=1,
            s2_gaf=1,
            expected_result="access_fault",
        ),
        "instruction_access_fault",
        id="only-stage2-guest-access-fault",
    ),
    pytest.param(
        _scenario(
            "translation-fault-all-stage-guest-page",
            mode="sv39",
            stage2_mode="sv39",
            s2xlate=3,
            gpa=_GPA,
            s1_pte=TranslationPte(asid=5, vmid=7),
            s2_pte=TranslationPte(vmid=7),
            vsatp_asid=5,
            hgatp_vmid=7,
            priv_virt=1,
            s2_gpf=1,
            expected_result="guest_fault",
        ),
        "instruction_guest_page_fault",
        id="all-stage-guest-page-fault",
    ),
    pytest.param(
        _scenario(
            "translation-fault-all-stage-guest-access",
            mode="sv39",
            stage2_mode="sv39",
            s2xlate=3,
            gpa=_GPA,
            s1_pte=TranslationPte(asid=5, vmid=7),
            s2_pte=TranslationPte(vmid=7),
            vsatp_asid=5,
            hgatp_vmid=7,
            priv_virt=1,
            s2_gaf=1,
            expected_result="access_fault",
        ),
        "instruction_access_fault",
        id="all-stage-guest-access-fault",
    ),
    *(
        pytest.param(
            _scenario(
                f"translation-fault-all-stage-{s1_name}-with-{s2_name}",
                mode="sv39",
                stage2_mode="sv39",
                s2xlate=3,
                gpa=_GPA,
                s1_pte=TranslationPte(asid=5, vmid=7),
                s2_pte=TranslationPte(vmid=7),
                vsatp_asid=5,
                hgatp_vmid=7,
                priv_virt=1,
                s1_pf=int(s1_name == "response-page"),
                s1_af=int(s1_name == "response-access"),
                s2_gpf=int(s2_name == "guest-page"),
                s2_gaf=int(s2_name == "guest-access"),
                expected_result=expected_result,
            ),
            expected_fault,
            id=f"all-stage-{s1_name}-with-{s2_name}",
        )
        for s1_name, s1_expected_result, s1_expected_fault in (
            ("response-page", "page_fault", "instruction_page_fault"),
            ("response-access", "access_fault", "instruction_access_fault"),
        )
        for s2_name, expected_result, expected_fault in (
            ("leaf", s1_expected_result, s1_expected_fault),
            ("guest-page", s1_expected_result, s1_expected_fault),
            ("guest-access", "access_fault", "instruction_access_fault"),
        )
    ),
)


@pytest.mark.parametrize("scenario,expected_fault", _FAULT_CASES)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_address_translation_fault(env, scenario: TranslationScenario, expected_fault: str) -> None:
    env.initialize(reset_vector=scenario.va, bare_mode=False)
    gpaddr_writes = _capture_gpaddr_writes(env)
    cfvec_deliveries = _capture_cfvec_deliveries(env)
    state = TranslationScenarioBuilder(env).build(scenario)

    def arm_before_reset_release() -> None:
        env.monitor.clear()
        env.monitor.set_expected_pc(scenario.va)
        env.arm_translation_scenario(state, page_indexes=(0,))

    env.reset(before_release=arm_before_reset_release)

    for _ in range(6000):
        active = env.translation_oracle.get_active()
        if active is not None:
            expected_responses = {
                (
                    int(request["vpn"]),
                    int(request["s2xlate"]),
                    int(request["get_gpa"]),
                )
                for request in active["expected_ptw_requests"]
            }
            observed_responses = {
                tuple(int(field) for field in response)
                for response in active["responded_ptw_request_keys"]
            }
            if active["fault_seen"] and expected_responses.issubset(observed_responses):
                break
        if env.get_errors():
            break
        env.step(1)

    assert state.expected_page_outcomes[0]["ok"] is False
    assert state.expected_page_outcomes[0]["outcome"] == expected_fault
    env.assert_translation_scenario()
    _, ftq_value = _assert_fault_ftq_identity(
        cfvec_deliveries,
        pc=scenario.va,
        expected_fault=expected_fault,
        cross_page=False,
    )
    if expected_fault == "instruction_guest_page_fault":
        assert gpaddr_writes, {"scenario": scenario.scenario_id}
        assert all(record["waddr"] == ftq_value for record in gpaddr_writes), gpaddr_writes
    else:
        assert not gpaddr_writes, {
            "scenario": scenario.scenario_id,
            "gpaddr_writes": gpaddr_writes,
        }
    assert not env.get_errors()


def _cacheable_cross_page_fault_scenario(
    *,
    s2xlate: int,
    response_field: str,
    expected_result: str,
) -> TranslationScenario:
    first_page_pa = _CACHEABLE_CROSS_PAGE_PA & ~(_PAGE_SIZE - 1)
    translation = {
        "mode": "bare" if s2xlate == 2 else "sv39",
        "s2xlate": s2xlate,
        "priv_virt": int(s2xlate != 0),
    }
    if s2xlate == 3:
        translation.update(
            {
                "gpa": _CACHEABLE_CROSS_PAGE_GPA,
                "s1_pte": TranslationPte(asid=5, vmid=7),
                "s2_pte": TranslationPte(vmid=7),
                "vsatp_asid": 5,
                "hgatp_vmid": 7,
            }
        )
    return TranslationScenario(
        scenario_id=f"cacheable-cross-page-second-s2xlate-{s2xlate}-{response_field}",
        va=_CACHEABLE_CROSS_PAGE_VA,
        pa=_CACHEABLE_CROSS_PAGE_PA,
        payload=_CACHEABLE_CROSS_PAGE_PAYLOAD,
        page_count=2,
        ptw_response_overrides=(
            TranslationPtwResponseOverride(
                vpn=(_CACHEABLE_CROSS_PAGE_VA >> 12) + 1,
                s2xlate=s2xlate,
                patch=((response_field, 1),),
            ),
        ),
        expected_path="fault",
        expected_result=expected_result,
        pmp_entries=tuple(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=page,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=first_page_pa + page * _PAGE_SIZE,
                size=_PAGE_SIZE,
            )
            for page in range(2)
        ),
        pma_entries=tuple(
            TranslationPmpPmaEntry(
                kind="pma",
                index=page,
                config=PmpPmaConfig(
                    match="napot",
                    read=True,
                    write=True,
                    execute=True,
                    cacheable=True,
                ),
                addr=first_page_pa + page * _PAGE_SIZE,
                size=_PAGE_SIZE,
            )
            for page in range(2)
        ),
        **translation,
    )


@pytest.mark.parametrize(
    "s2xlate,response_field,expected_result,expected_fault",
    _CROSS_PAGE_FAULT_CASES,
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_cacheable_cross_page_second_page_translation_fault(
    env,
    s2xlate: int,
    response_field: str,
    expected_result: str,
    expected_fault: str,
) -> None:
    env.initialize(reset_vector=_CACHEABLE_CROSS_PAGE_VA, bare_mode=False)
    scenario = _cacheable_cross_page_fault_scenario(
        s2xlate=s2xlate,
        response_field=response_field,
        expected_result=expected_result,
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    gpaddr_writes = _capture_gpaddr_writes(env)
    cfvec_deliveries = _capture_cfvec_deliveries(env)
    env.monitor.clear()
    env.monitor.set_expected_pc(_CACHEABLE_CROSS_PAGE_VA)
    env.arm_translation_scenario(state)
    env.translation_oracle.set_fetch_observation_ready(ready=False)
    env.backend_model.inject_redirect(
        _CACHEABLE_CROSS_PAGE_VA,
        "ctrl_redirect",
        delay_cycles=0,
    )

    for _ in range(12000):
        active = env.translation_oracle.get_active()
        if active is not None and (active["fault_seen"] or env.get_errors()):
            break
        env.step(1)

    active = env.translation_oracle.get_active()
    assert int(env.ptw_agent.get_stats().get("response_override_hit_count", 0)) >= 1
    assert active is not None and active.get("fault_seen"), {
        "s2xlate": s2xlate,
        "response_field": response_field,
        "active": active,
        "ptw": env.ptw_agent.get_stats(),
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    second_page_vpn = (_CACHEABLE_CROSS_PAGE_VA >> 12) + 1
    assert any(
        int(request["vpn"]) == second_page_vpn
        and int(request["s2xlate"]) == s2xlate
        for request in active["expected_ptw_requests"]
    ), active["expected_ptw_requests"]
    assert active["expected_fault"] == expected_fault
    exception_records = [
        record
        for record in env.translation_oracle.get_stats()["records"]
        if record["kind"] == "cfvec_exception"
    ]
    assert any(
        int(record["pc"]) == _CACHEABLE_CROSS_PAGE_RVI_PC
        and record["fault"] == expected_fault
        and bool(record["cross_page"])
        for record in exception_records
    ), exception_records
    first_line_pa = _CACHEABLE_CROSS_PAGE_PA & ~0x3F
    icache_records = env.icache_agent.get_stats().get("request_records", [])
    assert any(int(record["address"]) == first_line_pa for record in icache_records), icache_records
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == 0
    assert env.monitor.exception_mark_count > 0
    _, ftq_value = _assert_fault_ftq_identity(
        cfvec_deliveries,
        pc=_CACHEABLE_CROSS_PAGE_RVI_PC,
        expected_fault=expected_fault,
        cross_page=True,
    )
    if expected_fault == "instruction_guest_page_fault":
        assert gpaddr_writes, {"expected_fault": expected_fault}
        assert all(record["waddr"] == ftq_value for record in gpaddr_writes), gpaddr_writes
    else:
        assert not gpaddr_writes, {
            "expected_fault": expected_fault,
            "gpaddr_writes": gpaddr_writes,
        }
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()
    assert not env.get_errors()
