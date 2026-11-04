from __future__ import annotations

import os
import pytest

from env.runtime.frontend_init import FrontendInitConfig, initialize_frontend
from env.sequences import (
    TranslationPte,
    TranslationScenario,
    TranslationScenarioBuilder,
)
from tests.py.support.translation_faults import (
    _CROSS_PAGE_FAULT_CASES,
    _assert_fault_ftq_identity,
    _capture_cfvec_deliveries,
    _capture_gpaddr_writes,
    run_cacheable_cross_page_second_page_fault,
)
from tests.py.support.translation_scenarios import _randomize_ptw_response_timing


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_VA = 0x8020_0F00
_PA = 0x8040_0F00
_GPA = 0x8060_0F00
_PAGE_SIZE = 0x1000
_PAYLOAD = b"\x13\x00\x00\x00" * 512
pytestmark = pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)


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
    scenario = _randomize_ptw_response_timing(
        env,
        scenario,
        scenario_key=f"zhaoxinran/translation/fault/{scenario.scenario_id}",
        parameters={
            "page_count": scenario.page_count,
            "expected_path": "fault",
            "expected_fault": expected_fault,
        },
    )
    initialize_frontend(
        env, FrontendInitConfig(reset_vector=scenario.va, bare_mode=False, reset_cycles=20)
    )
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
    run_cacheable_cross_page_second_page_fault(
        env, s2xlate, response_field, expected_result, expected_fault
    )
