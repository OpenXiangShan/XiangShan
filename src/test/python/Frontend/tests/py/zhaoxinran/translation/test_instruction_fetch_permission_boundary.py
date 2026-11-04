from __future__ import annotations

import os

import pytest

from env.runtime.frontend_init import FrontendInitConfig
from env.sequences import (
    TranslationPte,
    TranslationScenario,
    TranslationScenarioPhase,
    TranslationScenarioSequence,
)
from env.support import PmpPmaConfig, fold_pc
from tests.py.support.translation_scenarios import _randomize_ptw_response_timing
from tests.py.support.backend_fault_recovery import (
    _PA,
    _PAGE_SIZE,
    _entry,
    _scenario,
    _run_backend_fault_redirect_recovery,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"


pytestmark = pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)


def _assert_exact_oracle_fault(env, *, pc: int, expected_fault: str) -> None:
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


_PMP_ALLOW = _entry(
    "pmp",
    0,
    PmpPmaConfig(match="napot", read=True, execute=True),
    _PA & ~(_PAGE_SIZE - 1),
    0x2000,
)
_PMA_ALLOW = _entry(
    "pma",
    0,
    PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True),
    _PA & ~(_PAGE_SIZE - 1),
    0x2000,
)
_PMA_UNCACHE_ALLOW = _entry(
    "pma",
    0,
    PmpPmaConfig(match="napot", read=True, execute=True, cacheable=False),
    _PA & ~(_PAGE_SIZE - 1),
    0x2000,
)
_PMP_DENY = _entry(
    "pmp",
    0,
    PmpPmaConfig(match="napot", read=True, execute=False),
    _PA & ~(_PAGE_SIZE - 1),
    0x2000,
)
_PMA_UNCACHE_DENY = _entry(
    "pma",
    0,
    PmpPmaConfig(match="napot", read=True, execute=False, cacheable=False),
    _PA & ~(_PAGE_SIZE - 1),
    0x2000,
)
_PMA_DENY = _entry(
    "pma",
    0,
    PmpPmaConfig(match="napot", read=True, execute=False, cacheable=True),
    _PA & ~(_PAGE_SIZE - 1),
    0x2000,
)


_PERMISSION_CASES = (
    pytest.param(
        _scenario(
            "fetch-permission-pmp-napot-deny",
            pmp_entries=(
                _entry(
                    "pmp",
                    0,
                    PmpPmaConfig(match="napot", read=True, execute=False),
                    _PA & ~(_PAGE_SIZE - 1),
                    0x2000,
                ),
            ),
            pma_entries=(_PMA_ALLOW,),
        ),
        "instruction_access_fault",
        id="pmp-napot-execute-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pma-napot-deny",
            pmp_entries=(_PMP_ALLOW,),
            pma_entries=(
                _entry(
                    "pma",
                    0,
                    PmpPmaConfig(match="napot", read=True, execute=False, cacheable=True),
                    _PA & ~(_PAGE_SIZE - 1),
                    0x2000,
                ),
            ),
        ),
        "instruction_access_fault",
        id="pma-napot-execute-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pma-tor-deny",
            pmp_entries=(_PMP_ALLOW,),
            pma_entries=(
                _entry("pma", 0, PmpPmaConfig(match="off"), _PA & ~(_PAGE_SIZE - 1)),
                _entry(
                    "pma",
                    1,
                    PmpPmaConfig(match="tor", read=True, execute=False, cacheable=True),
                    (_PA & ~(_PAGE_SIZE - 1)) + 0x2000,
                ),
            ),
        ),
        "instruction_access_fault",
        id="pma-tor-execute-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-overlap-first-match",
            pmp_entries=(
                _entry(
                    "pmp",
                    0,
                    PmpPmaConfig(match="napot", read=True, execute=False),
                    _PA & ~(_PAGE_SIZE - 1),
                    0x2000,
                ),
                _entry(
                    "pmp",
                    1,
                    PmpPmaConfig(match="napot", read=True, execute=True),
                    _PA & ~(_PAGE_SIZE - 1),
                    0x2000,
                ),
            ),
            pma_entries=(_PMA_ALLOW,),
        ),
        "instruction_access_fault",
        id="pmp-overlap-first-match",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-locked-mmode-deny",
            pmp_entries=(
                _entry(
                    "pmp",
                    0,
                    PmpPmaConfig(match="napot", read=True, execute=False, locked=True),
                    _PA & ~(_PAGE_SIZE - 1),
                    0x2000,
                ),
            ),
            pma_entries=(_PMA_ALLOW,),
            priv_imode=2,
        ),
        "instruction_access_fault",
        id="pmp-locked-mmode-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-only-uncache-deny",
            pmp_entries=(_PMP_DENY,),
            pma_entries=(_PMA_UNCACHE_ALLOW,),
            s1_pte=TranslationPte(pbmt=1),
            ptw_machine_pbmte=1,
        ),
        "instruction_access_fault",
        id="pmp-only-uncache-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pma-only-uncache-deny",
            pmp_entries=(_PMP_ALLOW,),
            pma_entries=(_PMA_UNCACHE_DENY,),
            s1_pte=TranslationPte(pbmt=1),
            ptw_machine_pbmte=1,
        ),
        "instruction_access_fault",
        id="pma-only-uncache-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-both-uncache-deny",
            pmp_entries=(_PMP_DENY,),
            pma_entries=(_PMA_UNCACHE_DENY,),
            s1_pte=TranslationPte(pbmt=1),
            ptw_machine_pbmte=1,
        ),
        "instruction_access_fault",
        id="both-uncache-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-both-cacheable-deny",
            pmp_entries=(_PMP_DENY,),
            pma_entries=(_PMA_DENY,),
        ),
        "instruction_access_fault",
        id="both-cacheable-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-napot-lower-boundary",
            va=0x8020_1000,
            pa=0x8040_1000,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=False), 0x8040_1000, 0x1000),),
            pma_entries=(_entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_1000, 0x1000),),
        ),
        "instruction_access_fault",
        id="napot-lower-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-napot-upper-boundary",
            va=0x8020_1C00,
            pa=0x8040_1C00,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=False), 0x8040_1000, 0x1000),),
            pma_entries=(_entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_1000, 0x1000),),
        ),
        "instruction_access_fault",
        id="napot-upper-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-tor-interior-boundary",
            pmp_entries=(_PMP_ALLOW,),
            pma_entries=(
                _entry("pma", 0, PmpPmaConfig(match="off"), _PA & ~(_PAGE_SIZE - 1)),
                _entry("pma", 1, PmpPmaConfig(match="tor", read=True, execute=False, cacheable=True), (_PA & ~(_PAGE_SIZE - 1)) + 0x2000),
            ),
        ),
        "instruction_access_fault",
        id="tor-interior-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-tor-lower-boundary",
            va=0x8020_0000,
            pa=0x8040_0000,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True), 0x8040_0000, 0x1000),),
            pma_entries=(
                _entry("pma", 0, PmpPmaConfig(match="off"), 0x8040_0000),
                _entry("pma", 1, PmpPmaConfig(match="tor", read=True, execute=False, cacheable=True), 0x8040_1000),
                _entry("pma", 2, PmpPmaConfig(match="tor", read=True, execute=False, cacheable=True), 0x8040_2000),
            ),
        ),
        "instruction_access_fault",
        id="tor-lower-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-tor-upper-boundary",
            va=0x8020_1000,
            pa=0x8040_1000,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True), 0x8040_0000, 0x4000),),
            pma_entries=(
                _entry("pma", 0, PmpPmaConfig(match="off"), 0x8040_0000),
                _entry("pma", 1, PmpPmaConfig(match="tor", read=True, execute=False, cacheable=True), 0x8040_1000),
                _entry("pma", 2, PmpPmaConfig(match="tor", read=True, execute=False, cacheable=True), 0x8040_2000),
            ),
        ),
        "instruction_access_fault",
        id="tor-upper-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-tor-lower-boundary",
            va=0x8020_0000,
            pa=0x8040_0000,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(
                _entry("pmp", 0, PmpPmaConfig(match="off"), 0x8040_0000),
                _entry("pmp", 1, PmpPmaConfig(match="tor", read=True, execute=False), 0x8040_1000),
            ),
            pma_entries=(_entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_0000, 0x1000),),
        ),
        "instruction_access_fault",
        id="pmp-tor-lower-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-tor-interior-boundary",
            pmp_entries=(
                _entry("pmp", 0, PmpPmaConfig(match="off"), _PA & ~(_PAGE_SIZE - 1)),
                _entry("pmp", 1, PmpPmaConfig(match="tor", read=True, execute=False), (_PA & ~(_PAGE_SIZE - 1)) + 0x2000),
            ),
            pma_entries=(_PMA_ALLOW,),
        ),
        "instruction_access_fault",
        id="pmp-tor-interior-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-tor-upper-boundary",
            va=0x8020_1000,
            pa=0x8040_1000,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(
                _entry("pmp", 0, PmpPmaConfig(match="tor", read=True, execute=False), 0x8040_1000),
                _entry("pmp", 1, PmpPmaConfig(match="tor", read=True, execute=False), 0x8040_2000),
            ),
            pma_entries=(_entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_0000, 0x4000),),
        ),
        "instruction_access_fault",
        id="pmp-tor-upper-boundary-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-napot-upper-boundary-allow",
            va=0x8020_1C00,
            pa=0x8040_1C00,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True), 0x8040_1000, 0x1000),),
            pma_entries=(_entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_1000, 0x1000),),
            expected_fault=None,
        ),
        None,
        id="napot-upper-boundary-allow",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-tor-lower-boundary-allow",
            va=0x8020_0000,
            pa=0x8040_0000,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(
                _entry("pmp", 0, PmpPmaConfig(match="off"), 0x8040_0000),
                _entry("pmp", 1, PmpPmaConfig(match="tor", read=True, execute=True), 0x8040_1000),
            ),
            pma_entries=(_entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_0000, 0x1000),),
            expected_fault=None,
        ),
        None,
        id="tor-lower-boundary-allow",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-tor-upper-boundary-allow",
            va=0x8020_1000,
            pa=0x8040_1000,
            payload=b"\x13\x00\x00\x00" * 256,
            page_count=1,
            pmp_entries=(
                _entry("pmp", 0, PmpPmaConfig(match="tor", read=True, execute=False), 0x8040_1000),
                _entry("pmp", 1, PmpPmaConfig(match="tor", read=True, execute=True), 0x8040_2000),
            ),
            pma_entries=(_entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_0000, 0x4000),),
            expected_fault=None,
        ),
        None,
        id="tor-upper-boundary-allow",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-tor-interior-boundary-allow",
            pmp_entries=(
                _entry("pmp", 0, PmpPmaConfig(match="off"), _PA & ~(_PAGE_SIZE - 1)),
                _entry("pmp", 1, PmpPmaConfig(match="tor", read=True, execute=True), (_PA & ~(_PAGE_SIZE - 1)) + 0x2000),
            ),
            pma_entries=(_PMA_ALLOW,),
            expected_fault=None,
        ),
        None,
        id="tor-interior-boundary-allow",
    ),
)


@pytest.mark.parametrize("scenario,expected_fault", _PERMISSION_CASES)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_instruction_fetch_permission_boundary(
    env,
    scenario: TranslationScenario,
    expected_fault: str,
) -> None:
    scenario = _randomize_ptw_response_timing(
        env,
        scenario,
        scenario_key=f"zhaoxinran/translation/permission/{scenario.scenario_id}",
        parameters={
            "page_count": scenario.page_count,
            "priv_imode": scenario.priv_imode,
            "expected_path": scenario.expected_path,
            "expected_fault": expected_fault,
        },
    )
    sequence = TranslationScenarioSequence(
        actions=(TranslationScenarioPhase(scenario=scenario, page_indexes=(0,)),)
    )
    sequence.initialize_first_phase(env)
    phases = [record for record in sequence.run(env) if record["kind"] == "phase"]

    assert len(phases) == 1
    state = phases[0]["state"]
    expected_outcome = "normal" if expected_fault is None else expected_fault
    assert state.expected_page_outcomes[0]["outcome"] == expected_outcome
    if expected_fault is None:
        assert any(int(observation.pc) == scenario.va for observation in env.monitor.observations)
    else:
        _assert_exact_oracle_fault(env, pc=scenario.va, expected_fault=expected_fault)
    assert not env.get_errors()


@pytest.mark.parametrize(
    ("fault_kind", "fault_bit", "redirect_faults"),
    (
        pytest.param("iaf", 1, {"backend_iaf": 1}, id="instruction-access-fault"),
        pytest.param("ipf", 12, {"backend_ipf": 1}, id="instruction-page-fault"),
        pytest.param("igpf", 20, {"backend_igpf": 1}, id="instruction-guest-page-fault"),
    ),
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_backend_fault_redirect_recovery(
    env,
    fault_kind: str,
    fault_bit: int,
    redirect_faults: dict[str, int],
) -> None:
    return _run_backend_fault_redirect_recovery(
        env,
        fault_kind=fault_kind,
        fault_bit=fault_bit,
        redirect_faults=redirect_faults,
    )


_PMP_LOCK_MODE_CASES = (
    pytest.param(
        _scenario(
            "fetch-permission-pmp-locked-mmode-allow",
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True, locked=True), _PA & ~0x1FFF, 0x2000),),
            pma_entries=(_PMA_ALLOW,),
            priv_imode=2,
            expected_fault=None,
        ),
        id="pmp-locked-mmode-allow",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-locked-sumode-allow",
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True, locked=True), _PA & ~0x1FFF, 0x2000),),
            pma_entries=(_PMA_ALLOW,),
            expected_fault=None,
        ),
        id="pmp-locked-sumode-allow",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-locked-sumode-deny",
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=False, locked=True), _PA & ~0x1FFF, 0x2000),),
            pma_entries=(_PMA_ALLOW,),
        ),
        id="pmp-locked-sumode-deny",
    ),
    pytest.param(
        _scenario(
            "fetch-permission-pmp-unlocked-mmode-bypass",
            pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=False), _PA & ~0x1FFF, 0x2000),),
            pma_entries=(_PMA_ALLOW,),
            priv_imode=2,
            expected_fault=None,
        ),
        id="pmp-unlocked-mmode-bypass",
    ),
)


@pytest.mark.parametrize("scenario", _PMP_LOCK_MODE_CASES)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_pmp_lock_mode(env, scenario: TranslationScenario) -> None:
    expected_fault = (
        "instruction_access_fault" if scenario.expected_path == "fault" else None
    )
    scenario = _randomize_ptw_response_timing(
        env,
        scenario,
        scenario_key=f"zhaoxinran/translation/permission/{scenario.scenario_id}",
        parameters={
            "page_count": scenario.page_count,
            "priv_imode": scenario.priv_imode,
            "expected_path": scenario.expected_path,
            "expected_fault": expected_fault,
        },
    )
    sequence = TranslationScenarioSequence(actions=(TranslationScenarioPhase(scenario=scenario, page_indexes=(0,)),))
    sequence.initialize_first_phase(env)
    phases = [record for record in sequence.run(env) if record["kind"] == "phase"]

    assert len(phases) == 1
    if scenario.expected_path == "fault":
        assert env.monitor.exception_mark_count > 0
    else:
        assert any(int(observation.pc) == scenario.va for observation in env.monitor.observations)
    assert not env.get_errors()
