from __future__ import annotations

import os

import pytest

from env.core.transactions import RedirectTxn
from env.sequences import (
    InjectRedirectSequence,
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationScenario,
    TranslationScenarioBuilder,
    TranslationScenarioPhase,
    TranslationScenarioSequence,
)
from env.support import PmpPmaConfig


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_VA = 0x8020_0F00
_PA = 0x8040_0F00
_PAGE_SIZE = 0x1000
_PAYLOAD = b"\x13\x00\x00\x00" * 512
_TRAP_VA = 0x8020_3F00
_TRAP_PA = 0x8040_3F00


def _entry(
    kind: str,
    index: int,
    config: PmpPmaConfig,
    addr: int,
    size: int | None = None,
) -> TranslationPmpPmaEntry:
    return TranslationPmpPmaEntry(kind, index, config, addr, size=size)


def _scenario(
    scenario_id: str,
    *,
    pmp_entries: tuple[TranslationPmpPmaEntry, ...],
    pma_entries: tuple[TranslationPmpPmaEntry, ...],
    va: int = _VA,
    pa: int = _PA,
    payload: bytes = _PAYLOAD,
    page_count: int = 2,
    priv_imode: int = 1,
    expected_fault: str | None = "instruction_access_fault",
    s1_pte: TranslationPte = TranslationPte(),
    ptw_machine_pbmte: int | None = None,
) -> TranslationScenario:
    return TranslationScenario(
        scenario_id=scenario_id,
        va=va,
        pa=pa,
        payload=payload,
        page_count=page_count,
        mode="sv39",
        priv_imode=priv_imode,
        s1_pte=s1_pte,
        ptw_machine_pbmte=ptw_machine_pbmte,
        pmp_entries=pmp_entries,
        pma_entries=pma_entries,
        expected_path="fault" if expected_fault is not None else "cacheable",
        expected_result="access_fault" if expected_fault is not None else "normal",
    )


def _wait_until(env, predicate, *, description: str, max_cycles: int = 2000) -> None:
    for _ in range(max_cycles):
        if predicate():
            return
        env.step(1)
    if not predicate():
        raise AssertionError(f"timed out waiting for {description}")


def _translation_complete(env) -> bool:
    active = env.translation_oracle.get_active()
    if active is None:
        return False
    expected_ptw = {
        (int(request["vpn"]), int(request["s2xlate"]), int(request["get_gpa"]))
        for request in active["expected_ptw_requests"]
    }
    return expected_ptw.issubset({tuple(map(int, item)) for item in active["responded_ptw_request_keys"]}) and len(
        active["expected_fetches"]
    ) == len(active["observed_fetch_pas"])


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
    sequence = TranslationScenarioSequence(
        actions=(TranslationScenarioPhase(scenario=scenario, page_indexes=(0,)),)
    )
    sequence.initialize_first_phase(env)
    phases = [record for record in sequence.run(env) if record["kind"] == "phase"]

    assert len(phases) == 1
    state = phases[0]["state"]
    assert state.expected_page_outcomes[0]["outcome"] == expected_fault
    if expected_fault is None:
        assert any(int(observation.pc) == scenario.va for observation in env.monitor.observations)
    else:
        assert env.monitor.exception_mark_count > 0
    assert not env.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_pmp_fault_redirects_from_exception_cfvec_to_trap(env) -> None:
    region_base = _PA & ~0x3FFF
    fault = _scenario(
        "fetch-permission-pmp-fault-redirect-source",
        pmp_entries=(
            _entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=False), region_base, 0x4000),
        ),
        pma_entries=(
            _entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), region_base, 0x4000),
        ),
    )
    trap = _scenario(
        "fetch-permission-pmp-fault-trap-target",
        va=_TRAP_VA,
        pa=_TRAP_PA,
        payload=_PAYLOAD,
        page_count=2,
        expected_fault=None,
        pmp_entries=(
            _entry("pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True), region_base, 0x4000),
        ),
        pma_entries=(
            _entry("pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), region_base, 0x4000),
        ),
    )
    builder = TranslationScenarioBuilder(env)

    env.initialize(reset_vector=fault.va, bare_mode=False)
    fault_state = builder.build(fault)
    env.monitor.clear()
    env.monitor.set_expected_pc(fault.va)
    env.arm_translation_scenario(fault_state, page_indexes=(0,))
    _wait_until(env, lambda: env.monitor.exception_mark_count > 0, description="PMP access-fault cfVec")
    env.assert_translation_scenario()

    trap_state = builder.build(trap)
    env.monitor.set_expected_pc(trap.va)
    env.arm_translation_scenario(trap_state)
    env.translation_oracle.set_fetch_observation_ready(ready=False)
    assert InjectRedirectSequence(
        RedirectTxn(
            source_pc=fault.va,
            target_pc=trap.va,
            reason="pmp-access-fault-trap",
            level=1,
            backend_iaf=1,
        )
    ).run(env)
    env.translation_oracle.set_fetch_observation_ready(ready=True)
    _wait_until(env, lambda: _translation_complete(env), description="trap-target translation")
    env.assert_translation_scenario()
    assert not env.get_errors()


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
    sequence = TranslationScenarioSequence(actions=(TranslationScenarioPhase(scenario=scenario, page_indexes=(0,)),))
    sequence.initialize_first_phase(env)
    phases = [record for record in sequence.run(env) if record["kind"] == "phase"]

    assert len(phases) == 1
    if scenario.expected_path == "fault":
        assert env.monitor.exception_mark_count > 0
    else:
        assert any(int(observation.pc) == scenario.va for observation in env.monitor.observations)
    assert not env.get_errors()
