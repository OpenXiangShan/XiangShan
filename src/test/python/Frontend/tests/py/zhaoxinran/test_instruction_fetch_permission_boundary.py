from __future__ import annotations

import os

import pytest

from env.sequences import (
    TranslationPmpPmaEntry,
    TranslationScenario,
    TranslationScenarioPhase,
    TranslationScenarioSequence,
)
from env.support import PmpPmaConfig


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_VA = 0x8020_0F00
_PA = 0x8040_0F00
_PAGE_SIZE = 0x1000
_PAYLOAD = b"\x13\x00\x00\x00" * 512


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
    priv_imode: int = 1,
    expected_fault: str | None = "instruction_access_fault",
) -> TranslationScenario:
    return TranslationScenario(
        scenario_id=scenario_id,
        va=_VA,
        pa=_PA,
        payload=_PAYLOAD,
        page_count=2,
        mode="sv39",
        priv_imode=priv_imode,
        pmp_entries=pmp_entries,
        pma_entries=pma_entries,
        expected_path="fault" if expected_fault is not None else "cacheable",
        expected_result="access_fault" if expected_fault is not None else "normal",
    )


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
    assert env.monitor.exception_mark_count > 0
    observations_after_fault = len(env.monitor.observations)
    env.step(64)
    assert len(env.monitor.observations) == observations_after_fault
    assert not env.get_errors()
