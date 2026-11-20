import random
from types import SimpleNamespace

import pytest

from env.sequences import TranslationPmpPmaEntry, TranslationPte, TranslationScenario, TranslationSectorLane
from env.support import PC_MEMORY, PmpPmaConfig, RandomPc, fold_pc
from tests.py.support.translation_faults import assert_exact_oracle_fault
from tests.py.support.translation_scenarios import (
    ptw_responses_complete,
    randomize_ptw_response,
    relocate_translation_entry,
)


@pytest.mark.parametrize(
    "responses,required_complete,all_complete",
    (
        ([], False, False),
        ([(1, 3, 0)], True, False),
        ([(1, 3, 0), (2, 3, 0)], True, False),
        ([(1, 3, 0), (2, 3, 1)], True, True),
    ),
)
def test_ptw_responses_complete(responses, required_complete, all_complete) -> None:
    active = {
        "expected_ptw_requests": [
            {"vpn": 1, "s2xlate": 3, "get_gpa": 0},
            {"vpn": 2, "s2xlate": 3, "get_gpa": 1},
        ],
        "required_ptw_request_keys": [(1, 3, 0)],
        "responded_ptw_request_keys": responses,
    }
    assert ptw_responses_complete(active, required_only=True) is required_complete
    assert ptw_responses_complete(active) is all_complete


@pytest.mark.parametrize(
    "kind,correct_fault,record_pc,correct_fold,accepted",
    (
        ("cfvec_exception", True, 0x8020_0F00, False, True),
        ("cfvec_exception", True, 0, True, True),
        ("cfvec_exception", True, 0, False, False),
        ("cfvec_exception", True, 0x8020_0F02, True, False),
        ("cfvec_exception", False, 0x8020_0F00, True, False),
        ("cfvec_normal", True, 0x8020_0F00, True, False),
    ),
)
def test_assert_exact_oracle_fault(kind, correct_fault, record_pc, correct_fold, accepted) -> None:
    pc = 0x8020_0F00
    expected_fault = "instruction_page_fault"
    record = {
        "kind": kind,
        "fault": expected_fault if correct_fault else "instruction_access_fault",
        "pc": record_pc,
        "folded_pc": fold_pc(pc) ^ int(not correct_fold),
    }
    env = SimpleNamespace(translation_oracle=SimpleNamespace(get_stats=lambda: {"records": [record]}))
    if accepted:
        assert_exact_oracle_fault(env, pc=pc, expected_fault=expected_fault)
    else:
        with pytest.raises(AssertionError):
            assert_exact_oracle_fault(env, pc=pc, expected_fault=expected_fault)


@pytest.mark.parametrize("relocate_gpa", (False, True))
@pytest.mark.parametrize("ppn_delta", (None, -7, 7))
def test_relocation_keeps_explicit_gpa_and_sector_policies(relocate_gpa, ppn_delta) -> None:
    pmp = TranslationPmpPmaEntry("pmp", 0, PmpPmaConfig(match="napot"), 0x8040_0000, size=0x4000)
    scenario = TranslationScenario(
        scenario_id="relocate", va=0x8020_0F00, pa=0x8040_0F00, gpa=0x8060_0F00,
        payload=b"\x13\x00\x00\x00", pmp_entries=(pmp,),
        s1_sector_lanes=(TranslationSectorLane(lane=1, ppn=0x80601),),
        s1_pte=TranslationPte(asid=7),
    )
    entry = RandomPc(va=scenario.va + 0x8000, pa=scenario.pa + 0x4000, memory=PC_MEMORY["cacheable"])
    result = relocate_translation_entry(scenario, entry, relocate_gpa=relocate_gpa, s1_ppn_delta=ppn_delta)
    assert (result.va, result.pa) == (entry.va, entry.pa)
    assert result.gpa == scenario.gpa + (0x8000 if relocate_gpa else 0)
    assert result.pmp_entries[0].addr == pmp.addr + 0x4000
    assert result.s1_sector_lanes[0].ppn == 0x80601 + (ppn_delta or 0)
    assert result.s1_pte == scenario.s1_pte
    assert scenario.pmp_entries[0].addr == 0x8040_0000


@pytest.mark.parametrize("latency_range", (None, (1, 5)))
@pytest.mark.parametrize("max_limit", (None, 8))
@pytest.mark.parametrize("keep_max", (False, True))
@pytest.mark.parametrize("periodic", (False, True))
def test_ptw_timing_keeps_draw_order_and_rng_state(latency_range, max_limit, keep_max, periodic) -> None:
    scenario = TranslationScenario(
        scenario_id="timing", va=0x8020_0000, pa=0x8040_0000, payload=b"\x13\x00\x00\x00",
        ptw_response_latency=3, ptw_response_latency_max=6,
        ptw_req_ready_strategy="periodic" if periodic else "always",
        ptw_req_ready_high_cycles=2, ptw_req_ready_low_cycles=3,
    )
    rng, reference = random.Random(17), random.Random(17)
    latency = reference.randint(*latency_range) if latency_range is not None else 3
    maximum = reference.randint(latency, max(latency, max_limit)) if max_limit is not None else (6 if keep_max else None)
    high = reference.randint(1, 2) if periodic else 2
    low = reference.randint(1, 4) if periodic else 3
    result = randomize_ptw_response(
        scenario, rng, seed=17, latency_range=latency_range, latency_max_limit=max_limit,
        keep_latency_max=keep_max, randomize_ready=True,
    )
    assert (result.ptw_response_latency, result.ptw_response_latency_max) == (latency, maximum)
    assert (result.ptw_req_ready_high_cycles, result.ptw_req_ready_low_cycles) == (high, low)
    assert result.ptw_response_seed == 17
    assert rng.getstate() == reference.getstate()


@pytest.mark.parametrize("clamp", (False, True))
def test_ptw_maximum_below_latency_keeps_context_only_clamping(clamp) -> None:
    scenario = TranslationScenario(
        scenario_id="maximum", va=0x8020_0000, pa=0x8040_0000, payload=b"\x13\x00\x00\x00",
    )
    kwargs = dict(seed=17, latency_range=(3, 3), latency_max_limit=1, clamp_latency_max_limit=clamp)
    if clamp:
        result = randomize_ptw_response(scenario, random.Random(17), **kwargs)
        assert result.ptw_response_latency_max == 3
    else:
        with pytest.raises(ValueError):
            randomize_ptw_response(scenario, random.Random(17), **kwargs)
