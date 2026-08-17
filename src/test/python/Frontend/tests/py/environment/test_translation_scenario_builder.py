from __future__ import annotations

import pytest

from env.core.frontend_env import FrontendEnv
from env.runtime.dut_factory import FakeDUTFrontend
from env.sequences import (
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationScenario,
    TranslationScenarioBuilder,
)
from env.support import PmpPmaConfig


def _env() -> FrontendEnv:
    return FrontendEnv(FakeDUTFrontend(), register_callbacks=False)


def test_builder_applies_one_sv39_description_to_page_table_memory_and_context() -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="sv39-nc-fetch",
        va=0x0000_0000_8020_0000,
        pa=0x0000_0000_8040_0000,
        payload=b"\x01\x00" * 32,
        s1_pte=TranslationPte(v=1, r=1, x=1, a=1, n=1, pbmt=1, asid=3),
        satp_asid=3,
        satp_ppn=0x81000,
        priv_imode=1,
        expected_path="uncache",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, execute=True),
                addr=0x8040_0000,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, execute=True, cacheable=False),
                addr=0x8040_0000,
                size=0x1000,
            ),
        ),
    )

    state = TranslationScenarioBuilder(env).build(scenario)

    assert state.scenario is scenario
    assert state.translation_epoch == env.translation_epoch
    assert env.memory.read_block(scenario.pa, len(scenario.payload)) == scenario.payload
    mapped = env.page_table.pte_map[scenario.va >> 12]
    assert (mapped.ppn, mapped.asid, mapped.n, mapped.pbmt) == (scenario.pa >> 12, 3, 1, 1)
    assert env.page_table.mode == "sv39"
    assert env.ptw_agent.response_source == "model"
    assert env.dut.io_tlbCsr_satp_mode.value == 8
    assert env.dut.io_tlbCsr_satp_asid.value == 3
    assert state.context["changed"] == {"satp": True, "vsatp": False, "hgatp": False, "priv_virt": True}
    assert state.expected_ptw_request == {
        "scenario_id": "sv39-nc-fetch",
        "vpn": scenario.va >> 12,
        "s2xlate": 0,
        "get_gpa": 0,
    }
    assert state.expected_outcome["pa"] == scenario.pa
    assert state.expected_outcome["outcome"] == "normal"
    assert state.expected_outcome["fetch_path"] == "uncache"
    assert len(state.pmp_writes) == 1
    assert len(state.pma_writes) == 1


def test_builder_applies_declared_ptw_response_timing() -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="sv39-ptw-response-timing",
        va=0x8020_0000,
        pa=0x8040_0000,
        payload=b"\x13\x00\x00\x00",
        ptw_response_latency=5,
        ptw_response_latency_max=9,
        ptw_response_seed=23,
    )

    TranslationScenarioBuilder(env).build(scenario)

    stats = env.ptw_agent.get_stats()
    assert stats["latency_min"] == 5
    assert stats["latency_max"] == 9
    assert stats["req_ready_strategy"] == "always"


def test_builder_rejects_invalid_ptw_response_latency_range() -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="sv39-invalid-ptw-response-timing",
        va=0x8020_0000,
        pa=0x8040_0000,
        payload=b"\x13\x00\x00\x00",
        ptw_response_latency=5,
        ptw_response_latency_max=4,
    )

    with pytest.raises(ValueError, match="latency_max"):
        TranslationScenarioBuilder(env).build(scenario)


def test_environment_rejects_arming_a_state_after_translation_context_changed() -> None:
    env = _env()
    state = TranslationScenarioBuilder(env).build(
        TranslationScenario(
            scenario_id="sv39-stale-scenario-state",
            va=0x8020_0000,
            pa=0x8040_0000,
            payload=b"\x13\x00\x00\x00",
        )
    )

    env.update_translation_context(satp_asid=1)

    with pytest.raises(ValueError, match="cannot arm translation scenario from epoch"):
        env.arm_translation_scenario(state)


def test_builder_derives_each_page_fetch_path_from_pma() -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="sv39-cross-page-pma-path",
        va=0x8020_0FFE,
        pa=0x8040_0FFE,
        payload=b"\x13\x00\x00\x00",
        page_count=2,
        pma_entries=(
            TranslationPmpPmaEntry(
                "pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_0000, size=0x1000
            ),
            TranslationPmpPmaEntry(
                "pma", 1, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=False), 0x8040_1000, size=0x1000
            ),
        ),
    )

    state = TranslationScenarioBuilder(env).build(scenario)

    assert [(outcome["va"], outcome["pa"], outcome["expected_path"]) for outcome in state.expected_page_outcomes] == [
        (0x8020_0FFE, 0x8040_0FFE, "cacheable"),
        (0x8020_1000, 0x8040_1000, "uncache"),
    ]


def test_builder_composes_all_stage_mapping_from_gpa_to_pa() -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="sv39-all-stage",
        va=0x0000_0000_8020_0004,
        gpa=0x0000_0000_8040_0004,
        pa=0x0000_0000_8060_0004,
        payload=b"\x13\x00\x00\x00",
        s2xlate=3,
        s1_pte=TranslationPte(v=1, r=1, x=1, asid=5, vmid=7),
        s2_pte=TranslationPte(v=1, r=1, x=1, n=1, vmid=7),
        satp_asid=5,
        vsatp_asid=5,
        hgatp_vmid=7,
        priv_virt=1,
    )

    state = TranslationScenarioBuilder(env).build(scenario)

    pa, ok, metadata = env.page_table.translate(scenario.va)
    assert ok is True
    assert pa == scenario.pa
    assert metadata["stage1_pa"] == scenario.gpa & ~0xFFF
    assert metadata["stage2_pa"] == scenario.pa & ~0xFFF
    response = env.page_table.build_ptw_resp(scenario.va >> 12, s2xlate=3)
    assert response["s1_entry_n"] == 0
    assert response["s2_entry_n"] == 1
    assert env.dut.io_tlbCsr_vsatp_mode.value == 8
    assert env.dut.io_tlbCsr_hgatp_mode.value == 8
    assert env.dut.io_tlbCsr_hgatp_vmid.value == 7
    assert state.expected_outcome["pa"] == scenario.pa
    assert state.expected_outcome["outcome"] == "normal"


def test_builder_binds_page_fault_outcome_without_rewriting_the_pte() -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="sv39-x-denied",
        va=0x80200000,
        pa=0x80400000,
        payload=b"\x13\x00\x00\x00",
        s1_pte=TranslationPte(v=1, r=1, x=0, a=1),
        expected_path="fault",
        expected_result="page_fault",
    )

    state = TranslationScenarioBuilder(env).build(scenario)
    response = env.page_table.build_ptw_resp(scenario.va >> 12)

    assert response["s1_entry_v"] == 1
    assert response["s1_entry_perm_x"] == 0
    assert response["s1_pf"] == 0
    assert state.expected_outcome["ok"] is False
    assert state.expected_outcome["outcome"] == "instruction_page_fault"
    assert state.expected_outcome["reason"] == "stage1_execute_denied"


@pytest.mark.parametrize(
    "pmp_execute,pma_execute,expected_reason",
    [
        (False, True, "pmp_execute_denied"),
        (True, False, "pma_execute_denied"),
        (False, False, "pmp_pma_execute_denied"),
    ],
)
def test_builder_binds_explicit_pmp_pma_execute_denial_as_access_fault(
    pmp_execute: bool,
    pma_execute: bool,
    expected_reason: str,
) -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id=f"permission-{expected_reason}",
        va=0x80200000,
        pa=0x80400000,
        payload=b"\x13\x00\x00\x00",
        pmp_entries=(
            TranslationPmpPmaEntry(
                "pmp", 0, PmpPmaConfig(match="napot", read=True, execute=pmp_execute), 0x80400000, size=0x1000
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                "pma", 0, PmpPmaConfig(match="napot", read=True, execute=pma_execute, cacheable=True), 0x80400000, size=0x1000
            ),
        ),
    )

    state = TranslationScenarioBuilder(env).build(scenario)

    assert state.expected_outcome["outcome"] == "instruction_access_fault"
    assert state.expected_outcome["reason"] == expected_reason
    assert state.expected_outcome["permission"]["execute_allowed"] is False


@pytest.mark.parametrize(
    "boundary_kind",
    ["pmp", "pma"],
)
def test_builder_rejects_na4_entries_for_the_current_frontend_platform(boundary_kind: str) -> None:
    env = _env()
    pmp_entry = TranslationPmpPmaEntry(
        "pmp",
        0,
        PmpPmaConfig(match="na4" if boundary_kind == "pmp" else "napot", read=True, execute=True),
        0x80400000,
        size=None if boundary_kind == "pmp" else 0x1000,
    )
    pma_entry = TranslationPmpPmaEntry(
        "pma",
        0,
        PmpPmaConfig(match="na4" if boundary_kind == "pma" else "napot", read=True, execute=True, cacheable=True),
        0x80400000,
        size=None if boundary_kind == "pma" else 0x1000,
    )
    scenario = TranslationScenario(
        scenario_id=f"{boundary_kind}-na4-unavailable",
        va=0x80200000,
        pa=0x80400000,
        payload=b"\x13\x00\x00\x00",
        pmp_entries=(pmp_entry,),
        pma_entries=(pma_entry,),
    )

    with pytest.raises(ValueError, match="NA4 is unavailable"):
        TranslationScenarioBuilder(env).build(scenario)

    assert env.page_table.pte_map == {}
    assert env.memory.mem == {}
    assert env.csr_write_log == []


@pytest.mark.parametrize(
    "scenario_id,s1_pf,s1_af,s2_gpf,s2_gaf,expected_outcome",
    [
        ("atp-140", 1, 0, 0, 0, "instruction_page_fault"),
        ("atp-141", 0, 1, 0, 0, "instruction_access_fault"),
        ("atp-142", 1, 0, 1, 0, "instruction_page_fault"),
        ("atp-143", 1, 0, 0, 1, "instruction_access_fault"),
        ("atp-144", 0, 1, 1, 0, "instruction_access_fault"),
        ("atp-145", 0, 1, 0, 1, "instruction_access_fault"),
    ],
)
def test_builder_composes_all_stage_response_fault_priority(
    scenario_id: str,
    s1_pf: int,
    s1_af: int,
    s2_gpf: int,
    s2_gaf: int,
    expected_outcome: str,
) -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id=scenario_id,
        va=0x80200000,
        gpa=0x80400000,
        pa=0x80600000,
        payload=b"\x13\x00\x00\x00",
        s2xlate=3,
        s1_pte=TranslationPte(asid=5, vmid=7),
        s2_pte=TranslationPte(vmid=7),
        vsatp_asid=5,
        hgatp_vmid=7,
        priv_virt=1,
        s1_pf=s1_pf,
        s1_af=s1_af,
        s2_gpf=s2_gpf,
        s2_gaf=s2_gaf,
        expected_path="fault",
    )

    state = TranslationScenarioBuilder(env).build(scenario)
    response = env.page_table.build_ptw_resp(scenario.va >> 12, s2xlate=3)

    assert tuple(response[name] for name in ("s1_pf", "s1_af", "s2_gpf", "s2_gaf")) == (
        s1_pf,
        s1_af,
        s2_gpf,
        s2_gaf,
    )
    assert state.expected_outcome["outcome"] == expected_outcome


@pytest.mark.parametrize(
    "scenario,match",
    [
        (
            TranslationScenario("both-s1-faults", 0x80200000, 0x80400000, b"\x13", s1_pf=1, s1_af=1),
            "both s1_pf and s1_af",
        ),
        (
            TranslationScenario("both-s2-faults", 0x80200000, 0x80400000, b"\x13", s2xlate=2, s2_gpf=1, s2_gaf=1),
            "both s2_gpf and s2_gaf",
        ),
        (
            TranslationScenario("s1-fault-only-s2", 0x80200000, 0x80400000, b"\x13", s2xlate=2, s1_pf=1),
            "only-stage2",
        ),
        (
            TranslationScenario("s2-fault-only-s1", 0x80200000, 0x80400000, b"\x13", s2_gpf=1),
            "without G-stage",
        ),
    ],
)
def test_builder_rejects_unrepresentable_response_fault_combinations(scenario, match: str) -> None:
    with pytest.raises(ValueError, match=match):
        TranslationScenarioBuilder(_env()).build(scenario)


@pytest.mark.parametrize(
    "scenario,match",
    [
        (
            TranslationScenario("sv48", 0x80200000, 0x80400000, b"\x13", mode="sv48"),
            "only Sv39",
        ),
        (
            TranslationScenario("noncanonical", 0x0000_0080_0000_0000, 0x80400000, b"\x13"),
            "non-canonical",
        ),
        (
            TranslationScenario("superpage", 0x80200000, 0x80400000, b"\x13", s1_pte=TranslationPte(level=1)),
            "level",
        ),
        (
            TranslationScenario("short-pages", 0x80200FF0, 0x80400FF0, b"\x13" * 17),
            "page_count",
        ),
        (
            TranslationScenario(
                "all-stage-vmid-mismatch",
                0x80200000,
                0x80600000,
                b"\x13",
                gpa=0x80400000,
                s2xlate=3,
                s1_pte=TranslationPte(asid=5, vmid=0),
                s2_pte=TranslationPte(vmid=7),
                vsatp_asid=5,
                hgatp_vmid=7,
                priv_virt=1,
            ),
            "VMID",
        ),
    ],
)
def test_builder_rejects_unsupported_or_incomplete_scenarios_before_mutating_env(scenario, match: str) -> None:
    env = _env()

    with pytest.raises(ValueError, match=match):
        TranslationScenarioBuilder(env).build(scenario)

    assert env.page_table.pte_map == {}
    assert env.memory.mem == {}
    assert env.csr_write_log == []


def test_builder_rejects_mismatched_pmp_entry_collection() -> None:
    env = _env()
    scenario = TranslationScenario(
        "wrong-pmp-kind",
        0x80200000,
        0x80400000,
        b"\x13",
        pmp_entries=(
            TranslationPmpPmaEntry("pma", 0, PmpPmaConfig(match="off"), 0x80400000),
        ),
    )

    with pytest.raises(ValueError, match="pmp_entries"):
        TranslationScenarioBuilder(env).build(scenario)


def test_builder_rejects_invalid_pmp_encoding_before_mutating_env() -> None:
    env = _env()
    scenario = TranslationScenario(
        "invalid-pmp",
        0x80200000,
        0x80400000,
        b"\x13",
        pmp_entries=(
            TranslationPmpPmaEntry("pmp", 0, PmpPmaConfig(match="napot"), 0x80400004, size=0x1000),
        ),
    )

    with pytest.raises(ValueError, match="align"):
        TranslationScenarioBuilder(env).build(scenario)

    assert env.page_table.pte_map == {}
    assert env.memory.mem == {}
    assert env.csr_write_log == []
