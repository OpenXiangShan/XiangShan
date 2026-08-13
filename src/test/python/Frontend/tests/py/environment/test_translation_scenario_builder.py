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
    assert len(state.pmp_writes) == 1
    assert len(state.pma_writes) == 1


def test_builder_composes_all_stage_mapping_from_gpa_to_pa() -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="sv39-all-stage",
        va=0x0000_0000_8020_0004,
        gpa=0x0000_0000_8040_0004,
        pa=0x0000_0000_8060_0004,
        payload=b"\x13\x00\x00\x00",
        s2xlate=3,
        s1_pte=TranslationPte(v=1, r=1, x=1, asid=5),
        s2_pte=TranslationPte(v=1, r=1, x=1, n=1, vmid=7),
        satp_asid=5,
        vsatp_asid=5,
        hgatp_vmid=7,
        priv_virt=1,
    )

    TranslationScenarioBuilder(env).build(scenario)

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
