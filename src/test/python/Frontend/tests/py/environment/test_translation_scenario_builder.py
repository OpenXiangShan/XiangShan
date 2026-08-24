from __future__ import annotations

import pytest

from env.core.frontend_env import FrontendEnv
from env.model.ptw_response_source import PTWRequestSnapshot
from env.nemu import ptw_adapter_template
from env.runtime.dut_factory import FakeDUTFrontend
from env.sequences import (
    TranslationPmpPmaEntry,
    TranslationPermissionProbe,
    TranslationPte,
    TranslationPtwResponseOverride,
    TranslationScenario,
    TranslationScenarioBuilder,
    TranslationSectorLane,
)
from env.support import PmpPmaConfig


def _env() -> FrontendEnv:
    return FrontendEnv(FakeDUTFrontend(), register_callbacks=False)


def _ptw_request(vpn: int, *, s2xlate: int = 0, get_gpa: int = 0) -> PTWRequestSnapshot:
    return PTWRequestSnapshot(
        sequence_id=1,
        cycle=1,
        vpn=vpn,
        s2xlate=s2xlate,
        get_gpa=get_gpa,
        memidx_is_ld=0,
        memidx_is_st=0,
        memidx_idx=0,
        priv_imode=1,
        satp_mode=8,
        vsatp_mode=0,
        hgatp_mode=0,
        sfence_valid=0,
        sfence_bits_rs1=0,
        sfence_bits_rs2=0,
        sfence_bits_addr=0,
        sfence_bits_id=0,
        sfence_bits_hv=0,
        sfence_bits_hg=0,
    )


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


def test_builder_applies_declared_ptw_transport_policy() -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="sv39-ptw-transport-policy",
        va=0x8020_0000,
        pa=0x8040_0000,
        payload=b"\x13\x00\x00\x00",
        ptw_response_latency=2,
        ptw_response_latency_max=5,
        ptw_response_seed=19,
        ptw_req_ready_strategy="periodic",
        ptw_req_ready_high_cycles=2,
        ptw_req_ready_low_cycles=3,
        ptw_flush_pending_on_sfence=False,
        ptw_strict_bare_mode=True,
    )

    TranslationScenarioBuilder(env).build(scenario)

    stats = env.ptw_agent.get_stats()
    assert stats["latency_min"] == 2
    assert stats["latency_max"] == 5
    assert stats["req_ready_strategy"] == "periodic"
    assert stats["strict_bare_mode"] is True
    assert env.ptw_agent.flush_pending_on_sfence is False


@pytest.mark.parametrize(
    "field,value,expected_outcome,expected_fault_field",
    [
        ("ptw_machine_pbmte", 0, "instruction_page_fault", "s1_pf"),
        ("ptw_machine_pbmte", 1, "normal", "s1_pf"),
    ],
)
def test_builder_models_nonvirtual_pbmte_before_driving_the_frontend_response(
    field, value, expected_outcome, expected_fault_field
) -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id=f"pbmte-{field}-{value}",
        va=0x8020_0000,
        pa=0x8040_0000,
        payload=b"\x13\x00\x00\x00",
        s1_pte=TranslationPte(pbmt=1),
        **{field: value},
    )

    state = TranslationScenarioBuilder(env).build(scenario)
    response = env.page_table.build_ptw_resp(state.expected_ptw_request["vpn"])

    assert state.expected_outcome["outcome"] == expected_outcome
    assert response[expected_fault_field] == (1 if value == 0 else 0)


def test_builder_models_gstage_pbmte_as_a_guest_page_fault() -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="gstage-pbmte-disabled",
        va=0x8020_0000,
        gpa=0x8030_0000,
        pa=0x8040_0000,
        payload=b"\x13\x00\x00\x00",
        s2xlate=2,
        s2_pte=TranslationPte(pbmt=1),
        ptw_machine_pbmte=0,
        expected_path="fault",
        expected_result="guest_fault",
    )

    state = TranslationScenarioBuilder(env).build(scenario)
    response = env.page_table.build_ptw_resp(state.expected_ptw_request["vpn"], s2xlate=2)

    assert state.expected_outcome["outcome"] == "instruction_guest_page_fault"
    assert response["s2_gpf"] == 1


def test_builder_resets_pbmte_policy_between_scenarios() -> None:
    env = _env()
    disabled = TranslationScenario(
        scenario_id="pbmte-disabled",
        va=0x8020_0000,
        pa=0x8040_0000,
        payload=b"\x13\x00\x00\x00",
        s1_pte=TranslationPte(pbmt=1),
        ptw_machine_pbmte=0,
    )
    legacy_raw_response = TranslationScenario(
        scenario_id="pbmte-raw-response",
        va=0x8030_0000,
        pa=0x8050_0000,
        payload=b"\x13\x00\x00\x00",
        s1_pte=TranslationPte(pbmt=1),
    )

    TranslationScenarioBuilder(env).build(disabled)
    state = TranslationScenarioBuilder(env).build(legacy_raw_response)
    response = env.page_table.build_ptw_resp(state.expected_ptw_request["vpn"])

    assert state.expected_outcome["outcome"] == "normal"
    assert response["s1_pf"] == 0


def test_nemu_adapter_serializes_pbmt_and_napot_pte_fields() -> None:
    vpn = 0x8020_0000 >> 12
    ptw_adapter_template.sync_sv39_page_table(
        (
            {
                "vpn": vpn,
                "ppn": 0x8040_0,
                "r": 1,
                "x": 1,
                "a": 1,
                "n": 1,
                "pbmt": 1,
            },
        )
    )

    response = ptw_adapter_template.build_ptw_resp(_ptw_request(vpn))

    assert response["s1_entry_n"] == 1
    assert response["s1_entry_pbmt"] == 1


def test_ptw_agent_normalizes_nemu_response_through_declared_pbmte_policy() -> None:
    env = _env()
    vpn = 0x8020_0000 >> 12
    ptw_adapter_template.sync_sv39_page_table(
        ({"vpn": vpn, "ppn": 0x8040_0, "r": 1, "x": 1, "a": 1, "pbmt": 1},)
    )
    env.page_table.set_ptw_pbmte_policy(machine=0, reset=True)
    env.ptw_agent.configure(
        mode="sv39",
        response_source="nemu",
        nemu_ptw_adapter="env.nemu.ptw_adapter_template:build_ptw_resp",
    )

    response = env.ptw_agent._build_response(_ptw_request(vpn))

    assert response["s1_entry_pbmt"] == 1
    assert response["s1_pf"] == 1


def test_builder_derives_explicit_fetch_range_permission_probes() -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="sv39-fetch-range-probe",
        va=0x8020_0FF8,
        pa=0x8040_0FF8,
        payload=b"\x13" * 16,
        page_count=2,
        pmp_entries=(
            TranslationPmpPmaEntry(
                "pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True), 0x8040_0000, size=0x1000
            ),
            TranslationPmpPmaEntry(
                "pmp", 1, PmpPmaConfig(match="napot", read=True, execute=True), 0x8040_1000, size=0x1000
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                "pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_0000, size=0x1000
            ),
            TranslationPmpPmaEntry(
                "pma", 1, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=False), 0x8040_1000, size=0x1000
            ),
        ),
        permission_probes=(
            TranslationPermissionProbe(va=0x8020_0FF8, size=8),
            TranslationPermissionProbe(va=0x8020_1000, size=8),
        ),
    )

    state = TranslationScenarioBuilder(env).build(scenario)

    assert state.expected_permission_probes == (
        {
            "va": 0x8020_0FF8,
            "size": 8,
            "pa": 0x8040_0FF8,
            "end": 0x8040_0FFF,
            "translated": True,
            "permission": state.expected_permission_probes[0]["permission"],
        },
        {
            "va": 0x8020_1000,
            "size": 8,
            "pa": 0x8040_1000,
            "end": 0x8040_1007,
            "translated": True,
            "permission": state.expected_permission_probes[1]["permission"],
        },
    )
    assert state.expected_permission_probes[0]["permission"]["pma_cacheable"] is True
    assert state.expected_permission_probes[1]["permission"]["pma_cacheable"] is False


def test_builder_splits_one_logical_permission_probe_at_declared_attribute_boundaries() -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="split-permission-probe",
        va=0x8020_0000,
        pa=0x8040_0000,
        payload=b"\x13" * 16,
        permission_probes=(TranslationPermissionProbe(va=0x8020_0000, size=8, segment_sizes=(4, 4)),),
    )

    state = TranslationScenarioBuilder(env).build(scenario)

    assert state.expected_permission_probes == (
        {
            "probe_index": 0,
            "segment_index": 0,
            "logical_va": 0x8020_0000,
            "logical_size": 8,
            "va": 0x8020_0000,
            "size": 4,
            "pa": 0x8040_0000,
            "end": 0x8040_0003,
            "translated": True,
            "permission": state.expected_permission_probes[0]["permission"],
        },
        {
            "probe_index": 0,
            "segment_index": 1,
            "logical_va": 0x8020_0000,
            "logical_size": 8,
            "va": 0x8020_0004,
            "size": 4,
            "pa": 0x8040_0004,
            "end": 0x8040_0007,
            "translated": True,
            "permission": state.expected_permission_probes[1]["permission"],
        },
    )


@pytest.mark.parametrize("segment_sizes", [(4, 2), (4, 0, 4)])
def test_builder_rejects_incomplete_or_empty_permission_probe_segments(segment_sizes) -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="invalid-permission-probe-segments",
        va=0x8020_0000,
        pa=0x8040_0000,
        payload=b"\x13" * 16,
        permission_probes=(TranslationPermissionProbe(va=0x8020_0000, size=8, segment_sizes=segment_sizes),),
    )

    with pytest.raises(ValueError, match="segment sizes"):
        TranslationScenarioBuilder(env).build(scenario)


def test_builder_rejects_permission_probe_outside_its_payload() -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="invalid-permission-probe",
        va=0x8020_0000,
        pa=0x8040_0000,
        payload=b"\x13" * 4,
        permission_probes=(TranslationPermissionProbe(va=0x8020_0004, size=8),),
    )

    with pytest.raises(ValueError, match="permission probe"):
        TranslationScenarioBuilder(env).build(scenario)


@pytest.mark.parametrize(
    "kwargs,match",
    [
        ({"ptw_req_ready_strategy": "unknown"}, "ready strategy"),
        ({"ptw_req_ready_probability": 2.0}, "probability"),
        ({"ptw_response_source": "nemu"}, "NEMU adapter"),
    ],
)
def test_builder_rejects_invalid_ptw_transport_policy(kwargs, match) -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="invalid-ptw-transport-policy",
        va=0x8020_0000,
        pa=0x8040_0000,
        payload=b"\x13",
        **kwargs,
    )

    with pytest.raises(ValueError, match=match):
        TranslationScenarioBuilder(env).build(scenario)


def test_builder_applies_request_keyed_ptw_response_override() -> None:
    env = _env()
    vpn = 0x8020_0000 >> 12
    scenario = TranslationScenario(
        scenario_id="sv39-sector-response-override",
        va=0x8020_0000,
        pa=0x8040_0000,
        payload=b"\x13\x00\x00\x00",
        ptw_response_overrides=(
            TranslationPtwResponseOverride(
                vpn=vpn,
                s2xlate=0,
                patch=(("s1_valididx", (0, 0, 0, 0, 0, 0, 0, 0)),),
            ),
        ),
    )

    TranslationScenarioBuilder(env).build(scenario)

    request = _ptw_request(vpn)
    response = env.ptw_agent._build_model_response(request)
    assert response["s1_valididx"] != [0] * 8
    overridden = env.ptw_agent._build_response(request)
    assert overridden["s1_valididx"] == [0] * 8
    assert env.ptw_agent.get_stats()["response_override_hit_count"] == 1


def test_builder_composes_declared_stage1_sector_lanes_into_one_ptw_response() -> None:
    env = _env()
    va = 0x8020_0000
    pa = 0x8040_0000
    state = TranslationScenarioBuilder(env).build(
        TranslationScenario(
            scenario_id="sv39-sector-lanes",
            va=va,
            pa=pa,
            payload=b"\x13\x00\x00\x00",
            s1_sector_lanes=(
                TranslationSectorLane(lane=1, ppn=(pa >> 12) + 1),
                TranslationSectorLane(lane=2, ppn=(pa >> 12) + 2, valid=0),
            ),
        )
    )

    response = env.page_table.build_ptw_resp(state.expected_ptw_request["vpn"])
    rewalk_response = env.page_table.build_ptw_resp((va >> 12) + 1)
    translated_pa, ok, _ = env.page_table.translate(va + 0x1000)

    assert response["s1_valididx"][:3] == [1, 1, 0]
    assert response["s1_pteidx"][:3] == [1, 1, 1]
    assert response["s1_ppn_low"][:3] == [0, 1, 2]
    assert rewalk_response["s1_pf"] == 0
    assert rewalk_response["s1_valididx"][1] == 1
    assert ok is True
    assert translated_pa == pa + 0x1000


def test_builder_keeps_missing_sector_lane_unmapped_for_rewalk_fault() -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="sv39-sector-missing-pte-lane",
        va=0x8020_0000,
        pa=0x8040_0000,
        payload=b"\x13\x00\x00\x00" * 1024,
        page_count=2,
        s1_sector_lanes=(
            TranslationSectorLane(lane=1, ppn=(0x8040_0000 >> 12) + 1, valid=0, pte_present=0),
        ),
    )

    state = TranslationScenarioBuilder(env).build(scenario)
    initial = env.page_table.build_ptw_resp(scenario.va >> 12)
    rewalk = env.page_table.build_ptw_resp((scenario.va >> 12) + 1)

    assert state.expected_page_outcomes[0]["ok"] is True
    assert state.expected_page_outcomes[1]["outcome"] == "instruction_page_fault"
    assert state.expected_page_outcomes[1]["expected_path"] == "fault"
    assert initial["s1_pf"] == 0
    assert initial["s1_valididx"][1] == 0
    assert rewalk["s1_pf"] == 1


def test_builder_applies_response_fault_override_to_selected_page_outcome() -> None:
    env = _env()
    va = 0x8020_0000
    scenario = TranslationScenario(
        scenario_id="sv39-sector-rewalk-fault-override",
        va=va,
        pa=0x8040_0000,
        payload=b"\x13\x00" * 2049,
        page_count=2,
        ptw_response_overrides=(
            TranslationPtwResponseOverride(
                vpn=(va >> 12) + 1,
                s2xlate=0,
                patch=(("s1_pf", 1),),
            ),
        ),
    )

    state = TranslationScenarioBuilder(env).build(scenario)

    assert state.expected_page_outcomes[0]["ok"] is True
    assert state.expected_page_outcomes[1]["outcome"] == "instruction_page_fault"
    assert state.expected_page_outcomes[1]["expected_path"] == "fault"


def test_builder_rejects_invalid_ptw_response_override_before_mutating_env() -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id="invalid-ptw-response-override",
        va=0x8020_0000,
        pa=0x8040_0000,
        payload=b"\x13\x00\x00\x00",
        ptw_response_overrides=(
            TranslationPtwResponseOverride(vpn=0x8020_0000 >> 12, s2xlate=0, patch=(("unknown", 1),)),
        ),
    )

    with pytest.raises(ValueError, match="unsupported PTW response override fields"):
        TranslationScenarioBuilder(env).build(scenario)

    assert env.page_table.pte_map == {}
    assert env.memory.mem == {}


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


@pytest.mark.parametrize("mode", ("sv39", "sv48"))
def test_builder_constructs_normal_cross_page_instruction_stream(mode: str) -> None:
    env = _env()
    scenario = TranslationScenario(
        scenario_id=f"normal-cross-page-{mode}",
        va=0x8020_0F00,
        pa=0x8040_0F00,
        payload=b"\x13\x00\x00\x00" * 512,
        page_count=2,
        mode=mode,
        pmp_entries=(
            TranslationPmpPmaEntry(
                "pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True), 0x8040_0000, size=0x2000
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                "pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_0000, size=0x2000
            ),
        ),
    )

    state = TranslationScenarioBuilder(env).build(scenario)

    assert len(state.expected_page_outcomes) == 2
    assert [(outcome["va"], outcome["pa"]) for outcome in state.expected_page_outcomes] == [
        (0x8020_0F00, 0x8040_0F00),
        (0x8020_1000, 0x8040_1000),
    ]
    assert all(outcome["ok"] and outcome["expected_path"] == "cacheable" for outcome in state.expected_page_outcomes)
    assert env.memory.read_block(scenario.pa, len(scenario.payload)) == scenario.payload
    assert len(state.pmp_writes) == len(state.pma_writes) == 1


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
    "scenario,expected_response,expected_outcome",
    [
        pytest.param(
            TranslationScenario(
                scenario_id="s1-write-without-read-response",
                va=0x8020_0000,
                pa=0x8040_0000,
                payload=b"\x13\x00\x00\x00",
                s2xlate=1,
                s1_pte=TranslationPte(r=0, w=1),
                expected_path="fault",
                expected_result="page_fault",
            ),
            (1, 0, 1, 1, 0, 0, 0, 0),
            "instruction_page_fault",
            id="stage1",
        ),
        pytest.param(
            TranslationScenario(
                scenario_id="s2-write-without-read-response",
                va=0x8020_0000,
                pa=0x8040_0000,
                payload=b"\x13\x00\x00\x00",
                mode="bare",
                s2xlate=2,
                priv_virt=1,
                s2_pte=TranslationPte(r=0, w=1),
                expected_path="fault",
                expected_result="guest_fault",
            ),
            (0, 0, 0, 0, 0, 0, 1, 1),
            "instruction_guest_page_fault",
            id="only-stage2",
        ),
        pytest.param(
            TranslationScenario(
                scenario_id="all-stage-gstage-write-without-read-response",
                va=0x8020_0000,
                gpa=0x8060_0000,
                pa=0x8040_0000,
                payload=b"\x13\x00\x00\x00",
                s2xlate=3,
                priv_virt=1,
                s1_pte=TranslationPte(asid=5, vmid=7),
                s2_pte=TranslationPte(r=0, w=1, vmid=7),
                vsatp_asid=5,
                hgatp_vmid=7,
                expected_path="fault",
                expected_result="guest_fault",
            ),
            (1, 1, 0, 0, 0, 0, 1, 1),
            "instruction_guest_page_fault",
            id="all-stage",
        ),
    ],
)
def test_builder_encodes_write_without_read_ptw_responses_like_l2tlb(
    scenario: TranslationScenario,
    expected_response: tuple[int, ...],
    expected_outcome: str,
) -> None:
    env = _env()
    state = TranslationScenarioBuilder(env).build(scenario)
    response = env.page_table.build_ptw_resp(scenario.va >> 12, s2xlate=scenario.s2xlate)

    assert tuple(
        response[name]
        for name in (
            "s1_entry_v",
            "s1_entry_perm_r",
            "s1_entry_perm_w",
            "s1_pf",
            "s2_entry_v",
            "s2_entry_perm_r",
            "s2_entry_perm_w",
            "s2_gpf",
        )
    ) == expected_response
    assert state.expected_outcome["outcome"] == expected_outcome


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
            TranslationScenario("noncanonical", 0x0000_0080_0000_0000, 0x80400000, b"\x13"),
            "non-canonical",
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
