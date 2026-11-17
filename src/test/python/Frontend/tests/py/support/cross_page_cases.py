from __future__ import annotations

import pytest

from env.model.golden_trace import GoldenTrace, TraceEntry
from env.runtime.frontend_init import FrontendInitConfig, initialize_frontend
from env.sequences import (
    TranslationPmpPmaEntry,
    TranslationScenario,
    TranslationScenarioBuilder,
    TranslationScenarioPhase,
    TranslationScenarioSequence,
)
from env.support import PcBoundary, PmpPmaConfig, RandomPc, random_pc, record_scenario, scenario_rng
from tests.py.support.translation_faults import _capture_cfvec_deliveries


_NOP = (0x00000013).to_bytes(4, "little")
_CNOP = b"\x01\x00"
_PAGE_SIZE = 0x1000
_START_OFFSETS = (0xF80, 0xF90, 0xFA0, 0xFB0, 0xFBC, 0xFC0)
# The branch must start at a fetch-block boundary for golden-trace recovery.
_BRANCH_START_OFFSETS = (0xF9C, 0xFBC)


def _case_addresses(
    env, scenario_id: str, virtual_base: int, offsets: tuple[int, ...] = _START_OFFSETS,
    boundaries: tuple[PcBoundary, ...] = (),
) -> RandomPc:
    key = f"zhaoxinran/translation/{scenario_id}"
    base_seed, seed, rng = scenario_rng(key)
    placement = random_pc(
        rng, va_range=(virtual_base, virtual_base + 64 * 0x2000),
        pa_range=(0x8040_0000, 0x8040_0000 + 64 * 0x2000), slot_size=0x2000,
        offsets=offsets, boundaries=boundaries,
        memory="cacheable",
    )
    va, pa = placement.va, placement.pa
    record_scenario(
        env, key, base_seed=base_seed, seed=seed,
        parameters={"scenario_id": scenario_id, "va": va, "pa": pa,
                    "start_offset": va % _PAGE_SIZE, "memory": "cacheable"},
    )
    return placement


def _beq_x0_x1(offset: int) -> int:
    assert offset % 2 == 0 and -4096 <= offset <= 4094
    bits = offset & 0x1FFF
    return (
        ((bits >> 12) & 1) << 31
        | ((bits >> 5) & 0x3F) << 25
        | (1 << 20)
        | ((bits >> 1) & 0xF) << 8
        | ((bits >> 11) & 1) << 7
        | 0x63
    )


def _cross_page_program(va: int, branch: int | None = None) -> tuple[bytes, int]:
    second_page = (va & ~(_PAGE_SIZE - 1)) + _PAGE_SIZE
    prefix = _NOP + branch.to_bytes(4, "little") if branch is not None else b""
    gap = second_page - 2 - va - len(prefix)
    assert gap >= 0 and gap % 2 == 0
    nops = gap // 2
    return prefix + _CNOP * nops + _NOP * 66, nops


def _mapped_scenario(scenario_id: str, placement: RandomPc, payload: bytes) -> TranslationScenario:
    va, pa = placement.va, placement.pa
    permission = PmpPmaConfig(
        match="napot", read=True, execute=True, cacheable=placement.memory.pma_cacheable,
    )
    return TranslationScenario(
        scenario_id=scenario_id, va=va, pa=pa, payload=payload, page_count=2, mode="sv39",
        s1_page_pbmt=(placement.memory.pbmt,) * 2,
        pmp_entries=(TranslationPmpPmaEntry("pmp", 0, permission, pa & ~0x1FFF, size=0x2000),),
        pma_entries=(TranslationPmpPmaEntry("pma", 0, permission, pa & ~0x1FFF, size=0x2000),),
    )


def run_mapped_cross_page_rvi(env) -> None:
    scenario_id = "translation-normal-rvi-cross-page-both-mapped"
    placement = _case_addresses(env, scenario_id, 0x8000_0000)
    va, pa = placement.va, placement.pa
    payload, _ = _cross_page_program(va)
    scenario = _mapped_scenario(scenario_id, placement, payload)
    cross_page_pc = (va & ~(_PAGE_SIZE - 1)) + _PAGE_SIZE - 2
    sequence = TranslationScenarioSequence(actions=(TranslationScenarioPhase(scenario=scenario),))
    cfvec = _capture_cfvec_deliveries(env)
    responses = []

    def capture_ptw_response(cycle: int, active_env) -> None:
        port = active_env.ptw_if
        if int(port.resp_valid.value) and int(port.resp_ready.value):
            driven = active_env.ptw_agent.last_drive_expectation
            responses.append((driven["vpn"], int(port.resp_bits_s1_entry_v.value),
                              int(port.resp_bits_s1_pf.value)))

    env.register_pre_drive_cycle_observer(capture_ptw_response)
    sequence.initialize_first_phase(env)
    sequence.run(env)

    expected_vpns = {va >> 12, (va >> 12) + 1}
    assert expected_vpns.issubset({vpn for vpn, valid, pf in responses if valid and not pf}), responses
    assert any(record["pc"] == cross_page_pc and not record["exception_bits"] for record in cfvec), cfvec[-64:]
    assert any(record["pc"] == cross_page_pc + 4 and not record["exception_bits"] for record in cfvec), cfvec[-64:]
    assert not [record for record in cfvec if record["exception_bits"]], cfvec[-64:]
    assert not env.get_errors()


def _wrong_path_scenario(env) -> tuple[TranslationScenario, GoldenTrace, GoldenTrace]:
    scenario_id = "wrong-path-second-page-pf-before-cross-page-rvi"
    placement = _case_addresses(
        env, scenario_id, 0x8020_0000, _BRANCH_START_OFFSETS,
        (PcBoundary(32, displacement=4, width=1, relation="start"),),
    )
    va, pa = placement.va, placement.pa
    second_page = (va & ~(_PAGE_SIZE - 1)) + _PAGE_SIZE
    branch_pc = va + 4
    wrong_target = second_page + 2
    cross_page_pc = second_page - 2
    branch = _beq_x0_x1(wrong_target - branch_pc)
    payload, nops = _cross_page_program(va, branch)
    scenario = _mapped_scenario(scenario_id, placement, payload)
    training = GoldenTrace([
        TraceEntry(0, va, 0x13, 4),
        TraceEntry(1, branch_pc, branch, 4, kind="branch", taken=True, target_pc=wrong_target),
        TraceEntry(2, wrong_target, 0x13, 4),
        *(TraceEntry(i + 3, wrong_target + 4 + 4 * i, 0x13, 4) for i in range(64)),
    ])
    recovery = GoldenTrace([
        TraceEntry(0, branch_pc, branch, 4, kind="branch", taken=False, target_pc=branch_pc + 4),
        *(TraceEntry(i + 1, branch_pc + 4 + 2 * i, 1, 2) for i in range(nops)),
        TraceEntry(nops + 1, cross_page_pc, 0x13, 4, exception=12),
    ])
    return scenario, training, recovery


def run_cross_page_fault_after_wrong_path_second_page_refill(env) -> None:
    scenario, training, recovery = _wrong_path_scenario(env)
    va, pa = scenario.va, scenario.pa
    branch_pc = va + 4
    second_page = (va & ~(_PAGE_SIZE - 1)) + _PAGE_SIZE
    wrong_target = second_page + 2
    cross_page_pc = second_page - 2
    initialize_frontend(env, FrontendInitConfig(reset_vector=va, bare_mode=False, reset_cycles=20))
    TranslationScenarioBuilder(env).build(scenario)
    # Frontend-only DUT does not execute x1; the backend trace supplies branch outcomes.
    phase = ["train"]
    cfvec = []
    resolves = []
    ptw_requests = []
    ptw_responses = []

    def capture(cycle, active_env) -> None:
        observe = active_env.backend_cfvec_if
        for slot in range(8):
            if int(observe.cfvec_valid[slot].value):
                cfvec.append((phase[0], cycle, active_env.observed_cfvec_pc(slot),
                              int(observe.cfvec_pred_taken[slot].value),
                              int(observe.cfvec_exception_vec_12[slot].value),
                              int(observe.cfvec_cross_page_ipf_fix[slot].value)))
        redirect = active_env.backend_to_ftq_if
        for slot in range(3):
            if int(redirect.resolve_valid[slot].value):
                resolves.append((phase[0], cycle, int(redirect.resolve_bits_target_addr[slot].value) << 1,
                                 int(redirect.resolve_bits_taken[slot].value),
                                 int(redirect.resolve_bits_mispredict[slot].value)))

    def capture_ptw(cycle, active_env) -> None:
        ptw = active_env.ptw_if
        if int(ptw.req_0_valid.value) and int(ptw.req_0_ready.value):
            ptw_requests.append((phase[0], cycle, int(ptw.req_0_bits_vpn.value)))
        if int(ptw.resp_valid.value) and int(ptw.resp_ready.value):
            driven = active_env.ptw_agent.last_drive_expectation
            ptw_responses.append((phase[0], cycle, int(driven["vpn"]),
                                  int(ptw.resp_bits_s1_pf.value), int(ptw.resp_bits_s1_entry_v.value)))

    env.register_cycle_observer(capture)
    env.register_pre_drive_cycle_observer(capture_ptw)
    env.backend_model.inject_redirect(va, "start-branch-training", delay_cycles=0)
    env.backend_model.set_golden_trace(training)
    for _ in range(750):
        if (any(kind == "train" and target == wrong_target and taken for kind, _, target, taken, _ in resolves)
                and any(kind == "train" and pc == wrong_target for kind, _, pc, _, _, _ in cfvec)):
            break
        env.step(1)
    else:
        pytest.fail(
            f"branch training did not reach the second page: "
            f"resolves={resolves[-8:]} cfvec={cfvec[:16]} "
            f"ptw_requests={ptw_requests[:16]} ptw_responses={ptw_responses[:16]} "
            f"icache={env.icache_agent.get_stats().get('request_records', [])[:16]} "
            f"errors={env.get_errors()[:16]}"
        )

    env.backend_model.set_golden_trace(None)
    env.backend_model.set_backend_mode("auto_python")
    env.backend_model.inject_redirect(branch_pc, "revisit-trained-branch", delay_cycles=0)
    env.step(2)
    env.page_table.map_page(second_page >> 12, (pa >> 12) + 1, v=0)
    env.pulse_sfence(addr=second_page, rs1=1, rs2=0)
    env.monitor.clear()
    env.monitor.set_expected_pc(branch_pc)
    phase[0] = "recovery"
    env.backend_model.set_golden_trace(recovery)
    for _ in range(6000):
        if any(kind == "recovery" and pc == cross_page_pc and pf and cross
               for kind, _, pc, _, pf, cross in cfvec):
            break
        env.step(1)

    wrong_path = [record for record in cfvec if record[0] == "recovery" and record[2] == branch_pc]
    faults = [record for record in cfvec if record[0] == "recovery" and record[2] == cross_page_pc]
    second_vpn = second_page >> 12
    pf_responses = [record for record in ptw_responses if record[0] == "recovery" and record[2:] == (second_vpn, 1, 0)]
    corrections = [record for record in resolves if record[0] == "recovery" and record[2:] == (branch_pc + 4, 0, 1)]
    assert any(record[3] == 1 for record in wrong_path), {"branch": wrong_path, "resolves": resolves[-12:]}
    assert pf_responses, {"requests": ptw_requests[-12:], "responses": ptw_responses[-12:]}
    assert corrections, {"resolves": resolves[-12:], "branch": wrong_path}
    assert pf_responses[0][1] <= corrections[0][1], {"pf": pf_responses, "corrections": corrections}
    assert [vpn for kind, _, vpn in ptw_requests if kind == "recovery" and vpn == second_vpn] == [second_vpn]
    assert any(kind == "recovery" and pc == branch_pc + 4 and not pf and cycle > corrections[0][1]
               for kind, cycle, pc, _, pf, _ in cfvec), cfvec[-24:]
    assert any(record[4:] == (1, 1) and record[1] > corrections[0][1] for record in faults), faults
    assert not [
        error for error in env.get_errors()
        if not (
            error.get("kind") == "INSTR_TRANSLATION_FAULT"
            and second_page <= int(error["pc"]) < second_page + _PAGE_SIZE
            and int(error["cycle"]) < corrections[0][1]
        )
    ], env.get_errors()[:16]
