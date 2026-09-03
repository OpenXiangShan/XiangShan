from __future__ import annotations

import pytest

from env.core.transactions import ProgramImage
from env.funcov.py.ifu import mmio_nc_owner_funcov as owner_funcov
from env.sequences import (
    LoadProgramSequence,
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationPtwResponseOverride,
    TranslationScenario,
    TranslationScenarioBuilder,
)
from env.support import PmpPmaConfig, fold_pc
from tests.py.zhaoxinran import test_address_translation_fault as translation_faults
from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


def _prepare_cross_page_mmio_rvi(env) -> tuple[int, int, bytes]:
    cross_page_va = uncache._NORMAL_BASE + uncache._SV39_PAGE_SIZE - 2
    cross_page_pa = uncache._MMIO_BASE + uncache._SV39_PAGE_SIZE - 2
    payload = int(uncache._ADDI_X0_X0_0).to_bytes(4, "little")
    payload += int(uncache._CNOP).to_bytes(2, "little") * 8
    env.memory.mmio_ranges.append(
        (uncache._MMIO_BASE, uncache._MMIO_BASE + 2 * uncache._SV39_PAGE_SIZE)
    )
    LoadProgramSequence(
        image=ProgramImage(payload=payload, base_addr=uncache._MMIO_BASE + uncache._SV39_PAGE_SIZE - 2),
        step_cycles=0,
    ).run(env)
    return cross_page_va, cross_page_pa, payload


def _cross_page_fault_scenario(
    *,
    case_id: str,
    s2xlate: int,
    response_field: str,
    expected_result: str,
    va: int,
    pa: int,
    payload: bytes,
) -> TranslationScenario:
    first_page_pa = pa & ~(uncache._SV39_PAGE_SIZE - 1)
    pmp_entries = tuple(
        TranslationPmpPmaEntry(
            kind="pmp",
            index=page,
            config=PmpPmaConfig(match="napot", read=True, write=True, execute=True),
            addr=first_page_pa + page * uncache._SV39_PAGE_SIZE,
            size=uncache._SV39_PAGE_SIZE,
        )
        for page in range(2)
    )
    pma_entries = tuple(
        TranslationPmpPmaEntry(
            kind="pma",
            index=page,
            config=PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=False),
            addr=first_page_pa + page * uncache._SV39_PAGE_SIZE,
            size=uncache._SV39_PAGE_SIZE,
        )
        for page in range(2)
    )
    translation = {
        "mode": "bare" if s2xlate == 2 else "sv39",
        "s2xlate": s2xlate,
        "priv_virt": int(s2xlate != 0),
    }
    if s2xlate == 3:
        translation.update(
            {
                "gpa": uncache._NORMAL_PHYS_BASE + uncache._SV39_PAGE_SIZE - 2,
                "s1_pte": TranslationPte(asid=5, vmid=7),
                "s2_pte": TranslationPte(vmid=7),
                "vsatp_asid": 5,
                "hgatp_vmid": 7,
            }
        )
    return TranslationScenario(
        scenario_id=f"mmio-cross-page-second-{case_id}",
        va=va,
        pa=pa,
        payload=payload,
        page_count=2,
        ptw_response_overrides=(
            TranslationPtwResponseOverride(
                vpn=(va >> 12) + 1,
                s2xlate=s2xlate,
                patch=((response_field, 1),),
            ),
        ),
        expected_path="fault",
        expected_result=expected_result,
        pmp_entries=pmp_entries,
        pma_entries=pma_entries,
        **translation,
    )


@pytest.mark.parametrize(
    "s2xlate,response_field,expected_result,expected_fault",
    translation_faults._CROSS_PAGE_FAULT_CASES,
)
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_cross_page_second_page_translation_fault(
    env,
    s2xlate: int,
    response_field: str,
    expected_result: str,
    expected_fault: str,
):
    cross_page_va, cross_page_pa, payload = _prepare_cross_page_mmio_rvi(env)
    uncache._initialize_sv39_fetch(env, reset_vector=cross_page_va)
    gpaddr_writes = translation_faults._capture_gpaddr_writes(env)
    cfvec_deliveries = translation_faults._capture_cfvec_deliveries(env)
    scenario = _cross_page_fault_scenario(
        case_id=f"s2xlate-{s2xlate}-{response_field}",
        s2xlate=s2xlate,
        response_field=response_field,
        expected_result=expected_result,
        va=cross_page_va,
        pa=cross_page_pa,
        payload=payload,
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(cross_page_va)
    env.arm_translation_scenario(state)
    uncache._force_redirect_to(env, cross_page_va)

    assert uncache._wait_for_request_addr(
        env,
        cross_page_pa & ~(uncache._UNCACHE_BEAT_BYTES - 1),
        max_cycles=12000,
    )
    for _ in range(12000):
        active = env.translation_oracle.get_active()
        if active is not None and active.get("fault_seen"):
            break
        env.step(1)

    active = env.translation_oracle.get_active()
    assert int(env.ptw_agent.get_stats().get("response_override_hit_count", 0)) >= 1
    assert active is not None and active.get("fault_seen"), {
        "s2xlate": s2xlate,
        "response_field": response_field,
        "active": active,
        "ptw": env.ptw_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    second_page_vpn = (cross_page_va >> 12) + 1
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
        int(record["pc"]) == cross_page_va
        and record["fault"] == expected_fault
        and bool(record["cross_page"])
        for record in exception_records
    ), exception_records
    uncache_stats = env.uncache_agent.get_stats()
    first_beat = cross_page_pa & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    next_page_pa = (cross_page_pa & ~(uncache._SV39_PAGE_SIZE - 1)) + uncache._SV39_PAGE_SIZE
    assert first_beat in uncache_stats.get("request_addrs", [])
    assert next_page_pa not in uncache_stats.get("request_addrs", [])
    assert env.monitor.exception_mark_count > 0
    _, ftq_value = translation_faults._assert_fault_ftq_identity(
        cfvec_deliveries,
        pc=cross_page_va,
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


@pytest.mark.funcov_bins("BIN-1123", "BIN-1124", "BIN-1125")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_page_tail_first_page_pmp_execute_fault_reports_iaf(env):
    """A first-page MMIO PMP execute denial must be IAF, never illegal."""
    cross_page_va, cross_page_pa, payload = _prepare_cross_page_mmio_rvi(env)
    first_page_pa = cross_page_pa & ~(uncache._SV39_PAGE_SIZE - 1)
    scenario = TranslationScenario(
        scenario_id="mmio-page-tail-first-page-pmp-execute-fault",
        va=cross_page_va,
        pa=cross_page_pa,
        payload=payload,
        page_count=2,
        expected_path="fault",
        expected_result="access_fault",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(
                    match="napot", read=True, write=True, execute=False
                ),
                addr=first_page_pa,
                size=uncache._SV39_PAGE_SIZE,
            ),
            TranslationPmpPmaEntry(
                kind="pmp",
                index=1,
                config=PmpPmaConfig(
                    match="napot", read=True, write=True, execute=True
                ),
                addr=first_page_pa + uncache._SV39_PAGE_SIZE,
                size=uncache._SV39_PAGE_SIZE,
            ),
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
                    cacheable=False,
                ),
                addr=first_page_pa + page * uncache._SV39_PAGE_SIZE,
                size=uncache._SV39_PAGE_SIZE,
            )
            for page in range(2)
        ),
    )
    snapshots: list[dict[str, int | None]] = []

    def capture(cycle: int, active_env) -> None:
        snapshots.append(
            {
                "cycle": int(cycle),
                **owner_funcov._snapshot(
                    active_env.functional_coverage, active_env.dut
                ),
            }
        )

    env.register_cycle_observer(capture)
    uncache._initialize_sv39_fetch(env, reset_vector=cross_page_va)
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(cross_page_va)
    env.arm_translation_scenario(state)
    uncache._force_redirect_to(env, cross_page_va)

    for _ in range(12000):
        if any(
            sample["s2_valid"] == 1
            and sample["s2_req_uncache"] == 1
            and sample["s2_pmp_mmio"] == 1
            and sample["s2_exception"] == 3
            and sample["s2_use_uncache"] == 0
            and sample["to_valid"] == 1
            and sample["to_exception"] == 3
            for sample in snapshots
        ):
            break
        env.step(1)

    matching = [
        sample
        for sample in snapshots
        if sample["s2_valid"] == 1
        and sample["s2_req_uncache"] == 1
        and sample["s2_pmp_mmio"] == 1
        and sample["s2_exception"] == 3
        and sample["s2_use_uncache"] == 0
        and sample["to_valid"] == 1
        and sample["to_exception"] == 3
    ]
    assert matching, {"snapshots": snapshots[-64:]}
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == 0
    assert uncache._wait_for_monitor_exception(env, max_cycles=6000)
    active = env.translation_oracle.get_active()
    assert active is not None and active.get("fault_seen")
    assert active["expected_fault"] == "instruction_access_fault"
    assert env.monitor.get_stats()["foldpc_recovery_count"] == 1
    assert any(
        record["kind"] == "cfvec_exception_foldpc_match"
        and int(record["expected_va"]) == int(cross_page_va)
        for record in env.translation_oracle.get_stats()["records"]
    )
    assert uncache._wait_for_ptw_resp(env, max_cycles=6000) >= 2
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1126")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_cross_page_second_page_pmp_execute_fault_keeps_original_pc(env):
    cross_page_va, cross_page_pa, payload = _prepare_cross_page_mmio_rvi(env)
    first_page_pa = cross_page_pa & ~(uncache._SV39_PAGE_SIZE - 1)
    next_page_pa = first_page_pa + uncache._SV39_PAGE_SIZE
    scenario = TranslationScenario(
        scenario_id="mmio-cross-page-second-page-pmp-execute-fault",
        va=cross_page_va,
        pa=cross_page_pa,
        payload=payload,
        page_count=2,
        expected_path="fault",
        expected_result="access_fault",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(
                    match="napot", read=True, write=True, execute=True
                ),
                addr=first_page_pa,
                size=uncache._SV39_PAGE_SIZE,
            ),
            TranslationPmpPmaEntry(
                kind="pmp",
                index=1,
                config=PmpPmaConfig(
                    match="napot", read=True, write=True, execute=False
                ),
                addr=next_page_pa,
                size=uncache._SV39_PAGE_SIZE,
            ),
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
                    cacheable=False,
                ),
                addr=first_page_pa + page * uncache._SV39_PAGE_SIZE,
                size=uncache._SV39_PAGE_SIZE,
            )
            for page in range(2)
        ),
    )
    snapshots: list[dict[str, int | None]] = []

    def capture(cycle: int, active_env) -> None:
        snapshots.append(
            {
                "cycle": int(cycle),
                **owner_funcov._snapshot(
                    active_env.functional_coverage, active_env.dut
                ),
            }
        )

    env.register_cycle_observer(capture)
    uncache._initialize_sv39_fetch(env, reset_vector=cross_page_va)
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(cross_page_va)
    env.arm_translation_scenario(state)
    uncache._force_redirect_to(env, cross_page_va)

    first_beat = cross_page_pa & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=12000)
    assert uncache._wait_for_monitor_exception(env, max_cycles=12000)
    assert uncache._wait_for_ptw_resp(env, max_cycles=6000) >= 2

    matching = [
        sample
        for sample in snapshots
        if sample["prev_end_half"] == 1
        and sample["to_valid"] == 1
        and sample["to_ready"] == 1
        and sample["to_exception_cross_page"] == 1
        and sample["to_exception"] == 3
        and sample["to_uncache_valid"] != 1
        and sample["tl_a_valid"] != 1
    ]
    assert matching, {"snapshots": snapshots[-64:]}
    stats = env.uncache_agent.get_stats()
    assert first_beat in stats.get("request_addrs", [])
    assert next_page_pa not in stats.get("request_addrs", [])
    exception_records = [
        record
        for record in env.translation_oracle.get_stats()["records"]
        if record["kind"] == "cfvec_exception"
    ]
    assert any(
        int(record["pc"]) == int(cross_page_va)
        or (
            int(record["pc"]) == 0
            and int(record["folded_pc"]) == fold_pc(cross_page_va)
        )
        for record in exception_records
    ), exception_records
    assert env.functional_coverage.key_hit(
        "ifu_instruncache_owner_v3", "instruncache_leaf_033"
    )
    assert env.assert_translation_scenario()["error_count"] == 0
    assert not env.monitor.get_errors()
