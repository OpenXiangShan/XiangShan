from __future__ import annotations

import os

import pytest

from env.core.transactions import ProgramImage
from env.funcov.py.ifu import mmio_nc_owner_funcov as owner_funcov
from env.sequences import LoadProgramSequence
from tests.py.support import uncache_scenarios as uncache
from tests.py.zhaoxinran.uncache import test_nc_fetch_paths as nc_paths
from env.support import PmpPmaConfig, record_scenario, scenario_rng

_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"


def _register_snapshot_observer(env) -> list[dict[str, int | None]]:
    snapshots: list[dict[str, int | None]] = []

    def capture(cycle: int, active_env) -> None:
        sample = owner_funcov._snapshot(active_env.functional_coverage, active_env.dut)
        sample["cycle"] = int(cycle)
        snapshots.append(sample)

    env.register_cycle_observer(capture)
    return snapshots


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_response_uses_reserved_ibuffer_slot_under_backend_pressure(env):
    scenario_key = "zhaoxinran/mmio/handoff/reserved-ibuffer-slot"
    base_seed, seed, rng = scenario_rng(scenario_key)
    latency = rng.randint(8, 24)
    env.uncache_agent.configure(latency=latency, mmio_latency=latency)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "latency": latency,
            "expected_path": "reserved_ibuffer_slot_under_pressure",
        },
    )
    uncache._prepare_mmio_cnop_stream(env)
    env.backend_model.set_can_accept(0)
    snapshots = _register_snapshot_observer(env)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_req(env)
    assert uncache._wait_for_uncache_resp(env)
    for _ in range(64):
        if any(
            sample["resp_valid"] == 1
            and sample["to_valid"] == 1
            and sample["to_ready"] == 1
            and sample["s2_req_uncache"] == 1
            and sample["s2_pmp_mmio"] == 1
            for sample in snapshots
        ):
            break
        env.step(1)

    assert any(
        sample["resp_valid"] == 1
        and sample["to_valid"] == 1
        and sample["to_ready"] == 1
        and sample["s2_req_uncache"] == 1
        and sample["s2_pmp_mmio"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-32:]}
    env.backend_model.set_can_accept(1)
    assert uncache._wait_for_observed_pc(env, uncache._MMIO_BASE, max_cycles=8000)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_backend_redirect_wins_over_uncache_response(env):
    scenario_key = "zhaoxinran/mmio/handoff/backend-redirect-wins"
    base_seed, seed, rng = scenario_rng(scenario_key)
    latency = rng.randint(24, 48)
    target_pc = uncache._MMIO_BASE + rng.randrange(0x40, 0x100, 8)
    env.uncache_agent.configure(latency=latency, mmio_latency=latency)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "latency": latency,
            "redirect_target": target_pc,
            "expected_path": "backend_redirect_wins_response",
        },
    )
    uncache._prepare_mmio_cnop_stream(env)
    snapshots = _register_snapshot_observer(env)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_req(env)
    assert env.uncache_agent.pending
    # Queue the redirect while InstrUncache is waiting for D, before D becomes
    # visible.  The backend drives a queued redirect on the next edge; waiting
    # for tl_d_valid first lets the response retire before the redirect can
    # overlap it.
    for _ in range(256):
        sample = snapshots[-1] if snapshots else None
        if sample and sample["entry_state"] == 3 and sample["tl_d_valid"] == 0:
            break
        env.step(1)
    assert snapshots and snapshots[-1]["entry_state"] == 3, {
        "snapshots": snapshots[-32:]
    }
    uncache._force_redirect_to(env, target_pc)
    for _ in range(256):
        if any(
            sample["backend_redirect"] == 1
            and sample["instr_resp_valid"] == 0
            and sample["to_valid"] == 0
            for sample in snapshots
        ):
            break
        env.step(1)

    overlap = [
        sample
        for sample in snapshots
        if sample["backend_redirect"] == 1
        and sample["instr_resp_valid"] == 0
        and sample["to_valid"] == 0
    ]
    assert overlap, {
        "states": [
            (
                sample["cycle"],
                sample["backend_redirect"],
                sample["tl_d_valid"],
                sample["instr_resp_valid"],
                sample["resp_valid"],
                sample["uncache_redirect"],
                sample["to_valid"],
            )
            for sample in snapshots[-32:]
        ]
    }
    redirect_cycle = min(int(sample["cycle"]) for sample in overlap)
    assert uncache._wait_for_observed_pc(env, target_pc, max_cycles=8000)
    assert not any(
        int(item.cycle) >= redirect_cycle and int(item.pc) == uncache._MMIO_BASE
        for item in env.monitor.observations
    )
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_request_selection_overlaps_natural_predchecker_writeback_redirect(env):
    """An older JAL redirect cancels a younger MMIO request before TL-A."""
    source_page = uncache._NORMAL_BASE
    mmio_page = source_page + uncache._SV39_PAGE_SIZE
    recovery_page = mmio_page + uncache._SV39_PAGE_SIZE
    cacheable_start = mmio_page - uncache._FETCH_BLOCK_SIZE
    branch_pc = mmio_page - 4
    recovery_cfi_pc = recovery_page + 0x140

    source_payload = bytearray(
        int(uncache._CNOP).to_bytes(2, "little")
        * (uncache._SV39_PAGE_SIZE // 2)
    )
    source_payload[-4:] = int(
        nc_paths._encode_jal_x0(recovery_page - branch_pc)
    ).to_bytes(4, "little")
    mmio_payload = bytes(
        int(uncache._CNOP).to_bytes(2, "little")
        * (uncache._SV39_PAGE_SIZE // 2)
    )
    recovery_payload = bytearray(mmio_payload)
    recovery_payload[0x140:0x144] = int(uncache._JAL_X0_PLUS_4).to_bytes(
        4, "little"
    )

    LoadProgramSequence(
        image=ProgramImage(payload=bytes(source_payload), base_addr=source_page),
        step_cycles=0,
    ).run(env)
    LoadProgramSequence(
        image=ProgramImage(payload=mmio_payload, base_addr=mmio_page),
        step_cycles=0,
    ).run(env)
    LoadProgramSequence(
        image=ProgramImage(payload=bytes(recovery_payload), base_addr=recovery_page),
        step_cycles=0,
    ).run(env)

    env.memory.mmio_ranges.append((mmio_page, mmio_page + uncache._SV39_PAGE_SIZE))
    env.icache_agent.configure(hit_latency=8, miss_latency=8, miss_rate=0.0, seed=17)
    env.uncache_agent.configure(latency=24, mmio_latency=24)
    env.initialize(reset_vector=cacheable_start, bare_mode=True, reset_cycles=20)
    env.write_pmp_entry(
        0,
        PmpPmaConfig(match="napot", read=True, write=True, execute=True),
        source_page,
        size=4 * uncache._SV39_PAGE_SIZE,
        settle_cycles=4,
    )
    pma_regions = (
        (mmio_page, False),
        (source_page, True),
        (recovery_page, True),
    )
    for index, (page_addr, cacheable) in enumerate(pma_regions):
        env.write_pma_entry(
            index,
            PmpPmaConfig(
                match="napot",
                read=True,
                write=True,
                execute=True,
                cacheable=cacheable,
                atomic=cacheable,
            ),
            page_addr,
            size=uncache._SV39_PAGE_SIZE,
            settle_cycles=4,
        )

    env.backend_model.commit_min_delay = 4096
    env.backend_model.commit_max_delay = 4096
    icache_line_baseline = int(env.icache_agent.get_stats()["resp_line_count"])
    uncache._force_redirect_to(env, cacheable_start)
    setup_branch_reached_s1 = False
    setup_checker_redirect = False
    for _ in range(4000):
        env.step(1)
        setup = owner_funcov._snapshot(env.functional_coverage, env.dut)
        setup_branch_reached_s1 |= bool(
            setup["s1_valid"] == 1
            and setup["s1_pc"] == (cacheable_start >> 1)
        )
        setup_checker_redirect |= bool(
            setup["checker_redirect"] == 1
            and setup["wb_pc"] == (cacheable_start >> 1)
        )
        if int(env.icache_agent.get_stats()["resp_line_count"]) > icache_line_baseline:
            break
    assert int(env.icache_agent.get_stats()["resp_line_count"]) > icache_line_baseline
    assert not setup_branch_reached_s1
    assert not setup_checker_redirect
    uncache._force_redirect_to(env, recovery_page)

    redirect_source = None
    for _ in range(8000):
        env.step(1)
        setup = owner_funcov._snapshot(env.functional_coverage, env.dut)
        setup_checker_redirect |= bool(
            setup["checker_redirect"] == 1
            and setup["wb_pc"] == (cacheable_start >> 1)
        )
        redirect_source = next(
            (
                entry
                for entry in env.backend_model._cfvec_queue
                if int(entry.pc) == recovery_cfi_pc and bool(entry.is_cfi)
            ),
            None,
        )
        if redirect_source is not None:
            break
    assert not setup_checker_redirect
    assert redirect_source is not None, {
        "reason": "cacheable recovery page did not provide a live redirect source",
        "backend": env.backend_model.get_stats(),
        "icache": env.icache_agent.get_stats(),
    }

    owner_funcov.reset_mmio_nc_owner_coverage_state(env.functional_coverage)
    env.monitor.clear()
    env.monitor.set_expected_pc(cacheable_start)
    snapshots = _register_snapshot_observer(env)
    request_baseline = int(env.uncache_agent.get_stats()["req_count"])
    env.backend_model.inject_redirect_from_cfvec(
        source_pc=int(redirect_source.pc),
        source_ftq_flag=int(redirect_source.ftq_flag),
        source_ftq_value=int(redirect_source.ftq_value),
        source_ftq_offset=int(redirect_source.ftq_offset),
        target_pc=cacheable_start,
        reason="mmio_checker_redirect_measurement_start",
        taken=1,
        level=0,
        delay_cycles=3,
    )


    overlap = None
    checked_snapshot_count = 0
    for _ in range(4000):
        overlap = next(
            (
                sample
                for sample in snapshots[checked_snapshot_count:]
                if sample["backend_redirect"] == 0
                and sample["checker_redirect"] == 1
                and sample["wb_path_valid"] == 1
                and sample["wb_redirect"] == 1
                and sample["ifu_flush"] == 1
                and sample["s2_valid"] == 1
                and sample["s2_req_uncache"] == 1
                and sample["s2_pmp_mmio"] == 1
                and sample["s2_wb_not_flush"] != 1
                and (sample["s2_ftq_flag"], sample["s2_ftq_value"])
                != (sample["wb_ftq_flag"], sample["wb_ftq_value"])
                and sample["req_valid"] == 1
                and sample["req_ready"] == 1
            ),
            None,
        )
        checked_snapshot_count = len(snapshots)
        if overlap is not None:
            break
        env.step(1)

    assert overlap is not None, {
        "reason": "older JAL redirect did not overlap the younger MMIO request",
        "branch_pc": hex(branch_pc),
        "mmio_page": hex(mmio_page),
        "snapshots": snapshots[-64:],
    }
    assert overlap["uncache_state"] == uncache._IFU_UNCACHE_INVALID
    assert overlap["tl_a_valid"] == 0
    assert int(env.uncache_agent.get_stats()["req_count"]) == request_baseline
    assert uncache._wait_for_observed_pc(env, recovery_cfi_pc, max_cycles=4000)
    assert not env.monitor.get_errors()
