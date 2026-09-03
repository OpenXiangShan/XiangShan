from __future__ import annotations

import pytest

from env.funcov.py.ifu import mmio_nc_owner_funcov as owner_funcov
from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache
from tests.py.zhaoxinran import test_nc_fetch_paths as nc_paths
from env.support import PmpPmaConfig


def _register_snapshot_observer(env) -> list[dict[str, int | None]]:
    snapshots: list[dict[str, int | None]] = []

    def capture(cycle: int, active_env) -> None:
        sample = owner_funcov._snapshot(active_env.functional_coverage, active_env.dut)
        sample["cycle"] = int(cycle)
        snapshots.append(sample)

    env.register_cycle_observer(capture)
    return snapshots


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_response_uses_reserved_ibuffer_slot_under_backend_pressure(env):
    uncache._prepare_mmio_cnop_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=16)
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


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_backend_redirect_wins_over_uncache_response(env):
    uncache._prepare_mmio_cnop_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=32)
    snapshots = _register_snapshot_observer(env)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_req(env)
    assert env.uncache_agent.pending
    for _ in range(256):
        if snapshots and snapshots[-1]["tl_d_valid"] == 1:
            break
        env.step(1)
    assert snapshots[-1]["tl_d_valid"] == 1, {"snapshots": snapshots[-32:]}

    target_pc = uncache._MMIO_BASE + 0x40
    uncache._force_redirect_to(env, target_pc)
    for _ in range(256):
        if any(
            sample["backend_redirect"] == 1
            and sample["resp_valid"] == 1
            and sample["to_valid"] == 0
            for sample in snapshots
        ):
            break
        env.step(1)

    overlap = [
        sample
        for sample in snapshots
        if sample["backend_redirect"] == 1
        and sample["resp_valid"] == 1
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
    assert uncache._wait_for_observed_pc(env, target_pc, max_cycles=8000)
    assert not any(int(item.pc) == uncache._MMIO_BASE for item in env.monitor.observations)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_request_selection_overlaps_natural_predchecker_writeback_redirect(env, tmp_path):
    """Cancel a stale prediction's MMIO request before it becomes pending."""
    source_va = uncache._NORMAL_BASE
    target_va = source_va + uncache._SV39_PAGE_SIZE
    source_branch_offset = 26
    target_branch_offset = uncache._SV39_PAGE_SIZE + uncache._FETCH_BLOCK_SIZE + 26
    source_branch_pc = source_va + source_branch_offset
    target_branch_pc = source_va + target_branch_offset

    payload = bytearray(
        int(uncache._CNOP).to_bytes(2, "little") * uncache._SV39_PAGE_SIZE
    )
    payload[source_branch_offset : source_branch_offset + 4] = (
        nc_paths._encode_jal_x0(target_va - source_branch_pc).to_bytes(4, "little")
    )
    payload[target_branch_offset : target_branch_offset + 4] = (
        nc_paths._encode_jal_x0(source_va - target_branch_pc).to_bytes(4, "little")
    )
    bin_path = tmp_path / "mmio_predchecker_overlap.bin"
    bin_path.write_bytes(bytes(payload))

    _expected_block, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=source_va,
        paddr_pages=(
            uncache._NORMAL_PHYS_BASE,
            uncache._NORMAL_PHYS_BASE + uncache._SV39_PAGE_SIZE,
        ),
        pbmt=uncache._PBMT_PMA,
        bin_path=bin_path,
    )
    target_pa = mapping.paddr_pages[1]
    env.icache_agent.configure(hit_latency=8, miss_latency=8, miss_rate=0.0, seed=17)
    env.uncache_agent.configure(latency=24, mmio_latency=24)
    snapshots = _register_snapshot_observer(env)
    uncache._initialize_sv39_fetch(env, reset_vector=source_va)
    for index, page_pa in enumerate(mapping.paddr_pages):
        env.write_pmp_entry(
            index,
            PmpPmaConfig(match="napot", read=True, write=True, execute=True),
            int(page_pa),
            size=uncache._SV39_PAGE_SIZE,
            settle_cycles=4,
        )
        env.write_pma_entry(
            index,
            PmpPmaConfig(
                match="napot",
                read=True,
                write=True,
                execute=True,
                cacheable=True,
                atomic=True,
            ),
            int(page_pa),
            size=uncache._SV39_PAGE_SIZE,
            settle_cycles=4,
        )

    uncache._force_redirect_to(env, source_va)

    assert nc_paths._wait_for_taken_prediction(
        env, source_branch_pc, max_cycles=24000
    ), {
        "source_branch_pc": hex(source_branch_pc),
        "backend": env.backend_model.get_stats(),
    }

    env.memory.mmio_ranges.append(
        (target_pa, target_pa + uncache._SV39_PAGE_SIZE)
    )
    env.write_pma_entry(
        1,
        PmpPmaConfig(
            match="napot",
            read=True,
            write=True,
            execute=True,
            cacheable=False,
        ),
        int(target_pa),
        size=uncache._SV39_PAGE_SIZE,
        settle_cycles=4,
    )
    source_branch_pa = mapping.paddr_pages[0] + source_branch_offset
    env.memory.write_u32(source_branch_pa, uncache._ADDI_X0_X0_0)
    env.clock_reset.io_fencei.value = 1
    env.step(1)
    env.clock_reset.io_fencei.value = 0
    env.step(2)
    uncache._pulse_sfence(env, addr=target_va, rs1=1, rs2=0)
    env.monitor.clear()
    env.monitor.set_expected_pc(source_va)
    snapshots.clear()
    request_count_before_fault = int(
        env.uncache_agent.get_stats().get("req_count", 0)
    )
    uncache._force_redirect_to(env, source_va)

    overlap = None
    for _ in range(12000):
        overlap = next(
            (
                sample
                for sample in snapshots
                if sample["wb_path_valid"] == 1
                and sample["wb_redirect"] == 1
                and sample["s2_req_uncache"] == 1
                and sample["s2_pmp_mmio"] == 1
                and sample["req_valid"] == 1
            ),
            None,
        )
        if overlap is not None:
            break
        env.step(1)

    assert overlap is not None, {
        "source_branch_pc": hex(source_branch_pc),
        "target_va": hex(target_va),
        "snapshots": snapshots[-64:],
    }
    assert overlap["uncache_state"] == uncache._IFU_UNCACHE_INVALID
    assert overlap["tl_a_valid"] == 0
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == request_count_before_fault
    assert uncache._wait_for_observed_pc(env, source_branch_pc, max_cycles=12000)
    source_observation = next(
        item for item in env.monitor.observations if int(item.pc) == source_branch_pc
    )
    assert int(source_observation.instr) == uncache._ADDI_X0_X0_0
    assert not bool(source_observation.is_rvc)
    assert not any(int(item.pc) == target_va for item in env.monitor.observations)
    assert not env.monitor.get_errors()
