from __future__ import annotations

import os

import pytest

from env.checkers.vtype_checker import VTypeChecker
from env.model.golden_trace import GoldenTrace
from tests.py.zhaoxinran.test_bin_trace_dut import test_bin_trace as run_bin_trace


@pytest.mark.skipif(
    os.getenv("TB_ENABLE_DUT_TESTS") != "1" or os.getenv("TB_BIN_TRACE_PIPELINE") != "1",
    reason="requires the bin-trace DUT pipeline",
)
def test_vector_vtype_bin_trace(env) -> None:
    assert int(os.getenv("TB_TRACE_START_INDEX", "0"), 0) == 0
    assert int(os.getenv("TB_TRACE_TARGET_CURSOR", "0"), 0) == 0
    assert int(os.getenv("TB_TRACE_MAX_CYCLES", "0"), 0) == 0
    trace = GoldenTrace.from_file(os.environ["TB_TRACE_PATH"])
    checker = VTypeChecker(trace.entries)
    checker.bind_env(env)
    config_pcs = {pc for pc, result in checker.expected_by_pc.items() if result.kind != "ordinary"}
    assert len(config_pcs) == 4
    config_by_cycle: dict[int, set[int]] = {}
    stalled_pcs: set[int] = set()
    stalled_slots: dict[int, tuple[int, ...]] = {}
    held_slot_checks = 0
    held_config_checks = 0
    accept_at_edge: dict[int, int] = {}
    stall_until = None

    env.register_pre_drive_cycle_observer(
        lambda cycle, active_env: accept_at_edge.__setitem__(
            int(cycle), int(active_env.backend_to_ibuf_if.decode_can_accept.value)
        )
    )

    def check_vtype(cycle: int, active_env) -> None:
        nonlocal stall_until, held_slot_checks, held_config_checks
        if active_env.backend_model.golden_trace is None:
            return
        checker.on_clock_edge(cycle)
        slots = active_env.monitor.cycle_snapshot.cfvec.slots
        observed_pcs = {
            active_env.observed_cfvec_pc(slot.slot) for slot in slots if slot.valid
        }
        config_by_cycle[int(cycle)] = {
            pc for pc in observed_pcs if pc in config_pcs
        }
        if accept_at_edge[int(cycle)] == 0:
            stalled_pcs.update(observed_pcs & checker.expected_by_pc.keys())
            current_slots = {}
            for slot in slots:
                if not slot.valid:
                    continue
                payload = (
                    active_env.observed_cfvec_pc(slot.slot),
                    slot.instr,
                    slot.vtype_illegal, slot.vtype_vma, slot.vtype_vta,
                    slot.vtype_vsew, slot.vtype_vlmul,
                    slot.specvtype_illegal, slot.specvtype_vma, slot.specvtype_vta,
                    slot.specvtype_vsew, slot.specvtype_vlmul,
                )
                current_slots[slot.slot] = payload
            for lane, previous in stalled_slots.items():
                assert current_slots.get(lane) == previous, (
                    f"cfVec lane {lane} changed under backpressure at cycle {cycle}: "
                    f"previous={previous} current={current_slots.get(lane)}"
                )
                held_slot_checks += 1
                if previous[0] in config_pcs:
                    held_config_checks += 1
            stalled_slots.clear()
            stalled_slots.update(current_slots)
        else:
            stalled_slots.clear()
        if stall_until is None:
            active_env.backend_model.set_can_accept(0)
            stall_until = -2
        elif stall_until == -2 and config_by_cycle[int(cycle)]:
            stall_until = int(cycle) + 4
        elif stall_until is not None and stall_until >= 0 and int(cycle) >= stall_until:
            active_env.backend_model.set_can_accept(1)
            stall_until = -1

    env.register_cycle_observer(check_vtype)

    run_bin_trace(env)

    assert env._last_run_until_golden_result.completed is True
    required_pcs = {pc for config_pc in config_pcs for pc in (config_pc, config_pc + 4)}
    assert required_pcs <= checker.observed_pcs, (
        f"missing vtype cfVec observations: {sorted(required_pcs - checker.observed_pcs)}"
    )
    assert checker.checked_count >= len(required_pcs)
    assert checker.error_count == 0
    assert any(len(pcs) >= 2 for pcs in config_by_cycle.values()), "no same-cycle vset pair observed"
    assert len([cycle for cycle, pcs in config_by_cycle.items() if pcs]) >= 2
    assert stalled_pcs, "no in-trace cfVec lane observed during backend backpressure"
    assert held_slot_checks > 0, "no cfVec lane held across backpressured cycles"
    assert held_config_checks > 0, "no vset instruction held across backpressured cycles"
