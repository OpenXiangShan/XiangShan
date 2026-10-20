from __future__ import annotations

import os

import pytest

from env.checkers.vtype_checker import VTypeChecker
from env.core.transactions import ProgramImage
from env.model.golden_trace import GoldenTrace
from env.model.vtype import VTypeState
from env.sequences import LoadProgramSequence
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
    assert len(trace.entries) >= 1000
    checker = VTypeChecker.from_golden_trace(trace)
    checker.bind_env(env)
    config_pcs = {pc for pc, result in checker.expected_by_pc.items() if result.kind != "ordinary"}
    assert len(config_pcs) == 11
    illegal_entries = [
        entry for entry in trace.entries
        if entry.vtype_after is not None and int(entry.vtype_after) >> 63
    ]
    assert len(illegal_entries) == 2
    recovery = trace.entries[illegal_entries[-1].index + 2]
    assert recovery.instr == 0x0D0072D7  # vsetvli t0, zero, e32, m1, ta, ma
    assert recovery.vtype_after == 0xD0
    restored = VTypeState(False, True, True, 2, 0)
    assert checker.expected_by_pc[recovery.pc].specvtype.illegal
    assert checker.expected_by_pc[recovery.pc].vtype == restored
    vector_entry, ordinary_entry = trace.entries[recovery.index + 1:recovery.index + 3]
    assert vector_entry.pc == recovery.pc + 4
    assert (vector_entry.instr & 0xFE00707F) == 0x02000057  # unmasked vadd.vv
    assert ordinary_entry.pc == recovery.pc + 8
    assert ordinary_entry.instr == 0x00140413  # addi s0, s0, 1
    for entry in trace.entries[recovery.index + 1:recovery.index + 3]:
        assert checker.expected_by_pc[entry.pc].specvtype == restored
        assert checker.expected_by_pc[entry.pc].vtype == restored
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
    dynamic_checker = env.backend_model.vtype_checker
    assert dynamic_checker is not None
    assert {recovery.index, recovery.index + 1, recovery.index + 2} <= dynamic_checker.checked_indices
    assert checker.error_count == 0
    assert any(len(pcs) >= 2 for pcs in config_by_cycle.values()), "no same-cycle vset pair observed"
    assert len([cycle for cycle, pcs in config_by_cycle.items() if pcs]) >= 2
    assert stalled_pcs, "no in-trace cfVec lane observed during backend backpressure"
    assert held_slot_checks > 0, "no cfVec lane held across backpressured cycles"
    assert held_config_checks > 0, "no vset instruction held across backpressured cycles"


@pytest.mark.skipif(
    os.getenv("TB_ENABLE_DUT_TESTS") != "1" or os.getenv("TB_BIN_TRACE_PIPELINE") != "1",
    reason="requires the bin-trace DUT pipeline",
)
def test_vector_vsetvl_bin_trace(env) -> None:
    assert int(os.getenv("TB_TRACE_START_INDEX", "0"), 0) == 0
    assert int(os.getenv("TB_TRACE_TARGET_CURSOR", "0"), 0) == 0
    assert int(os.getenv("TB_TRACE_MAX_CYCLES", "0"), 0) == 0
    trace = GoldenTrace.from_file(os.environ["TB_TRACE_PATH"])
    assert len(trace.entries) >= 1000
    assert sum(VTypeChecker._vector_config_kind(entry.instr) is not None for entry in trace.entries) == 5
    vsetvl_entries = [
        entry for entry in trace.entries
        if VTypeChecker._vector_config_kind(entry.instr) == "vsetvl"
    ]
    assert len(vsetvl_entries) == 1
    source = vsetvl_entries[0]
    assert source.vtype_after == 0x49
    vector_entry = trace.entries[source.index + 1]
    assert vector_entry.pc == source.pc + 4
    assert (vector_entry.instr & 0xFE00707F) == 0x02000057  # unmasked vadd.vv
    observed_redirects: list[tuple[int, int, int, int, int, int, tuple[int, ...]]] = []

    def observe_backend(cycle: int, active_env) -> None:
        to_ftq = active_env.backend_to_ftq_if
        to_ibuf = active_env.backend_to_ibuf_if
        if int(to_ftq.redirect_valid.value):
            observed_redirects.append((
                int(cycle),
                int(to_ftq.redirect_bits_pc.value),
                int(to_ftq.redirect_bits_target.value),
                int(to_ftq.redirect_bits_level.value),
                int(to_ibuf.commit_vtype_valid.value),
                int(to_ibuf.commit_vtype_has_vsetvl.value),
                tuple(int(getattr(to_ibuf, f"vsetvl_vtype_{field}").value) for field in (
                    "illegal", "vma", "vta", "vsew", "vlmul"
                )),
            ))

    env.register_cycle_observer(observe_backend)
    run_bin_trace(env)

    checker = env.backend_model.vtype_checker
    assert checker is not None
    assert env._last_run_until_golden_result.completed is True
    assert {source.index, source.index + 1, source.index + 2} <= checker.checked_indices
    assert any(
        pc == source.pc and target == source.pc + 4
        and level == 0 and commit_valid == 1 and has_vsetvl == 1
        and fields == (0, 0, 1, 1, 1)
        for _cycle, pc, target, level, commit_valid, has_vsetvl, fields in observed_redirects
    ), f"vsetvl commit redirect not observed: {observed_redirects}"
    assert checker.error_count == 0


def _slot_vtype(slot, prefix: str) -> VTypeState:
    return VTypeState(
        illegal=bool(getattr(slot, f"{prefix}_illegal")),
        vma=bool(getattr(slot, f"{prefix}_vma")),
        vta=bool(getattr(slot, f"{prefix}_vta")),
        vsew=int(getattr(slot, f"{prefix}_vsew")),
        vlmul=int(getattr(slot, f"{prefix}_vlmul")),
    )


def _vtype_gen_state(env, register: str) -> VTypeState:
    prefix = f"Frontend_top.Frontend.inner_ibuffer.vtypeGen.{register}_"

    def field(name: str) -> int:
        signal = env.dut.GetInternalSignal(prefix + name)
        assert signal is not None, f"missing VTypeGen register: {prefix + name}"
        return int(signal.value)

    return VTypeState(
        illegal=bool(field("illegal")),
        vma=bool(field("vma")),
        vta=bool(field("vta")),
        vsew=field("vsew"),
        vlmul=field("vlmul"),
    )


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires a real DUT")
@pytest.mark.parametrize(
    ("walk_to_arch", "restored"),
    [
        (False, VTypeState(False, False, True, 1, 1)),
        (True, VTypeState.initial()),
    ],
    ids=("walk_vtype", "walk_to_arch_vtype"),
)
def test_vector_vtype_redirect_recovery(env, walk_to_arch: bool, restored: VTypeState) -> None:
    base = 0x80000000
    target = base + 0x400
    vset = 0x0D0072D7  # vsetvli t0, zero, e32, m1, ta, ma
    old_instr = 0x00128293  # addi t0, t0, 1
    target_instr = 0x00230313  # addi t1, t1, 2
    old_state = VTypeState(False, True, True, 2, 0)
    instructions = [vset] + [old_instr] * 255 + [target_instr] * 256
    LoadProgramSequence(
        image=ProgramImage(
            payload=b"".join(instr.to_bytes(4, "little") for instr in instructions),
            base_addr=base,
        ),
        step_cycles=0,
    ).run(env)
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)

    for _ in range(1200):
        env.step(1)
        slots = env.monitor.cycle_snapshot.cfvec.slots
        configured = [
            slot for slot in slots
            if slot.valid and slot.instr == vset and env.observed_cfvec_pc(slot.slot) == base
        ]
        if configured:
            assert _slot_vtype(configured[0], "specvtype") == VTypeState.initial()
            assert _slot_vtype(configured[0], "vtype") == old_state
            break
    else:
        raise AssertionError("old-path vsetvli was not observed")

    env.backend_model.set_can_accept(0)
    for _ in range(16):
        env.step(1)
        if any(slot.valid and slot.instr == old_instr for slot in env.monitor.cycle_snapshot.cfvec.slots):
            break
    else:
        raise AssertionError("old-path instruction was not held before redirect")

    env.backend_model.inject_redirect(target, "vtype-recovery", delay_cycles=0)
    for _ in range(8):
        env.step(1)
        if int(env.backend_to_ftq_if.redirect_valid.value) == 1:
            break
    else:
        raise AssertionError("backend redirect was not driven")

    frozen = env.monitor.cycle_snapshot.cfvec.slots
    assert any(slot.valid and slot.instr == old_instr for slot in frozen)
    to_ibuf = env.backend_to_ibuf_if
    to_ibuf.resuming_vtype.value = 1
    if walk_to_arch:
        to_ibuf.walk_to_arch_vtype.value = 1
    else:
        to_ibuf.walk_vtype_valid.value = 1
        for name in ("illegal", "vma", "vta", "vsew", "vlmul"):
            getattr(to_ibuf, f"walk_vtype_bits_{name}").value = int(getattr(restored, name))

    env.step(1)
    assert env.monitor.cycle_snapshot.cfvec.slots == frozen, "resumingVType changed cfVec output"
    to_ibuf.walk_to_arch_vtype.value = 0
    to_ibuf.walk_vtype_valid.value = 0
    env.step(1)
    flush = env.dut.GetInternalSignal("Frontend_top.Frontend.inner_needFlush")
    assert flush is not None and int(flush.value) == 1, "redirect did not reach IBuffer flush"
    env.step(1)
    assert not any(slot.valid for slot in env.monitor.cycle_snapshot.cfvec.slots), (
        "redirect flush did not clear old-path cfVec"
    )
    to_ibuf.resuming_vtype.value = 0
    env.backend_model.set_can_accept(1)

    seen_target: set[int] = set()
    for _ in range(1200):
        env.step(1)
        for slot in env.monitor.cycle_snapshot.cfvec.slots:
            if not slot.valid:
                continue
            assert slot.instr == target_instr, "non-target instruction appeared after flush"
            pc = env.observed_cfvec_pc(slot.slot)
            assert target <= pc < target + 0x400, f"non-target PC after flush: 0x{pc:x}"
            assert _slot_vtype(slot, "specvtype") == restored
            assert _slot_vtype(slot, "vtype") == restored
            if pc in (target, target + 4):
                seen_target.add(pc)
        if seen_target == {target, target + 4}:
            break
    assert seen_target == {target, target + 4}, "redirect target was not delivered"
    assert not env.get_errors()


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires a real DUT")
@pytest.mark.parametrize("has_vsetvl", [False, True], ids=("commit_vtype", "vsetvl_priority"))
def test_vector_vtype_backend_commit(env, has_vsetvl: bool) -> None:
    base = 0x80000000
    nop = 0x00000013
    committed = VTypeState(False, False, True, 1, 1)
    vsetvl = VTypeState(False, True, False, 3, 2)
    competing_walk = VTypeState(False, False, False, 0, 0)
    expected = vsetvl if has_vsetvl else committed
    LoadProgramSequence(
        image=ProgramImage(payload=nop.to_bytes(4, "little") * 1024, base_addr=base),
        step_cycles=0,
    ).run(env)
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)
    env.backend_model.set_can_accept(0)

    for _ in range(1200):
        env.step(1)
        frozen = env.monitor.cycle_snapshot.cfvec.slots
        if any(slot.valid for slot in frozen):
            break
    else:
        raise AssertionError("no cfVec output before backend vtype commit")
    frozen_pcs = {env.observed_cfvec_pc(slot.slot) for slot in frozen if slot.valid}
    assert _vtype_gen_state(env, "vtypeSpec") == VTypeState.initial()
    assert _vtype_gen_state(env, "vtypeArch") == VTypeState.initial()

    to_ibuf = env.backend_to_ibuf_if
    to_ibuf.resuming_vtype.value = 1
    to_ibuf.commit_vtype_valid.value = 1
    for name in ("illegal", "vma", "vta", "vsew", "vlmul"):
        getattr(to_ibuf, f"commit_vtype_bits_{name}").value = int(getattr(committed, name))
    if has_vsetvl:
        to_ibuf.commit_vtype_has_vsetvl.value = 1
        to_ibuf.walk_vtype_valid.value = 1
        for name in ("illegal", "vma", "vta", "vsew", "vlmul"):
            getattr(to_ibuf, f"vsetvl_vtype_{name}").value = int(getattr(vsetvl, name))
            getattr(to_ibuf, f"walk_vtype_bits_{name}").value = int(getattr(competing_walk, name))
    env.step(2)
    assert env.monitor.cycle_snapshot.cfvec.slots == frozen, "commit changed held cfVec output"
    assert _vtype_gen_state(env, "vtypeArch") == expected
    assert _vtype_gen_state(env, "vtypeSpec") == (
        expected if has_vsetvl else VTypeState.initial()
    )

    to_ibuf.commit_vtype_valid.value = 0
    to_ibuf.commit_vtype_has_vsetvl.value = 0
    to_ibuf.walk_vtype_valid.value = 0
    to_ibuf.walk_to_arch_vtype.value = 1
    env.step(2)
    assert _vtype_gen_state(env, "vtypeSpec") == expected
    to_ibuf.walk_to_arch_vtype.value = 0
    to_ibuf.resuming_vtype.value = 0
    env.backend_model.set_can_accept(1)

    seen_new_pcs: set[int] = set()
    for _ in range(1200):
        env.step(1)
        for slot in env.monitor.cycle_snapshot.cfvec.slots:
            if not slot.valid:
                continue
            pc = env.observed_cfvec_pc(slot.slot)
            if pc <= max(frozen_pcs):
                continue
            assert slot.instr == nop
            assert _slot_vtype(slot, "specvtype") == expected
            assert _slot_vtype(slot, "vtype") == expected
            seen_new_pcs.add(pc)
        if len(seen_new_pcs) >= 2:
            break
    assert len(seen_new_pcs) >= 2, "no new cfVec instructions after backend vtype update"
    assert not env.get_errors()
