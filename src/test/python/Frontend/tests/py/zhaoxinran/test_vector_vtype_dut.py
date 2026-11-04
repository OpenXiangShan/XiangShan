from __future__ import annotations

import os

import pytest

from env.checkers.vtype_checker import VTypeChecker
from env.core.transactions import ProgramImage
from env.model.golden_trace import GoldenTrace
from env.model.vtype import VTypeModel, VTypeState
from env.sequences import LoadProgramSequence
from env.runtime.frontend_init import FrontendInitConfig, initialize_frontend
from env.support import record_scenario, scenario_rng
from tests.py.support.bin_trace import run_bin_trace


pytestmark = pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)


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
    assert len(config_pcs) == 43
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
    assert sum(VTypeChecker._vector_config_kind(entry.instr) is not None for entry in trace.entries) == 37
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


def _drive_vtype_fields(bundle, prefix: str, state: VTypeState) -> None:
    for name in ("illegal", "vma", "vta", "vsew", "vlmul"):
        getattr(bundle, f"{prefix}_{name}").value = int(getattr(state, name))


def _encode_vsetvli(rd: int, rs1: int, zimm: int) -> int:
    return ((int(zimm) & 0x7FF) << 20) | ((int(rs1) & 0x1F) << 15) | (7 << 12) | (int(rd) << 7) | 0x57


def _encode_vsetivli(rd: int, uimm: int, zimm: int) -> int:
    return 0xC0000000 | ((int(zimm) & 0x3FF) << 20) | ((int(uimm) & 0x1F) << 15) | (7 << 12) | (int(rd) << 7) | 0x57


def _encode_vsetvl(rd: int = 1, rs1: int = 2, rs2: int = 3) -> int:
    return 0x80007057 | ((int(rs2) & 0x1F) << 20) | ((int(rs1) & 0x1F) << 15) | (int(rd) << 7)


def _load_vtype_program(env, instructions: list[int], base: int = 0x80000000) -> None:
    LoadProgramSequence(
        image=ProgramImage(
            payload=b"".join(instr.to_bytes(4, "little") for instr in instructions),
            base_addr=base,
        ),
        step_cycles=0,
    ).run(env)
    initialize_frontend(env, FrontendInitConfig(reset_vector=base, bare_mode=True, reset_cycles=20))
    env.monitor.clear()
    env.monitor.set_expected_pc(base)


def _wait_for_cfvec(env, predicate, *, description: str, max_cycles: int = 1200):
    for _ in range(max_cycles):
        env.step(1)
        slots = env.monitor.cycle_snapshot.cfvec.slots
        matches = [slot for slot in slots if slot.valid and predicate(slot)]
        if matches:
            return matches
    raise AssertionError(f"{description} was not observed in cfVec")


_STRESS_VSET_COUNT = 32


def _vtype_stress_stream() -> list[int]:
    settings = (0xD0, 0x49, 0x9A, 0x149)
    return [
        (_encode_vsetvli(1, 0, settings[index % 4])
         if index % 2 == 0 else _encode_vsetivli(1, 4, settings[index % 4]))
        for index in range(_STRESS_VSET_COUNT)
    ] + [0x00000013]


def _check_vtype_stress(env, start_pc: int, stream: list[int], initial: VTypeState):
    model = VTypeModel()
    model.state = initial
    expected = {start_pc + 4 * index: model.step(instr) for index, instr in enumerate(stream)}
    accepted: set[int] = set()
    can_accept_at_edge: dict[int, bool] = {}

    def capture_accept(cycle: int, active_env) -> None:
        to_ibuf = active_env.backend_to_ibuf_if
        can_accept_at_edge[int(cycle)] = bool(int(to_ibuf.decode_can_accept.value)) and not bool(
            int(to_ibuf.resuming_vtype.value)
        )

    def check(cycle: int, active_env) -> None:
        for slot in active_env.monitor.cycle_snapshot.cfvec.slots:
            if not slot.valid:
                continue
            pc = active_env.observed_cfvec_pc(slot.slot)
            transition = expected.get(pc)
            if transition is None:
                continue
            assert slot.instr == transition.instruction, f"stress instruction mismatch at 0x{pc:x}"
            assert _slot_vtype(slot, "specvtype") == transition.specvtype, (
                f"stress specvtype mismatch at 0x{pc:x}"
            )
            assert _slot_vtype(slot, "vtype") == transition.vtype, f"stress vtype mismatch at 0x{pc:x}"
            if can_accept_at_edge[int(cycle)]:
                accepted.add(pc)

    env.register_pre_drive_cycle_observer(capture_accept)
    env.register_cycle_observer(check)

    def finish(max_cycles: int = 1200) -> None:
        for _ in range(max_cycles):
            if accepted == expected.keys():
                break
            env.step(1)
        assert accepted == expected.keys(), (
            f"only {len(accepted) - int(start_pc + 4 * (len(stream) - 1) in accepted)}"
            f"/{_STRESS_VSET_COUNT} vset instructions accepted; missing={sorted(expected.keys() - accepted)}"
        )
        assert not env.get_errors()

    return finish


def _load_vtype_stress_program(
    env, program: list[int], initial: VTypeState, base: int = 0x80000000
):
    stream = _vtype_stress_stream()
    start_pc = base + 4 * (len(program) + 16)
    _load_vtype_program(env, program + [0x00000013] * 16 + stream + [0x00000013] * 1024, base)
    return _check_vtype_stress(env, start_pc, stream, initial)


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires a real DUT")
@pytest.mark.parametrize("spacing", [0, 16], ids=("same_fetch_block", "separate_fetch_blocks"))
@pytest.mark.parametrize("sequence", ["plain_vsetvl_plain", "multiple_imm", "imm_then_vsetvl", "vsetvl_then_imm"])
def test_vector_vtype_instruction_order(env, sequence: str, spacing: int) -> None:
    base = 0x80000000
    nop = 0x00000013
    state_a = VTypeState(False, True, True, 2, 0)
    state_b = VTypeState(False, False, True, 1, 1)
    state_c = VTypeState(False, True, False, 3, 2)
    seed = _encode_vsetvli(1, 0, 0xD0)
    vset_b = _encode_vsetivli(1, 4, 0x49)
    vset_c = _encode_vsetvli(1, 2, 0x9A)
    vsetvl = _encode_vsetvl()
    cases = {
        "plain_vsetvl_plain": [(nop, state_a, state_a), (vsetvl, state_a, state_a), (nop, state_a, state_a)],
        "multiple_imm": [(vset_b, state_a, state_b), (nop, state_b, state_b), (vset_c, state_b, state_c)],
        "imm_then_vsetvl": [(vset_b, state_a, state_b), (vsetvl, state_b, state_b)],
        "vsetvl_then_imm": [(vsetvl, state_a, state_a), (vset_c, state_a, state_c)],
    }
    program = [seed] + [nop] * 16
    expected = {}
    focus_pcs = []
    for instr, before, after in cases[sequence]:
        pc = base + 4 * len(program)
        expected[pc] = (instr, before, after)
        focus_pcs.append(pc)
        program.append(instr)
        program.extend([nop] * spacing)
    finish_stress = _load_vtype_stress_program(env, program, cases[sequence][-1][2], base)

    observed = {}
    for _ in range(1200):
        env.step(1)
        snapshot = env.monitor.cycle_snapshot.cfvec
        for slot in snapshot.slots:
            if not slot.valid:
                continue
            pc = env.observed_cfvec_pc(slot.slot)
            if pc not in expected or pc in observed:
                continue
            instr, before, after = expected[pc]
            assert slot.instr == instr, f"wrong instruction at 0x{pc:x}"
            assert _slot_vtype(slot, "specvtype") == before, f"wrong specvtype at 0x{pc:x}"
            assert _slot_vtype(slot, "vtype") == after, f"wrong vtype at 0x{pc:x}"
            observed[pc] = int(snapshot.cycle)
        if len(observed) == len(expected):
            break
    assert set(observed) == set(expected), f"missing cfVec PCs: {set(expected) - set(observed)}"
    if spacing:
        assert len({observed[pc] for pc in focus_pcs}) > 1
    else:
        assert len({observed[pc] for pc in focus_pcs}) == 1
    finish_stress()
    assert not env.get_errors()


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires a real DUT")
@pytest.mark.parametrize("ordinal", range(3))
def test_vector_vtype_random_stream(env, ordinal: int) -> None:
    scenario_key = "zhaoxinran/vtype/random_stream"
    base_seed, seed, rng = scenario_rng(scenario_key, ordinal=ordinal)
    base = 0x80000000
    nop = 0x00000013
    cases = (["vsetvli", "vsetivli"] * 14) + (["vsetvli_reserved", "vsetivli_reserved"] * 2)
    assert len(cases) == _STRESS_VSET_COUNT
    rng.shuffle(cases)
    program = []
    for kind in cases:
        zimm = rng.randrange(4) | (rng.randrange(4) << 3) | (rng.randrange(4) << 6)
        if kind.endswith("_reserved"):
            zimm |= 1 << 8
        rd = rng.randrange(1, 32)
        operand = rng.randrange(1, 32)
        instr = (
            _encode_vsetivli(rd, operand, zimm)
            if kind.startswith("vsetivli") else _encode_vsetvli(rd, operand, zimm)
        )
        program.append(instr)
        program.extend([nop] * rng.randrange(6))
    program.append(nop)
    record_scenario(
        env, scenario_key, base_seed=base_seed, seed=seed, ordinal=ordinal,
        parameters={"program": [f"0x{instr:08x}" for instr in program]},
    )
    model = VTypeModel()
    expected = {base + 4 * index: model.step(instr) for index, instr in enumerate(program)}
    _load_vtype_program(env, program + [nop] * 1024, base)

    accept_at_edge: dict[int, bool] = {}

    def capture_accept(cycle: int, active_env) -> None:
        to_ibuf = active_env.backend_to_ibuf_if
        accept_at_edge[int(cycle)] = bool(int(to_ibuf.decode_can_accept.value)) and not bool(
            int(to_ibuf.resuming_vtype.value)
        )

    env.register_pre_drive_cycle_observer(capture_accept)
    accepted = set()
    stalled_cycles = 0
    for _ in range(1200):
        can_accept = rng.randrange(4) != 0
        env.backend_model.set_can_accept(int(can_accept))
        env.step(1)
        slots = env.monitor.cycle_snapshot.cfvec.slots
        edge_accept = accept_at_edge[int(env.monitor.cycle_snapshot.cfvec.cycle)]
        if not edge_accept and any(slot.valid for slot in slots):
            stalled_cycles += 1
        for slot in slots:
            if not slot.valid:
                continue
            pc = env.observed_cfvec_pc(slot.slot)
            transition = expected.get(pc)
            if transition is None:
                continue
            assert slot.instr == transition.instruction, f"seed={seed} pc=0x{pc:x}: wrong instruction"
            assert _slot_vtype(slot, "specvtype") == transition.specvtype, (
                f"seed={seed} pc=0x{pc:x}: wrong specvtype"
            )
            assert _slot_vtype(slot, "vtype") == transition.vtype, f"seed={seed} pc=0x{pc:x}: wrong vtype"
            if edge_accept:
                accepted.add(pc)
        if len(accepted) == len(expected):
            break
    assert accepted == expected.keys(), f"seed={seed}: missing accepted cfVec PCs {expected.keys() - accepted}"
    assert stalled_cycles > 0, f"seed={seed}: no backpressure applied to valid cfVec output"
    assert not env.get_errors()


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires a real DUT")
@pytest.mark.parametrize(
    ("scenario", "spacing"),
    [
        ("vsetvli_reserved", 0),
        ("vsetivli_reserved", 0),
        ("unsupported_sew_lmul", 0),
        ("keep_vl_same_vlmax", 0),
        ("keep_vl_same_vlmax", 16),
        ("keep_vl_changed_vlmax", 0),
        ("keep_vl_changed_vlmax", 16),
    ],
    ids=(
        "vsetvli_reserved", "vsetivli_reserved", "unsupported_sew_lmul",
        "keep_vl_same_vlmax_same_cycle", "keep_vl_same_vlmax_cross_cycle",
        "keep_vl_changed_vlmax_same_cycle", "keep_vl_changed_vlmax_cross_cycle",
    ),
)
def test_vector_vtype_special_configuration(env, scenario: str, spacing: int) -> None:
    base = 0x80000000
    nop = 0x00000013
    state_a = VTypeState(False, True, True, 2, 0)
    state_b = VTypeState(False, False, True, 1, 1)
    seed = _encode_vsetvli(1, 0, 0xD0)
    cases = {
        "vsetvli_reserved": (
            [_encode_vsetvli(1, 2, 0x149)], state_a,
            VTypeState(True, False, False, 0, 0),
        ),
        "vsetivli_reserved": (
            [_encode_vsetivli(1, 4, 0x149)], state_a,
            VTypeState(True, False, False, 0, 0),
        ),
        "unsupported_sew_lmul": (
            [_encode_vsetivli(1, 4, 0x9F)], state_a,
            VTypeState(True, False, False, 0, 0),
        ),
        "keep_vl_same_vlmax": (
            [_encode_vsetivli(1, 4, 0x49), _encode_vsetvli(0, 0, 0x49)], state_b,
            state_b,
        ),
        "keep_vl_changed_vlmax": (
            [_encode_vsetivli(1, 4, 0x49), _encode_vsetvli(0, 0, 0xD0)], state_b,
            VTypeState(True, True, True, 2, 0),
        ),
    }
    special_sequence, before, after = cases[scenario]
    if spacing:
        special_sequence = [special_sequence[0]] + [nop] * spacing + special_sequence[1:]
    program = [seed] + [nop] * 16 + special_sequence + [nop]
    special_pc = base + 4 * (17 + len(special_sequence) - 1)
    next_pc = special_pc + 4
    finish_stress = _load_vtype_stress_program(env, program, after, base)

    seen = set()
    cycles = {}
    keep_vl = scenario.startswith("keep_vl_")
    for _ in range(1200):
        env.step(1)
        for slot in env.monitor.cycle_snapshot.cfvec.slots:
            if not slot.valid:
                continue
            pc = env.observed_cfvec_pc(slot.slot)
            if keep_vl and pc == base + 4 * 17:
                assert _slot_vtype(slot, "vtype") == state_b
                cycles[pc] = int(env.monitor.cycle_snapshot.cfvec.cycle)
            if pc == special_pc:
                assert slot.instr == special_sequence[-1]
                assert _slot_vtype(slot, "specvtype") == before
                assert _slot_vtype(slot, "vtype") == after
                seen.add(pc)
                cycles[pc] = int(env.monitor.cycle_snapshot.cfvec.cycle)
            elif pc == next_pc:
                assert slot.instr == nop
                assert _slot_vtype(slot, "specvtype") == after
                assert _slot_vtype(slot, "vtype") == after
                seen.add(pc)
        if seen == {special_pc, next_pc}:
            break
    assert seen == {special_pc, next_pc}, f"missing special configuration outputs: {seen}"
    if keep_vl:
        assert base + 4 * 17 in cycles
        assert (cycles[base + 4 * 17] == cycles[special_pc]) == (spacing == 0)
    env.step(1)
    finish_stress()
    assert not env.get_errors()


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires a real DUT")
@pytest.mark.parametrize("illegal_kind", ["vsetvli", "vsetivli"])
def test_vector_vtype_legal_after_illegal(env, illegal_kind: str) -> None:
    base = 0x80000000
    nop = 0x00000013
    state_a = VTypeState(False, True, True, 2, 0)
    state_b = VTypeState(False, False, True, 1, 1)
    illegal = VTypeState(True, False, False, 0, 0)
    reserved_vset = (
        _encode_vsetvli(1, 2, 0x149)
        if illegal_kind == "vsetvli" else _encode_vsetivli(1, 4, 0x149)
    )
    legal_vset = _encode_vsetvli(1, 2, 0x49)
    program = [_encode_vsetvli(1, 0, 0xD0)] + [nop] * 16 + [reserved_vset, legal_vset, nop]
    finish_stress = _load_vtype_stress_program(env, program, state_b, base)

    expected = {
        base + 4 * 17: (reserved_vset, state_a, illegal),
        base + 4 * 18: (legal_vset, illegal, state_b),
        base + 4 * 19: (nop, state_b, state_b),
    }
    observed = set()
    for _ in range(1200):
        env.step(1)
        for slot in env.monitor.cycle_snapshot.cfvec.slots:
            if not slot.valid:
                continue
            pc = env.observed_cfvec_pc(slot.slot)
            if pc not in expected:
                continue
            instr, before, after = expected[pc]
            assert slot.instr == instr
            assert _slot_vtype(slot, "specvtype") == before
            assert _slot_vtype(slot, "vtype") == after
            observed.add(pc)
        if observed == expected.keys():
            break
    assert observed == expected.keys(), f"missing cfVec PCs: {expected.keys() - observed}"
    env.step(1)
    finish_stress()
    assert not env.get_errors()


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires a real DUT")
@pytest.mark.parametrize(
    "scenario", [
        "payload_without_commit", "equal_resuming", "different_resuming",
        "different_running", "walk_equal", "walk_different",
    ]
)
def test_vector_vsetvl_feedback_fields(env, scenario: str) -> None:
    base = 0x80000000
    nop = 0x00000013
    state_a = VTypeState(False, True, True, 2, 0)
    state_b = VTypeState(False, False, True, 1, 1)
    state_c = VTypeState(False, True, False, 3, 2)
    seed = _encode_vsetvli(1, 0, 0xD0)
    vsetvl = _encode_vsetvl()
    next_vset = _encode_vsetvli(1, 2, 0x9A)
    program = [seed] + [nop] * 16 + [vsetvl] + [nop] * 32 + [next_vset, nop]
    finish_stress = _load_vtype_stress_program(env, program, state_c, base)

    first = _wait_for_cfvec(env, lambda slot: slot.instr == vsetvl, description="vsetvl before feedback")
    assert _slot_vtype(first[0], "specvtype") == state_a
    assert _slot_vtype(first[0], "vtype") == state_a
    env.backend_model.set_can_accept(0)
    to_ibuf = env.backend_to_ibuf_if
    _drive_vtype_fields(to_ibuf, "vsetvl_vtype", state_a)

    if scenario == "payload_without_commit":
        env.step(1)
        _drive_vtype_fields(to_ibuf, "vsetvl_vtype", state_b)
        env.step(1)
        assert _vtype_gen_state(env, "vtypeSpec") == state_a
        assert _vtype_gen_state(env, "vtypeArch") == VTypeState.initial()
        expected = state_a
    else:
        expected = state_a if scenario == "equal_resuming" else state_b
        to_ibuf.commit_vtype_valid.value = 1
        to_ibuf.commit_vtype_has_vsetvl.value = 1
        _drive_vtype_fields(to_ibuf, "commit_vtype_bits", state_a)
        _drive_vtype_fields(to_ibuf, "vsetvl_vtype", expected)
        if scenario in ("walk_equal", "walk_different"):
            to_ibuf.walk_vtype_valid.value = 1
            _drive_vtype_fields(
                to_ibuf, "walk_vtype_bits", state_c if scenario == "walk_different" else expected
            )
        if scenario != "different_running":
            to_ibuf.resuming_vtype.value = 1
        env.step(2)
        assert _vtype_gen_state(env, "vtypeSpec") == expected
        assert _vtype_gen_state(env, "vtypeArch") == expected
        to_ibuf.commit_vtype_valid.value = 0
        to_ibuf.commit_vtype_has_vsetvl.value = 0
        to_ibuf.walk_vtype_valid.value = 0
        to_ibuf.resuming_vtype.value = 0

    env.backend_model.set_can_accept(1)
    next_slots = _wait_for_cfvec(env, lambda slot: slot.instr == next_vset, description="vsetvli after vsetvl feedback")
    assert _slot_vtype(next_slots[0], "specvtype") == expected
    assert _slot_vtype(next_slots[0], "vtype") == state_c
    env.step(1)
    finish_stress()
    assert not env.get_errors()


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires a real DUT")
def test_vector_vtype_full_cfvec_backpressure(env) -> None:
    base = 0x80000000
    nop = 0x00000013
    state_a = VTypeState(False, True, True, 2, 0)
    state_b = VTypeState(False, False, True, 1, 1)
    seed = _encode_vsetvli(1, 0, 0xD0)
    next_vset = _encode_vsetivli(1, 4, 0x49)
    next_pc = base + 4 * 17
    program = [seed] + [nop] * 16 + [next_vset, nop]
    finish_stress = _load_vtype_stress_program(env, program, state_b, base)
    env.backend_model.set_can_accept(0)

    held = None
    for _ in range(1200):
        env.step(1)
        slots = env.monitor.cycle_snapshot.cfvec.slots
        if len(slots) == 8 and all(slot.valid for slot in slots):
            held = slots
            break
    assert held is not None, "cfVec never filled under backend backpressure"
    assert all(env.observed_cfvec_pc(slot.slot) < next_pc for slot in held)
    for _ in range(3):
        env.step(1)
        assert env.monitor.cycle_snapshot.cfvec.slots == held

    env.backend_model.set_can_accept(1)
    observed = set()
    for _ in range(1200):
        env.step(1)
        for slot in env.monitor.cycle_snapshot.cfvec.slots:
            if not slot.valid:
                continue
            pc = env.observed_cfvec_pc(slot.slot)
            if pc == next_pc:
                assert slot.instr == next_vset
                assert _slot_vtype(slot, "specvtype") == state_a
                assert _slot_vtype(slot, "vtype") == state_b
                observed.add(pc)
            elif pc == next_pc + 4:
                assert slot.instr == nop
                assert _slot_vtype(slot, "specvtype") == state_b
                assert _slot_vtype(slot, "vtype") == state_b
                observed.add(pc)
        if observed == {next_pc, next_pc + 4}:
            break
    assert observed == {next_pc, next_pc + 4}, "backpressured vsetvli did not reach cfVec"
    finish_stress()
    assert not env.get_errors()


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires a real DUT")
@pytest.mark.parametrize("recovery", ["no_feedback", "walk_then_commit", "arch_walk_then_commit"])
def test_vector_vsetvl_redirect_feedback_order(env, recovery: str) -> None:
    base = 0x80000000
    target = base + 0x400
    nop = 0x00000013
    old_instr = 0x00128293
    state_a = VTypeState(False, True, True, 2, 0)
    state_b = VTypeState(False, False, True, 1, 1)
    state_c = VTypeState(False, True, False, 3, 2)
    seed = _encode_vsetvli(1, 0, 0xD0)
    vsetvl = _encode_vsetvl()
    target_vset = _encode_vsetvli(1, 2, 0x9A)
    program = [seed] + [nop] * 16 + [vsetvl] + [old_instr] * (256 - 18)
    finish_stress = _load_vtype_stress_program(env, program + [target_vset, nop], state_c, base)

    slots = _wait_for_cfvec(env, lambda slot: slot.instr == vsetvl, description="vsetvl before redirect")
    assert _slot_vtype(slots[0], "specvtype") == state_a
    assert _slot_vtype(slots[0], "vtype") == state_a
    env.backend_model.set_can_accept(0)
    to_ibuf = env.backend_to_ibuf_if
    if recovery == "no_feedback":
        env.step(2)
    if recovery == "arch_walk_then_commit":
        to_ibuf.resuming_vtype.value = 1
        to_ibuf.walk_to_arch_vtype.value = 1
        env.step(2)
        assert _vtype_gen_state(env, "vtypeSpec") == VTypeState.initial()
        to_ibuf.walk_to_arch_vtype.value = 0

    env.backend_model.inject_redirect(target, "vsetvl-vtype-recovery", delay_cycles=0)
    for _ in range(8):
        env.step(1)
        if int(env.backend_to_ftq_if.redirect_valid.value):
            break
    else:
        raise AssertionError("vsetvl redirect was not driven")
    if recovery != "no_feedback":
        to_ibuf.resuming_vtype.value = 1
        to_ibuf.walk_vtype_valid.value = 1
        _drive_vtype_fields(to_ibuf, "walk_vtype_bits", state_a)
    else:
        assert not int(to_ibuf.walk_vtype_valid.value)
        assert not int(to_ibuf.commit_vtype_valid.value)
    env.step(1)
    to_ibuf.walk_vtype_valid.value = 0
    env.step(1)
    flush = env.dut.GetInternalSignal("Frontend_top.Frontend.inner_needFlush")
    assert flush is not None and int(flush.value) == 1
    env.step(1)
    assert not any(slot.valid for slot in env.monitor.cycle_snapshot.cfvec.slots), (
        "redirect did not clear old-path cfVec"
    )

    expected = state_a
    if recovery != "no_feedback":
        for _ in range(24):
            env.step(1)
            assert not any(slot.valid for slot in env.monitor.cycle_snapshot.cfvec.slots), (
                "new-path cfVec output appeared while resumingVType was asserted"
            )
        to_ibuf.commit_vtype_valid.value = 1
        to_ibuf.commit_vtype_has_vsetvl.value = 1
        _drive_vtype_fields(to_ibuf, "commit_vtype_bits", state_a)
        _drive_vtype_fields(to_ibuf, "vsetvl_vtype", state_b)
        env.step(2)
        assert _vtype_gen_state(env, "vtypeArch") == state_b
        assert _vtype_gen_state(env, "vtypeSpec") == state_b
        assert not any(slot.valid for slot in env.monitor.cycle_snapshot.cfvec.slots)
        to_ibuf.commit_vtype_valid.value = 0
        to_ibuf.commit_vtype_has_vsetvl.value = 0
        to_ibuf.resuming_vtype.value = 0
        expected = state_b
    env.backend_model.set_can_accept(1)

    seen = set()
    for _ in range(1200):
        env.step(1)
        for slot in env.monitor.cycle_snapshot.cfvec.slots:
            if not slot.valid:
                continue
            pc = env.observed_cfvec_pc(slot.slot)
            assert target <= pc < target + 0x400, f"old-path instruction after redirect: 0x{pc:x}"
            if pc == target:
                assert slot.instr == target_vset
                assert _slot_vtype(slot, "specvtype") == expected
                assert _slot_vtype(slot, "vtype") == state_c
                seen.add(pc)
            elif pc == target + 4:
                assert slot.instr == nop
                assert _slot_vtype(slot, "specvtype") == state_c
                assert _slot_vtype(slot, "vtype") == state_c
                seen.add(pc)
        if seen == {target, target + 4}:
            break
    assert seen == {target, target + 4}, "new-path vsetvli did not reach cfVec"
    finish_stress()
    assert not env.get_errors()


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires a real DUT")
@pytest.mark.parametrize("committed_kind", ["vsetvli", "vsetivli"])
@pytest.mark.parametrize("output_kind", ["vsetvli", "vsetivli"])
def test_vector_vtype_commit_with_new_vset(env, committed_kind: str, output_kind: str) -> None:
    base = 0x80000000
    nop = 0x00000013
    state_a = VTypeState(False, True, True, 2, 0)
    state_b = VTypeState(False, False, True, 1, 1)
    state_c = VTypeState(False, True, False, 3, 2)
    encode = {
        "vsetvli": lambda zimm: _encode_vsetvli(1, 2, zimm),
        "vsetivli": lambda zimm: _encode_vsetivli(1, 4, zimm),
    }
    committed_vset = encode[committed_kind](0xD0)
    output_vset = encode[output_kind](0x49)
    next_vset = _encode_vsetvli(1, 2, 0x9A)
    program = [committed_vset] + [nop] * 16 + [output_vset] + [nop] * 16 + [next_vset, nop]
    finish_stress = _load_vtype_stress_program(env, program, state_c, base)

    first = _wait_for_cfvec(env, lambda slot: slot.instr == committed_vset, description="vset before commit")
    assert _slot_vtype(first[0], "vtype") == state_a
    to_ibuf = env.backend_to_ibuf_if
    commit_cycles = []
    overlap_cycles = []
    output_pc = base + 4 * 17
    # The pre-drive callback schedules commit for the next edge.
    preceding_foldpc = ((output_pc - 8 - base) >> 1) | 1

    def drive_commit_with_output(cycle: int, active_env) -> None:
        cfvec = active_env.backend_cfvec_if
        if commit_cycles:
            if int(to_ibuf.commit_vtype_valid.value) and any(
                int(cfvec.cfvec_valid[lane].value)
                and int(cfvec.cfvec_instr[lane].value) == output_vset
                for lane in range(8)
            ):
                overlap_cycles.append(int(cycle))
            to_ibuf.commit_vtype_valid.value = 0
            return
        if not any(
            int(cfvec.cfvec_valid[lane].value)
            and int(cfvec.cfvec_foldpc[lane].value) == preceding_foldpc
            for lane in range(8)
        ):
            return
        to_ibuf.commit_vtype_valid.value = 1
        to_ibuf.commit_vtype_has_vsetvl.value = 0
        _drive_vtype_fields(to_ibuf, "commit_vtype_bits", state_a)
        assert int(to_ibuf.commit_vtype_valid.value) == 1
        commit_cycles.append(int(cycle))

    env.register_pre_drive_cycle_observer(drive_commit_with_output)
    next_pc = base + 4 * 34
    next_observed = []

    def observe_next_vset(_cycle: int, active_env) -> None:
        for slot in active_env.monitor.cycle_snapshot.cfvec.slots:
            if slot.valid and active_env.observed_cfvec_pc(slot.slot) == next_pc:
                next_observed.append((slot.instr, _slot_vtype(slot, "specvtype"), _slot_vtype(slot, "vtype")))

    env.register_cycle_observer(observe_next_vset)
    output = _wait_for_cfvec(env, lambda slot: slot.instr == output_vset, description="vset at commit boundary")
    assert _slot_vtype(output[0], "specvtype") == state_a
    assert _slot_vtype(output[0], "vtype") == state_b
    env.step(2)
    assert len(commit_cycles) == len(overlap_cycles) == 1, (
        "commitVType and the new vset never overlapped at the DUT ports"
    )
    assert _vtype_gen_state(env, "vtypeArch") == state_a

    for _ in range(1200):
        if next_observed:
            break
        env.step(1)
    assert next_observed, "vset after commit boundary was not observed in cfVec"
    assert all(entry == (next_vset, state_b, state_c) for entry in next_observed)
    env.step(1)
    finish_stress()
    assert not env.get_errors()


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires a real DUT")
@pytest.mark.parametrize("kind", ["vsetvli", "vsetivli"])
def test_vector_vtype_fetch_fault_on_vset(env, kind: str) -> None:
    base = 0x80000000
    nop = 0x00000013
    state_a = VTypeState(False, True, True, 2, 0)
    state_b = VTypeState(False, False, True, 1, 1)
    seed = _encode_vsetvli(1, 0, 0xD0)
    faulting_vset = (
        _encode_vsetvli(1, 2, 0x49) if kind == "vsetvli" else _encode_vsetivli(1, 4, 0x49)
    )
    stress = _vtype_stress_stream()
    target = base + 4 * (len(stress) + 31)
    program = stress + [seed] + [nop] * 30 + [faulting_vset] + [nop] * 1024
    LoadProgramSequence(
        image=ProgramImage(
            payload=b"".join(instr.to_bytes(4, "little") for instr in program), base_addr=base
        ),
        step_cycles=0,
    ).run(env)
    env.icache_agent.inject_response_fault_at(target, denied=1)
    initialize_frontend(env, FrontendInitConfig(reset_vector=base, bare_mode=True, reset_cycles=20))
    env.monitor.clear()
    env.monitor.set_expected_pc(base)
    finish_stress = _check_vtype_stress(env, base, stress, VTypeState.initial())

    for _ in range(1200):
        env.step(1)
        for slot in env.monitor.cycle_snapshot.cfvec.slots:
            if not slot.valid or env.observed_cfvec_pc(slot.slot) != target:
                continue
            assert slot.instr == faulting_vset
            assert slot.exception_bits & (1 << 1), "instruction access fault was not marked"
            assert _slot_vtype(slot, "specvtype") == state_a
            assert _slot_vtype(slot, "vtype") == state_b
            finish_stress(max_cycles=0)
            assert not env.get_errors()
            return
    raise AssertionError("faulting vset instruction was not observed in cfVec")


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
    target_vtype = VTypeState(False, True, False, 1, 1)
    target_vset = _encode_vsetvli(1, 0, (1 << 7) | (1 << 3) | 1)
    old_state = VTypeState(False, True, True, 2, 0)
    instructions = [vset] + [old_instr] * 255 + [target_vset, target_instr]
    finish_stress = _load_vtype_stress_program(env, instructions, target_vtype, base)

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
            pc = env.observed_cfvec_pc(slot.slot)
            assert target <= pc < target + 0x400, f"non-target PC after flush: 0x{pc:x}"
            if pc == target:
                assert slot.instr == target_vset
                assert _slot_vtype(slot, "specvtype") == restored
                assert _slot_vtype(slot, "vtype") == target_vtype
            elif pc == target + 4:
                assert slot.instr == target_instr, "non-target instruction appeared after flush"
                assert _slot_vtype(slot, "specvtype") == target_vtype
                assert _slot_vtype(slot, "vtype") == target_vtype
            if pc in (target, target + 4):
                seen_target.add(pc)
        if seen_target == {target, target + 4}:
            break
    assert seen_target == {target, target + 4}, "redirect target was not delivered"
    finish_stress()
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
    finish_stress = _load_vtype_stress_program(env, [nop] * 64, expected, base)
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
    finish_stress()
    assert not env.get_errors()


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires a real DUT")
def test_vector_vtype_resuming_stalls_and_releases_ibuffer_output(env) -> None:
    base = 0x80000000
    nop = 0x00000013
    finish_stress = _load_vtype_stress_program(env, [nop] * 64, VTypeState.initial(), base)

    to_ibuf = env.backend_to_ibuf_if
    env.backend_model.set_can_accept(0)
    to_ibuf.resuming_vtype.value = 0

    held_slots = None
    for _ in range(1200):
        env.step(1)
        slots = env.monitor.cycle_snapshot.cfvec.slots
        if any(slot.valid for slot in slots):
            held_slots = slots
            break
    assert held_slots is not None, "no pending IBuffer output was observed"

    env.backend_model.set_can_accept(1)
    env.step(1)
    assert env.monitor.cycle_snapshot.cfvec.slots == held_slots
    to_ibuf.resuming_vtype.value = 1
    for _ in range(3):
        env.step(1)
        assert env.monitor.cycle_snapshot.cfvec.slots == held_slots, (
            "resumingVType changed cfVec while IBuffer output was paused"
        )

    to_ibuf.resuming_vtype.value = 0
    advanced = False
    for _ in range(16):
        env.step(1)
        slots = env.monitor.cycle_snapshot.cfvec.slots
        if slots != held_slots:
            advanced = True
            assert all(not slot.valid or slot.instr == nop for slot in slots)
            break
    assert advanced, "IBuffer did not resume output after resumingVType was lowered"
    finish_stress()
    assert not env.get_errors()


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires a real DUT")
def test_vector_vtype_walk_to_arch_and_commit_same_cycle(env) -> None:
    base = 0x80000000
    nop = 0x00000013
    committed = VTypeState(False, True, True, 1, 1)
    speculative = VTypeState(False, False, False, 2, 0)
    same_cycle_commit = VTypeState(False, True, False, 3, 2)
    vsetvli_a = _encode_vsetvli(1, 0, (1 << 7) | (1 << 6) | (1 << 3) | 1)
    vsetvli_b = _encode_vsetvli(1, 0, 2 << 3)
    program = [vsetvli_a] + [nop] * 16 + [vsetvli_b, nop]
    finish_stress = _load_vtype_stress_program(env, program, same_cycle_commit, base)

    env.backend_model.set_can_accept(1)
    first = _wait_for_cfvec(
        env,
        lambda slot: slot.instr == vsetvli_a,
        description="initial vsetvli",
    )
    assert _slot_vtype(first[0], "specvtype") == VTypeState.initial()
    assert _slot_vtype(first[0], "vtype") == committed

    env.backend_model.set_can_accept(0)
    to_ibuf = env.backend_to_ibuf_if
    to_ibuf.resuming_vtype.value = 1
    to_ibuf.commit_vtype_valid.value = 1
    to_ibuf.commit_vtype_has_vsetvl.value = 0
    _drive_vtype_fields(to_ibuf, "commit_vtype_bits", committed)
    env.step(2)
    assert _vtype_gen_state(env, "vtypeArch") == committed
    to_ibuf.commit_vtype_valid.value = 0
    to_ibuf.resuming_vtype.value = 0
    env.backend_model.set_can_accept(1)

    slots = _wait_for_cfvec(
        env,
        lambda slot: slot.instr == vsetvli_b,
        description="vsetvli with speculative configuration",
    )
    assert _slot_vtype(slots[0], "specvtype") == committed
    assert _slot_vtype(slots[0], "vtype") == speculative

    to_ibuf.resuming_vtype.value = 1
    _drive_vtype_fields(to_ibuf, "commit_vtype_bits", same_cycle_commit)
    to_ibuf.commit_vtype_valid.value = 1
    to_ibuf.commit_vtype_has_vsetvl.value = 0
    to_ibuf.walk_to_arch_vtype.value = 1
    env.backend_model.set_can_accept(1)
    env.step(2)

    assert _vtype_gen_state(env, "vtypeArch") == same_cycle_commit
    assert _vtype_gen_state(env, "vtypeSpec") == committed
    to_ibuf.commit_vtype_valid.value = 0
    to_ibuf.walk_to_arch_vtype.value = 0
    to_ibuf.resuming_vtype.value = 0
    env.backend_model.set_can_accept(1)
    assert _vtype_gen_state(env, "vtypeSpec") == committed
    assert _vtype_gen_state(env, "vtypeArch") == same_cycle_commit
    finish_stress()
    assert not env.get_errors()


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires a real DUT")
@pytest.mark.parametrize("walk_equal_commit", [True, False], ids=("walk_equal", "walk_different"))
def test_vector_vtype_commit_and_walk_same_cycle(env, walk_equal_commit: bool) -> None:
    base = 0x80000000
    nop = 0x00000013
    prior = VTypeState(False, False, True, 1, 1)
    committed = VTypeState(False, True, True, 2, 0)
    walked = committed if walk_equal_commit else VTypeState(False, False, True, 3, 2)
    new_config = VTypeState(False, True, False, 0, 0)
    vset_prior = _encode_vsetvli(1, 0, (1 << 6) | (1 << 3) | 1)
    vset_new = _encode_vsetvli(1, 0, (1 << 7))
    program = [vset_prior] + [nop] * 16 + [vset_new, nop]
    finish_stress = _load_vtype_stress_program(env, program, new_config, base)

    prior_slots = _wait_for_cfvec(
        env,
        lambda slot: slot.instr == vset_prior,
        description="prior vsetvli",
    )
    assert _slot_vtype(prior_slots[0], "vtype") == prior

    env.backend_model.set_can_accept(0)
    to_ibuf = env.backend_to_ibuf_if
    to_ibuf.resuming_vtype.value = 1
    to_ibuf.commit_vtype_valid.value = 1
    to_ibuf.commit_vtype_has_vsetvl.value = 0
    _drive_vtype_fields(to_ibuf, "commit_vtype_bits", committed)
    to_ibuf.walk_vtype_valid.value = 1
    _drive_vtype_fields(to_ibuf, "walk_vtype_bits", walked)
    env.step(2)

    assert _vtype_gen_state(env, "vtypeArch") == committed
    assert _vtype_gen_state(env, "vtypeSpec") == walked
    to_ibuf.commit_vtype_valid.value = 0
    to_ibuf.walk_vtype_valid.value = 0
    to_ibuf.resuming_vtype.value = 0
    env.backend_model.set_can_accept(1)

    new_slots = _wait_for_cfvec(
        env,
        lambda slot: slot.instr == vset_new,
        description="new-path vsetvli",
    )
    assert _slot_vtype(new_slots[0], "specvtype") == walked
    assert _slot_vtype(new_slots[0], "vtype") == new_config
    env.step(1)
    finish_stress()
    assert not env.get_errors()
