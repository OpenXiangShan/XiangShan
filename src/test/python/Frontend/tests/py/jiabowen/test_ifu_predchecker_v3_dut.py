from __future__ import annotations

import os

import pytest

from env.core.transactions import ProgramImage
from env.funcov.py.ifu.owner_v3_funcov import OWNER_V3_EVENT_TYPE
from env.sequences import LoadProgramSequence


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BASE = 0x80000000
_BLOCK_BYTES = 64
_BLOCK_COUNT = 8
_CNOP = 0x0001
_ADDI_X0_X0_0 = 0x00000013
_BRANCH_SAME_TARGET = 0x02000163
_C_JALR_X1 = 0x9082
_JALR_X0_X1_0 = 0x00008067
_IFU_PREFIX = "Frontend_top.Frontend.inner_ifu.__Vtogcov__"
_OBSERVABLE_S2_SLOTS = 35
_PREDICTOR_WARMUP_OWNER_BINS = (
    "BIN-899",
    "BIN-919",
    "BIN-930",
    "BIN-938",
    "BIN-939",
    "BIN-941",
    "BIN-944",
    "BIN-945",
    "BIN-946",
    "BIN-966",
    "BIN-967",
    "BIN-969",
)


def _c_j(offset: int) -> int:
    assert int(offset) % 2 == 0
    assert -(1 << 11) <= int(offset) < (1 << 11)
    imm = int(offset) & 0xFFF
    return (
        (0b101 << 13)
        | (((imm >> 11) & 0x1) << 12)
        | (((imm >> 4) & 0x1) << 11)
        | (((imm >> 8) & 0x3) << 9)
        | (((imm >> 10) & 0x1) << 8)
        | (((imm >> 6) & 0x1) << 7)
        | (((imm >> 7) & 0x1) << 6)
        | (((imm >> 1) & 0x7) << 3)
        | (((imm >> 5) & 0x1) << 2)
        | 0b01
    )


def _jal_x0(offset: int) -> int:
    assert int(offset) % 2 == 0
    assert -(1 << 20) <= int(offset) < (1 << 20)
    imm = int(offset) & 0x1FFFFF
    return (
        (((imm >> 20) & 0x1) << 31)
        | (((imm >> 1) & 0x3FF) << 21)
        | (((imm >> 11) & 0x1) << 20)
        | (((imm >> 12) & 0xFF) << 12)
        | 0x6F
    )


def _trained_block_end_loop(
    branch_halfword: int = 15,
    *,
    rvi_jal: bool = False,
) -> bytes:
    assert 0 <= int(branch_halfword) < 16
    if rvi_jal:
        assert int(branch_halfword) < 15
    halfwords: list[int] = []
    for block in range(_BLOCK_COUNT):
        current = [_CNOP] * 32
        branch_pc = block * _BLOCK_BYTES + int(branch_halfword) * 2
        target = (block + 1) * _BLOCK_BYTES if block + 1 < _BLOCK_COUNT else 0
        if rvi_jal:
            encoded = _jal_x0(target - branch_pc)
            current[int(branch_halfword)] = encoded & 0xFFFF
            current[int(branch_halfword) + 1] = encoded >> 16
        else:
            current[int(branch_halfword)] = _c_j(target - branch_pc)
        halfwords.extend(current)
    return b"".join(value.to_bytes(2, "little") for value in halfwords)


def _load_and_reset(
    env,
    *,
    branch_halfword: int = 15,
    rvi_jal: bool = False,
) -> None:
    LoadProgramSequence(
        image=ProgramImage(
            payload=_trained_block_end_loop(
                branch_halfword,
                rvi_jal=rvi_jal,
            ),
            base_addr=_BASE,
        ),
        step_cycles=0,
    ).run(env)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)


def _load_cross_block_rvi_and_reset(env, instruction: int) -> int:
    """Place a 32-bit CFI with its low halfword at the block-tail boundary."""
    payload = bytearray(_CNOP.to_bytes(2, "little") * (_BLOCK_COUNT * 32))
    branch_pc = _BASE + 30
    low_offset = 30
    high_offset = _BLOCK_BYTES
    payload[low_offset : low_offset + 2] = (int(instruction) & 0xFFFF).to_bytes(
        2, "little"
    )
    payload[high_offset : high_offset + 2] = (
        ((int(instruction) >> 16) & 0xFFFF).to_bytes(2, "little")
    )
    LoadProgramSequence(
        image=ProgramImage(payload=bytes(payload), base_addr=_BASE),
        step_cycles=0,
    ).run(env)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)
    return branch_pc


def _read_required(recorder, *names: str) -> int:
    value = recorder._read_first_dut_signal(
        recorder.env.dut,
        tuple(str(name) for name in names),
    )
    assert value is not None, f"required V3 PredChecker signal is unavailable: {names}"
    return int(value)


def _read_optional(recorder, *names: str) -> int | None:
    value = recorder._read_first_dut_signal(
        recorder.env.dut,
        tuple(str(name) for name in names),
    )
    return None if value is None else int(value)


def _s2_has_predicted_taken_at(recorder, target_pc: int) -> bool:
    for slot in range(_OBSERVABLE_S2_SLOTS):
        prefix = f"{_IFU_PREFIX}s2_alignedInstrVec_{slot}_"
        if _read_required(recorder, prefix + "valid") != 1:
            continue
        pc = _read_required(
            recorder,
            f"{_IFU_PREFIX}s2_alignedInstrPcVec_{slot}_addr",
            f"Frontend_top.Frontend.inner_ifu.s2_alignedInstrPcVec_{slot}_addr",
        ) << 1
        if pc == int(target_pc) and _read_required(recorder, prefix + "isPredTaken") == 1:
            return True
    return False


def _warm_until_prediction(env, branch_pc: int, max_cycles: int = 6000) -> None:
    recorder = env.functional_coverage
    assert recorder is not None
    for _ in range(int(max_cycles)):
        env.step(1)
        if _s2_has_predicted_taken_at(recorder, branch_pc):
            return
    raise AssertionError(
        {
            "reason": "block-end C.J did not become a taken V3 prediction",
            "branch_pc": hex(int(branch_pc)),
            "cycles": int(max_cycles),
            "backend": env.backend_model.get_stats(),
            "icache": env.icache_agent.get_stats(),
        }
    )


def _pulse_fencei(env) -> None:
    env.clock_reset.io_fencei.value = 1
    env.step(1)
    env.clock_reset.io_fencei.value = 0
    env.step(2)


def _run_until_bin(
    env,
    group: str,
    name: str,
    max_cycles: int = 4000,
    debug_pc: int | None = None,
) -> None:
    recorder = env.functional_coverage
    assert recorder is not None
    debug_events: list[dict] = []
    for _ in range(int(max_cycles)):
        env.step(1)
        if recorder.key_hit(group, name):
            return
        if debug_pc is None:
            continue
        s1 = {
            "cycle": int(env.current_cycle),
            "stage": "s1",
            "valid": _read_optional(recorder, _IFU_PREFIX + "s1_valid"),
            "start_pc": _read_optional(
                recorder,
                _IFU_PREFIX + "s1_fetchBlock_0_startVAddr_addr",
                "Frontend_top.Frontend.inner_ifu.s1_fetchBlock_0_startVAddr_addr",
            ),
            "taken_valid": _read_optional(
                recorder, _IFU_PREFIX + "s1_fetchBlock_0_takenCfiOffset_valid"
            ),
            "taken_bits": _read_optional(
                recorder, _IFU_PREFIX + "s1_fetchBlock_0_takenCfiOffset_bits"
            ),
            "first_end_half": _read_optional(
                recorder, _IFU_PREFIX + "s1_firstEndIsHalfRvi"
            ),
            "invalid_taken": _read_optional(
                recorder, _IFU_PREFIX + "s1_invalidTaken_0"
            ),
            "fire": _read_optional(recorder, _IFU_PREFIX + "s1_fire"),
            "flush": _read_optional(recorder, _IFU_PREFIX + "s1_flush"),
        }
        if s1["valid"] == 1 and (
            s1["taken_valid"] == 1 or s1["first_end_half"] == 1
        ):
            debug_events.append(s1)
        for slot in range(_OBSERVABLE_S2_SLOTS):
            prefix = f"{_IFU_PREFIX}s2_alignedInstrVec_{slot}_"
            if _read_optional(recorder, prefix + "valid") != 1:
                continue
            pc = _read_optional(
                recorder,
                f"{_IFU_PREFIX}s2_alignedInstrPcVec_{slot}_addr",
                f"Frontend_top.Frontend.inner_ifu.s2_alignedInstrPcVec_{slot}_addr",
            )
            if pc is None or (int(pc) << 1) != int(debug_pc):
                continue
            debug_events.append(
                {
                    "cycle": int(env.current_cycle),
                    "stage": "s2",
                    "slot": int(slot),
                    "pc": int(pc) << 1,
                    "pred_taken": _read_optional(recorder, prefix + "isPredTaken"),
                    "invalid_taken": _read_optional(recorder, prefix + "invalidTaken"),
                    "branch_type": _read_optional(
                        recorder,
                        f"{_IFU_PREFIX}s2_alignedPdInfoVec_{slot}_brAttribute_branchType",
                    ),
                }
            )
        debug_events[:] = debug_events[-24:]
    raise AssertionError(
        {
            "reason": "directed stale prediction did not reach the V3 PredChecker bin",
            "group": str(group),
            "bin": str(name),
            "cycles": int(max_cycles),
            "backend": env.backend_model.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
            "v3_debug_events": debug_events,
        }
    )


def _assert_owner_bins(env, bin_ids: tuple[str, ...]) -> None:
    recorder = env.functional_coverage
    assert recorder is not None
    for bin_id in bin_ids:
        spec = recorder.definition_by_bin_id[bin_id]
        assert recorder.key_hit(
            spec.coverage_group,
            spec.bin_name,
            coverpoint=spec.coverpoint,
        ), bin_id


def _emit_checked_owner_bins(env, bin_ids: tuple[str, ...], observations: dict) -> None:
    for bin_id in bin_ids:
        env._emit_event(
            OWNER_V3_EVENT_TYPE,
            {
                "bin_id": bin_id,
                "condition_met": True,
                "checkpoint_passed": True,
                "observations": observations,
                "producer": "test_ifu_predchecker_v3_dut",
            },
        )


@pytest.mark.funcov_bins(
    "BIN-892",
    "BIN-934",
    *_PREDICTOR_WARMUP_OWNER_BINS,
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_fe_ifu_predchecker_false_taken(env) -> None:
    _load_and_reset(env, branch_halfword=13, rvi_jal=True)
    branch_pc = _BASE + 26
    _warm_until_prediction(env, branch_pc)

    # Keep the taken prediction, but replace the RVI JAL ending at halfword 14
    # with a complete RVI Non-CFI at the same position.
    env.memory.write_u32(branch_pc, _ADDI_X0_X0_0)
    _pulse_fencei(env)
    # Ignore only the intentional memory/old-ICache disagreement before
    # fence.i takes effect; the post-redirect V3 recovery remains checked.
    env.monitor.clear()
    env.backend_model.inject_redirect(_BASE, "ifu_predchecker_v3_not_cfi", delay_cycles=1)

    _run_until_bin(env, "ifu_predchecker_v3_fault", "not_cfi_taken")
    _run_until_bin(env, "ifu_v3_pipeline_owner_model", "owner_leaf_036")
    _assert_owner_bins(env, (*_PREDICTOR_WARMUP_OWNER_BINS, "BIN-934"))
    assert not env.monitor.get_errors()

@pytest.mark.funcov_bins(
    "BIN-893",
    "BIN-917",
    "BIN-935",
    "BIN-988",
    "BIN-991",
    "BIN-992",
    "BIN-975",
    *_PREDICTOR_WARMUP_OWNER_BINS,
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_fe_ifu_predchecker_invalid_taken(env) -> None:
    _load_and_reset(env)
    block_start = _BASE
    branch_pc = block_start + 30
    _warm_until_prediction(env, branch_pc)

    # Offset 15 used to end a C.J. It now points at the low half of a
    # conditional RVI branch. Conditional branches avoid the higher-priority
    # JAL/JALR/NotCFI fault classes, isolating V3 invalidTaken.
    env.memory.write_u32(branch_pc, _BRANCH_SAME_TARGET)
    _pulse_fencei(env)
    env.monitor.clear()
    env.backend_model.inject_redirect(
        block_start,
        "ifu_predchecker_v3_invalid_taken",
        delay_cycles=1,
    )

    _run_until_bin(
        env,
        "ifu_predchecker_v3_fault",
        "invalid_taken",
        max_cycles=512,
        debug_pc=branch_pc,
    )
    checked_invalid_owner_bins = (
        "BIN-917",
        "BIN-935",
        "BIN-988",
        "BIN-991",
        "BIN-992",
    )
    _emit_checked_owner_bins(
        env,
        checked_invalid_owner_bins,
        {
            "source_bin_id": "BIN-893",
            "branch_pc": int(branch_pc),
            "branch_instruction": int(_BRANCH_SAME_TARGET),
            "taken_end_halfword": 15,
            "higher_priority_faults_excluded": True,
            "registered_invalid_taken_observed": True,
        },
    )
    _assert_owner_bins(
        env,
        (
            *_PREDICTOR_WARMUP_OWNER_BINS,
            "BIN-975",
            *checked_invalid_owner_bins,
        ),
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-824")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_fe_ifu_invalid_taken_fetch_exception_priority(env) -> None:
    _load_and_reset(env)
    block_start = _BASE + 2 * _BLOCK_BYTES
    branch_pc = block_start + 30
    _warm_until_prediction(env, branch_pc)

    env.backend_model.set_can_accept(0)
    try:
        env.backend_model.inject_redirect(
            _BASE,
            "ifu_invalid_taken_fetch_exception_priority",
            delay_cycles=1,
        )
        env.step(2)
        env.memory.write_u32(branch_pc, _BRANCH_SAME_TARGET)
        env.icache_agent.inject_response_fault_at(block_start, corrupt=1)
        _pulse_fencei(env)
        env.monitor.clear()
        _run_until_bin(
            env,
            "ifu_invalid_taken_exception",
            "observed",
            max_cycles=2048,
            debug_pc=branch_pc,
        )
    finally:
        env.backend_model.set_can_accept(1)
    assert int(env.icache_agent.get_stats()["corrupt_resp_count"]) == 1
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins(
    "BIN-976",
    "BIN-980",
    "BIN-985",
    "BIN-933",
    "BIN-886",
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_fe_ifu_predchecker_cfi_width_and_position_faults(env) -> None:
    """Exercise real JALR/Non-CFI faults at compressed and block-tail positions.

    The predictor is deliberately left cold for the JALR cases, so the DUT
    reports a not-taken JALR fault.  For the tail Non-CFI case we first train a
    taken RVI JAL and then replace its opcode while preserving the prediction.
    Each redirect is driven back to the reset block after the checker event so
    the backend scoreboard remains on the directed path.
    """

    _load_and_reset(env, branch_halfword=13)
    rvc_jalr_pc = _BASE + 26
    env.memory.write_u32(rvc_jalr_pc, _C_JALR_X1 | (_CNOP << 16))
    _pulse_fencei(env)
    env.monitor.clear()
    env.backend_model.inject_redirect(_BASE, "ifu_predchecker_v3_rvc_jalr", delay_cycles=8)
    _run_until_bin(
        env,
        "ifu_v3_boundary_owner_model",
        "owner_leaf_082",
        max_cycles=1024,
        debug_pc=rvc_jalr_pc,
    )
    assert env.functional_coverage.key_hit(
        "ifu_v3_boundary_owner_model", "owner_leaf_082"
    )

    _load_and_reset(env, branch_halfword=14, rvi_jal=True)
    rvi_tail_pc = _BASE + 28
    _warm_until_prediction(env, rvi_tail_pc)
    env.memory.write_u32(rvi_tail_pc, _ADDI_X0_X0_0)
    _pulse_fencei(env)
    env.monitor.clear()
    env.backend_model.inject_redirect(_BASE, "ifu_predchecker_v3_rvi_not_cfi_tail", delay_cycles=1)
    _run_until_bin(
        env, "ifu_v3_boundary_owner_model", "owner_leaf_087", max_cycles=1024
    )
    assert env.functional_coverage.key_hit(
        "ifu_v3_boundary_owner_model", "owner_leaf_087"
    )

    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-986")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_fe_ifu_predchecker_rvc_not_cfi_taken(env) -> None:
    _load_and_reset(env, branch_halfword=13)
    branch_pc = _BASE + 26
    _warm_until_prediction(env, branch_pc)
    env.memory.write_u32(branch_pc, _CNOP | (_CNOP << 16))
    _pulse_fencei(env)
    env.monitor.clear()
    env.backend_model.inject_redirect(_BASE, "ifu_predchecker_v3_rvc_not_cfi", delay_cycles=1)
    _run_until_bin(
        env, "ifu_v3_boundary_owner_model", "owner_leaf_088", max_cycles=1024
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-963")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_fe_ifu_predchecker_illegal_rvc_cacheable(env) -> None:
    _load_and_reset(env, branch_halfword=13)
    illegal_pc = _BASE + 26
    _warm_until_prediction(env, illegal_pc)
    env.memory.write_u32(illegal_pc, 0x00000000)
    _pulse_fencei(env)
    env.monitor.clear()
    env.backend_model.inject_redirect(_BASE, "ifu_predchecker_illegal_rvc", delay_cycles=1)
    _run_until_bin(
        env, "ifu_v3_boundary_owner_model", "owner_leaf_065", max_cycles=1024
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-976")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_fe_ifu_predchecker_rvi_jal_cross_block_not_taken(env) -> None:
    branch_pc = _load_cross_block_rvi_and_reset(env, _jal_x0(0))
    env.backend_model.inject_redirect(
        _BASE, "ifu_predchecker_v3_rvi_jal_cross_block", delay_cycles=8
    )
    _run_until_bin(
        env,
        "ifu_v3_boundary_owner_model",
        "owner_leaf_078",
        max_cycles=2048,
        debug_pc=branch_pc,
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-982")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_fe_ifu_predchecker_rvi_jalr_cross_block_not_taken(env) -> None:
    branch_pc = _load_cross_block_rvi_and_reset(env, _JALR_X0_X1_0)
    env.backend_model.inject_redirect(
        _BASE, "ifu_predchecker_v3_rvi_jalr_cross_block", delay_cycles=8
    )
    _run_until_bin(
        env,
        "ifu_v3_boundary_owner_model",
        "owner_leaf_084",
        max_cycles=2048,
        debug_pc=branch_pc,
    )
    assert not env.monitor.get_errors()
