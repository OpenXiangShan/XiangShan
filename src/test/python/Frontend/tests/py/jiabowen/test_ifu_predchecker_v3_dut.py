from __future__ import annotations

import os

import pytest

from env.core.transactions import ProgramImage
from env.sequences import LoadProgramSequence


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BASE = 0x80000000
_BLOCK_BYTES = 64
_BLOCK_COUNT = 8
_CNOP = 0x0001
_BRANCH_SAME_TARGET = 0x02000163
_IFU_PREFIX = "Frontend_top.Frontend.inner_ifu.__Vtogcov__"
_OBSERVABLE_S2_SLOTS = 35


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


def _trained_block_end_loop() -> bytes:
    halfwords: list[int] = []
    for block in range(_BLOCK_COUNT):
        halfwords.extend([_CNOP] * 15)
        branch_pc = block * _BLOCK_BYTES + 30
        target = (block + 1) * _BLOCK_BYTES if block + 1 < _BLOCK_COUNT else 0
        halfwords.append(_c_j(target - branch_pc))
        halfwords.extend([_CNOP] * 16)
    return b"".join(value.to_bytes(2, "little") for value in halfwords)


def _load_and_reset(env) -> None:
    LoadProgramSequence(
        image=ProgramImage(payload=_trained_block_end_loop(), base_addr=_BASE),
        step_cycles=0,
    ).run(env)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)


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


@pytest.mark.funcov_bins("BIN-892")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_fe_ifu_predchecker_false_taken(env) -> None:
    _load_and_reset(env)
    branch_pc = _BASE + 30
    _warm_until_prediction(env, branch_pc)

    # Keep the trained taken prediction, but make its predicted CFI a C.NOP.
    env.memory.write_u16(branch_pc, _CNOP)
    _pulse_fencei(env)
    # Ignore only the intentional memory/old-ICache disagreement before
    # fence.i takes effect; the post-redirect V3 recovery remains checked.
    env.monitor.clear()
    env.backend_model.inject_redirect(_BASE, "ifu_predchecker_v3_not_cfi", delay_cycles=1)

    _run_until_bin(env, "ifu_predchecker_v3_fault", "not_cfi_taken")
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-893")
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
    assert not env.monitor.get_errors()
