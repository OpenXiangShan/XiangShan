"""Shared PredChecker stimulus for directed Frontend tests."""

from __future__ import annotations

from env.core.transactions import ProgramImage
from env.sequences import LoadProgramSequence
from tests.py.support.instructions import encode_c_j as _c_j, encode_jal_x0 as _jal_x0


_BASE = 0x80000000
_BLOCK_BYTES = 64
_BLOCK_COUNT = 8
_CNOP = 0x0001
_ADDI_X0_X0_0 = 0x00000013
_BRANCH_SAME_TARGET = 0x02000163
_IFU_PREFIX = "Frontend_top.Frontend.inner_ifu.__Vtogcov__"
_OBSERVABLE_S2_SLOTS = 35


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


def _read_optional(recorder, *names: str) -> int | None:
    value = recorder._read_first_dut_signal(
        recorder.env.dut,
        tuple(str(name) for name in names),
    )
    return None if value is None else int(value)


def _read_direct(env, *names: str) -> int | None:
    for name in names:
        try:
            signal = getattr(env.dut, str(name), None)
            if signal is None:
                getter = getattr(env.dut, "GetInternalSignal", None)
                signal = getter(str(name)) if callable(getter) else None
            value = None if signal is None else getattr(signal, "value", None)
            if value is not None:
                return int(value)
        except Exception:
            continue
    return None


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
    debug_events: list[dict] = []
    for _ in range(int(max_cycles)):
        env.step(1)
        if recorder is not None and recorder.key_hit(group, name):
            return
        if recorder is None and group == "ifu_predchecker_v3_fault":
            redirect_valid = _read_direct(
                env,
                "Frontend_top.Frontend.inner_ifu.predChecker.__Vtogcov__io_resp_stage2Out_checkerRedirect_valid",
                "Frontend_top.Frontend.inner_ifu.predChecker.io_resp_stage2Out_checkerRedirect_valid",
            )
            invalid_taken = _read_direct(
                env,
                "Frontend_top.Frontend.inner_ifu.predChecker.__Vtogcov__io_resp_stage2Out_checkerRedirect_bits_invalidTaken",
                "Frontend_top.Frontend.inner_ifu.predChecker.io_resp_stage2Out_checkerRedirect_bits_invalidTaken",
            )
            not_cfi_taken = _read_direct(
                env,
                "Frontend_top.Frontend.inner_ifu.predChecker.__Vtogcov__io_resp_stage2Out_checkerRedirect_bits_notCfiTaken",
                "Frontend_top.Frontend.inner_ifu.predChecker.io_resp_stage2Out_checkerRedirect_bits_notCfiTaken",
            )
            if name == "invalid_taken" and redirect_valid == 1 and invalid_taken == 1:
                return
            if name == "not_cfi_taken" and redirect_valid == 1 and not_cfi_taken == 1:
                return
        if debug_pc is None or recorder is None:
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
                    "is_rvc": _read_optional(recorder, prefix + "isRvc"),
                    "block_sel": _read_optional(recorder, prefix + "blockSel"),
                    "is_cross_block_instr": _read_optional(
                        recorder, prefix + "isCrossBlockInstr"
                    ),
                    "end_offset": _read_optional(recorder, prefix + "endOffset"),
                    "branch_type": _read_optional(
                        recorder,
                        f"{_IFU_PREFIX}s2_alignedPdInfoVec_{slot}_brAttribute_branchType",
                    ),
                    "fetch_blocks": [
                        {
                            "valid": _read_optional(
                                recorder, f"{_IFU_PREFIX}s2_fetchBlock_{block}_valid"
                            ),
                            "start_pc": (
                                lambda value: None if value is None else int(value) << 1
                            )(
                                _read_optional(
                                    recorder,
                                    f"{_IFU_PREFIX}s2_fetchBlock_{block}_startVAddr_addr",
                                )
                            ),
                            "taken_valid": _read_optional(
                                recorder,
                                f"{_IFU_PREFIX}s2_fetchBlock_{block}_takenCfiOffset_valid",
                            ),
                            "taken_bits": _read_optional(
                                recorder,
                                f"{_IFU_PREFIX}s2_fetchBlock_{block}_takenCfiOffset_bits",
                            ),
                        }
                        for block in range(2)
                    ],
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
