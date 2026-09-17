"""Real-DUT first-block false-taken range clipping."""

from __future__ import annotations

import os

import pytest

from env.funcov.py.ifu.compact_funcov import (
    _read_ifu_internal_with_path,
    _read_predchecker_with_path,
)
from env.support.pc_utils import fold_pc
from tests.py.jiabowen.test_ifu_predchecker_v3_dut import (
    _ADDI_X0_X0_0,
    _BASE,
    _BLOCK_BYTES,
    _CNOP,
    _load_and_reset,
    _replace_u32s_after_redirect_flush,
    _warm_until_first_taken_two_fetch_window,
)


def _assert_clip_masks(slots, fault_slot, *, fixed, enq, valid):
    """Independent first-fault prefix check, including raw/effective ownership."""
    first = [s for s in slots if s["slot"] <= fault_slot]
    second = [s for s in slots if s["owner"] == 1]
    assert first and second, "both preclip owners must exist"
    assert all((s["owner"] | s["cross"]) == 0 for s in first), first
    assert all(s["slot"] > fault_slot for s in second), second
    expected = sum(1 << s["slot"] for s in first)
    assert fixed == enq == expected, (fixed, enq, expected)
    assert valid & expected == expected, (valid, expected)
    return first


def _assert_recovery_prefix(actual, expected):
    """Do not filter out unexpected/speculative entries from the checked prefix."""
    assert len(actual) >= len(expected), (len(actual), len(expected))
    assert actual[:len(expected)] == expected, (actual[:len(expected)], expected)


def _accepted_cfvec_slots(decode_can_accept, valids):
    assert decode_can_accept in (0, 1)
    assert len(valids) == 8
    assert all(v in (0, 1) for v in valids)
    return [i for i in range(8) if decode_can_accept and valids[i]]


@pytest.mark.funcov_bins("BIN-874")
@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires real Frontend DUT")
def test_first_block_false_taken_clips_second_fetch_current_dut(env):
    _exercise_first_block_false_taken_clip(env)


def _exercise_first_block_false_taken_clip(env):
    """A stale first-block prediction clips a real, pre-existing second block."""

    _load_and_reset(env, branch_halfword=13, rvi_jal=True)
    first_start, second_start = _BASE + 6 * _BLOCK_BYTES, _BASE + 7 * _BLOCK_BYTES
    for _ in range(64):
        if _warm_until_first_taken_two_fetch_window(env) == (first_start, second_start):
            break
    else:
        pytest.fail("the final trained pair never reached a dual-fetch window")
    fault_pc = int(first_start) + 26
    # Preserve earlier trained control flow so cold post-fence requests can
    # establish runahead before the altered first owner reaches the IFU.
    replacements = ((fault_pc, _ADDI_X0_X0_0),)
    recorder = env.functional_coverage
    assert recorder is not None
    paths: dict[str, str] = {}
    candidates: list[dict] = []
    windows: list[dict] = []
    checkpoints: list[dict] = []
    pending = None
    held = None
    monitor_start = None
    accepted_slots = []
    cfvec_transfers = []
    transfer_cycles = []

    def read(stem: str, *, pred=False) -> int:
        reader = _read_predchecker_with_path if pred else _read_ifu_internal_with_path
        value, path = reader(recorder, env.dut, stem)
        assert value is not None, {"missing_probe": stem}
        paths[("pred." if pred else "ifu.") + stem] = path
        return int(value)

    def canary(cycle, expected):
        observed = {}
        for stem, value in expected.items():
            readings = {}
            for prefix in ("Frontend_top.Frontend.inner_ifu.",
                           "Frontend_top.Frontend.inner_ifu.__Vtogcov__",
                           "Frontend_top.Frontend._inner_ifu_"):
                path = prefix + stem
                actual = recorder._try_read_dut_signal(env.dut, path)
                if actual is not None:
                    readings[path] = int(actual)
            assert readings and all(v == value for v in readings.values()), (stem, readings, value)
            observed[stem] = readings
        env._emit_event("ifu.first_owner_clip_signal_canary", dict(
            cycle=cycle, phase="cycle_observer", readings=observed))

    def observe_acceptance(cycle, _env):
        nonlocal accepted_slots
        def port(name):
            signal = getattr(env.dut, name, None)
            assert signal is not None, {"missing_port": name}
            paths[name] = name
            return int(signal.value)
        # The generic monitor records valid presentations, including stalls.
        # Use physical pre-drive handshake values, not the next-cycle ready
        # that BackendAgent writes before ordinary observers run.
        accept = port("io_backend_toIBuf_decodeCanAccept")
        valids = [port(f"io_backend_cfVec_{i}_valid") for i in range(8)]
        # Current IBuffer consumes outputEntries under decodeCanAccept; the
        # unused Decoupled ready ports are eliminated, not defaulted to one.
        assert port("io_backend_toIBuf_resumingVType") == 0
        accepted_slots = _accepted_cfvec_slots(accept, valids)

    def observe(cycle, _env):
        nonlocal pending, held, monitor_start
        if monitor_start is not None and accepted_slots:
            presented = {o.slot: o for o in reversed(env.monitor.observations)
                         if o.cycle == cycle}
            assert all(slot in presented for slot in accepted_slots), (cycle, accepted_slots)
            cfvec_transfers.extend((presented[slot].pc, presented[slot].instr)
                                   for slot in accepted_slots)
            transfer_cycles.append(dict(cycle=cycle, slots=list(accepted_slots)))
        if pending is not None:
            assert cycle == pending["cycle"] + 1
            prefix = "io_resp_stage2Out_checkerRedirect_"
            redirect = {key: read(prefix + stem, pred=True) for key, stem in {
                "valid": "valid", "owner": "bits_blockSel", "cross": "bits_isCrossBlockInstr",
                "target": "bits_target_addr", "offset": "bits_endOffset",
                "invalid": "bits_invalidTaken", "not_cfi": "bits_notCfiTaken",
                "taken": "bits_taken", "branch": "bits_attribute_branchType",
            }.items()}
            assert redirect == dict(valid=1, owner=0, cross=0, target=(fault_pc + 4) >> 1,
                                    offset=14, invalid=0, not_cfi=1, taken=0, branch=0), redirect
            expected = {
                "io_toFtq_wbRedirect_valid": 1,
                "io_toFtq_wbRedirect_bits_ftqIdx_flag": pending["blocks"][0]["flag"],
                "io_toFtq_wbRedirect_bits_ftqIdx_value": pending["blocks"][0]["index"],
                "io_toFtq_wbRedirect_bits_pc": first_start,
                "io_toFtq_wbRedirect_bits_target": fault_pc + 4,
                "io_toFtq_wbRedirect_bits_ftqOffset": 14,
                "io_toFtq_wbRedirect_bits_isRVC": 0,
                "io_toFtq_wbRedirect_bits_taken": 0,
                "io_toFtq_wbRedirect_bits_canTrain": 1,
            }
            actual = {stem: read(stem) for stem in expected}
            assert actual == expected, (pending, actual)
            canary(cycle, actual)
            checkpoints.append(dict(request=pending, cycle=cycle, redirect=redirect,
                                    writeback=actual, signal_paths=dict(paths)))
            pending = None
        if candidates:
            return
        if read("s2_valid_valid") != 1 or read("s2_flush") != 0:
            return
        blocks = [
            {
                key: read(f"s2_fetchBlock_{block}_{stem}")
                for key, stem in (("valid", "valid"), ("pc", "startVAddr_addr"),
                                  ("flag", "ftqIdx_flag"), ("index", "ftqIdx_value"))
            }
            for block in range(2)
        ]
        slots = []
        for slot in range(35):
            prefix = f"s2_alignedInstrVec_{slot}_"
            if read(prefix + "valid") != 1:
                continue
            slots.append(
                {
                    "slot": slot,
                    "pc": read(f"s2_alignedInstrPcVec_{slot}_addr") << 1,
                    "data": read(prefix + "data"),
                    "taken": read(prefix + "isPredTaken"),
                    "owner": read(prefix + "blockSel"),
                    "cross": read(prefix + "isCrossBlockInstr"),
                    "rvc": read(prefix + "isRvc"),
                    "offset": read(prefix + "endOffset"),
                    "invalid": read(prefix + "invalidTaken"),
                }
            )
        if len(windows) < 96:
            windows.append(dict(cycle=int(cycle), blocks=blocks,
                                fire=read("s2_fire"), slots=slots,
                                fixed=read("s2_fixedInstrValid"),
                                enq=read("io_toIBuffer_bits_enqEnable")))
        if any(block["valid"] != 1 for block in blocks):
            return
        fault = next(
            (
                item
                for item in slots
                if item["pc"] == fault_pc
                and item["data"] == _ADDI_X0_X0_0
                and item["taken"] == 1
                and item["owner"] == 0
                and item["cross"] == 0
            ),
            None,
        )
        preclip_second = [item for item in slots if item["owner"] == 1]
        if fault is None or not preclip_second:
            return
        fixed = read("s2_fixedInstrValid")
        enq = read("io_toIBuffer_bits_enqEnable")
        assert [b["pc"] << 1 for b in blocks] == [first_start, second_start]
        assert (blocks[0]["flag"], blocks[0]["index"]) != (blocks[1]["flag"], blocks[1]["index"])
        assert read("s2_reqIsUncache") == 0 and read("io_toIBuffer_valid") == 1
        assert read("io_toIBuffer_bits_exceptionType_value") == 0
        assert fault["invalid"] == 0 and fault["offset"] == 14 and fault["rvc"] == 0
        first = _assert_clip_masks(slots, fault["slot"], fixed=fixed, enq=enq,
                                  valid=read("io_toIBuffer_bits_valid"))
        expected_signals = {"s2_fixedInstrValid": fixed, "io_toIBuffer_bits_enqEnable": enq}
        for e in slots:
            slot = e["slot"]
            raw = env.memory.read_u32(e["pc"])
            is_rvc = int(raw & 3 != 3)
            assert e["rvc"] == is_rvc
            assert (e["data"] & (0xFFFF if is_rvc else 0xFFFFFFFF)) == (raw & (0xFFFF if is_rvc else 0xFFFFFFFF))
            if is_rvc:
                assert raw & 0xFFFF == _CNOP
            owner = e["owner"] | e["cross"]
            assert owner in (0, 1) and e["cross"] == 0
            assert e["offset"] == ((e["pc"] + (2 if is_rvc else 4) - 2 - (blocks[owner]["pc"] << 1)) // 2)
            branch = 2 if raw & 0x7F == 0x6F and not is_rvc else 0
            assert read(f"s2_alignedPdInfoVec_{slot}_isRVC") == is_rvc
            assert read(f"s2_alignedPdInfoVec_{slot}_brAttribute_branchType") == branch
            assert read(f"s2_alignedPdInfoVec_{slot}_brAttribute_rasAction") == 0
            expected_signals.update({
                f"s2_alignedInstrVec_{slot}_valid": 1,
                f"s2_alignedInstrVec_{slot}_blockSel": e["owner"],
                f"s2_alignedInstrVec_{slot}_isCrossBlockInstr": e["cross"],
                f"s2_alignedInstrPcVec_{slot}_addr": e["pc"] >> 1,
            })
        for e in first:
            slot = e["slot"]
            expected_output = {
                f"io_toIBuffer_bits_instrs_{slot}": _ADDI_X0_X0_0,
                f"io_toIBuffer_bits_foldpc_{slot}": fold_pc(e["pc"]),
                f"io_toIBuffer_bits_isRvc_{slot}": e["rvc"],
                f"io_toIBuffer_bits_ftqPtr_{slot}_flag": blocks[0]["flag"],
                f"io_toIBuffer_bits_ftqPtr_{slot}_value": blocks[0]["index"],
                f"io_toIBuffer_bits_instrEndOffset_{slot}_offset": e["offset"],
            }
            assert {stem: read(stem) for stem in expected_output} == expected_output
            expected_signals.update(expected_output)
        signature = dict(blocks=blocks, slots=slots, fixed=fixed, enq=enq)
        if held is None:
            held = signature
        else:
            assert signature == held, "held target payload changed before acceptance"
        if not checkpoints and pending is None:
            pending = dict(cycle=cycle, blocks=blocks, fault=fault)
        if read("s2_fire") != 1:
            return
        assert read("io_toIBuffer_ready") == 1
        canary(cycle, expected_signals)
        candidates.append(
            {
                "cycle": int(cycle),
                "fault": fault,
                "preclip_second_slots": [item["slot"] for item in preclip_second],
                "fixed": int(fixed),
                "enq": int(enq),
                "blocks": blocks,
                "signal_paths": dict(paths),
            }
        )

    env.register_pre_drive_cycle_observer(observe_acceptance)
    env.register_cycle_observer(observe)
    env.icache_agent.configure(hit_latency=24, miss_latency=24, miss_rate=0.0, seed=0x874)
    _replace_u32s_after_redirect_flush(
        env,
        replacements,
        redirect_target=_BASE,
        reason="ifu_predchecker_v3_first_block_clip_second_fetch_current",
    )
    # Backend observation trails IFU enqueue. Start at the completed physical
    # redirect, before any cold refill returns, not at target S2 visibility:
    # older legal blocks can still be queued in IBuffer at that later edge.
    monitor_start = len(env.monitor.observations)
    env.backend_model.set_can_accept(0)
    env.step(96)
    env.backend_model.set_can_accept(1)
    for _ in range(1024):
        env.step(1)
        if candidates and checkpoints:
            break
    assert candidates and checkpoints and monitor_start is not None, {
        "candidates": candidates, "checkpoints": checkpoints, "windows": windows}
    # The rejected second block may be fetched later, but cannot jump ahead
    # of the sequential fallthrough after the corrected Non-CFI.
    expected = []
    for block in range(6):
        start = _BASE + block * _BLOCK_BYTES
        expected += [(pc, _ADDI_X0_X0_0) for pc in range(start, start + 26, 2)]
        expected.append((start + 26, env.memory.read_u32(start + 26)))
    expected_pcs = list(range(first_start, fault_pc, 2)) + [fault_pc]
    expected_pcs += list(range(fault_pc + 4, second_start + 26, 2))
    expected += [(pc, _ADDI_X0_X0_0) for pc in expected_pcs]
    expected += [(second_start + 26, env.memory.read_u32(second_start + 26)),
                 (_BASE, _ADDI_X0_X0_0)]
    for _ in range(512):
        if len(cfvec_transfers) >= len(expected):
            break
        env.step(1)
    actual = cfvec_transfers
    _assert_recovery_prefix(actual, expected)
    recorder.risk_observations.append(
        {
            "event": "bin874_first_owner_clip_checkpoint",
            "warm_starts": [int(first_start), int(second_start)],
            "candidates": candidates,
            "windows": windows,
            "writeback_checkpoints": checkpoints,
            "checked_recovery_prefix": expected,
            "accepted_cfvec_cycles": transfer_cycles,
            "signal_paths": dict(paths),
        }
    )
    assert recorder.key_hit("ifu_data_slice", "second_block_suppressed"), {
        "warm_starts": [hex(int(first_start)), hex(int(second_start))],
        "candidates": candidates,
        "windows": windows,
        "backend": env.backend_model.get_stats(),
    }
    assert candidates
    assert not env.monitor.get_errors()
