"""Second-fetch ownership with a complete, stale-predicted Non-CFI instruction."""

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
    _replace_u32_after_redirect_flush,
    _warm_until_first_taken_two_fetch_window,
)


@pytest.mark.funcov_bins("BIN-947")
@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires real Frontend DUT")
def test_second_block_non_cfi_redirect_writes_second_ftq_entry(env):
    _load_and_reset(env, branch_halfword=13, rvi_jal=True)
    first_start, second_start = _BASE + 6 * _BLOCK_BYTES, _BASE + 7 * _BLOCK_BYTES
    for _ in range(64):
        if _warm_until_first_taken_two_fetch_window(env) == (first_start, second_start):
            break
    else:
        pytest.fail("the final trained pair never reached a dual-fetch window")
    fault_pc = second_start + 26
    target_pc = fault_pc + 4
    recorder = env.functional_coverage
    paths, windows, checkpoints, fired = {}, [], [], []
    pending = None

    def read(stem, *, pred=False):
        reader = _read_predchecker_with_path if pred else _read_ifu_internal_with_path
        value, path = reader(recorder, env.dut, stem)
        assert value is not None, {"missing_probe": stem, "predchecker": pred}
        paths[("pred." if pred else "ifu.") + stem] = path
        return int(value)

    def observe(cycle, _env):
        nonlocal pending
        if pending is not None:
            assert cycle == pending["cycle"] + 1
            prefix = "io_resp_stage2Out_checkerRedirect_"
            redirect = {key: read(prefix + stem, pred=True) for key, stem in {
                "valid": "valid", "raw_owner": "bits_blockSel",
                "cross": "bits_isCrossBlockInstr", "target_addr": "bits_target_addr",
                "offset": "bits_endOffset", "invalid": "bits_invalidTaken",
                "branch_type": "bits_attribute_branchType",
            }.items()}
            assert redirect == dict(valid=1, raw_owner=1, cross=0,
                                    target_addr=target_pc >> 1, offset=14,
                                    invalid=0, branch_type=0), (pending, redirect)
            wb = {key: read("io_toFtq_wbRedirect_" + stem) for key, stem in {
                "valid": "valid", "flag": "bits_ftqIdx_flag", "index": "bits_ftqIdx_value",
                "pc": "bits_pc", "target": "bits_target", "offset": "bits_ftqOffset",
                "is_rvc": "bits_isRVC", "can_train": "bits_canTrain",
            }.items()}
            recorder.risk_observations.append(dict(event="bin947_wb_diagnostic",
                request=pending, checker=redirect, wb=wb, signal_paths=dict(paths)))
            assert wb == dict(valid=1, flag=pending["blocks"][1]["flag"],
                              index=pending["blocks"][1]["index"], pc=second_start,
                              target=target_pc, offset=14, is_rvc=0, can_train=1), (pending, wb)
            checkpoints.append(dict(request=pending, checker=redirect, wb=wb,
                                    signal_paths=dict(paths)))
            env._emit_event("ifu.second_block_redirect_checkpoint", checkpoints[-1])
            pending = None
        if (checkpoints and fired) or read("s2_valid_valid") != 1 or read("s2_flush") != 0:
            return
        blocks = [{key: read(f"s2_fetchBlock_{b}_" + stem) for key, stem in {
            "valid": "valid", "pc_addr": "startVAddr_addr", "flag": "ftqIdx_flag",
            "index": "ftqIdx_value", "taken": "takenCfiOffset_valid",
            "offset": "takenCfiOffset_bits",
        }.items()} for b in range(2)]
        entries = []
        for slot in range(35):
            prefix = f"s2_alignedInstrVec_{slot}_"
            if read(prefix + "valid") != 1:
                continue
            entries.append(dict(slot=slot, pc=read(f"s2_alignedInstrPcVec_{slot}_addr") << 1,
                data=read(prefix + "data"), taken=read(prefix + "isPredTaken"),
                owner=read(prefix + "blockSel"), cross=read(prefix + "isCrossBlockInstr"),
                invalid=read(prefix + "invalidTaken"), offset=read(prefix + "endOffset")))
        if len(windows) < 64:
            windows.append(dict(cycle=cycle, blocks=blocks, entries=entries))
        fault = next((e for e in entries if e["pc"] == fault_pc
                      and e["data"] == _ADDI_X0_X0_0 and e["taken"] == 1
                      and e["owner"] == 1 and e["cross"] == 0), None)
        if fault is None or any(b["valid"] != 1 for b in blocks):
            return
        assert [b["pc_addr"] << 1 for b in blocks] == [first_start, second_start]
        assert fault["invalid"] == 0 and fault["offset"] == 14
        assert (blocks[0]["flag"], blocks[0]["index"]) != (blocks[1]["flag"], blocks[1]["index"])
        assert read("io_toIBuffer_valid") == 1
        enq, fixed = read("io_toIBuffer_bits_enqEnable"), read("s2_fixedInstrValid")
        expected_mask = sum(1 << e["slot"] for e in entries if e["slot"] <= fault["slot"])
        assert enq == fixed == expected_mask
        for e in entries:
            if not ((enq >> e["slot"]) & 1):
                continue
            slot, block = e["slot"], e["owner"] | e["cross"]
            expected_instr = env.memory.read_u32(e["pc"])
            if expected_instr & 3 != 3:
                assert expected_instr & 0xFFFF == _CNOP
                expected_instr = _ADDI_X0_X0_0
            assert read(f"io_toIBuffer_bits_instrs_{slot}") == expected_instr
            assert read(f"io_toIBuffer_bits_foldpc_{slot}") == fold_pc(e["pc"])
            assert read(f"io_toIBuffer_bits_ftqPtr_{slot}_flag") == blocks[block]["flag"]
            assert read(f"io_toIBuffer_bits_ftqPtr_{slot}_value") == blocks[block]["index"]
            assert read(f"io_toIBuffer_bits_instrEndOffset_{slot}_offset") == e["offset"]
        request = dict(cycle=cycle, blocks=blocks, fault=fault, enq=enq,
                       monitor_start=len(env.monitor.observations))
        # Writeback is enabled by the preceding S1 transfer, independently of
        # S2 output backpressure.  It must not be delayed to s2_fire + 1.
        if not checkpoints and pending is None:
            pending = request
        if read("s2_fire") == 1:
            assert read("io_toIBuffer_ready") == 1
            fired.append(request)

    env.register_cycle_observer(observe)
    # Refill latency permits prediction/WayLookup runahead after fence.i; the
    # first cold requests can be single-fetch without consuming the target.
    env.icache_agent.configure(hit_latency=24, miss_latency=24, miss_rate=0.0, seed=0x947)
    _replace_u32_after_redirect_flush(env, fault_pc, _ADDI_X0_X0_0,
        redirect_target=_BASE, reason="second_block_stale_non_cfi")
    env.backend_model.set_can_accept(0)
    env.step(96)
    env.backend_model.set_can_accept(1)
    for _ in range(1024):
        env.step(1)
        if checkpoints and fired:
            break
    recorder.risk_observations.append({
        "event": "bin947_second_block_stale_prediction_diagnostic",
        "warm_starts": [first_start, second_start], "windows": windows,
        "signal_paths": dict(paths), "checkpoint_count": len(checkpoints),
        "backend": env.backend_model.get_stats(),
    })
    env.logger.info("BIN-947 windows: %s", [
        dict(cycle=w["cycle"], blocks=w["blocks"],
             selected=[e for e in w["entries"] if e["taken"] or e["owner"] or e["pc"] == fault_pc])
        for w in windows
    ])
    assert checkpoints and fired, {"reason": "no complete second-block redirect/delivery witness",
                         "warm_starts": [first_start, second_start], "windows": windows,
                         "backend": env.backend_model.get_stats()}
    start = fired[0]["monitor_start"]
    for _ in range(256):
        delivered = env.monitor.observations[start:]
        if any(item.pc == fault_pc for item in delivered) and any(item.pc == target_pc for item in delivered):
            break
        env.step(1)
    delivered = env.monitor.observations[start:]
    assert any(item.pc == fault_pc and item.instr == _ADDI_X0_X0_0 for item in delivered)
    assert any(item.pc == target_pc for item in delivered)
    spec = recorder.definition_by_bin_id["BIN-947"]
    assert recorder.key_hit(spec.coverage_group, spec.bin_name, coverpoint=spec.coverpoint)
    assert not env.monitor.get_errors()
