"""Live half-RVI state across source-bound backend recovery."""

from __future__ import annotations

import os

import pytest

from env.core.transactions import ProgramImage
from env.funcov.py.ifu.compact_funcov import _read_ifu_internal_with_path
from env.model.golden_trace import GoldenTrace, TraceEntry
from env.sequences import LoadProgramSequence
from tests.py.jiabowen.test_ifu_predchecker_v3_dut import _BASE, _CNOP


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires real Frontend DUT")
def test_backend_redirect_isolates_live_half_rvi_before_recovery(env):
    source_pc, target_pc = _BASE + 4, _BASE + 0x400
    auipc, jalr, split_addi = 0x00000317, 0x40030067, 0x01300313
    program = bytearray(_CNOP.to_bytes(2, "little") * 2048)
    for offset, instruction in ((0, auipc), (4, jalr),
                                *((i - 2, split_addi) for i in range(64, 0x400, 64))):
        program[offset:offset + 4] = instruction.to_bytes(4, "little")
    LoadProgramSequence(image=ProgramImage(bytes(program), _BASE), step_cycles=0).run(env)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)
    trace = GoldenTrace([
        TraceEntry(0, _BASE, auipc, 4),
        TraceEntry(1, source_pc, jalr, 4, "jump_indirect", True, target_pc),
        *(TraceEntry(i + 2, target_pc + 2 * i, _CNOP, 2) for i in range(256)),
    ])
    env.backend_model.set_golden_trace(trace)
    env.backend_model.redirect_min_delay = env.backend_model.redirect_max_delay = 16
    recorder = env.functional_coverage
    paths, history, redirects, source_entries = {}, [], [], []

    def read(stem):
        value, path = _read_ifu_internal_with_path(recorder, env.dut, stem)
        assert value is not None, {"missing_ifu_probe": stem}
        paths[stem] = path
        return int(value)

    def observe_backend(cycle, _env):
        def port(stem):
            name = "io_backend_toFtq_redirect_" + stem
            signal = getattr(env.dut, name, None)
            assert signal is not None, {"missing_port": name}
            paths[name] = name
            return int(signal.value)

        if port("valid"):
            redirect = dict(cycle=cycle, pc=port("bits_pc"), target=port("bits_target"),
                            flag=port("bits_ftqIdx_flag"), value=port("bits_ftqIdx_value"),
                            offset=port("bits_ftqOffset"), level=port("bits_level"))
            assert source_entries
            assert redirect["pc"] == source_pc and redirect["target"] == target_pc
            assert (redirect["flag"], redirect["value"]) == source_entries[0]["ftq"]
            assert redirect["offset"] == 3 and redirect["level"] == 0
            redirects.append(redirect)
            env._emit_event("ifu.half_cleanup_backend_source", redirect)

    def observe(cycle, _env):
        for entry in env.backend_model._cfvec_queue:
            if entry.pc == source_pc and entry.golden_index == 1 and entry.path_state == "correct":
                assert entry.instr == jalr and entry.ftq_offset == 3
                if not source_entries:
                    source_entries.append(dict(pc=entry.pc, instr=entry.instr,
                                               ftq=(int(entry.ftq_flag), int(entry.ftq_value))))
        state = {stem: read(stem) for stem in (
            "io_fromFtq_redirect_valid", "s0_fire", "s1_fire", "s2_fire",
            "s0_flush", "s1_flush", "s2_flush", "s1_valid", "s2_valid_valid",
            "s0_prevEndIsHalfRvi", "s1_prevEndHalfRviInfo_valid",
            "s1_prevEndHalfRviInfo_bits_data", "s1_prevEndHalfRviInfo_bits_pc_addr",
            "s1_prevIBufEnqPtrDup_dup_0_value", "io_toIBuffer_valid",
            "s1_fetchBlock_0_startVAddr_addr", "s1_fetchBlock_0_ftqIdx_flag",
            "s1_fetchBlock_0_ftqIdx_value",
        )}
        state["cycle"] = cycle
        if not history or any(state[k] != history[-1][k] for k in state if k != "cycle"):
            env._emit_event("ifu.half_cleanup_state", state)
        history.append(state)

    env.register_cycle_observer(observe)
    env.register_pre_drive_cycle_observer(observe_backend)
    for _ in range(1024):
        env.step(1)
        if trace.cursor >= 18:
            break
    assert len(redirects) == 1 and trace.cursor >= 18
    collisions = [s for s in history if s["io_fromFtq_redirect_valid"]]
    assert len(collisions) == 1
    collision = collisions[0]
    after = next(s for s in history if s["cycle"] == collision["cycle"] + 1)
    first_s0 = next(s for s in history if s["cycle"] > collision["cycle"] and s["s0_fire"])
    first_s1 = next(s for s in history if s["cycle"] == first_s0["cycle"] + 1)
    isolated = [s for s in history if collision["cycle"] < s["cycle"] <= first_s0["cycle"]]
    assert all(s["s1_valid"] == s["s2_valid_valid"] == s["io_toIBuffer_valid"] == 0 for s in isolated)
    assert first_s0["s0_prevEndIsHalfRvi"] == 0
    assert first_s1["s1_valid"] == 1 and first_s1["s1_prevEndHalfRviInfo_valid"] == 0
    assert first_s1["s1_fetchBlock_0_startVAddr_addr"] << 1 == target_pc
    recovered = [item for item in env.monitor.observations if item.cycle > collision["cycle"]]
    assert len(recovered) >= 16
    assert [item.pc for item in recovered] == [target_pc + 2 * i for i in range(len(recovered))]
    assert all(item.instr == 0x13 and item.is_rvc for item in recovered)
    env._emit_event("ifu.half_cleanup_review", {
        "before": collision, "after": after, "backend_source": redirects[0],
        "first_recovery_s0": first_s0, "first_recovery_s1": first_s1,
        "isolated_cycle_count": len(isolated),
        "post_recovery_pcs": [item.pc for item in recovered], "signal_paths": paths,
    })
    assert collision["s1_prevEndHalfRviInfo_valid"] == 1, collision
    assert collision["s1_prevEndHalfRviInfo_bits_data"] == split_addi & 0xFFFF, collision
    assert all(collision[k] == 1 for k in ("s0_flush", "s1_flush", "s2_flush")), collision
    assert all(after[k] == 0 for k in (
        "s1_valid", "s2_valid_valid", "s0_prevEndIsHalfRvi",
        "s1_prevEndHalfRviInfo_bits_data", "s1_prevEndHalfRviInfo_bits_pc_addr",
        "s1_prevIBufEnqPtrDup_dup_0_value",
    )), after
    # Keep the unresolved literal checkpoint visible: no xfail, default-zero
    # substitute, or automatic promotion based only on successful recovery.
    # The valid register intentionally retains its prior value until s0_fire;
    # validity of the stale pipeline transaction is the safety boundary.
    assert after["s1_prevEndHalfRviInfo_valid"] == 1
    assert not env.monitor.get_errors()
