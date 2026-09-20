"""Uncache half-RVI state must not escape an unrelated backend recovery."""

from __future__ import annotations

import os

import pytest

from env.core.transactions import ProgramImage
from env.funcov.py.ifu.compact_funcov import _read_ifu_internal_with_path
from env.sequences import LoadProgramSequence
from env.support.pc_utils import fold_pc
from tests.py.support import uncache_scenarios as uncache


@pytest.mark.funcov_bins("BIN-922")
@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires real Frontend DUT")
def test_nc_half_rvi_backend_redirect_to_cacheable_isolates_old_state(env):
    first_va, first_pa = 0x80000000, 0x80001000
    second_pa, target = 0x80002000, 0x80003000
    old_pc = first_va + 4094
    old_instr, new_instr = 0x01300313, 0x02400393
    payload = bytearray(uncache._CNOP.to_bytes(2, "little") * 4096)
    payload[4094:4098] = old_instr.to_bytes(4, "little")
    env.page_table.clear()
    for va, pa, pbmt in ((first_va, first_pa, 1), (first_va + 4096, second_pa, 1),
                         (target, target, 0)):
        env.page_table.map_page(va >> 12, pa >> 12, v=1, r=1, x=1, pbmt=pbmt)
    env.ptw_agent.configure(mode="sv39", response_source="model", compare_drive_source="model")
    LoadProgramSequence(image=ProgramImage(bytes(payload), first_pa), step_cycles=0).run(env)
    LoadProgramSequence(image=ProgramImage(new_instr.to_bytes(4, "little") * 1024, target),
                        step_cycles=0).run(env)
    env.uncache_agent.configure(latency=96, mmio_latency=96)
    uncache._initialize_sv39_fetch(env, reset_vector=old_pc)
    uncache._configure_exec_attrs_16k(env, base_addr=0x80000000)

    history, paths = [], {}

    def read(stem):
        value, path = _read_ifu_internal_with_path(env.functional_coverage, env.dut, stem)
        assert value is not None, {"missing_ifu_probe": stem}
        paths[stem] = path
        if stem in {"uncacheRedirect_valid", "uncacheNeedResend", "uncachePc_addr",
                    "s0_fire", "s1_prevEndHalfRviInfo_valid"}:
            direct = "Frontend_top.Frontend.inner_ifu." + stem
            signal = env.dut.GetInternalSignal(direct)
            assert signal is not None and int(signal.value) == value, {
                "alias": path, "semantic_path": direct, "alias_value": value,
            }
            paths[stem + ":canary"] = direct
        return int(value)

    def observe(cycle, _env):
        state = {stem: read(stem) for stem in (
            "uncacheRedirect_valid", "uncacheNeedResend", "uncachePc_addr",
            "uncacheUnit.__Vtogcov__io_resp_bits_uncacheData",
            "io_fromFtq_redirect_valid", "wbRedirect_valid",
            "s0_fire", "s0_prevEndIsHalfRvi", "s1_valid", "s1_fire", "s1_flush",
            "s1_useUncacheFetch", "s1_prevEndHalfRviInfo_valid",
            "s1_prevEndHalfRviInfo_bits_pc_addr", "s1_prevEndHalfRviInfo_bits_data",
            "s1_fetchBlock_0_startVAddr_addr", "s1_fetchBlock_0_ftqIdx_flag",
            "s1_fetchBlock_0_ftqIdx_value", "s1_prevIBufEnqPtrDup_dup_0_value",
            "s2_valid_valid", "s2_useUncacheFetch", "s2_prevEndIsHalfRviInfo_valid",
            "s2_prevEndIsHalfRviInfo_bits_pc_addr", "s2_prevEndIsHalfRviInfo_bits_data",
            "io_toIBuffer_valid", "io_toIBuffer_ready", "s2_fire",
        )}
        lane = state["s1_prevIBufEnqPtrDup_dup_0_value"] & 3
        state.update(cycle=cycle, lane=lane, instr=read(f"s1_alignedInstrVec_{lane}_data"),
                     pc=read(f"s1_alignedInstrPcVec_{lane}_addr") << 1)
        if state["s2_fire"] and not state["s2_useUncacheFetch"]:
            # The recovery pointer is zero after backend redirect. Check the
            # first lane's registered identity and actual enqueue payload.
            state["delivery"] = {stem: read(stem) for stem in (
                "s2_alignedInstrPcVec_0_addr", "s2_alignedInstrVec_0_data",
                "s2_fetchBlock_0_ftqIdx_flag", "s2_fetchBlock_0_ftqIdx_value",
                "io_toIBuffer_bits_enqEnable", "io_toIBuffer_bits_valid",
                "io_toIBuffer_bits_foldpc_0", "io_toIBuffer_bits_instrs_0",
                "io_toIBuffer_bits_ftqPtr_0_flag", "io_toIBuffer_bits_ftqPtr_0_value",
                "io_toIBuffer_bits_instrEndOffset_0_offset",
            )}
        history.append(state)
        if state["uncacheRedirect_valid"] or state["io_fromFtq_redirect_valid"] or state["s1_fire"]:
            env._emit_event("ifu.uncache_half_isolation_state", state)

    env.register_cycle_observer(observe)
    uncache._force_redirect_to(env, old_pc)
    assert uncache._wait_for_request_addr(env, second_pa, max_cycles=6000)
    assert second_pa not in env.uncache_agent.get_stats()["response_addrs"]
    saved = [s for s in history if s["uncacheRedirect_valid"] and s["uncacheNeedResend"]]
    assert saved and saved[-1]["uncachePc_addr"] << 1 == old_pc, saved
    assert saved[-1]["uncacheUnit.__Vtogcov__io_resp_bits_uncacheData"] & 0xFFFF == old_instr & 0xFFFF
    assert any(s["s2_valid_valid"] and s["s2_prevEndIsHalfRviInfo_valid"]
               and s["s2_prevEndIsHalfRviInfo_bits_pc_addr"] << 1 == old_pc
               and s["s2_prevEndIsHalfRviInfo_bits_data"] == old_instr & 0xFFFF for s in history)

    queued_cycle = env.current_cycle
    uncache._force_redirect_to(env, target)
    assert uncache._wait_for_observed_pc(env, target + 60, max_cycles=6000)
    for _ in range(256):
        if second_pa in env.uncache_agent.get_stats()["response_addrs"]:
            break
        env.step(1)
    assert second_pa in env.uncache_agent.get_stats()["response_addrs"]
    redirect = next(s for s in history if s["cycle"] > queued_cycle and s["io_fromFtq_redirect_valid"])
    first_s0 = next(s for s in history if s["cycle"] > redirect["cycle"] and s["s0_fire"])
    first_s1 = next(s for s in history if s["cycle"] == first_s0["cycle"] + 1)
    assert first_s0["s0_prevEndIsHalfRvi"] == 0
    assert first_s1["s1_valid"] == 1 and first_s1["s1_useUncacheFetch"] == 0
    assert first_s1["s1_prevEndHalfRviInfo_valid"] == 0
    assert first_s1["pc"] == target and first_s1["instr"] == new_instr, first_s1
    first_s2 = next(s for s in history if s["cycle"] > first_s1["cycle"] and "delivery" in s)
    delivery = first_s2["delivery"]
    assert first_s2["s2_prevEndIsHalfRviInfo_valid"] == 0
    assert first_s2["io_toIBuffer_valid"] == first_s2["io_toIBuffer_ready"] == 1
    assert delivery["s2_alignedInstrPcVec_0_addr"] << 1 == target
    assert delivery["s2_alignedInstrVec_0_data"] == delivery["io_toIBuffer_bits_instrs_0"] == new_instr
    assert delivery["io_toIBuffer_bits_foldpc_0"] == fold_pc(target)
    assert delivery["io_toIBuffer_bits_enqEnable"] & delivery["io_toIBuffer_bits_valid"] & 1
    assert delivery["io_toIBuffer_bits_instrEndOffset_0_offset"] == 1
    for field in ("flag", "value"):
        assert (first_s1[f"s1_fetchBlock_0_ftqIdx_{field}"]
                == delivery[f"s2_fetchBlock_0_ftqIdx_{field}"]
                == delivery[f"io_toIBuffer_bits_ftqPtr_0_{field}"])
    recovered = [o for o in env.monitor.observations if o.cycle > redirect["cycle"]]
    assert len(recovered) >= 16
    assert [o.pc for o in recovered] == [target + i * 4 for i in range(len(recovered))]
    assert all(o.instr == new_instr and not o.is_rvc for o in recovered)
    assert not any(o.pc == old_pc for o in env.monitor.observations)
    env._emit_event("ifu.uncache_half_isolation_review", {
        "saved": saved[-1], "redirect": redirect, "recovery_s0": first_s0,
        "recovery_s1": first_s1, "recovery_s2": first_s2, "signal_paths": paths,
        "old_second_response_drained": True, "recovered_count": len(recovered),
    })
    assert not env.monitor.get_errors()
    assert env.functional_coverage.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_024")
