"""Source-bound backend recovery colliding with a younger checker writeback."""

from __future__ import annotations

import os

import pytest

from env.core.transactions import ProgramImage
from env.funcov.py.ifu.compact_funcov import _read_ifu_internal_with_path, _read_predchecker_with_path
from env.model.golden_trace import GoldenTrace, TraceEntry
from env.sequences import LoadProgramSequence
from env.support.pc_utils import fold_pc
from tests.py.jiabowen.test_ifu_predchecker_v3_dut import _BASE, _CNOP, _jal_x0


@pytest.mark.funcov_bins("BIN-949", "BIN-995")
@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires real Frontend DUT")
def test_backend_redirect_wins_younger_checker_writeback(env):
    # AUIPC x6,0; JALR x0,x6,1024 -> base+0x400. The fallthrough path
    # contains a cold JAL to base+0x200, giving a distinguishable wrong target.
    source_pc, target_pc, wrong_pc, wrong_target = _BASE + 4, _BASE + 0x400, _BASE + 0xC0, _BASE + 0x200
    auipc, jalr = 0x00000317, 0x40030067
    program = bytearray(_CNOP.to_bytes(2, "little") * 2048)
    for pc, instruction in ((_BASE, auipc), (source_pc, jalr),
                            (wrong_pc, _jal_x0(wrong_target - wrong_pc))):
        offset = pc - _BASE
        program[offset:offset + 4] = instruction.to_bytes(4, "little")
    LoadProgramSequence(image=ProgramImage(bytes(program), _BASE), step_cycles=0).run(env)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)
    entries = [TraceEntry(0, _BASE, auipc, 4),
               TraceEntry(1, source_pc, jalr, 4, "jump_indirect", True, target_pc)]
    entries.extend(TraceEntry(i + 2, target_pc + 2 * i, _CNOP, 2) for i in range(256))
    trace = GoldenTrace(entries)
    env.backend_model.set_golden_trace(trace)
    # Natural checker writeback is cycle 588; backend queued at 580 and
    # takes two interface/FTQ cycles after its six-cycle execution delay.
    env.backend_model.redirect_min_delay = env.backend_model.redirect_max_delay = 6
    paths, timeline, collisions = {}, [], []
    source_entries, backend_redirects, checker_requests = [], [], []
    pending = None
    recorder = env.functional_coverage

    def ifu(stem):
        value, path = _read_ifu_internal_with_path(recorder, env.dut, stem)
        assert value is not None, {"missing_ifu_probe": stem}
        paths["ifu." + stem] = path
        return int(value)

    def ftq(stem):
        # Ftq.sv directly assigns io_toBpu.redirect.valid = redirect_1_valid.
        if stem == "io_toBpu_redirect_valid":
            stem = "redirect_1_valid"
        for prefix in ("Frontend_top.Frontend.inner_ftq.",
                       "Frontend_top.Frontend.inner_ftq.__Vtogcov__",
                       "Frontend_top.Frontend._inner_ftq_"):
            path = prefix + stem
            value = recorder._try_read_dut_signal(env.dut, path)
            if value is not None:
                paths["ftq." + stem] = path
                return int(value)
        raise AssertionError({"missing_ftq_probe": stem})

    def port(name):
        signal = getattr(env.dut, name, None)
        assert signal is not None, {"missing_port": name}
        paths[name] = name
        return int(signal.value)

    def pred(stem):
        value, path = _read_predchecker_with_path(recorder, env.dut, stem)
        assert value is not None, {"missing_predchecker_probe": stem}
        paths["pred." + stem] = path
        return int(value)

    def canary(cycle, module, expected):
        observations = {}
        for stem, value in expected.items():
            readings = {}
            for prefix in (f"Frontend_top.Frontend.inner_{module}.",
                           f"Frontend_top.Frontend.inner_{module}.__Vtogcov__",
                           f"Frontend_top.Frontend._inner_{module}_"):
                path = prefix + stem
                observed = recorder._try_read_dut_signal(env.dut, path)
                if observed is not None:
                    readings[path] = int(observed)
            assert readings and all(v == value for v in readings.values()), (stem, value, readings)
            observations[stem] = readings
        env._emit_event("ifu.backend_checker_signal_canary", {
            "cycle": cycle, "module": module, "phase": "cycle_observer", "readings": observations,
        })

    def observe_backend_input(cycle, _env):
        if port("io_backend_toFtq_redirect_valid") != 1:
            return
        record = {key: port("io_backend_toFtq_redirect_" + stem) for key, stem in {
            "pc": "bits_pc", "target": "bits_target", "flag": "bits_ftqIdx_flag",
            "value": "bits_ftqIdx_value", "offset": "bits_ftqOffset", "level": "bits_level",
        }.items()}
        assert source_entries, "backend redirect has no observed JALR source"
        source = source_entries[0]
        assert record == dict(pc=source_pc, target=target_pc, flag=source["flag"],
                              value=source["value"], offset=3, level=0), record
        backend_redirects.append(dict(cycle=cycle, **record))
        env._emit_event("ifu.backend_checker_backend_source", backend_redirects[-1])

    def observe(cycle, _env):
        nonlocal pending
        for entry in env.backend_model._cfvec_queue:
            if entry.pc != source_pc or entry.golden_index != 1 or entry.path_state != "correct":
                continue
            assert entry.instr == jalr and entry.ftq_offset == 3
            source = dict(pc=entry.pc, instr=entry.instr, flag=int(entry.ftq_flag),
                          value=int(entry.ftq_value), offset=int(entry.ftq_offset))
            if not source_entries:
                source_entries.append(source)
                env._emit_event("ifu.backend_checker_source_instruction", dict(cycle=cycle, **source))
            assert source_entries[0] == source
        # Capture the actual younger checker request, before its registered
        # redirect. Its program bytes and request identity are independent of
        # the FTQ selected redirect being checked below.
        # wbEnable captures the request independently of IBuffer readiness.
        # Here backpressure deliberately leaves the younger JAL unaccepted.
        if ifu("s2_valid_valid") == 1 and ifu("wbEnable") == 1 and ifu("s2_flush") == 0:
            for slot in range(35):
                prefix = f"s2_alignedInstrVec_{slot}_"
                if ifu(prefix + "valid") != 1 or ifu(f"s2_alignedInstrPcVec_{slot}_addr") << 1 != wrong_pc:
                    continue
                assert ifu(prefix + "data") == _jal_x0(wrong_target - wrong_pc)
                assert ifu(prefix + "isPredTaken") == ifu(prefix + "invalidTaken") == 0
                raw, cross = ifu(prefix + "blockSel"), ifu(prefix + "isCrossBlockInstr")
                owner = int(bool(raw or cross))
                request = dict(cycle=cycle, slot=slot, raw=raw, cross=cross, owner=owner,
                    flag=ifu(f"s2_fetchBlock_{owner}_ftqIdx_flag"),
                    value=ifu(f"s2_fetchBlock_{owner}_ftqIdx_value"),
                    offset=ifu(prefix + "endOffset"),
                    block_pc=ifu(f"s2_fetchBlock_{owner}_startVAddr_addr") << 1)
                assert request["offset"] == 1 and request["block_pc"] == wrong_pc
                assert ifu("io_toIBuffer_valid") == 1
                assert ifu("io_toIBuffer_ready") == ifu("s2_fire") == 0
                assert ifu("io_toIBuffer_bits_enqEnable") == ifu("s2_fixedInstrValid") == 1 << slot
                assert ifu(f"io_toIBuffer_bits_instrs_{slot}") == _jal_x0(wrong_target - wrong_pc)
                assert ifu(f"io_toIBuffer_bits_foldpc_{slot}") == fold_pc(wrong_pc)
                assert ifu(f"io_toIBuffer_bits_ftqPtr_{slot}_flag") == request["flag"]
                assert ifu(f"io_toIBuffer_bits_ftqPtr_{slot}_value") == request["value"]
                assert ifu(f"io_toIBuffer_bits_instrEndOffset_{slot}_offset") == request["offset"]
                checker_requests.append(request)
                env._emit_event("ifu.backend_checker_younger_request", request)
        backend = ifu("io_fromFtq_redirect_valid")
        wb = ifu("wbRedirect_valid")
        outbound = ifu("io_toFtq_wbRedirect_valid")
        if backend or wb:
            record = dict(cycle=cycle, backend=backend, wb=wb, outbound=outbound,
                          checker_target=ifu("io_toFtq_wbRedirect_bits_target"),
                          selected_target=ftq("io_toBpu_redirect_bits_target_addr") << 1,
                          selected_valid=ftq("io_toBpu_redirect_valid"))
            timeline.append(record)
            env._emit_event("ifu.backend_checker_candidate", record)
        if pending:
            assert cycle == pending["cycle"] + 1
            assert ftq("ifuRedirect_valid") == 0
            assert ftq("ifuResolve_valid") == 0
            assert ifu("s1_valid") == ifu("s2_valid_valid") == 0
            assert ifu("s0_prevEndIsHalfRvi") == ifu("s1_prevEndHalfRviInfo_valid") == 0
            assert ifu("s1_prevEndHalfRviInfo_bits_data") == 0
            assert ifu("s1_prevEndHalfRviInfo_bits_pc_addr") == 0
            assert ifu("s1_prevIBufEnqPtrDup_dup_0_value") == 0
            canary(cycle, "ifu", {stem: 0 for stem in (
                "s1_valid", "s2_valid_valid", "s0_prevEndIsHalfRvi",
                "s1_prevEndHalfRviInfo_valid", "s1_prevEndHalfRviInfo_bits_data",
                "s1_prevEndHalfRviInfo_bits_pc_addr", "s1_prevIBufEnqPtrDup_dup_0_value",
            )})
            canary(cycle, "ftq", {"ifuRedirect_valid": 0, "ifuResolve_valid": 0})
            collisions.append({**pending, "check_cycle": cycle, "paths": dict(paths)})
            env._emit_event("ifu.backend_checker_priority_checkpoint", collisions[-1])
            pending = None
        if backend and wb:
            probes = {stem: ftq(stem) for stem in (
                "backendRedirect_valid", "aheadIdxMatch", "redirectReg_bits_target",
                "resolveQueue_io_backendRedirectPtr_flag", "resolveQueue_io_backendRedirectPtr_value",
                "backendRedirect_bits_ftqOffset", "redirect_1_valid", "redirect_1_bits_ftqIdx_flag",
                "redirect_1_bits_ftqIdx_value", "redirect_1_bits_ftqOffset",
                "io_toBpu_redirect_bits_target_addr",
            )}
            env._emit_event("ifu.backend_checker_selection_probes", {"cycle": cycle, **probes})
            canary(cycle, "ftq", probes)
            assert probes["backendRedirect_valid"] == 1, probes
            assert probes["redirect_1_bits_ftqIdx_flag"] == probes["resolveQueue_io_backendRedirectPtr_flag"], probes
            assert probes["redirect_1_bits_ftqIdx_value"] == probes["resolveQueue_io_backendRedirectPtr_value"], probes
            assert probes["redirect_1_bits_ftqOffset"] == probes["backendRedirect_bits_ftqOffset"], probes
            assert probes["redirectReg_bits_target"] == target_pc, probes
            assert len(backend_redirects) == 1 and probes["aheadIdxMatch"] == 0
            assert cycle == backend_redirects[0]["cycle"] + 1
            assert probes["resolveQueue_io_backendRedirectPtr_flag"] == source_entries[0]["flag"]
            assert probes["resolveQueue_io_backendRedirectPtr_value"] == source_entries[0]["value"]
            assert probes["backendRedirect_bits_ftqOffset"] == 3
            assert len(checker_requests) == 1
            request = checker_requests[0]
            assert cycle == request["cycle"] + 1
            for stem, expected in {"ftqIdx_flag": request["flag"], "ftqIdx_value": request["value"],
                                   "ftqOffset": request["offset"], "pc": request["block_pc"],
                                   "canTrain": 1, "isRVC": 0, "attribute_branchType": 2,
                                   "attribute_rasAction": 0}.items():
                assert ifu("io_toFtq_wbRedirect_bits_" + stem) == expected, stem
            for stem, expected in {"valid": 1, "bits_blockSel": request["raw"],
                                   "bits_isCrossBlockInstr": request["cross"],
                                   "bits_endOffset": request["offset"],
                                   "bits_target_addr": wrong_target >> 1,
                                   "bits_invalidTaken": 0}.items():
                assert pred("io_resp_stage2Out_checkerRedirect_" + stem) == expected, stem
            assert (request["flag"], request["value"]) != (source_entries[0]["flag"], source_entries[0]["value"])
            assert outbound == 1
            assert ifu("io_toFtq_wbRedirect_bits_target") == wrong_target
            assert ftq("io_toBpu_redirect_valid") == 1
            assert ftq("io_toBpu_redirect_bits_target_addr") << 1 == target_pc
            assert ifu("s0_flush") == ifu("s1_flush") == ifu("s2_flush") == 1
            assert ifu("io_toIBuffer_valid") == ifu("s2_fire") == 0
            canary(cycle, "ifu", {
                "io_fromFtq_redirect_valid": 1, "wbRedirect_valid": 1,
                "io_toFtq_wbRedirect_valid": 1, "s0_flush": 1, "s1_flush": 1, "s2_flush": 1,
            })
            pending = dict(timeline[-1], backend_source=backend_redirects[0],
                           checker_request=request, selected=probes)

    env.register_cycle_observer(observe)
    env.register_pre_drive_cycle_observer(observe_backend_input)
    for _ in range(1024):
        env.step(1)
        if trace.cursor >= 18:
            break
    assert collisions, {"reason": "no same-cycle source-bound collision", "timeline": timeline,
                        "trace_cursor": trace.cursor}
    assert trace.cursor >= 18
    assert len(collisions) == len(backend_redirects) == len(source_entries) == 1
    source_observed = [item for item in env.monitor.observations if item.pc == source_pc]
    assert len(source_observed) == 1 and source_observed[0].instr == jalr and not source_observed[0].is_rvc
    recovered = [item for item in env.monitor.observations if item.cycle > collisions[0]["cycle"]]
    assert len(recovered) >= 16
    assert [item.pc for item in recovered] == [target_pc + 2 * i for i in range(len(recovered))]
    assert all(item.instr == 0x13 and item.is_rvc for item in recovered)
    env._emit_event("ifu.backend_checker_recovery_delivery", {
        "trace_cursor": trace.cursor, "source": source_entries[0],
        "post_recovery_pcs": [item.pc for item in recovered], "signal_paths": dict(paths),
    })
    for bin_id in ("BIN-949", "BIN-995"):
        spec = recorder.definition_by_bin_id[bin_id]
        assert recorder.key_hit(spec.coverage_group, spec.bin_name, coverpoint=spec.coverpoint)
    assert not env.monitor.get_errors()
