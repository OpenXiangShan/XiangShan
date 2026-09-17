"""Same-page NC dual-fetch suppression with a source-bound context replay."""

import os
from collections import Counter

import pytest

from env.core.transactions import ProgramImage
from env.funcov.py.ifu.cacheable_pipeline_funcov import _UPSTREAM_SIGNALS, _SIGNALS, _req_signal_names
from env.funcov.py.ftq.two_fetch_funcov import _TWO_FETCH_SIGNALS
from env.model.golden_trace import GoldenTrace, TraceEntry
from env.sequences import LoadProgramSequence
from tests.py.jiabowen.test_two_fetch_directed_flow_dut import (
    _BASE, _CNOP, _c_j, _second_block_taken_loop,
)
from tests.py.support import uncache_scenarios as uncache


def _nc_loop_trace(laps=16):
    entries = []
    for _ in range(laps):
        for start, end, target in ((0x3E, 0x74, 0x100), (0x100, 0x13E, 0x3E)):
            for offset in range(start, end, 2):
                entries.append(TraceEntry(len(entries), _BASE + offset, _CNOP, 2))
            entries.append(TraceEntry(len(entries), _BASE + end, _c_j(target - end),
                                      2, "jump", True, _BASE + target))
    return GoldenTrace(entries)


def _candidate_paths(paths):
    expanded = list(paths)
    for path in paths:
        if ".__Vtogcov__" in path:
            expanded.append(path.replace(".__Vtogcov__", "."))
        elif ".mainPipe." in path:
            expanded.append(path.replace(".mainPipe.", ".mainPipe.__Vtogcov__"))
    return tuple(dict.fromkeys(expanded))


def _dual_nc_candidate(snapshot):
    return all(snapshot.get(k) == 1 for k in (
        "mainpipe_fire", "second_requested", "second_waylookup_valid", "first_mmio", "second_mmio"))


def _check_single_nc_response(request, response):
    assert response["valid0"] == 1 and response["valid1"] == 0
    assert response["pbmt"] == 1 and response["exception"] == 0
    assert (response["flag"], response["index"]) == (
        request["first_ftq_flag"], request["first_ftq_value"])
    assert response["start"] == request["first_start"]


def _check_target_acceptance(recorder, response_checkpoints):
    assert response_checkpoints, "BIN-904 has no accepted single-response checkpoint"
    definition = recorder.definition_by_bin_id["BIN-904"]
    # Runtime keys are tuples; the group::point::bin spelling is JSON-only.
    target = recorder.hits.get(definition.key)
    assert target is not None and target.hits > 0, "BIN-904 has no runtime hit"
    assert any(p["cycle"] == target.first_cycle for p in response_checkpoints), (
        "BIN-904 first hit does not match a checked IFU acceptance", target.first_cycle)


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires real Frontend DUT")
@pytest.mark.parametrize("warm_cacheable", [
    pytest.param(False, id="cold-nc"),
    pytest.param(True, id="trained-then-nc", marks=pytest.mark.funcov_bins("BIN-904")),
])
def test_same_page_nc_dual_request_signal_canary(env, warm_cacheable):
    pa = uncache._NORMAL_PHYS_BASE
    payload = _second_block_taken_loop()
    if warm_cacheable:
        # Preserve the real replay source until the delayed CSR/TLB flush has
        # propagated. This is backend commit latency, not an internal force.
        env.backend_model.commit_min_delay = env.backend_model.commit_max_delay = 32
    env.page_table.clear()
    env.page_table.map_page(_BASE >> 12, pa >> 12, v=1, r=1, x=1, pbmt=0 if warm_cacheable else 1)
    env.ptw_agent.configure(mode="sv39", response_source="model", compare_drive_source="model")
    LoadProgramSequence(image=ProgramImage(payload=payload, base_addr=pa), step_cycles=0).run(env)
    env.initialize(reset_vector=_BASE + 0x3E, bare_mode=False, reset_cycles=20)
    assert not env.monitor.get_errors()
    env.monitor.set_expected_pc(_BASE + 0x3E)
    trace = _nc_loop_trace()
    env.backend_model.set_golden_trace(trace)
    env.dut.io_tlbCsr_satp_changed.value = 1
    env.step(1)
    env.dut.io_tlbCsr_satp_changed.value = 0
    uncache._configure_exec_pmp_4k(env, base_addr=pa)
    uncache._configure_exec_cacheable_pma_4k(env, base_addr=pa)
    env.uncache_agent.configure(latency=4, mmio_latency=4)
    recorder = env.functional_coverage
    warmup = None
    if warm_cacheable:
        source = None
        for _ in range(4096):
            env.step(1)
            if trace.cursor >= 480 and recorder.key_hit("two_fetch_ftq_eligibility", "eligible_dual"):
                candidates = [e for e in env.backend_model._cfvec_queue
                              if e.pc == _BASE + 0x13E and e.path_state == "correct"
                              and e.golden_index is not None and e.is_cfi]
                if candidates:
                    source = candidates[-1]
                    break
        assert trace.cursor >= 480
        assert recorder.key_hit("two_fetch_ftq_eligibility", "eligible_dual")
        assert source is not None
        assert trace.entries[source.golden_index].instr == _c_j(-0x100)
        assert trace.entries[source.golden_index + 1].pc == _BASE + 0x3E
        assert not env.monitor.get_errors()
        warmup = dict(cycle=env.current_cycle, trace_cursor=trace.cursor,
                      source_pc=source.pc, source_index=source.golden_index,
                      source_ftq=[source.ftq_flag, source.ftq_value], source_offset=source.ftq_offset)
        transition_seen = []

        def observe_transition(cycle, _env):
            if int(env.dut.io_backend_toFtq_redirect_valid.value) != 1 or transition_seen:
                return
            port = lambda stem: int(getattr(env.dut, "io_backend_toFtq_redirect_bits_" + stem).value)
            if (port("ftqIdx_flag"), port("ftqIdx_value")) != (source.ftq_flag, source.ftq_value):
                return
            assert port("pc") == source.pc and port("target") == _BASE + 0x3E
            assert port("ftqOffset") == source.ftq_offset and port("level") == 0
            # A deliberate source-bound replay discards the younger speculative
            # trace suffix. Restore the ISA successor of that exact checked CFI,
            # never seek by an observed output PC or disable the recovery guard.
            transition_seen.append(dict(cycle=cycle, old_cursor=trace.cursor,
                                        replay_cursor=source.golden_index + 1))
            trace.reset(source.golden_index + 1)
            env._emit_event("ifu.nc_context_source_bound_replay", {**warmup, **transition_seen[-1]})

        env.register_pre_drive_cycle_observer(observe_transition)
        env.page_table.map_page(_BASE >> 12, pa >> 12, v=1, r=1, x=1, pbmt=1)
        # Let both predicted FTQ entries pass BPU S3 before the initial NC
        # translation returns; otherwise only younger, cancelled windows pair.
        env.ptw_agent.configure(latency=24)
        # SfenceBundle.rs1/rs2 encode "register is x0", not its index.
        # Both true means all VA/all ASID; defaults (0,0,addr=0) do not
        # invalidate this high-address page.
        fence = env.prepare_sfence(rs1=1, rs2=1)
        assert int(env.dut.io_sfence_valid.value) == 1
        assert int(env.dut.io_sfence_bits_rs1.value) == int(env.dut.io_sfence_bits_rs2.value) == 1
        # Software prefetch is a legal higher-priority transaction at the
        # PrefetchPipe ingress. Hold it briefly across recovery so the first
        # demand prefetch sees two predictions past BPU's point of no return.
        env.dut.io_softPrefetch_0_bits_vaddr.value = _BASE + 0x180
        env.dut.io_softPrefetch_0_valid.value = 1
        env.backend_model.inject_redirect_from_cfvec(
            source_pc=source.pc, source_ftq_flag=source.ftq_flag,
            source_ftq_value=source.ftq_value, source_ftq_offset=source.ftq_offset,
            target_pc=_BASE + 0x3E, reason="bin904-pbmt-context-replay", level=0,
            taken=1, delay_cycles=8)
        env.step(2)
        env.release_sfence(fence)
        for _ in range(16):
            if transition_seen:
                break
            env.step(1)
        assert transition_seen
        assert not env.monitor.get_errors()
        warmup["transition"] = transition_seen
        warmup["soft_release_cycle"] = int(env.current_cycle) + 12
    counts, combinations, selected_paths, old_missing = Counter(), Counter(), {}, set()
    checkpoints, response_checkpoints, presentations = [], [], []
    pending = {}

    def read_paths(candidates):
        values = {path: int(value) for path in _candidate_paths(candidates)
                  if (value := recorder._try_read_dut_signal(env.dut, path)) is not None}
        assert values and len(set(values.values())) == 1, (candidates, values)
        return next(iter(values.values())), values

    def observe(cycle, _env):
        snapshot, readings = {}, {}
        if warmup and cycle < warmup["soft_release_cycle"]:
            soft, soft_paths = read_paths(("Frontend_top.Frontend.inner_icache.softPrefetchValid",
                                          "Frontend_top.Frontend.inner_icache.__Vtogcov__softPrefetchValid"))
            ready, ready_paths = read_paths(_TWO_FETCH_SIGNALS["prefetch_ready"])
            assert soft == 1 and ready == 0, (cycle, soft, ready)
            warmup["soft_arbitration_canary"] = dict(cycle=cycle, soft=soft, ftq_ready=ready,
                                                    readings={"soft": soft_paths, "ready": ready_paths})
        for key, old_paths in _UPSTREAM_SIGNALS.items():
            values = {}
            for path in _candidate_paths(old_paths):
                value = recorder._try_read_dut_signal(env.dut, path)
                if value is not None:
                    values[path] = int(value)
            if not any(path in values for path in old_paths):
                old_missing.add(key)
            assert values, {"missing_probe": key, "candidates": _candidate_paths(old_paths)}
            assert len(set(values.values())) == 1, {"inconsistent_paths": key, "readings": values}
            snapshot[key] = next(iter(values.values()))
            selected_paths[key] = list(values)
            readings[key] = values
        # Every presented NC response is single, including when backpressured.
        # Complete a candidate only on real IFU acceptance and exact identity.
        if read_paths(_SIGNALS["req_valid"])[0] == 1:
            response, response_paths = {}, {}
            for key, block, field in (("valid0", 0, "valid"), ("valid1", 1, "valid"),
                                      ("flag", 0, "ftqIdx_flag"), ("index", 0, "ftqIdx_value"),
                                      ("start", 0, "startVAddr_addr"),
                                      ("pbmt", 0, "icacheMeta_itlbPbmt"),
                                      ("exception", 0, "icacheMeta_exception_value")):
                candidates = _req_signal_names(block, field)
                if key == "valid0":
                    # The aggregate ABI removes constant info(0).valid. Read
                    # its actual IFU semantic object, never synthesize a value.
                    candidates = tuple(root + "s0_fetchBlock_0_valid" for root in (
                        "Frontend_top.Frontend.inner_ifu.",
                        "Frontend_top.Frontend.inner_ifu.__Vtogcov__"))
                response[key], response_paths[key] = read_paths(candidates)
            if response["pbmt"] == 1:
                assert response["valid0"] == 1 and response["valid1"] == 0, response
                counts["single_nc_response_presentations"] += 1
            identity = (response["flag"], response["index"])
            match = pending.get(identity)
            output_fire = read_paths(_SIGNALS["s0_fire"])[0]
            if match:
                presentations.append(dict(cycle=cycle, input_cycle=match["cycle"],
                                          response=response, ifu_s0_fire=output_fire))
            if match and output_fire == 1:
                # Require the immediately registered response. Do not join a
                # later reused circular FTQ tag or cross a flush/time gap.
                if cycle == match["cycle"] + 1:
                    _check_single_nc_response(match["input"], response)
                    proof = dict(input=match, cycle=cycle, response=response,
                                 response_paths=response_paths, ifu_s0_fire=1)
                    response_checkpoints.append(proof)
                    env._emit_event("ifu.nc_dual_suppression_delivery", proof)
                del pending[identity]
        pending.clear()  # Only an adjacent input->output witness is eligible here.
        if snapshot["mainpipe_fire"]:
            counts["mainpipe_fire"] += 1
            fields = ("second_requested", "second_waylookup_valid", "first_mmio", "second_mmio", "real_two_fetch")
            combinations[tuple(snapshot[k] for k in fields)] += 1
            if _dual_nc_candidate(snapshot):
                assert snapshot["real_two_fetch"] == 0, snapshot
                counts["dual_nc_suppressed"] += 1
                snapshot["first_start"], readings["first_start"] = read_paths(_TWO_FETCH_SIGNALS["ftq_req0_start"])
                snapshot["second_start"], readings["second_start"] = read_paths(_TWO_FETCH_SIGNALS["ftq_req1_start"])
                assert snapshot["first_start"] >> 11 == snapshot["second_start"] >> 11
                checkpoint = dict(cycle=cycle, phase="cycle_observer", input=snapshot, readings=readings)
                pending[(snapshot["first_ftq_flag"], snapshot["first_ftq_value"])] = checkpoint
                if len(checkpoints) < 8:
                    checkpoints.append(checkpoint)
                    env._emit_event("ifu.nc_dual_suppression_canary", checkpoint)

    env.register_cycle_observer(observe)
    for _ in range(24000):
        if warmup and env.current_cycle == warmup["soft_release_cycle"]:
            env.dut.io_softPrefetch_0_valid.value = 0
        env.step(1)
        if trace.cursor == len(trace.entries):
            break
    result = dict(event="nc_dual_suppression_reachability", counts=dict(counts),
                  combinations=[dict(zip(("second_requested", "second_waylookup_valid", "first_mmio",
                                           "second_mmio", "real_two_fetch", "count"), (*k, v)))
                                for k, v in combinations.items()],
                  signal_paths=selected_paths, old_missing=sorted(old_missing),
                  checkpoints=checkpoints, trace_cursor=trace.cursor, trace_length=len(trace.entries),
                  response_checkpoints=response_checkpoints,
                  candidate_presentations=presentations,
                  warmup=warmup,
                  uncache={k: v for k, v in env.uncache_agent.get_stats().items()
                           if not isinstance(v, (list, dict))},
                  backend={k: v for k, v in env.backend_model.get_stats().items()
                           if not isinstance(v, (list, dict))})
    recorder.risk_observations.append(result)
    env._emit_event("ifu.nc_dual_suppression_summary", result)
    assert trace.cursor == len(trace.entries), result
    assert counts["mainpipe_fire"] > 0, result
    assert env.uncache_agent.get_stats()["resp_count"] > 0, result
    assert not env.monitor.get_errors()
    if warm_cacheable:
        _check_target_acceptance(recorder, response_checkpoints)
