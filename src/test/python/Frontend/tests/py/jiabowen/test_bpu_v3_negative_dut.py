from __future__ import annotations

import os

import pytest

from dataclasses import asdict
from env.support.bpu_v3_contract import (
    PREFETCH_DEPTH,
    BpuV3SignalUnavailable,
    read_mbtb_write_buffer_dirty,
    sample_bpu_target_diff_cycle,
    sample_bpu_v3_cycle,
    sample_mbtb_write_buffer_events,
    sample_prefetch_depth_cycle,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BASE = 0x8000_0000
_NOP = 0x0000_0013

from tests.py.support.bpu_target_program import build_bpu_target_change_program
from tests.py.support.mbtb_write_program import build_mbtb_write_compare_program
from tests.py.support.mbtb_training_request import decoded_source_registration, sample_mbtb_write_launch


def _cycle_limit(name: str, default: int) -> int:
    raw = os.getenv(str(name), "").strip()
    if not raw:
        return int(default)
    value = int(raw, 0)
    assert value > 0, f"{name} must be positive"
    return int(value)


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_bpu_all_not_taken_does_not_emit_s3_override(env) -> None:
    env.load_program(_NOP.to_bytes(4, "little") * 4096, _BASE)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)

    recorder = env.functional_coverage
    assert recorder is not None and recorder.env is env
    reader = lambda name: recorder._try_read_dut_signal(env.dut, name)

    candidate_cycles: list[int] = []
    actual_paths: dict[str, str] | None = None
    for _ in range(_cycle_limit("TB_BPU_ALL_NT_MAX_CYCLES", 1024)):
        env.step(1)
        try:
            sample = sample_bpu_v3_cycle(reader, require_native=True)
        except BpuV3SignalUnavailable as exc:
            pytest.fail(str(exc), pytrace=False)
        if not sample.is_all_not_taken_candidate:
            continue

        actual_paths = dict(sample.signal_paths)
        recorder.risk_observations.append(
            {
                "cycle": int(env.current_cycle),
                "risk": "bpu_all_not_taken_no_s3_override",
                "s3_valid": int(sample.s3_valid),
                "s3_s1_prediction_taken": int(sample.s3_s1_prediction_taken),
                "s3_taken_mask": list(sample.s3_taken_mask),
                "s3_override": int(sample.s3_override),
                "signal_paths": actual_paths,
                "passed": sample.s3_override == 0,
            }
        )
        assert sample.s3_override == 0, {
            "reason": "all-not-taken BPU s3 result emitted an override",
            "cycle": int(env.current_cycle),
            "sample": sample,
        }
        candidate_cycles.append(int(env.current_cycle))
        if len(candidate_cycles) >= 8:
            break

    assert len(candidate_cycles) >= 8, {
        "reason": "no repeatable all-not-taken BPU s3 observation",
        "candidate_cycles": candidate_cycles,
        "actual_signal_paths": actual_paths,
        "backend": env.backend_model.get_stats(),
        "monitor_errors": env.monitor.get_errors(),
    }
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.funcov_bins("BIN-742", "BIN-746", "BIN-755", "BIN-758")
def test_prefetch_depth_full_wrap_and_flush_recovery_canary(env) -> None:
    env.load_program(_NOP.to_bytes(4, "little") * 8192, _BASE)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)

    recorder = env.functional_coverage
    assert recorder is not None and recorder.env is env
    reader = lambda name: recorder._try_read_dut_signal(env.dut, name)

    env.backend_model.set_can_accept(0)
    max_occupancy = 0
    full_sample = None
    previous_sample = None
    dual_write_count = 0
    dual_wrap_cycles: list[int] = []
    for _ in range(_cycle_limit("TB_PREFETCH_DEPTH_FILL_MAX_CYCLES", 4096)):
        env.step(1)
        try:
            sample = sample_prefetch_depth_cycle(reader, require_native=True)
        except BpuV3SignalUnavailable as exc:
            pytest.fail(str(exc), pytrace=False)
        max_occupancy = max(max_occupancy, sample.num_valid_entries)
        if sample.dual_write_fire:
            dual_write_count += 1
        if (
            previous_sample is not None
            and previous_sample.dual_write_fire
            and previous_sample.write_ptr[1] in (30, 31)
        ):
            previous_raw = (
                (previous_sample.write_ptr[0] & 1) << 5
            ) | (previous_sample.write_ptr[1] & 0x1F)
            expected_raw = (previous_raw + 2) & 0x3F
            expected_ptr = (expected_raw >> 5, expected_raw & 0x1F)
            if sample.write_ptr == expected_ptr:
                dual_wrap_cycles.append(int(env.current_cycle))
        previous_sample = sample
        if not sample.full:
            continue
        full_sample = sample
        assert sample.shared_write_ready == 0, {
            "reason": "PrefetchDepth full state accepted a new WayLookup write",
            "cycle": int(env.current_cycle),
            "sample": sample,
        }
        break

    assert full_sample is not None, {
        "reason": "legal frontend backpressure did not reach PrefetchDepth",
        "prefetch_depth": PREFETCH_DEPTH,
        "max_occupancy": max_occupancy,
        "backend": env.backend_model.get_stats(),
        "icache": env.icache_agent.get_stats(),
    }
    assert dual_write_count > 0, {
        "reason": "no accepted dual write while filling PrefetchDepth",
        "max_occupancy": max_occupancy,
    }
    assert dual_wrap_cycles, {
        "reason": "no exact dual-write pointer wrap while filling PrefetchDepth",
        "dual_write_count": dual_write_count,
        "write_ptr": full_sample.write_ptr,
    }
    recorder.risk_observations.append(
        {
            "cycle": int(env.current_cycle),
            "risk": "prefetch_depth_full_raw_observation",
            "prefetch_depth": PREFETCH_DEPTH,
            "max_occupancy": max_occupancy,
            "full_write_valid": [
                int(full_sample.write0_valid),
                int(full_sample.write1_valid),
            ],
            "shared_write_ready": int(full_sample.shared_write_ready),
            "dual_write_count": dual_write_count,
            "dual_wrap_cycles": list(dual_wrap_cycles),
            "signal_paths": dict(full_sample.signal_paths),
        }
    )

    redirect_target = _BASE + 0x2000
    env.backend_model.inject_redirect(
        redirect_target,
        "prefetch_depth_full_flush_recovery",
        delay_cycles=0,
    )
    flush_cycle = None
    cleared_cycle = None
    for _ in range(_cycle_limit("TB_PREFETCH_DEPTH_FLUSH_MAX_CYCLES", 512)):
        env.step(1)
        sample = sample_prefetch_depth_cycle(reader, require_native=True)
        if sample.global_flush == 1 and flush_cycle is None:
            flush_cycle = int(env.current_cycle)
        if flush_cycle is not None and sample.num_valid_entries == 0:
            cleared_cycle = int(env.current_cycle)
            break

    assert flush_cycle is not None and cleared_cycle is not None, {
        "reason": "global redirect did not clear the full WayLookup queue",
        "flush_cycle": flush_cycle,
        "cleared_cycle": cleared_cycle,
        "max_occupancy": max_occupancy,
    }
    recorder.risk_observations.append(
        {
            "cycle": int(cleared_cycle),
            "risk": "prefetch_depth_flush_cleared",
            "flush_cycle": int(flush_cycle),
            "cleared_cycle": int(cleared_cycle),
        }
    )

    env.backend_model.set_can_accept(1)
    recovered = False
    for _ in range(_cycle_limit("TB_PREFETCH_DEPTH_RECOVERY_MAX_CYCLES", 1024)):
        env.step(1)
        sample = sample_prefetch_depth_cycle(reader, require_native=True)
        if sample.num_valid_entries > 0 or (
            sample.to_mainpipe_valid == 1 and sample.to_mainpipe_ready == 1
        ):
            recovered = True
            break

    assert recovered, {
        "reason": "WayLookup did not accept or deliver a post-flush transaction",
        "flush_cycle": flush_cycle,
        "cleared_cycle": cleared_cycle,
    }
    recorder.risk_observations.append(
        {
            "cycle": int(env.current_cycle),
            "risk": "prefetch_depth_post_flush_recovery",
            "flush_cycle": int(flush_cycle),
            "cleared_cycle": int(cleared_cycle),
        }
    )
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mbtb_write_buffer_compare_bits_semantics_canary(env) -> None:
    payload, trace, program = build_mbtb_write_compare_program(_BASE)
    env.load_program(payload, _BASE)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    assert not env.monitor.get_errors()
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)
    # The other BTBs can represent a wider target and must not hide the real
    # MainBtb target miss. Keep MainBtb enabled for both prediction and training.
    env.set_bp_ctrl_enable(ubtb_enable=0, abtb_enable=0, mbtb_enable=1,
                           tage_enable=0, sc_enable=0, ittage_enable=0)
    env.backend_model.set_golden_trace(trace)
    recorder = env.functional_coverage
    assert recorder is not None and recorder.env is env
    reader = lambda name: recorder._try_read_dut_signal(env.dut, name)
    source_identity = (program["set_idx"], program["tag"], program["position"])
    initial_bits = (3, 0, (program["initial_target"] >> 1) & ((1 << 20) - 1), 1)
    changed_bits = (3, 0, (program["changed_target"] >> 1) & ((1 << 20) - 1), 1)
    decoded_observations = {}
    equal_cycles, change_cycles, observations = [], [], []
    actual_paths = {}
    pending_events = []
    stopped_input = False
    previous_launch = None

    def observe_decoded_source() -> None:
        # Observe only the backend's real, decoded, golden-matched cfVecs. This
        # never queues, edits, reuses, or invents a ResolveEntry or FTQ identity.
        for entry in env.backend_model._cfvec_queue:
            if int(entry.pc) != program["source_pc"] or entry.golden_match_state != "matched":
                continue
            index = int(entry.golden_index)
            if index in decoded_observations:
                continue
            oracle = trace.entries[index]
            assert int(entry.instr) == program["source_instruction"] == oracle.instr
            assert not entry.is_rvc and entry.is_cfi and oracle.kind == "jump_indirect"
            assert entry.golden_target_pc == oracle.target_pc
            assert oracle.target_pc in (program["initial_target"], program["changed_target"])
            start_pc = decoded_source_registration(env.backend_model, entry, program)
            decoded_observations[index] = {
                "golden_index": index, "cycle": int(entry.cycle),
                "inst_pc": int(entry.pc), "instruction": int(entry.instr),
                "target": int(oracle.target_pc), "start_pc": int(start_pc),
                "ftq_flag": int(entry.ftq_flag), "ftq_value": int(entry.ftq_value),
                "ftq_offset": int(entry.ftq_offset),
                "native_from_ftq_index_bits": 6,
                "exact_ftq_occurrence_match_proven": False,
            }

    def sample_cycle() -> None:
        nonlocal pending_events, previous_launch
        current_launch = sample_mbtb_write_launch(
            reader, cycle=int(env.current_cycle), align_bank=program["align_bank"],
            internal_bank=program["internal_bank"],
        )
        for event in pending_events:
            dirty, path = read_mbtb_write_buffer_dirty(reader, event)
            assert dirty == int(event.semantic_changed), {
                "reason": "MBTB hitWritten dirty update did not match compareBits semantics",
                "cycle": int(env.current_cycle), "expected_dirty": int(event.semantic_changed),
                "observed_dirty": dirty, "dirty_path": path, "event": event,
            }
            actual_paths["dirty_after_" + path] = path
        pending_events = []
        try:
            events = sample_mbtb_write_buffer_events(reader)
        except BpuV3SignalUnavailable as exc:
            pytest.fail(str(exc), pytrace=False)
        seen_slots = set()
        for event in events:
            if ((event.align_bank, event.internal_bank) !=
                    (program["align_bank"], program["internal_bank"])
                    or event.incoming.identity != source_identity):
                continue
            slot = (event.align_bank, event.internal_bank, event.row, event.entry)
            assert slot not in seen_slots, {"reason": "ambiguous simultaneous source writes", "event": event}
            seen_slots.add(slot)
            assert event.identity_matches and event.dirty == 0, event
            assert event.incoming.compare_bits in (initial_bits, changed_bits), event
            assert event.stored.compare_bits in (initial_bits, changed_bits), event
            assert previous_launch is not None, {"reason": "no sampled native launch before registered event", "event": event}
            training = previous_launch.bind_event(event, program, event_cycle=int(env.current_cycle))
            matching_decodes = [item for item in decoded_observations.values()
                                if item["target"] == training.target]
            assert matching_decodes, {"reason": "source write has no real decoded oracle-bound CFI", "event": event}
            actual_paths.update(event.signal_paths)
            actual_paths.update(previous_launch.signal_paths)
            pending_events.append(event)
            if event.semantic_changed:
                assert event.stored.compare_bits == initial_bits and event.incoming.compare_bits == changed_bits, event
                assert equal_cycles, {"reason": "lower-target change arrived before semantic-equal evidence", "event": event}
                change_cycles.append(int(env.current_cycle))
            elif event.incoming.compare_bits == initial_bits:
                equal_cycles.append(int(env.current_cycle))
            observations.append({"cycle": int(env.current_cycle), "trace_cursor": trace.cursor,
                                 "event": asdict(event), "training": asdict(training),
                                 "native_launch": asdict(previous_launch), "registered_delay_edges": 1,
                                 "compatible_decoded_source_indices": [item["golden_index"] for item in matching_decodes],
                                 "exact_ftq_occurrence_match_proven": False})
        previous_launch = current_launch

    for _ in range(_cycle_limit("TB_MBTB_COMPARE_PROGRAM_MAX_CYCLES", 16000)):
        env.step(1)
        observe_decoded_source()
        sample_cycle()
        if trace.cursor >= len(trace.entries) and not stopped_input:
            # The trace is a fixed architectural prefix. Do not decode a new
            # unbound indirect CFI after that prefix while draining real writes.
            env.backend_model.set_can_accept(0)
            stopped_input = True
        if stopped_input and equal_cycles and change_cycles and not pending_events:
            break
    evidence = {
        **program, "registration_identity": list(source_identity),
        "trace_cursor": trace.cursor, "trace_entries": len(trace.entries),
        "decoded_sources": list(decoded_observations.values()),
        "semantic_equal_cycles": equal_cycles, "semantic_change_cycles": change_cycles,
        "observations": observations, "signal_paths": actual_paths,
        "synthetic_resolve": False, "canonical_promotion": False,
        "native_write_pipeline_delay_edges": 1,
        "decoded_binding_scope": "architectural source and full target; native train interface has no FTQ occurrence ID",
        "exact_ftq_occurrence_match_proven": False,
    }
    env._emit_event("bpu.mbtb_write_compare_architectural_program", evidence)
    recorder.risk_observations.append({"risk": "mbtb_write_buffer_compare_bits_semantics", **evidence})
    assert trace.cursor == len(trace.entries), {"reason": "architectural far JALR program did not complete", "evidence": evidence, "backend": env.backend_model.get_stats()}
    assert equal_cycles, {"reason": "real same-source/same-target JALR did not reach clean semantic-equal hitWritten", "evidence": evidence}
    assert change_cycles, {"reason": "real same-source lower-target change did not reach compareBits update", "evidence": evidence}
    assert not pending_events, {"reason": "last dirty update was not observed across the edge", "events": pending_events}
    assert not env.monitor.get_errors(), env.monitor.get_errors()
    assert not env.get_errors(), env.get_errors()

@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_btb_lower_target_diff_semantics_canary(env) -> None:
    payload, trace, program = build_bpu_target_change_program(_BASE)
    env.load_program(payload, _BASE)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    assert not env.monitor.get_errors()
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)
    env.set_bp_ctrl_enable(ittage_enable=0)
    env.backend_model.set_golden_trace(trace)
    recorder = env.functional_coverage
    assert recorder is not None and recorder.env is env
    reader = lambda name: recorder._try_read_dut_signal(env.dut, name)
    equal_cycles, diff_cycles, high_only_cycles, suppressed_cycles, observations = [], [], [], [], []
    actual_paths = {}
    for _ in range(_cycle_limit("TB_BTB_COMPARE_PROGRAM_MAX_CYCLES", 12000)):
        env.step(1)
        sample = sample_bpu_target_diff_cycle(reader)
        if (sample.target_only_candidate and sample.s3_start_pc == program["source_pc"]
                and sample.s3_branch_type == 3 and sample.s3_ras_action == 0
                and sample.target_source == "btb"):
            assert sample.s3_override == sample.expected_override, {
                "reason": "registered S3/S2 BTB target-only override equation mismatch",
                "cycle": int(env.current_cycle), "sample": sample,
            }
            actual_paths.update(sample.signal_paths)
            observation = {"cycle": int(env.current_cycle),
                           "trace_cursor": trace.cursor, "sample": vars(sample)}
            observations.append(observation)
            kind = sample.target_comparison_kind
            if kind == "equal":
                assert not sample.full_target_diff
                equal_cycles.append(int(env.current_cycle))
            elif kind == "suppressed":
                suppressed_cycles.append(int(env.current_cycle))
            elif kind == "high_only":
                high_only_cycles.append(int(env.current_cycle))
            else:
                diff_cycles.append(int(env.current_cycle))
        if trace.cursor >= len(trace.entries):
            break
    env._emit_event("bpu.target_comparison_architectural_program", {
        **program, "trace_cursor": trace.cursor, "trace_entries": len(trace.entries),
        "equal_cycles": equal_cycles, "diff_cycles": diff_cycles,
        "high_only_cycles": high_only_cycles, "suppressed_cycles": suppressed_cycles,
        "observations": observations,
        "signal_paths": actual_paths, "synthetic_resolve": False,
        "canonical_promotion": False,
    })
    assert trace.cursor == len(trace.entries), {"reason": "architectural JALR program did not complete",
        "cursor": trace.cursor, "entries": len(trace.entries), "backend": env.backend_model.get_stats()}
    assert len(equal_cycles) >= 2, {"reason": "repeated real JALR did not yield repeatable target-equal BTB evidence",
        "program": program, "equal_cycles": equal_cycles, "suppressed_cycles": suppressed_cycles,
        "observations": observations[-16:]}
    assert diff_cycles, {"reason": "architectural lower-target change did not reach a target-only BTB mismatch",
        "program": program, "observations": observations[-16:]}
    assert not env.monitor.get_errors(), env.monitor.get_errors()
    assert not env.get_errors(), env.get_errors()
