from __future__ import annotations

import os

import pytest

from env.core.transactions import ProgramImage
from env.model.backend_state import ResolveEntry
from env.sequences import LoadProgramSequence
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


def _cycle_limit(name: str, default: int) -> int:
    raw = os.getenv(str(name), "").strip()
    if not raw:
        return int(default)
    value = int(raw, 0)
    assert value > 0, f"{name} must be positive"
    return int(value)


def _wait_for_live_ftq_identity(env) -> dict[str, int]:
    for _ in range(_cycle_limit("TB_MBTB_IDENTITY_MAX_CYCLES", 1000)):
        env.step(1)
        queue = env.backend_model._cfvec_queue
        if not queue:
            continue
        entry = queue[-1]
        packed_ftq = (int(entry.ftq_flag) << 6) | (int(entry.ftq_value) & 0x3F)
        start_pc = env.backend_model._ftq_start_pc_cache.get(packed_ftq)
        assert start_pc is not None, {
            "reason": "plain-JALR FTQ identity had no authoritative startPc",
            "ftq": (int(entry.ftq_flag), int(entry.ftq_value)),
            "backend": env.backend_model.get_stats(),
        }
        return {
            "inst_pc": int(entry.pc),
            "start_pc": int(start_pc),
            "ftq_flag": int(entry.ftq_flag),
            "ftq_value": int(entry.ftq_value),
            "ftq_offset": int(entry.ftq_offset),
            "is_rvc": int(entry.is_rvc),
        }
    raise AssertionError(
        {
            "reason": "DUT did not expose a live FTQ identity for backend resolve",
            "backend": env.backend_model.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _queue_plain_jalr_mispredict(env, identity: dict[str, int], *, target: int) -> None:
    cycle = int(env.backend_model.current_cycle)
    env.backend_model._pending_resolves.append(
        ResolveEntry(
            ready_cycle=cycle,
            inst_pc=int(identity["inst_pc"]),
            pc=int(identity["start_pc"]),
            target=int(target),
            taken=True,
            mispredict=True,
            ftq_flag=int(identity["ftq_flag"]),
            ftq_value=int(identity["ftq_value"]),
            ftq_offset=int(identity["ftq_offset"]),
            branch_type=3,
            ras_action=0,
            queued_cycle=cycle,
            is_rvc=bool(identity["is_rvc"]),
            queue_index=None,
        )
    )


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
            sample = sample_bpu_v3_cycle(reader)
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
            sample = sample_prefetch_depth_cycle(reader)
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
        sample = sample_prefetch_depth_cycle(reader)
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
        sample = sample_prefetch_depth_cycle(reader)
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
    LoadProgramSequence(
        image=ProgramImage(payload=_NOP.to_bytes(4, "little") * 16, base_addr=_BASE),
        step_cycles=0,
    ).run(env)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)

    identity = _wait_for_live_ftq_identity(env)
    assert identity["start_pc"] == _BASE, identity
    assert identity["is_rvc"] == 0, identity
    env.backend_model.set_can_accept(0)

    recorder = env.functional_coverage
    assert recorder is not None and recorder.env is env
    reader = lambda name: recorder._try_read_dut_signal(env.dut, name)

    target = _BASE + 0x1000
    changed_target = target + 4
    semantic_equal_cycles: list[int] = []
    semantic_change_cycles: list[int] = []
    actual_paths: dict[str, str] = {}
    pending_events = []

    def sample_cycle() -> None:
        nonlocal pending_events, actual_paths
        for event in pending_events:
            dirty, dirty_path = read_mbtb_write_buffer_dirty(reader, event)
            expected_dirty = int(event.semantic_changed)
            assert dirty == expected_dirty, {
                "reason": "MBTB hitWritten dirty update did not match compareBits semantics",
                "cycle": int(env.current_cycle),
                "expected_dirty": expected_dirty,
                "observed_dirty": int(dirty),
                "dirty_path": dirty_path,
                "event": event,
            }
            actual_paths[f"dirty_after_{dirty_path}"] = dirty_path
        pending_events = []

        try:
            events = sample_mbtb_write_buffer_events(reader)
        except BpuV3SignalUnavailable as exc:
            pytest.fail(str(exc), pytrace=False)

        unique_events = {}
        for event in events:
            key = (
                event.align_bank,
                event.internal_bank,
                event.row,
                event.entry,
            )
            if key in unique_events:
                unique_events[key] = None
            else:
                unique_events[key] = event
        for event in unique_events.values():
            if event is None:
                continue
            assert event.dirty == 0, {
                "reason": "hitWritten selected an already dirty MBTB entry",
                "cycle": int(env.current_cycle),
                "event": event,
            }
            assert event.identity_matches, {
                "reason": "MBTB hitWritten identity did not match set/tag/position",
                "cycle": int(env.current_cycle),
                "event": event,
            }
            pending_events.append(event)
            actual_paths.update(event.signal_paths)
            target = (
                semantic_change_cycles
                if event.semantic_changed
                else semantic_equal_cycles
            )
            target.append(int(env.current_cycle))

    _queue_plain_jalr_mispredict(env, identity, target=target)
    for _ in range(_cycle_limit("TB_MBTB_INITIAL_DRAIN_CYCLES", 32)):
        env.step(1)
        sample_cycle()
    assert not semantic_equal_cycles and not semantic_change_cycles, {
        "reason": "initial plain-JALR allocation unexpectedly hit a clean MBTB entry",
        "identity": identity,
    }

    _queue_plain_jalr_mispredict(env, identity, target=target)
    for _ in range(_cycle_limit("TB_MBTB_EQUAL_MAX_CYCLES", 128)):
        env.step(1)
        sample_cycle()
        if semantic_equal_cycles:
            break
    assert semantic_equal_cycles, {
        "reason": "same-PC plain-JALR retrain did not reach a semantic-equal clean MBTB hitWritten entry",
        "identity": identity,
        "backend": env.backend_model.get_stats(),
        "icache": env.icache_agent.get_stats(),
    }

    if pending_events:
        env.step(1)
        sample_cycle()

    _queue_plain_jalr_mispredict(env, identity, target=changed_target)
    for _ in range(_cycle_limit("TB_MBTB_CHANGE_MAX_CYCLES", 128)):
        env.step(1)
        sample_cycle()
        if semantic_change_cycles:
            break
    assert semantic_change_cycles, {
        "reason": "same-PC plain-JALR lower-target change did not reach MBTB compareBits update",
        "identity": identity,
        "equal_cycles": semantic_equal_cycles,
        "backend": env.backend_model.get_stats(),
    }

    if pending_events:
        env.step(1)
        sample_cycle()

    recorder.risk_observations.append(
        {
            "cycle": int(env.current_cycle),
            "risk": "mbtb_write_buffer_compare_bits_semantics",
            "identity": dict(identity),
            "semantic_equal_target": int(target),
            "semantic_change_target": int(changed_target),
            "semantic_equal_cycles": list(semantic_equal_cycles),
            "semantic_change_cycles": list(semantic_change_cycles),
            "signal_paths": actual_paths,
        }
    )
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_btb_lower_target_diff_semantics_canary(env) -> None:
    env.load_program(_NOP.to_bytes(4, "little") * 4096, _BASE)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)
    identity = _wait_for_live_ftq_identity(env)
    env.set_bp_ctrl_enable(ittage_enable=0)

    recorder = env.functional_coverage
    assert recorder is not None and recorder.env is env
    reader = lambda name: recorder._try_read_dut_signal(env.dut, name)

    btb_target_equal_cycles: list[int] = []
    btb_target_diff_cycles: list[int] = []
    btb_high_only_cycles: list[int] = []
    actual_paths: dict[str, str] = {}

    def sample_cycle() -> None:
        try:
            sample = sample_bpu_target_diff_cycle(reader)
        except BpuV3SignalUnavailable as exc:
            pytest.fail(str(exc), pytrace=False)
        if not sample.target_only_candidate:
            return
        assert sample.s3_override == int(sample.target_diff), {
            "reason": "BPU target-only override did not match predictor-specific target comparison",
            "cycle": int(env.current_cycle),
            "sample": sample,
        }
        actual_paths.update(sample.signal_paths)
        if sample.target_source != "btb":
            return
        if sample.btb_high_only_diff:
            btb_high_only_cycles.append(int(env.current_cycle))
        elif sample.target_diff:
            btb_target_diff_cycles.append(int(env.current_cycle))
        else:
            btb_target_equal_cycles.append(int(env.current_cycle))

    def run_phase(max_cycles: int) -> None:
        for _ in range(int(max_cycles)):
            env.step(1)
            sample_cycle()

    target = _BASE + 0x1000
    lower_changed_target = target + 4
    # Same-startPc BTBs reconstruct upper target bits; only lower/carry changes
    # are legal dynamic mismatches. The high-only rule is checked structurally.

    _queue_plain_jalr_mispredict(env, identity, target=target)
    run_phase(_cycle_limit("TB_BTB_COMPARE_TRAIN_CYCLES", 64))
    env.backend_model.inject_redirect(
        identity["start_pc"],
        "btb_target_equal_probe",
        delay_cycles=0,
    )
    run_phase(_cycle_limit("TB_BTB_COMPARE_EQUAL_CYCLES", 128))

    assert btb_target_equal_cycles, {
        "reason": "trained OtherIndirect did not produce a target-equal BTB comparison",
        "identity": identity,
        "backend": env.backend_model.get_stats(),
    }

    equal_count = len(btb_target_equal_cycles)
    _queue_plain_jalr_mispredict(env, identity, target=target)
    run_phase(_cycle_limit("TB_BTB_COMPARE_TRAIN_CYCLES", 64))
    env.backend_model.inject_redirect(
        identity["start_pc"],
        "btb_target_equal_retrain_probe",
        delay_cycles=0,
    )
    run_phase(_cycle_limit("TB_BTB_COMPARE_EQUAL_CYCLES", 128))
    assert len(btb_target_equal_cycles) > equal_count, {
        "reason": "same-target retrain did not preserve a repeatable BTB comparison",
        "identity": identity,
        "equal_cycles": btb_target_equal_cycles,
        "backend": env.backend_model.get_stats(),
    }
    assert not btb_target_diff_cycles, {
        "reason": "BTB target diff appeared before the lower target changed",
        "diff_cycles": btb_target_diff_cycles,
    }

    _queue_plain_jalr_mispredict(env, identity, target=lower_changed_target)
    run_phase(_cycle_limit("TB_BTB_COMPARE_CHANGE_TRAIN_CYCLES", 128))
    env.backend_model.inject_redirect(
        identity["start_pc"],
        "btb_target_lower_change_probe",
        delay_cycles=0,
    )
    run_phase(_cycle_limit("TB_BTB_COMPARE_LOWER_CHANGE_CYCLES", 128))

    assert btb_target_diff_cycles, {
        "reason": "BTB lower-target change did not exercise s3 override",
        "identity": identity,
        "high_only_cycles": btb_high_only_cycles,
        "backend": env.backend_model.get_stats(),
    }

    recorder.risk_observations.append(
        {
            "cycle": int(env.current_cycle),
            "risk": "btb_lower_target_diff_semantics",
            "identity": dict(identity),
            "target": int(target),
            "lower_changed_target": int(lower_changed_target),
            "btb_target_equal_cycles": list(btb_target_equal_cycles),
            "btb_target_diff_cycles": list(btb_target_diff_cycles),
            "btb_high_only_cycles": list(btb_high_only_cycles),
            "btb_high_only_dynamic_status": "not_legally_constructible_same_start_pc",
            "signal_paths": actual_paths,
        }
    )
    assert not env.monitor.get_errors()
