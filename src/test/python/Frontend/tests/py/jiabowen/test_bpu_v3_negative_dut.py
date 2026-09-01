from __future__ import annotations

import os

import pytest

from env.support.bpu_v3_contract import (
    PREFETCH_DEPTH,
    BpuV3SignalUnavailable,
    sample_bpu_v3_cycle,
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
