"""Directed dual-fetch refill scenarios, including their original behavior checks."""

from __future__ import annotations

from env.runtime.frontend_init import (
    initialize_frontend,
)

from .mainpipe_support import (
    completed_dual_transactions as _completed_dual_transactions,
    register_mainpipe_observer as _register_mainpipe_observer,
    run_until as _run_until,
)

from tests.py.icache.directed_dut.init_plans import (
    make_dual_refill_init_config,
)


def run_trained_dual_refill(env) -> None:
    samples = _register_mainpipe_observer(env)
    initialize_frontend(env, make_dual_refill_init_config(env.config, seed=9508))
    assert _run_until(
        env,
        lambda: any(
            (
                item["pattern"] == "hit_hit"
                for item in _completed_dual_transactions(samples)
            )
        ),
        max_cycles=2000,
    ), {
        "reason": "trained loop never issued and delivered two resident blocks",
        "patterns": _completed_dual_transactions(samples)[-16:],
        "dual_valid_cycles": sum(
            (
                sample["s1_valid"] == 1 and sample["s1_req1_valid"] == 1
                for sample in samples
            )
        ),
        "dual_fire_cycles": sum(
            (
                sample["s1_fire"] == 1 and sample["s1_req1_valid"] == 1
                for sample in samples
            )
        ),
        "icache": env.icache_agent.get_stats(),
        "monitor_observations": len(env.monitor.observations),
        "branch_checker": env.branch_checker.get_stats(),
    }
    hit_hit = next(
        (
            item
            for item in reversed(_completed_dual_transactions(samples))
            if item["pattern"] == "hit_hit"
        )
    )
    assert hit_hit["req0_addr"] != hit_hit["req1_addr"]
    assert hit_hit["registered_exact"] is True
    split = len(samples)
    fencei = getattr(env.dut, "io_fencei", None)
    assert fencei is not None, {"missing_dut_signal": "io_fencei"}
    fencei.value = 1
    env.step(1)
    fencei.value = 0
    assert _run_until(
        env,
        lambda: any(
            (
                item["pattern"] == "miss_miss" and item["registered_exact"]
                for item in _completed_dual_transactions(samples[split:])
            )
        ),
        max_cycles=2000,
    ), {
        "reason": "fence.i did not create a completed dual miss/refill transaction",
        "patterns": _completed_dual_transactions(samples[split:])[-16:],
        "icache": env.icache_agent.get_stats(),
    }
    miss_miss = next(
        (
            item
            for item in _completed_dual_transactions(samples[split:])
            if item["pattern"] == "miss_miss" and item["registered_exact"]
        )
    )
    assert int(miss_miss["fire_cycle"]) >= max(
        (cycle for cycle in miss_miss["registered_cycles"] if cycle is not None)
    )
    assert not env.monitor.get_errors()


def run_asymmetric_dual_refill(env, expected_pattern, evict_req) -> None:
    samples = _register_mainpipe_observer(env)
    initialize_frontend(
        env, make_dual_refill_init_config(env.config, seed=9508 + int(evict_req))
    )
    assert _run_until(
        env,
        lambda: any(
            (
                item["pattern"] == "hit_hit"
                and item["req0_addr"] & ~63 != item["req1_addr"] & ~63
                for item in _completed_dual_transactions(samples)
            )
        ),
        max_cycles=2000,
    ), {
        "reason": "no cross-line resident dual transaction was trained",
        "patterns": _completed_dual_transactions(samples)[-16:],
    }
    resident = next(
        (
            item
            for item in reversed(_completed_dual_transactions(samples))
            if item["pattern"] == "hit_hit"
            and item["req0_addr"] & ~63 != item["req1_addr"] & ~63
        )
    )
    req_addrs = (int(resident["req0_addr"]), int(resident["req1_addr"]))
    fault_line = req_addrs[int(evict_req)] & ~63
    split = len(samples)
    split_cycle = int(samples[-1]["cycle"])
    env.icache_agent.inject_response_fault_at(fault_line, corrupt=1)
    fencei = getattr(env.dut, "io_fencei", None)
    assert fencei is not None, {"missing_dut_signal": "io_fencei"}
    fencei.value = 1
    env.step(1)
    fencei.value = 0
    assert _run_until(
        env,
        lambda: any(
            (
                int(record["address"]) == fault_line and int(record["corrupt"]) == 1
                for record in env.icache_agent.get_stats().get("response_records", [])
            )
        ),
        max_cycles=4000,
    ), {
        "reason": "faulted post-fence line never returned",
        "fault_line": hex(fault_line),
        "icache": env.icache_agent.get_stats(),
    }
    clean_line = req_addrs[1 - int(evict_req)] & ~63
    assert any(
        (
            int(record["cycle"]) >= split_cycle
            and int(record["address"]) == clean_line
            and (int(record["corrupt"]) == 0)
            and (int(record["denied"]) == 0)
            for record in env.icache_agent.get_stats().get("response_records", [])
        )
    ), {
        "reason": "the companion line did not complete a clean refill",
        "clean_line": hex(clean_line),
        "icache": env.icache_agent.get_stats(),
    }
    assert _run_until(
        env, lambda: int(env.monitor.exception_mark_count) > 0, max_cycles=1000
    ), {"reason": "fault-suppressed refill did not reach the IFU exception path"}
    assert _run_until(
        env,
        lambda: any(
            (
                item["pattern"] == expected_pattern and item["registered_exact"]
                for item in _completed_dual_transactions(samples[split:])
            )
        ),
        max_cycles=4000,
    ), {
        "reason": "fault-suppressed post-fence refill did not produce the target dual pattern",
        "expected_pattern": expected_pattern,
        "fault_line": hex(fault_line),
        "req_addrs": [hex(addr) for addr in req_addrs],
        "patterns": _completed_dual_transactions(samples[split:])[-16:],
        "icache": env.icache_agent.get_stats(),
    }
    result = next(
        (
            item
            for item in _completed_dual_transactions(samples[split:])
            if item["pattern"] == expected_pattern and item["registered_exact"]
        )
    )
    assert int(result["fire_cycle"]) >= max(
        (cycle for cycle in result["registered_cycles"] if cycle is not None)
    )
    recorder = getattr(env, "functional_coverage", None)
    assert recorder is not None
    detail = recorder.hit_detail_by_bin_id("BIN-887")
    assert detail is not None
    evidence = detail["evidence"][-1]
    assert tuple(evidence["source_valids"]) == (1, 1)
    assert tuple(evidence["source_tags"]) == tuple(evidence["wb_tags"])
    assert len(evidence["source_tags"]) == 2
    assert not env.monitor.get_errors()
