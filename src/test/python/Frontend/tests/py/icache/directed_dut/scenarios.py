"""Reusable execution flows and behavior checks for directed ICache tests."""

from __future__ import annotations

import hashlib
import logging

from env.core.transactions import CommitTarget
from env.runtime.frontend_init import (
    LoadProgram,
    initialize_frontend,
    run_initialization_steps,
)
from env.sequences import RunUntilCommitSequence
from env.support import record_scenario, scenario_rng

from .programs import (
    NOP,
    encode_beq as _beq,
    encode_jal as _jal,
    instructions_to_bytes as _instructions_to_bytes,
    program_image as _program_image,
)
from .support import (
    completed_dual_transactions as _completed_dual_transactions,
    make_dual_refill_init_config,
    randomize_resolve_delays,
    register_mainpipe_observer as _register_mainpipe_observer,
    run_until as _run_until,
)

logger = logging.getLogger(__name__)


# Shared randomized branch execution.


def run_multi_branch_random_positions(env):
    """Multiple JAL/BEQ at stride-spaced random positions; backward JAL loop at end.

    Program layout:
        [0, stride, 2*stride, ...]  — each randomly JAL+4 or BEQ+4 (skip 1 NOP)
        [prog_size - 1]             — JAL back to BASE (infinite loop)
        everywhere else             — NOP

    Randomized: prog_size, stride, branch type at each slot, resolve delays.
    """
    scenario_key = "zhaoxinran/multi-branch/random-positions"
    base_seed, seed, rng = scenario_rng(scenario_key)
    prog_size = rng.choice([64, 96, 128])
    stride = rng.randint(2, 5)
    min_d, max_d = randomize_resolve_delays(rng, env)
    prog = [NOP] * prog_size
    prog[prog_size - 1] = _jal(0, -(prog_size - 1) * 4)
    n_jal = 0
    n_beq = 0
    positions = []
    for pos in range(0, prog_size // 2, stride):
        if rng.random() < 0.5:
            prog[pos] = _jal(0, 4)
            n_jal += 1
        else:
            prog[pos] = _beq(0, 0, 4)
            n_beq += 1
        positions.append(pos)
    n_placed = n_jal + n_beq
    target_commits = max(n_placed * 3, 10)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "prog_size": prog_size,
            "stride": stride,
            "positions": positions,
            "n_jal": n_jal,
            "n_beq": n_beq,
            "resolve_delay": [min_d, max_d],
            "target_commits": target_commits,
            "program_sha256": hashlib.sha256(_instructions_to_bytes(prog)).hexdigest(),
        },
    )
    logger.info(
        "seed=%d prog_size=%d stride=%d positions=%s n_jal=%d n_beq=%d resolve_delay=[%d,%d] target_commits=%d",
        seed,
        prog_size,
        stride,
        positions,
        n_jal,
        n_beq,
        min_d,
        max_d,
        target_commits,
    )
    run_initialization_steps(
        env, (LoadProgram(image=_program_image(prog), step_cycles=1),)
    )
    commits = RunUntilCommitSequence(
        target=CommitTarget(target_count=target_commits, max_cycles=5000)
    ).run(env)
    stats = env.branch_checker.get_stats()
    logger.info("commits=%d stats=%s", commits, stats)
    assert commits >= target_commits
    assert stats["by_type"]["jump"] >= 1
    if n_beq > 0:
        assert stats["by_type"]["branch"] >= 1


# Shared dual-fetch refill execution.


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
