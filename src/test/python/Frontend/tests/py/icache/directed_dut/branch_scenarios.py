"""Random branch scenarios owned by directed ICache tests.

Existing scenario keys are retained to preserve derived seeds and replay records.
"""

from __future__ import annotations

import hashlib
import logging
from random import Random

from env.core.transactions import CommitTarget
from env.runtime.frontend_init import LoadProgram, run_initialization_steps
from env.sequences import RunUntilCommitSequence
from env.support import record_scenario, scenario_rng

from .programs import (
    NOP,
    encode_jal as _jal,
    encode_beq as _beq,
    encode_bne as _bne,
    instructions_to_bytes as _instructions_to_bytes,
    program_image as _program_image,
)

logger = logging.getLogger(__name__)


def _rand_resolve_delays(rng: Random, env) -> tuple:
    """Randomize backend resolve delays and return (min_d, max_d)."""
    min_d = rng.randint(1, 6)
    max_d = rng.randint(min_d + 1, min_d + 12)
    env.backend_model.resolve_min_delay = min_d
    env.backend_model.resolve_max_delay = max_d
    return (min_d, max_d)


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
    min_d, max_d = _rand_resolve_delays(rng, env)
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


def run_large_loop_multi_segment(env):
    """Large program (>=1000 instrs) split into N_SEG segments chained by JALs.

    Each segment has per-segment BEQ (always-taken) and BNE (always-not-taken)
    branches at randomized density, plus an end-of-segment JAL.  Segments 0..N-2
    end with a forward JAL to the next segment start; segment N-1 ends with a
    backward JAL to BASE, forming one large outer loop.

    Assertions:
      - commits >= target_commits (no deadlock)
      - no monitor errors
      - jump count >= N_SEG (each segment's JAL committed at least once)
      - branch count >= 1 if any BEQ/BNE was placed
    """
    scenario_key = "zhaoxinran/multi-branch/large-loop-multi-segment"
    base_seed, seed, rng = scenario_rng(scenario_key)
    prog_size = rng.choice([1024, 1280, 1536])
    N_SEG = rng.randint(3, 6)
    seg_len = prog_size // N_SEG
    min_d, max_d = _rand_resolve_delays(rng, env)
    n_beq_placed = 0
    n_bne_placed = 0
    prog = [NOP] * prog_size
    segment_parameters = []
    for i in range(N_SEG):
        seg_start = i * seg_len
        jal_pos = seg_start + seg_len - 1
        skip = rng.randint(1, 3)
        beq_density = rng.uniform(0.05, 0.15)
        bne_density = rng.uniform(0.05, 0.15)
        segment_parameters.append(
            {"skip": skip, "beq_density": beq_density, "bne_density": bne_density}
        )
        for s in range(seg_start, seg_start + seg_len - 1):
            pos_within_seg = s - seg_start
            effective_skip = min(skip, seg_len - 2 - pos_within_seg)
            if effective_skip < 1:
                pass
            elif rng.random() < beq_density:
                prog[s] = _beq(0, 0, effective_skip * 4)
                n_beq_placed += 1
            elif rng.random() < bne_density:
                prog[s] = _bne(0, 0, effective_skip * 4)
                n_bne_placed += 1
        if i < N_SEG - 1:
            prog[jal_pos] = _jal(0, ((i + 1) * seg_len - jal_pos) * 4)
        else:
            prog[jal_pos] = _jal(0, -jal_pos * 4)
    target_commits = prog_size // 3 + N_SEG
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "prog_size": prog_size,
            "segment_count": N_SEG,
            "segment_length": seg_len,
            "segments": segment_parameters,
            "n_beq": n_beq_placed,
            "n_bne": n_bne_placed,
            "resolve_delay": [min_d, max_d],
            "target_commits": target_commits,
            "program_sha256": hashlib.sha256(_instructions_to_bytes(prog)).hexdigest(),
        },
    )
    logger.info(
        "seed=%d prog_size=%d N_SEG=%d seg_len=%d n_beq=%d n_bne=%d resolve_delay=[%d,%d] target_commits=%d",
        seed,
        prog_size,
        N_SEG,
        seg_len,
        n_beq_placed,
        n_bne_placed,
        min_d,
        max_d,
        target_commits,
    )
    run_initialization_steps(
        env, (LoadProgram(image=_program_image(prog), step_cycles=1),)
    )
    commits = RunUntilCommitSequence(
        target=CommitTarget(target_count=target_commits, max_cycles=30000)
    ).run(env)
    stats = env.branch_checker.get_stats()
    logger.info("commits=%d stats=%s", commits, stats)
    assert commits >= target_commits
    assert not env.monitor.get_errors()
    assert stats["by_type"]["jump"] >= N_SEG
    if n_beq_placed > 0 or n_bne_placed > 0:
        assert stats["by_type"]["branch"] >= 1
