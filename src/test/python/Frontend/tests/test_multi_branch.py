import os
import random
import logging
from typing import Iterable

import pytest

from env.sequences import CheckPcSequence, LoadProgramSequence, RunUntilCommitSequence
from env.transactions import CommitTarget, PcSequenceExpectation, ProgramImage


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
logger = logging.getLogger(__name__)

NOP = 0x00000013
BASE = 0x80000000


def _jal(rd: int, offset: int) -> int:
    """Encode JAL rd, offset (PC-relative, must be even)."""
    imm = offset & 0x1FFFFF
    return (
        (((imm >> 20) & 0x1) << 31)
        | (((imm >> 1) & 0x3FF) << 21)
        | (((imm >> 11) & 0x1) << 20)
        | (((imm >> 12) & 0xFF) << 12)
        | (rd << 7)
        | 0x6F
    )


def _beq(rs1: int, rs2: int, offset: int) -> int:
    """Encode BEQ rs1, rs2, offset (PC-relative, must be even)."""
    imm = offset & 0x1FFF
    return (
        (((imm >> 12) & 0x1) << 31)
        | (((imm >> 5) & 0x3F) << 25)
        | (rs2 << 20)
        | (rs1 << 15)
        | (((imm >> 1) & 0xF) << 8)
        | (((imm >> 11) & 0x1) << 7)
        | 0x63
    )


def _bne(rs1: int, rs2: int, offset: int) -> int:
    """Encode BNE rs1, rs2, offset (PC-relative, must be even)."""
    imm = offset & 0x1FFF
    return (
        (((imm >> 12) & 0x1) << 31)
        | (((imm >> 5) & 0x3F) << 25)
        | (rs2 << 20)
        | (rs1 << 15)
        | (0x1 << 12)                    # funct3 = 1 (BNE)
        | (((imm >> 1) & 0xF) << 8)
        | (((imm >> 11) & 0x1) << 7)
        | 0x63
    )


def _make_program(size: int = 128) -> list:
    return [NOP] * size


def _instructions_to_bytes(instructions: Iterable[int]) -> bytes:
    buf = bytearray()
    for instr in instructions:
        buf.extend((int(instr) & 0xFFFFFFFF).to_bytes(4, "little"))
    return bytes(buf)


def _program_image(instructions: Iterable[int], base_addr: int = BASE) -> ProgramImage:
    return ProgramImage(payload=_instructions_to_bytes(instructions), base_addr=base_addr)


def _rand_resolve_delays(rng: random.Random, env) -> tuple:
    """Randomize backend resolve delays and return (min_d, max_d)."""
    min_d = rng.randint(1, 6)
    max_d = rng.randint(min_d + 1, min_d + 12)
    env.backend_model.resolve_min_delay = min_d
    env.backend_model.resolve_max_delay = max_d
    return min_d, max_d


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_jal_forward_jump_observes_target_pc(env):
    prog = _make_program(64)
    prog[0] = _jal(0, 8)

    LoadProgramSequence(image=_program_image(prog), step_cycles=1).run(env)

    assert CheckPcSequence(
        expectation=PcSequenceExpectation(expected_pcs=(BASE, BASE + 8), max_cycles=600),
    ).run(env)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_jal_resolve_drains_pending_queue(env):
    env.backend_model.resolve_min_delay = 1
    env.backend_model.resolve_max_delay = 3

    prog = _make_program(64)
    prog[8] = _jal(0, 8)

    LoadProgramSequence(image=_program_image(prog), step_cycles=1).run(env)
    commits = RunUntilCommitSequence(target=CommitTarget(target_count=6, max_cycles=3000)).run(env)

    assert commits >= 6
    assert len(env.backend_model._pending_resolves) == 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_multi_branch_random_positions(env):
    """Multiple JAL/BEQ at stride-spaced random positions; backward JAL loop at end.

    Program layout:
        [0, stride, 2*stride, ...]  — each randomly JAL+4 or BEQ+4 (skip 1 NOP)
        [prog_size - 1]             — JAL back to BASE (infinite loop)
        everywhere else             — NOP

    Randomized: prog_size, stride, branch type at each slot, resolve delays.
    """
    seed = random.randint(0, 0xFFFFFFFF)
    rng = random.Random(seed)

    prog_size = rng.choice([64, 96, 128])
    stride = rng.randint(2, 5)
    min_d, max_d = _rand_resolve_delays(rng, env)

    prog = [NOP] * prog_size
    prog[prog_size - 1] = _jal(0, -(prog_size - 1) * 4)   # loop back to BASE

    n_jal = 0
    n_beq = 0
    positions = []
    for pos in range(0, prog_size // 2, stride):
        if rng.random() < 0.5:
            prog[pos] = _jal(0, 4)          # unconditional: skip 1 NOP ahead
            n_jal += 1
        else:
            prog[pos] = _beq(0, 0, 4)       # x0 == x0, always taken; skip 1 NOP ahead
            n_beq += 1
        positions.append(pos)

    n_placed = n_jal + n_beq
    target_commits = max(n_placed * 3, 10)

    logger.info(
        "seed=%d prog_size=%d stride=%d positions=%s n_jal=%d n_beq=%d "
        "resolve_delay=[%d,%d] target_commits=%d",
        seed, prog_size, stride, positions, n_jal, n_beq, min_d, max_d, target_commits,
    )

    LoadProgramSequence(image=_program_image(prog), step_cycles=1).run(env)
    commits = RunUntilCommitSequence(
        target=CommitTarget(target_count=target_commits, max_cycles=5000),
    ).run(env)
    stats = env.branch_checker.get_stats()

    logger.info("commits=%d stats=%s", commits, stats)

    assert commits >= target_commits
    # The backward-loop JAL is always on the critical path, so jump count must be >= 1.
    assert stats["by_type"]["jump"] >= 1
    if n_beq > 0:
        assert stats["by_type"]["branch"] >= 1


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_multi_cfi_per_ftq_entry(env):
    """Multiple CFI instructions per FTQ entry, distinct start_addr per entry.

    Program layout (BLOCK_SIZE=16 instructions = 64-byte fetch block):
        block 0 .. block N-2 : beqs_per_block BEQ instructions spaced INNER_STRIDE
                               apart.  The first beqs_per_block-1 BEQs jump within
                               the same block (short forward skip) so all of them
                               are on the execution path.  The last BEQ in each
                               block jumps to the start of the NEXT block, making
                               the target cross a fetch-block boundary.
        block N-1            : all NOPs + backward JAL to a random non-first block.

    This ensures:
      - Multiple CFIs per FTQ entry (>= 2 BEQs dispatched from the same block).
      - At least one CFI target per block crosses a fetch-block boundary.
      - Each FTQ entry has a distinct start_addr (0x80000040, 0x80000080, ...).
    """
    seed = random.randint(0, 0xFFFFFFFF)
    rng = random.Random(seed)

    BLOCK_SIZE = 16       # 16 × 4 B = 64-byte fetch block
    INNER_STRIDE = 2      # instruction distance between consecutive within-block BEQs

    block_count    = rng.randint(3, 6)
    beqs_per_block = rng.randint(2, 4)
    min_d, max_d   = _rand_resolve_delays(rng, env)

    prog_size = block_count * BLOCK_SIZE
    prog = [NOP] * prog_size

    n_beq = 0
    for blk in range(block_count - 1):
        next_block_start = (blk + 1) * BLOCK_SIZE
        for k in range(beqs_per_block):
            pos = blk * BLOCK_SIZE + k * INNER_STRIDE
            if k < beqs_per_block - 1:
                # Within-block target: keeps execution in the same block so all
                # subsequent BEQs are reached.
                offset_bytes = INNER_STRIDE * 4
            else:
                # Cross-block target: jumps to the start of the next block.
                offset_bytes = (next_block_start - pos) * 4
            prog[pos] = _beq(0, 0, offset_bytes)
            n_beq += 1

    # Backward JAL to a non-BASE block boundary so steady-state loop never
    # resets to 0x80000000.
    loop_target_block = rng.randint(1, block_count - 2)
    loop_target_pos   = loop_target_block * BLOCK_SIZE
    jal_pos           = prog_size - 1
    prog[jal_pos] = _jal(0, (loop_target_pos - jal_pos) * 4)

    target_commits = max(n_beq * 3, 30)

    logger.info(
        "seed=%d block_count=%d beqs_per_block=%d n_beq=%d loop_target_block=%d "
        "resolve_delay=[%d,%d] target_commits=%d",
        seed, block_count, beqs_per_block, n_beq, loop_target_block,
        min_d, max_d, target_commits,
    )

    LoadProgramSequence(image=_program_image(prog), step_cycles=1).run(env)
    commits = RunUntilCommitSequence(
        target=CommitTarget(target_count=target_commits, max_cycles=8000),
    ).run(env)
    stats = env.branch_checker.get_stats()

    logger.info("commits=%d stats=%s", commits, stats)

    assert commits >= target_commits
    assert stats["by_type"]["jump"] >= 1
    # At least one full loop iteration worth of branches
    assert stats["by_type"]["branch"] >= (block_count - 1) * beqs_per_block


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_multi_branch_dense_loop(env):
    """Dense branch loop: BEQ every beq_stride instructions, then backward JAL.

    Program layout (example beq_stride=2, loop_size=8):
        [0]  BEQ +8   → skip to [2]
        [1]  NOP      (skipped)
        [2]  BEQ +8   → skip to [4]
        [3]  NOP      (skipped)
        [4]  BEQ +8   → skip to [6]
        [5]  NOP      (skipped)
        [6]  NOP
        [7]  JAL back to [0]

    Randomized: loop_size, beq_stride, target_commits, resolve delays.
    """
    seed = random.randint(0, 0xFFFFFFFF)
    rng = random.Random(seed)

    loop_size = rng.randint(8, 16)
    beq_stride = rng.randint(2, 3)
    target_commits = rng.randint(20, 40)
    min_d, max_d = _rand_resolve_delays(rng, env)

    prog = [NOP] * loop_size
    prog[loop_size - 1] = _jal(0, -(loop_size - 1) * 4)   # loop back to BASE

    n_beq = 0
    beq_positions = []
    pos = 0
    while pos + beq_stride < loop_size - 1:
        prog[pos] = _beq(0, 0, beq_stride * 4)    # always-taken branch over beq_stride NOPs
        n_beq += 1
        beq_positions.append(pos)
        pos += beq_stride

    logger.info(
        "seed=%d loop_size=%d beq_stride=%d beq_positions=%s n_beq=%d "
        "resolve_delay=[%d,%d] target_commits=%d",
        seed, loop_size, beq_stride, beq_positions, n_beq, min_d, max_d, target_commits,
    )

    LoadProgramSequence(image=_program_image(prog), step_cycles=1).run(env)
    commits = RunUntilCommitSequence(
        target=CommitTarget(target_count=target_commits, max_cycles=6000),
    ).run(env)
    stats = env.branch_checker.get_stats()

    logger.info("commits=%d stats=%s", commits, stats)

    assert commits >= target_commits
    assert stats["by_type"]["jump"] >= 1
    if n_beq > 0:
        assert stats["by_type"]["branch"] >= 1


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_large_loop_multi_segment(env):
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
    seed = random.randint(0, 0xFFFFFFFF)
    rng  = random.Random(seed)

    prog_size = rng.choice([1024, 1280, 1536])
    N_SEG     = rng.randint(3, 6)
    seg_len   = prog_size // N_SEG
    min_d, max_d = _rand_resolve_delays(rng, env)

    n_beq_placed = 0
    n_bne_placed = 0

    prog = [NOP] * prog_size

    for i in range(N_SEG):
        seg_start = i * seg_len
        jal_pos   = seg_start + seg_len - 1
        skip        = rng.randint(1, 3)
        beq_density = rng.uniform(0.05, 0.15)
        bne_density = rng.uniform(0.05, 0.15)

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
            prog[jal_pos] = _jal(0, (-jal_pos) * 4)

    # target_commits: prog_size//3 + N_SEG guarantees at least one complete outer
    # loop even with maximum BEQ density (skip=3 → each segment commits ~seg_len//3
    # instructions on the execution path).
    target_commits = prog_size // 3 + N_SEG

    logger.info(
        "seed=%d prog_size=%d N_SEG=%d seg_len=%d n_beq=%d n_bne=%d "
        "resolve_delay=[%d,%d] target_commits=%d",
        seed, prog_size, N_SEG, seg_len, n_beq_placed, n_bne_placed,
        min_d, max_d, target_commits,
    )

    LoadProgramSequence(image=_program_image(prog), step_cycles=1).run(env)
    commits = RunUntilCommitSequence(
        target=CommitTarget(target_count=target_commits, max_cycles=30000),
    ).run(env)
    stats = env.branch_checker.get_stats()

    logger.info("commits=%d stats=%s", commits, stats)

    assert commits >= target_commits
    assert not env.monitor.get_errors()
    assert stats["by_type"]["jump"] >= N_SEG
    if n_beq_placed > 0 or n_bne_placed > 0:
        assert stats["by_type"]["branch"] >= 1
