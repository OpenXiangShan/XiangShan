from __future__ import annotations

import os
from typing import Iterable

import pytest

from env.sequences import InjectRedirectSequence, LoadProgramSequence, RunUntilCommitSequence
from env.transactions import CommitTarget, ProgramImage, RedirectTxn


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BASE = 0x80000000
_MMIO_BASE = 0x1000
_NOP = 0x00000013


def _instructions_to_bytes(instructions: Iterable[int]) -> bytes:
    buf = bytearray()
    for instr in instructions:
        buf.extend((int(instr) & 0xFFFFFFFF).to_bytes(4, "little"))
    return bytes(buf)


def _program_image(instructions: Iterable[int], base_addr: int = _BASE) -> ProgramImage:
    return ProgramImage(payload=_instructions_to_bytes(instructions), base_addr=base_addr)


def _jal(rd: int, offset: int) -> int:
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
    imm = offset & 0x1FFF
    return (
        (((imm >> 12) & 0x1) << 31)
        | (((imm >> 5) & 0x3F) << 25)
        | (rs2 << 20)
        | (rs1 << 15)
        | (0x1 << 12)
        | (((imm >> 1) & 0xF) << 8)
        | (((imm >> 11) & 0x1) << 7)
        | 0x63
    )


def _load_nop_program(env, words: int = 256, base_addr: int = _BASE) -> None:
    LoadProgramSequence(image=_program_image([_NOP] * int(words), base_addr=base_addr), step_cycles=1).run(env)


def _warmup_commits(env, target_count: int = 4, max_cycles: int = 3000) -> int:
    return RunUntilCommitSequence(
        target=CommitTarget(target_count=int(target_count), max_cycles=int(max_cycles)),
    ).run(env)


def _step_until_coverage_hits(env, expected_keys, max_cycles: int = 32) -> bool:
    keys = list(expected_keys)
    for _ in range(max(0, int(max_cycles))):
        env.step(1)
        if all(env.functional_coverage.key_hit(*key) for key in keys):
            return True
    return all(env.functional_coverage.key_hit(*key) for key in keys)


def _queue_backend_fault_redirect(env, *, target_pc: int, **fault_bits: int) -> None:
    last_pc = env.monitor.recent_pcs(1)[-1]
    env.backend_model._queue_redirect_event(
        target_pc=int(target_pc),
        reason="backend_fault",
        delay_cycles=1,
        flush_on_drive=False,
        payload_extra={
            "pc": int(last_pc),
            "ftq_flag": int(env.backend_model.commit_ptr_flag),
            "ftq_value": int(env.backend_model.commit_ptr_value),
            "ftq_offset": 0,
            **fault_bits,
        },
    )


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_baremode_seq_icache_basic_pilot(env):
    _load_nop_program(env, words=128)
    commits = _warmup_commits(env, target_count=4)

    assert commits >= 4
    assert env.functional_coverage.key_hit("reset_boot_path", "seen")
    assert env.functional_coverage.key_hit("fetch_path_type", "icache_seq")
    assert env.functional_coverage.key_hit("bpu_basic_pred_type", "seq_no_cfi")
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_baremode_mmio_uncache_redirect_pilot(env):
    _load_nop_program(env, words=128)
    commits = _warmup_commits(env, target_count=4)

    env.backend_model.inject_redirect(_MMIO_BASE, "ctrl_redirect")
    covered = _step_until_coverage_hits(
        env,
        [
            ("fetch_path_type", "mmio_uncache"),
            ("frontend_exception_type", "af"),
            ("fetch_path_x_exception", "mmio_x_af"),
        ],
        max_cycles=24,
    )

    assert commits >= 4
    assert covered is True
    assert env.functional_coverage.key_hit("fetch_path_type", "mmio_uncache")
    assert env.functional_coverage.key_hit("frontend_exception_type", "af")
    assert env.functional_coverage.key_hit("fetch_path_x_exception", "mmio_x_af")
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_baremode_backend_memvio_redirect_pilot(env):
    _load_nop_program(env, words=128)
    commits = _warmup_commits(env, target_count=4)

    redirected = InjectRedirectSequence(
        txn=RedirectTxn(
            target_pc=_BASE + 0x80,
            reason="memVio",
            max_cycles=200,
        )
    ).run(env)

    assert commits >= 4
    assert redirected is True
    assert env.functional_coverage.key_hit("redirect_type", "memVio")
    assert env.functional_coverage.key_hit("fetch_path_type", "icache_seq")
    assert env.functional_coverage.key_hit("reset_boot_path", "seen")
    assert env.functional_coverage.key_hit("bpu_basic_pred_type", "seq_no_cfi")
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_baremode_backend_interrupt_redirect_pilot(env):
    _load_nop_program(env, words=128)
    commits = _warmup_commits(env, target_count=4)

    redirected = InjectRedirectSequence(
        txn=RedirectTxn(
            target_pc=_BASE + 0xA0,
            reason="interrupt",
            max_cycles=200,
        )
    ).run(env)

    assert commits >= 4
    assert redirected is True
    assert env.functional_coverage.key_hit("redirect_type", "interrupt")
    assert env.functional_coverage.key_hit("fetch_path_type", "icache_seq")
    assert env.functional_coverage.key_hit("reset_boot_path", "seen")
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_baremode_backend_ipf_redirect_pilot(env):
    _load_nop_program(env, words=128)
    commits = _warmup_commits(env, target_count=4)

    _queue_backend_fault_redirect(env, target_pc=_BASE + 0xC0, backend_ipf=1)
    env.step(30)

    assert commits >= 4
    assert env.functional_coverage.key_hit("frontend_exception_type", "pf")
    assert env.functional_coverage.key_hit("fetch_path_x_exception", "icache_x_pf")
    assert env.functional_coverage.key_hit("fetch_path_type", "icache_seq")
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_baremode_backend_iaf_redirect_pilot(env):
    _load_nop_program(env, words=128)
    commits = _warmup_commits(env, target_count=4)

    _queue_backend_fault_redirect(env, target_pc=_BASE + 0xE0, backend_iaf=1)
    env.step(30)

    assert commits >= 4
    assert env.functional_coverage.key_hit("frontend_exception_type", "af")
    assert env.functional_coverage.key_hit("fetch_path_type", "icache_seq")
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_baremode_backend_short_backpressure_pilot(env):
    _load_nop_program(env, words=512)
    commits_before = _warmup_commits(env, target_count=4)

    env.backend_model.set_can_accept(0)
    env.step(16)
    env.backend_model.set_can_accept(1)

    commits_after = _warmup_commits(env, target_count=4)

    assert commits_before >= 4
    assert commits_after >= 4
    assert env.functional_coverage.key_hit("backend_accept_mode", "all_block_short")
    assert env.functional_coverage.key_hit("fetch_path_type", "icache_seq")
    assert env.functional_coverage.key_hit("reset_boot_path", "seen")
    assert env.functional_coverage.key_hit("bpu_basic_pred_type", "seq_no_cfi")
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_baremode_direct_jmp_coverage_pilot(env):
    prog = [_NOP] * 64
    prog[0] = _jal(0, 8)

    LoadProgramSequence(image=_program_image(prog), step_cycles=1).run(env)
    commits = _warmup_commits(env, target_count=4)

    assert commits >= 4
    assert env.functional_coverage.key_hit("bpu_basic_pred_type", "direct_jmp")
    assert env.functional_coverage.key_hit("fetch_path_type", "icache_seq")
    assert env.functional_coverage.key_hit("reset_boot_path", "seen")
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_baremode_cond_nt_coverage_pilot(env):
    prog = [_NOP] * 64
    prog[0] = _bne(0, 0, 8)

    LoadProgramSequence(image=_program_image(prog), step_cycles=1).run(env)
    commits = _warmup_commits(env, target_count=4)

    assert commits >= 4
    assert env.functional_coverage.key_hit("bpu_basic_pred_type", "cond_nt")
    assert env.functional_coverage.key_hit("fetch_path_type", "icache_seq")
    assert env.functional_coverage.key_hit("reset_boot_path", "seen")
    assert not env.monitor.get_errors()
