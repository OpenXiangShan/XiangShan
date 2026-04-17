import os

import pytest

from env.sequences import CheckPcSequence, InjectRedirectSequence, LoadProgramSequence, RunUntilCommitSequence
from env.transactions import CommitTarget, PcSequenceExpectation, ProgramImage, RedirectTxn


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BASE = 0x80000000
_PHYS_BASE = 0x90000000
_SV39_MODE = 8
_NOP = 0x00000013


def _instructions_to_bytes(instructions):
    buf = bytearray()
    for instr in instructions:
        buf.extend((int(instr) & 0xFFFFFFFF).to_bytes(4, "little"))
    return bytes(buf)


def _program_image(instructions, base_addr=_BASE) -> ProgramImage:
    return ProgramImage(payload=_instructions_to_bytes(instructions), base_addr=base_addr)


def _configure_sv39_identity(env, virt_base: int, phys_base: int) -> None:
    env.page_table.clear()
    env.page_table.set_mode("sv39")
    env.csr_ctrl_if.io_tlbCsr_priv_imode.value = 1
    env.csr_ctrl_if.io_tlbCsr_satp_mode.value = _SV39_MODE
    env.csr_ctrl_if.io_tlbCsr_vsatp_mode.value = 0
    env.page_table.map_page(virt_base >> 12, phys_base >> 12, v=1, x=1, r=1)
    env.initialize(reset_vector=virt_base, bare_mode=False, reset_cycles=20)
    env.dut.io_tlbCsr_satp_changed.value = 1
    env.step(1)
    env.dut.io_tlbCsr_satp_changed.value = 0


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_bin004_backend_ctrl_redirect_pilot(env):
    """Pilot migration of BIN-004 on the new Frontend mainline.

    Intent:
      - build a minimal, manager-readable closure example on the migrated env
      - prove the new env can absorb the old coverage-closure methodology

    This testcase mirrors the earlier standalone Frontend BT trial:
      1. load a tiny program whose first instruction is JAL +8
      2. wait until the DUT reaches a stable commit state
      3. inject one backend ctrl redirect
      4. require redirect target PC to appear in monitor observations
      5. require no monitor errors
    """
    prog = [
        0x0080006F,  # jal x0, +8
        _NOP,
        _NOP,
        _NOP,
        _NOP,
        _NOP,
        _NOP,
        _NOP,
    ]

    LoadProgramSequence(image=_program_image(prog), step_cycles=1).run(env)
    commits = RunUntilCommitSequence(
        target=CommitTarget(target_count=6, max_cycles=4000),
    ).run(env)

    redirected = InjectRedirectSequence(
        txn=RedirectTxn(
            target_pc=_BASE + 0x20,
            reason="ctrl_redirect",
            max_cycles=200,
        )
    ).run(env)

    assert commits >= 6
    assert redirected is True
    assert CheckPcSequence(
        expectation=PcSequenceExpectation(
            expected_pcs=(_BASE, _BASE + 0x20),
            max_cycles=400,
        )
    ).run(env)
    assert env.branch_checker.get_stats()["by_type"]["jump"] >= 1
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_bin012_itlb_miss_walk_refill_pilot(env):
    """Pilot migration of BIN-012 on the new Frontend mainline.

    Goal:
      - prove the new mainline env can drive a minimal sv39 miss -> PTW -> refill style path
      - keep the translation-aware monitor aligned with the current sv39/PTW flow
    """
    prog = [_NOP] * 64

    _configure_sv39_identity(env, _BASE, _PHYS_BASE)
    LoadProgramSequence(image=_program_image(prog, base_addr=_PHYS_BASE), step_cycles=1).run(env)
    commits = RunUntilCommitSequence(
        target=CommitTarget(target_count=6, max_cycles=6000),
    ).run(env)

    ptw_stats = env.ptw_agent.get_stats()
    errors = env.monitor.get_errors()

    assert commits >= 6
    assert ptw_stats["req_count"] >= 1
    assert ptw_stats["resp_count"] >= 1
    assert env.functional_coverage.key_hit("itlb_result_type", "miss")
    assert env.functional_coverage.key_hit("ptw_resp_type", "leaf_pte")
    assert env.functional_coverage.key_hit("itlb_ptw_flow", "miss_walk_refill")
    assert not errors
