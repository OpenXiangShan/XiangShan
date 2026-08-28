"""Real-DUT coverage for the same-entry FTQ first-mispredict training mask."""

from __future__ import annotations

import os

import pytest

from env.core.transactions import ProgramImage
from env.model.backend_state import ResolveEntry
from env.sequences import LoadProgramSequence

_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BASE = 0x8000_0000
_NOP = 0x0000_0013
_BLOCK_INSTRS = 16


def _cacheable_nops() -> bytes:
    return b"".join(_NOP.to_bytes(4, "little") for _ in range(_BLOCK_INSTRS))


def _wait_for_live_ftq_identity(env, max_cycles: int = 1000) -> tuple[int, int]:
    for _ in range(int(max_cycles)):
        env.step(1)
        queue = env.backend_model._cfvec_queue
        if queue:
            return int(queue[-1].ftq_flag), int(queue[-1].ftq_value)
    raise AssertionError("cacheable fetch did not expose a live FTQ identity")


def _inject_same_entry_resolves(env, ftq_flag: int, ftq_value: int) -> None:
    """Queue backend-port stimulus; the FTQ RTL still aggregates and masks it."""
    cycle = int(env.backend_model.current_cycle)
    for offset, mispredict in ((1, True), (5, False), (9, False)):
        env.backend_model._pending_resolves.append(
            ResolveEntry(
                ready_cycle=cycle,
                inst_pc=_BASE + offset * 2,
                pc=_BASE,
                target=_BASE + offset * 2 + 4,
                taken=False,
                mispredict=bool(mispredict),
                ftq_flag=int(ftq_flag),
                ftq_value=int(ftq_value),
                ftq_offset=int(offset),
                branch_type=1,
                ras_action=0,
                queued_cycle=cycle,
            )
        )


@pytest.mark.funcov_bins("BIN-958")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_ifu_ftq_training_mask_same_entry_first_mispredict(env) -> None:
    LoadProgramSequence(
        image=ProgramImage(payload=_cacheable_nops(), base_addr=_BASE),
        step_cycles=0,
    ).run(env)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)

    ftq_flag, ftq_value = _wait_for_live_ftq_identity(env)
    _inject_same_entry_resolves(env, ftq_flag, ftq_value)

    for _ in range(32):
        if env.functional_coverage.key_hit(
            "ifu_v3_pipeline_owner_model", "owner_leaf_060"
        ):
            break
        env.step(1)

    assert env.functional_coverage.key_hit(
        "ifu_v3_pipeline_owner_model", "owner_leaf_060"
    ), {
        "backend": env.backend_model.get_stats(),
        "branch_checker": env.branch_checker.get_stats(),
        "injected_ftq": (ftq_flag, ftq_value),
        "monitor_errors": env.monitor.get_errors(),
    }
    assert not env.monitor.get_errors()
