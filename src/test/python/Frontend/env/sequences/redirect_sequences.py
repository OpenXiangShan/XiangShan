from __future__ import annotations

from dataclasses import dataclass

from ..transactions import PcSequenceExpectation, RedirectTxn


def _recent_pcs(env, limit: int = 16) -> list[int]:
    recent = getattr(env.monitor, "recent_pcs", None)
    if callable(recent):
        return [int(pc) for pc in recent(limit=int(limit))]
    observations = getattr(env.monitor, "observations", [])
    return [int(obs.pc) for obs in observations[-int(limit) :]]


def _advance_pc_matches(env, expected_pcs, max_cycles: int) -> bool:
    observations = getattr(env.monitor, "observations", [])
    idx = 0
    cursor = 0

    def _consume() -> int:
        nonlocal idx, cursor
        while cursor < len(observations) and idx < len(expected_pcs):
            if int(observations[cursor].pc) == int(expected_pcs[idx]):
                idx += 1
            cursor += 1
        return idx

    if _consume() >= len(expected_pcs):
        return True

    for _ in range(max(0, int(max_cycles))):
        env.step(1)
        if _consume() >= len(expected_pcs):
            return True
    return False


@dataclass(frozen=True)
class InjectRedirectSequence:
    txn: RedirectTxn
    redirect_delay_cycles: int = 0

    def run(self, env) -> bool:
        env.backend_model.inject_redirect(
            self.txn.target_pc,
            self.txn.reason,
            delay_cycles=int(self.redirect_delay_cycles),
        )
        if self.txn.target_pc in _recent_pcs(env, limit=16):
            return True
        for _ in range(max(0, int(self.txn.max_cycles))):
            env.step(1)
            if self.txn.target_pc in _recent_pcs(env, limit=16):
                return True
        return False


@dataclass(frozen=True)
class CheckPcSequence:
    expectation: PcSequenceExpectation

    def run(self, env) -> bool:
        if not self.expectation.expected_pcs:
            return True
        return _advance_pc_matches(
            env,
            self.expectation.expected_pcs,
            max_cycles=int(self.expectation.max_cycles),
        )
