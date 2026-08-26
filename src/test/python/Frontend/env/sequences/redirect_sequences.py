from __future__ import annotations

from dataclasses import dataclass

from ..core.transactions import PcSequenceExpectation, RedirectTxn


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

    def inject(self, env) -> None:
        """Queue the redirect without consuming a DUT cycle."""
        if self.txn.source_pc is None:
            env.backend_model.inject_redirect(
                self.txn.target_pc,
                self.txn.reason,
            )
        else:
            env.backend_model.inject_redirect_from_cfvec(
                source_pc=int(self.txn.source_pc),
                source_ftq_flag=self.txn.source_ftq_flag,
                source_ftq_value=self.txn.source_ftq_value,
                source_ftq_offset=self.txn.source_ftq_offset,
                target_pc=int(self.txn.target_pc),
                reason=str(self.txn.reason),
                taken=int(self.txn.taken),
                level=int(self.txn.level),
                backend_igpf=int(self.txn.backend_igpf),
                backend_ipf=int(self.txn.backend_ipf),
                backend_iaf=int(self.txn.backend_iaf),
                satp_flush=int(self.txn.satp_flush),
            )

    def run(self, env) -> bool:
        self.inject(env)
        return self.wait(env)

    def wait(self, env) -> bool:
        if self.txn.target_pc in _recent_pcs(env, limit=16):
            return True
        for _ in range(max(0, int(self.txn.max_cycles))):
            env.step(1)
            if self.txn.target_pc in _recent_pcs(env, limit=16):
                return True
        return False

    def wait_for_notification(self, env) -> bool:
        """Wait until the environment observes the redirect drive event."""
        monitor = getattr(env, "monitor", None)
        start_count = int(getattr(monitor, "redirect_count", 0))
        for _ in range(max(0, int(self.txn.max_cycles))):
            if int(getattr(monitor, "redirect_count", 0)) > start_count:
                return True
            env.step(1)
        return int(getattr(monitor, "redirect_count", 0)) > start_count


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
