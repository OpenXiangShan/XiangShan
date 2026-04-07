from __future__ import annotations

from dataclasses import dataclass

from ..transactions import PcSequenceExpectation, RedirectTxn


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
        for _ in range(max(0, int(self.txn.max_cycles))):
            env.step(1)
            if self.txn.target_pc in env.monitor.recent_pcs(limit=16):
                return True
        return False


@dataclass(frozen=True)
class CheckPcSequence:
    expectation: PcSequenceExpectation

    def run(self, env) -> bool:
        if not self.expectation.expected_pcs:
            return True

        idx = 0
        cursor = 0
        for _ in range(max(0, int(self.expectation.max_cycles))):
            env.step(1)
            observations = env.monitor.observations
            while cursor < len(observations) and idx < len(self.expectation.expected_pcs):
                if observations[cursor].pc == self.expectation.expected_pcs[idx]:
                    idx += 1
                cursor += 1
            if idx >= len(self.expectation.expected_pcs):
                return True
        return False
