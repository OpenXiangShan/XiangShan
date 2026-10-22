from __future__ import annotations

from memblock_toffee.datadef import FlushExpectation, FlushObservation
from memblock_toffee.errors import FailureContext, IssueCollector
from memblock_toffee.uvm import Scoreboard


class L2FlushScoreboard(Scoreboard[FlushExpectation, FlushObservation]):
    """Match flush observations by transaction identity, not arrival order."""

    def __init__(self) -> None:
        self._expected: dict[str, FlushExpectation] = {}
        self._observed: set[str] = set()
        self.issues = IssueCollector()

    def expect(self, expected: FlushExpectation) -> None:
        if expected.transaction_id in self._expected:
            raise ValueError(f"duplicate expectation for {expected.transaction_id}")
        self._expected[expected.transaction_id] = expected

    def observe(self, actual: FlushObservation) -> None:
        transaction_id = actual.transaction_id
        if transaction_id in self._observed:
            self.issues.add(
                FailureContext(
                    component="l2_flush_scoreboard",
                    phase="observe",
                    cycle=actual.cycle,
                    transaction_id=transaction_id,
                ),
                "duplicate observation",
            )
            return
        self._observed.add(transaction_id)
        expected = self._expected.get(transaction_id)
        if expected is None:
            self.issues.add(
                FailureContext(
                    component="l2_flush_scoreboard",
                    phase="observe",
                    cycle=actual.cycle,
                    transaction_id=transaction_id,
                ),
                "unexpected observation",
            )
            return
        if actual.pre_outer_enable != expected.pre_outer_enable:
            self.issues.add(
                FailureContext(
                    component="l2_flush_scoreboard",
                    phase="observe",
                    cycle=actual.cycle,
                    transaction_id=transaction_id,
                ),
                "pre-edge outer enable expected "
                f"{expected.pre_outer_enable}, got {actual.pre_outer_enable}",
            )
        if actual.pre_backend_done != expected.pre_backend_done:
            self.issues.add(
                FailureContext(
                    component="l2_flush_scoreboard",
                    phase="observe",
                    cycle=actual.cycle,
                    transaction_id=transaction_id,
                ),
                "pre-edge backend done expected "
                f"{expected.pre_backend_done}, got {actual.pre_backend_done}",
            )
        if actual.post_outer_enable != expected.post_outer_enable:
            self.issues.add(
                FailureContext(
                    component="l2_flush_scoreboard",
                    phase="observe",
                    cycle=actual.cycle,
                    transaction_id=transaction_id,
                ),
                "post-edge outer enable expected "
                f"{expected.post_outer_enable}, got {actual.post_outer_enable}",
            )
        if actual.post_backend_done != expected.post_backend_done:
            self.issues.add(
                FailureContext(
                    component="l2_flush_scoreboard",
                    phase="observe",
                    cycle=actual.cycle,
                    transaction_id=transaction_id,
                ),
                "post-edge backend done expected "
                f"{expected.post_backend_done}, got {actual.post_backend_done}",
            )

    def finish(self) -> None:
        missing = sorted(set(self._expected) - self._observed)
        for transaction_id in missing:
            self.issues.add(
                FailureContext(
                    component="l2_flush_scoreboard",
                    phase="finish",
                    transaction_id=transaction_id,
                ),
                "missing observation",
            )
        self.issues.raise_if_any()
