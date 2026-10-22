from __future__ import annotations

from memblock_toffee.datadef import FlushExpectation, FlushObservation
from memblock_toffee.errors import FailureContext
from memblock_toffee.transactions import OutstandingTable, TransactionKey
from memblock_toffee.uvm import Scoreboard


class L2FlushScoreboard(Scoreboard[FlushExpectation, FlushObservation]):
    """Match flush observations by transaction identity, not arrival order."""

    def __init__(self) -> None:
        self._table = OutstandingTable[
            TransactionKey,
            FlushExpectation,
            FlushObservation | None,
        ]("l2_flush_scoreboard")
        self.issues = self._table.issues

    def expect(self, expected: FlushExpectation) -> None:
        self._table.expect(
            TransactionKey("l2_flush", expected.transaction_id),
            expected,
        )

    def observe(self, actual: FlushObservation) -> None:
        transaction_id = actual.transaction_id
        key = TransactionKey("l2_flush", transaction_id)
        if not self._table.observe(key, actual, actual.cycle):
            return
        expected = self._table.expected(key)
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
        self._table.finish()
