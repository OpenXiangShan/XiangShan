from __future__ import annotations

from memblock_toffee.bundle import ControlBundle
from memblock_toffee.datadef import (
    FlushObservation,
    FlushPreSample,
    flush_cycle_transaction_id,
)
from memblock_toffee.timing import CycleToken
from memblock_toffee.uvm import Monitor


class L2FlushMonitor(Monitor[FlushObservation]):
    """Passively sample pre-edge and post-edge logical outputs."""

    def __init__(self, bundle: ControlBundle) -> None:
        self.bundle = bundle
        self.observations = 0

    def before(self, token: CycleToken) -> FlushPreSample:
        return FlushPreSample(
            token=token,
            outer_enable=bool(self.bundle.outer_l2_flush_enable.value),
            backend_done=bool(self.bundle.backend_l2_flush_done.value),
        )

    def after(
        self,
        token: CycleToken,
        before: FlushPreSample,
    ) -> FlushObservation:
        self.observations += 1
        return FlushObservation(
            transaction_id=flush_cycle_transaction_id(token.index),
            pre_outer_enable=before.outer_enable,
            pre_backend_done=before.backend_done,
            post_outer_enable=bool(self.bundle.outer_l2_flush_enable.value),
            post_backend_done=bool(self.bundle.backend_l2_flush_done.value),
            cycle=token.index,
        )
