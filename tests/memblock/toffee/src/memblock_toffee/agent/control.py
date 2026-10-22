from __future__ import annotations

from typing import Protocol

from toffee import Agent, driver_method

from memblock_toffee.bundle import ControlBundle
from memblock_toffee.datadef import FlushCommand
from memblock_toffee.timing import CycleToken, CycleTracker
from memblock_toffee.uvm import Driver


class CombinationalRefresh(Protocol):
    def refresh_comb(self) -> object: ...


class ControlDriverAgent(Agent, Driver[FlushCommand, None]):
    """Drive reset and the logical top-level flush interface."""

    def __init__(
        self,
        bundle: ControlBundle,
        timing: CombinationalRefresh,
        tracker: CycleTracker,
    ) -> None:
        super().__init__(bundle)
        self.bundle = bundle
        self.timing = timing
        self.tracker = tracker

    @driver_method()
    async def reset(self, cycles: int = 8) -> None:
        if cycles < 1:
            raise ValueError("reset cycles must be positive")
        self.bundle.reset.value = 1
        for _ in range(cycles):
            token = self.tracker.next_token()
            await self.bundle.step()
            self.tracker.commit(token)
        self.bundle.reset.value = 0
        for _ in range(16):
            token = self.tracker.next_token()
            await self.bundle.step()
            self.tracker.commit(token)
            if not self.bundle.backend_reset.value:
                return
        raise AssertionError("internal reset did not deassert within 16 cycles")

    @driver_method()
    async def prepare_l2_flush(self, command: FlushCommand) -> None:
        self.bundle.l2_flush_enable.value = int(command.enable)
        self.bundle.l2_flush_done.value = int(command.done)
        self.timing.refresh_comb()

    @driver_method()
    async def advance_cycle(self, token: CycleToken) -> None:
        await self.bundle.step()
        self.tracker.commit(token)
