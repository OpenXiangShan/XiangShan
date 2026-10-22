from __future__ import annotations

from typing import Protocol

from toffee import Agent, driver_method

from memblock_toffee.bundle import ControlBundle
from memblock_toffee.datadef import FlushCommand
from memblock_toffee.timing import CycleTracker
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
            async with self.tracker.reservation():
                await self.bundle.step()
        self.bundle.reset.value = 0
        for _ in range(16):
            async with self.tracker.reservation():
                await self.bundle.step()
            if not self.bundle.backend_reset.value:
                return
        raise AssertionError("internal reset did not deassert within 16 cycles")

    @driver_method()
    async def prepare_l2_flush(self, command: FlushCommand) -> None:
        self.bundle.l2_flush_enable.value = int(command.enable)
        self.bundle.l2_flush_done.value = int(command.done)
        self.timing.refresh_comb()

    @driver_method()
    async def advance_cycle(self) -> None:
        await self.bundle.step()
