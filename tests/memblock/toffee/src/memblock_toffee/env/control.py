from __future__ import annotations

from dataclasses import dataclass
from typing import Any

from memblock_toffee.agent import ControlDriverAgent
from memblock_toffee.bundle import ControlBundle, IdleBundle
from memblock_toffee.datadef import FlushCommand
from memblock_toffee.model import L2FlushModel
from memblock_toffee.monitor import L2FlushMonitor, QuiescenceChecker
from memblock_toffee.registry import EnvironmentContext
from memblock_toffee.scoreboard import L2FlushScoreboard
from memblock_toffee.sequence import L2FlushSequencer
from memblock_toffee.uvm import Sequence, Sequencer


@dataclass(frozen=True, slots=True)
class ControlRunResult:
    cycle: int
    observations: int
    enable_transitions: int
    done_transitions: int
    loop_enable_transitions: int
    loop_done_transitions: int
    cross_bitmap: int


class ControlComponent:
    """Self-contained reset, flush, monitoring, and checking component."""

    def __init__(self, context: EnvironmentContext) -> None:
        self.adapter = context.adapter
        self.tracker = context.cycle_tracker
        self.bundle = ControlBundle().bind(
            context.adapter,
            unconnected_signal_access=False,
        )
        self.idle_bundle = IdleBundle().bind(
            context.adapter,
            unconnected_signal_access=False,
        )
        self.driver = ControlDriverAgent(
            self.bundle,
            context.adapter,
            self.tracker,
        )
        self.monitor = L2FlushMonitor(self.bundle)
        self.quiescence = QuiescenceChecker(self.idle_bundle)
        self.model = L2FlushModel()
        self.scoreboard = L2FlushScoreboard()
        self.sequencer = L2FlushSequencer(
            self.driver,
            self.monitor,
            self.model,
            self.scoreboard,
            self.tracker,
        )

    @property
    def agents(self) -> dict[str, object]:
        return {"control_driver": self.driver}

    @property
    def sequencers(self) -> dict[str, Sequencer[Any]]:
        return {"control": self.sequencer}

    async def reset(self, cycles: int = 8) -> None:
        await self.driver.reset(cycles)

    async def execute(self, sequence: Sequence[FlushCommand]) -> ControlRunResult:
        await self.sequencer.execute(sequence)
        self.scoreboard.finish()
        return ControlRunResult(
            cycle=self.tracker.cycle,
            observations=self.monitor.observations,
            enable_transitions=self.model.enable_transitions,
            done_transitions=self.model.done_transitions,
            loop_enable_transitions=self.model.loop_enable_transitions,
            loop_done_transitions=self.model.loop_done_transitions,
            cross_bitmap=self.model.cross_bitmap,
        )

    async def tick(self, cycles: int = 1, *, require_idle: bool = False) -> None:
        if cycles < 0:
            raise ValueError("tick count must be non-negative")
        for _ in range(cycles):
            async with self.tracker.reservation():
                await self.driver.advance_cycle()
            if require_idle:
                self.require_idle()

    def require_idle(self) -> None:
        self.quiescence.require_idle()

    def finish(self) -> None:
        self.scoreboard.finish()
