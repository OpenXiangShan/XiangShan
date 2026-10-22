from __future__ import annotations

from memblock_toffee.agent import ControlDriverAgent
from memblock_toffee.datadef import FlushCommand
from memblock_toffee.model import L2FlushModel
from memblock_toffee.monitor import L2FlushMonitor
from memblock_toffee.scoreboard import L2FlushScoreboard
from memblock_toffee.timing import CycleTracker
from memblock_toffee.uvm import Sequence, Sequencer


class L2FlushSequence(Sequence[FlushCommand]):
    def __init__(self, patterns: tuple[tuple[bool, bool], ...]) -> None:
        self.patterns = patterns

    async def body(self, sequencer: Sequencer[FlushCommand]) -> None:
        for index, (enable, done) in enumerate(self.patterns):
            await sequencer.send(
                FlushCommand(
                    transaction_id=f"flush-command-{index}",
                    enable=enable,
                    done=done,
                )
            )


class L2FlushSequencer(Sequencer[FlushCommand]):
    def __init__(
        self,
        driver: ControlDriverAgent,
        monitor: L2FlushMonitor,
        model: L2FlushModel,
        scoreboard: L2FlushScoreboard,
        tracker: CycleTracker,
    ) -> None:
        super().__init__()
        self.driver = driver
        self.monitor = monitor
        self.model = model
        self.scoreboard = scoreboard
        self.tracker = tracker

    async def execute(self, sequence: Sequence[FlushCommand]) -> None:
        await super().run(sequence)
        for command in await self.drain():
            token = self.tracker.next_token()
            self.scoreboard.expect(
                self.model.accept(command, transaction_id=token.transaction_id)
            )
            await self.driver.prepare_l2_flush(command)
            before = self.monitor.before(token)
            await self.driver.advance_cycle(token)
            self.scoreboard.observe(self.monitor.after(token, before))
