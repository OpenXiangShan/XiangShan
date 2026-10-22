from __future__ import annotations

from toffee import Env

from memblock_toffee.adapters import MemBlockDutAdapter
from memblock_toffee.agent import ControlDriverAgent
from memblock_toffee.bundle import ControlBundle, IdleBundle
from memblock_toffee.model import L2FlushModel
from memblock_toffee.monitor import L2FlushMonitor, QuiescenceChecker
from memblock_toffee.scoreboard import L2FlushScoreboard
from memblock_toffee.sequence import L2FlushSequencer
from memblock_toffee.timing import CycleTracker


class MemBlockEnv(Env):
    """Composes protocol agents, pure models, and scoreboards."""

    def __init__(self, adapter: MemBlockDutAdapter) -> None:
        super().__init__()
        self.adapter = adapter
        self.control_bundle = ControlBundle().bind(
            adapter,
            unconnected_signal_access=False,
        )
        self.idle_bundle = IdleBundle().bind(
            adapter,
            unconnected_signal_access=False,
        )
        self.cycle_tracker = CycleTracker()
        self.control_driver = ControlDriverAgent(
            self.control_bundle,
            adapter,
            self.cycle_tracker,
        )
        self.control_monitor = L2FlushMonitor(self.control_bundle)
        self.quiescence = QuiescenceChecker(self.idle_bundle)
        self.control_model = L2FlushModel()
        self.control_scoreboard = L2FlushScoreboard()
        self.control_sequencer = L2FlushSequencer(
            self.control_driver,
            self.control_monitor,
            self.control_model,
            self.control_scoreboard,
            self.cycle_tracker,
        )
        self.control_coverage = None
