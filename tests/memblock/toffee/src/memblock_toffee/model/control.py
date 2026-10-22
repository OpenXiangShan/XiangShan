from __future__ import annotations

from memblock_toffee.datadef import FlushCommand, FlushExpectation
from memblock_toffee.uvm import ReferenceModel


class L2FlushModel(ReferenceModel[FlushCommand, FlushExpectation]):
    """Pin-free model of the top-level L2 flush pass-through contract."""

    def __init__(self) -> None:
        self._enabled = False
        self._previous_done = False
        self._done = False
        self._enable_transitions = 0
        self._done_transitions = 0
        self._loop_enable_transitions = 0
        self._loop_done_transitions = 0
        self._commands_seen = 0
        self._cross_bitmap = 0

    def expect(
        self,
        command: FlushCommand,
        transaction_id: str | None = None,
    ) -> FlushExpectation:
        enable_changed = command.enable != self._enabled
        done_changed = command.done != self._done
        if enable_changed:
            self._enable_transitions += 1
        if done_changed:
            self._done_transitions += 1
        if self._commands_seen < 8:
            self._loop_enable_transitions += int(enable_changed)
            self._loop_done_transitions += int(done_changed)
        self._commands_seen += 1
        self._cross_bitmap |= 1 << (
            (2 if command.enable else 0) | (1 if command.done else 0)
        )
        self._enabled = command.enable
        self._done = command.done
        expectation = FlushExpectation(
            transaction_id=transaction_id or command.transaction_id,
            pre_outer_enable=command.enable,
            pre_backend_done=self._previous_done,
            post_outer_enable=command.enable,
            post_backend_done=command.done,
        )
        self._previous_done = command.done
        return expectation

    @property
    def transitions(self) -> int:
        return self._enable_transitions + self._done_transitions

    @property
    def enable_transitions(self) -> int:
        return self._enable_transitions

    @property
    def done_transitions(self) -> int:
        return self._done_transitions

    @property
    def cross_bitmap(self) -> int:
        return self._cross_bitmap

    @property
    def loop_enable_transitions(self) -> int:
        return self._loop_enable_transitions

    @property
    def loop_done_transitions(self) -> int:
        return self._loop_done_transitions
