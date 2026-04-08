from __future__ import annotations

from toffee import Bundle, Signal


class ClockResetBundle(Bundle):
    clock = Signal()
    reset = Signal()
    io_reset_vector_addr = Signal()
    io_fencei = Signal()

    def drive_idle(self, reset_vector_addr: int = 0) -> None:
        self.reset.value = 1
        self.clock.value = 0
        self.io_reset_vector_addr.value = int(reset_vector_addr)
        self.io_fencei.value = 0
