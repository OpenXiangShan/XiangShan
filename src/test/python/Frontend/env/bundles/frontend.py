from __future__ import annotations

from toffee import Bundle, Signal, SignalList


class ClockResetBundle(Bundle):
    SIGNAL_BINDINGS = {
        "io_reset_in_frontend": "io_resetInFrontend",
    }

    clock = Signal()
    reset = Signal()
    io_reset_vector_addr = Signal()
    io_fencei = Signal()
    io_reset_in_frontend = Signal()

    def drive_idle(self, reset_vector_addr: int = 0) -> None:
        self.reset.value = 1
        self.clock.value = 0
        self.io_reset_vector_addr.value = int(reset_vector_addr)
        self.io_fencei.value = 0


class SoftPrefetchBundle(Bundle):
    SIGNAL_BINDINGS = {
        "valid": "io_softPrefetch_#_valid",
        "bits_vaddr": "io_softPrefetch_#_bits_vaddr",
    }

    valid = SignalList("io_softPrefetch_#_valid", 3)
    bits_vaddr = SignalList("io_softPrefetch_#_bits_vaddr", 3)

    def drive_idle(self) -> None:
        for signals in (self.valid, self.bits_vaddr):
            for signal in signals:
                signal.value = 0


class EccErrorBundle(Bundle):
    SIGNAL_BINDINGS = {
        "error_valid": "io_error_ecc_error_valid",
        "error_bits": "io_error_ecc_error_bits",
    }

    error_valid = Signal()
    error_bits = Signal()
