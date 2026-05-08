from __future__ import annotations

from toffee import Bundle, Signal


class UncacheBundle(Bundle):
    SIGNAL_BINDINGS = {
        "a_ready": "uncachePort_0_a_ready",
        "a_valid": "uncachePort_0_a_valid",
        "a_bits_address": "uncachePort_0_a_bits_address",
        "d_valid": "uncachePort_0_d_valid",
        "d_bits_source": "uncachePort_0_d_bits_source",
        "d_bits_denied": "uncachePort_0_d_bits_denied",
        "d_bits_data": "uncachePort_0_d_bits_data",
        "d_bits_corrupt": "uncachePort_0_d_bits_corrupt",
    }

    a_ready = Signal()
    a_valid = Signal()
    a_bits_address = Signal()
    d_valid = Signal()
    d_bits_source = Signal()
    d_bits_denied = Signal()
    d_bits_data = Signal()
    d_bits_corrupt = Signal()

    def drive_idle(self) -> None:
        self.a_ready.value = 0
        self.d_valid.value = 0
        self.d_bits_source.value = 0
        self.d_bits_denied.value = 0
        self.d_bits_data.value = 0
        self.d_bits_corrupt.value = 0
