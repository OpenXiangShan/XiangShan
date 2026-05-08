from __future__ import annotations

from toffee import Bundle, Signal


class ICacheBundle(Bundle):
    SIGNAL_BINDINGS = {
        "a_ready": "icacheMemPort_0_a_ready",
        "a_valid": "icacheMemPort_0_a_valid",
        "a_bits_source": "icacheMemPort_0_a_bits_source",
        "a_bits_address": "icacheMemPort_0_a_bits_address",
        "d_valid": "icacheMemPort_0_d_valid",
        "d_bits_opcode": "icacheMemPort_0_d_bits_opcode",
        "d_bits_source": "icacheMemPort_0_d_bits_source",
        "d_bits_denied": "icacheMemPort_0_d_bits_denied",
        "d_bits_data": "icacheMemPort_0_d_bits_data",
        "d_bits_corrupt": "icacheMemPort_0_d_bits_corrupt",
    }

    a_ready = Signal()
    a_valid = Signal()
    a_bits_source = Signal()
    a_bits_address = Signal()
    d_valid = Signal()
    d_bits_opcode = Signal()
    d_bits_source = Signal()
    d_bits_denied = Signal()
    d_bits_data = Signal()
    d_bits_corrupt = Signal()

    def drive_idle(self) -> None:
        self.a_ready.value = 0
        self.d_valid.value = 0
        self.d_bits_opcode.value = 0
        self.d_bits_source.value = 0
        self.d_bits_denied.value = 0
        self.d_bits_data.value = 0
        self.d_bits_corrupt.value = 0
