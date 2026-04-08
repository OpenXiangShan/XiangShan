from __future__ import annotations

from toffee import Bundle, Signal


class UncacheBundle(Bundle):
    SIGNAL_BINDINGS = {
        "a_ready": "auto_inner_instrUncache_client_out_a_ready",
        "a_valid": "auto_inner_instrUncache_client_out_a_valid",
        "a_bits_address": "auto_inner_instrUncache_client_out_a_bits_address",
        "d_valid": "auto_inner_instrUncache_client_out_d_valid",
        "d_bits_source": "auto_inner_instrUncache_client_out_d_bits_source",
        "d_bits_denied": "auto_inner_instrUncache_client_out_d_bits_denied",
        "d_bits_data": "auto_inner_instrUncache_client_out_d_bits_data",
        "d_bits_corrupt": "auto_inner_instrUncache_client_out_d_bits_corrupt",
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
