from __future__ import annotations

from toffee import Bundle, Signal


class ICacheControlBundle(Bundle):
    SIGNAL_BINDINGS = {
        "a_ready": "auto_inner_icache_ctrlUnitOpt_in_a_ready",
        "a_valid": "auto_inner_icache_ctrlUnitOpt_in_a_valid",
        "a_bits_opcode": "auto_inner_icache_ctrlUnitOpt_in_a_bits_opcode",
        "a_bits_size": "auto_inner_icache_ctrlUnitOpt_in_a_bits_size",
        "a_bits_source": "auto_inner_icache_ctrlUnitOpt_in_a_bits_source",
        "a_bits_address": "auto_inner_icache_ctrlUnitOpt_in_a_bits_address",
        "a_bits_mask": "auto_inner_icache_ctrlUnitOpt_in_a_bits_mask",
        "a_bits_data": "auto_inner_icache_ctrlUnitOpt_in_a_bits_data",
        "d_ready": "auto_inner_icache_ctrlUnitOpt_in_d_ready",
        "d_valid": "auto_inner_icache_ctrlUnitOpt_in_d_valid",
        "d_bits_opcode": "auto_inner_icache_ctrlUnitOpt_in_d_bits_opcode",
        "d_bits_size": "auto_inner_icache_ctrlUnitOpt_in_d_bits_size",
        "d_bits_source": "auto_inner_icache_ctrlUnitOpt_in_d_bits_source",
        "d_bits_data": "auto_inner_icache_ctrlUnitOpt_in_d_bits_data",
    }

    a_ready = Signal()
    a_valid = Signal()
    a_bits_opcode = Signal()
    a_bits_size = Signal()
    a_bits_source = Signal()
    a_bits_address = Signal()
    a_bits_mask = Signal()
    a_bits_data = Signal()
    d_ready = Signal()
    d_valid = Signal()
    d_bits_opcode = Signal()
    d_bits_size = Signal()
    d_bits_source = Signal()
    d_bits_data = Signal()

    def drive_idle(self) -> None:
        self.a_valid.value = 0
        self.a_bits_opcode.value = 0
        self.a_bits_size.value = 0
        self.a_bits_source.value = 0
        self.a_bits_address.value = 0
        self.a_bits_mask.value = 0
        self.a_bits_data.value = 0
        self.d_ready.value = 0
