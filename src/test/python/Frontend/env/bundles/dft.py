from __future__ import annotations

from toffee import Bundle, Signal


class DFTControlBundle(Bundle):
    io_dft_reset_lgc_rst_n = Signal()
    io_dft_reset_mode = Signal()
    io_dft_reset_scan_mode = Signal()
    io_dft_cgen = Signal()
    io_dft_ram_hold = Signal()
    io_dft_ram_bypass = Signal()
    io_dft_ram_bp_clken = Signal()
    io_dft_ram_aux_clk = Signal()
    io_dft_ram_aux_ckbp = Signal()
    io_dft_ram_mcp_hold = Signal()

    def drive_idle(self) -> None:
        self.io_dft_reset_lgc_rst_n.value = 1
        self.io_dft_reset_mode.value = 0
        self.io_dft_reset_scan_mode.value = 0
        self.io_dft_cgen.value = 0
        self.io_dft_ram_hold.value = 0
        self.io_dft_ram_bypass.value = 0
        self.io_dft_ram_bp_clken.value = 0
        self.io_dft_ram_aux_clk.value = 0
        self.io_dft_ram_aux_ckbp.value = 0
        self.io_dft_ram_mcp_hold.value = 0
