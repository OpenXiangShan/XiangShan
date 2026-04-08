from __future__ import annotations

from toffee import Bundle, Signal


class BackendFromFtqBundle(Bundle):
    io_backend_fromFtq_wen = Signal()
    io_backend_fromFtq_ftqIdx = Signal()
    io_backend_fromFtq_startPc_addr = Signal()
