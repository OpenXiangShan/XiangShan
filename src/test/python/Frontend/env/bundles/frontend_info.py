from __future__ import annotations

from toffee import Bundle, Signal


class FrontendInfoBundle(Bundle):
    io_frontendInfo_ibufFull = Signal()
