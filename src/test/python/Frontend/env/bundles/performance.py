from __future__ import annotations

from toffee import Bundle, SignalList


class FrontendPerformanceBundle(Bundle):
    values = SignalList("io_perf_#_value", 8)
