from __future__ import annotations

import logging
from collections import deque
from dataclasses import dataclass
from typing import Callable, Dict, Optional

from ..bundles import UncacheBundle, bind_bundle_optional
from ..memory_model import MemoryModel


@dataclass
class _PendingData:
    data: int
    ready_cycle: int


class UncacheAgent:
    def __init__(self, memory: MemoryModel) -> None:
        self.logger = logging.getLogger("env.agents.uncache")
        self.memory = memory
        self.interface = None
        self.latency = 2
        self.mmio_latency = 4
        self.pending = deque()
        self.event_sink: Optional[Callable[[Dict], None]] = None
        self.req_count = 0
        self.resp_count = 0

    @staticmethod
    def _read(signal, default: int = 0) -> int:
        try:
            value = getattr(signal, "value", None)
            return default if value is None else int(value)
        except Exception:
            return default

    @staticmethod
    def _write(signal, value: int) -> None:
        try:
            signal.value = int(value)
        except Exception:
            return

    def bind(self, target) -> None:
        if isinstance(target, UncacheBundle):
            self.interface = target
            return
        self.interface = bind_bundle_optional(UncacheBundle, target)

    def set_event_sink(self, sink: Optional[Callable[[Dict], None]]) -> None:
        self.event_sink = sink

    def _emit(self, cycle: int, event_type: str, payload: Dict, level: str = "INFO") -> None:
        if self.event_sink is None:
            return
        self.event_sink(
            {
                "type": event_type,
                "source": "uncache_agent",
                "cycle": int(cycle),
                "level": level,
                "payload": payload,
            }
        )

    def configure(self, latency: int = 2, mmio_latency: Optional[int] = None) -> None:
        self.latency = max(0, int(latency))
        self.mmio_latency = max(0, int(mmio_latency if mmio_latency is not None else latency))
        self.logger.info(
            "configured: latency=%d mmio_latency=%d",
            self.latency,
            self.mmio_latency,
        )

    def on_clock_edge(self, cycle: int) -> None:
        if self.interface is None:
            return
        self._write(self.interface.a_ready, 1)
        if self._read(self.interface.a_valid, 0) == 1:
            addr = self._read(self.interface.a_bits_address, 0)
            base = addr & ~0x1F
            blk = self.memory.read_block(base, 32)
            data = int.from_bytes(blk, "little")
            latency = self.mmio_latency if self.memory.is_mmio(addr) else self.latency
            self.pending.append(_PendingData(data=data, ready_cycle=cycle + latency))
            self.req_count += 1
            self._emit(
                cycle,
                "handshake.uncache_a",
                {"address": int(addr), "latency": int(latency)},
                level="DEBUG",
            )

        self._write(self.interface.d_valid, 0)
        if self.pending and cycle >= self.pending[0].ready_cycle:
            item = self.pending.popleft()
            self._write(self.interface.d_valid, 1)
            self._write(self.interface.d_bits_source, 0)
            self._write(self.interface.d_bits_data, item.data)
            self._write(self.interface.d_bits_denied, 0)
            self._write(self.interface.d_bits_corrupt, 0)
            self.resp_count += 1
            self._emit(
                cycle,
                "handshake.uncache_d",
                {"response_count": int(self.resp_count)},
                level="DEBUG",
            )

    def get_stats(self) -> dict:
        return {
            "req_count": self.req_count,
            "resp_count": self.resp_count,
            "pending": len(self.pending),
        }


__all__ = ["UncacheAgent"]
