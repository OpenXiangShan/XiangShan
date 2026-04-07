from __future__ import annotations

import logging
from collections import deque
from dataclasses import dataclass
from typing import Callable, Dict, Optional

from ..memory_model import MemoryModel
from ..signal_utils import get_sig, set_sig


@dataclass
class _PendingData:
    data: int
    ready_cycle: int


class UncacheAgent:
    def __init__(self, memory: MemoryModel) -> None:
        self.logger = logging.getLogger("env.agents.uncache")
        self.memory = memory
        self.dut = None
        self.latency = 2
        self.mmio_latency = 4
        self.pending = deque()
        self.event_sink: Optional[Callable[[Dict], None]] = None
        self.req_count = 0
        self.resp_count = 0

    def bind(self, dut) -> None:
        self.dut = dut

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
        if self.dut is None:
            return
        set_sig(self.dut, "auto_inner_instrUncache_client_out_a_ready", 1)
        if get_sig(self.dut, "auto_inner_instrUncache_client_out_a_valid", 0) == 1:
            addr = get_sig(self.dut, "auto_inner_instrUncache_client_out_a_bits_address", 0)
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

        set_sig(self.dut, "auto_inner_instrUncache_client_out_d_valid", 0)
        if self.pending and cycle >= self.pending[0].ready_cycle:
            item = self.pending.popleft()
            set_sig(self.dut, "auto_inner_instrUncache_client_out_d_valid", 1)
            set_sig(self.dut, "auto_inner_instrUncache_client_out_d_bits_source", 0)
            set_sig(self.dut, "auto_inner_instrUncache_client_out_d_bits_data", item.data)
            set_sig(self.dut, "auto_inner_instrUncache_client_out_d_bits_denied", 0)
            set_sig(self.dut, "auto_inner_instrUncache_client_out_d_bits_corrupt", 0)
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
