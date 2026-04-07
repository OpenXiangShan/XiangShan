from __future__ import annotations

import logging
import random
from collections import deque
from dataclasses import dataclass
from typing import Callable, Dict, Optional

from ..memory_model import MemoryModel
from ..signal_utils import get_sig, set_sig


@dataclass
class _ICachePending:
    source: int
    beat0: int
    beat1: int
    ready_cycle: int
    beat_idx: int = 0


class ICacheAgent:
    def __init__(self, memory: MemoryModel) -> None:
        self.logger = logging.getLogger("env.agents.icache")
        self.memory = memory
        self.dut = None
        self.hit_latency = 1
        self.miss_latency = 20
        self.miss_rate = 0.0
        self.rng = random.Random(1)
        self.pending = deque()
        self.event_sink: Optional[Callable[[Dict], None]] = None
        self.req_count = 0
        self.resp_beat_count = 0
        self.resp_line_count = 0
        self.miss_count = 0
        self.max_pending_depth = 0

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
                "source": "icache_agent",
                "cycle": int(cycle),
                "level": level,
                "payload": payload,
            }
        )

    def configure(self, hit_latency: int = 1, miss_latency: int = 20, miss_rate: float = 0.0, seed: int = 1) -> None:
        self.hit_latency = max(0, int(hit_latency))
        self.miss_latency = max(0, int(miss_latency))
        self.miss_rate = min(1.0, max(0.0, float(miss_rate)))
        self.rng.seed(int(seed))
        self.logger.info(
            "configured: hit_latency=%d miss_latency=%d miss_rate=%.4f seed=%d",
            self.hit_latency,
            self.miss_latency,
            self.miss_rate,
            int(seed),
        )

    def _handle_request(self, cycle: int) -> None:
        assert self.dut is not None
        set_sig(self.dut, "auto_inner_icache_client_out_a_ready", 1)
        if get_sig(self.dut, "auto_inner_icache_client_out_a_valid", 0) != 1:
            return
        source = get_sig(self.dut, "auto_inner_icache_client_out_a_bits_source", 0)
        addr = get_sig(self.dut, "auto_inner_icache_client_out_a_bits_address", 0)
        beat0, beat1 = self.memory.read_cacheline(addr, line_bytes=64)
        is_miss = self.rng.random() < self.miss_rate
        latency = self.miss_latency if is_miss else self.hit_latency
        self.pending.append(
            _ICachePending(
                source=source,
                beat0=beat0,
                beat1=beat1,
                ready_cycle=cycle + latency,
            )
        )
        self._emit(
            cycle,
            "handshake.icache_a",
            {
                "source": int(source),
                "address": int(addr),
                "latency": int(latency),
                "miss": bool(is_miss),
            },
            level="DEBUG",
        )
        self.req_count += 1
        if is_miss:
            self.miss_count += 1
        self.max_pending_depth = max(self.max_pending_depth, len(self.pending))

    def _drive_response(self, cycle: int) -> None:
        assert self.dut is not None
        set_sig(self.dut, "auto_inner_icache_client_out_d_valid", 0)
        if not self.pending:
            return
        top = self.pending[0]
        if cycle < top.ready_cycle:
            return
        data = top.beat0 if top.beat_idx == 0 else top.beat1
        set_sig(self.dut, "auto_inner_icache_client_out_d_valid", 1)
        set_sig(self.dut, "auto_inner_icache_client_out_d_bits_opcode", 1)
        set_sig(self.dut, "auto_inner_icache_client_out_d_bits_source", top.source)
        set_sig(self.dut, "auto_inner_icache_client_out_d_bits_data", data)
        set_sig(self.dut, "auto_inner_icache_client_out_d_bits_denied", 0)
        set_sig(self.dut, "auto_inner_icache_client_out_d_bits_corrupt", 0)

        self.resp_beat_count += 1
        sent_beat_idx = int(top.beat_idx)
        if top.beat_idx == 1:
            self.resp_line_count += 1
            self.pending.popleft()
        else:
            top.beat_idx = 1
        self._emit(
            cycle,
            "handshake.icache_d",
            {
                "source": int(top.source),
                "beat_idx": sent_beat_idx,
                "ready_cycle": int(top.ready_cycle),
            },
            level="DEBUG",
        )

    def on_clock_edge(self, cycle: int) -> None:
        if self.dut is None:
            return
        self._handle_request(cycle)
        self._drive_response(cycle)

    def get_stats(self) -> dict:
        return {
            "req_count": self.req_count,
            "resp_beat_count": self.resp_beat_count,
            "resp_line_count": self.resp_line_count,
            "miss_count": self.miss_count,
            "max_pending_depth": self.max_pending_depth,
            "pending": len(self.pending),
        }


__all__ = ["ICacheAgent"]
