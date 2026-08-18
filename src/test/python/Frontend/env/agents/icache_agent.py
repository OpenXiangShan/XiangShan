from __future__ import annotations

import logging
import random
from collections import deque
from dataclasses import dataclass
from typing import Callable, Dict, Optional

from ..bundles import ICacheBundle
from ..model.memory_model import MemoryModel


@dataclass
class _ICachePending:
    source: int
    addr: int
    beat0: int
    beat1: int
    ready_cycle: int
    denied: int = 0
    corrupt: int = 0
    beat_idx: int = 0


class ICacheAgent:
    def __init__(self, memory: MemoryModel) -> None:
        self.logger = logging.getLogger("env.agents.icache")
        self.memory = memory
        self.interface = None
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
        self.corrupt_resp_count = 0
        self.denied_resp_count = 0
        self.request_records = []
        self.response_records = []
        self._next_response_faults = deque()
        self._response_faults_by_addr: Dict[int, deque] = {}

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
        if not isinstance(target, ICacheBundle):
            raise TypeError(f"ICacheAgent.bind requires an icache interface, got {type(target).__name__}")
        self.interface = target

    def reset(self) -> None:
        self.pending.clear()
        if self.interface is None:
            return
        self._write(self.interface.a_ready, 0)
        self._write(self.interface.d_valid, 0)
        self._write(self.interface.d_bits_opcode, 0)
        self._write(self.interface.d_bits_size, 0)
        self._write(self.interface.d_bits_source, 0)
        self._write(self.interface.d_bits_denied, 0)
        self._write(self.interface.d_bits_data, 0)
        self._write(self.interface.d_bits_corrupt, 0)

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

    @staticmethod
    def _cacheline_addr(addr: int) -> int:
        return int(addr) & ~0x3F

    @staticmethod
    def _normalize_fault(*, denied: int = 0, corrupt: int = 0) -> Dict[str, int]:
        denied_value = 1 if int(denied) else 0
        # A denied AccessAckData beat must also be corrupt according to TileLink.
        corrupt_value = 1 if int(corrupt) or denied_value else 0
        return {"denied": denied_value, "corrupt": corrupt_value}

    def inject_next_response_fault(self, *, denied: int = 0, corrupt: int = 0) -> None:
        """Fault the next accepted cache-line request on every returned beat."""
        self._next_response_faults.append(
            self._normalize_fault(denied=denied, corrupt=corrupt)
        )

    def inject_response_fault_at(self, addr: int, *, denied: int = 0, corrupt: int = 0) -> None:
        """Fault the next response for one 64-byte-aligned cache line."""
        line_addr = self._cacheline_addr(addr)
        queue = self._response_faults_by_addr.setdefault(line_addr, deque())
        queue.append(self._normalize_fault(denied=denied, corrupt=corrupt))

    def _take_response_fault(self, addr: int) -> Dict[str, int]:
        line_addr = self._cacheline_addr(addr)
        queue = self._response_faults_by_addr.get(line_addr)
        if queue:
            fault = queue.popleft()
            if not queue:
                self._response_faults_by_addr.pop(line_addr, None)
            return fault
        if self._next_response_faults:
            return self._next_response_faults.popleft()
        return {"denied": 0, "corrupt": 0}

    def _handle_request(self, cycle: int) -> None:
        assert self.interface is not None
        self._write(self.interface.a_ready, 1)
        if self._read(self.interface.a_valid, 0) != 1:
            return
        source = self._read(self.interface.a_bits_source, 0)
        addr = self._read(self.interface.a_bits_address, 0)
        beat0, beat1 = self.memory.read_cacheline(addr, line_bytes=64)
        is_miss = self.rng.random() < self.miss_rate
        latency = self.miss_latency if is_miss else self.hit_latency
        fault = self._take_response_fault(addr)
        self.pending.append(
            _ICachePending(
                source=source,
                addr=self._cacheline_addr(addr),
                beat0=beat0,
                beat1=beat1,
                ready_cycle=cycle + latency,
                denied=int(fault["denied"]),
                corrupt=int(fault["corrupt"]),
            )
        )
        self.request_records.append(
            {
                "cycle": int(cycle),
                "source": int(source),
                "address": self._cacheline_addr(addr),
                "latency": int(latency),
                "miss": bool(is_miss),
                "denied": int(fault["denied"]),
                "corrupt": int(fault["corrupt"]),
            }
        )
        self._emit(
            cycle,
            "handshake.icache_a",
            {
                "source": int(source),
                "address": int(addr),
                "latency": int(latency),
                "miss": bool(is_miss),
                "denied": int(fault["denied"]),
                "corrupt": int(fault["corrupt"]),
            },
            level="DEBUG",
        )
        self.req_count += 1
        if is_miss:
            self.miss_count += 1
        self.max_pending_depth = max(self.max_pending_depth, len(self.pending))

    def _drive_response(self, cycle: int) -> None:
        assert self.interface is not None
        self._write(self.interface.d_valid, 0)
        if not self.pending:
            return
        top = self.pending[0]
        if cycle < top.ready_cycle:
            return
        data = top.beat0 if top.beat_idx == 0 else top.beat1
        self._write(self.interface.d_valid, 1)
        self._write(self.interface.d_bits_opcode, 1)
        self._write(self.interface.d_bits_size, 6)
        self._write(self.interface.d_bits_source, top.source)
        self._write(self.interface.d_bits_data, data)
        self._write(self.interface.d_bits_denied, top.denied)
        self._write(self.interface.d_bits_corrupt, top.corrupt)

        self.resp_beat_count += 1
        sent_beat_idx = int(top.beat_idx)
        self.response_records.append(
            {
                "cycle": int(cycle),
                "source": int(top.source),
                "address": int(top.addr),
                "beat_idx": sent_beat_idx,
                "denied": int(top.denied),
                "corrupt": int(top.corrupt),
            }
        )
        if top.beat_idx == 1:
            self.resp_line_count += 1
            self.denied_resp_count += int(top.denied)
            self.corrupt_resp_count += int(top.corrupt)
            self.pending.popleft()
        else:
            top.beat_idx = 1
        self._emit(
            cycle,
            "handshake.icache_d",
            {
                "source": int(top.source),
                "address": int(top.addr),
                "beat_idx": sent_beat_idx,
                "ready_cycle": int(top.ready_cycle),
                "denied": int(top.denied),
                "corrupt": int(top.corrupt),
            },
            level="DEBUG",
        )

    def on_clock_edge(self, cycle: int) -> None:
        if self.interface is None:
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
            "corrupt_resp_count": self.corrupt_resp_count,
            "denied_resp_count": self.denied_resp_count,
            "request_records": [dict(record) for record in self.request_records],
            "response_records": [dict(record) for record in self.response_records],
        }


__all__ = ["ICacheAgent"]
