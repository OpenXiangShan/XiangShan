from __future__ import annotations

import logging
from collections import deque
from dataclasses import dataclass
from typing import Callable, Dict, Optional

from ..bundles import UncacheBundle
from ..memory_model import MemoryModel


@dataclass
class _PendingData:
    data: int
    ready_cycle: int
    addr: int
    denied: int = 0
    corrupt: int = 0


class UncacheAgent:
    def __init__(self, memory: MemoryModel) -> None:
        self.logger = logging.getLogger("env.agents.uncache")
        self.memory = memory
        self.interface = None
        self.latency = 2
        self.mmio_latency = 4
        self.pending = deque()
        self.next_response_faults = deque()
        self.response_faults_by_addr = {}
        self.event_sink: Optional[Callable[[Dict], None]] = None
        self.a_ready_override: Optional[int] = None
        self.req_count = 0
        self.resp_count = 0
        self.denied_resp_count = 0
        self.corrupt_resp_count = 0
        self.request_addrs = []
        self.response_addrs = []

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
        if not isinstance(target, UncacheBundle):
            raise TypeError(f"UncacheAgent.bind requires an uncache interface, got {type(target).__name__}")
        self.interface = target

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

    def set_a_ready(self, value: Optional[int]) -> None:
        self.a_ready_override = None if value is None else (1 if int(value) else 0)

    def inject_next_response_fault(self, *, denied: int = 0, corrupt: int = 0) -> None:
        self.next_response_faults.append(
            {
                "denied": 1 if int(denied) else 0,
                "corrupt": 1 if int(corrupt) else 0,
            }
        )

    def inject_response_fault_at(self, addr: int, *, denied: int = 0, corrupt: int = 0) -> None:
        self.response_faults_by_addr.setdefault(int(addr), deque()).append(
            {
                "denied": 1 if int(denied) else 0,
                "corrupt": 1 if int(corrupt) else 0,
            }
        )

    def _next_fault_for_addr(self, addr: int) -> dict:
        faults = self.response_faults_by_addr.get(int(addr))
        if faults:
            fault = faults.popleft()
            if not faults:
                self.response_faults_by_addr.pop(int(addr), None)
            return fault
        if self.next_response_faults:
            return self.next_response_faults.popleft()
        return {}

    def on_clock_edge(self, cycle: int) -> None:
        if self.interface is None:
            return
        a_ready = 1 if self.a_ready_override is None else int(self.a_ready_override)
        self._write(self.interface.a_ready, a_ready)
        if self._read(self.interface.a_valid, 0) == 1 and a_ready == 1:
            addr = self._read(self.interface.a_bits_address, 0)
            blk = self.memory.read_block(addr, 32)
            data = int.from_bytes(blk, "little")
            latency = self.mmio_latency if self.memory.is_mmio(addr) else self.latency
            fault = self._next_fault_for_addr(addr)
            self.pending.append(
                _PendingData(
                    data=data,
                    ready_cycle=cycle + latency,
                    addr=addr,
                    denied=int(fault.get("denied", 0)),
                    corrupt=int(fault.get("corrupt", 0)),
                )
            )
            self.req_count += 1
            self.request_addrs.append(int(addr))
            self._emit(
                cycle,
                "handshake.uncache_a",
                {
                    "address": int(addr),
                    "latency": int(latency),
                    "denied": int(fault.get("denied", 0)),
                    "corrupt": int(fault.get("corrupt", 0)),
                },
                level="DEBUG",
            )

        self._write(self.interface.d_valid, 0)
        if self.pending and cycle >= self.pending[0].ready_cycle:
            item = self.pending.popleft()
            self._write(self.interface.d_valid, 1)
            self._write(self.interface.d_bits_source, 0)
            self._write(self.interface.d_bits_data, item.data)
            self._write(self.interface.d_bits_denied, item.denied)
            self._write(self.interface.d_bits_corrupt, item.corrupt)
            self.resp_count += 1
            self.response_addrs.append(int(item.addr))
            self.denied_resp_count += int(item.denied)
            self.corrupt_resp_count += int(item.corrupt)
            self._emit(
                cycle,
                "handshake.uncache_d",
                {
                    "response_count": int(self.resp_count),
                    "denied": int(item.denied),
                    "corrupt": int(item.corrupt),
                },
                level="DEBUG",
            )

    def get_stats(self) -> dict:
        return {
            "req_count": self.req_count,
            "resp_count": self.resp_count,
            "denied_resp_count": self.denied_resp_count,
            "corrupt_resp_count": self.corrupt_resp_count,
            "pending": len(self.pending),
            "a_ready_override": self.a_ready_override,
            "queued_response_faults": len(self.next_response_faults),
            "queued_addr_faults": sum(len(faults) for faults in self.response_faults_by_addr.values()),
            "request_addrs": list(self.request_addrs),
            "response_addrs": list(self.response_addrs),
        }


__all__ = ["UncacheAgent"]
