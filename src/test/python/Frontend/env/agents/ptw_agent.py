from __future__ import annotations

import logging
from collections import deque
from dataclasses import dataclass
from typing import Callable, Dict, Optional

from ..bundles import PTWBundle, bind_bundle_optional
from ..memory_model import PageTableModel


@dataclass
class _PTWPending:
    resp: dict
    ready_cycle: int


class PTWAgent:
    def __init__(self, page_table: PageTableModel) -> None:
        self.logger = logging.getLogger("env.agents.ptw")
        self.page_table = page_table
        self.interface = None
        self.latency = 3
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
        if isinstance(target, PTWBundle):
            self.interface = target
            return
        self.interface = bind_bundle_optional(PTWBundle, target)

    def set_event_sink(self, sink: Optional[Callable[[Dict], None]]) -> None:
        self.event_sink = sink

    def _emit(self, cycle: int, event_type: str, payload: Dict, level: str = "INFO") -> None:
        if self.event_sink is None:
            return
        self.event_sink(
            {
                "type": event_type,
                "source": "ptw_agent",
                "cycle": int(cycle),
                "level": level,
                "payload": payload,
            }
        )

    def configure(self, latency: int = 3, mode: Optional[str] = None) -> None:
        self.latency = max(0, int(latency))
        if mode is not None:
            self.page_table.set_mode(mode)
        self.logger.info(
            "configured: latency=%d mode=%s",
            self.latency,
            self.page_table.mode,
        )

    def on_clock_edge(self, cycle: int) -> None:
        if self.interface is None:
            return
        self._write(self.interface.req_0_ready, 1)
        if self._read(self.interface.req_0_valid, 0) == 1:
            vpn = self._read(self.interface.req_0_bits_vpn, 0)
            resp = self.page_table.build_ptw_resp(vpn)
            self.pending.append(_PTWPending(resp=resp, ready_cycle=cycle + self.latency))
            self.req_count += 1
            self._emit(
                cycle,
                "handshake.ptw_req",
                {"vpn": int(vpn), "latency": int(self.latency), "mode": self.page_table.mode},
                level="DEBUG",
            )

        self._write(self.interface.resp_valid, 0)
        if self.pending and cycle >= self.pending[0].ready_cycle:
            item = self.pending.popleft()
            self._write(self.interface.resp_valid, 1)
            self._write(self.interface.resp_bits_s2xlate, item.resp.get("s2xlate", 0))
            self._write(self.interface.resp_bits_s1_entry_tag, item.resp.get("s1_entry_tag", 0))
            self._write(self.interface.resp_bits_s1_entry_asid, item.resp.get("s1_entry_asid", 0))
            self._write(self.interface.resp_bits_s1_entry_vmid, item.resp.get("s1_entry_vmid", 0))
            self._write(self.interface.resp_bits_s1_entry_n, item.resp.get("s1_entry_n", 0))
            self._write(self.interface.resp_bits_s1_entry_pbmt, item.resp.get("s1_entry_pbmt", 0))
            self._write(self.interface.resp_bits_s1_entry_perm_a, item.resp.get("s1_entry_perm_a", 0))
            self._write(self.interface.resp_bits_s1_entry_perm_g, item.resp.get("s1_entry_perm_g", 0))
            self._write(self.interface.resp_bits_s1_entry_perm_u, item.resp.get("s1_entry_perm_u", 0))
            self._write(self.interface.resp_bits_s1_entry_perm_x, item.resp.get("s1_entry_perm_x", 0))
            self._write(self.interface.resp_bits_s1_entry_perm_w, item.resp.get("s1_entry_perm_w", 0))
            self._write(self.interface.resp_bits_s1_entry_perm_r, item.resp.get("s1_entry_perm_r", 0))
            self._write(self.interface.resp_bits_s1_entry_level, item.resp.get("s1_entry_level", 0))
            self._write(self.interface.resp_bits_s1_entry_v, item.resp.get("s1_entry_v", 0))
            self._write(self.interface.resp_bits_s1_entry_ppn, item.resp.get("s1_entry_ppn", 0))
            self._write(self.interface.resp_bits_s1_addr_low, item.resp.get("s1_addr_low", 0))
            self.resp_count += 1
            self._emit(
                cycle,
                "handshake.ptw_resp",
                {"response_count": int(self.resp_count), "ppn": int(item.resp.get("s1_entry_ppn", 0))},
                level="DEBUG",
            )

    def get_stats(self) -> dict:
        return {
            "req_count": self.req_count,
            "resp_count": self.resp_count,
            "pending": len(self.pending),
            "mode": self.page_table.mode,
        }


__all__ = ["PTWAgent"]
