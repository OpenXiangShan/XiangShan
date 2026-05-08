from __future__ import annotations

import logging
import random
from collections import deque
from dataclasses import dataclass
from typing import Any, Callable, Dict, Optional

from ..bundles import PTWBundle
from ..memory_model import PageTableModel
from ..model.ptw_response_source import NemuPtwResponseSource, PTWRequestSnapshot


@dataclass
class _PTWPending:
    resp: dict
    snapshot: PTWRequestSnapshot
    driven_source: str
    ready_cycle: int
    vpn: int
    s2xlate: int
    get_gpa: int
    memidx_is_ld: int
    memidx_is_st: int
    memidx_idx: int


class PTWAgent:
    def __init__(self, page_table: PageTableModel) -> None:
        self.logger = logging.getLogger("env.agents.ptw")
        self.page_table = page_table
        self.interface = None
        self.latency_min = 3
        self.latency_max = 3
        self.ready_strategy = "always"
        self.ready_probability = 1.0
        self.ready_high_cycles = 1
        self.ready_low_cycles = 0
        self.flush_pending_on_sfence = True
        self.strict_bare_mode = False
        self._rng = random.Random(1)
        self.response_source = "model"
        self.compare_drive_source = "nemu"
        self.nemu_ptw_adapter = ""
        self._nemu_source: Optional[NemuPtwResponseSource] = None
        self.nemu_sync_hook: Optional[Callable[..., None]] = None
        self.request_context_provider: Optional[Callable[[], Dict[str, int]]] = None
        self.pending = deque()
        self.active_resp: Optional[_PTWPending] = None
        self.last_drive_expectation: Optional[dict] = None
        self.event_sink: Optional[Callable[[Dict], None]] = None
        self.req_count = 0
        self.resp_count = 0
        self.req_blocked_cycles = 0
        self.sfence_flush_count = 0
        self.sfence_dropped_responses = 0
        self.bare_req_count = 0
        self.compare_count = 0
        self.compare_match_count = 0
        self.compare_mismatch_count = 0
        self.last_compare_diff: Optional[Dict[str, Dict[str, Any]]] = None

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
        if not isinstance(target, PTWBundle):
            raise TypeError(f"PTWAgent.bind requires a ptw interface, got {type(target).__name__}")
        self.interface = target

    def set_event_sink(self, sink: Optional[Callable[[Dict], None]]) -> None:
        self.event_sink = sink

    def set_request_context_provider(self, provider: Optional[Callable[[], Dict[str, int]]]) -> None:
        self.request_context_provider = provider

    def set_nemu_sync_hook(self, hook: Optional[Callable[..., None]]) -> None:
        self.nemu_sync_hook = hook

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

    def configure(
        self,
        latency: Optional[int] = None,
        mode: Optional[str] = None,
        *,
        latency_max: Optional[int] = None,
        req_ready_strategy: str = "always",
        req_ready_probability: float = 1.0,
        req_ready_high_cycles: int = 1,
        req_ready_low_cycles: int = 0,
        seed: int = 1,
        flush_pending_on_sfence: bool = True,
        strict_bare_mode: bool = False,
        response_source: Optional[str] = None,
        compare_drive_source: Optional[str] = None,
        nemu_ptw_adapter: Optional[str] = None,
    ) -> None:
        if latency is None:
            latency = self.latency_min
        if latency_max is None:
            latency_max = latency
        self.latency_min = max(0, int(latency))
        self.latency_max = max(self.latency_min, int(latency_max))
        strategy = str(req_ready_strategy).lower()
        if strategy not in {"always", "periodic", "random"}:
            raise ValueError(f"unsupported PTW req ready strategy: {req_ready_strategy}")
        self.ready_strategy = strategy
        self.ready_probability = min(1.0, max(0.0, float(req_ready_probability)))
        self.ready_high_cycles = max(0, int(req_ready_high_cycles))
        self.ready_low_cycles = max(0, int(req_ready_low_cycles))
        self.flush_pending_on_sfence = bool(flush_pending_on_sfence)
        self.strict_bare_mode = bool(strict_bare_mode)
        self._rng = random.Random(int(seed))
        source = self.response_source if response_source is None else str(response_source).lower()
        if source not in {"model", "nemu", "compare"}:
            raise ValueError(f"unsupported PTW response source: {response_source}")
        drive_source = self.compare_drive_source if compare_drive_source is None else str(compare_drive_source).lower()
        if drive_source not in {"model", "nemu"}:
            raise ValueError(f"unsupported PTW compare drive source: {compare_drive_source}")
        self.response_source = source
        self.compare_drive_source = drive_source
        self.nemu_ptw_adapter = self.nemu_ptw_adapter if nemu_ptw_adapter is None else str(nemu_ptw_adapter or "")
        if self.response_source in {"nemu", "compare"}:
            self._nemu_source = NemuPtwResponseSource(self.nemu_ptw_adapter)
        else:
            self._nemu_source = None
        if mode is not None:
            self.page_table.set_mode(mode)
        self.logger.info(
            "configured: latency=[%d,%d] mode=%s ready=%s prob=%.3f high=%d low=%d sfence_flush=%s strict_bare=%s response_source=%s compare_drive_source=%s",
            self.latency_min,
            self.latency_max,
            self.page_table.mode,
            self.ready_strategy,
            self.ready_probability,
            self.ready_high_cycles,
            self.ready_low_cycles,
            self.flush_pending_on_sfence,
            self.strict_bare_mode,
            self.response_source,
            self.compare_drive_source,
        )

    @staticmethod
    def _flatten_compare_fields(value: Any, prefix: str = "") -> Dict[str, Any]:
        if isinstance(value, dict):
            flat: Dict[str, Any] = {}
            for key in sorted(value.keys()):
                path = f"{prefix}.{key}" if prefix else str(key)
                flat.update(PTWAgent._flatten_compare_fields(value[key], path))
            return flat
        if isinstance(value, (list, tuple)):
            flat = {}
            for idx, item in enumerate(value):
                path = f"{prefix}[{idx}]"
                flat.update(PTWAgent._flatten_compare_fields(item, path))
            return flat
        return {prefix or "<root>": value}

    def _build_model_response(self, snapshot: PTWRequestSnapshot) -> dict:
        return self.page_table.build_ptw_resp(
            snapshot.vpn,
            s2xlate=snapshot.s2xlate,
            get_gpa=snapshot.get_gpa,
            memidx_is_ld=snapshot.memidx_is_ld,
            memidx_is_st=snapshot.memidx_is_st,
            memidx_idx=snapshot.memidx_idx,
            strict_bare_mode=self.strict_bare_mode,
        )

    def _build_nemu_response(self, snapshot: PTWRequestSnapshot) -> dict:
        if self._nemu_source is None:
            raise RuntimeError("PTW response source requires a NEMU adapter but none is configured")
        return self._nemu_source.build_response(snapshot)

    def _emit_compare_result(
        self,
        *,
        cycle: int,
        snapshot: PTWRequestSnapshot,
        model_resp: dict,
        nemu_resp: dict,
        driven_source: str,
    ) -> None:
        model_fields = self._flatten_compare_fields(model_resp)
        nemu_fields = self._flatten_compare_fields(nemu_resp)
        diff: Dict[str, Dict[str, Any]] = {}
        for key in sorted(set(model_fields.keys()) | set(nemu_fields.keys())):
            model_value = model_fields.get(key)
            nemu_value = nemu_fields.get(key)
            if model_value != nemu_value:
                diff[key] = {"model": model_value, "nemu": nemu_value}
        matched = len(diff) == 0
        self.compare_count += 1
        if matched:
            self.compare_match_count += 1
            self.last_compare_diff = None
        else:
            self.compare_mismatch_count += 1
            self.last_compare_diff = diff
        self._emit(
            cycle,
            "check.ptw_resp_compare",
            {
                "sequence_id": int(snapshot.sequence_id),
                "vpn": int(snapshot.vpn),
                "s2xlate": int(snapshot.s2xlate),
                "matched": matched,
                "diff_count": len(diff),
                "diff": diff,
                "model_resp": model_resp,
                "nemu_resp": nemu_resp,
                "driven_source": str(driven_source),
            },
            level=("DEBUG" if matched else "WARNING"),
        )

    def _build_request_snapshot(
        self,
        *,
        cycle: int,
        vpn: int,
        s2xlate: int,
        get_gpa: int,
        memidx_is_ld: int,
        memidx_is_st: int,
        memidx_idx: int,
    ) -> PTWRequestSnapshot:
        context: Dict[str, int] = {}
        if self.request_context_provider is not None:
            context = dict(self.request_context_provider() or {})
        sequence_id = self.req_count + 1
        known_fields = {
            "sequence_id",
            "cycle",
            "vpn",
            "s2xlate",
            "get_gpa",
            "memidx_is_ld",
            "memidx_is_st",
            "memidx_idx",
            "priv_imode",
            "satp_mode",
            "vsatp_mode",
            "hgatp_mode",
            "sfence_valid",
            "sfence_bits_rs1",
            "sfence_bits_rs2",
            "sfence_bits_addr",
            "sfence_bits_id",
            "sfence_bits_hv",
            "sfence_bits_hg",
        }
        extra = {k: int(v) for k, v in context.items() if k not in known_fields}
        return PTWRequestSnapshot(
            sequence_id=int(context.get("sequence_id", sequence_id)),
            cycle=int(context.get("cycle", cycle)),
            vpn=int(vpn),
            s2xlate=int(s2xlate),
            get_gpa=int(get_gpa),
            memidx_is_ld=int(memidx_is_ld),
            memidx_is_st=int(memidx_is_st),
            memidx_idx=int(memidx_idx),
            priv_imode=int(context.get("priv_imode", 3)),
            satp_mode=int(context.get("satp_mode", 0)),
            vsatp_mode=int(context.get("vsatp_mode", 0)),
            hgatp_mode=int(context.get("hgatp_mode", 0)),
            sfence_valid=int(context.get("sfence_valid", 0)),
            sfence_bits_rs1=int(context.get("sfence_bits_rs1", 0)),
            sfence_bits_rs2=int(context.get("sfence_bits_rs2", 0)),
            sfence_bits_addr=int(context.get("sfence_bits_addr", 0)),
            sfence_bits_id=int(context.get("sfence_bits_id", 0)),
            sfence_bits_hv=int(context.get("sfence_bits_hv", 0)),
            sfence_bits_hg=int(context.get("sfence_bits_hg", 0)),
            extra=extra,
        )

    def _build_response(self, snapshot: PTWRequestSnapshot) -> dict:
        if self.response_source == "nemu":
            return self._build_nemu_response(snapshot)
        model_resp = self._build_model_response(snapshot)
        if self.response_source == "compare":
            nemu_resp = self._build_nemu_response(snapshot)
            driven_source = self.compare_drive_source
            self._emit_compare_result(
                cycle=snapshot.cycle,
                snapshot=snapshot,
                model_resp=model_resp,
                nemu_resp=nemu_resp,
                driven_source=driven_source,
            )
            return nemu_resp if driven_source == "nemu" else model_resp
        return model_resp

    def _driven_source(self) -> str:
        if self.response_source == "compare":
            return self.compare_drive_source
        return self.response_source

    def get_active_drive_expectation(self) -> Optional[dict]:
        if self.active_resp is None:
            return None
        item = self.active_resp
        return {
            "sequence_id": int(item.snapshot.sequence_id),
            "vpn": int(item.vpn),
            "s2xlate": int(item.s2xlate),
            "cycle": int(item.snapshot.cycle),
            "ready_cycle": int(item.ready_cycle),
            "driven_source": str(item.driven_source),
            "resp": dict(item.resp),
        }

    def get_last_drive_expectation(self) -> Optional[dict]:
        if self.last_drive_expectation is None:
            return None
        return dict(self.last_drive_expectation)

    def _current_latency(self) -> int:
        if self.latency_max <= self.latency_min:
            return self.latency_min
        return self._rng.randint(self.latency_min, self.latency_max)

    def _compute_req_ready(self, cycle: int) -> int:
        if self.ready_strategy == "always":
            return 1
        if self.ready_strategy == "random":
            return 1 if self._rng.random() < self.ready_probability else 0
        period = self.ready_high_cycles + self.ready_low_cycles
        if period <= 0:
            return 1
        if self.ready_high_cycles <= 0:
            return 0
        return 1 if (int(cycle) % period) < self.ready_high_cycles else 0

    def _drive_response(self, resp: Optional[dict], valid: int) -> None:
        self._write(self.interface.resp_valid, valid)
        payload = resp or {}
        ppn_low_signals = self.interface.resp_bits_s1_ppn_low
        valididx_signals = self.interface.resp_bits_s1_valididx
        pteidx_signals = self.interface.resp_bits_s1_pteidx
        ppn_low_payload = payload.get("s1_ppn_low", [0] * len(ppn_low_signals.signals))
        valididx_payload = payload.get("s1_valididx", [0] * len(valididx_signals.signals))
        pteidx_payload = payload.get("s1_pteidx", [0] * len(pteidx_signals.signals))
        self._write(self.interface.resp_bits_s2xlate, payload.get("s2xlate", 0))
        self._write(self.interface.resp_bits_get_gpa, payload.get("get_gpa", 0))
        self._write(self.interface.resp_bits_memidx_is_ld, payload.get("memidx_is_ld", 0))
        self._write(self.interface.resp_bits_memidx_is_st, payload.get("memidx_is_st", 0))
        self._write(self.interface.resp_bits_memidx_idx, payload.get("memidx_idx", 0))
        self._write(self.interface.resp_bits_s2_entry_tag, payload.get("s2_entry_tag", 0))
        self._write(self.interface.resp_bits_s2_entry_vmid, payload.get("s2_entry_vmid", 0))
        self._write(self.interface.resp_bits_s2_entry_n, payload.get("s2_entry_n", 0))
        self._write(self.interface.resp_bits_s2_entry_pbmt, payload.get("s2_entry_pbmt", 0))
        self._write(self.interface.resp_bits_s2_entry_perm_a, payload.get("s2_entry_perm_a", 0))
        self._write(self.interface.resp_bits_s2_entry_perm_g, payload.get("s2_entry_perm_g", 0))
        self._write(self.interface.resp_bits_s2_entry_perm_u, payload.get("s2_entry_perm_u", 0))
        self._write(self.interface.resp_bits_s2_entry_perm_x, payload.get("s2_entry_perm_x", 0))
        self._write(self.interface.resp_bits_s2_entry_perm_w, payload.get("s2_entry_perm_w", 0))
        self._write(self.interface.resp_bits_s2_entry_perm_r, payload.get("s2_entry_perm_r", 0))
        self._write(self.interface.resp_bits_s2_entry_level, payload.get("s2_entry_level", 0))
        self._write(self.interface.resp_bits_s2_entry_v, payload.get("s2_entry_v", 0))
        self._write(self.interface.resp_bits_s2_entry_ppn, payload.get("s2_entry_ppn", 0))
        self._write(self.interface.resp_bits_s2_gpf, payload.get("s2_gpf", 0))
        self._write(self.interface.resp_bits_s2_gaf, payload.get("s2_gaf", 0))
        self._write(self.interface.resp_bits_s1_entry_tag, payload.get("s1_entry_tag", 0))
        self._write(self.interface.resp_bits_s1_entry_asid, payload.get("s1_entry_asid", 0))
        self._write(self.interface.resp_bits_s1_entry_vmid, payload.get("s1_entry_vmid", 0))
        self._write(self.interface.resp_bits_s1_entry_n, payload.get("s1_entry_n", 0))
        self._write(self.interface.resp_bits_s1_entry_pbmt, payload.get("s1_entry_pbmt", 0))
        self._write(self.interface.resp_bits_s1_entry_perm_a, payload.get("s1_entry_perm_a", 0))
        self._write(self.interface.resp_bits_s1_entry_perm_g, payload.get("s1_entry_perm_g", 0))
        self._write(self.interface.resp_bits_s1_entry_perm_u, payload.get("s1_entry_perm_u", 0))
        self._write(self.interface.resp_bits_s1_entry_perm_x, payload.get("s1_entry_perm_x", 0))
        self._write(self.interface.resp_bits_s1_entry_perm_w, payload.get("s1_entry_perm_w", 0))
        self._write(self.interface.resp_bits_s1_entry_perm_r, payload.get("s1_entry_perm_r", 0))
        self._write(self.interface.resp_bits_s1_entry_level, payload.get("s1_entry_level", 0))
        self._write(self.interface.resp_bits_s1_entry_v, payload.get("s1_entry_v", 0))
        self._write(self.interface.resp_bits_s1_entry_ppn, payload.get("s1_entry_ppn", 0))
        self._write(self.interface.resp_bits_s1_addr_low, payload.get("s1_addr_low", 0))
        for idx, signal in enumerate(ppn_low_signals):
            self._write(signal, ppn_low_payload[idx])
        for idx, signal in enumerate(valididx_signals):
            self._write(signal, valididx_payload[idx])
        for idx, signal in enumerate(pteidx_signals):
            self._write(signal, pteidx_payload[idx])
        self._write(self.interface.resp_bits_s1_pf, payload.get("s1_pf", 0))
        self._write(self.interface.resp_bits_s1_af, payload.get("s1_af", 0))

    def _flush_pending(self, cycle: int) -> None:
        dropped = len(self.pending) + (1 if self.active_resp is not None else 0)
        if dropped <= 0:
            return
        self.pending.clear()
        self.active_resp = None
        self.sfence_flush_count += 1
        self.sfence_dropped_responses += dropped
        self._emit(
            cycle,
            "control.ptw_sfence_flush",
            {"dropped": int(dropped), "flush_count": int(self.sfence_flush_count)},
            level="INFO",
        )

    def on_clock_edge(self, cycle: int) -> None:
        if self.interface is None:
            return

        if self.flush_pending_on_sfence and self._read(self.interface.sfence_valid, 0) == 1:
            self._flush_pending(cycle)

        req_ready = self._compute_req_ready(cycle)
        self._write(self.interface.req_0_ready, req_ready)

        req_valid = self._read(self.interface.req_0_valid, 0)
        if req_valid == 1 and req_ready == 1:
            vpn = self._read(self.interface.req_0_bits_vpn, 0)
            s2xlate = self._read(self.interface.req_0_bits_s2xlate, 0)
            get_gpa = self._read(self.interface.req_0_bits_get_gpa, 0)
            memidx_is_ld = self._read(self.interface.req_0_bits_memidx_is_ld, 0)
            memidx_is_st = self._read(self.interface.req_0_bits_memidx_is_st, 0)
            memidx_idx = self._read(self.interface.req_0_bits_memidx_idx, 0)
            if self.page_table.mode == "bare":
                self.bare_req_count += 1
            latency = self._current_latency()
            if self.nemu_sync_hook is not None and self.response_source in {"nemu", "compare"}:
                self.nemu_sync_hook(
                    cycle=cycle,
                    vpn=vpn,
                    s2xlate=s2xlate,
                    get_gpa=get_gpa,
                    memidx_is_ld=memidx_is_ld,
                    memidx_is_st=memidx_is_st,
                    memidx_idx=memidx_idx,
                )
            snapshot = self._build_request_snapshot(
                cycle=cycle,
                vpn=vpn,
                s2xlate=s2xlate,
                get_gpa=get_gpa,
                memidx_is_ld=memidx_is_ld,
                memidx_is_st=memidx_is_st,
                memidx_idx=memidx_idx,
            )
            resp = self._build_response(snapshot)
            self.logger.debug(
                "ptw req handshake: cycle=%d vpn=0x%x s2xlate=%d source=%s s1_ppn=0x%x s2_ppn=0x%x s1_pf=%d s2_gpf=%d",
                int(cycle),
                int(vpn),
                int(s2xlate),
                self._driven_source(),
                int(resp.get("s1_entry_ppn", 0)),
                int(resp.get("s2_entry_ppn", 0)),
                int(resp.get("s1_pf", 0)),
                int(resp.get("s2_gpf", 0)),
            )
            self.pending.append(
                _PTWPending(
                    resp=resp,
                    snapshot=snapshot,
                    driven_source=self._driven_source(),
                    ready_cycle=cycle + latency,
                    vpn=int(vpn),
                    s2xlate=int(s2xlate),
                    get_gpa=int(get_gpa),
                    memidx_is_ld=int(memidx_is_ld),
                    memidx_is_st=int(memidx_is_st),
                    memidx_idx=int(memidx_idx),
                )
            )
            self.req_count += 1
            self._emit(
                cycle,
                "handshake.ptw_req",
                {
                    "vpn": int(vpn),
                    "s2xlate": int(s2xlate),
                    "get_gpa": int(get_gpa),
                    "memidx": {
                        "is_ld": int(memidx_is_ld),
                        "is_st": int(memidx_is_st),
                        "idx": int(memidx_idx),
                    },
                    "latency": int(latency),
                    "mode": self.page_table.mode,
                    "strict_bare_mode": bool(self.strict_bare_mode),
                    "response_source": self.response_source,
                    "compare_drive_source": self.compare_drive_source,
                    "driven_source": self._driven_source(),
                    "sequence_id": int(snapshot.sequence_id),
                },
                level="DEBUG",
            )
        elif req_valid == 1:
            self.req_blocked_cycles += 1

        if self.active_resp is None and self.pending and cycle >= self.pending[0].ready_cycle:
            self.active_resp = self.pending.popleft()

        if self.active_resp is None:
            self.last_drive_expectation = None
            self._drive_response(None, 0)
            return

        self.last_drive_expectation = {
            "sequence_id": int(self.active_resp.snapshot.sequence_id),
            "vpn": int(self.active_resp.vpn),
            "s2xlate": int(self.active_resp.s2xlate),
            "cycle": int(self.active_resp.snapshot.cycle),
            "ready_cycle": int(self.active_resp.ready_cycle),
            "driven_source": str(self.active_resp.driven_source),
            "resp": dict(self.active_resp.resp),
        }
        self._drive_response(self.active_resp.resp, 1)
        if self._read(self.interface.resp_ready, 1) == 1:
            item = self.active_resp
            self.active_resp = None
            self.resp_count += 1
            self._emit(
                cycle,
                "handshake.ptw_resp",
                {
                    "response_count": int(self.resp_count),
                    "vpn": int(item.vpn),
                    "s2xlate": int(item.s2xlate),
                    "get_gpa": int(item.get_gpa),
                    "memidx": {
                        "is_ld": int(item.memidx_is_ld),
                        "is_st": int(item.memidx_is_st),
                        "idx": int(item.memidx_idx),
                    },
                    "ppn": int(item.resp.get("s1_entry_ppn", 0)),
                    "pf": int(item.resp.get("s1_pf", 0)),
                    "driven_source": str(item.driven_source),
                    "sequence_id": int(item.snapshot.sequence_id),
                },
                level="DEBUG",
            )

    def get_stats(self) -> dict:
        return {
            "req_count": self.req_count,
            "resp_count": self.resp_count,
            "pending": len(self.pending),
            "active_resp": int(self.active_resp is not None),
            "mode": self.page_table.mode,
            "latency_min": self.latency_min,
            "latency_max": self.latency_max,
            "req_ready_strategy": self.ready_strategy,
            "req_blocked_cycles": self.req_blocked_cycles,
            "sfence_flush_count": self.sfence_flush_count,
            "sfence_dropped_responses": self.sfence_dropped_responses,
            "bare_req_count": self.bare_req_count,
            "strict_bare_mode": self.strict_bare_mode,
            "response_source": self.response_source,
            "compare_drive_source": self.compare_drive_source,
            "nemu_ptw_adapter": self.nemu_ptw_adapter,
            "compare_count": self.compare_count,
            "compare_match_count": self.compare_match_count,
            "compare_mismatch_count": self.compare_mismatch_count,
            "last_compare_diff": self.last_compare_diff,
        }


__all__ = ["PTWAgent"]
