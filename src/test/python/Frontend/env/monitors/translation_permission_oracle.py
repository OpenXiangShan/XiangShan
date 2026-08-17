from __future__ import annotations

from typing import Any, Callable, Dict, Optional

from ..support.pmp_pma import reconstruct_pmp_request_addr


_EXCEPTION_BITS = {
    1: "instruction_access_fault",
    12: "instruction_page_fault",
    20: "instruction_guest_page_fault",
}

_MAINPIPE_S1_VALID = "Frontend_top.Frontend.inner_icache.mainPipe.s1_valid"
_MAINPIPE_START_VADDR = "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_0_vAddr_0_addr"
_MAINPIPE_PTAG = "Frontend_top.Frontend.inner_icache.mainPipe.s1_wayLookupEntry_0_pTag"
_PMP_REQUEST_SIZE_BYTES = 8


class TranslationPermissionOracle:
    """Check one armed translation scenario against DUT-facing observations."""

    def __init__(self) -> None:
        self.env = None
        self.event_sink: Optional[Callable[[Dict], None]] = None
        self.active: Optional[dict] = None
        self.errors: list[dict] = []
        self.records: list[dict] = []
        self._ptw_requests: list[dict] = []
        self._icache_cursor = 0
        self._uncache_cursor = 0

    @staticmethod
    def _read(signal: Any, default: int = 0) -> int:
        try:
            value = getattr(signal, "value", None)
            return int(default if value is None else value)
        except Exception:
            return int(default)

    def bind_env(self, env) -> None:
        self.env = env

    def set_event_sink(self, sink: Optional[Callable[[Dict], None]]) -> None:
        self.event_sink = sink

    def _emit(self, cycle: int, event_type: str, payload: dict, level: str = "INFO") -> None:
        if self.event_sink is None:
            return
        self.event_sink(
            {
                "type": event_type,
                "source": "translation_permission_oracle",
                "cycle": int(cycle),
                "level": level,
                "payload": dict(payload),
            }
        )

    def _context(self) -> dict:
        if self.active is None:
            return {}
        return {
            "scenario_id": self.active["scenario_id"],
            "translation_epoch": self.active["translation_epoch"],
            "va": self.active["va"],
            "vpn": self.active["expected_ptw_request"]["vpn"],
        }

    def _record(self, cycle: int, kind: str, **details) -> None:
        entry = {"cycle": int(cycle), "kind": str(kind), **self._context(), **details}
        self.records.append(entry)
        self._emit(cycle, f"translation.{kind}", entry, level="DEBUG")

    def _error(self, cycle: int, reason: str, **details) -> None:
        entry = {"cycle": int(cycle), "reason": str(reason), **self._context(), **details}
        self.errors.append(entry)
        self._emit(cycle, f"translation.{reason}", entry, level="WARNING")

    @staticmethod
    def _expected_fault(state) -> Optional[str]:
        outcome = str(state.expected_outcome.get("outcome", ""))
        if outcome in _EXCEPTION_BITS.values():
            return outcome
        return {
            "page_fault": "instruction_page_fault",
            "access_fault": "instruction_access_fault",
            "guest_fault": "instruction_guest_page_fault",
        }.get(str(state.scenario.expected_result))

    def arm(self, state, *, translation_epoch: int) -> dict:
        expected_fault = self._expected_fault(state)
        self.active = {
            "scenario_id": str(state.scenario.scenario_id),
            "translation_epoch": int(translation_epoch),
            "va": int(state.scenario.va),
            "payload_size": len(state.scenario.payload),
            "expected_ptw_request": dict(state.expected_ptw_request),
            "expected_outcome": dict(state.expected_outcome),
            "expected_path": "fault" if expected_fault else str(state.scenario.expected_path),
            "expected_fault": expected_fault,
            "request_seen": False,
            "response_seen": False,
            "fetch_seen": False,
            "fault_seen": False,
            "pmp_request_seen": False,
            "permission_check_required": bool(state.scenario.pmp_entries or state.scenario.pma_entries)
            and expected_fault in {None, "instruction_access_fault"},
            "pmp_signal_unavailable": False,
        }
        self._icache_cursor = len(getattr(getattr(self.env, "icache_agent", None), "request_records", []))
        self._uncache_cursor = len(getattr(getattr(self.env, "uncache_agent", None), "request_addrs", []))
        self._record(getattr(self.env, "current_cycle", 0), "armed")
        return self.get_active()

    def get_active(self) -> Optional[dict]:
        return None if self.active is None else dict(self.active)

    def observe_ptw_request(self, cycle: int, *, vpn: int, s2xlate: int, get_gpa: int) -> None:
        if self.active is None:
            return
        actual = {"vpn": int(vpn), "s2xlate": int(s2xlate), "get_gpa": int(get_gpa)}
        expected = self.active["expected_ptw_request"]
        if self.active["fetch_seen"] or self.active["fault_seen"]:
            self._error(cycle, "unexpected_followup_ptw_request", actual=actual)
            return
        record = {**actual, "translation_epoch": int(getattr(self.env, "translation_epoch", self.active["translation_epoch"]))}
        self._ptw_requests.append(record)
        self.active["request_seen"] = True
        self._record(cycle, "ptw_request", actual=actual)
        mismatches = {
            key: {"expected": int(expected[key]), "actual": int(actual[key])}
            for key in ("vpn", "s2xlate", "get_gpa")
            if int(expected[key]) != int(actual[key])
        }
        if mismatches:
            self._error(cycle, "ptw_request_mismatch", expected=expected, actual=actual, mismatches=mismatches)

    def observe_ptw_response(self, cycle: int, *, vpn: int, s2xlate: int, get_gpa: int, response: dict) -> None:
        if self.active is None:
            return
        actual = {"vpn": int(vpn), "s2xlate": int(s2xlate), "get_gpa": int(get_gpa)}
        expected = self.active["expected_ptw_request"]
        response_record = {
            "ppn": int(response.get("s1_entry_ppn", 0)),
            "pbmt": int(response.get("s1_entry_pbmt", 0)),
            "s1_perm_x": int(response.get("s1_entry_perm_x", 0)),
            "s2_perm_x": int(response.get("s2_entry_perm_x", 0)),
            "s1_pf": int(response.get("s1_pf", 0)),
            "s1_af": int(response.get("s1_af", 0)),
            "s2_gpf": int(response.get("s2_gpf", 0)),
            "s2_gaf": int(response.get("s2_gaf", 0)),
        }
        self._record(cycle, "ptw_response", actual=actual, response=response_record)
        request_index = next(
            (
                index
                for index, item in enumerate(self._ptw_requests)
                if item["vpn"] == int(vpn) and item["s2xlate"] == int(s2xlate) and item["get_gpa"] == int(get_gpa)
            ),
            None,
        )
        request_epoch = None if request_index is None else self._ptw_requests.pop(request_index)["translation_epoch"]
        if request_epoch is not None and int(request_epoch) != int(self.active["translation_epoch"]):
            self._record(cycle, "stale_ptw_response", response_epoch=request_epoch, actual=actual)
            return
        mismatches = {
            key: {"expected": int(expected[key]), "actual": int(actual[key])}
            for key in ("vpn", "s2xlate", "get_gpa")
            if int(expected[key]) != int(actual[key])
        }
        if mismatches:
            self._error(cycle, "ptw_response_context_mismatch", expected=expected, actual=actual, mismatches=mismatches)
            return
        self.active["response_seen"] = True

    def observe_fetch_request(self, cycle: int, *, path: str, pa: int) -> None:
        if self.active is None:
            return
        actual_path = str(path)
        actual_pa = int(pa)
        if self.active["expected_path"] == "fault":
            self._record(cycle, "fetch_request", path=actual_path, pa=actual_pa)
            self._error(cycle, "unexpected_fetch_after_fault", path=actual_path, pa=actual_pa)
            return
        if self.active["fetch_seen"]:
            return
        self._record(cycle, "fetch_request", path=actual_path, pa=actual_pa)
        expected_path = "icache" if self.active["expected_path"] == "cacheable" else "uncache"
        expected_pa = int(self.active["expected_outcome"].get("pa", 0))
        compare_pa = expected_pa & ~0x3F if expected_path == "icache" else expected_pa
        if actual_path != expected_path or actual_pa != compare_pa:
            self._error(
                cycle,
                "translated_pa_or_path_mismatch",
                expected_path=expected_path,
                actual_path=actual_path,
                expected_pa=compare_pa,
                actual_pa=actual_pa,
            )
            return
        self.active["fetch_seen"] = True

    def observe_mainpipe_pmp_request(self, cycle: int, *, addr: int) -> None:
        if self.active is None or not self.active["permission_check_required"]:
            return
        if self.active["pmp_request_seen"]:
            return
        actual_addr = int(addr)
        expected_addr = int(self.active["expected_outcome"].get("pa", 0))
        self._record(
            cycle,
            "mainpipe_pmp_request",
            addr=actual_addr,
            size=_PMP_REQUEST_SIZE_BYTES,
            end=actual_addr + _PMP_REQUEST_SIZE_BYTES - 1,
        )
        if actual_addr != expected_addr:
            self._error(
                cycle,
                "mainpipe_pmp_request_addr_mismatch",
                expected_addr=expected_addr,
                actual_addr=actual_addr,
                size=_PMP_REQUEST_SIZE_BYTES,
            )
            return
        self.active["pmp_request_seen"] = True

    def _read_internal_signal(self, name: str) -> Optional[int]:
        dut = getattr(self.env, "dut", None)
        if dut is None or bool(getattr(dut, "_is_fake_frontend_dut", False)):
            return None
        getter = getattr(dut, "GetInternalSignal", None)
        if not callable(getter):
            return None
        try:
            signal = getter(name)
            value = getattr(signal, "value", None)
            return None if value is None else int(value)
        except Exception:
            return None

    def _sample_mainpipe_pmp_request(self, cycle: int) -> None:
        if self.active is None or not self.active["permission_check_required"]:
            return
        if not self.active["response_seen"] or self.active["pmp_request_seen"]:
            return
        valid = self._read_internal_signal(_MAINPIPE_S1_VALID)
        if valid is None:
            if not self.active["pmp_signal_unavailable"]:
                self._error(cycle, "mainpipe_pmp_signal_unavailable", signal=_MAINPIPE_S1_VALID)
                self.active["pmp_signal_unavailable"] = True
            return
        if not valid:
            return
        start_vaddr_pruned = self._read_internal_signal(_MAINPIPE_START_VADDR)
        p_tag = self._read_internal_signal(_MAINPIPE_PTAG)
        if start_vaddr_pruned is None or p_tag is None:
            if not self.active["pmp_signal_unavailable"]:
                self._error(
                    cycle,
                    "mainpipe_pmp_signal_unavailable",
                    missing=[
                        name
                        for name, value in ((_MAINPIPE_START_VADDR, start_vaddr_pruned), (_MAINPIPE_PTAG, p_tag))
                        if value is None
                    ],
                )
                self.active["pmp_signal_unavailable"] = True
            return
        self.observe_mainpipe_pmp_request(
            cycle,
            addr=reconstruct_pmp_request_addr(p_tag, start_vaddr_pruned),
        )

    def observe_cfvec(self, cycle: int, *, pc: int, exception_bits: dict[int, int], cross_page: bool = False) -> None:
        if self.active is None:
            return
        actual_faults = [name for bit, name in _EXCEPTION_BITS.items() if int(exception_bits.get(bit, 0))]
        if not actual_faults:
            return
        actual_fault = actual_faults[0] if len(actual_faults) == 1 else "multiple"
        expected_fault = self.active["expected_fault"]
        if expected_fault is not None and self.active["fault_seen"]:
            return
        self._record(cycle, "cfvec_exception", pc=int(pc), fault=actual_fault, cross_page=bool(cross_page))
        if expected_fault is None:
            self._error(cycle, "unexpected_cfvec_exception", pc=int(pc), actual_fault=actual_fault)
            return
        va = int(self.active["va"])
        if not va <= int(pc) < va + int(self.active["payload_size"]):
            self._error(cycle, "cfvec_exception_pc_mismatch", expected_va=va, actual_pc=int(pc), actual_fault=actual_fault)
            return
        if actual_fault != expected_fault:
            self._error(cycle, "cfvec_exception_type_mismatch", expected_fault=expected_fault, actual_fault=actual_fault, pc=int(pc))
            return
        self.active["fault_seen"] = True

    def on_clock_edge(self, cycle: int) -> None:
        if self.active is None or self.env is None:
            return
        ptw_if = getattr(self.env, "ptw_if", None)
        if ptw_if is not None and self._read(ptw_if.req_0_valid) and self._read(ptw_if.req_0_ready):
            self.observe_ptw_request(
                cycle,
                vpn=self._read(ptw_if.req_0_bits_vpn),
                s2xlate=self._read(ptw_if.req_0_bits_s2xlate),
                get_gpa=self._read(ptw_if.req_0_bits_get_gpa),
            )
        expectation = getattr(self.env.ptw_agent, "get_last_drive_expectation", lambda: None)()
        if ptw_if is not None and self._read(ptw_if.resp_valid) and expectation:
            self.observe_ptw_response(
                cycle,
                vpn=int(expectation["vpn"]),
                s2xlate=int(expectation["s2xlate"]),
                get_gpa=int(expectation.get("get_gpa", 0)),
                response=dict(expectation["resp"]),
            )
        self._sample_mainpipe_pmp_request(cycle)
        icache_records = self.env.icache_agent.request_records
        for record in icache_records[self._icache_cursor :]:
            self.observe_fetch_request(record["cycle"], path="icache", pa=record["address"])
        self._icache_cursor = len(icache_records)
        uncache_addrs = self.env.uncache_agent.request_addrs
        for pa in uncache_addrs[self._uncache_cursor :]:
            self.observe_fetch_request(cycle, path="uncache", pa=pa)
        self._uncache_cursor = len(uncache_addrs)
        observe_if = getattr(self.env, "backend_observe_if", None)
        if observe_if is None:
            return
        for slot in range(8):
            if not self._read(observe_if.cfvec_valid[slot]):
                continue
            self.observe_cfvec(
                cycle,
                pc=self._read(observe_if.cfvec_pc[slot]),
                exception_bits={bit: self._read(observe_if.cfvec_exception_vec[slot][bit]) for bit in _EXCEPTION_BITS},
                cross_page=bool(self._read(observe_if.cfvec_cross_page_ipf_fix[slot])),
            )

    def assert_complete(self) -> dict:
        if self.active is None:
            raise AssertionError("translation oracle has no armed scenario")
        missing = []
        if not self.active["request_seen"]:
            missing.append("ptw_request")
        if not self.active["response_seen"]:
            missing.append("ptw_response")
        if self.active["expected_path"] == "fault":
            if not self.active["fault_seen"]:
                missing.append("cfvec_exception")
        elif not self.active["fetch_seen"]:
            missing.append("fetch_request")
        if self.active["permission_check_required"] and not self.active["pmp_request_seen"]:
            missing.append("mainpipe_pmp_request")
        if missing:
            self._error(getattr(self.env, "current_cycle", 0), "missing_observation", missing=missing)
        if self.errors:
            raise AssertionError(f"translation oracle failed: {self.errors}")
        return self.get_stats()

    def get_stats(self) -> dict:
        return {
            "active": self.get_active(),
            "error_count": len(self.errors),
            "errors": [dict(error) for error in self.errors],
            "records": [dict(record) for record in self.records],
        }

    def clear(self) -> None:
        self.active = None
        self.errors.clear()
        self.records.clear()
        self._ptw_requests.clear()


__all__ = ["TranslationPermissionOracle"]
