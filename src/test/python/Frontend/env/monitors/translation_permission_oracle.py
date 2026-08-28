from __future__ import annotations

from typing import Any, Callable, Dict, Optional

from ..support.pc_utils import fold_pc

_EXCEPTION_BITS = {
    1: "instruction_access_fault",
    12: "instruction_page_fault",
    20: "instruction_guest_page_fault",
}

_ITLB_PTW_REQ_GET_GPA = "Frontend_top.Frontend.inner_itlb.io_ptw_req_0_bits_getGpa"


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
    def _expected_fault(state, outcome: Optional[dict] = None) -> Optional[str]:
        selected_outcome = state.expected_outcome if outcome is None else outcome
        outcome_name = str(selected_outcome.get("outcome", ""))
        if outcome_name in _EXCEPTION_BITS.values():
            return outcome_name
        return {
            "page_fault": "instruction_page_fault",
            "access_fault": "instruction_access_fault",
            "guest_fault": "instruction_guest_page_fault",
        }.get(str(state.scenario.expected_result))

    def arm(self, state, *, translation_epoch: int, page_indexes=None, expect_ptw: bool = True) -> dict:
        all_page_outcomes = [dict(outcome) for outcome in getattr(state, "expected_page_outcomes", (state.expected_outcome,))]
        if page_indexes is None:
            selected_pages = tuple(range(len(all_page_outcomes)))
        else:
            selected_pages = tuple(int(page) for page in page_indexes)
        if (
            not selected_pages
            or len(set(selected_pages)) != len(selected_pages)
            or any(page < 0 or page >= len(all_page_outcomes) for page in selected_pages)
        ):
            raise ValueError("translation oracle page_indexes must select declared scenario pages")
        page_outcomes = [all_page_outcomes[page] for page in selected_pages]
        expected_fault = self._expected_fault(state, page_outcomes[0])
        s2xlate = int(state.expected_ptw_request["s2xlate"])
        response_covers_selected_pages = (
            (s2xlate != 2 and int(state.scenario.s1_pte.level) > 0)
            or (s2xlate in {2, 3} and int(state.scenario.s2_pte.level) > 0)
            or bool(state.scenario.s1_sector_lanes)
        )
        ptw_pages = selected_pages[:1] if response_covers_selected_pages else selected_pages
        expected_ptw_requests = [] if not expect_ptw or (
            str(state.scenario.mode).lower() == "bare" and s2xlate == 0
        ) else [
            {
                "scenario_id": str(state.scenario.scenario_id),
                "vpn": self.env.page_table.normalize_ptw_vpn((int(state.scenario.va) >> 12) + page),
                "s2xlate": s2xlate,
                "get_gpa": int(state.expected_ptw_request["get_gpa"]),
            }
            for page in ptw_pages
        ]
        if expected_fault == "instruction_guest_page_fault" and int(state.scenario.s2xlate) == 3:
            guest_fault_pages = tuple(
                page
                for page, outcome in zip(selected_pages, page_outcomes)
                if str(outcome.get("outcome", ""))
                == "instruction_guest_page_fault"
            )
            expected_ptw_requests.extend(
                {
                    "scenario_id": str(state.scenario.scenario_id),
                    "vpn": self.env.page_table.normalize_ptw_vpn((int(state.scenario.va) >> 12) + page),
                    "s2xlate": int(state.expected_ptw_request["s2xlate"]),
                    "get_gpa": 1,
                }
                for page in guest_fault_pages
            )
        expected_fetches = []
        allowed_out_of_scope_fetches = []
        payload_end = int(state.scenario.va) + len(state.scenario.payload)
        selected_page_set = set(selected_pages)
        for page, outcome in enumerate(all_page_outcomes):
            if not outcome.get("ok", False):
                continue
            path = "icache" if outcome["expected_path"] == "cacheable" else "uncache"
            outcome_va = int(outcome["va"])
            page_base_va = outcome_va & ~0xFFF
            start_va = max(int(state.scenario.va), page_base_va)
            end_va = min(payload_end, page_base_va + 0x1000)
            if page not in selected_page_set and end_va <= start_va:
                continue
            pa = int(outcome["pa"]) + (start_va - outcome_va)
            if path == "icache" and end_va > start_va:
                fetch_addrs = range(pa & ~0x3F, ((pa + (end_va - start_va - 1)) & ~0x3F) + 1, 0x40)
            elif path == "uncache":
                fetch_addrs = (pa & ~0x7,)
            else:
                fetch_addrs = (pa,)
            fetches = [
                {
                    "page": page,
                    "vpn": page_base_va >> 12,
                    "path": path,
                    "pa": fetch_pa,
                }
                for fetch_pa in fetch_addrs
            ]
            target_fetches = expected_fetches if page in selected_page_set else allowed_out_of_scope_fetches
            target_fetches.extend(fetches)
        self.active = {
            "scenario_id": str(state.scenario.scenario_id),
            "translation_epoch": int(translation_epoch),
            "va": int(page_outcomes[0]["va"]),
            "payload_size": len(state.scenario.payload),
            "expected_ptw_request": dict(state.expected_ptw_request),
            "expected_ptw_requests": expected_ptw_requests,
            "expected_outcome": dict(page_outcomes[0]),
            "selected_pages": list(selected_pages),
            "expected_page_outcomes": page_outcomes,
            "expected_fetches": expected_fetches,
            "allowed_out_of_scope_fetches": allowed_out_of_scope_fetches,
            "expected_path": "fault" if expected_fault else str(state.scenario.expected_path),
            "expected_fault": expected_fault,
            "expected_permission_probes": [
                dict(probe) for probe in getattr(state, "expected_permission_probes", ())
            ],
            "max_ptw_requests_per_key": getattr(state.scenario, "max_ptw_requests_per_key", None),
            "request_seen": False,
            "response_seen": False,
            "fetch_seen": False,
            "fault_seen": False,
            "requested_ptw_vpns": [],
            "responded_ptw_vpns": [],
            "requested_ptw_request_keys": [],
            "responded_ptw_request_keys": [],
            "ptw_request_counts": [],
            "fetched_pages": [],
            "observed_fetch_pas": [],
            "observed_out_of_scope_fetch_pas": [],
            "observed_normal_cfvec_pages": [],
            "observed_normal_cfvec_count": 0,
            "allow_speculative_before_response": False,
            "fetch_observation_ready": True,
            "fetch_observation_not_before": 0,
            "redirect_count_at_arm": int(getattr(getattr(self.env, "monitor", None), "redirect_count", 0)),
        }
        self._icache_cursor = len(getattr(getattr(self.env, "icache_agent", None), "request_records", []))
        self._uncache_cursor = len(getattr(getattr(self.env, "uncache_agent", None), "request_addrs", []))
        self._record(getattr(self.env, "current_cycle", 0), "armed")
        return self.get_active()

    def get_active(self) -> Optional[dict]:
        return None if self.active is None else dict(self.active)

    def _observation_ready(self, cycle: int) -> bool:
        return bool(
            self.active is not None
            and self.active["fetch_observation_ready"]
            and int(cycle) >= int(self.active["fetch_observation_not_before"])
        )

    def observe_ptw_request(self, cycle: int, *, vpn: int, s2xlate: int, get_gpa: int) -> None:
        if self.active is None:
            return
        actual = {"vpn": int(vpn), "s2xlate": int(s2xlate), "get_gpa": int(get_gpa)}
        if not self._observation_ready(cycle):
            self._record(cycle, "pre_redirect_ptw_request", actual=actual)
            self._ptw_requests.append(
                {
                    **actual,
                    "translation_epoch": int(self.active["translation_epoch"]),
                    "speculative": True,
                }
            )
            return
        expected = next(
            (
                request
                for request in self.active["expected_ptw_requests"]
                if int(request["vpn"]) == int(actual["vpn"])
                and int(request["s2xlate"]) == int(actual["s2xlate"])
                and int(request["get_gpa"]) == int(actual["get_gpa"])
            ),
            None,
        )
        if expected is None and self.active["expected_ptw_requests"] and not self.active["request_seen"]:
            self._record(cycle, "pre_target_ptw_request", actual=actual)
            self._ptw_requests.append(
                {
                    **actual,
                    "translation_epoch": int(self.active["translation_epoch"]),
                    "speculative": True,
                }
            )
            return
        if self.active["fault_seen"]:
            self._error(cycle, "unexpected_followup_ptw_request", actual=actual)
            return
        if expected is None:
            if self.active["allow_speculative_before_response"] and not self.active["response_seen"]:
                self._record(cycle, "pre_response_ptw_request", actual=actual)
                self._ptw_requests.append({**actual, "translation_epoch": int(getattr(self.env, "translation_epoch", self.active["translation_epoch"])), "speculative": True})
                return
            self._error(cycle, "ptw_request_mismatch", expected=self.active["expected_ptw_requests"], actual=actual)
            return
        record = {**actual, "translation_epoch": int(getattr(self.env, "translation_epoch", self.active["translation_epoch"]))}
        self._ptw_requests.append(record)
        self.active["request_seen"] = True
        request_key = (int(actual["vpn"]), int(actual["s2xlate"]), int(actual["get_gpa"]))
        request_count = next(
            (
                item
                for item in self.active["ptw_request_counts"]
                if (int(item["vpn"]), int(item["s2xlate"]), int(item["get_gpa"])) == request_key
            ),
            None,
        )
        if request_count is None:
            request_count = {"vpn": request_key[0], "s2xlate": request_key[1], "get_gpa": request_key[2], "count": 0}
            self.active["ptw_request_counts"].append(request_count)
        request_count["count"] += 1
        max_requests = self.active["max_ptw_requests_per_key"]
        if max_requests is not None and int(request_count["count"]) > int(max_requests):
            self._error(
                cycle,
                "ptw_request_limit_exceeded",
                actual=actual,
                max_ptw_requests_per_key=int(max_requests),
                request_count=int(request_count["count"]),
            )
        if request_key not in self.active["requested_ptw_request_keys"]:
            self.active["requested_ptw_request_keys"].append(request_key)
        if int(actual["vpn"]) not in self.active["requested_ptw_vpns"]:
            self.active["requested_ptw_vpns"].append(int(actual["vpn"]))
        self._record(cycle, "ptw_request", actual=actual)

    def observe_ptw_response(self, cycle: int, *, vpn: int, s2xlate: int, get_gpa: int, response: dict) -> None:
        if self.active is None:
            return
        actual = {"vpn": int(vpn), "s2xlate": int(s2xlate), "get_gpa": int(get_gpa)}
        if not self._observation_ready(cycle):
            self._record(cycle, "pre_redirect_ptw_response", actual=actual)
            return
        expected = next(
            (
                request
                for request in self.active["expected_ptw_requests"]
                if int(request["vpn"]) == int(actual["vpn"])
                and int(request["s2xlate"]) == int(actual["s2xlate"])
                and int(request["get_gpa"]) == int(actual["get_gpa"])
            ),
            None,
        )
        pending_request_known = any(
            item["vpn"] == int(vpn)
            and item["s2xlate"] == int(s2xlate)
            and item["get_gpa"] == int(get_gpa)
            for item in self._ptw_requests
        )
        if (
            expected is None
            and not pending_request_known
            and self.active["expected_ptw_requests"]
            and not self.active["request_seen"]
        ):
            self._record(cycle, "pre_target_ptw_response", actual=actual)
            return
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
        request_record = None if request_index is None else self._ptw_requests.pop(request_index)
        request_epoch = None if request_record is None else request_record["translation_epoch"]
        if request_record is not None and request_record.get("speculative"):
            self._record(cycle, "stale_speculative_ptw_response", actual=actual)
            return
        if request_epoch is not None and int(request_epoch) != int(self.active["translation_epoch"]):
            self._record(cycle, "stale_ptw_response", response_epoch=request_epoch, actual=actual)
            return
        if expected is None:
            self._error(
                cycle,
                "ptw_response_context_mismatch",
                expected=self.active["expected_ptw_requests"],
                actual=actual,
            )
            return
        self.active["response_seen"] = True
        response_key = (int(actual["vpn"]), int(actual["s2xlate"]), int(actual["get_gpa"]))
        if response_key not in self.active["responded_ptw_request_keys"]:
            self.active["responded_ptw_request_keys"].append(response_key)
        if int(actual["vpn"]) not in self.active["responded_ptw_vpns"]:
            self.active["responded_ptw_vpns"].append(int(actual["vpn"]))

    def observe_fetch_request(self, cycle: int, *, path: str, pa: int) -> None:
        if self.active is None:
            return
        actual_path = str(path)
        actual_pa = int(pa)
        if not self._observation_ready(cycle):
            self._record(cycle, "pre_redirect_fetch_request", path=actual_path, pa=actual_pa)
            return
        if self.active["expected_ptw_requests"] and not self.active["response_seen"]:
            # A phase transition can leave prior ICache requests in flight. They
            # cannot be attributed to the newly armed translation epoch yet.
            self._record(cycle, "pre_response_fetch_request", path=actual_path, pa=actual_pa)
            return
        matching_fetches = [
            item
            for item in self.active["expected_fetches"]
            if int(item["pa"]) == actual_pa and int(item["pa"]) not in self.active["observed_fetch_pas"]
        ]
        if matching_fetches:
            expected = matching_fetches[0]
            self._record(cycle, "fetch_request", page=expected["page"], path=actual_path, pa=actual_pa)
            if actual_path != expected["path"]:
                self._error(
                    cycle,
                    "translated_pa_or_path_mismatch",
                    page=expected["page"],
                    expected_path=expected["path"],
                    actual_path=actual_path,
                    expected_pa=expected["pa"],
                    actual_pa=actual_pa,
                )
                return
            self.active["fetched_pages"].append(int(expected["page"]))
            self.active["observed_fetch_pas"].append(actual_pa)
            self.active["fetch_seen"] = True
            return
        if self.active["expected_path"] == "fault":
            self._record(cycle, "fetch_request", path=actual_path, pa=actual_pa)
            self._error(cycle, "unexpected_fetch_after_fault", path=actual_path, pa=actual_pa)
            return
        matching_out_of_scope_fetches = [
            item
            for item in self.active["allowed_out_of_scope_fetches"]
            if int(item["pa"]) == actual_pa
            and int(item["pa"]) not in self.active["observed_out_of_scope_fetch_pas"]
        ]
        if matching_out_of_scope_fetches:
            expected = matching_out_of_scope_fetches[0]
            self._record(
                cycle,
                "out_of_scope_fetch_request",
                page=expected["page"],
                path=actual_path,
                pa=actual_pa,
            )
            if actual_path != expected["path"]:
                self._error(
                    cycle,
                    "translated_pa_or_path_mismatch",
                    page=expected["page"],
                    expected_path=expected["path"],
                    actual_path=actual_path,
                    expected_pa=expected["pa"],
                    actual_pa=actual_pa,
                )
                return
            self.active["observed_out_of_scope_fetch_pas"].append(actual_pa)
            return
        if self.active["fetch_seen"]:
            return
        expected = self.active["expected_fetches"][0]
        self._record(cycle, "fetch_request", page=expected["page"], path=actual_path, pa=actual_pa)
        if actual_path != expected["path"] or actual_pa != int(expected["pa"]):
            self._error(
                cycle,
                "translated_pa_or_path_mismatch",
                page=expected["page"],
                expected_path=expected["path"],
                actual_path=actual_path,
                expected_pa=expected["pa"],
                actual_pa=actual_pa,
            )
            return
        self.active["fetched_pages"].append(int(expected["page"]))
        self.active["observed_fetch_pas"].append(actual_pa)
        self.active["fetch_seen"] = True

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

    def _read_ptw_request_get_gpa(self, ptw_if) -> int:
        internal_get_gpa = self._read_internal_signal(_ITLB_PTW_REQ_GET_GPA)
        if internal_get_gpa is not None:
            return internal_get_gpa
        return self._read(ptw_if.req_0_bits_get_gpa)

    def observe_cfvec(
        self,
        cycle: int,
        *,
        pc: int,
        exception_bits: dict[int, int],
        cross_page: bool = False,
        folded_pc: Optional[int] = None,
    ) -> None:
        if self.active is None:
            return
        actual_faults = [name for bit, name in _EXCEPTION_BITS.items() if int(exception_bits.get(bit, 0))]
        if not actual_faults:
            if not self._observation_ready(cycle):
                self._record(cycle, "pre_redirect_cfvec_normal", pc=int(pc), cross_page=bool(cross_page))
                return
            if self.active["expected_ptw_requests"] and not self.active["request_seen"]:
                self._record(cycle, "pre_target_cfvec_normal", pc=int(pc), cross_page=bool(cross_page))
                return
            expected_page = next(
                (
                    page
                    for page, outcome in zip(self.active["selected_pages"], self.active["expected_page_outcomes"])
                    if bool(outcome.get("ok", False))
                    and int(outcome["va"]) <= int(pc) < (int(outcome["va"]) & ~0xFFF) + 0x1000
                ),
                None,
            )
            if expected_page is None:
                return
            self._record(cycle, "cfvec_normal", pc=int(pc), page=int(expected_page), cross_page=bool(cross_page))
            self.active["observed_normal_cfvec_count"] += 1
            if int(expected_page) not in self.active["observed_normal_cfvec_pages"]:
                self.active["observed_normal_cfvec_pages"].append(int(expected_page))
            return
        actual_fault = actual_faults[0] if len(actual_faults) == 1 else "multiple"
        if not self._observation_ready(cycle):
            self._record(
                cycle,
                "pre_redirect_cfvec_exception",
                pc=int(pc),
                fault=actual_fault,
                cross_page=bool(cross_page),
            )
            return
        if self.active["expected_ptw_requests"] and not self.active["request_seen"]:
            self._record(
                cycle,
                "pre_target_cfvec_exception",
                pc=int(pc),
                fault=actual_fault,
                cross_page=bool(cross_page),
            )
            return
        expected_fault = self.active["expected_fault"]
        if expected_fault is not None and self.active["fault_seen"]:
            return
        self._record(
            cycle,
            "cfvec_exception",
            pc=int(pc),
            folded_pc=None if folded_pc is None else int(folded_pc),
            fault=actual_fault,
            cross_page=bool(cross_page),
        )
        if expected_fault is None:
            self._error(cycle, "unexpected_cfvec_exception", pc=int(pc), actual_fault=actual_fault)
            return
        va = int(self.active["va"])
        pc_matches = va <= int(pc) < va + int(self.active["payload_size"])
        foldpc_matches = (
            int(pc) == 0
            and folded_pc is not None
            and int(folded_pc) == fold_pc(va)
        )
        if not pc_matches and not foldpc_matches:
            self._error(cycle, "cfvec_exception_pc_mismatch", expected_va=va, actual_pc=int(pc), actual_fault=actual_fault)
            return
        if foldpc_matches:
            self._record(
                cycle,
                "cfvec_exception_foldpc_match",
                expected_va=va,
                folded_pc=int(folded_pc),
                fault=actual_fault,
            )
        if actual_fault != expected_fault:
            self._error(cycle, "cfvec_exception_type_mismatch", expected_fault=expected_fault, actual_fault=actual_fault, pc=int(pc))
            return
        self.active["fault_seen"] = True

    def on_clock_edge(self, cycle: int) -> None:
        if self.active is None or self.env is None:
            return
        if not self.active["fetch_observation_ready"]:
            redirect_count = int(getattr(getattr(self.env, "monitor", None), "redirect_count", 0))
            if redirect_count > int(self.active["redirect_count_at_arm"]):
                self.active["fetch_observation_ready"] = True
                self.active["fetch_observation_not_before"] = int(cycle) + 2
        ptw_if = getattr(self.env, "ptw_if", None)
        request = getattr(self.env.ptw_agent, "get_last_request_expectation", lambda: None)()
        if request is not None and int(request["cycle"]) == int(cycle):
            self.observe_ptw_request(
                cycle,
                vpn=int(request["vpn"]),
                s2xlate=int(request["s2xlate"]),
                get_gpa=int(request["get_gpa"]),
            )
        elif ptw_if is not None and self._read(ptw_if.req_0_valid) and self._read(ptw_if.req_0_ready):
            self.observe_ptw_request(
                cycle,
                vpn=self._read(ptw_if.req_0_bits_vpn),
                s2xlate=self._read(ptw_if.req_0_bits_s2xlate),
                get_gpa=self._read_ptw_request_get_gpa(ptw_if),
            )
        expectation = getattr(self.env.ptw_agent, "get_last_drive_expectation", lambda: None)()
        if (
            ptw_if is not None
            and self._read(ptw_if.resp_valid)
            and self._read(ptw_if.resp_ready)
            and expectation
        ):
            self.observe_ptw_response(
                cycle,
                vpn=int(expectation["vpn"]),
                s2xlate=int(expectation["s2xlate"]),
                get_gpa=int(expectation.get("get_gpa", 0)),
                response=dict(expectation["resp"]),
            )
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
                folded_pc=self._read(observe_if.cfvec_foldpc[slot]),
                exception_bits={bit: self._read(observe_if.cfvec_exception_vec[slot][bit]) for bit in _EXCEPTION_BITS},
                cross_page=bool(self._read(observe_if.cfvec_cross_page_ipf_fix[slot])),
            )

    def assert_complete(self) -> dict:
        if self.active is None:
            raise AssertionError("translation oracle has no armed scenario")
        missing = []
        expected_ptw_request_keys = {
            (int(request["vpn"]), int(request["s2xlate"]), int(request["get_gpa"]))
            for request in self.active["expected_ptw_requests"]
        }
        if not expected_ptw_request_keys.issubset(self.active["requested_ptw_request_keys"]):
            missing.append("ptw_request")
        if not expected_ptw_request_keys.issubset(self.active["responded_ptw_request_keys"]):
            missing.append("ptw_response")
        if self.active["expected_path"] == "fault":
            if not self.active["fault_seen"]:
                missing.append("cfvec_exception")
        else:
            missing_fetches = [
                int(item["pa"])
                for item in self.active["expected_fetches"]
                if int(item["pa"]) not in self.active["observed_fetch_pas"]
            ]
            if missing_fetches:
                missing.append(f"fetch_block_{missing_fetches}")
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

    def set_speculative_request_policy(self, *, allow_before_response: bool) -> None:
        if self.active is not None:
            self.active["allow_speculative_before_response"] = bool(allow_before_response)

    def set_fetch_observation_ready(self, *, ready: bool) -> None:
        if self.active is not None:
            self.active["fetch_observation_ready"] = bool(ready)

    def discard_pending_ptw_responses(self, cycle: int, *, agent_dropped: int) -> None:
        if self.active is None or int(agent_dropped) <= 0:
            return
        pending = len(self._ptw_requests)
        self._ptw_requests.clear()
        self._record(
            cycle,
            "sfence_dropped_ptw_requests",
            oracle_pending=pending,
            agent_dropped=int(agent_dropped),
        )

    def disarm(self) -> None:
        if self.active is not None:
            self._record(getattr(self.env, "current_cycle", 0), "disarmed")
        self.active = None

    def clear(self) -> None:
        self.active = None
        self.errors.clear()
        self.records.clear()
        self._ptw_requests.clear()


__all__ = ["TranslationPermissionOracle"]
