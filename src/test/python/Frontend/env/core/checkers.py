from __future__ import annotations

import logging
from typing import Any, Optional

from ..bundles.ptw import PTWBundle


class _BasePTWChecker:
    def __init__(self) -> None:
        self.env = None
        self.event_sink = None
        self.checked_count = 0
        self.error_count = 0
        self.last_error: Optional[dict] = None

    @staticmethod
    def _read(signal: Any, default: int = 0) -> int:
        try:
            value = getattr(signal, "value", None)
            return default if value is None else int(value)
        except Exception:
            return int(default)

    def bind_env(self, env) -> None:
        self.env = env

    def set_event_sink(self, sink) -> None:
        self.event_sink = sink

    def _record_error(self, cycle: int, reason: str, details: dict) -> None:
        self.error_count += 1
        self.last_error = {
            "cycle": int(cycle),
            "reason": str(reason),
            "details": dict(details),
        }
        if self.event_sink is not None:
            self.event_sink(
                {
                    "type": f"check.{reason}",
                    "source": type(self).__name__,
                    "cycle": int(cycle),
                    "level": "ERROR",
                    "payload": dict(details),
                }
            )

    def on_clock_edge(self, cycle: int) -> None:
        del cycle

    def get_stats(self) -> dict:
        return {
            "checked_count": int(self.checked_count),
            "error_count": int(self.error_count),
            "last_error": None if self.last_error is None else dict(self.last_error),
        }


class PTWFullPpnChecker(_BasePTWChecker):
    def on_clock_edge(self, cycle: int) -> None:
        env = self.env
        if env is None or getattr(env, "ptw_if", None) is None:
            return
        ptw_if = env.ptw_if
        snapshot = getattr(getattr(env, "monitor", None), "cycle_snapshot", None)
        resp_valid = (
            snapshot.ptw["resp_valid"]
            if snapshot is not None and snapshot.cycle == int(cycle)
            else self._read(ptw_if.resp_valid, 0)
        )
        if resp_valid != 1:
            return
        expected = getattr(env.ptw_agent, "get_last_drive_expectation", lambda: None)()
        if not expected:
            return
        resp = dict(expected.get("resp", {}))
        sampled = snapshot.ptw["resp"] if snapshot is not None and snapshot.cycle == int(cycle) else None
        self.checked_count += 1
        if int(resp.get("s1_entry_v", 0)) == 1 and int(resp.get("s1_entry_level", 0)) == 0:
            addr_low = int(resp.get("s1_addr_low", 0)) & 0x7
            expected_full_ppn = (
                int(resp.get("s1_entry_ppn", 0)) << 3
            ) | (int(resp.get("s1_ppn_low", [0] * 8)[addr_low]) & 0x7)
            if sampled is not None:
                full_ppn = (
                    int(sampled["s1_entry_ppn"]) << 3
                ) | (int(sampled["s1_ppn_low"][addr_low]) & 0x7)
            else:
                full_ppn = (
                    self._read(ptw_if.resp_bits_s1_entry_ppn) << 3
                ) | (self._read(ptw_if.resp_bits_s1_ppn_low[addr_low]) & 0x7)
            if full_ppn != expected_full_ppn:
                self._record_error(
                    cycle,
                    "ptw_full_ppn_mismatch",
                    {"full_ppn": int(full_ppn), "expected_full_ppn": int(expected_full_ppn)},
                )
                cursor = getattr(getattr(env, "backend_model", None), "current_golden_cursor", lambda: None)()
                message = (
                    f"PTW full PPN mismatch cycle={int(cycle)} cursor={cursor}: "
                    f"expected=0x{expected_full_ppn:x} got=0x{full_ppn:x}"
                )
                logging.getLogger("env.checkers").error(message)
                raise AssertionError(message)


class PTWRespInputChecker(_BasePTWChecker):
    _SCALAR_FIELDS = tuple(
        name for name in PTWBundle.SIGNAL_BINDINGS if name.startswith("resp_bits_")
    )
    _ARRAY_FIELDS = ("s1_ppn_low", "s1_valididx", "s1_pteidx")

    def on_clock_edge(self, cycle: int) -> None:
        env = self.env
        if env is None or getattr(env, "ptw_if", None) is None:
            return
        ptw_if = env.ptw_if
        monitor = getattr(env, "monitor", None)
        snapshot = getattr(monitor, "cycle_snapshot", None)
        sampled_ptw = snapshot.ptw if snapshot is not None and snapshot.cycle == int(cycle) else None
        resp_valid = sampled_ptw["resp_valid"] if sampled_ptw is not None else self._read(ptw_if.resp_valid, 0)
        if resp_valid != 1:
            return
        # The agent may consume active_resp in this same cycle when resp_ready
        # is high.  last_drive_expectation is the stable snapshot of the
        # response payload driven before that handshake.
        expected = getattr(env.ptw_agent, "get_last_drive_expectation", lambda: None)()
        if not expected:
            return
        resp = dict(expected.get("resp", {}))
        mismatches = {}
        for signal_name in self._SCALAR_FIELDS:
            payload_key = signal_name.removeprefix("resp_bits_")
            got = (
                int(sampled_ptw["resp"][payload_key])
                if sampled_ptw is not None
                else self._read(getattr(ptw_if, signal_name), 0)
            )
            exp = int(resp.get(payload_key, 0))
            if got != exp:
                mismatches[payload_key] = {"expected": exp, "got": int(got)}
        for payload_key in self._ARRAY_FIELDS:
            signals = getattr(ptw_if, f"resp_bits_{payload_key}")
            expected_values = resp.get(payload_key, [0] * 8)
            for idx, signal in enumerate(signals):
                got = (
                    int(sampled_ptw["resp"][payload_key][idx])
                    if sampled_ptw is not None
                    else self._read(signal, 0)
                )
                exp = int(expected_values[idx])
                if got != exp:
                    mismatches[f"{payload_key}[{idx}]"] = {"expected": exp, "got": int(got)}
        self.checked_count += 1
        if mismatches:
            self._record_error(cycle, "ptw_resp_input_mismatch", mismatches)
            cursor = getattr(getattr(env, "backend_model", None), "current_golden_cursor", lambda: None)()
            message = f"PTW response input mismatch cycle={int(cycle)} cursor={cursor}: {mismatches}"
            logging.getLogger("env.checkers").error(message)
            raise AssertionError(message)


__all__ = ["PTWFullPpnChecker", "PTWRespInputChecker"]
