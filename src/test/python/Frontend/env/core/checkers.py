from __future__ import annotations

from typing import Any, Optional


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
                    "level": "WARNING",
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
        if self._read(ptw_if.resp_valid, 0) != 1:
            return
        expected = getattr(env.ptw_agent, "get_active_drive_expectation", lambda: None)()
        if not expected:
            return
        resp = dict(expected.get("resp", {}))
        addr_low = int(resp.get("s1_addr_low", 0)) & 0x7
        ppn_low = int(resp.get("s1_ppn_low", [0] * 8)[addr_low]) & 0x7
        full_ppn = (int(resp.get("s1_entry_ppn", 0)) << 3) | ppn_low
        self.checked_count += 1
        if int(resp.get("s1_entry_v", 0)) == 1 and int(resp.get("s1_entry_level", 0)) == 0:
            expected_full_ppn = int(resp.get("s1_entry_ppn", 0)) << 3 | ppn_low
            if full_ppn != expected_full_ppn:
                self._record_error(
                    cycle,
                    "ptw_full_ppn_mismatch",
                    {"full_ppn": int(full_ppn), "expected_full_ppn": int(expected_full_ppn)},
                )


class PTWRespInputChecker(_BasePTWChecker):
    _FIELDS = {
        "s2xlate": "resp_bits_s2xlate",
        "get_gpa": "resp_bits_get_gpa",
        "memidx_is_ld": "resp_bits_memidx_is_ld",
        "memidx_is_st": "resp_bits_memidx_is_st",
        "memidx_idx": "resp_bits_memidx_idx",
        "s1_entry_v": "resp_bits_s1_entry_v",
        "s1_entry_ppn": "resp_bits_s1_entry_ppn",
        "s1_entry_level": "resp_bits_s1_entry_level",
        "s1_entry_perm_x": "resp_bits_s1_entry_perm_x",
        "s1_pf": "resp_bits_s1_pf",
        "s1_af": "resp_bits_s1_af",
        "s2_entry_v": "resp_bits_s2_entry_v",
        "s2_entry_ppn": "resp_bits_s2_entry_ppn",
        "s2_gpf": "resp_bits_s2_gpf",
        "s2_gaf": "resp_bits_s2_gaf",
    }

    def on_clock_edge(self, cycle: int) -> None:
        env = self.env
        if env is None or getattr(env, "ptw_if", None) is None:
            return
        ptw_if = env.ptw_if
        if self._read(ptw_if.resp_valid, 0) != 1:
            return
        expected = getattr(env.ptw_agent, "get_active_drive_expectation", lambda: None)()
        if not expected:
            return
        resp = dict(expected.get("resp", {}))
        mismatches = {}
        for payload_key, signal_name in self._FIELDS.items():
            signal = getattr(ptw_if, signal_name, None)
            if signal is None:
                continue
            got = self._read(signal, 0)
            exp = int(resp.get(payload_key, 0))
            if got != exp:
                mismatches[payload_key] = {"expected": exp, "got": int(got)}
        self.checked_count += 1
        if mismatches:
            self._record_error(cycle, "ptw_resp_input_mismatch", mismatches)


__all__ = ["PTWFullPpnChecker", "PTWRespInputChecker"]
