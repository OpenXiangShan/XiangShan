from __future__ import annotations

import csv
import json
from collections import deque
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Tuple

from .funcov import (
    sample_backend_redirect_coverage,
    sample_bpu_subpredictor_coverage,
    sample_bpu_to_ftq_coverage,
    sample_bpu_v3_basic_prediction_coverage,
    sample_cfvec_coverage,
    sample_ftq_coverage,
)
from .rvc_decoder import expand_rvc


def _frontend_root() -> Path:
    return Path(__file__).resolve().parents[1]


def default_pilot_csv_path() -> Path:
    return _frontend_root() / "docs" / "frontend_bt_functional_coverage_pilot.csv"


def _sanitize(value: Any) -> Any:
    if isinstance(value, (str, int, float, bool)) or value is None:
        return value
    if isinstance(value, Path):
        return str(value)
    if isinstance(value, dict):
        return {str(k): _sanitize(v) for k, v in value.items()}
    if isinstance(value, (list, tuple)):
        return [_sanitize(v) for v in value]
    return str(value)


@dataclass(frozen=True)
class CoverageBinDef:
    bin_id: str
    stage: str
    coverage_type: str
    coverage_group: str
    bin_name: str
    mapped_path: str
    sample_event: str
    observe_object: str
    hit_rule: str
    priority: str
    suggested_testcase: str

    @property
    def key(self) -> Tuple[str, str]:
        return (self.coverage_group, self.bin_name)


@dataclass
class CoverageHit:
    hits: int = 0
    first_cycle: Optional[int] = None
    last_cycle: Optional[int] = None
    evidence: List[dict] = field(default_factory=list)


class FunctionalCoverageRecorder:
    _EXCEPTION_BINS = {
        1: "af",
        2: "ill",
        12: "pf",
        19: "hwe",
        20: "gpf",
    }

    def __init__(
        self,
        definitions: Iterable[CoverageBinDef],
        *,
        testcase_name: str,
        artifact_tag: str,
        output_dir: Path,
        source_csv: Optional[Path] = None,
        waveform_path: Optional[Path] = None,
        line_coverage_path: Optional[Path] = None,
    ) -> None:
        defs = list(definitions)
        self.definitions = defs
        self.definition_by_key = {d.key: d for d in defs}
        self.hits: Dict[Tuple[str, str], CoverageHit] = {}
        self.testcase_name = str(testcase_name)
        self.artifact_tag = str(artifact_tag)
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        self.source_csv = str(source_csv) if source_csv is not None else None
        self.waveform_path = str(waveform_path) if waveform_path is not None else None
        self.line_coverage_path = str(line_coverage_path) if line_coverage_path is not None else None
        self.events_tail: deque[dict] = deque(maxlen=256)
        self.env = None
        self._reset_seen_high = False
        self._reset_release_cycle: Optional[int] = None
        self._boot_recorded = False
        self._last_fetch_path = "icache_seq"
        self._last_fetch_cycle = -1
        self._ptw_req_pending = 0
        self._pending_ptw_resp_cycle: Optional[int] = None
        self._backend_block_start_cycle: Optional[int] = None
        self._last_ibuffer_state: Optional[str] = None
        self._last_ftq_state: Optional[str] = None
        self._dut_signal_cache: Dict[str, Any] = {}
        self._missing_dut_signals: set[str] = set()

    @classmethod
    def from_pilot_csv(
        cls,
        csv_path: Path,
        *,
        testcase_name: str,
        artifact_tag: str,
        output_dir: Path,
        waveform_path: Optional[Path] = None,
        line_coverage_path: Optional[Path] = None,
    ) -> "FunctionalCoverageRecorder":
        defs: List[CoverageBinDef] = []
        with Path(csv_path).open("r", encoding="utf-8-sig", newline="") as f:
            reader = csv.DictReader(f)
            for row in reader:
                defs.append(
                    CoverageBinDef(
                        bin_id=str(row["Bin_ID"]).strip(),
                        stage=str(row["阶段"]).strip(),
                        coverage_type=str(row["覆盖类型"]).strip(),
                        coverage_group=str(row["Coverage_Group"]).strip(),
                        bin_name=str(row["Bin_Name"]).strip(),
                        mapped_path=str(row["映射测试点路径"]).strip(),
                        sample_event=str(row["建议采样事件"]).strip(),
                        observe_object=str(row["建议观测对象"]).strip(),
                        hit_rule=str(row["命中判据"]).strip(),
                        priority=str(row["优先级"]).strip(),
                        suggested_testcase=str(row["建议试点用例"]).strip(),
                    )
                )
        return cls(
            defs,
            testcase_name=testcase_name,
            artifact_tag=artifact_tag,
            output_dir=output_dir,
            source_csv=Path(csv_path),
            waveform_path=waveform_path,
            line_coverage_path=line_coverage_path,
        )

    def attach(self, env) -> None:
        self.env = env

    def raw_path(self) -> Path:
        return self.output_dir / f"{self.artifact_tag}.funcov.json"

    def summary_path(self) -> Path:
        return self.output_dir / f"{self.artifact_tag}.funcov.summary.csv"

    def unhit_path(self) -> Path:
        return self.output_dir / f"{self.artifact_tag}.funcov.unhit.csv"

    def key_hit(self, coverage_group: str, bin_name: str) -> bool:
        hit = self.hits.get((str(coverage_group), str(bin_name)))
        return bool(hit and hit.hits > 0)

    def mark(self, coverage_group: str, bin_name: str, cycle: int, evidence: Optional[dict] = None) -> bool:
        key = (str(coverage_group), str(bin_name))
        if key not in self.definition_by_key:
            return False
        hit = self.hits.setdefault(key, CoverageHit())
        hit.hits += 1
        hit.last_cycle = int(cycle)
        if hit.first_cycle is None:
            hit.first_cycle = int(cycle)
        if evidence is not None and len(hit.evidence) < 8:
            hit.evidence.append(_sanitize(evidence))
        return True

    def handle_event(self, event: Dict[str, Any]) -> None:
        evt = _sanitize(event)
        self.events_tail.append(evt)

        event_type = str(evt.get("type", ""))
        cycle = int(evt.get("cycle", 0))
        payload = evt.get("payload", {}) or {}

        if event_type == "handshake.icache_a":
            self._last_fetch_path = "icache_seq"
            self._last_fetch_cycle = cycle
            self.mark("fetch_path_type", "icache_seq", cycle, {"event": event_type, "payload": payload})
        elif event_type == "handshake.uncache_a":
            self._last_fetch_path = "mmio_uncache"
            self._last_fetch_cycle = cycle
            self.mark("fetch_path_type", "mmio_uncache", cycle, {"event": event_type, "payload": payload})
        elif event_type == "handshake.ptw_req":
            self._ptw_req_pending += 1
            self.mark("itlb_result_type", "miss", cycle, {"event": event_type, "payload": payload})
        elif event_type == "handshake.ptw_resp":
            self._pending_ptw_resp_cycle = cycle
        elif event_type == "backend.redirect":
            redirect_bin = self._classify_redirect_reason(str(payload.get("reason", "")))
            self.mark("redirect_type", redirect_bin, cycle, {"event": event_type, "payload": payload})
        elif event_type == "backend.can_accept":
            self._handle_can_accept_event(cycle, payload)

    def on_cycle(self, cycle: int, env) -> None:
        dut = env.dut
        cycle = int(cycle)
        reset_val = self._read_dut_signal(dut, "reset", 0)
        if reset_val == 1:
            self._reset_seen_high = True
        elif self._reset_seen_high and self._reset_release_cycle is None:
            self._reset_release_cycle = cycle

        sample_cfvec_coverage(self, env, cycle)
        sample_bpu_subpredictor_coverage(self, env, cycle)
        sample_bpu_v3_basic_prediction_coverage(self, env, cycle)
        sample_bpu_to_ftq_coverage(self, env, cycle)

        if (
            self._read_dut_signal(dut, "io_ptw_req_0_valid", 0) == 1
            and self._read_dut_signal(dut, "io_ptw_req_0_ready", 0) == 1
        ):
            self._ptw_req_pending += 1
            self.mark(
                "itlb_result_type",
                "miss",
                cycle,
                {
                    "event": "cycle.ptw_req",
                    "vpn": self._read_dut_signal(dut, "io_ptw_req_0_bits_vpn", 0),
                },
            )

        self._sample_ibuffer_state(dut, cycle, env)
        sample_ftq_coverage(self, env, cycle)

        if self._read_dut_signal(dut, "io_backend_toFtq_redirect_valid", 0) == 1:
            sample_backend_redirect_coverage(self, env, cycle)
            self._sample_backend_redirect_faults(env, dut, cycle)

        if self._read_dut_signal(dut, "io_ptw_resp_valid", 0) == 1:
            self._sample_ptw_response(dut, cycle)
        elif self._pending_ptw_resp_cycle == cycle:
            self._sample_ptw_response(dut, cycle)

    def _lookup_dut_signal(self, dut, name: str):
        name = str(name)
        if name in self._dut_signal_cache:
            return self._dut_signal_cache[name]
        if name in self._missing_dut_signals:
            return None

        signal = getattr(dut, name, None)
        if signal is None:
            getter = getattr(dut, "GetInternalSignal", None)
            if callable(getter):
                try:
                    signal = getter(name)
                except Exception:
                    signal = None
        if signal is None:
            self._missing_dut_signals.add(name)
            return None

        self._dut_signal_cache[name] = signal
        return signal

    def _read_dut_signal(self, dut, name: str, default: int = 0) -> int:
        signal = self._lookup_dut_signal(dut, str(name))
        if signal is None:
            return int(default)
        value = getattr(signal, "value", None)
        if value is None:
            return int(default)
        return int(value)

    def _try_read_dut_signal(self, dut, name: str) -> Optional[int]:
        signal = self._lookup_dut_signal(dut, str(name))
        if signal is None:
            return None
        value = getattr(signal, "value", None)
        if value is None:
            return None
        try:
            return int(value)
        except Exception:
            return None

    def _read_first_dut_signal(self, dut, names: Iterable[str]) -> Optional[int]:
        for name in names:
            value = self._try_read_dut_signal(dut, str(name))
            if value is not None:
                return int(value)
        return None

    def _translate_fetch_addr(self, env, va: int) -> tuple[Optional[int], dict]:
        if env is None or getattr(env, "page_table", None) is None:
            return int(va), {"mode": "bare", "va": int(va), "pa": int(va), "ok": True}
        pa, ok, info = env.page_table.translate(int(va))
        meta = dict(info or {})
        meta["va"] = int(va)
        meta["ok"] = bool(ok)
        if ok:
            meta["pa"] = int(pa)
            return int(pa), meta
        return None, meta

    def _read_expected_fetch_raw(self, env, pc: int, size: int) -> tuple[Optional[int], dict]:
        if env is None or getattr(env, "memory", None) is None:
            return None, {"ok": False, "reason": "no_memory"}
        value = 0
        last_meta: dict = {"ok": True, "mode": "bare", "va": int(pc), "pa": int(pc)}
        for off in range(int(size)):
            pa, meta = self._translate_fetch_addr(env, int(pc) + int(off))
            last_meta = meta
            if pa is None:
                return None, meta
            value |= (int(env.memory.read_u8(int(pa))) & 0xFF) << (8 * int(off))
        return int(value), last_meta

    def _recover_unavailable_instr(self, env, pc: int, instr: int, is_rvc: bool, ex_sum: int) -> int:
        if int(instr) != 0:
            return int(instr)
        fetch_size = 2 if bool(is_rvc) else 4
        raw_fetch, fetch_meta = self._read_expected_fetch_raw(env, int(pc), fetch_size)
        if raw_fetch is None or not bool(fetch_meta.get("ok", False)):
            return int(instr)
        if bool(is_rvc):
            raw16 = int(raw_fetch) & 0xFFFF
            try:
                return int(expand_rvc(raw16)) & 0xFFFFFFFF
            except ValueError:
                return int(instr)
        return int(raw_fetch) & 0xFFFFFFFF

    def write_artifacts(self) -> dict:
        raw = self._raw_dict()
        raw_path = self.raw_path()
        summary_path = self.summary_path()
        unhit_path = self.unhit_path()

        raw_path.write_text(json.dumps(raw, ensure_ascii=False, indent=2), encoding="utf-8")

        with summary_path.open("w", encoding="utf-8", newline="") as f:
            writer = csv.DictWriter(
                f,
                fieldnames=[
                    "Coverage_Group",
                    "Total_Bins",
                    "Hit_Bins",
                    "Coverage_Pct",
                    "Hit_Bin_Names",
                    "Unhit_Bin_Names",
                ],
            )
            writer.writeheader()
            for row in self._summary_rows():
                writer.writerow(row)

        with unhit_path.open("w", encoding="utf-8", newline="") as f:
            writer = csv.DictWriter(
                f,
                fieldnames=[
                    "Bin_ID",
                    "Coverage_Group",
                    "Bin_Name",
                    "Priority",
                    "Stage",
                    "Mapped_Path",
                    "Suggested_Testcase",
                ],
            )
            writer.writeheader()
            for row in self._unhit_rows():
                writer.writerow(row)

        return {
            "raw_path": str(raw_path),
            "summary_path": str(summary_path),
            "unhit_path": str(unhit_path),
        }

    @classmethod
    def merge_raw_files(
        cls,
        raw_paths: Iterable[Path],
        *,
        artifact_tag: str,
        output_dir: Path,
    ) -> "FunctionalCoverageRecorder":
        raw_list = [Path(p) for p in raw_paths]
        if not raw_list:
            raise ValueError("merge_raw_files requires at least one raw coverage json")

        with raw_list[0].open("r", encoding="utf-8") as f:
            first = json.load(f)

        defs = [
            CoverageBinDef(
                bin_id=item["bin_id"],
                stage=item["stage"],
                coverage_type=item["coverage_type"],
                coverage_group=item["coverage_group"],
                bin_name=item["bin_name"],
                mapped_path=item["mapped_path"],
                sample_event=item["sample_event"],
                observe_object=item["observe_object"],
                hit_rule=item["hit_rule"],
                priority=item["priority"],
                suggested_testcase=item["suggested_testcase"],
            )
            for item in first["definitions"]
        ]

        merged = cls(
            defs,
            testcase_name="merged",
            artifact_tag=artifact_tag,
            output_dir=output_dir,
            source_csv=Path(first["source_csv"]) if first.get("source_csv") else None,
        )
        for raw_path in raw_list:
            with raw_path.open("r", encoding="utf-8") as f:
                data = json.load(f)
            for key_str, hit in data.get("hits", {}).items():
                group, bin_name = key_str.split("::", 1)
                target = merged.hits.setdefault((group, bin_name), CoverageHit())
                target.hits += int(hit.get("hits", 0))
                first_cycle = hit.get("first_cycle")
                if first_cycle is not None:
                    target.first_cycle = first_cycle if target.first_cycle is None else min(int(target.first_cycle), int(first_cycle))
                last_cycle = hit.get("last_cycle")
                if last_cycle is not None:
                    target.last_cycle = last_cycle if target.last_cycle is None else max(int(target.last_cycle), int(last_cycle))
                for item in hit.get("evidence", []):
                    if len(target.evidence) >= 8:
                        break
                    target.evidence.append(item)
        return merged

    def _handle_can_accept_event(self, cycle: int, payload: Dict[str, Any]) -> None:
        value = int(payload.get("value", 1))
        if value == 0:
            if self._backend_block_start_cycle is None:
                self._backend_block_start_cycle = cycle
            return

        if self._backend_block_start_cycle is None:
            return

        blocked_cycles = max(0, cycle - self._backend_block_start_cycle)
        if 0 < blocked_cycles <= 32:
            self.mark(
                "backend_accept_mode",
                "all_block_short",
                cycle,
                {"blocked_cycles": blocked_cycles, "event": "backend.can_accept"},
            )
        self._backend_block_start_cycle = None

    def _sample_ibuffer_state(self, dut, cycle: int, env) -> None:
        if self._reset_release_cycle is None:
            return

        ibuf_size = 48
        num_valid = self._read_first_dut_signal(
            dut,
            (
                "Frontend_top.Frontend.inner_ibuffer.numValid",
                "Frontend_top.Frontend.inner_ibuffer_numValid",
                "Frontend_top.Frontend.ibuffer.numValid",
                "Frontend_top.Frontend.ibuffer_numValid",
            ),
        )
        ibuf_full = self._try_read_dut_signal(dut, "io_frontendInfo_ibufFull")
        valid_outputs = sum(
            1 for slot in range(8) if self._read_dut_signal(dut, f"io_backend_cfVec_{slot}_valid", 0) == 1
        )

        if num_valid is None:
            evidence = {
                "event": "ibuffer_cfvec_proxy",
                "num_valid": None,
                "valid_outputs": int(valid_outputs),
                "size": int(ibuf_size),
            }
            if self._boot_recorded and valid_outputs == 0:
                self._mark_ibuffer_state("empty", cycle, evidence)
            if self._backend_block_start_cycle is not None and valid_outputs >= 6:
                self._mark_ibuffer_state("near_full", cycle, evidence)
            if self._backend_block_start_cycle is not None and valid_outputs >= 8:
                self._mark_ibuffer_state("full", cycle, evidence)
            if ibuf_full == 1:
                self._mark_ibuffer_state("near_full", cycle, {"event": "io_frontendInfo_ibufFull"})
                self._mark_ibuffer_state("full", cycle, {"event": "io_frontendInfo_ibufFull"})
            return

        evidence = {
            "event": "ibuffer_num_valid",
            "num_valid": int(num_valid),
            "valid_outputs": int(valid_outputs),
            "size": int(ibuf_size),
        }
        if int(num_valid) <= 0:
            self._mark_ibuffer_state("empty", cycle, evidence)
        if self._backend_block_start_cycle is not None and int(num_valid) >= 32:
            self._mark_ibuffer_state("near_full", cycle, evidence)
        if ibuf_full == 1 or int(num_valid) >= ibuf_size - 1:
            self._mark_ibuffer_state("full", cycle, {**evidence, "ibuf_full": int(ibuf_full or 0)})

    def _mark_ibuffer_state(self, state: str, cycle: int, evidence: dict) -> None:
        self._last_ibuffer_state = str(state)
        self.mark("ibuffer_state", state, cycle, evidence)
        if state in {"near_full", "full"} and self._backend_block_start_cycle is not None:
            self.mark(
                "ibuffer_state_x_backend_mode",
                "near_full_x_all_block",
                cycle,
                {
                    **evidence,
                    "backend_block_start_cycle": int(self._backend_block_start_cycle),
                },
            )

    def _mark_ftq_state(self, state: str, cycle: int, evidence: dict) -> None:
        self._last_ftq_state = str(state)
        self.mark("ftq_queue_state", state, cycle, evidence)

    @staticmethod
    def _circular_distance(newer_flag: int, newer_value: int, older_flag: int, older_value: int, size: int) -> int:
        size = max(1, int(size))
        modulo = size * 2
        newer = (int(newer_flag) & 1) * size + (int(newer_value) % size)
        older = (int(older_flag) & 1) * size + (int(older_value) % size)
        return (newer - older) % modulo

    def _sample_exception_slot(self, dut, base: str, slot: int, pc: int, cycle: int, fetch_path: str) -> None:
        for cause, bin_name in self._EXCEPTION_BINS.items():
            if self._read_dut_signal(dut, f"{base}bits_exceptionVec_{cause}", 0) != 1:
                continue
            evidence = {"slot": slot, "pc": pc, "cause": cause}
            self.mark("frontend_exception_type", bin_name, cycle, evidence)
            if fetch_path == "icache_seq" and bin_name == "pf":
                self.mark("fetch_path_x_exception", "icache_x_pf", cycle, evidence)
            if fetch_path == "mmio_uncache" and bin_name == "af":
                self.mark("fetch_path_x_exception", "mmio_x_af", cycle, evidence)

    def _sample_ptw_response(self, dut, cycle: int) -> None:
        entry_v = self._read_dut_signal(dut, "io_ptw_resp_bits_s1_entry_v", 0)
        perm_x = self._read_dut_signal(dut, "io_ptw_resp_bits_s1_entry_perm_x", 0)
        resp_pf = self._read_dut_signal(dut, "io_ptw_resp_bits_s1_pf", 0)
        resp_af = self._read_dut_signal(dut, "io_ptw_resp_bits_s1_af", 0)
        if resp_pf == 1 or resp_af == 1 or entry_v == 0 or perm_x == 0:
            self.mark(
                "ptw_resp_type",
                "pf",
                cycle,
                {"entry_v": entry_v, "perm_x": perm_x, "resp_pf": resp_pf, "resp_af": resp_af},
            )
        else:
            self.mark(
                "ptw_resp_type",
                "leaf_pte",
                cycle,
                {"entry_v": entry_v, "perm_x": perm_x, "resp_pf": resp_pf, "resp_af": resp_af},
            )
            if self._ptw_req_pending > 0:
                self.mark("itlb_ptw_flow", "miss_walk_refill", cycle, {"pending_reqs": self._ptw_req_pending})
                self.mark("itlb_state_x_ptw_resp", "single_miss_x_leaf_pte", cycle, {"pending_reqs": self._ptw_req_pending})
        self._ptw_req_pending = max(0, self._ptw_req_pending - 1)
        self._pending_ptw_resp_cycle = None

    def _sample_backend_redirect_faults(self, env, dut, cycle: int) -> None:
        target_pc = self._read_dut_signal(dut, "io_backend_toFtq_redirect_bits_target", 0)
        evidence = {
            "event": "backend_redirect_fault",
            "target_pc": int(target_pc),
        }
        backend_ipf = self._read_dut_signal(dut, "io_backend_toFtq_redirect_bits_backendIPF", 0)
        backend_iaf = self._read_dut_signal(dut, "io_backend_toFtq_redirect_bits_backendIAF", 0)
        backend_igpf = self._read_dut_signal(dut, "io_backend_toFtq_redirect_bits_backendIGPF", 0)
        if backend_ipf == 1:
            self.mark("frontend_exception_type", "pf", cycle, evidence)
            self.mark("fetch_path_x_exception", "icache_x_pf", cycle, evidence)
        if backend_iaf == 1:
            self.mark("frontend_exception_type", "af", cycle, evidence)
            if env.memory.is_mmio(int(target_pc)):
                self.mark("fetch_path_x_exception", "mmio_x_af", cycle, evidence)
        if backend_igpf == 1:
            self.mark("frontend_exception_type", "gpf", cycle, evidence)

    def _infer_fetch_path(self, env, pc: int, cycle: int) -> str:
        if env.memory.is_mmio(int(pc)):
            return "mmio_uncache"
        if self._last_fetch_path == "mmio_uncache" and (cycle - self._last_fetch_cycle) <= 32:
            return "mmio_uncache"
        return "icache_seq"

    @staticmethod
    def _classify_redirect_reason(reason: str) -> str:
        lowered = reason.lower()
        if "memvio" in lowered:
            return "memVio"
        if "interrupt" in lowered:
            return "interrupt"
        return "ctrl"

    def _raw_dict(self) -> dict:
        stats = {}
        errors: List[dict] = []
        if self.env is not None:
            try:
                stats = _sanitize(self.env.get_stats())
            except Exception:
                stats = {}
            try:
                errors = _sanitize(self.env.get_errors())
            except Exception:
                errors = []

        return {
            "testcase_name": self.testcase_name,
            "artifact_tag": self.artifact_tag,
            "source_csv": self.source_csv,
            "waveform_path": self.waveform_path,
            "line_coverage_path": self.line_coverage_path,
            "definitions": [asdict(d) for d in self.definitions],
            "hits": {
                f"{group}::{bin_name}": {
                    "bin_id": self.definition_by_key[(group, bin_name)].bin_id,
                    "hits": hit.hits,
                    "first_cycle": hit.first_cycle,
                    "last_cycle": hit.last_cycle,
                    "evidence": hit.evidence,
                }
                for (group, bin_name), hit in sorted(self.hits.items())
            },
            "summary": self._summary_rows(),
            "unhit": self._unhit_rows(),
            "stats": stats,
            "errors": errors,
            "recent_events": list(self.events_tail),
        }

    def _summary_rows(self) -> List[dict]:
        grouped: Dict[str, List[CoverageBinDef]] = {}
        for item in self.definitions:
            grouped.setdefault(item.coverage_group, []).append(item)

        rows: List[dict] = []
        for coverage_group in sorted(grouped):
            defs = grouped[coverage_group]
            hit_defs = [d for d in defs if self.key_hit(d.coverage_group, d.bin_name)]
            total = len(defs)
            hit_count = len(hit_defs)
            pct = 0.0 if total == 0 else (100.0 * hit_count / float(total))
            rows.append(
                {
                    "Coverage_Group": coverage_group,
                    "Total_Bins": total,
                    "Hit_Bins": hit_count,
                    "Coverage_Pct": f"{pct:.2f}",
                    "Hit_Bin_Names": ",".join(d.bin_name for d in hit_defs),
                    "Unhit_Bin_Names": ",".join(d.bin_name for d in defs if d not in hit_defs),
                }
            )
        return rows

    def _unhit_rows(self) -> List[dict]:
        rows: List[dict] = []
        for item in self.definitions:
            if self.key_hit(item.coverage_group, item.bin_name):
                continue
            rows.append(
                {
                    "Bin_ID": item.bin_id,
                    "Coverage_Group": item.coverage_group,
                    "Bin_Name": item.bin_name,
                    "Priority": item.priority,
                    "Stage": item.stage,
                    "Mapped_Path": item.mapped_path,
                    "Suggested_Testcase": item.suggested_testcase,
                }
            )
        return rows
