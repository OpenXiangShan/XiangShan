from __future__ import annotations

import csv
import hashlib
import json
import os
import platform
from collections import deque
from dataclasses import asdict, dataclass, field
from functools import cached_property, lru_cache
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Tuple

from .artifact_provenance import file_sha256, load_frontend_build_manifest
from .funcov import (
    CFVEC_SAMPLER_BIN_KEYS,
    TWO_FETCH_SAMPLER_BIN_KEYS,
    sample_cfvec_coverage,
    sample_two_fetch_coverage,
)
from .icache_funcov import (
    ICACHE_MAINPIPE_SAMPLER_BIN_KEYS,
    ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS,
    reset_icache_mainpipe_coverage_state,
    reset_icache_prefetchpipe_coverage_state,
    sample_icache_mainpipe_coverage,
    sample_icache_prefetchpipe_coverage,
)
from .pylib import frontend_pylib_path
from .rvc_decoder import expand_rvc


def _frontend_root() -> Path:
    return Path(__file__).resolve().parents[1]


def default_pilot_csv_path() -> Path:
    return _frontend_root() / "docs" / "03_funcov_model" / "frontend_bt_functional_coverage_pilot.csv"


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


@lru_cache(maxsize=None)
def _file_sha256(path_text: str) -> str:
    return file_sha256(Path(path_text))


def _json_sha256(value: Any) -> str:
    payload = json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


COMPATIBILITY_FIELDS = (
    "dut_source_sha",
    "dut_build_sha256",
    "dut_python_extension_sha256",
    "generated_rtl_sha256",
    "registry_sha256",
    "sampler_sha256",
    "signal_contract_sha256",
    "build_config",
    "toolchain",
)


def _normalize_string_list(values: Optional[Iterable[Any]]) -> List[str]:
    if values is None:
        return []
    result: List[str] = []
    seen: set[str] = set()
    for value in values:
        text = str(value).strip()
        if not text or text in seen:
            continue
        seen.add(text)
        result.append(text)
    return result


UNCACHE_EVENT_SAMPLER_BIN_KEYS = frozenset(
    {
        ("uncache_ordering", "pbmt_nc_pmp_mmio_wait_commit"),
        ("uncache_ordering", "pbmt_nc_non_mmio_no_commit_gate"),
        ("uncache_path_switch", "uncache_to_icache_clean"),
        ("fetch_path_switch", "icache_to_mmio_clean"),
        ("uncache_ordering", "pbmt_io_wait_commit"),
        ("uncache_path_switch", "icache_to_nc_clean"),
    }
)

FUNCTIONAL_COVERAGE_SAMPLER_BIN_KEYS = frozenset(
    set(CFVEC_SAMPLER_BIN_KEYS)
    | set(TWO_FETCH_SAMPLER_BIN_KEYS)
    | set(UNCACHE_EVENT_SAMPLER_BIN_KEYS)
    | set(ICACHE_MAINPIPE_SAMPLER_BIN_KEYS)
    | set(ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS)
)


@dataclass(frozen=True)
class CoverageBinDef:
    bin_id: str
    stage: str
    coverage_type: str
    coverage_group: str
    coverpoint: str
    bin_name: str
    mapped_path: str
    sample_event: str
    observe_object: str
    hit_rule: str
    priority: str
    suggested_testcase: str

    @property
    def key(self) -> Tuple[str, str, str]:
        return (self.coverage_group, self.coverpoint, self.bin_name)

    @property
    def group_bin_key(self) -> Tuple[str, str]:
        return (self.coverage_group, self.bin_name)


@dataclass
class CoverageHit:
    hits: int = 0
    first_cycle: Optional[int] = None
    last_cycle: Optional[int] = None
    evidence: List[dict] = field(default_factory=list)


class FunctionalCoverageRecorder:
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
        target_bin_ids: Optional[Iterable[Any]] = None,
        target_tp_ids: Optional[Iterable[Any]] = None,
        target_testcases: Optional[Iterable[Any]] = None,
    ) -> None:
        defs = list(definitions)
        self.definitions = defs
        self.definition_by_key = {d.key: d for d in defs}
        self.definition_by_group_bin = {d.group_bin_key: d for d in defs}
        self.definition_by_bin_id = {d.bin_id: d for d in defs}
        if len(self.definition_by_key) != len(defs):
            raise ValueError("duplicate functional coverage group/point/bin definition")
        if len(self.definition_by_group_bin) != len(defs):
            raise ValueError("duplicate functional coverage group/bin definition")
        if len(self.definition_by_bin_id) != len(defs):
            raise ValueError("duplicate functional coverage Bin_ID definition")
        self.hits: Dict[Tuple[str, str, str], CoverageHit] = {}
        self.testcase_name = str(testcase_name)
        self.artifact_tag = str(artifact_tag)
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        self.source_csv = str(source_csv) if source_csv is not None else None
        if source_csv is not None and Path(source_csv).resolve() == default_pilot_csv_path().resolve():
            registry_keys = set(self.definition_by_group_bin)
            sampler_keys = set(FUNCTIONAL_COVERAGE_SAMPLER_BIN_KEYS)
            if registry_keys != sampler_keys:
                missing = sorted(registry_keys - sampler_keys)
                stale = sorted(sampler_keys - registry_keys)
                raise ValueError(
                    "canonical functional coverage registry/sampler mismatch: "
                    f"missing_sampler={missing}, stale_sampler={stale}"
                )
        self.waveform_path = str(waveform_path) if waveform_path is not None else None
        self.line_coverage_path = str(line_coverage_path) if line_coverage_path is not None else None
        self.coverage_targets = self._build_coverage_targets(
            target_bin_ids=target_bin_ids,
            target_tp_ids=target_tp_ids,
            target_testcases=target_testcases,
        )
        self.provenance = self._build_provenance()
        self.run_metadata = {
            "run_id": os.getenv("TB_RUN_ID", "").strip() or None,
            "pytest_outcome": os.getenv("TB_RUN_OUTCOME", "unknown").strip().lower() or "unknown",
            "exit_code": self._optional_int(os.getenv("TB_RUN_EXIT_CODE")),
            "checker": {"status": "unknown", "error_count": None, "errors": []},
        }
        self.events_tail: deque[dict] = deque(maxlen=256)
        self.risk_observations: deque[dict] = deque(maxlen=128)
        self.env = None
        self._reset_seen_high = False
        self._reset_release_cycle: Optional[int] = None
        self._last_fetch_path = "icache_seq"
        self._last_fetch_cycle = -1
        self._redirected_fetch_path: Optional[dict] = None
        self._uncache_page_tail_requests: Dict[int, dict] = {}
        self._uncache_active_nc = False
        self._last_uncache_was_nc = False
        self._ifu_last_cfvec: Optional[dict] = None
        self._ifu_redirect_skip_until_cycle: Optional[int] = None
        self._two_fetch_last_fetch_ptr: Optional[tuple[int, int]] = None
        self._two_fetch_expected_ptr_step: Optional[int] = None
        self._two_fetch_waiting_refill = False
        self._two_fetch_ftq_pending = False
        self._two_fetch_last_dual_cycle: Optional[int] = None
        self._dut_signal_cache: Dict[str, Any] = {}
        self._missing_dut_signals: set[str] = set()

    @staticmethod
    def _optional_int(value: Any) -> Optional[int]:
        if value is None or str(value).strip() == "":
            return None
        try:
            return int(value)
        except (TypeError, ValueError):
            return None

    def set_run_metadata(
        self,
        *,
        outcome: Optional[str] = None,
        exit_code: Optional[int] = None,
        checker: Optional[dict] = None,
        run_id: Optional[str] = None,
        extra: Optional[dict] = None,
    ) -> None:
        """Attach pytest/checker outcome to the artifact before it is written."""
        if outcome is not None:
            self.run_metadata["pytest_outcome"] = str(outcome).strip().lower() or "unknown"
        if exit_code is not None:
            self.run_metadata["exit_code"] = self._optional_int(exit_code)
        if run_id is not None:
            self.run_metadata["run_id"] = str(run_id).strip() or None
        if checker is not None:
            self.run_metadata["checker"] = _sanitize(dict(checker))
        if extra is not None:
            self.run_metadata.update(_sanitize(dict(extra)))

    @staticmethod
    def _hit_key_for(item: CoverageBinDef) -> str:
        return f"{item.coverage_group}::{item.coverpoint}::{item.bin_name}"

    def _build_coverage_targets(
        self,
        *,
        target_bin_ids: Optional[Iterable[Any]],
        target_tp_ids: Optional[Iterable[Any]],
        target_testcases: Optional[Iterable[Any]],
    ) -> dict:
        bin_ids = _normalize_string_list(target_bin_ids)
        testcases = _normalize_string_list(target_testcases)
        explicit_testcases = _normalize_string_list(
            [
                *str(os.getenv("TB_FUNCOV_TARGET_TESTCASES", ""))
                .replace(",", " ")
                .replace(";", " ")
                .split(),
                Path(os.getenv("TB_BIN_PATH", "").strip()).stem
                if os.getenv("TB_BIN_PATH", "").strip()
                else "",
            ]
        )
        if explicit_testcases:
            unresolved = sorted(
                testcase
                for testcase in explicit_testcases
                if not any(item.suggested_testcase == testcase for item in self.definitions)
            )
            if unresolved:
                raise ValueError(
                    "functional coverage explicit testcase does not resolve to an active registry bin: "
                    f"{unresolved}"
                )
        if not bin_ids and testcases:
            testcase_set = set(testcases)
            bin_ids = [
                item.bin_id
                for item in self.definitions
                if item.suggested_testcase in testcase_set
            ]
            explicit_testcase_scope = bool(
                os.getenv("TB_BIN_PATH", "").strip()
                or os.getenv("TB_FUNCOV_TARGET_TESTCASES", "").strip()
            )
            if explicit_testcase_scope and not bin_ids:
                raise ValueError(
                    "functional coverage target testcase does not resolve to an active registry bin: "
                    f"{testcases}"
                )
        unknown = sorted(set(bin_ids) - set(self.definition_by_bin_id))
        if unknown:
            raise ValueError(f"unknown functional coverage target Bin_ID(s): {unknown}")
        target_defs = [self.definition_by_bin_id[bin_id] for bin_id in bin_ids]
        return {
            "bin_ids": bin_ids,
            "hit_keys": [self._hit_key_for(item) for item in target_defs],
            "tp_ids": _normalize_string_list(target_tp_ids),
            "testcases": testcases,
        }

    def _build_provenance(self) -> dict:
        frontend_root = _frontend_root()
        repo_root = frontend_root.parents[3]
        build_root = repo_root / "build-frontend"
        manifest_override = os.getenv("TB_DUT_BUILD_MANIFEST", "").strip()
        simulator = os.getenv("TB_FRONTEND_SIM", "verilator")
        build = load_frontend_build_manifest(
            build_root,
            Path(manifest_override) if manifest_override else None,
            simulator=simulator,
            pylib_dir=frontend_pylib_path() / "Frontend",
        )
        source_override = os.getenv("TB_DUT_SOURCE_SHA", "").strip()
        build_config_override = os.getenv("TB_DUT_BUILD_CONFIG", "").strip()
        manifest_status = str(build["build_manifest_status"]).strip().lower()
        manifest_was_valid = manifest_status == "valid"
        manifest_reasons = list(build.get("build_manifest_reasons") or [])
        manifest_source_sha = str(build.get("dut_source_sha") or "").strip()
        dut_source_sha = manifest_source_sha
        dut_source_origin = "build_manifest"
        if source_override:
            if manifest_was_valid and manifest_source_sha:
                if source_override.lower() != manifest_source_sha.lower():
                    manifest_status = "invalid"
                    manifest_reasons.append("source_sha_override_mismatch")
                # A matching environment value is only a consistency check;
                # the manifest remains the authoritative source.
            else:
                # Keep an override for diagnostics, but an invalid/missing
                # manifest still blocks DUT evidence in back-annotation.
                dut_source_sha = source_override
                dut_source_origin = "environment"
        build_config = str(build.get("build_config") or "frontend-default").strip()
        if build_config_override:
            if manifest_was_valid and build_config_override != build_config:
                manifest_status = "invalid"
                manifest_reasons.append("build_config_override_mismatch")
            elif not manifest_was_valid:
                build_config = build_config_override
        definitions_sha256 = _json_sha256([asdict(item) for item in self.definitions])
        sampler_sha256 = _json_sha256(
            {
                "functional_coverage.py": _file_sha256(str(Path(__file__).resolve())),
                "funcov.py": _file_sha256(str((Path(__file__).resolve().parent / "funcov.py"))),
                "icache_funcov.py": _file_sha256(
                    str((Path(__file__).resolve().parent / "icache_funcov.py"))
                ),
            }
        )
        provenance = {
            "dut_source_sha": dut_source_sha,
            "dut_source_origin": dut_source_origin,
            "dut_build_sha256": build["dut_build_sha256"],
            "dut_python_extension_sha256": build["dut_python_extension_sha256"],
            "generated_rtl_sha256": build["generated_rtl_sha256"],
            "registry_sha256": (
                _file_sha256(self.source_csv) if self.source_csv is not None else definitions_sha256
            ),
            "definitions_sha256": definitions_sha256,
            "sampler_sha256": sampler_sha256,
            "signal_contract_sha256": build["signal_contract_sha256"],
            "build_config": build_config,
            "build_manifest_status": manifest_status,
            "build_manifest_sha256": build["build_manifest_sha256"],
            "build_manifest_reasons": manifest_reasons,
            "toolchain": f"python-{platform.python_version()}",
        }
        provenance["compatibility_signature"] = _json_sha256(
            {field: provenance[field] for field in COMPATIBILITY_FIELDS}
        )
        return provenance

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
        target_bin_ids: Optional[Iterable[Any]] = None,
        target_tp_ids: Optional[Iterable[Any]] = None,
        target_testcases: Optional[Iterable[Any]] = None,
    ) -> "FunctionalCoverageRecorder":
        defs: List[CoverageBinDef] = []
        with Path(csv_path).open("r", encoding="utf-8-sig", newline="") as f:
            reader = csv.DictReader(f)
            for row in reader:
                coverpoint = str(row["Coverpoint"]).strip()
                if not coverpoint:
                    continue
                defs.append(
                    CoverageBinDef(
                        bin_id=str(row["Bin_ID"]).strip(),
                        stage=str(row["阶段"]).strip(),
                        coverage_type=str(row["覆盖类型"]).strip(),
                        coverage_group=str(row["Coverage_Group"]).strip(),
                        coverpoint=coverpoint,
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
            target_bin_ids=target_bin_ids,
            target_tp_ids=target_tp_ids,
            target_testcases=target_testcases,
        )

    def attach(self, env) -> None:
        self.env = env

    def raw_path(self) -> Path:
        return self.output_dir / f"{self.artifact_tag}.funcov.json"

    def summary_path(self) -> Path:
        return self.output_dir / f"{self.artifact_tag}.funcov.summary.csv"

    def unhit_path(self) -> Path:
        return self.output_dir / f"{self.artifact_tag}.funcov.unhit.csv"

    def key_hit(self, coverage_group: str, bin_name: str, *, coverpoint: Optional[str] = None) -> bool:
        key = self._coverage_key(str(coverage_group), str(bin_name), coverpoint=coverpoint)
        hit = self.hits.get(key)
        return bool(hit and hit.hits > 0)

    def mark(
        self,
        coverage_group: str,
        bin_name: str,
        cycle: int,
        evidence: Optional[dict] = None,
        *,
        coverpoint: Optional[str] = None,
    ) -> bool:
        key = self._coverage_key(str(coverage_group), str(bin_name), coverpoint=coverpoint)
        if key not in self.definition_by_key:
            raise KeyError(
                "functional coverage sampler attempted an unmodeled bin: "
                f"{key[0]}::{key[1]}::{key[2]}"
            )
        hit = self.hits.setdefault(key, CoverageHit())
        hit.hits += 1
        hit.last_cycle = int(cycle)
        if hit.first_cycle is None:
            hit.first_cycle = int(cycle)
        if evidence is not None and len(hit.evidence) < 8:
            hit.evidence.append(_sanitize(evidence))
        return True

    def _coverage_key(
        self,
        coverage_group: str,
        bin_name: str,
        *,
        coverpoint: Optional[str] = None,
    ) -> Tuple[str, str, str]:
        if coverpoint is not None:
            return (str(coverage_group), str(coverpoint), str(bin_name))

        definition = self.definition_by_group_bin.get((str(coverage_group), str(bin_name)))
        if definition is not None:
            return definition.key
        return (str(coverage_group), "", str(bin_name))

    def handle_event(self, event: Dict[str, Any]) -> None:
        evt = _sanitize(event)
        self.events_tail.append(evt)

        event_type = str(evt.get("type", ""))
        cycle = int(evt.get("cycle", 0))
        payload = evt.get("payload", {}) or {}

        if event_type == "handshake.icache_a":
            if (
                self._redirected_fetch_path is not None
                and self._redirected_fetch_path.get("path") == "mmio_uncache"
                and self._redirected_fetch_path.get("pbmt_nc") is True
            ):
                self.mark(
                    "uncache_path_switch",
                    "uncache_to_icache_clean",
                    cycle,
                    {"event": event_type, **self._redirected_fetch_path},
                )
                self._redirected_fetch_path = None
            if self._redirected_fetch_path is None or self._redirected_fetch_path.get("path") != "icache_seq":
                self._redirected_fetch_path = None
            self._last_fetch_path = "icache_seq"
            self._last_fetch_cycle = cycle
        elif event_type == "handshake.uncache_a":
            address = int(payload.get("address", 0))
            if (
                self._redirected_fetch_path is not None
                and self._redirected_fetch_path.get("path") == "icache_seq"
                and self._uncache_active_nc
            ):
                self.mark(
                    "uncache_path_switch",
                    "icache_to_nc_clean",
                    cycle,
                    {
                        "event": event_type,
                        "address": address,
                        "new_pbmt_nc": True,
                        **self._redirected_fetch_path,
                    },
                )
            if (
                self._redirected_fetch_path is not None
                and self._redirected_fetch_path.get("path") == "icache_seq"
                and self.env is not None
                and self.env.memory.is_mmio(address)
            ):
                self.mark(
                    "fetch_path_switch",
                    "icache_to_mmio_clean",
                    cycle,
                    {"event": event_type, "address": address, **self._redirected_fetch_path},
                )
            self._redirected_fetch_path = None
            self._last_fetch_path = "mmio_uncache"
            self._last_fetch_cycle = cycle
            self._sample_uncache_a_event(cycle, payload)
        elif event_type == "backend.redirect":
            self._redirected_fetch_path = {
                "path": self._last_fetch_path,
                "pbmt_nc": bool(self._last_uncache_was_nc),
            }
            self._ifu_last_cfvec = None
            self._ifu_redirect_skip_until_cycle = cycle + 1
            self._uncache_page_tail_requests.clear()

    def _clear_transient_sampling_state(self) -> None:
        self._last_fetch_path = "icache_seq"
        self._last_fetch_cycle = -1
        self._redirected_fetch_path = None
        self._uncache_page_tail_requests.clear()
        self._uncache_active_nc = False
        self._last_uncache_was_nc = False
        self._ifu_last_cfvec = None
        self._ifu_redirect_skip_until_cycle = None
        self._two_fetch_last_fetch_ptr = None
        self._two_fetch_expected_ptr_step = None
        self._two_fetch_waiting_refill = False
        self._two_fetch_ftq_pending = False
        self._two_fetch_last_dual_cycle = None
        self._two_fetch_last_waylookup_write_state = None
        reset_icache_mainpipe_coverage_state(self)
        reset_icache_prefetchpipe_coverage_state(self)

    def on_cycle(self, cycle: int, env) -> None:
        dut = env.dut
        cycle = int(cycle)
        reset_val = self._read_dut_signal(dut, "reset", 0)
        if reset_val == 1:
            self._reset_seen_high = True
            self._clear_transient_sampling_state()
            return
        elif self._reset_seen_high and self._reset_release_cycle is None:
            self._reset_release_cycle = cycle

        sample_two_fetch_coverage(self, env, cycle)
        sample_cfvec_coverage(self, env, cycle)
        sample_icache_mainpipe_coverage(self, env, cycle)
        sample_icache_prefetchpipe_coverage(self, env, cycle)

        self._sample_ibuffer_contract(dut, cycle)
        self._sample_uncache_cycle_state(dut, cycle, env)

    def _lookup_dut_signal(self, dut, name: str):
        name = str(name)
        if name in self._dut_signal_cache:
            return self._dut_signal_cache[name]
        if name in self._missing_dut_signals:
            return None

        signal = getattr(dut, name, None)
        if signal is None and self._is_registered_internal_signal(name):
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

    @cached_property
    def _registered_internal_signals(self) -> Optional[set[str]]:
        offset_yaml = _frontend_root().parents[3] / "build-frontend" / "pylib" / "Frontend" / "Frontend_offset.yaml"
        if not offset_yaml.exists():
            return None

        signals: set[str] = set()
        prefix = "  - name: "
        try:
            with offset_yaml.open("r", encoding="utf-8") as f:
                for line in f:
                    if line.startswith(prefix):
                        signals.add(line[len(prefix) :].strip())
        except OSError:
            return None
        return signals

    def _is_registered_internal_signal(self, name: str) -> bool:
        registered = self._registered_internal_signals
        return registered is None or str(name) in registered

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
                    "Coverpoint",
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
                    "Coverpoint",
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

        raw_artifacts: list[tuple[Path, dict]] = []
        compatibility_signature: Optional[str] = None
        first_definitions: Optional[list] = None
        for raw_path in raw_list:
            try:
                with raw_path.open("r", encoding="utf-8") as f:
                    data = json.load(f)
            except (OSError, json.JSONDecodeError) as exc:
                raise ValueError(
                    f"invalid functional coverage artifact {raw_path}: {type(exc).__name__}"
                ) from exc
            if not isinstance(data, dict):
                raise ValueError(f"invalid functional coverage artifact root: {raw_path}")
            if data.get("artifact_schema_version") != 2:
                raise ValueError(f"legacy functional coverage artifact cannot be merged: {raw_path}")

            provenance = data.get("provenance")
            if not isinstance(provenance, dict):
                raise ValueError(f"functional coverage artifact lacks provenance: {raw_path}")
            missing_fields = [
                field
                for field in COMPATIBILITY_FIELDS
                if provenance.get(field) is None or str(provenance.get(field)).strip() == ""
            ]
            if missing_fields:
                raise ValueError(
                    "functional coverage artifact lacks compatibility fields: "
                    f"{raw_path}: {missing_fields}"
                )
            recorded_signature = str(provenance.get("compatibility_signature") or "").strip().lower()
            expected_signature = _json_sha256(
                {field: provenance[field] for field in COMPATIBILITY_FIELDS}
            )
            if recorded_signature != expected_signature:
                raise ValueError(
                    "incompatible functional coverage artifacts: "
                    f"{raw_path} signature does not match its provenance"
                )
            if compatibility_signature is None:
                compatibility_signature = recorded_signature
            elif recorded_signature != compatibility_signature:
                raise ValueError(
                    "incompatible functional coverage artifacts: "
                    f"{raw_path} has signature {recorded_signature!r}, "
                    f"expected {compatibility_signature!r}"
                )

            definitions = data.get("definitions")
            if not isinstance(definitions, list):
                raise ValueError(f"functional coverage artifact lacks definitions: {raw_path}")
            recorded_definitions_sha256 = str(
                provenance.get("definitions_sha256") or ""
            ).strip().lower()
            expected_definitions_sha256 = _json_sha256(definitions)
            if recorded_definitions_sha256 != expected_definitions_sha256:
                raise ValueError(
                    "incompatible functional coverage artifacts: "
                    f"{raw_path} definitions do not match provenance"
                )
            if first_definitions is None:
                first_definitions = definitions
            elif definitions != first_definitions:
                raise ValueError(
                    "incompatible functional coverage artifacts: "
                    f"{raw_path} definitions differ from {raw_list[0]}"
                )
            raw_artifacts.append((raw_path, data))

        first = raw_artifacts[0][1]
        first_provenance = first["provenance"]

        defs = [
            CoverageBinDef(
                bin_id=item["bin_id"],
                stage=item["stage"],
                coverage_type=item["coverage_type"],
                coverage_group=item["coverage_group"],
                coverpoint=item.get("coverpoint", ""),
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
        merged.provenance = dict(first_provenance)
        for raw_path, data in raw_artifacts:
            hits = data.get("hits")
            if not isinstance(hits, dict):
                raise ValueError(f"functional coverage artifact has invalid hits: {raw_path}")
            for key_str, hit in hits.items():
                if not isinstance(hit, dict):
                    raise ValueError(f"invalid functional coverage hit record: {raw_path}:{key_str}")
                parts = key_str.split("::")
                if len(parts) != 3:
                    raise ValueError(f"invalid functional coverage hit key: {key_str}")
                group, coverpoint, bin_name = parts
                key = (group, coverpoint, bin_name)
                if key not in merged.definition_by_key:
                    raise ValueError(f"unknown functional coverage hit key: {key_str}")
                target = merged.hits.setdefault(key, CoverageHit())
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

    def _sample_uncache_a_event(self, cycle: int, payload: Dict[str, Any]) -> None:
        addr = int(payload.get("address", 0))
        self._last_uncache_was_nc = bool(self._uncache_active_nc)
        self._uncache_active_nc = False

        for page, tail in self._uncache_page_tail_requests.items():
            if addr == int(page) + 0x1000:
                tail["next_page_requested"] = True
        if addr & 0xFFF == 0xFF8:
            self._uncache_page_tail_requests[addr & ~0xFFF] = {
                "request_addr": addr,
                "request_cycle": cycle,
                "next_page_requested": False,
            }

    def _sample_uncache_cycle_state(self, dut, cycle: int, env) -> None:
        pbmt = self._try_read_dut_signal(
            dut, "Frontend_top.Frontend.inner_ifu.s1_icacheMetaIn_0_itlbPbmt"
        )
        pmp_mmio = self._try_read_dut_signal(
            dut, "Frontend_top.Frontend.inner_ifu.s1_icacheMetaIn_0_pmpMmio"
        )
        state = self._try_read_dut_signal(
            dut, "Frontend_top.Frontend.inner_ifu.uncacheUnit.uncacheState"
        )
        latched_pbmt = self._try_read_dut_signal(
            dut, "Frontend_top.Frontend.inner_ifu.uncacheUnit.itlbPbmt"
        )
        active_pbmt = latched_pbmt if state in {1, 2, 3} and latched_pbmt is not None else pbmt
        can_accept = self._try_read_dut_signal(dut, "Frontend_top.io_backend_canAccept")
        if active_pbmt == 1 and pmp_mmio == 0 and state in {2, 3}:
            self._uncache_active_nc = True
        if active_pbmt == 1 and pmp_mmio == 1 and state == 1:
            self.mark(
                "uncache_ordering",
                "pbmt_nc_pmp_mmio_wait_commit",
                cycle,
                {"event": "ifu_uncache_state", "pbmt": active_pbmt, "pmp_mmio": pmp_mmio, "state": state},
            )
        if active_pbmt == 2 and pmp_mmio == 0 and state == 1:
            self.mark(
                "uncache_ordering",
                "pbmt_io_wait_commit",
                cycle,
                {"event": "ifu_uncache_state", "pbmt": active_pbmt, "pmp_mmio": pmp_mmio, "state": state},
            )
        if active_pbmt == 1 and pmp_mmio == 0 and state == 2 and can_accept == 0:
            self.mark(
                "uncache_ordering",
                "pbmt_nc_non_mmio_no_commit_gate",
                cycle,
                {"event": "ifu_uncache_state", "pbmt": active_pbmt, "pmp_mmio": pmp_mmio, "state": state, "can_accept": can_accept},
            )

    def _sample_ibuffer_contract(self, dut, cycle: int) -> None:
        """Capture alignment/ownership facts without turning them into hits."""
        valid = self._try_read_dut_signal(dut, "Frontend_top.Frontend.inner_ifu.io_toIBuffer_valid")
        enq = self._try_read_dut_signal(dut, "Frontend_top.Frontend.inner_ifu.io_toIBuffer_bits_enqEnable_0")
        if valid is None or enq is None:
            return

        masks: list[int] = []
        for index in range(35):
            value = self._read_first_dut_signal(
                dut,
                (
                    f"Frontend_top.Frontend.inner_ifu.io_toIBuffer_bits_exceptionMask_{index}",
                    f"Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_bits_exceptionMask_{index}",
                    f"Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_exceptionMask_{index}",
                ),
            )
            if value is None:
                break
            masks.append(int(value) & 1)
        if not masks:
            return

        enq_bits = int(enq)
        mask_bits = sum(bit << index for index, bit in enumerate(masks))
        invalid_mask = mask_bits & ~enq_bits
        self.risk_observations.append(
            {
                "cycle": int(cycle),
                "risk": "ibuffer_exception_mask_enq_alignment",
                "valid": int(valid),
                "enq_enable": enq_bits,
                "exception_mask": mask_bits,
                "mask_without_enq": int(invalid_mask),
                "aligned": invalid_mask == 0,
            }
        )

    @staticmethod
    def _circular_distance(newer_flag: int, newer_value: int, older_flag: int, older_value: int, size: int) -> int:
        size = max(1, int(size))
        modulo = size * 2
        newer = (int(newer_flag) & 1) * size + (int(newer_value) % size)
        older = (int(older_flag) & 1) * size + (int(older_value) % size)
        return (newer - older) % modulo

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

        run = dict(self.run_metadata)
        checker = dict(run.get("checker") or {})
        monitor = (stats.get("monitor") or {}) if isinstance(stats, dict) else {}
        if checker.get("status") in {None, "", "unknown"}:
            checker_errors = len(errors) + int(monitor.get("error_count", 0) or 0)
            checker = {
                "status": "pass" if checker_errors == 0 else "fail",
                "error_count": checker_errors,
                "errors": errors[:32],
            }
        run["checker"] = _sanitize(checker)
        run["pytest_outcome"] = str(run.get("pytest_outcome") or "unknown").lower()
        run["exit_code"] = self._optional_int(run.get("exit_code"))

        return {
            "artifact_schema_version": 2,
            "testcase_name": self.testcase_name,
            "artifact_tag": self.artifact_tag,
            "source_csv": self.source_csv,
            "waveform_path": self.waveform_path,
            "line_coverage_path": self.line_coverage_path,
            "coverage_targets": self.coverage_targets,
            "provenance": self.provenance,
            "definitions": [asdict(d) for d in self.definitions],
            "hits": {
                f"{group}::{coverpoint}::{bin_name}": {
                    "bin_id": self.definition_by_key[(group, coverpoint, bin_name)].bin_id,
                    "coverpoint": coverpoint,
                    "hits": hit.hits,
                    "first_cycle": hit.first_cycle,
                    "last_cycle": hit.last_cycle,
                    "evidence": hit.evidence,
                }
                for (group, coverpoint, bin_name), hit in sorted(self.hits.items())
            },
            "summary": self._summary_rows(),
            "unhit": self._unhit_rows(),
            "stats": stats,
            "errors": errors,
            "run": _sanitize(run),
            "outcome": {
                "status": run["pytest_outcome"],
                "exit_code": run["exit_code"],
            },
            "checker": _sanitize(checker),
            "recent_events": list(self.events_tail),
            "risk_observations": list(self.risk_observations),
        }

    def _summary_rows(self) -> List[dict]:
        grouped: Dict[Tuple[str, str], List[CoverageBinDef]] = {}
        for item in self.definitions:
            grouped.setdefault((item.coverage_group, item.coverpoint), []).append(item)

        rows: List[dict] = []
        for coverage_group, coverpoint in sorted(grouped):
            defs = grouped[(coverage_group, coverpoint)]
            hit_defs = [
                d
                for d in defs
                if self.key_hit(d.coverage_group, d.bin_name, coverpoint=d.coverpoint)
            ]
            total = len(defs)
            hit_count = len(hit_defs)
            pct = 0.0 if total == 0 else (100.0 * hit_count / float(total))
            rows.append(
                {
                    "Coverage_Group": coverage_group,
                    "Coverpoint": coverpoint,
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
            if self.key_hit(item.coverage_group, item.bin_name, coverpoint=item.coverpoint):
                continue
            rows.append(
                {
                    "Bin_ID": item.bin_id,
                    "Coverage_Group": item.coverage_group,
                    "Coverpoint": item.coverpoint,
                    "Bin_Name": item.bin_name,
                    "Priority": item.priority,
                    "Stage": item.stage,
                    "Mapped_Path": item.mapped_path,
                    "Suggested_Testcase": item.suggested_testcase,
                }
            )
        return rows
