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

from ..runtime.artifact_provenance import (
    file_sha256,
    frontend_build_manifest_path,
    frontend_simulator,
    load_frontend_build_manifest,
)
from . import (
    CFVEC_SAMPLER_BIN_KEYS,
    IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS,
    IFU_CFVEC_SAMPLER_BIN_KEYS,
    OWNER_V3_SAMPLER_BIN_KEYS,
    MMIO_V3_SAMPLER_BIN_KEYS,
    MMIO_NC_OWNER_SAMPLER_BIN_KEYS,
    INSTR_UNCACHE_OWNER_SAMPLER_BIN_KEYS,
    handle_mmio_v3_checked_event,
    handle_owner_v3_event,
    initialize_ifu_cacheable_pipeline_state,
    initialize_mmio_v3_coverage_state,
    initialize_mmio_nc_owner_coverage_state,
    reset_ifu_cacheable_pipeline_state,
    reset_mmio_v3_coverage_state,
    reset_mmio_nc_owner_coverage_state,
    sample_ifu_cacheable_pipeline_coverage,
    sample_mmio_v3_coverage,
    sample_mmio_nc_owner_coverage,
    sample_cfvec_coverage,
)
from .py.ifu.compact_funcov import _sample_uncache_half_isolation
from .py.ftq.sampler import (
    TWO_FETCH_SAMPLER_BIN_KEYS,
    initialize_ftq_coverage_state,
    reset_ftq_coverage_state,
    sample_two_fetch_coverage,
)
from .py.ifu.owner_v3_funcov import derive_owner_v3_from_source
from .py.icache import (
    ICACHE_MISSUNIT_SAMPLER_BIN_KEYS,
    ICACHE_MAINPIPE_SAMPLER_BIN_KEYS,
    ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS,
    ICACHE_WAYLOOKUP_SAMPLER_BIN_KEYS,
    ICACHE_HITMISS_SAMPLER_BIN_KEYS,
    reset_icache_missunit_coverage_state,
    reset_icache_mainpipe_coverage_state,
    reset_icache_prefetchpipe_coverage_state,
    sample_icache_missunit_coverage,
    sample_icache_mainpipe_coverage,
    sample_icache_prefetchpipe_coverage,
    reset_icache_waylookup_coverage_state,
    sample_icache_waylookup_coverage,
    reset_icache_hitmiss_coverage_state,
    sample_icache_hitmiss_coverage,
)
from ..runtime.pylib import frontend_build_root_path, frontend_pylib_path
from ..support.rvc_decoder import expand_rvc


def _frontend_root() -> Path:
    return Path(__file__).resolve().parents[2]


def default_pilot_csv_path() -> Path:
    return _frontend_root() / "docs" / "03_funcov_model" / "frontend_bt_functional_coverage_pilot.csv"


def _decode_signal_inventory_name(value: str) -> str:
    raw = str(value).strip()
    if len(raw) >= 2 and raw[0] == raw[-1] == '"':
        try:
            return str(json.loads(raw))
        except ValueError:
            return raw
    if len(raw) >= 2 and raw[0] == raw[-1] == "'":
        return raw[1:-1].replace("''", "'")
    return raw


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


def funcov_sampler_paths() -> dict[str, Path]:
    """Return every source file that defines the canonical sampler contract."""

    root = Path(__file__).resolve().parent
    return {
        "funcov/recorder.py": root / "recorder.py",
        "funcov/__init__.py": root / "__init__.py",
        "funcov/py/ftq/sampler.py": root / "py" / "ftq" / "sampler.py",
        "funcov/py/ftq/two_fetch_funcov.py": root / "py" / "ftq" / "two_fetch_funcov.py",
        "funcov/py/ftq/ftq_request_funcov.py": root / "py" / "ftq" / "ftq_request_funcov.py",
        "funcov/py/ftq/waylookup_funcov.py": root / "py" / "ftq" / "waylookup_funcov.py",
        "funcov/py/ftq/mainpipe_funcov.py": root / "py" / "ftq" / "mainpipe_funcov.py",
        "funcov/py/ftq/ifu_delivery_funcov.py": root / "py" / "ftq" / "ifu_delivery_funcov.py",
        "funcov/py/ftq/checker_funcov.py": root / "py" / "ftq" / "checker_funcov.py",
        "funcov/py/icache/__init__.py": root / "py" / "icache" / "__init__.py",
        "funcov/py/icache/signal_contract.py": root / "py" / "icache" / "signal_contract.py",
        "funcov/py/icache/icache_mainpipe_funcov.py": root / "py" / "icache" / "icache_mainpipe_funcov.py",
        "funcov/py/icache/icache_prefetchpipe_funcov.py": root / "py" / "icache" / "icache_prefetchpipe_funcov.py",
        "funcov/py/icache/icache_missunit_funcov.py": root / "py" / "icache" / "icache_missunit_funcov.py",
        "funcov/py/icache/icache_waylookup_funcov.py": root / "py" / "icache" / "icache_waylookup_funcov.py",
        "funcov/py/icache/icache_hitmiss_funcov.py": root / "py" / "icache" / "icache_hitmiss_funcov.py",
        "funcov/py/ifu/sampler.py": root / "py" / "ifu" / "sampler.py",
        "funcov/py/ifu/cfvec_funcov.py": root / "py" / "ifu" / "cfvec_funcov.py",
        "funcov/py/ifu/compact_funcov.py": root / "py" / "ifu" / "compact_funcov.py",
        "funcov/py/ifu/owner_v3_funcov.py": root / "py" / "ifu" / "owner_v3_funcov.py",
        "funcov/py/ifu/mmio_v3_funcov.py": root / "py" / "ifu" / "mmio_v3_funcov.py",
        "funcov/py/ifu/mmio_nc_owner_funcov.py": root / "py" / "ifu" / "mmio_nc_owner_funcov.py",
        "funcov/py/ifu/cacheable_pipeline_funcov.py": root / "py" / "ifu" / "cacheable_pipeline_funcov.py",
        "funcov/toffee_bridge.py": root / "toffee_bridge.py",
        **{
            f"funcov/py/{path.relative_to(root / 'py').as_posix()}": path
            for path in sorted((root / "py").rglob("*_toffee.py"))
        },
    }


def current_funcov_sampler_sha256() -> str:
    return _json_sha256(
        {label: _file_sha256(str(path)) for label, path in funcov_sampler_paths().items()}
    )


def verification_environment_paths() -> dict[str, Path]:
    """Return executable verification sources that affect DUT evidence."""

    frontend_root = _frontend_root()
    env_root = frontend_root / "env"
    paths = [
        path
        for path in env_root.rglob("*")
        if path.is_file() and path.suffix in {".py", ".sv"}
    ]
    paths.extend(
        path
        for path in (
            frontend_root / "Frontend_api.py",
            frontend_root / "Frontend_env.py",
            frontend_root / "conftest.py",
            frontend_root / "tests" / "conftest.py",
        )
        if path.is_file()
    )
    return {
        path.relative_to(frontend_root).as_posix(): path
        for path in sorted(set(paths))
    }


def current_verification_environment_sha256() -> str:
    return _json_sha256(
        {
            label: _file_sha256(str(path))
            for label, path in verification_environment_paths().items()
        }
    )


COMPATIBILITY_FIELDS = (
    "simulator",
    "dut_source_sha",
    "implementation_sha",
    "design_baseline_sha",
    "source_sha_override",
    "source_delta_sha256",
    "source_delta_files",
    "source_delta_policy",
    "dut_build_sha256",
    "dut_python_extension_sha256",
    "generated_rtl_sha256",
    "registry_sha256",
    "sampler_sha256",
    "sampler_domains",
    "verification_env_sha256",
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


def _configured_sampler_domains() -> frozenset[str]:
    """Return the explicitly enabled functional-coverage sampler domains."""
    raw = os.getenv("TB_FUNCOV_SAMPLER_DOMAINS", "").strip().lower()
    if not raw:
        return frozenset({"all"})
    domains = {
        token
        for token in raw.replace(",", " ").replace(";", " ").split()
        if token
    }
    return frozenset(domains or {"all"})


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
    | set(IFU_CFVEC_SAMPLER_BIN_KEYS)
    | set(IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS)
    | set(OWNER_V3_SAMPLER_BIN_KEYS)
    | set(MMIO_V3_SAMPLER_BIN_KEYS)
    | set(MMIO_NC_OWNER_SAMPLER_BIN_KEYS)
    | set(INSTR_UNCACHE_OWNER_SAMPLER_BIN_KEYS)
    | set(TWO_FETCH_SAMPLER_BIN_KEYS)
    | set(UNCACHE_EVENT_SAMPLER_BIN_KEYS)
    | set(ICACHE_MAINPIPE_SAMPLER_BIN_KEYS)
    | set(ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS)
    | set(ICACHE_MISSUNIT_SAMPLER_BIN_KEYS)
    | set(ICACHE_WAYLOOKUP_SAMPLER_BIN_KEYS)
    | set(ICACHE_HITMISS_SAMPLER_BIN_KEYS)
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


class FrontendFuncovSampleHub:
    """Shared sampling hub for native Toffee funcov.

    Owns registry definitions, cycle snapshot, cross-cycle state, and event
    routing used by evaluate/native CovGroup sampling.  This class is not the
    legacy hit-ledger writer; formal runs must not treat it as a signoff
    artifact backend.
    """

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
        self.testcase_name = str(testcase_name)
        self.artifact_tag = str(artifact_tag)
        self.sampler_domains = _configured_sampler_domains()
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
        self.contract_errors: deque[dict] = deque(maxlen=128)
        self._contract_error_keys: deque[tuple[str, int, str]] = deque(maxlen=128)
        self.env = None
        self.toffee_sink = None
        self.toffee_direct_domains: set[str] = set()
        self.toffee_event_models = []
        self.toffee_owner_model = None
        self.toffee_uncache_model = None
        self._cycle_snapshot_cycle: Optional[int] = None
        self._cycle_snapshot_values: Dict[str, Optional[int]] = {}
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
        self._ifu_ibuffer_alignment_pending: Optional[dict] = None
        self._two_fetch_last_fetch_ptr: Optional[tuple[int, int]] = None
        self._two_fetch_expected_ptr_step: Optional[int] = None
        self._two_fetch_waiting_refill = False
        self._two_fetch_ftq_pending = False
        self._two_fetch_last_dual_cycle: Optional[int] = None
        initialize_ftq_coverage_state(self)
        initialize_ifu_cacheable_pipeline_state(self)
        initialize_mmio_v3_coverage_state(self)
        initialize_mmio_nc_owner_coverage_state(self)
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
            str(os.getenv("TB_FUNCOV_TARGET_TESTCASES", ""))
            .replace(",", " ")
            .replace(";", " ")
            .split()
        )
        if explicit_testcases:
            unresolved = sorted(
                testcase
                for testcase in explicit_testcases
                if not any(
                    testcase == item.suggested_testcase for item in self.definitions
                )
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
                os.getenv("TB_FUNCOV_TARGET_TESTCASES", "").strip()
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
        build_root = frontend_build_root_path()
        manifest_override = os.getenv("TB_DUT_BUILD_MANIFEST", "").strip()
        simulator = frontend_simulator(os.getenv("TB_FRONTEND_SIM", "verilator"))
        manifest_path = (
            Path(manifest_override).expanduser()
            if manifest_override
            else frontend_build_manifest_path(build_root, simulator)
        ).resolve(strict=False)
        build = load_frontend_build_manifest(
            build_root,
            manifest_path,
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
        sampler_sha256 = current_funcov_sampler_sha256()
        verification_env_sha256 = current_verification_environment_sha256()
        provenance = {
            "simulator": simulator,
            "dut_source_sha": dut_source_sha,
            "dut_source_origin": dut_source_origin,
            "implementation_sha": build.get("implementation_sha", "unavailable"),
            "design_baseline_sha": build.get("design_baseline_sha", "unavailable"),
            "source_sha_override": bool(build.get("source_sha_override", False)),
            "source_delta_sha256": build.get("source_delta_sha256", "unavailable"),
            "source_delta_files": list(build.get("source_delta_files") or []),
            "source_delta_policy": build.get("source_delta_policy", "unavailable"),
            "dut_build_sha256": build["dut_build_sha256"],
            "dut_python_extension_sha256": build["dut_python_extension_sha256"],
            "generated_rtl_sha256": build["generated_rtl_sha256"],
            "registry_sha256": (
                _file_sha256(self.source_csv) if self.source_csv is not None else definitions_sha256
            ),
            "definitions_sha256": definitions_sha256,
            "sampler_sha256": sampler_sha256,
            "sampler_domains": sorted(self.sampler_domains),
            "verification_env_sha256": verification_env_sha256,
            "signal_contract_sha256": build["signal_contract_sha256"],
            "build_config": build_config,
            "build_manifest_path": str(manifest_path),
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
    ) -> "FrontendFuncovSampleHub":
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

    def attach_toffee_sink(self, sink) -> None:
        self.toffee_sink = sink
        # DUT helpers query definitions/state through tb.functional_coverage (sink).
        if hasattr(sink, "attach_sample_hub"):
            sink.attach_sample_hub(self)
        sink.definitions = self.definitions
        sink.definition_by_key = self.definition_by_key
        sink.definition_by_bin_id = self.definition_by_bin_id
        sink.definition_by_group_bin = self.definition_by_group_bin

    def hit_count_by_bin_id(self, bin_id: str) -> int:
        if self.toffee_sink is None:
            return 0
        return int(self.toffee_sink.hit_count_by_bin_id(bin_id))

    def key_hit(
        self,
        coverage_group: str,
        bin_name: str,
        *,
        coverpoint: Optional[str] = None,
    ) -> bool:
        if self.toffee_sink is None:
            return False
        return bool(
            self.toffee_sink.key_hit(
                coverage_group,
                bin_name,
                coverpoint=coverpoint,
            )
        )

    def hit_detail(
        self,
        coverage_group: str,
        bin_name: str,
        *,
        coverpoint: Optional[str] = None,
    ):
        if self.toffee_sink is None:
            return None
        return self.toffee_sink.hit_detail(
            coverage_group,
            bin_name,
            coverpoint=coverpoint,
        )

    def hit_detail_by_bin_id(self, bin_id: str):
        if self.toffee_sink is None:
            return None
        return self.toffee_sink.hit_detail_by_bin_id(bin_id)

    def enable_toffee_direct_domain(self, domain: str) -> None:
        self.toffee_direct_domains.add(str(domain).strip().lower())

    def attach_toffee_event_model(self, model) -> None:
        self.toffee_event_models.append(model)

    def attach_toffee_owner_model(self, model) -> None:
        self.toffee_owner_model = model

    def attach_toffee_uncache_model(self, model) -> None:
        self.toffee_uncache_model = model

    def sampler_domain_enabled(self, domain: str) -> bool:
        return "all" in self.sampler_domains or str(domain).strip().lower() in self.sampler_domains

    def record_contract_error(
        self,
        event: str,
        cycle: int,
        details: Optional[dict] = None,
    ) -> None:
        sanitized = _sanitize(dict(details or {}))
        key = (str(event), int(cycle), json.dumps(sanitized, sort_keys=True))
        if key in self._contract_error_keys:
            return
        self._contract_error_keys.append(key)
        self.contract_errors.append(
            {
                "kind": "FUNCOV_CONTRACT_ERROR",
                "event": str(event),
                "cycle": int(cycle),
                **sanitized,
            }
        )

    def mark(
        self,
        coverage_group: str,
        bin_name: str,
        cycle: int,
        evidence: Optional[dict] = None,
        *,
        coverpoint: Optional[str] = None,
        forward_to_toffee: bool = True,
        derive_owner: bool = True,
    ) -> bool:
        """Route a coverage observation into the attached Toffee sink and owner derivation."""
        key = self._coverage_key(str(coverage_group), str(bin_name), coverpoint=coverpoint)
        if key not in self.definition_by_key:
            raise KeyError(
                "functional coverage sampler attempted an unmodeled bin: "
                f"{key[0]}::{key[1]}::{key[2]}"
            )
        if forward_to_toffee and self.toffee_sink is not None:
            self.toffee_sink.mark(
                coverage_group,
                bin_name,
                cycle,
                evidence,
                coverpoint=coverpoint,
            )
        definition = self.definition_by_key[key]
        if derive_owner and self.sampler_domain_enabled("ifu"):
            if self.toffee_owner_model is not None:
                self.toffee_owner_model.derive_from_source(
                    definition.bin_id,
                    int(cycle),
                    evidence,
                )
            else:
                derive_owner_v3_from_source(
                    self,
                    definition.bin_id,
                    int(cycle),
                    evidence,
                )
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

        # Event-driven state outside ICache must not influence an ICache-only run.
        event_sampling_enabled = any(
            self.sampler_domain_enabled(domain)
            for domain in ("ifu", "ftq", "uncache", "mmio", "ibuffer")
        )
        if not event_sampling_enabled:
            return

        if self.sampler_domain_enabled("ifu"):
            if self.toffee_owner_model is None:
                handle_owner_v3_event(self, evt)
            if "ifu_mmio_v3" not in self.toffee_direct_domains:
                handle_mmio_v3_checked_event(self, evt)
        for model in self.toffee_event_models:
            model.handle_event(evt)

        path_mark_target = self.toffee_uncache_model or self
        if event_type == "handshake.icache_a":
            if (
                self._redirected_fetch_path is not None
                and self._redirected_fetch_path.get("path") == "mmio_uncache"
                and self._redirected_fetch_path.get("pbmt_nc") is True
            ):
                path_mark_target.mark(
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
        elif event_type == "handshake.uncache_a" and self.sampler_domain_enabled("uncache"):
            address = int(payload.get("address", 0))
            if (
                self._redirected_fetch_path is not None
                and self._redirected_fetch_path.get("path") == "icache_seq"
                and self._uncache_active_nc
            ):
                path_mark_target.mark(
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
                path_mark_target.mark(
                    "fetch_path_switch",
                    "icache_to_mmio_clean",
                    cycle,
                    {"event": event_type, "address": address, **self._redirected_fetch_path},
                )
            self._redirected_fetch_path = None
            self._last_fetch_path = "mmio_uncache"
            self._last_fetch_cycle = cycle
            self._sample_uncache_a_event(cycle, payload)
        elif event_type == "backend.redirect" and self.sampler_domain_enabled("ifu"):
            self._redirected_fetch_path = {
                "path": self._last_fetch_path,
                "pbmt_nc": bool(self._last_uncache_was_nc),
            }
            self._ifu_last_cfvec = None
            self._ifu_redirect_skip_until_cycle = cycle + 2
            self._uncache_page_tail_requests.clear()

    def _clear_transient_sampling_state(self) -> None:
        self._ifu_frontend_trigger_state = None
        self._ifu_invalid_taken_half_delivery = None
        self._ifu_backend_checker_pending = None
        self._ifu_uncache_half_isolation = None
        self._ifu_uncache_half_isolation_sample_cycle = None
        self._last_fetch_path = "icache_seq"
        self._last_fetch_cycle = -1
        self._redirected_fetch_path = None
        self._uncache_page_tail_requests.clear()
        self._uncache_active_nc = False
        self._last_uncache_was_nc = False
        self._ifu_last_cfvec = None
        self._ifu_redirect_skip_until_cycle = None
        self._ifu_ibuffer_alignment_pending = None
        self._two_fetch_last_fetch_ptr = None
        self._two_fetch_expected_ptr_step = None
        self._two_fetch_waiting_refill = False
        self._two_fetch_ftq_pending = False
        self._two_fetch_last_dual_cycle = None
        self._two_fetch_last_waylookup_write_state = None
        reset_ftq_coverage_state(self)
        reset_ifu_cacheable_pipeline_state(self)
        reset_mmio_v3_coverage_state(self)
        reset_mmio_nc_owner_coverage_state(self)
        reset_icache_mainpipe_coverage_state(self)
        reset_icache_prefetchpipe_coverage_state(self)
        reset_icache_missunit_coverage_state(self)
        reset_icache_waylookup_coverage_state(self)
        reset_icache_hitmiss_coverage_state(self)

    def on_cycle(self, cycle: int, env) -> None:
        dut = env.dut
        cycle = int(cycle)
        self.begin_cycle_snapshot(cycle)
        reset_val = self._read_dut_signal(dut, "reset", 0)
        if reset_val == 1:
            self._reset_seen_high = True
            self._clear_transient_sampling_state()
            return
        elif self._reset_seen_high and self._reset_release_cycle is None:
            self._reset_release_cycle = cycle

        if self.sampler_domain_enabled("ftq"):
            if "ftq_two_fetch" not in self.toffee_direct_domains:
                sample_two_fetch_coverage(self, env, cycle)
        if self.sampler_domain_enabled("ifu"):
            if "ifu_cfvec" not in self.toffee_direct_domains:
                sample_cfvec_coverage(self, env, cycle)
            # Keep the cross-path half-RVI producer on the recorder's canonical
            # cycle clock. cfVec sampling may return early when no lane is valid,
            # while BIN-922 observes redirect state in valid-hole cycles.
            _sample_uncache_half_isolation(self, dut, cycle)
            if "ifu_cacheable_pipeline" not in self.toffee_direct_domains:
                sample_ifu_cacheable_pipeline_coverage(self, env, cycle)
            if "ifu_mmio_v3" not in self.toffee_direct_domains:
                sample_mmio_v3_coverage(self, env, cycle)
            if "ifu_mmio_nc_owner" not in self.toffee_direct_domains:
                sample_mmio_nc_owner_coverage(self, env, cycle)
        if self.sampler_domain_enabled("icache"):
            if "icache_mainpipe" not in self.toffee_direct_domains:
                sample_icache_mainpipe_coverage(self, env, cycle)
            if "icache_prefetchpipe" not in self.toffee_direct_domains:
                sample_icache_prefetchpipe_coverage(self, env, cycle)
            if "icache_missunit" not in self.toffee_direct_domains:
                sample_icache_missunit_coverage(self, env, cycle)
            if "icache_waylookup" not in self.toffee_direct_domains:
                sample_icache_waylookup_coverage(self, env, cycle)
            if "icache_hitmiss" not in self.toffee_direct_domains:
                sample_icache_hitmiss_coverage(self, env, cycle)

        if self.sampler_domain_enabled("ibuffer"):
            self._sample_ibuffer_contract(dut, cycle)
        if (
            self.sampler_domain_enabled("uncache")
            and "uncache_event" not in self.toffee_direct_domains
        ):
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

    def begin_cycle_snapshot(self, cycle: int) -> None:
        """Start the read-once DUT value snapshot for one StepRis cycle."""
        cycle = int(cycle)
        if self._cycle_snapshot_cycle != cycle:
            self._cycle_snapshot_cycle = cycle
            self._cycle_snapshot_values.clear()

    @cached_property
    def _registered_internal_signals(self) -> Optional[set[str]]:
        offset_yaml = frontend_pylib_path() / "Frontend" / "Frontend_offset.yaml"
        if not offset_yaml.exists():
            return None

        signals: set[str] = set()
        prefix = "  - name: "
        try:
            with offset_yaml.open("r", encoding="utf-8") as f:
                for line in f:
                    if line.startswith(prefix):
                        signals.add(
                            _decode_signal_inventory_name(line[len(prefix) :])
                        )
        except OSError:
            return None
        return signals

    def _is_registered_internal_signal(self, name: str) -> bool:
        registered = self._registered_internal_signals
        return registered is None or str(name) in registered

    def _read_dut_signal(self, dut, name: str, default: int = 0) -> int:
        value = self._try_read_dut_signal(dut, str(name))
        return int(default) if value is None else int(value)

    def _try_read_dut_signal(self, dut, name: str) -> Optional[int]:
        name = str(name)
        if self._cycle_snapshot_cycle is not None and name in self._cycle_snapshot_values:
            return self._cycle_snapshot_values[name]
        if self._cycle_snapshot_cycle is None and self.toffee_direct_domains:
            raise RuntimeError(
                "DUT signal reads require an active cycle snapshot once Toffee "
                f"direct domains are enabled (signal={name})"
            )
        signal = self._lookup_dut_signal(dut, str(name))
        if signal is None:
            if self._cycle_snapshot_cycle is not None:
                self._cycle_snapshot_values[name] = None
            return None
        value = getattr(signal, "value", None)
        if value is None:
            if self._cycle_snapshot_cycle is not None:
                self._cycle_snapshot_values[name] = None
            return None
        try:
            result = int(value)
        except Exception:
            if self._cycle_snapshot_cycle is not None:
                self._cycle_snapshot_values[name] = None
            return None
        if self._cycle_snapshot_cycle is not None:
            self._cycle_snapshot_values[name] = result
        return result

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

    def _sample_uncache_cycle_state(self, dut, cycle: int, env, *, mark_target=None) -> None:
        mark_target = self if mark_target is None else mark_target
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
        can_accept = self._try_read_dut_signal(dut, "Frontend_top.io_backend_toIBuf_decodeCanAccept")
        if active_pbmt == 1 and pmp_mmio == 0 and state in {2, 3}:
            self._uncache_active_nc = True
        if active_pbmt == 1 and pmp_mmio == 1 and state == 1:
            mark_target.mark(
                "uncache_ordering",
                "pbmt_nc_pmp_mmio_wait_commit",
                cycle,
                {"event": "ifu_uncache_state", "pbmt": active_pbmt, "pmp_mmio": pmp_mmio, "state": state},
            )
        if active_pbmt == 2 and pmp_mmio == 0 and state == 1:
            mark_target.mark(
                "uncache_ordering",
                "pbmt_io_wait_commit",
                cycle,
                {"event": "ifu_uncache_state", "pbmt": active_pbmt, "pmp_mmio": pmp_mmio, "state": state},
            )
        if active_pbmt == 1 and pmp_mmio == 0 and state == 2 and can_accept == 0:
            mark_target.mark(
                "uncache_ordering",
                "pbmt_nc_non_mmio_no_commit_gate",
                cycle,
                {"event": "ifu_uncache_state", "pbmt": active_pbmt, "pmp_mmio": pmp_mmio, "state": state, "can_accept": can_accept},
            )

    def _sample_ibuffer_contract(self, dut, cycle: int) -> None:
        """Capture alignment/ownership facts without turning them into hits."""
        valid = self._read_first_dut_signal(
            dut,
            (
                "Frontend_top.Frontend._inner_ifu_io_toIBuffer_valid",
                "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_valid",
            ),
        )
        enq = self._read_first_dut_signal(
            dut,
            (
                "Frontend_top.Frontend.inner_ifu.io_toIBuffer_bits_enqEnable_0",
                "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_bits_enqEnable",
            ),
        )
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
