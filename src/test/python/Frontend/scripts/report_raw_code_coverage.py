#!/usr/bin/env python3

import argparse
import hashlib
import json
import re
import sys
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path


FRONTEND_ROOT = Path(__file__).resolve().parents[1]
PROVENANCE_ROOT = FRONTEND_ROOT / "env"
if str(PROVENANCE_ROOT) not in sys.path:
    sys.path.insert(0, str(PROVENANCE_ROOT))

from artifact_provenance import (  # noqa: E402
    BUILD_HASH_FIELDS as MANIFEST_BUILD_HASH_FIELDS,
    load_frontend_build_manifest,
)


FIELD_RE = re.compile(r"\x01([^\x02]+)\x02([^\x01]*)")
SHA256_RE = re.compile(r"[0-9a-fA-F]{64}\Z")

# These fields are the build identity.  A raw Verilator .dat file does not
# carry provenance itself, so the matching funcov sidecar is the authority
# that binds it to one clean DUT build.
BUILD_HASH_FIELDS = tuple(MANIFEST_BUILD_HASH_FIELDS)
COMPATIBILITY_FIELDS = (
    "dut_source_sha",
    "implementation_sha",
    "design_baseline_sha",
    "source_sha_override",
    "source_delta_sha256",
    "source_delta_files",
    "source_delta_policy",
    *BUILD_HASH_FIELDS,
    "registry_sha256",
    "sampler_sha256",
    "build_config",
    "toolchain",
)
PASS_OUTCOMES = {"pass", "passed", "ok", "success", "successful"}


class CoverageProvenanceError(ValueError):
    """Raised before merging when a .dat lacks trustworthy build identity."""


FRONTEND_TOP_RE = re.compile(r"(Frontend|Frontend_top)")
IFU_STRICT_RE = re.compile(r"(Frontend|ICache|TLB|TLBFA|BTB|Btb|Tage|Ittage|Ras|PMP|Ifu|IBuffer|Ftq)")
IFU_RE = re.compile(
    r"(Ifu|PreDecode|PredChecker|RvcExpander|InstrBoundary|InstrCompact|F3PreDecode|FrontendTrigger)"
)
ICACHE_RE = re.compile(r"(ICache|InstrUncache|IfuUncache)")
BPU_RE = re.compile(
    r"(Bpu|BTB|Btb|Tage|Ittage|Ras|Phr|Pred|Sc|AheadBtb|MainBtb|MicroBtb|MicroTage|FallThroughPredictor|SaturateCounter|CompareMatrix|WriteBuffer)"
)
FTQ_RE = re.compile(r"(Ftq|CfiQueue|CommitQueue|MetaQueue|ResolveQueue|SpeculationQueue|RedirectReceiver)")
ITLB_RE = re.compile(r"(TLB|TLBFA|PTW)")
IBUFFER_RE = re.compile(r"(IBuffer|IBuf)")
PMP_RE = re.compile(r"(PMP)")
TLB_PMP_RE = re.compile(r"(TLB|TLBFA|PTW|PMP)")
FAULT_PATH_RE = re.compile(r"(Ifu|Frontend|ICache|InstrUncache|TLB|TLBFA|PMP)")

IFU_CORE_NAMES = {
    "AheadBtb.sv",
    "AheadBtbReplacer.sv",
    "Frontend.sv",
    "Frontend_top.sv",
    "ICache.sv",
    "ICacheCtrlUnit.sv",
    "ICacheDataArray.sv",
    "IBuffer.sv",
    "Ifu.sv",
    "IfuPerfAnalysis.sv",
    "InstrCompact.sv",
    "InstrUncacheEntry.sv",
    "Ftq.sv",
    "EntryQueue.sv",
    "CommitQueue.sv",
    "CfiQueue.sv",
    "ResolveQueue.sv",
    "Ittage.sv",
    "IttageTable.sv",
    "MainBtb.sv",
    "MainBtbAlignBank.sv",
    "MainBtbInternalBank.sv",
    "MicroBtb.sv",
    "MicroRas.sv",
    "MicroTage.sv",
    "MicroTageTable.sv",
    "PMP.sv",
    "RasStack.sv",
    "TLBFA.sv",
    "Tage.sv",
    "TageTable.sv",
    "TageTable_1.sv",
    "TageTable_2.sv",
    "TageTable_3.sv",
    "TageTable_4.sv",
    "TageTable_5.sv",
    "TageTable_6.sv",
    "TageTable_7.sv",
}


@dataclass
class Counter:
    total: int = 0
    hit: int = 0

    def add(self, hit: bool) -> None:
        self.total += 1
        if hit:
            self.hit += 1

    @property
    def pct(self) -> float:
        if self.total == 0:
            return 0.0
        return self.hit * 100.0 / self.total


def parse_args() -> argparse.Namespace:
    repo_root = Path(__file__).resolve().parents[5]
    parser = argparse.ArgumentParser(
        description="Merge Frontend verilator .dat files and report raw coverage points."
    )
    parser.add_argument(
        "--data-dir",
        type=Path,
        required=True,
        help="One run/suite directory containing compatible testcase .dat files",
    )
    parser.add_argument(
        "--source-root",
        type=Path,
        default=repo_root / "build-frontend",
        help="Generated source tree used to report real SV source line counts",
    )
    parser.add_argument(
        "--source-glob",
        default="*.sv",
        help="Recursive glob under --source-root for generated source LOC accounting",
    )
    parser.add_argument(
        "--glob",
        default="*.dat",
        help="Glob used under --data-dir to select .dat files",
    )
    parser.add_argument(
        "--top-n",
        type=int,
        default=12,
        help="How many low-coverage modules to print per table",
    )
    parser.add_argument(
        "--min-points",
        type=int,
        default=20,
        help="Minimum raw points required before a module enters the low-coverage table",
    )
    parser.add_argument(
        "--json-output",
        type=Path,
        help="Write a machine-readable copy of the summary without changing stdout",
    )
    parser.add_argument("--run-id", default="", help="Run or suite identifier stored in JSON output")
    return parser.parse_args()


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _manifest_path(source_root: Path) -> Path:
    """Resolve the build manifest for either a build root or its rtl child."""
    source_root = source_root.resolve()
    candidates = [source_root / "frontend_build_manifest.json"]
    if source_root.name == "rtl":
        candidates.append(source_root.parent / "frontend_build_manifest.json")
    for candidate in candidates:
        if candidate.is_file():
            return candidate
    # Return the canonical location so the error names the expected file.
    return candidates[0]


def _run_roots_for_dat(dat_path: Path) -> list[Path]:
    """Return likely run roots for a coverage file."""
    dat_path = dat_path.resolve()
    roots = [dat_path.parent]
    if dat_path.parent.name in {"coverage", "codecov"}:
        roots.insert(0, dat_path.parent.parent)
    elif dat_path.parent.parent != dat_path.parent:
        roots.append(dat_path.parent.parent)
    return list(dict.fromkeys(roots))


def _funcov_candidates(dat_files: list[Path], data_dir: Path) -> list[Path]:
    """Find sidecars in each selected run before considering a broad scan."""
    candidates: set[Path] = set()
    search_dirs: set[Path] = {data_dir.resolve() / "funcov"}
    for dat_path in dat_files:
        for root in _run_roots_for_dat(dat_path):
            search_dirs.add(root / "funcov")
    for directory in sorted(search_dirs):
        if not directory.exists():
            continue
        if directory.is_file():
            continue
        for path in directory.rglob("*.funcov.json"):
            if path.is_file():
                candidates.add(path.resolve())
    return sorted(candidates)


def _compatibility_signature(provenance: dict) -> str:
    payload = {field: provenance.get(field) for field in COMPATIBILITY_FIELDS}
    encoded = json.dumps(payload, ensure_ascii=False, sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(encoded.encode("utf-8")).hexdigest()


def _load_manifest(manifest_path: Path) -> tuple[dict, str]:
    if not manifest_path.is_file():
        raise CoverageProvenanceError(f"build manifest missing: {manifest_path}")
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise CoverageProvenanceError(f"build manifest unreadable: {manifest_path}") from exc
    if not isinstance(manifest, dict):
        raise CoverageProvenanceError(f"build manifest is not an object: {manifest_path}")
    if manifest.get("source_tree_dirty") is not False:
        raise CoverageProvenanceError("build manifest source_tree_dirty is not false")
    artifacts = manifest.get("artifacts")
    if not isinstance(artifacts, dict):
        raise CoverageProvenanceError("build manifest lacks artifacts object")
    for field in BUILD_HASH_FIELDS:
        value = str(artifacts.get(field) or "").strip()
        if SHA256_RE.fullmatch(value) is None:
            raise CoverageProvenanceError(f"build manifest has invalid artifact hash: {field}")
    runtime = load_frontend_build_manifest(manifest_path.parent, manifest_path)
    if str(runtime.get("build_manifest_status") or "").strip().lower() != "valid":
        reasons = runtime.get("build_manifest_reasons") or ["unknown"]
        raise CoverageProvenanceError(
            "build manifest runtime validation failed: " + ",".join(map(str, reasons))
        )
    for field in BUILD_HASH_FIELDS:
        if str(runtime.get(field) or "").strip().lower() != str(artifacts[field]).strip().lower():
            raise CoverageProvenanceError(f"runtime artifact hash mismatch: {field}")
    return manifest, file_sha256(manifest_path)


def _index_funcov_sidecars(
    candidates: list[Path],
) -> tuple[dict[str, list[tuple[Path, dict]]], dict[str, str]]:
    by_coverage: dict[str, list[tuple[Path, dict]]] = defaultdict(list)
    malformed: dict[str, str] = {}
    for sidecar in candidates:
        try:
            raw = json.loads(sidecar.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as exc:
            malformed[str(sidecar)] = f"unreadable sidecar ({type(exc).__name__})"
            continue
        if not isinstance(raw, dict):
            malformed[str(sidecar)] = "sidecar root is not an object"
            continue
        coverage_raw = str(raw.get("line_coverage_path") or "").strip()
        if not coverage_raw:
            malformed[str(sidecar)] = "sidecar lacks line_coverage_path"
            continue
        try:
            coverage_path = str(Path(coverage_raw).expanduser().resolve())
        except (OSError, RuntimeError) as exc:
            malformed[str(sidecar)] = f"invalid line_coverage_path ({type(exc).__name__})"
            continue
        by_coverage[coverage_path].append((sidecar, raw))
    return by_coverage, malformed


def validate_dat_provenance(
    dat_files: list[Path],
    *,
    data_dir: Path,
    source_root: Path,
) -> dict:
    """Validate every selected .dat before it contributes to the aggregate."""
    if not dat_files:
        raise CoverageProvenanceError("no .dat files selected")
    manifest_path = _manifest_path(source_root)
    manifest, manifest_hash = _load_manifest(manifest_path)
    manifest_artifacts = manifest["artifacts"]
    candidates = _funcov_candidates(dat_files, data_dir)
    by_coverage, _malformed = _index_funcov_sidecars(candidates)
    selected_paths = {str(path.resolve()) for path in dat_files}
    if selected_paths - set(by_coverage):
        # Compatibility fallback for custom runners that keep sidecars under a
        # suite-level directory instead of each case run root.
        broad_candidates = {
            path.resolve()
            for path in data_dir.resolve().rglob("*.funcov.json")
            if path.is_file()
        }
        by_coverage, _malformed = _index_funcov_sidecars(
            sorted(set(candidates) | broad_candidates)
        )

    records = []
    run_ids: set[str] = set()
    compatibility_signature = None
    build_identity = None
    for dat_path in dat_files:
        dat_path = dat_path.resolve()
        try:
            if dat_path.stat().st_size <= 0:
                raise CoverageProvenanceError(f"empty .dat: {dat_path}")
        except OSError as exc:
            raise CoverageProvenanceError(f"cannot stat .dat: {dat_path}") from exc

        matches = by_coverage.get(str(dat_path), [])
        if len(matches) != 1:
            detail = "missing" if not matches else f"ambiguous ({len(matches)} sidecars)"
            raise CoverageProvenanceError(f"{detail} funcov sidecar for .dat: {dat_path}")
        sidecar_path, raw = matches[0]
        if raw.get("artifact_schema_version") != 2:
            raise CoverageProvenanceError(f"legacy funcov sidecar: {sidecar_path}")
        provenance = raw.get("provenance")
        if not isinstance(provenance, dict):
            raise CoverageProvenanceError(f"sidecar lacks provenance: {sidecar_path}")
        manifest_status = str(provenance.get("build_manifest_status") or "").strip().lower()
        if manifest_status != "valid":
            raise CoverageProvenanceError(f"sidecar build manifest is not valid: {sidecar_path}")
        manifest_reasons = provenance.get("build_manifest_reasons")
        if not isinstance(manifest_reasons, list) or manifest_reasons:
            raise CoverageProvenanceError(f"sidecar build manifest has reasons: {sidecar_path}")
        recorded_manifest_hash = str(provenance.get("build_manifest_sha256") or "").strip().lower()
        if recorded_manifest_hash != manifest_hash:
            raise CoverageProvenanceError(f"manifest hash mismatch for sidecar: {sidecar_path}")
        for field in BUILD_HASH_FIELDS:
            value = str(provenance.get(field) or "").strip().lower()
            expected = str(manifest_artifacts.get(field) or "").strip().lower()
            if SHA256_RE.fullmatch(value) is None:
                raise CoverageProvenanceError(f"sidecar has invalid {field}: {sidecar_path}")
            if value != expected:
                raise CoverageProvenanceError(f"{field} mismatch for sidecar: {sidecar_path}")

        missing_compat = [
            field
            for field in COMPATIBILITY_FIELDS
            if field not in provenance
            or provenance[field] is None
            or (isinstance(provenance[field], str) and not provenance[field].strip())
        ]
        if missing_compat:
            raise CoverageProvenanceError(
                f"sidecar lacks compatibility fields {missing_compat}: {sidecar_path}"
            )
        recorded_signature = str(provenance.get("compatibility_signature") or "").strip().lower()
        if SHA256_RE.fullmatch(recorded_signature) is None:
            raise CoverageProvenanceError(f"sidecar has invalid compatibility signature: {sidecar_path}")
        if recorded_signature != _compatibility_signature(provenance):
            raise CoverageProvenanceError(f"compatibility signature mismatch: {sidecar_path}")
        if compatibility_signature is None:
            compatibility_signature = recorded_signature
        elif recorded_signature != compatibility_signature:
            raise CoverageProvenanceError(f"cross-build compatibility mismatch: {sidecar_path}")

        run = raw.get("run")
        if not isinstance(run, dict):
            raise CoverageProvenanceError(f"sidecar lacks run metadata: {sidecar_path}")
        run_id = str(run.get("run_id") or "").strip()
        if not run_id:
            raise CoverageProvenanceError(f"sidecar lacks run_id: {sidecar_path}")
        if run_id in run_ids:
            raise CoverageProvenanceError(f"duplicate run_id across .dat files: {run_id}")
        run_ids.add(run_id)
        outcome = str(run.get("pytest_outcome") or "").strip().lower()
        if outcome not in PASS_OUTCOMES or run.get("exit_code") not in {0, "0"}:
            raise CoverageProvenanceError(f"pytest gate failed for sidecar: {sidecar_path}")
        checker = run.get("checker")
        if not isinstance(checker, dict) or str(checker.get("status") or "").strip().lower() not in PASS_OUTCOMES:
            raise CoverageProvenanceError(f"checker gate failed for sidecar: {sidecar_path}")
        if "error_count" not in checker:
            raise CoverageProvenanceError(f"checker error_count is missing for sidecar: {sidecar_path}")
        try:
            checker_error_count = int(checker.get("error_count"))
        except (TypeError, ValueError) as exc:
            raise CoverageProvenanceError(
                f"checker error_count is invalid for sidecar: {sidecar_path}"
            ) from exc
        if checker_error_count != 0:
            raise CoverageProvenanceError(f"checker errors present for sidecar: {sidecar_path}")
        checker_errors = checker.get("errors")
        if checker_errors != []:
            raise CoverageProvenanceError(f"checker error details present for sidecar: {sidecar_path}")

        stats = raw.get("stats")
        monitor = stats.get("monitor") if isinstance(stats, dict) else None
        if not isinstance(monitor, dict):
            raise CoverageProvenanceError(f"sidecar lacks monitor statistics: {sidecar_path}")
        if "cycles_total" not in monitor:
            raise CoverageProvenanceError(f"monitor cycles_total is missing for sidecar: {sidecar_path}")
        try:
            monitor_cycles = int(monitor.get("cycles_total"))
        except (TypeError, ValueError) as exc:
            raise CoverageProvenanceError(
                f"monitor cycles_total is invalid for sidecar: {sidecar_path}"
            ) from exc
        if monitor_cycles <= 0:
            raise CoverageProvenanceError(f"monitor cycles_total is not positive for sidecar: {sidecar_path}")
        if "error_count" not in monitor:
            raise CoverageProvenanceError(f"monitor error_count is missing for sidecar: {sidecar_path}")
        try:
            monitor_error_count = int(monitor.get("error_count"))
        except (TypeError, ValueError) as exc:
            raise CoverageProvenanceError(
                f"monitor error_count is invalid for sidecar: {sidecar_path}"
            ) from exc
        if monitor_error_count != 0:
            raise CoverageProvenanceError(f"monitor errors present for sidecar: {sidecar_path}")
        monitor_errors = monitor.get("errors")
        if monitor_errors not in (None, []):
            raise CoverageProvenanceError(f"monitor error details present for sidecar: {sidecar_path}")
        if raw.get("errors") != []:
            raise CoverageProvenanceError(f"funcov errors present for sidecar: {sidecar_path}")

        waveform_raw = str(raw.get("waveform_path") or "").strip()
        if not waveform_raw or not Path(waveform_raw).is_absolute():
            raise CoverageProvenanceError(f"sidecar lacks absolute waveform path: {sidecar_path}")
        waveform_path = Path(waveform_raw)
        try:
            if not waveform_path.is_file() or waveform_path.stat().st_size <= 0:
                raise CoverageProvenanceError(
                    f"waveform gate failed for sidecar: {sidecar_path}"
                )
        except OSError as exc:
            raise CoverageProvenanceError(
                f"cannot stat waveform for sidecar: {sidecar_path}"
            ) from exc

        identity = tuple(str(provenance[field]).strip().lower() for field in BUILD_HASH_FIELDS)
        if build_identity is None:
            build_identity = identity
        elif identity != build_identity:
            raise CoverageProvenanceError(f"DUT build identity mismatch: {sidecar_path}")
        records.append(
            {
                "path": str(dat_path),
                "size_bytes": dat_path.stat().st_size,
                "funcov_path": str(sidecar_path),
                "run_id": run_id,
                "waveform_path": str(waveform_path),
                "build_manifest_sha256": recorded_manifest_hash,
                "compatibility_signature": recorded_signature,
            }
        )
    return {
        "manifest_path": str(manifest_path.resolve()),
        "manifest_sha256": manifest_hash,
        "run_ids": sorted(run_ids),
        "compatibility_signature": compatibility_signature,
        "build_hashes": dict(zip(BUILD_HASH_FIELDS, build_identity or ())),
        "dat_files": records,
    }


def normalize_module(path_text: str) -> str:
    if not path_text:
        return "<unknown>"
    return Path(path_text).name


def load_merged_points(dat_files: list[Path]) -> dict[str, dict[str, object]]:
    points: dict[str, dict[str, object]] = {}
    for dat_file in dat_files:
        with dat_file.open("r", errors="ignore") as fh:
            for raw in fh:
                if not raw.startswith("C "):
                    continue
                point_key, count_text = raw.rsplit(" ", 1)
                item = points.setdefault(
                    point_key,
                    {"fields": dict(FIELD_RE.findall(raw)), "count": 0},
                )
                item["count"] += int(count_text)
    return points


def build_stats(
    points: dict[str, dict[str, object]],
    source_suffix: str | None = None,
) -> tuple[dict[str, Counter], dict[str, dict[str, Counter]]]:
    overall: dict[str, Counter] = defaultdict(Counter)
    modules: dict[str, dict[str, Counter]] = defaultdict(lambda: defaultdict(Counter))
    for point in points.values():
        fields = point["fields"]
        if source_suffix and not fields.get("f", "").endswith(source_suffix):
            continue
        kind = fields.get("t", "<unknown>")
        module = normalize_module(fields.get("f", ""))
        hit = point["count"] > 0
        overall[kind].add(hit)
        modules[module][kind].add(hit)
    return overall, modules


def load_source_line_counts(source_root: Path, source_glob: str) -> tuple[dict[str, int], int]:
    lines_by_module: dict[str, int] = defaultdict(int)
    file_count = 0
    if not source_root.exists():
        return lines_by_module, file_count

    for source_path in sorted(source_root.rglob(source_glob)):
        if not source_path.is_file():
            continue
        file_count += 1
        with source_path.open("r", errors="ignore") as fh:
            line_count = sum(1 for _ in fh)
        lines_by_module[normalize_module(str(source_path))] += int(line_count)
    return lines_by_module, file_count


def match_all(_: str) -> bool:
    return True


def match_regex(regex: re.Pattern[str]):
    return lambda module: bool(regex.search(module))


def match_ifu_core(module: str) -> bool:
    return module in IFU_CORE_NAMES


SCOPE_MATCHERS = {
    "all": match_all,
    "frontend_top": match_regex(FRONTEND_TOP_RE),
    "ifu_strict": match_regex(IFU_STRICT_RE),
    "ifu_core": match_ifu_core,
    "ifu": match_regex(IFU_RE),
    "icache": match_regex(ICACHE_RE),
    "bpu": match_regex(BPU_RE),
    "ftq": match_regex(FTQ_RE),
    "itlb": match_regex(ITLB_RE),
    "ibuffer": match_regex(IBUFFER_RE),
    "pmp": match_regex(PMP_RE),
    "tlb_pmp": match_regex(TLB_PMP_RE),
    "fault_path": match_regex(FAULT_PATH_RE),
}


SCOPE_NOTES = {
    "all": "all raw points after de-duplicating coverage keys across testcase .dat files",
    "frontend_top": "Frontend and Frontend_top wrapper files",
    "ifu_strict": "filename token match: Frontend/ICache/TLB/BTB/Tage/Ittage/Ras/PMP/Ifu/IBuffer/Ftq",
    "ifu_core": "hand-picked IFU core path module set used by the main control agent",
    "ifu": "IFU pipeline, predecode, pred-checker, RVC expansion and instruction compaction files",
    "icache": "ICache, InstrUncache and IfuUncache related files",
    "bpu": "BPU, BTB, TAGE/ITTAGE, RAS, predictor tables and predictor helper files",
    "ftq": "FTQ, entry/commit/resolve/speculation queues and redirect receiver files",
    "itlb": "ITLB/TLB/PTW translation related files",
    "ibuffer": "IBuffer and IBuf bundle related files",
    "pmp": "PMP related files",
    "tlb_pmp": "TLB, PTW and PMP related files",
    "fault_path": "heuristic fault propagation path, based on IFU/Frontend/ICache/TLB/PMP filenames",
}

SUMMARY_SCOPES = (
    "all",
    "frontend_top",
    "ifu_core",
    "ifu",
    "icache",
    "bpu",
    "ftq",
    "itlb",
    "ibuffer",
    "pmp",
    "fault_path",
)


def scope_counter(
    modules: dict[str, dict[str, Counter]],
    scope: str,
    kind: str,
) -> tuple[Counter, int]:
    matcher = SCOPE_MATCHERS[scope]
    merged = Counter()
    matched_modules = 0
    for module_name, module_stats in modules.items():
        if not matcher(module_name):
            continue
        matched_modules += 1
        counter = module_stats.get(kind)
        if counter is None:
            continue
        merged.total += counter.total
        merged.hit += counter.hit
    return merged, matched_modules


def low_coverage_modules(
    modules: dict[str, dict[str, Counter]],
    scope: str,
    kind: str,
    top_n: int,
    min_points: int,
) -> list[tuple[str, Counter]]:
    matcher = SCOPE_MATCHERS[scope]
    rows: list[tuple[str, Counter]] = []
    for module_name, module_stats in modules.items():
        if not matcher(module_name):
            continue
        counter = module_stats.get(kind)
        if counter is None or counter.total < min_points:
            continue
        rows.append((module_name, counter))
    rows.sort(key=lambda item: (item[1].pct, -item[1].total, item[0]))
    return rows[:top_n]


def source_line_scope(
    source_lines: dict[str, int],
    scope: str,
) -> tuple[int, int]:
    matcher = SCOPE_MATCHERS[scope]
    total_lines = 0
    matched_modules = 0
    for module_name, line_count in source_lines.items():
        if not matcher(module_name):
            continue
        matched_modules += 1
        total_lines += int(line_count)
    return total_lines, matched_modules


def print_header(title: str) -> None:
    print(title)
    print("-" * len(title))


def print_table(headers: list[str], rows: list[list[str]]) -> None:
    widths = [len(header) for header in headers]
    for row in rows:
        for idx, cell in enumerate(row):
            widths[idx] = max(widths[idx], len(cell))
    fmt = "  ".join(f"{{:{width}}}" for width in widths)
    print(fmt.format(*headers))
    print(fmt.format(*["-" * width for width in widths]))
    for row in rows:
        print(fmt.format(*row))


def counter_dict(counter: Counter) -> dict[str, int | float]:
    return {"hit": counter.hit, "total": counter.total, "pct": round(counter.pct, 6)}


def main() -> int:
    args = parse_args()
    dat_files = sorted(args.data_dir.glob(args.glob))
    if not dat_files:
        raise SystemExit(f"no .dat files matched: {args.data_dir / args.glob}")

    try:
        provenance = validate_dat_provenance(
            dat_files,
            data_dir=args.data_dir,
            source_root=args.source_root,
        )
    except CoverageProvenanceError as exc:
        print(f"[frontend][error] code coverage provenance gate failed: {exc}", file=sys.stderr)
        return 2

    points = load_merged_points(dat_files)
    overall, modules = build_stats(points)
    sv_overall, sv_modules = build_stats(points, ".sv")
    source_lines, source_files = load_source_line_counts(args.source_root, args.source_glob)

    print_header("Frontend Raw Coverage Report")
    print(f"data_dir   : {args.data_dir}")
    print(f"dat_files  : {len(dat_files)}")
    print(f"point_keys : {len(points)}")
    print(f"source_root: {args.source_root}")
    print(f"source_glob: {args.source_glob}")
    print(f"source_files: {source_files}")
    print(f"manifest   : {provenance['manifest_path']}")
    print(f"manifest_sha256: {provenance['manifest_sha256']}")
    print(f"run_ids    : {', '.join(provenance['run_ids'])}")
    print()

    overall_rows = []
    for kind in ("line", "branch", "expr", "toggle"):
        counter = overall.get(kind, Counter())
        overall_rows.append(
            [kind, str(counter.hit), str(counter.total), f"{counter.pct:.2f}%"]
        )
    print_header("Overall Raw Coverage Points")
    print_table(["kind", "hit", "total", "pct"], overall_rows)
    print()

    sv_overall_rows = []
    for kind in ("line", "branch", "expr", "toggle"):
        counter = sv_overall.get(kind, Counter())
        sv_overall_rows.append(
            [kind, str(counter.hit), str(counter.total), f"{counter.pct:.2f}%"]
        )
    print_header("Overall Raw Coverage Points (.sv only)")
    print_table(["kind", "hit", "total", "pct"], sv_overall_rows)
    print()

    scope_rows = []
    for scope in SUMMARY_SCOPES:
        counter, matched_modules = scope_counter(sv_modules, scope, "line")
        scope_rows.append(
            [
                scope,
                str(counter.hit),
                str(counter.total),
                f"{counter.pct:.2f}%",
                str(matched_modules),
            ]
        )
    print_header("Raw Line Coverage Points By Scope (.sv only)")
    print_table(["scope", "point_hit", "point_total", "pct", "modules"], scope_rows)
    print()

    scope_kind_rows = []
    for scope in SUMMARY_SCOPES:
        branch_counter, _ = scope_counter(sv_modules, scope, "branch")
        toggle_counter, _ = scope_counter(sv_modules, scope, "toggle")
        scope_kind_rows.append(
            [
                scope,
                str(branch_counter.hit),
                str(branch_counter.total),
                f"{branch_counter.pct:.2f}%",
                str(toggle_counter.hit),
                str(toggle_counter.total),
                f"{toggle_counter.pct:.2f}%",
            ]
        )
    print_header("Raw Branch/Toggle Coverage Points By Scope (.sv only)")
    print_table(
        [
            "scope",
            "branch_hit",
            "branch_total",
            "branch_pct",
            "toggle_hit",
            "toggle_total",
            "toggle_pct",
        ],
        scope_kind_rows,
    )
    print()

    for scope in SUMMARY_SCOPES:
        if scope == "all":
            continue
        print(f"{scope:10s}: {SCOPE_NOTES[scope]}")
    print()

    source_rows = []
    for scope in SUMMARY_SCOPES:
        line_count, matched_modules = source_line_scope(source_lines, scope)
        source_rows.append([scope, str(line_count), str(matched_modules)])
    print_header("Generated SV Source Lines By Scope")
    print_table(["scope", "source_lines", "modules"], source_rows)
    print()

    for scope in ("ifu_strict", "all"):
        rows = low_coverage_modules(sv_modules, scope, "line", args.top_n, args.min_points)
        printable_rows = [
            [module, str(counter.hit), str(counter.total), f"{counter.pct:.2f}%"]
            for module, counter in rows
        ]
        print_header(
            f"Lowest Raw Line Point Modules ({scope}, min_points={args.min_points}, top_n={args.top_n})"
        )
        if printable_rows:
            print_table(["module", "hit", "total", "pct"], printable_rows)
        else:
            print("no modules matched the current filter")
        print()

    if args.json_output is not None:
        output_path = args.json_output.resolve()
        if output_path.exists():
            raise SystemExit(f"refusing to overwrite existing coverage summary: {output_path}")
        build_manifest_path = Path(provenance["manifest_path"])
        summary = {
            "schema_version": 1,
            "run_id": str(args.run_id).strip() or None,
            "data_dir": str(args.data_dir.resolve()),
            "selection_glob": args.glob,
            "dat_files": [
                {"path": item["path"], "size_bytes": item["size_bytes"]}
                for item in provenance["dat_files"]
            ],
            "point_keys": len(points),
            "source_root": str(args.source_root.resolve()),
            "source_glob": args.source_glob,
            "source_files": source_files,
            "build_manifest_path": str(build_manifest_path),
            "build_manifest_sha256": provenance["manifest_sha256"],
            "provenance": provenance,
            "overall": {
                kind: counter_dict(overall.get(kind, Counter()))
                for kind in ("line", "branch", "expr", "toggle")
            },
            "sv_overall": {
                kind: counter_dict(sv_overall.get(kind, Counter()))
                for kind in ("line", "branch", "expr", "toggle")
            },
            "scopes": {},
        }
        for scope in SUMMARY_SCOPES:
            line_counter, matched_modules = scope_counter(sv_modules, scope, "line")
            branch_counter, _ = scope_counter(sv_modules, scope, "branch")
            toggle_counter, _ = scope_counter(sv_modules, scope, "toggle")
            source_line_count, source_modules = source_line_scope(source_lines, scope)
            summary["scopes"][scope] = {
                "line": counter_dict(line_counter),
                "branch": counter_dict(branch_counter),
                "toggle": counter_dict(toggle_counter),
                "matched_coverage_modules": matched_modules,
                "source_lines": source_line_count,
                "source_modules": source_modules,
            }
        output_path.parent.mkdir(parents=True, exist_ok=True)
        temporary = output_path.with_name(f".{output_path.name}.tmp")
        temporary.write_text(json.dumps(summary, indent=2) + "\n", encoding="utf-8")
        temporary.replace(output_path)
        print(f"json_output: {output_path}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
