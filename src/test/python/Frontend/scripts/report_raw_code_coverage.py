#!/usr/bin/env python3

import argparse
import re
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path


FIELD_RE = re.compile(r"\x01([^\x02]+)\x02([^\x01]*)")

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
        default=Path(__file__).resolve().parent.parent / "data",
        help="Directory containing testcase .dat files",
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
    return parser.parse_args()


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


def main() -> int:
    args = parse_args()
    dat_files = sorted(args.data_dir.glob(args.glob))
    if not dat_files:
        raise SystemExit(f"no .dat files matched: {args.data_dir / args.glob}")

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

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
