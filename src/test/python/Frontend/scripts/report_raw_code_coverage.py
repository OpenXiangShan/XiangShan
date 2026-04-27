#!/usr/bin/env python3

import argparse
import re
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path


FIELD_RE = re.compile(r"\x01([^\x02]+)\x02([^\x01]*)")

IFU_STRICT_RE = re.compile(r"(Frontend|ICache|TLB|TLBFA|BTB|Btb|Tage|Ittage|Ras|PMP|Ifu)")
ICACHE_RE = re.compile(r"(ICache|InstrUncache|IfuUncache)")
BPU_RE = re.compile(
    r"(Bpu|BTB|Btb|Tage|Ittage|Ras|Phr|Pred|Sc|AheadBtb|MainBtb|MicroBtb|MicroTage|Ftq)"
)
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
    "Ifu.sv",
    "IfuPerfAnalysis.sv",
    "InstrCompact.sv",
    "InstrUncacheEntry.sv",
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
) -> tuple[dict[str, Counter], dict[str, dict[str, Counter]]]:
    overall: dict[str, Counter] = defaultdict(Counter)
    modules: dict[str, dict[str, Counter]] = defaultdict(lambda: defaultdict(Counter))
    for point in points.values():
        fields = point["fields"]
        kind = fields.get("t", "<unknown>")
        module = normalize_module(fields.get("f", ""))
        hit = point["count"] > 0
        overall[kind].add(hit)
        modules[module][kind].add(hit)
    return overall, modules


def match_all(_: str) -> bool:
    return True


def match_regex(regex: re.Pattern[str]):
    return lambda module: bool(regex.search(module))


def match_ifu_core(module: str) -> bool:
    return module in IFU_CORE_NAMES


SCOPE_MATCHERS = {
    "all": match_all,
    "ifu_strict": match_regex(IFU_STRICT_RE),
    "ifu_core": match_ifu_core,
    "icache": match_regex(ICACHE_RE),
    "bpu": match_regex(BPU_RE),
    "tlb_pmp": match_regex(TLB_PMP_RE),
    "fault_path": match_regex(FAULT_PATH_RE),
}


SCOPE_NOTES = {
    "all": "all raw points after de-duplicating coverage keys across testcase .dat files",
    "ifu_strict": "filename token match: Frontend/ICache/TLB/BTB/Tage/Ittage/Ras/PMP/Ifu",
    "ifu_core": "hand-picked IFU core path module set used by the main control agent",
    "icache": "ICache, InstrUncache and IfuUncache related files",
    "bpu": "BPU, BTB, Tage, Ras, predictor and FTQ related files",
    "tlb_pmp": "TLB, PTW and PMP related files",
    "fault_path": "heuristic fault propagation path, based on IFU/Frontend/ICache/TLB/PMP filenames",
}


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

    print_header("Frontend Raw Coverage Report")
    print(f"data_dir   : {args.data_dir}")
    print(f"dat_files  : {len(dat_files)}")
    print(f"point_keys : {len(points)}")
    print()

    overall_rows = []
    for kind in ("line", "branch", "expr", "toggle"):
        counter = overall.get(kind, Counter())
        overall_rows.append(
            [kind, str(counter.hit), str(counter.total), f"{counter.pct:.2f}%"]
        )
    print_header("Overall Raw Coverage")
    print_table(["kind", "hit", "total", "pct"], overall_rows)
    print()

    scope_rows = []
    for scope in ("all", "ifu_strict", "ifu_core", "icache", "bpu", "tlb_pmp", "fault_path"):
        counter, matched_modules = scope_counter(modules, scope, "line")
        scope_rows.append(
            [
                scope,
                str(counter.hit),
                str(counter.total),
                f"{counter.pct:.2f}%",
                str(matched_modules),
            ]
        )
    print_header("Raw Line Coverage By Scope")
    print_table(["scope", "hit", "total", "pct", "modules"], scope_rows)
    print()
    for scope in ("ifu_strict", "ifu_core", "icache", "bpu", "tlb_pmp", "fault_path"):
        print(f"{scope:10s}: {SCOPE_NOTES[scope]}")
    print()

    for scope in ("ifu_strict", "all"):
        rows = low_coverage_modules(modules, scope, "line", args.top_n, args.min_points)
        printable_rows = [
            [module, str(counter.hit), str(counter.total), f"{counter.pct:.2f}%"]
            for module, counter in rows
        ]
        print_header(
            f"Lowest Raw Line Modules ({scope}, min_points={args.min_points}, top_n={args.top_n})"
        )
        if printable_rows:
            print_table(["module", "hit", "total", "pct"], printable_rows)
        else:
            print("no modules matched the current filter")
        print()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
