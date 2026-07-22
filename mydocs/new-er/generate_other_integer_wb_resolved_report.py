#!/usr/bin/env python3

import argparse
import gzip
import json
import re
import subprocess
from collections import Counter
from dataclasses import dataclass
from pathlib import Path


WORKLOAD_ORDER = [
    "cputest",
    "riscv-tests",
    "misc-tests",
    "rvh-tests",
    "microbench",
    "coremark",
    "linux-hello-opensbi",
    "iopmp-test",
    "povray",
    "copy_and_run",
    "f16_test",
    "zcb-test",
]

WB_COUNTERS = [
    "int_er_rob_wb_resolve_eligible_enq",
    "int_er_rob_wb_resolve_final_candidate",
    "int_er_rob_resolved_by_writeback",
    "int_er_rob_resolved_by_writeback_alu",
    "int_er_rob_resolved_by_writeback_mul",
    "int_er_rob_resolved_by_writeback_div",
    "int_er_rob_resolved_by_writeback_other",
    "int_er_rob_wb_resolve_blocked_need_flush",
    "int_er_rob_wb_resolve_blocked_redirect_recovery",
    "int_er_rob_wb_resolve_rejected_identity_reuse_raw",
    "int_er_rob_wb_resolved_entry_cycle",
    "int_er_rob_interrupt_deferred_for_guard_cycle",
    "int_er_rob_interrupt_deferred_for_guard_episode",
    "int_er_rob_outstanding_guard_sum",
]

ST_COUNTERS = [
    "int_er_rob_st_cycle",
    "int_er_rob_st_no_work_cycle",
    "int_er_rob_st_pending_work_cycle",
    "int_er_rob_st_pending_global_stop_cycle",
    "int_er_rob_st_pending_caught_up_after_scan_cycle",
    "int_er_rob_st_pending_walk_width_limited_cycle",
    "int_er_rob_st_pending_invalid_frontier_cycle",
    "int_er_rob_st_pending_valid_frontier_blocker_cycle",
    "int_er_rob_st_blocker_need_flush_cycle",
    "int_er_rob_st_blocker_not_writebacked_cycle",
    "int_er_rob_st_blocker_writebacked_wait_commit_cycle",
    "int_er_rob_st_blocker_not_resolved_cycle",
    "int_er_rob_st_blocker_class_scalar_load_cycle",
    "int_er_rob_st_blocker_class_scalar_store_cycle",
    "int_er_rob_st_blocker_class_branch_jump_cycle",
    "int_er_rob_st_blocker_class_csr_cycle",
    "int_er_rob_st_blocker_class_fence_cycle",
    "int_er_rob_st_blocker_class_amo_cycle",
    "int_er_rob_st_blocker_class_other_integer_cycle",
    "int_er_rob_st_blocker_class_other_integer_reason_not_resolved_cycle",
    "int_er_rob_st_blocker_class_other_integer_reason_not_writebacked_cycle",
    "int_er_rob_st_blocker_class_other_integer_reason_need_flush_cycle",
    "int_er_rob_st_blocker_class_other_integer_reason_writebacked_wait_commit_cycle",
]

TREND_COUNTERS = [
    "int_er_rename_int_freelist_stall_cycle",
    "int_er_me_freelist_free_reg_sum",
    "int_er_uc_early_free",
    "int_er_uc_early_free_eligible_all",
    "int_er_uc_early_free_deferred_width",
    "int_er_uc_commit_suppress",
    "int_er_uc_released_reused_before_commit",
    "int_er_uc_released_unreused_at_commit",
]

PERF_RE = re.compile(r"\[PERF \]\[time=\s*(\d+)\]\s+[^:]+:\s+([A-Za-z_][A-Za-z0-9_]*),\s*(-?\d+)")
IPC_RE = re.compile(r"Core-0 instrCnt =\s*(\d+), cycleCnt =\s*(\d+), IPC =\s*([0-9.]+)")
ANSI_RE = re.compile(r"\x1b\[[0-9;]*m")


@dataclass
class Workload:
    name: str
    exists: bool
    exit_code: str
    verdict: str
    pass_reason: str
    seconds: str
    good_traps: int
    command: str
    stdout_log: Path
    stderr_log: Path
    wave_dir: str
    instr: int
    cycles: int
    ipc: float
    counters: Counter
    present: set[str]


def read_text(path: Path) -> str:
    if not path.exists():
        return ""
    if path.suffix == ".gz":
        with gzip.open(path, "rt", encoding="utf-8", errors="replace") as f:
            return f.read()
    return path.read_text(encoding="utf-8", errors="replace")


def strip_ansi(text: str) -> str:
    return ANSI_RE.sub("", text)


def parse_kv(text: str) -> dict[str, str]:
    out = {}
    for line in text.splitlines():
        if ":" in line:
            key, value = line.split(":", 1)
            out[key.strip()] = value.strip()
    return out


def parse_ipc(stdout: str) -> tuple[int, int, float]:
    instr = 0
    cycles = 0
    for match in IPC_RE.finditer(strip_ansi(stdout)):
        instr += int(match.group(1))
        cycles += int(match.group(2))
    return instr, cycles, instr / cycles if cycles else 0.0


def parse_perf(stderr: str) -> tuple[Counter, set[str]]:
    counters = Counter()
    present = set()
    for match in PERF_RE.finditer(strip_ansi(stderr)):
        name = match.group(2)
        value = int(match.group(3))
        if name.startswith("int_er_"):
            counters[name] += value
            present.add(name)
    return counters, present


def parse_workload(matrix_root: Path, name: str) -> Workload:
    work_dir = matrix_root / "matrix" / name
    stdout_log = work_dir / "stdout.log"
    stderr_log = work_dir / "stderr.log"
    if not stdout_log.exists() and (work_dir / "stdout.log.gz").exists():
        stdout_log = work_dir / "stdout.log.gz"
    if not stderr_log.exists() and (work_dir / "stderr.log.gz").exists():
        stderr_log = work_dir / "stderr.log.gz"
    stdout = read_text(stdout_log)
    stderr = read_text(stderr_log)
    classification = parse_kv(read_text(work_dir / "classification.txt"))
    command = read_text(work_dir / "command.txt").strip()
    exit_code = read_text(work_dir / "exit_code.txt").strip()
    instr, cycles, ipc = parse_ipc(stdout)
    counters, present = parse_perf(stderr)
    return Workload(
        name=name,
        exists=work_dir.exists(),
        exit_code=classification.get("exit_code", exit_code or "missing"),
        verdict=classification.get("verdict", "MISSING"),
        pass_reason=classification.get("pass_reason", "-"),
        seconds=classification.get("seconds", "-"),
        good_traps=len(re.findall(r"HIT GOOD TRAP", strip_ansi(stdout))),
        command=command,
        stdout_log=stdout_log,
        stderr_log=stderr_log,
        wave_dir=classification.get("wave_dir", "-"),
        instr=instr,
        cycles=cycles,
        ipc=ipc,
        counters=counters,
        present=present,
    )


def git_output(args: list[str]) -> str:
    try:
        result = subprocess.run(
            ["git", *args],
            check=True,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        return result.stdout.strip()
    except Exception as exc:
        return f"unavailable: {exc}"


def load_workloads(root: Path) -> dict[str, Workload]:
    return {name: parse_workload(root, name) for name in WORKLOAD_ORDER if (root / "matrix" / name).exists()}


def aggregate(workloads: dict[str, Workload]) -> tuple[Counter, set[str]]:
    counters = Counter()
    present = set()
    for work in workloads.values():
        counters.update(work.counters)
        present.update(work.present)
    return counters, present


def fmt_int(value) -> str:
    if value is None:
        return "-"
    return f"{int(value):,}"


def fmt_float(value, digits=6) -> str:
    if value is None:
        return "-"
    return f"{float(value):.{digits}f}"


def fmt_pct(num, den) -> str:
    if num is None or den in (None, 0):
        return "-"
    return f"{num / den:.3%}"


def table(headers: list[str], rows: list[list[str]]) -> str:
    return "\n".join(
        ["| " + " | ".join(headers) + " |", "| " + " | ".join(["---"] * len(headers)) + " |"]
        + ["| " + " | ".join(row) + " |" for row in rows]
    )


def c(counters: Counter, name: str) -> int:
    return int(counters.get(name, 0))


def closure_rows(counters: Counter) -> list[list[str]]:
    resolved = c(counters, "int_er_rob_resolved_by_writeback")
    class_sum = sum(c(counters, name) for name in [
        "int_er_rob_resolved_by_writeback_alu",
        "int_er_rob_resolved_by_writeback_mul",
        "int_er_rob_resolved_by_writeback_div",
        "int_er_rob_resolved_by_writeback_other",
    ])
    final = c(counters, "int_er_rob_wb_resolve_final_candidate")
    outcome_sum = sum(c(counters, name) for name in [
        "int_er_rob_resolved_by_writeback",
        "int_er_rob_wb_resolve_blocked_need_flush",
        "int_er_rob_wb_resolve_blocked_redirect_recovery",
    ])
    st_pending = c(counters, "int_er_rob_st_pending_work_cycle")
    st_pending_sum = sum(c(counters, name) for name in [
        "int_er_rob_st_pending_global_stop_cycle",
        "int_er_rob_st_pending_caught_up_after_scan_cycle",
        "int_er_rob_st_pending_walk_width_limited_cycle",
        "int_er_rob_st_pending_invalid_frontier_cycle",
        "int_er_rob_st_pending_valid_frontier_blocker_cycle",
    ])
    st_blocker = c(counters, "int_er_rob_st_pending_valid_frontier_blocker_cycle")
    st_reason_sum = sum(c(counters, name) for name in [
        "int_er_rob_st_blocker_need_flush_cycle",
        "int_er_rob_st_blocker_not_writebacked_cycle",
        "int_er_rob_st_blocker_writebacked_wait_commit_cycle",
        "int_er_rob_st_blocker_not_resolved_cycle",
    ])
    return [
        ["WB resolved class closure", "resolved_by_writeback", fmt_int(resolved), fmt_int(class_sum), fmt_int(resolved - class_sum), "PASS" if resolved == class_sum else "FAIL"],
        ["WB final-candidate outcome closure", "final_candidate", fmt_int(final), fmt_int(outcome_sum), fmt_int(final - outcome_sum), "PASS" if final == outcome_sum else "FAIL"],
        ["ST pending-work outcome closure", "pending_work", fmt_int(st_pending), fmt_int(st_pending_sum), fmt_int(st_pending - st_pending_sum), "PASS" if st_pending == st_pending_sum else "FAIL"],
        ["ST valid-frontier reason closure", "valid_frontier_blocker", fmt_int(st_blocker), fmt_int(st_reason_sum), fmt_int(st_blocker - st_reason_sum), "PASS" if st_blocker == st_reason_sum else "FAIL"],
    ]


def workload_rows(workloads: dict[str, Workload]) -> list[list[str]]:
    rows = []
    for name in WORKLOAD_ORDER:
        work = workloads.get(name)
        if work is None:
            rows.append([f"`{name}`", "MISSING", "-", "-", "-", "-", "-", "-", "-"])
            continue
        rows.append([
            f"`{name}`",
            work.exit_code,
            work.verdict,
            work.pass_reason,
            work.seconds,
            fmt_int(work.good_traps),
            fmt_int(work.instr),
            fmt_int(work.cycles),
            fmt_float(work.ipc),
        ])
    return rows


def counter_rows(workloads: dict[str, Workload], names: list[str]) -> list[list[str]]:
    rows = []
    totals, _ = aggregate(workloads)
    for name in names:
        row = [f"`{name}`", fmt_int(c(totals, name))]
        for workload in WORKLOAD_ORDER:
            work = workloads.get(workload)
            row.append(fmt_int(c(work.counters, name)) if work else "-")
        rows.append(row)
    return rows


def compare_rows(current: Counter, baseline: dict[str, int], names: list[str]) -> list[list[str]]:
    rows = []
    for name in names:
        before = int(baseline.get(name, 0))
        after = c(current, name)
        rows.append([f"`{name}`", fmt_int(before), fmt_int(after), fmt_int(after - before), fmt_pct(after - before, before)])
    return rows


def st_breakdown_rows(counters: Counter) -> list[list[str]]:
    blocker = c(counters, "int_er_rob_st_pending_valid_frontier_blocker_cycle")
    pending = c(counters, "int_er_rob_st_pending_work_cycle")
    items = [
        "scalar_load",
        "scalar_store",
        "other_integer",
        "csr",
        "branch_jump",
        "fence",
        "amo",
    ]
    rows = []
    for klass in items:
        name = f"int_er_rob_st_blocker_class_{klass}_cycle"
        value = c(counters, name)
        rows.append([klass, f"`{name}`", fmt_int(value), fmt_pct(value, blocker), fmt_pct(value, pending)])
    rows.sort(key=lambda row: int(row[2].replace(",", "")) if row[2] != "-" else 0, reverse=True)
    return rows


def writeback_class_rows(counters: Counter) -> list[list[str]]:
    total = c(counters, "int_er_rob_resolved_by_writeback")
    items = [
        ("alu", "int_er_rob_resolved_by_writeback_alu"),
        ("mul", "int_er_rob_resolved_by_writeback_mul"),
        ("div", "int_er_rob_resolved_by_writeback_div"),
        ("other", "int_er_rob_resolved_by_writeback_other"),
    ]
    return [[label, f"`{name}`", fmt_int(c(counters, name)), fmt_pct(c(counters, name), total)] for label, name in items]


def load_baseline(path: Path) -> dict[str, int]:
    if not path.exists():
        return {}
    data = json.loads(path.read_text(encoding="utf-8"))
    return {k: int(v) for k, v in data.get("feature_counter_totals", {}).items()}


def current_config_note() -> str:
    config_sources = [
        "src/main/scala/top/Configs.scala",
        "src/main/scala/xiangshan/Parameters.scala",
    ]
    hits = []
    for path in config_sources:
        text = read_text(Path(path))
        for needle in [
            "enableOtherIntegerWritebackResolve = true",
            "enableOtherIntegerWritebackResolve = false",
            "enableOtherIntegerWritebackResolve: Boolean = false",
            "def IntEREnableOtherIntegerWritebackResolve",
            "new WithIntEarlyReleaseFunctional",
        ]:
            if needle in text:
                hits.append(f"`{needle}` in `{path}`")
    return "; ".join(hits) if hits else "config source evidence not found"


def generate_report(root: Path, output: Path, baseline_path: Path, allow_incomplete: bool) -> int:
    workloads = load_workloads(root)
    missing = [name for name in WORKLOAD_ORDER if name not in workloads or workloads[name].verdict == "MISSING"]
    nonpass = [
        name for name in WORKLOAD_ORDER
        if name in workloads and workloads[name].verdict not in ("PASS", "MISSING")
    ]
    if (missing or nonpass) and not allow_incomplete:
        print("matrix is incomplete or not all PASS")
        if missing:
            print("missing:", ", ".join(missing))
        if nonpass:
            print("nonpass:", ", ".join(nonpass))
        return 2

    counters, present = aggregate(workloads)
    baseline = load_baseline(baseline_path)
    instr = sum(work.instr for work in workloads.values())
    cycles = sum(work.cycles for work in workloads.values())
    ipc = instr / cycles if cycles else 0.0
    build_dir = root / "build"
    clean_exit = read_text(build_dir / "clean.exit_code.txt").strip()
    build_exit = read_text(build_dir / "build.exit_code.txt").strip()
    fail_name = read_text(root / "matrix" / "fail_name.txt").strip()
    git_head = read_text(build_dir / "git-head.txt").strip() or git_output(["rev-parse", "HEAD"])
    git_status = read_text(build_dir / "git-status-short.txt").strip() or git_output(["status", "--short"])
    debug_records = sorted(Path("mydebug/new-er/records").glob("*other-integer-wb-resolved*.md"))

    lines = [
        "# IntER Other-Integer Writeback-Resolved Implementation Report",
        "",
        "## Summary",
        "",
        "本报告对应 `mydocs/new-er/plan/int-er-other-integer-writeback-resolved-plan.md`。当前实现把严格白名单内的简单 ALU、MUL/DIV、`bku/i2f/i2v` 从 only-commit-resolved 前移到 final accepted writeback 后 resolved。Branch、Load/Store、AMO、CSR、Fence、VSet、FP/Vector、compressed/fusion/multi-uop entry 仍保守等待 actual normal commit。",
        "",
        table(
            ["Item", "Value"],
            [
                ["Artifact root", f"`{root}`"],
                ["Git head captured by runner", f"`{git_head}`"],
                ["Current git HEAD", f"`{git_output(['rev-parse', 'HEAD'])}`"],
                ["Dirty status captured by runner", "`clean`" if not git_status else f"`{git_status}`"],
                ["Clean exit", clean_exit or "-"],
                ["Build exit", build_exit or "-"],
                ["Matrix fail_name", fail_name or "-"],
                ["Total instr", fmt_int(instr)],
                ["Total cycles", fmt_int(cycles)],
                ["Aggregate IPC", fmt_float(ipc)],
            ],
        ),
        "",
        "## Config Evidence",
        "",
        "- `DefaultConfig` 经过 `WithIntEarlyReleaseFunctional`，当前 functional IntER 开启，`observeOnly=false`，并显式开启 `enableOtherIntegerWritebackResolve=true`。",
        f"- Source evidence: {current_config_note()}",
        "- `IntEarlyReleaseParams()` 的裸默认值保持 `enableOtherIntegerWritebackResolve=false`，用于 bisect 和禁用配置。",
        "",
        "## Verification Matrix",
        "",
        "判定规则：除既有 `povray --max-instr` 特例外，以最终 `HIT GOOD TRAP` / runner classification 为准；`rvh-tests` 内部 self-check `FAILED/failed` 文本不覆盖最终 good-trap 判定。",
        "",
        table(["Workload", "Exit", "Verdict", "Reason", "Seconds", "Good traps", "Instr", "Cycles", "IPC"], workload_rows(workloads)),
        "",
        "## Writeback-Resolved Counter Closure",
        "",
        table(["Closure", "LHS", "LHS value", "RHS sum", "Delta", "Status"], closure_rows(counters)),
        "",
        "## Writeback-Resolved Events",
        "",
        table(["Class", "Counter", "Value", "Share of resolved"], writeback_class_rows(counters)),
        "",
        table(["Counter", "Aggregate"] + [f"`{name}`" for name in WORKLOAD_ORDER], counter_rows(workloads, WB_COUNTERS)),
        "",
        "## ST Blocker / Bottleneck Counters",
        "",
        table(["Class", "Counter", "Cycles", "Share of valid-frontier blocker", "Share of pending-work"], st_breakdown_rows(counters)),
        "",
        table(["Counter", "Task34 pre-WB baseline", "Current", "Delta", "Delta / baseline"], compare_rows(counters, baseline, ST_COUNTERS)),
        "",
        "## Free-list, UCA, and IPC Trends",
        "",
        table(["Counter", "Task34 pre-WB baseline", "Current", "Delta", "Delta / baseline"], compare_rows(counters, baseline, TREND_COUNTERS)),
        "",
        "## Debug Protocol",
        "",
    ]
    if debug_records:
        lines.append("本任务存在 debug 记录：")
        lines.extend(f"- `{path}`" for path in debug_records)
    else:
        lines.append("本任务未产生系统级失败 debug 记录；没有 `mydebug/new-er/records/*other-integer-wb-resolved*.md`。")
    lines.extend([
        "",
        "## Log Paths",
        "",
        table(
            ["Workload", "stdout", "stderr", "wave"],
            [
                [
                    f"`{name}`",
                    f"`{workloads[name].stdout_log}`" if name in workloads else "-",
                    f"`{workloads[name].stderr_log}`" if name in workloads else "-",
                    f"`{workloads[name].wave_dir}`" if name in workloads else "-",
                ]
                for name in WORKLOAD_ORDER
            ],
        ),
        "",
        "## Interpretation",
        "",
        "- `int_er_rob_resolved_by_writeback` 统计实际由 final accepted writeback 提前置 resolved 的 entry。",
        "- `int_er_rob_wb_resolve_final_candidate` 到 `resolved / blocked_need_flush / blocked_redirect_recovery` 的闭包用于确认安全拒绝路径没有漏计。",
        "- `int_er_rob_wb_resolve_rejected_identity_reuse_raw` 不参与 accepted-candidate 闭包；它只诊断 raw writeback index 命中但完整 ROB generation/slot owner 不匹配的 stale/reuse 事件。",
        "- `int_er_rob_interrupt_deferred_for_guard_*` 是正确性成本：存在 guard-emitted redefiner 时，ROB 延迟实际 interrupt flush，防止不可撤销 early-free 证明链被异步中断冲刷。",
    ])

    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print(output)
    return 0


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--output", type=Path, default=Path("mydocs/new-er/other-integer-wb-resolved-report.md"))
    parser.add_argument("--baseline", type=Path, default=Path("mydocs/new-er/task34/int-er-bottleneck-performance-counter-summary.json"))
    parser.add_argument("--allow-incomplete", action="store_true")
    args = parser.parse_args()
    return generate_report(args.root, args.output, args.baseline, args.allow_incomplete)


if __name__ == "__main__":
    raise SystemExit(main())
