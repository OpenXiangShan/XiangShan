#!/usr/bin/env python3
"""
Usage:
    nightly_perf_diff.py <current_score> <previous_score>

Compare two SPEC score.txt files and print a table of scores, percent
changes and coverage markers (✔/✘).
"""
import sys
import re
from pathlib import Path


def parse_score(path: Path) -> dict:
    # Parse a score.txt file and return a mapping bench -> (score, cov).
    out = {}
    with path.open() as f:
        for line in f:
            if re.match(r"^\d", line) or line.startswith("SPEC"):
                parts = line.split()
                if not parts:
                    continue
                bench = parts[0].rstrip(":")
                # determine score and coverage fields
                score = ""
                cov = ""
                if len(parts) >= 3:
                    # assume last two are score and coverage
                    score = parts[-2]
                    cov = parts[-1]
                elif len(parts) == 2:
                    score = parts[-1]
                out[bench] = (score, cov)
    return out


def main():
    if len(sys.argv) != 3:
        prog = Path(sys.argv[0]).name
        print(f"Usage: {prog} CURRENT_SCORE PREVIOUS_SCORE", file=sys.stderr)
        sys.exit(1)
    cur_path = Path(sys.argv[1])
    prev_path = Path(sys.argv[2])

    if not cur_path.exists() or not prev_path.exists():
        print("Both score files must exist", file=sys.stderr)
        sys.exit(2)

    curr = parse_score(cur_path)
    prev = parse_score(prev_path)

    # print coverage mismatches first (skip SPEC lines)
    delta_cov = []
    for bench in sorted(set(curr) | set(prev)):
        if bench.startswith("SPEC"):
            continue
        cv = curr.get(bench, ("", ""))[1]
        pv = prev.get(bench, ("", ""))[1]
        if cv != pv:
            delta_cov.append((bench, cv, pv))
    if delta_cov:
        print("Coverage mismatches:")
        for bench, cv, pv in delta_cov:
            print(f"  {bench}: curr {cv} prev {pv}")
        print()

    # table of scores with wider spacing (exclude SPEC)
    print("```")
    print("Bench            curr_score   prev_score      diff_pct   cov_change")
    benches = [b for b in sorted(set(curr) | set(prev)) if not b.startswith("SPEC")]
    for bench in benches:
        cs, cv = curr.get(bench, ("", ""))
        ps, pv = prev.get(bench, ("", ""))
        diff_pct = ""
        try:
            c = float(cs); p = float(ps)
            diffval = c - p
            if p != 0:
                pct = diffval / p * 100
                if pct > 0:
                    emoji = "🟢"
                elif pct < 0:
                    emoji = "🔴"
                else:
                    emoji = "⚪"
                diff_pct = f"{pct:+.2f}%{emoji}"
        except Exception:
            pass
        mark = "✔" if cv == pv else "✘"
        print(f"{bench:15} {cs:>10}   {ps:>10}   {diff_pct:>11}     {mark}")
    print("```")

    # summary for SPEC metrics
    for key in ["SPECint2006/GHz", "SPECfp2006/GHz", "SPEC2006/GHz"]:
        if key in curr or key in prev:
            cs = curr.get(key, ("", ""))[0]
            ps = prev.get(key, ("", ""))[0]
            diff_pct = ""
            try:
                c = float(cs); p = float(ps)
                if p != 0:
                    pct = (c - p) / p * 100
                    emoji = "🟢" if pct > 0 else "🔴" if pct < 0 else ""
                    diff_pct = f"{pct:+.2f}%{emoji}"
            except Exception:
                pass
            print(f"{key:20} curr {cs} prev {ps} diff {diff_pct}")

if __name__ == "__main__":
    main()
