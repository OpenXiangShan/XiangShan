#!/usr/bin/env python3
"""Rank repeated L2 and ZhuJiang LLC evictions from ChiselDB SQLite traces."""

import argparse
import json
import re
import sqlite3


TABLES = re.compile(r"^(L2LineReplacementTrace_hart\d+_slice\d+|ZJ_LLC_Lifecycle_bank\d+)$")
COLUMN_NAMES = {"HNFBANK": "hnf_bank", "DIRBANK": "dir_bank", "SETIDX": "set_idx", "LINEADDR": "line_addr"}


def analyze(db, start=None, end=None, top=10):
    if top < 1:
        raise ValueError("top must be positive")
    if start is not None and end is not None and end < start:
        raise ValueError("end must be no earlier than start")

    tables = sorted(row[0] for row in db.execute("SELECT name FROM sqlite_master WHERE type='table'")
                    if TABLES.fullmatch(row[0]))
    results = []
    for table in tables:
        l3 = table.startswith("ZJ_LLC_")
        fields = ["HNFBANK", "DIRBANK"] if l3 else []
        line_fields = fields + ["SETIDX", "LINEADDR"]
        set_fields = fields + ["SETIDX"]
        conditions = ["EVENTTYPE=2", "VICTIMVALID=1"] if l3 else []
        params = []
        if start is not None:
            conditions.append("STAMP>=?")
            params.append(start)
        if end is not None:
            conditions.append("STAMP<=?")
            params.append(end)
        where = " WHERE " + " AND ".join(conditions) if conditions else ""
        source = f'"{table}"{where}'

        total = db.execute(f"SELECT COUNT(*) FROM {source}", params).fetchone()[0]
        first, last = db.execute(f"SELECT MIN(STAMP), MAX(STAMP) FROM {source}", params).fetchone()
        window_start = start if start is not None else first
        window_end = end if end is not None else last
        window_cycles = max(0, window_end - window_start + 1) if window_start is not None and window_end is not None else 0
        grouped_lines = ", ".join(line_fields)
        unique, repeated = db.execute(
            "SELECT COUNT(*), COALESCE(SUM(CASE WHEN hits >= 2 THEN 1 ELSE 0 END), 0) "
            f"FROM (SELECT COUNT(*) AS hits FROM {source} GROUP BY {grouped_lines})", params
        ).fetchone()

        def rank(group_fields):
            columns = ", ".join(group_fields)
            rows = db.execute(
                f"SELECT {columns}, COUNT(*) AS hits FROM {source} "
                f"GROUP BY {columns} ORDER BY hits DESC, {columns} LIMIT ?", (*params, top)
            )
            ranked = []
            for row in rows:
                entry = dict(zip([COLUMN_NAMES[field] for field in group_fields] + ["count"], row))
                entry["per_million_cycles"] = round(entry["count"] * 1000000 / window_cycles, 2) if window_cycles else 0.0
                ranked.append(entry)
            return ranked

        results.append({
            "layer": "L3" if l3 else "L2",
            "table": table,
            "total": total,
            "window_cycles": window_cycles,
            "window_basis": "requested" if start is not None and end is not None else "observed_span",
            "unique_lines": unique,
            "repeated_lines": repeated,
            "top_lines": rank(line_fields),
            "top_sets": rank(set_fields),
        })
    return results


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("db", help="ChiselDB SQLite output from an instrumented simulation")
    parser.add_argument("--start", type=int, help="first included table STAMP cycle")
    parser.add_argument("--end", type=int, help="last included table STAMP cycle")
    parser.add_argument("--top", type=int, default=10)
    parser.add_argument("--json", action="store_true", help="emit machine-readable results")
    args = parser.parse_args()
    try:
        with sqlite3.connect(f"file:{args.db}?mode=ro", uri=True) as db:
            results = analyze(db, args.start, args.end, args.top)
    except (sqlite3.Error, ValueError) as error:
        parser.error(str(error))
    if not results:
        parser.error("no L2LineReplacementTrace or ZJ_LLC_Lifecycle tables found")
    if args.json:
        print(json.dumps(results, indent=2))
        return
    for result in results:
        total = result["total"]
        print(f'{result["layer"]} {result["table"]}: {total} replacements in '
              f'{result["window_cycles"]} cycles ({result["window_basis"]}), '
              f'{result["unique_lines"]} distinct lines, {result["repeated_lines"]} repeated lines')
        for name, ranked in (("Lines", result["top_lines"]), ("Sets", result["top_sets"])):
            print(f"  {name}:")
            for entry in ranked:
                identity = ", ".join(f"{key}={hex(value) if key == 'line_addr' else value}"
                                     for key, value in entry.items() if key not in ("count", "per_million_cycles"))
                share = 100 * entry["count"] / total if total else 0
                print(f'    {identity}: {entry["count"]} ({share:.2f}%, '
                      f'{entry["per_million_cycles"]:.2f}/million cycles)')


if __name__ == "__main__":
    main()
