#!/usr/bin/env python3
"""Check a complete-from-reset PDBVictimObserver ChiselDB trace, read-only."""

import argparse
from collections import OrderedDict
import json
from pathlib import Path
import re
import sqlite3
from urllib.parse import quote


def check_trace(database, table="PDBVictimObserver0", entries=256, lanes=3):
    if entries < 1 or lanes < 1:
        raise ValueError("entries and lanes must be positive")
    if not re.fullmatch(r"[A-Za-z_][A-Za-z_0-9]*", table):
        raise ValueError("invalid table name")
    fifo = OrderedDict()
    totals = dict(rows=0, victims=0, demand_queries=0, usedVictimHits=0,
                  unusedVictimHits=0, streamUsedVictimHits=0,
                  streamUnusedVictimHits=0, duplicate_victims=0,
                  fifo_full_evictions=0)
    previous_stamp = -1
    uri = "file:" + quote(str(Path(database).resolve())) + "?mode=ro"
    with sqlite3.connect(uri, uri=True) as connection:
        connection.row_factory = sqlite3.Row
        for raw in connection.execute(f'SELECT * FROM "{table}" ORDER BY STAMP, ID'):
            row = {key.lower(): raw[key] for key in raw.keys()}

            def equal(field, expected):
                actual = row[field.lower()]
                if actual != expected:
                    raise ValueError(f"row {row['id']}, stamp {row['stamp']}: "
                                     f"{field}={actual}, expected {expected}")

            if row["stamp"] <= previous_stamp:
                raise ValueError("expected one strictly ordered event batch per cycle")
            previous_stamp = row["stamp"]
            queries = [row[f"demand_{i}_bits"] for i in range(lanes)
                       if row[f"demand_{i}_valid"]]
            victim = row["victim_bits_blockaddr"] if row["victim_valid"] else None
            duplicate = victim is not None and victim in fifo
            hits = [fifo.pop(addr) for addr in set(queries) if addr in fifo]
            counts = {
                "usedHits": sum(used for used, _ in hits),
                "unusedHits": sum(not used for used, _ in hits),
                "streamUsedHits": sum(used and stream for used, stream in hits),
                "streamUnusedHits": sum(not used and stream for used, stream in hits),
            }
            eviction = None
            if victim is not None:
                fifo.pop(victim, None)
                if len(fifo) == entries:
                    eviction = fifo.popitem(last=False)
                fifo[victim] = (bool(row["victim_bits_used"]), bool(row["victim_bits_stream"]))
            for field, expected in counts.items():
                equal(field, expected)
            equal("duplicate", int(duplicate))
            equal("eviction_valid", int(eviction is not None))
            if eviction is not None:
                addr, (used, stream) = eviction
                equal("eviction_bits_blockaddr", addr)
                equal("eviction_bits_used", int(used))
                equal("eviction_bits_stream", int(stream))
            equal("occupancy", len(fifo))
            totals["rows"] += 1
            totals["victims"] += int(victim is not None)
            totals["demand_queries"] += len(queries)
            totals["duplicate_victims"] += int(duplicate)
            totals["fifo_full_evictions"] += int(eviction is not None)
            for field, total in (("usedHits", "usedVictimHits"), ("unusedHits", "unusedVictimHits"),
                                 ("streamUsedHits", "streamUsedVictimHits"),
                                 ("streamUnusedHits", "streamUnusedVictimHits")):
                totals[total] += counts[field]
    if totals["rows"] == 0:
        raise ValueError("empty trace cannot verify the observer")
    return dict(totals, final_occupancy=len(fifo), entries=entries, table=table)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("database", type=Path)
    parser.add_argument("--table", default="PDBVictimObserver0")
    parser.add_argument("--entries", type=int, default=256)
    args = parser.parse_args()
    try:
        result = check_trace(args.database, args.table, args.entries)
    except (ValueError, KeyError, sqlite3.Error) as error:
        parser.exit(1, f"Victim trace check failed: {error}\n")
    print(json.dumps(result, indent=2))


if __name__ == "__main__":
    main()
