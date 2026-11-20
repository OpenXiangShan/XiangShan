#!/usr/bin/env python3
"""Cells and flip-flop bits per kept module, from yosys write_json.

Run on the kept modules after `proc; opt -fast -purge`, once on the RTL
without the surgery and once with it: the difference is what the folded
constants remove before synthesis maps anything. Coarse cells, not
library cells; flip-flop bits are $dff-family cell widths.

Usage: kept_cells.py <design.json> --out cells.json
"""

import argparse
import json
import sys


def count(design):
    out = {}
    for name, mod in sorted(design["modules"].items()):
        cells = mod.get("cells", {})
        ff = 0
        for cell in cells.values():
            t = cell["type"]
            if t.startswith("$") and (
                "dff" in t or t in ("$dlatch", "$adlatch", "$sr")
            ):
                # json writes parameters as binary strings.
                ff += int(cell["parameters"]["WIDTH"], 2)
        ports = sum(len(p["bits"]) for p in mod.get("ports", {}).values())
        out[name] = {"cells": len(cells), "ff_bits": ff, "port_bits": ports}
    return out


def main():
    p = argparse.ArgumentParser()
    p.add_argument("design")
    p.add_argument("--out", required=True)
    a = p.parse_args()
    with open(a.design) as f:
        result = count(json.load(f))
    with open(a.out, "w") as f:
        json.dump(result, f, indent=1, sort_keys=True)
        f.write("\n")
    total = {
        k: sum(v[k] for v in result.values()) for k in ("cells", "ff_bits", "port_bits")
    }
    sys.stdout.write("%d modules: %s\n" % (len(result), total))


if __name__ == "__main__":
    main()
