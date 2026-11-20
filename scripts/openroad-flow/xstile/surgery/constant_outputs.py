#!/usr/bin/env python3
"""Outputs of kept modules that yosys proves constant, from write_json.

Run after `proc; opt -full` with the hierarchy kept: an output port whose
every bit yosys's json backend writes as "0" or "1" is driven by a
constant inside its module, which the parent cannot see across a kept
boundary. One row per such port, with the bits as a Verilog literal.

Usage: constant_outputs.py select --plan plan.json --plan-bzl plan.bzl --out select.ys
       constant_outputs.py scan <design.json> --plan plan.json --plan-bzl plan.bzl --out rows.json
"""

import argparse
import json
import re
import sys

_KEEP = re.compile(r'"SYNTH_KEEP_MODULES":\s*"([^"]*)"')


def kept(plan_json, plan_bzl):
    """The modules the plan keeps: blocks, netlists, SYNTH_KEEP_MODULES."""
    keep = set(m if isinstance(m, str) else m["name"] for m in plan_json["macros"])
    keep |= set(n["module"] for n in plan_json.get("netlists", []))
    keep |= set(plan_json["parent"].get("keep", []))
    for group in _KEEP.findall(plan_bzl):
        keep |= set(group.split())
    return keep


def literal(bits):
    # json lists bits LSB first.
    return "%d'b%s" % (len(bits), "".join(reversed(bits)))


def scan(design, keep):
    rows = []
    partial = 0
    scanned = []
    for name, mod in sorted(design["modules"].items()):
        base = name.split("$")[0]
        if base not in keep:
            continue
        scanned.append(name)
        for port, info in sorted(mod.get("ports", {}).items()):
            if info["direction"] != "output":
                continue
            bits = [str(b) for b in info["bits"]]
            const = [b in ("0", "1") for b in bits]
            if all(const):
                rows.append({"module": name, "port": port, "value": literal(bits)})
            elif any(const):
                partial += 1
    if not scanned:
        raise SystemExit("constant_outputs: no kept module in the design")
    return rows, partial, scanned


def main():
    p = argparse.ArgumentParser()
    p.add_argument("cmd", choices=["select", "scan"])
    p.add_argument("design", nargs="?")
    p.add_argument("--plan", required=True)
    p.add_argument("--plan-bzl", required=True)
    p.add_argument("--out", required=True)
    a = p.parse_args()
    with open(a.plan) as f:
        plan_json = json.load(f)
    with open(a.plan_bzl) as f:
        keep = kept(plan_json, f.read())
    if a.cmd == "select":
        # slang names a module instanced more than once after each
        # instance's path: <name>$<path>.
        with open(a.out, "w") as f:
            f.write("select %s\n" % " ".join("%s %s$*" % (m, m) for m in sorted(keep)))
        return
    with open(a.design) as f:
        design = json.load(f)
    rows, partial, scanned = scan(design, keep)
    with open(a.out, "w") as f:
        json.dump(
            {
                "constant": rows,
                "partially_constant_ports": partial,
                "modules_scanned": scanned,
            },
            f,
            indent=1,
            sort_keys=True,
        )
        f.write("\n")
    sys.stdout.write(
        "%d constant outputs, %d partially constant\n" % (len(rows), partial)
    )


if __name__ == "__main__":
    main()
