"""Check an extraction (extract.py) against the module it came from: the
registers are the same, so the check is combinational, every register a
cut point (its Q an input, its D, enable and reset outputs, by the
original register's name in both designs) and a miter of the two proved
by SAT (yosys miter, sat; not the equiv_* passes, which are too slow).

    check.py <yosys> <original.json> <extracted.json> <module> [mutate]
    check.py <yosys> --lemma <entries> <index bits>

With mutate, one gate of the extracted cell is changed first: a negative
control that must come out not equivalent. With mutate-tree, one leaf of
a broadcast's inverter tree (extract.py --fanout-tree) reads the
broadcast itself instead of the root inverter: its eight entries see the
complement, which must come out not equivalent too. The leaf is one of
the broadcast the cell reads at the most pins.

Decode-once is proved in two steps, because a miter of the whole design
no longer collapses once the comparisons are rewritten (everything
downstream of them differs in structure): the extraction with the index
kept (extract.py --keep-idx) against the original, by the miter, and the
lemma that decode-once rests on, (1 << x)[i] == (x == i) for every entry
i and every x: replacing a signal by one equal to it changes nothing.
"""

import json
import os
import re
import subprocess
import sys

from entries import FLOPS, outputs


def run(yosys, script):
    r = subprocess.run([yosys, "-q", "-p", script], capture_output=True, text=True)
    return r.returncode, r.stdout + r.stderr


def cut(mod, renames, tag):
    """The module with every register cut: its Q a port, its other ports
    outputs, all named after the register."""
    ports = {k: v for k, v in mod["ports"].items()}
    cells = {}
    for name, c in mod["cells"].items():
        if not c["type"].startswith("$"):
            # a blackbox (a register file, a macro of its own): cut like a
            # register, its outputs inputs and its inputs outputs
            safe = "".join(ch if ch.isalnum() else "_" for ch in name)
            for p, bs in c["connections"].items():
                out = c["port_directions"][p] == "output"
                ports[f"{'q' if out else 'd'}_box_{safe}_{p}"] = {
                    "direction": "input" if out else "output",
                    "bits": bs,
                }
            continue
        if c["type"] not in FLOPS:
            cells[name] = c
            continue
        # flatten names a cell's flop $flatten\<instance>.<name>
        reg = renames.get(name.removeprefix("$flatten\\"), name)
        safe = "".join(ch if ch.isalnum() else "_" for ch in reg)
        ports[f"q_{safe}"] = {"direction": "input", "bits": c["connections"]["Q"]}
        for p, bs in c["connections"].items():
            if p not in outputs(c):
                ports[f"d_{safe}_{p}"] = {"direction": "output", "bits": bs}
    return {"attributes": {}, "ports": ports, "cells": cells, "netnames": {}}


def lemma(yosys, n, w, work):
    """(1 << x)[i] == (x == i) for i < n, x of w bits, proved by SAT."""
    src = os.path.join(work, f"lemma_{n}_{w}.v")
    with open(src, "w") as f:
        f.write(f"module lemma(input [{w - 1}:0] x, output ok);\n")
        f.write(f"  wire [{n - 1}:0] shifted = {n}'d1 << x;\n")
        f.write(f"  wire [{n - 1}:0] compared;\n")
        f.write(f"  genvar i;\n  generate for (i = 0; i < {n}; i = i + 1)\n")
        f.write(f"    assign compared[i] = x == {w}'d0 + i;\n  endgenerate\n")
        f.write("  assign ok = shifted == compared;\nendmodule\n")
    rc, log = run(
        yosys,
        f"read_verilog {src}; hierarchy -top lemma; proc; flatten; sat -verify -prove ok 1 lemma",
    )
    print(
        f"check: lemma (1 << x)[i] == (x == i), {n} entries, {w}-bit x: {'proved' if rc == 0 else 'FAILED'}"
    )
    sys.exit(rc)


def main(argv):
    if argv[2] == "--lemma":
        lemma(argv[1], int(argv[3]), int(argv[4]), os.getcwd())
    yosys, orig, extracted, module = argv[1:5]
    mutate = len(argv) > 5
    # a Bazel test's runfiles are read-only: its scratch is TEST_TMPDIR
    work = os.environ.get("TEST_TMPDIR") or os.path.dirname(os.path.abspath(extracted))
    flat = os.path.join(work, "check_flat.json")
    src = extracted
    if mutate and argv[5] == "mutate-tree":
        design = json.load(open(extracted))
        top = design["modules"][module]["cells"]
        # the leaves of the broadcast the cell reads most first: a bit
        # the cell compares but nothing observes (one of Uncache's) is a
        # mutant the miter rightly calls equivalent
        cell = design["modules"][module + "Entry"]
        reads = {}
        for c in cell["cells"].values():
            for p, bs in c["connections"].items():
                if c["port_directions"].get(p) == "input":
                    for b in bs:
                        reads[b] = reads.get(b, 0) + 1
        ent = next(c for c in top.values() if c["type"] == module + "Entry")
        port = {bs[0]: p for p, bs in ent["connections"].items() if len(bs) == 1}

        def uses(n):
            y = top[n]["connections"]["Y"][0]
            p = port.get(y)
            return 0 if p is None else reads.get(cell["ports"][p]["bits"][0], 0)

        leaves = sorted(
            (
                n
                for n, c in top.items()
                if c["type"] == module + "FanoutInv" and not n.endswith("_root")
            ),
            key=lambda n: (-uses(n), n),
        )
        if not leaves:
            sys.exit("check: mutate-tree, but the extraction has no inverter tree")
        victim = leaves[int(os.environ.get("CHECK_MUTANT", "0")) % len(leaves)]
        root = top[victim.rsplit("_", 1)[0] + "_root"]
        top[victim]["connections"]["A"] = root["connections"]["A"]
        src = os.path.join(work, "check_mutant.json")
        json.dump(design, open(src, "w"))
        print(f"check: mutant, {victim} reads the broadcast, not its root")
    elif mutate:
        # change one thing that a register sees: swap the inputs of the
        # multiplexer in front of a register's D, from the nth such register
        design = json.load(open(extracted))
        mod = design["modules"][module + "Entry"]
        cell = mod["cells"]
        driver = {
            b: n
            for n, c in cell.items()
            for p, bs in c["connections"].items()
            if c["port_directions"][p] == "output"
            for b in bs
        }
        muxes = []
        for n in sorted(cell):
            c = cell[n]
            if c["type"] in FLOPS:
                m = driver.get(c["connections"]["D"][0])
                if (
                    m
                    and cell[m]["type"] == "$mux"
                    and cell[m]["connections"]["A"] != cell[m]["connections"]["B"]
                ):
                    muxes.append(m)
        nth = int(os.environ.get("CHECK_MUTANT", "0"))
        victim = muxes[nth % len(muxes)]
        cn = cell[victim]["connections"]
        cn["A"], cn["B"] = cn["B"], cn["A"]
        src = os.path.join(work, "check_mutant.json")
        json.dump(design, open(src, "w"))
        print(f"check: mutant, the inputs of {victim} (in front of a register) swapped")
    rc, log = run(
        yosys,
        f"read_json {src}; hierarchy -top {module}; "
        f"setattr -mod -unset keep_hierarchy {module}Entry; flatten; write_json {flat}",
    )
    if rc:
        sys.exit(log[-3000:])
    renames = json.load(open(extracted + ".renames"))
    gold = cut(json.load(open(orig))["modules"][module], {}, "gold")
    gate = cut(json.load(open(flat))["modules"][module], renames, "gate")
    if sorted(gold["ports"]) != sorted(gate["ports"]):
        missing = sorted(set(gold["ports"]) ^ set(gate["ports"]))[:8]
        sys.exit(f"check: the cut designs' ports differ: {missing}")
    both = os.path.join(work, "check_both.json")
    json.dump({"modules": {"gold": gold, "gate": gate}}, open(both, "w"))
    # the miter as an AIG, proved by ABC's iprove: SAT sweeping proves the
    # two sides' internal nodes equal one by one (a comparison yosys wrote
    # as a reduction against the same comparison as an $eq) and merges what
    # follows, so the proof stays as small as the change, where yosys's sat
    # takes the whole design as one problem once any form differs
    aig = os.path.join(work, "check_miter.aig")
    rc, log = run(
        yosys,
        # no -ignore_gold_x and no opt -full: with $eqx and the full
        # optimisation the miter folded to a constant and every verdict was
        # empty
        f"read_json {both}; miter -equiv -flatten gold gate miter; "
        "hierarchy -top miter; "
        "opt -fast; techmap; opt -fast; setundef -zero; aigmap; "
        f"write_aiger -zinit -ascii {aig}.aag; write_aiger -zinit {aig}",
    )
    if rc:
        sys.exit("check: writing the miter failed\n" + log[-2000:])
    abc = os.path.join(os.path.dirname(yosys), "yosys-abc")
    stats = subprocess.run(
        [abc, "-c", f"read_aiger {aig}; print_stats"], capture_output=True, text=True
    ).stdout
    ands = re.search(r"and\s*=\s*(\d+)", stats)
    if not ands:
        sys.exit(f"check: ABC could not read the miter: {stats.strip()[-200:]}")
    # ABC hashes the AIG as it reads it: where the two sides are the same
    # structure they merge and the miter's gates fall away (0 left is a
    # proof by structure); the negative control (mutate) is what shows the
    # check is not blind, its miter keeping gates and coming out different
    print(f"check: miter of {ands.group(1)} AND gates after structural hashing")
    r = subprocess.run(
        [abc, "-c", f"read_aiger {aig}; strash; iprove"], capture_output=True, text=True
    )
    out = r.stdout + r.stderr
    # the miter's one output, trigger, is 1 where the two sides differ
    if "UNSATISFIABLE" in out or "Networks are equivalent" in out:
        rc = 0
    elif "SATISFIABLE" in out:
        rc = 1
    else:
        sys.exit("check: ABC gave no verdict\n" + out[-2000:])
    verdict = "equivalent" if rc == 0 else "NOT equivalent"
    print(f"check: {module}: {verdict}")
    sys.exit(0 if (rc == 0) != mutate else 1)


if __name__ == "__main__":
    main(sys.argv)
