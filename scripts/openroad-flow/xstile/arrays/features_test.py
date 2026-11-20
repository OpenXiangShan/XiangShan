"""The front end end to end on a design small enough to read: an 8-entry
CAM written as XiangShan's generator writes its queues (data_<i> and
valid_<i>, each written where a broadcast index equals i), its match
reduced across entries, and a submodule kept a blackbox.

    features_test.py <yosys> <slang.so> <fold_entries.tcl>

Checked, each as the feature that makes XSTile's blocks extractable:
the entries found and every one following one rule; the index comparison
decoded once in the parent; the match left in the entry cell (the read
side); the blackbox kept an instance of the block; the extraction proven
equal to the design and the negative control proven different; and
slang's per-instance copies of the entry cell folded onto one module.

A second design, a queue whose entries are freed below a bound, checks
thresholds: x >= i + 1 and x < i + 1, written as firtool folds them for
each i (VirtualLoadQueue's entryHitBound), read as one form and decoded
once.

A fourth, a queue that retires two entries a cycle as XiangShan's
store queue does (VirtualStoreQueue): entry i is freed where
deq_k == i and the retire carry c_k holds, c_1 = c_0 && the entry at
deq_1 is done, a serial carry. The enable is decoded with the index,
(1 << deq_k) & {c_k}, so each cell gets one wire and c_k reaches the decoder
alone, not every entry (as the write enable of the CAM above).

A third, a CAM with a priority select in banks of four, checks a read
side that differs between entries (LoadQueueRAW): entry i's match is
read through one inverter per entry above it in its bank, a different
number in every entry, and the match itself is what every entry has.
Its match registers against q1 and q2, named as firtool names a vector
(m_r, m_r_1, ..., m_r_15; word p * 8 + i entry i's on port p, as
Uncache's f1_ptagMatches_r), are two arrays of the eight entries.
"""

import json
import os
import re
import subprocess
import sys
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
N = 8


SUB = "module Sub(input [7:0] a, output [7:0] y);\n  assign y = a ^ 8'h5a;\nendmodule\n"


def cam(query_copy=False):
    """query_copy: each entry compares against its own copy of q + 1, the
    same adder written once per entry, as firtool writes a per-entry
    computation of a shared input."""
    lines = [
        "module Cam(input clock, input reset, input we, input [2:0] widx,",
        "           input [7:0] wdata, input [7:0] q, output hit, output [7:0] mixed);",
        "  wire [7:0] wd;",
        "  Sub sub (.a(wdata), .y(wd));",
    ]
    for i in range(N):
        lines += [
            f"  reg [7:0] data_{i};",
            f"  reg valid_{i};",
            f"  wire [7:0] q_{i} = q + 8'd1;" if query_copy else "",
            "  always @(posedge clock) begin",
            f"    if (reset) valid_{i} <= 1'b0;",
            f"    else if (we && widx == 3'd{i}) valid_{i} <= 1'b1;",
            f"    if (we && widx == 3'd{i}) data_{i} <= wd;",
            "  end",
        ]
    match = " | ".join(
        f"(valid_{i} && data_{i} == {f'q_{i}' if query_copy else 'q'})"
        for i in range(N)
    )
    lines += [
        f"  assign hit = {match};",
        "  assign mixed = " + " ^ ".join(f"data_{i}" for i in range(N)) + ";",
        "endmodule",
    ]
    return "\n".join(lines) + "\n"


def threshold_queue():
    """Entry i frees when lo < i + 1 and bound >= i + 1, both written as
    firtool writes them: a reduction of the top bits at a power of two,
    x[3] alone at 8, a comparison elsewhere."""

    def ge(x, k):  # x >= k, 4 bits
        if k & (k - 1) == 0:
            top = k.bit_length() - 1
            return f"{x}[3]" if top == 3 else f"(|{x}[3:{top}])"
        return f"({x} > 4'd{k - 1})"

    def lt(x, k):  # x < k, 4 bits
        if k == 1:
            return f"({x} == 4'h0)"
        if k & (k - 1) == 0:
            top = k.bit_length() - 1
            return f"(~{x}[3])" if top == 3 else f"(~|{x}[3:{top}])"
        return f"({x} < 4'd{k})"

    lines = [
        "module Queue(input clock, input reset, input we, input [2:0] widx,",
        "             input [7:0] wdata, input [3:0] lo, input [3:0] bound,",
        "             output any, output [7:0] mixed);",
    ]
    for i in range(N):
        lines += [
            f"  reg [7:0] data_{i};",
            f"  reg live_{i};",
            f"  wire free_{i} = {lt('lo', i + 1)} & {ge('bound', i + 1)};",
            "  always @(posedge clock) begin",
            f"    if (reset) live_{i} <= 1'b0;",
            f"    else if (we && widx == 3'd{i}) live_{i} <= 1'b1;",
            f"    else if (free_{i}) live_{i} <= 1'b0;",
            f"    if (we && widx == 3'd{i}) data_{i} <= wdata;",
            "  end",
        ]
    lines += [
        "  assign any = " + " | ".join(f"live_{i}" for i in range(N)) + ";",
        "  assign mixed = " + " ^ ".join(f"data_{i}" for i in range(N)) + ";",
        "endmodule",
    ]
    return "\n".join(lines) + "\n"


def ordered_queue():
    """Entries compared with a pointer by order, i < a, i > a, i >= b and
    i <= b, as the Rob's entries are against its pointers: each a
    threshold on the pointer, decoded once. The index through a wire, as
    firtool writes it: a literal 0 > a would be folded to 0 in entry 0
    alone, and the entries would not look alike."""
    lines = [
        "module Ordered(input clock, input reset, input we, input [2:0] widx,",
        "               input [3:0] a, input [3:0] b, output any, output old);",
    ]
    for i in range(N):
        lines += [
            f"  reg live_{i};",
            f"  reg old_{i};",
            f"  wire [3:0] k_{i} = 4'd{i};",
            "  always @(posedge clock) begin",
            f"    if (reset) live_{i} <= 1'b0;",
            f"    else if (we && widx == 3'd{i}) live_{i} <= 1'b1;",
            f"    else if (k_{i} < a) live_{i} <= 1'b0;",
            f"    old_{i} <= live_{i} & ((k_{i} > a) ^ (k_{i} >= b) ^ (k_{i} <= b));",
            "  end",
        ]
    lines += [
        "  assign any = " + " | ".join(f"live_{i}" for i in range(N)) + ";",
        "  assign old = " + " ^ ".join(f"old_{i}" for i in range(N)) + ";",
        "endmodule",
    ]
    return "\n".join(lines) + "\n"


def priority_cam():
    lines = [
        "module PriCam(input clock, input reset, input we, input [2:0] widx,",
        "              input [7:0] wdata, input [7:0] q0, input [7:0] q1, input [7:0] q2,",
        "              output [7:0] first, output any, output [15:0] seen);",
    ]
    for i in range(N):
        lines += [
            f"  reg [7:0] data_{i};",
            f"  reg valid_{i};",
            "  always @(posedge clock) begin",
            f"    if (reset) valid_{i} <= 1'b0;",
            f"    else if (we && widx == 3'd{i}) valid_{i} <= 1'b1;",
            f"    if (we && widx == 3'd{i}) data_{i} <= wdata;",
            "  end",
            f"  wire hit_{i} = valid_{i} && data_{i} == q0;",
        ]
    for i in range(N):
        # the first hit in its bank of four, each lower entry inverted
        # where it is used, as firtool writes it
        lower = "".join(f" & ~hit_{j}" for j in range(i - i % 4, i))
        lines.append(f"  assign first[{i}] = hit_{i}{lower};")
    lines.append("  assign any = " + " | ".join(f"hit_{i}" for i in range(N)) + ";")
    regs = []
    for p in range(2):
        for i in range(N):
            k = p * N + i
            name = "m_r" if k == 0 else f"m_r_{k}"
            regs.append(name)
            lines += [
                f"  reg {name};",
                f"  always @(posedge clock) {name} <= valid_{i} && data_{i} == q{p + 1};",
            ]
    lines += [
        "  assign seen = {" + ", ".join(reversed(regs)) + "};",
        "endmodule",
    ]
    return "\n".join(lines) + "\n"


def retire_queue():
    lines = [
        "module Retire(input clock, input reset, input we, input [2:0] widx,",
        "              input [7:0] wdata, input go, input [2:0] deq0,",
        "              input [2:0] deq1, output any, output [7:0] mixed);",
    ]
    for i in range(N):
        lines += [
            f"  reg [7:0] data_{i};",
            f"  reg live_{i};",
            f"  reg done_{i};",
        ]
    done = "{" + ", ".join(f"done_{i}" for i in reversed(range(N))) + "}"
    lines += [
        f"  wire [7:0] done = {done};",
        "  wire c0 = go && done[deq0];",
        "  wire c1 = c0 && done[deq1];",
    ]
    for i in range(N):
        lines += [
            "  always @(posedge clock) begin",
            f"    if (reset) live_{i} <= 1'b0;",
            f"    else if (we && widx == 3'd{i}) live_{i} <= 1'b1;",
            f"    else if ((c0 && deq0 == 3'd{i}) || (c1 && deq1 == 3'd{i})) live_{i} <= 1'b0;",
            f"    if (we && widx == 3'd{i}) begin data_{i} <= wdata; done_{i} <= wdata[0]; end",
            "  end",
        ]
    lines += [
        "  assign any = " + " | ".join(f"live_{i}" for i in range(N)) + ";",
        "  assign mixed = " + " ^ ".join(f"data_{i}" for i in range(N)) + ";",
        "endmodule",
    ]
    return "\n".join(lines) + "\n"


def run(*args):
    # under Bazel the interpreter runs with PYTHONSAFEPATH, so a script
    # does not see the modules beside it unless told
    env = dict(os.environ, PYTHONPATH=HERE)
    r = subprocess.run(
        [sys.executable, *args], capture_output=True, text=True, cwd=HERE, env=env
    )
    return r.returncode, r.stdout + r.stderr


class Features(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # runfiles paths, made absolute: the steps run beside their scripts
        cls.yosys, cls.slang, cls.fold = map(os.path.abspath, sys.argv[1:4])
        cls.work = os.environ.get("TEST_TMPDIR", HERE)
        cls.src = os.path.join(cls.work, "cam.sv")
        with open(cls.src, "w") as f:
            f.write(SUB + cam())
        cls.proc = os.path.join(cls.work, "cam.json")
        rc, log = run(
            "proc_json.py", cls.yosys, cls.slang, cls.src, "Cam", cls.proc, "Sub"
        )
        assert rc == 0, log
        cls.found = run("entries.py", cls.proc, "Cam")[1]
        cls.extracted = os.path.join(cls.work, "extracted.json")
        rc, cls.extract_log = run("extract.py", cls.proc, "Cam", cls.extracted)
        assert rc == 0, cls.extract_log
        cls.stats = dict(re.findall(r"(\w+) (\d+)", cls.extract_log))

    def test_entries_follow_one_rule(self):
        self.assertIn("uniform: 8 of 8 entries follow one rule", self.found)
        self.assertEqual(self.stats["entries"], str(N))

    def test_index_decoded_once(self):
        self.assertEqual(self.stats["decoded_once"], "1")
        self.assertEqual(self.stats["index_sites_left"], "0")

    def test_write_enable_decoded_once(self):
        # we && widx == i: (1 << widx) & {we} in the parent, one wire per cell
        self.assertEqual(self.stats["enables_decoded"], "1")

    def test_match_in_the_cell(self):
        # each entry's compare against q stays in its cell; one bit leaves
        self.assertIn("read side: 1 bits leave each entry's read cone", self.found)

    def test_blackbox_kept(self):
        top = json.load(open(self.extracted))["modules"]["Cam"]
        types = [c["type"] for c in top["cells"].values()]
        self.assertEqual(types.count("Sub"), 1)
        self.assertEqual(types.count("CamEntry"), N)

    def test_extraction_equal(self):
        rc, log = run("check.py", self.yosys, self.proc, self.extracted, "Cam")
        self.assertEqual(rc, 0, log)
        self.assertIn("check: Cam: equivalent", log)

    def test_control_differs(self):
        rc, log = run(
            "check.py", self.yosys, self.proc, self.extracted, "Cam", "mutate"
        )
        self.assertEqual(rc, 0, log)
        self.assertIn("check: Cam: NOT equivalent", log)

    def test_fold(self):
        verilog = os.path.join(self.work, "extracted.v")
        r = subprocess.run(
            [
                self.yosys,
                "-q",
                "-p",
                f"read_json {self.extracted}; hierarchy -top Cam; write_verilog -noattr {verilog}",
            ],
            capture_output=True,
            text=True,
        )
        self.assertEqual(r.returncode, 0, r.stderr)
        # the blackbox's definition stays the core's, as in XSTile
        sub = os.path.join(self.work, "sub.sv")
        with open(sub, "w") as f:
            f.write(SUB)
        listing = os.path.join(self.work, "modules.txt")
        script = os.path.join(self.work, "fold.tcl")
        with open(script, "w") as f:
            f.write(
                "yosys -import\n"
                f"plugin -i {self.slang}\n"
                "yosys read_slang --keep-hierarchy --top Cam "
                f"--blackboxed-module Sub {verilog} {sub}\n"
                "yosys hierarchy -check -top Cam\n"
                f"source {self.fold}\n"
                f"yosys tee -q -o {listing} select -list-mod =*\n"
            )
        env = dict(os.environ, DESIGN_NAME="Cam", XS_FOLD_MODULES="CamEntry")
        r = subprocess.run(
            [self.yosys, "-q", "-c", script], capture_output=True, text=True, env=env
        )
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
        self.assertIn(f"CamEntry, {N} copies folded onto one", r.stdout)
        modules = sorted(m.strip().lstrip("\\") for m in open(listing) if m.strip())
        self.assertEqual(modules, ["Cam", "CamEntry", "Sub"])


class Tree(unittest.TestCase):
    """The CAM's broadcasts (the write data, the query) through two
    levels of inverters, a module of one $not each: proven through the
    tree, and one leaf reading the broadcast instead of its root comes
    out different."""

    QUERY_COPY = False

    @classmethod
    def setUpClass(cls):
        cls.yosys, cls.slang = map(os.path.abspath, sys.argv[1:3])
        cls.work = os.environ.get("TEST_TMPDIR", HERE)
        tag = "cam_tree_copy" if cls.QUERY_COPY else "cam_tree"
        src = os.path.join(cls.work, tag + ".sv")
        with open(src, "w") as f:
            f.write(SUB + cam(cls.QUERY_COPY))
        cls.proc = os.path.join(cls.work, tag + ".json")
        rc, log = run("proc_json.py", cls.yosys, cls.slang, src, "Cam", cls.proc, "Sub")
        assert rc == 0, log
        cls.extracted = os.path.join(cls.work, tag + "_extracted.json")
        rc, log = run(
            "extract.py",
            cls.proc,
            "Cam",
            cls.extracted,
            "--fanout-tree=4",
            *(["--tree-copies"] if cls.QUERY_COPY else []),
        )
        assert rc == 0, log
        cls.stats = dict(re.findall(r"(\w+) (\d+)", log))

    def test_broadcasts_treed(self):
        self.assertGreater(int(self.stats["fanout_trees"]), 0)

    def test_clock_not_treed(self):
        cam = json.load(open(self.extracted))["modules"]["Cam"]
        clock = cam["ports"]["clock"]["bits"]
        ents = [c for c in cam["cells"].values() if c["type"] == "CamEntry"]
        self.assertTrue(ents)
        for e in ents:
            self.assertTrue(
                any(bs == clock for bs in e["connections"].values()),
                "an entry's clock is not the block's clock",
            )

    def test_tree_equal(self):
        rc, log = run("check.py", self.yosys, self.proc, self.extracted, "Cam")
        self.assertEqual(rc, 0, log)
        self.assertIn("check: Cam: equivalent", log)

    def test_leaf_control_differs(self):
        rc, log = run(
            "check.py", self.yosys, self.proc, self.extracted, "Cam", "mutate-tree"
        )
        self.assertEqual(rc, 0, log)
        self.assertIn("check: Cam: NOT equivalent", log)


class TreeCopy(Tree):
    """The query reaches each entry through its own copy of q + 1, one
    adder per entry in the block: treed as the one broadcast synthesis
    would merge it into, proven, and the leaf control still differs."""

    QUERY_COPY = True

    def test_copies_treed(self):
        self.assertEqual(int(self.stats["fanout_trees_shared"]), 8)

    def test_copies_left_alone_by_default(self):
        out = os.path.join(self.work, "cam_tree_copy_default.json")
        rc, log = run("extract.py", self.proc, "Cam", out, "--fanout-tree=4")
        self.assertEqual(rc, 0, log)
        self.assertIn("fanout_trees_shared 0", log)


class Thresholds(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.yosys, cls.slang = map(os.path.abspath, sys.argv[1:3])
        cls.work = os.environ.get("TEST_TMPDIR", HERE)
        src = os.path.join(cls.work, "queue.sv")
        with open(src, "w") as f:
            f.write(threshold_queue())
        cls.proc = os.path.join(cls.work, "queue.json")
        rc, log = run("proc_json.py", cls.yosys, cls.slang, src, "Queue", cls.proc)
        assert rc == 0, log
        cls.found = run("entries.py", cls.proc, "Queue")[1]
        cls.kept = os.path.join(cls.work, "queue_kept.json")
        rc, log = run("extract.py", cls.proc, "Queue", cls.kept, "--keep-idx")
        assert rc == 0, log
        cls.extracted = os.path.join(cls.work, "queue_extracted.json")
        rc, log = run("extract.py", cls.proc, "Queue", cls.extracted)
        assert rc == 0, log
        cls.stats = dict(re.findall(r"(\w+) (\d+)", log))

    def test_entries_follow_one_rule(self):
        self.assertIn("uniform: 8 of 8 entries follow one rule", self.found)
        # live_<i>, freed below the bound, in the cell with data_<i>
        self.assertNotIn("left inline", self.found)

    def test_thresholds_decoded_once(self):
        # widx == i, lo < i + 1 and bound >= i + 1: one decoder each
        self.assertEqual(self.stats["decoded_once"], "3")
        self.assertEqual(self.stats["index_sites_left"], "0")

    def test_index_kept_equal(self):
        rc, log = run("check.py", self.yosys, self.proc, self.kept, "Queue")
        self.assertEqual(rc, 0, log)
        self.assertIn("check: Queue: equivalent", log)

    def test_decoded_equal(self):
        rc, log = run("check.py", self.yosys, self.proc, self.extracted, "Queue")
        self.assertEqual(rc, 0, log)
        self.assertIn("check: Queue: equivalent", log)

    def test_control_differs(self):
        rc, log = run(
            "check.py", self.yosys, self.proc, self.extracted, "Queue", "mutate"
        )
        self.assertEqual(rc, 0, log)
        self.assertIn("check: Queue: NOT equivalent", log)


class Ordered(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.yosys, cls.slang = map(os.path.abspath, sys.argv[1:3])
        cls.work = os.environ.get("TEST_TMPDIR", HERE)
        src = os.path.join(cls.work, "ordered.sv")
        with open(src, "w") as f:
            f.write(ordered_queue())
        cls.proc = os.path.join(cls.work, "ordered.json")
        rc, log = run("proc_json.py", cls.yosys, cls.slang, src, "Ordered", cls.proc)
        assert rc == 0, log
        cls.extracted = os.path.join(cls.work, "ordered_extracted.json")
        rc, log = run("extract.py", cls.proc, "Ordered", cls.extracted)
        assert rc == 0, log
        cls.stats = dict(re.findall(r"(\w+) (\d+)", log))

    def test_ordered_decoded_once(self):
        # widx == i, and i < a, i > a, i >= b, i <= b: no index left in the
        # cell, every comparison a decoder in the module
        self.assertEqual(self.stats["index_sites_left"], "0")
        self.assertEqual(self.stats["decoded_once"], "5")

    def test_decoded_equal(self):
        rc, log = run("check.py", self.yosys, self.proc, self.extracted, "Ordered")
        self.assertEqual(rc, 0, log)
        self.assertIn("check: Ordered: equivalent", log)

    def test_control_differs(self):
        rc, log = run(
            "check.py", self.yosys, self.proc, self.extracted, "Ordered", "mutate"
        )
        self.assertEqual(rc, 0, log)
        self.assertIn("check: Ordered: NOT equivalent", log)


class Enables(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.yosys, cls.slang = map(os.path.abspath, sys.argv[1:3])
        cls.work = os.environ.get("TEST_TMPDIR", HERE)
        src = os.path.join(cls.work, "retire.sv")
        with open(src, "w") as f:
            f.write(retire_queue())
        cls.proc = os.path.join(cls.work, "retire.json")
        rc, log = run("proc_json.py", cls.yosys, cls.slang, src, "Retire", cls.proc)
        assert rc == 0, log
        cls.found = run("entries.py", cls.proc, "Retire")[1]
        cls.extracted = os.path.join(cls.work, "retire_extracted.json")
        rc, log = run("extract.py", cls.proc, "Retire", cls.extracted)
        assert rc == 0, log
        cls.stats = dict(re.findall(r"(\w+) (\d+)", log))

    def test_entries_follow_one_rule(self):
        self.assertIn("uniform: 8 of 8 entries follow one rule", self.found)

    def test_enables_decoded_once(self):
        # widx == i, deq0 == i and deq1 == i, each with its enable
        self.assertEqual(self.stats["decoded_once"], "3")
        self.assertEqual(self.stats["enables_decoded"], "3")

    def test_carry_reaches_no_cell(self):
        # c0 and c1 drive the decoders; no entry cell reads them
        top = json.load(open(self.extracted))["modules"]["Retire"]
        bits = {
            b
            for name, v in top["netnames"].items()
            if name in ("c0", "c1")
            for b in v["bits"]
        }
        self.assertEqual(len(bits), 2)
        for c in top["cells"].values():
            if c["type"] == "RetireEntry":
                read = {b for bs in c["connections"].values() for b in bs}
                self.assertFalse(read & bits)

    def test_extraction_equal(self):
        rc, log = run("check.py", self.yosys, self.proc, self.extracted, "Retire")
        self.assertEqual(rc, 0, log)
        self.assertIn("check: Retire: equivalent", log)

    def test_control_differs(self):
        rc, log = run(
            "check.py", self.yosys, self.proc, self.extracted, "Retire", "mutate"
        )
        self.assertEqual(rc, 0, log)
        self.assertIn("check: Retire: NOT equivalent", log)


class ReadSides(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.yosys, cls.slang = map(os.path.abspath, sys.argv[1:3])
        cls.work = os.environ.get("TEST_TMPDIR", HERE)
        src = os.path.join(cls.work, "pricam.sv")
        with open(src, "w") as f:
            f.write(priority_cam())
        cls.proc = os.path.join(cls.work, "pricam.json")
        rc, log = run("proc_json.py", cls.yosys, cls.slang, src, "PriCam", cls.proc)
        assert rc == 0, log
        cls.found = run("entries.py", cls.proc, "PriCam")[1]
        cls.extracted = os.path.join(cls.work, "pricam_extracted.json")
        rc, log = run("extract.py", cls.proc, "PriCam", cls.extracted)
        assert rc == 0, log

    def test_entries_follow_one_rule(self):
        self.assertIn("uniform: 8 of 8 entries follow one rule", self.found)
        self.assertNotIn("left inline", self.found)

    def test_common_read_side_in_the_cell(self):
        # the match leaves each entry once; the inverters stay outside
        self.assertIn("read side: 1 bits leave each entry's read cone", self.found)

    def test_port_registers_are_entries(self):
        # data, valid and both ports' m_r: every register in the entries
        self.assertIn("(100%)", self.found)

    def test_extraction_equal(self):
        rc, log = run("check.py", self.yosys, self.proc, self.extracted, "PriCam")
        self.assertEqual(rc, 0, log)
        self.assertIn("check: PriCam: equivalent", log)

    def test_control_differs(self):
        rc, log = run(
            "check.py", self.yosys, self.proc, self.extracted, "PriCam", "mutate"
        )
        self.assertEqual(rc, 0, log)
        self.assertIn("check: PriCam: NOT equivalent", log)


if __name__ == "__main__":
    unittest.main(argv=sys.argv[:1])
