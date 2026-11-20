"""Do a module's per-entry arrays follow one rule per entry? The core of
generate_array's front end, on yosys JSON after proc (proc_json.py).

An array is a register vector whose words yosys keeps as one flop each,
$driver$<array>[<i>], or firtool's scalarised vector of bundles,
<array>_<i>_<field>. Arrays of the same length make up entries: entry i
is word i of every one of them. Entry i's private cone is the logic that
reaches its flops' inputs and no other entry's; logic that reaches
several entries is shared and stays outside.

Each private cone is walked into a structural signature: the entry's own
flops named by array and bit, another entry's by its offset from this
one, a shared or outside bit by its net, relative to the entry where the
net is per entry (bit i, or bit i*w+j of a w-bit field i), and every
constant as K. Entries with one signature have the same structure; their
constants, met in the same order, must then each be the same in every
entry or the entry's index (s*i+o). Such entries follow one rule: one
cell, instanced per entry, its index an input (extract.py).

yosys writes the comparison with index 0 differently (x == 0 as !x or
~|x); those are read as the comparison they are. So are thresholds,
x >= K and x < K, which firtool folds differently for each K (x >= 4 as
|x[6:2], x >= 64 as x[6] alone, x >= 3 as x > 2): on a net where some
entry compares against a threshold that is no x == K, every comparison
of the net reads as one (x != 0 as x >= 1).

    entries.py <design.json> <module> [min entries]
"""

import collections
import hashlib
import json
import os
import re
import sys

_WORD = re.compile(r"^\$driver\$(?P<arr>.+)\[(?P<i>\d+)\]$")
FLOPS = {
    "$dff",
    "$adff",
    "$sdff",
    "$dffe",
    "$adffe",
    "$sdffe",
    "$sdffce",
    "$aldff",
    "$dffsr",
}
OUT = {"Y", "Q"}
# the unsigned threshold comparisons, as a form evaluates them
_CMP = {
    "$lt": lambda x, y: x < y,
    "$le": lambda x, y: x <= y,
    "$gt": lambda x, y: x > y,
    "$ge": lambda x, y: x >= y,
}
# the buffers Entries puts in front of a threshold net's top bit, read alone
_TOP = "$threshold_top$"


def load(path, module):
    return json.load(open(path))["modules"][module]


def outputs(cell):
    return [p for p, d in cell["port_directions"].items() if d == "output"]


class Design:
    """A module's cells, who drives each bit, and each bit's net."""

    def __init__(self, mod):
        self.mod = mod
        self.cells = mod["cells"]
        self.driver = {}
        for name, c in self.cells.items():
            for p in outputs(c):
                for k, b in enumerate(c["connections"][p]):
                    self.driver[b] = (name, p, k)
        self.bitname = {}
        for n, v in mod["netnames"].items():
            for k, b in enumerate(v["bits"]):
                if not isinstance(b, str):
                    self.bitname.setdefault(b, (n, k, len(v["bits"])))
        self.inputs = set()
        for v in mod.get("ports", {}).values():
            if v["direction"] == "input":
                self.inputs.update(b for b in v["bits"] if not isinstance(b, str))
        # a bit a $buf chain ties to a constant is that constant
        self.const = {}
        for b in list(self.driver):
            self._const_of(b)
        self.flop_bits = {
            b
            for c in self.cells.values()
            if c["type"] in FLOPS
            for b in c["connections"]["Q"]
        }
        self.top_buffers = []  # (buffer, cell, port, bit, the bit it read)

    def top_buffer(self, c, p, k):
        """A $buf put between bit k of cell c's port p and the bit it reads:
        an entry's threshold form when it reads a threshold net's top bit
        alone (x >= 2^h is x[h]). Function kept; the cell is new."""
        if not hasattr(self, "_next_bit"):
            self._next_bit = 1 + max(
                [
                    b
                    for v in list(self.mod["netnames"].values())
                    + list(self.mod.get("ports", {}).values())
                    for b in v["bits"]
                    if not isinstance(b, str)
                ]
                + [
                    b
                    for cl in self.cells.values()
                    for bs in cl["connections"].values()
                    for b in bs
                    if not isinstance(b, str)
                ]
            )
        y = self._next_bit
        self._next_bit += 1
        name = f"{_TOP}{y}"
        self.cells[name] = {
            "hide_name": 1,
            "type": "$buf",
            "parameters": {"WIDTH": format(1, "032b")},
            "attributes": {},
            "port_directions": {"A": "input", "Y": "output"},
            "connections": {"A": [self.cells[c]["connections"][p][k]], "Y": [y]},
        }
        self.top_buffers.append((name, c, p, k, self.cells[c]["connections"][p][k]))
        self.cells[c]["connections"][p][k] = y
        self.driver[y] = (name, "Y", 0)
        self.mod["netnames"][name + "_y"] = {
            "hide_name": 1,
            "bits": [y],
            "attributes": {},
        }
        self.bitname[y] = (name + "_y", 0, 1)
        return name

    def drop_top_buffers(self, keep):
        """The design without the top buffers put in after the first keep."""
        while len(self.top_buffers) > keep:
            name, c, p, k, b = self.top_buffers.pop()
            y = self.cells[name]["connections"]["Y"][0]
            self.cells[c]["connections"][p][k] = b
            del self.cells[name], self.driver[y], self.bitname[y]
            del self.mod["netnames"][name + "_y"]
            self._next_bit = y

    def _const_of(self, b, depth=0):
        if b in self.const:
            return self.const[b]
        v = None
        if b in self.driver and depth < 64:
            c, p, k = self.driver[b]
            cl = self.cells[c]
            if cl["type"] in ("$buf", "$pos") and len(cl["connections"]["A"]) > k:
                a = cl["connections"]["A"][k]
                v = (
                    a
                    if isinstance(a, str) and a in "01"
                    else (None if isinstance(a, str) else self._const_of(a, depth + 1))
                )
        self.const[b] = v
        return v

    def groups(self, min_n=8):
        """Arrays by length: {n: {array: {i: flop}}}."""
        arrays = collections.defaultdict(dict)
        loose = []
        for name, c in self.cells.items():
            if c["type"] not in FLOPS:
                continue
            m = _WORD.match(name)
            if m:
                arrays[m["arr"]][int(m["i"])] = name
            else:
                loose.append(name)
        # firtool's scalarised vectors of bundles: uop_<i>_<field>; any
        # number in a flop's name may be the entry, and the reading that
        # makes a whole group 0..N-1 is the one taken
        candidates = collections.defaultdict(dict)
        for name in loose:
            for m in re.finditer(r"\d+", name):
                key = name[: m.start()] + "#" + name[m.end() :]
                candidates[key][int(m.group())] = name
            # firtool names a vector of registers x, x_1, x_2, ...: word 0
            # without a number (Uncache's f1_fwdDataCandidates_r)
            candidates[name + "_#"][0] = name
        taken = set()
        for key, words in sorted(candidates.items(), key=lambda kv: -len(kv[1])):
            if len(words) >= min_n and sorted(words) == list(range(len(words))):
                if not taken & set(words.values()):
                    arrays[key.replace("$driver$", "")] = words
                    taken |= set(words.values())
        by_len = collections.defaultdict(dict)
        for arr, words in arrays.items():
            if len(words) >= min_n and sorted(words) == list(range(len(words))):
                by_len[len(words)][arr] = words
        return by_len


class Entries:
    """One group of n entries: cones, signatures and the common rule."""

    def __init__(
        self, design, n, arrays, read=False, thresholds=False, common_read=False
    ):
        self.d = design
        self.n = n
        self.read = read
        self.common_read = common_read
        # thresholds read as forms (x >= K, x < K), else as the cells they are
        self.thresholds = thresholds
        self._evaluable = self._EVAL | (set(_CMP) if thresholds else set())
        self.arrays = arrays
        self.inline = []  # arrays left out by narrowed()
        self.arrs = sorted(arrays)
        self.flops = {i: [arrays[a][i] for a in self.arrs] for i in range(n)}
        self.array_of = {arrays[a][i]: a for a in self.arrs for i in range(n)}
        self.owner = {}
        for a in self.arrs:
            for i, f in arrays[a].items():
                for k, b in enumerate(design.cells[f]["connections"]["Q"]):
                    self.owner[b] = (a, i, k)
        self.owner_cells = set(self.array_of)
        self._forms = {}
        self.cones = [self._cone(i) for i in range(n)]
        self.read_roots = [[] for _ in range(n)]
        if read:
            self._read_side()
        self.reach = collections.Counter(c for cs in self.cones for c in cs)
        self._thresholds()
        self.walks = [self._walk(i) for i in range(n)]
        if read:
            # the read roots by signature: their readers are per-entry nodes
            # of the reductions, so no name orders them alike in every entry
            for i in range(n):
                sigs = self.walks[i]["read_sigs"]
                self.read_roots[i] = [
                    r for _, r in sorted(zip(sigs, self.read_roots[i]))
                ]
            self.walks = [self._walk(i) for i in range(n)]
            if common_read:
                self._common_read()
        self._rule()

    def _read_side(self):
        """Each entry's read cone joins its cone: the cells its registers
        feed and no other entry's do, outside every entry's write cone
        (PhysicalStoreQueue's forwarding compares, 33 per entry). The bits
        that leave it, into the reductions across entries, are the walk's
        further roots, ordered alike in every entry: by their readers
        (cell, port, then bit position) and their net's name pattern."""
        d, n = self.d, self.n
        written = set().union(*self.cones)
        readers = collections.defaultdict(list)
        for c, cl in d.cells.items():
            if cl["type"] in FLOPS:
                continue
            for p, bs in cl["connections"].items():
                if p not in outputs(cl):
                    for k, b in enumerate(bs):
                        if not isinstance(b, str):
                            readers[b].append((c, p, k))
        fed = collections.defaultdict(set)
        for b, (_, i, _) in self.owner.items():
            todo, seen = [b], set()
            while todo:
                x = todo.pop()
                for c, _, _ in readers.get(x, ()):
                    if c in seen or c in written:
                        continue
                    seen.add(c)
                    fed[c].add(i)
                    cl = d.cells[c]
                    for p in outputs(cl):
                        todo += cl["connections"][p]
        cones = [set() for _ in range(n)]
        for c, es in fed.items():
            if len(es) == 1:
                cones[next(iter(es))].add(c)
        out_bits = {
            b
            for v in d.mod["ports"].values()
            if v["direction"] == "output"
            for b in v["bits"]
        }
        for i in range(n):
            roots = []
            for c in cones[i]:
                cl = d.cells[c]
                for p in outputs(cl):
                    for b in cl["connections"][p]:
                        outside = [
                            r for r in readers.get(b, ()) if r[0] not in cones[i]
                        ]
                        if outside or b in out_bits:
                            net = d.bitname.get(b, ("", 0, 1))[0]
                            pattern = (
                                _net_pattern(net)[0] if not net.startswith("$") else ""
                            )
                            first = min((r[2] for r in outside), default=0)
                            roots.append((pattern, first, b))
            # ordered for now by name; __init__ orders them again by their
            # signatures once a first walk has computed them
            roots.sort()
            self.read_roots[i] = roots
            # only what the roots read: a cell whose output nothing outside
            # reads is not the entry's to carry
            keep, todo = set(), [b for _, _, b in roots]
            while todo:
                b = todo.pop()
                drv = d.driver.get(b)
                if not drv or drv[0] not in cones[i] or drv[0] in keep:
                    continue
                keep.add(drv[0])
                cl = d.cells[drv[0]]
                for p, bs in cl["connections"].items():
                    if p not in outputs(cl):
                        todo += [x for x in bs if not isinstance(x, str)]
            self.cones[i] |= keep

    def _common_read(self):
        """The read side narrowed to the roots every entry has, by their
        signatures: LoadQueueRAW's priority select within each bank of
        eight reads entry j's select bit through j inverters of its own, a
        different number in every entry; the select bits themselves are
        common. What only the dropped roots read stays in the module."""
        d, n = self.d, self.n
        common = collections.Counter(self.walks[0]["read_sigs"])
        for w in self.walks[1:]:
            common &= collections.Counter(w["read_sigs"])
        for i in range(n):
            left, roots = collections.Counter(common), []
            for r, s in zip(self.read_roots[i], self.walks[i]["read_sigs"]):
                if left[s] > 0:
                    left[s] -= 1
                    roots.append(r)
            self.read_roots[i] = roots
            # the cone again, from the registers' inputs and the roots kept,
            # through the cells it had
            full, cone = self.cones[i], set()
            todo = [b for _, _, b in roots] + [
                b
                for f in self.flops[i]
                for p, bs in d.cells[f]["connections"].items()
                if p not in OUT
                for b in bs
                if not isinstance(b, str)
            ]
            while todo:
                b = todo.pop()
                drv = d.driver.get(b)
                if (
                    b in d.inputs
                    or b in d.flop_bits
                    or not drv
                    or drv[0] not in full
                    or drv[0] in cone
                ):
                    continue
                cone.add(drv[0])
                cl = d.cells[drv[0]]
                for p, bs in cl["connections"].items():
                    if p not in outputs(cl):
                        todo += [x for x in bs if not isinstance(x, str)]
            self.cones[i] = cone
        self.reach = collections.Counter(c for cs in self.cones for c in cs)
        self._forms = {}
        self._thresholds()
        self.walks = [self._walk(i) for i in range(n)]

    def root(self, b):
        """The bit b buffers through private $buf/$pos cells, and those
        cells: a flattened cell copies its inputs through its own port
        buffers, the Rob's 352 RobEntryCells their shared bus. Only to a bit
        from outside the entry: a buffer of the entry's own logic or
        register is that logic's, and moves with it into the cell."""
        d, bufs, r = self.d, [], b
        while not isinstance(r, str) and r in d.driver and r not in self.owner:
            c, _, k = d.driver[r]
            cl = d.cells[c]
            if (
                cl["type"] not in ("$buf", "$pos")
                or (c.startswith(_TOP) and self.thresholds)
                or self.reach.get(c) != 1
                or len(cl["connections"]["A"]) != len(cl["connections"]["Y"])
            ):
                break
            bufs.append(c)
            r = cl["connections"]["A"][k]
        shared = (
            isinstance(r, str)
            or r not in d.driver
            or r in d.inputs
            or (r in d.flop_bits and r not in self.owner)
            or self.reach.get(d.driver[r][0], 0) > 1
        )
        return (r, bufs) if bufs and shared else (b, [])

    def _thresholds(self):
        """The nets read as thresholds (thr_nets: net -> the compared width):
        those where some entry's private cell is x >= K or x < K and no
        x == K or x != K of the bits it reads (x > 2). A private cell that
        reads such a net's top bit alone, and is no form of its own, reads
        it through a buffer put in front of it (Design.top_buffer): that
        entry's x >= 2^h, as the other entries' forms are."""
        d = self.d
        self.thr_nets = {}
        if not self.thresholds:
            return
        widths = collections.defaultdict(set)
        for cone in self.cones:
            for c in cone:
                if self.reach[c] != 1 or c in self.owner_cells:
                    continue
                eq, thr, _ = self._raw(c)
                if thr and not eq:
                    widths[d.bitname[thr[1][0]][0]].add(len(thr[1]))
        self.thr_nets = {net: w.pop() for net, w in widths.items() if len(w) == 1}
        tops = {
            d.mod["netnames"][net]["bits"][w - 1] for net, w in self.thr_nets.items()
        }
        for cone in self.cones:
            for c in sorted(cone):
                if (
                    self.reach[c] != 1
                    or c in self.owner_cells
                    or c.startswith(_TOP)
                    or any(self._raw(c)[:2])
                ):
                    continue
                cl = d.cells[c]
                for p, bs in list(cl["connections"].items()):
                    if p in outputs(cl):
                        continue
                    for k, b in enumerate(bs):
                        if b in tops:
                            buf = d.top_buffer(c, p, k)
                            cone.add(buf)
                            self.reach[buf] = 1

    def narrowed(self):
        """The group without the arrays whose structure differs between
        entries, when the rest follows one rule (IBuffer: the last entry of
        each bank of four reads predTaken differently, one bit of 61);
        those arrays stay inline in the module. Else the group as it is."""
        if self.uniform:
            return self
        varying = [a for a in self.arrs if len({w["parts"][a] for w in self.walks}) > 1]
        rest = {a: v for a, v in self.arrays.items() if a not in varying}
        if not varying or not rest:
            return self
        e = Entries(self.d, self.n, rest, self.read, self.thresholds, self.common_read)
        if not e.uniform:
            return self
        e.inline = varying
        return e

    def _cone(self, i):
        d = self.d
        seen, todo = set(), []
        for f in self.flops[i]:
            for p, bs in d.cells[f]["connections"].items():
                if p not in OUT:
                    todo += [b for b in bs if not isinstance(b, str)]
        while todo:
            b = todo.pop()
            if b in d.inputs or b in d.flop_bits or b not in d.driver:
                continue
            c = d.driver[b][0]
            if c in seen:
                continue
            seen.add(c)
            for p, bs in d.cells[c]["connections"].items():
                if p not in outputs(d.cells[c]):
                    todo += [x for x in bs if not isinstance(x, str)]
        return seen

    def _walk(self, i):
        """Entry i's signature per array, its private cells in walk order,
        its constant sites and its references to shared bits, both in the
        order the walk met them (a reference's bit index, like a constant,
        is decided across entries: _rule)."""
        d, n = self.d, self.n
        memo, order, consts, refs = {}, [], [], []
        tokens, bodies = {}, {}
        # a cell the same as one already met (its type, parameters and input
        # bits; a comparison by its form) is that cell, as opt_merge would
        # have it: yosys leaves one entry's x == 0 in two cells and another
        # entry's x == 1 in one, and the walks would no longer line up
        seen, merged, folded = {}, {}, set()

        def bit(b):
            if isinstance(b, str):
                return b
            if d.const.get(b) is not None:
                return d.const[b]
            if b in self.owner:
                arr, j, k = self.owner[b]
                if j == i:
                    return f"{arr}[{k}]"
                t = f"{arr}@{(j - i) % n}[{k}]"
                tokens[t] = b
                return t
            r, bufs = self.root(b)
            if r != b:
                # a private buffer (a flattened cell's port) is what it buffers
                folded.update(bufs)
                return bit(r)
            if (
                b in d.inputs
                or b in d.flop_bits
                or b not in d.driver
                or self.reach[d.driver[b][0]] != 1
            ):
                net, k, width = d.bitname.get(b, (str(b), 0, 1))
                pattern, numbers = _net_pattern(net)
                refs.append((b, net, k, width, numbers))
                return f"R:{pattern}"
            c, p, k = d.driver[b]
            return f"{cell(c)}.{p}{k}"

        def port(bs, where):
            bs = [d.const.get(x) or x if not isinstance(x, str) else x for x in bs]
            if all(isinstance(x, str) and x in "01" for x in bs):
                consts.append((where, int("".join(reversed(bs)), 2), len(bs)))
                return f"K{len(bs)}"
            return ",".join(bit(x) for x in bs)

        def cell(c):
            if c in memo:
                return memo[c]
            memo[c] = "?"  # a loop through private logic
            eq = self._eq_form(c)
            if eq:
                key = ("EQ", eq[0], tuple(eq[1]), eq[2])
            else:
                typ, params, ins = self.normal(c)
                key = (
                    typ,
                    str(sorted(params.items())),
                    tuple((p, tuple(bs)) for p, bs in sorted(ins.items())),
                )
                folded.update(self.folded)
            if key in seen and memo.get(seen[key], "?") != "?":
                memo[c] = memo[seen[key]]
                merged[c] = seen[key]
                if eq:
                    folded.update(self.folded)
                return memo[c]
            seen.setdefault(key, c)
            if eq:
                folded.update(self.folded)
                # x == K (or x >= K) however yosys wrote it, x up to 12 bits
                # of one net
                sense, leaves, value = eq
                tag = "EQ" if sense in ("EQ", "NE") else "GE"
                consts.append(((c, tag, tuple(leaves)), value, len(leaves)))
                body = f"{sense}{len(leaves)}|" + ",".join(bit(x) for x in leaves)
            else:
                body = (
                    typ
                    + str(sorted(params.items()))
                    + "".join(
                        f"|{p}:{port(bs, (c, p))}" for p, bs in sorted(ins.items())
                    )
                )
            memo[c] = hashlib.sha1(body.encode()).hexdigest()[:16]
            bodies[memo[c]] = body
            order.append(c)
            return memo[c]

        parts = {}
        for f in self.flops[i]:
            cl = d.cells[f]
            body = "".join(
                f"|{p}:{port(bs, (f, p))}"
                for p, bs in sorted(cl["connections"].items())
                if p not in OUT
            )
            parts[self.array_of[f]] = hashlib.sha1(body.encode()).hexdigest()[:16]
            bodies[self.array_of[f]] = body
        # the read side: each bit that leaves the entry's read cone
        reads = [bit(b) for _, _, b in self.read_roots[i]]
        if reads:
            body = "|".join(reads)
            parts["read"] = hashlib.sha1(body.encode()).hexdigest()[:16]
            bodies["read"] = body
        return {
            "read_sigs": reads,
            "parts": parts,
            "order": order,
            "merged": merged,
            # the cells inside a comparison form, read through it
            "folded": folded,
            "tokens": tokens,
            "consts": consts,
            "refs": refs,
            "bodies": bodies,
        }

    _EVAL = {
        "$and",
        "$or",
        "$xor",
        "$xnor",
        "$not",
        "$buf",
        "$pos",
        "$eq",
        "$ne",
        "$logic_not",
        "$logic_and",
        "$logic_or",
        "$reduce_and",
        "$reduce_or",
        "$reduce_xor",
        "$reduce_bool",
        "$mux",
    }

    def _eq_form(self, c):
        """(sense, x's bits, K) when the one-bit cell c and the private
        cells behind it compute x == K or x != K (EQ, NE) of up to 12 bits
        of one shared net, or x >= K or x < K (GE, LT) of the net's bits up
        to the highest one read, else None. A threshold net's comparisons
        read as thresholds (x != 0 as x >= 1, x == 0 as x < 1), every other
        net's as x == K where they are one."""
        eq, thr, self.folded = self._raw(c)
        if thr and (not eq or self.d.bitname[thr[1][0]][0] in self.thr_nets):
            return thr
        return eq

    def _raw(self, c, limit=12):
        """Both readings of cell c, (x == K or None, x >= K or None, the
        private cells inside the form), once per cell."""
        if c not in self._forms:
            self._forms[c] = self._read_form(c, limit)
        return self._forms[c]

    def _read_form(self, c, limit):
        none = (None, None, set())
        d = self.d
        cl = d.cells[c]
        if cl["type"] not in self._evaluable:
            return none
        outs = outputs(cl)
        if len(outs) != 1 or len(cl["connections"][outs[0]]) != 1:
            return none
        cone, leaves, todo = set(), set(), [c]
        while todo:
            x = todo.pop()
            if x in cone:
                continue
            cx = d.cells[x]
            if cx["type"] not in self._evaluable or any(
                v not in ("0", "1", 0, 1) and str(v).strip("0") not in ("", "1")
                for k, v in cx.get("parameters", {}).items()
                if k.endswith("_SIGNED")
            ):
                return none
            if any(
                int(v, 2) if isinstance(v, str) else v
                for k, v in cx.get("parameters", {}).items()
                if k.endswith("_SIGNED")
            ):
                return none
            cone.add(x)
            for p, bs in cx["connections"].items():
                if p in outputs(cx):
                    continue
                for b in bs:
                    if isinstance(b, str) or d.const.get(b) is not None:
                        continue
                    drv = d.driver.get(b)
                    if (
                        drv
                        and drv[0] not in self.owner_cells
                        and self.reach[drv[0]] == 1
                        and b not in d.flop_bits
                    ):
                        todo.append(drv[0])
                    else:
                        leaves.add(b)
            if len(cone) > 64 or len(leaves) > limit:
                return none
        nets = {d.bitname.get(b, (None,))[0] for b in leaves}
        if (
            len(nets) != 1
            or None in nets
            or not leaves
            or any(b in self.owner for b in leaves)
        ):
            return none
        leaves = sorted(leaves, key=lambda b: d.bitname[b][1])
        y = cl["connections"][outs[0]][0]
        # a threshold reads the top bits x[h:j] and is a step in them; an
        # x == K or x != K has one 1 or one 0: neither, and the walk stops
        table, steps, ones = [], 0, 0
        for m in range(1 << len(leaves)):
            val = {b: (m >> k) & 1 for k, b in enumerate(leaves)}
            v = self._eval(y, val, {})
            steps += bool(table) and v != table[-1]
            ones += v
            table.append(v)
            if (steps > 1 or not self.thresholds) and 1 < ones < m:
                return none
        eq = None
        if ones == 1:
            eq = ("EQ", leaves, table.index(1))
        elif ones == len(table) - 1:
            eq = ("NE", leaves, table.index(0))
        thr = None
        if self.thresholds and steps == 1:
            thr = self._threshold(nets.pop(), leaves, table, limit)
        if not eq and not thr:
            return none
        return eq, thr, cone - {c}

    def _threshold(self, net, leaves, table, limit):
        """("GE" or "LT", x's bits, K) when the function table (over the
        leaves, one step) is x >= K or x < K, x the net's bits up to the
        highest leaf: the leaves its top bits x[h:j], K the step's place
        times 2^j."""
        bits = self.d.mod["netnames"][net]["bits"]
        pos = [self.d.bitname[b][1] for b in leaves]
        j, w = pos[0], pos[-1] + 1
        x_bits = bits[:w]
        if (
            pos != list(range(j, w))
            or w < 2
            or w > limit
            or any(isinstance(b, str) or b in self.owner for b in x_bits)
        ):
            return None
        step = next(m for m in range(1, len(table)) if table[m] != table[m - 1])
        return ("GE" if table[-1] else "LT", x_bits, step << j)

    def _eval(self, b, val, memo):
        """A bit's value given the leaves' values (val)."""
        if isinstance(b, str):
            return int(b == "1")
        if b in val:
            return val[b]
        if self.d.const.get(b) is not None:
            return int(self.d.const[b] == "1")
        if b in memo:
            return memo[b]
        c, p, k = self.d.driver[b]
        cl = self.d.cells[c]
        cn, t = cl["connections"], cl["type"]
        vec = lambda q: [self._eval(x, val, memo) for x in cn[q]]  # noqa: E731
        if t in ("$buf", "$pos"):
            r = vec("A")[k] if k < len(cn["A"]) else 0
        elif t == "$not":
            r = 1 - vec("A")[k]
        elif t in ("$and", "$or", "$xor", "$xnor"):
            a, bb = vec("A"), vec("B")
            x = a[k] if k < len(a) else 0
            y = bb[k] if k < len(bb) else 0
            r = {"$and": x & y, "$or": x | y, "$xor": x ^ y, "$xnor": 1 - (x ^ y)}[t]
        elif t == "$mux":
            r = vec("B")[k] if vec("S")[0] else vec("A")[k]
        elif t in ("$eq", "$ne"):
            a, bb = vec("A"), vec("B")
            w = max(len(a), len(bb))
            a += [0] * (w - len(a))
            bb += [0] * (w - len(bb))
            r = int((a == bb) == (t == "$eq")) if k == 0 else 0
        elif t in _CMP:
            a, bb = vec("A"), vec("B")
            x = sum(v << j for j, v in enumerate(a))
            y = sum(v << j for j, v in enumerate(bb))
            r = int(_CMP[t](x, y)) if k == 0 else 0
        elif t == "$logic_not":
            r = int(not any(vec("A"))) if k == 0 else 0
        elif t == "$logic_and":
            r = int(any(vec("A")) and any(vec("B"))) if k == 0 else 0
        elif t == "$logic_or":
            r = int(any(vec("A")) or any(vec("B"))) if k == 0 else 0
        elif t == "$reduce_and":
            r = int(all(vec("A"))) if k == 0 else 0
        elif t in ("$reduce_or", "$reduce_bool"):
            r = int(any(vec("A"))) if k == 0 else 0
        elif t == "$reduce_xor":
            r = sum(vec("A")) & 1 if k == 0 else 0
        else:
            raise ValueError(t)
        memo[b] = r
        return r

    def diff(self, a, b):
        """The first cells where entries a and b part, for each array that
        differs: what a new normal form has to cover."""
        out = []
        for arr in self.arrs:
            x, y = self.walks[a]["bodies"][arr], self.walks[b]["bodies"][arr]
            for _ in range(60):
                if x == y:
                    break
                tx = re.split(r"([0-9a-f]{16})", x)
                ty = re.split(r"([0-9a-f]{16})", y)
                if len(tx) != len(ty) or tx[0::2] != ty[0::2]:
                    ux, uy = re.split(r"[|,]", x), re.split(r"[|,]", y)
                    d = [(p, q) for p, q in zip(ux, uy) if p != q][:4]
                    out.append(
                        f"  {arr}: {x.split('|')[0][:60]} vs {y.split('|')[0][:60]}: {d}"
                    )
                    break
                k = next(k for k in range(1, len(tx), 2) if tx[k] != ty[k])
                x, y = self.walks[a]["bodies"][tx[k]], self.walks[b]["bodies"][ty[k]]
        return out

    def normal(self, c):
        """A cell as type, parameters and inputs, yosys's forms of x == 0
        (!x, ~|x) read as the comparison."""
        d = self.d
        cl = d.cells[c]
        typ, params, conns = cl["type"], cl.get("parameters", {}), cl["connections"]
        ins = {p: bs for p, bs in conns.items() if p not in outputs(cl)}
        self.folded = set()
        if typ == "$not" and len(ins["A"]) == 1 and ins["A"][0] in d.driver:
            c2 = d.driver[ins["A"][0]][0]
            if d.cells[c2]["type"] == "$reduce_or" and self.reach[c2] == 1:
                self.folded = {c2}
                p2 = d.cells[c2]["parameters"]
                typ, ins = "$logic_not", {"A": d.cells[c2]["connections"]["A"]}
                params = {
                    "A_SIGNED": p2.get("A_SIGNED"),
                    "A_WIDTH": p2.get("A_WIDTH"),
                    "Y_WIDTH": params.get("Y_WIDTH"),
                }
        if typ == "$logic_not":
            w = len(ins["A"])
            typ = "$eq"
            params = {
                "A_SIGNED": params.get("A_SIGNED"),
                "A_WIDTH": params.get("A_WIDTH"),
                "B_SIGNED": params.get("A_SIGNED"),
                "B_WIDTH": params.get("A_WIDTH"),
                "Y_WIDTH": params.get("Y_WIDTH"),
            }
            ins = {"A": ins["A"], "B": ["0"] * w}
        return typ, params, ins

    def _rule(self):
        n = self.n
        self.sigs = [
            "".join(w["parts"][a] for a in sorted(w["parts"])) for w in self.walks
        ]
        self.top = collections.Counter(self.sigs).most_common(1)[0][0]
        self.members = [i for i in range(n) if self.sigs[i] == self.top]
        m = self.members
        cols = list(zip(*([v for _, v, _ in self.walks[i]["consts"]] for i in m)))
        self.index_sites, self.bad_sites = {}, []
        for k, col in enumerate(cols):
            if len(set(col)) == 1:
                continue
            fit = _affine(col, m)
            if fit:
                self.index_sites[k] = fit
            else:
                self.bad_sites.append(k)
        # a shared bit's net and index: its net's numbers (by position in
        # its pattern, a missing one 0) and its bit index each the same in
        # every entry, s*i+o, i // B or i % B (a bank's entries)
        rcols = list(zip(*([r[1:] for r in self.walks[i]["refs"]] for i in m)))
        self.ref_rule = []
        for col in rcols:
            ks = [k for _, k, _, _ in col]
            positions = sorted({p for _, _, _, nums in col for p in nums})
            rules = [
                _index_rule([nums.get(p, 0) for _, _, _, nums in col], m, None)
                for p in positions
            ]
            rules.append(_index_rule(ks, m, col[0][2]))
            self.ref_rule.append(None if None in rules else tuple(rules))
        self.bad_refs = [k for k, r in enumerate(self.ref_rule) if r is None]
        self.uniform = len(m) == n and not self.bad_sites and not self.bad_refs
        # each entry's boundary bits by a token that names them alike in
        # every entry, the reference's column in it: a net's pattern and
        # rule alone are not unique (LoadQueueRAR's matchMaskReg_1,
        # matchMaskReg_1_1 and matchMaskReg_2_1 shared one, and its
        # extraction wired one bit to all three)
        for i in m:
            w = self.walks[i]
            for k, ((b, net, _, _, _), r) in enumerate(zip(w["refs"], self.ref_rule)):
                w["tokens"][f"{k}:{_net_pattern(net)[0]}{r}"] = b

    def report(self):
        d, n = self.d, self.n
        bits = sum(len(d.cells[f]["connections"]["Q"]) for f in self.flops[0])
        private = [
            sum(1 for c in self.cones[i] if self.reach[c] == 1) for i in range(n)
        ]
        shared = sum(1 for k in self.reach.values() if k > 1)
        lines = [f"# {n} entries x {len(self.arrs)} arrays, {bits} bits per entry"]
        for a in self.arrs:
            k = len(set(w["parts"][a] for w in self.walks))
            if k > 1:
                lines.append(f"    {a}: {k} structures over {n} entries")
        lines.append(
            f"  common structure: {len(self.members)} of {n} entries"
            f" ({len(set(self.sigs))} structures); constants:"
            f" {len(self.walks[self.members[0]]['consts'])} sites,"
            f" {len(self.index_sites)} the index, {len(self.bad_sites)} neither;"
            f" shared bits: {len(self.ref_rule)} sites, {len(self.bad_refs)} undecided"
        )
        lines.append(
            f"  uniform: {n if self.uniform else 0} of {n} entries follow one rule"
        )
        lines.append(
            f"  private cells per entry: {min(private)}..{max(private)}, shared cells: {shared}"
        )
        odd = [i for i in range(n) if i not in self.members]
        if odd:
            lines.append(
                f"  entries off the common rule: {odd[:20]}{' ...' if len(odd) > 20 else ''}"
            )
        lines.append(
            "  arrays: "
            + ", ".join(self.arrs[:12])
            + (" ..." if len(self.arrs) > 12 else "")
        )
        return "\n".join(lines)


def _net_pattern(net):
    """A net's name without its numbered segments (_<n>), and those numbers
    by where they stood: firtool names one per bank or per entry
    (pipMod, pipMod_3; _T_1, _T_2)."""
    numbers, out, last = {}, [], 0
    # an unnamed net ($23708y) is numbered by yosys, one per entry's cell:
    # every number in it is decided across entries like a name's
    rx = r"(\d+)" if net.startswith("$") else r"_(\d+)(?=_|\.|$)"
    for m in re.finditer(rx, net):
        out.append(net[last : m.start()])
        numbers[len("".join(out))] = int(m.group(1))
        last = m.end()
    out.append(net[last:])
    return "".join(out), numbers


def _index_rule(vals, idx, width):
    """How values (one per entry) follow the entry: the same in every one,
    s*i+o, i // B or i % B (a bank's entries), else None."""
    if len(set(vals)) == 1:
        return ("abs", vals[0])
    fit = _affine(vals, idx)
    if fit:
        return ("rel",) + fit
    # affine in a bank's number (i // B) or in the entry's place in its
    # bank (i % B): firtool numbers a bank's modules in steps (pipMod_3, _6)
    for b in range(2, max(idx) + 2):
        fit = _affine(vals, [i % b for i in idx])
        if fit and all(v == fit[0] * (i % b) + fit[1] for v, i in zip(vals, idx)):
            return ("mod", b) + fit
        fit = _affine(vals, [i // b for i in idx])
        if fit and all(v == fit[0] * (i // b) + fit[1] for v, i in zip(vals, idx)):
            return ("div", b) + fit
    return None


def _affine(vals, idx):
    """(s, o) with vals[k] == s * idx[k] + o for every k, s != 0, else None."""
    if len(vals) < 2:
        return None
    # two entries whose idx differ fix s and o
    k = next((k for k in range(1, len(idx)) if idx[k] != idx[0]), None)
    if k is None:
        return None
    s = (vals[k] - vals[0]) // (idx[k] - idx[0])
    o = vals[0] - s * idx[0]
    if s and all(v == s * i + o for v, i in zip(vals, idx)):
        return (s, o)
    return None


def covered_bits(e):
    """The register bits of a group that follows one rule, else 0."""
    if not e.uniform:
        return 0
    return sum(
        len(e.d.cells[f]["connections"]["Q"]) for fs in e.flops.values() for f in fs
    )


# a study (extract.py --no-read): every read side left in the block
NO_READ = False


def best(d, n, arrays):
    """The group with its read side when that follows one rule, else with
    the part of it every entry has (Entries._common_read), else without
    it; arrays that differ between entries left inline. Each first with
    the comparisons read as x == K alone, then with thresholds too
    (x >= K, VirtualLoadQueue), taken where they cover more: a block that
    follows one rule without them is read as it always was."""
    # a threshold try not taken leaves no buffer behind
    keep = len(d.top_buffers)
    for read, common in (
        ((False, False),) if NO_READ else ((True, False), (True, True), (False, False))
    ):
        plain = Entries(d, n, arrays, read, common_read=common).narrowed()
        if plain.uniform and not plain.inline:
            return plain
        e = Entries(d, n, arrays, read, True, common).narrowed()
        if covered_bits(e) > covered_bits(plain):
            return e
        d.drop_top_buffers(keep)
        if plain.uniform:
            return plain
    return plain


def plan(d, min_n=8):
    """Each group of entries as best() takes it, longest first. An array of
    k * N words (k >= 2) whose own group does not follow one rule may be k
    arrays of the group of N, word p * N + i (or i * k + p) entry i's:
    Uncache's f1_ptagMatches_r, 48 words, is its 16 entries' match per
    load port. Taken where the group of N then covers more."""
    groups = d.groups(min_n)
    found = {n: best(d, n, arrays) for n, arrays in groups.items()}
    for big in sorted(groups, reverse=True):
        if found[big] is None or found[big].uniform:
            continue
        for n in sorted(groups):
            if n >= big or big % n or found.get(n) is None:
                continue
            k = big // n
            for word in (lambda p, i: p * n + i, lambda p, i: i * k + p):
                split = {
                    f"{a}/{p}": {i: words[word(p, i)] for i in range(n)}
                    for a, words in groups[big].items()
                    for p in range(k)
                }
                e = best(d, n, {**groups[n], **split})
                if covered_bits(e) > covered_bits(found[n]):
                    found[n], found[big] = e, None
                    groups[n] = {**groups[n], **split}
                    break
            if found[big] is None:
                break
    return [found[n] for n in sorted(found, reverse=True) if found[n] is not None]


def main(argv):
    d = Design(load(argv[1], argv[2]))
    min_n = int(argv[3]) if len(argv) > 3 else 8
    total = len(d.flop_bits)
    covered = 0
    for e in plan(d, min_n):
        n = e.n
        print(e.report())
        if e.inline:
            print(f"  without {e.inline} (left inline): uniform")
        if e.read and any(e.read_roots):
            print(
                f"  read side: {len(e.read_roots[0])} bits leave each entry's read cone"
            )
        if os.environ.get("ENTRIES_DIFF") and not e.uniform:
            a, b = (int(x) for x in os.environ["ENTRIES_DIFF"].split(","))
            if max(a, b) < n:
                print("\n".join(e.diff(a, b)[:4]))
        covered += covered_bits(e)
    print(
        f"# coverage: {covered} of {total} register bits in entries that follow one rule"
        f" ({100 * covered // max(1, total)}%)"
    )


if __name__ == "__main__":
    main(sys.argv)
