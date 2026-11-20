#!/usr/bin/env python3
"""Fold the constants XiangShan ties into the modules this flow keeps.

firtool propagates constants across modules, but leaves some on instance
connections; yosys folds those wherever it flattens and cannot across a
boundary the flow keeps (a hardened block, SYNTH_KEEP_MODULES). The
manifest names each one: a module, an input port, the literal every
instance of the module ties it to. For each entry this script

  a) checks the precondition, exactly: the module is defined once and
     instanced exactly `instances` times in the whole file, every instance
     connects the port to `value` (a literal, or a wire its parent declares
     equal to one); anything else stops with the entry named;
  b) does the surgery: the port leaves the module's header and every
     instance's connection list, and the module declares
     `wire <range> <port> = <value>;` in its place.

Entries are applied in round order, each against the text the previous
ones left. A new XiangShan that moves anything makes this fail, never skip.

  xs_surgery.py apply --manifest manifest.json [--protect XSTile] <in.sv> <out.sv>
  xs_surgery.py propose --boundary XSTile_boundary_constants.json
      --verilog <in.sv> --out manifest.json

`propose` is how the manifest was written: every port boundary_constants.py
found tied to one literal on every instance, with the instance count taken
from the whole file. Python 3.6.
"""

import argparse
import json
import os
import re
import sys

_MODULE = re.compile(r"^module\s+([A-Za-z_][A-Za-z_0-9$]*)\s*\(", re.M)
_LITERAL = re.compile(r"^\d+'[bhd][0-9a-fA-F_xz]+$")
_RANGE = re.compile(r"^\[[^\]]+\]$")
_DIRECTIONS = ("input", "output", "inout")


class SurgeryError(Exception):
    pass


_ANY_INSTANCE = re.compile(
    r"^\s+([A-Za-z_][A-Za-z_0-9$]*)\s+[A-Za-z_][A-Za-z_0-9$]*\s*\(", re.M
)


class Netlist(object):
    """The file as segments, one per module, indexed once.

    segments[i] is [name or None, text]; a module's text runs from `module`
    through `endmodule`. where[name] is the segment defining it, users[name]
    the segments that instance it.
    """

    def __init__(self, text):
        self.segments = []
        pos = 0
        for m in _MODULE.finditer(text):
            if m.start() < pos:
                raise SurgeryError("module %s inside another" % m.group(1))
            end = text.find("endmodule", m.end())
            if end < 0:
                raise SurgeryError("module %s is not closed" % m.group(1))
            end += len("endmodule")
            self.segments.append([None, text[pos : m.start()]])
            self.segments.append([m.group(1), text[m.start() : end]])
            pos = end
        self.segments.append([None, text[pos:]])
        self.where = {}
        self.users = {}
        for i, (name, chunk) in enumerate(self.segments):
            if name is None:
                continue
            self.where.setdefault(name, []).append(i)
            for used in set(_ANY_INSTANCE.findall(chunk)):
                self.users.setdefault(used, []).append(i)

    def text(self):
        return "".join(chunk for _, chunk in self.segments)


def _strip_comment(line):
    i = line.find("//")
    return line if i < 0 else line[:i]


def parse_header(header):
    """Ports of a firtool ANSI header: [(direction, range, name)].

    firtool writes one port per line and leaves direction and range off a
    port that shares them with the one before; those are inherited here.
    """
    ports = []
    direction = None
    rng = ""
    for raw in header.splitlines():
        line = _strip_comment(raw).strip().rstrip(",").strip()
        if not line:
            continue
        tokens = line.split()
        if tokens[0] in _DIRECTIONS:
            direction = tokens[0]
            tokens = tokens[1:]
            rng = ""
            if tokens and tokens[0] in ("wire", "logic"):
                tokens = tokens[1:]
            if tokens and _RANGE.match(tokens[0]):
                rng = tokens[0]
                tokens = tokens[1:]
        if direction is None or len(tokens) != 1:
            raise SurgeryError("unrecognised port declaration %r" % raw.strip())
        ports.append((direction, rng, tokens[0]))
    return ports


def emit_header(name, ports):
    lines = ["module %s(" % name]
    for i, (direction, rng, port) in enumerate(ports):
        sep = "," if i + 1 < len(ports) else ""
        lines.append("  %s %s%s%s" % (direction, rng + " " if rng else "", port, sep))
    return "\n".join(lines) + "\n"


def _connections(body):
    """Connections of an instance body, [(port, expr)], parens matched."""
    out = []
    i = 0
    while True:
        dot = body.find(".", i)
        if dot < 0:
            break
        m = re.match(r"\.(\w+)\s*\(", body[dot:])
        if not m:
            raise SurgeryError("unrecognised connection near %r" % body[dot : dot + 40])
        j = dot + m.end()
        depth = 1
        k = j
        while depth:
            if k >= len(body):
                raise SurgeryError("unbalanced connection .%s" % m.group(1))
            c = body[k]
            depth += c == "("
            depth -= c == ")"
            k += 1
        # firtool writes an unconnected output as `/* unused */`.
        out.append((m.group(1), re.sub(r"/\*.*?\*/", "", body[j : k - 1]).strip()))
        i = k
    return out


def _instances(text, module):
    """[(start of connections, end of connections)] of every instance."""
    pat = re.compile(
        r"^(\s+)%s\s+([A-Za-z_][A-Za-z_0-9$]*)\s*\(" % re.escape(module), re.M
    )
    out = []
    for m in pat.finditer(text):
        # The connection list ends at the `);` that closes this instance.
        depth = 1
        k = m.end()
        while depth:
            if k >= len(text):
                raise SurgeryError(
                    "instance %s of %s is not closed" % (m.group(2), module)
                )
            depth += text[k] == "("
            depth -= text[k] == ")"
            k += 1
        out.append((m.end(), k - 1, m.group(1)))
    return out


def _parent_constant(chunk, pos, name):
    """The literal `wire <name> = <literal>;` or `assign <name> = <literal>;`
    in this module gives it, the latter as an earlier round wrote it."""
    pat = r"^\s*(?:wire\s+(?:\[[^\]]+\]\s*)?|assign\s+)%s\s*=\s*([^;]+);" % re.escape(
        name
    )
    values = [v.strip() for v in re.findall(pat, chunk, re.M)]
    values = [v for v in values if _LITERAL.match(v)]
    return values[0] if len(values) == 1 else None


_IDENT = re.compile(r"^[A-Za-z_][A-Za-z_0-9$]*$")


def proven_outputs(found):
    """constant_outputs.py's rows -> {module: (instances, {port: {value: n}})}.

    yosys names every kept module after its instance, <module>$<path>; a
    module seen under n paths has n instances in the design it read.
    """
    seen = {}
    for name in found["modules_scanned"]:
        base = name.split("$")[0]
        seen[base] = seen.get(base, 0) + 1
    ports = {}
    for row in found["constant"]:
        base = row["module"].split("$")[0]
        values = ports.setdefault(base, {}).setdefault(row["port"], {})
        values[row["value"]] = values.get(row["value"], 0) + 1
    return {base: (n, ports.get(base, {})) for base, n in seen.items()}


def _module_instances(net, module, count, where):
    defs = net.where.get(module, [])
    if len(defs) != 1:
        raise SurgeryError(
            "%s: module defined %d times, expected once" % (where, len(defs))
        )
    insts = [
        (i, x)
        for i in net.users.get(module, [])
        for x in _instances(net.segments[i][1], module)
    ]
    if len(insts) != count:
        raise SurgeryError(
            "%s: %d instances, the manifest says %d" % (where, len(insts), count)
        )
    return defs[0], sorted(set(i for i, _ in insts))


def _header(net, i, port, direction, where):
    chunk = net.segments[i][1]
    open_paren = chunk.find("(")
    header_end = chunk.find(");", open_paren)
    ports = parse_header(chunk[open_paren + 1 : header_end])
    hit = [q for q in ports if q[2] == port]
    if len(hit) != 1 or hit[0][0] != direction:
        raise SurgeryError("%s: not one %s port in the header" % (where, direction))
    return chunk, header_end, [q for q in ports if q[2] != port], hit[0][1]


def tie_output(net, e, proven):
    """An output yosys proves constant in every instance: the parent gets
    the constant, the module loses the port and keeps its driver as a wire
    synthesis then removes. drop_output: an output no instance connects,
    which the module then loses the same way."""
    module, port, value, count = e["module"], e["port"], e.get("value"), e["instances"]
    where = "%s.%s" % (module, port)
    drop = e["action"] == "drop_output"
    if not drop:
        if proven is None:
            raise SurgeryError("%s: a tie_output entry needs --proven" % where)
        n, ports = proven.get(module, (0, {}))
        if n != count or ports.get(port, {}).get(value, 0) != count:
            raise SurgeryError(
                "%s: yosys proves %r in %d of %d instances, the manifest says all %d"
                % (where, value, ports.get(port, {}).get(value, 0), n, count)
            )
    defn, users = _module_instances(net, module, count, where)
    for i in users:
        chunk = net.segments[i][1]
        assigns = []
        for start, end, indent in sorted(_instances(chunk, module), reverse=True):
            conns = _connections(chunk[start:end])
            found = [x for p, x in conns if p == port]
            if len(found) != 1:
                raise SurgeryError(
                    "%s: an instance connects the port %d times" % (where, len(found))
                )
            if drop and found[0]:
                raise SurgeryError(
                    "%s: an instance connects it to %r" % (where, found[0])
                )
            if found[0] and not _IDENT.match(found[0]):
                raise SurgeryError(
                    "%s: an instance drives %r, not a net" % (where, found[0])
                )
            if found[0]:
                assigns.append(
                    "  assign %s = %s; // xs_surgery: constant output of %s\n"
                    % (found[0], value, module)
                )
            kept = [(p, x) for p, x in conns if p != port]
            inner = ",\n".join("%s  .%s (%s)" % (indent, p, x) for p, x in kept)
            chunk = (
                chunk[:start]
                + ("\n" + inner + "\n" + indent if kept else "")
                + chunk[end:]
            )
        end = chunk.rfind("endmodule")
        net.segments[i][1] = chunk[:end] + "".join(assigns) + chunk[end:]
    chunk, header_end, rest, rng = _header(net, defn, port, "output", where)
    why = "unconnected by every instance" if drop else "constant %s" % value
    wire = "  wire %s%s; // xs_surgery: %s, no longer a port\n" % (
        rng + " " if rng else "",
        port,
        why,
    )
    net.segments[defn][1] = (
        emit_header(module, rest) + ");\n" + wire + chunk[header_end + 2 :]
    )


def apply_entry(net, e, proven=None):
    if e.get("action", "tie_input") in ("tie_output", "drop_output"):
        return tie_output(net, e, proven)
    module, port, value, count = e["module"], e["port"], e["value"], e["instances"]
    where = "%s.%s" % (module, port)
    defs = net.where.get(module, [])
    if len(defs) != 1:
        raise SurgeryError(
            "%s: module defined %d times, expected once" % (where, len(defs))
        )
    insts = [
        (i, x)
        for i in net.users.get(module, [])
        for x in _instances(net.segments[i][1], module)
    ]
    if len(insts) != count:
        raise SurgeryError(
            "%s: %d instances, the manifest says %d" % (where, len(insts), count)
        )
    for i in sorted(set(i for i, _ in insts)):
        chunk = net.segments[i][1]
        # Back to front, so offsets stay valid.
        for start, end, indent in sorted(_instances(chunk, module), reverse=True):
            conns = _connections(chunk[start:end])
            found = [x for p, x in conns if p == port]
            if len(found) != 1:
                raise SurgeryError(
                    "%s: an instance connects the port %d times" % (where, len(found))
                )
            got = found[0]
            if not _LITERAL.match(got):
                got = _parent_constant(chunk, start, got) or got
            if got != value:
                raise SurgeryError(
                    "%s: an instance ties it to %r, the manifest says %r"
                    % (where, got, value)
                )
            kept = [(p, x) for p, x in conns if p != port]
            inner = ",\n".join("%s  .%s (%s)" % (indent, p, x) for p, x in kept)
            chunk = (
                chunk[:start]
                + ("\n" + inner + "\n" + indent if kept else "")
                + chunk[end:]
            )
        net.segments[i][1] = chunk
    # The module: header without the port, the port as a constant wire.
    i = defs[0]
    chunk = net.segments[i][1]
    open_paren = chunk.find("(")
    header_end = chunk.find(");", open_paren)
    ports = parse_header(chunk[open_paren + 1 : header_end])
    hit = [q for q in ports if q[2] == port]
    if len(hit) != 1 or hit[0][0] != "input":
        raise SurgeryError("%s: not one input port in the header" % where)
    rest = [q for q in ports if q[2] != port]
    rng = hit[0][1]
    wire = "  wire %s%s = %s; // xs_surgery: tied by every instance\n" % (
        rng + " " if rng else "",
        port,
        value,
    )
    net.segments[i][1] = (
        emit_header(module, rest) + ");\n" + wire + chunk[header_end + 2 :]
    )


def apply(text, manifest, protect=(), rounds=None, proven=None, kept=None):
    """Apply the manifest's entries of rounds lo..hi (all when None).

    kept, when given, is the set of modules the plan keeps: an entry for
    any other module is skipped, since synthesis flattens that module and
    folds the constant itself; a carve that changes the blocks changes
    which entries apply, and never makes a kept module's entry pass."""
    net = Netlist(text)
    entries = manifest["entries"]
    if rounds is not None:
        entries = [e for e in entries if rounds[0] <= e["round"] <= rounds[1]]
    if kept is not None:
        skipped = [e for e in entries if e["module"] not in kept]
        for e in skipped:
            sys.stderr.write(
                "xs_surgery: %s.%s: not kept by this plan, skipped\n"
                % (e["module"], e["port"])
            )
        entries = [e for e in entries if e["module"] in kept]
    for e in sorted(entries, key=lambda e: e["round"]):
        if e["module"] in protect:
            raise SurgeryError(
                "%s.%s: %s's ports are protected"
                % (e["module"], e["port"], e["module"])
            )
        apply_entry(net, e, proven)
    return net.text(), len(entries)


def propose(boundary, text, origin):
    net = Netlist(text)
    counts = {}
    entries = []
    for row in boundary:
        tied = row["tied"]
        if len(tied) != 1:
            continue
        value, n = list(tied.items())[0]
        if value != "unconnected" and not _LITERAL.match(value):
            continue
        module = row["module"]
        if module not in counts:
            counts[module] = sum(
                len(_instances(net.segments[i][1], module))
                for i in net.users.get(module, [])
            )
        # Every instance in the file, not only those under the top, must
        # tie it: the module text is shared.
        if n != counts[module]:
            continue
        e = {
            "round": 1,
            "module": module,
            "kind": row["kind"],
            "port": row["port"],
            "instances": counts[module],
            "origin": origin,
        }
        if value == "unconnected":
            e["action"] = "drop_output"
            e["origin"] = "boundary_constants.py: no instance connects it"
        else:
            e["value"] = value
        entries.append(e)
    return {"entries": entries}


def propose_outputs(found, text, round_, origin):
    """tie_output entries: outputs yosys proves constant in every instance."""
    net = Netlist(text)
    entries = []
    for module, (n, ports) in sorted(proven_outputs(found).items()):
        count = sum(
            len(_instances(net.segments[i][1], module))
            for i in net.users.get(module, [])
        )
        if count != n or len(net.where.get(module, [])) != 1:
            continue
        for port, values in sorted(ports.items()):
            if len(values) != 1:
                continue
            value, k = list(values.items())[0]
            if k != n:
                continue
            entries.append(
                {
                    "round": round_,
                    "action": "tie_output",
                    "module": module,
                    "port": port,
                    "value": value,
                    "instances": n,
                    "origin": origin,
                }
            )
    return entries


def main(argv=None):
    p = argparse.ArgumentParser()
    sub = p.add_subparsers(dest="cmd")
    a = sub.add_parser("apply")
    a.add_argument("--manifest", required=True)
    a.add_argument(
        "--protect",
        action="append",
        default=[],
        help="a module whose ports must not change",
    )
    a.add_argument("--rounds", help="lo-hi: apply only these rounds")
    a.add_argument(
        "--proven", help="constant_outputs.py's json, for tie_output entries"
    )
    a.add_argument("--plan", help="the plan's json: only the modules it keeps")
    a.add_argument("--plan-bzl", help="the plan's bzl, with --plan")
    a.add_argument("input")
    a.add_argument("output")
    q = sub.add_parser("propose")
    q.add_argument("--boundary", help="boundary_constants.py's json: tie_input entries")
    q.add_argument("--outputs", help="constant_outputs.py's json: tie_output entries")
    q.add_argument("--round", type=int, default=1)
    q.add_argument("--manifest", help="an existing manifest to extend")
    q.add_argument("--verilog", required=True)
    q.add_argument(
        "--origin", default="boundary_constants.py: one literal on every instance"
    )
    q.add_argument("--out", required=True)
    args = p.parse_args(argv)
    if args.cmd == "apply":
        with open(args.manifest) as f:
            manifest = json.load(f)
        with open(args.input) as f:
            text = f.read()
        rounds = None
        if args.rounds:
            lo, hi = args.rounds.split("-")
            rounds = (int(lo), int(hi))
        proven = None
        if args.proven:
            with open(args.proven) as f:
                proven = proven_outputs(json.load(f))
        kept = None
        if args.plan:
            # beside this file; rules_python does not put its directory on the path
            sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
            from constant_outputs import kept as plan_kept

            with open(args.plan) as f, open(args.plan_bzl) as g:
                kept = plan_kept(json.load(f), g.read())
        try:
            text, n = apply(text, manifest, args.protect, rounds, proven, kept)
        except SurgeryError as e:
            sys.stderr.write("xs_surgery: %s\n" % e)
            return 1
        with open(args.output, "w") as f:
            f.write(text)
        sys.stdout.write("xs_surgery: %d entries applied\n" % n)
        return 0
    if args.cmd == "propose":
        with open(args.verilog) as f:
            text = f.read()
        manifest = {"entries": []}
        if args.manifest:
            with open(args.manifest) as f:
                manifest = json.load(f)
        if args.boundary:
            with open(args.boundary) as f:
                new = propose(json.load(f), text, args.origin)["entries"]
            for e in new:
                e["round"] = args.round
        else:
            with open(args.outputs) as f:
                new = propose_outputs(
                    json.load(f),
                    text,
                    args.round,
                    "constant_outputs.py: yosys proves it in every instance",
                )
        manifest["entries"] = [
            e for e in manifest["entries"] if e["round"] < args.round
        ] + new
        with open(args.out, "w") as f:
            json.dump(manifest, f, indent=1, sort_keys=True)
            f.write("\n")
        sys.stdout.write("xs_surgery: %d entries proposed\n" % len(manifest["entries"]))
        return 0
    p.print_help()
    return 2


if __name__ == "__main__":
    sys.exit(main())
