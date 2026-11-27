"""The core with extracted modules in place of the originals: each
extracted file's top module's definition dropped from the core, the
extracted file (its top and its entry cell) appended.

    substitute.py <core.sv> <out.sv> <extracted.v>...

Stops if a top is not defined in the core exactly once, or an entry cell
already is: the substitution then would not be the one the proofs cover.
"""

import re
import sys

_MODULE = re.compile(r"^module\s+([A-Za-z_][A-Za-z0-9_$]*)")


def modules(path):
    with open(path) as f:
        return [m.group(1) for m in map(_MODULE.match, f) if m]


def main(argv):
    core, out, extracted = argv[1], argv[2], argv[3:]
    tops = {}
    for path in extracted:
        names = modules(path)
        entries = [n for n in names if n.endswith("Entry")]
        rest = [n for n in names if not n.endswith("Entry")]
        if len(rest) != 1 or len(entries) != 1:
            sys.exit(
                f"substitute: {path} defines {names}, not one top and one entry cell"
            )
        tops[rest[0]] = entries[0]
    found = dict.fromkeys(tops, 0)
    entries = set(tops.values())
    skipping = False
    with open(core) as f, open(out, "w") as o:
        for line in f:
            m = _MODULE.match(line)
            if m:
                if m.group(1) in entries:
                    sys.exit(f"substitute: the core already defines {m.group(1)}")
                skipping = m.group(1) in found
                if skipping:
                    found[m.group(1)] += 1
            if not skipping:
                o.write(line)
            elif line.startswith("endmodule"):
                skipping = False
        if skipping:
            sys.exit("substitute: the core ends inside a module")
        bad = {t: n for t, n in found.items() if n != 1}
        if bad:
            sys.exit(f"substitute: modules not defined exactly once in the core: {bad}")
        for path in extracted:
            with open(path) as f:
                o.write(f.read())
    print(f"substitute: {', '.join(sorted(tops))} replaced by their extractions")


if __name__ == "__main__":
    main(sys.argv)
