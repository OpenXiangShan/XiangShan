"""One module of the packed-vector elaboration after proc, and nothing
else, as yosys JSON: the input of entries.py.

    proc_json.py <yosys> <slang.so> <sv dir> <top> <out.json> [<module>,...]

The modules named last stay blackboxes (the Rob's register files, each a
macro of its own) and are written out by nobody.
"""

import subprocess
import sys

from dryrun import closure


def main(argv):
    yosys, slang, sv_dir, top, out = argv[1:6]
    boxes = [m for m in (argv[6] if len(argv) > 6 else "").split(",") if m]
    files = closure(sv_dir, top)
    script = "; ".join(
        [
            f"read_slang --top {top} "
            + "".join(f"--blackboxed-module {m} " for m in boxes)
            + " ".join(files),
            f"hierarchy -top {top}",
            "proc",
            f"write_json {out}",
        ]
    )
    r = subprocess.run(
        [yosys, "-q", "-m", slang, "-p", script], capture_output=True, text=True
    )
    if r.returncode:
        sys.exit(r.stdout[-3000:] + r.stderr[-3000:])


if __name__ == "__main__":
    main(sys.argv)
