"""A dry run of generate_array's front end on one XiangShan module: which
of its arrays come back as memories (array_reroll) and what the rest are
(classify.py), from the packed-vector elaboration (xiangshan_sv_vec).
Nothing is changed; a report is written.

    dryrun.py <yosys> <slang.so> <arrays.so> <classify.py> <sv dir> <top> <out.txt>
"""

import os
import re
import subprocess
import sys

_INST = re.compile(r"^\s*(\w+)\s+(?:#\(.*?\)\s*)?\w+\s*\(", re.M)


def closure(sv_dir, top):
    """The files of top and every module it instances, from the directory;
    a single file (the surgered core) as it is."""
    if os.path.isfile(sv_dir):
        return [sv_dir]
    have = {f[: -len(".sv")] for f in os.listdir(sv_dir) if f.endswith(".sv")}
    seen, todo = [], [top]
    while todo:
        m = todo.pop()
        if m in seen or m not in have:
            continue
        seen.append(m)
        text = open(os.path.join(sv_dir, m + ".sv"), errors="replace").read()
        todo += [i for i in set(_INST.findall(text)) if i in have and i not in seen]
    return [os.path.join(sv_dir, m + ".sv") for m in seen]


def main(argv):
    yosys, slang, arrays, classify, sv_dir, top, out = argv[1:8]
    files = closure(sv_dir, top)
    work = os.path.dirname(os.path.abspath(out))
    js = os.path.join(work, top + ".json")
    script = "; ".join(
        [
            f"read_slang --top {top} " + " ".join(files),
            f"hierarchy -top {top}",
            "proc",
            "opt -fast",
            "tee -o " + os.path.join(work, top + "_before.txt") + " stat",
            "array_reroll",
            "opt -fast",
            "tee -o " + os.path.join(work, top + "_after.txt") + " stat",
            f"write_json {js}",
        ]
    )
    log = subprocess.run(
        [yosys, "-q", "-m", slang, "-m", arrays, "-p", script],
        capture_output=True,
        text=True,
    )
    with open(out, "w") as f:
        f.write(f"# {top}: {len(files)} files\n")
        if log.returncode:
            f.write(log.stdout[-4000:] + log.stderr[-4000:])
            sys.exit(f"dryrun: yosys failed on {top}, see {out}")
        for tag in ("before", "after"):
            stat = open(os.path.join(work, f"{top}_{tag}.txt")).read()
            mems = re.findall(r"^\s+(\d+)\s+\S*\s*memories", stat, re.M)
            bits = re.findall(r"^\s+(\d+)\s+\S*\s*memory bits", stat, re.M)
            f.write(
                f"# {tag} array_reroll: memories {mems[:1]}, memory bits {bits[:1]}\n"
            )
        cls = subprocess.run(
            [sys.executable, classify, js, "--module", top],
            capture_output=True,
            text=True,
        )
        f.write(cls.stdout + cls.stderr[-2000:])


if __name__ == "__main__":
    main(sys.argv)
