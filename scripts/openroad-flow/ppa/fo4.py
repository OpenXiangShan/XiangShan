"""Measure fanout-of-4 (FO4) inverter delay directly from NLDM Liberty tables.

A chain of minimum-sized inverters (`INVx1`), each driving four copies of
itself (`load = 4 * C_in`), converges rapidly to steady-state rising and
falling slews. The mean of the rising and falling stage delays is 1 FO4 in
the library's time unit (ps for ASAP7).

Dividing any OpenSTA timing measurement (period or path delay in ps) by
`fo4_ps` expresses timing in PDK-independent FO4 units. XiangShan's 3 GHz
(333 ps) commercial 7 nm target corresponds to `333 / 8.1 = 41.1 FO4` at
published ASAP7 RVT (`8.1 ps`, Clark et al. 2017), or ~591 ps in ORFS's
extracted ASAP7 RVT FF NLDM library (`fo4_ps ~= 14.37 ps`).
"""

import argparse
import gzip
import json
import re
import sys

XIANGSHAN_7NM_TARGET_PS = 333.0
PUBLISHED_ASAP7_FO4_PS = {"RVT": 8.1, "LVT": 6.8, "SLVT": 6.0}


def _block(text, header):
    start = text.index(header)
    i = text.index("{", start) + 1
    depth = 1
    j = i
    while depth:
        c = text[j]
        if c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
        j += 1
    return text[i : j - 1]


def _floats(s):
    return [float(x) for x in re.findall(r"[-+0-9.eE]+", s)]


def _table(timing, name):
    body = _block(timing, name + " (")
    index_1 = _floats(re.search(r"index_1\s*\(([^)]*)\)", body).group(1))
    index_2 = _floats(re.search(r"index_2\s*\(([^)]*)\)", body).group(1))
    values = _floats(re.search(r"values\s*\((.*?)\)\s*;", body, re.S).group(1))
    rows = [
        values[k * len(index_2) : (k + 1) * len(index_2)]
        for k in range(len(index_1))
    ]
    return index_1, index_2, rows


def _bracket(axis, x):
    k = 0
    while k < len(axis) - 2 and x > axis[k + 1]:
        k += 1
    return k, (x - axis[k]) / (axis[k + 1] - axis[k])


def lookup(table, slew, load):
    index_1, index_2, rows = table
    i, u = _bracket(index_1, slew)
    j, v = _bracket(index_2, load)
    a = rows[i][j] + v * (rows[i][j + 1] - rows[i][j])
    b = rows[i + 1][j] + v * (rows[i + 1][j + 1] - rows[i + 1][j])
    return a + u * (b - a)


def read_lib(path):
    opener = gzip.open if path.endswith(".gz") else open
    with opener(path, "rt", encoding="utf-8", errors="replace") as f:
        return f.read()


def measure_fo4_ps(lib_text, cell):
    """Steady-state FO4 delay in the Liberty file's time unit (ps for ASAP7)."""
    body = _block(lib_text, "cell (%s)" % cell)
    cin = float(
        re.search(r"\bcapacitance\s*:\s*([0-9.eE+-]+)", _block(body, "pin (A)")).group(1)
    )
    timing = _block(_block(body, "pin (Y)"), "timing ()")
    cell_rise = _table(timing, "cell_rise")
    cell_fall = _table(timing, "cell_fall")
    rise_transition = _table(timing, "rise_transition")
    fall_transition = _table(timing, "fall_transition")
    load = 4.0 * cin
    rise_slew = fall_slew = 10.0
    d_rise = d_fall = 0.0
    for _ in range(200):
        d_fall = lookup(cell_fall, rise_slew, load)
        fall_slew_next = lookup(fall_transition, rise_slew, load)
        d_rise = lookup(cell_rise, fall_slew_next, load)
        rise_slew_next = lookup(rise_transition, fall_slew_next, load)
        if (
            abs(rise_slew_next - rise_slew) < 1e-9
            and abs(fall_slew_next - fall_slew) < 1e-9
        ):
            rise_slew, fall_slew = rise_slew_next, fall_slew_next
            break
        rise_slew, fall_slew = rise_slew_next, fall_slew_next
    return (d_rise + d_fall) / 2.0


def build_fo4_calibration(triples):
    """Build calibration dictionary from `[(vt, lib_path, cell_name), ...]`."""
    vt_fo4 = {}
    for vt, lib_path, cell in triples:
        vt_fo4[vt] = round(measure_fo4_ps(read_lib(lib_path), cell), 4)
    primary_vt, _, primary_cell = triples[0]
    primary_fo4 = vt_fo4[primary_vt]
    pub_fo4 = PUBLISHED_ASAP7_FO4_PS.get(primary_vt, 8.1)
    target_fo4 = XIANGSHAN_7NM_TARGET_PS / pub_fo4
    return {
        "pdk": "asap7",
        "primary_vt": primary_vt,
        "primary_cell": primary_cell,
        "fo4_ps": primary_fo4,
        "vt_fo4_ps": vt_fo4,
        "xiangshan_7nm_target_ps": XIANGSHAN_7NM_TARGET_PS,
        "published_fo4_ps": PUBLISHED_ASAP7_FO4_PS,
        "target_fo4": round(target_fo4, 2),
        "asap7_target_ps": round(target_fo4 * primary_fo4, 1),
    }


def main(argv=None):
    ap = argparse.ArgumentParser(description="Measure Liberty FO4 calibration JSON.")
    ap.add_argument("--out", required=True, help="Output calibration JSON path.")
    ap.add_argument(
        "specs",
        nargs="+",
        help="Triples of <VT> <lib[.gz]> <inverter_cell>, primary VT first.",
    )
    args = ap.parse_args(argv)
    if len(args.specs) % 3 != 0:
        ap.error("specs must be <VT> <lib> <cell> triples")
    triples = [
        tuple(args.specs[i : i + 3]) for i in range(0, len(args.specs), 3)
    ]
    cal = build_fo4_calibration(triples)
    with open(args.out, "w", encoding="utf-8") as f:
        json.dump(cal, f, indent=2, sort_keys=True)
        f.write("\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
