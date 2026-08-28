#!/usr/bin/env python3
"""Validate the committed Constantin files against the selected GEM5 DSE points."""

from __future__ import annotations

from pathlib import Path
import sys


ROOT = Path(__file__).resolve().parents[1]
CONFIG_DIR = ROOT / "configs" / "tage-dse"
NUM_BANKS = 4
NUM_TABLES = 8
RTL_SET_LOG2_BOUNDS = (4, 11)
GEM5_SET_LOG2_BOUNDS = (
    (4, 9),
    (4, 9),
    (5, 11),
    (5, 11),
    (5, 11),
    (4, 9),
    (4, 9),
    (4, 9),
)

EXPECTED = {
    "gem5-baseline.cst": {
        "set_log2": [9] * 8,
        "ways": [2] * 8,
        "tag_bits": [13] * 8,
        "capacity_bits": 589824,
    },
    "gem5-p1-trial-0748.cst": {
        "set_log2": [5, 6, 9, 10, 9, 8, 7, 8],
        "ways": [8, 3, 4, 3, 3, 5, 1, 3],
        "tag_bits": [15, 11, 8, 11, 18, 18, 15, 17],
        "capacity_bits": 672768,
    },
    "gem5-c4-trial-0166.cst": {
        "set_log2": [6, 6, 9, 10, 9, 9, 8, 9],
        "ways": [2, 3, 2, 3, 3, 2, 2, 1],
        "tag_bits": [15, 18, 16, 11, 18, 11, 13, 13],
        "capacity_bits": 591104,
    },
}


def parse_cst(path: Path) -> dict[str, int]:
    values: dict[str, int] = {}
    for line_number, line in enumerate(path.read_text(encoding="ascii").splitlines(), start=1):
        fields = line.split()
        if len(fields) != 2:
            raise ValueError(f"{path}:{line_number}: expected '<name> <unsigned decimal>'")
        name, value = fields
        if name in values:
            raise ValueError(f"{path}:{line_number}: duplicate Constantin key {name}")
        if not value.isdecimal():
            raise ValueError(f"{path}:{line_number}: expected an unsigned decimal value")
        values[name] = int(value, 10)
    return values


def table_values(values: dict[str, int], field: str) -> list[int]:
    return [values[f"tageTable{field}_0_{index}"] for index in range(NUM_TABLES)]


def expected_keys() -> set[str]:
    table_fields = ("NumSetsLog2", "NumWays", "TagWidth")
    return {
        *(f"tageTable{field}_0_{index}" for field in table_fields for index in range(NUM_TABLES)),
        "tageUsefulCtrWidth_0",
    }


def validate_dse_domain(filename: str, set_log2: list[int], ways: list[int], tag_bits: list[int]) -> None:
    for table_index, (sets, ways_per_table, tag_width) in enumerate(zip(set_log2, ways, tag_bits)):
        gem5_min, gem5_max = GEM5_SET_LOG2_BOUNDS[table_index]
        if not gem5_min <= sets <= gem5_max:
            raise ValueError(
                f"{filename}: T{table_index} set log2 {sets} is outside the GEM5 DSE domain "
                f"{gem5_min}..{gem5_max}"
            )
        if not RTL_SET_LOG2_BOUNDS[0] <= sets <= RTL_SET_LOG2_BOUNDS[1]:
            raise ValueError(f"{filename}: T{table_index} set log2 {sets} is outside the RTL domain")
        if not 1 <= ways_per_table <= 8:
            raise ValueError(f"{filename}: T{table_index} ways {ways_per_table} is outside the RTL domain")
        if not 8 <= tag_width <= 20:
            raise ValueError(f"{filename}: T{table_index} tag width {tag_width} is outside the RTL domain")


def require_equal(filename: str, field: str, actual: object, expected: object) -> None:
    if actual != expected:
        raise ValueError(f"{filename}: {field}={actual!r}, expected {expected!r}")


def main() -> int:
    for filename, expected in EXPECTED.items():
        path = CONFIG_DIR / filename
        values = parse_cst(path)
        if values.keys() != expected_keys():
            missing = sorted(expected_keys() - values.keys())
            unexpected = sorted(values.keys() - expected_keys())
            raise ValueError(f"{filename}: missing={missing}, unexpected={unexpected}")
        set_log2 = table_values(values, "NumSetsLog2")
        ways = table_values(values, "NumWays")
        tag_bits = table_values(values, "TagWidth")
        validate_dse_domain(filename, set_log2, ways, tag_bits)
        capacity_bits = sum(
            NUM_BANKS * (1 << log2_sets) * ways_per_table * (tag_width + 5)
            for log2_sets, ways_per_table, tag_width in zip(set_log2, ways, tag_bits)
        )

        require_equal(filename, "set_log2", set_log2, expected["set_log2"])
        require_equal(filename, "ways", ways, expected["ways"])
        require_equal(filename, "tag_bits", tag_bits, expected["tag_bits"])
        require_equal(filename, "tageUsefulCtrWidth_0", values["tageUsefulCtrWidth_0"], 2)
        require_equal(filename, "capacity_bits", capacity_bits, expected["capacity_bits"])
        print(f"{filename}: {capacity_bits} bit ({capacity_bits / 8192:.5f} KiB)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
