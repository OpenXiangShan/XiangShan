#!/usr/bin/env python3
from __future__ import annotations

import csv
import io
import re
import runpy
from pathlib import Path


_FRONTEND_ROOT = Path(__file__).resolve().parents[1]
_OWNER_MODEL = runpy.run_path(
    str(_FRONTEND_ROOT / "env" / "funcov" / "py" / "ifu" / "owner_v3_funcov.py")
)
OWNER_V3_BIN_SPECS = _OWNER_MODEL["OWNER_V3_BIN_SPECS"]
OWNER_V3_COVERPOINT = _OWNER_MODEL["OWNER_V3_COVERPOINT"]
OWNER_V3_EVENT_TYPE = _OWNER_MODEL["OWNER_V3_EVENT_TYPE"]


_BIN_RE = re.compile(r"BIN-\d+")
_TESTPOINT_PATH = (
    _FRONTEND_ROOT
    / "docs"
    / "02_testpoint"
    / "Frontend_testpoint_0525_coverage_backannotated.csv"
)
_PILOT_PATH = (
    _FRONTEND_ROOT
    / "docs"
    / "03_funcov_model"
    / "frontend_bt_functional_coverage_pilot.csv"
)


def _read_csv(path: Path) -> tuple[list[str], list[dict[str, str]]]:
    with path.open(encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        return list(reader.fieldnames or []), list(reader)


def _write_csv(path: Path, fields: list[str], rows: list[dict[str, str]]) -> None:
    with path.open("w", encoding="utf-8-sig", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def _append_pilot_rows(path: Path, fields: list[str], rows: list[dict[str, str]]) -> None:
    buffer = io.StringIO(newline="")
    writer = csv.DictWriter(buffer, fieldnames=fields, lineterminator="\n")
    writer.writerows(rows)
    with path.open("a", encoding="utf-8", newline="") as handle:
        handle.write(buffer.getvalue())


def _owner_blocks(rows: list[dict[str, str]]) -> list[list[int]]:
    level_fields = ["一级测试点", "二级测试点", "三级测试点", "四级测试点", "五级测试点"]
    marker_indices = [
        index
        for index, row in enumerate(rows)
        if "@加柏文" in "".join(str(row.get(field, "")) for field in level_fields)
    ]
    if len(marker_indices) != 4:
        raise ValueError(f"expected four @加柏文 owner markers, found {len(marker_indices)}")

    blocks: list[list[int]] = []
    for marker_index in marker_indices:
        marker = rows[marker_index]
        marker_depth = next(
            index for index, field in enumerate(level_fields) if marker[field].strip()
        )
        block_end = len(rows)
        for index in range(marker_index + 1, len(rows)):
            if any(rows[index][field].strip() for field in level_fields[: marker_depth + 1]):
                block_end = index
                break
        blocks.append(list(range(marker_index, block_end)))
    return blocks


def _mapped_path(rows: list[dict[str, str]], row_index: int) -> str:
    level_fields = ["一级测试点", "二级测试点", "三级测试点", "四级测试点", "五级测试点"]
    headings = [""] * len(level_fields)
    for index in range(row_index + 1):
        row = rows[index]
        for depth, field in enumerate(level_fields):
            value = row[field].strip()
            if not value:
                continue
            headings[depth] = value.replace("@加柏文", "").strip()
            headings[depth + 1 :] = [""] * (len(headings) - depth - 1)
    return "/".join(value for value in headings if value)


def synchronize() -> dict[str, int]:
    testpoint_fields, testpoint_rows = _read_csv(_TESTPOINT_PATH)
    pilot_fields, pilot_rows = _read_csv(_PILOT_PATH)
    target_ids = {spec.bin_id for spec in OWNER_V3_BIN_SPECS}
    blocks = _owner_blocks(testpoint_rows)

    target_indices: list[int] = []
    for block_number, block in enumerate(blocks, start=1):
        if block_number == 2:
            continue
        for index in block:
            row = testpoint_rows[index]
            if not row["五级测试点"].strip():
                continue
            bin_ids = set(_BIN_RE.findall(row["coverage"]))
            if row["status"].strip() == "UNMAPPED" or bin_ids & target_ids:
                target_indices.append(index)

    if len(target_indices) != len(OWNER_V3_BIN_SPECS):
        raise ValueError(
            f"owner model target count changed: rows={len(target_indices)} "
            f"specs={len(OWNER_V3_BIN_SPECS)}"
        )

    pilot_by_id = {row["Bin_ID"].strip(): row for row in pilot_rows}
    new_pilot_rows: list[dict[str, str]] = []
    for row_index, spec in zip(target_indices, OWNER_V3_BIN_SPECS, strict=True):
        row = testpoint_rows[row_index]
        path = _mapped_path(testpoint_rows, row_index)
        coverage = (
            f"covergroup {spec.coverage_group}, coverpoint {OWNER_V3_COVERPOINT}, "
            f"bins {spec.bin_name} ({spec.bin_id})"
        )
        row["coverage"] = coverage
        row["status"] = "MODELED"
        row["testcase"] = spec.suggested_testcase
        row["evidence"] = "MODEL:test_ifu_v3_owner_event_model"

        pilot_row = {
            "Bin_ID": spec.bin_id,
            "阶段": "L1",
            "覆盖类型": "语义事件覆盖",
            "Coverage_Group": spec.coverage_group,
            "Coverpoint": OWNER_V3_COVERPOINT,
            "Bin_Name": spec.bin_name,
            "映射测试点路径": path,
            "建议采样事件": (
                f"Directed checker emits {OWNER_V3_EVENT_TYPE} after evaluating "
                "the leaf condition and checkpoint"
            ),
            "建议观测对象": row["Object"].strip(),
            "命中判据": (
                "condition_met与checkpoint_passed均为true，且observations包含实际观测值"
            ),
            "优先级": "P1",
            "建议试点用例": spec.suggested_testcase,
            "Legacy_Bin_ID": "",
        }
        existing = pilot_by_id.get(spec.bin_id)
        if existing is None:
            new_pilot_rows.append(pilot_row)
            pilot_by_id[spec.bin_id] = pilot_row
        elif any(existing.get(field, "") != value for field, value in pilot_row.items()):
            raise ValueError(f"existing pilot row differs from owner model: {spec.bin_id}")

    _write_csv(_TESTPOINT_PATH, testpoint_fields, testpoint_rows)
    if new_pilot_rows:
        if len(new_pilot_rows) != len(OWNER_V3_BIN_SPECS):
            raise ValueError("owner pilot registry is only partially populated")
        _append_pilot_rows(_PILOT_PATH, pilot_fields, new_pilot_rows)
    return {
        "owner_leaves_modeled": len(target_indices),
        "pilot_rows": len(pilot_rows) + len(new_pilot_rows),
        "first_bin": OWNER_V3_BIN_SPECS[0].bin_id,
        "last_bin": OWNER_V3_BIN_SPECS[-1].bin_id,
    }


if __name__ == "__main__":
    print(synchronize())
