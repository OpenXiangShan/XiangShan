import csv
from pathlib import Path

import pytest

from tools import backannotate_funcov


def _write_pilot(path: Path) -> None:
    path.write_text(
        "Bin_ID,Coverage_Group,Coverpoint,Bin_Name,建议试点用例\n"
        "BIN-001,group_a,point_a,value_a,case_point\n"
        "BIN-002,group_a,point_a,value_b,case_point\n",
        encoding="utf-8-sig",
    )


def _write_testpoints(path: Path, *coverage_rows: str) -> None:
    path.write_text(
        "一级测试点,coverage,status,testcase,evidence\n"
        + "".join(
            f'leaf_{index},"{coverage}",MODELED,,\n'
            for index, coverage in enumerate(coverage_rows, 1)
        ),
        encoding="utf-8-sig",
    )


def test_point_level_leaf_owns_the_coverpoint_distribution_bins(tmp_path):
    pilot_path = tmp_path / "pilot.csv"
    testpoint_path = tmp_path / "testpoints.csv"
    _write_pilot(pilot_path)
    _write_testpoints(testpoint_path, "covergroup group_a, coverpoint point_a")

    mapping = backannotate_funcov.validate_mapping(
        testpoint_path,
        backannotate_funcov.load_pilot(pilot_path),
    )

    assert mapping == {"BIN-001": 2, "BIN-002": 2}


def test_point_and_bin_level_ownership_cannot_overlap(tmp_path):
    pilot_path = tmp_path / "pilot.csv"
    testpoint_path = tmp_path / "testpoints.csv"
    _write_pilot(pilot_path)
    _write_testpoints(
        testpoint_path,
        "covergroup group_a, coverpoint point_a",
        "covergroup group_a, coverpoint point_a, bins value_a (BIN-001)",
    )

    with pytest.raises(
        ValueError, match="mixes bin-level ownership with point-level ownership"
    ):
        backannotate_funcov.validate_mapping(
            testpoint_path,
            backannotate_funcov.load_pilot(pilot_path),
        )


def test_semicolon_separated_sv_and_python_mappings_are_both_owned(tmp_path):
    pilot_path = tmp_path / "pilot.csv"
    testpoint_path = tmp_path / "testpoints.csv"
    _write_pilot(pilot_path)
    _write_testpoints(
        testpoint_path,
        "covergroup group_a, coverpoint point_a, bins value_a (BIN-001); "
        "covergroup group_a, coverpoint point_a, bins value_b (BIN-002)",
    )

    mapping = backannotate_funcov.validate_mapping(
        testpoint_path,
        backannotate_funcov.load_pilot(pilot_path),
    )

    assert mapping == {"BIN-001": 2, "BIN-002": 2}


def test_one_positive_child_bin_hits_a_point_level_leaf(tmp_path, monkeypatch):
    pilot_path = tmp_path / "pilot.csv"
    testpoint_path = tmp_path / "testpoints.csv"
    _write_pilot(pilot_path)
    _write_testpoints(testpoint_path, "covergroup group_a, coverpoint point_a")
    monkeypatch.setattr(
        backannotate_funcov,
        "evaluate_artifact",
        lambda _raw: {"eligible": True, "reasons": []},
    )
    raw = {
        "artifact_tag": "case_point",
        "coverage_targets": {"hit_keys": ["group_a::point_a"]},
        "hits": {
            "group_a::point_a::value_a": {"hits": 2},
            "group_a::point_a::value_b": {"hits": 0},
        },
        "stats": {"monitor": {"error_count": 0}},
        "run": {"run_id": "point-level-unit"},
    }

    counts = backannotate_funcov.backannotate(
        testpoint_path,
        backannotate_funcov.load_pilot(pilot_path),
        [(tmp_path / "case_point.funcov.json", raw, "dut")],
        apply=True,
    )

    assert counts["hit"] == 1
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        row = next(csv.DictReader(handle))
    assert row["status"] == "HIT"
    assert row["testcase"] == "case_point"
    assert "DUT:case_point:hits=2" in row["evidence"]


def test_hierarchy_filter_preserves_rows_outside_the_selected_branch(
    tmp_path, monkeypatch
):
    pilot_path = tmp_path / "pilot.csv"
    testpoint_path = tmp_path / "testpoints.csv"
    _write_pilot(pilot_path)
    testpoint_path.write_text(
        "一级测试点,二级测试点,coverage,status,testcase,evidence\n"
        'cacheable取指,ICache,"covergroup group_a, coverpoint point_a, bins value_a (BIN-001)",MODELED,,\n'
        '其他取指,ICache,"covergroup group_a, coverpoint point_a, bins value_b (BIN-002)",HIT,,historical\n',
        encoding="utf-8-sig",
    )
    monkeypatch.setattr(
        backannotate_funcov,
        "evaluate_artifact",
        lambda _raw: {"eligible": True, "reasons": []},
    )
    raw = {
        "artifact_tag": "case_point",
        "coverage_targets": {"bin_ids": ["BIN-001"]},
        "hits": {"group_a::point_a::value_a": {"hits": 1}},
        "stats": {"monitor": {"error_count": 0}},
        "run": {"run_id": "hierarchy-unit"},
    }

    counts = backannotate_funcov.backannotate(
        testpoint_path,
        backannotate_funcov.load_pilot(pilot_path),
        [(tmp_path / "case_point.funcov.json", raw, "dut")],
        apply=True,
        hierarchy_filters={"一级测试点": "cacheable取指", "二级测试点": "icache"},
    )

    assert counts["hit"] == 1
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        rows = list(csv.DictReader(handle))
    assert [row["status"] for row in rows] == ["HIT", "HIT"]
    assert rows[1]["evidence"] == "historical"


def test_scoped_artifacts_do_not_downgrade_existing_partial(tmp_path):
    pilot_path = tmp_path / "pilot.csv"
    testpoint_path = tmp_path / "testpoints.csv"
    _write_pilot(pilot_path)
    _write_testpoints(
        testpoint_path,
        "covergroup group_a, coverpoint point_a, bins value_a (BIN-001)",
    )
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        fields = list(csv.DictReader(handle).fieldnames or [])
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        rows = list(csv.DictReader(handle))
    rows[0]["status"] = "PARTIAL"
    with testpoint_path.open("w", encoding="utf-8-sig", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)

    counts = backannotate_funcov.backannotate(
        testpoint_path,
        backannotate_funcov.load_pilot(pilot_path, bin_prefix="BIN-001"),
        [],
        apply=True,
        bin_prefix="BIN-001",
    )

    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        row = next(csv.DictReader(handle))
    assert counts["partial"] == 1
    assert row["status"] == "PARTIAL"


def test_artifact_gate_is_evaluated_once_for_multiple_owned_bins(
    tmp_path, monkeypatch
):
    pilot_path = tmp_path / "pilot.csv"
    testpoint_path = tmp_path / "testpoints.csv"
    _write_pilot(pilot_path)
    _write_testpoints(
        testpoint_path,
        "covergroup group_a, coverpoint point_a, bins value_a (BIN-001)",
        "covergroup group_a, coverpoint point_a, bins value_b (BIN-002)",
    )
    evaluations = 0

    def eligible(_raw):
        nonlocal evaluations
        evaluations += 1
        return {"eligible": True, "reasons": []}

    monkeypatch.setattr(backannotate_funcov, "evaluate_artifact", eligible)
    raw = {
        "artifact_tag": "case_point",
        "coverage_targets": {"bin_ids": ["BIN-001", "BIN-002"]},
        "hits": {
            "group_a::point_a::value_a": {"hits": 1},
            "group_a::point_a::value_b": {"hits": 1},
        },
        "stats": {"monitor": {"error_count": 0}},
        "run": {"run_id": "gate-cache-unit"},
    }

    counts = backannotate_funcov.backannotate(
        testpoint_path,
        backannotate_funcov.load_pilot(pilot_path),
        [(tmp_path / "case_point.funcov.json", raw, "dut")],
        apply=False,
    )

    assert counts["hit"] == 2
    assert evaluations == 1
