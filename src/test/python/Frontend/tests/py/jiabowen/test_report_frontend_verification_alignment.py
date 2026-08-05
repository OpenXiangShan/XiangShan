import csv
import importlib.util
from pathlib import Path


def _load_report_module():
    path = Path(__file__).resolve().parents[3] / "tools" / "report_frontend_verification_alignment.py"
    spec = importlib.util.spec_from_file_location("frontend_alignment_report", path)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_alignment_report_counts_terminal_leaves_at_any_hierarchy_depth(tmp_path):
    report = _load_report_module()
    path = tmp_path / "testpoints.csv"
    fields = [
        "一级测试点", "二级测试点", "三级测试点", "四级测试点", "五级测试点",
        "Condition", "Checkpoint", "Object", "coverage", "status",
    ]
    rows = [
        {"一级测试点": "TOP"},
        {"一级测试点": "TOP", "二级测试点": "feature3", "三级测试点": "unmapped",
         "Condition": "condition", "Checkpoint": "checkpoint", "Object": "object", "status": "UNMAPPED"},
        {"一级测试点": "TOP", "二级测试点": "feature4", "三级测试点": "group", "四级测试点": "modeled",
         "Condition": "condition", "Checkpoint": "checkpoint", "Object": "object",
         "coverage": "covergroup g, coverpoint p, bins b (BIN-001)", "status": "MODELED"},
        {"一级测试点": "TOP", "二级测试点": "feature5", "三级测试点": "group", "四级测试点": "group",
         "五级测试点": "missing status", "Condition": "condition", "Checkpoint": "checkpoint", "Object": "object",
         "coverage": "covergroup g, coverpoint p, bins c (BIN-002)"},
        {"一级测试点": "TOP", "二级测试点": "feature4b", "三级测试点": "partial missing coverage",
         "Condition": "condition", "Checkpoint": "checkpoint", "Object": "object", "status": "PARTIAL"},
    ]
    with path.open("w", encoding="utf-8-sig", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)

    summary = report.summarize_testpoints(path)

    assert summary["leaf_denominator"] == 4
    assert summary["status_counts"] == {
        "UNMAPPED": 1,
        "MODELED": 1,
        "PARTIAL": 1,
        "HIT": 0,
        "CLOSED": 0,
        "BLOCKED": 0,
        "N-A": 0,
        "SV_FUNCOV": 0,
        "UNSPECIFIED": 1,
    }
    assert summary["coverage_mapped_leaf_count"] == 2
    assert summary["modeled_progress_leaf_count"] == 2
    assert {item["kind"] for item in summary["schema_errors"]} == {
        "missing_status",
        "modeled_status_without_coverage",
    }


def test_alignment_report_excludes_explicit_na_from_progress_denominator(tmp_path):
    report = _load_report_module()
    path = tmp_path / "testpoints.csv"
    fields = [
        "一级测试点", "二级测试点", "三级测试点", "四级测试点", "五级测试点",
        "Condition", "Checkpoint", "Object", "coverage", "status",
    ]
    rows = [
        {"一级测试点": "TOP", "二级测试点": "executable", "Condition": "c", "Checkpoint": "p", "Object": "o", "status": "UNMAPPED"},
        {"一级测试点": "TOP", "二级测试点": "not_applicable", "Condition": "c", "Checkpoint": "p", "Object": "o", "status": "N-A"},
    ]
    with path.open("w", encoding="utf-8-sig", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)

    summary = report.summarize_testpoints(path)

    assert summary["all_leaf_count"] == 2
    assert summary["leaf_denominator"] == 1
    assert summary["non_executable_leaf_count"] == 1
    assert summary["schema_error_count"] == 0
