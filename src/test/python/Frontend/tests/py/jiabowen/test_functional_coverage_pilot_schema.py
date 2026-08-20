import csv
import re
from pathlib import Path

import pytest

from tools.backannotate_funcov import validate_pilot_schema


IFU_FUNCOV_BIN_IDS = {
    *(f"BIN-{index:03d}" for index in range(401, 416)),
    *(f"BIN-{index:03d}" for index in range(424, 433)),
    *(f"BIN-{index:03d}" for index in range(501, 542)),
    "BIN-824",
    *(f"BIN-{index:03d}" for index in range(832, 837)),
}

ICACHE_PREFETCH_FUNCOV_BIN_IDS = {
    *(f"BIN-{index:03d}" for index in range(650, 674)),
    *(f"BIN-{index:03d}" for index in range(677, 686)),
    *(f"BIN-{index:03d}" for index in range(777, 781)),
}

ICACHE_MISSUNIT_FUNCOV_BIN_IDS = {
    *(f"BIN-{index:03d}" for index in range(686, 717)),
}

ICACHE_WAYLOOKUP_FUNCOV_BIN_IDS = {
    *(f"BIN-{index:03d}" for index in range(717, 759)),
}

ICACHE_HITMISS_FUNCOV_BIN_IDS = {
    *(f"BIN-{index:03d}" for index in range(759, 769)),
}

IFU_CACHEABLE_PIPELINE_BIN_IDS = {
    *(f"BIN-{index:03d}" for index in range(801, 824)),
    *(f"BIN-{index:03d}" for index in range(825, 832)),
    *(f"BIN-{index:03d}" for index in range(837, 844)),
    *(f"BIN-{index:03d}" for index in range(844, 854)),
}

ICACHE_MAINPIPE_S2_ECC_BIN_IDS = {
    "BIN-641",
    "BIN-642",
    "BIN-645",
    *(f"BIN-{index:03d}" for index in range(770, 777)),
}

ICACHE_MAPPED_STATUSES = {"MODELED", "PARTIAL", "HIT"}


def test_active_pilot_has_global_unique_identifiers_and_mappings():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )

    summary = validate_pilot_schema(pilot_path)

    assert summary == {"rows": 451, "bin_ids": 451, "mapping_keys": 451, "legacy_ids": 4}


def test_legacy_bpu_ftq_rows_are_unmapped_and_cannot_enter_runtime_model():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        rows = list(csv.DictReader(handle))

    active = [row for row in rows if row["Coverpoint"].strip()]
    legacy_bpu_ftq = [row for row in rows if "旧BPU_FTQ" in row["映射测试点路径"]]

    assert len(active) == 293
    assert {row["Bin_ID"] for row in active} == {
        *(f"BIN-{index:03d}" for index in range(401, 424)),
        *(f"BIN-{index:03d}" for index in range(424, 433)),
        *(f"BIN-{index:03d}" for index in range(501, 542)),
        *(f"BIN-{index:03d}" for index in range(601, 619)),
        *(f"BIN-{index:03d}" for index in range(620, 627)),
        *(f"BIN-{index:03d}" for index in range(628, 632)),
        *(f"BIN-{index:03d}" for index in range(633, 637)),
        "BIN-638",
        "BIN-769",
        *(f"BIN-{index:03d}" for index in range(770, 781)),
        "BIN-641",
        "BIN-642",
        "BIN-645",
        *(f"BIN-{index:03d}" for index in range(674, 676)),
        *(f"BIN-{index:03d}" for index in range(650, 674)),
        *(f"BIN-{index:03d}" for index in range(677, 686)),
        *(f"BIN-{index:03d}" for index in range(686, 717)),
        *(f"BIN-{index:03d}" for index in range(717, 759)),
        *(f"BIN-{index:03d}" for index in range(759, 769)),
        *(f"BIN-{index:03d}" for index in range(801, 824)),
        "BIN-824",
        *(f"BIN-{index:03d}" for index in range(825, 832)),
        *(f"BIN-{index:03d}" for index in range(832, 837)),
        *(f"BIN-{index:03d}" for index in range(837, 844)),
        *(f"BIN-{index:03d}" for index in range(844, 854)),
    }
    assert legacy_bpu_ftq
    assert all(not row["Coverpoint"].strip() for row in legacy_bpu_ftq)


def test_pilot_schema_rejects_duplicate_bin_id(tmp_path):
    pilot_path = tmp_path / "pilot.csv"
    pilot_path.write_text(
        "Bin_ID,Coverage_Group,Coverpoint,Bin_Name,建议试点用例\n"
        "BIN-001,group_a,point_a,bin_a,case_a\n"
        "BIN-001,group_b,point_b,bin_b,case_b\n",
        encoding="utf-8-sig",
    )

    with pytest.raises(ValueError, match="duplicate Bin_ID BIN-001"):
        validate_pilot_schema(pilot_path)


def test_icache_mainpipe_s2_ecc_leaves_are_single_bin_and_match_registry():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )
    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {
            row["Bin_ID"]: row
            for row in csv.DictReader(handle)
            if row["Bin_ID"] in ICACHE_MAINPIPE_S2_ECC_BIN_IDS
        }
    assert set(pilot_rows) == ICACHE_MAINPIPE_S2_ECC_BIN_IDS

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = (
                set(re.findall(r"BIN-\d{3}", row["coverage"]))
                & ICACHE_MAINPIPE_S2_ECC_BIN_IDS
            )
            if not bin_ids:
                continue
            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows, bin_id
            assert all(
                row[column].strip()
                for column in ("Condition", "Checkpoint", "Object")
            )
            pilot = pilot_rows[bin_id]
            assert row["coverage"] == (
                f"covergroup {pilot['Coverage_Group']}, "
                f"coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert row["status"] in ICACHE_MAPPED_STATUSES
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == ICACHE_MAINPIPE_S2_ECC_BIN_IDS


def test_ifu_predecode_and_two_fetch_leaves_are_single_bin_and_actionable():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {row["Bin_ID"]: row for row in csv.DictReader(handle) if row["Bin_ID"] in IFU_FUNCOV_BIN_IDS}

    assert set(pilot_rows) == IFU_FUNCOV_BIN_IDS

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = set(re.findall(r"BIN-\d{3}", row["coverage"])) & IFU_FUNCOV_BIN_IDS
            if not bin_ids:
                continue

            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows, bin_id
            assert all(row[column].strip() for column in ("Condition", "Checkpoint", "Object"))
            assert "构造V3" not in row["Condition"] + row["Checkpoint"]

            pilot = pilot_rows[bin_id]
            expected_coverage = (
                f"covergroup {pilot['Coverage_Group']}, coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert row["coverage"] == expected_coverage
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == IFU_FUNCOV_BIN_IDS


def test_ifu_cacheable_pipeline_leaves_are_single_bin_and_match_registry():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {
            row["Bin_ID"]: row
            for row in csv.DictReader(handle)
            if row["Bin_ID"] in IFU_CACHEABLE_PIPELINE_BIN_IDS
        }
    assert set(pilot_rows) == IFU_CACHEABLE_PIPELINE_BIN_IDS

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = (
                set(re.findall(r"BIN-\d{3}", row["coverage"]))
                & IFU_CACHEABLE_PIPELINE_BIN_IDS
            )
            if not bin_ids:
                continue
            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows, bin_id
            assert all(
                row[column].strip()
                for column in ("Condition", "Checkpoint", "Object")
            )

            pilot = pilot_rows[bin_id]
            expected_coverage = (
                f"covergroup {pilot['Coverage_Group']}, coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert row["coverage"] == expected_coverage
            assert row["status"] in ICACHE_MAPPED_STATUSES
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == IFU_CACHEABLE_PIPELINE_BIN_IDS


def test_icache_prefetch_leaves_are_single_bin_and_match_registry():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {
            row["Bin_ID"]: row
            for row in csv.DictReader(handle)
            if row["Bin_ID"] in ICACHE_PREFETCH_FUNCOV_BIN_IDS
        }
    assert set(pilot_rows) == ICACHE_PREFETCH_FUNCOV_BIN_IDS

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = (
                set(re.findall(r"BIN-\d{3}", row["coverage"]))
                & ICACHE_PREFETCH_FUNCOV_BIN_IDS
            )
            if not bin_ids:
                continue
            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows, bin_id
            assert all(
                row[column].strip()
                for column in ("Condition", "Checkpoint", "Object")
            )

            pilot = pilot_rows[bin_id]
            expected_coverage = (
                f"covergroup {pilot['Coverage_Group']}, coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert row["coverage"] == expected_coverage
            assert row["status"] in ICACHE_MAPPED_STATUSES
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == ICACHE_PREFETCH_FUNCOV_BIN_IDS


def test_icache_missunit_leaves_are_single_bin_and_match_registry():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {
            row["Bin_ID"]: row
            for row in csv.DictReader(handle)
            if row["Bin_ID"] in ICACHE_MISSUNIT_FUNCOV_BIN_IDS
        }
    assert set(pilot_rows) == ICACHE_MISSUNIT_FUNCOV_BIN_IDS

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = set(re.findall(r"BIN-\d{3}", row["coverage"])) & ICACHE_MISSUNIT_FUNCOV_BIN_IDS
            if not bin_ids:
                continue
            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows, bin_id
            assert all(row[column].strip() for column in ("Condition", "Checkpoint", "Object"))
            pilot = pilot_rows[bin_id]
            expected_coverage = (
                f"covergroup {pilot['Coverage_Group']}, coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert row["coverage"] == expected_coverage
            assert row["status"] in ICACHE_MAPPED_STATUSES
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == ICACHE_MISSUNIT_FUNCOV_BIN_IDS


def test_icache_waylookup_leaves_are_single_bin_and_match_registry():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {
            row["Bin_ID"]: row
            for row in csv.DictReader(handle)
            if row["Bin_ID"] in ICACHE_WAYLOOKUP_FUNCOV_BIN_IDS
        }
    assert set(pilot_rows) == ICACHE_WAYLOOKUP_FUNCOV_BIN_IDS

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = set(re.findall(r"BIN-\d{3}", row["coverage"])) & ICACHE_WAYLOOKUP_FUNCOV_BIN_IDS
            if not bin_ids:
                continue
            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows, bin_id
            assert all(row[column].strip() for column in ("Condition", "Checkpoint", "Object"))
            pilot = pilot_rows[bin_id]
            expected_coverage = (
                f"covergroup {pilot['Coverage_Group']}, coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert row["coverage"] == expected_coverage
            assert row["status"] in ICACHE_MAPPED_STATUSES
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == ICACHE_WAYLOOKUP_FUNCOV_BIN_IDS


def test_icache_hitmiss_leaves_are_single_bin_and_match_registry():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {
            row["Bin_ID"]: row
            for row in csv.DictReader(handle)
            if row["Bin_ID"] in ICACHE_HITMISS_FUNCOV_BIN_IDS
        }
    assert set(pilot_rows) == ICACHE_HITMISS_FUNCOV_BIN_IDS

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = set(re.findall(r"BIN-\d{3}", row["coverage"])) & ICACHE_HITMISS_FUNCOV_BIN_IDS
            if not bin_ids:
                continue
            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows, bin_id
            assert all(row[column].strip() for column in ("Condition", "Checkpoint", "Object"))
            pilot = pilot_rows[bin_id]
            expected_coverage = (
                f"covergroup {pilot['Coverage_Group']}, coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert row["coverage"] == expected_coverage
            assert row["status"] in ICACHE_MAPPED_STATUSES
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == ICACHE_HITMISS_FUNCOV_BIN_IDS
