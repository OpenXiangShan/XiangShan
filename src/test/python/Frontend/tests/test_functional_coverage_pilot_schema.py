from pathlib import Path

import pytest

from tools.backannotate_funcov import validate_pilot_schema


def test_active_pilot_has_global_unique_identifiers_and_mappings():
    repo_root = Path(__file__).resolve().parents[5]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_功能覆盖率建模/frontend_bt_functional_coverage_pilot.csv"
    )

    summary = validate_pilot_schema(pilot_path)

    assert summary == {"rows": 222, "bin_ids": 222, "mapping_keys": 222, "legacy_ids": 4}


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
