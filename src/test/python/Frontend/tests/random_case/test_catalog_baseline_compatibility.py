"""Offline selection of current canonical CSV; never copy or alter its content."""
import hashlib

import pytest

from .testpoint_catalog import build_catalog, default_testpoint_source


@pytest.mark.parametrize("current_exists,historical_exists", [(True, True), (True, False), (False, True), (False, False)])
def test_default_csv_uses_current_name_without_losing_older_checkout_support(tmp_path, current_exists, historical_exists):
    directory = tmp_path / "docs" / "02_testpoint"
    directory.mkdir(parents=True)
    current = directory / "Frontend_testpoint_v1.0.csv"
    historical = directory / "Frontend_testpoint_0525_coverage_backannotated.csv"
    if current_exists:
        current.write_text("current canonical file")
    if historical_exists:
        historical.write_text("historical file")
    chosen = default_testpoint_source(tmp_path)
    assert chosen == (historical if historical_exists and not current_exists else current)
    if not current_exists and not historical_exists:
        with pytest.raises(FileNotFoundError):
            build_catalog(chosen)
    elif current_exists:
        assert current.read_text() == "current canonical file"


def test_catalog_hashes_actual_chosen_csv():
    from .testpoint_catalog import DEFAULT_SOURCE
    before = DEFAULT_SOURCE.read_bytes()
    result = build_catalog(DEFAULT_SOURCE)
    assert result["source_sha256"] == hashlib.sha256(before).hexdigest()
    assert DEFAULT_SOURCE.read_bytes() == before


def test_reviewed_testpoint_move_requires_exact_semantics():
    from copy import deepcopy
    from .scenario_testpoint_mapping import _resolve_testpoint_ids
    from .testpoint_catalog import DEFAULT_SOURCE
    catalog = build_catalog(DEFAULT_SOURCE)
    index = {item["testpoint_id"]: item for item in catalog["testpoints"]}
    old_id, new_id = "TP-9E9B3ECAF7B8", "TP-F0A234CD3E54"
    if new_id not in index:
        assert _resolve_testpoint_ids((old_id,), index) == (old_id,)
        return
    assert old_id not in index
    assert _resolve_testpoint_ids((old_id,), index) == (new_id,)
    for field in ("condition", "checkpoint", "object", "coverage"):
        changed = deepcopy(index)
        changed[new_id][field] += " semantic change"
        with pytest.raises(ValueError, match="changed semantics"):
            _resolve_testpoint_ids((old_id,), changed)
    assert _resolve_testpoint_ids((old_id,), {}) == (old_id,)


def test_all_four_family_intents_resolve_without_canonical_edits():
    from .scenario_testpoint_mapping import FAMILY_TESTPOINT_IDS, _resolve_testpoint_ids
    from .testpoint_catalog import DEFAULT_SOURCE
    catalog = build_catalog(DEFAULT_SOURCE)
    index = {item["testpoint_id"]: item for item in catalog["testpoints"]}
    for ids in FAMILY_TESTPOINT_IDS.values():
        resolved = _resolve_testpoint_ids(ids, index)
        assert len(ids) == len(resolved)
        assert all(testpoint_id in index for testpoint_id in resolved)
