"""Current-DUT reset-state contract for V3 IFU pipeline state."""

from __future__ import annotations

import os

import pytest

from env.funcov.py.ifu.compact_funcov import _read_ifu_internal_with_path


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_GROUP = "ifu_v3_pipeline_owner_model"
_BIN = "BIN-953"
_FIELDS = (
    "s0_prevEndIsHalfRvi",
    "s1_prevEndHalfRviInfo_valid",
    "s1_prevEndHalfRviInfo_bits_data",
    "s1_prevEndHalfRviInfo_bits_pc_addr",
    "s1_prevIBufEnqPtrDup_dup_0_value",
    "s1_prevIBufEnqPtrDup_dup_1_value",
    "s1_valid",
    "s2_valid_valid",
)


def _snapshot_reset_state(env) -> tuple[dict[str, int | None], dict[str, str], list[str]]:
    values: dict[str, int | None] = {}
    paths: dict[str, str] = {}
    missing: list[str] = []
    for field in _FIELDS:
        value, path = _read_ifu_internal_with_path(
            env.functional_coverage, env.dut, field
        )
        values[field] = value
        if value is None:
            missing.append(field)
        elif path is not None:
            paths[field] = path
    return values, paths, missing


@pytest.mark.funcov_bins(_BIN)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_ifu_reset_release_clears_pipeline_and_half_state(env) -> None:
    """The fixture's reset release must leave all contract state at zero."""

    values, paths, missing = _snapshot_reset_state(env)
    assert not missing, {"missing": missing, "paths": paths}
    assert all(value == 0 for value in values.values()), values

    definition = env.functional_coverage.definition_by_bin_id[_BIN]
    assert env.functional_coverage.key_hit(
        definition.coverage_group,
        definition.bin_name,
        coverpoint=definition.coverpoint,
    )
    assert not env.monitor.get_errors()
