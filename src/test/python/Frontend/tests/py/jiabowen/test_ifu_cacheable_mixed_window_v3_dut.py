"""PMA-only dual-fetch suppression shares the checked source-bound replay."""

from __future__ import annotations

import os
import pytest

from env.funcov.py.ifu.cacheable_pipeline_funcov import _UPSTREAM_SIGNALS, _read_upstream_signal
from tests.py.jiabowen.test_ifu_nc_dual_suppression_v3_dut import _run_dual_suppression_case


def _upstream_snapshot(recorder) -> dict[str, int | None]:
    snapshot: dict[str, int | None] = {}
    for key in _UPSTREAM_SIGNALS:
        value = _read_upstream_signal(recorder, key)
        assert value is not None, {"missing_native_upstream_probe": key}
        snapshot[str(key)] = int(value)
    return snapshot


@pytest.mark.funcov_bins("BIN-904")
@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires real Frontend DUT")
def test_two_fetch_mmio_window_suppresses_second_icache_response(env) -> None:
    """Retain PBMT0 and prove PMA1 independently on the accepted response."""
    _run_dual_suppression_case(env, warm_cacheable=True, attribute_kind="pma")
