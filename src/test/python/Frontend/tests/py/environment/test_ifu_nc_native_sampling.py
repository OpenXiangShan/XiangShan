"""NC dual-fetch observations must use live DUT values, not toggle history."""

from tests.py.jiabowen.test_ifu_nc_dual_suppression_v3_dut import _candidate_paths


def test_nc_response_candidate_resolves_native_alias_and_excludes_toggle_history():
    old = "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_bits_info_1_valid"
    candidates = _candidate_paths((old,))
    assert "inner_icache.io_toIfu_req_bits_info_1_valid" in candidates
    assert all("__Vtogcov__" not in path for path in candidates)
