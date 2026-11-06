"""Reject indirect metadata that cannot establish an exact native probe."""

import pytest

from tools.audit_bpu_ten_slot_contract import resolve_native


@pytest.mark.parametrize("middle", [
    {"kind": "direct", "source": "leaf", "rtl_width": "16"},
    {"kind": "projection", "source": "leaf", "rtl_width": "1", "expr": "leaf ^ 1"},
    {"kind": "unknown", "source": "leaf", "rtl_width": "1"},
    {"kind": "direct", "source": "leaf"},
])
def test_native_alias_cannot_hide_invalid_intermediate_metadata(middle):
    records = {
        "alias": {"kind": "direct", "source": "middle", "rtl_width": "1"},
        "middle": middle,
        "leaf": {"offset": "10", "rtl_width": "1"},
    }
    assert "missing_native_probe" in resolve_native(records, ("alias",), 1)


@pytest.mark.parametrize("leaf_patch", [
    {"kind": "unknown"}, {"offset": "-1"}, {"offset": "unavailable"},
])
def test_native_terminal_requires_known_kind_and_nonnegative_offset(leaf_patch):
    records = {"leaf": {"offset": "10", "rtl_width": "1", **leaf_patch}}
    assert "missing_native_probe" in resolve_native(records, ("leaf",), 1)


def test_native_direct_alias_chain_retains_exact_width_and_terminal_offset():
    records = {
        "alias": {"kind": "direct", "source": "middle", "rtl_width": "1"},
        "middle": {"kind": "direct", "source": "leaf", "rtl_width": "1"},
        "leaf": {"offset": "10", "rtl_width": "1"},
    }
    assert resolve_native(records, ("alias",), 1) == {
        "path": "alias", "source": "leaf", "width": 1, "offset": "10",
    }


@pytest.mark.parametrize("source", ["alias", "absent", "__Vtogcov__leaf"])
def test_native_alias_rejects_cycles_missing_leaves_and_toggle_sources(source):
    records = {
        "alias": {"kind": "direct", "source": "middle", "rtl_width": "1"},
        "middle": {"kind": "direct", "source": source, "rtl_width": "1"},
        "__Vtogcov__leaf": {"offset": "10", "rtl_width": "1"},
    }
    assert "missing_native_probe" in resolve_native(records, ("alias",), 1)


def test_invalid_candidate_does_not_hide_later_exact_native_probe():
    records = {
        "bad": {"kind": "expr", "source": "good", "rtl_width": "1"},
        "good": {"offset": "20", "rtl_width": "1"},
    }
    assert resolve_native(records, ("bad", "good"), 1)["path"] == "good"


@pytest.mark.parametrize("leaf_patch", [
    {"kind": "unknown"}, {"offset": "-1"}, {"offset": "unavailable"},
])
def test_native_reduction_requires_a_valid_native_sixteen_bit_leaf(leaf_patch):
    records = {
        "reduction": {"kind": "projection", "source": "state", "rtl_width": "1",
                      "expr": "0U != (IData)(state)"},
        "state": {"offset": "10", "rtl_width": "16", **leaf_patch},
    }
    assert "missing_native_probe" in resolve_native(records, ("reduction",), 1)
