from __future__ import annotations

import builtins

import pytest

from env.dut_factory import FakeDUTFrontend, create_frontend_dut
from env.memory_model import MemoryModel
from env.model.page_table_model import PageTableModel
from env.monitors.frontend_monitor import FrontendMonitor


def test_monitor_reads_expected_instr_through_sv39_translation() -> None:
    memory = MemoryModel()
    page_table = PageTableModel(mode="sv39")
    monitor = FrontendMonitor(memory=memory, page_table=page_table)

    virt_base = 0x80000000
    phys_base = 0x90000000
    nop = 0x00000013

    page_table.map_page(virt_base >> 12, phys_base >> 12, v=1, x=1, r=1)
    memory.write_u32(phys_base, nop)

    raw_fetch, meta = monitor._read_expected_fetch_raw(virt_base, 4)

    assert raw_fetch == nop
    assert meta["ok"] is True
    assert meta["mode"] == "sv39"
    assert meta["pa"] == phys_base + 3


def test_monitor_skips_zero_instr_compare_for_sv39_translation_aware_gap() -> None:
    fetch_meta = {"ok": True, "mode": "sv39", "pa": 0x90000000}

    assert FrontendMonitor._should_skip_instr_compare(fetch_meta, got=0, ex_sum=0) is True
    assert FrontendMonitor._should_skip_instr_compare(fetch_meta, got=0x13, ex_sum=0) is False
    assert FrontendMonitor._should_skip_instr_compare(fetch_meta, got=0, ex_sum=1) is False
    assert FrontendMonitor._should_skip_instr_compare({"ok": True, "mode": "bare"}, got=0, ex_sum=0) is False


def test_create_frontend_dut_raises_when_real_dut_is_required_but_missing(monkeypatch: pytest.MonkeyPatch) -> None:
    real_import = builtins.__import__

    def _fake_import(name, globals=None, locals=None, fromlist=(), level=0):
        if name == "Frontend":
            raise ModuleNotFoundError("No module named 'Frontend'", name="Frontend")
        return real_import(name, globals, locals, fromlist, level)

    monkeypatch.setenv("TB_ENABLE_DUT_TESTS", "1")
    monkeypatch.setattr(builtins, "__import__", _fake_import)

    with pytest.raises(RuntimeError, match="run `make frontend`"):
        create_frontend_dut("unit")


def test_create_frontend_dut_falls_back_to_fake_when_dut_tests_are_not_required(monkeypatch: pytest.MonkeyPatch) -> None:
    real_import = builtins.__import__

    def _fake_import(name, globals=None, locals=None, fromlist=(), level=0):
        if name == "Frontend":
            raise ModuleNotFoundError("No module named 'Frontend'", name="Frontend")
        return real_import(name, globals, locals, fromlist, level)

    monkeypatch.delenv("TB_ENABLE_DUT_TESTS", raising=False)
    monkeypatch.setattr(builtins, "__import__", _fake_import)

    dut = create_frontend_dut("unit")

    assert isinstance(dut, FakeDUTFrontend)
    assert dut._frontend_is_fake_dut is True
