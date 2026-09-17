"""BIN-954 legal component accumulation and current-signal negative contracts."""

from pathlib import Path

import pytest

from env.funcov.py.ifu.compact_funcov import _sample_exception_metadata
from tests.py.jiabowen.test_ifu_compact_functional_coverage import _PREFIX, _make_recorder, _set_ifu_output


_GROUP, _BIN = "ifu_v3_pipeline_owner_model", "owner_leaf_056"
_EXPECTED = {
    "satp": {"satp_flush"}, "backend": {"backend_exception"},
    "cross": {"cross_page"}, "gpf": {"gpaddr", "vs_nonleaf_pte"},
}


def _component(dut, name):
    exception = {"satp": 0, "backend": 1, "cross": 1, "gpf": 2}[name]
    _set_ifu_output(
        dut, [(0, 0x80000FFE if name == "cross" else 0x80001000, 0x13, 0, 0, 1, 7, int(exception != 0))],
        exception_type=exception, is_backend_exception=int(name == "backend"),
        has_satp_flush=int(name == "satp"), exception_cross_page=int(name == "cross"),
        gp_addr_mem_wen=int(name == "gpf"), gp_addr_mem_waddr=7,
        gp_addr=0x80601000, is_for_vs_nonleaf_pte=int(name == "gpf"),
        s2_prev_end_is_half_rvi=int(name == "cross"), s2_prev_end_half_pc=0x80000FFE,
        s2_prev_end_half_data=0x13, s2_fetch_block_start_pc=0x80001000,
    )
    dut.set(_PREFIX + "s2_icacheMeta_0_exception_value", exception)
    dut.set(_PREFIX + "s2_fetchBlock_0_ftqIdx_flag", 1)


@pytest.mark.parametrize("component", _EXPECTED)
def test_each_legal_component_is_partial_not_whole_bin(tmp_path, component):
    recorder, _env, dut, _ = _make_recorder(tmp_path)
    _component(dut, component)
    _sample_exception_metadata(recorder, dut, 10)
    assert recorder._ifu_exception_metadata_checks == _EXPECTED[component]
    assert not recorder.key_hit(_GROUP, _BIN)
    for witness in recorder._ifu_exception_metadata_witnesses.values():
        assert witness["cycle"] == 10 and witness["ftq"] == [1, 7]
        assert witness["signal_paths"] and witness["ibuffer_delivery"]


def test_all_components_require_complete_same_run_witnesses(tmp_path):
    recorder, _env, dut, _ = _make_recorder(tmp_path)
    for cycle, component in enumerate(_EXPECTED, 10):
        _component(dut, component)
        _sample_exception_metadata(recorder, dut, cycle)
        assert recorder.key_hit(_GROUP, _BIN) == (component == "gpf")
        # A reset does not erase already completed coverage witnesses. There
        # is no pending half-transaction to stitch across this reset.
        recorder._clear_transient_sampling_state()
    assert {w["cycle"] for w in recorder._ifu_exception_metadata_witnesses.values()} == {10, 11, 12, 13}
    independent, _env2, _dut2, _ = _make_recorder(tmp_path / "independent")
    assert not getattr(independent, "_ifu_exception_metadata_witnesses", {})
    assert not independent.key_hit(_GROUP, _BIN)


@pytest.mark.parametrize("stem,value", [
    ("io_toIBuffer_valid", 0), ("io_toIBuffer_ready", 0), ("io_toIBuffer_bits_enqEnable", 0),
])
def test_non_delivery_does_not_accumulate(tmp_path, stem, value):
    recorder, _env, dut, _ = _make_recorder(tmp_path)
    _component(dut, "gpf")
    dut.set(_PREFIX + stem, value)
    _sample_exception_metadata(recorder, dut, 10)
    assert not getattr(recorder, "_ifu_exception_metadata_checks", set())


@pytest.mark.parametrize("component,stem,value", [
    ("satp", "s2_icacheMeta_0_hasSatpFlush", 0),
    ("satp", "io_toIBuffer_bits_hasSatpFlush", 0),
    ("backend", "s2_icacheMeta_0_isBackendException", 0),
    ("backend", "io_toIBuffer_bits_isBackendException", 0),
    ("backend", "s2_icacheMeta_0_exception_value", 0),
    ("backend", "io_toIBuffer_bits_exceptionType_value", 3),
    ("cross", "s2_prevEndIsHalfRviInfo_valid", 0),
    ("cross", "io_toIBuffer_bits_exceptionCrossPage", 0),
    ("cross", "s2_prevEndIsHalfRviInfo_bits_pc_addr", 0x8000001E >> 1),
    ("cross", "s2_prevEndIsHalfRviInfo_bits_data", 0x1),
    ("cross", "s2_fetchBlock_0_startVAddr_addr", 0x80002000 >> 1),
    ("cross", "s2_icacheMeta_0_exception_value", 0),
    ("gpf", "s2_icacheMeta_0_exception_value", 3),
    ("gpf", "io_toIBuffer_bits_exceptionType_value", 5),
    ("gpf", "io_toBackend_gpAddrMem_wen", 0),
    ("gpf", "io_toBackend_gpAddrMem_waddr", 8),
    ("gpf", "io_toBackend_gpAddrMem_wdata_gpaddr", 0x80601002),
    ("gpf", "s2_icacheMeta_0_isForVSnonLeafPTE", 0),
])
def test_wrong_metadata_or_identity_does_not_accumulate(tmp_path, component, stem, value):
    recorder, _env, dut, _ = _make_recorder(tmp_path)
    _component(dut, component)
    dut.set(_PREFIX + stem, value)
    _sample_exception_metadata(recorder, dut, 10)
    assert not getattr(recorder, "_ifu_exception_metadata_checks", set())
    assert not recorder.key_hit(_GROUP, _BIN)


@pytest.mark.parametrize("mode", ["absent", "unreadable"])
@pytest.mark.parametrize("component,stem", [
    ("satp", "s2_icacheMeta_0_hasSatpFlush"),
    ("satp", "io_toIBuffer_bits_hasSatpFlush"),
    ("backend", "io_toIBuffer_bits_isBackendException"),
    ("cross", "s2_prevEndIsHalfRviInfo_bits_pc_addr"),
    ("cross", "s2_prevEndIsHalfRviInfo_bits_data"),
    ("gpf", "io_toBackend_gpAddrMem_wdata_gpaddr"),
    ("gpf", "s2_icacheMeta_0_isForVSnonLeafPTE"),
    ("gpf", "s2_fetchBlock_0_ftqIdx_value"),
    ("gpf", "s2_icacheMeta_0_exception_value"),
])
def test_missing_probe_is_visible_and_never_defaults_to_hit(tmp_path, component, stem, mode):
    recorder, _env, dut, _ = _make_recorder(tmp_path)
    _component(dut, component)
    if mode == "absent":
        delattr(dut, _PREFIX + stem)
    else:
        getattr(dut, _PREFIX + stem).value = None
    _sample_exception_metadata(recorder, dut, 10)
    assert not getattr(recorder, "_ifu_exception_metadata_checks", set())
    assert not recorder.key_hit(_GROUP, _BIN)
    assert any(r.get("event") == "ifu_exception_metadata_missing_probe" for r in recorder.risk_observations)


def test_irrelevant_gp_probe_does_not_mask_normal_satp_delivery(tmp_path):
    recorder, _env, dut, _ = _make_recorder(tmp_path)
    _component(dut, "satp")
    delattr(dut, _PREFIX + "io_toBackend_gpAddrMem_wdata_gpaddr")
    _sample_exception_metadata(recorder, dut, 10)
    assert recorder._ifu_exception_metadata_checks == {"satp_flush"}


def test_leaf_pte_gpf_does_not_cover_nonleaf_requirement(tmp_path):
    recorder, _env, dut, _ = _make_recorder(tmp_path)
    _component(dut, "gpf")
    dut.set(_PREFIX + "io_toBackend_gpAddrMem_wdata_isForVSnonLeafPTE", 0)
    dut.set(_PREFIX + "s2_icacheMeta_0_isForVSnonLeafPTE", 0)
    _sample_exception_metadata(recorder, dut, 10)
    assert recorder._ifu_exception_metadata_checks == {"gpaddr"}


def test_mismatched_gp_fragments_cannot_join_across_cycles(tmp_path):
    recorder, _env, dut, _ = _make_recorder(tmp_path)
    _component(dut, "gpf")
    dut.set(_PREFIX + "io_toBackend_gpAddrMem_wdata_gpaddr", 0xDEAD0000)
    _sample_exception_metadata(recorder, dut, 10)
    dut.set(_PREFIX + "io_toBackend_gpAddrMem_wdata_gpaddr", 0x80601000)
    dut.set(_PREFIX + "io_toBackend_gpAddrMem_waddr", 8)
    _sample_exception_metadata(recorder, dut, 11)
    assert not getattr(recorder, "_ifu_exception_metadata_witnesses", {})


def test_missing_probe_recovers_only_with_observed_value(tmp_path):
    recorder, _env, dut, _ = _make_recorder(tmp_path)
    _component(dut, "satp")
    signal = getattr(dut, _PREFIX + "io_toIBuffer_bits_hasSatpFlush")
    signal.value = None
    _sample_exception_metadata(recorder, dut, 10)
    assert not getattr(recorder, "_ifu_exception_metadata_witnesses", {})
    signal.value = 1
    _sample_exception_metadata(recorder, dut, 11)
    assert recorder._ifu_exception_metadata_witnesses["satp_flush"]["cycle"] == 11


def test_diagnostic_witness_and_gap_survive_bounded_risk_tail(tmp_path):
    recorder, _env, dut, _ = _make_recorder(tmp_path)
    _component(dut, "satp")
    _sample_exception_metadata(recorder, dut, 10)
    _component(dut, "gpf")
    delattr(dut, _PREFIX + "io_toBackend_gpAddrMem_wdata_gpaddr")
    _sample_exception_metadata(recorder, dut, 11)
    for i in range(160):
        recorder.risk_observations.append({"unrelated": i})
    diagnostic = recorder._raw_dict()["sampler_diagnostics"]["exception_metadata"]
    assert set(diagnostic["component_witnesses"]) == {"satp_flush"}
    assert diagnostic["missing_probes"] == [["gpaddr", "gpAddrMem.wdata_gpaddr"]]


def test_current_rtl_satp_and_gpaddr_contract():
    root = Path(__file__).resolve().parents[7]
    ifu = (root / "src/main/scala/xiangshan/frontend/ifu/Ifu.scala").read_text()
    ftq = (root / "src/main/scala/xiangshan/frontend/ftq/Ftq.scala").read_text()
    emitted = (root / "build-frontend/rtl/Ifu.sv").read_text()
    assert "hasSatpFlush     := backendRedirect.bits.satpFlush" in ftq
    assert "io.toIBuffer.bits.hasSatpFlush       := s2_icacheMeta(0).hasSatpFlush" in ifu
    assert "io.toBackend.gpAddrMem.wen                     := s2_toIBufferValid && s2_icacheMeta(0).exception.isGpf" in ifu
    assert "assign io_toIBuffer_bits_hasSatpFlush = s2_icacheMeta_0_hasSatpFlush;" in emitted
    assert "assign io_toBackend_gpAddrMem_wdata_gpaddr = {s2_icacheMeta_0_gpAddr_addr, 1'h0};" in emitted
