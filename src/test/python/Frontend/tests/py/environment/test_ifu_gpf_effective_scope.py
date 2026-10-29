"""GPF witness must belong to a valid accepted entry in this cycle, offline only."""
import pytest
from env.funcov.py.ifu.compact_funcov import _sample_exception_metadata
from tests.py.jiabowen.test_ifu_compact_functional_coverage import _PREFIX, _make_recorder, _set_ifu_output


def _valid_gpf(dut, block=1, lane=0):
    _set_ifu_output(dut, [(lane, 0x80002000, 0x13, 0, 0, 0, 8, 1)],
                    exception_type=2, gp_addr_mem_wen=1, gp_addr_mem_waddr=8,
                    gp_addr=0x90602000, is_for_vs_nonleaf_pte=1)
    for i in range(2):
        for field, value in {"valid": 1, "ftqIdx_flag": 0, "ftqIdx_value": 8 if i == block else 7,
                             "startVAddr_addr": 0x80002000 >> 1}.items():
            dut.set(_PREFIX + f"s2_fetchBlock_{i}_{field}", value)
        for field, value in {"exception_value": 2 if i == block else 0,
                             "gpAddr_addr": 0x90602000 >> 1, "isForVSnonLeafPTE": 1,
                             "isBackendException": 0, "hasSatpFlush": 0}.items():
            dut.set(_PREFIX + f"s2_icacheMeta_{i}_{field}", value)
    dut.set(_PREFIX + f"s2_alignedInstrVec_{lane}_blockSel", block)
    dut.set(_PREFIX + f"s2_alignedInstrVec_{lane}_isCrossBlockInstr", 0)


@pytest.mark.parametrize("stem,value", [
    ("s2_fetchBlock_1_valid", 0),
    ("io_toIBuffer_bits_valid", 0),
    ("io_toIBuffer_bits_exceptionMask", 0),
    ("io_toIBuffer_bits_exceptionMask", 1 << 16),
    ("io_toIBuffer_bits_enqEnable", 2),
    ("io_toIBuffer_bits_ftqPtr_0_flag", 1),
    ("io_toIBuffer_bits_ftqPtr_0_value", 7),
    ("s2_alignedInstrVec_0_blockSel", 0),
    ("s2_alignedInstrVec_0_isCrossBlockInstr", 2),
])
def test_invalid_or_unowned_block1_payload_never_publishes_gpf(tmp_path, stem, value):
    hub, _env, dut, _ = _make_recorder(tmp_path)
    _valid_gpf(dut)
    dut.set(_PREFIX + stem, value)
    _sample_exception_metadata(hub, dut, 20)
    assert not getattr(hub, "_ifu_exception_metadata_witnesses", {})
    assert not hub.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_056")


@pytest.mark.parametrize("mode", ["absent", "unreadable"])
@pytest.mark.parametrize("stem", [
    "s2_fetchBlock_1_valid", "io_toIBuffer_bits_valid", "io_toIBuffer_bits_exceptionMask",
    "s2_fetchBlock_1_startVAddr_addr",
    "s2_alignedInstrVec_0_blockSel", "s2_alignedInstrVec_0_isCrossBlockInstr",
    "io_toIBuffer_bits_ftqPtr_0_flag", "io_toIBuffer_bits_ftqPtr_0_value",
])
def test_missing_gpf_scope_probe_fails_closed_and_records_gap(tmp_path, stem, mode):
    hub, _env, dut, _ = _make_recorder(tmp_path)
    _valid_gpf(dut)
    if mode == "absent":
        delattr(dut, _PREFIX + stem)
    else:
        getattr(dut, _PREFIX + stem).value = None
    _sample_exception_metadata(hub, dut, 20)
    assert not getattr(hub, "_ifu_exception_metadata_witnesses", {})
    assert ("gpaddr", stem) in getattr(hub, "_ifu_exception_metadata_gaps", set())


@pytest.mark.parametrize("block", [0, 1])
@pytest.mark.parametrize("lane", [0, 16, 35])
def test_gpf_witness_proves_effective_entry_source_and_ftq(tmp_path, block, lane):
    hub, _env, dut, _ = _make_recorder(tmp_path)
    _valid_gpf(dut, block, lane)
    _sample_exception_metadata(hub, dut, 22)
    witness = hub.toffee_sink._ifu_exception_metadata_witnesses["gpaddr"]
    assert witness["gpaddr_source_block"] == block
    assert witness["exception_lane"] == lane
    assert witness["effective_exception_mask"] == 1 << lane
    assert witness["ftq"] == witness["delivered_ftq"] == [0, 8]
    assert witness["selected_block_valid"] is True
    assert f"s2_fetchBlock_{block}_valid" in witness["signal_paths"]
    assert f"io_toIBuffer_bits_ftqPtr_{lane}_flag" in witness["signal_paths"]
    assert not hub.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_056")


def test_cross_block_exception_entry_belongs_to_second_block(tmp_path):
    hub, _env, dut, _ = _make_recorder(tmp_path)
    _valid_gpf(dut)
    dut.set(_PREFIX + "s2_alignedInstrVec_0_blockSel", 0)
    dut.set(_PREFIX + "s2_alignedInstrVec_0_isCrossBlockInstr", 1)
    _sample_exception_metadata(hub, dut, 22)
    assert hub.toffee_sink._ifu_exception_metadata_witnesses["gpaddr"]["gpaddr_source_block"] == 1


@pytest.mark.parametrize("change", ["invalid0", "invalid1", "wrong-flag", "wrong-selector",
                                    "multibit-mask", "invalid-enq", "out-of-width-mask"])
def test_block0_is_subject_to_same_scope_and_never_borrows_block1(tmp_path, change):
    hub, _env, dut, _ = _make_recorder(tmp_path)
    _valid_gpf(dut, 0)
    changes = {
        "invalid0": {"s2_fetchBlock_0_valid": 0},
        "invalid1": {"s2_icacheMeta_0_exception_value": 0, "s2_icacheMeta_1_exception_value": 2,
                     "s2_fetchBlock_1_valid": 0},
        "wrong-flag": {"io_toIBuffer_bits_ftqPtr_0_flag": 1},
        "wrong-selector": {"s2_alignedInstrVec_0_blockSel": 1},
        "multibit-mask": {"io_toIBuffer_bits_exceptionMask": 3},
        "invalid-enq": {"io_toIBuffer_bits_enqEnable": 3},
        "out-of-width-mask": {"io_toIBuffer_bits_exceptionMask": 1 << 36},
    }
    for stem, value in changes[change].items():
        dut.set(_PREFIX + stem, value)
    _sample_exception_metadata(hub, dut, 22)
    assert not getattr(hub, "_ifu_exception_metadata_witnesses", {})


def test_invalid_priority0_does_not_fall_back_to_valid_block1(tmp_path):
    hub, _env, dut, _ = _make_recorder(tmp_path)
    _valid_gpf(dut, 1)
    dut.set(_PREFIX + "s2_icacheMeta_0_exception_value", 2)
    dut.set(_PREFIX + "s2_fetchBlock_0_valid", 0)
    _sample_exception_metadata(hub, dut, 22)
    assert not getattr(hub, "_ifu_exception_metadata_witnesses", {})


def test_no_cross_cycle_join_of_effective_entry_and_gpa_payload(tmp_path):
    hub, _env, dut, _ = _make_recorder(tmp_path)
    _valid_gpf(dut)
    dut.set(_PREFIX + "io_toIBuffer_bits_exceptionMask", 1 << 16)
    _sample_exception_metadata(hub, dut, 22)
    dut.set(_PREFIX + "io_toIBuffer_bits_exceptionMask", 1)
    dut.set(_PREFIX + "io_toBackend_gpAddrMem_wdata_gpaddr", 0xDEAD0000)
    _sample_exception_metadata(hub, dut, 23)
    assert not getattr(hub, "_ifu_exception_metadata_witnesses", {})


@pytest.mark.parametrize("block", [0, 1])
@pytest.mark.parametrize("field", ["block-valid", "exception-mask", "entry-valid", "ftq-flag", "selector"])
def test_native_sink_cannot_publish_inactive_or_unowned_gpf(tmp_path, block, field):
    from tests.py.environment.test_ifu_metadata_native_visibility import _runtime, _cycle
    hub, _env, dut, model = _runtime(tmp_path)
    _valid_gpf(dut, block)
    changes = {
        "block-valid": (f"s2_fetchBlock_{block}_valid", 0),
        "exception-mask": ("io_toIBuffer_bits_exceptionMask", 1 << 16),
        "entry-valid": ("io_toIBuffer_bits_valid", 0),
        "ftq-flag": ("io_toIBuffer_bits_ftqPtr_0_flag", 1),
        "selector": ("s2_alignedInstrVec_0_blockSel", 1 - block),
    }
    stem, value = changes[field]
    dut.set(_PREFIX + stem, value)
    _cycle(hub, dut, model, 27)
    assert not getattr(hub.toffee_sink, "_ifu_exception_metadata_witnesses", {})
    assert not hub.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_056")


@pytest.mark.parametrize("invalid", [None, "block-valid", "exception-mask", "ftq-flag"])
def test_gpf_diagnostic_uses_same_effective_scope(tmp_path, monkeypatch, invalid):
    from tests.py.jiabowen import test_ifu_exception_metadata_v3_dut as diagnostic
    hub, _env, dut, _ = _make_recorder(tmp_path)
    _valid_gpf(dut, 1)
    def read(stem):
        return int(getattr(dut, _PREFIX + stem).value)
    stems = (
        "io_toIBuffer_valid", "io_toIBuffer_ready", "io_toIBuffer_bits_valid",
        "io_toIBuffer_bits_enqEnable", "io_toIBuffer_bits_exceptionMask",
        "io_toIBuffer_bits_exceptionType_value", "s2_icacheMeta_0_exception_value",
        "s2_icacheMeta_1_exception_value", "s2_fetchBlock_1_valid",
        "s2_fetchBlock_1_ftqIdx_flag", "s2_fetchBlock_1_ftqIdx_value",
        "s2_fetchBlock_1_startVAddr_addr", "s2_icacheMeta_1_gpAddr_addr",
        "s2_icacheMeta_1_isForVSnonLeafPTE", "s2_alignedInstrVec_0_blockSel",
        "s2_alignedInstrVec_0_isCrossBlockInstr", "io_toIBuffer_bits_ftqPtr_0_flag",
        "io_toIBuffer_bits_ftqPtr_0_value",
    )
    record = {stem: read(stem) for stem in stems}
    record["gpaddr"] = {"wen": 1, "waddr": 8, "addr": diagnostic.gstage._GPA, "nonleaf": 1}
    record["s2_icacheMeta_1_gpAddr_addr"] = diagnostic.gstage._GPA >> 1
    record["s2_fetchBlock_1_startVAddr_addr"] = diagnostic.gstage._VA >> 1
    if invalid == "block-valid":
        record["s2_fetchBlock_1_valid"] = 0
    elif invalid == "exception-mask":
        record["io_toIBuffer_bits_exceptionMask"] = 1 << 16
    elif invalid == "ftq-flag":
        record["io_toIBuffer_bits_ftqPtr_0_flag"] = 1
    monkeypatch.setattr(diagnostic, "_capture_metadata", lambda env: [record])
    monkeypatch.setattr(diagnostic.gstage, "_scenario", lambda *args, **kwargs: object())
    monkeypatch.setattr(diagnostic.gstage, "test_address_translation_gstage_provenance", lambda *args: None)
    saved = []
    monkeypatch.setattr(diagnostic, "_save_review", lambda *args: saved.append(args[-1]))
    if invalid:
        with pytest.raises(AssertionError, match="no accepted effective GPF metadata"):
            diagnostic.test_ifu_exception_metadata_vs_nonleaf_gpf_diagnostic(object())
        assert not saved
    else:
        diagnostic.test_ifu_exception_metadata_vs_nonleaf_gpf_diagnostic(object())
        assert saved == ["vs_nonleaf_gpf"]
        assert record["effective_gpf_scope"]["selected_block_valid"]


def test_frozen_inventory_gaps_remain_explicit_and_fail_closed(tmp_path):
    from pathlib import Path
    from env.funcov.py.ifu.compact_funcov import _IFU_INTERNAL_PREFIXES
    root = Path(__file__).resolve().parents[7]
    names = {line[len("  - name: "):].strip() for line in
             (root / "build-frontend/pylib-verilator/Frontend/Frontend_offset.yaml").read_text().splitlines()
             if line.startswith("  - name: ")}
    stems = {"io_toIBuffer_valid", "io_toIBuffer_ready", "io_toIBuffer_bits_valid",
             "io_toIBuffer_bits_enqEnable", "io_toIBuffer_bits_exceptionMask",
             "io_toIBuffer_bits_exceptionType_value", "io_toBackend_gpAddrMem_wen",
             "io_toBackend_gpAddrMem_waddr", "io_toBackend_gpAddrMem_wdata_gpaddr",
             "io_toBackend_gpAddrMem_wdata_isForVSnonLeafPTE"}
    for block in range(2):
        stems.update(f"s2_fetchBlock_{block}_{field}" for field in
                     ("valid", "ftqIdx_flag", "ftqIdx_value", "startVAddr_addr"))
        stems.update(f"s2_icacheMeta_{block}_{field}" for field in
                     ("exception_value", "gpAddr_addr", "isForVSnonLeafPTE"))
    for lane in range(36):
        stems.update((f"s2_alignedInstrVec_{lane}_blockSel",
                      f"s2_alignedInstrVec_{lane}_isCrossBlockInstr",
                      f"io_toIBuffer_bits_ftqPtr_{lane}_flag",
                      f"io_toIBuffer_bits_ftqPtr_{lane}_value"))
    def candidates(stem):
        paths = [prefix + stem for prefix in _IFU_INTERNAL_PREFIXES]
        if stem.startswith("io_toBackend_gpAddrMem_"):
            suffix = stem.removeprefix("io_toBackend_gpAddrMem_")
            paths.extend((f"Frontend_top.io_backend_fromIfu_gpAddrMem_{suffix}",
                          f"Frontend_top.__Vtogcov__io_backend_fromIfu_gpAddrMem_{suffix}"))
        return paths
    missing = sorted(stem for stem in stems if not any(path in names for path in candidates(stem)))
    assert missing == ["s2_alignedInstrVec_34_isCrossBlockInstr",
                       "s2_alignedInstrVec_35_blockSel", "s2_alignedInstrVec_35_isCrossBlockInstr"]
    # Identify and preserve real absent inventory. Synthetic high-lane positives
    # above exercise rules only; they cannot qualify runtime readability.
    for lane, stem in ((34, missing[0]), (35, missing[1])):
        hub, _env, dut, _ = _make_recorder(tmp_path / str(lane))
        _valid_gpf(dut, 0, lane)
        delattr(dut, _PREFIX + stem)
        _sample_exception_metadata(hub, dut, 30)
        assert ("gpaddr", stem) in hub.toffee_sink._ifu_exception_metadata_gaps
        assert not getattr(hub.toffee_sink, "_ifu_exception_metadata_witnesses", {})
