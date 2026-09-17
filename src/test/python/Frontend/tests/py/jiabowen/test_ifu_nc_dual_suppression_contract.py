"""Independent program and signal-contract checks for the NC canary."""

from pathlib import Path

import pytest

from env.funcov.recorder import CoverageHit, FunctionalCoverageRecorder, default_pilot_csv_path
from tests.py.jiabowen.test_ifu_nc_dual_suppression_v3_dut import (
    _BASE, _candidate_paths, _nc_loop_trace, _second_block_taken_loop,
    _dual_nc_candidate, _check_single_nc_response, _check_target_acceptance,
)


def test_nc_loop_trace_is_independent_and_matches_program():
    payload, trace = _second_block_taken_loop(), _nc_loop_trace(laps=2)
    assert len(trace.entries) == 120
    for i, entry in enumerate(trace.entries):
        offset = entry.pc - _BASE
        assert int.from_bytes(payload[offset:offset + entry.size], "little") == entry.instr
        assert entry.pc >> 12 == _BASE >> 12
        if i + 1 < len(trace.entries):
            assert trace.entries[i + 1].pc == (entry.target_pc if entry.taken else entry.pc + entry.size)


def test_nc_canary_signal_paths_do_not_invent_defaults():
    assert _candidate_paths(("top.mainPipe.foo",)) == (
        "top.mainPipe.foo", "top.mainPipe.__Vtogcov__foo")
    assert _candidate_paths(("top.__Vtogcov__foo",)) == ("top.__Vtogcov__foo", "top.foo")
    assert _candidate_paths(()) == ()


def test_current_rtl_nc_attribute_suppresses_dual_not_waylookup_enqueue():
    root = Path(__file__).resolve().parents[7]
    main = (root / "src/main/scala/xiangshan/frontend/icache/ICacheMainPipe.scala").read_text()
    prefetch = (root / "src/main/scala/xiangshan/frontend/icache/ICachePrefetchPipe.scala").read_text()
    assert "!s0_dataSramReadConflict && !s0_hasMmio && !s0_hasItlbException" in main
    assert "s0_req(1).valid := s0_realTwoFetchValid" in main
    assert "s1_isMmio := s1_pmpMmio || Pbmt.isUncache(s1_itlbPbmt)" in prefetch
    assert "port.bits.entry.isMmio      := s1_isMmio" in prefetch
    assert "(if (i == 0) true.B else s1_twoPrefetchCase.valid)" in prefetch


def test_sfence_scope_bits_mean_source_register_is_zero():
    root = Path(__file__).resolve().parents[7]
    storage = (root / "src/main/scala/xiangshan/cache/mmu/TLBStorage.scala").read_text()
    assert "*.rs1 <- (rs1===0.U)" in storage
    assert "*.rs2 <- (rs2===0.U)" in storage


def test_first_block_valid_uses_current_semantic_object_not_absent_port():
    root = Path(__file__).resolve().parents[7]
    ifu = (root / "build-frontend/rtl/Ifu.sv").read_text()
    assert "wire              s0_fetchBlock_0_valid = 1'h1;" in ifu
    assert "io_fromICache_req_bits_info_0_valid" not in ifu
    assert "io_fromICache_req_bits_info_1_valid" in ifu


def test_software_prefetch_legally_stalls_ftq_ingress():
    root = Path(__file__).resolve().parents[7]
    icache = (root / "src/main/scala/xiangshan/frontend/icache/ICacheImp.scala").read_text()
    assert "io.fromFtq.toPrefetch.ready       := prefetcher.io.fromFtq.ready && !softPrefetchValid" in icache
    assert "Mux(softPrefetchValid, softPrefetch, io.fromFtq.toPrefetch.bits.req(0))" in icache


@pytest.mark.parametrize("missing", [None, "mainpipe_fire", "second_requested", "second_waylookup_valid",
                                    "first_mmio", "second_mmio"])
@pytest.mark.parametrize("value", [None, 0])
def test_dual_nc_requires_actual_ftq_second_request(missing, value):
    snapshot = dict.fromkeys(("mainpipe_fire", "second_requested", "second_waylookup_valid",
                              "first_mmio", "second_mmio"), 1)
    if missing:
        snapshot[missing] = value
    assert _dual_nc_candidate(snapshot) == (missing is None)


@pytest.mark.parametrize("wrong", [None, "valid0", "valid1", "pbmt", "exception", "flag", "index", "start"])
def test_nc_response_must_be_single_and_keep_input_identity(wrong):
    request = dict(first_ftq_flag=0, first_ftq_value=27, first_start=0x40000030)
    response = dict(valid0=1, valid1=0, pbmt=1, exception=0, flag=0, index=27, start=0x40000030)
    if wrong:
        response[wrong] ^= 1
        with pytest.raises(AssertionError):
            _check_single_nc_response(request, response)
    else:
        _check_single_nc_response(request, response)


@pytest.mark.parametrize("fault", [None, "missing_hit", "zero_hit", "serialized_key",
                                  "wrong_cycle", "no_acceptance"])
def test_target_lookup_uses_runtime_key_and_requires_matching_acceptance(tmp_path, fault):
    recorder = FunctionalCoverageRecorder.from_pilot_csv(
        default_pilot_csv_path(), testcase_name="nc_target_lookup_unit",
        artifact_tag="nc_target_lookup_unit", output_dir=tmp_path,
    )
    key = recorder.definition_by_bin_id["BIN-904"].key
    assert isinstance(key, tuple) and len(key) == 3
    checkpoints = [] if fault == "no_acceptance" else [{"cycle": 708}]
    if fault != "missing_hit":
        recorder.hits["::".join(key) if fault == "serialized_key" else key] = CoverageHit(
            hits=0 if fault == "zero_hit" else 1,
            first_cycle=709 if fault == "wrong_cycle" else 708,
        )
    if fault:
        with pytest.raises(AssertionError):
            _check_target_acceptance(recorder, checkpoints)
    else:
        _check_target_acceptance(recorder, checkpoints)
