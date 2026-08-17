from pathlib import Path
from types import SimpleNamespace

import pytest

from env.funcov.py.ifu.cacheable_pipeline_funcov import (
    IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS,
    _SIGNALS,
    sample_ifu_cacheable_pipeline_coverage,
)
from env.funcov.recorder import FunctionalCoverageRecorder, default_pilot_csv_path


_ICACHE_PREFIX = "Frontend_top.Frontend.inner_icache."
_IFU_PREFIX = "Frontend_top.Frontend.inner_ifu."


class _Signal:
    def __init__(self, value=0):
        self.value = int(value)


class _FakeDut:
    def set(self, name, value):
        signal = getattr(self, str(name), None)
        if signal is None:
            signal = _Signal()
            setattr(self, str(name), signal)
        signal.value = int(value)


def _make_recorder(tmp_path):
    dut = _FakeDut()
    env = SimpleNamespace(dut=dut)
    recorder = FunctionalCoverageRecorder.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="ifu_cacheable_pipeline_unit",
        artifact_tag="ifu_cacheable_pipeline_unit",
        output_dir=tmp_path,
    )
    recorder.attach(env)
    for candidates in _SIGNALS.values():
        dut.set(candidates[0], 0)
    return recorder, env, dut


def _set_request(
    dut,
    *,
    valid=1,
    ready=1,
    fire=1,
    flush=0,
    ftq0=(0, 3),
    start0=0x40000000,
    taken0=(0, 7),
    size0=8,
    range0=0xFF,
    second=None,
):
    dut.set(_SIGNALS["req_valid"][0], valid)
    dut.set(_SIGNALS["req_ready"][0], ready)
    dut.set(_SIGNALS["s0_fire"][0], fire)
    dut.set(_SIGNALS["s0_flush"][0], flush)
    blocks = [
        {
            "valid": 1,
            "ftqIdx_flag": ftq0[0],
            "ftqIdx_value": ftq0[1],
            "startVAddr_addr": start0,
            "takenCfiOffset_valid": taken0[0],
            "takenCfiOffset_bits": taken0[1],
            "range": range0,
            "size": size0,
            "data": 0x11223344,
            "maybeRvcMap": 0x55,
        },
        second
        or {
            "valid": 0,
        },
    ]
    for index, block in enumerate(blocks):
        for field, value in block.items():
            if field in {"data", "maybeRvcMap"}:
                name = f"{_ICACHE_PREFIX}mainPipe.io_toIfu_req_bits_{index}_{field}"
            else:
                name = f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_{index}_{field}"
            dut.set(name, value)
    return blocks


def _set_s1(dut, blocks, *, valid=1, ready=1):
    dut.set(_SIGNALS["s1_valid"][0], valid)
    dut.set(_SIGNALS["s1_ready"][0], ready)
    for index, block in enumerate(blocks):
        for field, value in block.items():
            if field in {"data", "maybeRvcMap"}:
                continue
            dut.set(f"{_IFU_PREFIX}s1_fetchBlock_{index}_{field}", value)


def _idle_request(dut):
    dut.set(_SIGNALS["req_valid"][0], 0)
    dut.set(_SIGNALS["req_ready"][0], 1)
    dut.set(_SIGNALS["s0_fire"][0], 0)
    dut.set(_SIGNALS["s0_flush"][0], 0)


def test_cacheable_single_block_transfer_requires_matching_s1_metadata(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    _idle_request(dut)
    _set_s1(dut, blocks)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_cacheable_ingress", "accepted")
    assert recorder.key_hit("ifu_cacheable_window", "single_block")
    assert recorder.key_hit("ifu_cacheable_metadata", "first_ftq_preserved")
    assert recorder.key_hit("ifu_cacheable_metadata", "not_taken_preserved")
    assert not recorder.key_hit("ifu_cacheable_metadata", "second_ftq_preserved")


def test_recorder_on_cycle_invokes_cacheable_pipeline_sampler(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set("reset", 0)
    _set_request(dut)

    recorder.on_cycle(1, env)

    assert recorder.key_hit("ifu_cacheable_ingress", "accepted")


def test_cacheable_dual_block_transfer_preserves_both_ftq_sources(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    second = {
        "valid": 1,
        "ftqIdx_flag": 0,
        "ftqIdx_value": 4,
        "startVAddr_addr": 0x40000010,
        "takenCfiOffset_valid": 1,
        "takenCfiOffset_bits": 5,
        "range": 0x3F,
        "size": 6,
        "data": 0x55667788,
        "maybeRvcMap": 0x33,
    }
    blocks = _set_request(dut, taken0=(1, 7), second=second)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    _idle_request(dut)
    _set_s1(dut, blocks)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_cacheable_window", "dual_block")
    assert recorder.key_hit("ifu_cacheable_metadata", "first_ftq_preserved")
    assert recorder.key_hit("ifu_cacheable_metadata", "second_ftq_preserved")
    assert not recorder.key_hit("ifu_cacheable_metadata", "not_taken_preserved")


def test_cacheable_ingress_payload_must_stay_stable_while_backpressured(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_request(dut, ready=0, fire=0)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_cacheable_ingress", "backpressured")
    assert recorder.key_hit("ifu_cacheable_ingress", "backpressure_payload_stable")

    dut.set(
        f"{_ICACHE_PREFIX}mainPipe.io_toIfu_req_bits_0_data",
        0xDEADBEEF,
    )
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)
    assert any(
        item["event"] == "ifu_cacheable_backpressure_payload_changed"
        for item in recorder.risk_observations
    )


def test_cacheable_s1_metadata_must_stay_stable_while_blocked(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut, valid=0, fire=0)
    _set_s1(dut, blocks, ready=0)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_cacheable_transfer", "s1_payload_stable")


def test_cacheable_back_to_back_and_gapped_transfers_are_distinct(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    first = _set_request(dut, ftq0=(0, 1))
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    second = _set_request(dut, ftq0=(0, 2), start0=0x40000010)
    _set_s1(dut, first)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)
    assert recorder.key_hit("ifu_cacheable_ingress", "back_to_back_accept")

    _idle_request(dut)
    _set_s1(dut, second)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 3)
    dut.set(_SIGNALS["s1_valid"][0], 0)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 4)

    third = _set_request(dut, ftq0=(0, 7), start0=0x40000040)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 5)
    _idle_request(dut)
    _set_s1(dut, third)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 6)
    assert recorder.key_hit("ifu_cacheable_transfer", "gapped_metadata_isolated")


@pytest.mark.parametrize(
    ("cause", "bin_name"),
    [
        ("backend_redirect", "backend_redirect_blocks"),
        ("wb_redirect", "wb_redirect_blocks"),
        ("bpu_match", "bpu_match_blocks"),
    ],
)
def test_cacheable_flush_causes_block_old_s0_return(tmp_path, cause, bin_name):
    recorder, env, dut = _make_recorder(tmp_path / cause)
    _set_request(dut, ready=1, fire=0, flush=1)
    if cause == "bpu_match":
        dut.set(_SIGNALS["bpu_s3_flush"][0], 1)
        dut.set(_SIGNALS["s0_flush_bpu"][0], 1)
    else:
        dut.set(_SIGNALS[cause][0], 1)

    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_cacheable_flush", bin_name)
    assert recorder.key_hit("ifu_cacheable_flush", "flush_wins_fire")
    assert not recorder.key_hit("ifu_cacheable_ingress", "accepted")


def test_cacheable_nonmatching_bpu_flush_allows_s0_fire(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    _set_request(dut)
    dut.set(_SIGNALS["bpu_s3_flush"][0], 1)
    dut.set(_SIGNALS["s0_flush_bpu"][0], 0)

    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_cacheable_flush", "bpu_miss_allows")
    assert recorder.key_hit("ifu_cacheable_ingress", "accepted")


def test_cacheable_metadata_mismatch_is_diagnostic_not_coverage(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    blocks[0]["ftqIdx_value"] += 1
    _idle_request(dut)
    _set_s1(dut, blocks)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert not recorder.key_hit("ifu_cacheable_window", "single_block")
    assert not recorder.key_hit("ifu_cacheable_metadata", "first_ftq_preserved")
    assert any(
        item["event"] == "ifu_cacheable_s1_metadata_mismatch"
        for item in recorder.risk_observations
    )


def test_cacheable_pending_transfer_is_not_credited_when_s1_flushes(tmp_path):
    recorder, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 1)
    _idle_request(dut)
    _set_s1(dut, blocks)
    dut.set(_SIGNALS["s1_flush"][0], 1)
    sample_ifu_cacheable_pipeline_coverage(recorder, env, 2)

    assert not recorder.key_hit("ifu_cacheable_window", "single_block")
    assert not recorder.key_hit("ifu_cacheable_metadata", "first_ftq_preserved")
    assert any(
        item["event"] == "ifu_cacheable_pending_transfer_flushed"
        for item in recorder.risk_observations
    )


def test_cacheable_sampler_signals_match_generated_contract():
    root = Path(__file__).resolve().parents[7]
    offset = root / "build-frontend/pylib-verilator/Frontend/Frontend_offset.yaml"
    names = {
        line[len("  - name: ") :].strip()
        for line in offset.read_text(encoding="utf-8").splitlines()
        if line.startswith("  - name: ")
    }
    required = {
        candidates[0]
        for key, candidates in _SIGNALS.items()
        if key not in {"wb_redirect"}
    }
    required |= {
        _SIGNALS["wb_redirect"][0],
        f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_0_ftqIdx_flag",
        f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_0_startVAddr_addr",
        f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_1_valid",
        f"{_ICACHE_PREFIX}mainPipe.io_toIfu_req_bits_0_data",
        f"{_IFU_PREFIX}s1_fetchBlock_0_ftqIdx_flag",
        f"{_IFU_PREFIX}s1_fetchBlock_0_startVAddr_addr",
        f"{_IFU_PREFIX}s1_fetchBlock_1_valid",
    }
    assert required <= names
    assert len(IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS) == 16
