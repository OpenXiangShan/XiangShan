"""Native BIN-900 sampler proof stays observable through hub and sink."""

import pytest

from env.funcov.native_toffee import EvaluateFlagRecorder
from env.funcov.toffee_runtime import create_toffee_runtime
from env.support.pc_utils import fold_pc
from tests.py.jiabowen.test_ifu_compact_functional_coverage import (
    _PREFIX, _make_recorder, _set_ifu_output,
)


_LOW = 0x1000
_HIGH = (1 << 50) - 0x2000
_GROUP, _BIN = "ifu_v3_pipeline_owner_model", "owner_leaf_002"


def _output(dut, pc, **kwargs):
    _set_ifu_output(dut, [(0, pc, 0x13, 0, 1, 0, 3, 0)],
                    s2_fetch_block_start_pc=pc, **kwargs)
    dut.set(_PREFIX + "s2_fetchBlock_0_valid", 1)
    dut.set(_PREFIX + "s2_fetchBlock_1_valid", 0)
    dut.set(_PREFIX + "s2_icacheMeta_0_exception_value", kwargs.get("exception_type", 0))
    dut.set(_PREFIX + "s2_icacheMeta_1_exception_value", 0)


def _runtime(tmp_path):
    hub, env, dut, _ = _make_recorder(tmp_path)
    _output(dut, _LOW)
    runtime = create_toffee_runtime(hub, hub.toffee_sink)
    model = next(m for m in runtime.cycle_models if type(m).__name__ == "IfuCfvecToffeeCoverage")
    dut.set("reset", 1)
    _cycle(hub, env, model, 0)
    dut.set("reset", 0)
    return hub, env, dut, model


def _cycle(hub, env, model, cycle):
    hub.begin_cycle_snapshot(cycle)
    hub.on_cycle(cycle, env)
    model.on_cycle(cycle)


def test_actual_native_low_high_witnesses_publish_before_public_waiter_reads(tmp_path):
    hub, env, dut, model = _runtime(tmp_path)
    _cycle(hub, env, model, 1)
    low = model._ifu_owner_address_boundary_witnesses
    assert set(low) == {"low"}
    for source in (hub, hub.toffee_sink):
        assert source._ifu_owner_address_boundary_witnesses is low
        assert source._ifu_owner_address_boundary_witnesses["low"]["cycle"] == 1
    assert "_ifu_owner_address_boundary_witnesses" not in model.__dict__
    assert not hub.toffee_sink.key_hit(_GROUP, _BIN)
    _output(dut, _HIGH)
    _cycle(hub, env, model, 2)
    for source in (hub, hub.toffee_sink, model):
        assert set(source._ifu_owner_address_boundary_witnesses) == {"low", "high"}
        assert all(source._ifu_owner_address_boundary_witnesses["high"]["checks"].values())
    assert hub.toffee_sink.key_hit(_GROUP, _BIN)


def test_completed_boundary_run_witness_survives_hardware_reset_but_not_new_hub(tmp_path):
    hub, env, dut, model = _runtime(tmp_path)
    _cycle(hub, env, model, 1)
    low = hub.toffee_sink._ifu_owner_address_boundary_witnesses
    dut.set("reset", 1)
    _cycle(hub, env, model, 2)
    assert hub.toffee_sink._ifu_owner_address_boundary_witnesses is low
    dut.set("reset", 0)
    _output(dut, _HIGH)
    _cycle(hub, env, model, 3)
    assert hub.toffee_sink.key_hit(_GROUP, _BIN)
    other, _env2, _dut2, other_model = _runtime(tmp_path / "new-run")
    assert not other_model._ifu_owner_address_boundary_witnesses
    assert not getattr(other.toffee_sink, "_ifu_owner_address_boundary_witnesses", {})
    assert not other.toffee_sink.key_hit(_GROUP, _BIN)


@pytest.mark.parametrize("failure", [
    "foldpc_mismatch", "pc_outside_region", "exception", "uncache", "missing_pc",
    "not_ready", "not_valid", "zero_enq",
])
def test_real_native_boundary_producer_rejects_unchecked_or_unaccepted_delivery(tmp_path, failure):
    hub, env, dut, model = _runtime(tmp_path)
    _output(dut, _LOW, exception_type=int(failure == "exception"),
            s2_req_is_uncache=int(failure == "uncache"))
    if failure == "foldpc_mismatch":
        dut.set(_PREFIX + "io_toIBuffer_bits_foldpc_0", fold_pc(_LOW) ^ 1)
    elif failure == "pc_outside_region":
        dut.set(_PREFIX + "s2_alignedInstrPcVec_0_addr", 0x4000 >> 1)
        dut.set(_PREFIX + "io_toIBuffer_bits_foldpc_0", fold_pc(0x4000))
    elif failure == "missing_pc":
        delattr(dut, _PREFIX + "s2_alignedInstrPcVec_0_addr")
    elif failure == "not_ready":
        dut.set(_PREFIX + "io_toIBuffer_ready", 0)
    elif failure == "not_valid":
        dut.set(_PREFIX + "io_toIBuffer_valid", 0)
    elif failure == "zero_enq":
        dut.set(_PREFIX + "io_toIBuffer_bits_enqEnable", 0)
    _cycle(hub, env, model, 1)
    assert not getattr(model, "_ifu_owner_address_boundary_witnesses", {})
    assert not getattr(hub.toffee_sink, "_ifu_owner_address_boundary_witnesses", {})
    assert not hub.toffee_sink.key_hit(_GROUP, _BIN)
    if failure in {"foldpc_mismatch", "pc_outside_region", "missing_pc"}:
        assert any(error["event"] == "ifu_address_boundary_pc_reconstruction_mismatch"
                   for error in hub.contract_errors)


def test_address_witness_sharing_does_not_forward_other_native_state(tmp_path):
    hub, _env, _dut, model = _runtime(tmp_path)
    EvaluateFlagRecorder(model, {}, {})._unrelated_native_state = "model-only"
    assert model._unrelated_native_state == "model-only"
    assert not hasattr(hub, "_unrelated_native_state")
    assert not hasattr(hub.toffee_sink, "_unrelated_native_state")


@pytest.mark.parametrize("fault", [1, 2, 3, 5])
def test_native_boundary_accepts_only_inactive_bus_exception_payload(tmp_path, fault):
    hub, env, dut, model = _runtime(tmp_path)
    # Model the exact closed DUT waveform: low-16 enqueue with lane16 mask.
    # Use a 1-lane version so this test still checks the actual accepted lane.
    dut.set(_PREFIX + "io_toIBuffer_bits_exceptionType_value", fault)
    dut.set(_PREFIX + "io_toIBuffer_bits_exceptionMask", 1 << 16)
    dut.set(_PREFIX + "s2_icacheMeta_1_exception_value", fault)
    _cycle(hub, env, model, 1)
    witness = hub.toffee_sink._ifu_owner_address_boundary_witnesses["low"]
    scope = witness["exception_scope"]
    assert scope["exception_type_payload"] == fault
    assert scope["exception_mask"] == 1 << 16
    assert scope["effective_exception_mask"] == 0
    assert scope["fetch_block_valids"] == [1, 0]
    assert scope["fetch_block_exceptions"] == [0, fault]
    assert not hub.toffee_sink.key_hit(_GROUP, _BIN)


@pytest.mark.parametrize("condition", [
    "active_output_fault", "active_mask_with_zero_bus", "valid_block0_fault",
    "valid_block1_fault", "no_valid_block", "invalid_block_valid_bit",
    "enq_outside_valid",
])
def test_native_boundary_rejects_effective_or_valid_block_fault(tmp_path, condition):
    hub, env, dut, model = _runtime(tmp_path)
    if condition in {"active_output_fault", "active_mask_with_zero_bus"}:
        dut.set(_PREFIX + "io_toIBuffer_bits_exceptionType_value", 0 if condition.endswith("zero_bus") else 5)
        dut.set(_PREFIX + "io_toIBuffer_bits_exceptionMask", 1)
    elif condition == "valid_block0_fault":
        dut.set(_PREFIX + "s2_icacheMeta_0_exception_value", 5)
    elif condition == "valid_block1_fault":
        dut.set(_PREFIX + "s2_fetchBlock_1_valid", 1)
        dut.set(_PREFIX + "s2_icacheMeta_1_exception_value", 5)
    elif condition == "no_valid_block":
        dut.set(_PREFIX + "s2_fetchBlock_0_valid", 0)
    elif condition == "invalid_block_valid_bit":
        dut.set(_PREFIX + "s2_fetchBlock_1_valid", 2)
    elif condition == "enq_outside_valid":
        dut.set(_PREFIX + "io_toIBuffer_bits_valid", 0)
    _cycle(hub, env, model, 1)
    assert not getattr(hub.toffee_sink, "_ifu_owner_address_boundary_witnesses", {})
    assert not hub.toffee_sink.key_hit(_GROUP, _BIN)


@pytest.mark.parametrize("field", [
    "io_toIBuffer_bits_exceptionMask", "io_toIBuffer_bits_exceptionType_value",
    "s2_fetchBlock_0_valid", "s2_fetchBlock_1_valid",
    "s2_icacheMeta_0_exception_value", "s2_icacheMeta_1_exception_value",
])
def test_native_boundary_missing_exception_scope_probe_is_not_a_clean_default(tmp_path, field):
    hub, env, dut, model = _runtime(tmp_path)
    delattr(dut, _PREFIX + field)
    _cycle(hub, env, model, 1)
    assert not getattr(hub.toffee_sink, "_ifu_owner_address_boundary_witnesses", {})
    assert not hub.toffee_sink.key_hit(_GROUP, _BIN)


def test_native_boundary_keeps_exact_first_high_pc_with_inactive_lane16_payload(tmp_path):
    hub, env, dut, model = _runtime(tmp_path)
    entries = [(slot, _HIGH + 4 * slot, 0x13, 0, 2 * slot + 1, 0, 3, 0)
               for slot in range(16)]
    _set_ifu_output(dut, entries, exception_type=5, s2_fetch_block_start_pc=_HIGH)
    dut.set(_PREFIX + "s2_fetchBlock_0_valid", 1)
    dut.set(_PREFIX + "s2_fetchBlock_1_valid", 0)
    dut.set(_PREFIX + "s2_icacheMeta_0_exception_value", 0)
    dut.set(_PREFIX + "s2_icacheMeta_1_exception_value", 5)
    dut.set(_PREFIX + "io_toIBuffer_bits_exceptionMask", 0x10000)
    _cycle(hub, env, model, 1)
    witness = hub.toffee_sink._ifu_owner_address_boundary_witnesses["high"]
    assert witness["start_pc"] == _HIGH
    assert [item["pc"] for item in witness["slots"]] == [_HIGH + 4 * i for i in range(16)]
    scope = witness["exception_scope"]
    assert scope["enq_enable"] == scope["valid_mask"] == 0xffff
    assert scope["exception_type_payload"] == 5 and scope["exception_mask"] == 0x10000
    assert scope["effective_exception_mask"] == 0
    assert scope["fetch_block_valids"] == [1, 0]
    assert scope["fetch_block_exceptions"] == [0, 5]
    assert not hub.toffee_sink.key_hit(_GROUP, _BIN)  # Still needs a checked low endpoint.
