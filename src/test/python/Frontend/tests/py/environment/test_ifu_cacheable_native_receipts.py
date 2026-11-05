"""Actual native cacheable producer/cfVec consumer proof channel, offline."""

import pytest

from env.funcov.native_toffee import EvaluateFlagRecorder
from env.funcov.toffee_runtime import create_toffee_runtime
from env.support.pc_utils import fold_pc
from tests.py.jiabowen.test_ifu_cacheable_pipeline_functional_coverage import (
    _SIGNALS, _make_recorder, _set_request, _set_s1, _idle_request,
)


_GROUP = "ifu_cacheable_delivery"
_RECOVERY = "backend_recovery_multi_instr"
_PAIR = "same_cacheline_multi_rvi"


def _cfvec(dut, pcs=(), *, ftq=(0, 3), widths=None, instrs=None):
    if widths is None:
        widths = [0] * len(pcs)
    if instrs is None:
        instrs = [0x13] * len(pcs)
    dut._test_cfvec_pcs = dict(enumerate(pcs))
    for slot in range(8):
        stem = f"io_backend_cfVec_{slot}_"
        valid = slot < len(pcs)
        dut.set(stem + "valid", int(valid))
        dut.set(stem + "bits_instr", instrs[slot] if valid else 0x13)
        dut.set(stem + "bits_isRvc", widths[slot] if valid else 0)
        dut.set(stem + "bits_ftqPtr_flag", ftq[0])
        dut.set(stem + "bits_ftqPtr_value", ftq[1])
        dut.set(stem + "bits_foldpc", fold_pc(pcs[slot]) if valid else 0)
        for cause in (1, 2, 12, 19, 20):
            dut.set(stem + f"bits_exceptionVec_{cause}", 0)


def _runtime(tmp_path):
    hub, env, dut = _make_recorder(tmp_path)
    env.observed_cfvec_pc = lambda slot: dut._test_cfvec_pcs[int(slot)]
    _cfvec(dut)
    # Compiled DUT probes exist before cycle one; create future metadata
    # upfront so the real read-once absence cache is exercised honestly.
    blocks = _set_request(dut)
    _set_s1(dut, blocks, valid=0, fire=0)
    _idle_request(dut)
    dut.set("io_backend_toIBuf_decodeCanAccept", 0)
    dut.set("io_backend_toFtq_redirect_valid", 0)
    runtime = create_toffee_runtime(hub, hub.toffee_sink)
    models = tuple(m for m in runtime.cycle_models if type(m).__name__ in {
        "IfuCfvecToffeeCoverage", "IfuCacheablePipelineToffeeCoverage",
    })
    assert [type(m).__name__ for m in models] == [
        "IfuCfvecToffeeCoverage", "IfuCacheablePipelineToffeeCoverage",
    ]
    dut.set("reset", 1)
    _cycle(hub, env, models, 0)
    dut.set("reset", 0)
    return hub, env, dut, models


def _cycle(hub, env, models, cycle):
    # Match the real fixture's hub/snapshot then cfVec then cacheable order.
    hub.begin_cycle_snapshot(cycle)
    hub.on_cycle(cycle, env)
    for model in models:
        model.on_cycle(cycle)


def _produce(hub, env, dut, models, *, mismatch=False, flush=False,
             s0_flush=False, s0_ready=True):
    blocks = _set_request(dut, flush=int(s0_flush), ready=int(s0_ready),
                          fire=int(s0_ready and not s0_flush))
    _cycle(hub, env, models, 1)
    if mismatch:
        blocks[0]["ftqIdx_value"] += 1
    _idle_request(dut)
    _set_s1(dut, blocks)
    dut.set(_SIGNALS["s1_flush"][0], int(flush))
    _cycle(hub, env, models, 2)
    dut.set(_SIGNALS["s1_valid"][0], 0)
    dut.set(_SIGNALS["s1_fire"][0], 0)
    dut.set(_SIGNALS["s1_flush"][0], 0)
    return blocks


def _deliver(hub, env, dut, models, cycle=3, *, pcs=(0x80000000, 0x80000004),
             ftq=(0, 3), widths=None, instrs=None):
    _cfvec(dut, pcs, ftq=ftq, widths=widths, instrs=instrs)
    dut.set("io_backend_toIBuf_decodeCanAccept", 1)
    _cycle(hub, env, models, cycle)


def test_actual_native_producer_publishes_verified_receipt_to_consumer(tmp_path):
    hub, env, dut, models = _runtime(tmp_path)
    _produce(hub, env, dut, models)
    cfvec, producer = models
    receipt = producer._ifu_cacheable_verified_windows
    assert len(receipt) == 1
    assert receipt is hub._ifu_cacheable_verified_windows
    assert receipt is cfvec._ifu_cacheable_verified_windows
    assert receipt is hub.toffee_sink._ifu_cacheable_verified_windows
    assert "_ifu_cacheable_verified_windows" not in producer.__dict__
    assert hub.key_hit("ifu_cacheable_s1", "source_ftq_address_matched")
    _deliver(hub, env, dut, models)
    assert hub.toffee_sink.key_hit(_GROUP, _RECOVERY)
    assert hub.toffee_sink.key_hit(_GROUP, _PAIR)


@pytest.mark.parametrize("widths,pcs,expected", [
    ([1, 1], [0x80000000, 0x80000002], "same_cacheline_multi_rvc"),
    ([1, 0], [0x80000000, 0x80000002], "rvc_then_rvi"),
    ([0, 1], [0x80000000, 0x80000004], "rvi_then_rvc"),
])
def test_real_consumer_classifies_verified_mixed_width_pairs(tmp_path, widths, pcs, expected):
    hub, env, dut, models = _runtime(tmp_path)
    _produce(hub, env, dut, models)
    _deliver(hub, env, dut, models, pcs=pcs, widths=widths)
    assert hub.toffee_sink.key_hit(_GROUP, _RECOVERY)
    assert hub.toffee_sink.key_hit(_GROUP, expected)


@pytest.mark.parametrize("condition", ["mismatch", "flush", "s0_flush", "s0_backpressure"])
def test_unverified_producer_transaction_never_reaches_real_consumer(tmp_path, condition):
    hub, env, dut, models = _runtime(tmp_path)
    _produce(hub, env, dut, models, mismatch=condition == "mismatch",
             flush=condition == "flush", s0_flush=condition == "s0_flush",
             s0_ready=condition != "s0_backpressure")
    assert not models[-1]._ifu_cacheable_verified_windows
    assert not hub._ifu_cacheable_verified_windows
    _deliver(hub, env, dut, models)
    assert not hub.toffee_sink.key_hit(_GROUP, _RECOVERY)
    assert not hub.toffee_sink.key_hit(_GROUP, _PAIR)


@pytest.mark.parametrize("ftq,pcs", [
    ((0, 4), (0x80000000, 0x80000004)),
    ((1, 3), (0x80000000, 0x80000004)),
    ((0, 3), (0x80000010, 0x80000014)),
    ((0, 3), (0x7ffffff8, 0x7ffffffc)),
])
def test_valid_receipt_cannot_credit_wrong_ftq_or_address(tmp_path, ftq, pcs):
    hub, env, dut, models = _runtime(tmp_path)
    _produce(hub, env, dut, models)
    assert len(models[-1]._ifu_cacheable_verified_windows) == 1
    _deliver(hub, env, dut, models, ftq=ftq, pcs=pcs)
    assert not hub.toffee_sink.key_hit(_GROUP, _RECOVERY)
    assert not hub.toffee_sink.key_hit(_GROUP, _PAIR)


@pytest.mark.parametrize("age,expected", [(128, True), (129, False)])
def test_real_consumer_enforces_existing_128_cycle_receipt_lifetime(tmp_path, age, expected):
    hub, env, dut, models = _runtime(tmp_path)
    _produce(hub, env, dut, models)
    _deliver(hub, env, dut, models, cycle=2 + age)
    assert hub.toffee_sink.key_hit(_GROUP, _RECOVERY) is expected
    assert hub.toffee_sink.key_hit(_GROUP, _PAIR) is expected


def test_hub_then_native_reset_invalidates_receipts_and_old_pending_transfer(tmp_path):
    hub, env, dut, models = _runtime(tmp_path)
    _produce(hub, env, dut, models)
    old = models[-1]._ifu_cacheable_verified_windows
    assert len(old) == 1
    dut.set("reset", 1)
    _cycle(hub, env, models, 3)
    fresh = hub._ifu_cacheable_verified_windows
    assert fresh is not old and len(fresh) == 0 and fresh.maxlen == 64
    assert models[-1]._ifu_cacheable_verified_windows is fresh
    assert models[0]._ifu_cacheable_verified_windows is fresh
    dut.set("reset", 0)
    # Reusing the exact FTQ/PC after reset is still not a verified transfer.
    _cycle(hub, env, models, 4)
    _deliver(hub, env, dut, models, cycle=5)
    assert not hub.toffee_sink.key_hit(_GROUP, _RECOVERY)
    assert not hub.toffee_sink.key_hit(_GROUP, _PAIR)
    assert models[-1]._ifu_cacheable_pending_transfer is None


def test_only_the_explicit_receipt_store_is_shared(tmp_path):
    hub, _env, _dut, models = _runtime(tmp_path)
    producer = models[-1]
    EvaluateFlagRecorder(producer, {}, {})._unrelated_native_state = "model-only"
    assert producer._unrelated_native_state == "model-only"
    assert not hasattr(hub, "_unrelated_native_state")
    assert not hasattr(models[0], "_unrelated_native_state")
    assert not hasattr(hub.toffee_sink, "_unrelated_native_state")
    assert producer._ifu_cacheable_verified_windows.maxlen == 64


def test_completed_receipts_remain_bounded_under_real_producer_calls(tmp_path):
    hub, env, dut, models = _runtime(tmp_path)
    for i in range(65):
        cycle = 1 + 2 * i
        dut.set(_SIGNALS["s1_valid"][0], 0)
        dut.set(_SIGNALS["s1_fire"][0], 0)
        blocks = _set_request(dut, ftq0=(0, i % 64))
        _cycle(hub, env, models, cycle)
        _idle_request(dut)
        _set_s1(dut, blocks)
        _cycle(hub, env, models, cycle + 1)
    receipts = models[-1]._ifu_cacheable_verified_windows
    assert len(receipts) == 64 and receipts.maxlen == 64
    assert receipts[0]["cycle"] == 4 and receipts[-1]["cycle"] == 130
    assert hub._ifu_cacheable_verified_windows is receipts


@pytest.mark.parametrize("instr,bin_name", [
    (0x00000463, "branch_next_pc_matches_decode"),
    (0x0080006f, "jal_target_without_stale_delivery"),
])
def test_real_native_consumer_credits_verified_cfi_successor(tmp_path, instr, bin_name):
    hub, env, dut, models = _runtime(tmp_path)
    _produce(hub, env, dut, models)
    _deliver(hub, env, dut, models, pcs=[0x80000000], instrs=[instr])
    assert not hub.toffee_sink.key_hit("ifu_cacheable_cfi_flow", bin_name)
    _deliver(hub, env, dut, models, cycle=4, pcs=[0x80000008])
    assert hub.toffee_sink.key_hit("ifu_cacheable_cfi_flow", bin_name)


def test_reset_discards_uncompleted_producer_transfer(tmp_path):
    hub, env, dut, models = _runtime(tmp_path)
    blocks = _set_request(dut)
    _cycle(hub, env, models, 1)
    assert models[-1]._ifu_cacheable_pending_transfer is not None
    _idle_request(dut)
    dut.set("reset", 1)
    _cycle(hub, env, models, 2)
    dut.set("reset", 0)
    _set_s1(dut, blocks)
    _cycle(hub, env, models, 3)
    assert not models[-1]._ifu_cacheable_verified_windows
    _deliver(hub, env, dut, models, cycle=4)
    assert not hub.toffee_sink.key_hit(_GROUP, _RECOVERY)
    assert not hub.toffee_sink.key_hit(_GROUP, _PAIR)
    assert not hub.toffee_sink.key_hit("ifu_cacheable_s1", "source_ftq_address_matched")
