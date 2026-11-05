"""Real native evaluator/hub/sink state identity and reset order, offline."""

import pytest

from env.funcov.native_toffee import EvaluateFlagRecorder
from env.funcov.toffee_bridge import ToffeeCoverageSink
from env.funcov.toffee_runtime import create_toffee_runtime
from env.funcov.runtime_context import default_pilot_csv_path
from env.support import fold_pc
from tests.py.jiabowen.test_ifu_compact_functional_coverage import _make_recorder


_IFU = "Frontend_top.Frontend.inner_ifu."
_UNCACHE = "Frontend_top.Frontend.inner_ifu.uncacheUnit."
_INSTR = "Frontend_top.Frontend.inner_instrUncache."
_ENTRY = "Frontend_top.Frontend.inner_instrUncache.entries_0."
_FIELDS = {
    **{field: _UNCACHE + signal for field, signal in {
        "req_valid": "io_req_valid", "req_ready": "io_req_ready",
        "req_is_mmio": "io_req_bits_isMmio", "req_pbmt": "io_req_bits_pbmt",
    }.items()},
    **{field: _IFU + signal for field, signal in {
        "s2_ftq_flag": "s2_fetchBlock_0_ftqIdx_flag",
        "s2_ftq_value": "s2_fetchBlock_0_ftqIdx_value",
        "s2_instr_pc": "s2_alignedInstrPcVec_0_addr",
        "to_valid": "io_toIBuffer_valid", "to_ready": "io_toIBuffer_ready",
        "to_enq": "io_toIBuffer_bits_enqEnable",
        "to_ftq_flag": "io_toIBuffer_bits_ftqPtr_0_flag",
        "to_ftq_value": "io_toIBuffer_bits_ftqPtr_0_value",
        "to_foldpc": "io_toIBuffer_bits_foldpc_0",
        "to_exception": "io_toIBuffer_bits_exceptionType_value",
    }.items()},
    **{field: _ENTRY + signal for field, signal in {
        "entry_state": "state", "entry_req_addr": "reqReg_addr_addr",
        "entry_resending": "resending",
    }.items()},
    **{field: "auto_inner_instrUncache_client_out_" + signal for field, signal in {
        "tl_a_valid": "a_valid", "tl_a_ready": "a_ready", "tl_a_addr": "a_bits_address",
        "tl_d_valid": "d_valid", "tl_d_data": "d_bits_data",
        "tl_d_corrupt": "d_bits_corrupt", "tl_d_denied": "d_bits_denied",
    }.items()},
    **{field: _INSTR + signal for field, signal in {
        "instr_resp_valid": "io_toIfu_resp_valid", "instr_resp_data": "io_toIfu_resp_bits_data",
        "instr_resp_corrupt": "io_toIfu_resp_bits_corrupt",
        "instr_resp_denied": "io_toIfu_resp_bits_denied",
        "instr_resp_need_resend": "io_toIfu_resp_bits_needResend",
    }.items()},
    "backend_redirect": "Frontend_top.Frontend.inner_ftq.backendRedirect_valid",
}


def _runtime(tmp_path):
    hub, env, dut, _ = _make_recorder(tmp_path)
    runtime = create_toffee_runtime(hub, hub.toffee_sink)
    model = next(m for m in runtime.cycle_models if type(m).__name__ == "MmioNcOwnerToffeeCoverage")
    for path in _FIELDS.values():
        dut.set(path, 0)
    dut.set(_IFU + "s2_alignShiftNum", 0)
    for slot in range(36):
        dut.set(_IFU + f"io_toIBuffer_bits_isRvc_{slot}", 0)
    dut.set("reset", 1)
    hub.begin_cycle_snapshot(0)
    hub.on_cycle(0, env)
    model.on_cycle(0)
    return hub, env, dut, model


def _cycle(hub, dut, model, cycle, **updates):
    for field, value in updates.items():
        dut.set(_FIELDS[field], value)
    dut.set("reset", 0)
    hub.begin_cycle_snapshot(cycle)
    # Same order as fixture: hub reset/snapshot path, then native evaluator.
    hub.on_cycle(cycle, hub.env)
    model.on_cycle(cycle)


def _redirected_episode(hub, dut, model):
    _cycle(hub, dut, model, 1, req_valid=1, req_ready=1, req_is_mmio=1,
           s2_ftq_flag=0, s2_ftq_value=7, s2_instr_pc=0x4000)
    _cycle(hub, dut, model, 2, req_valid=0, entry_state=1, entry_req_addr=0x803,
           tl_a_valid=1, tl_a_ready=1, tl_a_addr=0x1006)
    _cycle(hub, dut, model, 3, entry_state=2, tl_a_valid=0,
           tl_d_valid=1, tl_d_data=0x0003 << 48)
    state = hub.toffee_sink._ifu_instr_uncache_owner_state
    assert state["cross_8b_pending"]["old_identity"] == (0, 7, fold_pc(0x8000))
    _cycle(hub, dut, model, 4, entry_state=3, tl_d_valid=0,
           instr_resp_valid=1, instr_resp_data=3, instr_resp_need_resend=1)
    _cycle(hub, dut, model, 5, entry_state=1, entry_resending=1,
           instr_resp_valid=0, tl_a_valid=1, tl_a_addr=0x1008)
    assert state["cross_8b_pending"]["second_a_fire"] is True
    _cycle(hub, dut, model, 6, entry_state=2, tl_a_valid=0, backend_redirect=1)
    return state


def test_native_redirected_resend_state_is_visible_and_completes_strictly(tmp_path):
    hub, _env, dut, model = _runtime(tmp_path)
    state = _redirected_episode(hub, dut, model)
    for source in (model, hub, hub.toffee_sink):
        assert source._ifu_instr_uncache_owner_state is state
        assert source._ifu_mmio_nc_owner_state is hub._ifu_mmio_nc_owner_state
    pending = state["redirected_resend_pending"]
    assert pending["redirect_cycle"] == 6 and pending["saw_second_a_fire"] is True
    assert pending["saw_second_d"] is False
    assert "_ifu_instr_uncache_owner_state" not in model.__dict__
    assert "_ifu_mmio_nc_owner_state" not in model.__dict__
    _cycle(hub, dut, model, 7, backend_redirect=0, tl_d_valid=1, tl_d_data=0x13)
    _cycle(hub, dut, model, 8, entry_state=3, entry_resending=0, tl_d_valid=0,
           instr_resp_valid=1, instr_resp_data=0x00130003, instr_resp_need_resend=0)
    _cycle(hub, dut, model, 9, entry_state=0, instr_resp_valid=0,
           to_valid=1, to_ready=1, to_enq=1, to_ftq_value=8,
           to_foldpc=fold_pc(0x8080))
    assert state["redirected_resend_pending"] is None
    assert hub.toffee_sink.key_hit("ifu_instruncache_owner_v3", "instruncache_leaf_011")


def test_native_hub_then_model_reset_replaces_shared_pending_state(tmp_path):
    hub, _env, dut, model = _runtime(tmp_path)
    old = _redirected_episode(hub, dut, model)
    old_mmio = hub._ifu_mmio_nc_owner_state
    assert old["redirected_resend_pending"] is not None
    old_mmio["nc_checker_redirect_pending"] = {"cycle": 6}
    dut.set("reset", 1)
    hub.begin_cycle_snapshot(7)
    hub.on_cycle(7, hub.env)
    after_hub = hub._ifu_instr_uncache_owner_state
    assert after_hub is not old
    assert model._ifu_instr_uncache_owner_state is after_hub
    assert after_hub["cross_8b_pending"] is after_hub["redirected_resend_pending"] is None
    model.on_cycle(7)
    after_model = hub.toffee_sink._ifu_instr_uncache_owner_state
    assert after_model is model._ifu_instr_uncache_owner_state is hub._ifu_instr_uncache_owner_state
    assert after_model is not after_hub
    assert after_model["cross_8b_pending"] is after_model["redirected_resend_pending"] is None
    assert hub._ifu_mmio_nc_owner_state is not old_mmio
    assert model._ifu_mmio_nc_owner_state["nc_checker_redirect_pending"] is None
    assert not hub.toffee_sink.key_hit("ifu_instruncache_owner_v3", "instruncache_leaf_011")
    # Old second-D/response fragments cannot finish an episode after reset.
    _cycle(hub, dut, model, 8, backend_redirect=0, entry_state=2,
           entry_resending=1, tl_d_valid=1, tl_d_data=0x13)
    _cycle(hub, dut, model, 9, entry_state=3, entry_resending=0, tl_d_valid=0,
           instr_resp_valid=1, instr_resp_data=0x00130003, instr_resp_need_resend=0)
    _cycle(hub, dut, model, 10, entry_state=0, instr_resp_valid=0,
           to_valid=1, to_ready=1, to_enq=1, to_ftq_value=8,
           to_foldpc=fold_pc(0x8080))
    assert not hub.toffee_sink.key_hit("ifu_instruncache_owner_v3", "instruncache_leaf_011")


def test_only_explicit_owner_fields_are_shared_and_unattached_sink_stays_strict(tmp_path):
    hub, _env, _dut, model = _runtime(tmp_path)
    wrapped = EvaluateFlagRecorder(model, {}, {})
    wrapped._unrelated_native_state = "model-only"
    assert model._unrelated_native_state == "model-only"
    assert not hasattr(hub, "_unrelated_native_state")
    assert not hasattr(hub.toffee_sink, "_unrelated_native_state")
    sink = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    with pytest.raises(AttributeError):
        _ = sink._ifu_instr_uncache_owner_state


@pytest.mark.parametrize("mode", ["absent", "unreadable"])
def test_mmio_owner_snapshot_without_native_probes_does_not_fabricate_pending(tmp_path, mode):
    hub, _env, dut, model = _runtime(tmp_path)
    for path in _FIELDS.values():
        if mode == "absent":
            delattr(dut, path)
        else:
            getattr(dut, path).value = None
    _cycle(hub, dut, model, 1)
    state = hub.toffee_sink._ifu_instr_uncache_owner_state
    assert state["latest_accepted_request"] is None
    assert state["cross_8b_pending"] is None
    assert state["redirected_resend_pending"] is None
    assert not hub.toffee_sink.key_hit("ifu_instruncache_owner_v3", "instruncache_leaf_011")
