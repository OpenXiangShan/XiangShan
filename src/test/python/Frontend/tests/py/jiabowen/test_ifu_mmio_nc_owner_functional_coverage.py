from types import SimpleNamespace

from env.funcov.py.ifu import mmio_nc_owner_funcov as owner


class _Recorder:
    def __init__(self, signals=None):
        self.signals = dict(signals or {})
        self.hits = set()

    def _read_first_dut_signal(self, _dut, names):
        return next(
            (self.signals[name] for name in names if name in self.signals), None
        )

    def mark(self, group, bin_name, _cycle, _evidence):
        self.hits.add((group, bin_name))


def _state(recorder):
    owner.initialize_mmio_nc_owner_coverage_state(recorder)
    return recorder._ifu_mmio_nc_owner_state


def _empty_snapshot():
    return owner._snapshot(_Recorder(), object())


def test_sampler_contract_has_39_mmio_and_39_nc_leaf_bins():
    assert len(owner.MMIO_NC_OWNER_SAMPLER_BIN_KEYS) == 78
    assert (
        sum(
            group == owner.MMIO_OWNER_GROUP
            for group, _ in owner.MMIO_NC_OWNER_SAMPLER_BIN_KEYS
        )
        == 39
    )
    assert (
        sum(
            group == owner.NC_OWNER_GROUP
            for group, _ in owner.MMIO_NC_OWNER_SAMPLER_BIN_KEYS
        )
        == 39
    )


def test_snapshot_reads_current_verilator_derived_aliases():
    recorder = _Recorder(
        {
            "uncacheUnit.io_req_ready": 1,
            "uncacheUnit.io_resp_valid": 1,
            "uncacheUnit.io_resp_bits_uncacheData": 0x1234,
            "uncacheUnit.io_ifuStall": 0,
            "uncacheUnit.io_emptyAfter": 1,
            "Frontend_top.Frontend.inner_ifu.s2_uncacheData": 0xAABBCCDD,
            "Frontend_top.Frontend.inner_instrUncache.__Vtogcov__io_toIfu_resp_valid": 1,
            "Frontend_top.Frontend.inner_instrUncache.__Vtogcov__io_toIfu_resp_bits_data": 0x5678,
            "Frontend_top.Frontend.inner_instrUncache.__Vtogcov__io_toIfu_resp_bits_corrupt": 0,
            "Frontend_top.Frontend.inner_instrUncache.__Vtogcov__io_toIfu_resp_bits_denied": 1,
            "Frontend_top.Frontend.inner_instrUncache.__Vtogcov__io_toIfu_resp_bits_needResend": 0,
            "inner_ibuffer.io_empty": 1,
            "inner_icache.wayLookup.io_toMainPipe_valid": 1,
            "inner_icache.wayLookup.io_toMainPipe_bits_wayLookupInfo_0_bits_entry_waymask_0": 3,
            "inner_icache.wayLookup.io_toMainPipe_bits_wayLookupInfo_0_bits_entry_waymask_1": 0,
        }
    )

    snapshot = owner._snapshot(recorder, object())

    assert snapshot["req_ready"] == 1
    assert snapshot["resp_valid"] == 1
    assert snapshot["resp_data"] == 0x1234
    assert snapshot["ifu_stall"] == 0
    assert snapshot["empty_after"] == 1
    assert snapshot["s2_uncache_data"] == 0xAABBCCDD
    assert snapshot["instr_resp_valid"] == 1
    assert snapshot["instr_resp_data"] == 0x5678
    assert snapshot["instr_resp_corrupt"] == 0
    assert snapshot["instr_resp_denied"] == 1
    assert snapshot["instr_resp_need_resend"] == 0
    assert snapshot["ibuffer_empty"] == 1
    assert snapshot["waylookup_valid"] == 1
    assert snapshot["waymask_0"] == 3
    assert snapshot["waymask_1"] == 0


def test_nc_accept_uses_current_pbmt_encoding_and_selects_uncache_path():
    snapshot = _empty_snapshot()
    snapshot.update(
        {
            "s2_valid": 1,
            "s2_req_uncache": 1,
            "s2_use_uncache": 1,
            "s2_pmp_mmio": 0,
            "s2_pbmt": 2,
            "req_valid": 1,
            "req_ready": 1,
        }
    )
    recorder = _Recorder()
    owner._sample_nc(recorder, 1, snapshot, _state(recorder))
    assert (owner.NC_OWNER_GROUP, "nc_leaf_001") not in recorder.hits

    snapshot["s2_pbmt"] = 1
    recorder = _Recorder()
    owner._sample_nc(recorder, 2, snapshot, _state(recorder))
    assert (owner.NC_OWNER_GROUP, "nc_leaf_001") in recorder.hits
    assert (owner.NC_OWNER_GROUP, "nc_leaf_003") in recorder.hits

    snapshot.update({"waylookup_valid": 1, "waymask_0": 0, "waymask_1": 0})
    recorder = _Recorder()
    owner._sample_nc(recorder, 3, snapshot, _state(recorder))
    assert (owner.NC_OWNER_GROUP, "nc_leaf_003") in recorder.hits


def test_page_tail_uses_pruned_entry_address_low_11_bits():
    snapshot = _empty_snapshot()
    snapshot.update(
        {
            "entry_req_addr": 0x3FF,
            "resp_valid": 1,
            "resp_data": 0x0001,
            "resp_need_resend": 0,
        }
    )
    recorder = _Recorder()
    state = _state(recorder)
    state["mmio_active"] = True
    owner._sample_mmio(recorder, 1, snapshot, state)
    assert (owner.MMIO_OWNER_GROUP, "mmio_leaf_027") not in recorder.hits

    snapshot["entry_req_addr"] = 0x7FF
    owner._sample_mmio(recorder, 2, snapshot, state)
    assert (owner.MMIO_OWNER_GROUP, "mmio_leaf_027") in recorder.hits


def test_beat_tail_uses_pruned_entry_address_low_two_bits():
    snapshot = _empty_snapshot()
    snapshot.update(
        {
            "entry_req_addr": 0x2,
            "entry_state": owner._ENTRY_REFILL_RESP,
            "entry_resending": 0,
            "tl_d_valid": 1,
            "tl_d_data": 0x0001 << (2 * 16),
            "tl_d_corrupt": 0,
            "tl_d_denied": 0,
        }
    )
    recorder = _Recorder()
    state = _state(recorder)
    state["nc_active"] = True
    owner._sample_nc(recorder, 1, snapshot, state)
    assert (owner.NC_OWNER_GROUP, "nc_leaf_024") not in recorder.hits

    snapshot["entry_req_addr"] = 0x3
    snapshot["tl_d_data"] = 0x0001 << (3 * 16)
    owner._sample_nc(recorder, 2, snapshot, state)
    assert (owner.NC_OWNER_GROUP, "nc_leaf_024") in recorder.hits


def test_mmio_path_is_latched_through_response_and_delivery():
    snapshot = _empty_snapshot()
    snapshot.update(
        {
            "s2_valid": 1,
            "s2_req_uncache": 1,
            "s2_pmp_mmio": 1,
            "s2_pbmt": 0,
            "entry_state": owner._ENTRY_REFILL_RESP,
            "tl_d_valid": 1,
            "tl_d_corrupt": 0,
            "tl_d_denied": 0,
            "instr_resp_valid": 1,
            "instr_resp_corrupt": 0,
            "instr_resp_denied": 0,
        }
    )
    recorder = _Recorder()
    state = _state(recorder)
    owner._sample_mmio(recorder, 1, snapshot, state)

    snapshot.update(
        {
            "s2_valid": 0,
            "s2_req_uncache": 0,
            "s2_pmp_mmio": 0,
            "entry_state": 3,
            "tl_d_valid": 0,
            "resp_valid": 1,
            "resp_need_resend": 0,
            "resp_exception": 3,
            "to_valid": 1,
            "to_ready": 1,
            "to_enq": 1,
            "to_is_rvc": 1,
        }
    )
    owner._sample_mmio(recorder, 2, snapshot, state)

    for leaf in (6, 7, 20, 38):
        assert (owner.MMIO_OWNER_GROUP, f"mmio_leaf_{leaf:03d}") in recorder.hits


def test_nc_response_and_single_delivery_accept_one_observed_classification():
    snapshot = _empty_snapshot()
    snapshot.update(
        {
            "entry_state": owner._ENTRY_REFILL_RESP,
            "tl_d_valid": 1,
            "tl_d_corrupt": 0,
            "tl_d_denied": 0,
            "instr_resp_valid": 1,
            "instr_resp_corrupt": 0,
            "instr_resp_denied": 0,
        }
    )
    recorder = _Recorder()
    state = _state(recorder)
    state["nc_active"] = True
    owner._sample_nc(recorder, 1, snapshot, state)

    snapshot.update(
        {
            "entry_state": 3,
            "tl_d_valid": 0,
            "resp_valid": 1,
            "resp_exception": 0,
            "uncache_state": owner._IDLE,
            "to_valid": 1,
            "to_ready": 1,
            "to_enq": 1,
            "to_is_rvc": 1,
        }
    )
    owner._sample_nc(recorder, 2, snapshot, state)

    assert (owner.NC_OWNER_GROUP, "nc_leaf_011") in recorder.hits
    assert (owner.NC_OWNER_GROUP, "nc_leaf_019") in recorder.hits


def test_uncache_delivery_type_uses_rtl_s2_uncache_data():
    snapshot = _empty_snapshot()
    snapshot.update(
        {
            "to_valid": 1,
            "to_ready": 1,
            "to_enq": 1,
            "to_is_rvc": 0,
            "resp_exception": 0,
            "s2_uncache_data": 0x0001,
            "uncache_pc": 0x100,
        }
    )
    recorder = _Recorder()
    state = _state(recorder)
    state["nc_active"] = True
    state["nc_last_delivery"] = {"pc": 0xFF, "is_rvc": True}
    owner._sample_nc(recorder, 1, snapshot, state)

    assert (owner.NC_OWNER_GROUP, "nc_leaf_016") in recorder.hits
    assert (owner.NC_OWNER_GROUP, "nc_leaf_017") not in recorder.hits


def test_nc_path_transition_recognizes_pbmt_io_as_strong_order_mmio():
    snapshot = _empty_snapshot()
    snapshot.update(
        {
            "s2_valid": 1,
            "s2_pmp_mmio": 0,
            "s2_pbmt": owner._PBMT_IO,
            "req_valid": 1,
            "req_ready": 1,
            "uncache_state": owner._WAIT_LAST_COMMIT,
        }
    )
    recorder = _Recorder()
    state = _state(recorder)
    state["previous_path"] = "nc"
    owner._sample_nc(recorder, 1, snapshot, state)

    assert (owner.NC_OWNER_GROUP, "nc_leaf_035") in recorder.hits
    assert (owner.NC_OWNER_GROUP, "nc_leaf_038") in recorder.hits


def test_owner_path_cross_requires_all_four_sequential_transitions():
    recorder = _Recorder()
    state = _state(recorder)

    def sample(cycle, path):
        snapshot = _empty_snapshot()
        if path == "cacheable":
            state["nc_active"] = False
            snapshot.update(
                {
                    "s2_valid": 1,
                    "s2_req_uncache": 0,
                    "to_valid": 1,
                    "to_ready": 1,
                }
            )
        elif path == "nc":
            snapshot.update(
                {
                    "s2_valid": 1,
                    "s2_req_uncache": 1,
                    "s2_use_uncache": 1,
                    "s2_pmp_mmio": 0,
                    "s2_pbmt": owner._PBMT_NC,
                    "req_valid": 1,
                    "req_ready": 1,
                }
            )
        else:
            state["nc_active"] = False
            snapshot.update(
                {
                    "s2_valid": 1,
                    "s2_req_uncache": 1,
                    "s2_pmp_mmio": 1,
                    "s2_pbmt": 0,
                    "req_valid": 1,
                    "req_ready": 1,
                }
            )
        owner._sample_nc(recorder, cycle, snapshot, state)

    for cycle, path in enumerate(
        ("cacheable", "nc", "cacheable", "mmio", "nc"), start=1
    ):
        sample(cycle, path)
    assert (
        "ifu_v3_pipeline_owner_model",
        "owner_leaf_059",
    ) not in recorder.hits

    sample(6, "mmio")
    assert (
        "ifu_v3_pipeline_owner_model",
        "owner_leaf_059",
    ) in recorder.hits
    assert set(state["path_transition_observations"]) == {
        "cacheable_to_nc",
        "cacheable_to_mmio",
        "nc_to_cacheable",
        "nc_to_mmio",
    }


def test_nc_response_cannot_hit_mmio_response_leaf():
    snapshot = _empty_snapshot()
    snapshot.update(
        {
            "s2_valid": 1,
            "s2_req_uncache": 1,
            "s2_use_uncache": 1,
            "s2_pmp_mmio": 0,
            "s2_pbmt": owner._PBMT_NC,
            "req_valid": 1,
            "req_ready": 1,
        }
    )
    recorder = _Recorder()
    state = _state(recorder)
    owner._sample_mmio(recorder, 1, snapshot, state)
    owner._sample_nc(recorder, 1, snapshot, state)

    snapshot.update(
        {
            "s2_valid": 0,
            "s2_req_uncache": 0,
            "s2_use_uncache": 0,
            "req_valid": 0,
            "req_ready": 0,
            "uncache_state": owner._WAIT_RESP,
            "instr_resp_valid": 1,
            "instr_resp_corrupt": 0,
            "instr_resp_denied": 0,
        }
    )
    state["nc_active"] = True
    owner._sample_mmio(recorder, 2, snapshot, state)
    owner._sample_nc(recorder, 2, snapshot, state)
    snapshot.update(
        {
            "uncache_state": owner._IDLE,
            "instr_resp_valid": 0,
            "resp_valid": 1,
        }
    )
    owner._sample_mmio(recorder, 3, snapshot, state)
    owner._sample_nc(recorder, 3, snapshot, state)

    assert (owner.NC_OWNER_GROUP, "nc_leaf_011") in recorder.hits
    assert (owner.MMIO_OWNER_GROUP, "mmio_leaf_020") not in recorder.hits


def test_mmio_fsm_leaves_follow_current_ifu_uncache_state_machine():
    snapshot = _empty_snapshot()
    snapshot.update(
        {
            "s2_valid": 1,
            "s2_req_uncache": 1,
            "s2_pmp_mmio": 1,
            "s2_pbmt": 0,
            "uncache_state": owner._WAIT_LAST_COMMIT,
            "is_first": 1,
            "ifu_stall": 0,
            "req_ready": 0,
        }
    )
    recorder = _Recorder()
    state = _state(recorder)
    owner._sample_mmio(recorder, 1, snapshot, state)
    state["previous_uncache_state"] = owner._WAIT_LAST_COMMIT

    snapshot.update(
        {
            "s2_valid": 0,
            "s2_req_uncache": 0,
            "uncache_state": owner._SEND_REQ,
        }
    )
    owner._sample_mmio(recorder, 2, snapshot, state)
    state["previous_uncache_state"] = owner._SEND_REQ

    snapshot["uncache_state"] = owner._WAIT_RESP
    snapshot["instr_resp_valid"] = 0
    owner._sample_mmio(recorder, 3, snapshot, state)

    for leaf in (12, 16, 18, 19):
        assert (owner.MMIO_OWNER_GROUP, f"mmio_leaf_{leaf:03d}") in recorder.hits


def test_mmio_redirect_cancels_waiting_response_path():
    snapshot = _empty_snapshot()
    snapshot.update(
        {
            "uncache_state": owner._WAIT_RESP,
            "backend_redirect": 1,
            "to_valid": 0,
            "to_ready": 1,
        }
    )
    recorder = _Recorder()
    state = _state(recorder)
    state["mmio_active"] = True

    owner._sample_mmio(recorder, 1, snapshot, state)

    assert (owner.MMIO_OWNER_GROUP, "mmio_leaf_015") in recorder.hits


def test_mmio_backend_redirect_masks_ftq_writeback_on_response():
    snapshot = _empty_snapshot()
    snapshot.update(
        {
            "uncache_state": owner._IDLE,
            "backend_redirect": 1,
            "resp_valid": 1,
            "uncache_redirect": 1,
            "wb_redirect": 0,
        }
    )
    recorder = _Recorder()
    state = _state(recorder)
    state["mmio_active"] = True

    owner._sample_mmio(recorder, 1, snapshot, state)

    assert (owner.MMIO_OWNER_GROUP, "mmio_leaf_037") in recorder.hits
