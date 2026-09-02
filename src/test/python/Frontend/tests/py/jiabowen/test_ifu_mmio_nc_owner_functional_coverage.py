from env.funcov.py.ifu import mmio_nc_owner_funcov as owner
from env.funcov.recorder import _decode_signal_inventory_name


class _Recorder:
    def __init__(self, signals=None):
        self.signals = dict(signals or {})
        self.hits = set()
        self.evidence = []

    def _read_first_dut_signal(self, _dut, names):
        return next(
            (self.signals[name] for name in names if name in self.signals), None
        )

    def mark(self, group, bin_name, _cycle, _evidence):
        self.hits.add((group, bin_name))
        self.evidence.append((group, bin_name, dict(_evidence)))


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


def test_signal_inventory_name_decoder_handles_yaml_quoted_aliases():
    assert _decode_signal_inventory_name(
        '"uncacheUnit.io_req_bits_isMmio"'
    ) == "uncacheUnit.io_req_bits_isMmio"
    assert _decode_signal_inventory_name(
        "'uncacheUnit.io_req_bits_pbmt'"
    ) == "uncacheUnit.io_req_bits_pbmt"
    assert _decode_signal_inventory_name(
        "Frontend_top.Frontend.inner_ifu.s2_valid"
    ) == "Frontend_top.Frontend.inner_ifu.s2_valid"


def test_snapshot_reads_current_verilator_derived_aliases():
    recorder = _Recorder(
        {
            "uncacheUnit.io_req_ready": 1,
            "uncacheUnit.io_resp_valid": 1,
            "uncacheUnit.io_resp_bits_uncacheData": 0x1234,
            "uncacheUnit.io_ifuStall": 0,
            "uncacheUnit.io_toUncache_req_valid": 1,
            "uncacheUnit.uncachePAddr_addr": 0x2080,
            "uncacheUnit.io_emptyAfter": 1,
            "Frontend_top.Frontend.inner_ifu.s2_uncacheData": 0xAABBCCDD,
            "Frontend_top.Frontend.inner_ifu.s2_alignShiftNum": 0,
            "Frontend_top.Frontend.inner_ifu.s2_alignedInstrPcVec_0_addr": 0x400,
            "Frontend_top.Frontend.inner_ifu.io_toIBuffer_bits_enqEnable": 1,
            "Frontend_top.Frontend.inner_ifu.io_toIBuffer_bits_pc_0_addr": 0x400,
            "Frontend_top.Frontend.inner_ifu.__Vtogcov__s2_flush": 1,
            "Frontend_top.Frontend.inner_ifu.__Vtogcov__wbRedirect_valid": 1,
            "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_ready": 1,
            "Frontend_top.Frontend.inner_ifu.io_toIBuffer_valid": 1,
            "Frontend_top.Frontend.inner_instrUncache.entries_0.state": owner._IDLE,
            "Frontend_top.Frontend.inner_ifu.uncacheUnit.__Vtogcov__io_resp_valid": 1,
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
    timing = owner.read_nc_timing_runtime_snapshot(recorder, object())

    assert snapshot["req_ready"] == 1
    assert snapshot["resp_valid"] == 1
    assert snapshot["resp_data"] == 0x1234
    assert snapshot["ifu_stall"] == 0
    assert snapshot["to_uncache_valid"] == 1
    assert snapshot["to_uncache_ready"] == 1
    assert snapshot["to_uncache_addr"] == 0x2080
    assert snapshot["empty_after"] == 1
    assert snapshot["s2_uncache_data"] == 0xAABBCCDD
    assert snapshot["s2_instr_pc"] == 0x400
    assert snapshot["to_pc"] == 0x400
    assert snapshot["ifu_flush"] == 1
    assert snapshot["checker_redirect"] == 1
    assert snapshot["to_valid"] == 1
    assert snapshot["to_ready"] == 1
    assert snapshot["instr_resp_valid"] == 1
    assert snapshot["instr_resp_data"] == 0x5678
    assert snapshot["instr_resp_corrupt"] == 0
    assert snapshot["instr_resp_denied"] == 1
    assert snapshot["instr_resp_need_resend"] == 0
    assert snapshot["ibuffer_empty"] == 1
    assert snapshot["waylookup_valid"] == 1
    assert snapshot["waymask_0"] == 3
    assert snapshot["waymask_1"] == 0
    assert timing["ifu_flush"] == 1
    assert timing["resp_valid"] == 1
    assert timing["instr_resp_valid"] == 1
    assert timing["to_valid"] == 1
    assert timing["to_ready"] == 1
    assert timing["checker_redirect"] == 1


def test_nc_pending_reconstructs_optimized_sv_wire_from_runtime_state():
    snapshot = _empty_snapshot()
    assert not owner.derive_nc_pending(snapshot, nc_active=False)

    snapshot.update(
        {
            "s2_valid": 1,
            "s2_req_uncache": 1,
            "s2_pmp_mmio": 0,
            "s2_pbmt": owner._PBMT_NC,
        }
    )
    assert owner.derive_nc_pending(snapshot, nc_active=False)

    snapshot.update(
        {
            "s2_valid": 0,
            "uncache_busy": 1,
            "uncache_state": owner._WAIT_RESP,
        }
    )
    assert owner.derive_nc_pending(snapshot, nc_active=False)

    snapshot.update({"uncache_busy": 0, "uncache_state": owner._IDLE})
    assert owner.derive_nc_pending(snapshot, nc_active=True)


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
            "to_uncache_valid": 1,
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


def test_send_request_stall_uses_observed_valid_and_nc_witnesses_canonical_bin():
    snapshot = _empty_snapshot()
    snapshot.update(
        {
            "uncache_state": owner._SEND_REQ,
            "ifu_stall": 1,
            "to_uncache_valid": 1,
        }
    )
    recorder = _Recorder()
    state = _state(recorder)
    state["nc_active"] = True

    owner._sample_nc(recorder, 1, snapshot, state)
    assert (owner.NC_OWNER_GROUP, "nc_leaf_008") not in recorder.hits
    assert (owner.MMIO_OWNER_GROUP, "mmio_leaf_017") not in recorder.hits

    snapshot["to_uncache_valid"] = 0
    owner._sample_nc(recorder, 2, snapshot, state)
    assert (owner.NC_OWNER_GROUP, "nc_leaf_008") in recorder.hits
    assert (owner.MMIO_OWNER_GROUP, "mmio_leaf_017") in recorder.hits


def test_nc_first_page_iaf_uses_ftq_identity_and_ignores_debug_pc():
    snapshot = _empty_snapshot()
    snapshot.update(
        {
            "s2_valid": 1,
            "s2_req_uncache": 1,
            "s2_use_uncache": 0,
            "s2_pmp_mmio": 0,
            "s2_pbmt": owner._PBMT_NC,
            "s2_exception": 3,
            "s2_pc": 0x7FF,
            "s2_instr_pc": 0x7FF,
            "s2_ftq_flag": 0,
            "s2_ftq_value": 9,
            "req_valid": 0,
            "to_uncache_valid": 0,
            "tl_a_valid": 0,
            "to_valid": 1,
            "to_ready": 1,
            "to_exception": 3,
            "to_pc": 0,
            "to_ftq_flag": 0,
            "to_ftq_value": 9,
            "to_ftq_offset": 0,
        }
    )
    recorder = _Recorder()
    state = _state(recorder)

    owner._sample_nc(recorder, 1, snapshot, state)
    assert (owner.NC_OWNER_GROUP, "nc_leaf_032") in recorder.hits
    assert (owner.NC_OWNER_GROUP, "nc_leaf_030") in recorder.hits

    recorder = _Recorder()
    snapshot["to_ftq_value"] = 10
    owner._sample_nc(recorder, 2, snapshot, _state(recorder))
    assert (owner.NC_OWNER_GROUP, "nc_leaf_030") not in recorder.hits


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


def _checker_redirect_snapshot(*, younger_stage=None, same_ftq=False):
    snapshot = _empty_snapshot()
    snapshot.update(
        {
            "checker_redirect": 1,
            "wb_path_valid": 1,
            "wb_redirect": 1,
            "ifu_flush": 1,
            "backend_redirect": 0,
            "wb_ftq_flag": 0,
            "wb_ftq_value": 4,
            "wb_pc": 0x3E0,
            "uncache_state": owner._IDLE,
            "uncache_busy": 0,
            "req_valid": 0,
            "req_ready": 1,
            "to_uncache_valid": 0,
            "to_uncache_ready": 1,
            "tl_a_valid": 0,
            "tl_a_ready": 1,
            "instr_resp_valid": 0,
            "resp_valid": 0,
            "to_valid": 0,
            "to_ready": 1,
        }
    )
    younger_ftq = 4 if same_ftq else 5
    if younger_stage == "s1":
        snapshot.update(
            {
                "s1_valid": 1,
                "s1_flush": 1,
                "s1_req_uncache": 1,
                "s1_pmp_mmio": 0,
                "s1_pbmt": owner._PBMT_NC,
                "s1_pc": 0x400,
                "s1_paddr": 0x2000,
                "s1_ftq_flag": 0,
                "s1_ftq_value": younger_ftq,
            }
        )
    elif younger_stage == "s2":
        snapshot.update(
            {
                "s2_valid": 1,
                "s2_req_uncache": 1,
                "s2_use_uncache": 1,
                "s2_pmp_mmio": 0,
                "s2_pbmt": owner._PBMT_NC,
                "s2_pc": 0x400,
                "s2_paddr": 0x2000,
                "s2_ftq_flag": 0,
                "s2_ftq_value": younger_ftq,
                "s2_wb_not_flush": int(same_ftq),
                "req_valid": 1,
                "req_ready": 1,
            }
        )
    return snapshot


def _recovery_snapshot():
    snapshot = _empty_snapshot()
    snapshot.update(
        {
            "s2_valid": 1,
            "s2_req_uncache": 1,
            "s2_use_uncache": 1,
            "s2_pmp_mmio": 0,
            "s2_pbmt": owner._PBMT_NC,
            "s2_pc": 0x500,
            "s2_paddr": 0x2080,
            "s2_ftq_flag": 0,
            "s2_ftq_value": 6,
            "checker_redirect": 0,
            "wb_path_valid": 0,
            "wb_redirect": 0,
            "ifu_flush": 0,
            "backend_redirect": 0,
            "req_valid": 1,
            "req_ready": 1,
            "to_uncache_valid": 0,
            "to_uncache_ready": 1,
            "tl_a_valid": 0,
            "tl_a_ready": 1,
            "instr_resp_valid": 0,
            "resp_valid": 0,
            "to_valid": 0,
            "to_ready": 1,
        }
    )
    return snapshot


def _complete_recovery(
    recorder,
    state,
    *,
    first_cycle=2,
    recovery_ftq=6,
    recovery_pc=0x500,
    recovery_paddr=0x2080,
):
    snapshot = _recovery_snapshot()
    snapshot.update(
        {
            "s2_ftq_value": recovery_ftq,
            "s2_pc": recovery_pc,
            "s2_paddr": recovery_paddr,
        }
    )
    owner._sample_nc(recorder, first_cycle, snapshot, state)

    request = _empty_snapshot()
    request.update(
        {
            "to_uncache_valid": 1,
            "to_uncache_ready": 1,
            "to_uncache_addr": recovery_paddr,
            "tl_a_valid": 1,
            "tl_a_ready": 1,
            "tl_a_addr": (recovery_paddr << 1) & ~0x7,
            "to_valid": 0,
            "to_ready": 1,
        }
    )
    owner._sample_nc(recorder, first_cycle + 1, request, state)

    response = _empty_snapshot()
    response.update(
        {
            "instr_resp_valid": 1,
            "resp_valid": 1,
            "to_valid": 0,
            "to_ready": 1,
        }
    )
    owner._sample_nc(recorder, first_cycle + 2, response, state)

    delivery = _empty_snapshot()
    delivery.update(
        {
            "to_valid": 1,
            "to_ready": 1,
            "to_ftq_flag": 0,
            "to_ftq_value": recovery_ftq,
            "to_pc": recovery_pc,
        }
    )
    owner._sample_nc(recorder, first_cycle + 3, delivery, state)


def test_nc_checker_redirect_without_younger_nc_does_not_hit_on_later_recovery():
    snapshot = _checker_redirect_snapshot()
    recorder = _Recorder()
    state = _state(recorder)

    owner._sample_nc(recorder, 1, snapshot, state)
    assert state["nc_checker_redirect_pending"] is None
    _complete_recovery(recorder, state)
    assert (owner.NC_OWNER_GROUP, "nc_leaf_013") not in recorder.hits


def test_nc_checker_redirect_records_s1_flush_but_keeps_parent_partial():
    recorder = _Recorder()
    state = _state(recorder)
    owner._sample_nc(recorder, 1, _checker_redirect_snapshot(younger_stage="s1"), state)

    pending = state["nc_checker_redirect_pending"]
    assert pending["younger_nc_present_in_s1"]
    assert not pending["younger_nc_present_in_s2"]
    assert not pending["younger_nc_internal_req_races_flush"]
    _complete_recovery(recorder, state)
    assert (owner.NC_OWNER_GROUP, "nc_leaf_013") not in recorder.hits


def test_nc_checker_redirect_s2_internal_request_flush_wins_and_recovery_hits():
    recorder = _Recorder()
    state = _state(recorder)
    owner._sample_nc(recorder, 1, _checker_redirect_snapshot(younger_stage="s2"), state)

    pending = state["nc_checker_redirect_pending"]
    assert pending["younger_nc_present_in_s2"]
    assert pending["younger_nc_internal_req_races_flush"]
    _complete_recovery(recorder, state)
    assert (owner.NC_OWNER_GROUP, "nc_leaf_013") in recorder.hits
    evidence = next(
        item[2]
        for item in recorder.evidence
        if item[:2] == (owner.NC_OWNER_GROUP, "nc_leaf_013")
    )
    assert evidence["old_nc_no_instruncache_request"]
    assert evidence["old_nc_no_tl_a_fire"]
    assert evidence["old_nc_no_ibuffer_delivery"]
    assert evidence["recovery_nc_new_identity"]
    assert evidence["recovery_nc_request_and_delivery"]


def test_nc_checker_redirect_keeps_overlap_when_redirect_is_asserted_two_cycles():
    recorder = _Recorder()
    state = _state(recorder)
    redirect = _checker_redirect_snapshot(younger_stage="s2")

    owner._sample_nc(recorder, 1, redirect, state)
    owner._sample_nc(recorder, 2, redirect, state)

    pending = state["nc_checker_redirect_pending"]
    assert pending is not None
    assert pending["redirect_cycle"] == 1
    assert pending["failure_reasons"] == []

    _complete_recovery(recorder, state, first_cycle=3)
    assert (owner.NC_OWNER_GROUP, "nc_leaf_013") in recorder.hits


def test_nc_checker_redirect_accepts_reused_recovery_ftq_with_new_address():
    recorder = _Recorder()
    state = _state(recorder)
    owner._sample_nc(recorder, 1, _checker_redirect_snapshot(younger_stage="s2"), state)

    _complete_recovery(recorder, state, recovery_ftq=5)

    assert (owner.NC_OWNER_GROUP, "nc_leaf_013") in recorder.hits
    evidence = next(
        item[2]
        for item in recorder.evidence
        if item[:2] == (owner.NC_OWNER_GROUP, "nc_leaf_013")
    )
    assert evidence["old_nc_ftq"] == evidence["recovery_nc_ftq"]
    assert evidence["recovery_nc_identity_changes"] == ["pc", "paddr"]


def test_nc_checker_redirect_can_overlap_active_younger_nc_state():
    recorder = _Recorder()
    state = _state(recorder)
    state["nc_active"] = True
    owner._sample_nc(recorder, 1, _checker_redirect_snapshot(younger_stage="s2"), state)

    assert state["nc_checker_redirect_pending"] is not None
    assert state["nc_checker_redirect_pending"][
        "younger_nc_internal_req_races_flush"
    ]


def test_nc_checker_redirect_old_path_side_effect_prevents_hit():
    recorder = _Recorder()
    state = _state(recorder)
    owner._sample_nc(recorder, 1, _checker_redirect_snapshot(younger_stage="s2"), state)

    leaked = _empty_snapshot()
    leaked.update(
        {
            "to_uncache_valid": 1,
            "to_uncache_ready": 1,
            "to_uncache_addr": 0x2000,
            "to_valid": 0,
            "to_ready": 1,
        }
    )
    owner._sample_nc(recorder, 2, leaked, state)
    _complete_recovery(recorder, state, first_cycle=3)
    assert (owner.NC_OWNER_GROUP, "nc_leaf_013") not in recorder.hits
    assert "old_nc_instruncache_request" in state[
        "nc_checker_redirect_pending"
    ]["failure_reasons"]


def test_backend_redirect_and_same_ftq_wb_not_flush_do_not_count_as_bin1067():
    recorder = _Recorder()
    state = _state(recorder)
    backend = _checker_redirect_snapshot(younger_stage="s2")
    backend["backend_redirect"] = 1
    owner._sample_nc(recorder, 1, backend, state)
    assert state["nc_checker_redirect_pending"] is None

    same_ftq = _checker_redirect_snapshot(younger_stage="s2", same_ftq=True)
    owner._sample_nc(recorder, 2, same_ftq, state)
    assert state["nc_checker_redirect_pending"] is None
    assert (owner.NC_OWNER_GROUP, "nc_leaf_013") not in recorder.hits


def test_nc_checker_redirect_requires_distinct_recovery_identity_and_address():
    recorder = _Recorder()
    state = _state(recorder)
    owner._sample_nc(recorder, 1, _checker_redirect_snapshot(younger_stage="s2"), state)

    recovery = _recovery_snapshot()
    recovery.update(
        {
            "s2_ftq_value": 5,
            "s2_pc": 0x400,
            "s2_paddr": 0x2000,
        }
    )
    owner._sample_nc(recorder, 2, recovery, state)
    assert state["nc_checker_redirect_pending"]["recovery"] is None
    assert "recovery_nc_identity_not_distinct" in state[
        "nc_checker_redirect_pending"
    ]["failure_reasons"]
    assert (owner.NC_OWNER_GROUP, "nc_leaf_013") not in recorder.hits


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
