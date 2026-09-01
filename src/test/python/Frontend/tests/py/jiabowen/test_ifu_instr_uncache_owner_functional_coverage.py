import ast
import inspect

from env.funcov.py.ifu import instr_uncache_owner_funcov as protocol
from env.funcov.py.ifu import mmio_nc_owner_funcov as owner
from env.support import fold_pc


class _Recorder:
    def __init__(self, signals=None):
        self.hits = set()
        self.evidence = []
        self.risk_observations = []
        self.signals = {} if signals is None else dict(signals)

    def _read_first_dut_signal(self, _dut, names):
        return next((self.signals[name] for name in names if name in self.signals), None)

    def mark(self, group, bin_name, _cycle, evidence):
        self.hits.add((group, bin_name))
        self.evidence.append((group, bin_name, dict(evidence)))


def _snapshot():
    return owner._snapshot(_Recorder(), object())


def _sample(recorder, cycle, snapshot):
    protocol.sample_instr_uncache_owner_coverage(recorder, cycle, snapshot)


def _hit(recorder, index):
    return (
        protocol.INSTR_UNCACHE_OWNER_GROUP,
        f"instruncache_leaf_{index:03d}",
    ) in recorder.hits


def test_protocol_sampler_contract_has_all_38_canonical_leaves():
    assert protocol.INSTR_UNCACHE_OWNER_LEAF_COUNT == 38
    assert len(protocol.INSTR_UNCACHE_OWNER_SAMPLER_BIN_KEYS) == 38
    assert all(
        group == protocol.INSTR_UNCACHE_OWNER_GROUP
        for group, _ in protocol.INSTR_UNCACHE_OWNER_SAMPLER_BIN_KEYS
    )


def test_bin1104_leaf11_has_bin_specific_runtime_mark_site():
    tree = ast.parse(inspect.getsource(protocol))
    literal_mark_indices = {
        int(node.args[1].value)
        for node in ast.walk(tree)
        if isinstance(node, ast.Call)
        and isinstance(node.func, ast.Name)
        and node.func.id == "_mark"
        and len(node.args) >= 2
        and isinstance(node.args[1], ast.Constant)
        and isinstance(node.args[1].value, int)
    }
    assert 11 in literal_mark_indices


def test_tl_a_stall_requires_each_observed_field_to_remain_stable():
    snapshot = _snapshot()
    snapshot.update(
        {
            "tl_a_valid": 1,
            "tl_a_ready": 0,
            "tl_a_addr": 0x2000,
            "tl_a_mem_back_type_mm": 1,
            "tl_a_mem_page_type_nc": 1,
        }
    )
    recorder = _Recorder()
    _sample(recorder, 1, snapshot)
    assert not any(_hit(recorder, index) for index in (1, 2, 3))

    _sample(recorder, 2, snapshot)
    assert all(_hit(recorder, index) for index in (1, 2, 3))

    changed = dict(snapshot)
    changed["tl_a_addr"] += 8
    changed["tl_a_mem_page_type_nc"] = 0
    recorder = _Recorder()
    _sample(recorder, 1, snapshot)
    _sample(recorder, 2, changed)
    assert not _hit(recorder, 1)
    assert _hit(recorder, 2)
    assert not _hit(recorder, 3)


def test_tl_a_stall_missing_user_attributes_do_not_default_to_hits():
    snapshot = _snapshot()
    snapshot.update(
        {
            "tl_a_valid": 1,
            "tl_a_ready": 0,
            "tl_a_addr": 0x2000,
            "tl_a_mem_back_type_mm": None,
            "tl_a_mem_page_type_nc": None,
        }
    )
    recorder = _Recorder()

    _sample(recorder, 1, snapshot)
    _sample(recorder, 2, snapshot)

    assert _hit(recorder, 1)
    assert not _hit(recorder, 2)
    assert not _hit(recorder, 3)


def test_tl_a_stall_user_attribute_changes_reject_corresponding_hits():
    snapshot = _snapshot()
    snapshot.update(
        {
            "tl_a_valid": 1,
            "tl_a_ready": 0,
            "tl_a_addr": 0x2000,
            "tl_a_mem_back_type_mm": 1,
            "tl_a_mem_page_type_nc": 1,
        }
    )
    changed = dict(snapshot)
    changed["tl_a_mem_back_type_mm"] = 0
    changed["tl_a_mem_page_type_nc"] = 0
    recorder = _Recorder()

    _sample(recorder, 1, snapshot)
    _sample(recorder, 2, changed)

    assert _hit(recorder, 1)
    assert not _hit(recorder, 2)
    assert not _hit(recorder, 3)


def test_snapshot_resolves_v3_tilelink_user_attribute_names():
    recorder = _Recorder(
        {
            "auto_inner_instrUncache_client_out_a_bits_user_memBackType_MM": 1,
            "auto_inner_instrUncache_client_out_a_bits_user_memPageType_NC": 0,
        }
    )
    snapshot = owner._snapshot(recorder, object())
    assert snapshot["tl_a_mem_back_type_mm"] == 1
    assert snapshot["tl_a_mem_page_type_nc"] == 0


def test_tl_a_fire_must_be_followed_by_entry_waiting_for_d():
    snapshot = _snapshot()
    snapshot.update(
        {
            "entry_state": protocol._ENTRY_REFILL_REQ,
            "tl_a_valid": 1,
            "tl_a_ready": 1,
        }
    )
    recorder = _Recorder()
    _sample(recorder, 1, snapshot)
    snapshot.update(
        {
            "entry_state": protocol._ENTRY_REFILL_RESP,
            "tl_a_valid": 0,
            "tl_a_ready": 1,
        }
    )
    _sample(recorder, 2, snapshot)
    assert _hit(recorder, 4)


def test_wfi_retract_requires_stalled_request_and_same_address_recovery():
    snapshot = _snapshot()
    snapshot.update(
        {
            "tl_a_valid": 1,
            "tl_a_ready": 0,
            "tl_a_addr": 0x4000,
            "wfi_req": 0,
        }
    )
    recorder = _Recorder()
    _sample(recorder, 1, snapshot)
    snapshot.update({"tl_a_valid": 0, "wfi_req": 1})
    _sample(recorder, 2, snapshot)
    assert not _hit(recorder, 9)
    snapshot.update({"tl_a_valid": 1, "wfi_req": 0, "tl_a_ready": 0})
    _sample(recorder, 3, snapshot)
    assert _hit(recorder, 9)

    recorder = _Recorder()
    snapshot.update({"tl_a_valid": 1, "wfi_req": 0, "tl_a_addr": 0x4000})
    _sample(recorder, 1, snapshot)
    snapshot.update({"tl_a_valid": 0, "wfi_req": 1})
    _sample(recorder, 2, snapshot)
    snapshot.update({"tl_a_valid": 1, "wfi_req": 0, "tl_a_addr": 0x5000})
    _sample(recorder, 3, snapshot)
    assert not _hit(recorder, 9)


def test_redirected_wait_d_requires_old_response_completion_and_new_identity():
    def drive(*, d_before_redirect=False, leak_old_identity=False):
        recorder = _Recorder()
        snapshot = _snapshot()
        snapshot.update(
            {
                "req_valid": 1,
                "req_ready": 1,
                "req_is_mmio": 1,
                "req_pbmt": 0,
                "s2_ftq_flag": 0,
                "s2_ftq_value": 7,
                "s2_instr_pc": 0x4000,
            }
        )
        _sample(recorder, 1, snapshot)
        snapshot.update(
            {
                "req_valid": 0,
                "entry_state": protocol._ENTRY_REFILL_REQ,
                "tl_a_valid": 1,
                "tl_a_ready": 1,
                "tl_a_addr": 0x8000,
            }
        )
        _sample(recorder, 2, snapshot)
        snapshot.update(
            {
                "entry_state": protocol._ENTRY_REFILL_RESP,
                "tl_a_valid": 0,
                "tl_d_valid": int(d_before_redirect),
                "tl_d_data": 0x00000013,
                "tl_d_corrupt": 0,
                "tl_d_denied": 0,
            }
        )
        _sample(recorder, 3, snapshot)
        snapshot.update({"backend_redirect": 1, "ifu_flush": 1, "tl_d_valid": 0})
        _sample(recorder, 4, snapshot)
        snapshot.update({"backend_redirect": 0, "ifu_flush": 0, "tl_d_valid": 1})
        _sample(recorder, 5, snapshot)
        snapshot.update(
            {
                "entry_state": protocol._ENTRY_IDLE,
                "tl_d_valid": 0,
                "instr_resp_valid": 1,
                "instr_resp_data": 0x00000013,
                "instr_resp_corrupt": 0,
                "instr_resp_denied": 0,
                "instr_resp_need_resend": 0,
            }
        )
        _sample(recorder, 6, snapshot)
        snapshot.update(
            {
                "instr_resp_valid": 0,
                "to_valid": 1,
                "to_ready": 1,
                "to_enq": 1,
                "to_ftq_flag": 0,
                "to_ftq_value": 7 if leak_old_identity else 8,
                "to_pc": 0x4000 if leak_old_identity else 0x4040,
                "to_exception": 0,
            }
        )
        _sample(recorder, 7, snapshot)
        if leak_old_identity:
            snapshot.update({"to_ftq_value": 8, "to_pc": 0x4040})
            _sample(recorder, 8, snapshot)
        return recorder

    assert _hit(drive(), 10)
    assert not _hit(drive(d_before_redirect=True), 10)
    assert not _hit(drive(leak_old_identity=True), 10)


def _drive_redirected_cross_8b_resend(
    *,
    redirect_kind="backend",
    redirect_before_second_a=False,
    leak_old_identity=False,
    omit_second_d=False,
):
    recorder = _Recorder()
    snapshot = _snapshot()
    first_addr = 0x803
    second_addr = ((first_addr >> 2) + 1) << 3
    old_identity = (0, 7, 0x4000)

    snapshot.update(
        {
            "req_valid": 1,
            "req_ready": 1,
            "req_is_mmio": 1,
            "req_pbmt": 0,
            "s2_ftq_flag": old_identity[0],
            "s2_ftq_value": old_identity[1],
            "s2_instr_pc": old_identity[2],
        }
    )
    _sample(recorder, 1, snapshot)
    snapshot.update(
        {
            "req_valid": 0,
            "entry_state": protocol._ENTRY_REFILL_REQ,
            "entry_req_addr": first_addr,
            "tl_a_valid": 1,
            "tl_a_ready": 1,
            "tl_a_addr": first_addr << 1,
        }
    )
    _sample(recorder, 2, snapshot)
    snapshot.update(
        {
            "entry_state": protocol._ENTRY_REFILL_RESP,
            "tl_a_valid": 0,
            "tl_d_valid": 1,
            "tl_d_data": 0x0003 << 48,
            "tl_d_corrupt": 0,
            "tl_d_denied": 0,
        }
    )
    _sample(recorder, 3, snapshot)
    snapshot.update(
        {
            "entry_state": 3,
            "tl_d_valid": 0,
            "instr_resp_valid": 1,
            "instr_resp_data": 0x0003,
            "instr_resp_corrupt": 0,
            "instr_resp_denied": 0,
            "instr_resp_need_resend": 1,
        }
    )
    _sample(recorder, 4, snapshot)

    snapshot.update(
        {
            "entry_state": protocol._ENTRY_REFILL_RESP
            if redirect_before_second_a
            else protocol._ENTRY_REFILL_REQ,
            "entry_resending": 1,
            "instr_resp_valid": 0,
            "tl_a_valid": int(not redirect_before_second_a),
            "tl_a_ready": 1,
            "tl_a_addr": second_addr,
            "backend_redirect": int(
                redirect_before_second_a and redirect_kind == "backend"
            ),
            "checker_redirect": int(
                redirect_before_second_a and redirect_kind == "checker"
            ),
            "wb_redirect": int(
                redirect_before_second_a and redirect_kind == "checker"
            ),
            "ifu_flush": int(redirect_before_second_a),
        }
    )
    _sample(recorder, 5, snapshot)
    if redirect_before_second_a:
        snapshot.update(
            {
                "entry_state": protocol._ENTRY_REFILL_REQ,
                "tl_a_valid": 1,
                "backend_redirect": 0,
                "checker_redirect": 0,
                "wb_redirect": 0,
                "ifu_flush": 0,
            }
        )
        _sample(recorder, 6, snapshot)

    redirect_cycle = 7 if redirect_before_second_a else 6
    snapshot.update(
        {
            "entry_state": protocol._ENTRY_REFILL_RESP,
            "tl_a_valid": 0,
            "backend_redirect": int(
                not redirect_before_second_a and redirect_kind == "backend"
            ),
            "checker_redirect": int(
                not redirect_before_second_a and redirect_kind == "checker"
            ),
            "wb_redirect": int(
                not redirect_before_second_a and redirect_kind == "checker"
            ),
            "ifu_flush": int(not redirect_before_second_a),
        }
    )
    _sample(recorder, redirect_cycle, snapshot)
    snapshot.update(
        {
            "backend_redirect": 0,
            "checker_redirect": 0,
            "wb_redirect": 0,
            "ifu_flush": 0,
            "tl_d_valid": int(not omit_second_d),
            "tl_d_data": 0x00000013,
        }
    )
    _sample(recorder, redirect_cycle + 1, snapshot)
    snapshot.update(
        {
            "entry_state": 3,
            "entry_resending": 0,
            "tl_d_valid": 0,
            "instr_resp_valid": 1,
            "instr_resp_data": 0x00130003,
            "instr_resp_need_resend": 0,
        }
    )
    _sample(recorder, redirect_cycle + 2, snapshot)
    snapshot.update(
        {
            "entry_state": protocol._ENTRY_IDLE,
            "instr_resp_valid": 0,
            "to_valid": 1,
            "to_ready": 1,
            "to_enq": 1,
            "to_ftq_flag": old_identity[0],
            "to_ftq_value": old_identity[1] if leak_old_identity else 8,
            "to_pc": old_identity[2] if leak_old_identity else 0x4040,
            "to_exception": 0,
        }
    )
    _sample(recorder, redirect_cycle + 3, snapshot)
    if leak_old_identity:
        snapshot.update({"to_ftq_value": 8, "to_pc": 0x4040})
        _sample(recorder, redirect_cycle + 4, snapshot)
    return recorder, snapshot, redirect_cycle


def test_redirected_cross_8b_resend_requires_natural_completion_and_recovery():
    for redirect_kind in ("backend", "checker"):
        recorder, _snapshot_after, _redirect_cycle = _drive_redirected_cross_8b_resend(
            redirect_kind=redirect_kind
        )
        assert _hit(recorder, 11)
        evidence = next(item[2] for item in recorder.evidence if item[1].endswith("011"))
        assert evidence["redirect_kind"] == redirect_kind
        assert evidence["second_a_fire_before_redirect"] is True
        assert evidence["second_d_after_redirect"] is True
        assert evidence["old_identity_delivery_suppressed"] is True


def test_redirected_cross_8b_resend_rejects_incomplete_or_leaking_episodes():
    recorder, _snapshot_after, _redirect_cycle = _drive_redirected_cross_8b_resend(
        redirect_before_second_a=True
    )
    assert not _hit(recorder, 11)

    recorder, _snapshot_after, _redirect_cycle = _drive_redirected_cross_8b_resend(
        omit_second_d=True
    )
    assert not _hit(recorder, 11)

    recorder, _snapshot_after, _redirect_cycle = _drive_redirected_cross_8b_resend(
        leak_old_identity=True
    )
    assert not _hit(recorder, 11)
    assert any(
        item["event"] == "ifu_instruncache_redirected_resend_old_identity_leak"
        for item in recorder.risk_observations
    )


def test_redirected_cross_8b_resend_timeout_and_reset_clear_pending_state():
    recorder, snapshot, redirect_cycle = _drive_redirected_cross_8b_resend(
        omit_second_d=True
    )
    snapshot.update(
        {
            "to_valid": 0,
            "to_ready": 0,
            "to_enq": 0,
            "entry_resending": 0,
            "instr_resp_valid": 0,
        }
    )
    _sample(
        recorder,
        redirect_cycle + protocol._REDIRECTED_RESEND_TIMEOUT_CYCLES + 1,
        snapshot,
    )
    assert recorder._ifu_instr_uncache_owner_state["redirected_resend_pending"] is None
    assert any(
        item["event"] == "ifu_instruncache_redirected_resend_timeout"
        for item in recorder.risk_observations
    )

    recorder, _snapshot_after, _redirect_cycle = _drive_redirected_cross_8b_resend(
        omit_second_d=True
    )
    assert recorder._ifu_instr_uncache_owner_state["redirected_resend_pending"]
    protocol.reset_instr_uncache_owner_coverage_state(recorder)
    assert recorder._ifu_instr_uncache_owner_state["redirected_resend_pending"] is None


def test_d_response_fields_are_checked_at_instruncache_response():
    snapshot = _snapshot()
    snapshot.update(
        {
            "entry_state": protocol._ENTRY_REFILL_RESP,
            "entry_req_addr": 0x100,
            "entry_resending": 0,
            "tl_d_valid": 1,
            "tl_d_data": 0x00000013,
            "tl_d_corrupt": 1,
            "tl_d_denied": 1,
        }
    )
    recorder = _Recorder()
    _sample(recorder, 1, snapshot)
    snapshot.update(
        {
            "entry_state": 3,
            "entry_resending": 0,
            "tl_d_valid": 0,
            "instr_resp_valid": 1,
            "instr_resp_data": 0x00000013,
            "instr_resp_corrupt": 1,
            "instr_resp_denied": 1,
            "instr_resp_need_resend": 0,
        }
    )
    _sample(recorder, 2, snapshot)
    assert all(_hit(recorder, index) for index in (5, 6, 7))


def test_combined_fault_leaves_require_both_response_flags():
    for resending, index in ((0, 16), (1, 20)):
        for response_denied, expected_hit in ((0, False), (1, True)):
            snapshot = _snapshot()
            snapshot.update(
                {
                    "entry_state": protocol._ENTRY_REFILL_RESP,
                    "entry_req_addr": 0x803,
                    "entry_resending": resending,
                    "tl_d_valid": 1,
                    "tl_d_data": 0x0003 << 48,
                    "tl_d_corrupt": 1,
                    "tl_d_denied": 1,
                }
            )
            recorder = _Recorder()
            _sample(recorder, 1, snapshot)
            snapshot.update(
                {
                    "entry_state": 3,
                    "entry_resending": 0,
                    "tl_d_valid": 0,
                    "instr_resp_valid": 1,
                    "instr_resp_corrupt": 1,
                    "instr_resp_denied": response_denied,
                    "instr_resp_need_resend": 0,
                }
            )
            _sample(recorder, 2, snapshot)
            assert _hit(recorder, index) is expected_hit


def test_non_crossing_rvi_leaf_requires_all_three_halfword_offsets():
    snapshot = _snapshot()
    recorder = _Recorder()
    for cycle, byte_offset in enumerate((0, 2, 4), start=1):
        entry_addr = 0x800 + byte_offset // 2
        snapshot.update(
            {
                "entry_state": protocol._ENTRY_REFILL_RESP,
                "entry_req_addr": entry_addr,
                "entry_resending": 0,
                "tl_d_valid": 1,
                "tl_d_data": 0x00000013 << (byte_offset * 8),
                "tl_d_corrupt": 0,
                "tl_d_denied": 0,
            }
        )
        _sample(recorder, cycle * 2 - 1, snapshot)
        snapshot.update(
            {
                "entry_state": 3,
                "tl_d_valid": 0,
                "instr_resp_valid": 1,
                "instr_resp_data": 0x00000013,
                "instr_resp_corrupt": 0,
                "instr_resp_denied": 0,
                "instr_resp_need_resend": 0,
            }
        )
        _sample(recorder, cycle * 2, snapshot)
        assert _hit(recorder, 13) is (byte_offset == 4)
        snapshot["instr_resp_valid"] = 0


def test_cross_8b_resend_checks_second_address_and_single_rvi_delivery():
    first_addr = 0x803
    second_addr = ((first_addr >> 2) + 1) << 3
    snapshot = _snapshot()
    snapshot.update(
        {
            "entry_state": protocol._ENTRY_REFILL_RESP,
            "entry_req_addr": first_addr,
            "entry_resending": 0,
            "tl_d_valid": 1,
                "tl_d_data": 0x0003 << 48,
            "tl_d_corrupt": 0,
            "tl_d_denied": 0,
        }
    )
    recorder = _Recorder()
    _sample(recorder, 1, snapshot)

    snapshot.update(
        {
            "entry_state": protocol._ENTRY_REFILL_REQ,
            "entry_resending": 1,
            "tl_d_valid": 0,
            "tl_a_valid": 1,
            "tl_a_ready": 0,
            "tl_a_addr": second_addr,
        }
    )
    _sample(recorder, 2, snapshot)
    assert _hit(recorder, 12)
    assert _hit(recorder, 17)

    snapshot.update(
        {
            "entry_state": protocol._ENTRY_REFILL_RESP,
            "tl_a_valid": 0,
            "tl_d_valid": 1,
            "tl_d_data": 0x00000013,
        }
    )
    _sample(recorder, 3, snapshot)
    assert not _hit(recorder, 18)

    snapshot.update(
        {
            "entry_state": 3,
            "entry_resending": 0,
            "tl_d_valid": 0,
            "instr_resp_valid": 1,
            "instr_resp_data": 0x00130003,
            "instr_resp_corrupt": 0,
            "instr_resp_denied": 0,
            "instr_resp_need_resend": 0,
        }
    )
    _sample(recorder, 4, snapshot)
    assert _hit(recorder, 18)
    assert _hit(recorder, 21)

    snapshot.update(
        {
            "entry_state": protocol._ENTRY_IDLE,
            "entry_resending": 0,
            "instr_resp_valid": 0,
            "to_valid": 1,
            "to_ready": 1,
            "to_enq": 1,
            "to_is_rvc": 0,
        }
    )
    _sample(recorder, 5, snapshot)
    assert _hit(recorder, 21)


def test_page_tail_need_resend_does_not_count_an_internal_second_beat():
    snapshot = _snapshot()
    snapshot.update(
        {
            "entry_state": protocol._ENTRY_REFILL_RESP,
            "entry_req_addr": 0x7FF,
            "entry_resending": 0,
            "tl_d_valid": 1,
            "tl_d_data": 0x0013 << 48,
            "tl_d_corrupt": 0,
            "tl_d_denied": 0,
        }
    )
    recorder = _Recorder()
    _sample(recorder, 1, snapshot)
    snapshot.update(
        {
            "entry_state": 3,
            "tl_d_valid": 0,
            "instr_resp_valid": 1,
            "instr_resp_data": 0x0013,
            "instr_resp_corrupt": 0,
            "instr_resp_denied": 0,
            "instr_resp_need_resend": 1,
        }
    )
    _sample(recorder, 2, snapshot)
    assert _hit(recorder, 8)
    assert _hit(recorder, 22)

    snapshot.update({"entry_state": protocol._ENTRY_IDLE, "instr_resp_valid": 0})
    _sample(recorder, 3, snapshot)
    assert _hit(recorder, 23)

    snapshot.update(
        {
            "uncache_redirect": 1,
            "resp_need_resend": 1,
            "resp_data": 0x0013,
            "uncache_pc": 0x7FF,
        }
    )
    _sample(recorder, 4, snapshot)
    assert not _hit(recorder, 26)

    snapshot.update(
        {
            "uncache_redirect": 0,
            "resp_need_resend": 0,
            "prev_end_half": 1,
            "prev_half_data": 0x0013,
            "prev_half_pc": 0x7FF,
        }
    )
    _sample(recorder, 5, snapshot)
    assert _hit(recorder, 26)

    snapshot.update({"to_uncache_valid": 1, "to_uncache_ready": 1})
    _sample(recorder, 6, snapshot)
    assert _hit(recorder, 27)

    snapshot.update(
        {
            "prev_end_half": 0,
            "to_uncache_valid": 0,
            "to_valid": 1,
            "to_ready": 1,
            "to_enq": 1,
            "to_is_rvc": 0,
            "to_exception": 0,
            "to_pc": 0x800,
            "s2_uncache_data": 0x00000013,
        }
    )
    _sample(recorder, 7, snapshot)
    assert not _hit(recorder, 28)
    snapshot["to_pc"] = 0x7FF
    _sample(recorder, 8, snapshot)
    assert _hit(recorder, 28)


def test_page_tail_fault_requires_legal_corrupt_response_and_no_resend():
    for denied in (0, 1):
        snapshot = _snapshot()
        snapshot.update(
            {
                "entry_state": protocol._ENTRY_REFILL_RESP,
                "entry_req_addr": 0x7FF,
                "entry_resending": 0,
                "tl_d_valid": 1,
                "tl_d_data": 0x0003 << 48,
                "tl_d_corrupt": 1,
                "tl_d_denied": denied,
            }
        )
        recorder = _Recorder()
        _sample(recorder, 1, snapshot)
        snapshot.update(
            {
                "entry_state": 3,
                "tl_d_valid": 0,
                "instr_resp_valid": 1,
                "instr_resp_corrupt": 1,
                "instr_resp_denied": denied,
                "instr_resp_need_resend": 0,
            }
        )
        _sample(recorder, 2, snapshot)
        assert _hit(recorder, 25)

        recorder = _Recorder()
        snapshot.update(
            {
                "entry_state": protocol._ENTRY_REFILL_RESP,
                "tl_d_valid": 1,
                "instr_resp_valid": 0,
            }
        )
        _sample(recorder, 1, snapshot)
        snapshot.update(
            {
                "entry_state": 3,
                "tl_d_valid": 0,
                "instr_resp_valid": 1,
                "instr_resp_need_resend": 1,
            }
        )
        _sample(recorder, 2, snapshot)
        assert not _hit(recorder, 25)


def test_cross_page_half_flush_waits_for_old_response_and_blocks_old_delivery():
    snapshot = _snapshot()
    recorder = _Recorder()
    snapshot.update(
        {
            "entry_state": protocol._ENTRY_REFILL_RESP,
            "entry_req_addr": 0x7FF,
            "entry_resending": 0,
            "tl_d_valid": 1,
            "tl_d_data": 0x0013 << 48,
            "tl_d_corrupt": 0,
            "tl_d_denied": 0,
        }
    )
    _sample(recorder, 1, snapshot)
    snapshot.update(
        {
            "entry_state": 3,
            "tl_d_valid": 0,
            "instr_resp_valid": 1,
            "instr_resp_data": 0x0013,
            "instr_resp_corrupt": 0,
            "instr_resp_denied": 0,
            "instr_resp_need_resend": 1,
        }
    )
    _sample(recorder, 2, snapshot)
    snapshot.update(
        {
            "entry_state": protocol._ENTRY_IDLE,
            "instr_resp_valid": 0,
            "uncache_redirect": 1,
            "resp_need_resend": 1,
            "resp_data": 0x0013,
            "uncache_pc": 0x7FF,
        }
    )
    _sample(recorder, 3, snapshot)
    snapshot.update(
        {
            "uncache_redirect": 0,
            "resp_need_resend": 0,
            "prev_end_half": 1,
            "prev_half_data": 0x0013,
            "prev_half_pc": 0x7FF,
            "to_uncache_valid": 1,
            "to_uncache_ready": 1,
            "to_uncache_addr": 0x800,
        }
    )
    _sample(recorder, 4, snapshot)

    snapshot.update(
        {
            "entry_state": protocol._ENTRY_REFILL_RESP,
            "entry_req_addr": 0x800,
            "to_uncache_valid": 0,
            "backend_redirect": 1,
            "s2_valid": 1,
        }
    )
    _sample(recorder, 5, snapshot)
    assert not _hit(recorder, 29)

    snapshot.update(
        {
            "backend_redirect": 0,
            "prev_end_half": 0,
            "prev_half_data": 0,
            "prev_half_pc": 0,
            "s2_valid": 0,
        }
    )
    _sample(recorder, 6, snapshot)
    assert not _hit(recorder, 29)

    snapshot.update({"entry_state": 3, "instr_resp_valid": 1})
    _sample(recorder, 7, snapshot)
    assert _hit(recorder, 29)


def test_first_page_pmp_fault_requires_original_identity_and_no_uncache_request():
    snapshot = _snapshot()
    snapshot.update(
        {
            "s2_valid": 1,
            "s2_req_uncache": 1,
            "s2_use_uncache": 0,
            "s2_exception": 3,
            "s2_pc": 0x7FF,
            "s2_instr_pc": 0x7FF,
            "s2_ftq_flag": 0,
            "s2_ftq_value": 7,
            "to_valid": 1,
            "to_ready": 1,
            "to_enq": 1,
            "to_exception": 3,
            "to_exception_cross_page": 0,
            "to_ftq_flag": 0,
            "to_ftq_value": 7,
            "to_ftq_offset": 0,
            "to_foldpc": fold_pc(0xFFE),
            "prev_end_half": 0,
            "to_uncache_valid": 0,
            "tl_a_valid": 0,
        }
    )
    recorder = _Recorder()
    _sample(recorder, 1, snapshot)
    assert all(_hit(recorder, index) for index in (30, 31, 32))

    wrong_identity = dict(snapshot)
    wrong_identity["to_foldpc"] ^= 1
    recorder = _Recorder()
    _sample(recorder, 1, wrong_identity)
    assert _hit(recorder, 30)
    assert not _hit(recorder, 31)
    assert _hit(recorder, 32)

    wrong_offset = dict(snapshot)
    wrong_offset["to_ftq_offset"] = 1
    recorder = _Recorder()
    _sample(recorder, 1, wrong_offset)
    assert _hit(recorder, 30)
    assert not _hit(recorder, 31)
    assert _hit(recorder, 32)

    leaked_request = dict(snapshot)
    leaked_request["tl_a_valid"] = 1
    recorder = _Recorder()
    _sample(recorder, 1, leaked_request)
    assert not any(_hit(recorder, index) for index in (30, 31, 32))

    illegal = dict(snapshot)
    illegal["to_exception"] = 4
    recorder = _Recorder()
    _sample(recorder, 1, illegal)
    assert not any(_hit(recorder, index) for index in (30, 31, 32))


def test_second_page_exception_matrix_requires_original_half_identity():
    def drive_exception(recorder, cycle, exception_type, *, foldpc_delta=0):
        snapshot = _snapshot()
        snapshot.update(
            {
                "entry_state": protocol._ENTRY_REFILL_RESP,
                "entry_req_addr": 0x7FF,
                "entry_resending": 0,
                "tl_d_valid": 1,
                "tl_d_data": 0x0013 << 48,
                "tl_d_corrupt": 0,
                "tl_d_denied": 0,
            }
        )
        _sample(recorder, cycle, snapshot)
        snapshot.update(
            {
                "entry_state": 3,
                "tl_d_valid": 0,
                "instr_resp_valid": 1,
                "instr_resp_data": 0x0013,
                "instr_resp_corrupt": 0,
                "instr_resp_denied": 0,
                "instr_resp_need_resend": 1,
            }
        )
        _sample(recorder, cycle + 1, snapshot)
        snapshot.update(
            {
                "entry_state": protocol._ENTRY_IDLE,
                "instr_resp_valid": 0,
                "uncache_redirect": 1,
                "resp_need_resend": 1,
                "resp_data": 0x0013,
                "uncache_pc": 0x7FF,
            }
        )
        _sample(recorder, cycle + 2, snapshot)
        snapshot.update(
            {
                "uncache_redirect": 0,
                "resp_need_resend": 0,
                "prev_end_half": 1,
                "prev_half_data": 0x0013,
                "prev_half_pc": 0x7FF,
                "s2_ftq_flag": 0,
                "s2_ftq_value": exception_type,
                "to_valid": 1,
                "to_ready": 1,
                "to_enq": 1,
                "to_exception": exception_type,
                "to_exception_cross_page": 1,
                "to_ftq_flag": 0,
                "to_ftq_value": exception_type,
                "to_ftq_offset": 0,
                "to_foldpc": fold_pc(0xFFE) + foldpc_delta,
                "to_uncache_valid": 0,
                "tl_a_valid": 0,
            }
        )
        _sample(recorder, cycle + 3, snapshot)

    recorder = _Recorder()
    drive_exception(recorder, 1, 3, foldpc_delta=1)
    assert not any(_hit(recorder, index) for index in (33, 34, 35))

    recorder = _Recorder()
    drive_exception(recorder, 10, 3)
    assert _hit(recorder, 33)
    assert not _hit(recorder, 34)
    assert not _hit(recorder, 35)
    drive_exception(recorder, 20, 1)
    assert not _hit(recorder, 34)
    drive_exception(recorder, 30, 2)
    assert _hit(recorder, 34)
    assert not _hit(recorder, 35)


def test_first_page_tl_fault_suppresses_cross_page_refetch_and_illegal_decode():
    def drive(
        *, leaked_request=False, delivered_exception=3, need_resend=0, end_offset=1
    ):
        recorder = _Recorder()
        snapshot = _snapshot()
        snapshot.update(
            {
                "entry_state": protocol._ENTRY_REFILL_RESP,
                "entry_req_addr": 0x7FF,
                "entry_resending": 0,
                "tl_d_valid": 1,
                "tl_d_data": 0x0013 << 48,
                "tl_d_corrupt": 1,
                "tl_d_denied": 1,
                "s2_pc": 0x7FF,
                "s2_instr_pc": 0x7FF,
                "s2_ftq_flag": 0,
                "s2_ftq_value": 9,
                "uncache_pc": 0x7FF,
            }
        )
        _sample(recorder, 1, snapshot)
        snapshot.update(
            {
                "entry_state": 3,
                "tl_d_valid": 0,
                "instr_resp_valid": 1,
                "instr_resp_corrupt": 1,
                "instr_resp_denied": 1,
                "instr_resp_need_resend": need_resend,
                "to_uncache_valid": int(leaked_request),
            }
        )
        _sample(recorder, 2, snapshot)
        snapshot.update(
            {
                "instr_resp_valid": 0,
                "to_uncache_valid": 0,
                "to_valid": 1,
                "to_ready": 1,
                "to_enq": 1,
                "to_pc": 0x7FF,
                "to_ftq_flag": 0,
                "to_ftq_value": 9,
                "to_ftq_offset": end_offset,
                "to_foldpc": fold_pc(0xFFE),
                "to_exception": delivered_exception,
                "to_exception_cross_page": 0,
                "prev_end_half": 0,
            }
        )
        _sample(recorder, 3, snapshot)
        return recorder

    recorder = drive()
    assert _hit(recorder, 35)
    evidence = next(item[2] for item in recorder.evidence if item[1].endswith("035"))
    assert evidence["need_resend_suppressed"] is True
    assert evidence["no_second_page_request"] is True
    assert evidence["illegal_instruction"] is False

    assert not _hit(drive(leaked_request=True), 35)
    assert not _hit(drive(delivered_exception=4), 35)
    assert not _hit(drive(need_resend=1), 35)
    assert not _hit(drive(end_offset=0), 35)
    assert not _hit(drive(end_offset=2), 35)


def test_tl_user_attributes_must_match_entry_and_cover_mmio_and_nc_modes():
    snapshot = _snapshot()
    recorder = _Recorder()

    def drive_transaction(cycle, *, mode, ftq_value, pc):
        is_mmio = int(mode == "mmio")
        pbmt = 0 if is_mmio else 1
        snapshot.update(
            {
                "req_valid": 1,
                "req_ready": 1,
                "req_is_mmio": is_mmio,
                "req_pbmt": pbmt,
                "s2_ftq_flag": 0,
                "s2_ftq_value": ftq_value,
                "s2_instr_pc": pc,
                "tl_a_valid": 0,
                "tl_a_ready": 1,
                "tl_a_mem_back_type_mm": int(not is_mmio),
                "tl_a_mem_page_type_nc": int(pbmt == 1),
                "instr_resp_valid": 0,
                "to_valid": 0,
            }
        )
        _sample(recorder, cycle, snapshot)
        snapshot.update(
            {
                "req_valid": 0,
                "req_ready": 0,
                "tl_a_valid": 1,
                "tl_a_addr": 0x8000 + 8 * ftq_value,
            }
        )
        _sample(recorder, cycle + 1, snapshot)
        snapshot.update({"tl_a_valid": 0, "instr_resp_valid": 1})
        _sample(recorder, cycle + 2, snapshot)
        snapshot.update(
            {
                "instr_resp_valid": 0,
                "to_valid": 1,
                "to_ready": 1,
                "to_enq": 1,
                "to_exception": 0,
                "to_ftq_flag": 0,
                "to_ftq_value": ftq_value,
                "to_pc": pc,
            }
        )
        _sample(recorder, cycle + 3, snapshot)
        snapshot["to_valid"] = 0

    drive_transaction(1, mode="mmio", ftq_value=1, pc=0x4000)
    assert _hit(recorder, 36)
    assert _hit(recorder, 37)
    assert not _hit(recorder, 38)

    drive_transaction(10, mode="nc", ftq_value=2, pc=0x5000)
    assert _hit(recorder, 38)


def test_mixed_attribute_modes_require_completed_distinct_ifu_identities():
    snapshot = _snapshot()
    snapshot.update(
        {
            "req_valid": 1,
            "req_ready": 1,
            "req_is_mmio": 1,
            "req_pbmt": 0,
            "s2_ftq_flag": 0,
            "s2_ftq_value": 1,
            "s2_instr_pc": 0x4000,
            "tl_a_valid": 0,
        }
    )
    recorder = _Recorder()
    _sample(recorder, 1, snapshot)
    snapshot.update({"req_valid": 0, "tl_a_valid": 1, "tl_a_ready": 1})
    _sample(recorder, 2, snapshot)
    snapshot.update({"tl_a_valid": 0, "instr_resp_valid": 1})
    _sample(recorder, 3, snapshot)
    snapshot.update(
        {
            "instr_resp_valid": 0,
            "to_valid": 1,
            "to_ready": 1,
            "to_enq": 1,
            "to_exception": 0,
            "to_ftq_flag": 0,
            "to_ftq_value": 7,
            "to_pc": 0x4000,
        }
    )
    _sample(recorder, 4, snapshot)
    assert not _hit(recorder, 38)

    recorder = _Recorder()
    snapshot.update(
        {
            "req_valid": 1,
            "req_ready": 1,
            "req_is_mmio": 0,
            "req_pbmt": 1,
            "tl_a_valid": 0,
            "tl_a_mem_page_type_nc": 0,
        }
    )
    _sample(recorder, 1, snapshot)
    snapshot.update({"req_valid": 0, "req_ready": 0, "tl_a_valid": 1})
    _sample(recorder, 2, snapshot)
    assert not _hit(recorder, 37)
