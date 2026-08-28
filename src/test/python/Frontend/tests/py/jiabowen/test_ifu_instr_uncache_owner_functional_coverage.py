from env.funcov.py.ifu import instr_uncache_owner_funcov as protocol
from env.funcov.py.ifu import mmio_nc_owner_funcov as owner


class _Recorder:
    def __init__(self, signals=None):
        self.hits = set()
        self.evidence = []
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


def test_tl_user_attributes_must_match_entry_and_cover_mmio_and_nc_modes():
    snapshot = _snapshot()
    snapshot.update(
        {
            "req_valid": 1,
            "req_ready": 1,
            "req_is_mmio": 1,
            "req_pbmt": 0,
            "tl_a_valid": 0,
            "tl_a_ready": 1,
            "tl_a_mem_back_type_mm": 0,
            "tl_a_mem_page_type_nc": 0,
        }
    )
    recorder = _Recorder()
    _sample(recorder, 1, snapshot)
    assert not any(_hit(recorder, index) for index in (36, 37, 38))

    snapshot.update({"req_valid": 0, "req_ready": 0, "tl_a_valid": 1})
    _sample(recorder, 2, snapshot)
    assert _hit(recorder, 36)
    assert _hit(recorder, 37)
    assert not _hit(recorder, 38)

    snapshot.update(
        {
            "req_valid": 1,
            "req_ready": 1,
            "req_is_mmio": 0,
            "req_pbmt": 1,
            "tl_a_valid": 0,
            "tl_a_mem_back_type_mm": 1,
            "tl_a_mem_page_type_nc": 1,
        }
    )
    _sample(recorder, 3, snapshot)
    snapshot.update({"req_valid": 0, "req_ready": 0, "tl_a_valid": 1})
    _sample(recorder, 4, snapshot)
    assert not _hit(recorder, 38)

    recorder = _Recorder()
    snapshot["tl_a_mem_page_type_nc"] = 0
    snapshot.update({"req_valid": 1, "req_ready": 1, "tl_a_valid": 0})
    _sample(recorder, 1, snapshot)
    snapshot.update({"req_valid": 0, "req_ready": 0, "tl_a_valid": 1})
    _sample(recorder, 2, snapshot)
    assert not _hit(recorder, 37)
