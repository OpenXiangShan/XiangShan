from env.funcov.py.ifu import instr_uncache_owner_funcov as protocol
from env.funcov.py.ifu import mmio_nc_owner_funcov as owner


class _Recorder:
    def __init__(self):
        self.hits = set()
        self.evidence = []

    def _read_first_dut_signal(self, _dut, _names):
        return None

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
            "tl_d_data": 0x0003 << 48,
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
            "instr_resp_data": 0x0003,
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


def test_tl_user_attributes_must_match_entry_and_cover_mmio_and_nc_modes():
    snapshot = _snapshot()
    snapshot.update(
        {
            "tl_a_valid": 1,
            "tl_a_ready": 1,
            "entry_mem_back_type_mm": 0,
            "entry_mem_page_type_nc": 0,
            "tl_a_mem_back_type_mm": 0,
            "tl_a_mem_page_type_nc": 0,
        }
    )
    recorder = _Recorder()
    _sample(recorder, 1, snapshot)
    assert _hit(recorder, 36)
    assert _hit(recorder, 37)
    assert not _hit(recorder, 38)

    snapshot.update(
        {
            "entry_mem_back_type_mm": 1,
            "entry_mem_page_type_nc": 1,
            "tl_a_mem_back_type_mm": 1,
            "tl_a_mem_page_type_nc": 1,
        }
    )
    _sample(recorder, 2, snapshot)
    assert not _hit(recorder, 38)

    recorder = _Recorder()
    snapshot["tl_a_mem_page_type_nc"] = 0
    _sample(recorder, 1, snapshot)
    assert not _hit(recorder, 37)
