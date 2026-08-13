from types import SimpleNamespace

from env.funcov.py.icache import (
    ICACHE_MISSUNIT_SAMPLER_BIN_KEYS,
    ICACHE_MAINPIPE_SAMPLER_BIN_KEYS,
    ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS,
    ICACHE_WAYLOOKUP_SAMPLER_BIN_KEYS,
    ICACHE_HITMISS_SAMPLER_BIN_KEYS,
    sample_icache_missunit_coverage,
    sample_icache_mainpipe_coverage,
    sample_icache_prefetchpipe_coverage,
    sample_icache_waylookup_coverage,
    sample_icache_hitmiss_coverage,
)
from env.funcov.py.icache.icache_mainpipe_funcov import (
    _DATA_BANKS,
    _MAIN,
    _SIGNALS,
)
from env.funcov.py.icache.icache_prefetchpipe_funcov import _PREFETCH_SIGNALS
from env.funcov.py.icache.icache_waylookup_funcov import _SIGNALS as _WAYLOOKUP_SIGNALS
from env.funcov.py.icache.icache_missunit_funcov import _ICACHE, _MISS, _TOP
from env.funcov.py.icache.icache_hitmiss_funcov import (
    _MAIN as _HITMISS_MAIN,
    _MISS as _HITMISS_MISS,
    _SIGNALS as _HITMISS_SIGNALS,
)


class _Signal:
    def __init__(self, value=0):
        self.value = int(value)


class _Dut:
    def set(self, name, value):
        setattr(self, str(name), _Signal(value))


class _Recorder:
    def __init__(self):
        self.env = SimpleNamespace(dut=_Dut())
        self.hits = set()

    @staticmethod
    def _read_first_dut_signal(dut, names):
        for name in names:
            signal = getattr(dut, name, None)
            if signal is not None:
                return int(signal.value)
        return None

    def mark(self, group, bin_name, cycle, evidence, *, coverpoint=None):
        del cycle, evidence
        self.hits.add((group, coverpoint, bin_name))

    def set_key(self, key, value):
        self.env.dut.set(_SIGNALS[key][0], value)

    def set_prefetch_key(self, key, value):
        self.env.dut.set(_PREFETCH_SIGNALS[key][0], value)

    def set_waylookup_key(self, key, value):
        self.env.dut.set(_WAYLOOKUP_SIGNALS[key][0], value)

    def set_waylookup_signal(self, key, value, index=0):
        self.env.dut.set(_WAYLOOKUP_SIGNALS[key][index], value)

    def set_missunit_signal(self, name, value):
        self.env.dut.set(name, value)

    def set_hitmiss_key(self, key, value):
        self.env.dut.set(_HITMISS_SIGNALS[key][0], value)


def _hit(recorder, group, bin_name):
    return any(hit[0] == group and hit[2] == bin_name for hit in recorder.hits)


def test_icache_mainpipe_sampler_contract_has_one_key_per_leaf():
    assert len(ICACHE_MAINPIPE_SAMPLER_BIN_KEYS) == 42
    assert len(set(ICACHE_MAINPIPE_SAMPLER_BIN_KEYS)) == 42


def test_icache_prefetchpipe_sampler_contract_has_one_key_per_leaf():
    assert len(ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS) == 33
    assert len(set(ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS)) == 33


def test_icache_missunit_sampler_contract_has_one_key_per_leaf():
    assert len(ICACHE_MISSUNIT_SAMPLER_BIN_KEYS) == 31
    assert len(set(ICACHE_MISSUNIT_SAMPLER_BIN_KEYS)) == 31


def test_icache_waylookup_sampler_contract_has_one_key_per_leaf():
    assert len(ICACHE_WAYLOOKUP_SAMPLER_BIN_KEYS) == 42
    assert len(set(ICACHE_WAYLOOKUP_SAMPLER_BIN_KEYS)) == 42


def test_icache_hitmiss_sampler_contract_has_one_key_per_leaf():
    assert len(ICACHE_HITMISS_SAMPLER_BIN_KEYS) == 10
    assert len(set(ICACHE_HITMISS_SAMPLER_BIN_KEYS)) == 10


def _set_mainpipe_single_bank_range(recorder, *, offset: int, mask: list[int]) -> None:
    recorder.set_key("s1_valid", 1)
    recorder.set_key("cross0", 0)
    recorder.env.dut.set(_MAIN + "s1_req_0_vAddr_0_addr", int(offset))
    recorder.env.dut.set(_MAIN + "s1_sramRespValid", int(mask[-1]))
    for bank, value in enumerate(mask[:-1]):
        recorder.env.dut.set(_MAIN + f"s1_bankSramValid_0_{bank}", int(value))


def test_mainpipe_single_line_bank_range_requires_expected_bank_mask():
    recorder = _Recorder()
    _set_mainpipe_single_bank_range(
        recorder,
        offset=0x08,
        mask=[0, 0, 1, 1, 1, 1, 1, 1],
    )

    sample_icache_mainpipe_coverage(recorder, recorder.env, 10)

    assert _hit(recorder, "icache_mainpipe_s1_sram", "single_line_bank_range")

    recorder = _Recorder()
    _set_mainpipe_single_bank_range(
        recorder,
        offset=0x08,
        mask=[0, 1, 1, 1, 1, 1, 1, 1],
    )

    sample_icache_mainpipe_coverage(recorder, recorder.env, 11)

    assert not _hit(recorder, "icache_mainpipe_s1_sram", "single_line_bank_range")


def _set_mainpipe_should_fetch(recorder, values):
    for index, value in enumerate(values):
        recorder.env.dut.set(_MAIN + f"s1_shouldFetch_{index}", int(value))


def _set_mainpipe_single_sram_hit(recorder, *, fetch_finish=1, miss_req=0, should=(0, 0, 0, 0)):
    recorder.set_key("s1_valid", 1)
    recorder.set_key("cross0", 0)
    recorder.set_key("fetch_finish", fetch_finish)
    recorder.set_key("miss_req_valid", miss_req)
    recorder.set_key("pmp_instr", 0)
    _set_mainpipe_should_fetch(recorder, should)
    recorder.env.dut.set(_MAIN + "s1_sramRespValid", 1)
    recorder.env.dut.set(_MAIN + "s1_wayLookupEntry_0_waymask_0", 1)
    recorder.env.dut.set(_MAIN + "s1_hits_0_0", 1)


def test_mainpipe_single_sram_hit_samples_condition_not_completion_checkpoint():
    recorder = _Recorder()
    _set_mainpipe_single_sram_hit(recorder)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 12)

    assert _hit(recorder, "icache_mainpipe_s1_sram", "single_line_sram_hit")

    recorder = _Recorder()
    _set_mainpipe_single_sram_hit(recorder, fetch_finish=0)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 13)
    assert _hit(recorder, "icache_mainpipe_s1_sram", "single_line_sram_hit")

    recorder = _Recorder()
    _set_mainpipe_single_sram_hit(recorder, miss_req=1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 14)
    assert _hit(recorder, "icache_mainpipe_s1_sram", "single_line_sram_hit")

    recorder = _Recorder()
    _set_mainpipe_single_sram_hit(recorder, should=(0, 1, 0, 0))
    sample_icache_mainpipe_coverage(recorder, recorder.env, 15)
    assert _hit(recorder, "icache_mainpipe_s1_sram", "single_line_sram_hit")


def _set_mainpipe_crossline_sram_hit(recorder, *, bank_mask=(1, 1, 1, 1, 1, 1, 1, 1)):
    _set_mainpipe_single_sram_hit(recorder)
    recorder.set_key("cross0", 1)
    recorder.env.dut.set(_MAIN + "s1_sramValid_0_1", 1)
    recorder.env.dut.set(_MAIN + "s1_wayLookupEntry_0_waymask_1", 1)
    recorder.env.dut.set(_MAIN + "s1_hits_0_1", 1)
    recorder.env.dut.set(_MAIN + "s1_req_0_vAddr_0_addr", 0x18)
    for bank, value in enumerate(bank_mask[:-1]):
        recorder.env.dut.set(_MAIN + f"s1_bankSramValid_0_{bank}", int(value))
    recorder.env.dut.set(_MAIN + "s1_sramRespValid", int(bank_mask[-1]))


def test_mainpipe_crossline_sram_bins_sample_conditions_not_checkpoints():
    recorder = _Recorder()
    _set_mainpipe_crossline_sram_hit(recorder)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 16)

    assert _hit(recorder, "icache_mainpipe_s1_sram", "cross_line_dual_sram_hit")
    assert _hit(recorder, "icache_mainpipe_s1_sram", "cross_line_bank_mapping")

    recorder = _Recorder()
    _set_mainpipe_crossline_sram_hit(recorder)
    recorder.env.dut.set(_MAIN + "s1_hits_0_1", 0)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 17)
    assert not _hit(recorder, "icache_mainpipe_s1_sram", "cross_line_dual_sram_hit")

    recorder = _Recorder()
    _set_mainpipe_crossline_sram_hit(recorder, bank_mask=(1, 1, 0, 1, 1, 1, 1, 1))
    sample_icache_mainpipe_coverage(recorder, recorder.env, 18)
    assert _hit(recorder, "icache_mainpipe_s1_sram", "cross_line_bank_mapping")

    recorder = _Recorder()
    _set_mainpipe_crossline_sram_hit(recorder)
    recorder.env.dut.set(_MAIN + "s1_req_0_vAddr_0_addr", 0)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 19)
    assert not _hit(recorder, "icache_mainpipe_s1_sram", "cross_line_bank_mapping")


def _set_mainpipe_dual_request_independent(recorder):
    recorder.set_key("s1_valid", 1)
    recorder.set_key("req1_valid", 1)
    recorder.set_key("cross0", 0)
    recorder.set_key("cross1", 1)
    recorder.set_key("fetch_finish", 1)
    _set_mainpipe_should_fetch(recorder, (0, 0, 0, 0))
    for req in range(2):
        for line in range(2):
            recorder.env.dut.set(_MAIN + f"s1_wayLookupEntry_{req}_waymask_{line}", req + line + 1)
            recorder.env.dut.set(_MAIN + f"s1_hits_{req}_{line}", 0)
    recorder.env.dut.set(_MAIN + "s1_hits_0_0", 1)
    recorder.env.dut.set(_MAIN + "s1_hits_1_0", 1)
    recorder.env.dut.set(_MAIN + "s1_hits_1_1", 1)


def test_mainpipe_dual_request_samples_condition_not_hit_checkpoints():
    recorder = _Recorder()
    _set_mainpipe_dual_request_independent(recorder)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 20)

    assert _hit(recorder, "icache_mainpipe_s1_sram", "dual_request_independent")

    recorder = _Recorder()
    _set_mainpipe_dual_request_independent(recorder)
    recorder.env.dut.set(_MAIN + "s1_hits_0_1", 1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 21)

    assert _hit(recorder, "icache_mainpipe_s1_sram", "dual_request_independent")

    recorder = _Recorder()
    _set_mainpipe_dual_request_independent(recorder)
    for req in range(2):
        for line in range(2):
            recorder.env.dut.set(_MAIN + f"s1_hits_{req}_{line}", 0)
    _set_mainpipe_should_fetch(recorder, (1, 0, 0, 0))
    recorder.set_key("fetch_finish", 1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 22)

    assert _hit(recorder, "icache_mainpipe_s1_sram", "dual_request_independent")

    recorder = _Recorder()
    _set_mainpipe_dual_request_independent(recorder)
    recorder.set_key("cross1", 0)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 23)

    assert not _hit(recorder, "icache_mainpipe_s1_sram", "dual_request_independent")


def _set_single_hit(recorder):
    recorder.set_hitmiss_key("s1_valid", 1)
    recorder.set_hitmiss_key("cross0", 0)
    recorder.env.dut.set(_HITMISS_MAIN + "s1_hits_0_0", 1)
    recorder.env.dut.set(_HITMISS_MAIN + "s1_wayLookupEntry_0_waymask_0", 1)


def test_hitmiss_hit_path_leaves_use_dut_hit_and_protection_conditions():
    recorder = _Recorder()
    _set_single_hit(recorder)
    recorder.set_hitmiss_key("req0_start", 0x1000)
    recorder.set_hitmiss_key("req1_start", 0x2000)
    sample_icache_hitmiss_coverage(recorder, recorder.env, 40)
    assert _hit(recorder, "icache_hit_path", "continuous_same_line_sram_hit")

    recorder = _Recorder()
    recorder.set_hitmiss_key("s1_valid", 1)
    recorder.set_hitmiss_key("cross0", 1)
    recorder.env.dut.set(_HITMISS_MAIN + "s1_hits_0_0", 1)
    recorder.env.dut.set(_HITMISS_MAIN + "s1_hits_0_1", 1)
    recorder.env.dut.set(_HITMISS_MAIN + "s1_wayLookupEntry_0_waymask_0", 1)
    recorder.env.dut.set(_HITMISS_MAIN + "s1_wayLookupEntry_0_waymask_1", 1)
    sample_icache_hitmiss_coverage(recorder, recorder.env, 41)
    assert _hit(recorder, "icache_hit_path", "continuous_cross_line_sram_hit")

    recorder = _Recorder()
    _set_single_hit(recorder)
    recorder.set_hitmiss_key("req1_valid", 1)
    recorder.set_hitmiss_key("req0_start", 0x1000)
    recorder.set_hitmiss_key("req1_start", 0x2000)
    recorder.env.dut.set(_HITMISS_MAIN + "s1_hits_1_0", 1)
    recorder.env.dut.set(_HITMISS_MAIN + "s1_wayLookupEntry_1_waymask_0", 1)
    sample_icache_hitmiss_coverage(recorder, recorder.env, 42)
    assert _hit(recorder, "icache_hit_path", "dual_request_independent_hit")

    recorder = _Recorder()
    _set_single_hit(recorder)
    recorder.set_hitmiss_key("itlb_exception", 1)
    recorder.set_hitmiss_key("pmp_instr", 1)
    sample_icache_hitmiss_coverage(recorder, recorder.env, 43)
    assert _hit(recorder, "icache_hit_path", "hit_itlb_exception")
    assert _hit(recorder, "icache_hit_path", "hit_pmp_exception")


def test_hitmiss_concurrent_fetch_hit_and_prefetch_miss_requires_different_key():
    recorder = _Recorder()
    for key, value in {
        "fetch_valid": 1,
        "fetch_hit": 1,
        "fetch_paddr": 0x100,
        "fetch_vset": 2,
        "prefetch_valid": 1,
        "prefetch_hit": 0,
        "prefetch_paddr": 0x200,
        "prefetch_vset": 3,
    }.items():
        recorder.set_hitmiss_key(key, value)
    sample_icache_hitmiss_coverage(recorder, recorder.env, 44)
    assert _hit(recorder, "icache_miss_path", "fetch_hit_prefetch_miss_concurrent")


def _set_clean_fetch_refill(recorder, paddr=0x300, vset=4):
    for key, value in {
        "last_fire_next": 1,
        "id_next": 0,
        "corrupt_reg": 0,
        "denied_reg": 0,
    }.items():
        recorder.set_hitmiss_key(key, value)
    recorder.env.dut.set(_HITMISS_MISS + "allMshr_0.valid", 1)
    recorder.env.dut.set(_HITMISS_MISS + "allMshr_0.flush", 0)
    recorder.env.dut.set(_HITMISS_MISS + "allMshr_0.fencei", 0)
    recorder.env.dut.set(_HITMISS_MISS + "allMshr_0.blkPAddr", paddr)
    recorder.env.dut.set(_HITMISS_MISS + "allMshr_0.vSetIdx", vset)


def test_hitmiss_refill_associations_cover_prefetch_and_later_fetch_hit():
    recorder = _Recorder()
    _set_clean_fetch_refill(recorder)
    sample_icache_hitmiss_coverage(recorder, recorder.env, 45)

    recorder.set_hitmiss_key("last_fire_next", 0)
    recorder.set_hitmiss_key("prefetch_valid", 1)
    recorder.set_hitmiss_key("prefetch_hit", 1)
    recorder.set_hitmiss_key("prefetch_paddr", 0x300)
    recorder.set_hitmiss_key("prefetch_vset", 4)
    sample_icache_hitmiss_coverage(recorder, recorder.env, 46)
    assert _hit(recorder, "icache_miss_path", "fetch_refill_prefetch_hit")

    recorder.set_hitmiss_key("prefetch_valid", 0)
    recorder.set_hitmiss_key("fetch_valid", 1)
    recorder.set_hitmiss_key("fetch_hit", 1)
    recorder.set_hitmiss_key("fetch_paddr", 0x300)
    recorder.set_hitmiss_key("fetch_vset", 4)
    recorder.env.dut.set(_HITMISS_MISS + "allMshr_0.valid", 0)
    sample_icache_hitmiss_coverage(recorder, recorder.env, 47)
    assert _hit(recorder, "icache_miss_path", "refill_then_fetch_hit")


def test_hitmiss_merge_and_plru_bins_use_current_missunit_state():
    recorder = _Recorder()
    for key, value in {
        "fetch_valid": 1,
        "fetch_hit": 1,
        "fetch_paddr": 0x500,
        "fetch_vset": 5,
    }.items():
        recorder.set_hitmiss_key(key, value)
    recorder.env.dut.set(_HITMISS_MISS + "allMshr_0.valid", 1)
    recorder.env.dut.set(_HITMISS_MISS + "allMshr_0.blkPAddr", 0x500)
    recorder.env.dut.set(_HITMISS_MISS + "allMshr_0.vSetIdx", 5)
    sample_icache_hitmiss_coverage(recorder, recorder.env, 48)
    assert _hit(recorder, "icache_miss_path", "continuous_fetch_miss_merge")

    recorder = _Recorder()
    recorder.set_hitmiss_key("fetch_valid", 1)
    recorder.set_hitmiss_key("fetch_hit", 0)
    recorder.set_hitmiss_key("victim_req", 1)
    recorder.set_hitmiss_key("victim_way", 2)
    sample_icache_hitmiss_coverage(recorder, recorder.env, 49)
    assert _hit(recorder, "icache_miss_path", "plru_victim_on_miss")


def test_missunit_same_key_merge_accepts_rtl_prefetch_hit():
    recorder = _Recorder()
    for name, value in {
        _MAIN + "__Vtogcov__io_missReq_valid": 1,
        _MAIN + "__Vtogcov__io_missReq_bits_blkPAddr": 0x100,
        _MAIN + "__Vtogcov__io_missReq_bits_vSetIdx": 3,
        _MISS + "io_prefetchReq_valid": 1,
        _MISS + "__Vtogcov__io_prefetchReq_bits_blkPAddr": 0x100,
        _MISS + "__Vtogcov__io_prefetchReq_bits_vSetIdx": 3,
        _MISS + "fetchHit": 0,
        # RTL sets this for the same-cycle fetch/prefetch merge.
        _MISS + "prefetchHit": 1,
    }.items():
        recorder.set_missunit_signal(name, value)

    sample_icache_missunit_coverage(recorder, recorder.env, 1)

    assert _hit(recorder, "icache_missunit_request", "same_key_fetch_prefetch_merge")


def test_missunit_refill_counts_grant_data_opcode_5():
    recorder = _Recorder()
    base = _TOP + "auto_inner_icache_client_out_d_bits_"
    recorder.set_missunit_signal(_TOP + "auto_inner_icache_client_out_d_valid", 1)
    recorder.set_missunit_signal(base + "opcode", 5)
    recorder.set_missunit_signal(base + "corrupt", 0)
    recorder.set_missunit_signal(base + "denied", 0)

    sample_icache_missunit_coverage(recorder, recorder.env, 2)
    sample_icache_missunit_coverage(recorder, recorder.env, 3)

    recorder.set_missunit_signal(_TOP + "auto_inner_icache_client_out_d_valid", 0)
    recorder.set_missunit_signal(_MISS + "lastFireNext", 1)
    recorder.set_missunit_signal(_MISS + "idNext", 0)
    recorder.set_missunit_signal(_MISS + "allMshr_0.valid", 1)
    recorder.set_missunit_signal(_MISS + "allMshr_0.flush", 0)
    recorder.set_missunit_signal(_MISS + "allMshr_0.fencei", 0)
    recorder.set_missunit_signal(_MISS + "corruptReg", 0)
    recorder.set_missunit_signal(_MISS + "deniedReg", 0)
    sample_icache_missunit_coverage(recorder, recorder.env, 4)

    assert _hit(recorder, "icache_missunit_refill", "clean_doublebeat_refill_write")


def test_missunit_fifo_issue_order_requires_source_to_match_fifo_head():
    recorder = _Recorder()
    for name, value in {
        _TOP + "auto_inner_icache_client_out_a_valid": 1,
        _TOP + "auto_inner_icache_client_out_a_ready": 1,
        _TOP + "auto_inner_icache_client_out_a_bits_source": 4,
        _MISS + "priorityFIFO.io_deq_ready": 1,
        _MISS + "priorityFIFO.deqPtr_value": 0,
        _MISS + "priorityFIFO.regFiles_0": 1,
    }.items():
        recorder.set_missunit_signal(name, value)

    sample_icache_missunit_coverage(recorder, recorder.env, 5)
    assert not _hit(recorder, "icache_missunit_acquire", "prefetch_fifo_issue_order")

    recorder = _Recorder()
    for name, value in {
        _TOP + "auto_inner_icache_client_out_a_valid": 1,
        _TOP + "auto_inner_icache_client_out_a_ready": 1,
        _TOP + "auto_inner_icache_client_out_a_bits_source": 5,
        _MISS + "priorityFIFO.io_deq_ready": 1,
        _MISS + "priorityFIFO.deqPtr_value": 0,
        _MISS + "priorityFIFO.regFiles_0": 1,
    }.items():
        recorder.set_missunit_signal(name, value)

    sample_icache_missunit_coverage(recorder, recorder.env, 6)
    assert _hit(recorder, "icache_missunit_acquire", "prefetch_fifo_issue_order")


def test_missunit_sram_suppression_requires_valid_response():
    recorder = _Recorder()
    for name, value in {
        _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush": 1,
        _MISS + "lastFireNext": 1,
        _MISS + "idNext": 0,
        _MISS + "allMshr_0.flush": 0,
        _MISS + "allMshr_0.fencei": 0,
    }.items():
        recorder.set_missunit_signal(name, value)

    sample_icache_missunit_coverage(recorder, recorder.env, 7)
    assert not _hit(recorder, "icache_missunit_flush", "redirect_suppresses_sram_write")

    recorder.set_missunit_signal(_MISS + "allMshr_0.valid", 1)
    sample_icache_missunit_coverage(recorder, recorder.env, 8)
    assert _hit(recorder, "icache_missunit_flush", "redirect_suppresses_sram_write")


def test_missunit_source_route_uses_final_beat_context():
    recorder = _Recorder()
    for index in (0, 1):
        recorder.set_missunit_signal(_MISS + f"allMshr_{index}.valid", 1)
        recorder.set_missunit_signal(_MISS + f"allMshr_{index}.issue", 1)
        recorder.set_missunit_signal(_MISS + f"allMshr_{index}.flush", 0)
        recorder.set_missunit_signal(_MISS + f"allMshr_{index}.fencei", 0)
    recorder.set_missunit_signal(_MISS + "lastFire", 1)
    recorder.set_missunit_signal(_TOP + "auto_inner_icache_client_out_d_bits_source", 1)
    sample_icache_missunit_coverage(recorder, recorder.env, 9)

    recorder.set_missunit_signal(_MISS + "lastFire", 0)
    recorder.set_missunit_signal(_MISS + "lastFireNext", 1)
    recorder.set_missunit_signal(_MISS + "idNext", 1)
    sample_icache_missunit_coverage(recorder, recorder.env, 10)

    assert _hit(recorder, "icache_missunit_refill", "source_routes_refill")


def test_waylookup_empty_state_samples_reset_empty():
    recorder = _Recorder()
    for key, value in {
        "empty": 1,
        "read_flag": 0,
        "read_value": 0,
        "write_flag": 0,
        "write_value": 0,
        "exception_valid": 0,
    }.items():
        recorder.set_waylookup_key(key, value)

    sample_icache_waylookup_coverage(recorder, recorder.env, 1)

    assert _hit(recorder, "icache_waylookup_queue", "reset_empty")


def test_waylookup_dual_dequeue_uses_real_two_fetch_observation():
    recorder = _Recorder()
    for key, value in {
        "to_valid": 1,
        "to_ready": 1,
        "info1_valid": 1,
        "real_two": 1,
    }.items():
        recorder.set_waylookup_key(key, value)

    sample_icache_waylookup_coverage(recorder, recorder.env, 2)

    assert _hit(recorder, "icache_waylookup_read", "dual_entry_dequeue")


def test_waylookup_capacity_samples_full_queue_without_checkpoint_signal():
    recorder = _Recorder()
    for key, value in {
        "num_valid": 32,
        "write0_valid": 1,
        "write0_ready": 0,
    }.items():
        recorder.set_waylookup_key(key, value)

    sample_icache_waylookup_coverage(recorder, recorder.env, 3)

    assert _hit(recorder, "icache_waylookup_capacity", "full_blocks_write")


def test_waylookup_update_uses_mainpipe_response_and_readptr_relative_entry():
    recorder = _Recorder()
    for key, value in {
        "read_value": 3,
        "num_valid": 2,
        "update_valid": 1,
    }.items():
        recorder.set_waylookup_key(key, value)

    recorder.set_waylookup_signal("update_updated", 1, index=3 * 2)
    recorder.set_waylookup_signal("update_same_tag", 1, index=3 * 2)
    sample_icache_waylookup_coverage(recorder, recorder.env, 4)

    assert _hit(recorder, "icache_waylookup_update", "update_head")
    assert _hit(recorder, "icache_waylookup_update", "update_same_tag")


def test_waylookup_corrupt_update_requires_no_metadata_update():
    recorder = _Recorder()
    recorder.set_waylookup_key("read_value", 0)
    recorder.set_waylookup_key("num_valid", 1)
    recorder.set_waylookup_key("update_valid", 1)
    recorder.set_waylookup_key("update_corrupt", 1)

    sample_icache_waylookup_coverage(recorder, recorder.env, 5)

    assert _hit(recorder, "icache_waylookup_update", "update_corrupt_ignored")


def test_waylookup_exception_dequeue_requires_exception_pointer_at_readptr():
    recorder = _Recorder()
    for key, value in {
        "to_valid": 1,
        "to_ready": 1,
        "num_valid": 3,
        "read_flag": 0,
        "read_value": 0,
        "exception_valid": 1,
        "exception_ptr_flag": 0,
        "exception_ptr_value": 2,
    }.items():
        recorder.set_waylookup_key(key, value)

    sample_icache_waylookup_coverage(recorder, recorder.env, 6)
    assert not _hit(recorder, "icache_waylookup_exception", "exception_dequeue")

    recorder.set_waylookup_key("exception_ptr_value", 0)
    sample_icache_waylookup_coverage(recorder, recorder.env, 7)
    assert _hit(recorder, "icache_waylookup_exception", "exception_dequeue")


def test_waylookup_exception_written_into_empty_queue_is_not_bypassed():
    recorder = _Recorder()
    for key, value in {
        "empty": 1,
        "write0_valid": 1,
        "write0_ready": 1,
        "write0_exception": 1,
        "exception_valid": 0,
        "to_valid": 0,
    }.items():
        recorder.set_waylookup_key(key, value)
    sample_icache_waylookup_coverage(recorder, recorder.env, 8)

    for key, value in {
        "empty": 1,
        "write0_valid": 0,
        "write0_exception": 0,
        "exception_valid": 1,
        "to_valid": 0,
    }.items():
        recorder.set_waylookup_key(key, value)
    sample_icache_waylookup_coverage(recorder, recorder.env, 9)

    assert _hit(recorder, "icache_waylookup_exception", "exception_no_bypass")


def test_waylookup_flush_suppresses_nonempty_read_output():
    recorder = _Recorder()
    for key, value in {
        "flush": 1,
        "num_valid": 1,
        "to_valid": 0,
    }.items():
        recorder.set_waylookup_key(key, value)

    sample_icache_waylookup_coverage(recorder, recorder.env, 8)

    assert _hit(recorder, "icache_waylookup_flush", "flush_wins_read")


def test_prefetch_ftq_accept_samples_entry_condition():
    recorder = _Recorder()
    for key, value in {
        "from_valid": 1,
        "from_soft": 0,
        "s0_fire": 1,
        # Checkpoint-side observations deliberately disagree.
        "from_ready": 0,
        "way0_valid": 0,
    }.items():
        recorder.set_prefetch_key(key, value)

    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 1)

    assert _hit(recorder, "icache_prefetchpipe_s0_entry", "ftq_accept_all_ready")


def test_prefetch_bpu_compare_requires_match_and_miss_cases():
    recorder = _Recorder()
    for key, value in {
        "from_valid": 1,
        "from_soft": 0,
        "bpu_valid": 1,
        "global_flush": 0,
        "s1_ready": 1,
        "meta_ready": 1,
        "from_ready": 0,
    }.items():
        recorder.set_prefetch_key(key, value)

    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 2)
    assert not _hit(recorder, "icache_prefetchpipe_s0_entry", "bpu_flush_match_only")

    recorder.set_prefetch_key("from_ready", 1)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 3)
    assert _hit(recorder, "icache_prefetchpipe_s0_entry", "bpu_flush_match_only")


def test_prefetch_dual_layout_bin_requires_all_four_legal_cases():
    recorder = _Recorder()
    recorder.set_prefetch_key("s1_valid", 1)

    for cycle, layout in enumerate((1, 2, 4), start=10):
        recorder.set_prefetch_key("s1_two_case", layout)
        sample_icache_prefetchpipe_coverage(recorder, recorder.env, cycle)
    assert not _hit(recorder, "icache_prefetchpipe_s1_meta", "four_dual_layouts")

    recorder.set_prefetch_key("s1_two_case", 8)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 13)
    assert _hit(recorder, "icache_prefetchpipe_s1_meta", "four_dual_layouts")


def test_prefetch_redirect_boundary_requires_ready_low_and_high():
    recorder = _Recorder()
    for key, value in {
        "s2_valid": 1,
        "s2_miss0": 1,
        "global_flush": 1,
        "miss_ready": 0,
    }.items():
        recorder.set_prefetch_key(key, value)

    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 20)
    assert not _hit(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "redirect_flush_ready_boundary",
    )

    recorder.set_prefetch_key("miss_ready", 1)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 21)
    assert _hit(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "redirect_flush_ready_boundary",
    )


def test_prefetch_corrupt_refill_matches_s2_not_new_s1_set():
    recorder = _Recorder()
    for key, value in {
        "s2_valid": 1,
        "s2_miss0": 1,
        "refill_valid": 1,
        "refill_corrupt": 1,
        "refill_vset": 7,
        "s2_set0": 7,
        "s2_set1": 8,
        # A concurrent newer s1 request must not affect the s2 association.
        "s1_set0": 20,
        "s1_set1": 21,
    }.items():
        recorder.set_prefetch_key(key, value)

    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 22)

    assert _hit(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "corrupt_refill_reprefetch",
    )


def test_prefetch_miss_backpressure_then_recovery_samples_state_transition():
    recorder = _Recorder()
    for key, value in {
        "miss_valid": 1,
        "miss_ready": 0,
    }.items():
        recorder.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 23)

    recorder.set_prefetch_key("miss_ready", 1)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 24)
    assert _hit(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "missunit_backpressure_recovery",
    )


def test_prefetch_nonmatching_clean_refill_is_scoped_to_s2_sets():
    recorder = _Recorder()
    for key, value in {
        "s2_valid": 1,
        "s2_set0": 3,
        "s2_set1": 4,
        "refill_valid": 1,
        "refill_corrupt": 0,
        "refill_vset": 9,
    }.items():
        recorder.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 25)
    assert _hit(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "nonmatching_clean_refill_ignored",
    )


def test_single_request_entry_bin():
    recorder = _Recorder()
    for key, value in {
        "from_valid": 1,
        # Deliberately violate checkpoint-side observations.  Coverage is for
        # the stimulus Condition and must still sample the bin.
        "from_ready": 0,
        "data_ready": 1,
        "data_valid": 0,
        "data_req1_valid": 0,
        "io_flush": 0,
        "s0_flush": 0,
        "bpu_valid": 0,
        "s1_ready": 1,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 1)

    assert _hit(recorder, "icache_mainpipe_s0_entry", "single_request_latched")
    assert not _hit(recorder, "icache_mainpipe_s0_entry", "dual_request_data_read")


def test_mainpipe_global_s0_flush_samples_condition_not_checkpoint():
    recorder = _Recorder()
    for key, value in {
        "ftq_valid": 1,
        "from_valid": 1,
        "data_ready": 1,
        "s1_ready": 1,
        "io_flush": 1,
        # Deliberately violate the checkpoint so a checker can report it.
        "s0_flush": 0,
        "from_ready": 1,
        "data_valid": 1,
        "s1_valid": 1,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 2)

    assert _hit(recorder, "icache_mainpipe_s0_flush", "global_flush_cancels_entry")


def test_mainpipe_global_s0_flush_requires_both_inputs_and_io_flush():
    base = {
        "ftq_valid": 1,
        "from_valid": 1,
        "data_ready": 1,
        "s1_ready": 1,
        "io_flush": 1,
        "s0_flush": 1,
    }
    for missing_key in ("ftq_valid", "from_valid", "io_flush"):
        recorder = _Recorder()
        for key, value in {**base, missing_key: 0}.items():
            recorder.set_key(key, value)

        sample_icache_mainpipe_coverage(recorder, recorder.env, 3)

        assert not _hit(
            recorder,
            "icache_mainpipe_s0_flush",
            "global_flush_cancels_entry",
        )


def test_mainpipe_bpu_s0_flush_uses_precise_ftq_match_when_available():
    recorder = _Recorder()
    for key, value in {
        "ftq_valid": 1,
        "from_valid": 1,
        "data_ready": 1,
        "s1_ready": 1,
        "io_flush": 0,
        # Deliberately violate the checkpoint; Condition coverage must hit.
        "s0_flush": 0,
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 3,
        "s0_ftq_flag": 0,
        "s0_ftq_value": 3,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 2)
    assert _hit(recorder, "icache_mainpipe_s0_flush", "bpu_match_cancels_entry")

    recorder = _Recorder()
    for key, value in {
        "ftq_valid": 1,
        "from_valid": 1,
        "data_ready": 1,
        "s1_ready": 1,
        "io_flush": 0,
        "s0_flush": 1,
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 4,
        "s0_ftq_flag": 0,
        "s0_ftq_value": 3,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 3)
    assert not _hit(recorder, "icache_mainpipe_s0_flush", "bpu_match_cancels_entry")


def test_mainpipe_bpu_s0_flush_requires_observable_ftq_pointer_match():
    recorder = _Recorder()
    for key, value in {
        "ftq_valid": 1,
        "from_valid": 1,
        "data_ready": 1,
        "s1_ready": 1,
        "io_flush": 0,
        "s0_flush": 1,
        "bpu_valid": 1,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 4)
    assert not _hit(recorder, "icache_mainpipe_s0_flush", "bpu_match_cancels_entry")


def test_mainpipe_bpu_s0_flush_requires_ftq_valid():
    recorder = _Recorder()
    for key, value in {
        "ftq_valid": 0,
        "from_valid": 1,
        "data_ready": 1,
        "s1_ready": 1,
        "io_flush": 0,
        "s0_flush": 1,
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 3,
        "s0_ftq_flag": 0,
        "s0_ftq_value": 3,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 5)
    assert not _hit(recorder, "icache_mainpipe_s0_flush", "bpu_match_cancels_entry")


def test_mainpipe_bpu_s0_miss_requires_ftq_valid_and_precise_relation():
    recorder = _Recorder()
    for key, value in {
        "ftq_valid": 1,
        "from_valid": 1,
        "data_ready": 1,
        "s1_ready": 1,
        "io_flush": 0,
        # Deliberately violate the checkpoint; Condition coverage must hit.
        "s0_flush": 1,
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 4,
        "s0_ftq_flag": 0,
        "s0_ftq_value": 3,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 6)
    assert _hit(recorder, "icache_mainpipe_s0_flush", "bpu_miss_allows_entry")

    recorder = _Recorder()
    for key, value in {
        "ftq_valid": 0,
        "from_valid": 1,
        "data_ready": 1,
        "s1_ready": 1,
        "io_flush": 0,
        "s0_flush": 0,
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 4,
        "s0_ftq_flag": 0,
        "s0_ftq_value": 3,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 7)
    assert not _hit(recorder, "icache_mainpipe_s0_flush", "bpu_miss_allows_entry")

    recorder = _Recorder()
    for key, value in {
        "ftq_valid": 1,
        "from_valid": 1,
        "data_ready": 1,
        "s1_ready": 1,
        "io_flush": 0,
        "s0_flush": 0,
        "bpu_valid": 1,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 8)
    assert not _hit(recorder, "icache_mainpipe_s0_flush", "bpu_miss_allows_entry")

    recorder = _Recorder()
    for key, value in {
        "ftq_valid": 1,
        "from_valid": 1,
        "data_ready": 1,
        "s1_ready": 1,
        "io_flush": 0,
        "s0_flush": 0,
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 3,
        "s0_ftq_flag": 0,
        "s0_ftq_value": 3,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 9)
    assert not _hit(recorder, "icache_mainpipe_s0_flush", "bpu_miss_allows_entry")


def test_mainpipe_bpu_s0_bins_require_global_flush_low():
    for bin_name, bpu_value in (
        ("bpu_match_cancels_entry", 3),
        ("bpu_miss_allows_entry", 4),
    ):
        recorder = _Recorder()
        for key, value in {
            "ftq_valid": 1,
            "from_valid": 1,
            "data_ready": 1,
            "s1_ready": 1,
            "io_flush": 1,
            "bpu_valid": 1,
            "bpu_flag": 0,
            "bpu_value": bpu_value,
            "s0_ftq_flag": 0,
            "s0_ftq_value": 3,
        }.items():
            recorder.set_key(key, value)

        sample_icache_mainpipe_coverage(recorder, recorder.env, 10)

        assert not _hit(recorder, "icache_mainpipe_s0_flush", bin_name)


def test_mainpipe_bpu_s1_flush_match_and_miss_use_ftq_pointer():
    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "s1_flush": 1,
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 7,
        "s1_ftq_flag": 0,
        "s1_ftq_value": 7,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 5)
    assert _hit(recorder, "icache_mainpipe_s1_flush", "bpu_match_clears_s1")

    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "s1_flush": 0,
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 8,
        "s1_ftq_flag": 0,
        "s1_ftq_value": 7,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 6)
    assert _hit(recorder, "icache_mainpipe_s1_flush", "bpu_miss_keeps_s1")


def test_missunit_backpressure_samples_condition_not_stable_payload_checkpoint():
    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "miss_req_ready": 0,
        "miss_req_vset": 3,
        "miss_req_paddr": 0x80000000,
    }.items():
        recorder.set_key(key, value)
    recorder.env.dut.set(_MAIN + "s1_shouldFetch_0", 1)
    for index in range(1, 4):
        recorder.env.dut.set(_MAIN + f"s1_shouldFetch_{index}", 0)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 10)
    assert not _hit(recorder, "icache_mainpipe_s1_miss", "missunit_backpressure_stable")

    # Payload stability is a Checkpoint.  Change it while completing the
    # Condition sequence (ready low, then recovery); the bin must still hit.
    recorder.set_key("miss_req_vset", 7)
    recorder.set_key("miss_req_paddr", 0x90000000)
    recorder.set_key("miss_req_ready", 1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 11)
    assert _hit(recorder, "icache_mainpipe_s1_miss", "missunit_backpressure_stable")


def test_global_s1_flush_does_not_require_checkpoint_outputs():
    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "s1_flush": 1,
        "io_flush": 1,
        "bpu_valid": 0,
        # These are deliberately the opposite of the expected checkpoint.
        "toifu_valid": 1,
        "s1_fire": 1,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 15)

    assert _hit(recorder, "icache_mainpipe_s1_flush", "global_flush_clears_s1")


def test_ftq_and_waylookup_skew_requires_atomic_join_and_s1_latch():
    recorder = _Recorder()
    for key, value in {
        "ftq_valid": 1,
        "ftq_ready": 0,
        "from_valid": 0,
        "from_ready": 0,
        "data_valid": 0,
        "data_ready": 1,
        "s0_flush": 0,
        "io_flush": 0,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 15)

    assert not _hit(recorder, "icache_mainpipe_s0_entry", "ftq_waylookup_skew")

    for key, value in {
        "from_valid": 1,
        "ftq_ready": 1,
        "from_ready": 1,
        "data_valid": 1,
        "data_ready": 1,
        "s1_valid": 0,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 16)

    assert not _hit(recorder, "icache_mainpipe_s0_entry", "ftq_waylookup_skew")

    recorder.set_key("s1_valid", 1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 17)

    assert _hit(recorder, "icache_mainpipe_s0_entry", "ftq_waylookup_skew")


def test_four_line_arbiter_samples_request_condition_before_priority_checkpoint():
    recorder = _Recorder()
    recorder.set_key("s1_valid", 1)
    recorder.set_key("miss_req_ready", 1)
    # Do not provide missReq.valid or hasSend observations: those belong to
    # the fixed-priority Checkpoint, not to the four-line stimulus Condition.
    for index in range(4):
        recorder.env.dut.set(_MAIN + f"s1_shouldFetch_{index}", 1)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 16)

    assert _hit(recorder, "icache_mainpipe_s1_miss", "four_line_fixed_priority")


def test_registered_refill_is_cancelled_by_next_cycle_flush():
    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "miss_resp_valid": 1,
        "s1_flush": 0,
    }.items():
        recorder.set_key(key, value)
    recorder.env.dut.set(_MAIN + "s1_mshrValid_0_0", 1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 30)

    recorder.set_key("miss_resp_valid", 0)
    recorder.set_key("s1_flush", 1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 31)

    assert _hit(
        recorder,
        "icache_mainpipe_s1_flush",
        "flush_cancels_registered_refill",
    )


def test_error_refill_state_is_scoped_to_the_next_new_request():
    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "miss_resp_valid": 1,
        "miss_resp_corrupt": 1,
        "s1_flush": 0,
        "io_flush": 0,
    }.items():
        recorder.set_key(key, value)
    recorder.env.dut.set(_MAIN + "s1_mshrValid_0_0", 1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 32)

    recorder.set_key("miss_resp_valid", 0)
    recorder.set_key("miss_resp_corrupt", 0)
    recorder.set_key("s1_fire", 1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 33)

    assert _hit(
        recorder,
        "icache_mainpipe_s1_refill",
        "error_state_cleared_on_new_request",
    )


def test_late_refill_is_counted_only_after_flushed_sent_miss():
    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "s1_flush": 1,
        "bpu_valid": 0,
        "miss_resp_valid": 0,
        "toifu_valid": 0,
    }.items():
        recorder.set_key(key, value)
    recorder.env.dut.set(_MAIN + "s1_hasSend_valid", 1)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 20)
    assert not _hit(recorder, "icache_mainpipe_s1_flush", "late_refill_ignored_after_flush")

    recorder.set_key("s1_valid", 0)
    recorder.set_key("s1_flush", 0)
    recorder.set_key("miss_resp_valid", 1)
    recorder.env.dut.set(_MAIN + "s1_hasSend_valid", 0)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 21)

    assert _hit(recorder, "icache_mainpipe_s1_flush", "late_refill_ignored_after_flush")
