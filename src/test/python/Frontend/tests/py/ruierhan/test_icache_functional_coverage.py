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
    assert len(ICACHE_MAINPIPE_SAMPLER_BIN_KEYS) == 47
    assert len(set(ICACHE_MAINPIPE_SAMPLER_BIN_KEYS)) == 47


def test_icache_prefetchpipe_sampler_contract_has_one_key_per_leaf():
    assert len(ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS) == 37
    assert len(set(ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS)) == 37


def test_icache_missunit_sampler_contract_has_one_key_per_leaf():
    assert len(ICACHE_MISSUNIT_SAMPLER_BIN_KEYS) == 30
    assert len(set(ICACHE_MISSUNIT_SAMPLER_BIN_KEYS)) == 30


def _set_fetch_mshr_allocate_inputs(recorder, *, prefetch=0, flush=0, fencei=0):
    for name, value in {
        _MAIN + "__Vtogcov__io_missReq_valid": 1,
        _MAIN + "__Vtogcov__io_missReq_ready": 1,
        _MAIN + "__Vtogcov__io_missReq_bits_blkPAddr": 0x480,
        _MAIN + "__Vtogcov__io_missReq_bits_vSetIdx": 7,
        _MISS + "fetchHit": 0,
        _MISS + "io_prefetchReq_valid": prefetch,
        _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush": flush,
        _TOP + "io_fencei": fencei,
        _MISS + "priorityFIFO.io_enq_valid": 0,
    }.items():
        recorder.set_missunit_signal(name, value)
    for index in range(4):
        recorder.set_missunit_signal(_MISS + f"allMshr_{index}.valid", 0)


def test_missunit_fetch_allocate_requires_single_clean_request_and_tracks_checkpoint():
    recorder = _Recorder()
    _set_fetch_mshr_allocate_inputs(recorder)
    recorder.set_missunit_signal(_MISS + "allMshr_1.valid", 1)

    sample_icache_missunit_coverage(recorder, recorder.env, 100)

    assert _hit(recorder, "icache_missunit_request", "fetch_mshr_allocate")
    pending = recorder._icache_missunit_cov_state["pending_fetch_allocations"]
    assert pending == [{
        "trigger_cycle": 100,
        "expected_index": 0,
        "paddr": 0x480,
        "vset": 7,
    }]

    recorder.set_missunit_signal(_MAIN + "__Vtogcov__io_missReq_valid", 0)
    recorder.set_missunit_signal(_MISS + "allMshr_0.valid", 1)
    recorder.set_missunit_signal(_MISS + "allMshr_0.blkPAddr", 0x480)
    recorder.set_missunit_signal(_MISS + "allMshr_0.vSetIdx", 7)
    sample_icache_missunit_coverage(recorder, recorder.env, 101)

    checkpoint = recorder._icache_missunit_cov_state["last_fetch_allocation_checkpoint"]
    assert checkpoint["expected_index"] == 0
    assert checkpoint["payload_matches"]
    assert checkpoint["fifo_not_enqueued"]
    assert checkpoint["complete"]


def test_missunit_fetch_allocate_rejects_non_single_or_blocked_requests():
    for kwargs in (
        {"prefetch": 1},
        {"flush": 1},
        {"fencei": 1},
    ):
        recorder = _Recorder()
        _set_fetch_mshr_allocate_inputs(recorder, **kwargs)
        sample_icache_missunit_coverage(recorder, recorder.env, 102)
        assert not _hit(recorder, "icache_missunit_request", "fetch_mshr_allocate")

    recorder = _Recorder()
    _set_fetch_mshr_allocate_inputs(recorder)
    for index in range(4):
        recorder.set_missunit_signal(_MISS + f"allMshr_{index}.valid", 1)
    sample_icache_missunit_coverage(recorder, recorder.env, 103)
    assert not _hit(recorder, "icache_missunit_request", "fetch_mshr_allocate")


def _set_full_missunit_capacity_inputs(recorder, *, prefetch=False, duplicate=False):
    request_paddr = 0x900 if not duplicate else (0x404 if prefetch else 0x400)
    request_vset = 11 if not duplicate else (8 if prefetch else 4)
    signals = {
        _MAIN + "__Vtogcov__io_missReq_valid": 0 if prefetch else 1,
        _MISS + "io_prefetchReq_valid": 1 if prefetch else 0,
        _MISS + "fetchHit": 1 if (not prefetch and duplicate) else 0,
        _MISS + "prefetchHit": 1 if (prefetch and duplicate) else 0,
        _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush": 0,
        _TOP + "io_fencei": 0,
    }
    if prefetch:
        signals.update(
            {
                _MISS + "io_prefetchReq_ready": 1 if duplicate else 0,
                _MISS + "__Vtogcov__io_prefetchReq_bits_blkPAddr": request_paddr,
                _MISS + "__Vtogcov__io_prefetchReq_bits_vSetIdx": request_vset,
            }
        )
    else:
        signals.update(
            {
                _MAIN + "__Vtogcov__io_missReq_ready": 1 if duplicate else 0,
                _MAIN + "__Vtogcov__io_missReq_bits_blkPAddr": request_paddr,
                _MAIN + "__Vtogcov__io_missReq_bits_vSetIdx": request_vset,
            }
        )
    for name, value in signals.items():
        recorder.set_missunit_signal(name, value)

    for index in range(14):
        in_full_pool = index >= 4 if prefetch else index < 4
        recorder.set_missunit_signal(
            _MISS + f"allMshr_{index}.valid", int(in_full_pool)
        )
        recorder.set_missunit_signal(
            _MISS + f"allMshr_{index}.blkPAddr", 0x400 + index
        )
        recorder.set_missunit_signal(_MISS + f"allMshr_{index}.vSetIdx", 4 + index)


def test_missunit_capacity_bins_only_sample_nonduplicate_backpressure():
    recorder = _Recorder()
    _set_full_missunit_capacity_inputs(recorder)
    sample_icache_missunit_coverage(recorder, recorder.env, 104)
    assert _hit(recorder, "icache_missunit_capacity", "fetch_full_backpressure")
    assert not _hit(recorder, "icache_missunit_capacity", "prefetch_full_backpressure")

    recorder = _Recorder()
    _set_full_missunit_capacity_inputs(recorder, prefetch=True)
    sample_icache_missunit_coverage(recorder, recorder.env, 105)
    assert _hit(recorder, "icache_missunit_capacity", "prefetch_full_backpressure")
    assert not _hit(recorder, "icache_missunit_capacity", "fetch_full_backpressure")


def test_missunit_capacity_bins_do_not_duplicate_merge_coverage():
    recorder = _Recorder()
    _set_full_missunit_capacity_inputs(recorder, duplicate=True)
    sample_icache_missunit_coverage(recorder, recorder.env, 106)
    assert _hit(recorder, "icache_missunit_dedup", "fetch_merge_any_mshr")
    assert not _hit(recorder, "icache_missunit_capacity", "fetch_full_backpressure")

    recorder = _Recorder()
    _set_full_missunit_capacity_inputs(recorder, prefetch=True, duplicate=True)
    sample_icache_missunit_coverage(recorder, recorder.env, 107)
    assert _hit(recorder, "icache_missunit_dedup", "prefetch_merge_any_mshr")
    assert not _hit(recorder, "icache_missunit_capacity", "prefetch_full_backpressure")


def _set_missunit_dedup_inputs(
    recorder,
    *,
    request="fetch",
    existing_index=None,
    issue=0,
    same_cycle_fetch_prefetch=False,
):
    fetch_valid = request == "fetch" or same_cycle_fetch_prefetch
    prefetch_valid = request == "prefetch" or same_cycle_fetch_prefetch
    signals = {
        _MAIN + "__Vtogcov__io_missReq_valid": int(fetch_valid),
        _MAIN + "__Vtogcov__io_missReq_ready": 1,
        _MAIN + "__Vtogcov__io_missReq_bits_blkPAddr": 0x400,
        _MAIN + "__Vtogcov__io_missReq_bits_vSetIdx": 4,
        _MISS + "fetchHit": int(request == "fetch" and existing_index is not None),
        _MISS + "io_prefetchReq_valid": int(prefetch_valid),
        _MISS + "io_prefetchReq_ready": 1,
        _MISS + "__Vtogcov__io_prefetchReq_bits_blkPAddr": 0x400,
        _MISS + "__Vtogcov__io_prefetchReq_bits_vSetIdx": 4,
        _MISS + "prefetchHit": int(prefetch_valid),
        _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush": 0,
        _TOP + "io_fencei": 0,
    }
    for name, value in signals.items():
        recorder.set_missunit_signal(name, value)
    for index in range(14):
        recorder.set_missunit_signal(_MISS + f"allMshr_{index}.valid", 0)
    if existing_index is not None:
        recorder.set_missunit_signal(_MISS + f"allMshr_{existing_index}.valid", 1)
        recorder.set_missunit_signal(_MISS + f"allMshr_{existing_index}.issue", issue)
        recorder.set_missunit_signal(_MISS + f"allMshr_{existing_index}.flush", 0)
        recorder.set_missunit_signal(_MISS + f"allMshr_{existing_index}.fencei", 0)
        recorder.set_missunit_signal(_MISS + f"allMshr_{existing_index}.blkPAddr", 0x400)
        recorder.set_missunit_signal(_MISS + f"allMshr_{existing_index}.vSetIdx", 4)


def test_missunit_dedup_accepts_fetch_and_prefetch_mshr_before_or_after_issue():
    for request, existing_index, bin_name in (
        ("fetch", 4, "fetch_merge_any_mshr"),
        ("prefetch", 0, "prefetch_merge_any_mshr"),
    ):
        for issue in (0, 1):
            recorder = _Recorder()
            _set_missunit_dedup_inputs(
                recorder,
                request=request,
                existing_index=existing_index,
                issue=issue,
            )
            sample_icache_missunit_coverage(recorder, recorder.env, 108 + issue)
            assert _hit(recorder, "icache_missunit_dedup", bin_name)


def test_missunit_dedup_requires_existing_mshr_and_clean_controls():
    for request, bin_name in (
        ("fetch", "fetch_merge_any_mshr"),
        ("prefetch", "prefetch_merge_any_mshr"),
    ):
        recorder = _Recorder()
        _set_missunit_dedup_inputs(recorder, request=request, existing_index=None)
        sample_icache_missunit_coverage(recorder, recorder.env, 110)
        assert not _hit(recorder, "icache_missunit_dedup", bin_name)

        for control_signal in (
            _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush",
            _TOP + "io_fencei",
        ):
            recorder = _Recorder()
            _set_missunit_dedup_inputs(
                recorder,
                request=request,
                existing_index=0 if request == "fetch" else 4,
            )
            recorder.set_missunit_signal(control_signal, 1)
            sample_icache_missunit_coverage(recorder, recorder.env, 111)
            assert not _hit(recorder, "icache_missunit_dedup", bin_name)


def test_missunit_prefetch_dedup_excludes_same_cycle_fetch_merge():
    recorder = _Recorder()
    _set_missunit_dedup_inputs(
        recorder,
        request="prefetch",
        same_cycle_fetch_prefetch=True,
    )
    sample_icache_missunit_coverage(recorder, recorder.env, 112)
    assert not _hit(recorder, "icache_missunit_dedup", "prefetch_merge_any_mshr")


def test_missunit_capacity_bins_require_full_pool_and_clean_control():
    for prefetch, free_index, bin_name in (
        (False, 0, "fetch_full_backpressure"),
        (True, 4, "prefetch_full_backpressure"),
    ):
        recorder = _Recorder()
        _set_full_missunit_capacity_inputs(recorder, prefetch=prefetch)
        recorder.set_missunit_signal(_MISS + f"allMshr_{free_index}.valid", 0)
        sample_icache_missunit_coverage(recorder, recorder.env, 108)
        assert not _hit(recorder, "icache_missunit_capacity", bin_name)

        for control_signal in (
            _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush",
            _TOP + "io_fencei",
        ):
            recorder = _Recorder()
            _set_full_missunit_capacity_inputs(recorder, prefetch=prefetch)
            recorder.set_missunit_signal(control_signal, 1)
            sample_icache_missunit_coverage(recorder, recorder.env, 109)
            assert not _hit(recorder, "icache_missunit_capacity", bin_name)


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


def _set_mainpipe_protection_miss(recorder):
    for key, value in {
        "s1_valid": 1,
        "s1_flush": 0,
        "cross0": 0,
        "req1_valid": 0,
        "backend_exception": 0,
        "itlb_exception": 0,
        "exception": 0,
        "pmp_instr": 0,
        "pmp_mmio": 0,
        "pbmt": 0,
    }.items():
        recorder.set_key(key, value)
    recorder.env.dut.set(_MAIN + "s1_hits_0_0", 0)


def test_mainpipe_pmp_exception_bin_excludes_flush_and_backend_exception():
    recorder = _Recorder()
    _set_mainpipe_protection_miss(recorder)
    recorder.set_key("pmp_instr", 1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 1)
    assert _hit(recorder, "icache_mainpipe_s1_protection", "pmp_exception_suppresses_miss")

    for blocker in ("s1_flush", "backend_exception"):
        recorder = _Recorder()
        _set_mainpipe_protection_miss(recorder)
        recorder.set_key("pmp_instr", 1)
        recorder.set_key(blocker, 1)
        sample_icache_mainpipe_coverage(recorder, recorder.env, 2)
        assert not _hit(
            recorder,
            "icache_mainpipe_s1_protection",
            "pmp_exception_suppresses_miss",
        )


def test_mainpipe_uncache_sources_sample_independent_bins():
    cases = (
        ("pmp_mmio_suppresses_refill", {"pmp_mmio": 1, "pbmt": 0}),
        ("pbmt_uncache_suppresses_refill", {"pmp_mmio": 0, "pbmt": 1}),
        ("pbmt_uncache_suppresses_refill", {"pmp_mmio": 0, "pbmt": 2}),
    )
    all_bins = {name for name, _ in cases}

    for cycle, (expected_bin, overrides) in enumerate(cases, start=10):
        recorder = _Recorder()
        _set_mainpipe_protection_miss(recorder)
        for key, value in overrides.items():
            recorder.set_key(key, value)
        sample_icache_mainpipe_coverage(recorder, recorder.env, cycle)

        assert _hit(recorder, "icache_mainpipe_s1_protection", expected_bin)
        assert all(
            not _hit(recorder, "icache_mainpipe_s1_protection", other_bin)
            for other_bin in all_bins - {expected_bin}
        )


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


def _set_missunit_concurrent_request(
    recorder,
    *,
    fetch_paddr,
    prefetch_paddr,
    fetch_vset,
    prefetch_vset,
    prefetch_hit=0,
):
    for name, value in {
        _MAIN + "__Vtogcov__io_missReq_valid": 1,
        _MAIN + "__Vtogcov__io_missReq_bits_blkPAddr": fetch_paddr,
        _MAIN + "__Vtogcov__io_missReq_bits_vSetIdx": fetch_vset,
        _MISS + "io_prefetchReq_valid": 1,
        _MISS + "__Vtogcov__io_prefetchReq_bits_blkPAddr": prefetch_paddr,
        _MISS + "__Vtogcov__io_prefetchReq_bits_vSetIdx": prefetch_vset,
        _MISS + "fetchHit": 0,
        _MISS + "prefetchHit": prefetch_hit,
        _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush": 0,
        _TOP + "io_fencei": 0,
    }.items():
        recorder.set_missunit_signal(name, value)
    for index in range(14):
        recorder.set_missunit_signal(_MISS + f"allMshr_{index}.valid", 0)
        recorder.set_missunit_signal(_MISS + f"allMshr_{index}.blkPAddr", 0)
        recorder.set_missunit_signal(_MISS + f"allMshr_{index}.vSetIdx", 0)


def test_missunit_concurrent_request_bins_require_complete_miss_conditions():
    cases = (
        ("same_key_fetch_prefetch_merge", 0x100, 0x100, 3, 3, 1),
        ("distinct_key_parallel_allocate", 0x100, 0x140, 3, 3, 0),
        ("same_paddr_diff_vset_separate", 0x100, 0x100, 3, 4, 0),
    )
    for cycle, (
        bin_name,
        fetch_paddr,
        prefetch_paddr,
        fetch_vset,
        prefetch_vset,
        prefetch_hit,
    ) in enumerate(cases, 1):
        recorder = _Recorder()
        _set_missunit_concurrent_request(
            recorder,
            fetch_paddr=fetch_paddr,
            prefetch_paddr=prefetch_paddr,
            fetch_vset=fetch_vset,
            prefetch_vset=prefetch_vset,
            prefetch_hit=prefetch_hit,
        )
        sample_icache_missunit_coverage(recorder, recorder.env, cycle)
        assert _hit(recorder, "icache_missunit_request", bin_name)


def test_missunit_concurrent_request_bins_reject_controls_and_existing_mshr():
    for control_signal in (
        _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush",
        _TOP + "io_fencei",
    ):
        recorder = _Recorder()
        _set_missunit_concurrent_request(
            recorder,
            fetch_paddr=0x100,
            prefetch_paddr=0x100,
            fetch_vset=3,
            prefetch_vset=3,
            prefetch_hit=1,
        )
        recorder.set_missunit_signal(control_signal, 1)
        sample_icache_missunit_coverage(recorder, recorder.env, 10)
        assert not _hit(recorder, "icache_missunit_request", "same_key_fetch_prefetch_merge")

    recorder = _Recorder()
    _set_missunit_concurrent_request(
        recorder,
        fetch_paddr=0x100,
        prefetch_paddr=0x100,
        fetch_vset=3,
        prefetch_vset=3,
    )
    sample_icache_missunit_coverage(recorder, recorder.env, 10)
    assert not _hit(recorder, "icache_missunit_request", "same_key_fetch_prefetch_merge")

    recorder = _Recorder()
    _set_missunit_concurrent_request(
        recorder,
        fetch_paddr=0x100,
        prefetch_paddr=0x100,
        fetch_vset=3,
        prefetch_vset=3,
        prefetch_hit=1,
    )
    recorder.set_missunit_signal(_MISS + "allMshr_0.valid", 1)
    recorder.set_missunit_signal(_MISS + "allMshr_0.blkPAddr", 0x100)
    recorder.set_missunit_signal(_MISS + "allMshr_0.vSetIdx", 3)
    sample_icache_missunit_coverage(recorder, recorder.env, 11)
    assert not _hit(recorder, "icache_missunit_request", "same_key_fetch_prefetch_merge")


def test_missunit_distinct_ptag_bin_excludes_same_ptag_different_block():
    recorder = _Recorder()
    _set_missunit_concurrent_request(
        recorder,
        fetch_paddr=0x100,
        prefetch_paddr=0x101,
        fetch_vset=3,
        prefetch_vset=3,
    )

    sample_icache_missunit_coverage(recorder, recorder.env, 1)

    assert not _hit(recorder, "icache_missunit_request", "distinct_key_parallel_allocate")


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


def _set_missunit_acquire_controls(recorder, *, ready=1, flush=0, fencei=0):
    for name, value in {
        _TOP + "auto_inner_icache_client_out_a_ready": ready,
        _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush": flush,
        _TOP + "io_fencei": fencei,
    }.items():
        recorder.set_missunit_signal(name, value)


def test_missunit_fetch_prefetch_priority_samples_competition_conditions_only():
    recorder = _Recorder()
    _set_missunit_acquire_controls(recorder)
    recorder.set_missunit_signal(_MISS + "acquireArb.io_in_0_valid", 1)
    recorder.set_missunit_signal(_MISS + "prefetchArb.io_out_valid", 1)

    sample_icache_missunit_coverage(recorder, recorder.env, 7)

    assert _hit(recorder, "icache_missunit_acquire", "fetch_priority_over_prefetch")

    for missing_signal in (
        _MISS + "acquireArb.io_in_0_valid",
        _MISS + "prefetchArb.io_out_valid",
        _TOP + "auto_inner_icache_client_out_a_ready",
    ):
        recorder = _Recorder()
        _set_missunit_acquire_controls(recorder)
        recorder.set_missunit_signal(_MISS + "acquireArb.io_in_0_valid", 1)
        recorder.set_missunit_signal(_MISS + "prefetchArb.io_out_valid", 1)
        recorder.set_missunit_signal(missing_signal, 0)
        sample_icache_missunit_coverage(recorder, recorder.env, 8)
        assert not _hit(recorder, "icache_missunit_acquire", "fetch_priority_over_prefetch")


def test_missunit_fetch_index_priority_requires_two_candidates_and_clean_controls():
    recorder = _Recorder()
    _set_missunit_acquire_controls(recorder)
    recorder.set_missunit_signal(_MISS + "acquireArb.io_in_0_valid", 1)
    recorder.set_missunit_signal(_MISS + "acquireArb.io_in_2_valid", 1)

    sample_icache_missunit_coverage(recorder, recorder.env, 9)

    assert _hit(recorder, "icache_missunit_acquire", "fetch_index_priority")

    recorder = _Recorder()
    _set_missunit_acquire_controls(recorder, flush=1)
    recorder.set_missunit_signal(_MISS + "acquireArb.io_in_0_valid", 1)
    recorder.set_missunit_signal(_MISS + "acquireArb.io_in_2_valid", 1)
    sample_icache_missunit_coverage(recorder, recorder.env, 10)
    assert not _hit(recorder, "icache_missunit_acquire", "fetch_index_priority")


def test_missunit_acquire_backpressure_requires_two_blocked_cycles_and_recovery():
    recorder = _Recorder()
    _set_missunit_acquire_controls(recorder, ready=0)
    recorder.set_missunit_signal(_TOP + "auto_inner_icache_client_out_a_valid", 1)

    sample_icache_missunit_coverage(recorder, recorder.env, 11)
    assert not _hit(recorder, "icache_missunit_acquire", "acquire_backpressure_recovery")
    sample_icache_missunit_coverage(recorder, recorder.env, 12)
    assert not _hit(recorder, "icache_missunit_acquire", "acquire_backpressure_recovery")

    recorder.set_missunit_signal(_TOP + "auto_inner_icache_client_out_a_ready", 1)
    sample_icache_missunit_coverage(recorder, recorder.env, 13)
    assert _hit(recorder, "icache_missunit_acquire", "acquire_backpressure_recovery")


def test_missunit_acquire_backpressure_window_resets_on_control_or_invalid():
    for interruption, signal_name in (
        (1, _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush"),
        (0, _TOP + "auto_inner_icache_client_out_a_valid"),
    ):
        recorder = _Recorder()
        _set_missunit_acquire_controls(recorder, ready=0)
        recorder.set_missunit_signal(_TOP + "auto_inner_icache_client_out_a_valid", 1)
        sample_icache_missunit_coverage(recorder, recorder.env, 14)
        recorder.set_missunit_signal(signal_name, interruption)
        sample_icache_missunit_coverage(recorder, recorder.env, 15)
        recorder.set_missunit_signal(signal_name, 0 if interruption else 1)
        recorder.set_missunit_signal(_TOP + "auto_inner_icache_client_out_a_ready", 1)
        sample_icache_missunit_coverage(recorder, recorder.env, 16)
        assert not _hit(recorder, "icache_missunit_acquire", "acquire_backpressure_recovery")


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


def _set_prefetch_s1_completion_scenario(recorder, **overrides):
    values = {
        "s1_valid": 1,
        "pf_enable": 1,
        "s1_flush": 0,
        "s1_state": 4,
        "s1_soft": 0,
        "s2_ready": 0,
        "way0_ready": 0,
        "refill_valid": 0,
    }
    values.update(overrides)
    for key, value in values.items():
        recorder.set_prefetch_key(key, value)


def test_prefetch_s2_busy_recovery_requires_qualified_wait_episode():
    recorder = _Recorder()
    _set_prefetch_s1_completion_scenario(recorder)

    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 1)
    assert not _hit(
        recorder,
        "icache_prefetchpipe_s1_completion",
        "s2_busy_enters_s2_recovery",
    )

    recorder.set_prefetch_key("s2_ready", 1)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 2)
    assert _hit(
        recorder,
        "icache_prefetchpipe_s1_completion",
        "s2_busy_enters_s2_recovery",
    )


def test_prefetch_s2_busy_recovery_clears_on_disqualification():
    for disqualifier in ("s1_valid", "pf_enable"):
        recorder = _Recorder()
        _set_prefetch_s1_completion_scenario(recorder)
        sample_icache_prefetchpipe_coverage(recorder, recorder.env, 1)

        recorder.set_prefetch_key(disqualifier, 0)
        sample_icache_prefetchpipe_coverage(recorder, recorder.env, 2)

        recorder.set_prefetch_key(disqualifier, 1)
        recorder.set_prefetch_key("s2_ready", 1)
        sample_icache_prefetchpipe_coverage(recorder, recorder.env, 3)
        assert not _hit(
            recorder,
            "icache_prefetchpipe_s1_completion",
            "s2_busy_enters_s2_recovery",
        )

    recorder = _Recorder()
    _set_prefetch_s1_completion_scenario(recorder)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 4)
    recorder.set_prefetch_key("s1_flush", 1)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 5)
    recorder.set_prefetch_key("s1_flush", 0)
    recorder.set_prefetch_key("s2_ready", 1)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 6)
    assert not _hit(
        recorder,
        "icache_prefetchpipe_s1_completion",
        "s2_busy_enters_s2_recovery",
    )


def test_prefetch_flush_completion_requires_enqway_or_enters2_eligibility():
    positive_scenarios = (
        {"s1_state": 3, "s1_soft": 0, "way0_ready": 1, "s2_ready": 1},
        {"s1_state": 3, "s1_soft": 1, "way0_ready": 0, "s2_ready": 1},
        {"s1_state": 4, "s2_ready": 1},
    )
    for cycle, scenario in enumerate(positive_scenarios, start=1):
        recorder = _Recorder()
        _set_prefetch_s1_completion_scenario(
            recorder,
            s1_flush=1,
            **scenario,
        )
        sample_icache_prefetchpipe_coverage(recorder, recorder.env, cycle)
        assert _hit(
            recorder,
            "icache_prefetchpipe_s1_completion",
            "flush_blocks_s1_completion",
        )

    negative_scenarios = (
        {"s1_state": 1, "s2_ready": 1},
        {"s1_state": 3, "s1_soft": 0, "way0_ready": 0, "s2_ready": 1},
        {"s1_state": 3, "s1_soft": 0, "way0_ready": 1, "s2_ready": 0},
        {
            "s1_state": 3,
            "s1_soft": 0,
            "way0_ready": 1,
            "s2_ready": 1,
            "refill_valid": 1,
        },
        {"s1_state": 4, "s2_ready": 0},
    )
    for cycle, scenario in enumerate(negative_scenarios, start=10):
        recorder = _Recorder()
        _set_prefetch_s1_completion_scenario(
            recorder,
            s1_flush=1,
            **scenario,
        )
        sample_icache_prefetchpipe_coverage(recorder, recorder.env, cycle)
        assert not _hit(
            recorder,
            "icache_prefetchpipe_s1_completion",
            "flush_blocks_s1_completion",
        )


def _set_prefetch_entry_scenario(recorder):
    for key, value in {
        "from_valid": 1,
        "from_soft": 0,
        "s1_ready": 1,
        "meta_ready": 1,
        "global_flush": 0,
        "bpu_valid": 0,
        "s1_valid": 0,
        "s0_ftq_flag": 0,
        "s0_ftq_value": 4,
        "s1_ftq_flag": 0,
        "s1_ftq_value": 4,
    }.items():
        recorder.set_prefetch_key(key, value)


def test_prefetch_redirect_flush_requires_dynamic_entry_resources_ready():
    recorder = _Recorder()
    _set_prefetch_entry_scenario(recorder)
    recorder.set_prefetch_key("global_flush", 1)
    recorder.set_prefetch_key("meta_ready", 0)

    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 2)
    assert not _hit(recorder, "icache_prefetchpipe_s0_entry", "redirect_flush_blocks_hw")

    recorder.set_prefetch_key("meta_ready", 1)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 3)
    assert _hit(recorder, "icache_prefetchpipe_s0_entry", "redirect_flush_blocks_hw")


def test_prefetch_bpu_match_and_miss_are_independent_bins():
    match_recorder = _Recorder()
    _set_prefetch_entry_scenario(match_recorder)
    for key, value in {
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 4,
    }.items():
        match_recorder.set_prefetch_key(key, value)

    sample_icache_prefetchpipe_coverage(match_recorder, match_recorder.env, 4)
    assert _hit(
        match_recorder,
        "icache_prefetchpipe_s0_entry",
        "bpu_flush_match_blocks_hw",
    )
    assert not _hit(
        match_recorder,
        "icache_prefetchpipe_s0_entry",
        "bpu_flush_miss_allows_hw",
    )

    miss_recorder = _Recorder()
    _set_prefetch_entry_scenario(miss_recorder)
    for key, value in {
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 5,
    }.items():
        miss_recorder.set_prefetch_key(key, value)

    sample_icache_prefetchpipe_coverage(miss_recorder, miss_recorder.env, 5)
    assert _hit(
        miss_recorder,
        "icache_prefetchpipe_s0_entry",
        "bpu_flush_miss_allows_hw",
    )
    assert not _hit(
        miss_recorder,
        "icache_prefetchpipe_s0_entry",
        "bpu_flush_match_blocks_hw",
    )


def test_prefetch_bpu_miss_bin_excludes_flush_of_current_s1_request():
    recorder = _Recorder()
    _set_prefetch_entry_scenario(recorder)
    for key, value in {
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 5,
        "s1_valid": 1,
        "s1_ftq_flag": 0,
        "s1_ftq_value": 6,
    }.items():
        recorder.set_prefetch_key(key, value)

    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 6)
    assert not _hit(
        recorder,
        "icache_prefetchpipe_s0_entry",
        "bpu_flush_miss_allows_hw",
    )


def test_prefetch_soft_entry_bins_cover_their_complete_trigger_conditions():
    soft_bpu = _Recorder()
    for key, value in {
        "from_valid": 1,
        "from_soft": 1,
        "bpu_valid": 1,
        "global_flush": 0,
        "s0_fire": 1,
    }.items():
        soft_bpu.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(soft_bpu, soft_bpu.env, 7)
    assert _hit(soft_bpu, "icache_prefetchpipe_s0_entry", "soft_ignores_bpu_flush")
    blocked_soft_bpu = _Recorder()
    for key in ("from_valid", "from_soft", "bpu_valid", "global_flush", "s0_fire"):
        value = 1
        blocked_soft_bpu.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(blocked_soft_bpu, blocked_soft_bpu.env, 8)
    assert not _hit(
        blocked_soft_bpu,
        "icache_prefetchpipe_s0_entry",
        "soft_ignores_bpu_flush",
    )

    soft_priority = _Recorder()
    for key, value in {
        "from_valid": 1,
        "from_soft": 1,
        "soft_pending": 1,
        "ftq_prefetch_valid": 1,
        "s0_fire": 1,
    }.items():
        soft_priority.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(soft_priority, soft_priority.env, 9)
    assert _hit(
        soft_priority,
        "icache_prefetchpipe_s0_entry",
        "soft_priority_over_ftq",
    )
    no_soft_selection = _Recorder()
    for key, value in {
        "from_valid": 1,
        "from_soft": 0,
        "soft_pending": 1,
        "ftq_prefetch_valid": 1,
        "s0_fire": 1,
    }.items():
        no_soft_selection.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(no_soft_selection, no_soft_selection.env, 10)
    assert not _hit(
        no_soft_selection,
        "icache_prefetchpipe_s0_entry",
        "soft_priority_over_ftq",
    )

    multi_soft = _Recorder()
    for key, value in {
        "soft_pending": 0,
        "soft0_valid": 1,
        "soft1_valid": 1,
        "soft2_valid": 0,
    }.items():
        multi_soft.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(multi_soft, multi_soft.env, 11)
    assert _hit(
        multi_soft,
        "icache_prefetchpipe_s0_entry",
        "multi_soft_single_accept",
    )
    pending_multi_soft = _Recorder()
    for key, value in {
        "soft_pending": 1,
        "soft0_valid": 1,
        "soft1_valid": 1,
    }.items():
        pending_multi_soft.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(pending_multi_soft, pending_multi_soft.env, 12)
    assert not _hit(
        pending_multi_soft,
        "icache_prefetchpipe_s0_entry",
        "multi_soft_single_accept",
    )


def test_prefetch_soft_ftq_same_cycle_requires_ready_unflushed_entry():
    recorder = _Recorder()
    _set_prefetch_entry_scenario(recorder)
    for key, value in {
        "soft_pending": 0,
        "ftq_prefetch_valid": 1,
        "soft0_valid": 1,
        "soft1_valid": 0,
        "soft2_valid": 0,
    }.items():
        recorder.set_prefetch_key(key, value)

    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 10)
    assert _hit(
        recorder,
        "icache_prefetchpipe_s0_entry",
        "soft_ftq_same_cycle_capture",
    )

    for blocker, value in (
        ("s1_ready", 0),
        ("meta_ready", 0),
        ("global_flush", 1),
    ):
        blocked = _Recorder()
        _set_prefetch_entry_scenario(blocked)
        for key, signal_value in {
            "soft_pending": 0,
            "ftq_prefetch_valid": 1,
            "soft0_valid": 1,
            blocker: value,
        }.items():
            blocked.set_prefetch_key(key, signal_value)
        sample_icache_prefetchpipe_coverage(blocked, blocked.env, 11)
        assert not _hit(
            blocked,
            "icache_prefetchpipe_s0_entry",
            "soft_ftq_same_cycle_capture",
        )

    bpu_blocked = _Recorder()
    _set_prefetch_entry_scenario(bpu_blocked)
    for key, value in {
        "soft_pending": 0,
        "ftq_prefetch_valid": 1,
        "soft0_valid": 1,
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 4,
    }.items():
        bpu_blocked.set_prefetch_key(key, value)

    sample_icache_prefetchpipe_coverage(bpu_blocked, bpu_blocked.env, 12)
    assert not _hit(
        bpu_blocked,
        "icache_prefetchpipe_s0_entry",
        "soft_ftq_same_cycle_capture",
    )


def test_prefetch_s1_itlb_flush_samples_redirect_or_matching_bpu_source():
    for cycle, source in enumerate(("redirect", "bpu"), start=10):
        recorder = _Recorder()
        for key, value in {
            "s1_valid": 1,
            "s1_wait_itlb": 1,
            "s1_soft": 0,
            "global_flush": int(source == "redirect"),
            "bpu_valid": int(source == "bpu"),
            "bpu_flag": 0,
            "bpu_value": 4,
            "s1_ftq_flag": 0,
            "s1_ftq_value": 4,
        }.items():
            recorder.set_prefetch_key(key, value)
        sample_icache_prefetchpipe_coverage(recorder, recorder.env, cycle)
        assert _hit(
            recorder,
            "icache_prefetchpipe_s1_meta",
            "flush_cancels_itlb_wait",
        )

    for soft, bpu_value in ((0, 5), (1, 4)):
        recorder = _Recorder()
        for key, value in {
            "s1_valid": 1,
            "s1_wait_itlb": 1,
            "s1_soft": soft,
            "global_flush": 0,
            "bpu_valid": 1,
            "bpu_flag": 0,
            "bpu_value": bpu_value,
            "s1_ftq_flag": 0,
            "s1_ftq_value": 4,
        }.items():
            recorder.set_prefetch_key(key, value)
        sample_icache_prefetchpipe_coverage(recorder, recorder.env, 12)
        assert not _hit(
            recorder,
            "icache_prefetchpipe_s1_meta",
            "flush_cancels_itlb_wait",
        )


def test_prefetch_itlb_resend_requires_miss_then_same_transaction_success():
    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "s1_flush": 0,
        "s1_wait_itlb": 0,
        "itlb_req_valid": 1,
        "itlb_resp_miss": 1,
        "s1_tlb_finish": 0,
    }.items():
        recorder.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 20)
    assert not _hit(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "itlb_miss_resend_meta_retry",
    )

    recorder.set_prefetch_key("s1_wait_itlb", 1)
    recorder.set_prefetch_key("itlb_resp_miss", 0)
    recorder.set_prefetch_key("s1_tlb_finish", 1)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 21)
    assert _hit(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "itlb_miss_resend_meta_retry",
    )

    no_miss = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "s1_flush": 0,
        "s1_wait_itlb": 1,
        "itlb_req_valid": 1,
        "itlb_resp_miss": 0,
        "s1_tlb_finish": 1,
    }.items():
        no_miss.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(no_miss, no_miss.env, 22)
    assert not _hit(
        no_miss,
        "icache_prefetchpipe_s1_meta",
        "itlb_miss_resend_meta_retry",
    )


def test_prefetch_meta_resend_requires_two_blocked_cycles_then_recovery():
    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "s1_state": 2,
        "s1_flush": 0,
        "meta_req_valid": 1,
        "meta_ready": 0,
    }.items():
        recorder.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 30)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 31)
    assert not _hit(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "meta_resend_backpressure_recovery",
    )

    recorder.set_prefetch_key("meta_ready", 1)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 32)
    assert _hit(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "meta_resend_backpressure_recovery",
    )

    one_blocked = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "s1_state": 2,
        "s1_flush": 0,
        "meta_req_valid": 1,
        "meta_ready": 0,
    }.items():
        one_blocked.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(one_blocked, one_blocked.env, 33)
    one_blocked.set_prefetch_key("meta_ready", 1)
    sample_icache_prefetchpipe_coverage(one_blocked, one_blocked.env, 34)
    assert not _hit(
        one_blocked,
        "icache_prefetchpipe_s1_meta",
        "meta_resend_backpressure_recovery",
    )


def _set_prefetch_full_waylookup(recorder, *, exception_valid=0, num_valid=32):
    for key, value in {
        "s1_valid": 1,
        "s1_state": 3,
        "s1_soft": 0,
        "s1_two_case": 1,
        "s1_flush": 0,
        "s1_ftq_flag": 0,
        "s1_ftq_value": 6,
        "way0_valid": 1,
        "way1_valid": 1,
        "way0_ready": 0,
        "way1_ready": 0,
        "waylookup_num_valid": num_valid,
        "waylookup_exception_valid": exception_valid,
        "refill_valid": 0,
    }.items():
        recorder.set_prefetch_key(key, value)


def test_prefetch_waylookup_recovery_requires_capacity_full_not_exception_stall():
    recorder = _Recorder()
    _set_prefetch_full_waylookup(recorder)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 40)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 41)
    recorder.set_prefetch_key("waylookup_num_valid", 30)
    recorder.set_prefetch_key("way0_ready", 1)
    recorder.set_prefetch_key("way1_ready", 1)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 42)
    assert _hit(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "waylookup_backpressure_recovery",
    )

    for exception_valid, num_valid in ((1, 32), (0, 31)):
        blocked = _Recorder()
        _set_prefetch_full_waylookup(
            blocked,
            exception_valid=exception_valid,
            num_valid=num_valid,
        )
        sample_icache_prefetchpipe_coverage(blocked, blocked.env, 43)
        sample_icache_prefetchpipe_coverage(blocked, blocked.env, 44)
        blocked.set_prefetch_key("way0_ready", 1)
        blocked.set_prefetch_key("way1_ready", 1)
        sample_icache_prefetchpipe_coverage(blocked, blocked.env, 45)
        assert not _hit(
            blocked,
            "icache_prefetchpipe_s1_meta",
            "waylookup_backpressure_recovery",
        )


def _set_prefetch_clean_refill(recorder):
    for key, value in {
        "s1_valid": 1,
        "refill_valid": 1,
        "refill_corrupt": 0,
        "refill_denied": 0,
        "s1_mshr_valid": 1,
        "refill_vset": 7,
        "s1_set0": 7,
        "s1_set1": 8,
        "s1_ptag_same": 1,
    }.items():
        recorder.set_prefetch_key(key, value)


def test_prefetch_clean_refill_rejects_denied_response():
    recorder = _Recorder()
    _set_prefetch_clean_refill(recorder)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 50)
    assert _hit(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "clean_refill_updates_meta",
    )

    denied = _Recorder()
    _set_prefetch_clean_refill(denied)
    denied.set_prefetch_key("refill_denied", 1)
    sample_icache_prefetchpipe_coverage(denied, denied.env, 51)
    assert not _hit(
        denied,
        "icache_prefetchpipe_s1_meta",
        "clean_refill_updates_meta",
    )


def test_prefetch_replacement_requires_per_port_one_hot_old_hit():
    recorder = _Recorder()
    _set_prefetch_clean_refill(recorder)
    for key, value in {
        "s1_ptag_same": 0,
        "refill_waymask": 2,
        "s1_old_way0": 2,
        "s1_old_way1": 4,
    }.items():
        recorder.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 60)
    assert _hit(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "same_way_new_tag_invalidates_old",
    )

    for old0, old1 in ((0, 2), (3, 0)):
        mismatch = _Recorder()
        _set_prefetch_clean_refill(mismatch)
        for key, value in {
            "s1_ptag_same": 0,
            "refill_waymask": 2,
            "s1_old_way0": old0,
            "s1_old_way1": old1,
        }.items():
            mismatch.set_prefetch_key(key, value)
        sample_icache_prefetchpipe_coverage(mismatch, mismatch.env, 61)
        assert not _hit(
            mismatch,
            "icache_prefetchpipe_s1_meta",
            "same_way_new_tag_invalidates_old",
        )


def test_prefetch_dual_layouts_are_independent_bins():
    layouts = {
        1: "dual_layout_same_line",
        2: "dual_layout_overlap1",
        4: "dual_layout_overlap2",
        8: "dual_layout_interleave",
    }
    for cycle, (layout, expected_bin) in enumerate(layouts.items(), start=70):
        recorder = _Recorder()
        recorder.set_prefetch_key("s1_valid", 1)
        recorder.set_prefetch_key("s1_soft", 0)
        recorder.set_prefetch_key("s1_two_case", layout)
        sample_icache_prefetchpipe_coverage(recorder, recorder.env, cycle)
        assert _hit(recorder, "icache_prefetchpipe_s1_meta", expected_bin)
        assert all(
            not _hit(recorder, "icache_prefetchpipe_s1_meta", other_bin)
            for other_bin in set(layouts.values()) - {expected_bin}
        )


def test_prefetch_soft_probe_requires_correlated_meta_response_after_tlb_finish():
    recorder = _Recorder()
    for key, value in {
        "from_valid": 1,
        "from_soft": 1,
        "s0_fire": 1,
        "meta_req_valid": 1,
        "meta_ready": 1,
        "s1_flush": 0,
    }.items():
        recorder.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 80)

    for key, value in {
        "from_valid": 0,
        "from_soft": 0,
        "s0_fire": 0,
        "meta_req_valid": 0,
        "s1_valid": 1,
        "s1_soft": 1,
        "s1_tlb_finish": 1,
        "s1_sram_valid0": 1,
    }.items():
        recorder.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 81)
    assert _hit(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "soft_probe_no_waylookup_ftq",
    )

    uncorrelated = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "s1_soft": 1,
        "s1_tlb_finish": 1,
        "s1_sram_valid0": 1,
        "s1_flush": 0,
    }.items():
        uncorrelated.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(uncorrelated, uncorrelated.env, 82)
    assert not _hit(
        uncorrelated,
        "icache_prefetchpipe_s1_meta",
        "soft_probe_no_waylookup_ftq",
    )


def test_prefetch_redirect_boundary_requires_ready_high_only():
    recorder = _Recorder()
    for key, value in {
        "s2_valid": 1,
        "s2_miss0": 1,
        "miss_valid": 1,
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
        "s2_double": 0,
        "s2_sram0": 0,
        "s2_exception": 0,
        "s2_mmio": 0,
        "s2_ptag": 0x12,
        "refill_valid": 1,
        "refill_corrupt": 1,
        "refill_vset": 7,
        "refill_paddr": 0x12 << 6,
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
        "s2_valid": 1,
        "s2_double": 0,
        "s2_ptag": 0x14,
        "s2_set0": 5,
        "miss_valid": 1,
        "miss_ready": 0,
        "global_flush": 0,
        "refill_valid": 0,
    }.items():
        recorder.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 23)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 24)

    recorder.set_prefetch_key("miss_ready", 1)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 25)
    assert _hit(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "missunit_backpressure_recovery",
    )


def test_prefetch_s2_hit_samples_sram_or_exact_clean_refill_scenarios():
    sram_hit = _Recorder()
    for key, value in {
        "s2_valid": 1,
        "s2_double": 0,
        "s2_sram0": 1,
    }.items():
        sram_hit.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(sram_hit, sram_hit.env, 26)
    assert _hit(sram_hit, "icache_prefetchpipe_s2_miss", "sram_or_clean_mshr_hit")

    clean_refill = _Recorder()
    for key, value in {
        "s2_valid": 1,
        "s2_double": 0,
        "s2_sram0": 0,
        "s2_ptag": 0x21,
        "s2_set0": 6,
        "refill_valid": 1,
        "refill_corrupt": 0,
        "refill_vset": 6,
        "refill_paddr": 0x21 << 6,
    }.items():
        clean_refill.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(clean_refill, clean_refill.env, 27)
    assert _hit(
        clean_refill,
        "icache_prefetchpipe_s2_miss",
        "sram_or_clean_mshr_hit",
    )


def test_prefetch_clean_refill_cancel_requires_exact_s2_match():
    recorder = _Recorder()
    for key, value in {
        "s2_valid": 1,
        "s2_double": 0,
        "s2_ptag": 0x31,
        "s2_set0": 9,
        "miss_valid": 1,
        "miss_ready": 0,
        "global_flush": 0,
        "refill_valid": 0,
    }.items():
        recorder.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 28)

    for key, value in {
        "refill_valid": 1,
        "refill_corrupt": 0,
        "refill_vset": 9,
        "refill_paddr": 0x32 << 6,
    }.items():
        recorder.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 29)
    assert not _hit(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "clean_mshr_cancels_backpressured_miss",
    )

    for key, value in {
        "refill_paddr": 0x31 << 6,
    }.items():
        recorder.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 30)
    assert _hit(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "clean_mshr_cancels_backpressured_miss",
    )


def test_prefetch_protection_condition_uses_s1_meta_miss_and_raw_source():
    recorder = _Recorder()
    for key, value in {
        "s1_real_fire": 1,
        "s1_double": 0,
        "s1_sram_hit0": 0,
        "s1_pbmt": 1,
    }.items():
        recorder.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 30)
    assert _hit(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "exception_or_mmio_suppresses",
    )


def test_prefetch_bpu_flush_requires_tracked_s2_ftq_match():
    recorder = _Recorder()
    for key, value in {
        "s1_real_fire": 1,
        "s1_soft": 0,
        "s1_ftq_flag": 0,
        "s1_ftq_value": 5,
        "s2_valid": 0,
        "global_flush": 0,
    }.items():
        recorder.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 31)

    for key, value in {
        "s1_real_fire": 0,
        "s2_valid": 1,
        "miss_valid": 1,
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 5,
    }.items():
        recorder.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(recorder, recorder.env, 32)
    assert _hit(recorder, "icache_prefetchpipe_s2_miss", "bpu_flush_keeps_s2")

    nonmatch = _Recorder()
    for key, value in {
        "s1_real_fire": 1,
        "s1_soft": 0,
        "s1_ftq_flag": 0,
        "s1_ftq_value": 5,
        "s2_valid": 0,
        "global_flush": 0,
    }.items():
        nonmatch.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(nonmatch, nonmatch.env, 33)
    for key, value in {
        "s1_real_fire": 0,
        "s2_valid": 1,
        "miss_valid": 1,
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 6,
    }.items():
        nonmatch.set_prefetch_key(key, value)
    sample_icache_prefetchpipe_coverage(nonmatch, nonmatch.env, 34)
    assert not _hit(nonmatch, "icache_prefetchpipe_s2_miss", "bpu_flush_keeps_s2")


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
        "io_flush": 0,
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
        "io_flush": 0,
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 8,
        "s1_ftq_flag": 0,
        "s1_ftq_value": 7,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 6)
    assert _hit(recorder, "icache_mainpipe_s1_flush", "bpu_miss_keeps_s1")


def test_mainpipe_bpu_s1_bins_require_global_flush_low_and_known_pointers():
    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "s1_flush": 1,
        "io_flush": 1,
        "bpu_valid": 1,
        "bpu_flag": 0,
        "bpu_value": 7,
        "s1_ftq_flag": 0,
        "s1_ftq_value": 7,
    }.items():
        recorder.set_key(key, value)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 7)
    assert not _hit(recorder, "icache_mainpipe_s1_flush", "bpu_match_clears_s1")

    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "s1_flush": 1,
        "io_flush": 0,
        "bpu_valid": 1,
    }.items():
        recorder.set_key(key, value)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 8)
    assert not _hit(recorder, "icache_mainpipe_s1_flush", "bpu_match_clears_s1")


def test_missunit_backpressure_requires_two_consecutive_stall_cycles():
    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "s1_flush": 0,
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

    # Payload stability is a Checkpoint, so it is not part of this Condition.
    recorder.set_key("miss_req_vset", 7)
    recorder.set_key("miss_req_paddr", 0x90000000)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 11)
    assert _hit(recorder, "icache_mainpipe_s1_miss", "missunit_backpressure_stable")


def test_missunit_backpressure_sequence_resets_when_s1_is_flushed():
    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "s1_flush": 0,
        "miss_req_ready": 0,
    }.items():
        recorder.set_key(key, value)
    recorder.env.dut.set(_MAIN + "s1_shouldFetch_0", 1)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 20)
    recorder.set_key("s1_flush", 1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 21)
    recorder.set_key("s1_flush", 0)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 22)

    assert not _hit(recorder, "icache_mainpipe_s1_miss", "missunit_backpressure_stable")

    sample_icache_mainpipe_coverage(recorder, recorder.env, 23)
    assert _hit(recorder, "icache_mainpipe_s1_miss", "missunit_backpressure_stable")


def test_global_s1_flush_samples_io_flush_without_checkpoint_outputs():
    recorder = _Recorder()
    _set_mainpipe_single_sram_hit(recorder)
    for key, value in {
        "s1_flush": 0,
        "io_flush": 1,
        "bpu_valid": 0,
        "req1_valid": 0,
        "cross1": 0,
        "pmp_mmio": 0,
        "is_mmio": 0,
        "itlb_exception": 0,
        "exception": 0,
        # These are deliberately the opposite of the expected checkpoint.
        "toifu_valid": 1,
        "s1_fire": 1,
    }.items():
        recorder.set_key(key, value)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 15)

    assert _hit(recorder, "icache_mainpipe_s1_flush", "global_flush_clears_s1_hit")


def test_global_s1_flush_pending_miss_samples_separate_bin():
    recorder = _Recorder()
    recorder.set_key("s1_valid", 1)
    recorder.set_key("io_flush", 1)
    recorder.env.dut.set(_MAIN + "s1_shouldFetch_0", 1)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 16)

    assert _hit(
        recorder,
        "icache_mainpipe_s1_flush",
        "global_flush_clears_s1_pending_miss",
    )
    assert not _hit(recorder, "icache_mainpipe_s1_flush", "global_flush_clears_s1_hit")


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
    for key, value in {
        "s1_valid": 1,
        "s1_flush": 0,
        "req1_valid": 1,
        "cross0": 1,
        "cross1": 1,
        "miss_req_ready": 1,
    }.items():
        recorder.set_key(key, value)
    # Do not provide missReq.valid or hasSend observations: those belong to
    # the fixed-priority Checkpoint, not to the four-line stimulus Condition.
    for index in range(4):
        recorder.env.dut.set(_MAIN + f"s1_shouldFetch_{index}", 1)

    for cycle in range(16, 19):
        sample_icache_mainpipe_coverage(recorder, recorder.env, cycle)
        assert not _hit(recorder, "icache_mainpipe_s1_miss", "four_line_fixed_priority")

    recorder.set_key("cross1", 0)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 19)
    assert not _hit(recorder, "icache_mainpipe_s1_miss", "four_line_fixed_priority")

    recorder.set_key("cross1", 1)
    for cycle in range(20, 23):
        sample_icache_mainpipe_coverage(recorder, recorder.env, cycle)
        assert not _hit(recorder, "icache_mainpipe_s1_miss", "four_line_fixed_priority")

    sample_icache_mainpipe_coverage(recorder, recorder.env, 23)
    assert _hit(recorder, "icache_mainpipe_s1_miss", "four_line_fixed_priority")


def test_registered_refill_is_cancelled_by_next_cycle_flush():
    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "miss_resp_valid": 1,
        "io_flush": 0,
        "s1_flush": 0,
    }.items():
        recorder.set_key(key, value)
    recorder.env.dut.set(_MAIN + "s1_mshrValid_0_0", 1)
    recorder.env.dut.set(_MAIN + "s1_shouldFetch_0", 1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 30)

    recorder.set_key("miss_resp_valid", 0)
    recorder.set_key("io_flush", 1)
    recorder.set_key("s1_flush", 0)
    recorder.env.dut.set(_MAIN + "s1_mshrValid_0_0", 0)
    recorder.env.dut.set(_MAIN + "s1_mshrValidReg_0_0", 1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 31)

    assert _hit(
        recorder,
        "icache_mainpipe_s1_flush",
        "flush_cancels_registered_refill",
    )


def test_late_refill_samples_flush_before_refill_without_response_or_send():
    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "s1_flush": 0,
        "io_flush": 1,
        "bpu_valid": 0,
        "miss_resp_valid": 0,
    }.items():
        recorder.set_key(key, value)
    recorder.env.dut.set(_MAIN + "s1_shouldFetch_0", 1)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 20)
    assert _hit(recorder, "icache_mainpipe_s1_flush", "late_refill_ignored_after_flush")


def test_same_cycle_refill_flush_requires_global_flush_and_match():
    recorder = _Recorder()
    for key, value in {
        "s1_valid": 1,
        "io_flush": 1,
        "s1_flush": 0,
        "miss_resp_valid": 1,
    }.items():
        recorder.set_key(key, value)
    recorder.env.dut.set(_MAIN + "s1_mshrValid_0_0", 1)

    sample_icache_mainpipe_coverage(recorder, recorder.env, 22)
    assert _hit(recorder, "icache_mainpipe_s1_flush", "flush_wins_matching_refill")


def _prime_mainpipe_s2(recorder, *, bank_mshr: int | None = None) -> None:
    for key, value in {
        "s1_fire": 1,
        "req1_valid": 0,
        "cross0": 0,
        "cross1": 0,
    }.items():
        recorder.set_key(key, value)
    if bank_mshr is not None:
        recorder.env.dut.set(
            _MAIN + f"s1_bankMshrValidReg_0_{bank_mshr}",
            1,
        )
    sample_icache_mainpipe_coverage(recorder, recorder.env, 40)
    for key, value in {
        "s1_fire": 0,
        "s2_valid": 1,
        "local_ecc_enable": 1,
    }.items():
        recorder.set_key(key, value)
    recorder.env.dut.set(_MAIN + "s2_isCrossLine_0", 0)
    recorder.env.dut.set(_MAIN + "s2_isCrossLine_1", 0)


def _set_mainpipe_meta_source(
    recorder,
    *,
    line_index: int,
    hitnum: int,
    mismatch: bool,
) -> None:
    req, line = divmod(line_index, 2)
    suffix = "" if line_index == 0 else f"_{line_index}"
    recorder.env.dut.set(
        _MAIN + f"s2_corruptInfo_metaCorrupt_hitNum{suffix}",
        hitnum,
    )
    recorder.env.dut.set(_MAIN + "s2_pTag", 0)
    recorder.env.dut.set(
        _MAIN + f"s2_wayLookupEntry_{req}_maybeRvcMap_{line}",
        0,
    )
    recorder.env.dut.set(
        _MAIN + f"s2_wayLookupEntry_{req}_metaCodes_{line}",
        int(mismatch),
    )


def test_mainpipe_meta_ecc_bins_sample_distinct_source_conditions():
    cases = (
        (1, True, "meta_code_mismatch_single_way"),
        (2, False, "meta_multiway_hit"),
        (0, True, "meta_code_mismatch_zero_way_ignored"),
    )
    all_bins = {case[2] for case in cases}
    for hitnum, mismatch, expected in cases:
        recorder = _Recorder()
        _prime_mainpipe_s2(recorder)
        _set_mainpipe_meta_source(
            recorder,
            line_index=0,
            hitnum=hitnum,
            mismatch=mismatch,
        )
        sample_icache_mainpipe_coverage(recorder, recorder.env, 41)
        assert _hit(recorder, "icache_mainpipe_s2_ecc", expected)
        for other in all_bins - {expected}:
            assert not _hit(recorder, "icache_mainpipe_s2_ecc", other)


def test_mainpipe_meta_invalid_line_has_separate_masking_bin():
    recorder = _Recorder()
    _prime_mainpipe_s2(recorder)
    _set_mainpipe_meta_source(
        recorder,
        line_index=1,
        hitnum=1,
        mismatch=True,
    )
    sample_icache_mainpipe_coverage(recorder, recorder.env, 41)
    assert _hit(recorder, "icache_mainpipe_s2_ecc", "meta_invalid_line_masked")
    assert not _hit(
        recorder,
        "icache_mainpipe_s2_ecc",
        "meta_code_mismatch_single_way",
    )


def _set_mainpipe_data_source(
    recorder,
    *,
    bank: int,
    bank_valid: int,
    port_hit: int,
    offset: int = 0,
) -> None:
    recorder.env.dut.set(_MAIN + "s2_offset_0", offset)
    recorder.env.dut.set(_MAIN + f"s2_sramDatas_0_{bank}", 1)
    recorder.env.dut.set(_MAIN + f"s2_sramCodes_0_{bank}", 0)
    recorder.env.dut.set(_MAIN + f"s2_bankSramValid_0_{bank}", bank_valid)
    recorder.env.dut.set(_MAIN + "s2_sramHits_0_0", port_hit)


def test_mainpipe_data_ecc_selected_and_unselected_bins_are_distinct():
    recorder = _Recorder()
    _prime_mainpipe_s2(recorder)
    _set_mainpipe_data_source(recorder, bank=0, bank_valid=1, port_hit=1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 41)
    assert _hit(
        recorder,
        "icache_mainpipe_s2_ecc",
        "data_ecc_selected_valid_sram_bank",
    )
    assert not _hit(
        recorder,
        "icache_mainpipe_s2_ecc",
        "data_ecc_unselected_bank_ignored",
    )

    recorder = _Recorder()
    _prime_mainpipe_s2(recorder)
    _set_mainpipe_data_source(recorder, bank=1, bank_valid=1, port_hit=1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 41)
    assert _hit(
        recorder,
        "icache_mainpipe_s2_ecc",
        "data_ecc_unselected_bank_ignored",
    )
    assert not _hit(
        recorder,
        "icache_mainpipe_s2_ecc",
        "data_ecc_selected_valid_sram_bank",
    )


def test_mainpipe_data_ecc_mshr_bypass_and_port_miss_bins():
    recorder = _Recorder()
    _prime_mainpipe_s2(recorder, bank_mshr=0)
    _set_mainpipe_data_source(recorder, bank=0, bank_valid=0, port_hit=0)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 41)
    assert _hit(
        recorder,
        "icache_mainpipe_s2_ecc",
        "data_ecc_mshr_bypass_skips_sram_bank",
    )

    recorder = _Recorder()
    _prime_mainpipe_s2(recorder)
    _set_mainpipe_data_source(recorder, bank=0, bank_valid=1, port_hit=0)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 41)
    assert _hit(
        recorder,
        "icache_mainpipe_s2_ecc",
        "data_ecc_port_miss_ignored",
    )


def test_mainpipe_s2_global_and_bpu_flush_bins_are_independent():
    recorder = _Recorder()
    for key, value in {
        "s2_valid": 1,
        "io_flush": 1,
        "bpu_valid": 0,
    }.items():
        recorder.set_key(key, value)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 50)
    assert _hit(recorder, "icache_mainpipe_s2_ecc", "global_flush_clears_s2")
    assert not _hit(recorder, "icache_mainpipe_s2_ecc", "bpu_s3_flush_keeps_s2")

    recorder = _Recorder()
    for key, value in {
        "s2_valid": 1,
        "io_flush": 0,
        "bpu_valid": 1,
    }.items():
        recorder.set_key(key, value)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 51)
    assert _hit(recorder, "icache_mainpipe_s2_ecc", "bpu_s3_flush_keeps_s2")
    assert not _hit(recorder, "icache_mainpipe_s2_ecc", "global_flush_clears_s2")
