from types import SimpleNamespace

from env.funcov.py.icache.sampler import (
    ICACHE_MAINPIPE_SAMPLER_BIN_KEYS,
    ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS,
    _MAIN,
    _PREFETCH_SIGNALS,
    _SIGNALS,
    sample_icache_mainpipe_coverage,
    sample_icache_prefetchpipe_coverage,
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


def _hit(recorder, group, bin_name):
    return any(hit[0] == group and hit[2] == bin_name for hit in recorder.hits)


def test_icache_mainpipe_sampler_contract_has_one_key_per_leaf():
    assert len(ICACHE_MAINPIPE_SAMPLER_BIN_KEYS) == 49
    assert len(set(ICACHE_MAINPIPE_SAMPLER_BIN_KEYS)) == 49


def test_icache_prefetchpipe_sampler_contract_has_one_key_per_leaf():
    assert len(ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS) == 24
    assert len(set(ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS)) == 24


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


def test_meta_error_condition_does_not_require_flush_waymask_checkpoint():
    recorder = _Recorder()
    recorder.set_key("s1_fire", 1)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 30)

    for key, value in {
        "s1_fire": 0,
        "ecc_enable": 1,
        "error_valid": 1,
        "error_meta": 1,
        # Deliberately violate the expected MetaFlush checkpoint.
        "meta_flush0_valid": 0,
        "meta_flush1_valid": 0,
        "meta_flush0_waymask": 0,
        "meta_flush1_waymask": 0,
    }.items():
        recorder.set_key(key, value)
    sample_icache_mainpipe_coverage(recorder, recorder.env, 31)

    assert _hit(recorder, "icache_mainpipe_s2_meta_flush", "meta_error_flush_all_ways")


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
