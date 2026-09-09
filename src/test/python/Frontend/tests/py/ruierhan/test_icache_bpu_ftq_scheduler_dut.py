"""Public scheduler integration and exact MainPipe BPU-flush reachability tests."""

import os

import pytest

from tests.py.ruierhan import test_icache_mainpipe_s0_flush_closure_dut as s0
from tests.py.ruierhan import test_icache_mainpipe_s1_flush_closure_dut as s1


pytestmark = pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires compiled DUT")


def test_bpu_scheduler_drives_live_identity_resolve(env):
    base = 0x80000000
    # Use decoded control-flow instructions so the captured identity is a
    # legal source for directed resolve injection.  JAL x0,+4 keeps execution
    # inside the loaded image while providing an exact decoded target.
    jal_x0_plus_4 = 0x0040006F
    env.load_program(jal_x0_plus_4.to_bytes(4, "little") * 16, base)
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)
    identity = env.bpu_ftq_scheduler.wait_live_identity(
        cfi_only=True,
        max_cycles=1000,
    )
    assert identity["is_cfi"] is True
    target = int(identity["actual_target"])
    env.backend_model.set_can_accept(0)
    observed = []
    def sample(_cycle, active):
        bundle = active.backend_ctrl_if
        for lane in range(3):
            if int(bundle.resolve_valid[lane].value):
                observed.append((int(bundle.resolve_bits_ftq_idx_flag[lane].value),
                    int(bundle.resolve_bits_ftq_idx_value[lane].value),
                    int(bundle.resolve_bits_pc_addr[lane].value) << 1,
                    int(bundle.resolve_bits_target_addr[lane].value) << 1))
    env.register_cycle_observer(sample)
    try:
        env.bpu_ftq_scheduler.queue_mispredict(identity, target=target)
        env.step(4)
        assert (
            identity["ftq_flag"],
            identity["ftq_value"],
            identity["start_pc"],
            target,
        ) in observed
        assert not env.get_errors()
    finally:
        env.backend_model.set_can_accept(1)


@pytest.mark.funcov_closure_pending
@pytest.mark.xfail(strict=True, raises=AssertionError,
                   reason="exact BPU stage-3/MainPipe identity alignment remains a reachability target")
@pytest.mark.parametrize("stage,bin_name", [
    pytest.param(0, "bpu_match_cancels_entry", marks=pytest.mark.funcov_bins("BIN-606")),
    pytest.param(1, "bpu_match_clears_s1", marks=pytest.mark.funcov_bins("BIN-617")),
])
def test_icache_scheduled_bpu_matching_flush(env, stage, bin_name):
    module = s0 if stage == 0 else s1
    module._require_bpu_s3_ftq_observable(env)
    s0._initialize_bpu_s3_stream(env)
    scheduler = env.bpu_ftq_scheduler
    samples = []
    env.register_cycle_observer(lambda _, active: samples.append(module._snapshot(active)))
    group = f"icache_mainpipe_s{stage}_flush"
    match = s0._bpu_matches_s0 if stage == 0 else s1._bpu_matches_s1
    try:
        for attempt in range(3):
            # Queue training only from an observed live identity. The actual
            # BPU s3 pointer must subsequently satisfy the sampler predicate.
            identity = scheduler.wait_live_identity(cfi_only=True, max_cycles=128)
            scheduler.queue_mispredict(identity, target=int(identity["actual_target"]))
            env.step(4)
            scheduler.pulse_predictor_transition()
            for _ in range(128):
                env.step(1)
                if env.functional_coverage.key_hit(group, bin_name):
                    assert any(s["bpu_valid"] == 1 and match(s) for s in samples)
                    assert not env.get_errors()
                    return
            s0._restore_predictors(env)
        raise AssertionError({"reason": "BPU matching flush not observed", "stage": stage,
                              "tail": samples[-8:], "checker": env.get_errors()[:8]})
    finally:
        s0._restore_predictors(env)
        env.backend_model.set_can_accept(1)
