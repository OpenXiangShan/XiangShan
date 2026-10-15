"""Public scheduler integration and exact MainPipe BPU-flush reachability tests."""

from env.runtime.frontend_init import (
    FrontendInitConfig,
    initialize_frontend,
)


import os

import pytest

from tests.py.icache.directed_dut import test_icache_mainpipe_s0_flush_closure_dut as s0
from tests.py.icache.directed_dut import test_icache_mainpipe_s1_flush_closure_dut as s1
from tests.py.icache.directed_dut.support import (
    restore_predictors,
)

from tests.py.icache.directed_dut.init_plans import (
    make_bpu_s3_init_config,
    make_live_cfi_init_config,
)


pytestmark = pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires compiled DUT")


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)
def test_bpu_scheduler_drives_live_identity_resolve(env):
    base = 0x80000000
    # Use decoded control-flow instructions so the captured identity is a
    # legal source for directed resolve injection.  JAL x0,+4 keeps execution
    # inside the loaded image while providing an exact decoded target.
    initialize_frontend(env, make_live_cfi_init_config(env.config, base))
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
    initialize_frontend(env, make_bpu_s3_init_config(env.config))
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
            restore_predictors(env)
        raise AssertionError({"reason": "BPU matching flush not observed", "stage": stage,
                              "tail": samples[-8:], "checker": env.get_errors()[:8]})
    finally:
        restore_predictors(env)
        env.backend_model.set_can_accept(1)
