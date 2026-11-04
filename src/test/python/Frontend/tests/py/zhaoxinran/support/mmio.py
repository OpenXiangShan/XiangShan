"""MMIO redirect scenarios shared by directed tests."""

from env.core.transactions import BackendRedirectClass
from env.funcov.py.ifu import mmio_nc_owner_funcov as owner_funcov
from tests.py.support.dut_support import _wait_for_req_delta, _wait_for_resp_delta
from tests.py.support import uncache_scenarios as uncache


def _run_mmio_redirect_cancels_wait_last_commit_before_request(
    env,
    *,
    target_pc: int = uncache._MMIO_BASE + 0x40,
    latency: int | None = None,
):
    """Redirect a non-first MMIO while it is still commit ordered."""
    uncache._prepare_mmio_cnop_stream(env)
    if latency is not None:
        env.uncache_agent.configure(latency=latency, mmio_latency=latency)
    env.backend_model.set_can_accept(1)
    env.backend_model.commit_min_delay = 1000
    env.backend_model.commit_max_delay = 1000
    env.backend_model.backend_empty_for_dut = lambda: 0
    snapshots: list[dict[str, int | None]] = []

    def capture(cycle: int, active_env) -> None:
        snapshots.append(
            {
                "cycle": int(cycle),
                **owner_funcov._snapshot(
                    active_env.functional_coverage, active_env.dut
                ),
            }
        )

    env.register_cycle_observer(capture)
    uncache._initialize_mmio_fetch(env)

    assert _wait_for_req_delta(env, env.uncache_agent)
    assert _wait_for_resp_delta(env, env.uncache_agent)
    for _ in range(8000):
        if snapshots and snapshots[-1]["uncache_state"] == uncache._IFU_UNCACHE_WAIT_LAST_COMMIT:
            break
        env.step(1)
    assert snapshots[-1]["uncache_state"] == uncache._IFU_UNCACHE_WAIT_LAST_COMMIT, {
        "snapshots": snapshots[-64:]
    }

    old_req_count = int(env.uncache_agent.get_stats().get("req_count", 0))
    observations_before_redirect = len(env.monitor.observations)
    source = next(
        entry
        for entry in env.backend_model._cfvec_queue
        if int(entry.pc) == uncache._MMIO_BASE
    )
    env.backend_model.inject_redirect_from_cfvec(
        source_pc=int(source.pc),
        source_ftq_flag=int(source.ftq_flag),
        source_ftq_value=int(source.ftq_value),
        source_ftq_offset=int(source.ftq_offset),
        target_pc=target_pc,
        reason="mmio_wait_last_commit_cancel",
        redirect_class=BackendRedirectClass.MEMORY_VIOLATION,
        delay_cycles=0,
    )
    env.step(8)

    assert any(
        sample["uncache_state"] == uncache._IFU_UNCACHE_WAIT_LAST_COMMIT
        and sample["backend_redirect"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}
    assert any(
        sample["uncache_state"] == uncache._IFU_UNCACHE_WAIT_LAST_COMMIT
        and sample["ifu_flush"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == old_req_count

    env.backend_model.backend_empty_for_dut = lambda: 1
    assert uncache._wait_for_request_addr(env, target_pc, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, target_pc, max_cycles=8000)
    assert not any(
        int(item.pc) == uncache._MMIO_BASE
        for item in env.monitor.observations[observations_before_redirect:]
    )
    assert not env.monitor.get_errors()
