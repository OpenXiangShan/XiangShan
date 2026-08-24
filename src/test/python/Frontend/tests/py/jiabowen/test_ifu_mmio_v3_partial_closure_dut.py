from __future__ import annotations

import pytest

from env.funcov.py.ifu.mmio_v3_funcov import MMIO_V3_CHECKED_EVENT_TYPE
from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


@pytest.mark.funcov_bins("BIN-1012")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_tl_a_stall_holds_request_context(env):
    uncache._prepare_mmio_cnop_stream(env)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_req(env) > 0
    assert uncache._wait_for_uncache_resp(env) > 0

    env.uncache_agent.set_a_ready(0)
    for _ in range(256):
        env.step(1)
        if int(env.uncache_if.a_valid.value) == 1:
            break

    req_before = int(env.uncache_agent.get_stats().get("req_count", 0))
    assert int(env.uncache_if.a_ready.value) == 0
    assert int(env.uncache_if.a_valid.value) == 1
    stalled_addr = int(env.uncache_if.a_bits_address.value)

    env.step(8)
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == req_before
    assert int(env.uncache_if.a_valid.value) == 1
    assert int(env.uncache_if.a_bits_address.value) == stalled_addr
    assert env.functional_coverage.key_hit("ifu_mmio_tl_a_stall", "stable_until_accept")

    env.uncache_agent.set_a_ready(None)
    assert uncache._wait_for_uncache_req_delta(env, 1)
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1013")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_page_tail_rvc_delivers_next_pc_plus_2b(env):
    uncache._prepare_cross_page_rvc_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=16)
    uncache._initialize_mmio_fetch(env, reset_vector=uncache._CROSS_PAGE_PC)

    first_beat = uncache._CROSS_PAGE_PC & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=5000)
    assert uncache._wait_for_observed_pc(env, uncache._CROSS_PAGE_PC, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, uncache._CROSS_PAGE_PC + 2, max_cycles=8000), {
        "observed": [
            (int(obs.pc), int(obs.instr), bool(obs.is_rvc))
            for obs in env.monitor.observations[-16:]
        ],
        "uncache": env.uncache_agent.get_stats(),
    }

    assert env.functional_coverage.key_hit("ifu_mmio_page_tail", "next_pc_plus_2b")
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1014")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_cross_page_first_page_iaf_beats_illegal_instruction(env):
    cross_page_va = uncache._NORMAL_BASE + uncache._SV39_PAGE_SIZE - 2
    cross_page_pa = uncache._MMIO_BASE + uncache._SV39_PAGE_SIZE - 2
    first_page_pa = cross_page_pa & ~(uncache._SV39_PAGE_SIZE - 1)
    second_page_pa = first_page_pa + uncache._SV39_PAGE_SIZE
    payload = int(uncache._ADDI_X0_X0_0).to_bytes(4, "little")

    uncache._initialize_sv39_fetch(env, reset_vector=cross_page_va)
    scenario = uncache.TranslationScenario(
        scenario_id="bin-1007-mmio-cross-page-first-iaf",
        va=cross_page_va,
        pa=cross_page_pa,
        payload=payload,
        page_count=2,
        expected_path="fault",
        expected_result="access_fault",
        pmp_entries=(
            uncache.TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=uncache.PmpPmaConfig(
                    match="napot", read=True, write=True, execute=False
                ),
                addr=first_page_pa,
                size=uncache._SV39_PAGE_SIZE,
            ),
            uncache.TranslationPmpPmaEntry(
                kind="pmp",
                index=1,
                config=uncache.PmpPmaConfig(
                    match="napot", read=True, write=True, execute=True
                ),
                addr=second_page_pa,
                size=uncache._SV39_PAGE_SIZE,
            ),
        ),
        pma_entries=(
            uncache.TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=uncache.PmpPmaConfig(
                    match="napot", read=True, write=True, execute=True, cacheable=False
                ),
                addr=first_page_pa,
                size=uncache._SV39_PAGE_SIZE,
            ),
            uncache.TranslationPmpPmaEntry(
                kind="pma",
                index=1,
                config=uncache.PmpPmaConfig(
                    match="napot", read=True, write=True, execute=True, cacheable=False
                ),
                addr=second_page_pa,
                size=uncache._SV39_PAGE_SIZE,
            ),
        ),
    )
    state = uncache.TranslationScenarioBuilder(env).build(scenario)
    first_permission = state.expected_page_outcomes[0]["permission"]
    second_permission = state.expected_page_outcomes[1]["permission"]
    assert first_permission["execute_allowed"] is False
    assert second_permission["execute_allowed"] is True

    ifu_exceptions = []
    backend_exceptions = []

    def capture_exception_result(cycle, active_env):
        recorder = active_env.functional_coverage
        dut = active_env.dut
        to_valid = recorder._read_first_dut_signal(
            dut,
            (
                "Frontend_top.Frontend.inner_ifu.io_toIBuffer_valid",
                "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_valid",
                "Frontend_top.Frontend._inner_ifu_io_toIBuffer_valid",
                "Frontend.inner_ifu.io_toIBuffer_valid",
            ),
        )
        if to_valid == 1:
            ifu_exceptions.append(
                recorder._read_first_dut_signal(
                    dut,
                    (
                        "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_bits_exceptionType_value",
                        "Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_exceptionType_value",
                        "Frontend.inner_ifu.io_toIBuffer_bits_exceptionType_value",
                    ),
                )
            )
        monitor_if = active_env.monitor.interface
        for slot in range(8):
            if int(monitor_if.cfvec_valid[slot].value) != 1:
                continue
            backend_exceptions.append(
                {
                    "cycle": int(cycle),
                    "pc": int(monitor_if.cfvec_pc[slot].value),
                    "iaf": int(monitor_if.cfvec_exception_vec[slot][1].value),
                    "illegal": int(monitor_if.cfvec_exception_vec[slot][2].value),
                }
            )

    env.register_cycle_observer(capture_exception_result)
    env.monitor.clear()
    env.monitor.set_expected_pc(cross_page_va)
    uncache._force_redirect_to(env, cross_page_va)

    for _ in range(4000):
        env.step(1)
        exception_results = [
            item for item in backend_exceptions if item["iaf"] or item["illegal"]
        ]
        if any(value == 3 for value in ifu_exceptions) and any(
            item["iaf"] == 1 and item["illegal"] == 0
            for item in exception_results
        ):
            break

    exception_results = [
        item for item in backend_exceptions if item["iaf"] or item["illegal"]
    ]
    delivered_exception = next((value for value in ifu_exceptions if value), None)
    assert delivered_exception == 3, {"ifu_exceptions": ifu_exceptions}
    assert exception_results, {"backend_exceptions": backend_exceptions}
    assert exception_results[0]["iaf"] == 1
    assert exception_results[0]["illegal"] == 0
    assert not env.monitor.get_errors()

    env._emit_event(
        MMIO_V3_CHECKED_EVENT_TYPE,
        {
            "bin_id": "BIN-1014",
            "condition_met": True,
            "checkpoint_passed": True,
            "observations": {
                "first_page_execute": False,
                "second_page_execute": True,
                "delivered_exception": delivered_exception,
                "illegal_instruction": False,
            },
            "producer": "test_mmio_cross_page_first_page_iaf_beats_illegal_instruction",
        },
    )
    assert env.functional_coverage.key_hit(
        "ifu_mmio_exception_priority", "second_page_exec_not_illegal"
    )


@pytest.mark.funcov_bins("BIN-1015")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_response_uses_reserved_ibuffer_slot_under_backend_pressure(env):
    uncache._prepare_mmio_cnop_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=16)
    env.backend_model.set_can_accept(0)
    uncache._initialize_mmio_fetch(env)

    recorder = env.functional_coverage
    assert uncache._wait_for_uncache_req(env) > 0
    assert uncache._wait_for_uncache_resp(env) > 0
    for _ in range(32):
        env.step(1)
        if recorder.key_hit("ifu_mmio_backpressure", "reserved_slot_fire"):
            break

    assert recorder.key_hit("ifu_mmio_backpressure", "reserved_slot_fire")
    env.backend_model.set_can_accept(1)
    env.step(32)
    assert not env.monitor.get_errors()
