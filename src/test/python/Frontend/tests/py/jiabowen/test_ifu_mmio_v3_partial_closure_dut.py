from __future__ import annotations

import pytest

from env.funcov.py.ifu import mmio_nc_owner_funcov as owner_funcov
from env.funcov.py.ifu.mmio_v3_funcov import MMIO_V3_CHECKED_EVENT_TYPE
from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


def _two_rvi_cross_beat_payload() -> bytes:
    payload = bytearray(int(uncache._CNOP).to_bytes(2, "little") * 3)
    payload.extend(int(uncache._ADDI_X0_X0_0).to_bytes(4, "little") * 2)
    payload.extend(int(uncache._CNOP).to_bytes(2, "little") * 128)
    return bytes(payload)


def _cross_page_payload(*, rvi_tail: bool) -> bytes:
    payload = bytearray(
        int(uncache._CNOP).to_bytes(2, "little")
        * (uncache._SV39_PAGE_SIZE // 2 + 128)
    )
    if rvi_tail:
        payload[
            uncache._SV39_PAGE_SIZE - 2 : uncache._SV39_PAGE_SIZE + 2
        ] = int(uncache._ADDI_X0_X0_0).to_bytes(4, "little")
    return bytes(payload)


def _branch_payload() -> bytes:
    # beq x0, x0, +4 followed by a sequential RVI and RVC padding.
    payload = bytearray(int(0x00000263).to_bytes(4, "little"))
    payload.extend(int(uncache._ADDI_X0_X0_0).to_bytes(4, "little"))
    payload.extend(int(uncache._CNOP).to_bytes(2, "little") * 128)
    return bytes(payload)


def _register_cross_8b_trace(env) -> list[dict[str, int | None]]:
    trace: list[dict[str, int | None]] = []

    def capture(cycle, active_env):
        snapshot = owner_funcov._snapshot(
            active_env.functional_coverage, active_env.dut
        )
        if not any(
            snapshot[name] == 1
            for name in ("tl_a_valid", "tl_d_valid", "instr_resp_valid", "resp_valid")
        ):
            return
        trace.append(
            {
                "cycle": int(cycle),
                "entry_state": snapshot["entry_state"],
                "resending": snapshot["entry_resending"],
                "req_addr": snapshot["entry_req_addr"],
                "tl_a_valid": snapshot["tl_a_valid"],
                "tl_a_ready": snapshot["tl_a_ready"],
                "tl_d_valid": snapshot["tl_d_valid"],
                "tl_d_data": snapshot["tl_d_data"],
                "instr_resp_valid": snapshot["instr_resp_valid"],
                "resp_valid": snapshot["resp_valid"],
                "to_valid": snapshot["to_valid"],
                "to_ready": snapshot["to_ready"],
            }
        )

    env.register_cycle_observer(capture)
    return trace


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


@pytest.mark.funcov_bins("BIN-1045")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_cross_page_second_page_pf_attributes_to_rvi_start(env):
    """Exercise the owner leaf's first-half/second-page fault contract."""
    cross_page_va = uncache._NORMAL_BASE + uncache._SV39_PAGE_SIZE - 2
    cross_page_pa = uncache._MMIO_BASE + uncache._SV39_PAGE_SIZE - 2
    next_page_pa = uncache._MMIO_BASE + uncache._SV39_PAGE_SIZE
    payload = int(uncache._ADDI_X0_X0_0).to_bytes(4, "little") + int(uncache._CNOP).to_bytes(2, "little") * 64

    uncache._initialize_sv39_fetch(env, reset_vector=cross_page_va)
    scenario = uncache.TranslationScenario(
        scenario_id="bin-1045-mmio-cross-page-second-pf",
        va=cross_page_va,
        pa=cross_page_pa,
        payload=payload,
        page_count=2,
        s1_pte=uncache.TranslationPte(pbmt=uncache._PBMT_IO),
        expected_path="fault",
        pmp_entries=(
            uncache.TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=uncache.PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=uncache._MMIO_BASE,
                size=0x1000,
            ),
            uncache.TranslationPmpPmaEntry(
                kind="pmp",
                index=1,
                config=uncache.PmpPmaConfig(match="napot", read=True, write=True, execute=True),
                addr=next_page_pa,
                size=0x1000,
            ),
        ),
        pma_entries=(
            uncache.TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=uncache.PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=False),
                addr=uncache._MMIO_BASE,
                size=0x1000,
            ),
            uncache.TranslationPmpPmaEntry(
                kind="pma",
                index=1,
                config=uncache.PmpPmaConfig(match="napot", read=True, write=True, execute=True, cacheable=False),
                addr=next_page_pa,
                size=0x1000,
            ),
        ),
        ptw_response_overrides=(
            uncache.TranslationPtwResponseOverride(
                vpn=(cross_page_va >> 12) + 1,
                s2xlate=0,
                patch=(("s1_pf", 1),),
            ),
        ),
    )
    state = uncache.TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(cross_page_va)
    env.arm_translation_scenario(state)
    uncache._force_redirect_to(env, cross_page_va)

    for _ in range(12000):
        if env.functional_coverage.key_hit("ifu_mmio_owner_v3", "mmio_leaf_030"):
            break
        env.step(1)

    assert env.functional_coverage.key_hit(
        "ifu_mmio_owner_v3", "mmio_leaf_030"
    ), {
        "ptw": env.ptw_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
        "observations": [
            (int(obs.pc), int(obs.instr), bool(obs.is_rvc))
            for obs in env.monitor.observations[-16:]
        ],
    }
    for _ in range(256):
        if env.monitor.exception_mark_count > 0:
            break
        env.step(1)
    assert env.monitor.exception_mark_count > 0
    ptw_stats = env.ptw_agent.get_stats()
    assert int(ptw_stats.get("response_override_hit_count", 0)) >= 1, ptw_stats
    assert (cross_page_pa & ~(uncache._UNCACHE_BEAT_BYTES - 1)) in env.uncache_agent.get_stats().get(
        "request_addrs", []
    )
    assert not env.monitor.get_errors()


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


@pytest.mark.funcov_bins("BIN-1032")
@pytest.mark.skipif(
    not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration"
)
def test_instr_uncache_send_request_stall_uses_legal_nc_witness(env):
    """Exercise the canonical SendReq backpressure contract on a legal NC path."""
    expected, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        instr_count=4096,
    )
    uncache._initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    uncache._configure_exec_attrs_16k(env, base_addr=0x80000000)
    env.backend_model.set_can_accept(0)
    uncache._force_redirect_to(env, mapping.vaddr)

    stalled_cycles = []
    handshake_samples = {}

    def observe_send_stall(cycle, active_env):
        snapshot = owner_funcov._snapshot(
            active_env.functional_coverage, active_env.dut
        )
        sample = (
            snapshot["uncache_state"],
            snapshot["ifu_stall"],
            snapshot["to_uncache_valid"],
        )
        handshake_samples[sample] = handshake_samples.get(sample, 0) + 1
        if (
            snapshot["uncache_state"] == owner_funcov._SEND_REQ
            and snapshot["ifu_stall"] == 1
            and snapshot["to_uncache_valid"] == 0
        ):
            stalled_cycles.append(int(cycle))

    env.register_cycle_observer(observe_send_stall)
    for _ in range(4000):
        env.step(1)
        if env.functional_coverage.key_hit(
            "ifu_mmio_owner_v3", "mmio_leaf_017"
        ):
            break

    assert stalled_cycles, {
        "mapping": mapping,
        "uncache": env.uncache_agent.get_stats(),
        "backend": env.backend_model.get_stats(),
        "handshake_samples": handshake_samples,
    }
    assert env.functional_coverage.key_hit(
        "ifu_mmio_owner_v3", "mmio_leaf_017"
    )

    env.backend_model.set_can_accept(1)
    assert uncache._wait_for_observed_pc(
        env, expected[0][0], max_cycles=12000
    ), {
        "observed": [int(item.pc) for item in env.monitor.observations[-16:]],
        "uncache": env.uncache_agent.get_stats(),
    }
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1056")
@pytest.mark.skipif(
    not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration"
)
def test_nc_execute_denied_preserves_fetch_exception(env):
    payload = int(uncache._CNOP).to_bytes(2, "little") * 64
    start_pc = uncache._NORMAL_BASE

    uncache._initialize_sv39_fetch(env, reset_vector=start_pc)
    scenario = uncache.TranslationScenario(
        scenario_id="bin-1056-nc-execute-denied",
        va=start_pc,
        pa=uncache._NORMAL_PHYS_BASE,
        payload=payload,
        s1_pte=uncache.TranslationPte(pbmt=uncache._PBMT_NC),
        expected_path="fault",
        pmp_entries=(
            uncache.TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=uncache.PmpPmaConfig(
                    match="napot", read=True, write=True, execute=False
                ),
                addr=uncache._NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
        pma_entries=(
            uncache.TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=uncache.PmpPmaConfig(
                    match="napot",
                    read=True,
                    write=True,
                    execute=True,
                    cacheable=True,
                    atomic=True,
                ),
                addr=uncache._NORMAL_PHYS_BASE,
                size=0x1000,
            ),
        ),
    )
    state = uncache.TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(start_pc)
    exception_samples = []

    def capture_exception(cycle, active_env):
        snapshot = owner_funcov._snapshot(
            active_env.functional_coverage, active_env.dut
        )
        if snapshot["to_valid"] == 1 and snapshot["to_exception"] not in {None, 0}:
            exception_samples.append(
                {"cycle": int(cycle), "exception": int(snapshot["to_exception"])}
            )

    env.register_cycle_observer(capture_exception)
    env.arm_translation_scenario(state)
    uncache._force_redirect_to(env, start_pc)

    for _ in range(6000):
        if env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_002"):
            break
        env.step(1)

    assert env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_002"), {
        "ptw": env.ptw_agent.get_stats(),
        "observations": [
            (int(obs.pc), int(obs.instr), bool(obs.is_rvc))
            for obs in env.monitor.observations[-16:]
        ],
    }
    assert exception_samples, {
        "reason": "NC exception did not reach toIBuffer",
        "samples": exception_samples,
    }
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1086")
@pytest.mark.skipif(
    not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration"
)
def test_nc_page_tail_first_page_execute_denied_delivers_iaf(env):
    start_pc = uncache._NORMAL_BASE + uncache._SV39_PAGE_SIZE - 2
    start_paddr = uncache._NORMAL_PHYS_BASE + uncache._SV39_PAGE_SIZE - 2
    first_page = start_paddr & ~(uncache._SV39_PAGE_SIZE - 1)
    second_page = first_page + uncache._SV39_PAGE_SIZE
    payload = int(uncache._ADDI_X0_X0_0).to_bytes(4, "little")

    uncache._initialize_sv39_fetch(env, reset_vector=start_pc)
    scenario = uncache.TranslationScenario(
        scenario_id="bin-1086-nc-page-tail-first-page-iaf",
        va=start_pc,
        pa=start_paddr,
        payload=payload,
        page_count=2,
        s1_pte=uncache.TranslationPte(pbmt=uncache._PBMT_NC),
        expected_path="fault",
        expected_result="access_fault",
        pmp_entries=(
            uncache.TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=uncache.PmpPmaConfig(
                    match="napot", read=True, write=True, execute=False
                ),
                addr=first_page,
                size=uncache._SV39_PAGE_SIZE,
            ),
            uncache.TranslationPmpPmaEntry(
                kind="pmp",
                index=1,
                config=uncache.PmpPmaConfig(
                    match="napot", read=True, write=True, execute=True
                ),
                addr=second_page,
                size=uncache._SV39_PAGE_SIZE,
            ),
        ),
        pma_entries=(
            uncache.TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=uncache.PmpPmaConfig(
                    match="napot",
                    read=True,
                    write=True,
                    execute=True,
                    cacheable=True,
                    atomic=True,
                ),
                addr=first_page,
                size=uncache._SV39_PAGE_SIZE,
            ),
            uncache.TranslationPmpPmaEntry(
                kind="pma",
                index=1,
                config=uncache.PmpPmaConfig(
                    match="napot",
                    read=True,
                    write=True,
                    execute=True,
                    cacheable=True,
                    atomic=True,
                ),
                addr=second_page,
                size=uncache._SV39_PAGE_SIZE,
            ),
        ),
    )
    state = uncache.TranslationScenarioBuilder(env).build(scenario)
    assert state.expected_page_outcomes[0]["permission"]["execute_allowed"] is False
    assert state.expected_page_outcomes[1]["permission"]["execute_allowed"] is True

    samples = []

    def capture_first_page_fault(cycle, active_env):
        snapshot = owner_funcov._snapshot(
            active_env.functional_coverage, active_env.dut
        )
        if snapshot["to_valid"] == 1 and snapshot["to_exception"] == 3:
            samples.append(
                {
                    "cycle": int(cycle),
                    "s2_pc": snapshot["s2_pc"],
                    "s2_instr_pc": snapshot["s2_instr_pc"],
                    "s2_req_uncache": snapshot["s2_req_uncache"],
                    "s2_use_uncache": snapshot["s2_use_uncache"],
                    "s2_exception": snapshot["s2_exception"],
                    "to_pc": snapshot["to_pc"],
                }
            )

    env.register_cycle_observer(capture_first_page_fault)
    env.monitor.clear()
    env.monitor.set_expected_pc(start_pc)
    env.arm_translation_scenario(state)
    uncache._force_redirect_to(env, start_pc)

    for _ in range(6000):
        if env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_032"):
            break
        env.step(1)

    assert env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_032"), {
        "samples": samples,
        "ptw": env.ptw_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    assert samples
    assert samples[-1]["s2_instr_pc"] == start_pc >> 1
    assert samples[-1]["s2_req_uncache"] == 1
    assert samples[-1]["s2_use_uncache"] == 0
    assert samples[-1]["s2_exception"] == 3
    assert not env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_030")
    assert any(
        item.get("event") == "nc_first_page_fault_pc_mismatch"
        for item in env.functional_coverage.risk_observations
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1085")
@pytest.mark.skipif(
    not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration"
)
def test_nc_cross_page_second_page_pf_attributes_to_rvi_start(env):
    cross_page_va = uncache._NORMAL_BASE + uncache._SV39_PAGE_SIZE - 2
    cross_page_pa = uncache._NORMAL_PHYS_BASE + uncache._SV39_PAGE_SIZE - 2
    first_page = cross_page_pa & ~(uncache._SV39_PAGE_SIZE - 1)
    second_page = first_page + uncache._SV39_PAGE_SIZE
    payload = int(uncache._ADDI_X0_X0_0).to_bytes(4, "little")

    uncache._initialize_sv39_fetch(env, reset_vector=cross_page_va)
    scenario = uncache.TranslationScenario(
        scenario_id="bin-1085-nc-cross-page-second-pf",
        va=cross_page_va,
        pa=cross_page_pa,
        payload=payload,
        page_count=2,
        s1_pte=uncache.TranslationPte(pbmt=uncache._PBMT_NC),
        expected_path="fault",
        pmp_entries=(
            uncache.TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=uncache.PmpPmaConfig(
                    match="napot", read=True, write=True, execute=True
                ),
                addr=first_page,
                size=uncache._SV39_PAGE_SIZE,
            ),
            uncache.TranslationPmpPmaEntry(
                kind="pmp",
                index=1,
                config=uncache.PmpPmaConfig(
                    match="napot", read=True, write=True, execute=True
                ),
                addr=second_page,
                size=uncache._SV39_PAGE_SIZE,
            ),
        ),
        pma_entries=(
            uncache.TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=uncache.PmpPmaConfig(
                    match="napot",
                    read=True,
                    write=True,
                    execute=True,
                    cacheable=True,
                    atomic=True,
                ),
                addr=first_page,
                size=uncache._SV39_PAGE_SIZE,
            ),
            uncache.TranslationPmpPmaEntry(
                kind="pma",
                index=1,
                config=uncache.PmpPmaConfig(
                    match="napot",
                    read=True,
                    write=True,
                    execute=True,
                    cacheable=True,
                    atomic=True,
                ),
                addr=second_page,
                size=uncache._SV39_PAGE_SIZE,
            ),
        ),
        ptw_response_overrides=(
            uncache.TranslationPtwResponseOverride(
                vpn=(cross_page_va >> 12) + 1,
                s2xlate=0,
                patch=(("s1_pf", 1),),
            ),
        ),
    )
    state = uncache.TranslationScenarioBuilder(env).build(scenario)
    exception_samples = []

    def capture_cross_page_fault(cycle, active_env):
        snapshot = owner_funcov._snapshot(
            active_env.functional_coverage, active_env.dut
        )
        if (
            snapshot["to_valid"] == 1
            and snapshot["to_exception_cross_page"] == 1
            and snapshot["to_exception"] not in {None, 0}
        ):
            exception_samples.append(
                {
                    "cycle": int(cycle),
                    "exception": snapshot["to_exception"],
                    "s2_instr_pc": snapshot["s2_instr_pc"],
                    "to_pc": snapshot["to_pc"],
                }
            )

    env.register_cycle_observer(capture_cross_page_fault)
    env.monitor.clear()
    env.monitor.set_expected_pc(cross_page_va)
    env.arm_translation_scenario(state)
    uncache._force_redirect_to(env, cross_page_va)

    for _ in range(12000):
        if env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_031"):
            break
        env.step(1)

    assert env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_031"), {
        "samples": exception_samples,
        "ptw": env.ptw_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }
    assert exception_samples
    assert exception_samples[-1]["exception"] == 1
    assert exception_samples[-1]["to_pc"] == cross_page_va >> 1
    ptw_stats = env.ptw_agent.get_stats()
    assert int(ptw_stats.get("response_override_hit_count", 0)) >= 1, ptw_stats
    assert (
        cross_page_pa & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    ) in env.uncache_agent.get_stats().get("request_addrs", [])
    for _ in range(256):
        if env.monitor.exception_mark_count > 0:
            break
        env.step(1)
    assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1052")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_backend_redirect_wins_response_writeback(env):
    uncache._prepare_mmio_cnop_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=32)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_req(env) > 0
    assert env.uncache_agent.pending
    ready_cycle = int(env.uncache_agent.pending[0].ready_cycle)
    trace = []

    def capture_overlap(cycle, active_env):
        snapshot = owner_funcov._snapshot(
            active_env.functional_coverage, active_env.dut
        )
        if any(
            snapshot[name] == 1
            for name in (
                "tl_d_valid",
                "resp_valid",
                "backend_redirect",
                "uncache_redirect",
                "ifu_flush",
            )
        ):
            trace.append(
                {
                    "cycle": int(cycle),
                    "tl_d_valid": snapshot["tl_d_valid"],
                    "resp_valid": snapshot["resp_valid"],
                    "backend_redirect": snapshot["backend_redirect"],
                    "uncache_redirect": snapshot["uncache_redirect"],
                    "ifu_flush": snapshot["ifu_flush"],
                    "uncache_state": snapshot["uncache_state"],
                }
            )

    env.register_cycle_observer(capture_overlap)
    while int(env.current_cycle) < ready_cycle:
        env.step(1)
    target_pc = uncache._MMIO_BASE + 0x40
    uncache._force_redirect_to(env, target_pc)

    for _ in range(256):
        env.step(1)
        if env.functional_coverage.key_hit("ifu_mmio_owner_v3", "mmio_leaf_037"):
            break

    assert env.functional_coverage.key_hit(
        "ifu_mmio_owner_v3", "mmio_leaf_037"
    ), "\n".join(str(item) for item in trace)
    assert uncache._wait_for_observed_pc(env, target_pc, max_cycles=6000)
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1047", "BIN-1048")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_cross_8b_clean_resend_delivers_two_ordered_rvi(env):
    cross_8b_trace = _register_cross_8b_trace(env)
    payload = _two_rvi_cross_beat_payload()
    env.memory.mmio_ranges.append(
        (uncache._MMIO_BASE, uncache._MMIO_BASE + len(payload))
    )
    uncache.LoadProgramSequence(
        image=uncache.ProgramImage(payload=payload, base_addr=uncache._MMIO_BASE),
        step_cycles=0,
    ).run(env)
    uncache._initialize_mmio_fetch(env, reset_vector=uncache._CROSS_BEAT_PC)

    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE)
    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE + 8)
    assert uncache._wait_for_observed_pc(env, uncache._CROSS_BEAT_PC, max_cycles=8000)
    assert uncache._wait_for_observed_pc(
        env, uncache._CROSS_BEAT_PC + 4, max_cycles=8000
    )

    observed = {
        int(item.pc): item
        for item in env.monitor.observations
        if int(item.pc) in {uncache._CROSS_BEAT_PC, uncache._CROSS_BEAT_PC + 4}
    }
    assert int(observed[uncache._CROSS_BEAT_PC].instr) == uncache._ADDI_X0_X0_0
    assert int(observed[uncache._CROSS_BEAT_PC + 4].instr) == uncache._ADDI_X0_X0_0
    assert not bool(observed[uncache._CROSS_BEAT_PC].is_rvc)
    assert not bool(observed[uncache._CROSS_BEAT_PC + 4].is_rvc)
    assert env.functional_coverage.key_hit(
        "ifu_mmio_owner_v3", "mmio_leaf_033"
    ), "\n".join(str(item) for item in cross_8b_trace[-80:])
    assert env.functional_coverage.key_hit(
        "ifu_mmio_owner_v3", "mmio_leaf_032"
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1071", "BIN-1077", "BIN-1079")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_cross_8b_clean_resend_delivers_two_ordered_rvi(env, tmp_path):
    cross_8b_trace = _register_cross_8b_trace(env)
    payload = _two_rvi_cross_beat_payload()
    bin_path = tmp_path / "pbmt_nc_cross_8b_two_rvi.bin"
    bin_path.write_bytes(payload)
    _expected, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        bin_path=bin_path,
    )
    start_pc = mapping.vaddr + 6
    uncache._initialize_sv39_fetch(env, reset_vector=start_pc)
    uncache._configure_exec_attrs_for_mapping(env, mapping)
    uncache._force_redirect_to(env, start_pc)

    assert uncache._wait_for_request_addr(env, mapping.paddr, max_cycles=6000)
    assert uncache._wait_for_request_addr(env, mapping.paddr + 8, max_cycles=6000)
    assert uncache._wait_for_observed_pc(env, start_pc, max_cycles=12000)
    assert uncache._wait_for_observed_pc(env, start_pc + 4, max_cycles=12000)

    observed = {
        int(item.pc): item
        for item in env.monitor.observations
        if int(item.pc) in {start_pc, start_pc + 4}
    }
    assert int(observed[start_pc].instr) == uncache._ADDI_X0_X0_0
    assert int(observed[start_pc + 4].instr) == uncache._ADDI_X0_X0_0
    assert not bool(observed[start_pc].is_rvc)
    assert not bool(observed[start_pc + 4].is_rvc)
    assert env.functional_coverage.key_hit(
        "ifu_nc_owner_v3", "nc_leaf_023"
    ), "\n".join(str(item) for item in cross_8b_trace[-80:])
    assert env.functional_coverage.key_hit(
        "ifu_nc_owner_v3", "nc_leaf_025"
    ), "\n".join(str(item) for item in cross_8b_trace[-80:])
    assert env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_017")
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1080", "BIN-1081", "BIN-1083")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_cross_page_clean_rvi_resumes_and_delivers_once(env, tmp_path):
    payload = _cross_page_payload(rvi_tail=True)
    bin_path = tmp_path / "pbmt_nc_cross_page_rvi.bin"
    bin_path.write_bytes(payload)
    _expected, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr_pages=(uncache._NORMAL_PHYS_BASE, uncache._NORMAL_ALT_PHYS_BASE),
        bin_path=bin_path,
    )
    start_pc = mapping.vaddr + uncache._SV39_PAGE_SIZE - 2
    first_beat = mapping.paddr_pages[0] + uncache._SV39_PAGE_SIZE - 8
    second_page = mapping.paddr_pages[1]
    uncache._initialize_sv39_fetch(env, reset_vector=start_pc)
    uncache._configure_exec_attrs_16k(env, base_addr=0x80000000)
    uncache._force_redirect_to(env, start_pc)

    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=6000)
    assert uncache._wait_for_request_addr(env, second_page, max_cycles=6000)
    assert uncache._wait_for_observed_pc(env, start_pc, max_cycles=12000)

    deliveries = [item for item in env.monitor.observations if int(item.pc) == start_pc]
    assert len(deliveries) == 1
    assert int(deliveries[0].instr) == uncache._ADDI_X0_X0_0
    assert not bool(deliveries[0].is_rvc)
    for leaf in (26, 27, 29):
        assert env.functional_coverage.key_hit(
            "ifu_nc_owner_v3", f"nc_leaf_{leaf:03d}"
        )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1082")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_page_tail_rvc_delivers_without_half_instruction_resend(env, tmp_path):
    payload = _cross_page_payload(rvi_tail=False)[: uncache._SV39_PAGE_SIZE]
    bin_path = tmp_path / "pbmt_nc_page_tail_rvc.bin"
    bin_path.write_bytes(payload)
    _expected, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        bin_path=bin_path,
    )
    start_pc = mapping.vaddr + uncache._SV39_PAGE_SIZE - 2
    first_beat = mapping.paddr + uncache._SV39_PAGE_SIZE - 8
    uncache._initialize_sv39_fetch(env, reset_vector=start_pc)
    uncache._configure_exec_attrs_for_mapping(env, mapping)
    uncache._force_redirect_to(env, start_pc)

    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=6000)
    assert uncache._wait_for_observed_pc(env, start_pc, max_cycles=12000)

    delivered = next(item for item in env.monitor.observations if int(item.pc) == start_pc)
    assert bool(delivered.is_rvc)
    assert env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_028")
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1049")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_branch_reuses_common_predecode(env):
    payload = _branch_payload()
    env.memory.mmio_ranges.append(
        (uncache._MMIO_BASE, uncache._MMIO_BASE + len(payload))
    )
    uncache.LoadProgramSequence(
        image=uncache.ProgramImage(payload=payload, base_addr=uncache._MMIO_BASE),
        step_cycles=0,
    ).run(env)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_observed_pc(env, uncache._MMIO_BASE, max_cycles=8000)
    assert env.functional_coverage.key_hit(
        "ifu_mmio_owner_v3", "mmio_leaf_034"
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1075")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_branch_reuses_common_predecode(env, tmp_path):
    bin_path = tmp_path / "pbmt_nc_branch.bin"
    bin_path.write_bytes(_branch_payload())
    _expected, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        bin_path=bin_path,
    )
    uncache._initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    uncache._configure_exec_attrs_for_mapping(env, mapping)
    uncache._force_redirect_to(env, mapping.vaddr)

    assert uncache._wait_for_observed_pc(env, mapping.vaddr, max_cycles=12000)
    assert env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_021")
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1023", "BIN-1089", "BIN-1092")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_page_tail_naturally_advances_to_pbmt_io(env):
    nc_vaddr = uncache._NORMAL_BASE
    io_vaddr = nc_vaddr + uncache._SV39_PAGE_SIZE
    nc_paddr = uncache._NORMAL_PHYS_BASE
    io_paddr = uncache._NORMAL_ALT_PHYS_BASE
    payload = int(uncache._CNOP).to_bytes(2, "little") * (
        uncache._SV39_PAGE_SIZE // 2
    )
    env.page_table.clear()
    env.page_table.map_page(
        nc_vaddr >> 12,
        nc_paddr >> 12,
        v=1,
        r=1,
        x=1,
        pbmt=uncache._PBMT_NC,
    )
    env.page_table.map_page(
        io_vaddr >> 12,
        io_paddr >> 12,
        v=1,
        r=1,
        x=1,
        pbmt=uncache._PBMT_IO,
    )
    env.ptw_agent.configure(
        mode="sv39", response_source="model", compare_drive_source="model"
    )
    for paddr in (nc_paddr, io_paddr):
        uncache.LoadProgramSequence(
            image=uncache.ProgramImage(payload=payload, base_addr=paddr),
            step_cycles=0,
        ).run(env)

    start_pc = nc_vaddr + uncache._SV39_PAGE_SIZE - 2
    uncache._initialize_sv39_fetch(env, reset_vector=start_pc)
    uncache._configure_exec_attrs_16k(env, base_addr=0x80000000)
    uncache._force_redirect_to(env, start_pc)

    assert uncache._wait_for_request_addr(
        env, nc_paddr + uncache._SV39_PAGE_SIZE - 8, max_cycles=12000
    )
    assert uncache._wait_for_observed_pc(env, start_pc, max_cycles=12000)
    assert uncache._wait_for_request_addr(env, io_paddr, max_cycles=12000)
    assert uncache._wait_for_observed_pc(env, io_vaddr, max_cycles=12000)

    assert env.functional_coverage.key_hit(
        "ifu_mmio_owner_v3", "mmio_leaf_008"
    )
    for leaf in (35, 38):
        assert env.functional_coverage.key_hit(
            "ifu_nc_owner_v3", f"nc_leaf_{leaf:03d}"
        )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1063")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_tl_a_stall_holds_and_releases_same_request(env, tmp_path):
    bin_path = tmp_path / "pbmt_nc_tl_a_stall.bin"
    bin_path.write_bytes(int(uncache._CNOP).to_bytes(2, "little") * 256)
    _expected, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        bin_path=bin_path,
    )
    uncache._initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    uncache._configure_exec_attrs_for_mapping(env, mapping)
    env.uncache_agent.set_a_ready(0)
    uncache._force_redirect_to(env, mapping.vaddr)

    for _ in range(6000):
        env.step(1)
        if int(env.uncache_if.a_valid.value) == 1:
            break
    assert int(env.uncache_if.a_valid.value) == 1
    assert int(env.uncache_if.a_ready.value) == 0
    stalled_addr = int(env.uncache_if.a_bits_address.value)
    req_count = int(env.uncache_agent.get_stats().get("req_count", 0))
    env.step(8)
    assert int(env.uncache_if.a_valid.value) == 1
    assert int(env.uncache_if.a_bits_address.value) == stalled_addr
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == req_count

    env.uncache_agent.set_a_ready(None)
    assert uncache._wait_for_request_addr(env, mapping.paddr, max_cycles=6000)
    assert env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_009")
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1062")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_nc_send_request_is_suppressed_when_ibuffer_not_ready(env):
    expected, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        instr_count=4096,
    )
    uncache._initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    uncache._configure_exec_attrs_16k(env, base_addr=0x80000000)
    env.backend_model.set_can_accept(0)
    uncache._force_redirect_to(env, mapping.vaddr)

    stalled_cycles = []

    def observe_stall(cycle, active_env):
        snapshot = owner_funcov._snapshot(
            active_env.functional_coverage, active_env.dut
        )
        if snapshot["uncache_state"] == 2 and snapshot["ifu_stall"] == 1:
            stalled_cycles.append(int(cycle))

    env.register_cycle_observer(observe_stall)
    for _ in range(20000):
        env.step(1)
        if env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_008"):
            break

    assert stalled_cycles, {
        "mapping": mapping,
        "uncache": env.uncache_agent.get_stats(),
        "backend": env.backend_model.get_stats(),
    }
    assert env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_008")

    env.backend_model.set_can_accept(1)
    assert uncache._wait_for_observed_pc(
        env, expected[0][0], max_cycles=12000
    ), {
        "observed": [int(item.pc) for item in env.monitor.observations[-16:]],
        "uncache": env.uncache_agent.get_stats(),
    }
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1090")
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_cacheable_delivery_then_pbmt_nc_starts_with_clean_first_instruction(env):
    nc_expected, cacheable_pcs = uncache._prepare_sv39_dual_nc_cacheable_stream(env)
    env.icache_agent.configure(
        hit_latency=4, miss_latency=4, miss_rate=0.0, seed=1090
    )
    uncache._initialize_sv39_fetch(env, reset_vector=uncache._NORMAL_ALT_BASE)
    uncache._configure_exec_attrs_16k(env, base_addr=0x80000000)
    uncache._force_redirect_to(env, uncache._NORMAL_ALT_BASE)

    assert uncache._wait_for_observed_pc(
        env, cacheable_pcs[0], max_cycles=12000
    ), env.icache_agent.get_stats()
    switch_index = len(env.monitor.observations)
    uncache._pulse_sfence(
        env, addr=uncache._NORMAL_ALT_BASE, rs1=1, rs2=0
    )
    uncache._force_redirect_to(env, uncache._NORMAL_BASE)

    assert uncache._wait_for_request_addr(
        env, uncache._NORMAL_PHYS_BASE, max_cycles=6000
    ), env.uncache_agent.get_stats()
    assert uncache._wait_for_observed_pc(
        env, uncache._NORMAL_BASE, max_cycles=12000
    ), env.uncache_agent.get_stats()

    first_nc = next(
        observation
        for observation in list(env.monitor.observations)[switch_index:]
        if int(observation.pc) == uncache._NORMAL_BASE
    )
    expected_pc, expected_instr, expected_is_rvc = nc_expected[0]
    assert int(first_nc.pc) == int(expected_pc)
    assert int(first_nc.instr) == int(expected_instr)
    assert bool(first_nc.is_rvc) == bool(expected_is_rvc)
    assert env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_036")
    assert not env.monitor.get_errors()
