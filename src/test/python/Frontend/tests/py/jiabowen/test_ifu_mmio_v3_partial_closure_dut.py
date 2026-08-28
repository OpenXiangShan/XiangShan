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


def _jal_x0(offset: int) -> int:
    assert int(offset) % 2 == 0
    assert -(1 << 20) <= int(offset) < (1 << 20)
    imm = int(offset) & 0x1FFFFF
    return (
        (((imm >> 20) & 0x1) << 31)
        | (((imm >> 1) & 0x3FF) << 21)
        | (((imm >> 11) & 0x1) << 20)
        | (((imm >> 12) & 0xFF) << 12)
        | 0x6F
    )


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


@pytest.mark.funcov_bins("BIN-1084", "BIN-1086")
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
                    "s2_ftq_flag": snapshot["s2_ftq_flag"],
                    "s2_ftq_value": snapshot["s2_ftq_value"],
                    "to_ftq_flag": snapshot["to_ftq_flag"],
                    "to_ftq_value": snapshot["to_ftq_value"],
                    "to_ftq_offset": snapshot["to_ftq_offset"],
                    "req_valid": snapshot["req_valid"],
                    "to_uncache_valid": snapshot["to_uncache_valid"],
                    "tl_a_valid": snapshot["tl_a_valid"],
                }
            )

    env.register_cycle_observer(capture_first_page_fault)
    env.monitor.clear()
    env.monitor.set_expected_pc(start_pc)
    env.arm_translation_scenario(state)
    uncache._force_redirect_to(env, start_pc)

    for _ in range(6000):
        if all(
            env.functional_coverage.key_hit("ifu_nc_owner_v3", leaf)
            for leaf in ("nc_leaf_030", "nc_leaf_032")
        ):
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
    assert samples[-1]["req_valid"] != 1
    assert samples[-1]["to_uncache_valid"] != 1
    assert samples[-1]["tl_a_valid"] != 1
    assert samples[-1]["to_ftq_flag"] == samples[-1]["s2_ftq_flag"]
    assert samples[-1]["to_ftq_value"] == samples[-1]["s2_ftq_value"]
    assert samples[-1]["to_ftq_offset"] is not None
    assert env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_030")
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


@pytest.mark.funcov_bins("BIN-1067")
@pytest.mark.skipif(
    not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration"
)
def test_cacheable_checker_redirect_flushes_younger_nc_internal_request(env):
    virtual_page = uncache._NORMAL_BASE
    physical_page = 0x80004000
    cacheable_start = virtual_page + uncache._SV39_PAGE_SIZE - uncache._FETCH_BLOCK_SIZE
    nc_start = virtual_page + uncache._SV39_PAGE_SIZE
    nc_physical_page = physical_page + uncache._SV39_PAGE_SIZE
    branch_pc = nc_start - 4
    recovery_nc = nc_start + 0x100
    recovery_nc_pa = nc_physical_page + 0x100
    warm_redirect_pc = recovery_nc + uncache._FETCH_BLOCK_SIZE

    cacheable_payload = bytearray(
        int(uncache._CNOP).to_bytes(2, "little")
        * (uncache._SV39_PAGE_SIZE // 2)
    )
    cacheable_payload[-4:] = int(_jal_x0(recovery_nc - branch_pc)).to_bytes(
        4, "little"
    )
    nc_payload = bytearray(
        int(uncache._CNOP).to_bytes(2, "little")
        * (uncache._SV39_PAGE_SIZE // 2)
    )
    nc_payload[warm_redirect_pc - nc_start : warm_redirect_pc - nc_start + 4] = int(
        uncache._JAL_X0_PLUS_4
    ).to_bytes(4, "little")

    env.page_table.clear()
    env.page_table.map_page(
        virtual_page >> 12,
        physical_page >> 12,
        v=1,
        r=1,
        x=1,
        pbmt=uncache._PBMT_PMA,
    )
    env.page_table.map_page(
        nc_start >> 12,
        nc_physical_page >> 12,
        v=1,
        r=1,
        x=1,
        pbmt=uncache._PBMT_NC,
    )
    env.ptw_agent.configure(
        latency=0,
        mode="sv39",
        response_source="model",
        compare_drive_source="model",
    )
    uncache.LoadProgramSequence(
        image=uncache.ProgramImage(
            payload=bytes(cacheable_payload), base_addr=physical_page
        ),
        step_cycles=0,
    ).run(env)
    uncache.LoadProgramSequence(
        image=uncache.ProgramImage(
            payload=bytes(nc_payload), base_addr=nc_physical_page
        ),
        step_cycles=0,
    ).run(env)
    env.uncache_agent.configure(latency=8, mmio_latency=8)
    uncache._initialize_sv39_fetch(env, reset_vector=cacheable_start)
    uncache._configure_exec_attrs_16k(env, base_addr=physical_page)

    env.monitor.clear()
    env.monitor.set_expected_pc(cacheable_start)

    # Warm A only through s1, then flush it before PredChecker can train the JAL.
    # The NC page is warmed through its own legal translation.  Its live JAL
    # identity supplies the sole source-bound redirect that starts measurement.
    env.backend_model.commit_min_delay = 4096
    env.backend_model.commit_max_delay = 4096
    icache_line_baseline = int(
        env.icache_agent.get_stats().get("resp_line_count", 0)
    )
    uncache._force_redirect_to(env, cacheable_start)
    setup_a_reached_s1 = False
    for _ in range(4000):
        env.step(1)
        setup_snapshot = owner_funcov._snapshot(
            env.functional_coverage, env.dut
        )
        setup_a_reached_s1 |= bool(
            setup_snapshot["s1_valid"] == 1
            and setup_snapshot["s1_pc"] == (cacheable_start >> 1)
        )
        if int(env.icache_agent.get_stats().get("resp_line_count", 0)) > int(
            icache_line_baseline
        ):
            break
    assert int(env.icache_agent.get_stats().get("resp_line_count", 0)) > int(
        icache_line_baseline
    ), env.icache_agent.get_stats()
    assert not setup_a_reached_s1
    uncache._force_redirect_to(env, recovery_nc)

    redirect_source = None
    setup_a_checker_redirect = False
    for _ in range(8000):
        env.step(1)
        setup_snapshot = owner_funcov._snapshot(
            env.functional_coverage, env.dut
        )
        setup_a_checker_redirect |= bool(
            setup_snapshot["checker_redirect"] == 1
            and setup_snapshot["wb_pc"] == (cacheable_start >> 1)
        )
        redirect_source = next(
            (
                entry
                for entry in env.backend_model._cfvec_queue
                if int(entry.pc) == warm_redirect_pc and bool(entry.is_cfi)
            ),
            None,
        )
        if redirect_source is not None:
            break
    assert not setup_a_checker_redirect
    assert redirect_source is not None, {
        "cfvec_queue": [
            {
                "pc": int(entry.pc),
                "ftq": (int(entry.ftq_flag), int(entry.ftq_value)),
                "offset": int(entry.ftq_offset),
                "is_cfi": bool(entry.is_cfi),
            }
            for entry in env.backend_model._cfvec_queue
        ],
        "ptw": env.ptw_agent.get_stats(),
        "icache": env.icache_agent.get_stats(),
        "uncache": env.uncache_agent.get_stats(),
    }

    owner_funcov.reset_mmio_nc_owner_coverage_state(env.functional_coverage)
    request_baseline = len(env.uncache_agent.get_stats().get("request_addrs", []))
    env.monitor.clear()
    trace = []

    def capture_checker_nc_overlap(cycle, active_env):
        snapshot = owner_funcov._snapshot(
            active_env.functional_coverage, active_env.dut
        )
        trace.append(
            {
                "cycle": int(cycle),
                "s1_valid": snapshot["s1_valid"],
                "s1_flush": snapshot["s1_flush"],
                "s1_pc": snapshot["s1_pc"],
                "s1_req_uncache": snapshot["s1_req_uncache"],
                "s1_pbmt": snapshot["s1_pbmt"],
                "s1_pmp_mmio": snapshot["s1_pmp_mmio"],
                "s1_ftq": (snapshot["s1_ftq_flag"], snapshot["s1_ftq_value"]),
                "s2_pc": snapshot["s2_pc"],
                "s2_paddr": snapshot["s2_paddr"],
                "s2_valid": snapshot["s2_valid"],
                "s2_req_uncache": snapshot["s2_req_uncache"],
                "s2_use_uncache": snapshot["s2_use_uncache"],
                "s2_pbmt": snapshot["s2_pbmt"],
                "s2_pmp_mmio": snapshot["s2_pmp_mmio"],
                "s2_ftq": (snapshot["s2_ftq_flag"], snapshot["s2_ftq_value"]),
                "s2_wb_not_flush": snapshot["s2_wb_not_flush"],
                "req_valid": snapshot["req_valid"],
                "req_ready": snapshot["req_ready"],
                "uncache_state": snapshot["uncache_state"],
                "checker_redirect": snapshot["checker_redirect"],
                "wb_path_valid": snapshot["wb_path_valid"],
                "wb_redirect": snapshot["wb_redirect"],
                "ifu_flush": snapshot["ifu_flush"],
                "wb_pc": snapshot["wb_pc"],
                "wb_ftq": (snapshot["wb_ftq_flag"], snapshot["wb_ftq_value"]),
                "to_uncache_valid": snapshot["to_uncache_valid"],
                "to_uncache_ready": snapshot["to_uncache_ready"],
                "to_uncache_addr": snapshot["to_uncache_addr"],
                "tl_a_valid": snapshot["tl_a_valid"],
                "tl_a_ready": snapshot["tl_a_ready"],
                "tl_a_addr": snapshot["tl_a_addr"],
                "instr_resp_valid": snapshot["instr_resp_valid"],
                "resp_valid": snapshot["resp_valid"],
                "to_valid": snapshot["to_valid"],
                "to_ready": snapshot["to_ready"],
                "to_pc": snapshot["to_pc"],
                "to_ftq": (snapshot["to_ftq_flag"], snapshot["to_ftq_value"]),
                "req_count": int(
                    active_env.uncache_agent.get_stats().get("req_count", 0)
                ),
            }
        )

    env.register_cycle_observer(capture_checker_nc_overlap)
    env.backend_model.inject_redirect_from_cfvec(
        source_pc=int(redirect_source.pc),
        source_ftq_flag=int(redirect_source.ftq_flag),
        source_ftq_value=int(redirect_source.ftq_value),
        source_ftq_offset=int(redirect_source.ftq_offset),
        target_pc=cacheable_start,
        reason="bin1067_warm_return",
        taken=1,
        level=0,
        delay_cycles=3,
    )

    for _ in range(4000):
        if env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_013"):
            break
        env.step(1)

    if not env.functional_coverage.key_hit("ifu_nc_owner_v3", "nc_leaf_013"):
        pending = getattr(
            env.functional_coverage, "_ifu_mmio_nc_owner_state", {}
        ).get("nc_checker_redirect_pending")
        raise AssertionError(
            {
                "checker_trace": [
                    sample
                    for sample in trace
                    if sample["checker_redirect"] == 1
                ],
                "recovery_trace": [
                    sample
                    for sample in trace
                    if sample["req_valid"] == 1
                    and sample["s2_req_uncache"] == 1
                    and int(sample["cycle"]) > 0
                ][-8:],
                "pending_summary": None
                if pending is None
                else {
                    "redirect_cycle": pending["redirect_cycle"],
                    "old_ftq": pending["old_ftq"],
                    "old_pc": pending["old_pc"],
                    "old_paddr": pending["old_paddr"],
                    "younger_nc_present_in_s1": pending[
                        "younger_nc_present_in_s1"
                    ],
                    "younger_nc_present_in_s2": pending[
                        "younger_nc_present_in_s2"
                    ],
                    "younger_nc_internal_req_races_flush": pending[
                        "younger_nc_internal_req_races_flush"
                    ],
                    "old_nc_no_instruncache_request": pending[
                        "old_nc_no_instruncache_request"
                    ],
                    "old_nc_no_tl_a_fire": pending["old_nc_no_tl_a_fire"],
                    "old_nc_no_ibuffer_delivery": pending[
                        "old_nc_no_ibuffer_delivery"
                    ],
                    "old_nc_no_response": pending["old_nc_no_response"],
                    "failure_reasons": pending["failure_reasons"],
                    "recovery": pending["recovery"],
                },
            }
        )
    overlap_samples = [
        sample
        for sample in trace
        if sample["checker_redirect"] == 1
        and sample["wb_path_valid"] == 1
        and sample["wb_redirect"] == 1
        and sample["ifu_flush"] == 1
        and sample["s2_valid"] == 1
        and sample["s2_req_uncache"] == 1
        and sample["s2_pbmt"] == owner_funcov._PBMT_NC
        and sample["s2_pmp_mmio"] == 0
        and sample["s2_wb_not_flush"] != 1
        and sample["s2_ftq"] != sample["wb_ftq"]
        and sample["req_valid"] == 1
        and sample["req_ready"] == 1
    ]
    assert overlap_samples, trace[-192:]
    overlap = overlap_samples[-1]
    redirect_cycle = int(overlap["cycle"])
    assert overlap["s2_pc"] == (nc_start >> 1)
    assert overlap["s2_paddr"] == (nc_physical_page >> 1)
    assert overlap["to_uncache_valid"] != 1
    assert overlap["tl_a_valid"] != 1

    recovery_requests = [
        sample
        for sample in trace
        if int(sample["cycle"]) > redirect_cycle
        and sample["s2_req_uncache"] == 1
        and sample["req_valid"] == 1
        and sample["req_ready"] == 1
        and sample["s2_pc"] == (recovery_nc >> 1)
        and sample["s2_paddr"] == (recovery_nc_pa >> 1)
    ]
    assert recovery_requests, trace[-96:]
    recovery = recovery_requests[0]
    assert (
        recovery["s2_ftq"],
        recovery["s2_pc"],
        recovery["s2_paddr"],
    ) != (
        overlap["s2_ftq"],
        overlap["s2_pc"],
        overlap["s2_paddr"],
    )
    measured_addrs = env.uncache_agent.get_stats().get("request_addrs", [])[request_baseline:]
    assert nc_physical_page not in measured_addrs, measured_addrs
    assert recovery_nc_pa in measured_addrs, measured_addrs
    old_tl_requests = [
        sample
        for sample in trace
        if int(sample["cycle"]) >= redirect_cycle
        and sample["tl_a_valid"] == 1
        and sample["tl_a_ready"] == 1
        and sample["tl_a_addr"] == nc_physical_page
    ]
    assert not old_tl_requests, old_tl_requests
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


@pytest.mark.funcov_bins(
    "BIN-1047",
    "BIN-1048",
    "BIN-1105",
    "BIN-1110",
    "BIN-1111",
    "BIN-1114",
)
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
    for leaf in (12, 17, 18, 21):
        assert env.functional_coverage.key_hit(
            "ifu_instruncache_owner_v3", f"instruncache_leaf_{leaf:03d}"
        )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins(
    "BIN-1071",
    "BIN-1077",
    "BIN-1079",
    "BIN-1105",
    "BIN-1110",
    "BIN-1111",
    "BIN-1114",
)
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
    for leaf in (12, 17, 18, 21):
        assert env.functional_coverage.key_hit(
            "ifu_instruncache_owner_v3", f"instruncache_leaf_{leaf:03d}"
        )
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
