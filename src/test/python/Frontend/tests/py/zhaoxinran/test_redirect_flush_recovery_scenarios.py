"""DUT scenarios for the redirect/flush/recovery functional group.

The tests in this file are the RFR closure entry points.  Existing focused DUT
tests provide the stimulus and low-level checkpoints; this file keeps the
coverage plan's eight scenarios discoverable in one place and adds the
source-bound FTQ-ahead/boundary stream that was previously missing.
"""

from __future__ import annotations

import os
from pathlib import Path

import pytest

from env.core.transactions import BackendRedirectClass, RedirectTxn, ProgramImage
from env.sequences import InjectRedirectSequence, LoadProgramSequence
from tests.py.jiabowen import test_ifu_predchecker_v3_dut as predchecker
from tests.py.ruierhan import test_icache_lowrisk_gap_closure_dut as lowrisk
from tests.py.ruierhan import test_icache_mainpipe_s1_flush_closure_dut as s1_flush
from tests.py.zhaoxinran import test_instruction_fetch_permission_boundary as faults
from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache
from tests.py.zhaoxinran import test_mmio_fetch_boundary as mmio
from tests.py.zhaoxinran import test_address_translation_context_switch as context_switch


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BASE = 0x8000_0000


def _wait_for_pc(env, pc: int, *, max_cycles: int = 8000) -> None:
    for _ in range(int(max_cycles)):
        if any(int(observation.pc) == int(pc) for observation in env.monitor.observations):
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": "redirect target was not delivered",
            "target_pc": hex(int(pc)),
            "observed_tail": [hex(int(item.pc)) for item in env.monitor.observations[-16:]],
            "monitor_errors": env.monitor.get_errors(),
        }
    )


_MAIN_FETCH_VALID_SIGNALS = (
    "Frontend_top.Frontend._inner_ftq_io_toICache_toMainPipe_valid",
    "Frontend_top.Frontend.inner_ftq.io_toICache_toMainPipe_valid",
)
_MAIN_FETCH_READY_SIGNALS = (
    "Frontend_top.Frontend._inner_icache_io_fromFtq_toMainPipe_ready",
    "Frontend_top.Frontend.inner_icache.io_fromFtq_toMainPipe_ready",
)
_UNCACHE_RESPONSE_VALID_SIGNALS = (
    "Frontend_top.Frontend._inner_instrUncache_io_toIfu_resp_valid",
    "Frontend_top.Frontend.inner_instrUncache.io_toIfu_resp_valid",
)
_UNCACHE_NEED_RESEND_SIGNALS = (
    "Frontend_top.Frontend._inner_instrUncache_io_toIfu_resp_bits_needResend",
    "Frontend_top.Frontend.inner_instrUncache.io_toIfu_resp_bits_needResend",
)
_UNCACHE_REQ_ADDR_SIGNALS = (
    "Frontend_top.Frontend.inner_instrUncache.entries_0.reqReg_addr_addr",
)
_IFU_WB_REDIRECT_SIGNALS = (
    "Frontend_top.Frontend._inner_ifu_io_toFtq_wbRedirect_valid",
    "Frontend_top.Frontend.inner_ifu.io_toFtq_wbRedirect_valid",
)
_IBUFFER_READY_SIGNALS = (
    "Frontend_top.Frontend._inner_ibuffer_io_in_ready",
)


def _boundary_loop_payload(*, is_rvc: bool, ftq_offset: int) -> bytes:
    start_halfword = int(ftq_offset) if is_rvc else int(ftq_offset) - 1
    assert 0 <= start_halfword < 32
    assert is_rvc or start_halfword < 31
    halfwords = [predchecker._CNOP] * (predchecker._BLOCK_COUNT * 32)
    for block in range(predchecker._BLOCK_COUNT):
        branch_pc = block * predchecker._BLOCK_BYTES + start_halfword * 2
        target = ((block + 2) % predchecker._BLOCK_COUNT) * predchecker._BLOCK_BYTES
        index = block * 32 + start_halfword
        if is_rvc:
            halfwords[index] = predchecker._c_j(target - branch_pc)
        else:
            encoded = predchecker._jal_x0(target - branch_pc)
            halfwords[index] = encoded & 0xFFFF
            halfwords[index + 1] = encoded >> 16
    return b"".join(value.to_bytes(2, "little") for value in halfwords)


def _cross_block_rvi_head_payload() -> bytes:
    payload = bytearray(
        predchecker._CNOP.to_bytes(2, "little")
        * (predchecker._BLOCK_COUNT * 32)
    )
    instruction = predchecker._jal_x0(0)
    low_offset = predchecker._BLOCK_BYTES - 2
    payload[low_offset : low_offset + 2] = (instruction & 0xFFFF).to_bytes(2, "little")
    payload[predchecker._BLOCK_BYTES : predchecker._BLOCK_BYTES + 2] = (
        (instruction >> 16).to_bytes(2, "little")
    )
    return bytes(payload)


def _load_boundary_stream(env, *, is_rvc: bool, ftq_offset: int) -> None:
    """Create a real CFI whose cfVec identity has the requested FTQ offset."""
    if not is_rvc and int(ftq_offset) == 0:
        payload = _cross_block_rvi_head_payload()
    else:
        payload = _boundary_loop_payload(is_rvc=is_rvc, ftq_offset=ftq_offset)
    LoadProgramSequence(
        image=ProgramImage(payload=payload, base_addr=_BASE),
        step_cycles=0,
    ).run(env)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)


def _find_live_cfi(env, *, is_rvc: bool, ftq_offset: int):
    for _ in range(6000):
        entries = [
            entry
            for entry in env.backend_model._cfvec_queue
            if bool(getattr(entry, "is_cfi", False))
            and bool(entry.is_rvc) is bool(is_rvc)
            and int(entry.ftq_offset) == int(ftq_offset)
        ]
        if entries:
            return entries[0]
        env.step(1)
    raise AssertionError(
        {
            "reason": "requested live CFI FTQ boundary did not occur",
            "expected_is_rvc": bool(is_rvc),
            "expected_ftq_offset": int(ftq_offset),
            "queue_tail": [
                {
                    "pc": hex(int(entry.pc)),
                    "offset": int(entry.ftq_offset),
                    "is_rvc": bool(entry.is_rvc),
                }
                for entry in list(env.backend_model._cfvec_queue)[-16:]
            ],
        }
    )


def _register_main_fetch_observer(env) -> list[dict[str, int]]:
    samples: list[dict[str, int]] = []

    def capture(cycle, current_env) -> None:
        samples.append(
            {
                "cycle": int(cycle),
                "redirect": int(current_env.backend_ctrl_if.redirect_valid.value),
                "valid": uncache._require_first_dut_signal(
                    current_env, _MAIN_FETCH_VALID_SIGNALS
                ),
                "ready": uncache._require_first_dut_signal(
                    current_env, _MAIN_FETCH_READY_SIGNALS
                ),
            }
        )

    env.register_cycle_observer(capture)
    return samples


def _register_uncache_redirect_observer(
    env, *, target_pc: int | None = None
) -> list[dict[str, int]]:
    samples: list[dict[str, int]] = []

    def capture(cycle, current_env) -> None:
        target_seen = 0
        if target_pc is not None:
            target_seen = int(
                any(
                    int(current_env.backend_observe_if.cfvec_valid[slot].value or 0) == 1
                    and int(current_env.backend_observe_if.cfvec_pc[slot].value or 0)
                    == int(target_pc)
                    for slot in range(8)
                )
            )
        samples.append(
            {
                "cycle": int(cycle),
                "response": uncache._require_first_dut_signal(
                    current_env, _UNCACHE_RESPONSE_VALID_SIGNALS
                ),
                "need_resend": uncache._require_first_dut_signal(
                    current_env, _UNCACHE_NEED_RESEND_SIGNALS
                ),
                "req_addr": uncache._require_first_dut_signal(
                    current_env, _UNCACHE_REQ_ADDR_SIGNALS
                ),
                "wb_redirect": uncache._require_first_dut_signal(
                    current_env, _IFU_WB_REDIRECT_SIGNALS
                ),
                "ibuffer_ready": uncache._require_first_dut_signal(
                    current_env, _IBUFFER_READY_SIGNALS
                ),
                "backend_redirect": int(
                    current_env.backend_ctrl_if.redirect_valid.value
                ),
                "target_seen": target_seen,
            }
        )

    env.register_cycle_observer(capture)
    return samples


def _wait_for_three_main_fetches_after_redirect(
    env, samples: list[dict[str, int]], *, max_cycles: int = 1024
) -> None:
    redirect_cycles = [sample["cycle"] for sample in samples if sample["redirect"] == 1]
    assert redirect_cycles, {"reason": "backend fault redirect was not observed"}
    redirect_cycle = max(redirect_cycles)
    for _ in range(int(max_cycles)):
        fires = [
            sample
            for sample in samples
            if sample["cycle"] > redirect_cycle
            and sample["valid"] == 1
            and sample["ready"] == 1
        ]
        if len(fires) >= 3:
            env.step(2)
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": "fewer than three main-fetch handshakes followed backend fault redirect",
            "redirect_cycle": redirect_cycle,
            "samples_tail": samples[-64:],
        }
    )


def _wait_for_monitor_taken_prediction(env, pc: int, *, max_cycles: int = 6000) -> None:
    for _ in range(int(max_cycles)):
        if any(
            int(observation.pc) == int(pc)
            and bool(getattr(observation, "pred_taken", False))
            for observation in env.monitor.observations
        ):
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": "target CFI never reached cfVec with pred_taken",
            "pc": hex(int(pc)),
            "cycles": int(max_cycles),
            "observed_tail": [
                {
                    "pc": hex(int(item.pc)),
                    "pred_taken": bool(getattr(item, "pred_taken", False)),
                }
                for item in env.monitor.observations[-32:]
            ],
        }
    )


def _run_late_uncache_response_after_redirect(env) -> None:
    nc_expected, cacheable_pcs = uncache._prepare_sv39_dual_nc_cacheable_stream(env)
    target_pc = int(cacheable_pcs[0])
    expected_instr = int(nc_expected[0][1])
    redirect_samples = _register_uncache_redirect_observer(env, target_pc=target_pc)
    env.uncache_agent.configure(latency=96, mmio_latency=96)
    env.icache_agent.configure(hit_latency=1, miss_latency=1, miss_rate=0.0, seed=17)
    uncache._initialize_sv39_fetch(env, reset_vector=uncache._NORMAL_BASE)
    uncache._configure_exec_attrs_16k(env, base_addr=0x80000000)
    uncache._force_redirect_to(env, uncache._NORMAL_BASE)

    assert uncache._wait_for_request_addr(
        env, uncache._NORMAL_PHYS_BASE, max_cycles=6000
    ), env.uncache_agent.get_stats()
    assert env.uncache_agent.pending
    request_cycle = int(env.current_cycle)
    env.backend_model.set_can_accept(0)
    env.backend_model.inject_redirect(target_pc, "rfr-late-uncache", delay_cycles=1)

    for _ in range(32):
        env.step(1)
        redirect_cycles = [
            sample["cycle"]
            for sample in redirect_samples
            if sample["backend_redirect"] == 1
        ]
        if redirect_cycles and any(
            sample["cycle"] > max(redirect_cycles) and sample["target_seen"] == 1
            for sample in redirect_samples
        ):
            break
    else:
        raise AssertionError(
            {
                "reason": "cacheable target cfVec did not become valid after redirect",
                "samples_tail": redirect_samples[-64:],
            }
        )

    assert env.uncache_agent.pending
    response_count = int(env.uncache_agent.get_stats().get("resp_count", 0))
    env.uncache_agent.pending[0].ready_cycle = int(env.current_cycle) + 1
    env.step(1)
    assert int(env.uncache_agent.get_stats().get("resp_count", 0)) == response_count + 1
    env.backend_model.set_can_accept(1)
    _wait_for_pc(env, target_pc, max_cycles=6000)
    env.step(8)

    redirect_cycles = [
        sample["cycle"]
        for sample in redirect_samples
        if sample["backend_redirect"] == 1
    ]
    response_cycles = [
        sample["cycle"]
        for sample in redirect_samples
        if sample["response"] == 1
    ]
    assert redirect_cycles and response_cycles, redirect_samples[-128:]
    assert min(response_cycles) > max(redirect_cycles) >= request_cycle
    assert min(response_cycles) - max(redirect_cycles) <= 15
    coincident = [
        sample
        for sample in redirect_samples
        if sample["response"] == 1 and sample["target_seen"] == 1
    ]
    assert coincident, {
        "reason": "late old uncache response did not coincide with target cfVec",
        "response_cycles": response_cycles,
        "samples_tail": redirect_samples[-128:],
    }
    observed = next(
        observation
        for observation in env.monitor.observations
        if int(observation.pc) == target_pc
    )
    assert int(observed.instr) == expected_instr
    assert not env.monitor.get_errors()
    assert not env.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.parametrize(
    "fault_kind,fault_bit,redirect_faults",
    [
        pytest.param("iaf", 1, {"backend_iaf": 1}, id="iaf"),
        pytest.param("ipf", 12, {"backend_ipf": 1}, id="ipf"),
        pytest.param("igpf", 20, {"backend_igpf": 1}, id="igpf"),
    ],
)
def test_rfr_s01_fault_redirect_recovery_three_fetches(
    env, fault_kind: str, fault_bit: int, redirect_faults: dict[str, int]
) -> None:
    """Deliver a backend fetch fault and observe three recovery fetches."""
    main_fetch_samples = _register_main_fetch_observer(env)
    faults.test_backend_fault_redirect_recovery(
        env,
        fault_kind=fault_kind,
        fault_bit=fault_bit,
        redirect_faults=redirect_faults,
    )
    _wait_for_three_main_fetches_after_redirect(env, main_fetch_samples)
    assert not env.monitor.get_errors()
    assert not env.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_rfr_s02_memory_violation_redirect_cancels_mmio(env) -> None:
    """Use the structured memory-violation class on a live MMIO source."""
    mmio.test_mmio_redirect_cancels_wait_last_commit_before_request(env)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.parametrize(
    "is_rvc,ftq_offset,level",
    (
        pytest.param(False, 15, 0, id="flush-after-rvi-interior"),
        pytest.param(False, 31, 0, id="flush-after-rvi-tail"),
        pytest.param(True, 15, 0, id="flush-after-rvc-interior"),
        pytest.param(True, 31, 0, id="flush-after-rvc-tail"),
        pytest.param(False, 0, 1, id="flush-itself-rvi-head"),
        pytest.param(False, 31, 1, id="flush-itself-rvi-tail"),
        pytest.param(True, 0, 1, id="flush-itself-rvc-head"),
        pytest.param(True, 31, 1, id="flush-itself-rvc-tail"),
    ),
)
def test_rfr_s03_source_bound_ahead_match_and_boundaries(
    env, is_rvc: bool, ftq_offset: int, level: int
) -> None:
    """Hit one missing range/offset cross with a matching live cfVec source."""
    _load_boundary_stream(env, is_rvc=is_rvc, ftq_offset=ftq_offset)
    source = _find_live_cfi(env, is_rvc=is_rvc, ftq_offset=ftq_offset)
    assert bool(source.is_rvc) is bool(is_rvc)
    assert int(source.ftq_offset) == int(ftq_offset)
    target_pc = int(source.pc) + 0x100
    before = len(env.monitor.observations)
    InjectRedirectSequence(
        RedirectTxn(
            source_pc=int(source.pc),
            source_ftq_flag=int(source.ftq_flag),
            source_ftq_value=int(source.ftq_value),
            source_ftq_offset=int(source.ftq_offset),
            target_pc=target_pc,
            reason="rfr-s03-source-boundary",
            level=int(level),
            redirect_class=(
                BackendRedirectClass.OTHER
                if not is_rvc and int(ftq_offset) == 0
                else BackendRedirectClass.CONTROL_FLOW
            ),
            ftq_idx_ahead_flag=int(source.ftq_flag),
            ftq_idx_ahead_value=int(source.ftq_value),
        )
    ).inject(env)
    _wait_for_pc(env, target_pc)
    assert not any(
        int(observation.pc) == int(source.pc)
        for observation in env.monitor.observations[before:]
    )
    assert not env.monitor.get_errors()
    assert not env.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.parametrize(
    "path",
    (
        pytest.param("late-old-response", id="late-old-response"),
        pytest.param("cross-page-resend", id="cross-page-resend"),
        pytest.param("sequential-response", id="sequential-response"),
    ),
)
def test_rfr_s04_uncache_response_redirect_recovery(env, path: str) -> None:
    """Cover delayed-old and natural cross-page/sequential uncache redirects."""
    if path == "late-old-response":
        _run_late_uncache_response_after_redirect(env)
    else:
        samples = _register_uncache_redirect_observer(env)
        if path == "cross-page-resend":
            uncache.test_uncache_page_tail_rvi_need_resend_rechecks_next_page(env)
            matches = [
                sample
                for sample in samples
                if sample["response"] == 1
                and sample["need_resend"] == 1
                and ((sample["req_addr"] >> 1) & 0x7FF) == 0x7FF
                and sample["wb_redirect"] == 1
                and sample["backend_redirect"] == 0
            ]
        else:
            uncache.test_uncache_pbmt_nc_non_mmio_uses_uncache_path(env)
            matches = [
                sample
                for sample in samples
                if sample["response"] == 1
                and sample["need_resend"] == 0
                and sample["ibuffer_ready"] == 1
                and sample["wb_redirect"] == 1
                and sample["backend_redirect"] == 0
            ]
        assert matches, {"path": path, "samples_tail": samples[-128:]}
    assert not env.monitor.get_errors()
    assert not env.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_rfr_s05_bpu_s3_override_flushes_old_window(env) -> None:
    """Keep the existing BPU s3 miss stimulus under the RFR entry point."""
    s1_flush.test_tc_icache_mainpipe_s1_bpu_miss(env)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.parametrize("flush", ("fencei", "sfence"))
def test_rfr_s06_fencei_with_cache_state_restarts_fetch(env, flush: str) -> None:
    """Exercise both architectural frontend flush inputs on live DUT state."""
    if flush == "fencei":
        lowrisk.test_icache_lowrisk_missunit_merge_and_fencei(env)
    else:
        context_switch.test_sfence_scope_after_refill(
            env,
            scenario_id="rfr-sfence-all-address-all-id",
            rs1=1,
            rs2=1,
            ident=0,
            retranslation_page_indexes=None,
        )
    assert not env.monitor.get_errors()
    assert not env.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.parametrize("stream", ["not-cfi", "invalid-taken"])
def test_rfr_s07_predchecker_not_cfi_and_invalid_taken(env, stream: str) -> None:
    """Run each natural PredChecker redirect stream in an isolated DUT run."""
    if stream == "not-cfi":
        predchecker._load_and_reset(env, branch_halfword=13, rvi_jal=True)
        source_pc = _BASE + 26
        _wait_for_monitor_taken_prediction(env, source_pc)
        env.memory.write_u32(source_pc, predchecker._ADDI_X0_X0_0)
        group, name = "ifu_predchecker_v3_fault", "not_cfi_taken"
    else:
        predchecker._load_and_reset(env)
        source_pc = _BASE + 30
        _wait_for_monitor_taken_prediction(env, source_pc)
        env.memory.write_u32(source_pc, predchecker._BRANCH_SAME_TARGET)
        group, name = "ifu_predchecker_v3_fault", "invalid_taken"
    predchecker._pulse_fencei(env)
    env.monitor.clear()
    env.backend_model.inject_redirect(_BASE, f"rfr-s07-{stream}", delay_cycles=1)
    predchecker._run_until_bin(env, group, name, max_cycles=2048, debug_pc=source_pc)
    if stream == "invalid-taken":
        env.step(1)
    assert not env.monitor.get_errors()
    assert not env.get_errors()


def test_rfr_s08_unclassified_redirect_recovery_definition() -> None:
    """Lock the corrected RFR coverage and bind sampling contracts."""
    funcov_dir = Path(__file__).parents[3] / "env" / "funcov" / "sv"
    source = funcov_dir / "redirect_flush_recovery_funcov.sv"
    bind = funcov_dir / "zz_frontend_funcov_bind.sv"
    text = source.read_text(encoding="utf-8")
    bind_text = bind.read_text(encoding="utf-8")
    assert "wire redirect_other = backend_redirect_valid" in text
    assert "!backend_redirect_iaf && !backend_redirect_ipf && !backend_redirect_igpf" in text
    assert "recovery_unclassified &&" in text
    assert "cfvec_target_seen" in text
    assert "input logic [50:0]          backend_redirect_target" in text
    assert "input logic [49:0]          to_bpu_redirect_target" in text
    assert "to_bpu_redirect_target == backend_redirect_target[50:1]" in text
    assert "main_fetch_fire && main_fetch_has_backend_exception" in text
    assert "backend_exception_active_prev && !backend_exception_active" in text
    registered_uncache_response = (
        ".rfr_uncache_response_valid(_inner_instrUncache_io_toIfu_resp_valid)"
    )
    raw_uncache_response = (
        ".rfr_uncache_response_valid(auto_inner_instrUncache_client_out_d_valid)"
    )
    assert registered_uncache_response in bind_text
    assert raw_uncache_response not in bind_text
