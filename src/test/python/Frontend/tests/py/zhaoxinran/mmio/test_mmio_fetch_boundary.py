from __future__ import annotations

import hashlib
import os

import pytest
from env.funcov.py.ifu import mmio_nc_owner_funcov as owner_funcov
from env.model.golden_trace import GoldenTrace
from env.runtime.frontend_init import FrontendInitConfig
from env.support import record_scenario, scenario_rng
from tests.py.support.dut_support import _wait_for_req_delta, _wait_for_resp_delta, run_until
from tests.py.support import uncache_scenarios as uncache
from tests.py.support.translation_faults import _read_exception_bit

from tests.py.zhaoxinran.support.mmio import (
    _run_mmio_redirect_cancels_wait_last_commit_before_request,
    load_mmio_payload as _load_mmio_payload,
    mmio_cross_beat_rvi_stream as _random_cross_beat_rvi_stream,
    mmio_non_crossing_rvi_stream,
    load_random_mmio_stream,
    prepare_mmio_cnop_stream,
    prepare_mmio_page_tail_stream,
    mmio_rvc_beat_tail_entry,
)

_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"


_INSTRUCTION_ACCESS_FAULT_BIT = 1
_HARDWARE_ERROR_BIT = 19


def _capture_cfvec_exceptions(env) -> list[dict]:
    records: list[dict] = []

    def capture(cycle: int, active_env) -> None:
        observe = active_env.backend_cfvec_if
        for slot in range(8):
            if int(observe.cfvec_valid[slot].value) != 1:
                continue
            records.append(
                {
                    "cycle": int(cycle),
                    "pc": active_env.observed_cfvec_pc(slot),
                    "exception_bits": tuple(
                        bit
                        for bit in (1, 2, 12, 19, 20)
                        if _read_exception_bit(getattr(observe, f"cfvec_exception_vec_{bit}")[slot]) == 1
                    ),
                }
            )

    env.register_cycle_observer(capture)
    return records


def _assert_target_exception(records: list[dict], *, pc: int, expected_bit: int) -> None:
    target = [record for record in records if int(record["pc"]) == int(pc)]
    assert target, {"missing_exception_pc": hex(int(pc)), "records": records[-64:]}
    assert all(record["exception_bits"] == (int(expected_bit),) for record in target), target


def _payload_sha256(payload: bytes) -> str:
    return hashlib.sha256(bytes(payload)).hexdigest()


def _configure_random_mmio_latency(env, rng) -> int:
    latency = rng.randint(1, 16)
    env.uncache_agent.configure(latency=latency, mmio_latency=latency)
    return latency


def _rvi_rvc_payload(*, first: str) -> bytes:
    rvi = int(uncache._ADDI_X0_X0_0).to_bytes(4, "little")
    rvc = int(uncache._CNOP).to_bytes(2, "little")
    padding = rvc * 128
    return (rvi + rvc + padding) if first == "rvi" else (rvc + rvi + padding)


@pytest.mark.parametrize("frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_rvi_non_tail_8b_offsets_deliver_instruction(env):
    scenario_key = "zhaoxinran/mmio/rvi-non-tail"
    base_seed, seed, rng = scenario_rng(scenario_key)
    _, layout_seed, layout_rng = scenario_rng(f"{scenario_key}/pc", base_seed=base_seed)
    images, trace = mmio_non_crossing_rvi_stream(layout_rng)
    target_pcs = [entry.pc for entry in trace if entry.size == 4 and entry.kind == "normal"]
    jumps = [(entry.pc, entry.target_pc) for entry in trace if entry.kind == "jump"]
    assert len(jumps) == len(images) - 1 >= 1
    for image in images:
        _load_mmio_payload(env, image.payload, base=image.base_addr)
    # Let the backend model resolve the JALs from the delivered instruction stream.
    env.backend_model.set_golden_trace(GoldenTrace(trace))
    latency = _configure_random_mmio_latency(env, rng)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "pc": trace[0].pc,
            "target_count": len(target_pcs),
            "page_count": len(images),
            "target_pcs": target_pcs,
            "jumps": jumps,
            "layout_seed": layout_seed,
            "latency": latency,
            "payload_sha256": _payload_sha256(b"".join(image.payload for image in images)),
            "expected_path": "single_beat_normal",
        },
    )
    uncache._initialize_mmio_fetch(env, reset_vector=trace[0].pc)

    beat_bytes = uncache._UNCACHE_BEAT_BYTES
    observations = env.monitor.observations
    requests = env.uncache_agent.request_records
    for index, entry in enumerate(trace[:-1]):  # Exclude the final monitor guard entry.
        pc = entry.pc
        beat = pc & ~(beat_bytes - 1)
        context = {"base_seed": base_seed, "index": index, "pc": hex(pc), "offset": pc - beat}
        if index and trace[index - 1].kind == "jump":
            context["jump_pc"] = hex(trace[index - 1].pc)
        assert run_until(env, lambda: len(observations) > index, max_cycles=8000), {
            **context, "last_pc": hex(observations[-1].pc) if observations else None,
            "monitor_errors": env.monitor.get_errors(),
        }
        observed = observations[index]
        assert observed.pc == pc, {**context, "observed_pc": hex(observed.pc)}
        expected_instr = uncache._ADDI_X0_X0_0 if entry.size == 2 else entry.instr
        assert observed.instr == expected_instr and observed.is_rvc == (entry.size == 2), {
            **context, "expected_instr": hex(expected_instr), "instr": hex(observed.instr), "is_rvc": observed.is_rvc,
        }
        if entry.size == 2:
            continue
        # A preceding C.NOP in this beat can have its own request to the same address.
        previous_cycle = observations[index - 1].cycle if index else -1
        assert any(
            record["address"] == beat and previous_cycle <= record["cycle"] <= observed.cycle
            for record in requests
        ), {**context, "missing_request_beat": hex(beat), "delivery_cycle": observed.cycle}
    assert not (errors := env.monitor.get_errors()), {"base_seed": base_seed, "errors": errors}


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_rvc_at_8b_tail_advances_by_2b_without_second_beat(env):
    scenario_key = "zhaoxinran/mmio/rvc-8b-tail"
    base_seed, seed, rng = scenario_rng(scenario_key)
    placement, payload = mmio_rvc_beat_tail_entry(rng)
    pc = placement.pc
    _load_mmio_payload(env, payload)
    latency = _configure_random_mmio_latency(env, rng)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "beat_index": (pc - uncache._MMIO_BASE) // uncache._UNCACHE_BEAT_BYTES,
            "pc": pc,
            "latency": latency,
            "payload_sha256": _payload_sha256(payload),
            "expected_path": "tail_rvc_no_resend",
        },
    )
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    first_beat = pc & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    assert uncache._wait_for_request_addr(env, first_beat)
    assert uncache._wait_for_observed_pc(env, pc, max_cycles=8000)
    stats_at_delivery = env.uncache_agent.get_stats()
    assert stats_at_delivery.get("request_addrs", []).count(first_beat) == 1
    assert first_beat + 8 not in stats_at_delivery.get("request_addrs", [])
    assert uncache._wait_for_observed_pc(env, pc + 2, max_cycles=8000)
    observed = {
        int(item.pc): item
        for item in env.monitor.observations
        if int(item.pc) in {pc, pc + 2}
    }
    assert bool(observed[pc].is_rvc)
    assert bool(observed[pc + 2].is_rvc)
    assert not env.monitor.get_errors()


@pytest.mark.parametrize("first", ["rvc", "rvi"])
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_adjacent_rvc_rvi_stream_preserves_pc_progress(env, first: str):
    scenario_key = f"zhaoxinran/mmio/adjacent-rvc-rvi/{first}"
    base_seed, seed, rng = scenario_rng(scenario_key)
    payload = _rvi_rvc_payload(first=first)
    first_pc = load_random_mmio_stream(env, scenario_rng(f"{scenario_key}/pc")[2], payload)
    latency = _configure_random_mmio_latency(env, rng)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "first": first,
            "pc": first_pc,
            "latency": latency,
            "payload_sha256": _payload_sha256(payload),
            "expected_path": "adjacent_rvc_rvi_pc_progress",
        },
    )
    uncache._initialize_mmio_fetch(env, reset_vector=first_pc)

    second_pc = first_pc + (2 if first == "rvc" else 4)
    assert uncache._wait_for_request_addr(env, first_pc)
    assert uncache._wait_for_observed_pc(env, first_pc, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, second_pc, max_cycles=8000)
    observed = {
        int(item.pc): item
        for item in env.monitor.observations
        if int(item.pc) in {first_pc, second_pc}
    }
    assert int(observed[first_pc].instr) == uncache._ADDI_X0_X0_0
    assert int(observed[second_pc].instr) == uncache._ADDI_X0_X0_0
    assert bool(observed[first_pc].is_rvc) is (first == "rvc")
    assert bool(observed[second_pc].is_rvc) is (first != "rvc")
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_cross_8b_clean_rvi_requests_next_beat_and_delivers(env):
    scenario_key = "zhaoxinran/mmio/cross-8b-clean-rvi"
    base_seed, seed, rng = scenario_rng(scenario_key)
    pc, first_beat, second_beat, payload = _random_cross_beat_rvi_stream(env, rng)
    latency = _configure_random_mmio_latency(env, rng)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "pc": pc,
            "first_beat": first_beat,
            "second_beat": second_beat,
            "latency": latency,
            "payload_sha256": _payload_sha256(payload),
            "expected_path": "second_beat_normal",
        },
    )
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    assert uncache._wait_for_request_addr(env, first_beat)
    assert uncache._wait_for_request_addr(env, second_beat)
    assert uncache._wait_for_observed_pc(env, pc, max_cycles=8000)
    observed = next(
        item for item in env.monitor.observations if int(item.pc) == pc
    )
    assert int(observed.instr) == uncache._ADDI_X0_X0_0
    assert not bool(observed.is_rvc)
    stats = env.uncache_agent.get_stats()
    assert stats.get("request_addrs", []).count(first_beat) == 1
    assert stats.get("request_addrs", []).count(second_beat) == 1
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "fault",
    [
        pytest.param({"corrupt": 1}, id="corrupt"),
        pytest.param({"corrupt": 1, "denied": 1}, id="corrupt_and_denied"),
    ],
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_cross_8b_first_beat_fault_reports_without_resend(env, fault: dict[str, int]):
    scenario_key = "zhaoxinran/mmio/cross-8b-first-beat-fault"
    ordinal = int(bool(fault.get("denied")))
    base_seed, seed, rng = scenario_rng(scenario_key, ordinal=ordinal)
    pc, first_beat, second_beat, payload = _random_cross_beat_rvi_stream(env, rng)
    latency = _configure_random_mmio_latency(env, rng)
    cfvec_records = _capture_cfvec_exceptions(env)
    env.uncache_agent.inject_response_fault_at(
        first_beat,
        **fault,
    )
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        ordinal=ordinal,
        parameters={
            "pc": pc,
            "first_beat": first_beat,
            "second_beat": second_beat,
            "fault": fault,
            "latency": latency,
            "payload_sha256": _payload_sha256(payload),
            "expected_path": "first_beat_fault_no_resend",
        },
    )
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    assert uncache._wait_for_request_addr(env, first_beat)
    assert _wait_for_resp_delta(env, env.uncache_agent)
    assert uncache._wait_for_monitor_exception(env)
    stats = env.uncache_agent.get_stats()
    assert int(stats.get("corrupt_resp_count", 0)) == 1
    assert int(stats.get("denied_resp_count", 0)) == int(fault.get("denied", 0))
    assert stats.get("request_addrs", []).count(first_beat) == 1
    assert second_beat not in stats.get("request_addrs", [])
    _assert_target_exception(
        cfvec_records,
        pc=pc,
        expected_bit=(
            _INSTRUCTION_ACCESS_FAULT_BIT if fault.get("denied") else _HARDWARE_ERROR_BIT
        ),
    )
    assert not env.monitor.get_errors()
    assert not env.get_errors()


@pytest.mark.parametrize("fault", [None, "corrupt", "corrupt_and_denied"])
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_cross_8b_second_beat_response_modes(env, fault: str | None):
    scenario_key = "zhaoxinran/mmio/cross-8b-second-beat-response"
    ordinal = {None: 0, "corrupt": 1, "corrupt_and_denied": 2}[fault]
    base_seed, seed, rng = scenario_rng(scenario_key, ordinal=ordinal)
    pc, first_beat, second_beat, payload = _random_cross_beat_rvi_stream(env, rng)
    latency = _configure_random_mmio_latency(env, rng)
    cfvec_records = _capture_cfvec_exceptions(env)
    if fault is not None:
        env.uncache_agent.inject_response_fault_at(
            second_beat,
            corrupt=1,
            denied=int(fault == "corrupt_and_denied"),
        )
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        ordinal=ordinal,
        parameters={
            "pc": pc,
            "first_beat": first_beat,
            "second_beat": second_beat,
            "fault": fault or "none",
            "latency": latency,
            "payload_sha256": _payload_sha256(payload),
            "expected_path": "second_beat_normal" if fault is None else "second_beat_fault",
        },
    )
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    assert uncache._wait_for_request_addr(env, first_beat)
    assert uncache._wait_for_request_addr(env, second_beat)
    assert uncache._wait_for_resp_count(env, 2)
    if fault is None:
        assert uncache._wait_for_observed_pc(env, pc, max_cycles=8000)
        observed = next(
            item for item in env.monitor.observations if int(item.pc) == pc
        )
        assert int(observed.instr) == uncache._ADDI_X0_X0_0
        assert not bool(observed.is_rvc)
        assert env.monitor.exception_mark_count == 0
    else:
        assert uncache._wait_for_monitor_exception(env)
        assert env.monitor.exception_mark_count > 0
        stats = env.uncache_agent.get_stats()
        assert stats.get("request_addrs", []).count(first_beat) == 1
        assert stats.get("request_addrs", []).count(second_beat) == 1
        _assert_target_exception(
            cfvec_records,
            pc=pc,
            expected_bit=(
                _INSTRUCTION_ACCESS_FAULT_BIT
                if fault == "corrupt_and_denied"
                else _HARDWARE_ERROR_BIT
            ),
        )
    assert not env.monitor.get_errors()
    assert not env.get_errors()


@pytest.mark.parametrize(
    "fault",
    [
        pytest.param({"corrupt": 1}, id="corrupt"),
        pytest.param({"corrupt": 1, "denied": 1}, id="corrupt-and-denied"),
    ],
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_single_beat_d_response_fault_is_reported(env, fault: dict[str, int]):
    scenario_key = "zhaoxinran/mmio/single-beat-response-fault"
    ordinal = int(bool(fault.get("denied")))
    base_seed, seed, rng = scenario_rng(scenario_key, ordinal=ordinal)
    pc = prepare_mmio_cnop_stream(env, scenario_rng(f"{scenario_key}/pc", ordinal=ordinal)[2])
    latency = _configure_random_mmio_latency(env, rng)
    cfvec_records = _capture_cfvec_exceptions(env)
    env.uncache_agent.inject_next_response_fault(**fault)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        ordinal=ordinal,
        parameters={
            "pc": pc,
            "fault": fault,
            "latency": latency,
            "expected_path": "single_beat_response_fault",
        },
    )
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    assert _wait_for_req_delta(env, env.uncache_agent)
    assert _wait_for_resp_delta(env, env.uncache_agent)
    assert uncache._wait_for_monitor_exception(env)
    stats = env.uncache_agent.get_stats()
    assert int(stats.get("corrupt_resp_count", 0)) == 1
    assert int(stats.get("denied_resp_count", 0)) == int(fault.get("denied", 0))
    assert env.monitor.exception_mark_count > 0
    _assert_target_exception(
        cfvec_records,
        pc=pc,
        expected_bit=(
            _INSTRUCTION_ACCESS_FAULT_BIT if fault.get("denied") else _HARDWARE_ERROR_BIT
        ),
    )
    assert not env.monitor.get_errors()
    assert not env.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_branch_instruction_is_delivered_as_control_flow(env):
    scenario_key = "zhaoxinran/mmio/control-flow/branch"
    base_seed, seed, rng = scenario_rng(scenario_key)
    branch = int(0x00000263).to_bytes(4, "little")
    payload = branch + int(uncache._ADDI_X0_X0_0).to_bytes(4, "little")
    payload += int(uncache._CNOP).to_bytes(2, "little") * 128
    pc = load_random_mmio_stream(env, scenario_rng(f"{scenario_key}/pc")[2], payload)
    latency = _configure_random_mmio_latency(env, rng)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "pc": pc,
            "latency": latency,
            "payload_sha256": _payload_sha256(payload),
            "expected_path": "branch_control_flow",
        },
    )
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
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    assert uncache._wait_for_request_addr(env, pc)
    assert any(
        sample["req_valid"] == 1
        and sample["is_first"] == 1
        and sample["uncache_state"] == uncache._IFU_UNCACHE_INVALID
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}
    assert any(
        sample["uncache_state"] == uncache._IFU_UNCACHE_SEND_REQ
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}
    assert uncache._wait_for_observed_pc(env, pc, max_cycles=8000)
    observed = next(
        item for item in env.monitor.observations if int(item.pc) == pc
    )
    assert int(observed.instr) == 0x00000263
    assert not bool(observed.is_rvc)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_jal_instruction_is_delivered_as_control_flow(env):
    scenario_key = "zhaoxinran/mmio/control-flow/jal"
    base_seed, seed, rng = scenario_rng(scenario_key)
    payload = int(uncache._JAL_X0_PLUS_4).to_bytes(4, "little")
    payload += int(uncache._ADDI_X0_X0_0).to_bytes(4, "little")
    payload += int(uncache._CNOP).to_bytes(2, "little") * 128
    pc = load_random_mmio_stream(env, scenario_rng(f"{scenario_key}/pc")[2], payload)
    latency = _configure_random_mmio_latency(env, rng)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "pc": pc,
            "latency": latency,
            "payload_sha256": _payload_sha256(payload),
            "expected_path": "jal_control_flow",
        },
    )
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    assert uncache._wait_for_request_addr(env, pc)
    assert uncache._wait_for_observed_pc(env, pc, max_cycles=8000)
    observed = next(
        item for item in env.monitor.observations if int(item.pc) == pc
    )
    assert int(observed.instr) == uncache._JAL_X0_PLUS_4
    assert not bool(observed.is_rvc)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_tl_a_backpressure_holds_request_until_accepted(env):
    scenario_key = "zhaoxinran/mmio/tl-a-backpressure"
    base_seed, seed, rng = scenario_rng(scenario_key)
    stall_cycles = rng.randint(4, 20)
    pc = prepare_mmio_cnop_stream(env, scenario_rng(f"{scenario_key}/pc")[2])
    latency = _configure_random_mmio_latency(env, rng)
    env.uncache_agent.set_a_ready(0)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "pc": pc,
            "latency": latency,
            "stall_cycles": stall_cycles,
            "expected_path": "tl_a_backpressure_then_accept",
        },
    )
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    assert uncache._wait_for_uncache_a_valid_addr(env, pc)
    stalled_stats = env.uncache_agent.get_stats()
    stalled_req_count = int(stalled_stats.get("req_count", 0))
    assert int(env.uncache_if.a_valid.value) == 1
    assert int(env.uncache_if.a_bits_address.value) == pc
    env.step(stall_cycles)
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == stalled_req_count
    assert int(env.uncache_if.a_valid.value) == 1
    assert int(env.uncache_if.a_bits_address.value) == pc

    env.uncache_agent.set_a_ready(None)
    assert uncache._wait_for_request_addr(env, pc)
    assert _wait_for_resp_delta(env, env.uncache_agent)
    assert uncache._wait_for_observed_pc(env, pc, max_cycles=8000)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_redirect_drops_a_ready_stalled_request(env):
    scenario_key = "zhaoxinran/mmio/redirect-drops-stalled-request"
    base_seed, seed, rng = scenario_rng(scenario_key)
    pc = prepare_mmio_cnop_stream(env, scenario_rng(f"{scenario_key}/pc")[2])
    latency = _configure_random_mmio_latency(env, rng)
    target_pc = pc + rng.choice((0x40, 0x60, 0x80, 0xA0))
    env.uncache_agent.set_a_ready(0)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "source_pc": pc,
            "target_pc": target_pc,
            "latency": latency,
            "expected_path": "redirect_cancels_unaccepted_request",
        },
    )
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
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    assert uncache._wait_for_uncache_a_valid_addr(env, pc)
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == 0
    uncache._force_redirect_to(env, target_pc)
    env.step(8)
    assert pc not in env.uncache_agent.get_stats().get("request_addrs", [])
    assert any(
        sample["uncache_state"] == uncache._IFU_UNCACHE_WAIT_RESP
        and sample["tl_a_valid"] == 1
        and sample["tl_a_ready"] == 0
        and sample["backend_redirect"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}

    env.uncache_agent.set_a_ready(None)
    assert uncache._wait_for_request_addr(env, target_pc, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, target_pc, max_cycles=8000)
    assert not any(int(item.pc) == pc for item in env.monitor.observations)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_redirect_cancels_wait_last_commit_before_request(env):
    scenario_key = "zhaoxinran/mmio/redirect-cancels-wait-last-commit"
    base_seed, seed, rng = scenario_rng(scenario_key)
    pc = prepare_mmio_cnop_stream(env, scenario_rng(f"{scenario_key}/pc")[2])
    target_pc = pc + rng.choice((0x40, 0x60, 0x80, 0xA0))
    latency = rng.randint(1, 16)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "source_pc": pc,
            "target_pc": target_pc,
            "latency": latency,
            "expected_path": "redirect_cancels_wait_last_commit",
        },
    )
    return _run_mmio_redirect_cancels_wait_last_commit_before_request(
        env,
        pc=pc,
        target_pc=target_pc,
        latency=latency,
    )


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_page_tail_rvi_rechecks_next_page_before_delivery(env):
    scenario_key = "zhaoxinran/mmio/page-tail/rvi-recheck"
    base_seed, seed, rng = scenario_rng(scenario_key)
    pc = prepare_mmio_page_tail_stream(env, scenario_rng(f"{scenario_key}/pc")[2])
    latency = _configure_random_mmio_latency(env, rng)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "pc": pc,
            "latency": latency,
            "expected_path": "rvi_rechecks_next_page_before_delivery",
        },
    )
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    first_beat = pc & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    next_page = pc + 2
    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=8000)
    assert uncache._wait_for_request_addr(env, next_page, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, pc, max_cycles=8000)
    observed = next(
        item for item in env.monitor.observations if int(item.pc) == pc
    )
    assert int(observed.instr) == uncache._ADDI_X0_X0_0
    assert not bool(observed.is_rvc)
    stats = env.uncache_agent.get_stats()
    assert stats.get("request_addrs", []).count(first_beat) == 1
    assert stats.get("request_addrs", []).count(next_page) == 1
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_page_tail_rvc_delivers_before_next_page_fetch(env):
    scenario_key = "zhaoxinran/mmio/page-tail/rvc-before-next-page"
    base_seed, seed, rng = scenario_rng(scenario_key)
    pc = prepare_mmio_page_tail_stream(env, scenario_rng(f"{scenario_key}/pc")[2], is_rvc=True)
    latency = _configure_random_mmio_latency(env, rng)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "pc": pc,
            "latency": latency,
            "expected_path": "rvc_delivers_before_next_page_fetch",
        },
    )
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    first_beat = pc & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    next_page = pc + 2
    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, pc, max_cycles=8000)
    stats_at_delivery = env.uncache_agent.get_stats()
    assert next_page not in stats_at_delivery.get("request_addrs", [])
    assert uncache._wait_for_observed_pc(env, pc + 2, max_cycles=8000)
    observed = {
        int(item.pc): item
        for item in env.monitor.observations
        if int(item.pc) in {pc, pc + 2}
    }
    assert bool(observed[pc].is_rvc)
    assert bool(observed[pc + 2].is_rvc)
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "fault,expect_page_recheck",
    [
        pytest.param({"corrupt": 1}, False, id="corrupt"),
        pytest.param({"denied": 1}, True, id="denied"),
    ],
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_page_tail_first_beat_fault_preserves_resend_contract(
    env, fault: dict[str, int], expect_page_recheck: bool
):
    scenario_key = "zhaoxinran/mmio/page-tail/first-beat-fault"
    ordinal = int(bool(fault.get("denied")))
    base_seed, seed, rng = scenario_rng(scenario_key, ordinal=ordinal)
    pc = prepare_mmio_page_tail_stream(env, scenario_rng(f"{scenario_key}/pc", ordinal=ordinal)[2])
    latency = _configure_random_mmio_latency(env, rng)
    first_beat = pc & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    next_page = pc + 2
    env.uncache_agent.inject_response_fault_at(first_beat, **fault)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        ordinal=ordinal,
        parameters={
            "pc": pc,
            "first_beat": first_beat,
            "next_page": next_page,
            "fault": fault,
            "latency": latency,
            "expect_page_recheck": expect_page_recheck,
            "expected_path": "first_beat_fault_resend_contract",
        },
    )
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=8000)
    assert _wait_for_resp_delta(env, env.uncache_agent)
    stats = env.uncache_agent.get_stats()
    assert int(stats.get("corrupt_resp_count", 0)) == int(fault.get("corrupt", 0))
    assert int(stats.get("denied_resp_count", 0)) == int(fault.get("denied", 0))
    if expect_page_recheck:
        assert uncache._wait_for_request_addr(env, next_page, max_cycles=8000)
        assert uncache._wait_for_observed_pc(env, pc, max_cycles=8000)
        assert env.monitor.exception_mark_count == 0
    else:
        assert uncache._wait_for_monitor_exception(env)
        assert next_page not in stats.get("request_addrs", [])
        assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()
