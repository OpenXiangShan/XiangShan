from __future__ import annotations

import pytest

from tests.py.jiabowen import test_icache_mainpipe_miss_response as icache


_CROSS64_PC = 0x8007_003E
_CROSS64_FIRST_LINE = _CROSS64_PC & ~0x3F
_CROSS64_SECOND_LINE = _CROSS64_FIRST_LINE + 0x40
_ILLEGAL_RVC_BASE = 0x8008_0000
_ILLEGAL_RVC_PC = _ILLEGAL_RVC_BASE + 0x3E
_LEGAL_AFTER_ILLEGAL_PC = _ILLEGAL_RVC_BASE + 0x40
_CNOP = 0x0001
_ADDI_X0_X0_0 = 0x0000_0013
_TRIGGER_PC = 0x8009_0000
_TRIGGER_DEBUG_MODE = 1
_INSTRUCTION_ACCESS_FAULT_BIT = 1
_HARDWARE_ERROR_BIT = 19


def _read_exception_bit(signal) -> int:
    value = getattr(signal, "value", None)
    return 0 if value is None else int(value)


def _require_dut_signal(env, name: str):
    signal = getattr(env.dut, name, None)
    assert signal is not None, {"missing_dut_signal": name}
    return signal


def _capture_backend_exception_state(env) -> list[dict]:
    records: list[dict] = []

    def capture(cycle: int, active_env) -> None:
        observe = active_env.backend_observe_if
        for slot in range(8):
            if int(observe.cfvec_valid[slot].value) != 1:
                continue
            exception_bits = tuple(
                bit
                for bit in range(24)
                if _read_exception_bit(observe.cfvec_exception_vec[slot][bit]) == 1
            )
            records.append(
                {
                    "cycle": int(cycle),
                    "slot": slot,
                    "pc": int(observe.cfvec_pc[slot].value),
                    "ftq_flag": int(observe.cfvec_ftq_ptr_flag[slot].value),
                    "ftq_value": int(observe.cfvec_ftq_ptr_value[slot].value),
                    "exception_bits": exception_bits,
                }
            )

    env.register_cycle_observer(capture)
    return records


def _capture_backend_cfvec_cycles(
    env, *, include_trigger: bool = False
) -> dict[int, list[dict]]:
    records: dict[int, list[dict]] = {}

    def capture(cycle: int, active_env) -> None:
        observe = active_env.backend_observe_if
        slots: list[dict] = []
        for slot in range(8):
            if int(observe.cfvec_valid[slot].value) != 1:
                continue
            trigger = 0
            if include_trigger:
                trigger = int(
                    _require_dut_signal(
                        active_env, f"io_backend_cfVec_{slot}_bits_trigger"
                    ).value
                )
            slots.append(
                {
                    "slot": slot,
                    "pc": int(observe.cfvec_pc[slot].value),
                    "instr": int(observe.cfvec_instr[slot].value),
                    "is_rvc": bool(observe.cfvec_is_rvc[slot].value),
                    "trigger": trigger,
                    "exception_bits": tuple(
                        bit
                        for bit in range(24)
                        if _read_exception_bit(observe.cfvec_exception_vec[slot][bit]) == 1
                    ),
                }
            )
        records[int(cycle)] = slots

    env.register_cycle_observer(capture)
    return records


def _configure_frontend_execute_trigger(env, pc: int) -> None:
    prefix = "io_csrCtrl_frontend_trigger_"
    values = {
        f"{prefix}tUpdate_bits_addr": 0,
        f"{prefix}tUpdate_bits_tdata_matchType": 0,
        f"{prefix}tUpdate_bits_tdata_select": 0,
        f"{prefix}tUpdate_bits_tdata_timing": 0,
        f"{prefix}tUpdate_bits_tdata_action": _TRIGGER_DEBUG_MODE,
        f"{prefix}tUpdate_bits_tdata_chain": 0,
        f"{prefix}tUpdate_bits_tdata_tdata2": int(pc),
        f"{prefix}tEnableVec_0": 1,
        f"{prefix}tEnableVec_1": 0,
        f"{prefix}tEnableVec_2": 0,
        f"{prefix}tEnableVec_3": 0,
        f"{prefix}debugMode": 0,
        f"{prefix}triggerCanRaiseBpExp": 0,
    }
    for name, value in values.items():
        _require_dut_signal(env, name).value = int(value)
    valid_name = f"{prefix}tUpdate_valid"
    _require_dut_signal(env, valid_name).value = 1


@pytest.mark.parametrize("fault_line", ["first", "second"])
@pytest.mark.parametrize(
    "fault,expected_denied",
    [
        pytest.param({"corrupt": 1}, 0, id="corrupt"),
        pytest.param({"denied": 1}, 1, id="corrupt-and-denied"),
    ],
)
@pytest.mark.skipif(not icache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_cacheable_cross64_refill_fault_delivery(
    env,
    fault_line: str,
    fault: dict[str, int],
    expected_denied: int,
) -> None:
    target_line = _CROSS64_FIRST_LINE if fault_line == "first" else _CROSS64_SECOND_LINE
    clean_line = _CROSS64_SECOND_LINE if fault_line == "first" else _CROSS64_FIRST_LINE
    samples = icache._register_mainpipe_observer(env)
    backend_records = _capture_backend_exception_state(env)
    env.icache_agent.inject_response_fault_at(target_line, **fault)
    icache._initialize_cacheable_stream(env, _CROSS64_PC, latency=12, samples=samples)

    assert icache._run_until(
        env,
        lambda: {
            int(record["address"])
            for record in env.icache_agent.get_stats().get("request_records", [])
        }.issuperset({_CROSS64_FIRST_LINE, _CROSS64_SECOND_LINE}),
        max_cycles=6000,
    ), env.icache_agent.get_stats()
    expected_icache_exception = 3 if expected_denied else 5
    expected_exception_bit = (
        _INSTRUCTION_ACCESS_FAULT_BIT if expected_denied else _HARDWARE_ERROR_BIT
    )
    assert icache._run_until(
        env,
        lambda: any(
            sample["to_ifu_valid"] == 1
            and sample["to_ifu_exception"] == expected_icache_exception
            for sample in samples
        ),
        max_cycles=2000,
    ), {"samples": samples[-64:], "icache": env.icache_agent.get_stats()}
    assert icache._run_until(
        env,
        lambda: any(
            record["pc"] == _CROSS64_PC
            and record["exception_bits"] == (expected_exception_bit,)
            for record in backend_records
        ),
        max_cycles=2000,
    ), {"backend_records": backend_records, "samples": samples[-64:]}

    stats = env.icache_agent.get_stats()
    request_addrs = [
        int(record["address"])
        for record in stats.get("request_records", [])
        if int(record["address"]) in {_CROSS64_FIRST_LINE, _CROSS64_SECOND_LINE}
    ]
    assert request_addrs.index(_CROSS64_FIRST_LINE) < request_addrs.index(_CROSS64_SECOND_LINE)
    fault_responses = [
        record
        for record in stats.get("response_records", [])
        if int(record["address"]) == target_line
        and int(record["corrupt"]) == 1
        and int(record["denied"]) == expected_denied
    ]
    clean_responses = [
        record
        for record in stats.get("response_records", [])
        if int(record["address"]) == clean_line
        and int(record["corrupt"]) == 0
        and int(record["denied"]) == 0
    ]
    assert {int(record["beat_idx"]) for record in fault_responses} == {0, 1}
    assert {int(record["beat_idx"]) for record in clean_responses} == {0, 1}
    target_records = [
        record for record in backend_records if int(record["pc"]) == _CROSS64_PC
    ]
    assert target_records
    assert all(
        record["exception_bits"] == (expected_exception_bit,) for record in target_records
    ), target_records
    assert len({(record["ftq_flag"], record["ftq_value"]) for record in target_records}) == 1
    assert int(stats["corrupt_resp_count"]) == 1
    assert int(stats["denied_resp_count"]) == expected_denied
    assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()
    assert not env.get_errors()


@pytest.mark.skipif(not icache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_cacheable_illegal_rvc_is_followed_by_clean_legal_delivery(env) -> None:
    payload = bytearray(_CNOP.to_bytes(2, "little") * 96)
    payload[0x3E:0x40] = b"\x00\x00"
    env.load_program(bytes(payload), _ILLEGAL_RVC_BASE)
    env.icache_agent.configure(hit_latency=12, miss_latency=12, miss_rate=1.0, seed=0x1FED)
    cfvec_cycles = _capture_backend_cfvec_cycles(env)
    env.initialize(reset_vector=_ILLEGAL_RVC_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_ILLEGAL_RVC_BASE)

    def consecutive_illegal_then_legal() -> bool:
        for cycle, current in cfvec_cycles.items():
            previous = cfvec_cycles.get(cycle - 1, [])
            if not any(
                record["pc"] == _ILLEGAL_RVC_PC
                and record["is_rvc"]
                and record["exception_bits"] == (2,)
                for record in previous
            ):
                continue
            if any(
                record["pc"] == _LEGAL_AFTER_ILLEGAL_PC
                and record["instr"] == _ADDI_X0_X0_0
                and record["is_rvc"]
                and not record["exception_bits"]
                for record in current
            ):
                return True
        return False

    assert icache._run_until(
        env,
        consecutive_illegal_then_legal,
        max_cycles=6000,
    ), {
        "cfvec_cycles": list(cfvec_cycles.items())[-64:],
        "icache": env.icache_agent.get_stats(),
    }
    legal_cycle = next(
        cycle
        for cycle, current in cfvec_cycles.items()
        if any(
            record["pc"] == _LEGAL_AFTER_ILLEGAL_PC
            and record["instr"] == _ADDI_X0_X0_0
            and record["is_rvc"]
            and not record["exception_bits"]
            for record in current
        )
        and any(
            record["pc"] == _ILLEGAL_RVC_PC
            and record["is_rvc"]
            and record["exception_bits"] == (2,)
            for record in cfvec_cycles.get(cycle - 1, [])
        )
    )
    assert all(
        2 not in record["exception_bits"] for record in cfvec_cycles[legal_cycle]
    ), cfvec_cycles[legal_cycle]
    illegal_records = [
        record
        for cycle in cfvec_cycles.values()
        for record in cycle
        if record["pc"] == _ILLEGAL_RVC_PC
    ]
    assert illegal_records
    assert all(
        record["is_rvc"] and record["exception_bits"] == (2,)
        for record in illegal_records
    ), illegal_records
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == 0
    assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()
    assert not env.get_errors()


@pytest.mark.skipif(not icache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_cacheable_execute_pc_trigger_is_delivered_to_cfvec(env) -> None:
    samples = _capture_backend_cfvec_cycles(env, include_trigger=True)
    icache._initialize_cacheable_stream(env, _TRIGGER_PC, latency=12)
    _configure_frontend_execute_trigger(env, _TRIGGER_PC)
    env.step(2)
    _require_dut_signal(
        env, "io_csrCtrl_frontend_trigger_tUpdate_valid"
    ).value = 0

    def target_triggered() -> bool:
        return any(
            record["pc"] == _TRIGGER_PC
            and record["trigger"] == _TRIGGER_DEBUG_MODE
            for cycle in samples.values()
            for record in cycle
        )

    assert icache._run_until(env, target_triggered, max_cycles=6000), {
        "cfvec_cycles": list(samples.items())[-64:],
        "icache": env.icache_agent.get_stats(),
    }
    target_records = [
        record
        for cycle in samples.values()
        for record in cycle
        if record["pc"] == _TRIGGER_PC
    ]
    assert target_records
    assert all(
        record["trigger"] == _TRIGGER_DEBUG_MODE
        and record["instr"] == _ADDI_X0_X0_0
        and not record["is_rvc"]
        and not record["exception_bits"]
        for record in target_records
    )
    assert all(
        record["trigger"] != _TRIGGER_DEBUG_MODE
        for cycle in samples.values()
        for record in cycle
        if record["pc"] != _TRIGGER_PC
    )
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == 0
    assert env.monitor.exception_mark_count == 0
    assert not env.monitor.get_errors()
    assert not env.get_errors()
