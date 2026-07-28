from __future__ import annotations

import os
from pathlib import Path
from types import SimpleNamespace
from typing import Callable, Sequence

import pytest

from env.agents.icache_agent import ICacheAgent
from env.memory_model import MemoryModel


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BASE = 0x8004_0000
_REDIRECT_BASE = 0x8005_0000
_DUAL_BASE = 0x8006_0000
_NOP = 0x0000_0013


def _aliases(name: str) -> tuple[str, str]:
    return (name, f"TOP.{name}")


_SIGNALS = {
    "last_fire": _aliases("Frontend_top.Frontend.inner_icache.missUnit.lastFire"),
    "last_fire_next": _aliases("Frontend_top.Frontend.inner_icache.missUnit.lastFireNext"),
    "miss_resp_valid": _aliases("Frontend_top.Frontend.inner_icache.mainPipe.io_missResp_valid"),
    "miss_resp_corrupt": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.__Vtogcov__io_missResp_bits_corrupt"
    ),
    "miss_resp_denied": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.__Vtogcov__io_missResp_bits_denied"
    ),
    "s1_valid": _aliases("Frontend_top.Frontend.inner_icache.mainPipe.s1_valid"),
    "s1_req1_valid": _aliases("Frontend_top.Frontend.inner_icache.mainPipe.s1_req_1_valid"),
    "s1_req0_addr": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_0_vAddr_0_addr"
    ),
    "s1_req1_addr": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_1_vAddr_0_addr"
    ),
    "s1_ftq0_flag": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_0_ftqIdx_flag"
    ),
    "s1_ftq0_value": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_0_ftqIdx_value"
    ),
    "s1_ftq1_flag": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_1_ftqIdx_flag"
    ),
    "s1_ftq1_value": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_1_ftqIdx_value"
    ),
    "s1_exception": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_exception_value"
    ),
    "s1_mmio": _aliases("Frontend_top.Frontend.inner_icache.mainPipe.s1_isMmio"),
    "backend_redirect": _aliases(
        "Frontend_top.Frontend.inner_ftq.backendRedirect_valid"
    ),
    "s1_fire": _aliases("Frontend_top.Frontend.inner_icache.mainPipe.s1_fire"),
    "s1_flush": _aliases("Frontend_top.Frontend.inner_icache.mainPipe.s1_flush"),
    "to_ifu_valid": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.io_toIfu_req_valid"
    ),
    "to_ifu_exception": _aliases(
        "Frontend_top.Frontend._inner_icache_io_toIfu_req_bits_0_icacheMeta_exception_value"
    ),
}

for _index in range(4):
    _SIGNALS[f"should_fetch_{_index}"] = _aliases(
        f"Frontend_top.Frontend.inner_icache.mainPipe.s1_shouldFetch_{_index}"
    )
    _SIGNALS[f"mshr_valid_{_index}"] = _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe."
        f"s1_mshrValid_{_index // 2}_{_index % 2}"
    )
    _SIGNALS[f"mshr_valid_reg_{_index}"] = _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe."
        f"s1_mshrValidReg_{_index // 2}_{_index % 2}"
    )


def _try_read(env, names: Sequence[str]) -> int | None:
    for name in names:
        try:
            signal = getattr(env.dut, str(name), None)
            if signal is None:
                getter = getattr(env.dut, "GetInternalSignal", None)
                signal = getter(str(name)) if callable(getter) else None
            value = None if signal is None else getattr(signal, "value", None)
            if value is not None:
                return int(value)
        except Exception:
            continue
    return None


def _require_read(env, names: Sequence[str]) -> int:
    value = _try_read(env, names)
    assert value is not None, {"missing_internal_signals": list(names)}
    return int(value)


def _sample_mainpipe(env, cycle: int) -> dict:
    return {
        "cycle": int(cycle),
        **{name: _require_read(env, aliases) for name, aliases in _SIGNALS.items()},
    }


def _register_mainpipe_observer(env) -> list[dict]:
    samples: list[dict] = []

    def observe(cycle, active_env) -> None:
        samples.append(_sample_mainpipe(active_env, cycle))

    env.register_cycle_observer(observe)
    return samples


def _run_until(env, predicate: Callable[[], bool], *, max_cycles: int) -> bool:
    for _ in range(int(max_cycles)):
        if predicate():
            return True
        env.step(1)
    return bool(predicate())


def _load_nops(env, base: int, *, words: int = 256) -> None:
    payload = (_NOP.to_bytes(4, "little")) * int(words)
    env.load_program(payload, int(base))


def _load_two_fetch_loop(env, base: int) -> None:
    """Load trained-case-equivalent RVC blocks on disjoint data banks.

    Consecutive 32-byte blocks alternate between the upper half of one line
    and the lower half of the next.  Their MainPipe bank masks therefore do
    not trigger the intentional two-request SRAM conflict fallback even when
    adjacent lines occupy the same replacement way.
    """
    payload = bytearray()
    payload.extend((0x0200_0413).to_bytes(4, "little"))  # addi s0, zero, 32
    payload.extend((_NOP.to_bytes(4, "little")) * 7)
    for block in range(8):
        payload.extend((0x0001).to_bytes(2, "little") * 15)  # c.nop
        # c.j +2 for the first seven blocks.  The final -254-byte c.j closes
        # the loop at the first aligned block without depending on GPR state.
        c_jump = 0xA009 if block != 7 else 0xB709
        payload.extend(int(c_jump).to_bytes(2, "little"))
    env.load_program(bytes(payload), int(base))


def _dual_tag(sample: dict) -> tuple[tuple[int, int], tuple[int, int]]:
    return (
        (int(sample["s1_ftq0_flag"]), int(sample["s1_ftq0_value"])),
        (int(sample["s1_ftq1_flag"]), int(sample["s1_ftq1_value"])),
    )


def _completed_dual_transactions(samples: Sequence[dict]) -> list[dict]:
    completed: list[dict] = []
    active: dict | None = None
    last_started_tag = None
    for sample in samples:
        if (
            sample["s1_valid"] == 1
            and sample["s1_req1_valid"] == 1
            and sample["s1_exception"] == 0
            and sample["s1_mmio"] == 0
            and sample["s1_flush"] == 0
        ):
            tag = _dual_tag(sample)
            if tag != last_started_tag:
                misses = [bool(sample[f"should_fetch_{index}"]) for index in range(4)]
                req0_miss = bool(misses[0] or misses[1])
                req1_miss = bool(misses[2] or misses[3])
                active = {
                    "tag": tag,
                    "start_cycle": int(sample["cycle"]),
                    "req0_addr": int(sample["s1_req0_addr"]) << 1,
                    "req1_addr": int(sample["s1_req1_addr"]) << 1,
                    "pattern": {
                        (False, False): "hit_hit",
                        (False, True): "hit_miss",
                        (True, False): "miss_hit",
                        (True, True): "miss_miss",
                    }[(req0_miss, req1_miss)],
                    "required": misses,
                    "raw_cycles": [None] * 4,
                    "registered_cycles": [None] * 4,
                }
                last_started_tag = tag
        if active is None:
            continue
        for index, required in enumerate(active["required"]):
            if not required:
                continue
            if sample[f"mshr_valid_{index}"] == 1 and active["raw_cycles"][index] is None:
                active["raw_cycles"][index] = int(sample["cycle"])
            if (
                sample[f"mshr_valid_reg_{index}"] == 1
                and active["registered_cycles"][index] is None
            ):
                active["registered_cycles"][index] = int(sample["cycle"])
        if (
            sample["s1_valid"] == 1
            and sample["s1_req1_valid"] == 1
            and _dual_tag(sample) == active["tag"]
            and sample["s1_fire"] == 1
        ):
            active["fire_cycle"] = int(sample["cycle"])
            active["registered_exact"] = all(
                not required
                or (
                    active["raw_cycles"][index] is not None
                    and active["registered_cycles"][index]
                    == active["raw_cycles"][index] + 1
                )
                for index, required in enumerate(active["required"])
            )
            completed.append(active)
            active = None
    return completed


def _initialize_cacheable_stream(
    env,
    base: int,
    *,
    latency: int,
    samples: list[dict] | None = None,
) -> list[dict]:
    _load_nops(env, int(base))
    env.icache_agent.configure(
        hit_latency=int(latency),
        miss_latency=int(latency),
        miss_rate=1.0,
        seed=0x6219,
    )
    observed = samples if samples is not None else _register_mainpipe_observer(env)
    env.initialize(reset_vector=int(base), bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(int(base))
    return observed


def _target_response_records(env, addr: int) -> list[dict]:
    line_addr = int(addr) & ~0x3F
    return [
        record
        for record in env.icache_agent.get_stats().get("response_records", [])
        if int(record["address"]) == line_addr
    ]


def _find_registered_refill(samples: Sequence[dict]) -> tuple[int, int]:
    for position, sample in enumerate(samples):
        if sample["miss_resp_valid"] != 1 or sample["last_fire_next"] != 1:
            continue
        for index in range(4):
            if sample[f"mshr_valid_{index}"] == 1:
                return position, index
    raise AssertionError(
        {
            "reason": "no MainPipe-associated miss response was observed",
            "tail": list(samples[-32:]),
        }
    )


def test_icache_miss_response_signal_contract_matches_dut_inventory() -> None:
    offset = Path(__file__).resolve().parents[7] / "build-frontend/pylib/Frontend/Frontend_offset.yaml"
    assert offset.is_file(), "DUT signal inventory is required before signal-contract tests"
    registered = {
        line[len("  - name: ") :].strip()
        for line in offset.read_text(encoding="utf-8").splitlines()
        if line.startswith("  - name: ")
    }
    missing = [
        list(aliases)
        for aliases in _SIGNALS.values()
        if not any(alias in registered for alias in aliases)
    ]
    assert not missing, {"missing_internal_signals": missing}


class _Signal:
    def __init__(self, value: int = 0) -> None:
        self.value = int(value)


def _unit_icache_interface() -> SimpleNamespace:
    return SimpleNamespace(
        a_ready=_Signal(),
        a_valid=_Signal(),
        a_bits_source=_Signal(),
        a_bits_address=_Signal(),
        d_valid=_Signal(),
        d_bits_opcode=_Signal(),
        d_bits_source=_Signal(),
        d_bits_denied=_Signal(),
        d_bits_data=_Signal(),
        d_bits_corrupt=_Signal(),
    )


@pytest.mark.parametrize(
    "fault,expected_denied,expected_corrupt",
    [
        ({"corrupt": 1}, 0, 1),
        ({"denied": 1}, 1, 1),
    ],
)
def test_icache_agent_fault_injection_obeys_tilelink_contract(
    fault,
    expected_denied,
    expected_corrupt,
) -> None:
    memory = MemoryModel()
    memory.load_bin((_NOP.to_bytes(4, "little")) * 16, _BASE)
    agent = ICacheAgent(memory)
    interface = _unit_icache_interface()
    agent.interface = interface
    agent.configure(hit_latency=0, miss_latency=0, miss_rate=1.0, seed=1)
    agent.inject_response_fault_at(_BASE + 8, **fault)

    interface.a_valid.value = 1
    interface.a_bits_source.value = 2
    interface.a_bits_address.value = _BASE
    agent.on_clock_edge(10)
    interface.a_valid.value = 0
    agent.on_clock_edge(11)

    stats = agent.get_stats()
    assert int(stats["resp_line_count"]) == 1
    assert int(stats["denied_resp_count"]) == expected_denied
    assert int(stats["corrupt_resp_count"]) == expected_corrupt
    assert [int(item["beat_idx"]) for item in stats["response_records"]] == [0, 1]
    assert all(int(item["denied"]) == expected_denied for item in stats["response_records"])
    assert all(int(item["corrupt"]) == expected_corrupt for item in stats["response_records"])


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_refill_response_registers_one_cycle_before_mainpipe_delivery(env) -> None:
    samples = _initialize_cacheable_stream(env, _BASE, latency=16)
    assert _run_until(
        env,
        lambda: any(
            sample["miss_resp_valid"] == 1
            and any(sample[f"mshr_valid_{index}"] == 1 for index in range(4))
            for sample in samples
        ),
        max_cycles=6000,
    ), env.icache_agent.get_stats()
    env.step(2)

    position, refill_index = _find_registered_refill(samples)
    assert position > 0
    response = samples[position]
    previous = samples[position - 1]
    registered = samples[position + 1]

    # #6219 deliberately registers MissUnit's response before it participates
    # in s1_hits.  The raw response and its RegNext consumer must not collapse
    # into one sampling cycle.
    assert previous["last_fire"] == 1
    assert response["last_fire_next"] == 1
    assert response["miss_resp_valid"] == 1
    assert response[f"mshr_valid_{refill_index}"] == 1
    assert registered[f"mshr_valid_reg_{refill_index}"] == 1
    assert int(registered["cycle"]) == int(response["cycle"]) + 1
    assert response["to_ifu_valid"] == 0
    assert any(response[f"should_fetch_{index}"] == 1 for index in range(4))
    assert registered["to_ifu_valid"] == 1

    target_records = _target_response_records(env, _BASE)
    assert [int(record["beat_idx"]) for record in target_records[:2]] == [0, 1]
    stalled = [
        sample
        for sample in samples[: position + 1]
        if sample["s1_valid"] == 1
        and sample["s1_req0_addr"] == (_BASE >> 1)
        and sample["to_ifu_valid"] == 0
        and any(sample[f"should_fetch_{index}"] == 1 for index in range(4))
    ]
    assert len(stalled) >= 4, {"stalled_samples": stalled, "response": response}
    assert len({sample["s1_req0_addr"] for sample in stalled}) == 1
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-520", "BIN-523", "BIN-524")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_trained_two_fetch_hit_hit_then_fencei_miss_miss(env) -> None:
    samples = _register_mainpipe_observer(env)
    _load_two_fetch_loop(env, _DUAL_BASE)
    env.icache_agent.configure(
        hit_latency=8,
        miss_latency=8,
        miss_rate=1.0,
        seed=0x2524,
    )
    env.initialize(reset_vector=_DUAL_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_DUAL_BASE)

    assert _run_until(
        env,
        lambda: any(
            item["pattern"] == "hit_hit"
            for item in _completed_dual_transactions(samples)
        ),
        max_cycles=2_000,
    ), {
        "reason": "trained loop never issued and delivered two resident blocks",
        "patterns": _completed_dual_transactions(samples)[-16:],
        "dual_valid_cycles": sum(
            sample["s1_valid"] == 1 and sample["s1_req1_valid"] == 1
            for sample in samples
        ),
        "dual_fire_cycles": sum(
            sample["s1_fire"] == 1 and sample["s1_req1_valid"] == 1
            for sample in samples
        ),
        "icache": env.icache_agent.get_stats(),
        "monitor_observations": len(env.monitor.observations),
        "branch_checker": env.branch_checker.get_stats(),
    }
    hit_hit = next(
        item
        for item in reversed(_completed_dual_transactions(samples))
        if item["pattern"] == "hit_hit"
    )
    assert hit_hit["req0_addr"] != hit_hit["req1_addr"]
    assert hit_hit["registered_exact"] is True

    split = len(samples)
    fencei = getattr(env.dut, "io_fencei", None)
    assert fencei is not None, {"missing_dut_signal": "io_fencei"}
    fencei.value = 1
    env.step(1)
    fencei.value = 0

    assert _run_until(
        env,
        lambda: any(
            item["pattern"] == "miss_miss" and item["registered_exact"]
            for item in _completed_dual_transactions(samples[split:])
        ),
        max_cycles=2_000,
    ), {
        "reason": "fence.i did not create a completed dual miss/refill transaction",
        "patterns": _completed_dual_transactions(samples[split:])[-16:],
        "icache": env.icache_agent.get_stats(),
    }
    miss_miss = next(
        item
        for item in _completed_dual_transactions(samples[split:])
        if item["pattern"] == "miss_miss" and item["registered_exact"]
    )
    assert int(miss_miss["fire_cycle"]) >= max(
        cycle for cycle in miss_miss["registered_cycles"] if cycle is not None
    )
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "expected_pattern,evict_req",
    [
        pytest.param(
            "hit_miss",
            1,
            marks=pytest.mark.funcov_bins("BIN-521", "BIN-524"),
        ),
        pytest.param(
            "miss_hit",
            0,
            marks=[
                pytest.mark.funcov_bins("BIN-522", "BIN-524"),
                pytest.mark.skip(
                    reason=(
                        "BLOCKED(environment): faulting req0 after fence.i prevents "
                        "req1 from becoming resident before exception recovery; "
                        "the harness has no per-line invalidate/residency control"
                    )
                ),
            ],
        ),
    ],
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_trained_two_fetch_asymmetric_line_refill(
    env,
    expected_pattern,
    evict_req,
) -> None:
    samples = _register_mainpipe_observer(env)
    _load_two_fetch_loop(env, _DUAL_BASE)
    env.icache_agent.configure(
        hit_latency=8,
        miss_latency=8,
        miss_rate=1.0,
        seed=0x2524 + int(evict_req),
    )
    env.initialize(reset_vector=_DUAL_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_DUAL_BASE)

    assert _run_until(
        env,
        lambda: any(
            item["pattern"] == "hit_hit"
            and (item["req0_addr"] & ~0x3F) != (item["req1_addr"] & ~0x3F)
            for item in _completed_dual_transactions(samples)
        ),
        max_cycles=2_000,
    ), {
        "reason": "no cross-line resident dual transaction was trained",
        "patterns": _completed_dual_transactions(samples)[-16:],
    }
    resident = next(
        item
        for item in reversed(_completed_dual_transactions(samples))
        if item["pattern"] == "hit_hit"
        and (item["req0_addr"] & ~0x3F) != (item["req1_addr"] & ~0x3F)
    )
    req_addrs = (int(resident["req0_addr"]), int(resident["req1_addr"]))
    fault_line = req_addrs[int(evict_req)] & ~0x3F
    split = len(samples)
    split_cycle = int(samples[-1]["cycle"])
    env.icache_agent.inject_response_fault_at(fault_line, corrupt=1)
    fencei = getattr(env.dut, "io_fencei", None)
    assert fencei is not None, {"missing_dut_signal": "io_fencei"}
    fencei.value = 1
    env.step(1)
    fencei.value = 0

    assert _run_until(
        env,
        lambda: any(
            int(record["address"]) == fault_line and int(record["corrupt"]) == 1
            for record in env.icache_agent.get_stats().get("response_records", [])
        ),
        max_cycles=4_000,
    ), {
        "reason": "faulted post-fence line never returned",
        "fault_line": hex(fault_line),
        "icache": env.icache_agent.get_stats(),
    }
    clean_line = req_addrs[1 - int(evict_req)] & ~0x3F
    assert any(
        int(record["cycle"]) >= split_cycle
        and int(record["address"]) == clean_line
        and int(record["corrupt"]) == 0
        and int(record["denied"]) == 0
        for record in env.icache_agent.get_stats().get("response_records", [])
    ), {
        "reason": "the companion line did not complete a clean refill",
        "clean_line": hex(clean_line),
        "icache": env.icache_agent.get_stats(),
    }
    assert _run_until(
        env,
        lambda: int(env.monitor.exception_mark_count) > 0,
        max_cycles=1000,
    ), {"reason": "fault-suppressed refill did not reach the IFU exception path"}
    assert _run_until(
        env,
        lambda: any(
            item["pattern"] == expected_pattern and item["registered_exact"]
            for item in _completed_dual_transactions(samples[split:])
        ),
        max_cycles=4_000,
    ), {
        "reason": "fault-suppressed post-fence refill did not produce the target dual pattern",
        "expected_pattern": expected_pattern,
        "fault_line": hex(fault_line),
        "req_addrs": [hex(addr) for addr in req_addrs],
        "patterns": _completed_dual_transactions(samples[split:])[-16:],
        "icache": env.icache_agent.get_stats(),
    }
    result = next(
        item
        for item in _completed_dual_transactions(samples[split:])
        if item["pattern"] == expected_pattern and item["registered_exact"]
    )
    assert int(result["fire_cycle"]) >= max(
        cycle for cycle in result["registered_cycles"] if cycle is not None
    )
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "fault,expected_exception",
    [
        ({"corrupt": 1}, 5),
        ({"denied": 1}, 3),
    ],
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_refill_fault_reaches_ifu_exception(env, fault, expected_exception) -> None:
    samples = _register_mainpipe_observer(env)
    env.icache_agent.inject_response_fault_at(_BASE, **fault)
    _initialize_cacheable_stream(env, _BASE, latency=12, samples=samples)

    assert _run_until(
        env,
        lambda: any(
            sample["to_ifu_valid"] == 1
            and sample["to_ifu_exception"] == int(expected_exception)
            for sample in samples
        ),
        max_cycles=6000,
    ), {
        "icache": env.icache_agent.get_stats(),
        "samples": samples[-32:],
    }
    assert _run_until(
        env,
        lambda: int(env.monitor.exception_mark_count) > 0,
        max_cycles=1000,
    )

    fault_responses = [
        sample
        for sample in samples
        if sample["miss_resp_valid"] == 1
        and sample["miss_resp_corrupt"] == 1
        and sample["miss_resp_denied"] == (1 if fault.get("denied") else 0)
    ]
    assert fault_responses
    stats = env.icache_agent.get_stats()
    assert int(stats["corrupt_resp_count"]) == 1
    assert int(stats["denied_resp_count"]) == (1 if fault.get("denied") else 0)
    assert not env.monitor.get_errors()


@pytest.mark.parametrize("fault", [{"corrupt": 1}, {"denied": 1}])
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_flush_drops_outstanding_fault_response(env, fault) -> None:
    samples = _register_mainpipe_observer(env)
    _load_nops(env, _REDIRECT_BASE)
    env.icache_agent.inject_response_fault_at(_BASE, **fault)
    _initialize_cacheable_stream(env, _BASE, latency=64, samples=samples)

    assert _run_until(
        env,
        lambda: any(
            int(item["address"]) == (_BASE & ~0x3F)
            for item in env.icache_agent.get_stats().get("request_records", [])
        )
        and any(
            sample["s1_valid"] == 1
            and sample["s1_req0_addr"] == (_BASE >> 1)
            and any(sample[f"should_fetch_{index}"] == 1 for index in range(4))
            for sample in samples
        ),
        max_cycles=6000,
    ), env.icache_agent.get_stats()

    old_transaction = next(
        sample
        for sample in reversed(samples)
        if sample["s1_valid"] == 1 and sample["s1_req0_addr"] == (_BASE >> 1)
    )
    old_tag = _dual_tag(old_transaction)
    redirect_cycle = int(env.current_cycle)
    env.backend_model.inject_redirect(_REDIRECT_BASE, "ctrl_redirect", delay_cycles=0)
    assert _run_until(
        env,
        lambda: bool(_target_response_records(env, _BASE)),
        max_cycles=2000,
    )
    assert _run_until(
        env,
        lambda: any(int(obs.pc) == _REDIRECT_BASE for obs in env.monitor.observations),
        max_cycles=6000,
    ), {
        "icache": env.icache_agent.get_stats(),
        "observed_pcs": [hex(int(obs.pc)) for obs in env.monitor.observations[-32:]],
    }

    old_fault_response_cycles = {
        int(record["cycle"])
        for record in _target_response_records(env, _BASE)
        if int(record["corrupt"]) == 1
    }
    assert old_fault_response_cycles
    old_response_end_cycle = max(old_fault_response_cycles)
    redirect_samples = [
        sample
        for sample in samples
        if sample["backend_redirect"] == 1 and int(sample["cycle"]) >= redirect_cycle
    ]
    assert redirect_samples, {
        "reason": "backend redirect was not observable at the FTQ boundary",
        "redirect_cycle": redirect_cycle,
        "tail": samples[-32:],
    }
    assert any(
        sample["s1_flush"] == 1
        and abs(int(sample["cycle"]) - int(redirect_sample["cycle"])) <= 1
        for sample in samples
        for redirect_sample in redirect_samples
    ), {
        "reason": "backend redirect did not flush the outstanding MainPipe transaction",
        "redirect_cycle": redirect_cycle,
        "tail": samples[-32:],
    }
    associated_fault_responses = [
        sample
        for sample in samples
        if (
            sample["miss_resp_valid"] == 1
            and sample["miss_resp_corrupt"] == 1
            and int(sample["cycle"]) >= redirect_cycle
        )
    ]
    if associated_fault_responses:
        assert all(sample["to_ifu_exception"] == 0 for sample in associated_fault_responses)
    else:
        # A BPU/FTQ redirect flushes prefetch MSHRs.  If the target line was
        # first allocated as a prefetch (source >= the four fetch MSHRs), the
        # external D beats still return but mshrValid suppresses missResp.
        # This is a legal and stronger early-drop path, not missing evidence.
        response_sources = {
            int(record["source"])
            for record in _target_response_records(env, _BASE)
            if int(record["corrupt"]) == 1
        }
        assert response_sources and min(response_sources) >= 4, {
            "reason": "fault response disappeared without a flushed prefetch MSHR",
            "response_sources": sorted(response_sources),
            "samples": samples[-32:],
        }
    assert not any(
        redirect_cycle <= int(sample["cycle"]) <= old_response_end_cycle + 4
        and sample["s1_valid"] == 1
        and sample["s1_fire"] == 1
        and sample["s1_req0_addr"] == (_BASE >> 1)
        and _dual_tag(sample) == old_tag
        for sample in samples
    ), {
        "reason": "flushed FTQ tag was delivered after the outstanding response returned",
        "old_tag": old_tag,
    }
    # The target miss had not delivered before redirect.  Its later faulted
    # response may complete the MSHR, but must not create a stale IFU exception.
    assert not any(int(obs.pc) == _BASE for obs in env.monitor.observations)
    assert int(env.monitor.exception_mark_count) == 0
    assert not env.monitor.get_errors()
