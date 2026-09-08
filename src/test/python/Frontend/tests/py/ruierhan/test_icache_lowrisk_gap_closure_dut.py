"""Low-risk DUT scenarios for the currently uncovered ICache bins.

This module deliberately drives only existing testbench/DUT top-level inputs.
It does not force internal MSHR, queue, ready, or coverage-model state.
"""

from __future__ import annotations

import os

import pytest

from env.sequences import (
    WayLookupCapacitySequence,
    TranslationScenario,
    TranslationScenarioBuilder,
)
from env.sequences import TranslationPmpPmaEntry
from env.funcov.py.icache.icache_waylookup_funcov import (
    _SIGNALS as _WAYLOOKUP_SIGNALS,
)
from env.support.pmp_pma import PmpPmaConfig
from tests.py.jiabowen.test_icache_mainpipe_miss_response import (
    _initialize_cacheable_stream,
    test_icache_trained_two_fetch_asymmetric_line_refill as _run_asymmetric_refill,
)
from tests.py.jiabowen.test_two_fetch_directed_flow_dut import (
    _load_and_reset as _load_two_fetch_loop,
    _trained_short_block_loop,
    _warm_frontend_execution as _warm_two_fetch_execution,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_NOP = 0x0000_0013
_MAIN = "Frontend_top.Frontend.inner_icache.mainPipe."
_ICACHE = "Frontend_top.Frontend.inner_icache."


def _try_read_internal(env, names: tuple[str, ...]) -> int | None:
    cache = getattr(env, "_ruierhan_internal_signal_cache", None)
    if cache is None:
        cache = {}
        setattr(env, "_ruierhan_internal_signal_cache", cache)
    cache_key = tuple(str(name) for name in names)
    if cache_key in cache:
        signal = cache[cache_key]
        value = None if signal is None else getattr(signal, "value", None)
        return None if value is None else int(value)

    for name in names:
        try:
            signal = getattr(env.dut, str(name), None)
            if signal is None:
                getter = getattr(env.dut, "GetInternalSignal", None)
                signal = getter(str(name)) if callable(getter) else None
            value = None if signal is None else getattr(signal, "value", None)
            if value is not None:
                cache[cache_key] = signal
                return int(value)
        except Exception:
            continue
    cache[cache_key] = None
    return None


def _miss_request_snapshot(env) -> dict[str, int | None]:
    return {
        "cycle": int(env.current_cycle),
        "valid": _try_read_internal(
            env, (_MAIN + "__Vtogcov__io_missReq_valid",)
        ),
        "ready": _try_read_internal(
            env, (_MAIN + "__Vtogcov__io_missReq_ready",)
        ),
        "vset": _try_read_internal(
            env,
            (
                _MAIN + "__Vtogcov__io_missReq_bits_vSetIdx",
                _ICACHE + "_mainPipe_io_missReq_bits_vSetIdx",
            ),
        ),
        "paddr": _try_read_internal(
            env,
            (
                _MAIN + "__Vtogcov__io_missReq_bits_blkPAddr",
                _ICACHE + "_mainPipe_io_missReq_bits_blkPAddr",
            ),
        ),
    }


def _waylookup_value(env, key: str) -> int | None:
    return _try_read_internal(env, tuple(_WAYLOOKUP_SIGNALS[str(key)]))


def _waylookup_snapshot(env) -> dict[str, int | None]:
    snapshot = {
        "cycle": int(env.current_cycle),
        "empty": _waylookup_value(env, "empty"),
        "num_valid": _waylookup_value(env, "num_valid"),
        "read_flag": _waylookup_value(env, "read_flag"),
        "read_value": _waylookup_value(env, "read_value"),
        "write_flag": _waylookup_value(env, "write_flag"),
        "write_value": _waylookup_value(env, "write_value"),
        "exception_valid": _waylookup_value(env, "exception_valid"),
        "write0_valid": _waylookup_value(env, "write0_valid"),
        "write0_ready": _waylookup_value(env, "write0_ready"),
        "write1_valid": _waylookup_value(env, "write1_valid"),
        "write1_ready": _waylookup_value(env, "write1_ready"),
        "to_valid": _waylookup_value(env, "to_valid"),
        "to_ready": _waylookup_value(env, "to_ready"),
        "update_valid": _waylookup_value(env, "update_valid"),
        "prefetch_s1_valid": _waylookup_value(env, "prefetch_s1_valid"),
        "prefetch_s1_state": _waylookup_value(env, "prefetch_s1_state"),
        "prefetch_s1_tlb_finish": _waylookup_value(env, "prefetch_s1_tlb_finish"),
        "flush": _waylookup_value(env, "flush"),
        "bpu_flush": _waylookup_value(env, "bpu_flush"),
        "bpu_flush_match": _waylookup_value(env, "bpu_flush_match"),
        "ftq_req1_valid": _try_read_internal(
            env,
            (_MAIN + "io_fromFtq_bits_req_1_valid",),
        ),
        "info1_valid": _try_read_internal(
            env,
            (
                _MAIN + "io_fromWayLookup_bits_wayLookupInfo_1_valid",
                _MAIN + "__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_1_valid",
            ),
        ),
        "real_two": _try_read_internal(
            env,
            (_ICACHE + "__Vtogcov__io_toFtq_fromMainPipe_realTwoFetchValid",),
        ),
        "info0_mmio": _try_read_internal(
            env,
            (
                _MAIN + "io_fromWayLookup_bits_wayLookupInfo_0_bits_entry_isMmio",
                _MAIN + "__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_0_bits_entry_isMmio",
            ),
        ),
        "info1_mmio": _try_read_internal(
            env,
            (
                _MAIN + "io_fromWayLookup_bits_wayLookupInfo_1_bits_entry_isMmio",
                _MAIN + "__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_1_bits_entry_isMmio",
            ),
        ),
        "info0_exception": _try_read_internal(
            env,
            (_MAIN + "__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_0_bits_exceptionEntry_itlbException_value",),
        ),
        "info1_exception": _try_read_internal(
            env,
            (_MAIN + "__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_1_bits_exceptionEntry_itlbException_value",),
        ),
        "write0_exception": _try_read_internal(
            env,
            (_ICACHE + "prefetcher.__Vtogcov__io_wayLookupWrite_0_bits_exceptionEntry_itlbException_value",),
        ),
    }
    return snapshot


def _wait_waylookup_occupancy(env, minimum: int, *, max_cycles: int) -> None:
    _run_until(
        env,
        lambda: (
            _waylookup_value(env, "num_valid") is not None
            and int(_waylookup_value(env, "num_valid")) >= int(minimum)
        ),
        max_cycles=max_cycles,
        label=f"WayLookup occupancy >= {int(minimum)}",
    )


def _waylookup_contains_line(env, target: int, waymask: int) -> bool:
    """Return whether a live queue entry holds the requested SRAM-hit line."""
    read_value = _waylookup_value(env, "read_value")
    num_valid = _waylookup_value(env, "num_valid")
    if read_value is None or num_valid is None:
        return False
    expected_ptag = int(target) >> 12
    expected_vset = (int(target) >> 6) & 0xFF
    for offset in range(int(num_valid)):
        index = (int(read_value) + offset) % 32
        prefix = f"Frontend_top.Frontend.inner_icache.wayLookup.entries_{index}"
        cov_prefix = (
            "Frontend_top.Frontend.inner_icache.wayLookup."
            f"__Vtogcov__entries_{index}"
        )
        if (
            _try_read_internal(env, (prefix + "_pTag", cov_prefix + "_pTag"))
            == expected_ptag
            and _try_read_internal(
                env, (prefix + "_vSetIdx_0", cov_prefix + "_vSetIdx_0")
            )
            == expected_vset
            and _try_read_internal(
                env, (prefix + "_waymask_0", cov_prefix + "_waymask_0")
            )
            == int(waymask)
        ):
            return True
    return False


def _waylookup_live_entries(env) -> list[dict[str, int | None]]:
    """Return compact metadata for currently live WayLookup entries."""
    read_value = _waylookup_value(env, "read_value")
    num_valid = _waylookup_value(env, "num_valid")
    if read_value is None or num_valid is None:
        return []
    entries: list[dict[str, int | None]] = []
    for offset in range(min(int(num_valid), 32)):
        index = (int(read_value) + offset) % 32
        prefix = f"Frontend_top.Frontend.inner_icache.wayLookup.entries_{index}"
        cov_prefix = (
            "Frontend_top.Frontend.inner_icache.wayLookup."
            f"__Vtogcov__entries_{index}"
        )
        entries.append(
            {
                "index": index,
                "ptag": _try_read_internal(
                    env, (prefix + "_pTag", cov_prefix + "_pTag")
                ),
                "vset": _try_read_internal(
                    env, (prefix + "_vSetIdx_0", cov_prefix + "_vSetIdx_0")
                ),
                "waymask": _try_read_internal(
                    env, (prefix + "_waymask_0", cov_prefix + "_waymask_0")
                ),
            }
        )
    return entries
_IFU_CACHEABLE_REQ_VALID = (
    "Frontend_top.Frontend.inner_icache.mainPipe.io_toIfu_req_valid",
    "Frontend_top.Frontend.inner_icache.mainPipe.__Vtogcov__io_toIfu_req_valid",
)


def _load_nops(env, base: int, *, words: int = 512) -> None:
    env.load_program((_NOP.to_bytes(4, "little")) * int(words), int(base))


def _load_idle_loop(env, base: int) -> None:
    """Keep the reset-vector stream in one line while the directed probe runs."""
    env.load_program(_jal(0, 0).to_bytes(4, "little") + _NOP.to_bytes(4, "little") * 255, int(base))


def _jal(rd: int, offset: int) -> int:
    """Encode a JAL whose signed immediate is within the architectural range."""
    assert int(offset) % 2 == 0
    assert -(1 << 20) <= int(offset) < (1 << 20)
    imm = int(offset) & 0x1F_FFFF
    return (
        (((imm >> 20) & 1) << 31)
        | (((imm >> 1) & 0x3FF) << 21)
        | (((imm >> 11) & 1) << 20)
        | (((imm >> 12) & 0xFF) << 12)
        | ((int(rd) & 0x1F) << 7)
        | 0x6F
    )


def _load_same_set_jump_loop(
    env,
    base: int,
    *,
    stride: int,
    line_count: int,
    segment_lines: int = 8,
) -> tuple[int, ...]:
    """Load sequential segments whose anchors share one set and JAL in a loop."""
    targets = tuple(int(base) + index * int(stride) for index in range(line_count))
    payload = bytearray((_NOP.to_bytes(4, "little")) * 32768)
    for index, target in enumerate(targets):
        next_target = targets[(index + 1) % len(targets)]
        branch = target + int(segment_lines) * 64 - 4
        offset = branch - int(base)
        payload[offset : offset + 4] = _jal(0, next_target - branch).to_bytes(4, "little")
    env.load_program(bytes(payload), int(base))
    return targets


def _run_until(env, predicate, *, max_cycles: int, label: str) -> None:
    for _ in range(int(max_cycles)):
        if predicate():
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": f"timeout while waiting for {label}",
            "max_cycles": int(max_cycles),
            "current_cycle": int(env.current_cycle),
            "stats": env.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _wait_funcov_hit(
    env,
    group: str,
    bin_name: str,
    *,
    max_cycles: int,
    label: str | None = None,
) -> None:
    max_cycles = int(os.getenv("TB_ICACHE_LOWRISK_MAX_CYCLES", str(max_cycles)), 0)
    target = (group, bin_name)
    for _ in range(max_cycles):
        if env.functional_coverage.key_hit(*target):
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": f"timeout while waiting for {label or f'{group}.{bin_name}'}",
            "missing": [f"{group}.{bin_name}"],
            "max_cycles": max_cycles,
            "current_cycle": int(env.current_cycle),
            "waylookup": _waylookup_snapshot(env),
            "miss_request": _miss_request_snapshot(env),
            "coverage_state": getattr(
                env.functional_coverage, "_icache_hitmiss_cov_state", None
            ),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _wait_funcov_hit_checker_clean(
    env,
    group: str,
    bin_name: str,
    *,
    max_cycles: int,
    label: str,
) -> None:
    """Stop a closure probe as soon as its evidence becomes ineligible."""
    for _ in range(int(max_cycles)):
        if env.functional_coverage.key_hit(group, bin_name):
            return
        errors = env.monitor.get_errors()
        if errors:
            raise AssertionError(
                {
                    "reason": f"checker error while waiting for {label}",
                    "first_monitor_error": errors[0],
                    "current_cycle": int(env.current_cycle),
                }
            )
        env.step(1)
    _wait_funcov_hit(
        env,
        group,
        bin_name,
        max_cycles=1,
        label=label,
    )


def _wait_funcov_hits(
    env,
    targets: tuple[tuple[str, str], ...],
    *,
    max_cycles: int,
    label: str,
) -> None:
    max_cycles = int(os.getenv("TB_ICACHE_LOWRISK_MAX_CYCLES", str(max_cycles)), 0)
    for _ in range(max_cycles):
        missing = [
            (group, name)
            for group, name in targets
            if not env.functional_coverage.key_hit(group, name)
        ]
        if not missing:
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": f"timeout while waiting for {label}",
            "missing": [f"{group}.{name}" for group, name in missing],
            "max_cycles": max_cycles,
            "current_cycle": int(env.current_cycle),
            "waylookup": _waylookup_snapshot(env),
            "miss_request": _miss_request_snapshot(env),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _wait_for_target_response(
    env,
    target: int,
    *,
    max_cycles: int,
    label: str,
    after_cycle: int | None = None,
) -> None:
    """Wait for the specific cache line requested by a redirect."""
    line = int(target) & ~0x3F
    for _ in range(int(max_cycles)):
        if any(
            int(record.get("address", -1)) == line
            and int(record.get("beat_idx", -1)) == 1
            and (
                after_cycle is None
                or int(record.get("cycle", -1)) > int(after_cycle)
            )
            for record in env.icache_agent.get_stats().get("response_records", [])
        ):
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": f"timeout while waiting for {label}",
            "target_line": line,
            "current_cycle": int(env.current_cycle),
            "stats": env.icache_agent.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _wait_for_target_request(
    env,
    target: int,
    *,
    max_cycles: int,
    after_cycle: int | None = None,
) -> dict:
    """Return the first accepted TileLink request for ``target`` in this phase."""
    line = int(target) & ~0x3F
    for _ in range(int(max_cycles)):
        matches = [
            record
            for record in env.icache_agent.get_stats().get("request_records", [])
            if int(record.get("address", -1)) == line
            and (
                after_cycle is None
                or int(record.get("cycle", -1)) > int(after_cycle)
            )
        ]
        if matches:
            return matches[0]
        env.step(1)
    raise AssertionError(
        {
            "reason": "timeout while waiting for target ICache request",
            "target_line": line,
            "current_cycle": int(env.current_cycle),
            "stats": env.icache_agent.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _assert_no_target_request(
    env,
    target: int,
    *,
    after_cycle: int,
    label: str,
) -> None:
    """Assert that a line was not fetched during the preceding phase."""
    line = int(target) & ~0x3F
    records = [
        record
        for record in env.icache_agent.get_stats().get("request_records", [])
        if int(record.get("address", -1)) == line
        and int(record.get("cycle", -1)) > int(after_cycle)
    ]
    assert not records, {
        "reason": f"unexpected ICache request during {label}",
        "target_line": hex(line),
        "after_cycle": int(after_cycle),
        "requests": records,
    }


def _mixed_source_hit_evidence(env) -> dict:
    """Return the sampler evidence for BIN-1139's mixed-source hit."""
    group = "icache_mainpipe_maybe_rvc_align"
    name = "mixed_source_merge"
    definition = env.functional_coverage.definition_by_group_bin[(group, name)]
    hit = env.functional_coverage.hits.get(definition.key)
    assert hit is not None and hit.hits > 0, {
        "reason": "BIN-1139 was not marked by the functional-coverage sampler",
        "coverage_key": definition.key,
    }
    assert hit.evidence, {
        "reason": "BIN-1139 hit has no sampler evidence",
        "coverage_key": definition.key,
    }
    evidence = hit.evidence[-1]
    assert tuple(evidence.get("mshr_source_lines", ())) and tuple(
        evidence.get("sram_source_lines", ())
    ), {"reason": "BIN-1139 hit evidence omitted source-line classification", "evidence": evidence}
    assert any(evidence["mshr_source_lines"]), evidence
    assert any(evidence["sram_source_lines"]), evidence
    return evidence


def _wait_mshr_line_released(env, target: int, *, max_cycles: int) -> None:
    """Wait until no live MissUnit entry owns the target cache line."""
    block = (int(target) & ~0x3F) >> 6
    for _ in range(int(max_cycles) + 1):
        entries = []
        signals_available = False
        for index in range(14):
            prefix = _ICACHE + f"missUnit.allMshr_{index}."
            valid = _try_read_internal(
                env, (prefix + "valid", prefix + "__Vtogcov__valid")
            )
            paddr = _try_read_internal(
                env, (prefix + "blkPAddr", prefix + "__Vtogcov__blkPAddr")
            )
            signals_available |= valid is not None and paddr is not None
            if valid == 1 and paddr == block:
                entries.append(index)
        assert signals_available, {
            "reason": "MissUnit MSHR ownership signals are unavailable",
            "target_block": hex(block),
        }
        if not entries:
            return
        if _ < int(max_cycles):
            env.step(1)
    raise AssertionError(
        {
            "reason": "soft-prefetch MSHR was not released",
            "target_block": hex(block),
            "live_entries": entries,
            "current_cycle": int(env.current_cycle),
        }
    )


def _collect_target_refill_waymasks(
    env,
    targets: tuple[int, ...],
    *,
    minimum_commits: int,
    max_cycles: int,
) -> dict[int, int]:
    """Collect one-hot refill ways while a legal demand-fetch loop trains."""
    target_by_block = {(int(target) & ~0x3F) >> 6: int(target) for target in targets}
    resident_by_waymask: dict[int, int] = {}
    for _ in range(int(max_cycles)):
        valid = _waylookup_value(env, "update_valid")
        paddr = _waylookup_value(env, "update_paddr")
        waymask = _waylookup_value(env, "update_waymask")
        if valid == 1 and paddr in target_by_block and waymask is not None:
            assert int(waymask).bit_count() == 1, {
                "reason": "demand refill waymask is not one-hot",
                "paddr": paddr,
                "waymask": waymask,
            }
            resident_by_waymask[int(waymask)] = target_by_block[int(paddr)]
        if (
            len(resident_by_waymask) == len(targets)
            and int(env.backend_model.get_stats().get("commit_count", 0))
            >= int(minimum_commits)
        ):
            return resident_by_waymask
        errors = env.monitor.get_errors()
        if errors:
            raise AssertionError(
                {
                    "reason": "checker error while training same-set fetch loop",
                    "first_monitor_error": errors[0],
                    "resident_by_waymask": resident_by_waymask,
                }
            )
        env.step(1)
    raise AssertionError(
        {
            "reason": "same-set demand loop did not fill four ways and train",
            "resident_by_waymask": resident_by_waymask,
            "backend": env.backend_model.get_stats(),
            "waylookup": _waylookup_snapshot(env),
        }
    )


def _read_first_signal(env, names: tuple[str, ...]) -> int | None:
    for name in names:
        try:
            signal = getattr(env.dut, name, None)
            if signal is None:
                getter = getattr(env.dut, "GetInternalSignal", None)
                signal = getter(name) if callable(getter) else None
            value = None if signal is None else getattr(signal, "value", None)
            if value is not None:
                return int(value)
        except Exception:
            continue
    return None


def _clear_soft_prefetch(env) -> None:
    for slot in range(3):
        valid = getattr(env.dut, f"io_softPrefetch_{slot}_valid", None)
        address = getattr(env.dut, f"io_softPrefetch_{slot}_bits_vaddr", None)
        if valid is not None:
            valid.value = 0
        if address is not None:
            address.value = 0


def _pulse_fencei(env) -> None:
    signal = getattr(env.clock_reset, "io_fencei", None)
    assert signal is not None, {"missing_signal": "io_fencei"}
    signal.value = 1
    env.step(1)
    signal.value = 0
    env.step(2)


def _drive_soft_prefetch(env, addresses: list[int]) -> None:
    _clear_soft_prefetch(env)
    for slot, address in enumerate(addresses[:3]):
        valid = getattr(env.dut, f"io_softPrefetch_{slot}_valid", None)
        value = getattr(env.dut, f"io_softPrefetch_{slot}_bits_vaddr", None)
        assert valid is not None and value is not None, {
            "missing_signal": f"io_softPrefetch_{slot}"
        }
        valid.value = 1
        value.value = int(address)
    env.step(1)
    _clear_soft_prefetch(env)


def _set_predictors(env, enabled: bool) -> None:
    value = 1 if enabled else 0
    env.set_bp_ctrl_enable(
        ubtb_enable=value,
        abtb_enable=value,
        mbtb_enable=value,
        tage_enable=value,
        sc_enable=value,
        ittage_enable=value,
    )


@pytest.fixture
def lowrisk_cleanup(env):
    """Restore top-level test inputs even when a scenario fails."""
    yield env
    _clear_soft_prefetch(env)
    fencei = getattr(env.clock_reset, "io_fencei", None)
    if fencei is not None:
        fencei.value = 0
    env.backend_model.set_can_accept(1)
    _set_predictors(env, True)


@pytest.mark.funcov_bins("BIN-685")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_mainpipe_flush_refill(env) -> None:
    base = 0x8004_0000
    redirect = base + 0x100
    _initialize_cacheable_stream(env, base, latency=32)

    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["req_count"]) >= 1,
        max_cycles=6000,
        label="initial MainPipe cache request",
    )
    env.backend_model.inject_redirect(redirect, "ctrl_redirect", delay_cycles=0)
    _run_until(
        env,
        lambda: any(int(observation.pc) == redirect for observation in env.monitor.observations),
        max_cycles=1024,
        label="redirect target delivery",
    )
    assert int(env.icache_agent.get_stats()["req_count"]) >= 2
    _wait_funcov_hit(
        env,
        "icache_prefetchpipe_s2_miss",
        "missunit_backpressure_recovery",
        max_cycles=256,
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins(
    "BIN-655", "BIN-656", "BIN-657", "BIN-663", "BIN-677",
    "BIN-661", "BIN-778", "BIN-779", "BIN-780"
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_prefetch_soft_requests(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    base = 0x8004_0000
    _load_nops(env, base, words=512)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=1.0,
        seed=0x6255,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)

    for layout in (
        (base + 0x100, base + 0x180),
        (base + 0x200, base + 0x280),
        (base + 0x300,),
        (base + 0x380,),
    ):
        _drive_soft_prefetch(env, list(layout))
        env.step(8)

    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["req_count"]) >= 1,
        max_cycles=512,
        label="soft-prefetch or Fetch cache request",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-674", "BIN-602")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_mainpipe_s0_entry_two_fetch_dut(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    _load_two_fetch_loop(env)
    _warm_two_fetch_execution(env)

    _wait_funcov_hit(
        env,
        "icache_mainpipe_s0_entry",
        "dual_request_data_read",
        max_cycles=4096,
        label="dual FTQ/WayLookup request plus DataArray read",
    )
    _wait_funcov_hit(
        env,
        "icache_mainpipe_s0_entry",
        "ftq_waylookup_skew",
        max_cycles=4096,
        label="FTQ/WayLookup skew followed by atomic join and s1 latch",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-674", "BIN-603")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_mainpipe_s0_entry_data_backpressure_dut(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    base = 0x8000_0000

    _load_nops(env, base, words=512)
    _run_until(
        env,
        lambda: int(env.backend_model.get_stats().get("commit_count", 0)) >= 4,
        max_cycles=3000,
        label="warmup commits before backend pressure",
    )
    env.backend_model.set_can_accept(0)
    env.step(16)
    env.backend_model.set_can_accept(1)
    _wait_funcov_hit(
        env,
        "icache_mainpipe_s0_entry",
        "data_array_backpressure",
        max_cycles=4096,
        label="WayLookup held while DataArray request is not ready",
    )
    _wait_funcov_hit(
        env,
        "icache_mainpipe_s0_entry",
        "ftq_waylookup_skew",
        max_cycles=4096,
        label="FTQ/WayLookup skew followed by atomic join and s1 latch",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-610")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_mainpipe_single_bank_range_dut(env) -> None:
    base = 0x8008_0000
    target = base + 0x08

    _load_nops(env, base, words=512)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=16,
        miss_rate=1.0,
        seed=0x6610,
    )
    env.initialize(reset_vector=target, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(target)

    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["resp_line_count"]) >= 1,
        max_cycles=1024,
        label="initial target line refill",
    )
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=16,
        miss_rate=0.0,
        seed=0x6611,
    )
    env.backend_model.inject_redirect(target, "ctrl_redirect", delay_cycles=0)
    _run_until(
        env,
        lambda: env.functional_coverage.key_hit(
            "icache_mainpipe_s1_sram",
            "single_line_bank_range",
        ),
        max_cycles=1024,
        label="single-line nonzero-bank SRAM hit",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins(
    "BIN-686", "BIN-690", "BIN-691", "BIN-692", "BIN-699", "BIN-705", "BIN-1005",
    "BIN-707", "BIN-708", "BIN-709", "BIN-710", "BIN-711", "BIN-712", "BIN-1006", "BIN-1007", "BIN-1008",
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_missunit_merge_and_fencei(lowrisk_cleanup) -> None:
    return _run_icache_lowrisk_missunit_merge_and_fencei(lowrisk_cleanup)


def _run_icache_lowrisk_missunit_merge_and_fencei(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    base = 0x8004_0000
    _initialize_cacheable_stream(env, base, latency=96)

    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["req_count"]) >= 1,
        max_cycles=6000,
        label="initial MissUnit request",
    )
    _drive_soft_prefetch(env, [base + 0x100, base + 0x180, base + 0x200])
    _pulse_fencei(env)
    env.backend_model.inject_redirect(base + 0x400, "ctrl_redirect", delay_cycles=0)
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["resp_beat_count"]) >= 2,
        max_cycles=1024,
        label="post-Fence.i refill response",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-746")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_waylookup_updates_and_flush(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    base = 0x8006_0000
    _load_nops(env, base, words=8192)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=24,
        miss_rate=1.0,
        seed=0x746,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)
    env.step(32)
    env.backend_model.inject_redirect(base + 0x400, "ctrl_redirect", delay_cycles=0)
    _wait_funcov_hit(
        env,
        "icache_waylookup_flush",
        "flush_recovery",
        max_cycles=4096,
        label="WayLookup flush recovery coverage",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins(
    "BIN-720", "BIN-726", "BIN-727", "BIN-728", "BIN-735", "BIN-777",
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_waylookup_queue_read_dut(lowrisk_cleanup) -> None:
    """Build normal WayLookup entries, then exercise read-side backpressure."""
    env = lowrisk_cleanup
    _load_two_fetch_loop(env)
    _warm_two_fetch_execution(env)

    _wait_waylookup_occupancy(env, 2, max_cycles=4096)
    _wait_funcov_hits(
        env,
        (
            ("icache_waylookup_queue", "entry_fields"),
            ("icache_waylookup_read", "dual_entry_dequeue"),
        ),
        max_cycles=4096,
        label="WayLookup entry integrity and dual-entry dequeue coverage",
    )
    env.backend_model.set_can_accept(0)
    env.step(12)
    blocked = _waylookup_snapshot(env)
    env.backend_model.set_can_accept(1)
    # Refill updates can make the second queued entry temporarily unreadable,
    # which is the RTL's intended single-service fallback condition.
    _pulse_fencei(env)
    _set_predictors(env, False)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=24,
        miss_rate=1.0,
        seed=0x727,
    )
    _wait_funcov_hit(
        env,
        "icache_waylookup_read",
        "single_entry_fallback",
        max_cycles=4096,
        label="WayLookup single-entry fallback coverage",
    )
    _set_predictors(env, True)

    _wait_funcov_hits(
        env,
        (
            ("icache_waylookup_update", "update_head"),
            ("icache_waylookup_update", "update_second_entry_stall"),
            ("icache_prefetchpipe_s0_entry", "bpu_flush_miss_allows_hw"),
        ),
        max_cycles=1,
        label="WayLookup update and nonmatching BPU flush coverage",
    )

    assert blocked["num_valid"] is not None and int(blocked["num_valid"]) >= 1
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-733")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_waylookup_corrupt_update_dut(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    _run_asymmetric_refill(env, expected_pattern="hit_miss", evict_req=1)
    _wait_funcov_hit(
        env,
        "icache_waylookup_update",
        "update_corrupt_ignored",
        max_cycles=32,
        label="corrupt refill matching a queued WayLookup entry",
    )


@pytest.mark.funcov_bins("BIN-731")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_waylookup_same_way_new_tag_update_dut(lowrisk_cleanup) -> None:
    """Keep the old tag queued while a same-set replacement refill returns."""
    env = lowrisk_cleanup
    base = 0x8060_0000
    same_set_stride = 0x4000
    loop_targets = _load_same_set_jump_loop(
        env,
        base,
        stride=same_set_stride,
        line_count=4,
    )
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=48,
        miss_rate=1.0,
        seed=0x731,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    _set_predictors(env, True)
    env.backend_model.set_can_accept(1)

    # Fill the four ways with normal demand fetches.  Besides giving the SRAM
    # checker-valid instruction data, repeated execution trains all four JAL
    # targets without synthetic redirects.
    resident_by_waymask = _collect_target_refill_waymasks(
        env,
        loop_targets,
        minimum_commits=1024,
        max_cycles=16384,
    )
    assert len(resident_by_waymask) == 4, {
        "reason": "same-set demand fetches did not occupy four distinct ways",
        "resident_by_waymask": resident_by_waymask,
    }

    # Once the backend is blocked, predicted fetches keep running ahead and
    # retain SRAM-hit entries in WayLookup.  Wait until each possible victim
    # way has an old-tag entry, then allocate the fifth miss.
    env.backend_model.set_can_accept(0)
    for _ in range(4096):
        if all(
            _waylookup_contains_line(env, target, waymask)
            for waymask, target in resident_by_waymask.items()
        ):
            break
        env.step(1)
    else:
        raise AssertionError(
            {
                "reason": "four possible old victim entries did not queue",
                "resident_by_waymask": resident_by_waymask,
                "live_entries": _waylookup_live_entries(env),
                "waylookup": _waylookup_snapshot(env),
            }
        )
    live_entries = _waylookup_live_entries(env)
    env.logger.info(
        "BIN-731 queued old SRAM hits before replacement: entries=%s",
        live_entries,
    )

    # Only now allocate the fifth line as a soft prefetch and delay its clean
    # response.  Soft prefetch does not add another WayLookup entry.
    replacement = base + 4 * same_set_stride
    request_phase = int(env.current_cycle)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=512,
        miss_rate=1.0,
        seed=0x731,
    )
    for _ in range(64):
        _drive_soft_prefetch(env, [replacement])
        try:
            request = _wait_for_target_request(
                env,
                replacement,
                max_cycles=16,
                after_cycle=request_phase,
            )
            break
        except AssertionError:
            continue
    else:
        raise AssertionError(
            {
                "reason": "replacement soft prefetch did not reach MissUnit",
                "replacement": replacement,
            }
        )
    source = int(request["source"])
    env.step(1)
    replacement_way = _try_read_internal(
        env,
        (
            f"Frontend_top.Frontend.inner_icache.missUnit.allMshr_{source}.way",
            f"Frontend_top.Frontend.inner_icache.missUnit.allMshr_{source}."
            "__Vtogcov__io_info_bits_way",
        ),
    )
    assert replacement_way is not None, {
        "reason": "replacement MSHR way is not observable",
        "source": source,
    }

    replacement_waymask = 1 << int(replacement_way)
    assert replacement_waymask in resident_by_waymask, {
        "replacement_waymask": replacement_waymask,
        "resident_by_waymask": resident_by_waymask,
    }
    old_tag = resident_by_waymask[replacement_waymask]
    assert _waylookup_contains_line(env, old_tag, replacement_waymask), {
        "reason": "selected victim no longer has an old-tag WayLookup entry",
        "old_tag": old_tag,
        "replacement_waymask": replacement_waymask,
        "waylookup": _waylookup_snapshot(env),
    }
    env.logger.info(
        "BIN-731 victim mapping: replacement_way=%d replacement_waymask=0x%x old_tag=0x%x residents=%s",
        int(replacement_way),
        replacement_waymask,
        int(old_tag),
        {hex(int(k)): hex(int(v)) for k, v in resident_by_waymask.items()},
    )
    _wait_funcov_hit_checker_clean(
        env,
        "icache_waylookup_update",
        "update_same_way_new_tag",
        max_cycles=1024,
        label="same-way different-tag WayLookup refill update",
    )

    env.backend_model.set_can_accept(1)
    env.step(32)
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-734")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_waylookup_update_priority_over_write_dut(lowrisk_cleanup) -> None:
    """Update wins over a pending write, which resumes after the update."""
    env = lowrisk_cleanup
    base = 0x8070_0000
    _load_nops(env, base, words=32768)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=1.0,
        seed=0x734,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)
    _wait_funcov_hit(
        env,
        "icache_waylookup_update",
        "update_priority_over_pending_write",
        max_cycles=4096,
        label="WayLookup update priority and pending-write recovery",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-744")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_waylookup_flush_wins_write_dut(lowrisk_cleanup) -> None:
    """Flush immediately after an accepted write and discard the old entry."""
    env = lowrisk_cleanup
    base = 0x8080_0000
    _load_nops(env, base, words=32768)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=24,
        miss_rate=1.0,
        seed=0x744,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)
    _run_until(
        env,
        lambda: (
            _waylookup_value(env, "write0_valid") == 1
            and _waylookup_value(env, "write0_ready") == 1
        ),
        max_cycles=1024,
        label="accepted WayLookup write before redirect",
    )
    target = base + 0x40
    env.monitor.clear()
    env.monitor.set_expected_pc(target)
    env.backend_model.inject_redirect(target, "ctrl_redirect", delay_cycles=0)
    env.step(1)

    _wait_funcov_hit(
        env,
        "icache_waylookup_flush",
        "flush_wins_write",
        max_cycles=16,
        label="WayLookup redirect flush after accepted write",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins(
    "BIN-737", "BIN-738", "BIN-739", "BIN-1010", "BIN-740", "BIN-741", "BIN-762",
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_waylookup_exception_entry_dut(lowrisk_cleanup) -> None:
    """Drive a cacheable instruction page fault through the existing PTW model."""
    env = lowrisk_cleanup
    va = 0x8020_0F00
    pa = 0x8040_0F00
    payload = (_NOP.to_bytes(4, "little")) * 512
    scenario = TranslationScenario(
        scenario_id="waylookup-cacheable-instruction-page-fault",
        va=va,
        pa=pa,
        payload=payload,
        page_count=2,
        expected_path="fault",
        mode="sv39",
        s1_pf=1,
        expected_result="page_fault",
    )
    _initialize_cacheable_stream(env, pa, latency=24)
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["resp_line_count"]) >= 1,
        max_cycles=4096,
        label="cacheline refill before ITLB exception",
    )
    state = TranslationScenarioBuilder(env).build(scenario)

    env.monitor.clear()
    env.monitor.set_expected_pc(va)
    env.arm_translation_scenario(state, page_indexes=(0,))
    # Capture the exception through a port-0-only request first.  BIN-741 is
    # defined on the empty-queue, single-write edge, so a concurrent soft
    # prefetch here would make that condition structurally impossible.
    _set_predictors(env, False)
    env.backend_model.inject_redirect(va, "ctrl_redirect", delay_cycles=0)
    for _ in range(6000):
        if all(
            env.functional_coverage.key_hit("icache_waylookup_exception", name)
            for name in ("exception_capture", "exception_no_bypass")
        ):
            break
        env.step(1)
    _run_until(
        env,
        lambda: bool(env.translation_oracle.get_active())
        and bool(env.translation_oracle.get_active().get("fault_seen")),
        max_cycles=6000,
        label="cacheable instruction page fault",
    )
    _wait_funcov_hit(
        env,
        "icache_waylookup_exception",
        "exception_capture",
        max_cycles=256,
        label="WayLookup exception capture coverage",
    )
    _wait_funcov_hit(
        env,
        "icache_waylookup_exception",
        "exception_no_bypass",
        max_cycles=256,
        label="WayLookup exception write without empty-queue bypass",
    )
    # Re-enable the second hardware-prefetch producer while the captured
    # exception is still blocking WayLookup, then observe atomic dual-write
    # backpressure as a separate phase.
    _set_predictors(env, True)
    _drive_soft_prefetch(env, [va])
    _wait_funcov_hit(
        env,
        "icache_waylookup_exception",
        "exception_blocks_dual_write",
        max_cycles=512,
        label="WayLookup exception backpressure for a dual write",
    )
    env.assert_translation_scenario()

    # The blocked transaction remains dual until the exception is cleared.
    # Start a second fault episode with predictors disabled so PrefetchPipe
    # presents a port-0-only request behind the persistent exception entry.
    _pulse_fencei(env)
    _set_predictors(env, False)
    env.arm_translation_scenario(state, page_indexes=(0,))
    env.backend_model.inject_redirect(va, "ctrl_redirect", delay_cycles=0)
    _wait_funcov_hit(
        env,
        "icache_waylookup_exception",
        "exception_blocks_single_write",
        max_cycles=6000,
        label="WayLookup exception backpressure for a single write",
    )
    _set_predictors(env, True)
    _wait_funcov_hit(
        env,
        "icache_hit_path",
        "hit_itlb_exception",
        max_cycles=256,
        label="cache hit with ITLB exception coverage",
    )
    env.step(16)
    assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-763")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_hit_pmp_exception(lowrisk_cleanup) -> None:
    """Present a cacheable line with execute permission denied by the PMP model."""
    env = lowrisk_cleanup
    # NAPOT entries are encoded at page granularity by the existing PMP/PMA
    # support.  Keep the instruction offset in the page while aligning the
    # entry base to the required 4-KiB grain.
    va = 0x8021_0000
    pa = 0x8041_0000
    payload = (_NOP.to_bytes(4, "little")) * 512
    scenario = TranslationScenario(
        scenario_id="cacheable-hit-pmp-instruction-access-fault",
        va=va,
        pa=pa,
        payload=payload,
        page_count=2,
        mode="sv39",
        expected_path="fault",
        expected_result="access_fault",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=False),
                addr=pa,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(
                    match="napot",
                    read=True,
                    write=True,
                    execute=True,
                    cacheable=True,
                    atomic=True,
                ),
                addr=pa,
                size=0x1000,
            ),
        ),
    )
    _initialize_cacheable_stream(env, pa, latency=24)
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["resp_line_count"]) >= 1,
        max_cycles=4096,
        label="cacheline refill before PMP exception",
    )
    state = TranslationScenarioBuilder(env).build(scenario)

    env.monitor.clear()
    env.monitor.set_expected_pc(va)
    env.arm_translation_scenario(state, page_indexes=(0,))
    env.backend_model.inject_redirect(va, "ctrl_redirect", delay_cycles=0)
    _run_until(
        env,
        lambda: bool(env.translation_oracle.get_active())
        and bool(env.translation_oracle.get_active().get("fault_seen")),
        max_cycles=6000,
        label="cacheable hit with PMP execute fault",
    )
    _wait_funcov_hit(
        env,
        "icache_hit_path",
        "hit_pmp_exception",
        max_cycles=256,
        label="cache hit with PMP exception coverage",
    )
    env.step(16)
    env.assert_translation_scenario()
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins(
    "BIN-680", "BIN-753", "BIN-754", "BIN-756", "BIN-757", "BIN-758", "BIN-1011"
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.funcov_closure_pending
@pytest.mark.xfail(
    strict=True,
    reason=(
        "current V3 DUT does not build a 32-entry WayLookup backlog from "
        "top-level traffic; retain for nightly reachability checks"
    ),
)
def test_icache_lowrisk_waylookup_capacity_wrap_dut(lowrisk_cleanup) -> None:
    """Fill WayLookup with dual writes, then release one blocked transaction."""
    env = lowrisk_cleanup
    # Keep the consumer stopped while issuing distinct legal soft-prefetch
    # lines. This makes queue depth an observed consequence of PrefetchPipe
    # traffic instead of a direct write to WayLookup state.
    base = 0x8000_0000
    _load_nops(env, base, words=4096)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=1.0,
        seed=0x680,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1

    env.backend_model.set_can_accept(0)
    capacity = WayLookupCapacitySequence(env)
    max_cycles = int(os.getenv("TB_WAYLOOKUP_CAPACITY_MAX_CYCLES", "12000"))
    for index in range(max_cycles):
        address = base + 0x4000 + index * 0x40
        _drive_soft_prefetch(env, [address, address + 0x1000])
        snapshot = capacity.sample()
        if snapshot.full:
            break
    else:
        capacity.wait_full(max_cycles=1)
    _wait_funcov_hits(
        env,
        (
            ("icache_waylookup_capacity", "one_slot_blocks_dual"),
            ("icache_waylookup_capacity", "full_blocks_write"),
        ),
        max_cycles=256,
        label="WayLookup one-slot and full dual-write backpressure",
    )
    env.step(4)
    env.backend_model.set_can_accept(1)

    _wait_funcov_hits(
        env,
        (
            ("icache_prefetchpipe_s1_meta", "waylookup_backpressure_recovery"),
            ("icache_waylookup_capacity", "read_write_boundary"),
            ("icache_waylookup_wrap", "single_read_wrap"),
            ("icache_waylookup_wrap", "single_write_wrap"),
            ("icache_waylookup_wrap", "dual_wrap"),
        ),
        max_cycles=12000,
        label="WayLookup capacity recovery and pointer wrap coverage",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-759", "BIN-760")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_hit_path_sequences(lowrisk_cleanup) -> None:
    """Fill adjacent lines, then let the normal sequential stream exercise clean hits."""
    env = lowrisk_cleanup
    base = 0x800A_0000
    env.load_program(_trained_short_block_loop(), base)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=24,
        miss_rate=1.0,
        seed=0x6275,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["resp_line_count"]) >= 4,
        max_cycles=4096,
        label="two cacheline refills for hit-path sequence",
    )
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=24,
        miss_rate=0.0,
        seed=0x6275,
    )
    _wait_funcov_hits(
        env,
        (
            ("icache_hit_path", "continuous_same_line_sram_hit"),
            ("icache_hit_path", "continuous_cross_line_sram_hit"),
        ),
        max_cycles=4096,
        label="same-line and cross-line clean SRAM hit coverage",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-761")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_dual_independent_hit(lowrisk_cleanup) -> None:
    """Train the existing two-fetch loop with both requested lines resident."""
    env = lowrisk_cleanup
    base = 0x8000_0000
    env.load_program(_trained_short_block_loop(), base)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=24,
        miss_rate=1.0,
        seed=0x6276,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)
    _warm_two_fetch_execution(env)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=24,
        miss_rate=0.0,
        seed=0x6276,
    )
    _wait_funcov_hit(
        env,
        "icache_hit_path",
        "dual_request_independent_hit",
        max_cycles=4096,
        label="dual independent clean SRAM hit coverage",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-765", "BIN-767", "BIN-768")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_hitmiss_refill_sequence(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    base = 0x8004_0000
    _load_nops(env, base, words=32768)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=1.0,
        seed=0x6276,
    )
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 0
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    # Keep the first FTQ context live throughout the directed redirects.  This
    # prevents the synthetic redirect from selecting a committed FTQ context
    # while the ICache is deliberately stalled on refills.
    env.backend_model.set_can_accept(0)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)

    target = base + 0x100
    # A sequential NOP stream can produce unrelated refills while the redirect
    # is in flight, so align the monitor and wait for this target line itself.
    env.monitor.set_expected_pc(target)
    env.backend_model.inject_redirect(target, "ctrl_redirect", delay_cycles=0)
    _wait_for_target_response(env, target, max_cycles=4096, label="clean demand cache refill")
    env.monitor.clear()
    env.monitor.set_expected_pc(target)
    # The first refill is intentionally forced to miss.  Subsequent soft
    # prefetches must see the refilled line as an SRAM hit.
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=0.0,
        seed=0x6276,
    )
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1
    for _ in range(32):
        _drive_soft_prefetch(env, [target])
        if env.functional_coverage.key_hit("icache_miss_path", "fetch_refill_prefetch_hit"):
            break
    _wait_funcov_hit(
        env,
        "icache_miss_path",
        "fetch_refill_prefetch_hit",
        max_cycles=4096,
        label="prefetch SRAM hit after clean fetch refill",
    )
    _pulse_fencei(env)
    # Do not reuse the pre-fence base line.  Drive each fresh same-set tag through
    # the demand redirect path: a one-cycle soft-prefetch pulse can be consumed
    # by a stale post-fence lookup without ever reaching MissUnit.  The fifth
    # demand tag is then the replacement candidate.
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=1.0,
        seed=0x6276,
    )
    same_set_stride = 0x4000
    for index in range(1, 5):
        fill_target = base + index * same_set_stride
        request_cycle = int(env.current_cycle)
        env.monitor.clear()
        env.monitor.set_expected_pc(fill_target)
        env.backend_model.inject_redirect(
            fill_target,
            "ctrl_redirect",
            delay_cycles=0,
        )
        _wait_for_target_response(
            env,
            fill_target,
            max_cycles=4096,
            label=f"same-set refill {index}",
            after_cycle=request_cycle,
        )
        # The TileLink response precedes MissUnit's MetaArray write.  Do not
        # launch the next same-set lookup until that write is observable.
        env.step(8)
    victim_target = base + 5 * same_set_stride
    victim_request_cycle = int(env.current_cycle)
    env.monitor.clear()
    env.monitor.set_expected_pc(victim_target)
    env.backend_model.inject_redirect(victim_target, "ctrl_redirect", delay_cycles=0)
    _wait_funcov_hit(
        env,
        "icache_miss_path",
        "plru_victim_on_miss",
        max_cycles=4096,
        label="full-set PLRU victim miss coverage",
    )
    _wait_for_target_response(
        env,
        victim_target,
        max_cycles=4096,
        label="PLRU victim demand refill",
        after_cycle=victim_request_cycle,
    )

    # Train a finite loop before invalidating the cache.  After fence.i the
    # trained predictions revisit each block without another redirect, giving
    # the sampler an uninterrupted fetch-refill -> MSHR-release -> SRAM-hit
    # sequence for the same line.
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 0
    loop_base = base + 0x1_8000
    env.load_program(_trained_short_block_loop(), loop_base)
    env.backend_model.set_can_accept(1)
    env.monitor.clear()
    env.monitor.set_expected_pc(loop_base)
    env.backend_model.inject_redirect(loop_base, "ctrl_redirect", delay_cycles=0)
    _warm_two_fetch_execution(env)
    trained_commit_count = int(env.backend_model.get_stats().get("commit_count", 0))
    _run_until(
        env,
        lambda: int(env.backend_model.get_stats().get("commit_count", 0))
        >= trained_commit_count + 256,
        max_cycles=4096,
        label="stable trained short-block loop",
    )
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=1.0,
        seed=0x6276,
    )
    _pulse_fencei(env)
    _wait_funcov_hit(
        env,
        "icache_miss_path",
        "refill_then_fetch_hit",
        max_cycles=4096,
        label="demand SRAM hit after refill and MSHR release",
    )
    assert int(env.icache_agent.get_stats()["resp_line_count"]) >= 1
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-1139")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_mainpipe_mixed_sram_mshr_sources_dut(lowrisk_cleanup) -> None:
    """Fetch one cross-line transaction from SRAM (line 0) and MSHR (line 1).

    The soft prefetch is deliberately used only to make line 0 resident.  It
    does not enter WayLookup in the V3 RTL.  The subsequent demand redirect is
    therefore the first transaction that can observe line 0 as a clean SRAM
    hit while issuing the independent line-1 miss.
    """
    env = lowrisk_cleanup
    idle_base = 0x8000_0000
    target = 0x80A0_0000
    _load_idle_loop(env, idle_base)
    payload = bytearray((0x0001).to_bytes(2, "little") * 128)
    # A 32-bit NOP beginning at byte 62 forces the fetch block at +0x20 to
    # consume bytes from both cachelines.
    payload[62:66] = _NOP.to_bytes(4, "little")
    env.load_program(bytes(payload), target)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=48,
        miss_rate=1.0,
        seed=0x1139,
    )
    env.initialize(reset_vector=idle_base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(idle_base)
    _set_predictors(env, False)
    env.backend_model.set_can_accept(0)
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1
    # Every ICache data-bank SRAM clears one set per cycle after reset.  A
    # refill accepted during that 256-cycle sweep can update metadata while
    # its data write is superseded by the SRAM's reset write.  Keep this
    # directed warmup outside that initialization window.
    env.step(300)
    phase_cycle = int(env.current_cycle)
    for _ in range(64):
        _drive_soft_prefetch(env, [target])
        try:
            _wait_for_target_request(
                env,
                target,
                max_cycles=16,
                after_cycle=phase_cycle,
            )
            break
        except AssertionError:
            continue
    else:
        raise AssertionError("line-0 warmup prefetch did not reach MissUnit")
    _wait_for_target_response(
        env,
        target,
        max_cycles=4096,
        label="clean soft-prefetch line-0 refill",
        after_cycle=phase_cycle,
    )
    resident_way = env.icache_ecc_agent.wait_resident(target, max_cycles=4096)
    assert 0 <= int(resident_way) < 4
    _wait_mshr_line_released(env, target, max_cycles=64)
    prefetch_done_cycle = int(env.current_cycle)
    # The soft-prefetch path requests only line 0 and is not allowed to
    # speculate line 1.  This makes the later MSHR source unambiguous.
    _assert_no_target_request(
        env,
        target + 0x40,
        after_cycle=phase_cycle,
        label="soft-prefetch line-0 warmup",
    )
    env.step(8)
    assert env.icache_ecc_agent.read_resident_line(target) == bytes(payload[:64]), {
        "reason": "soft-prefetch refill did not persist the expected line-0 data",
        "resident_way": int(resident_way),
    }

    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=96,
        miss_rate=1.0,
        seed=0x1139,
    )
    fetch_pc = target + 0x20
    env.monitor.clear()
    env.monitor.set_expected_pc(fetch_pc)
    env.backend_model.set_can_accept(1)
    env.backend_model.inject_redirect(fetch_pc, "ctrl_redirect", delay_cycles=0)
    _wait_funcov_hit_checker_clean(
        env,
        "icache_mainpipe_maybe_rvc_align",
        "mixed_source_merge",
        max_cycles=4096,
        label="cross-line request with SRAM line 0 and MSHR line 1",
    )
    demand_line1 = _wait_for_target_request(
        env,
        target + 0x40,
        max_cycles=1,
        after_cycle=prefetch_done_cycle,
    )
    assert int(demand_line1["address"]) == target + 0x40
    evidence = _mixed_source_hit_evidence(env)
    assert any(evidence["sram_source_lines"][:2])
    assert any(evidence["mshr_source_lines"][:2])
    env.backend_model.set_can_accept(1)
    env.step(32)
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-759")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.xfail(
    strict=True,
    reason=(
        "current V3 standalone DUT does not expose a checker-eligible repeated "
        "same-line SRAM-hit sequence in this fixture"
    ),
)
def test_icache_cacheable_same_line_sram_hit(lowrisk_cleanup) -> None:
    """Refetch a completed line so the hit-path sampler sees a real SRAM hit."""
    env = lowrisk_cleanup
    base = 0x8004_0000
    _load_nops(env, base, words=256)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=1.0,
        seed=0x6277,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)
    initial_resp_line_count = int(env.icache_agent.get_stats()["resp_line_count"])

    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["resp_line_count"])
        > initial_resp_line_count,
        max_cycles=2048,
        label="initial cache line refill",
    )
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=0.0,
        seed=0x6278,
    )
    env.backend_model.inject_redirect(base, "ctrl_redirect", delay_cycles=0)
    _wait_funcov_hit(
        env,
        "icache_hit_path",
        "continuous_same_line_sram_hit",
        max_cycles=2048,
        label="cacheable same-line SRAM hit after redirect",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-812", "BIN-816")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.xfail(
    strict=True,
    reason=(
        "current V3 standalone DUT/backend scheduler does not provide a stable "
        "non-stale FTQ context for repeated cacheable redirect attempts"
    ),
)
def test_ifu_cacheable_backend_redirect_blocks_pending_response(lowrisk_cleanup) -> None:
    """Align a real backend redirect with an in-flight ICache-to-IFU response."""
    env = lowrisk_cleanup
    base = 0x8004_0000
    _load_nops(env, base, words=512)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=1.0,
        seed=0x6278,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)

    for attempt in range(48):
        for _ in range(4096):
            if env.functional_coverage.key_hit(
                "ifu_cacheable_flush", "backend_redirect_blocks"
            ):
                assert env.functional_coverage.key_hit(
                    "ifu_cacheable_flush", "flush_wins_fire"
                )
                assert not env.monitor.get_errors()
                return
            req_valid = _read_first_signal(env, _IFU_CACHEABLE_REQ_VALID)
            if req_valid == 1:
                env.backend_model.inject_redirect(
                    base + 0x100 + ((attempt & 0x1F) * 0x40),
                    "ctrl_redirect",
                    delay_cycles=0,
                )
                break
            env.step(1)
        else:
            raise AssertionError(
                {
                    "reason": "no ICache-to-IFU response window observed",
                    "attempt": attempt,
                    "current_cycle": int(env.current_cycle),
                    "icache": env.icache_agent.get_stats(),
                }
            )
        for _ in range(64):
            if env.functional_coverage.key_hit(
                "ifu_cacheable_flush", "backend_redirect_blocks"
            ):
                assert env.functional_coverage.key_hit(
                    "ifu_cacheable_flush", "flush_wins_fire"
                )
                assert not env.monitor.get_errors()
                return
            env.step(1)

    raise AssertionError(
        {
            "reason": "backend redirect did not align with IFU cacheable flush window",
            "attempts": 48,
            "current_cycle": int(env.current_cycle),
            "icache": env.icache_agent.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )
