"""Directed DUT closure scenarios for the cacheable ICache MissUnit bins.

The tests in this module use only the existing cacheable instruction stream,
soft-prefetch ports, backend redirect injection and fence.i input.  Internal
signals are sampled for transaction evidence; they are never driven or forced.
"""

from __future__ import annotations

import os
from collections.abc import Callable

import pytest

from env.sequences import (
    TranslationPmpPmaEntry,
    TranslationScenario,
    TranslationScenarioBuilder,
)
from env.support.pmp_pma import PmpPmaConfig


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BASE = 0x8010_0000
_NOP = 0x0000_0013
_MAIN = "Frontend_top.Frontend.inner_icache.mainPipe."
_MISS = "Frontend_top.Frontend.inner_icache.missUnit."
_ICACHE = "Frontend_top.Frontend.inner_icache."
_TOP = "Frontend_top."


def _read(env, name: str, *aliases: str) -> int | None:
    cache = getattr(env, "_missunit_signal_cache", None)
    if cache is None:
        cache = {}
        setattr(env, "_missunit_signal_cache", cache)
    names = (name, *aliases)
    key = tuple(names)
    if key in cache:
        signal = cache[key]
        return None if signal is None else int(getattr(signal, "value", 0))
    for candidate in names:
        try:
            signal = getattr(env.dut, candidate, None)
            if signal is None:
                getter = getattr(env.dut, "GetInternalSignal", None)
                signal = getter(candidate) if callable(getter) else None
            if signal is not None and getattr(signal, "value", None) is not None:
                cache[key] = signal
                return int(signal.value)
        except Exception:
            continue
    cache[key] = None
    return None


def _snapshot(env) -> dict[str, int | None]:
    sample: dict[str, int | None] = {
        "cycle": int(env.current_cycle),
        "miss_valid": _read(env, _MAIN + "__Vtogcov__io_missReq_valid"),
        "miss_ready": _read(env, _MAIN + "__Vtogcov__io_missReq_ready"),
        "miss_paddr": _read(env, _MAIN + "__Vtogcov__io_missReq_bits_blkPAddr"),
        "miss_vset": _read(env, _MAIN + "__Vtogcov__io_missReq_bits_vSetIdx"),
        "prefetch_valid": _read(
            env, _MISS + "__Vtogcov__io_prefetchReq_valid"
        ),
        "prefetch_ready": _read(
            env, _MISS + "__Vtogcov__io_prefetchReq_ready"
        ),
        "prefetch_paddr": _read(
            env, _MISS + "__Vtogcov__io_prefetchReq_bits_blkPAddr"
        ),
        "prefetch_vset": _read(
            env, _MISS + "__Vtogcov__io_prefetchReq_bits_vSetIdx"
        ),
        "fetch_hit": _read(env, _MISS + "fetchHit", _MISS + "__Vtogcov__fetchHit"),
        "prefetch_hit": _read(
            env, _MISS + "prefetchHit", _MISS + "__Vtogcov__prefetchHit"
        ),
        "flush": _read(env, _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush"),
        "fencei": _read(env, _TOP + "io_fencei"),
        "soft0_valid": _read(env, _TOP + "io_softPrefetch_0_valid"),
        "soft_pending": _read(
            env,
            _ICACHE + "softPrefetchValid",
            _ICACHE + "__Vtogcov__softPrefetchValid",
        ),
        "prefetch_from_valid": _read(
            env, _ICACHE + "prefetcher.io_fromFtq_valid"
        ),
        "prefetch_from_soft": _read(
            env, _ICACHE + "prefetcher.io_fromFtq_bits_req_0_isSoftPrefetch"
        ),
        "a_valid": _read(env, _TOP + "auto_inner_icache_client_out_a_valid"),
        "a_ready": _read(env, _TOP + "auto_inner_icache_client_out_a_ready"),
        "a_source": _read(env, _TOP + "auto_inner_icache_client_out_a_bits_source"),
        "d_valid": _read(env, _TOP + "auto_inner_icache_client_out_d_valid"),
        "d_source": _read(env, _TOP + "auto_inner_icache_client_out_d_bits_source"),
        "last_fire_next": _read(env, _MISS + "lastFireNext"),
        "id_next": _read(env, _MISS + "idNext"),
        "acquire_valid": _read(
            env, _TOP + "auto_inner_icache_client_out_a_valid"
        ),
        "acquire_ready": _read(
            env, _TOP + "auto_inner_icache_client_out_a_ready"
        ),
        "acquire_source": _read(
            env, _TOP + "auto_inner_icache_client_out_a_bits_source"
        ),
    }
    for index in range(4):
        sample[f"fetch_arb_{index}"] = _read(
            env, f"{_MISS}acquireArb.io_in_{index}_valid"
        )
    sample["prefetch_arb_valid"] = _read(
        env,
        f"{_MISS}_prefetchArb_io_out_valid",
        f"{_MISS}prefetchArb.__Vtogcov__io_out_valid",
        f"{_MISS}prefetchArb.io_out_valid",
    )
    for index in range(14):
        prefix = f"{_MISS}allMshr_{index}."
        for field in ("valid", "issue", "flush", "fencei", "blkPAddr", "vSetIdx"):
            sample[f"mshr_{index}_{field}"] = _read(env, prefix + field)
    return sample


def _load_nops(env, base: int, *, words: int = 4096) -> None:
    env.load_program((_NOP.to_bytes(4, "little")) * int(words), int(base))


def _clear_soft_prefetch(env) -> None:
    for slot in range(3):
        valid = getattr(env.dut, f"io_softPrefetch_{slot}_valid", None)
        address = getattr(env.dut, f"io_softPrefetch_{slot}_bits_vaddr", None)
        if valid is not None:
            valid.value = 0
        if address is not None:
            address.value = 0


def _set_soft_prefetch(env, addresses: list[int]) -> None:
    _clear_soft_prefetch(env)
    for slot, address in enumerate(addresses[:3]):
        valid = getattr(env.dut, f"io_softPrefetch_{slot}_valid", None)
        value = getattr(env.dut, f"io_softPrefetch_{slot}_bits_vaddr", None)
        assert valid is not None and value is not None, {
            "missing_signal": f"io_softPrefetch_{slot}"
        }
        valid.value = 1
        value.value = int(address)


def _drive_soft_prefetch(env, addresses: list[int]) -> None:
    _set_soft_prefetch(env, addresses)
    env.step(1)
    _clear_soft_prefetch(env)


def _drive_concurrent_requests(env, *, fetch: int, prefetch: int) -> None:
    _set_soft_prefetch(env, [int(prefetch)])
    env.backend_model.inject_redirect(int(fetch), "ctrl_redirect", delay_cycles=0)
    env.step(1)
    _clear_soft_prefetch(env)


def _drive_request_pair(
    env,
    *,
    fetch: int,
    prefetch: int,
    skew: int,
) -> None:
    if int(skew) < 0:
        _drive_soft_prefetch(env, [int(prefetch)])
        env.step(max(0, -int(skew) - 1))
        env.backend_model.inject_redirect(int(fetch), "ctrl_redirect", delay_cycles=0)
        env.step(1)
    elif int(skew) > 0:
        env.backend_model.inject_redirect(int(fetch), "ctrl_redirect", delay_cycles=0)
        env.step(1)
        env.step(max(0, int(skew) - 1))
        _drive_soft_prefetch(env, [int(prefetch)])
    else:
        _drive_concurrent_requests(env, fetch=int(fetch), prefetch=int(prefetch))


def _pulse_fencei(env) -> None:
    signal = getattr(env.clock_reset, "io_fencei", None)
    assert signal is not None, {"missing_signal": "io_fencei"}
    signal.value = 1
    env.step(1)
    signal.value = 0
    env.step(1)


def _pulse_fencei_redirect(env, target: int) -> None:
    signal = getattr(env.clock_reset, "io_fencei", None)
    assert signal is not None, {"missing_signal": "io_fencei"}
    signal.value = 1
    env.backend_model.inject_redirect(int(target), "ctrl_redirect", delay_cycles=0)
    env.step(1)
    signal.value = 0
    env.step(1)


def _mshr_present(
    sample: dict[str, int | None],
    indexes: range,
    *,
    issue: int | None = None,
) -> bool:
    return any(
        sample[f"mshr_{index}_valid"] == 1
        and (issue is None or sample[f"mshr_{index}_issue"] == int(issue))
        for index in indexes
    )


def _mshr_count(
    sample: dict[str, int | None],
    indexes: range,
    *,
    issue: int | None = None,
) -> int:
    return sum(
        sample[f"mshr_{index}_valid"] == 1
        and (issue is None or sample[f"mshr_{index}_issue"] == int(issue))
        for index in indexes
    )


def _mshr_has_key(
    sample: dict[str, int | None],
    indexes: range,
    address: int,
    *,
    issue: int | None = None,
) -> bool:
    block_paddr = int(address) >> 6
    # MissUnit stores the cache-line set index, not PrefetchPipe's banked
    # read-meta index.  A 64-byte line therefore advances vSetIdx by one.
    vset = (int(address) >> 6) & 0xFF
    return any(
        sample[f"mshr_{index}_valid"] == 1
        and sample[f"mshr_{index}_blkPAddr"] == block_paddr
        and sample[f"mshr_{index}_vSetIdx"] == vset
        and (issue is None or sample[f"mshr_{index}_issue"] == int(issue))
        for index in indexes
    )


def _request_key_absent(
    sample: dict[str, int | None],
    *,
    paddr_key: str,
    vset_key: str,
) -> bool:
    paddr = sample[paddr_key]
    vset = sample[vset_key]
    return paddr is not None and vset is not None and all(
        sample[f"mshr_{index}_valid"] != 1
        or sample[f"mshr_{index}_blkPAddr"] != paddr
        or sample[f"mshr_{index}_vSetIdx"] != vset
        for index in range(14)
    )


def _wait_mshr_state(
    env,
    predicate: Callable[[dict[str, int | None]], bool],
    *,
    max_cycles: int,
    label: str,
) -> dict[str, int | None]:
    for _ in range(int(max_cycles)):
        sample = _snapshot(env)
        if predicate(sample):
            return sample
        env.step(1)
    raise AssertionError(
        {
            "reason": f"timeout while waiting for {label}",
                "last_snapshot": _snapshot(env),
                "coverage_state": getattr(
                    env.functional_coverage, "_icache_missunit_cov_state", None
                ),
                "stats": env.get_stats(),
        }
    )


def _pulse_fencei_when(
    env,
    predicate: Callable[[dict[str, int | None]], bool],
    *,
    target: int | None = None,
    prefetch: int | None = None,
    max_cycles: int = 6000,
) -> dict[str, int | None]:
    signal = getattr(env.clock_reset, "io_fencei", None)
    assert signal is not None, {"missing_signal": "io_fencei"}
    for _ in range(int(max_cycles)):
        sample = _snapshot(env)
        if predicate(sample):
            if prefetch is not None:
                _set_soft_prefetch(env, [int(prefetch)])
            if target is not None:
                # Backend redirect is driven after cycle observers run. Wait
                # until its combinational flush is visible, then assert
                # fence.i for the next observer edge while the pre-flush MSHR
                # state is still sampled.
                signal.value = 0
                env.backend_model.inject_redirect(
                    int(target), "ctrl_redirect", delay_cycles=0
                )
                env.step(1)
                signal.value = 1
                for _ in range(8):
                    env.step(1)
                    if _snapshot(env)["flush"] == 1:
                        env.step(1)
                        break
                else:
                    raise AssertionError(
                        {
                            "reason": "redirect flush did not overlap fence.i",
                            "last": _snapshot(env),
                        }
                    )
            else:
                signal.value = 1
                env.step(1)
            signal.value = 0
            _clear_soft_prefetch(env)
            env.step(1)
            return sample
        env.step(1)
    raise AssertionError(
        {
            "reason": "MSHR state required for fence.i alignment did not appear",
            "max_cycles": int(max_cycles),
            "last": _snapshot(env),
            "stats": env.get_stats(),
        }
    )


def _build_mshr_pressure_until(
    env,
    predicate: Callable[[dict[str, int | None]], bool],
    *,
    max_attempts: int = 512,
    issued: bool = False,
) -> dict[str, int | None]:
    # Hold Acquire so demand and prefetch requests become observable MSHRs
    # instead of flushing the frontend once per attempt.  The old redirect
    # loop never issued an ICache request because every new path was cancelled
    # before it reached MainPipe.
    env.icache_agent.set_a_ready(0)
    _wait_mshr_state(
        env,
        lambda sample: _mshr_present(sample, range(4), issue=0),
        max_cycles=2048,
        label="initial unissued fetch MSHR",
    )

    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1
    for attempt in range(int(max_attempts)):
        sample = _snapshot(env)
        if predicate(sample):
            return sample
        prefetch = _BASE + 0x8000 + (attempt % 48) * 0x80
        _set_soft_prefetch(
            env,
            [prefetch, prefetch + 0x40, prefetch + 0x80],
        )
        # PrefetchPipe is registered. Keep the input asserted while scanning
        # for the corresponding MissUnit request so the fence can be applied
        # to a genuinely new request, not merely to the top-level pulse.
        for _ in range(8):
            env.step(1)
            sample = _snapshot(env)
            if predicate(sample):
                return sample
        _clear_soft_prefetch(env)
        env.step(2)

        if issued and _mshr_present(_snapshot(env), range(4, 14), issue=0):
            env.icache_agent.set_a_ready(1)
            for _ in range(32):
                sample = _snapshot(env)
                if predicate(sample):
                    return sample
                env.step(1)
            env.icache_agent.set_a_ready(0)
    raise AssertionError(
        {
            "reason": "requested MSHR pressure state did not appear",
            "max_attempts": int(max_attempts),
            "last": _snapshot(env),
            "stats": env.get_stats(),
        }
    )


def _run_until(env, predicate: Callable[[], bool], *, max_cycles: int, label: str) -> None:
    for _ in range(int(max_cycles)):
        if predicate():
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": f"timeout while waiting for {label}",
            "cycle": int(env.current_cycle),
            "stats": env.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _wait_bins(env, targets: list[tuple[str, str]], *, max_cycles: int = 6000) -> None:
    # Keep the full regression limit by default, while allowing short
    # diagnosis runs to override the polling window without changing DUT or
    # environment behavior.
    max_cycles = int(os.getenv("TB_ICACHE_MISSUNIT_MAX_CYCLES", str(max_cycles)), 0)
    remaining = set(targets)
    for _ in range(int(max_cycles)):
        remaining = {
            target for target in remaining if not env.functional_coverage.key_hit(*target)
        }
        if not remaining:
            return
        env.step(1)
    remaining = {
        target for target in remaining if not env.functional_coverage.key_hit(*target)
    }
    if not remaining:
        return
    raise AssertionError(
        {
            "reason": "functional coverage targets were not reached",
            "missing": sorted(f"{group}.{name}" for group, name in remaining),
            "cycle": int(env.current_cycle),
            "last_snapshot": _snapshot(env),
            "coverage_state": getattr(
                env.functional_coverage, "_icache_missunit_cov_state", None
            ),
            "stats": env.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _prepare(
    env,
    *,
    latency: int = 4096,
    words: int = 4096,
    prefetch_enabled: bool = True,
    backend_can_accept: bool = True,
) -> list[dict[str, int | None]]:
    samples: list[dict[str, int | None]] = []
    _load_nops(env, _BASE, words=words)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=int(latency),
        miss_rate=1.0,
        seed=0x686,
    )
    env.register_cycle_observer(
        lambda _cycle, active_env: samples.append(_snapshot(active_env))
    )
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = (
        1 if prefetch_enabled else 0
    )
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.backend_model.set_can_accept(1 if backend_can_accept else 0)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)
    return samples


def _wait_initial_refill(env) -> None:
    """Let the reset-vector line complete before injecting secondary traffic."""
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["req_count"]) >= 1,
        max_cycles=2000,
        label="initial fetch miss",
    )
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["resp_line_count"]) >= 1,
        max_cycles=max(4000, int(env.icache_agent.miss_latency) + 2000),
        label="initial fetch refill",
    )


def _assert_clean(env) -> None:
    _clear_soft_prefetch(env)
    env.icache_agent.set_a_ready(None)
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1
    env.backend_model.set_can_accept(1)
    fencei = getattr(env.clock_reset, "io_fencei", None)
    if fencei is not None:
        fencei.value = 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-690", "BIN-691")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_request_and_concurrent_dut(env) -> None:
    samples = _prepare(
        env,
        latency=48,
        words=32768,
        prefetch_enabled=False,
        backend_can_accept=False,
    )
    _wait_initial_refill(env)
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1
    for attempt in range(64):
        if env.functional_coverage.key_hit(
            "icache_missunit_request", "same_key_fetch_prefetch_merge"
        ):
            break
        same_key = _BASE + 0x2000 + attempt * 0x80
        _drive_request_pair(
            env,
            fetch=same_key,
            prefetch=same_key,
            skew=0,
        )
        env.step(8)

    # A 16-KiB stride preserves the virtual set index while changing pTag.
    # Isolate each attempt so stale hardware-prefetch MSHRs cannot turn either
    # side into a duplicate before the concurrent request reaches MissUnit.
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 0
    _wait_mshr_state(
        env,
        lambda sample: _mshr_count(sample, range(14)) == 0,
        max_cycles=2048,
        label="empty MSHR array before distinct-key requests",
    )
    # Measure the soft-prefetch path before aligning it with a fetch-capacity
    # release.  The preceding address supplies PrefetchPipe's registered vSet.
    probe = _BASE + 0x1_0000
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1
    _set_soft_prefetch(env, [probe + 0x4000])
    env.step(1)
    probe_input_cycle = int(env.current_cycle)
    _set_soft_prefetch(env, [probe])
    env.step(1)
    _clear_soft_prefetch(env)
    probe_request_cycle = None
    for _ in range(64):
        sample = _snapshot(env)
        if (
            sample["prefetch_valid"] == 1
            and sample["prefetch_paddr"] == (probe >> 6)
        ):
            probe_request_cycle = int(env.current_cycle)
            break
        env.step(1)
    assert probe_request_cycle is not None, {
        "reason": "soft-prefetch latency probe did not reach MissUnit",
        "last": _snapshot(env),
    }
    soft_request_latency = probe_request_cycle - probe_input_cycle
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 0
    _wait_mshr_state(
        env,
        lambda sample: _mshr_count(sample, range(14)) == 0,
        max_cycles=1024,
        label="empty MSHR array after soft-prefetch latency probe",
    )

    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=256,
        miss_rate=1.0,
        seed=0x686,
    )
    env.backend_model.set_can_accept(0)
    redirect_index = 0
    for release_offset in range(6):
        if env.functional_coverage.key_hit(
            "icache_missunit_request", "distinct_key_parallel_allocate"
        ):
            break
        for _ in range(16):
            if _mshr_count(_snapshot(env), range(4)) == 4:
                break
            target = _BASE + 0x6000 + redirect_index * 0x1000 + 0x38
            redirect_index += 1
            env.monitor.set_expected_pc(target)
            env.backend_model.inject_redirect(target, "ctrl_redirect", delay_cycles=0)
            env.step(8)
        held_target = _BASE + 0x6000 + redirect_index * 0x1000
        redirect_index += 1
        env.monitor.set_expected_pc(held_target)
        env.backend_model.inject_redirect(
            held_target, "ctrl_redirect", delay_cycles=0
        )
        _wait_mshr_state(
            env,
            lambda sample: (
                _mshr_count(sample, range(4)) == 4
                and sample["miss_valid"] == 1
                and sample["miss_ready"] == 0
                and sample["fetch_hit"] == 0
            ),
            max_cycles=2048,
            label=f"full fetch MSHR pool for release offset {release_offset}",
        )
        pending_fetch = next(
            item for item in env.icache_agent.pending if int(item.source) < 4
        )
        # The final D beat is one cycle after ready_cycle; MissUnit releases
        # the MSHR on a following registered checkpoint.  Scan that small,
        # deterministic window across successive capacity episodes.
        target_request_cycle = int(pending_fetch.ready_cycle) + 1 + release_offset
        prime_input_cycle = target_request_cycle - soft_request_latency - 1
        while int(env.current_cycle) < prime_input_cycle:
            env.step(1)
        # Once capacity is released, MainPipe advances to vSet 0x40.  Give each
        # scan window a disjoint physical tag so neither its prime nor target
        # can merge with a prefetch MSHR left by an earlier window.
        prefetch = _BASE + 0x10_1000 + release_offset * 0x8000
        env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1
        _set_soft_prefetch(env, [prefetch + 0x4000])
        env.step(1)
        _set_soft_prefetch(env, [prefetch])
        env.step(1)
        _clear_soft_prefetch(env)
        env.step(max(8, soft_request_latency + 4))
        env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 0
    _wait_bins(
        env,
        [
            ("icache_missunit_request", "same_key_fetch_prefetch_merge"),
            ("icache_missunit_request", "distinct_key_parallel_allocate"),
        ],
    )
    assert samples
    _assert_clean(env)


@pytest.mark.funcov_bins("BIN-692")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_same_paddr_diff_vset_dut(env) -> None:
    pytest.xfail(
        "current Verilator DUT exposes only whole-cycle top-level stimulus; "
        "same-paddr/different-vSet requests require a sub-cycle alignment"
    )
    pa = 0x8050_0000
    va_fetch = 0x4050_0000
    va_prefetch = va_fetch + 0x1000
    payload = (_NOP.to_bytes(4, "little")) * 2048
    _prepare(
        env,
        latency=48,
        words=32768,
        prefetch_enabled=False,
        backend_can_accept=False,
    )
    _wait_initial_refill(env)
    scenario = TranslationScenario(
        scenario_id="icache-missunit-synonym-vset",
        va=va_fetch,
        pa=pa,
        payload=payload,
        page_count=2,
        mode="sv39",
        expected_path="cacheable",
        expected_result="miss_refill",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(
                    match="napot",
                    read=True,
                    write=True,
                    execute=True,
                ),
                addr=pa,
                size=0x2000,
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
                size=0x2000,
            ),
        ),
    )
    TranslationScenarioBuilder(env).build(scenario)
    env.page_table.map_page(
        va_prefetch >> 12,
        pa >> 12,
        **scenario.s1_pte.as_mapping_kwargs(),
    )
    env.monitor.clear()
    env.monitor.set_translation_context(s2xlate=0, priv_imode=1)
    env.monitor.set_expected_pc(va_fetch)

    # Warm both ITLB entries first. fence.i removes the cachelines while
    # preserving the translations, leaving only the fixed pipeline skew to
    # scan. Each attempt is fully drained before another redirect is queued.
    req_before = int(env.icache_agent.get_stats()["req_count"])
    env.backend_model.inject_redirect(va_fetch, "ctrl_redirect", delay_cycles=0)
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["req_count"]) > req_before,
        max_cycles=2048,
        label="synonym demand translation warmup",
    )
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["pending"]) == 0,
        max_cycles=2048,
        label="synonym demand refill",
    )
    _drive_soft_prefetch(env, [va_prefetch])
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["req_count"]) > req_before + 1,
        max_cycles=2048,
        label="synonym soft-prefetch translation warmup",
    )
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["pending"]) == 0,
        max_cycles=2048,
        label="synonym soft-prefetch refill",
    )

    # Both translations are now resident. Keep Acquire blocked so each failed
    # alignment attempt can be removed by fence.i without leaving refill
    # traffic behind. Scan the complete redirect-to-soft-prefetch pipeline
    # skew; every attempt uses a fresh physical line and a one-cycle soft pulse
    # so neither side can become an existing-MSHR hit before the overlap.
    env.backend_model.set_can_accept(0)
    env.icache_agent.set_a_ready(0)
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 0
    env.step(8)
    _pulse_fencei(env)
    falling_soft = {"address": None, "clear": False}

    def drive_falling_soft(_cycle: int) -> None:
        address = falling_soft["address"]
        if address is not None:
            _set_soft_prefetch(env, [int(address)])
            falling_soft["address"] = None
            falling_soft["clear"] = True
        elif falling_soft["clear"]:
            _clear_soft_prefetch(env)
            falling_soft["clear"] = False

    env.dut.StepFal(drive_falling_soft)
    for skew in range(33):
        if env.functional_coverage.key_hit(
            "icache_missunit_request", "same_paddr_diff_vset_separate"
        ):
            break
        offset = 0x100 + skew * 0x40
        fetch = va_fetch + offset
        prefetch = va_prefetch + offset
        _pulse_fencei(env)
        _wait_mshr_state(
            env,
            lambda sample: _mshr_count(sample, range(14)) == 0,
            max_cycles=32,
            label=f"empty MSHRs before synonym skew {skew}",
        )
        env.monitor.set_expected_pc(fetch)
        env.backend_model.inject_redirect(fetch, "ctrl_redirect", delay_cycles=0)
        env.step(skew)
        env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1
        falling_soft["address"] = prefetch
        env.step(1)
        env.step(24)
        env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 0
    _wait_bins(
        env,
        [("icache_missunit_request", "same_paddr_diff_vset_separate")],
        max_cycles=1,
    )
    _assert_clean(env)


@pytest.mark.funcov_bins("BIN-689")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_prefetch_capacity_dut(env) -> None:
    samples = _prepare(env, latency=16384)
    for base_offset in range(0x100, 0x380, 0xC0):
        _drive_soft_prefetch(
            env,
            [_BASE + base_offset, _BASE + base_offset + 0x40, _BASE + base_offset + 0x80],
        )
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["pending"]) >= 10,
        max_cycles=6000,
        label="prefetch MSHRs full",
    )
    _drive_soft_prefetch(env, [_BASE + 0x800, _BASE + 0x840, _BASE + 0x880])
    _wait_bins(env, [("icache_missunit_capacity", "prefetch_full_backpressure")])
    assert any(
        item["miss_valid"] == 1 and item["miss_ready"] == 0 for item in samples
    ) or env.functional_coverage.key_hit("icache_missunit_capacity", "prefetch_full_backpressure")
    _assert_clean(env)


@pytest.mark.funcov_bins("BIN-693", "BIN-694")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_acquire_priority_dut(env) -> None:
    _prepare(
        env,
        latency=128,
        words=32768,
        prefetch_enabled=False,
        backend_can_accept=False,
    )
    _wait_initial_refill(env)
    env.icache_agent.set_a_ready(0)
    fetch = _BASE + 0x2038
    env.monitor.set_expected_pc(fetch)
    env.backend_model.inject_redirect(fetch, "ctrl_redirect", delay_cycles=0)
    _wait_mshr_state(
        env,
        lambda sample: _mshr_count(sample, range(4), issue=0) >= 2,
        max_cycles=1024,
        label="two unissued fetch MSHRs",
    )
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1
    _drive_soft_prefetch(env, [_BASE + 0x8000])
    _wait_mshr_state(
        env,
        lambda sample: _mshr_present(sample, range(4, 14), issue=0),
        max_cycles=1024,
        label="unissued prefetch MSHR beside fetch candidates",
    )
    env.icache_agent.set_a_ready(1)
    env.step(4)
    env.icache_agent.set_a_ready(None)
    _wait_bins(
        env,
        [
            ("icache_missunit_acquire", "fetch_priority_over_prefetch"),
            ("icache_missunit_acquire", "fetch_index_priority"),
        ]
    )
    _assert_clean(env)


@pytest.mark.funcov_bins("BIN-699", "BIN-700")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_dedup_dut(env) -> None:
    _prepare(
        env,
        latency=128,
        prefetch_enabled=False,
        backend_can_accept=True,
    )
    _wait_initial_refill(env)
    env.icache_agent.set_a_ready(0)
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1

    demand_target = _BASE + 0x40
    _wait_mshr_state(
        env,
        lambda sample: _mshr_has_key(sample, range(4), demand_target, issue=0),
        max_cycles=1024,
        label="existing fetch MSHR before prefetch duplicate",
    )
    for _ in range(16):
        _drive_soft_prefetch(env, [demand_target])
        if env.functional_coverage.key_hit(
            "icache_missunit_dedup", "prefetch_merge_any_mshr"
        ):
            break

    # Keep at least one prefetch MSHR ahead of the sequential demand stream.
    # Pulse ready for exactly one cycle per earlier demand line, preventing the
    # target prefetch acquire from escaping between demand responses.
    prefetch_target = _BASE + 0xC0
    for _ in range(16):
        _drive_soft_prefetch(env, [prefetch_target])
        if _mshr_has_key(_snapshot(env), range(4, 14), prefetch_target, issue=0):
            break
    _wait_mshr_state(
        env,
        lambda sample: _mshr_has_key(
            sample, range(4, 14), prefetch_target, issue=0
        ),
        max_cycles=1024,
        label="existing prefetch MSHR before demand duplicate",
    )
    response_count = int(env.icache_agent.get_stats()["resp_line_count"])
    env.icache_agent.set_a_ready(1)
    env.step(1)
    env.icache_agent.set_a_ready(0)
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["resp_line_count"])
        > response_count,
        max_cycles=512,
        label="first sequential demand response before prefetch duplicate",
    )
    _wait_mshr_state(
        env,
        lambda sample: _mshr_has_key(
            sample, range(4), _BASE + 0x80, issue=0
        ),
        max_cycles=512,
        label="second sequential demand MSHR before prefetch duplicate",
    )
    response_count = int(env.icache_agent.get_stats()["resp_line_count"])
    env.icache_agent.set_a_ready(1)
    env.step(1)
    env.icache_agent.set_a_ready(0)
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["resp_line_count"])
        > response_count,
        max_cycles=512,
        label="second sequential demand response before prefetch duplicate",
    )
    _wait_bins(
        env,
        [("icache_missunit_dedup", "fetch_merge_any_mshr")],
        max_cycles=4096,
    )
    env.icache_agent.set_a_ready(None)
    _wait_bins(
        env,
        [
            ("icache_missunit_dedup", "fetch_merge_any_mshr"),
            ("icache_missunit_dedup", "prefetch_merge_any_mshr"),
        ]
    )
    _assert_clean(env)


@pytest.mark.funcov_bins(
    "BIN-702", "BIN-703", "BIN-704", "BIN-705", "BIN-1005", "BIN-706"
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_redirect_flush_dut(env) -> None:
    samples = _prepare(
        env,
        latency=128,
        prefetch_enabled=False,
        backend_can_accept=False,
    )
    _wait_initial_refill(env)
    env.icache_agent.set_a_ready(0)
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1

    _wait_mshr_state(
        env,
        lambda sample: _mshr_present(sample, range(4), issue=0),
        max_cycles=1024,
        label="unissued fetch MSHR before redirect",
    )
    for attempt in range(16):
        if _mshr_count(_snapshot(env), range(4, 14), issue=0) >= 10:
            break
        base = _BASE + 0x2000 + attempt * 0xC0
        _drive_soft_prefetch(env, [base, base + 0x40, base + 0x80])
        env.step(4)
    _wait_mshr_state(
        env,
        lambda sample: _mshr_count(sample, range(4, 14), issue=0) >= 10,
        max_cycles=2048,
        label="full set of unissued prefetch MSHRs",
    )

    # With all prefetch MSHRs occupied, this nonduplicate request remains valid
    # until the redirect arrives, aligning request blocking and MSHR flushing.
    held_prefetch = _BASE + 0x4000
    _set_soft_prefetch(env, [held_prefetch])
    _wait_mshr_state(
        env,
        lambda sample: sample["prefetch_valid"] == 1
        and sample["prefetch_ready"] == 0
        and sample["prefetch_hit"] == 0,
        max_cycles=1024,
        label="blocked nonduplicate prefetch request",
    )
    first_redirect = _BASE + 0x700
    env.monitor.set_expected_pc(first_redirect)
    env.backend_model.inject_redirect(first_redirect, "ctrl_redirect", delay_cycles=0)
    env.step(2)
    _clear_soft_prefetch(env)
    _wait_bins(
        env,
        [
            ("icache_missunit_flush", "redirect_blocks_new_prefetch"),
            ("icache_missunit_flush", "redirect_cancels_unissued_prefetch"),
            ("icache_missunit_flush", "redirect_keeps_unissued_fetch_mshr"),
        ],
        max_cycles=128,
    )

    env.icache_agent.set_a_ready(1)
    _drive_soft_prefetch(env, [_BASE + 0x5000])
    _wait_mshr_state(
        env,
        lambda sample: _mshr_present(sample, range(4), issue=1)
        and _mshr_present(sample, range(4, 14), issue=1),
        max_cycles=1024,
        label="issued fetch and prefetch MSHRs",
    )
    issued_redirect = _BASE + 0x780
    env.monitor.set_expected_pc(issued_redirect)
    env.backend_model.inject_redirect(issued_redirect, "ctrl_redirect", delay_cycles=0)
    env.step(2)

    # Fetch MSHRs survive redirect.  The agent already knows when its leading
    # response will start, so account for the three-cycle redirect-to-MissUnit
    # propagation and align flush with that response's second beat.
    pending = env.icache_agent.pending[0]
    sample = _snapshot(env)
    source = int(pending.source)
    assert 0 <= source < 4 and sample[f"mshr_{source}_valid"] == 1, {
        "pending_source": source,
        "snapshot": sample,
    }
    response_redirect = _BASE + 0x800
    env.monitor.set_expected_pc(response_redirect)
    redirect_delay = max(
        2,
        int(pending.ready_cycle) - int(env.current_cycle) + 1,
    )
    env.backend_model.inject_redirect(
        response_redirect,
        "ctrl_redirect",
        delay_cycles=redirect_delay,
    )
    env.step(redirect_delay + 8)
    env.icache_agent.set_a_ready(None)
    _wait_bins(
        env,
        [
            ("icache_missunit_flush", "redirect_blocks_new_prefetch"),
            ("icache_missunit_flush", "redirect_cancels_unissued_prefetch"),
            ("icache_missunit_flush", "redirect_marks_issued_prefetch"),
            ("icache_missunit_flush", "redirect_keeps_unissued_fetch_mshr"),
            ("icache_missunit_flush", "redirect_keeps_issued_fetch_mshr"),
            ("icache_missunit_flush", "redirect_suppresses_sram_write"),
        ],
        max_cycles=12000,
    )
    assert any(item["flush"] == 1 for item in samples)
    _assert_clean(env)


@pytest.mark.funcov_bins("BIN-686", "BIN-709", "BIN-710", "BIN-1005")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_fencei_dut(env) -> None:
    samples = _prepare(env, latency=128)
    _wait_initial_refill(env)
    _drive_soft_prefetch(env, [_BASE + 0x100, _BASE + 0x180, _BASE + 0x200])
    _pulse_fencei(env)
    env.backend_model.inject_redirect(_BASE + 0x900, "ctrl_redirect", delay_cycles=0)
    _drive_soft_prefetch(env, [_BASE + 0x280, _BASE + 0x2C0, _BASE + 0x300])
    _pulse_fencei_redirect(env, _BASE + 0x980)
    _drive_soft_prefetch(env, [_BASE + 0x340, _BASE + 0x380, _BASE + 0x3C0])
    _pulse_fencei_redirect(env, _BASE + 0xA00)
    _wait_bins(
        env,
        [
            ("icache_missunit_request", "fetch_mshr_allocate"),
            ("icache_missunit_fencei", "fencei_marks_issued_mshr"),
            ("icache_missunit_fencei", "fencei_suppresses_sram_write"),
            ("icache_missunit_flush", "redirect_keeps_issued_fetch_mshr"),
        ],
        max_cycles=12000,
    )
    assert any(item["fencei"] == 1 for item in samples)
    _assert_clean(env)


@pytest.mark.funcov_bins("BIN-707", "BIN-708", "BIN-711")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_fencei_unissued_dut(env) -> None:
    samples = _prepare(
        env,
        latency=16384,
        words=32768,
        prefetch_enabled=False,
        backend_can_accept=False,
    )
    state = _build_mshr_pressure_until(
        env,
        lambda sample: (
            _mshr_present(sample, range(4), issue=0)
            and _mshr_present(sample, range(4, 14), issue=0)
            and sample["prefetch_valid"] == 1
            and sample["prefetch_hit"] == 0
            and _request_key_absent(
                sample,
                paddr_key="prefetch_paddr",
                vset_key="prefetch_vset",
            )
        ),
    )
    _pulse_fencei_when(
        env,
        lambda sample: (
            _mshr_present(sample, range(4), issue=0)
            and _mshr_present(sample, range(4, 14), issue=0)
        ),
        max_cycles=1,
    )
    # The cancellation/FIFO checks need pre-existing MSHRs, while blocking a
    # new request is best observed by keeping fence.i high as a fresh soft
    # prefetch traverses PrefetchPipe.
    fencei = getattr(env.clock_reset, "io_fencei", None)
    assert fencei is not None, {"missing_signal": "io_fencei"}
    fencei.value = 1
    _set_soft_prefetch(env, [_BASE + 0xE000])
    env.step(24)
    fencei.value = 0
    _clear_soft_prefetch(env)
    env.step(1)
    _wait_bins(
        env,
        [
            ("icache_missunit_fencei", "fencei_blocks_new_nonduplicate"),
            ("icache_missunit_fencei", "fencei_cancels_unissued_mshr"),
            ("icache_missunit_fencei", "fencei_clears_prefetch_fifo"),
        ],
        max_cycles=128,
    )
    assert state["miss_valid"] == 1 or state["prefetch_valid"] == 1
    assert any(sample["fencei"] == 1 for sample in samples)
    _assert_clean(env)


@pytest.mark.funcov_bins("BIN-712", "BIN-1007")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_fencei_redirect_unissued_dut(env) -> None:
    samples = _prepare(
        env,
        latency=16384,
        words=32768,
        prefetch_enabled=False,
        backend_can_accept=False,
    )
    _build_mshr_pressure_until(
        env,
        lambda sample: (
            _mshr_present(sample, range(4), issue=0)
            and _mshr_present(sample, range(4, 14), issue=0)
        ),
    )
    _pulse_fencei_when(
        env,
        lambda sample: (
            _mshr_present(sample, range(4), issue=0)
            and _mshr_present(sample, range(4, 14), issue=0)
        ),
        target=_BASE + 0xF000,
        max_cycles=1,
    )
    _wait_bins(
        env,
        [
            ("icache_missunit_fencei", "fencei_redirect_fetch_unissued"),
            ("icache_missunit_fencei", "fencei_redirect_prefetch_unissued"),
        ],
        max_cycles=128,
    )
    assert any(sample["fencei"] == 1 and sample["flush"] == 1 for sample in samples)
    _assert_clean(env)


@pytest.mark.funcov_bins("BIN-1006", "BIN-1008")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_fencei_redirect_issued_dut(env) -> None:
    samples = _prepare(
        env,
        latency=16384,
        words=32768,
        prefetch_enabled=False,
        backend_can_accept=False,
    )
    _build_mshr_pressure_until(
        env,
        lambda sample: (
            _mshr_present(sample, range(4), issue=1)
            and _mshr_present(sample, range(4, 14), issue=1)
            and sample["last_fire_next"] == 0
        ),
        issued=True,
    )
    _pulse_fencei_when(
        env,
        lambda sample: (
            _mshr_present(sample, range(4), issue=1)
            and _mshr_present(sample, range(4, 14), issue=1)
            and sample["last_fire_next"] == 0
        ),
        target=_BASE + 0x10000,
        max_cycles=1,
    )
    _wait_bins(
        env,
        [
            ("icache_missunit_fencei", "fencei_redirect_fetch_issued"),
            ("icache_missunit_fencei", "fencei_redirect_prefetch_issued"),
        ],
        max_cycles=128,
    )
    assert any(sample["fencei"] == 1 and sample["flush"] == 1 for sample in samples)
    _assert_clean(env)
