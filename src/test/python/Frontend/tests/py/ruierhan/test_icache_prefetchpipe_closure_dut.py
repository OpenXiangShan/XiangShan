"""Top-level stimulus closure for low-risk ICache PrefetchPipe bins.

Only existing frontend controls, memory/translation models, redirects,
predictor enables and soft-prefetch ports are driven. Internal signals are
sampled only to align public stimulus with the transaction stage under test.
"""

from __future__ import annotations

import os
from collections.abc import Iterable

import pytest

from env.funcov.py.icache.icache_prefetchpipe_funcov import _read_prefetch
from env.sequences import TranslationScenario, TranslationScenarioBuilder
from tests.py.jiabowen.test_icache_mainpipe_miss_response import (
    test_icache_trained_two_fetch_hit_hit_then_fencei_miss_miss as _run_trained_refill,
)
from tests.py.jiabowen.test_two_fetch_directed_flow_dut import _c_j
from tests.py.zhaoxinran.test_multi_branch import (
    test_large_loop_multi_segment as _run_large_loop,
)
from tests.py.ruierhan.test_icache_mainpipe_s0_flush_closure_dut import (
    _initialize_bpu_s3_stream,
    _restore_predictors,
    _s0_sampling_window,
    _trigger_bpu_s3_flush,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_NOP = 0x0000_0013
_SOFT_BASE = 0x8004_0000


def _recorder(env):
    recorder = getattr(env, "functional_coverage", None)
    assert recorder is not None, "PrefetchPipe closure requires functional coverage"
    return recorder


def _hit(env, group: str, bin_name: str) -> bool:
    return bool(_recorder(env).key_hit(group, bin_name))


def _wait_bins(
    env,
    targets: Iterable[tuple[str, str]],
    *,
    max_cycles: int = 6000,
) -> None:
    remaining = set(targets)
    for _ in range(int(max_cycles)):
        remaining = {
            target for target in remaining if not _hit(env, target[0], target[1])
        }
        if not remaining:
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": "PrefetchPipe functional coverage targets were not reached",
            "missing": sorted(f"{group}.{name}" for group, name in remaining),
            "cycle": int(env.current_cycle),
            "icache": env.icache_agent.get_stats(),
            "backend": env.backend_model.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _signal(env, key: str) -> int | None:
    return _read_prefetch(_recorder(env), key)


def _internal_signal(env, *names: str) -> int | None:
    return _recorder(env)._read_first_dut_signal(env.dut, names)


def _wait_icache_request(env, address: int, *, max_cycles: int = 4096) -> dict:
    line = int(address) & ~0x3F
    for _ in range(int(max_cycles)):
        matches = [
            record
            for record in env.icache_agent.get_stats()["request_records"]
            if int(record["address"]) == line
        ]
        if matches:
            return matches[-1]
        env.step(1)
    raise AssertionError({"reason": "ICache request was not observed", "line": line})


def _wait_refill_waymask(env, address: int, *, max_cycles: int = 4096) -> int:
    block = (int(address) & ~0x3F) >> 6
    for _ in range(int(max_cycles)):
        if _signal(env, "refill_valid") == 1 and _signal(env, "refill_paddr") == block:
            waymask = _signal(env, "refill_waymask")
            assert waymask is not None and int(waymask).bit_count() == 1
            return int(waymask)
        env.step(1)
    raise AssertionError({"reason": "ICache refill was not observed", "block": block})


def _load_nops(env, base: int, *, words: int = 8192) -> None:
    env.load_program((_NOP.to_bytes(4, "little")) * int(words), int(base))


def _load_overlap2_loop(env, base: int) -> int:
    # A starts at +0x60 and jumps to B at +0x30 without crossing a line.
    # B crosses into the next line and jumps back to A at +0x50.  Once both
    # branches are trained, consecutive FTQ entries repeat the Overlap2 layout.
    noncross_entry = int(base) + 0x60
    payload = bytearray((0x0001).to_bytes(2, "little") * 128)
    payload[0x50:0x52] = _c_j(0x10).to_bytes(2, "little")
    payload[0x70:0x72] = _c_j(-0x40).to_bytes(2, "little")
    env.load_program(bytes(payload), int(base))
    return noncross_entry


def _prepare_nops(
    env,
    base: int,
    *,
    latency: int,
    seed: int,
    words: int = 8192,
) -> None:
    _load_nops(env, base, words=words)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=int(latency),
        miss_rate=1.0,
        seed=int(seed),
    )
    env.initialize(reset_vector=int(base), bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(int(base))


def _clear_soft_prefetch(env) -> None:
    for slot in range(3):
        valid = getattr(env.dut, f"io_softPrefetch_{slot}_valid", None)
        address = getattr(env.dut, f"io_softPrefetch_{slot}_bits_vaddr", None)
        if valid is not None:
            valid.value = 0
        if address is not None:
            address.value = 0


def _set_soft_prefetch(env, addresses: Iterable[int]) -> None:
    _clear_soft_prefetch(env)
    for slot, address in enumerate(tuple(addresses)[:3]):
        valid = getattr(env.dut, f"io_softPrefetch_{slot}_valid", None)
        value = getattr(env.dut, f"io_softPrefetch_{slot}_bits_vaddr", None)
        assert valid is not None and value is not None, {
            "missing_signal": f"io_softPrefetch_{slot}"
        }
        valid.value = 1
        value.value = int(address)


def _present_soft_prefetch(env, addresses: Iterable[int]) -> None:
    _set_soft_prefetch(env, addresses)
    env.step(1)
    _clear_soft_prefetch(env)


def _present_aligned_soft_prefetch(env, target: int) -> None:
    """Let one same-set request dequeue before presenting the directed tag."""
    _present_soft_prefetch(env, (int(target) + 0x4000,))
    env.step(1)
    _present_soft_prefetch(env, (int(target),))


@pytest.fixture
def prefetchpipe_env(env):
    try:
        yield env
    finally:
        _clear_soft_prefetch(env)
        env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1
        env.backend_model.set_can_accept(1)
        _restore_predictors(env)


@pytest.mark.funcov_bins("BIN-656", "BIN-657", "BIN-663", "BIN-677")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_soft_arbitration(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    _prepare_nops(env, _SOFT_BASE, latency=48, seed=0x6657)
    targets = {
        ("icache_prefetchpipe_s0_entry", "soft_priority_over_ftq"),
        ("icache_prefetchpipe_s0_entry", "multi_soft_single_accept"),
        ("icache_prefetchpipe_s0_entry", "soft_ftq_same_cycle_capture"),
        ("icache_prefetchpipe_s1_meta", "soft_probe_no_waylookup_ftq"),
    }

    for attempt in range(128):
        if all(_hit(env, *target) for target in targets):
            break
        for _ in range(32):
            if _signal(env, "soft_pending") == 0 and _signal(
                env, "ftq_prefetch_valid"
            ) == 1:
                break
            env.step(1)
        offset = 0x100 + (attempt % 32) * 0x80
        _present_soft_prefetch(
            env,
            (_SOFT_BASE + offset, _SOFT_BASE + offset + 0x40),
        )
        env.step(2)

    _wait_bins(env, targets, max_cycles=2048)
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-654")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.funcov_closure_pending
@pytest.mark.xfail(
    strict=True,
    reason=(
        "the current top-level API cannot align BPU stage3's FTQ pointer with "
        "the hardware-prefetch s0 entry; retain as a reachability check"
    ),
)
def test_tc_icache_prefetchpipe_bpu_flush(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    _initialize_bpu_s3_stream(env)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=96,
        miss_rate=1.0,
        seed=0x6654,
    )
    for _ in range(64):
        if _s0_sampling_window(env):
            break
        env.step(1)
    assert _s0_sampling_window(env), "MainPipe s0 did not reach a BPU trigger window"
    env.set_bp_ctrl_enable(
        ubtb_enable=0,
        abtb_enable=0,
        mbtb_enable=0,
        tage_enable=0,
        sc_enable=0,
        ittage_enable=0,
    )
    fencei = getattr(env.dut, "io_fencei", None)
    assert fencei is not None, {"missing_signal": "io_fencei"}
    fencei.value = 1
    env.step(1)
    fencei.value = 0
    _wait_bins(
        env,
        [("icache_prefetchpipe_s0_entry", "bpu_flush_match_blocks_hw")],
        max_cycles=32,
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-653")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetch_s0_redirect(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    _initialize_bpu_s3_stream(env)
    _load_nops(env, _SOFT_BASE)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=64,
        miss_rate=1.0,
        seed=0x6653,
    )

    for attempt in range(128):
        for _ in range(512):
            if (
                _signal(env, "from_valid") == 1
                and _signal(env, "from_soft") == 0
                and _signal(env, "s1_ready") == 1
                and _signal(env, "meta_ready") == 1
                and _signal(env, "global_flush") == 0
            ):
                break
            env.step(1)
        env.backend_model.inject_redirect(
            _SOFT_BASE + 0x1000 + attempt * 0x40,
            "ctrl_redirect",
            delay_cycles=0,
        )
        env.step(2)
        if _hit(env, "icache_prefetchpipe_s0_entry", "redirect_flush_blocks_hw"):
            break

    _wait_bins(
        env,
        [("icache_prefetchpipe_s0_entry", "redirect_flush_blocks_hw")],
        max_cycles=1,
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-655")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.funcov_closure_pending
@pytest.mark.xfail(
    strict=True,
    reason=(
        "the current top-level API cannot deterministically align a soft-prefetch "
        "capture with BPU stage3 valid; retain as a reachability check"
    ),
)
def test_tc_icache_prefetch_soft_bpu(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    _initialize_bpu_s3_stream(env)
    _load_nops(env, _SOFT_BASE)
    try:
        for attempt in range(64):
            if _hit(env, "icache_prefetchpipe_s0_entry", "soft_ignores_bpu_flush"):
                break
            _set_soft_prefetch(
                env,
                (_SOFT_BASE + 0x4000 + (attempt % 64) * 0x40,),
            )
            _trigger_bpu_s3_flush(env)
            for _ in range(40):
                if _hit(
                    env,
                    "icache_prefetchpipe_s0_entry",
                    "soft_ignores_bpu_flush",
                ):
                    break
                env.step(1)
            _restore_predictors(env)
    finally:
        _clear_soft_prefetch(env)
        _restore_predictors(env)

    _wait_bins(
        env,
        [("icache_prefetchpipe_s0_entry", "soft_ignores_bpu_flush")],
        max_cycles=1,
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-664")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_disabled(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    _prepare_nops(env, _SOFT_BASE, latency=32, seed=0x6664)
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 0
    for attempt in range(64):
        _present_soft_prefetch(
            env,
            (_SOFT_BASE + 0x100 + (attempt % 32) * 0x40,),
        )
        if _hit(env, "icache_prefetchpipe_s1_completion", "prefetch_disabled_no_s2"):
            break
        env.step(4)
    _wait_bins(
        env,
        [("icache_prefetchpipe_s1_completion", "prefetch_disabled_no_s2")],
        max_cycles=512,
    )
    assert not env.monitor.get_errors()


def _translation_state(
    env,
    *,
    scenario_id: str,
    va: int,
    pa: int,
    latency: int,
    page_fault: bool = False,
):
    scenario = TranslationScenario(
        scenario_id=scenario_id,
        va=int(va),
        pa=int(pa),
        payload=(_NOP.to_bytes(4, "little")) * 1024,
        page_count=2,
        mode="sv39",
        ptw_response_latency=int(latency),
        s1_pf=1 if page_fault else 0,
        expected_path="fault" if page_fault else "cacheable",
        expected_result="page_fault" if page_fault else "miss_refill",
    )
    return TranslationScenarioBuilder(env).build(scenario)


@pytest.mark.funcov_bins("BIN-658", "BIN-678", "BIN-738", "BIN-740")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_itlb_control(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    pa0 = 0x8040_0F00
    va0 = 0x4020_0F00
    _prepare_nops(env, pa0, latency=32, seed=0x6678)
    first = _translation_state(
        env,
        scenario_id="prefetchpipe-itlb-resend",
        va=va0,
        pa=pa0,
        latency=8,
    )
    env.monitor.clear()
    env.monitor.set_expected_pc(va0)
    env.arm_translation_scenario(first, page_indexes=(0, 1))
    env.backend_model.inject_redirect(va0, "ctrl_redirect", delay_cycles=0)
    _wait_bins(
        env,
        [("icache_prefetchpipe_s1_meta", "itlb_miss_resend_meta_retry")],
        max_cycles=6000,
    )

    pa1 = 0x8042_0F00
    va1 = 0x4022_0F00
    second = _translation_state(
        env,
        scenario_id="prefetchpipe-itlb-wait-flush",
        va=va1,
        pa=pa1,
        latency=64,
    )
    env.monitor.clear()
    env.monitor.set_expected_pc(va1)
    env.arm_translation_scenario(second, page_indexes=(0, 1))
    env.backend_model.inject_redirect(va1, "ctrl_redirect", delay_cycles=0)
    for attempt in range(32):
        for _ in range(512):
            if _signal(env, "s1_valid") == 1 and _signal(
                env, "s1_wait_itlb"
            ) == 1:
                break
            env.step(1)
        env.backend_model.inject_redirect(
            va1 + ((attempt + 1) % 8) * 0x40,
            "ctrl_redirect",
            delay_cycles=0,
        )
        env.step(2)
        if _hit(env, "icache_prefetchpipe_s1_meta", "flush_cancels_itlb_wait"):
            break
    _wait_bins(
        env,
        [
            ("icache_prefetchpipe_s1_meta", "flush_cancels_itlb_wait"),
            ("icache_waylookup_exception", "exception_dequeue"),
            ("icache_waylookup_exception", "exception_waits_flush"),
        ],
        max_cycles=512,
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-679")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_meta_resend_backpressure(prefetchpipe_env) -> None:
    """Hold MetaArray behind fence.i while a translated retry completes."""
    env = prefetchpipe_env
    pa = 0x8046_0F00
    va = 0x4026_0F00
    _prepare_nops(env, pa, latency=32, seed=0x6679)
    state = _translation_state(
        env,
        scenario_id="prefetchpipe-meta-resend-backpressure",
        va=va,
        pa=pa,
        latency=32,
    )
    env.monitor.clear()
    env.monitor.set_expected_pc(va)
    env.arm_translation_scenario(state, page_indexes=(0, 1))
    env.backend_model.inject_redirect(va, "ctrl_redirect", delay_cycles=0)

    for _ in range(4096):
        if _signal(env, "s1_valid") == 1 and _signal(env, "s1_wait_itlb") == 1:
            break
        env.step(1)
    else:
        raise AssertionError("PrefetchPipe did not enter the ITLB retry state")

    fencei = getattr(env.clock_reset, "io_fencei", None)
    assert fencei is not None, {"missing_signal": "io_fencei"}
    try:
        # fence.i legally owns the single-port MetaArray without flushing the
        # PrefetchPipe.  Keep it asserted until the completed ITLB retry has
        # spent two cycles in MetaResend, then release the same transaction.
        fencei.value = 1
        blocked_cycles = 0
        for _ in range(4096):
            if (
                _signal(env, "s1_valid") == 1
                and _signal(env, "s1_state") == 2
                and _signal(env, "meta_req_valid") == 1
                and _signal(env, "meta_ready") == 0
            ):
                blocked_cycles += 1
                if blocked_cycles >= 2:
                    break
            env.step(1)
        else:
            raise AssertionError("MetaRead retry did not remain blocked for two cycles")
        fencei.value = 0
        env.step(1)
    finally:
        fencei.value = 0

    _wait_bins(
        env,
        [("icache_prefetchpipe_s1_meta", "meta_resend_backpressure_recovery")],
        max_cycles=32,
    )


@pytest.mark.funcov_bins("BIN-661", "BIN-666", "BIN-700")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_refill_layout(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    _run_trained_refill(env)
    targets = {
        ("icache_prefetchpipe_s1_meta", "dual_layout_same_line"),
        ("icache_prefetchpipe_s2_miss", "sram_or_clean_mshr_hit"),
        ("icache_missunit_dedup", "prefetch_merge_any_mshr"),
    }
    _wait_bins(env, targets, max_cycles=4000)
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-683")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetch_clean_mshr_before_first_miss_fire(prefetchpipe_env) -> None:
    """A clean response for an existing MSHR cancels an unissued s2 miss."""
    env = prefetchpipe_env
    _run_trained_refill(env)
    _wait_bins(
        env,
        [("icache_prefetchpipe_s2_miss", "clean_mshr_cancels_unissued_miss")],
        max_cycles=4000,
    )
    hit = _recorder(env).hits[
        (
            "icache_prefetchpipe_s2_miss",
            "miss_behavior",
            "clean_mshr_cancels_unissued_miss",
        )
    ]
    evidence = hit.evidence[0]
    matched_ports = tuple(int(port) for port in evidence["unissued_clean_refill_ports"])
    assert matched_ports
    for port in matched_ports:
        assert int(evidence[f"s2_has_send{port}"]) == 0
        assert int(evidence[f"s2_miss{port}"]) == 0
    assert int(evidence["miss_valid"]) == 0
    target = int(evidence["refill_paddr"]) << 6
    target_requests_before = sum(
        int(record["address"]) == (int(target) & ~0x3F)
        for record in env.icache_agent.get_stats()["request_records"]
    )
    env.step(8)
    target_requests_after = sum(
        int(record["address"]) == (int(target) & ~0x3F)
        for record in env.icache_agent.get_stats()["request_records"]
    )
    assert target_requests_after == target_requests_before
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-659")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_clean_refill_updates_meta(prefetchpipe_env) -> None:
    """Keep an exact prefetch probe live across its clean refill window."""
    env = prefetchpipe_env
    base = _SOFT_BASE + 0x2_0000
    _prepare_nops(env, base, latency=96, seed=0x6659, words=32768)
    env.set_bp_ctrl_enable(
        ubtb_enable=0,
        abtb_enable=0,
        mbtb_enable=0,
        tage_enable=0,
        sc_enable=0,
        ittage_enable=0,
    )
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1
    env.backend_model.set_can_accept(0)

    for episode in range(8):
        if _hit(env, "icache_prefetchpipe_s1_meta", "clean_refill_updates_meta"):
            break
        target = base + 0x1000 + episode * 0x4000
        for _ in range(16):
            _present_aligned_soft_prefetch(env, target)
            try:
                request = _wait_icache_request(env, target, max_cycles=32)
                break
            except AssertionError:
                continue
        else:
            raise AssertionError(
                {
                    "reason": "directed soft prefetch did not reach ICache",
                    "target": target,
                    "icache": env.icache_agent.get_stats(),
                }
            )
        request_cycle = int(request["cycle"])

        # The response is scheduled 96 cycles after the accepted request.
        # Re-presenting the exact key keeps a matching s1 transaction live so
        # the sampler can observe the clean refill updating its Meta result.
        while int(env.current_cycle) <= request_cycle + 104:
            _present_soft_prefetch(env, (target,))
            if _hit(
                env,
                "icache_prefetchpipe_s1_meta",
                "clean_refill_updates_meta",
            ):
                break
        env.step(4)

    _wait_bins(
        env,
        [("icache_prefetchpipe_s1_meta", "clean_refill_updates_meta")],
        max_cycles=1,
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-660")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetch_refill_replace(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    base = _SOFT_BASE
    _prepare_nops(env, base, latency=64, seed=0x6660, words=32768)

    same_set_stride = 0x4000
    resident_by_way: dict[int, int] = {}
    for index in range(4):
        target = base + index * same_set_stride
        env.backend_model.inject_redirect(target, "ctrl_redirect", delay_cycles=0)
        _wait_icache_request(env, target)
        resident_by_way[_wait_refill_waymask(env, target)] = target

    assert len(resident_by_way) == 4, {
        "reason": "four same-set lines did not occupy distinct ICache ways",
        "resident_by_way": resident_by_way,
    }

    replacement = base + 4 * same_set_stride
    env.backend_model.inject_redirect(replacement, "ctrl_redirect", delay_cycles=0)
    replacement_request = _wait_icache_request(env, replacement)
    source = int(replacement_request["source"])
    # The TL acquire is recorded on the edge that latches the selected victim
    # into the MSHR.  Sample the registered way on the following cycle.
    env.step(1)
    replacement_way = _internal_signal(
        env,
        f"Frontend_top.Frontend.inner_icache.missUnit.allMshr_{source}.way",
        f"Frontend_top.Frontend.inner_icache.missUnit.allMshr_{source}.__Vtogcov__io_info_bits_way",
    )
    assert replacement_way is not None
    victim_waymask = 1 << int(replacement_way)
    assert victim_waymask in resident_by_way, {
        "replacement_waymask": victim_waymask,
        "resident_by_way": resident_by_way,
    }
    victim = resident_by_way[victim_waymask]

    _set_soft_prefetch(env, (victim,))
    for _ in range(512):
        if _hit(
            env,
            "icache_prefetchpipe_s1_meta",
            "same_way_new_tag_invalidates_old",
        ):
            break
        env.step(1)
    _clear_soft_prefetch(env)

    _wait_bins(
        env,
        [("icache_prefetchpipe_s1_meta", "same_way_new_tag_invalidates_old")],
        max_cycles=1,
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-667")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetch_corrupt_refill(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    target = _SOFT_BASE + 0x6000
    _prepare_nops(env, _SOFT_BASE, latency=32, seed=0x6667)
    env.icache_agent.inject_response_fault_at(target, corrupt=1)
    for _ in range(64):
        _present_soft_prefetch(env, (target,))
        if _hit(env, "icache_prefetchpipe_s2_miss", "corrupt_refill_reprefetch"):
            break
        env.step(2)
    _wait_bins(
        env,
        [("icache_prefetchpipe_s2_miss", "corrupt_refill_reprefetch")],
        max_cycles=4096,
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-758", "BIN-771", "BIN-772", "BIN-778", "BIN-780")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_large_loop_layout(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    _run_large_loop(env)
    _wait_bins(
        env,
        [
            ("icache_prefetchpipe_s1_meta", "dual_layout_overlap1"),
            ("icache_prefetchpipe_s1_meta", "dual_layout_interleave"),
            ("icache_waylookup_wrap", "dual_wrap"),
            ("icache_mainpipe_s2_ecc", "meta_code_mismatch_zero_way_ignored"),
            ("icache_mainpipe_s2_ecc", "meta_invalid_line_masked"),
        ],
        max_cycles=6000,
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-779")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.funcov_closure_pending
@pytest.mark.xfail(
    strict=True,
    reason=(
        "current V3 DUT has not produced the Overlap2 encoding from legal "
        "top-level traffic; retain for nightly reachability checks"
    ),
)
def test_tc_icache_prefetchpipe_overlap2_layout(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    base = _SOFT_BASE + 0x1_0000
    noncross_entry = _load_overlap2_loop(env, base)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=16,
        miss_rate=1.0,
        seed=0x6779,
    )
    env.initialize(reset_vector=noncross_entry, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(noncross_entry)
    for _ in range(1200):
        if int(env.backend_model.get_stats().get("commit_count", 0)) >= 96:
            break
        env.step(1)
    else:
        raise AssertionError(
            {
                "reason": "Overlap2 branch pair did not train",
                "backend": env.backend_model.get_stats(),
            }
        )

    env.backend_model.set_can_accept(0)
    for _ in range(1600):
        occupancy = _signal(env, "waylookup_num_valid")
        if occupancy is not None and int(occupancy) >= 30:
            break
        env.step(1)
    else:
        raise AssertionError(
            {
                "reason": "FTQ did not build enough prefetch backlog",
                "waylookup_num_valid": _signal(env, "waylookup_num_valid"),
                "backend": env.backend_model.get_stats(),
            }
        )
    env.backend_model.set_can_accept(1)
    _wait_bins(
        env,
        [("icache_prefetchpipe_s1_meta", "dual_layout_overlap2")],
        max_cycles=2000,
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-681")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_s2_pressure(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    _run_trained_refill(env)
    _wait_bins(
        env,
        [("icache_prefetchpipe_s1_completion", "s2_busy_enters_s2_recovery")],
        max_cycles=64,
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins(
    "BIN-682", "BIN-672", "BIN-702", "BIN-703", "BIN-704", "BIN-747"
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_flush_boundaries(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    _run_trained_refill(env)
    targets = {
        ("icache_prefetchpipe_s1_completion", "flush_blocks_s1_completion"),
        ("icache_prefetchpipe_s2_miss", "redirect_flush_ready_boundary"),
        ("icache_missunit_flush", "redirect_blocks_new_prefetch"),
        ("icache_missunit_flush", "redirect_cancels_unissued_prefetch"),
        ("icache_missunit_flush", "redirect_marks_issued_prefetch"),
        ("icache_waylookup_flush", "bpu_flush_empty"),
    }
    _wait_bins(env, targets, max_cycles=1000)
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-668")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_protection(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    pa = 0x8044_0F00
    va = 0x4024_0F00
    _prepare_nops(env, pa, latency=32, seed=0x6668)
    state = _translation_state(
        env,
        scenario_id="prefetchpipe-cacheable-page-fault",
        va=va,
        pa=pa,
        latency=8,
        page_fault=True,
    )
    env.monitor.clear()
    env.monitor.set_expected_pc(va)
    env.arm_translation_scenario(state, page_indexes=(0, 1))
    env.backend_model.inject_redirect(va, "ctrl_redirect", delay_cycles=0)
    _wait_bins(
        env,
        [("icache_prefetchpipe_s2_miss", "exception_or_mmio_suppresses")],
        max_cycles=6000,
    )
    assert not env.monitor.get_errors()
