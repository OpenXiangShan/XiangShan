from __future__ import annotations

from typing import Any, Optional

from .flush_from_bpu import BpuS3Flush, ftq_ptr_matches_or_before


_MAIN = "Frontend_top.Frontend.inner_icache.mainPipe."
_ICACHE = "Frontend_top.Frontend.inner_icache."
_WAYLOOKUP = _ICACHE + "wayLookup."


def _on(value: Optional[int]) -> bool:
    return value is not None and int(value) != 0


def _off(value: Optional[int]) -> bool:
    return value is not None and int(value) == 0


_PREFETCH = "Frontend_top.Frontend.inner_icache.prefetcher."
_TOP = "Frontend_top."


ICACHE_PREFETCHPIPE_COVERPOINTS = {
    "icache_prefetchpipe_s0_entry": "entry_arbitration_flush",
    "icache_prefetchpipe_s1_meta": "itlb_meta_state",
    "icache_prefetchpipe_s1_completion": "completion_mode",
    "icache_prefetchpipe_s2_miss": "miss_behavior",
}


ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS = frozenset(
    {
        ("icache_prefetchpipe_s0_entry", "ftq_accept_all_ready"),
        ("icache_prefetchpipe_s0_entry", "itlb_or_meta_backpressure"),
        ("icache_prefetchpipe_s0_entry", "s1_busy_backpressure"),
        ("icache_prefetchpipe_s0_entry", "redirect_flush_blocks_hw"),
        ("icache_prefetchpipe_s0_entry", "bpu_flush_match_blocks_hw"),
        ("icache_prefetchpipe_s0_entry", "bpu_flush_miss_allows_hw"),
        ("icache_prefetchpipe_s0_entry", "soft_ignores_bpu_flush"),
        ("icache_prefetchpipe_s0_entry", "soft_priority_over_ftq"),
        ("icache_prefetchpipe_s0_entry", "multi_soft_single_accept"),
        ("icache_prefetchpipe_s0_entry", "soft_ftq_same_cycle_capture"),
        ("icache_prefetchpipe_s1_meta", "flush_cancels_itlb_wait"),
        ("icache_prefetchpipe_s1_meta", "itlb_miss_resend_meta_retry"),
        ("icache_prefetchpipe_s1_meta", "meta_resend_backpressure_recovery"),
        ("icache_prefetchpipe_s1_meta", "waylookup_backpressure_recovery"),
        ("icache_prefetchpipe_s1_meta", "clean_refill_updates_meta"),
        ("icache_prefetchpipe_s1_meta", "same_way_new_tag_invalidates_old"),
        ("icache_prefetchpipe_s1_meta", "dual_layout_same_line"),
        ("icache_prefetchpipe_s1_meta", "dual_layout_overlap1"),
        ("icache_prefetchpipe_s1_meta", "dual_layout_overlap2"),
        ("icache_prefetchpipe_s1_meta", "dual_layout_interleave"),
        ("icache_prefetchpipe_s1_meta", "conflict_first_request_only"),
        ("icache_prefetchpipe_s1_meta", "soft_probe_no_waylookup_ftq"),
        ("icache_prefetchpipe_s1_completion", "prefetch_disabled_no_s2"),
        ("icache_prefetchpipe_s1_completion", "prefetch_enabled_enters_s2"),
        ("icache_prefetchpipe_s1_completion", "s2_busy_enters_s2_recovery"),
        ("icache_prefetchpipe_s1_completion", "flush_blocks_s1_completion"),
        ("icache_prefetchpipe_s2_miss", "sram_or_clean_mshr_hit"),
        ("icache_prefetchpipe_s2_miss", "clean_mshr_cancels_backpressured_miss"),
        ("icache_prefetchpipe_s2_miss", "corrupt_refill_reprefetch"),
        ("icache_prefetchpipe_s2_miss", "nonmatching_clean_refill_ignored"),
        ("icache_prefetchpipe_s2_miss", "exception_or_mmio_suppresses"),
        ("icache_prefetchpipe_s2_miss", "single_line_miss_request"),
        ("icache_prefetchpipe_s2_miss", "dual_line_miss_requests"),
        ("icache_prefetchpipe_s2_miss", "missunit_backpressure_recovery"),
        ("icache_prefetchpipe_s2_miss", "single_line_masks_second_port"),
        ("icache_prefetchpipe_s2_miss", "redirect_flush_ready_boundary"),
        ("icache_prefetchpipe_s2_miss", "bpu_flush_keeps_s2"),
    }
)


_PREFETCH_SIGNALS = {
    "from_valid": (_PREFETCH + "io_fromFtq_valid",),
    "from_ready": (
        _PREFETCH + "io_fromFtq_ready",
        _PREFETCH + "__Vtogcov__io_fromFtq_ready",
    ),
    "from_soft": (
        _PREFETCH + "io_fromFtq_bits_req_0_isSoftPrefetch",
        _PREFETCH + "__Vtogcov__io_fromFtq_bits_req_0_isSoftPrefetch",
    ),
    "s0_fire": (_PREFETCH + "s0_fire", _PREFETCH + "__Vtogcov__s0_fire"),
    "s1_ready": (_PREFETCH + "__Vtogcov__s1_ready",),
    "s2_ready": (_PREFETCH + "s2_ready", _PREFETCH + "__Vtogcov__s2_ready"),
    "s1_valid": (_PREFETCH + "s1_valid", _PREFETCH + "__Vtogcov__s1_valid"),
    "s1_soft": (
        _PREFETCH + "s1_isSoftPrefetch",
        _PREFETCH + "__Vtogcov__s1_isSoftPrefetch",
    ),
    "s1_state": (_PREFETCH + "s1_state", _PREFETCH + "__Vtogcov__s1_state"),
    "s1_wait_itlb": (
        _PREFETCH + "s1_waitItlb",
        _PREFETCH + "__Vtogcov__s1_waitItlb",
    ),
    "s1_tlb_finish": (
        _PREFETCH + "tlbValidLatch",
        _PREFETCH + "__Vtogcov__tlbValidLatch",
    ),
    "s1_two_case": (
        _PREFETCH + "s1_twoPrefetchCase_value",
        _PREFETCH + "__Vtogcov__s1_twoPrefetchCase_value",
    ),
    "s1_fire": (_PREFETCH + "s1_fire", _PREFETCH + "__Vtogcov__s1_fire"),
    "s1_real_fire": (
        _PREFETCH + "s1_realFire",
        _PREFETCH + "__Vtogcov__s1_realFire",
    ),
    "s1_flush": (_PREFETCH + "io_itlbFlushPipe",),
    "s1_next_state": (_PREFETCH + "s1_nextState", _PREFETCH + "__Vtogcov__s1_nextState"),
    "itlb_flush": (_PREFETCH + "io_itlbFlushPipe",),
    "itlb_req_valid": (_PREFETCH + "io_itlb_req_valid",),
    "itlb_resp_miss": (_PREFETCH + "io_itlb_resp_bits_miss",),
    "meta_req_valid": (
        _PREFETCH + "io_metaRead_req_valid",
        _PREFETCH + "__Vtogcov__io_metaRead_req_valid",
    ),
    "meta_ready": (
        _PREFETCH + "io_metaRead_req_ready",
        _PREFETCH + "__Vtogcov__io_metaRead_req_ready",
    ),
    "way0_ready": (
        _PREFETCH + "io_wayLookupWrite_0_ready",
        _PREFETCH + "__Vtogcov__io_wayLookupWrite_0_ready",
    ),
    "way0_valid": (
        _PREFETCH + "io_wayLookupWrite_0_valid",
        _PREFETCH + "__Vtogcov__io_wayLookupWrite_0_valid",
    ),
    "way1_valid": (
        _PREFETCH + "io_wayLookupWrite_1_valid",
        _PREFETCH + "__Vtogcov__io_wayLookupWrite_1_valid",
    ),
    "way1_ready": (
        # ICacheWayLookup intentionally drives both write ready signals together;
        # generated RTL may therefore retain only port 0.
        _PREFETCH + "io_wayLookupWrite_0_ready",
        _PREFETCH + "__Vtogcov__io_wayLookupWrite_0_ready",
        _PREFETCH + "io_wayLookupWrite_1_ready",
        _PREFETCH + "__Vtogcov__io_wayLookupWrite_1_ready",
    ),
    "waylookup_num_valid": (_WAYLOOKUP + "numValidEntries",),
    "waylookup_exception_valid": (_WAYLOOKUP + "exceptionEntry_valid",),
    "global_flush": (
        _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush",
        _ICACHE + "io_fromFtq_redirectFlush",
        _PREFETCH + "io_flush",
    ),
    "bpu_valid": (
        _ICACHE + "__Vtogcov__io_fromFtq_flushFromBpu_s3_valid",
        _PREFETCH + "io_flushFromBpu_s3_valid",
    ),
    "bpu_flag": (
        _PREFETCH + "io_flushFromBpu_s3_bits_flag",
        _ICACHE + "__Vtogcov__io_fromFtq_flushFromBpu_s3_bits_flag",
    ),
    "bpu_value": (
        _PREFETCH + "io_flushFromBpu_s3_bits_value",
        _ICACHE + "__Vtogcov__io_fromFtq_flushFromBpu_s3_bits_value",
    ),
    "s0_ftq_flag": (_PREFETCH + "io_fromFtq_bits_req_0_ftqIdx_flag",),
    "s0_ftq_value": (_PREFETCH + "io_fromFtq_bits_req_0_ftqIdx_value",),
    "s1_ftq_flag": (_PREFETCH + "s1_ftqIdx_flag",),
    "s1_ftq_value": (_PREFETCH + "s1_ftqIdx_value",),
    "ftq_prefetch_valid": (_ICACHE + "io_fromFtq_toPrefetch_valid",),
    "soft_pending": (
        _ICACHE + "softPrefetchValid",
        _ICACHE + "__Vtogcov__softPrefetchValid",
    ),
    "soft0_valid": (_TOP + "io_softPrefetch_0_valid",),
    "soft1_valid": (_TOP + "io_softPrefetch_1_valid",),
    "soft2_valid": (_TOP + "io_softPrefetch_2_valid",),
    "pf_enable": (
        "Frontend_top.Frontend.inner_icache_io_csrPfEnable_REG",
        "Frontend_top.Frontend.__Vtogcov__inner_icache_io_csrPfEnable_REG",
    ),
    "refill_valid": (
        _MAIN + "io_missResp_valid",
        _MAIN + "__Vtogcov__io_missResp_valid",
    ),
    "refill_vset": (
        _MAIN + "__Vtogcov__io_missResp_bits_vSetIdx",
        _PREFETCH + "io_missResp_bits_vSetIdx",
    ),
    "refill_corrupt": (
        _MAIN + "__Vtogcov__io_missResp_bits_corrupt",
        _PREFETCH + "io_missResp_bits_corrupt",
    ),
    "refill_denied": (
        _MAIN + "__Vtogcov__io_missResp_bits_denied",
        _PREFETCH + "io_missResp_bits_denied",
    ),
    "refill_waymask": (_ICACHE + "missUnit.__Vtogcov__io_resp_bits_waymask",),
    "s1_mshr_valid": (
        _PREFETCH + "s1_mshrValid",
        _PREFETCH + "__Vtogcov__s1_mshrValid",
    ),
    "s1_ptag_same": (
        _PREFETCH + "newInfo_pTagSame",
        _PREFETCH + "__Vtogcov__newInfo_pTagSame",
    ),
    "s1_set0": (
        _PREFETCH + "s1_readMetaSetIdx_0",
        _PREFETCH + "__Vtogcov__s1_readMetaSetIdx_0",
    ),
    "s1_set1": (
        _PREFETCH + "s1_readMetaSetIdx_1",
        _PREFETCH + "__Vtogcov__s1_readMetaSetIdx_1",
    ),
    "s1_old_way0": (
        _PREFETCH + "s1_metaInfoReg_r_waymask",
        _PREFETCH + "__Vtogcov__s1_metaInfoReg_r_waymask",
    ),
    "s1_old_way1": (
        _PREFETCH + "s1_metaInfoReg_r_1_waymask",
        _PREFETCH + "__Vtogcov__s1_metaInfoReg_r_1_waymask",
    ),
    "s1_sram_valid0": (
        _PREFETCH + "s1_sramValid_0",
        _PREFETCH + "__Vtogcov__s1_sramValid_0",
    ),
    "s1_sram_hit0": (
        _PREFETCH + "s1_sramHits_0",
        _PREFETCH + "__Vtogcov__s1_sramHits_0",
        _PREFETCH + "s1_metaInfo_0_waymask",
    ),
    "s1_sram_hit1": (
        _PREFETCH + "s1_sramHits_1",
        _PREFETCH + "__Vtogcov__s1_sramHits_1",
        _PREFETCH + "s1_metaInfo_1_waymask",
    ),
    "s1_double": (
        _PREFETCH + "s1_readDoubleLine",
        _PREFETCH + "__Vtogcov__s1_readDoubleLine",
    ),
    "s1_backend_exception": (
        _PREFETCH + "s1_backendException_value",
        _PREFETCH + "__Vtogcov__s1_backendException_value",
    ),
    "s1_itlb_exception_raw": (
        _PREFETCH + "s1_itlbExceptionRaw_value",
        _PREFETCH + "__Vtogcov__s1_itlbExceptionRaw_value",
    ),
    "s1_pmp_exception": (
        _PREFETCH + "s1_pmpException_value",
        _PREFETCH + "__Vtogcov__s1_pmpException_value",
        _PREFETCH + "io_pmp_resp_instr",
    ),
    "s1_pmp_mmio": (
        _PREFETCH + "s1_pmpMmio",
        _PREFETCH + "__Vtogcov__s1_pmpMmio",
        _PREFETCH + "io_pmp_resp_mmio",
    ),
    "s1_pbmt": (
        _PREFETCH + "s1_itlbPbmt",
        _PREFETCH + "__Vtogcov__s1_itlbPbmt",
    ),
    "s2_valid": (_PREFETCH + "s2_valid", _PREFETCH + "__Vtogcov__s2_valid"),
    "s2_double": (
        _PREFETCH + "s2_doubleline",
        _PREFETCH + "__Vtogcov__s2_doubleline",
    ),
    "s2_exception": (
        _PREFETCH + "s2_exception_value",
        _PREFETCH + "__Vtogcov__s2_exception_value",
    ),
    "s2_mmio": (_PREFETCH + "s2_isMmio", _PREFETCH + "__Vtogcov__s2_isMmio"),
    "s2_sram0": (
        _PREFETCH + "s2_sramHits_0",
        _PREFETCH + "__Vtogcov__s2_sramHits_0",
    ),
    "s2_sram1": (
        _PREFETCH + "s2_sramHits_1",
        _PREFETCH + "__Vtogcov__s2_sramHits_1",
    ),
    "s2_set0": (
        _PREFETCH + "s2_readMetaSetIdx_0",
        _PREFETCH + "__Vtogcov__s2_readMetaSetIdx_0",
    ),
    "s2_set1": (
        _PREFETCH + "s2_readMetaSetIdx_1",
        _PREFETCH + "__Vtogcov__s2_readMetaSetIdx_1",
    ),
    "s2_ptag": (_PREFETCH + "s2_pTag", _PREFETCH + "__Vtogcov__s2_pTag"),
    "s2_mshr0": (
        _PREFETCH + "s2_mshrHits_valid",
        _PREFETCH + "__Vtogcov__s2_mshrHits_valid",
    ),
    "s2_mshr1": (
        _PREFETCH + "s2_mshrHits_valid_1",
        _PREFETCH + "__Vtogcov__s2_mshrHits_valid_1",
    ),
    "s2_miss0": (_PREFETCH + "s2_miss_0", _PREFETCH + "__Vtogcov__s2_miss_0"),
    "s2_miss1": (_PREFETCH + "s2_miss_1", _PREFETCH + "__Vtogcov__s2_miss_1"),
    "s2_has_send0": (
        _PREFETCH + "s2_hasSend_0",
        _PREFETCH + "__Vtogcov__s2_hasSend_0",
    ),
    "s2_has_send1": (
        _PREFETCH + "s2_hasSend_1",
        _PREFETCH + "__Vtogcov__s2_hasSend_1",
    ),
    "miss_ready": (
        _ICACHE + "missUnit.io_prefetchReq_ready",
        _ICACHE + "missUnit.__Vtogcov__io_prefetchReq_ready",
    ),
    "miss_valid": (
        _ICACHE + "missUnit.io_prefetchReq_valid",
        _ICACHE + "missUnit.__Vtogcov__io_prefetchReq_valid",
    ),
    "refill_paddr": (
        _PREFETCH + "__Vtogcov__io_missResp_bits_blkPAddr",
        _PREFETCH + "io_missResp_bits_blkPAddr",
    ),
}


def _read_prefetch(recorder, key: str) -> Optional[int]:
    dut = getattr(getattr(recorder, "env", None), "dut", None)
    if dut is None:
        return None
    return recorder._read_first_dut_signal(dut, _PREFETCH_SIGNALS[key])


def reset_icache_prefetchpipe_coverage_state(recorder) -> None:
    recorder._icache_prefetchpipe_cov_state = {
        "itlb_miss_pending": False,
        "meta_blocked_cycles": 0,
        "waylookup_full_cycles": 0,
        "waylookup_ftq": None,
        "soft_meta_read_pending": False,
        "s2_blocked": False,
        "clean_mshr_pending": None,
        "missunit_backpressure_cycles": 0,
        "missunit_backpressure_signature": None,
        "s2_ftq": None,
    }


def _mark_prefetch(
    recorder,
    group: str,
    bin_name: str,
    cycle: int,
    condition: bool,
    evidence: dict[str, Any],
) -> None:
    if condition:
        recorder.mark(
            group,
            bin_name,
            cycle,
            evidence,
            coverpoint=ICACHE_PREFETCHPIPE_COVERPOINTS[group],
        )


def _ftq_ptr(s: dict[str, Any], prefix: str) -> tuple[int, int] | None:
    flag = s.get(f"{prefix}_ftq_flag")
    value = s.get(f"{prefix}_ftq_value")
    if flag is None or value is None:
        return None
    return int(flag), int(value)


def _bpu_flush_match(s: dict[str, Any], prefix: str) -> bool | None:
    return ftq_ptr_matches_or_before(
        BpuS3Flush(
            valid=s.get("bpu_valid"),
            flag=s.get("bpu_flag"),
            value=s.get("bpu_value"),
        ),
        _ftq_ptr(s, prefix),
    )


def _active_s2_ports(s: dict[str, Any]) -> tuple[int, ...]:
    return (0, 1) if _on(s["s2_double"]) else (0,)


def _s2_signature(s: dict[str, Any]) -> tuple[int, tuple[int, ...]] | None:
    ptag = s["s2_ptag"]
    sets = tuple(s[f"s2_set{port}"] for port in _active_s2_ports(s))
    if ptag is None or any(value is None for value in sets):
        return None
    return int(ptag), tuple(int(value) for value in sets)


def _matching_s2_refill_ports(
    s: dict[str, Any], *, corrupt: bool
) -> tuple[int, ...]:
    if (
        not _on(s["s2_valid"])
        or not _on(s["refill_valid"])
        or s["refill_corrupt"] is None
        or bool(_on(s["refill_corrupt"])) != bool(corrupt)
        or s["refill_vset"] is None
        or s["refill_paddr"] is None
        or s["s2_ptag"] is None
    ):
        return ()

    # MissResp.blkPAddr contains pTag followed by the 4-KiB page's six
    # cacheline-index bits for the 64-B ICache line size.
    refill_ptag = int(s["refill_paddr"]) >> 6
    if refill_ptag != int(s["s2_ptag"]):
        return ()
    return tuple(
        port
        for port in _active_s2_ports(s)
        if s[f"s2_set{port}"] is not None
        and int(s[f"s2_set{port}"]) == int(s["refill_vset"])
    )


def sample_icache_prefetchpipe_coverage(recorder, env, cycle: int) -> None:
    del env
    state = getattr(recorder, "_icache_prefetchpipe_cov_state", None)
    if state is None:
        reset_icache_prefetchpipe_coverage_state(recorder)
        state = recorder._icache_prefetchpipe_cov_state

    s = {key: _read_prefetch(recorder, key) for key in _PREFETCH_SIGNALS}
    evidence = {key: value for key, value in s.items() if value is not None}
    s2_signature = _s2_signature(s) if _on(s["s2_valid"]) else None
    clean_s2_refill_ports = _matching_s2_refill_ports(s, corrupt=False)
    corrupt_s2_refill_ports = _matching_s2_refill_ports(s, corrupt=True)
    tracked_s2_ftq = state["s2_ftq"]
    s2_set_values = {
        value for value in (s["s2_set0"], s["s2_set1"]) if value is not None
    }
    hw_request = _on(s["from_valid"]) and _off(s["from_soft"])
    soft_request = _on(s["from_valid"]) and _on(s["from_soft"])
    # This pipe uses a non-blocking ITLB port whose request ready is an RTL invariant.
    entry_resources_ready = _on(s["s1_ready"]) and _on(s["meta_ready"])
    bpu_s0_match = _bpu_flush_match(s, "s0")
    bpu_s1_match = _bpu_flush_match(s, "s1") if _on(s["s1_valid"]) else False
    bpu_does_not_block_entry = (
        _off(s["bpu_valid"])
        or (bpu_s0_match is False and bpu_s1_match is False)
    )

    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s0_entry",
        "ftq_accept_all_ready",
        cycle,
        hw_request and _on(s["s0_fire"]),
        evidence,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s0_entry",
        "soft_ftq_same_cycle_capture",
        cycle,
        _off(s["soft_pending"])
        and _on(s["ftq_prefetch_valid"])
        and any(_on(s[key]) for key in ("soft0_valid", "soft1_valid", "soft2_valid"))
        and hw_request
        and entry_resources_ready
        and _off(s["global_flush"])
        and bpu_does_not_block_entry,
        evidence,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s0_entry",
        "itlb_or_meta_backpressure",
        cycle,
        hw_request
        and _on(s["s1_ready"])
        and _off(s["meta_ready"])
        and _off(s["global_flush"]),
        evidence,
    )
    s1_state = s["s1_state"]
    if _on(s["s1_flush"]) or _off(s["s1_valid"]):
        state["itlb_miss_pending"] = False
    if (
        _on(s["s1_valid"])
        and _on(s["itlb_req_valid"])
        and _on(s["itlb_resp_miss"])
        and _off(s["s1_flush"])
    ):
        state["itlb_miss_pending"] = True
    itlb_resend_success = (
        state["itlb_miss_pending"]
        and _on(s["s1_valid"])
        and _on(s["s1_wait_itlb"])
        and _on(s["itlb_req_valid"])
        and _off(s["itlb_resp_miss"])
        and _on(s["s1_tlb_finish"])
        and _off(s["s1_flush"])
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "itlb_miss_resend_meta_retry",
        cycle,
        itlb_resend_success,
        evidence,
    )
    if itlb_resend_success:
        state["itlb_miss_pending"] = False

    meta_blocked = (
        _on(s["s1_valid"])
        and s1_state == 2
        and _on(s["meta_req_valid"])
        and _off(s["meta_ready"])
        and _off(s["s1_flush"])
    )
    if meta_blocked:
        state["meta_blocked_cycles"] += 1
    meta_recovered = (
        state["meta_blocked_cycles"] >= 2
        and _on(s["s1_valid"])
        and s1_state == 2
        and _on(s["meta_req_valid"])
        and _on(s["meta_ready"])
        and _off(s["s1_flush"])
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "meta_resend_backpressure_recovery",
        cycle,
        meta_recovered,
        evidence,
    )
    if meta_recovered or not meta_blocked:
        state["meta_blocked_cycles"] = 0

    s1_ftq = _ftq_ptr(s, "s1")
    waylookup_full_blocked = (
        _on(s["s1_valid"])
        and s1_state == 3
        and _off(s["s1_soft"])
        and s["s1_two_case"] in (1, 2, 4, 8)
        and _on(s["way0_valid"])
        and _on(s["way1_valid"])
        and _off(s["way0_ready"])
        and _off(s["way1_ready"])
        and s["waylookup_num_valid"] == 32
        and _off(s["waylookup_exception_valid"])
        and _off(s["refill_valid"])
        and _off(s["s1_flush"])
        and s1_ftq is not None
    )
    if waylookup_full_blocked:
        if state["waylookup_ftq"] != s1_ftq:
            state["waylookup_full_cycles"] = 0
            state["waylookup_ftq"] = s1_ftq
        state["waylookup_full_cycles"] += 1
    waylookup_recovered = (
        state["waylookup_full_cycles"] >= 2
        and state["waylookup_ftq"] == s1_ftq
        and _on(s["s1_valid"])
        and s1_state == 3
        and _on(s["way0_valid"])
        and _on(s["way1_valid"])
        and _on(s["way0_ready"])
        and _on(s["way1_ready"])
        and _off(s["s1_flush"])
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "waylookup_backpressure_recovery",
        cycle,
        waylookup_recovered,
        evidence,
    )
    waylookup_capacity_wait = (
        state["waylookup_full_cycles"] >= 2
        and state["waylookup_ftq"] == s1_ftq
        and _on(s["s1_valid"])
        and s1_state == 3
        and _on(s["way0_valid"])
        and _on(s["way1_valid"])
        and _off(s["way0_ready"])
        and _off(s["way1_ready"])
        and _off(s["waylookup_exception_valid"])
        and _off(s["s1_flush"])
    )
    if waylookup_recovered or (
        not waylookup_full_blocked and not waylookup_capacity_wait
    ):
        state["waylookup_full_cycles"] = 0
        state["waylookup_ftq"] = None

    soft_meta_pending = bool(state["soft_meta_read_pending"])
    if soft_meta_pending and _off(s["s1_valid"]):
        state["soft_meta_read_pending"] = False
        soft_meta_pending = False
    soft_meta_response = soft_meta_pending and _on(s["s1_sram_valid0"])
    soft_meta_complete = (
        soft_meta_response
        and _on(s["s1_valid"])
        and _on(s["s1_soft"])
        and _on(s["s1_tlb_finish"])
        and _off(s["s1_flush"])
    )
    if soft_meta_response or _on(s["s1_flush"]):
        state["soft_meta_read_pending"] = False
    soft_meta_read_fire = (
        _on(s["meta_req_valid"])
        and _on(s["meta_ready"])
        and (
            (soft_request and _on(s["s0_fire"]))
            or (_on(s["s1_valid"]) and _on(s["s1_soft"]))
        )
        and _off(s["s1_flush"])
    )
    if soft_meta_read_fire:
        state["soft_meta_read_pending"] = True
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s0_entry",
        "s1_busy_backpressure",
        cycle,
        hw_request
        and _on(s["s1_valid"])
        and _off(s["s1_ready"])
        and _off(s["global_flush"]),
        evidence,
    )
    s2_waiting = (
        _on(s["s1_valid"])
        and _on(s["pf_enable"])
        and _off(s["s1_flush"])
        and s1_state == 4
    )
    if state["s2_blocked"] and not s2_waiting:
        state["s2_blocked"] = False
    if s2_waiting and _off(s["s2_ready"]):
        state["s2_blocked"] = True
    s2_recovered = state["s2_blocked"] and s2_waiting and _on(s["s2_ready"])
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s1_completion",
        "s2_busy_enters_s2_recovery",
        cycle,
        s2_recovered,
        evidence,
    )
    if s2_recovered:
        state["s2_blocked"] = False

    enqway_can_complete = (
        s1_state == 3
        and _on(s["s2_ready"])
        and (
            _on(s["s1_soft"])
            or (_off(s["refill_valid"]) and _on(s["way0_ready"]))
        )
    )
    enters2_can_complete = s1_state == 4 and _on(s["s2_ready"])
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s1_completion",
        "flush_blocks_s1_completion",
        cycle,
        _on(s["s1_valid"])
        and _on(s["s1_flush"])
        and _on(s["pf_enable"])
        and (enqway_can_complete or enters2_can_complete),
        evidence,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s0_entry",
        "redirect_flush_blocks_hw",
        cycle,
        hw_request and _on(s["global_flush"]) and entry_resources_ready,
        evidence,
    )
    pending_clean_mshr_signature = state["clean_mshr_pending"]
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "clean_mshr_cancels_backpressured_miss",
        cycle,
        pending_clean_mshr_signature is not None
        and pending_clean_mshr_signature == s2_signature
        and bool(clean_s2_refill_ports),
        evidence,
    )
    pending_miss = (
        _on(s["s2_valid"])
        and _on(s["miss_valid"])
        and _off(s["miss_ready"])
        and _off(s["global_flush"])
        and not clean_s2_refill_ports
        and s2_signature is not None
    )
    if (
        _on(s["global_flush"])
        or _off(s["s2_valid"])
        or clean_s2_refill_ports
    ):
        state["clean_mshr_pending"] = None
    elif pending_miss:
        state["clean_mshr_pending"] = s2_signature
    elif state["clean_mshr_pending"] != s2_signature:
        state["clean_mshr_pending"] = None

    bpu_entry_scenario = (
        hw_request
        and _on(s["bpu_valid"])
        and _off(s["global_flush"])
        and entry_resources_ready
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s0_entry",
        "bpu_flush_match_blocks_hw",
        cycle,
        bpu_entry_scenario and bpu_s0_match is True,
        evidence,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s0_entry",
        "bpu_flush_miss_allows_hw",
        cycle,
        bpu_entry_scenario
        and bpu_s0_match is False
        and bpu_s1_match is False,
        evidence,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "nonmatching_clean_refill_ignored",
        cycle,
        _on(s["s2_valid"])
        and _on(s["refill_valid"])
        and _off(s["refill_corrupt"])
        and s["refill_vset"] is not None
        and s["refill_vset"] not in s2_set_values,
        evidence,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s0_entry",
        "soft_ignores_bpu_flush",
        cycle,
        soft_request
        and _on(s["bpu_valid"])
        and _off(s["global_flush"])
        and _on(s["s0_fire"]),
        evidence,
    )
    backpressure_blocked = (
        _on(s["s2_valid"])
        and _on(s["miss_valid"])
        and _off(s["miss_ready"])
        and _off(s["global_flush"])
        and not clean_s2_refill_ports
        and s2_signature is not None
    )
    if backpressure_blocked:
        if state["missunit_backpressure_signature"] == s2_signature:
            state["missunit_backpressure_cycles"] += 1
        else:
            state["missunit_backpressure_signature"] = s2_signature
            state["missunit_backpressure_cycles"] = 1
    backpressure_recovered = (
        state["missunit_backpressure_cycles"] >= 2
        and state["missunit_backpressure_signature"] == s2_signature
        and _on(s["s2_valid"])
        and _on(s["miss_valid"])
        and _on(s["miss_ready"])
        and _off(s["global_flush"])
        and not clean_s2_refill_ports
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "missunit_backpressure_recovery",
        cycle,
        backpressure_recovered,
        evidence,
    )
    if backpressure_recovered or not backpressure_blocked:
        state["missunit_backpressure_cycles"] = 0
        state["missunit_backpressure_signature"] = None
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s0_entry",
        "soft_priority_over_ftq",
        cycle,
        _on(s["soft_pending"])
        and _on(s["ftq_prefetch_valid"])
        and soft_request
        and _on(s["s0_fire"]),
        evidence,
    )
    soft_input_count = sum(
        _on(s[key]) for key in ("soft0_valid", "soft1_valid", "soft2_valid")
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s0_entry",
        "multi_soft_single_accept",
        cycle,
        soft_input_count >= 2 and _off(s["soft_pending"]),
        evidence,
    )

    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "flush_cancels_itlb_wait",
        cycle,
        _on(s["s1_valid"])
        and _on(s["s1_wait_itlb"])
        and (
            _on(s["global_flush"])
            or (
                _off(s["s1_soft"])
                and bpu_s1_match is True
            )
        ),
        evidence,
    )
    set_values = {value for value in (s["s1_set0"], s["s1_set1"]) if value is not None}
    refill_set_match = s["refill_vset"] is not None and s["refill_vset"] in set_values
    clean_refill = (
        _on(s["s1_valid"])
        and _on(s["refill_valid"])
        and _off(s["refill_corrupt"])
        and _off(s["refill_denied"])
        and _on(s["s1_mshr_valid"])
        and refill_set_match
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "clean_refill_updates_meta",
        cycle,
        clean_refill and _on(s["s1_ptag_same"]),
        evidence,
    )
    same_way_new_tag = False
    if clean_refill and _off(s["s1_ptag_same"]) and s["refill_waymask"] is not None:
        refill_waymask = int(s["refill_waymask"])
        for set_key, way_key in (
            ("s1_set0", "s1_old_way0"),
            ("s1_set1", "s1_old_way1"),
        ):
            old_waymask = s[way_key]
            if (
                s[set_key] == s["refill_vset"]
                and old_waymask is not None
                and int(old_waymask) != 0
                and int(old_waymask).bit_count() == 1
                and int(old_waymask) == refill_waymask
            ):
                same_way_new_tag = True
                break
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "same_way_new_tag_invalidates_old",
        cycle,
        same_way_new_tag,
        evidence,
    )
    for layout, bin_name in (
        (1, "dual_layout_same_line"),
        (2, "dual_layout_overlap1"),
        (4, "dual_layout_overlap2"),
        (8, "dual_layout_interleave"),
    ):
        _mark_prefetch(
            recorder,
            "icache_prefetchpipe_s1_meta",
            bin_name,
            cycle,
            _on(s["s1_valid"])
            and _off(s["s1_soft"])
            and s["s1_two_case"] == layout,
            evidence,
        )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "conflict_first_request_only",
        cycle,
        _on(s["s1_valid"])
        and _off(s["s1_soft"])
        and s["s1_two_case"] == 0
        and _on(s["s1_tlb_finish"]),
        evidence,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "soft_probe_no_waylookup_ftq",
        cycle,
        soft_meta_complete,
        evidence,
    )

    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s1_completion",
        "prefetch_disabled_no_s2",
        cycle,
        _on(s["s1_fire"]) and _off(s["pf_enable"]),
        evidence,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s1_completion",
        "prefetch_enabled_enters_s2",
        cycle,
        _on(s["s1_real_fire"]) and _on(s["pf_enable"]),
        evidence,
    )

    sram_hit_on_s2_entry = any(
        _on(s[f"s2_sram{port}"]) for port in _active_s2_ports(s)
    )
    clean_refill_updates_s2_miss = any(
        _off(s[f"s2_sram{port}"]) for port in clean_s2_refill_ports
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "sram_or_clean_mshr_hit",
        cycle,
        _on(s["s2_valid"])
        and (sram_hit_on_s2_entry or clean_refill_updates_s2_miss),
        evidence,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "corrupt_refill_reprefetch",
        cycle,
        _on(s["s2_valid"])
        and _off(s["s2_exception"])
        and _off(s["s2_mmio"])
        and any(
            _off(s[f"s2_sram{port}"])
            for port in corrupt_s2_refill_ports
        ),
        evidence,
    )
    active_s1_ports = (0, 1) if _on(s["s1_double"]) else (0,)
    s1_target_meta_miss = any(
        _off(s[f"s1_sram_hit{port}"]) for port in active_s1_ports
    )
    s1_protection_source = any(
        _on(s[key])
        for key in (
            "s1_backend_exception",
            "s1_itlb_exception_raw",
            "s1_pmp_exception",
            "s1_pmp_mmio",
        )
    ) or s["s1_pbmt"] in (1, 2)
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "exception_or_mmio_suppresses",
        cycle,
        _on(s["s1_real_fire"])
        and s1_target_meta_miss
        and s1_protection_source,
        evidence,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "single_line_miss_request",
        cycle,
        _on(s["s2_valid"])
        and _off(s["s2_double"])
        and _on(s["s2_miss0"])
        and _on(s["miss_ready"]),
        evidence,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "dual_line_miss_requests",
        cycle,
        _on(s["s2_valid"])
        and _on(s["s2_double"])
        and _on(s["s2_miss0"])
        and _on(s["s2_miss1"])
        and _on(s["miss_ready"]),
        evidence,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "single_line_masks_second_port",
        cycle,
        _on(s["s2_valid"]) and _off(s["s2_double"]),
        evidence,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "redirect_flush_ready_boundary",
        cycle,
        _on(s["s2_valid"])
        and _on(s["miss_valid"])
        and _on(s["global_flush"])
        and _on(s["miss_ready"]),
        evidence,
    )
    tracked_bpu_s2_match = ftq_ptr_matches_or_before(
        BpuS3Flush(
            valid=s.get("bpu_valid"),
            flag=s.get("bpu_flag"),
            value=s.get("bpu_value"),
        ),
        tracked_s2_ftq,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "bpu_flush_keeps_s2",
        cycle,
        _on(s["s2_valid"])
        and _on(s["miss_valid"])
        and _off(s["global_flush"])
        and tracked_bpu_s2_match is True,
        evidence,
    )
    if _on(s["global_flush"]) or (
        _off(s["s2_valid"]) and _off(s["s1_real_fire"])
    ):
        state["s2_ftq"] = None
    if _on(s["s1_real_fire"]):
        state["s2_ftq"] = s1_ftq if _off(s["s1_soft"]) else None
