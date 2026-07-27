from __future__ import annotations

from typing import Any, Iterable, Optional


_MAIN = "Frontend_top.Frontend.inner_icache.mainPipe."
_ICACHE = "Frontend_top.Frontend.inner_icache."


ICACHE_MAINPIPE_COVERPOINTS = {
    "icache_mainpipe_s0_entry": "entry_behavior",
    "icache_mainpipe_s0_flush": "flush_behavior",
    "icache_mainpipe_s1_sram": "hit_bank_behavior",
    "icache_mainpipe_s1_backpressure": "stall_behavior",
    "icache_mainpipe_s1_flush": "flush_behavior",
    "icache_mainpipe_s1_refill": "refill_behavior",
    "icache_mainpipe_s1_miss": "miss_request_behavior",
    "icache_mainpipe_s1_protection": "protection_behavior",
    "icache_mainpipe_s2_ecc": "ecc_behavior",
    "icache_mainpipe_s2_meta_flush": "meta_flush_behavior",
}


ICACHE_MAINPIPE_SAMPLER_BIN_KEYS = frozenset(
    {
        ("icache_mainpipe_s0_entry", "single_request_latched"),
        ("icache_mainpipe_s0_entry", "dual_request_data_read"),
        ("icache_mainpipe_s0_entry", "data_array_backpressure"),
        ("icache_mainpipe_s0_entry", "s1_busy_backpressure"),
        ("icache_mainpipe_s0_flush", "global_flush_cancels_entry"),
        ("icache_mainpipe_s0_flush", "bpu_match_cancels_entry"),
        ("icache_mainpipe_s0_flush", "bpu_miss_allows_entry"),
        ("icache_mainpipe_s1_sram", "single_line_sram_hit"),
        ("icache_mainpipe_s1_sram", "cross_line_dual_sram_hit"),
        ("icache_mainpipe_s1_sram", "single_line_bank_range"),
        ("icache_mainpipe_s1_sram", "cross_line_bank_mapping"),
        ("icache_mainpipe_s1_sram", "dual_request_independent"),
        ("icache_mainpipe_s1_backpressure", "hit_response_stall"),
        ("icache_mainpipe_s1_backpressure", "refill_completion_stall"),
        ("icache_mainpipe_s1_backpressure", "pending_miss_blocks_upstream"),
        ("icache_mainpipe_s1_flush", "global_flush_clears_s1"),
        ("icache_mainpipe_s1_flush", "bpu_match_clears_s1"),
        ("icache_mainpipe_s1_flush", "bpu_miss_keeps_s1"),
        ("icache_mainpipe_s1_flush", "flush_while_miss_backpressured"),
        ("icache_mainpipe_s1_flush", "late_refill_ignored_after_flush"),
        ("icache_mainpipe_s1_flush", "flush_wins_matching_refill"),
        ("icache_mainpipe_s1_refill", "clean_refill_match"),
        ("icache_mainpipe_s1_refill", "nonmatching_refill_ignored"),
        ("icache_mainpipe_s1_refill", "corrupt_refill_saved"),
        ("icache_mainpipe_s1_refill", "cross_line_split_refill"),
        ("icache_mainpipe_s1_refill", "refill_request_line_selective"),
        ("icache_mainpipe_s1_miss", "single_line_miss_request"),
        ("icache_mainpipe_s1_miss", "four_line_fixed_priority"),
        ("icache_mainpipe_s1_miss", "missunit_backpressure_stable"),
        ("icache_mainpipe_s1_miss", "has_send_no_duplicate"),
        ("icache_mainpipe_s1_miss", "invalid_line_no_miss"),
        ("icache_mainpipe_s1_protection", "pmp_exec_check"),
        ("icache_mainpipe_s1_protection", "itlb_over_pmp_priority"),
        ("icache_mainpipe_s1_protection", "pmp_exception_suppresses_miss"),
        ("icache_mainpipe_s1_protection", "mmio_pbmt_suppresses_refill"),
        ("icache_mainpipe_s1_protection", "tl_error_to_exception"),
        ("icache_mainpipe_s1_protection", "dual_request_shared_protection"),
        ("icache_mainpipe_s2_ecc", "s1_fire_latches_s2"),
        ("icache_mainpipe_s2_ecc", "ecc_disabled_masks_errors"),
        ("icache_mainpipe_s2_ecc", "forced_ecc_when_disabled"),
        ("icache_mainpipe_s2_ecc", "meta_code_or_multiway_corrupt"),
        ("icache_mainpipe_s2_ecc", "data_ecc_selected_bank_only"),
        ("icache_mainpipe_s2_ecc", "mshr_bypass_skips_data_ecc"),
        ("icache_mainpipe_s2_ecc", "corrupt_sideband_per_line"),
        ("icache_mainpipe_s2_ecc", "global_flush_clears_s2_bpu_does_not"),
        ("icache_mainpipe_s2_meta_flush", "first_corrupt_request_only"),
        ("icache_mainpipe_s2_meta_flush", "meta_error_flush_all_ways"),
        ("icache_mainpipe_s2_meta_flush", "data_error_flush_hit_way"),
        ("icache_mainpipe_s2_meta_flush", "corrupt_refetch_disabled_no_flush"),
    }
)


_SIGNALS = {
    "from_valid": (_MAIN + "io_fromWayLookup_valid", _MAIN + "__Vtogcov__io_fromWayLookup_valid"),
    "from_ready": (_MAIN + "io_fromWayLookup_ready", _MAIN + "__Vtogcov__io_fromWayLookup_ready"),
    "data_ready": (
        _ICACHE + "dataArray.io_read_req_ready",
        _ICACHE + "dataArray.__Vtogcov__io_read_req_ready",
    ),
    "data_valid": (
        _ICACHE + "dataArray.io_read_req_valid",
        _ICACHE + "dataArray.__Vtogcov__io_read_req_valid",
    ),
    "data_req1_valid": (_ICACHE + "dataArray.io_read_req_bits_1_valid",),
    "io_flush": (_MAIN + "io_flush",),
    "s0_flush": (_MAIN + "s0_flush", _MAIN + "__Vtogcov__s0_flush"),
    "bpu_valid": (_MAIN + "io_flushFromBpu_s3_valid",),
    "s1_ready": (_MAIN + "s1_ready", _MAIN + "__Vtogcov__s1_ready"),
    "s1_valid": (_MAIN + "s1_valid", _MAIN + "__Vtogcov__s1_valid"),
    "s1_flush": (_MAIN + "s1_flush", _MAIN + "__Vtogcov__s1_flush"),
    "req1_valid": (_MAIN + "s1_req_1_valid",),
    "cross0": (_MAIN + "s1_req_0_isCrossLine",),
    "cross1": (_MAIN + "s1_req_1_isCrossLine",),
    "toifu_valid": (_MAIN + "io_toIfu_req_valid",),
    "toifu_ready": (_MAIN + "io_toIfu_req_ready",),
    "s1_fire": (_MAIN + "s1_fire",),
    "fetch_finish": (_MAIN + "__Vtogcov__s1_fetchFinish",),
    "miss_req_valid": (_MAIN + "__Vtogcov__io_missReq_valid",),
    "miss_req_ready": (_MAIN + "__Vtogcov__io_missReq_ready",),
    "miss_req_vset": (
        _MAIN + "__Vtogcov__io_missReq_bits_vSetIdx",
        _ICACHE + "_mainPipe_io_missReq_bits_vSetIdx",
    ),
    "miss_req_paddr": (
        _MAIN + "__Vtogcov__io_missReq_bits_blkPAddr",
        _ICACHE + "_mainPipe_io_missReq_bits_blkPAddr",
    ),
    "miss_resp_valid": (
        _MAIN + "io_missResp_valid",
        _MAIN + "__Vtogcov__io_missResp_valid",
    ),
    "miss_resp_vset": (_MAIN + "__Vtogcov__io_missResp_bits_vSetIdx",),
    "miss_resp_paddr": (_MAIN + "__Vtogcov__io_missResp_bits_blkPAddr",),
    "miss_resp_corrupt": (_MAIN + "__Vtogcov__io_missResp_bits_corrupt",),
    "miss_resp_denied": (_MAIN + "__Vtogcov__io_missResp_bits_denied",),
    "pmp_instr": (_MAIN + "io_pmp_resp_instr",),
    "pmp_mmio": (_MAIN + "io_pmp_resp_mmio",),
    "itlb_exception": (
        _MAIN + "s1_exceptionInfo_0_itlbException_value",
        _MAIN + "__Vtogcov__s1_exceptionInfo_0_itlbException_value",
    ),
    "exception": (_MAIN + "s1_exception_value",),
    "is_mmio": (_MAIN + "s1_isMmio",),
    "pbmt": (_MAIN + "s1_wayLookupEntry_0_itlbPbmt",),
    "ecc_enable": (_MAIN + "io_eccEnable", _ICACHE + "io_eccEnable"),
    "local_ecc_enable": (_MAIN + "eccEnable", _MAIN + "__Vtogcov__eccEnable"),
    "error_valid": (_MAIN + "io_error_valid", _MAIN + "__Vtogcov__io_error_valid"),
    "error_meta": (_MAIN + "io_error_bits_source_tag",),
    "error_data": (_MAIN + "io_error_bits_source_data",),
    "meta_flush0_valid": (_MAIN + "io_metaFlush_req_0_valid",),
    "meta_flush1_valid": (_MAIN + "io_metaFlush_req_1_valid",),
    "meta_flush0_waymask": (_MAIN + "io_metaFlush_req_0_bits_waymask",),
    "meta_flush1_waymask": (_MAIN + "io_metaFlush_req_1_bits_waymask",),
}

_EVIDENCE_SCALARS = frozenset(
    {
        "from_valid",
        "from_ready",
        "data_ready",
        "data_valid",
        "io_flush",
        "s0_flush",
        "bpu_valid",
        "s1_ready",
        "s1_valid",
        "s1_flush",
        "req1_valid",
        "cross0",
        "cross1",
        "toifu_valid",
        "toifu_ready",
        "s1_fire",
        "fetch_finish",
        "miss_req_valid",
        "miss_req_ready",
        "miss_req_vset",
        "miss_resp_valid",
        "miss_resp_corrupt",
        "miss_resp_denied",
        "pmp_instr",
        "pmp_mmio",
        "itlb_exception",
        "exception",
        "is_mmio",
        "pbmt",
        "ecc_enable",
        "local_ecc_enable",
        "error_valid",
    }
)


def _read(recorder, key: str) -> Optional[int]:
    dut = getattr(getattr(recorder, "env", None), "dut", None)
    if dut is None:
        return None
    return recorder._read_first_dut_signal(dut, _SIGNALS[key])


def _read_names(recorder, names: Iterable[str]) -> tuple[Optional[int], ...]:
    dut = getattr(getattr(recorder, "env", None), "dut", None)
    if dut is None:
        return tuple(None for _ in names)
    return tuple(recorder._read_first_dut_signal(dut, (name,)) for name in names)


def _vec(
    recorder,
    stem: str,
    count: int,
    *,
    first_unsuffixed: bool = False,
) -> tuple[Optional[int], ...]:
    names = []
    for index in range(count):
        suffix = "" if first_unsuffixed and index == 0 else f"_{index}"
        names.append(_MAIN + stem + suffix)
    return _read_names(recorder, names)


def _on(value: Optional[int]) -> bool:
    return value is not None and int(value) != 0


def _off(value: Optional[int]) -> bool:
    return value is not None and int(value) == 0


def _known(values: Iterable[Optional[int]]) -> bool:
    return all(value is not None for value in values)


def _bits(values: Iterable[Optional[int]]) -> tuple[int, ...]:
    return tuple(int(value or 0) for value in values)


def _mark(
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
            coverpoint=ICACHE_MAINPIPE_COVERPOINTS[group],
        )


def reset_icache_mainpipe_coverage_state(recorder) -> None:
    recorder._icache_mainpipe_cov_state = {
        "prev": None,
        "late_refill_pending": False,
        "split_refill_pending": False,
        "split_refill_lines": set(),
        "miss_backpressure_seen": False,
        "s2_global_flush_seen": False,
        "s2_bpu_only_seen": False,
    }


def _snapshot(recorder) -> dict[str, Any]:
    scalar = {key: _read(recorder, key) for key in _SIGNALS}
    scalar.update(
        {
            "hits": _read_names(
                recorder,
                tuple(
                    _MAIN + f"s1_hits_{req}_{line}"
                    for req in range(2)
                    for line in range(2)
                ),
            ),
            "sram_valid": _read_names(
                recorder,
                (
                    _MAIN + "s1_sramRespValid",
                    _MAIN + "s1_sramValid_0_1",
                    _MAIN + "s1_sramValid_1_0",
                    _MAIN + "s1_sramValid_1_1",
                ),
            ),
            "waymask": _read_names(
                recorder,
                tuple(
                    _MAIN + f"s1_wayLookupEntry_{req}_waymask_{line}"
                    for req in range(2)
                    for line in range(2)
                ),
            ),
            "start_vaddr": _read_names(
                recorder,
                (
                    _MAIN + "s1_req_0_vAddr_0_addr",
                    _MAIN + "s1_req_1_vAddr_0_addr",
                ),
            ),
            "bank_sram": _read_names(
                recorder,
                tuple(
                    _MAIN + f"s1_bankSramValid_{req}_{bank}"
                    for req in range(2)
                    for bank in range(7)
                ),
            ),
            "mshr": _read_names(
                recorder,
                tuple(
                    _MAIN + f"s1_mshrValid_{req}_{line}"
                    for req in range(2)
                    for line in range(2)
                ),
            ),
            "bank_mshr": _read_names(
                recorder,
                tuple(
                    _MAIN + f"s1_bankMshrValid_{req}_{bank}"
                    for req in range(2)
                    for bank in range(7)
                ),
            ),
            "should": _vec(recorder, "s1_shouldFetch", 4),
            "has_send": _vec(recorder, "s1_hasSend_valid", 4, first_unsuffixed=True),
            "arb_valid": _read_names(
                recorder,
                tuple(
                    _MAIN + f"toMissArbiter.io_in_{index}_valid"
                    for index in range(4)
                ),
            ),
            "tl_corrupt": _vec(
                recorder,
                "s1_tlCorrupt_r",
                4,
                first_unsuffixed=True,
            ),
            "tl_denied": _read_names(
                recorder,
                (
                    _MAIN + "s1_tlDenied_0_0",
                    _MAIN + "s1_tlDenied_r_1",
                    _MAIN + "s1_tlDenied_r_2",
                    _MAIN + "s1_tlDenied_r_3",
                ),
            ),
            "vset": _read_names(
                recorder,
                tuple(
                    _MAIN + f"s1_req_{req}_vSetIdx_{line}"
                    for req in range(2)
                    for line in range(2)
                ),
            ),
            "s2_corrupt": _read_names(
                recorder,
                tuple(
                    _MAIN + f"io_toIfu_corrupt_{req}_{line}"
                    for req in range(2)
                    for line in range(2)
                ),
            ),
            "s2_meta_hitnum": _read_names(
                recorder,
                tuple(
                    _MAIN
                    + "s2_corruptInfo_metaCorrupt_hitNum"
                    + ("" if index == 0 else f"_{index}")
                    for index in range(4)
                ),
            ),
            "s2_bank_sram": _read_names(
                recorder,
                tuple(
                    _MAIN + f"s2_bankSramValid_{req}_{bank}"
                    for req in range(2)
                    for bank in range(8)
                ),
            ),
            "s2_sram_hits": _read_names(
                recorder,
                tuple(
                    _MAIN + f"s2_sramHits_{req}_{line}"
                    for req in range(2)
                    for line in range(2)
                ),
            ),
            "s2_waymask": _read_names(
                recorder,
                tuple(
                    _MAIN + f"s2_wayMask_{req}_{line}"
                    for req in range(2)
                    for line in range(2)
                ),
            ),
        }
    )
    return scalar


def sample_icache_mainpipe_coverage(recorder, env, cycle: int) -> None:
    del env
    state = getattr(recorder, "_icache_mainpipe_cov_state", None)
    if state is None:
        reset_icache_mainpipe_coverage_state(recorder)
        state = recorder._icache_mainpipe_cov_state

    s = _snapshot(recorder)
    prev = state["prev"]
    should = _bits(s["should"])
    hits = _bits(s["hits"])
    mshr = _bits(s["mshr"])
    has_send = _bits(s["has_send"])
    corrupt = _bits(s["s2_corrupt"])
    local_ecc_enable = (
        s["local_ecc_enable"]
        if s["local_ecc_enable"] is not None
        else s["ecc_enable"]
    )
    evidence = {
        key: value
        for key, value in s.items()
        if key in _EVIDENCE_SCALARS and value is not None
    }
    evidence.update(
        {
            "should_fetch": should,
            "hits": hits,
            "mshr_valid": mshr,
            "has_send": has_send,
            "s2_corrupt": corrupt,
        }
    )

    pending_miss = _on(s["s1_valid"]) and any(should)
    refill_match = _on(s["miss_resp_valid"]) and any(mshr)
    global_s0_flush = _on(s["io_flush"])
    bpu_s0_flush = _on(s["s0_flush"]) and _on(s["bpu_valid"])
    global_s1_flush = _on(s["io_flush"])
    bpu_s1_flush = _on(s["s1_flush"]) and _on(s["bpu_valid"])

    # A bin is sampled when the testpoint's Condition is present.  Signals
    # listed by the testpoint as Checkpoint remain useful evidence, but must
    # never be conjoined here: doing that turns a coverage sample into a
    # (silent) correctness checker and makes a failing checkpoint look unhit.
    s0_accept_condition = (
        _on(s["from_valid"])
        and _on(s["data_ready"])
        and _on(s["s1_ready"])
        and _off(s["s0_flush"])
    )

    _mark(
        recorder,
        "icache_mainpipe_s0_entry",
        "single_request_latched",
        cycle,
        s0_accept_condition and _off(s["data_req1_valid"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s0_entry",
        "dual_request_data_read",
        cycle,
        s0_accept_condition and _on(s["data_req1_valid"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s0_entry",
        "data_array_backpressure",
        cycle,
        _on(s["from_valid"])
        and _on(s["s1_ready"])
        and _off(s["data_ready"])
        and _off(s["s0_flush"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s0_entry",
        "s1_busy_backpressure",
        cycle,
        _on(s["from_valid"])
        and _on(s["s1_valid"])
        and _off(s["s1_ready"]),
        evidence,
    )

    _mark(
        recorder,
        "icache_mainpipe_s0_flush",
        "global_flush_cancels_entry",
        cycle,
        _on(s["from_valid"])
        and global_s0_flush
        and _on(s["data_ready"])
        and _on(s["s1_ready"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s0_flush",
        "bpu_match_cancels_entry",
        cycle,
        _on(s["from_valid"])
        and bpu_s0_flush
        and _off(s["io_flush"])
        and _on(s["data_ready"])
        and _on(s["s1_ready"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s0_flush",
        "bpu_miss_allows_entry",
        cycle,
        _on(s["from_valid"])
        and _on(s["bpu_valid"])
        and _off(s["io_flush"])
        and _off(s["s0_flush"])
        and _on(s["data_ready"])
        and _on(s["s1_ready"]),
        evidence,
    )

    line_valid = (
        1,
        int(_on(s["cross0"])),
        int(_on(s["req1_valid"])),
        int(_on(s["req1_valid"]) and _on(s["cross1"])),
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_sram",
        "single_line_sram_hit",
        cycle,
        _on(s["s1_valid"])
        and _off(s["cross0"])
        and _known(s["sram_valid"][:1])
        and _on(s["sram_valid"][0])
        and _known(s["waymask"][:1])
        and int(s["waymask"][0]) != 0
        and _off(s["pmp_instr"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_sram",
        "cross_line_dual_sram_hit",
        cycle,
        _on(s["s1_valid"])
        and _on(s["cross0"])
        and _known(s["sram_valid"][:2])
        and _bits(s["sram_valid"][:2]) == (1, 1)
        and _known(s["waymask"][:2])
        and all(int(value) != 0 for value in s["waymask"][:2]),
        evidence,
    )
    start_offset0 = (
        None if s["start_vaddr"][0] is None else int(s["start_vaddr"][0]) & 0x3F
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_sram",
        "single_line_bank_range",
        cycle,
        _on(s["s1_valid"])
        and _off(s["cross0"])
        and start_offset0 is not None
        and start_offset0 >= 8,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_sram",
        "cross_line_bank_mapping",
        cycle,
        _on(s["s1_valid"])
        and _on(s["cross0"])
        and start_offset0 is not None
        and 0 < start_offset0 < 64
        and _known(s["sram_valid"][:2])
        and _bits(s["sram_valid"][:2]) == (1, 1),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_sram",
        "dual_request_independent",
        cycle,
        _on(s["s1_valid"])
        and _on(s["req1_valid"])
        and _known(s["waymask"])
        and (
            s["waymask"][:2] != s["waymask"][2:]
            or _on(s["cross0"]) != _on(s["cross1"])
        ),
        evidence,
    )

    _mark(
        recorder,
        "icache_mainpipe_s1_backpressure",
        "hit_response_stall",
        cycle,
        _on(s["s1_valid"])
        and _on(s["fetch_finish"])
        and _off(s["toifu_ready"])
        and _off(s["s1_flush"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_backpressure",
        "refill_completion_stall",
        cycle,
        refill_match
        and _off(s["toifu_ready"])
        and _off(s["s1_flush"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_backpressure",
        "pending_miss_blocks_upstream",
        cycle,
        pending_miss,
        evidence,
    )

    _mark(
        recorder,
        "icache_mainpipe_s1_flush",
        "global_flush_clears_s1",
        cycle,
        _on(s["s1_valid"])
        and global_s1_flush,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_flush",
        "bpu_match_clears_s1",
        cycle,
        _on(s["s1_valid"])
        and bpu_s1_flush
        and _off(s["io_flush"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_flush",
        "bpu_miss_keeps_s1",
        cycle,
        _on(s["s1_valid"])
        and _on(s["bpu_valid"])
        and _off(s["io_flush"])
        and _off(s["s1_flush"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_flush",
        "flush_while_miss_backpressured",
        cycle,
        pending_miss
        and _off(s["miss_req_ready"])
        and _on(s["s1_flush"]),
        evidence,
    )
    late_refill_condition = state["late_refill_pending"] and _on(s["miss_resp_valid"])
    _mark(
        recorder,
        "icache_mainpipe_s1_flush",
        "late_refill_ignored_after_flush",
        cycle,
        late_refill_condition,
        evidence,
    )
    if late_refill_condition:
        state["late_refill_pending"] = False
    if _on(s["s1_flush"]) and any(has_send):
        state["late_refill_pending"] = True
    _mark(
        recorder,
        "icache_mainpipe_s1_flush",
        "flush_wins_matching_refill",
        cycle,
        _on(s["s1_flush"])
        and refill_match,
        evidence,
    )

    clean_refill = (
        refill_match
        and _off(s["miss_resp_corrupt"])
        and _off(s["miss_resp_denied"])
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_refill",
        "clean_refill_match",
        cycle,
        clean_refill,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_refill",
        "nonmatching_refill_ignored",
        cycle,
        _on(s["s1_valid"])
        and _on(s["miss_resp_valid"])
        and _known(s["mshr"])
        and not any(mshr),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_refill",
        "corrupt_refill_saved",
        cycle,
        refill_match
        and (_on(s["miss_resp_corrupt"]) or _on(s["miss_resp_denied"])),
        evidence,
    )
    if _on(s["s1_valid"]) and _on(s["cross0"]) and should[0:2] == (1, 1):
        state["split_refill_pending"] = True
        state["split_refill_lines"].clear()
    if state["split_refill_pending"] and _on(s["miss_resp_valid"]):
        state["split_refill_lines"].update(
            index for index, matched in enumerate(mshr[0:2]) if matched
        )
    split_done = state["split_refill_pending"] and len(state["split_refill_lines"]) == 2
    _mark(
        recorder,
        "icache_mainpipe_s1_refill",
        "cross_line_split_refill",
        cycle,
        split_done,
        evidence,
    )
    if split_done:
        state["split_refill_pending"] = False
        state["split_refill_lines"].clear()
    if _on(s["s1_flush"]):
        state["split_refill_pending"] = False
        state["split_refill_lines"].clear()
    _mark(
        recorder,
        "icache_mainpipe_s1_refill",
        "refill_request_line_selective",
        cycle,
        _on(s["req1_valid"])
        and refill_match
        and 0 < sum(mshr) < sum(line_valid),
        evidence,
    )

    _mark(
        recorder,
        "icache_mainpipe_s1_miss",
        "single_line_miss_request",
        cycle,
        _on(s["s1_valid"])
        and line_valid == (1, 0, 0, 0)
        and should == (1, 0, 0, 0)
        and _on(s["miss_req_ready"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_miss",
        "four_line_fixed_priority",
        cycle,
        should == (1, 1, 1, 1) and _on(s["miss_req_ready"]),
        evidence,
    )
    recovered_from_miss_backpressure = (
        state["miss_backpressure_seen"]
        and _on(s["s1_valid"])
        and any(should)
        and _on(s["miss_req_ready"])
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_miss",
        "missunit_backpressure_stable",
        cycle,
        recovered_from_miss_backpressure,
        evidence,
    )
    if _on(s["s1_valid"]) and any(should) and _off(s["miss_req_ready"]):
        state["miss_backpressure_seen"] = True
    elif recovered_from_miss_backpressure or _on(s["s1_flush"]):
        state["miss_backpressure_seen"] = False
    _mark(
        recorder,
        "icache_mainpipe_s1_miss",
        "has_send_no_duplicate",
        cycle,
        _on(s["s1_valid"]) and any(has_send) and _off(s["miss_resp_valid"]),
        evidence,
    )
    invalid_line_configured = _known(s["waymask"]) and any(
        not valid and int(s["waymask"][index] or 0) == 0
        for index, valid in enumerate(line_valid)
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_miss",
        "invalid_line_no_miss",
        cycle,
        _on(s["s1_valid"]) and invalid_line_configured,
        evidence,
    )

    _mark(
        recorder,
        "icache_mainpipe_s1_protection",
        "pmp_exec_check",
        cycle,
        _on(s["s1_valid"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_protection",
        "itlb_over_pmp_priority",
        cycle,
        _on(s["itlb_exception"])
        and _on(s["pmp_instr"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_protection",
        "pmp_exception_suppresses_miss",
        cycle,
        _on(s["s1_valid"])
        and _off(s["itlb_exception"])
        and _on(s["pmp_instr"])
        and any(
            valid and not hit
            for valid, hit in zip(line_valid, hits)
        ),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_protection",
        "mmio_pbmt_suppresses_refill",
        cycle,
        _on(s["s1_valid"])
        and (_on(s["pmp_mmio"]) or s["pbmt"] in (1, 2))
        and any(valid and not hit for valid, hit in zip(line_valid, hits)),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_protection",
        "tl_error_to_exception",
        cycle,
        refill_match
        and (_on(s["miss_resp_corrupt"]) or _on(s["miss_resp_denied"])),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_protection",
        "dual_request_shared_protection",
        cycle,
        _on(s["s1_valid"])
        and _on(s["req1_valid"])
        and (
            _on(s["itlb_exception"])
            or _on(s["pmp_instr"])
            or _on(s["pmp_mmio"])
            or s["pbmt"] in (1, 2)
        ),
        evidence,
    )

    prior_s1_fire = bool(prev) and _on(prev["s1_fire"])
    _mark(
        recorder,
        "icache_mainpipe_s2_ecc",
        "s1_fire_latches_s2",
        cycle,
        _on(s["s1_fire"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s2_ecc",
        "ecc_disabled_masks_errors",
        cycle,
        prior_s1_fire
        and _off(s["ecc_enable"])
        and _off(local_ecc_enable),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s2_ecc",
        "forced_ecc_when_disabled",
        cycle,
        prior_s1_fire
        and _off(s["ecc_enable"])
        and _on(local_ecc_enable),
        evidence,
    )
    meta_hitnum = _bits(s["s2_meta_hitnum"])
    meta_error = _on(s["error_meta"]) or (
        _on(s["error_valid"]) and any(value > 1 for value in meta_hitnum)
    )
    data_error = _on(s["error_data"]) or (
        _on(s["error_valid"])
        and _off(s["error_meta"])
        and any(_bits(s["s2_bank_sram"]))
    )
    _mark(
        recorder,
        "icache_mainpipe_s2_ecc",
        "meta_code_or_multiway_corrupt",
        cycle,
        prior_s1_fire and _on(local_ecc_enable) and meta_error,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s2_ecc",
        "data_ecc_selected_bank_only",
        cycle,
        prior_s1_fire
        and _on(local_ecc_enable)
        and data_error
        and any(_bits(s["s2_sram_hits"])),
        evidence,
    )
    prior_mshr_bank = bool(prev) and any(_bits(prev["bank_mshr"]))
    _mark(
        recorder,
        "icache_mainpipe_s2_ecc",
        "mshr_bypass_skips_data_ecc",
        cycle,
        prior_s1_fire
        and prior_mshr_bank,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s2_ecc",
        "corrupt_sideband_per_line",
        cycle,
        prior_s1_fire and (meta_error or data_error),
        evidence,
    )
    if prior_s1_fire and _on(s["io_flush"]):
        state["s2_global_flush_seen"] = True
    if (
        prior_s1_fire
        and _on(s["bpu_valid"])
        and _off(s["io_flush"])
        and _off(s["s0_flush"])
        and _off(s["s1_flush"])
    ):
        state["s2_bpu_only_seen"] = True
    _mark(
        recorder,
        "icache_mainpipe_s2_ecc",
        "global_flush_clears_s2_bpu_does_not",
        cycle,
        state["s2_global_flush_seen"] and state["s2_bpu_only_seen"],
        evidence,
    )

    _mark(
        recorder,
        "icache_mainpipe_s2_meta_flush",
        "first_corrupt_request_only",
        cycle,
        prior_s1_fire
        and sum(corrupt) > 1,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s2_meta_flush",
        "meta_error_flush_all_ways",
        cycle,
        prior_s1_fire
        and meta_error,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s2_meta_flush",
        "data_error_flush_hit_way",
        cycle,
        prior_s1_fire
        and data_error,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s2_meta_flush",
        "corrupt_refetch_disabled_no_flush",
        cycle,
        prior_s1_fire
        and any(corrupt),
        evidence,
    )

    state["prev"] = s


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
        ("icache_prefetchpipe_s0_entry", "bpu_flush_match_only"),
        ("icache_prefetchpipe_s0_entry", "soft_ignores_bpu_flush"),
        ("icache_prefetchpipe_s0_entry", "soft_priority_over_ftq"),
        ("icache_prefetchpipe_s0_entry", "multi_soft_single_accept"),
        ("icache_prefetchpipe_s1_meta", "flush_cancels_itlb_wait"),
        ("icache_prefetchpipe_s1_meta", "clean_refill_updates_meta"),
        ("icache_prefetchpipe_s1_meta", "same_way_new_tag_invalidates_old"),
        ("icache_prefetchpipe_s1_meta", "four_dual_layouts"),
        ("icache_prefetchpipe_s1_meta", "conflict_first_request_only"),
        ("icache_prefetchpipe_s1_meta", "soft_probe_no_waylookup_ftq"),
        ("icache_prefetchpipe_s1_completion", "prefetch_disabled_no_s2"),
        ("icache_prefetchpipe_s1_completion", "prefetch_enabled_enters_s2"),
        ("icache_prefetchpipe_s2_miss", "sram_or_clean_mshr_hit"),
        ("icache_prefetchpipe_s2_miss", "corrupt_refill_reprefetch"),
        ("icache_prefetchpipe_s2_miss", "exception_or_mmio_suppresses"),
        ("icache_prefetchpipe_s2_miss", "single_line_miss_request"),
        ("icache_prefetchpipe_s2_miss", "dual_line_miss_requests"),
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
    "itlb_flush": (_PREFETCH + "io_itlbFlushPipe",),
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
    "global_flush": (_ICACHE + "__Vtogcov__io_fromFtq_redirectFlush",),
    "bpu_valid": (_ICACHE + "__Vtogcov__io_fromFtq_flushFromBpu_s3_valid",),
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
    "refill_vset": (_MAIN + "__Vtogcov__io_missResp_bits_vSetIdx",),
    "refill_corrupt": (_MAIN + "__Vtogcov__io_missResp_bits_corrupt",),
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
}


def _read_prefetch(recorder, key: str) -> Optional[int]:
    dut = getattr(getattr(recorder, "env", None), "dut", None)
    if dut is None:
        return None
    return recorder._read_first_dut_signal(dut, _PREFETCH_SIGNALS[key])


def reset_icache_prefetchpipe_coverage_state(recorder) -> None:
    recorder._icache_prefetchpipe_cov_state = {
        "bpu_match_seen": False,
        "bpu_miss_seen": False,
        "dual_layouts": set(),
        "redirect_ready": set(),
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


def sample_icache_prefetchpipe_coverage(recorder, env, cycle: int) -> None:
    del env
    state = getattr(recorder, "_icache_prefetchpipe_cov_state", None)
    if state is None:
        reset_icache_prefetchpipe_coverage_state(recorder)
        state = recorder._icache_prefetchpipe_cov_state

    s = {key: _read_prefetch(recorder, key) for key in _PREFETCH_SIGNALS}
    evidence = {key: value for key, value in s.items() if value is not None}
    hw_request = _on(s["from_valid"]) and _off(s["from_soft"])
    soft_request = _on(s["from_valid"]) and _on(s["from_soft"])
    entry_resources_ready = _on(s["s1_ready"]) and _on(s["meta_ready"])

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
        "itlb_or_meta_backpressure",
        cycle,
        hw_request
        and _on(s["s1_ready"])
        and _off(s["meta_ready"])
        and _off(s["global_flush"]),
        evidence,
    )
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
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s0_entry",
        "redirect_flush_blocks_hw",
        cycle,
        hw_request and _on(s["global_flush"]) and entry_resources_ready,
        evidence,
    )

    bpu_comparison = (
        hw_request
        and _on(s["bpu_valid"])
        and _off(s["global_flush"])
        and entry_resources_ready
    )
    if bpu_comparison and _on(s["from_ready"]):
        state["bpu_miss_seen"] = True
    if bpu_comparison and _off(s["from_ready"]):
        state["bpu_match_seen"] = True
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s0_entry",
        "bpu_flush_match_only",
        cycle,
        state["bpu_match_seen"] and state["bpu_miss_seen"],
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
        and _on(s["itlb_flush"]),
        evidence,
    )
    set_values = {value for value in (s["s1_set0"], s["s1_set1"]) if value is not None}
    refill_set_match = s["refill_vset"] is not None and s["refill_vset"] in set_values
    clean_refill = (
        _on(s["s1_valid"])
        and _on(s["refill_valid"])
        and _off(s["refill_corrupt"])
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
    old_waymask = 0
    for key in ("s1_old_way0", "s1_old_way1"):
        if s[key] is not None:
            old_waymask |= int(s[key])
    same_way_new_tag = (
        clean_refill
        and _off(s["s1_ptag_same"])
        and s["refill_waymask"] is not None
        and (old_waymask & int(s["refill_waymask"])) != 0
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "same_way_new_tag_invalidates_old",
        cycle,
        same_way_new_tag,
        evidence,
    )
    if _on(s["s1_valid"]) and s["s1_two_case"] in (1, 2, 4, 8):
        state["dual_layouts"].add(int(s["s1_two_case"]))
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s1_meta",
        "four_dual_layouts",
        cycle,
        state["dual_layouts"] == {1, 2, 4, 8},
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
        _on(s["s1_valid"])
        and _on(s["s1_soft"])
        and _on(s["s1_tlb_finish"]),
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

    any_s2_hit = any(
        _on(s[key]) for key in ("s2_sram0", "s2_sram1", "s2_mshr0", "s2_mshr1")
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "sram_or_clean_mshr_hit",
        cycle,
        _on(s["s2_valid"]) and any_s2_hit,
        evidence,
    )
    s2_set_values = {
        value for value in (s["s2_set0"], s["s2_set1"]) if value is not None
    }
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "corrupt_refill_reprefetch",
        cycle,
        _on(s["s2_valid"])
        and _on(s["refill_valid"])
        and _on(s["refill_corrupt"])
        and s["refill_vset"] in s2_set_values
        and (_on(s["s2_miss0"]) or _on(s["s2_miss1"])),
        evidence,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "exception_or_mmio_suppresses",
        cycle,
        _on(s["s2_valid"])
        and (_on(s["s2_exception"]) or _on(s["s2_mmio"])),
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
    if (
        _on(s["s2_valid"])
        and _on(s["global_flush"])
        and (_on(s["s2_miss0"]) or _on(s["s2_miss1"]))
        and s["miss_ready"] is not None
    ):
        state["redirect_ready"].add(int(_on(s["miss_ready"])))
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "redirect_flush_ready_boundary",
        cycle,
        state["redirect_ready"] == {0, 1},
        evidence,
    )
    _mark_prefetch(
        recorder,
        "icache_prefetchpipe_s2_miss",
        "bpu_flush_keeps_s2",
        cycle,
        _on(s["s2_valid"])
        and _on(s["bpu_valid"])
        and _off(s["global_flush"]),
        evidence,
    )
