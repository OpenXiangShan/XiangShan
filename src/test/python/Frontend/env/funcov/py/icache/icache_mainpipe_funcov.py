from __future__ import annotations

from typing import Any, Iterable, Optional


_MAIN = "Frontend_top.Frontend.inner_icache.mainPipe."
_ICACHE = "Frontend_top.Frontend.inner_icache."
_DATA_BANKS = 8

_S1_CROSS = (
    (_MAIN + "s1_req_0_isCrossLine", _MAIN + "accessTrace_crossLine"),
    (_MAIN + "s1_req_1_isCrossLine", _MAIN + "s1_isCrossLine_1"),
)
_S1_HITS = tuple(
    (
        _MAIN + f"s1_hits_{req}_{line}",
        _MAIN + "s1_hits_r" + (f"_{req * 2 + line}" if req * 2 + line else ""),
    )
    for req in range(2)
    for line in range(2)
)


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
}


ICACHE_MAINPIPE_SAMPLER_BIN_KEYS = frozenset(
    {
        ("icache_mainpipe_s0_entry", "single_request_latched"),
        ("icache_mainpipe_s0_entry", "ftq_waylookup_skew"),
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
        ("icache_mainpipe_s1_flush", "late_refill_ignored_after_flush"),
        ("icache_mainpipe_s1_flush", "flush_wins_matching_refill"),
        ("icache_mainpipe_s1_flush", "flush_cancels_registered_refill"),
        ("icache_mainpipe_s1_refill", "clean_refill_match"),
        ("icache_mainpipe_s1_refill", "nonmatching_refill_ignored"),
        ("icache_mainpipe_s1_refill", "corrupt_refill_saved"),
        ("icache_mainpipe_s1_refill", "cross_line_split_refill"),
        ("icache_mainpipe_s1_refill", "refill_request_line_selective"),
        ("icache_mainpipe_s1_refill", "error_state_cleared_on_new_request"),
        ("icache_mainpipe_s1_miss", "four_line_fixed_priority"),
        ("icache_mainpipe_s1_miss", "missunit_backpressure_stable"),
        ("icache_mainpipe_s1_miss", "has_send_no_duplicate"),
        ("icache_mainpipe_s1_miss", "invalid_line_no_miss"),
        ("icache_mainpipe_s1_protection", "itlb_over_pmp_priority"),
        ("icache_mainpipe_s1_protection", "pmp_exception_suppresses_miss"),
        ("icache_mainpipe_s1_protection", "mmio_pbmt_suppresses_refill"),
        ("icache_mainpipe_s1_protection", "tl_error_to_exception"),
        ("icache_mainpipe_s1_protection", "dual_request_shared_protection"),
        ("icache_mainpipe_s2_ecc", "meta_code_or_multiway_corrupt"),
        ("icache_mainpipe_s2_ecc", "data_ecc_selected_bank_only"),
        ("icache_mainpipe_s2_ecc", "mshr_bypass_skips_data_ecc"),
        ("icache_mainpipe_s2_ecc", "corrupt_sideband_per_line"),
        ("icache_mainpipe_s2_ecc", "global_flush_clears_s2_bpu_does_not"),
    }
)


_SIGNALS = {
    "ftq_valid": (_MAIN + "io_fromFtq_valid",),
    "ftq_ready": (_MAIN + "io_fromFtq_ready",),
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
    "io_flush": (
        _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush",
    ),
    "s0_flush": (_MAIN + "s0_flush", _MAIN + "__Vtogcov__s0_flush"),
    "bpu_valid": (_MAIN + "io_flushFromBpu_s3_valid",),
    "s1_ready": (_MAIN + "s1_ready", _MAIN + "__Vtogcov__s1_ready"),
    "s1_valid": (_MAIN + "s1_valid", _MAIN + "__Vtogcov__s1_valid"),
    "s1_flush": (_MAIN + "s1_flush", _MAIN + "__Vtogcov__s1_flush"),
    "req1_valid": (_MAIN + "s1_req_1_valid",),
    "cross0": _S1_CROSS[0],
    "cross1": _S1_CROSS[1],
    "toifu_valid": (_MAIN + "io_toIfu_req_valid",),
    "toifu_ready": (_MAIN + "io_toIfu_req_ready",),
    "s1_fire": (_MAIN + "s1_fire",),
    "fetch_finish": (_MAIN + "io_toIfu_req_valid",),
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
}

_EVIDENCE_SCALARS = frozenset(
    {
        "ftq_valid",
        "ftq_ready",
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


def _read_candidates(recorder, candidates: Iterable[Iterable[str]]) -> tuple[Optional[int], ...]:
    dut = getattr(getattr(recorder, "env", None), "dut", None)
    if dut is None:
        return tuple(None for _ in candidates)
    return tuple(recorder._read_first_dut_signal(dut, tuple(names)) for names in candidates)


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


def _all_zero(values: Iterable[Optional[int]]) -> bool:
    values = tuple(values)
    return _known(values) and all(int(value) == 0 for value in values)


def _s1_bank_sram_names(req: int, bank: int) -> tuple[str, ...]:
    if bank < _DATA_BANKS - 1:
        return (_MAIN + f"s1_bankSramValid_{req}_{bank}",)
    if req == 0:
        return (_MAIN + "s1_sramRespValid",)
    return (_MAIN + "s1_sramValid_1_0",)


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
        "error_refill_pending": False,
        "s2_global_flush_seen": False,
        "s2_bpu_only_seen": False,
        "ftq_waylookup_skew_pending": False,
        "ftq_waylookup_join_pending": False,
    }


def _snapshot(recorder) -> dict[str, Any]:
    scalar = {key: _read(recorder, key) for key in _SIGNALS}
    scalar.update(
        {
            "hits": _read_candidates(recorder, _S1_HITS),
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
                    name
                    for req in range(2)
                    for bank in range(_DATA_BANKS)
                    for name in _s1_bank_sram_names(req, bank)
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
                    for bank in range(_DATA_BANKS)
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
                    for bank in range(_DATA_BANKS)
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
    if local_ecc_enable is None:
        # The current ICache control unit defaults ECC to enabled, but the
        # generated Verilator inventory drops the internal enable net.
        local_ecc_enable = 1
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
    prior_refill_match = bool(prev) and _on(prev["miss_resp_valid"]) and any(
        _bits(prev["mshr"])
    )
    global_s0_flush = _on(s["s0_flush"]) and _off(s["bpu_valid"])
    bpu_s0_flush = _on(s["s0_flush"]) and _on(s["bpu_valid"])
    global_s1_flush = _on(s["s1_flush"]) and _off(s["bpu_valid"])
    bpu_s1_flush = _on(s["s1_flush"]) and _on(s["bpu_valid"])
    ftq_fire = _on(s["ftq_valid"]) and _on(s["ftq_ready"])
    from_fire = _on(s["from_valid"]) and _on(s["from_ready"])
    data_fire = _on(s["data_valid"]) and _on(s["data_ready"])

    # Most bins sample the testpoint Condition and keep Checkpoint signals as
    # evidence.  Bins that explicitly describe a temporal handshake, such as
    # ftq_waylookup_skew, track the required sequence in recorder state.
    s0_accept_condition = (
        _on(s["from_valid"])
        and _on(s["data_ready"])
        and _on(s["s1_ready"])
        and _off(s["s0_flush"])
    )

    if global_s0_flush:
        state["ftq_waylookup_skew_pending"] = False
        state["ftq_waylookup_join_pending"] = False
    _mark(
        recorder,
        "icache_mainpipe_s0_entry",
        "ftq_waylookup_skew",
        cycle,
        bool(state["ftq_waylookup_join_pending"]) and _on(s["s1_valid"]),
        {**evidence, "skew_join_observed_prev_cycle": True},
    )
    state["ftq_waylookup_join_pending"] = False
    skew_condition = (
        _known((s["ftq_valid"], s["from_valid"]))
        and (_on(s["ftq_valid"]) != _on(s["from_valid"]))
        and (_on(s["ftq_valid"]) or _on(s["from_valid"]))
        and _off(s["s0_flush"])
        and not ftq_fire
        and not from_fire
    )
    if skew_condition:
        state["ftq_waylookup_skew_pending"] = True
    if (
        state["ftq_waylookup_skew_pending"]
        and ftq_fire
        and from_fire
        and data_fire
        and _off(s["s0_flush"])
    ):
        state["ftq_waylookup_join_pending"] = True
        state["ftq_waylookup_skew_pending"] = False

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
    line_valid_bits = tuple(line_valid)
    req0_line_valid = line_valid_bits[:2]
    req1_line_valid = line_valid_bits[2:]
    req0_hits = hits[:2]
    req1_hits = hits[2:]
    req0_should = should[:2]
    req1_should = should[2:]
    single_line_hit_complete = (
        _on(s["fetch_finish"])
        and _off(s["miss_req_valid"])
        and _all_zero(s["should"])
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
        and _known(s["hits"][:1])
        and hits[0] == 1
        and single_line_hit_complete
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
        and all(int(value) != 0 for value in s["waymask"][:2])
        and _known(s["hits"][:2])
        and hits[:2] == (1, 1)
        and single_line_hit_complete,
        evidence,
    )
    start_offset0 = (
        None if s["start_vaddr"][0] is None else int(s["start_vaddr"][0]) & 0x3F
    )
    bank_sram0 = s["bank_sram"][:_DATA_BANKS]
    start_bank0 = None if start_offset0 is None else (start_offset0 & 0x1C) >> 2
    expected_single_line_banks = (
        ()
        if start_bank0 is None
        else tuple(1 if bank >= start_bank0 else 0 for bank in range(_DATA_BANKS))
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_sram",
        "single_line_bank_range",
        cycle,
        _on(s["s1_valid"])
        and _off(s["cross0"])
        and start_offset0 is not None
        and start_offset0 >= 8
        and _known(bank_sram0)
        and _bits(bank_sram0) == expected_single_line_banks,
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
        and _bits(s["sram_valid"][:2]) == (1, 1)
        and _known(bank_sram0)
        and _bits(bank_sram0) == tuple(1 for _ in range(_DATA_BANKS)),
        evidence,
    )
    req0_has_valid_hit = any(
        valid and hit for valid, hit in zip(req0_line_valid, req0_hits)
    )
    req1_has_valid_hit = any(
        valid and hit for valid, hit in zip(req1_line_valid, req1_hits)
    )
    no_invalid_line_hit = all(
        valid or not hit for valid, hit in zip(line_valid_bits, hits)
    )
    pending_line_blocks_finish = (
        not any(should)
        or (_known((s["fetch_finish"],)) and _off(s["fetch_finish"]))
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_sram",
        "dual_request_independent",
        cycle,
        _on(s["s1_valid"])
        and _on(s["req1_valid"])
        and _known(s["waymask"])
        and _known(s["hits"])
        and _known(s["should"])
        and (
            s["waymask"][:2] != s["waymask"][2:]
            or req0_line_valid != req1_line_valid
            or req0_hits != req1_hits
            or req0_should != req1_should
        )
        and req0_has_valid_hit
        and req1_has_valid_hit
        and no_invalid_line_hit
        and pending_line_blocks_finish,
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
        and bpu_s1_flush,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_flush",
        "bpu_miss_keeps_s1",
        cycle,
        _on(s["s1_valid"])
        and _on(s["bpu_valid"])
        and _off(s["s1_flush"]),
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
    _mark(
        recorder,
        "icache_mainpipe_s1_flush",
        "flush_cancels_registered_refill",
        cycle,
        prior_refill_match and _on(s["s1_flush"]),
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
    if prior_s1_fire and global_s1_flush:
        state["s2_global_flush_seen"] = True
    if (
        prior_s1_fire
        and _on(s["bpu_valid"])
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

    error_refill = refill_match and (
        _on(s["miss_resp_corrupt"]) or _on(s["miss_resp_denied"])
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_refill",
        "error_state_cleared_on_new_request",
        cycle,
        state["error_refill_pending"]
        and _on(s["s1_fire"])
        and _off(s["s1_flush"]),
        evidence,
    )
    if state["error_refill_pending"] and (
        _on(s["s1_fire"]) or _on(s["s1_flush"])
    ):
        state["error_refill_pending"] = False
    if error_refill:
        state["error_refill_pending"] = True

    state["prev"] = s
