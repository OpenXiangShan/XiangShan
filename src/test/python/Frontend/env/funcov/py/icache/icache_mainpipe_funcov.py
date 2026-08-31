from __future__ import annotations

from typing import Any, Iterable, Optional

from .flush_from_bpu import (
    BpuS3Flush,
    ftq_ptr_is_strictly_after_current,
    ftq_ptr_matches_or_before,
)


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
        ("icache_mainpipe_s1_flush", "global_flush_clears_s1_hit"),
        ("icache_mainpipe_s1_flush", "global_flush_clears_s1_pending_miss"),
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
        ("icache_mainpipe_s1_miss", "four_line_fixed_priority"),
        ("icache_mainpipe_s1_miss", "missunit_backpressure_stable"),
        ("icache_mainpipe_s1_miss", "has_send_no_duplicate"),
        ("icache_mainpipe_s1_miss", "invalid_line_no_miss"),
        ("icache_mainpipe_s1_protection", "itlb_over_pmp_priority"),
        ("icache_mainpipe_s1_protection", "pmp_exception_suppresses_miss"),
        ("icache_mainpipe_s1_protection", "pmp_mmio_suppresses_refill"),
        ("icache_mainpipe_s1_protection", "pbmt_uncache_suppresses_refill"),
        ("icache_mainpipe_s1_protection", "tl_error_to_exception"),
        ("icache_mainpipe_s2_ecc", "meta_code_mismatch_single_way"),
        ("icache_mainpipe_s2_ecc", "meta_multiway_hit"),
        ("icache_mainpipe_s2_ecc", "meta_code_mismatch_zero_way_ignored"),
        ("icache_mainpipe_s2_ecc", "meta_invalid_line_masked"),
        ("icache_mainpipe_s2_ecc", "data_ecc_selected_valid_sram_bank"),
        ("icache_mainpipe_s2_ecc", "data_ecc_unselected_bank_ignored"),
        ("icache_mainpipe_s2_ecc", "data_ecc_mshr_bypass_skips_sram_bank"),
        ("icache_mainpipe_s2_ecc", "data_ecc_port_miss_ignored"),
        ("icache_mainpipe_s2_ecc", "global_flush_clears_s2"),
        ("icache_mainpipe_s2_ecc", "bpu_s3_flush_keeps_s2"),
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
    "bpu_flag": (
        _ICACHE + "__Vtogcov__io_fromFtq_flushFromBpu_s3_bits_flag",
        _MAIN + "io_flushFromBpu_s3_bits_flag",
    ),
    "bpu_value": (
        _ICACHE + "__Vtogcov__io_fromFtq_flushFromBpu_s3_bits_value",
        _MAIN + "io_flushFromBpu_s3_bits_value",
    ),
    "s1_ready": (_MAIN + "s1_ready", _MAIN + "__Vtogcov__s1_ready"),
    "s0_fire": (_MAIN + "s0_fire", _MAIN + "__Vtogcov__s0_fire"),
    "s1_valid": (_MAIN + "s1_valid", _MAIN + "__Vtogcov__s1_valid"),
    "s1_flush": (_MAIN + "s1_flush", _MAIN + "__Vtogcov__s1_flush"),
    "s0_ftq_flag": (
        _MAIN + "io_fromFtq_bits_req_0_ftqIdx_flag",
        _ICACHE + "__Vtogcov__io_fromFtq_toMainPipe_bits_req_0_ftqIdx_flag",
    ),
    "s0_ftq_value": (
        _MAIN + "io_fromFtq_bits_req_0_ftqIdx_value",
        _ICACHE + "__Vtogcov__io_fromFtq_toMainPipe_bits_req_0_ftqIdx_value",
    ),
    "s1_ftq_flag": (
        _MAIN + "s1_req_0_ftqIdx_flag",
        _MAIN + "__Vtogcov__s1_req_0_ftqIdx_flag",
    ),
    "s1_ftq_value": (
        _MAIN + "s1_req_0_ftqIdx_value",
        _MAIN + "__Vtogcov__s1_req_0_ftqIdx_value",
    ),
    "req1_valid": (_MAIN + "s1_req_1_valid",),
    "backend_exception": (_MAIN + "s1_req_0_hasBackendException",),
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
    "s1_pTag": (_MAIN + "s1_pTag", _MAIN + "__Vtogcov__s1_pTag"),
    "miss_resp_corrupt": (_MAIN + "__Vtogcov__io_missResp_bits_corrupt",),
    "miss_resp_denied": (_MAIN + "__Vtogcov__io_missResp_bits_denied",),
    "mshr_reg": tuple(
        _MAIN + f"s1_mshrValidReg_{req}_{line}"
        for req in range(2)
        for line in range(2)
    ),
    "pmp_instr": (_MAIN + "io_pmp_resp_instr",),
    "pmp_mmio": (_MAIN + "io_pmp_resp_mmio",),
    "itlb_exception": (
        _MAIN + "s1_exceptionInfo_0_itlbException_value",
        _MAIN + "__Vtogcov__s1_exceptionInfo_0_itlbException_value",
    ),
    "exception": (_MAIN + "s1_exception_value",),
    "is_mmio": (_MAIN + "s1_isMmio",),
    "pbmt": (_MAIN + "s1_wayLookupEntry_0_itlbPbmt",),
    "s2_valid": (_MAIN + "s2_valid", _MAIN + "__Vtogcov__s2_valid"),
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
        "bpu_flag",
        "bpu_value",
        "s1_ready",
        "s0_fire",
        "s1_valid",
        "s1_flush",
        "s0_ftq_flag",
        "s0_ftq_value",
        "s1_ftq_flag",
        "s1_ftq_value",
        "req1_valid",
        "backend_exception",
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
        "s2_valid",
        "error_valid",
        "error_meta",
        "error_data",
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


def _parity(value: Optional[int]) -> Optional[int]:
    if value is None:
        return None
    return int(value).bit_count() & 1


def _ecc_mismatch(data: Optional[int], code: Optional[int]) -> bool:
    parity = _parity(data)
    return parity is not None and code is not None and parity != (int(code) & 1)


def _meta_ecc_mismatch(
    ptag: Optional[int], maybe_rvc_map: Optional[int], code: Optional[int]
) -> bool:
    tag_parity = _parity(ptag)
    rvc_parity = _parity(maybe_rvc_map)
    return (
        tag_parity is not None
        and rvc_parity is not None
        and code is not None
        and (tag_parity ^ rvc_parity) != (int(code) & 1)
    )


def _s2_valid_lines(prev: Optional[dict[str, Any]], s: dict[str, Any]) -> tuple[bool, ...]:
    if not prev or not _on(prev["s1_fire"]):
        return (False, False, False, False)
    req1_valid = _on(prev["req1_valid"])
    cross = s["s2_cross"]
    return (
        True,
        _on(cross[0]),
        req1_valid,
        req1_valid and _on(cross[1]),
    )


def _s2_bank_selected(offset: Optional[int], line: int, bank: int, cross: bool) -> bool:
    if offset is None:
        return False
    bank_low = (int(offset) >> 3) & (_DATA_BANKS - 1)
    # ICacheMainPipe passes s2_valid as getBankSel's end offset, so the high
    # bank is zero whenever the s2 context is active.
    bank_high = 0
    if line == 0:
        return bank >= bank_low and (cross or bank <= bank_high)
    return cross and bank <= bank_high


def _pure_cache_hit(
    s: dict[str, Any],
    hits: tuple[Optional[int], ...],
    mshr_reg: tuple[Optional[int], ...],
) -> bool:
    """Return true only when every valid fetch line is a non-refill hit."""
    if not _on(s["s1_valid"]) or not _off(s["s1_flush"]):
        return False
    if _on(s["pmp_mmio"]) or _on(s["is_mmio"]):
        return False
    if _on(s["itlb_exception"]) or _on(s["exception"]):
        return False
    if not _known((s["cross0"], s["cross1"], s["req1_valid"])):
        return False
    if not _known(hits) or not _known(mshr_reg):
        return False

    # MainPipe has one mandatory line for request 0; the remaining lines are
    # present only for cross-line/request-1 fetches.
    valid_lines = [True, _on(s["cross0"]), _on(s["req1_valid"]),
                   _on(s["req1_valid"]) and _on(s["cross1"])]
    hit_bits = _bits(hits)
    mshr_reg_bits = _bits(mshr_reg)
    return all(not valid_lines[index] or hit_bits[index] for index in range(4)) and not any(
        mshr_reg_bits[index] for index in range(4)
    )


def _all_valid_lines_cache_hit(
    s: dict[str, Any], hits: tuple[Optional[int], ...]
) -> bool:
    """Return true when every valid s1 line is a cache hit without protection faults."""
    if not _on(s["s1_valid"]) or any(_bits(s["should"])):
        return False
    if not _off(s["pmp_mmio"]) or not _off(s["is_mmio"]):
        return False
    if not _off(s["itlb_exception"]) or not _off(s["exception"]):
        return False
    if not _known((s["cross0"], s["cross1"], s["req1_valid"])):
        return False

    valid_lines = (
        True,
        _on(s["cross0"]),
        _on(s["req1_valid"]),
        _on(s["req1_valid"]) and _on(s["cross1"]),
    )
    return all(not valid or _on(hits[index]) for index, valid in enumerate(valid_lines))


def _last_pending_refill(should: tuple[int, ...], mshr: tuple[int, ...]) -> int | None:
    """Return the sole outstanding line completed by this refill response."""
    pending = [index for index, value in enumerate(should) if value]
    matches = [index for index in pending if mshr[index]]
    if len(pending) == 1 and len(matches) == 1:
        return matches[0]
    return None


def _s1_bank_sram_names(req: int, bank: int) -> tuple[str, ...]:
    if bank < _DATA_BANKS - 1:
        return (_MAIN + f"s1_bankSramValid_{req}_{bank}",)
    if req == 0:
        return (_MAIN + "s1_sramRespValid",)
    return (_MAIN + "s1_sramValid_1_0",)


def _bpu_s3_flush(s: dict[str, Any]) -> BpuS3Flush:
    return BpuS3Flush(
        valid=s.get("bpu_valid"),
        flag=s.get("bpu_flag"),
        value=s.get("bpu_value"),
    )


def _ftq_ptr_from_snapshot(s: dict[str, Any], prefix: str) -> tuple[int, int] | None:
    flag = s.get(f"{prefix}_ftq_flag")
    value = s.get(f"{prefix}_ftq_value")
    if flag is None or value is None:
        return None
    return int(flag), int(value)


def _bpu_flush_matches_or_before_current(
    s: dict[str, Any], prefix: str
) -> bool | None:
    return ftq_ptr_matches_or_before(_bpu_s3_flush(s), _ftq_ptr_from_snapshot(s, prefix))


def _bpu_flush_is_after_current(s: dict[str, Any], prefix: str) -> bool | None:
    return ftq_ptr_is_strictly_after_current(
        _bpu_s3_flush(s), _ftq_ptr_from_snapshot(s, prefix)
    )


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
        "four_line_ready_cycles": 0,
        "miss_backpressure_cycles": 0,
        "ftq_waylookup_skew_pending": False,
        "ftq_waylookup_join_pending": False,
        "refill_completion_pending": None,
        "hit_stall_active": False,
        "registered_refill_pending": None,
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
            "bank_mshr_reg": _read_names(
                recorder,
                tuple(
                    _MAIN + f"s1_bankMshrValidReg_{req}_{bank}"
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
            "s2_meta_maps": _read_names(
                recorder,
                tuple(
                    _MAIN + f"s2_wayLookupEntry_{req}_maybeRvcMap_{line}"
                    for req in range(2)
                    for line in range(2)
                ),
            ),
            "s2_meta_codes": _read_names(
                recorder,
                tuple(
                    _MAIN + f"s2_wayLookupEntry_{req}_metaCodes_{line}"
                    for req in range(2)
                    for line in range(2)
                ),
            ),
            "s2_ptag": _read_names(recorder, (_MAIN + "s2_pTag",)),
            "s2_cross": _read_names(
                recorder,
                tuple(_MAIN + f"s2_isCrossLine_{req}" for req in range(2)),
            ),
            "s2_offset": _read_names(
                recorder,
                tuple(_MAIN + f"s2_offset_{req}" for req in range(2)),
            ),
            "s2_sram_data": _read_names(
                recorder,
                tuple(
                    _MAIN + f"s2_sramDatas_{req}_{bank}"
                    for req in range(2)
                    for bank in range(_DATA_BANKS)
                ),
            ),
            "s2_sram_code": _read_names(
                recorder,
                tuple(
                    _MAIN + f"s2_sramCodes_{req}_{bank}"
                    for req in range(2)
                    for bank in range(_DATA_BANKS)
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
            "mshr_reg": _read_names(
                recorder,
                tuple(
                    _MAIN + f"s1_mshrValidReg_{req}_{line}"
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
    mshr_reg = _bits(s["mshr_reg"])
    has_send = _bits(s["has_send"])
    corrupt = tuple(
        None if value is None else int(value) for value in s["s2_corrupt"]
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
            "mshr_valid_reg": mshr_reg,
            "has_send": has_send,
            "s2_corrupt": corrupt,
        }
    )

    pending_miss = _on(s["s1_valid"]) and any(should)
    refill_match = _on(s["miss_resp_valid"]) and any(mshr)
    s1_vset = s["vset"][0]
    refill_vset = s["miss_resp_vset"]
    vset_mismatch = (
        _known((s1_vset, refill_vset)) and int(s1_vset) != int(refill_vset)
    )
    # A miss-response block address contains pTag followed by the page-local
    # cacheline index.  The ICache has 64-B lines and 4-KiB pages, so removing
    # its six cacheline-index bits yields the pTag compared by checkMshrHitVec.
    ptag_mismatch = (
        _known((s["s1_pTag"], s["miss_resp_paddr"]))
        and (int(s["miss_resp_paddr"]) >> 6) != int(s["s1_pTag"])
    )
    pure_cache_hit = _pure_cache_hit(s, hits, mshr_reg)
    all_valid_lines_cache_hit = _all_valid_lines_cache_hit(s, s["hits"])
    last_pending_line = (
        _last_pending_refill(should, mshr)
        if _on(s["s1_valid"]) and _on(s["miss_resp_valid"])
        else None
    )
    global_s0_flush = _on(s["io_flush"])
    global_s1_flush = _on(s["s1_flush"]) and _off(s["bpu_valid"])
    bpu_s1_flush = _on(s["s1_flush"]) and _on(s["bpu_valid"])
    s0_bpu_match = _bpu_flush_matches_or_before_current(s, "s0")
    s1_bpu_match = _bpu_flush_matches_or_before_current(s, "s1")
    previous_global_s0_candidate = (
        prev is not None
        and _on(prev["ftq_valid"])
        and _on(prev["from_valid"])
        and _on(prev["data_ready"])
        and _on(prev["s1_ready"])
        and _off(prev["io_flush"])
    )
    s0_bpu_miss = _bpu_flush_is_after_current(s, "s0")
    s1_bpu_miss = _bpu_flush_is_after_current(s, "s1")
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
        (
            _on(s["ftq_valid"])
            and _on(s["from_valid"])
            and global_s0_flush
            and _on(s["data_ready"])
            and _on(s["s1_ready"])
        )
        or (
            previous_global_s0_candidate
            and global_s0_flush
            and _off(s["s0_fire"])
        ),
        {
            **evidence,
            "previous_global_s0_candidate": bool(previous_global_s0_candidate),
        },
    )
    _mark(
        recorder,
        "icache_mainpipe_s0_flush",
        "bpu_match_cancels_entry",
        cycle,
        _on(s["ftq_valid"])
        and _on(s["from_valid"])
        and _off(s["io_flush"])
        and _on(s["bpu_valid"])
        and s0_bpu_match is True
        and _on(s["data_ready"])
        and _on(s["s1_ready"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s0_flush",
        "bpu_miss_allows_entry",
        cycle,
        _on(s["ftq_valid"])
        and _on(s["from_valid"])
        and _off(s["io_flush"])
        and _on(s["bpu_valid"])
        and s0_bpu_miss is True
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
    protection_shape_known = _known((s["cross0"], s["req1_valid"])) and (
        not _on(s["req1_valid"]) or s["cross1"] is not None
    )
    valid_line_miss = protection_shape_known and all(
        not valid or hit is not None
        for valid, hit in zip(line_valid, s["hits"])
    ) and any(
        valid and _off(hit)
        for valid, hit in zip(line_valid, s["hits"])
    )
    no_backend_exception = _off(s["backend_exception"])
    req0_hits = hits[:2]
    req1_hits = hits[2:]
    req0_should = should[:2]
    req1_should = should[2:]
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
        and hits[:2] == (1, 1),
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
        and start_bank0 is not None
        and start_bank0 > 0
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
        and _known(s["hits"])
        and _known(s["should"])
        and _known((s["cross0"], s["cross1"]))
        and int(s["cross0"]) != int(s["cross1"])
        and (
            s["waymask"][:2] != s["waymask"][2:]
            or req0_hits != req1_hits
            or req0_should != req1_should
        ),
        evidence,
    )

    hit_stall = (
        pure_cache_hit
        and _on(s["toifu_valid"])
        and _off(s["toifu_ready"])
    )
    hit_stall_start = hit_stall and not state["hit_stall_active"]
    if hit_stall and state["refill_completion_pending"] is None:
        state["hit_stall_active"] = True
    elif not hit_stall:
        state["hit_stall_active"] = False

    _mark(
        recorder,
        "icache_mainpipe_s1_backpressure",
        "hit_response_stall",
        cycle,
        hit_stall_start,
        evidence,
    )

    if last_pending_line is not None:
        state["refill_completion_pending"] = {
            "line": last_pending_line,
            "cycle": cycle,
        }
    refill_pending = state["refill_completion_pending"]
    refill_completion_stall = (
        refill_pending is not None
        and cycle == refill_pending["cycle"] + 1
        and _on(s["s1_valid"])
        and _off(s["s1_flush"])
        and _on(s["fetch_finish"])
        and _on(s["toifu_valid"])
        and _off(s["toifu_ready"])
        and mshr_reg[refill_pending["line"]] == 1
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_backpressure",
        "refill_completion_stall",
        cycle,
        refill_completion_stall,
        evidence,
    )
    if refill_completion_stall or (
        refill_pending is not None
        and (_on(s["s1_flush"]) or not _on(s["s1_valid"]))
    ):
        state["refill_completion_pending"] = None
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
        "global_flush_clears_s1_hit",
        cycle,
        _on(s["io_flush"]) and all_valid_lines_cache_hit,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_flush",
        "global_flush_clears_s1_pending_miss",
        cycle,
        _on(s["io_flush"]) and pending_miss,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_flush",
        "bpu_match_clears_s1",
        cycle,
        _on(s["s1_valid"])
        and _off(s["io_flush"])
        and bpu_s1_flush
        and s1_bpu_match is True,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_flush",
        "bpu_miss_keeps_s1",
        cycle,
        _on(s["s1_valid"])
        and _off(s["io_flush"])
        and _on(s["bpu_valid"])
        and _off(s["s1_flush"])
        and s1_bpu_miss is True,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_flush",
        "late_refill_ignored_after_flush",
        cycle,
        _on(s["io_flush"]) and pending_miss and not refill_match,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_flush",
        "flush_wins_matching_refill",
        cycle,
        _on(s["io_flush"]) and _on(s["s1_valid"]) and refill_match,
        evidence,
    )
    registered_refill = state["registered_refill_pending"]
    _mark(
        recorder,
        "icache_mainpipe_s1_flush",
        "flush_cancels_registered_refill",
        cycle,
        registered_refill is not None
        and cycle == registered_refill["cycle"] + 1
        and _on(s["io_flush"])
        and _on(s["s1_valid"])
        and mshr_reg[registered_refill["line"]] == 1,
        evidence,
    )
    if last_pending_line is not None and _off(s["io_flush"]):
        state["registered_refill_pending"] = {
            "cycle": cycle,
            "line": last_pending_line,
        }
    elif registered_refill is not None and cycle >= registered_refill["cycle"] + 1:
        state["registered_refill_pending"] = None

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
        and (vset_mismatch or ptag_mismatch),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_refill",
        "corrupt_refill_saved",
        cycle,
        refill_match
        and _on(s["miss_resp_corrupt"])
        and _known((s["miss_resp_denied"],)),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_refill",
        "cross_line_split_refill",
        cycle,
        _on(s["s1_valid"])
        and _on(s["cross0"])
        and should[0:2] == (1, 1),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_refill",
        "refill_request_line_selective",
        cycle,
        _on(s["s1_valid"])
        and _on(s["req1_valid"])
        and _known((s["vset"][0], s["vset"][2]))
        and int(s["vset"][0]) != int(s["vset"][2]),
        evidence,
    )

    four_line_ready = (
        _on(s["s1_valid"])
        and _off(s["s1_flush"])
        and _on(s["req1_valid"])
        and _on(s["cross0"])
        and _on(s["cross1"])
        and should == (1, 1, 1, 1)
        and _on(s["miss_req_ready"])
    )
    state["four_line_ready_cycles"] = (
        min(int(state["four_line_ready_cycles"]) + 1, 4)
        if four_line_ready
        else 0
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_miss",
        "four_line_fixed_priority",
        cycle,
        state["four_line_ready_cycles"] >= 4,
        {
            **evidence,
            "four_line_ready_cycles": state["four_line_ready_cycles"],
        },
    )
    miss_backpressure = (
        _on(s["s1_valid"])
        and _off(s["s1_flush"])
        and any(should)
        and _off(s["miss_req_ready"])
    )
    state["miss_backpressure_cycles"] = (
        min(int(state["miss_backpressure_cycles"]) + 1, 2)
        if miss_backpressure
        else 0
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_miss",
        "missunit_backpressure_stable",
        cycle,
        state["miss_backpressure_cycles"] >= 2,
        {
            **evidence,
            "miss_backpressure_cycles": state["miss_backpressure_cycles"],
        },
    )
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
        and _off(s["s1_flush"])
        and _off(s["itlb_exception"])
        and _on(s["pmp_instr"])
        and no_backend_exception
        and valid_line_miss,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_protection",
        "pmp_mmio_suppresses_refill",
        cycle,
        _on(s["s1_valid"])
        and _off(s["s1_flush"])
        and _off(s["exception"])
        and no_backend_exception
        and _on(s["pmp_mmio"])
        and s["pbmt"] == 0
        and valid_line_miss,
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s1_protection",
        "pbmt_uncache_suppresses_refill",
        cycle,
        _on(s["s1_valid"])
        and _off(s["s1_flush"])
        and _off(s["exception"])
        and no_backend_exception
        and _off(s["pmp_mmio"])
        and s["pbmt"] in (1, 2)
        and valid_line_miss,
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
    prior_s1_fire = bool(prev) and _on(prev["s1_fire"])
    s2_valid_observed = s["s2_valid"]
    s2_active = (
        prior_s1_fire
        and _off(s["io_flush"])
        and (s2_valid_observed is None or _on(s2_valid_observed))
    )
    s2_context = s2_active
    valid_lines = _s2_valid_lines(prev, s)
    prior_req1_valid = prev["req1_valid"] if prev else None
    s2_context_evidence = {
        "prior_s1_fire": prior_s1_fire,
        "s2_valid_lines": tuple(valid_lines),
        "s2_prior_req1_valid": prior_req1_valid,
        "s2_cross_lines": tuple(s["s2_cross"]),
        "s2_context_source": (
            "observed_s2_valid"
            if s2_valid_observed is not None
            else "reconstructed_from_prior_s1_fire"
        ),
        "s2_valid_observed": s2_valid_observed,
        "s2_ecc_mode": "static_enabled_in_frontend_rtl",
        "s2_global_flush": s["io_flush"],
    }
    meta_single_way: list[int] = []
    meta_multiway: list[int] = []
    meta_zero_way: list[int] = []
    meta_invalid_line: list[int] = []
    ptag = s["s2_ptag"][0]
    for index, (hitnum, maybe_rvc_map, code) in enumerate(
        zip(s["s2_meta_hitnum"], s["s2_meta_maps"], s["s2_meta_codes"])
    ):
        if hitnum is None:
            continue
        count = int(hitnum)
        mismatch = _meta_ecc_mismatch(ptag, maybe_rvc_map, code)
        if valid_lines[index]:
            if count == 1 and mismatch:
                meta_single_way.append(index)
            elif count > 1:
                meta_multiway.append(index)
            elif count == 0 and mismatch:
                meta_zero_way.append(index)
        elif count > 1 or (count == 1 and mismatch):
            meta_invalid_line.append(index)

    for bin_name, matches in (
        ("meta_code_mismatch_single_way", meta_single_way),
        ("meta_multiway_hit", meta_multiway),
        ("meta_code_mismatch_zero_way_ignored", meta_zero_way),
        ("meta_invalid_line_masked", meta_invalid_line),
    ):
        _mark(
            recorder,
            "icache_mainpipe_s2_ecc",
            bin_name,
            cycle,
            s2_context and bool(matches),
            {
                **evidence,
                **s2_context_evidence,
                "s2_matching_lines": tuple(matches),
                "s2_invalid_line_reasons": tuple(
                    (
                        index,
                        "req_invalid"
                        if index >= 2 and not _on(prior_req1_valid)
                        else "non_cross_line",
                    )
                    for index in matches
                    if not valid_lines[index]
                ),
            },
        )

    selected_valid: list[tuple[int, int, int]] = []
    unselected: list[tuple[int, int, int]] = []
    mshr_bypass: list[tuple[int, int, int]] = []
    port_miss: list[tuple[int, int, int]] = []
    prior_bank_mshr = prev["bank_mshr_reg"] if prev else (None,) * (2 * _DATA_BANKS)
    for req in range(2):
        cross = _on(s["s2_cross"][req])
        for line in range(2):
            line_index = req * 2 + line
            if not valid_lines[line_index]:
                continue
            port_hit = s["s2_sram_hits"][line_index]
            for bank in range(_DATA_BANKS):
                bank_index = req * _DATA_BANKS + bank
                if not _ecc_mismatch(
                    s["s2_sram_data"][bank_index], s["s2_sram_code"][bank_index]
                ):
                    continue
                selected = _s2_bank_selected(s["s2_offset"][req], line, bank, cross)
                bank_valid = s["s2_bank_sram"][bank_index]
                item = (req, line, bank)
                if selected and _on(bank_valid) and _on(port_hit):
                    selected_valid.append(item)
                if not selected and _on(bank_valid) and _on(port_hit):
                    unselected.append(item)
                if selected and _off(bank_valid) and _on(prior_bank_mshr[bank_index]):
                    mshr_bypass.append(item)
                if selected and _on(bank_valid) and _off(port_hit):
                    port_miss.append(item)

    for bin_name, matches in (
        ("data_ecc_selected_valid_sram_bank", selected_valid),
        ("data_ecc_unselected_bank_ignored", unselected),
        ("data_ecc_mshr_bypass_skips_sram_bank", mshr_bypass),
        ("data_ecc_port_miss_ignored", port_miss),
    ):
        _mark(
            recorder,
            "icache_mainpipe_s2_ecc",
            bin_name,
            cycle,
            s2_context and bool(matches),
            {
                **evidence,
                **s2_context_evidence,
                "s2_matching_req_line_banks": tuple(matches),
                "s2_prior_bank_mshr_valid": tuple(prior_bank_mshr),
            },
        )

    _mark(
        recorder,
        "icache_mainpipe_s2_ecc",
        "global_flush_clears_s2",
        cycle,
        _on(s["s2_valid"]) and _on(s["io_flush"]) and _off(s["bpu_valid"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_mainpipe_s2_ecc",
        "bpu_s3_flush_keeps_s2",
        cycle,
        _on(s["s2_valid"]) and _off(s["io_flush"]) and _on(s["bpu_valid"]),
        evidence,
    )

    state["prev"] = s
