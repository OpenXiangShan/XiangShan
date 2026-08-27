"""Cycle predicates for the cacheable ICache hit/miss path leaves."""

from __future__ import annotations

from typing import Any, Iterable, Optional


_TOP = "Frontend_top."
_ICACHE = _TOP + "Frontend.inner_icache."
_MAIN = _ICACHE + "mainPipe."
_MISS = _ICACHE + "missUnit."
_PREFETCH = _ICACHE + "prefetcher."
_NUM_WAYS = 4
_PAGE_CACHELINE_INDEX_BITS = 6

_S1_CROSS = (
    (_MAIN + "s1_req_0_isCrossLine", _MAIN + "accessTrace_crossLine"),
    (_MAIN + "s1_req_1_isCrossLine", _MAIN + "s1_isCrossLine_1"),
)
_S1_HITS = tuple(
    (
        _MAIN + "s1_hits_r" + (f"_{req * 2 + line}" if req * 2 + line else ""),
        _MAIN + f"s1_hits_{req}_{line}",
    )
    for req in range(2)
    for line in range(2)
)
_PREFETCH_META_VALIDS = tuple(
    (
        # The generated wrapper optimizes PrefetchPipe's response ports into
        # the shared MetaArray/ICacheCtrlUnit wires.  CtrlUnit observes the
        # same port-0 metadata response while it is not injecting ECC traffic.
        _ICACHE + f"ctrlUnitOpt.io_metaRead_resp_entries_0_{way}_valid",
        _ICACHE + f"ctrlUnitOpt.__Vtogcov__io_metaRead_resp_entries_0_{way}_valid",
        _PREFETCH + f"io_metaRead_resp_entries_0_{way}_valid",
        _PREFETCH + f"__Vtogcov__io_metaRead_resp_entries_0_{way}_valid",
    )
    for way in range(_NUM_WAYS)
)


ICACHE_HITMISS_COVERPOINTS = {
    "icache_hit_path": "hit_behavior",
    "icache_miss_path": "miss_behavior",
}


ICACHE_HITMISS_SAMPLER_BIN_KEYS = frozenset(
    {
        ("icache_hit_path", "continuous_same_line_sram_hit"),
        ("icache_hit_path", "continuous_cross_line_sram_hit"),
        ("icache_hit_path", "dual_request_independent_hit"),
        ("icache_hit_path", "hit_itlb_exception"),
        ("icache_hit_path", "hit_pmp_exception"),
        ("icache_miss_path", "fetch_hit_prefetch_miss_concurrent"),
        ("icache_miss_path", "fetch_refill_prefetch_hit"),
        ("icache_miss_path", "continuous_fetch_miss_merge"),
        ("icache_miss_path", "plru_victim_on_miss"),
        ("icache_miss_path", "refill_then_fetch_hit"),
    }
)


_SIGNALS = {
    "s1_valid": (_MAIN + "s1_valid", _MAIN + "__Vtogcov__s1_valid"),
    "s1_fire": (_MAIN + "__Vtogcov__s1_fire", _MAIN + "s1_fire"),
    "s1_flush": (_MAIN + "s1_flush", _MAIN + "__Vtogcov__s1_flush"),
    "global_flush": (
        _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush",
        _ICACHE + "io_fromFtq_redirectFlush",
    ),
    "cross0": _S1_CROSS[0],
    "cross1": _S1_CROSS[1],
    "req1_valid": (_MAIN + "s1_req_1_valid",),
    "req0_start": (_MAIN + "s1_req_0_vAddr_0_addr",),
    "req1_start": (_MAIN + "s1_req_1_vAddr_0_addr",),
    "main_s1_ptag": (
        _MAIN + "s1_wayLookupEntry_0_pTag",
        _MAIN + "s1_pTag",
        _MAIN + "__Vtogcov__s1_pTag",
    ),
    "main_s1_vset": (
        _MAIN + "s1_req_0_vSetIdx_0",
        _MAIN + "s1_wayLookupEntry_0_vSetIdx_0",
    ),
    "main_s1_ftq_flag": (
        _MAIN + "s1_req_0_ftqIdx_flag",
        _MAIN + "__Vtogcov__s1_req_0_ftqIdx_flag",
    ),
    "main_s1_ftq_value": (
        _MAIN + "s1_req_0_ftqIdx_value",
        _MAIN + "__Vtogcov__s1_req_0_ftqIdx_value",
    ),
    "main_s1_mmio": (_MAIN + "s1_isMmio", _MAIN + "__Vtogcov__s1_isMmio"),
    "fencei": (_TOP + "io_fencei",),
    "pmp_instr": (_MAIN + "io_pmp_resp_instr",),
    "itlb_exception": (
        _MAIN + "s1_exceptionInfo_0_itlbException_value",
        _MAIN + "__Vtogcov__s1_exceptionInfo_0_itlbException_value",
    ),
    "sram_valid": (
        _MAIN + "s1_sramRespValid",
        _MAIN + "__Vtogcov__s1_sramRespValid",
    ),
    "sram_valid_cross0": (_MAIN + "s1_sramValid_0_1",),
    "sram_valid_req1": (_MAIN + "s1_sramValid_1_0",),
    "sram_valid_req1_cross": (_MAIN + "s1_sramValid_1_1",),
    "victim_req": (
        _MISS + "io_victim_req_valid",
        _MISS + "__Vtogcov__io_victim_req_valid",
    ),
    "victim_way": (_MISS + "__Vtogcov__io_victim_resp_way",),
    "last_fire_next": (_MISS + "lastFireNext", _MISS + "__Vtogcov__lastFireNext"),
    "id_next": (_MISS + "idNext", _MISS + "__Vtogcov__idNext"),
    "fetch_valid": (
        _MAIN + "io_missReq_valid",
        _MAIN + "__Vtogcov__io_missReq_valid",
        _MISS + "fetchDemux.io_in_valid",
        _MISS + "fetchDemux.__Vtogcov__io_in_valid",
    ),
    "fetch_ready": (
        _MAIN + "io_missReq_ready",
        _MAIN + "__Vtogcov__io_missReq_ready",
        _MISS + "fetchDemux.io_in_ready",
        _MISS + "fetchDemux.__Vtogcov__io_in_ready",
    ),
    "fetch_hit": (_MISS + "fetchHit", _MISS + "__Vtogcov__fetchHit"),
    "fetch_paddr": (
        _MAIN + "io_missReq_bits_blkPAddr",
        _MAIN + "__Vtogcov__io_missReq_bits_blkPAddr",
        _ICACHE + "_mainPipe_io_missReq_bits_blkPAddr",
    ),
    "fetch_vset": (
        _MAIN + "io_missReq_bits_vSetIdx",
        _MAIN + "__Vtogcov__io_missReq_bits_vSetIdx",
        _ICACHE + "_mainPipe_io_missReq_bits_vSetIdx",
    ),
    "prefetch_valid": (
        _MISS + "io_prefetchReq_valid",
        _MISS + "__Vtogcov__io_prefetchReq_valid",
        _MISS + "prefetchDemux.io_in_valid",
        _MISS + "prefetchDemux.__Vtogcov__io_in_valid",
    ),
    "prefetch_ready": (
        _MISS + "io_prefetchReq_ready",
        _MISS + "__Vtogcov__io_prefetchReq_ready",
        _MISS + "prefetchDemux.io_in_ready",
        _MISS + "prefetchDemux.__Vtogcov__io_in_ready",
    ),
    "prefetch_hit": (_MISS + "prefetchHit", _MISS + "__Vtogcov__prefetchHit"),
    "prefetch_paddr": (_MISS + "__Vtogcov__io_prefetchReq_bits_blkPAddr",),
    "prefetch_vset": (_MISS + "__Vtogcov__io_prefetchReq_bits_vSetIdx",),
    "d_valid": (_TOP + "auto_inner_icache_client_out_d_valid",),
    "d_opcode": (_TOP + "auto_inner_icache_client_out_d_bits_opcode",),
    "d_source": (_TOP + "auto_inner_icache_client_out_d_bits_source",),
    "d_corrupt": (_TOP + "auto_inner_icache_client_out_d_bits_corrupt",),
    "d_denied": (_TOP + "auto_inner_icache_client_out_d_bits_denied",),
    "corrupt_reg": (_MISS + "corruptReg",),
    "denied_reg": (_MISS + "deniedReg",),
    "prefetch_s1_valid": (
        _PREFETCH + "s1_valid",
        _PREFETCH + "__Vtogcov__s1_valid",
    ),
    "prefetch_s1_soft": (
        _PREFETCH + "s1_isSoftPrefetch",
        _PREFETCH + "__Vtogcov__s1_isSoftPrefetch",
    ),
    "prefetch_s1_sram_valid": (
        _PREFETCH + "s1_sramValid_0",
        _PREFETCH + "__Vtogcov__s1_sramValid_0",
    ),
    "prefetch_s1_sram_hit": (
        # The generated PrefetchPipe exports the selected metadata waymask,
        # while the original scalar s1_sramHits signal is optimized away.
        _PREFETCH + "s1_metaInfo_0_waymask",
        _PREFETCH + "__Vtogcov__s1_metaInfo_0_waymask",
        _PREFETCH + "s1_sramHits_0",
        _PREFETCH + "__Vtogcov__s1_sramHits_0",
    ),
    "prefetch_s1_ptag": (
        _PREFETCH + "s1_pTag",
        _PREFETCH + "__Vtogcov__s1_pTag",
    ),
    "prefetch_s1_vset": (
        _PREFETCH + "s1_readMetaSetIdx_0",
        _PREFETCH + "__Vtogcov__s1_readMetaSetIdx_0",
    ),
    "prefetch_s1_ftq_flag": (_PREFETCH + "s1_ftqIdx_flag",),
    "prefetch_s1_ftq_value": (_PREFETCH + "s1_ftqIdx_value",),
    "prefetch_s2_valid": (
        _PREFETCH + "s2_valid",
        _PREFETCH + "__Vtogcov__s2_valid",
    ),
    "prefetch_s2_fire": (
        _PREFETCH + "s2_fire",
        _PREFETCH + "__Vtogcov__s2_fire",
    ),
    "prefetch_s2_exception": (
        _PREFETCH + "s2_exception_value",
        _PREFETCH + "__Vtogcov__s2_exception_value",
    ),
    "prefetch_s2_mmio": (
        _PREFETCH + "s2_isMmio",
        _PREFETCH + "__Vtogcov__s2_isMmio",
    ),
    "prefetch_s2_sram_hit": (
        _PREFETCH + "s2_sramHits_0",
        _PREFETCH + "__Vtogcov__s2_sramHits_0",
    ),
    "prefetch_s2_mshr_hit": (
        _PREFETCH + "s2_mshrHits_valid",
        _PREFETCH + "__Vtogcov__s2_mshrHits_valid",
    ),
    "prefetch_s2_ptag": (
        _PREFETCH + "s2_pTag",
        _PREFETCH + "__Vtogcov__s2_pTag",
    ),
    "prefetch_s2_vset": (
        _PREFETCH + "s2_readMetaSetIdx_0",
        _PREFETCH + "__Vtogcov__s2_readMetaSetIdx_0",
    ),
}


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


def _mshr_snapshot(recorder) -> list[dict[str, Optional[int]]]:
    result = []
    for index in range(14):
        base = f"{_MISS}allMshr_{index}."
        result.append(
            {
                "valid": _read_names(recorder, (base + "valid",))[0],
                "issue": _read_names(recorder, (base + "issue",))[0],
                "flush": _read_names(recorder, (base + "flush",))[0],
                "fencei": _read_names(recorder, (base + "fencei",))[0],
                "paddr": _read_names(recorder, (base + "blkPAddr",))[0],
                "vset": _read_names(recorder, (base + "vSetIdx",))[0],
            }
        )
    return result


def _on(value: Optional[int]) -> bool:
    return value is not None and int(value) != 0


def _off(value: Optional[int]) -> bool:
    return value is not None and int(value) == 0


def _same_key(
    item: dict[str, Optional[int]],
    paddr: Optional[int],
    vset: Optional[int],
) -> bool:
    return (
        _on(item["valid"])
        and paddr is not None
        and vset is not None
        and item["paddr"] is not None
        and item["vset"] is not None
        and int(item["paddr"]) == int(paddr)
        and int(item["vset"]) == int(vset)
    )


def _mark(
    recorder,
    group: str,
    name: str,
    cycle: int,
    condition: bool,
    evidence: dict[str, Any],
) -> None:
    if condition:
        recorder.mark(
            group,
            name,
            cycle,
            evidence,
            coverpoint=ICACHE_HITMISS_COVERPOINTS[group],
        )


def reset_icache_hitmiss_coverage_state(recorder) -> None:
    recorder._icache_hitmiss_cov_state = {
        "refilled_keys": {},
        "full_set_miss_signatures": {},
        "last_clean_hit": None,
        "pending_refill_hit": None,
        "last_mainpipe_observation": None,
    }


def _snapshot(recorder) -> dict[str, Any]:
    scalar = {key: _read(recorder, key) for key in _SIGNALS}
    scalar["hits"] = _read_candidates(recorder, _S1_HITS)
    scalar["waymask"] = _read_names(
        recorder,
        tuple(
            _MAIN + f"s1_wayLookupEntry_{req}_waymask_{line}"
            for req in range(2)
            for line in range(2)
        ),
    )
    scalar["prefetch_meta_valids"] = _read_candidates(
        recorder, _PREFETCH_META_VALIDS
    )
    return scalar


def _line_valids(snapshot: dict[str, Any]) -> tuple[bool, bool, bool, bool]:
    req1 = _on(snapshot["req1_valid"])
    cross0 = _on(snapshot["cross0"])
    cross1 = _on(snapshot["cross1"])
    return (True, cross0, req1, req1 and cross1)


def _line_hits(snapshot: dict[str, Any]) -> tuple[bool, bool, bool, bool]:
    hits = snapshot["hits"]
    waymask = snapshot["waymask"]
    sram_valid = (
        snapshot["sram_valid"],
        snapshot["sram_valid_cross0"],
        snapshot["sram_valid_req1"],
        snapshot["sram_valid_req1_cross"],
    )
    valid = _line_valids(snapshot)
    return tuple(
        valid[index]
        and hits[index] is not None
        and int(hits[index]) != 0
        and waymask[index] is not None
        and int(waymask[index]) != 0
        and sram_valid[index] is not None
        and int(sram_valid[index]) != 0
        for index in range(4)
    )


def _clean_hit(snapshot: dict[str, Any], line_hits: tuple[bool, bool, bool, bool]) -> bool:
    line_valids = _line_valids(snapshot)
    return (
        _on(snapshot["s1_valid"])
        and all(
            (not valid) or hit
            for valid, hit in zip(line_valids, line_hits)
        )
        and _off(snapshot["itlb_exception"])
        and _off(snapshot["pmp_instr"])
        and _off(snapshot["s1_flush"])
        and _off(snapshot["fencei"])
    )


def _hit_identity(snapshot: dict[str, Any]) -> Optional[dict[str, Any]]:
    if snapshot["req0_start"] is None:
        return None
    start = int(snapshot["req0_start"])
    return {
        "line": start >> 6,
        "offset": start & 0x3F,
        "cross": _on(snapshot["cross0"]),
    }


def _clean_fetch_refill(snapshot: dict[str, Any], mshrs: list[dict[str, Optional[int]]]) -> Optional[tuple[int, int]]:
    if (
        not _on(snapshot["last_fire_next"])
        or snapshot["id_next"] is None
        or not _off(snapshot["global_flush"])
        or not _off(snapshot["fencei"])
    ):
        return None
    source = int(snapshot["id_next"])
    if not 0 <= source < len(mshrs):
        return None
    item = mshrs[source]
    if source >= 4 or not _on(item["valid"]) or _on(item["flush"]) or _on(item["fencei"]):
        return None
    if not _off(snapshot["corrupt_reg"]) or not _off(snapshot["denied_reg"]):
        return None
    if item["paddr"] is None or item["vset"] is None:
        return None
    return int(item["paddr"]) >> _PAGE_CACHELINE_INDEX_BITS, int(item["vset"])


def _cache_key(ptag: Optional[int], vset: Optional[int]) -> Optional[tuple[int, int]]:
    if ptag is None or vset is None:
        return None
    return int(ptag), int(vset)


def _full_set_miss_signature(snapshot: dict[str, Any]) -> Optional[tuple[int, int, int, int]]:
    values = (
        snapshot["prefetch_s1_ptag"],
        snapshot["prefetch_s1_vset"],
        snapshot["prefetch_s1_ftq_flag"],
        snapshot["prefetch_s1_ftq_value"],
    )
    if (
        not _on(snapshot["prefetch_s1_valid"])
        or not _off(snapshot["prefetch_s1_soft"])
        or not _off(snapshot["global_flush"])
        or not _off(snapshot["fencei"])
        or not _on(snapshot["prefetch_s1_sram_valid"])
        or not all(_on(valid) for valid in snapshot["prefetch_meta_valids"])
        or not _off(snapshot["prefetch_s1_sram_hit"])
        or any(value is None for value in values)
    ):
        return None
    return tuple(int(value) for value in values)


def _fetch_miss_signature(snapshot: dict[str, Any]) -> Optional[tuple[int, int, int, int]]:
    values = (
        snapshot["fetch_paddr"],
        snapshot["fetch_vset"],
        snapshot["main_s1_ftq_flag"],
        snapshot["main_s1_ftq_value"],
    )
    if any(value is None for value in values):
        return None
    return (
        int(values[0]) >> _PAGE_CACHELINE_INDEX_BITS,
        int(values[1]),
        int(values[2]),
        int(values[3]),
    )


def _main_s1_signature(snapshot: dict[str, Any]) -> Optional[tuple[int, int, int, int]]:
    values = (
        snapshot["main_s1_ptag"],
        snapshot["main_s1_vset"],
        snapshot["main_s1_ftq_flag"],
        snapshot["main_s1_ftq_value"],
    )
    if any(value is None for value in values):
        return None
    return tuple(int(value) for value in values)


def _same_cache_key(
    item: dict[str, Optional[int]], key: Optional[tuple[int, int]]
) -> bool:
    return (
        key is not None
        and _on(item["valid"])
        and item["paddr"] is not None
        and item["vset"] is not None
        and (int(item["paddr"]) >> _PAGE_CACHELINE_INDEX_BITS) == key[0]
        and int(item["vset"]) == key[1]
    )


def sample_icache_hitmiss_coverage(recorder, env, cycle: int) -> None:
    del env
    state = getattr(recorder, "_icache_hitmiss_cov_state", None)
    if state is None:
        reset_icache_hitmiss_coverage_state(recorder)
        state = recorder._icache_hitmiss_cov_state

    snapshot = _snapshot(recorder)
    mshrs = _mshr_snapshot(recorder)
    evidence = {
        key: value
        for key, value in snapshot.items()
        if value is not None
    }
    evidence["mshr"] = mshrs
    line_valids = _line_valids(snapshot)
    line_hits = _line_hits(snapshot)
    if (
        _on(snapshot["s1_fire"])
        or _on(snapshot["sram_valid"])
        or any(_on(hit) for hit in snapshot["hits"])
    ):
        state["last_mainpipe_observation"] = {
            "cycle": cycle,
            "s1_valid": snapshot["s1_valid"],
            "s1_fire": snapshot["s1_fire"],
            "sram_valid": snapshot["sram_valid"],
            "hits": snapshot["hits"],
            "ptag": snapshot["main_s1_ptag"],
            "vset": snapshot["main_s1_vset"],
            "ftq_flag": snapshot["main_s1_ftq_flag"],
            "ftq_value": snapshot["main_s1_ftq_value"],
        }

    if (
        _on(snapshot["global_flush"])
        or _on(snapshot["s1_flush"])
        or _on(snapshot["fencei"])
    ):
        state["refilled_keys"].clear()
        state["full_set_miss_signatures"].clear()
        state["pending_refill_hit"] = None

    full_set_signature = _full_set_miss_signature(snapshot)
    if full_set_signature is not None:
        state["full_set_miss_signatures"][full_set_signature] = cycle

    # These two bins describe a sequence of accepted clean hit requests.  A
    # gap between requests is allowed, but an intervening non-hit request,
    # exception, flush, or fence.i breaks the sequence.
    if _on(snapshot["s1_flush"]) or _on(snapshot["fencei"]):
        state["last_clean_hit"] = None
    if _on(snapshot["s1_fire"]):
        current_hit = _hit_identity(snapshot) if _clean_hit(snapshot, line_hits) else None
        previous_hit = state["last_clean_hit"]
        if current_hit is not None and previous_hit is not None:
            same_line_different_offset = (
                current_hit["line"] == previous_hit["line"]
                and current_hit["offset"] != previous_hit["offset"]
            )
            crossed_cacheline = (
                current_hit["line"] != previous_hit["line"]
                or current_hit["cross"]
                or previous_hit["cross"]
            )
            sequence_evidence = dict(evidence)
            sequence_evidence["previous_clean_hit"] = previous_hit
            sequence_evidence["current_clean_hit"] = current_hit
            _mark(
                recorder,
                "icache_hit_path",
                "continuous_same_line_sram_hit",
                cycle,
                same_line_different_offset,
                sequence_evidence,
            )
            _mark(
                recorder,
                "icache_hit_path",
                "continuous_cross_line_sram_hit",
                cycle,
                crossed_cacheline,
                sequence_evidence,
            )
        state["last_clean_hit"] = current_hit
    elif _on(snapshot["s1_valid"]) and (
        _on(snapshot["itlb_exception"])
        or _on(snapshot["pmp_instr"])
        or _on(snapshot["s1_flush"])
        or _on(snapshot["fencei"])
    ):
        state["last_clean_hit"] = None

    independent = (
        _on(snapshot["s1_valid"])
        and _on(snapshot["req1_valid"])
        and all(hit == valid for hit, valid in zip(line_hits, line_valids))
        and snapshot["req0_start"] is not None
        and snapshot["req1_start"] is not None
        and (int(snapshot["req0_start"]) >> 6) != (int(snapshot["req1_start"]) >> 6)
        and _off(snapshot["itlb_exception"])
        and _off(snapshot["pmp_instr"])
        and _off(snapshot["s1_flush"])
        and _off(snapshot["fencei"])
    )
    _mark(recorder, "icache_hit_path", "dual_request_independent_hit", cycle, independent, evidence)
    _mark(
        recorder,
        "icache_hit_path",
        "hit_itlb_exception",
        cycle,
        _on(snapshot["s1_valid"]) and line_hits[0] and _on(snapshot["itlb_exception"]),
        evidence,
    )
    _mark(
        recorder,
        "icache_hit_path",
        "hit_pmp_exception",
        cycle,
        _on(snapshot["s1_valid"]) and line_hits[0] and _on(snapshot["pmp_instr"]),
        evidence,
    )

    fetch_valid = _on(snapshot["fetch_valid"])
    prefetch_valid = _on(snapshot["prefetch_valid"])
    fetch_hit = _on(snapshot["fetch_hit"])
    prefetch_hit = _on(snapshot["prefetch_hit"])
    different_key = (
        snapshot["fetch_paddr"] is not None
        and snapshot["prefetch_paddr"] is not None
        and snapshot["fetch_vset"] is not None
        and snapshot["prefetch_vset"] is not None
        and (
            int(snapshot["fetch_paddr"]) != int(snapshot["prefetch_paddr"])
            or int(snapshot["fetch_vset"]) != int(snapshot["prefetch_vset"])
        )
    )
    _mark(
        recorder,
        "icache_miss_path",
        "fetch_hit_prefetch_miss_concurrent",
        cycle,
        fetch_valid and fetch_hit and prefetch_valid and not prefetch_hit and different_key,
        evidence,
    )

    clean_refill_key = _clean_fetch_refill(snapshot, mshrs)
    if clean_refill_key is not None:
        state["refilled_keys"][clean_refill_key] = cycle
    prefetch_key = _cache_key(
        snapshot["prefetch_s2_ptag"], snapshot["prefetch_s2_vset"]
    )
    prefetch_refill_cycle = state["refilled_keys"].get(prefetch_key)
    _mark(
        recorder,
        "icache_miss_path",
        "fetch_refill_prefetch_hit",
        cycle,
        _on(snapshot["prefetch_s2_valid"])
        and _on(snapshot["prefetch_s2_fire"])
        and _off(snapshot["prefetch_s2_exception"])
        and _off(snapshot["prefetch_s2_mmio"])
        and _on(snapshot["prefetch_s2_sram_hit"])
        and _off(snapshot["prefetch_s2_mshr_hit"])
        and _off(snapshot["global_flush"])
        and _off(snapshot["fencei"])
        and prefetch_refill_cycle is not None
        and prefetch_refill_cycle < cycle,
        evidence,
    )

    fetch_key = None
    if snapshot["fetch_paddr"] is not None and snapshot["fetch_vset"] is not None:
        fetch_key = (int(snapshot["fetch_paddr"]), int(snapshot["fetch_vset"]))
    merge = fetch_valid and fetch_hit and any(
        _same_key(item, *(fetch_key or (None, None))) for item in mshrs
    )
    _mark(recorder, "icache_miss_path", "continuous_fetch_miss_merge", cycle, merge, evidence)

    fetch_miss_signature = _fetch_miss_signature(snapshot)
    full_set_cycle = state["full_set_miss_signatures"].get(fetch_miss_signature)
    fetch_plru_condition = (
        fetch_valid
        and _on(snapshot["fetch_ready"])
        and not fetch_hit
        and full_set_cycle is not None
        and full_set_cycle < cycle
        and _off(snapshot["global_flush"])
        and _off(snapshot["fencei"])
    )
    prefetch_full_signature = None
    prefetch_full_cycle = None
    if snapshot["prefetch_paddr"] is not None and snapshot["prefetch_vset"] is not None:
        prefetch_ptag = int(snapshot["prefetch_paddr"]) >> _PAGE_CACHELINE_INDEX_BITS
        prefetch_vset = int(snapshot["prefetch_vset"])
        for signature, signature_cycle in state["full_set_miss_signatures"].items():
            if signature[:2] == (prefetch_ptag, prefetch_vset):
                prefetch_full_signature = signature
                prefetch_full_cycle = signature_cycle
                break
    prefetch_plru_condition = (
        prefetch_valid
        and _on(snapshot["prefetch_ready"])
        and not prefetch_hit
        and prefetch_full_cycle is not None
        and prefetch_full_cycle < cycle
        and _off(snapshot["global_flush"])
        and _off(snapshot["fencei"])
    )
    plru_condition = fetch_plru_condition or prefetch_plru_condition
    evidence["plru_request_kind"] = (
        "fetch" if fetch_plru_condition else "prefetch" if prefetch_plru_condition else None
    )
    _mark(
        recorder,
        "icache_miss_path",
        "plru_victim_on_miss",
        cycle,
        plru_condition,
        evidence,
    )
    if plru_condition:
        consumed_signature = (
            fetch_miss_signature if fetch_plru_condition else prefetch_full_signature
        )
        state["full_set_miss_signatures"].pop(consumed_signature, None)
    elif _on(snapshot["s1_fire"]) and _clean_hit(snapshot, line_hits):
        # A MainPipe miss reaches MissUnit in a later cycle.  Preserve the
        # PrefetchPipe full-set signature until that accepted miss can consume
        # it; only a real SRAM hit makes the pending victim check stale.
        state["full_set_miss_signatures"].pop(_main_s1_signature(snapshot), None)

    demand_key = _cache_key(snapshot["main_s1_ptag"], snapshot["main_s1_vset"])
    demand_signature = _main_s1_signature(snapshot)
    demand_refill_cycle = state["refilled_keys"].get(demand_key)
    # MainPipe's registered SRAM hit pulse precedes s1_fire while the request
    # is held.  The matching MSHR can still be retiring at this point, so
    # remember the response and require its absence at the later acceptance.
    registered_sram_hit = (
        _on(snapshot["s1_valid"])
        and snapshot["hits"][0] is not None
        and int(snapshot["hits"][0]) != 0
        and _on(snapshot["sram_valid"])
        and _off(snapshot["itlb_exception"])
        and _off(snapshot["pmp_instr"])
        and _off(snapshot["main_s1_mmio"])
        and _off(snapshot["s1_flush"])
        and _off(snapshot["fencei"])
        and demand_signature is not None
        and demand_refill_cycle is not None
        and demand_refill_cycle < cycle
    )
    if registered_sram_hit:
        state["pending_refill_hit"] = {
            "signature": demand_signature,
            "key": demand_key,
            "response_cycle": cycle,
        }

    pending_refill_hit = state["pending_refill_hit"]
    pending_key = (
        pending_refill_hit["key"] if pending_refill_hit is not None else None
    )
    pending_refill_cycle = state["refilled_keys"].get(pending_key)
    refill_then_hit = (
        _on(snapshot["s1_fire"])
        and pending_refill_hit is not None
        and _off(snapshot["s1_flush"])
        and _off(snapshot["fencei"])
        and pending_refill_cycle is not None
        and pending_refill_cycle <= pending_refill_hit["response_cycle"]
        and not any(_same_cache_key(item, pending_key) for item in mshrs)
    )
    _mark(recorder, "icache_miss_path", "refill_then_fetch_hit", cycle, refill_then_hit, evidence)
    if _on(snapshot["s1_fire"]):
        state["pending_refill_hit"] = None


__all__ = (
    "ICACHE_HITMISS_COVERPOINTS",
    "ICACHE_HITMISS_SAMPLER_BIN_KEYS",
    "reset_icache_hitmiss_coverage_state",
    "sample_icache_hitmiss_coverage",
)
