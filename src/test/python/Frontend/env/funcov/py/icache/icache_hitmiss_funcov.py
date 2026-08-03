"""Cycle predicates for the cacheable ICache hit/miss path leaves."""

from __future__ import annotations

from typing import Any, Iterable, Optional


_TOP = "Frontend_top."
_ICACHE = _TOP + "Frontend.inner_icache."
_MAIN = _ICACHE + "mainPipe."
_MISS = _ICACHE + "missUnit."

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
    "cross0": _S1_CROSS[0],
    "cross1": _S1_CROSS[1],
    "req1_valid": (_MAIN + "s1_req_1_valid",),
    "req0_start": (_MAIN + "s1_req_0_vAddr_0_addr",),
    "req1_start": (_MAIN + "s1_req_1_vAddr_0_addr",),
    "pmp_instr": (_MAIN + "io_pmp_resp_instr",),
    "itlb_exception": (
        _MAIN + "s1_exceptionInfo_0_itlbException_value",
        _MAIN + "__Vtogcov__s1_exceptionInfo_0_itlbException_value",
    ),
    "sram_valid": (
        _MAIN + "s1_sramRespValid",
        _MAIN + "s1_sramValid_0_1",
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
        _MAIN + "__Vtogcov__io_missReq_valid",
        _MISS + "fetchDemux.io_in_valid",
        _MISS + "fetchDemux.__Vtogcov__io_in_valid",
    ),
    "fetch_hit": (_MISS + "fetchHit", _MISS + "__Vtogcov__fetchHit"),
    "fetch_paddr": (
        _MAIN + "__Vtogcov__io_missReq_bits_blkPAddr",
        _ICACHE + "_mainPipe_io_missReq_bits_blkPAddr",
    ),
    "fetch_vset": (
        _MAIN + "__Vtogcov__io_missReq_bits_vSetIdx",
        _ICACHE + "_mainPipe_io_missReq_bits_vSetIdx",
    ),
    "prefetch_valid": (
        _MISS + "io_prefetchReq_valid",
        _MISS + "__Vtogcov__io_prefetchReq_valid",
        _MISS + "prefetchDemux.io_in_valid",
        _MISS + "prefetchDemux.__Vtogcov__io_in_valid",
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
    recorder._icache_hitmiss_cov_state = {"refilled_keys": set()}


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
    return scalar


def _line_valids(snapshot: dict[str, Any]) -> tuple[bool, bool, bool, bool]:
    req1 = _on(snapshot["req1_valid"])
    cross0 = _on(snapshot["cross0"])
    cross1 = _on(snapshot["cross1"])
    return (True, cross0, req1, req1 and cross1)


def _line_hits(snapshot: dict[str, Any]) -> tuple[bool, bool, bool, bool]:
    hits = snapshot["hits"]
    waymask = snapshot["waymask"]
    valid = _line_valids(snapshot)
    return tuple(
        valid[index]
        and hits[index] is not None
        and int(hits[index]) != 0
        and waymask[index] is not None
        and int(waymask[index]) != 0
        for index in range(4)
    )


def _clean_fetch_refill(snapshot: dict[str, Any], mshrs: list[dict[str, Optional[int]]]) -> Optional[tuple[int, int]]:
    if not _on(snapshot["last_fire_next"]) or snapshot["id_next"] is None:
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
    return int(item["paddr"]), int(item["vset"])


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
    single_hit = line_hits[0] and not _on(snapshot["cross0"])
    cross_hit = line_hits[0] and line_hits[1] and _on(snapshot["cross0"])

    _mark(
        recorder,
        "icache_hit_path",
        "continuous_same_line_sram_hit",
        cycle,
        _on(snapshot["s1_valid"]) and single_hit,
        evidence,
    )
    _mark(
        recorder,
        "icache_hit_path",
        "continuous_cross_line_sram_hit",
        cycle,
        _on(snapshot["s1_valid"]) and cross_hit,
        evidence,
    )
    independent = (
        _on(snapshot["s1_valid"])
        and _on(snapshot["req1_valid"])
        and all(hit == valid for hit, valid in zip(line_hits, line_valids))
        and snapshot["req0_start"] is not None
        and snapshot["req1_start"] is not None
        and (int(snapshot["req0_start"]) >> 6) != (int(snapshot["req1_start"]) >> 6)
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
        state["refilled_keys"].add(clean_refill_key)
    prefetch_key = None
    if snapshot["prefetch_paddr"] is not None and snapshot["prefetch_vset"] is not None:
        prefetch_key = (int(snapshot["prefetch_paddr"]), int(snapshot["prefetch_vset"]))
    _mark(
        recorder,
        "icache_miss_path",
        "fetch_refill_prefetch_hit",
        cycle,
        prefetch_valid and prefetch_hit and prefetch_key in state["refilled_keys"],
        evidence,
    )

    fetch_key = None
    if snapshot["fetch_paddr"] is not None and snapshot["fetch_vset"] is not None:
        fetch_key = (int(snapshot["fetch_paddr"]), int(snapshot["fetch_vset"]))
    merge = fetch_valid and fetch_hit and any(
        _same_key(item, *(fetch_key or (None, None))) for item in mshrs
    )
    _mark(recorder, "icache_miss_path", "continuous_fetch_miss_merge", cycle, merge, evidence)

    _mark(
        recorder,
        "icache_miss_path",
        "plru_victim_on_miss",
        cycle,
        fetch_valid and not fetch_hit and _on(snapshot["victim_req"]) and snapshot["victim_way"] is not None,
        evidence,
    )
    refill_then_hit = fetch_valid and fetch_hit and fetch_key in state["refilled_keys"] and not any(
        _same_key(item, *(fetch_key or (None, None))) for item in mshrs
    )
    _mark(recorder, "icache_miss_path", "refill_then_fetch_hit", cycle, refill_then_hit, evidence)


__all__ = (
    "ICACHE_HITMISS_COVERPOINTS",
    "ICACHE_HITMISS_SAMPLER_BIN_KEYS",
    "reset_icache_hitmiss_coverage_state",
    "sample_icache_hitmiss_coverage",
)
