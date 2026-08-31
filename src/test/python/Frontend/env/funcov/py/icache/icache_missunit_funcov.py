"""Cycle predicates for the ICache MissUnit testpoint leaves."""

from __future__ import annotations

from typing import Iterable, Optional


_TOP = "Frontend_top."
_ICACHE = "Frontend_top.Frontend.inner_icache."
_MISS = _ICACHE + "missUnit."
_MAIN = _ICACHE + "mainPipe."


ICACHE_MISSUNIT_COVERPOINTS = {
    "icache_missunit_request": "request_behavior",
    "icache_missunit_capacity": "capacity_behavior",
    "icache_missunit_acquire": "acquire_behavior",
    "icache_missunit_dedup": "dedup_behavior",
    "icache_missunit_flush": "flush_behavior",
    "icache_missunit_fencei": "fencei_behavior",
    "icache_missunit_refill": "refill_behavior",
}


ICACHE_MISSUNIT_SAMPLER_BIN_KEYS = frozenset(
    {
        ("icache_missunit_request", "fetch_mshr_allocate"),
        ("icache_missunit_request", "prefetch_mshr_allocate"),
        ("icache_missunit_request", "same_key_fetch_prefetch_merge"),
        ("icache_missunit_request", "distinct_key_parallel_allocate"),
        ("icache_missunit_request", "same_paddr_diff_vset_separate"),
        ("icache_missunit_capacity", "fetch_full_backpressure"),
        ("icache_missunit_capacity", "prefetch_full_backpressure"),
        ("icache_missunit_acquire", "fetch_priority_over_prefetch"),
        ("icache_missunit_acquire", "fetch_index_priority"),
        ("icache_missunit_acquire", "prefetch_fifo_enqueue"),
        ("icache_missunit_acquire", "prefetch_fifo_issue_order"),
        ("icache_missunit_acquire", "acquire_backpressure_recovery"),
        ("icache_missunit_dedup", "fetch_merge_any_mshr"),
        ("icache_missunit_dedup", "prefetch_merge_any_mshr"),
        ("icache_missunit_dedup", "key_mismatch_no_merge"),
        ("icache_missunit_flush", "redirect_blocks_new_prefetch"),
        ("icache_missunit_flush", "redirect_cancels_unissued_prefetch"),
        ("icache_missunit_flush", "redirect_marks_issued_prefetch"),
        ("icache_missunit_flush", "redirect_keeps_unissued_fetch_mshr"),
        ("icache_missunit_flush", "redirect_keeps_issued_fetch_mshr"),
        ("icache_missunit_flush", "redirect_suppresses_sram_write"),
        ("icache_missunit_fencei", "fencei_blocks_new_nonduplicate"),
        ("icache_missunit_fencei", "fencei_cancels_unissued_mshr"),
        ("icache_missunit_fencei", "fencei_marks_issued_mshr"),
        ("icache_missunit_fencei", "fencei_suppresses_sram_write"),
        ("icache_missunit_fencei", "fencei_clears_prefetch_fifo"),
        ("icache_missunit_fencei", "fencei_redirect_fetch_unissued"),
        ("icache_missunit_fencei", "fencei_redirect_fetch_issued"),
        ("icache_missunit_fencei", "fencei_redirect_prefetch_unissued"),
        ("icache_missunit_fencei", "fencei_redirect_prefetch_issued"),
        ("icache_missunit_refill", "clean_doublebeat_refill_write"),
        ("icache_missunit_refill", "source_routes_refill"),
        ("icache_missunit_refill", "error_first_beat_no_sram_write"),
        ("icache_missunit_refill", "error_second_beat_no_sram_write"),
    }
)


def _on(value: Optional[int]) -> bool:
    return value is not None and int(value) != 0


def _off(value: Optional[int]) -> bool:
    return value is not None and int(value) == 0


def _read(recorder, names: Iterable[str]) -> Optional[int]:
    dut = getattr(getattr(recorder, "env", None), "dut", None)
    if dut is None:
        return None
    return recorder._read_first_dut_signal(dut, tuple(names))


def _mark(recorder, group: str, name: str, cycle: int, condition: bool, evidence: dict) -> None:
    if condition:
        recorder.mark(
            group,
            name,
            cycle,
            evidence,
            coverpoint=ICACHE_MISSUNIT_COVERPOINTS[group],
        )


def _names(signal: str, *alternatives: str) -> tuple[str, ...]:
    return tuple(alternatives) + (signal,)


def _mshr_signal(index: int, field: str) -> tuple[str, ...]:
    base = f"{_MISS}allMshr_{index}."
    return _names(base + field, base + f"__Vtogcov__{field}")


def _mshr_snapshot(recorder) -> list[dict[str, Optional[int]]]:
    result = []
    for index in range(14):
        result.append(
            {
                "valid": _read(recorder, _mshr_signal(index, "valid")),
                "issue": _read(recorder, _mshr_signal(index, "issue")),
                "flush": _read(recorder, _mshr_signal(index, "flush")),
                "fencei": _read(recorder, _mshr_signal(index, "fencei")),
                "paddr": _read(recorder, _mshr_signal(index, "blkPAddr")),
                "vset": _read(recorder, _mshr_signal(index, "vSetIdx")),
            }
        )
    return result


def _any_status(mshrs: list[dict[str, Optional[int]]], indexes: range, field: str) -> bool:
    return any(_on(mshrs[index][field]) for index in indexes)


def _all_valid(mshrs: list[dict[str, Optional[int]]], indexes: range) -> bool:
    return all(_on(mshrs[index]["valid"]) for index in indexes)


def _free_indexes(
    mshrs: list[dict[str, Optional[int]]], indexes: range
) -> list[int]:
    """Return explicitly observable free entries, preserving allocation priority."""
    return [
        index
        for index in indexes
        if mshrs[index]["valid"] is not None and not _on(mshrs[index]["valid"])
    ]


def _same_key_exists(
    mshrs: list[dict[str, Optional[int]]],
    indexes: range,
    paddr: Optional[int],
    vset: Optional[int],
) -> bool:
    if paddr is None or vset is None:
        return False
    return any(
        _on(item["valid"])
        and item["paddr"] is not None
        and item["vset"] is not None
        and int(item["paddr"]) == int(paddr)
        and int(item["vset"]) == int(vset)
        for item in (mshrs[index] for index in indexes)
    )


def _mshr_key_miss(
    mshrs: list[dict[str, Optional[int]]],
    paddr: Optional[int],
    vset: Optional[int],
) -> bool:
    """Require a known non-match against every valid MSHR."""
    if paddr is None or vset is None:
        return False
    for item in mshrs:
        if not _on(item["valid"]):
            continue
        if item["paddr"] is None or item["vset"] is None:
            return False
        if int(item["paddr"]) == int(paddr) and int(item["vset"]) == int(vset):
            return False
    return True


def _ptag_from_blk(paddr: Optional[int]) -> Optional[int]:
    if paddr is None:
        return None
    # ICache getPTagFromBlk removes the 4-KiB page's cacheline-index bits.
    return int(paddr) >> 6


def _mshr_response_valid(mshrs: list[dict[str, Optional[int]]], source: Optional[int]) -> bool:
    if source is None or not 0 <= int(source) < len(mshrs):
        return False
    item = mshrs[int(source)]
    return _on(item["valid"]) and _off(item["flush"]) and _off(item["fencei"])


def _mshr_key(
    mshrs: list[dict[str, Optional[int]]], source: Optional[int]
) -> Optional[tuple[int, int]]:
    if source is None or not 0 <= int(source) < len(mshrs):
        return None
    item = mshrs[int(source)]
    if not _on(item["valid"]) or item["paddr"] is None or item["vset"] is None:
        return None
    return int(item["paddr"]), int(item["vset"])


def reset_icache_missunit_coverage_state(recorder) -> None:
    recorder._icache_missunit_cov_state = {
        "acquire_blocked_cycles": 0,
        "clean_beats": 0,
        "refill_beat_index": 0,
        "refill_source": None,
        "refill_paddr": None,
        "refill_vset": None,
        "refill_key_valid": False,
        "refill_source_consistent": True,
        "refill_controls_clear": True,
        "refill_error_first": False,
        "refill_error_second": False,
        "pending_refill": None,
        "pending_refill_release": None,
        # A response is eligible for the error bins only when its MSHR key
        # was first observed from the request that allocated that MSHR.
        "pending_mshr_request_keys": [],
        "mshr_request_keys": {},
        "last_refill_source": None,
        "last_refill_outstanding": 0,
        "last_flush": None,
        "last_fencei": None,
        # BIN-686 records the expected allocation here so a checker can
        # validate the registered MSHR contents on the following cycle.
        "pending_fetch_allocations": [],
        "last_fetch_allocation_checkpoint": None,
        "last_redirect_sram_write_checkpoint": None,
        "last_fetch_request_observation": None,
        "last_parallel_request_observation": None,
    }


def sample_icache_missunit_coverage(recorder, env, cycle: int) -> None:
    del env
    state = getattr(recorder, "_icache_missunit_cov_state", None)
    if state is None:
        reset_icache_missunit_coverage_state(recorder)
        state = recorder._icache_missunit_cov_state

    signals = {
        "fetch_valid": _read(
            recorder,
            (
                _MAIN + "__Vtogcov__io_missReq_valid",
                _MISS + "io_fetchReq_valid",
                _MISS + "__Vtogcov__io_fetchReq_valid",
                _MISS + "fetchDemux.io_in_valid",
                _MISS + "fetchDemux.__Vtogcov__io_in_valid",
            ),
        ),
        "fetch_ready": _read(
            recorder,
            (
                _MAIN + "__Vtogcov__io_missReq_ready",
                _MISS + "io_fetchReq_ready",
                _MISS + "__Vtogcov__io_fetchReq_ready",
                _MISS + "fetchDemux.io_in_ready",
                _MISS + "fetchDemux.__Vtogcov__io_in_ready",
            ),
        ),
        "fetch_paddr": _read(
            recorder,
            (
                _MAIN + "__Vtogcov__io_missReq_bits_blkPAddr",
                _MISS + "io_fetchReq_bits_blkPAddr",
                _MISS + "__Vtogcov__io_fetchReq_bits_blkPAddr",
                _ICACHE + "_mainPipe_io_missReq_bits_blkPAddr",
            ),
        ),
        "fetch_vset": _read(
            recorder,
            (
                _MAIN + "__Vtogcov__io_missReq_bits_vSetIdx",
                _MISS + "io_fetchReq_bits_vSetIdx",
                _MISS + "__Vtogcov__io_fetchReq_bits_vSetIdx",
                _ICACHE + "_mainPipe_io_missReq_bits_vSetIdx",
            ),
        ),
        "prefetch_valid": _read(
            recorder,
            _names(
                _MISS + "io_prefetchReq_valid",
                _MISS + "__Vtogcov__io_prefetchReq_valid",
                _MISS + "prefetchDemux.io_in_valid",
                _MISS + "prefetchDemux.__Vtogcov__io_in_valid",
            ),
        ),
        "prefetch_ready": _read(
            recorder,
            _names(
                _MISS + "io_prefetchReq_ready",
                _MISS + "__Vtogcov__io_prefetchReq_ready",
                _MISS + "prefetchDemux.io_in_ready",
                _MISS + "prefetchDemux.__Vtogcov__io_in_ready",
            ),
        ),
        "prefetch_paddr": _read(
            recorder,
            (_MISS + "__Vtogcov__io_prefetchReq_bits_blkPAddr",),
        ),
        "prefetch_vset": _read(
            recorder,
            (_MISS + "__Vtogcov__io_prefetchReq_bits_vSetIdx",),
        ),
        "fetch_hit": _read(recorder, (_MISS + "fetchHit", _MISS + "__Vtogcov__fetchHit")),
        "prefetch_hit": _read(
            recorder, (_MISS + "prefetchHit", _MISS + "__Vtogcov__prefetchHit")
        ),
        "flush": _read(
            recorder,
            (_ICACHE + "__Vtogcov__io_fromFtq_redirectFlush",),
        ),
        "fencei": _read(recorder, (_TOP + "io_fencei", _TOP + "__Vtogcov__io_fencei")),
        "acquire_valid": _read(
            recorder,
            (
                _TOP + "auto_inner_icache_client_out_a_valid",
                _TOP + "__Vtogcov__auto_inner_icache_client_out_a_valid",
            ),
        ),
        "acquire_ready": _read(
            recorder,
            (
                _TOP + "auto_inner_icache_client_out_a_ready",
                _TOP + "__Vtogcov__auto_inner_icache_client_out_a_ready",
            ),
        ),
        "acquire_source": _read(
            recorder,
            (
                _TOP + "auto_inner_icache_client_out_a_bits_source",
                _TOP + "__Vtogcov__auto_inner_icache_client_out_a_bits_source",
            ),
        ),
        "d_valid": _read(
            recorder,
            (
                _TOP + "auto_inner_icache_client_out_d_valid",
                _TOP + "__Vtogcov__auto_inner_icache_client_out_d_valid",
            ),
        ),
        "d_opcode": _read(recorder, (_TOP + "auto_inner_icache_client_out_d_bits_opcode",)),
        "d_source": _read(recorder, (_TOP + "auto_inner_icache_client_out_d_bits_source",)),
        "d_corrupt": _read(recorder, (_TOP + "auto_inner_icache_client_out_d_bits_corrupt",)),
        "d_denied": _read(recorder, (_TOP + "auto_inner_icache_client_out_d_bits_denied",)),
        "last_fire_next": _read(recorder, (_MISS + "lastFireNext", _MISS + "__Vtogcov__lastFireNext")),
        "last_fire": _read(recorder, (_MISS + "lastFire", _MISS + "__Vtogcov__lastFire")),
        "read_beat": _read(recorder, (_MISS + "readBeatCnt", _MISS + "__Vtogcov__readBeatCnt")),
        "id_next": _read(recorder, (_MISS + "idNext", _MISS + "__Vtogcov__idNext")),
        "corrupt_reg": _read(recorder, (_MISS + "corruptReg",)),
        "denied_reg": _read(recorder, (_MISS + "deniedReg",)),
        "meta_write": _read(
            recorder,
            (_MISS + "io_metaWrite_req_valid", _MISS + "__Vtogcov__io_metaWrite_req_valid"),
        ),
        "data_write": _read(
            recorder,
            (
                _ICACHE + "ctrlUnitOpt.io_dataWrite_req_valid",
                _ICACHE + "ctrlUnitOpt.__Vtogcov__io_dataWrite_req_valid",
            ),
        ),
        "victim_req": _read(
            recorder,
            (_MISS + "io_victim_req_valid", _MISS + "__Vtogcov__io_victim_req_valid"),
        ),
        "fifo_enq": _read(recorder, (_MISS + "priorityFIFO.io_enq_valid",)),
        "fifo_deq": _read(recorder, (_MISS + "priorityFIFO.io_deq_ready",)),
        "fifo_flush": _read(recorder, (_MISS + "priorityFIFO.__Vtogcov__io_flush",)),
        "fifo_enq_flag": _read(recorder, (_MISS + "priorityFIFO.enqPtr_flag",)),
        "fifo_enq_value": _read(recorder, (_MISS + "priorityFIFO.enqPtr_value",)),
        "fifo_deq_flag": _read(recorder, (_MISS + "priorityFIFO.deqPtr_flag",)),
        "fifo_deq_value": _read(recorder, (_MISS + "priorityFIFO.deqPtr_value",)),
        "prefetch_arb_valid": _read(
            recorder,
            (
                _MISS + "_prefetchArb_io_out_valid",
                _MISS + "prefetchArb.__Vtogcov__io_out_valid",
                _MISS + "prefetchArb.io_out_valid",
            ),
        ),
        "prefetch_arb_selected": _read(recorder, (_MISS + "prefetchArb.io_sel",)),
        "prefetch_demux_chosen": _read(
            recorder,
            (_MISS + "prefetchDemux.io_chosen", _MISS + "prefetchDemux.__Vtogcov__io_chosen"),
        ),
    }
    for index in range(10):
        signals[f"fifo_entry_{index}"] = _read(
            recorder,
            (
                _MISS + f"priorityFIFO.regFiles_{index}",
                _MISS + f"priorityFIFO.__Vtogcov__regFiles_{index}",
            ),
        )
    for index in range(4):
        signals[f"fetch_arb_{index}"] = _read(
            recorder,
            (
                f"{_MISS}acquireArb.io_in_{index}_valid",
                f"{_MISS}acquireArb.__Vtogcov__io_in_{index}_valid",
            ),
        )
    mshrs = _mshr_snapshot(recorder)
    evidence = {
        key: value
        for key, value in signals.items()
        if value is not None
    }
    evidence["mshr_valid"] = [item["valid"] for item in mshrs]
    evidence["mshr_issue"] = [item["issue"] for item in mshrs]
    evidence["mshr_paddr"] = [item["paddr"] for item in mshrs]
    evidence["mshr_vset"] = [item["vset"] for item in mshrs]

    # An explicitly invalid MSHR can no longer carry a request association.
    for index in tuple(state["mshr_request_keys"]):
        if _off(mshrs[index]["valid"]):
            state["mshr_request_keys"].pop(index)

    # Allocation is registered in the MSHR one sample after its request
    # handshake.  Record the original miss key only when that registration is
    # observable, so a later TileLink response cannot be attributed by source
    # alone.
    for pending in state["pending_mshr_request_keys"]:
        if pending["trigger_cycle"] >= cycle:
            continue
        index = pending["expected_index"]
        item = mshrs[index]
        if (
            _on(item["valid"])
            and item["paddr"] is not None
            and item["vset"] is not None
            and int(item["paddr"]) == pending["paddr"]
            and int(item["vset"]) == pending["vset"]
        ):
            state["mshr_request_keys"][index] = (
                pending["paddr"], pending["vset"]
            )
    state["pending_mshr_request_keys"] = []
    evidence["mshr_request_keys"] = dict(state["mshr_request_keys"])

    # The allocation handshake and the registered MSHR contents are observed
    # on adjacent samples.  Keep this diagnostic association separate from the
    # coverpoint trigger: Checkpoint results must not be used to manufacture a
    # functional-coverage hit.
    completed_allocations = []
    pending_allocations = state["pending_fetch_allocations"]
    for pending in pending_allocations:
        if pending["trigger_cycle"] >= cycle:
            continue
        index = pending["expected_index"]
        item = mshrs[index]
        payload_matches = (
            _on(item["valid"])
            and item["paddr"] is not None
            and item["vset"] is not None
            and int(item["paddr"]) == pending["paddr"]
            and int(item["vset"]) == pending["vset"]
        )
        completed_allocations.append(
            {
                **pending,
                "observed_cycle": cycle,
                "payload_matches": payload_matches,
                "fifo_not_enqueued": _off(signals["fifo_enq"]),
                "complete": payload_matches and _off(signals["fifo_enq"]),
            }
        )
    state["pending_fetch_allocations"] = []
    if completed_allocations:
        state["last_fetch_allocation_checkpoint"] = completed_allocations[-1]
        evidence["last_fetch_allocation_checkpoint"] = completed_allocations[-1]

    fetch_valid = _on(signals["fetch_valid"])
    prefetch_valid = _on(signals["prefetch_valid"])
    fetch_hit = _on(signals["fetch_hit"])
    prefetch_hit = _on(signals["prefetch_hit"])
    fetch_fire = fetch_valid and _on(signals["fetch_ready"])
    prefetch_fire = prefetch_valid and _on(signals["prefetch_ready"])
    fetch_demux_fire = fetch_fire and not fetch_hit
    prefetch_demux_fire = prefetch_fire and not prefetch_hit
    if fetch_valid or fetch_hit:
        state["last_fetch_request_observation"] = {
            "cycle": cycle,
            "valid": signals["fetch_valid"],
            "ready": signals["fetch_ready"],
            "hit": signals["fetch_hit"],
            "paddr": signals["fetch_paddr"],
            "vset": signals["fetch_vset"],
        }
    if fetch_valid and prefetch_valid:
        state["last_parallel_request_observation"] = {
            "cycle": cycle,
            "fetch_paddr": signals["fetch_paddr"],
            "fetch_vset": signals["fetch_vset"],
            "fetch_hit": signals["fetch_hit"],
            "prefetch_paddr": signals["prefetch_paddr"],
            "prefetch_vset": signals["prefetch_vset"],
            "prefetch_hit": signals["prefetch_hit"],
        }
    fetch_candidates = sum(_on(signals[f"fetch_arb_{index}"]) for index in range(4))
    fetch_full = _all_valid(mshrs, range(4))
    fetch_free_indexes = _free_indexes(mshrs, range(4))
    fetch_expected_index = fetch_free_indexes[0] if fetch_free_indexes else None
    prefetch_full = _all_valid(mshrs, range(4, 14))
    prefetch_free_indexes = _free_indexes(mshrs, range(4, 14))
    prefetch_chosen = signals["prefetch_demux_chosen"]
    prefetch_expected_index = None
    if prefetch_chosen is not None and 0 <= int(prefetch_chosen) < 10:
        prefetch_expected_index = 4 + int(prefetch_chosen)
    any_mshr = _any_status(mshrs, range(14), "valid")
    any_fetch = _any_status(mshrs, range(4), "valid")
    any_prefetch = _any_status(mshrs, range(4, 14), "valid")
    acquire_fire = _on(signals["acquire_valid"]) and _on(signals["acquire_ready"])
    source = signals["acquire_source"]
    prefetch_acquire_fire = acquire_fire and source is not None and int(source) >= 4
    fifo_head = None
    if signals["fifo_deq_value"] is not None:
        fifo_index = int(signals["fifo_deq_value"])
        if 0 <= fifo_index < 10:
            fifo_head = signals[f"fifo_entry_{fifo_index}"]

    flush = _on(signals["flush"])
    fencei = _on(signals["fencei"])
    fencei_first = fencei and _off(state["last_fencei"])
    controls_clear = _off(signals["flush"]) and _off(signals["fencei"])
    fetch_existing_mshr_hit = _same_key_exists(
        mshrs, range(14), signals["fetch_paddr"], signals["fetch_vset"]
    )
    prefetch_existing_mshr_hit = _same_key_exists(
        mshrs, range(14), signals["prefetch_paddr"], signals["prefetch_vset"]
    )
    same_cycle_fetch_prefetch_key = (
        fetch_valid
        and prefetch_valid
        and signals["fetch_paddr"] is not None
        and signals["prefetch_paddr"] is not None
        and signals["fetch_vset"] is not None
        and signals["prefetch_vset"] is not None
        and int(signals["fetch_paddr"]) == int(signals["prefetch_paddr"])
        and int(signals["fetch_vset"]) == int(signals["prefetch_vset"])
    )
    fetch_mshr_allocate = (
        fetch_demux_fire
        and _off(signals["prefetch_valid"])
        and _off(signals["flush"])
        and _off(signals["fencei"])
        and fetch_expected_index is not None
        and signals["fetch_paddr"] is not None
        and signals["fetch_vset"] is not None
    )
    evidence["fetch_free_mshr_indexes"] = fetch_free_indexes
    evidence["fetch_expected_mshr"] = fetch_expected_index
    if fetch_mshr_allocate:
        evidence["fetch_allocation_key"] = {
            "blkPAddr": signals["fetch_paddr"],
            "vSetIdx": signals["fetch_vset"],
        }
        if signals["fetch_paddr"] is not None and signals["fetch_vset"] is not None:
            state["pending_fetch_allocations"].append(
                {
                    "trigger_cycle": cycle,
                    "expected_index": fetch_expected_index,
                    "paddr": int(signals["fetch_paddr"]),
                    "vset": int(signals["fetch_vset"]),
                }
            )
            state["pending_mshr_request_keys"].append(
                {
                    "trigger_cycle": cycle,
                    "expected_index": fetch_expected_index,
                    "paddr": int(signals["fetch_paddr"]),
                    "vset": int(signals["fetch_vset"]),
                }
            )

    prefetch_mshr_key_observable = (
        prefetch_demux_fire
        and prefetch_expected_index is not None
        and prefetch_expected_index in prefetch_free_indexes
        and signals["prefetch_paddr"] is not None
        and signals["prefetch_vset"] is not None
        and controls_clear
    )
    evidence["prefetch_expected_mshr"] = prefetch_expected_index
    if prefetch_mshr_key_observable:
        state["pending_mshr_request_keys"].append(
            {
                "trigger_cycle": cycle,
                "expected_index": prefetch_expected_index,
                "paddr": int(signals["prefetch_paddr"]),
                "vset": int(signals["prefetch_vset"]),
            }
        )

    _mark(
        recorder, "icache_missunit_request", "fetch_mshr_allocate", cycle,
        fetch_mshr_allocate, evidence,
    )
    _mark(
        recorder, "icache_missunit_request", "prefetch_mshr_allocate", cycle,
        prefetch_demux_fire, evidence,
    )
    fetch_mshr_miss = _mshr_key_miss(
        mshrs, signals["fetch_paddr"], signals["fetch_vset"]
    )
    prefetch_mshr_miss = _mshr_key_miss(
        mshrs, signals["prefetch_paddr"], signals["prefetch_vset"]
    )
    fetch_ptag = _ptag_from_blk(signals["fetch_paddr"])
    prefetch_ptag = _ptag_from_blk(signals["prefetch_paddr"])
    concurrent_miss_base = (
        fetch_valid
        and prefetch_valid
        and fetch_mshr_miss
        and prefetch_mshr_miss
        and _off(signals["flush"])
        and _off(signals["fencei"])
        and bool(fetch_free_indexes)
        and bool(_free_indexes(mshrs, range(4, 14)))
    )
    evidence["fetch_mshr_miss"] = fetch_mshr_miss
    evidence["prefetch_mshr_miss"] = prefetch_mshr_miss
    evidence["fetch_ptag"] = fetch_ptag
    evidence["prefetch_ptag"] = prefetch_ptag
    same_key = (
        concurrent_miss_base
        and _off(signals["fetch_hit"])
        # With no existing matching MSHR, this can only be the RTL's
        # same-cycle fetch/prefetch merge.
        and _on(signals["prefetch_hit"])
        and signals["fetch_paddr"] is not None
        and signals["prefetch_paddr"] is not None
        and signals["fetch_vset"] is not None
        and signals["prefetch_vset"] is not None
        and int(signals["fetch_paddr"]) == int(signals["prefetch_paddr"])
        and int(signals["fetch_vset"]) == int(signals["prefetch_vset"])
    )
    different_ptag_same_vset = (
        concurrent_miss_base
        and _off(signals["fetch_hit"])
        and _off(signals["prefetch_hit"])
        and fetch_ptag is not None
        and prefetch_ptag is not None
        and signals["fetch_vset"] is not None
        and signals["prefetch_vset"] is not None
        and fetch_ptag != prefetch_ptag
        and int(signals["fetch_vset"]) == int(signals["prefetch_vset"])
    )
    same_paddr_diff_vset = (
        concurrent_miss_base
        and _off(signals["fetch_hit"])
        and _off(signals["prefetch_hit"])
        and signals["fetch_paddr"] is not None
        and signals["prefetch_paddr"] is not None
        and signals["fetch_vset"] is not None
        and signals["prefetch_vset"] is not None
        and int(signals["fetch_paddr"]) == int(signals["prefetch_paddr"])
        and int(signals["fetch_vset"]) != int(signals["prefetch_vset"])
    )
    _mark(recorder, "icache_missunit_request", "same_key_fetch_prefetch_merge", cycle, same_key, evidence)
    _mark(recorder, "icache_missunit_request", "distinct_key_parallel_allocate", cycle, different_ptag_same_vset, evidence)
    _mark(recorder, "icache_missunit_request", "same_paddr_diff_vset_separate", cycle, same_paddr_diff_vset, evidence)
    fetch_key_known = signals["fetch_paddr"] is not None and signals["fetch_vset"] is not None
    prefetch_key_known = signals["prefetch_paddr"] is not None and signals["prefetch_vset"] is not None
    fetch_nonduplicate = fetch_key_known and not _same_key_exists(
        mshrs, range(14), signals["fetch_paddr"], signals["fetch_vset"]
    )
    prefetch_nonduplicate = prefetch_key_known and not _same_key_exists(
        mshrs, range(14), signals["prefetch_paddr"], signals["prefetch_vset"]
    )
    fetch_full_backpressure = (
        fetch_full
        and fetch_valid
        and fetch_nonduplicate
        and _off(signals["fetch_hit"])
        and _off(signals["fetch_ready"])
        and _off(signals["flush"])
        and _off(signals["fencei"])
    )
    prefetch_full_backpressure = (
        prefetch_full
        and prefetch_valid
        and prefetch_nonduplicate
        and _off(signals["prefetch_hit"])
        and _off(signals["prefetch_ready"])
        and _off(signals["flush"])
        and _off(signals["fencei"])
    )
    _mark(
        recorder,
        "icache_missunit_capacity",
        "fetch_full_backpressure",
        cycle,
        fetch_full_backpressure,
        evidence,
    )
    _mark(
        recorder,
        "icache_missunit_capacity",
        "prefetch_full_backpressure",
        cycle,
        prefetch_full_backpressure,
        evidence,
    )

    _mark(
        recorder, "icache_missunit_acquire", "fetch_priority_over_prefetch", cycle,
        fetch_candidates >= 1
        and _on(signals["prefetch_arb_valid"])
        and _on(signals["acquire_ready"])
        and _off(signals["flush"])
        and _off(signals["fencei"]),
        evidence,
    )
    _mark(
        recorder, "icache_missunit_acquire", "fetch_index_priority", cycle,
        fetch_candidates >= 2
        and _on(signals["acquire_ready"])
        and _off(signals["flush"])
        and _off(signals["fencei"]),
        evidence,
    )
    _mark(
        recorder, "icache_missunit_acquire", "prefetch_fifo_enqueue", cycle,
        prefetch_demux_fire and _on(signals["fifo_enq"]), evidence,
    )
    fifo_issue_order = (
        prefetch_acquire_fire
        and fifo_head is not None
        and source is not None
        and 4 <= int(source) < 14
        and int(source) - 4 == int(fifo_head)
    )
    _mark(
        recorder, "icache_missunit_acquire", "prefetch_fifo_issue_order", cycle,
        fifo_issue_order, evidence,
    )
    if _on(signals["acquire_valid"]) and controls_clear and _off(signals["acquire_ready"]):
        state["acquire_blocked_cycles"] += 1
    elif _on(signals["acquire_valid"]) and controls_clear and _on(signals["acquire_ready"]):
        recovered_after_backpressure = state["acquire_blocked_cycles"] >= 2
        state["acquire_blocked_cycles"] = 0
        _mark(
            recorder,
            "icache_missunit_acquire",
            "acquire_backpressure_recovery",
            cycle,
            recovered_after_backpressure,
            evidence,
        )
    else:
        state["acquire_blocked_cycles"] = 0

    fetch_merge_any_mshr = (
        fetch_fire
        and controls_clear
        and fetch_hit
    )
    prefetch_merge_any_mshr = (
        prefetch_fire
        and controls_clear
        and prefetch_hit
        and prefetch_existing_mshr_hit
        and not same_cycle_fetch_prefetch_key
    )
    evidence["fetch_existing_mshr_hit"] = fetch_existing_mshr_hit
    evidence["prefetch_existing_mshr_hit"] = prefetch_existing_mshr_hit
    evidence["same_cycle_fetch_prefetch_key"] = same_cycle_fetch_prefetch_key
    _mark(
        recorder,
        "icache_missunit_dedup",
        "fetch_merge_any_mshr",
        cycle,
        fetch_merge_any_mshr,
        evidence,
    )
    _mark(
        recorder,
        "icache_missunit_dedup",
        "prefetch_merge_any_mshr",
        cycle,
        prefetch_merge_any_mshr,
        evidence,
    )
    fetch_mismatch = fetch_demux_fire and any_mshr and not _same_key_exists(mshrs, range(14), signals["fetch_paddr"], signals["fetch_vset"])
    prefetch_mismatch = prefetch_demux_fire and any_mshr and not _same_key_exists(mshrs, range(14), signals["prefetch_paddr"], signals["prefetch_vset"])
    _mark(recorder, "icache_missunit_dedup", "key_mismatch_no_merge", cycle, fetch_mismatch or prefetch_mismatch, evidence)

    prefetch_miss = prefetch_valid and _off(signals["prefetch_hit"])
    response_next = _on(signals["last_fire_next"])
    _mark(recorder, "icache_missunit_flush", "redirect_blocks_new_prefetch", cycle, flush and prefetch_miss, evidence)
    _mark(recorder, "icache_missunit_flush", "redirect_cancels_unissued_prefetch", cycle, flush and any(_on(item["valid"]) and _off(item["issue"]) for item in mshrs[4:]), evidence)
    _mark(recorder, "icache_missunit_flush", "redirect_marks_issued_prefetch", cycle, flush and any(_on(item["valid"]) and _on(item["issue"]) for item in mshrs[4:]), evidence)
    unissued_fetch = any(_on(item["valid"]) and _off(item["issue"]) for item in mshrs[:4])
    issued_fetch = any(_on(item["valid"]) and _on(item["issue"]) for item in mshrs[:4])
    no_response_next = _off(signals["last_fire_next"])
    _mark(recorder, "icache_missunit_flush", "redirect_keeps_unissued_fetch_mshr", cycle, flush and unissued_fetch and no_response_next, evidence)
    _mark(recorder, "icache_missunit_flush", "redirect_keeps_issued_fetch_mshr", cycle, flush and issued_fetch and no_response_next, evidence)
    response_valid = response_next and _mshr_response_valid(mshrs, signals["id_next"])
    clean_response = _off(signals["corrupt_reg"]) and _off(signals["denied_reg"])
    redirect_first = flush and _off(state["last_flush"])
    redirect_response = redirect_first and response_valid and clean_response
    evidence["redirect_first"] = redirect_first
    evidence["clean_response"] = clean_response
    if redirect_response:
        writes_known = signals["meta_write"] is not None and signals["data_write"] is not None
        checkpoint = {
            "trigger_cycle": cycle,
            "meta_write": signals["meta_write"],
            "data_write": signals["data_write"],
            "writes_known": writes_known,
            "complete": writes_known
            and _off(signals["meta_write"])
            and _off(signals["data_write"]),
        }
        state["last_redirect_sram_write_checkpoint"] = checkpoint
        evidence["redirect_sram_write_checkpoint"] = checkpoint
    _mark(
        recorder,
        "icache_missunit_flush",
        "redirect_suppresses_sram_write",
        cycle,
        redirect_response,
        evidence,
    )

    fetch_new_nonduplicate = (
        fetch_valid
        and not fetch_hit
        and _mshr_key_miss(mshrs, signals["fetch_paddr"], signals["fetch_vset"])
    )
    prefetch_new_nonduplicate = (
        prefetch_valid
        and not prefetch_hit
        and _mshr_key_miss(
            mshrs, signals["prefetch_paddr"], signals["prefetch_vset"]
        )
    )
    fetch_unissued = any(
        _on(item["valid"]) and _off(item["issue"]) for item in mshrs[:4]
    )
    prefetch_unissued = any(
        _on(item["valid"]) and _off(item["issue"]) for item in mshrs[4:]
    )

    def issued_waiting_for_grant(index: int) -> bool:
        item = mshrs[index]
        if not (_on(item["valid"]) and _on(item["issue"])):
            return False
        if _off(signals["last_fire_next"]):
            return True
        return (
            _on(signals["last_fire_next"])
            and signals["id_next"] is not None
            and int(signals["id_next"]) != index
        )

    fetch_issued_waiting = any(issued_waiting_for_grant(index) for index in range(4))
    prefetch_issued_waiting = any(
        issued_waiting_for_grant(index) for index in range(4, 14)
    )
    _mark(
        recorder,
        "icache_missunit_fencei",
        "fencei_blocks_new_nonduplicate",
        cycle,
        fencei and not flush and (fetch_new_nonduplicate or prefetch_new_nonduplicate),
        evidence,
    )
    _mark(
        recorder,
        "icache_missunit_fencei",
        "fencei_cancels_unissued_mshr",
        cycle,
        fencei and fetch_unissued and prefetch_unissued,
        evidence,
    )
    _mark(
        recorder,
        "icache_missunit_fencei",
        "fencei_marks_issued_mshr",
        cycle,
        fencei and fetch_issued_waiting and prefetch_issued_waiting,
        evidence,
    )
    _mark(
        recorder,
        "icache_missunit_fencei",
        "fencei_suppresses_sram_write",
        cycle,
        fencei_first
        and response_next
        and response_valid
        and _off(signals["corrupt_reg"])
        and _off(signals["denied_reg"]),
        evidence,
    )
    fifo_nonempty = (
        signals["fifo_enq_flag"] is not None and signals["fifo_deq_flag"] is not None
        and signals["fifo_enq_value"] is not None and signals["fifo_deq_value"] is not None
        and (int(signals["fifo_enq_flag"]) != int(signals["fifo_deq_flag"]) or int(signals["fifo_enq_value"]) != int(signals["fifo_deq_value"]))
    )
    fifo_has_unissued_prefetch = fifo_nonempty and any(
        entry is not None
        and 0 <= int(entry) < len(mshrs) - 4
        and _on(mshrs[4 + int(entry)]["valid"])
        and _off(mshrs[4 + int(entry)]["issue"])
        for entry in (signals[f"fifo_entry_{index}"] for index in range(10))
    )
    _mark(
        recorder,
        "icache_missunit_fencei",
        "fencei_clears_prefetch_fifo",
        cycle,
        fencei and fifo_has_unissued_prefetch,
        evidence,
    )
    _mark(
        recorder,
        "icache_missunit_fencei",
        "fencei_redirect_fetch_unissued",
        cycle,
        fencei and flush and fetch_unissued,
        evidence,
    )
    _mark(
        recorder,
        "icache_missunit_fencei",
        "fencei_redirect_fetch_issued",
        cycle,
        fencei and flush and fetch_issued_waiting,
        evidence,
    )
    _mark(
        recorder,
        "icache_missunit_fencei",
        "fencei_redirect_prefetch_unissued",
        cycle,
        fencei and flush and prefetch_unissued,
        evidence,
    )
    _mark(
        recorder,
        "icache_missunit_fencei",
        "fencei_redirect_prefetch_issued",
        cycle,
        fencei and flush and prefetch_issued_waiting,
        evidence,
    )

    opcode = signals["d_opcode"]
    response_next = _on(signals["last_fire_next"])
    response_valid = response_next and _mshr_response_valid(mshrs, signals["id_next"])

    # The RTL invalidates the completed MSHR after presenting the refill
    # response.  Delay the error-bin sample until the following cycle so the
    # coverage point proves that the associated MSHR was actually released.
    pending_release = state["pending_refill_release"]
    if pending_release is not None and cycle == pending_release["response_cycle"] + 1:
        source = pending_release["source"]
        mshr_released = (
            source is not None
            and 0 <= int(source) < len(mshrs)
            and _off(mshrs[int(source)]["valid"])
        )
        evidence["refill_release_source"] = source
        evidence["refill_mshr_released"] = mshr_released
        _mark(
            recorder,
            "icache_missunit_refill",
            pending_release["bin_name"],
            cycle,
            pending_release["base_conditions"] and mshr_released,
            evidence,
        )
        if source is not None:
            state["mshr_request_keys"].pop(int(source), None)
        state["pending_refill_release"] = None
    elif pending_release is not None and cycle > pending_release["response_cycle"] + 1:
        state["pending_refill_release"] = None

    # A refill response is formed one cycle after its final D beat.  Validate
    # the completed transaction against the source-selected MSHR and require
    # both SRAM write enables to be known low for the error bins.
    completed_refill = state["pending_refill"]
    if completed_refill is None:
        # Keep the synthetic unit-test contract useful when lastFire is not
        # modeled and the sampler only sees the two D beats plus lastFireNext.
        completed_refill = {
            "source": state["refill_source"],
            "paddr": state["refill_paddr"],
            "vset": state["refill_vset"],
            "key_valid": state["refill_key_valid"],
            "source_consistent": state["refill_source_consistent"],
            "error_first": state["refill_error_first"],
            "error_second": state["refill_error_second"],
            "clean_beats": state["clean_beats"],
            "complete_doublebeat": False,
            "controls_clear": state["refill_controls_clear"],
        }
    completed_source = completed_refill["source"]
    completed_key = (
        completed_refill["paddr"],
        completed_refill["vset"],
    )
    response_key = None
    if signals["id_next"] is not None:
        response_key = _mshr_key(mshrs, signals["id_next"])
    # MSHR blkPAddr/vSetIdx are the registered key captured from the miss
    # request.  Require both D beats and the response-selected MSHR to retain
    # that key, so the sampled response cannot be attributed to another miss.
    refill_key_matches = (
        completed_refill["key_valid"]
        and completed_refill["source_consistent"]
        and completed_source is not None
        and signals["id_next"] is not None
        and int(completed_source) == int(signals["id_next"])
        and response_key == completed_key
    )
    request_key_matches = (
        completed_source is not None
        and state["mshr_request_keys"].get(int(completed_source)) == completed_key
    )
    writes_known = signals["meta_write"] is not None and signals["data_write"] is not None
    no_sram_write = writes_known and _off(signals["meta_write"]) and _off(signals["data_write"])
    response_corrupt = _on(signals["corrupt_reg"])
    evidence["refill_key_matches"] = refill_key_matches
    evidence["refill_request_key_matches"] = request_key_matches
    evidence["refill_completed_source"] = completed_source
    evidence["refill_completed_paddr"] = completed_refill["paddr"]
    evidence["refill_completed_vset"] = completed_refill["vset"]
    evidence["refill_completed_ptag"] = (
        _ptag_from_blk(completed_refill["paddr"])
        if completed_refill["paddr"] is not None else None
    )
    if response_next:
        clean_response = _off(signals["corrupt_reg"]) and _off(signals["denied_reg"])
        _mark(
            recorder,
            "icache_missunit_refill",
            "clean_doublebeat_refill_write",
            cycle,
            response_valid and clean_response and completed_refill["clean_beats"] >= 2,
            evidence,
        )
        refill_source = state["last_refill_source"]
        source_routes = (
            response_valid
            and state["last_refill_outstanding"] >= 2
            and refill_source is not None
            and 0 <= int(refill_source) < len(mshrs)
        )
        _mark(
            recorder,
            "icache_missunit_refill",
            "source_routes_refill",
            cycle,
            source_routes,
            evidence,
        )
        error_base = (
            response_valid
            and response_corrupt
            and refill_key_matches
            and request_key_matches
            and completed_refill["complete_doublebeat"]
            and completed_refill["controls_clear"]
            and no_sram_write
            and controls_clear
        )
        if error_base and completed_refill["error_first"] and not completed_refill["error_second"]:
            state["pending_refill_release"] = {
                "response_cycle": cycle,
                "source": completed_source,
                "bin_name": "error_first_beat_no_sram_write",
                "base_conditions": True,
            }
        elif error_base and completed_refill["error_second"] and not completed_refill["error_first"]:
            state["pending_refill_release"] = {
                "response_cycle": cycle,
                "source": completed_source,
                "bin_name": "error_second_beat_no_sram_write",
                "base_conditions": True,
            }
        state["pending_refill"] = None
        state["clean_beats"] = 0
        state["refill_beat_index"] = 0
        state["refill_source"] = None
        state["refill_paddr"] = None
        state["refill_vset"] = None
        state["refill_key_valid"] = False
        state["refill_source_consistent"] = True
        state["refill_controls_clear"] = True
        state["refill_error_first"] = False
        state["refill_error_second"] = False
        state["last_refill_source"] = None
        state["last_refill_outstanding"] = 0

    has_data_beat = (
        _on(signals["d_valid"])
        and opcode is not None
        and (int(opcode) & 1) != 0
    )
    if has_data_beat:
        beat_index = state["refill_beat_index"]
        beat_source = signals["d_source"]
        beat_key = _mshr_key(mshrs, beat_source)
        if beat_index == 0:
            state["refill_source"] = beat_source
            state["refill_paddr"] = beat_key[0] if beat_key is not None else None
            state["refill_vset"] = beat_key[1] if beat_key is not None else None
            state["refill_key_valid"] = beat_key is not None
            state["refill_controls_clear"] = controls_clear
        else:
            state["refill_source_consistent"] = state["refill_source_consistent"] and (
                beat_source == state["refill_source"] and beat_key == (
                    state["refill_paddr"], state["refill_vset"]
                )
            )
            state["refill_controls_clear"] = (
                state["refill_controls_clear"] and controls_clear
            )
        beat_corrupt = _on(signals["d_corrupt"])
        if beat_index == 0:
            state["refill_error_first"] = beat_corrupt
        elif beat_index == 1:
            state["refill_error_second"] = beat_corrupt
        if _off(signals["d_corrupt"]) and _off(signals["d_denied"]):
            state["clean_beats"] += 1
        state["refill_beat_index"] += 1
    if _on(signals["last_fire"]):
        state["last_refill_source"] = signals["d_source"]
        state["last_refill_outstanding"] = sum(
            _on(item["valid"]) and _on(item["issue"])
            for item in mshrs
        )
        state["pending_refill"] = {
            "source": state["refill_source"],
            "paddr": state["refill_paddr"],
            "vset": state["refill_vset"],
            "key_valid": state["refill_key_valid"],
            "source_consistent": state["refill_source_consistent"],
            "error_first": state["refill_error_first"],
            "error_second": state["refill_error_second"],
            "clean_beats": state["clean_beats"],
            "complete_doublebeat": (
                has_data_beat
                and state["refill_beat_index"] == 2
                and state["refill_source"] == signals["d_source"]
            ),
            "controls_clear": state["refill_controls_clear"],
        }
    state["last_flush"] = signals["flush"]
    state["last_fencei"] = signals["fencei"]
