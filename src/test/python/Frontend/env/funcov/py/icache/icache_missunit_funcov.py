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
        ("icache_missunit_capacity", "fetch_full_duplicate_merge"),
        ("icache_missunit_capacity", "prefetch_full_duplicate_merge"),
        ("icache_missunit_acquire", "fetch_priority_over_prefetch"),
        ("icache_missunit_acquire", "fetch_index_priority"),
        ("icache_missunit_acquire", "prefetch_fifo_enqueue"),
        ("icache_missunit_acquire", "prefetch_fifo_issue_order"),
        ("icache_missunit_acquire", "prefetch_starvation_recovery"),
        ("icache_missunit_acquire", "acquire_backpressure_recovery"),
        ("icache_missunit_dedup", "fetch_merge_any_mshr"),
        ("icache_missunit_dedup", "prefetch_merge_any_mshr"),
        ("icache_missunit_dedup", "key_mismatch_no_merge"),
        ("icache_missunit_flush", "redirect_blocks_new_prefetch"),
        ("icache_missunit_flush", "redirect_cancels_unissued_prefetch"),
        ("icache_missunit_flush", "redirect_marks_issued_prefetch"),
        ("icache_missunit_flush", "redirect_keeps_fetch_mshr"),
        ("icache_missunit_flush", "redirect_suppresses_sram_write"),
        ("icache_missunit_fencei", "fencei_blocks_new_nonduplicate"),
        ("icache_missunit_fencei", "fencei_cancels_unissued_mshr"),
        ("icache_missunit_fencei", "fencei_marks_issued_mshr"),
        ("icache_missunit_fencei", "fencei_suppresses_sram_write"),
        ("icache_missunit_fencei", "fencei_clears_prefetch_fifo"),
        ("icache_missunit_fencei", "fencei_redirect_combined"),
        ("icache_missunit_refill", "clean_doublebeat_refill_write"),
        ("icache_missunit_refill", "source_routes_refill"),
        ("icache_missunit_refill", "error_beats_accumulate"),
        ("icache_missunit_refill", "clean_refill_clears_error"),
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


def _mshr_response_valid(mshrs: list[dict[str, Optional[int]]], source: Optional[int]) -> bool:
    if source is None or not 0 <= int(source) < len(mshrs):
        return False
    item = mshrs[int(source)]
    return _on(item["valid"]) and _off(item["flush"]) and _off(item["fencei"])


def reset_icache_missunit_coverage_state(recorder) -> None:
    recorder._icache_missunit_cov_state = {
        "prefetch_starved": False,
        "acquire_blocked": False,
        "clean_beats": 0,
        "error_response_seen": False,
        "last_refill_source": None,
        "last_refill_outstanding": 0,
        # BIN-686 records the expected allocation here so a checker can
        # validate the registered MSHR contents on the following cycle.
        "pending_fetch_allocations": [],
        "last_fetch_allocation_checkpoint": None,
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
            _names(
                _MAIN + "__Vtogcov__io_missReq_valid",
                _MISS + "fetchDemux.io_in_valid",
                _MISS + "fetchDemux.__Vtogcov__io_in_valid",
            ),
        ),
        "fetch_ready": _read(
            recorder,
            _names(
                _MAIN + "__Vtogcov__io_missReq_ready",
                _MISS + "fetchDemux.io_in_ready",
                _MISS + "fetchDemux.__Vtogcov__io_in_ready",
            ),
        ),
        "fetch_paddr": _read(
            recorder,
            _names(
                _MAIN + "__Vtogcov__io_missReq_bits_blkPAddr",
                _ICACHE + "_mainPipe_io_missReq_bits_blkPAddr",
            ),
        ),
        "fetch_vset": _read(
            recorder,
            _names(
                _MAIN + "__Vtogcov__io_missReq_bits_vSetIdx",
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
                _MISS + "prefetchArb.io_out_valid",
                _MISS + "prefetchArb.__Vtogcov__io_out_valid",
            ),
        ),
        "prefetch_arb_selected": _read(recorder, (_MISS + "prefetchArb.io_sel",)),
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
    fetch_candidates = sum(_on(signals[f"fetch_arb_{index}"]) for index in range(4))
    fetch_full = _all_valid(mshrs, range(4))
    fetch_free_indexes = _free_indexes(mshrs, range(4))
    fetch_expected_index = fetch_free_indexes[0] if fetch_free_indexes else None
    prefetch_full = _all_valid(mshrs, range(4, 14))
    any_mshr = _any_status(mshrs, range(14), "valid")
    any_fetch = _any_status(mshrs, range(4), "valid")
    any_prefetch = _any_status(mshrs, range(4, 14), "valid")
    acquire_fire = _on(signals["acquire_valid"]) and _on(signals["acquire_ready"])
    source = signals["acquire_source"]
    fetch_acquire_fire = acquire_fire and source is not None and int(source) < 4
    prefetch_acquire_fire = acquire_fire and source is not None and int(source) >= 4
    fifo_head = None
    if signals["fifo_deq_value"] is not None:
        fifo_index = int(signals["fifo_deq_value"])
        if 0 <= fifo_index < 10:
            fifo_head = signals[f"fifo_entry_{fifo_index}"]

    flush = _on(signals["flush"])
    fencei = _on(signals["fencei"])
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

    _mark(
        recorder, "icache_missunit_request", "fetch_mshr_allocate", cycle,
        fetch_mshr_allocate, evidence,
    )
    _mark(
        recorder, "icache_missunit_request", "prefetch_mshr_allocate", cycle,
        prefetch_demux_fire, evidence,
    )
    same_key = (
        fetch_valid and prefetch_valid
        and signals["fetch_paddr"] is not None
        and signals["prefetch_paddr"] is not None
        and signals["fetch_vset"] is not None
        and signals["prefetch_vset"] is not None
        and int(signals["fetch_paddr"]) == int(signals["prefetch_paddr"])
        and int(signals["fetch_vset"]) == int(signals["prefetch_vset"])
        # prefetchHit includes this same-cycle fetch/prefetch merge in RTL.
        and not fetch_hit
    )
    different_paddr = (
        fetch_valid and prefetch_valid
        and signals["fetch_paddr"] is not None
        and signals["prefetch_paddr"] is not None
        and int(signals["fetch_paddr"]) != int(signals["prefetch_paddr"])
        and not fetch_hit and not prefetch_hit
    )
    same_paddr_diff_vset = (
        fetch_valid and prefetch_valid
        and signals["fetch_paddr"] is not None
        and signals["prefetch_paddr"] is not None
        and signals["fetch_vset"] is not None
        and signals["prefetch_vset"] is not None
        and int(signals["fetch_paddr"]) == int(signals["prefetch_paddr"])
        and int(signals["fetch_vset"]) != int(signals["prefetch_vset"])
        and not fetch_hit and not prefetch_hit
    )
    _mark(recorder, "icache_missunit_request", "same_key_fetch_prefetch_merge", cycle, same_key, evidence)
    _mark(recorder, "icache_missunit_request", "distinct_key_parallel_allocate", cycle, different_paddr, evidence)
    _mark(recorder, "icache_missunit_request", "same_paddr_diff_vset_separate", cycle, same_paddr_diff_vset, evidence)
    _mark(recorder, "icache_missunit_capacity", "fetch_full_duplicate_merge", cycle, fetch_full and fetch_valid and fetch_hit, evidence)
    _mark(recorder, "icache_missunit_capacity", "prefetch_full_duplicate_merge", cycle, prefetch_full and prefetch_valid and prefetch_hit, evidence)

    _mark(
        recorder, "icache_missunit_acquire", "fetch_priority_over_prefetch", cycle,
        fetch_acquire_fire and _on(signals["prefetch_arb_valid"]), evidence,
    )
    _mark(
        recorder, "icache_missunit_acquire", "fetch_index_priority", cycle,
        fetch_acquire_fire and fetch_candidates >= 2, evidence,
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
    if fetch_candidates and _on(signals["prefetch_arb_valid"]):
        state["prefetch_starved"] = True
    _mark(
        recorder, "icache_missunit_acquire", "prefetch_starvation_recovery", cycle,
        state["prefetch_starved"] and prefetch_acquire_fire and not fetch_candidates, evidence,
    )
    if _on(signals["acquire_valid"]) and _off(signals["acquire_ready"]):
        state["acquire_blocked"] = True
    _mark(
        recorder, "icache_missunit_acquire", "acquire_backpressure_recovery", cycle,
        state["acquire_blocked"] and acquire_fire, evidence,
    )

    _mark(recorder, "icache_missunit_dedup", "fetch_merge_any_mshr", cycle, fetch_valid and fetch_hit, evidence)
    _mark(recorder, "icache_missunit_dedup", "prefetch_merge_any_mshr", cycle, prefetch_valid and prefetch_hit, evidence)
    fetch_mismatch = fetch_demux_fire and any_mshr and not _same_key_exists(mshrs, range(14), signals["fetch_paddr"], signals["fetch_vset"])
    prefetch_mismatch = prefetch_demux_fire and any_mshr and not _same_key_exists(mshrs, range(14), signals["prefetch_paddr"], signals["prefetch_vset"])
    _mark(recorder, "icache_missunit_dedup", "key_mismatch_no_merge", cycle, fetch_mismatch or prefetch_mismatch, evidence)

    _mark(recorder, "icache_missunit_flush", "redirect_blocks_new_prefetch", cycle, flush and prefetch_valid and not prefetch_hit, evidence)
    _mark(recorder, "icache_missunit_flush", "redirect_cancels_unissued_prefetch", cycle, flush and any(_on(item["valid"]) and _off(item["issue"]) for item in mshrs[4:]), evidence)
    _mark(recorder, "icache_missunit_flush", "redirect_marks_issued_prefetch", cycle, flush and any(_on(item["valid"]) and _on(item["issue"]) for item in mshrs[4:]), evidence)
    _mark(recorder, "icache_missunit_flush", "redirect_keeps_fetch_mshr", cycle, flush and any_fetch, evidence)
    response_next = _on(signals["last_fire_next"])
    response_valid = response_next and _mshr_response_valid(mshrs, signals["id_next"])
    writes = _on(signals["meta_write"]) or _on(signals["data_write"])
    _mark(
        recorder,
        "icache_missunit_flush",
        "redirect_suppresses_sram_write",
        cycle,
        flush and response_next and response_valid and not writes,
        evidence,
    )

    _mark(recorder, "icache_missunit_fencei", "fencei_blocks_new_nonduplicate", cycle, fencei and ((fetch_valid and not fetch_hit) or (prefetch_valid and not prefetch_hit)), evidence)
    _mark(recorder, "icache_missunit_fencei", "fencei_cancels_unissued_mshr", cycle, fencei and any(_on(item["valid"]) and _off(item["issue"]) for item in mshrs), evidence)
    _mark(recorder, "icache_missunit_fencei", "fencei_marks_issued_mshr", cycle, fencei and any(_on(item["valid"]) and _on(item["issue"]) for item in mshrs), evidence)
    _mark(
        recorder,
        "icache_missunit_fencei",
        "fencei_suppresses_sram_write",
        cycle,
        fencei and response_next and response_valid and not writes,
        evidence,
    )
    fifo_nonempty = (
        signals["fifo_enq_flag"] is not None and signals["fifo_deq_flag"] is not None
        and signals["fifo_enq_value"] is not None and signals["fifo_deq_value"] is not None
        and (int(signals["fifo_enq_flag"]) != int(signals["fifo_deq_flag"]) or int(signals["fifo_enq_value"]) != int(signals["fifo_deq_value"]))
    )
    _mark(recorder, "icache_missunit_fencei", "fencei_clears_prefetch_fifo", cycle, fencei and fifo_nonempty and _on(signals["fifo_flush"]), evidence)
    _mark(recorder, "icache_missunit_fencei", "fencei_redirect_combined", cycle, fencei and flush and (any_mshr or fifo_nonempty), evidence)

    opcode = signals["d_opcode"]
    has_data_beat = (
        _on(signals["d_valid"])
        and opcode is not None
        and (int(opcode) & 1) != 0
    )
    if has_data_beat:
        if _off(signals["d_corrupt"]) and _off(signals["d_denied"]):
            state["clean_beats"] += 1
        else:
            state["error_response_seen"] = False
    if _on(signals["last_fire"]):
        state["last_refill_source"] = signals["d_source"]
        state["last_refill_outstanding"] = sum(
            _on(item["valid"]) and _on(item["issue"])
            for item in mshrs
        )
    if response_next:
        clean_response = _off(signals["corrupt_reg"]) and _off(signals["denied_reg"])
        _mark(
            recorder,
            "icache_missunit_refill",
            "clean_doublebeat_refill_write",
            cycle,
            response_valid and clean_response and state["clean_beats"] >= 2,
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
        errored = _on(signals["corrupt_reg"]) or _on(signals["denied_reg"])
        _mark(
            recorder,
            "icache_missunit_refill",
            "error_beats_accumulate",
            cycle,
            response_valid and errored,
            evidence,
        )
        _mark(
            recorder,
            "icache_missunit_refill",
            "clean_refill_clears_error",
            cycle,
            response_valid and state["error_response_seen"] and clean_response,
            evidence,
        )
        state["error_response_seen"] = errored
        state["clean_beats"] = 0
        state["last_refill_source"] = None
        state["last_refill_outstanding"] = 0
