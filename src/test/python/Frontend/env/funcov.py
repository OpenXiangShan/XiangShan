from __future__ import annotations


def _dut(recorder):
    return getattr(getattr(recorder, "env", None), "dut", None)


def _read(recorder, name: str, default: int = 0) -> int:
    dut = _dut(recorder)
    if dut is None:
        return int(default)
    return recorder._read_dut_signal(dut, name, default)


def _read_first(recorder, names) -> int | None:
    dut = _dut(recorder)
    if dut is None:
        return None
    return recorder._read_first_dut_signal(dut, names)


def _count_truthy(values) -> int:
    return sum(1 for value in values if int(value or 0) != 0)


_TWO_FETCH_SIGNALS = {
    "ftq_valid": (
        "Frontend_top.Frontend.inner_icache.wayLookup.io_fromFtq_valid",
    ),
    "ftq_ready": (
        "Frontend_top.Frontend.inner_icache.wayLookup.io_fromFtq_ready",
    ),
    "ftq_req1_valid": (
        "Frontend_top.Frontend.inner_icache.wayLookup.io_fromFtq_bits_req_1_valid",
    ),
    "ftq_req0_start": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_fromFtq_toWayLookup_bits_req_0_startVAddr_addr",
    ),
    "ftq_req1_start": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_fromFtq_toWayLookup_bits_req_1_startVAddr_addr",
    ),
    "ftq_req0_end": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_fromFtq_toWayLookup_bits_req_0_takenCfiOffset_bits",
    ),
    "ftq_req1_end": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_fromFtq_toWayLookup_bits_req_1_takenCfiOffset_bits",
    ),
    "ftq_req0_exception": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_fromFtq_toWayLookup_bits_req_0_hasBackendException",
    ),
    "bpu_ptr_flag": (
        "Frontend_top.Frontend.inner_ftq.bpuPtr_ptrs_0_flag",
    ),
    "bpu_ptr_value": (
        "Frontend_top.Frontend.inner_ftq.bpuPtr_ptrs_0_value",
    ),
    "fetch_ptr_flag": (
        "Frontend_top.Frontend.inner_ftq.fetchPtr_ptrs_0_flag",
    ),
    "fetch_ptr_value": (
        "Frontend_top.Frontend.inner_ftq.fetchPtr_ptrs_0_value",
    ),
    "bpu_s3_flush": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_fromFtq_flushFromBpu_s3_valid",
    ),
    "prefetch_valid": (
        "Frontend_top.Frontend.inner_icache.io_fromFtq_toPrefetch_valid",
    ),
    "prefetch_ready": (
        "Frontend_top.Frontend.inner_icache.io_fromFtq_toPrefetch_ready",
    ),
    "prefetch_case": (
        "Frontend_top.Frontend.inner_ftq.io_toICache_toPrefetch_bits_twoPrefetchCase_value_0",
    ),
    "way_req1_valid": (
        "Frontend_top.Frontend.inner_icache.wayLookup.io_fromFtq_bits_req_1_valid",
    ),
    "way_out_valid": (
        "Frontend_top.Frontend.inner_icache.wayLookup.io_fromFtq_valid",
    ),
    "way_out_ready": (
        "Frontend_top.Frontend.inner_icache.wayLookup.io_fromFtq_ready",
    ),
    "way_real_two": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toFtq_fromWayLookup_realTwoFetchValid",
    ),
    "way_num_valid": (
        "Frontend_top.Frontend.inner_icache.wayLookup.numValidEntries",
    ),
    "way_read_ptr_flag": (
        "Frontend_top.Frontend.inner_icache.wayLookup.readPtr_flag",
    ),
    "way_read_ptr_value": (
        "Frontend_top.Frontend.inner_icache.wayLookup.readPtr_value",
    ),
    "way_exception_valid": (
        "Frontend_top.Frontend.inner_icache.wayLookup.exceptionEntry_valid",
    ),
    "way_exception_ptr_flag": (
        "Frontend_top.Frontend.inner_icache.wayLookup.exceptionPtr_flag",
    ),
    "way_exception_ptr_value": (
        "Frontend_top.Frontend.inner_icache.wayLookup.exceptionPtr_value",
    ),
    "main_s1_valid": (
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_valid",
    ),
    "main_req1_valid": (
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_1_valid",
    ),
    "ifu_valid": (
        "Frontend_top.Frontend.inner_icache.mainPipe.io_toIfu_req_valid",
    ),
    "ifu_ready": (
        "Frontend_top.Frontend.inner_icache.mainPipe.io_toIfu_req_ready",
    ),
    "ifu_req1_valid": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_bits_1_valid",
    ),
    "ifu_req0_size": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_bits_0_size",
    ),
    "ifu_first_taken": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_bits_0_takenCfiOffset_valid",
    ),
    "ifu_first_invalid": (
        "Frontend_top.Frontend.inner_ifu.s0_invalidTaken_0",
    ),
    "ifu_fixed_second_valid": (
        "Frontend_top.Frontend.inner_ifu.s0_fixedFetchBlock_1_valid",
    ),
    "ifu_second_valid": (
        "Frontend_top.Frontend.inner_ifu.s2_fetchBlock_1_valid",
    ),
    "to_ibuffer_valid": (
        "Frontend_top.Frontend.inner_ifu.io_toIBuffer_valid",
    ),
    "to_ibuffer_ready": (
        "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_ready",
    ),
    "ifu_flush": (
        "Frontend_top.Frontend.inner_ifu.s1_flush",
    ),
    "checker_valid": (
        "Frontend_top.Frontend.inner_ifu.predChecker.io_resp_stage2Out_checkerRedirect_valid",
    ),
    "checker_select": (
        "Frontend_top.Frontend.inner_ifu.predChecker.__Vtogcov__io_resp_stage2Out_checkerRedirect_bits_selectBlock",
    ),
    "checker_invalid": (
        "Frontend_top.Frontend.inner_ifu.predChecker.__Vtogcov__io_resp_stage2Out_checkerRedirect_bits_invalidTaken",
    ),
}


# Single source of truth for the 2-fetch coverpoint names used by the pilot,
# Toffee covergroups, raw funcov sampler, and testpoint back-annotation.
TWO_FETCH_COVERPOINTS = {
    "two_fetch_ftq_eligibility": "request_eligibility",
    "two_fetch_pointer_advance": "fetch_ptr_step",
    "two_fetch_flush_flow": "flush_stage",
    "two_prefetch_layout": "address_layout",
    "two_fetch_waylookup_result": "serve_width",
    "two_fetch_waylookup_block_reason": "fallback_reason",
    "two_fetch_mainpipe_hit_pattern": "dual_hit_pattern",
    "two_fetch_mainpipe_completion": "completion_mode",
    "two_fetch_ifu_window": "window_width",
    "two_fetch_ifu_source": "source_mapping",
    "two_fetch_cross_block": "boundary_kind",
    "two_fetch_invalid_taken": "invalid_taken_block",
    "two_fetch_checker_priority": "earliest_fault",
    "two_fetch_checker_redirect": "select_block",
    "two_fetch_delivery": "delivery_state",
}


TWO_FETCH_SAMPLER_BIN_KEYS = frozenset(
    {
        ("two_fetch_ftq_eligibility", "eligible_dual"),
        ("two_fetch_ftq_eligibility", "blocked_runahead"),
        ("two_fetch_ftq_eligibility", "blocked_size"),
        ("two_fetch_ftq_eligibility", "blocked_cross_page"),
        ("two_fetch_ftq_eligibility", "blocked_backend_exception"),
        ("two_fetch_pointer_advance", "step_two"),
        ("two_fetch_pointer_advance", "step_one"),
        ("two_fetch_pointer_advance", "wrap_step_two"),
        ("two_fetch_flush_flow", "bpu_s3_drop_before_issue"),
        ("two_prefetch_layout", "same_line"),
        ("two_prefetch_layout", "overlap1"),
        ("two_prefetch_layout", "overlap2"),
        ("two_prefetch_layout", "interleave"),
        ("two_fetch_waylookup_result", "dual_served"),
        ("two_fetch_waylookup_block_reason", "insufficient_meta"),
        ("two_fetch_waylookup_block_reason", "data_bank_conflict"),
        ("two_fetch_waylookup_block_reason", "mmio"),
        ("two_fetch_waylookup_block_reason", "itlb_exception"),
        ("two_fetch_waylookup_result", "single_fallback"),
        ("two_fetch_mainpipe_hit_pattern", "hit_hit"),
        ("two_fetch_mainpipe_hit_pattern", "hit_miss"),
        ("two_fetch_mainpipe_hit_pattern", "miss_hit"),
        ("two_fetch_mainpipe_hit_pattern", "miss_miss"),
        ("two_fetch_mainpipe_completion", "wait_refill_then_dual"),
        ("two_fetch_ifu_window", "dual_window"),
        ("two_fetch_ifu_source", "blocksel_switch"),
        ("two_fetch_cross_block", "taken_separates_blocks"),
        ("two_fetch_ifu_source", "two_ftq_sources"),
        ("two_fetch_cross_block", "rvi_stitch"),
        ("two_fetch_cross_block", "rvc_independent"),
        ("two_fetch_cross_block", "mixed_rvc_rvi"),
        ("two_fetch_invalid_taken", "first_masks_second"),
        ("two_fetch_invalid_taken", "second_redirect"),
        ("two_fetch_checker_priority", "first_masks_second"),
        ("two_fetch_checker_redirect", "first_block"),
        ("two_fetch_checker_redirect", "second_block"),
        ("two_fetch_delivery", "two_ftq_entries_same_cycle"),
        ("two_fetch_delivery", "dual_fire"),
        ("two_fetch_delivery", "dual_stall"),
        ("two_fetch_flush_flow", "backend_redirect_drops_inflight"),
        ("two_fetch_checker_priority", "second_after_first_valid"),
    }
)


def _tf_read(recorder, key: str) -> int | None:
    return _read_first(recorder, _TWO_FETCH_SIGNALS[key])


def _tf_vector(recorder, module: str, template: str, count: int) -> list[int | None]:
    values: list[int | None] = []
    for index in range(int(count)):
        signal = str(template).format(index=index, suffix="" if index == 0 else f"_{index}")
        values.append(
            _read_first(
                recorder,
                (
                    f"Frontend_top.Frontend.{module}.{signal}",
                ),
            )
        )
    return values


def _tf_evidence(event: str, **values) -> dict:
    return {"event": str(event), **{key: value for key, value in values.items() if value is not None}}


def _tf_next_ptr(flag: int, value: int, size: int) -> tuple[int, int]:
    value = int(value) + 1
    if value >= int(size):
        return int(flag) ^ 1, 0
    return int(flag), value


def _tf_waylookup_reasons(recorder) -> dict[str, bool] | None:
    num_valid = _tf_read(recorder, "way_num_valid")
    read_flag = _tf_read(recorder, "way_read_ptr_flag")
    read_value = _tf_read(recorder, "way_read_ptr_value")
    exception_valid = _tf_read(recorder, "way_exception_valid")
    exception_flag = _tf_read(recorder, "way_exception_ptr_flag")
    exception_value = _tf_read(recorder, "way_exception_ptr_value")
    if None in (num_valid, read_flag, read_value, exception_valid, exception_flag, exception_value):
        return None

    waylookup_size = 32
    index0 = int(read_value) % waylookup_size
    index1 = (index0 + 1) % waylookup_size
    updates = _tf_vector(recorder, "inner_icache.wayLookup", "entryUpdate_updated{suffix}", 64)
    mmio = _tf_vector(recorder, "inner_icache.wayLookup", "entries_{index}_isMmio", waylookup_size)
    if any(value is None for value in updates) or any(value is None for value in mmio):
        return None

    update_stall0 = bool(int(updates[index0 * 2]) or int(updates[index0 * 2 + 1]))
    update_stall1 = bool(int(updates[index1 * 2]) or int(updates[index1 * 2 + 1]))
    can_deq_second = int(num_valid) > 1 and not update_stall0 and not update_stall1
    has_mmio = bool(int(mmio[index0]) or int(mmio[index1]))

    read_ptr = (int(read_flag), index0)
    next_ptr = _tf_next_ptr(read_flag, index0, waylookup_size)
    exception_ptr = (int(exception_flag), int(exception_value) % waylookup_size)
    has_itlb_exception = int(exception_valid) == 1 and exception_ptr in (read_ptr, next_ptr)

    return {
        "insufficient_meta": not can_deq_second,
        "mmio": has_mmio,
        "itlb_exception": has_itlb_exception,
        # realTwoFetchValid has only these four blockers. Once the other three
        # are excluded, a remaining fallback is the data SRAM bank conflict.
        "data_bank_conflict": can_deq_second and not has_mmio and not has_itlb_exception,
    }


def sample_two_fetch_coverage(recorder, env, cycle: int) -> None:
    if _dut(recorder) is None:
        return

    cycle = int(cycle)
    backend_cfg = getattr(getattr(env, "config", None), "backend", None)
    ftq_size = int(getattr(backend_cfg, "ftq_size", 64) or 64)

    ftq_valid = _tf_read(recorder, "ftq_valid")
    ftq_ready = _tf_read(recorder, "ftq_ready")
    ftq_req1_valid = _tf_read(recorder, "ftq_req1_valid")
    ftq_fire = ftq_valid == 1 and ftq_ready == 1

    bpu_s3_flush = _tf_read(recorder, "bpu_s3_flush")
    if bpu_s3_flush == 1 and recorder._two_fetch_ftq_pending:
        recorder.mark(
            "two_fetch_flush_flow",
            "bpu_s3_drop_before_issue",
            cycle,
            _tf_evidence("bpu_s3_flush_pending_dual"),
        )
    recorder._two_fetch_ftq_pending = bool(ftq_valid == 1 and ftq_ready == 0 and ftq_req1_valid == 1)

    if ftq_fire and ftq_req1_valid is not None:
        start0 = _tf_read(recorder, "ftq_req0_start")
        start1 = _tf_read(recorder, "ftq_req1_start")
        end0 = _tf_read(recorder, "ftq_req0_end")
        end1 = _tf_read(recorder, "ftq_req1_end")
        exc0 = _tf_read(recorder, "ftq_req0_exception")
        bpu_flag = _tf_read(recorder, "bpu_ptr_flag")
        bpu_value = _tf_read(recorder, "bpu_ptr_value")
        fetch_flag = _tf_read(recorder, "fetch_ptr_flag")
        fetch_value = _tf_read(recorder, "fetch_ptr_value")
        runahead_distance = None
        if None not in (bpu_flag, bpu_value, fetch_flag, fetch_value):
            runahead_distance = recorder._circular_distance(
                int(bpu_flag), int(bpu_value), int(fetch_flag), int(fetch_value), ftq_size
            )
        # PrunedAddr.addr is halfword-addressed, so its bit 11 is virtual-address bit 12.
        cross_page = None not in (start0, start1) and (int(start0) >> 11) != (int(start1) >> 11)
        size_block = None not in (end0, end1) and int(end0) + int(end1) + 2 > 32
        evidence = _tf_evidence(
            "ftq_to_waylookup",
            req1_valid=int(ftq_req1_valid),
            start0=start0,
            start1=start1,
            end0=end0,
            end1=end1,
            exception0=exc0,
            runahead_distance=runahead_distance,
        )
        if int(ftq_req1_valid) == 1:
            recorder.mark("two_fetch_ftq_eligibility", "eligible_dual", cycle, evidence)
        else:
            if runahead_distance is not None and int(runahead_distance) <= 3:
                recorder.mark("two_fetch_ftq_eligibility", "blocked_runahead", cycle, evidence)
            if size_block:
                recorder.mark("two_fetch_ftq_eligibility", "blocked_size", cycle, evidence)
            if cross_page:
                recorder.mark("two_fetch_ftq_eligibility", "blocked_cross_page", cycle, evidence)
            # The generated DUT exposes hasBackendException for req0 only. Do
            # not infer req1's state from unrelated eligibility conditions.
            if exc0 == 1:
                recorder.mark("two_fetch_ftq_eligibility", "blocked_backend_exception", cycle, evidence)

    fetch_flag = _tf_read(recorder, "fetch_ptr_flag")
    fetch_value = _tf_read(recorder, "fetch_ptr_value")
    if None not in (fetch_flag, fetch_value):
        current_ptr = (int(fetch_flag), int(fetch_value))
        last_ptr = recorder._two_fetch_last_fetch_ptr
        if last_ptr is not None:
            delta = recorder._circular_distance(
                current_ptr[0], current_ptr[1], last_ptr[0], last_ptr[1], ftq_size
            )
            evidence = _tf_evidence("fetch_ptr_advance", before=list(last_ptr), after=list(current_ptr), delta=delta)
            if delta == 1:
                recorder.mark("two_fetch_pointer_advance", "step_one", cycle, evidence)
            elif delta == 2:
                recorder.mark("two_fetch_pointer_advance", "step_two", cycle, evidence)
                if current_ptr[1] < last_ptr[1] or current_ptr[0] != last_ptr[0]:
                    recorder.mark("two_fetch_pointer_advance", "wrap_step_two", cycle, evidence)
        recorder._two_fetch_last_fetch_ptr = current_ptr

    prefetch_valid = _tf_read(recorder, "prefetch_valid")
    prefetch_ready = _tf_read(recorder, "prefetch_ready")
    prefetch_case = _tf_read(recorder, "prefetch_case")
    if prefetch_valid == 1 and prefetch_ready == 1 and prefetch_case is not None:
        layout = {1: "same_line", 2: "overlap1", 4: "overlap2", 8: "interleave"}.get(int(prefetch_case))
        if layout is not None:
            recorder.mark(
                "two_prefetch_layout",
                layout,
                cycle,
                _tf_evidence("two_prefetch", case=int(prefetch_case)),
            )

    way_req1 = _tf_read(recorder, "way_req1_valid")
    way_valid = _tf_read(recorder, "way_out_valid")
    way_ready = _tf_read(recorder, "way_out_ready")
    way_real_two = _tf_read(recorder, "way_real_two")
    way_fire = way_valid == 1 and way_ready == 1
    if way_fire and way_req1 == 1 and way_real_two is not None:
        evidence = _tf_evidence(
            "waylookup_to_mainpipe",
            real_two=int(way_real_two),
        )
        if int(way_real_two) == 1:
            recorder.mark("two_fetch_waylookup_result", "dual_served", cycle, evidence)
        else:
            way_reasons = _tf_waylookup_reasons(recorder)
            evidence["reasons"] = way_reasons
            recorder.mark("two_fetch_waylookup_result", "single_fallback", cycle, evidence)
            if way_reasons is not None:
                for bin_name, hit in way_reasons.items():
                    if hit:
                        recorder.mark("two_fetch_waylookup_block_reason", bin_name, cycle, evidence)

    main_valid = _tf_read(recorder, "main_s1_valid")
    main_req1 = _tf_read(recorder, "main_req1_valid")
    should_fetch = _tf_vector(recorder, "inner_icache.mainPipe", "s1_shouldFetch_{index}", 4)
    if main_valid == 1 and main_req1 == 1 and all(value is not None for value in should_fetch):
        req0_miss = bool(int(should_fetch[0]) or int(should_fetch[1]))
        req1_miss = bool(int(should_fetch[2]) or int(should_fetch[3]))
        pattern = {
            (False, False): "hit_hit",
            (False, True): "hit_miss",
            (True, False): "miss_hit",
            (True, True): "miss_miss",
        }[(req0_miss, req1_miss)]
        recorder.mark(
            "two_fetch_mainpipe_hit_pattern",
            pattern,
            cycle,
            _tf_evidence("mainpipe_dual", should_fetch=[int(value) for value in should_fetch]),
        )
        if req0_miss or req1_miss:
            recorder._two_fetch_waiting_refill = True

    ifu_valid = _tf_read(recorder, "ifu_valid")
    ifu_ready = _tf_read(recorder, "ifu_ready")
    ifu_req1 = _tf_read(recorder, "ifu_req1_valid")
    ifu_fire = ifu_valid == 1 and ifu_ready == 1
    if ifu_fire and ifu_req1 == 1:
        recorder._two_fetch_last_dual_cycle = cycle
        recorder.mark("two_fetch_ifu_window", "dual_window", cycle, _tf_evidence("icache_to_ifu_dual"))
        if recorder._two_fetch_waiting_refill:
            recorder.mark(
                "two_fetch_mainpipe_completion",
                "wait_refill_then_dual",
                cycle,
                _tf_evidence("dual_after_refill"),
            )
            recorder._two_fetch_waiting_refill = False

        first_size = _tf_read(recorder, "ifu_req0_size")
        block_sel = _tf_vector(
            recorder,
            "inner_ifu.instrBoundary",
            "io_resp_rawInstrVec_{index}_blockSel",
            31,
        )
        readable_block_sel = [value for value in block_sel if value is not None]
        if first_size is not None and readable_block_sel and any(int(value) for value in readable_block_sel):
            recorder.mark(
                "two_fetch_ifu_source",
                "blocksel_switch",
                cycle,
                _tf_evidence(
                    "ifu_blocksel",
                    first_size=int(first_size),
                    second_block_slots=_count_truthy(readable_block_sel),
                ),
            )

        cross_flags = _tf_vector(
            recorder,
            "inner_ifu.instrBoundary",
            "io_resp_rawInstrVec_{index}_isCrossBlockInstr",
            31,
        )
        readable_cross_flags = [value for value in cross_flags if value is not None]
        if readable_cross_flags and any(int(value) for value in readable_cross_flags):
            recorder.mark(
                "two_fetch_cross_block",
                "rvi_stitch",
                cycle,
                _tf_evidence("cross_block_rvi", cross_count=_count_truthy(readable_cross_flags)),
            )
        if _tf_read(recorder, "ifu_first_taken") == 1 and readable_cross_flags and not any(
            int(value) for value in readable_cross_flags
        ):
            recorder.mark(
                "two_fetch_cross_block",
                "taken_separates_blocks",
                cycle,
                _tf_evidence("first_taken_no_cross_stitch"),
            )

    if ifu_fire and _tf_read(recorder, "ifu_first_invalid") == 1 and _tf_read(
        recorder, "ifu_fixed_second_valid"
    ) == 0:
        recorder.mark(
            "two_fetch_invalid_taken",
            "first_masks_second",
            cycle,
            _tf_evidence("first_invalid_taken"),
        )

    checker_valid = _tf_read(recorder, "checker_valid")
    checker_select = _tf_read(recorder, "checker_select")
    checker_invalid = _tf_read(recorder, "checker_invalid")
    recent_dual = recorder._two_fetch_last_dual_cycle is not None and (
        cycle - int(recorder._two_fetch_last_dual_cycle)
    ) <= 8
    if checker_valid == 1 and checker_select is not None and recent_dual:
        selected_bin = "second_block" if int(checker_select) else "first_block"
        recorder.mark(
            "two_fetch_checker_redirect",
            selected_bin,
            cycle,
            _tf_evidence("checker_redirect", select_block=int(checker_select), invalid_taken=checker_invalid),
        )
        if int(checker_select) == 0:
            recorder.mark(
                "two_fetch_checker_priority",
                "first_masks_second",
                cycle,
                _tf_evidence("checker_first_block"),
            )
        else:
            recorder.mark(
                "two_fetch_checker_priority",
                "second_after_first_valid",
                cycle,
                _tf_evidence("checker_second_block"),
            )
            if checker_invalid == 1:
                recorder.mark(
                    "two_fetch_invalid_taken",
                    "second_redirect",
                    cycle,
                    _tf_evidence("checker_second_invalid_taken"),
                )

    second_valid = _tf_read(recorder, "ifu_second_valid")
    to_ibuffer_valid = _tf_read(recorder, "to_ibuffer_valid")
    to_ibuffer_ready = _tf_read(recorder, "to_ibuffer_ready")
    if second_valid == 1 and to_ibuffer_valid == 1:
        recorder._two_fetch_last_dual_cycle = cycle
        if to_ibuffer_ready == 1:
            recorder.mark("two_fetch_delivery", "dual_fire", cycle, _tf_evidence("to_ibuffer_dual_fire"))
        elif to_ibuffer_ready == 0:
            recorder.mark("two_fetch_delivery", "dual_stall", cycle, _tf_evidence("to_ibuffer_dual_stall"))

    backend_redirect = _read(recorder, "io_backend_toFtq_redirect_valid", 0)
    if backend_redirect == 1 and recent_dual and _tf_read(recorder, "ifu_flush") == 1:
        recorder.mark(
            "two_fetch_flush_flow",
            "backend_redirect_drops_inflight",
            cycle,
            _tf_evidence("backend_redirect_dual_inflight"),
        )


def _classify_block_pos(pc: int) -> str:
    halfword = (int(pc) & 0x3F) >> 1
    if halfword <= 1:
        return "head"
    if halfword >= 30:
        return "tail"
    return "mid"


def _classify_cfi_kind(instr: int, is_rvc: bool) -> str:
    instr = int(instr) & 0xFFFFFFFF
    if bool(is_rvc):
        raw16 = instr & 0xFFFF
        quadrant = raw16 & 0x3
        funct3 = (raw16 >> 13) & 0x7
        if quadrant == 0x1 and funct3 in {0x5, 0x1}:
            return "jal"
        if quadrant == 0x1 and funct3 in {0x6, 0x7}:
            return "branch"
        if quadrant == 0x2 and funct3 == 0x4 and ((raw16 >> 2) & 0x1F) == 0:
            return "jalr"
        return "non_cfi"

    opcode = instr & 0x7F
    if opcode == 0x63:
        return "branch"
    if opcode == 0x6F:
        return "jal"
    if opcode == 0x67:
        return "jalr"
    return "non_cfi"


def _sample_ifu_cfvec_coverage(recorder, cycle: int, slot: int, pc: int, instr: int, is_rvc: bool) -> None:
    size_bin = "rvc_seen" if bool(is_rvc) else "rvi_seen"
    pos_bin = _classify_block_pos(int(pc))
    cfi_bin = _classify_cfi_kind(int(instr), bool(is_rvc))
    evidence = {
        "event": "cfvec",
        "slot": int(slot),
        "pc": int(pc),
        "instr": int(instr) & 0xFFFFFFFF,
        "is_rvc": int(bool(is_rvc)),
        "block_pos": pos_bin,
        "cfi_kind": cfi_bin,
    }

    recorder.mark("ifu_instr_size_type", size_bin, cycle, evidence)
    recorder.mark("ifu_fetch_block_position", pos_bin, cycle, evidence)
    recorder.mark("ifu_cfi_decode_type", cfi_bin, cycle, evidence)

    if bool(is_rvc):
        recorder._ifu_seen_rvc = True
        recorder.mark("ifu_boundary_event", "rvc_start", cycle, evidence)
    else:
        recorder._ifu_seen_rvi = True
        recorder.mark("ifu_boundary_event", "rvi_start", cycle, evidence)

    if recorder._ifu_seen_rvc and recorder._ifu_seen_rvi:
        recorder.mark("ifu_instr_size_type", "mixed_rvi_rvc_seen", cycle, evidence)

    last = getattr(recorder, "_ifu_last_cfvec", None)
    if last is not None:
        last_pc = int(last.get("pc", 0))
        last_is_rvc = bool(last.get("is_rvc", 0))
        expected_step = 2 if last_is_rvc else 4
        actual_step = int(pc) - last_pc
        if 0 < actual_step <= 4:
            if actual_step == expected_step:
                recorder.mark(
                    "ifu_pc_step_type",
                    "step_2b_rvc" if last_is_rvc else "step_4b_rvi",
                    cycle,
                    {**evidence, "last_pc": last_pc, "actual_step": actual_step},
                )
                recorder.mark(
                    "ifu_pc_step_type",
                    "mixed_no_gap_no_dup",
                    cycle,
                    {**evidence, "last_pc": last_pc, "actual_step": actual_step},
                )
            if not last_is_rvc and actual_step == 4:
                recorder.mark(
                    "ifu_boundary_event",
                    "rvi_high_half_suppressed",
                    cycle,
                    {**evidence, "last_pc": last_pc},
                )
    recorder._ifu_last_cfvec = {"pc": int(pc), "is_rvc": int(bool(is_rvc)), "slot": int(slot)}


def _slot_half_region(slot: int) -> str:
    return "front" if int(slot) < 4 else "back"


def sample_cfvec_coverage(recorder, env, cycle: int) -> None:
    dut = getattr(env, "dut", None)
    if dut is None:
        return

    valid_slots: list[int] = []
    cf_entries: list[dict] = []
    saw_cfi = False
    for slot in range(8):
        base = f"io_backend_cfVec_{slot}_"
        if recorder._read_dut_signal(dut, base + "valid", 0) != 1:
            continue

        valid_slots.append(slot)
        pc = int(recorder._read_dut_signal(dut, base + "bits_pc", 0))
        instr = int(recorder._read_dut_signal(dut, base + "bits_instr", 0)) & 0xFFFFFFFF
        is_rvc = bool(recorder._read_dut_signal(dut, base + "bits_isRvc", 0))
        pred_taken = bool(recorder._read_dut_signal(dut, base + "bits_predTaken", 0))
        ftq_flag = recorder._read_dut_signal(dut, base + "bits_ftqPtr_flag", 0)
        ftq_value = recorder._read_dut_signal(dut, base + "bits_ftqPtr_value", 0)
        ex_sum = (
            recorder._read_dut_signal(dut, base + "bits_exceptionVec_1", 0)
            + recorder._read_dut_signal(dut, base + "bits_exceptionVec_2", 0)
            + recorder._read_dut_signal(dut, base + "bits_exceptionVec_12", 0)
            + recorder._read_dut_signal(dut, base + "bits_exceptionVec_19", 0)
            + recorder._read_dut_signal(dut, base + "bits_exceptionVec_20", 0)
        )
        instr = recorder._recover_unavailable_instr(env, int(pc), int(instr), bool(is_rvc), int(ex_sum))
        cf_entries.append(
            {
                "slot": int(slot),
                "pc": int(pc),
                "is_rvc": int(bool(is_rvc)),
                "ftq_ptr": (int(ftq_flag), int(ftq_value)),
            }
        )
        _sample_ifu_cfvec_coverage(recorder, cycle, slot, pc, instr, is_rvc)

        fetch_path = recorder._infer_fetch_path(env, pc, cycle)
        recorder.mark("fetch_path_type", fetch_path, cycle, {"pc": pc})

        opcode = instr & 0x7F
        if not is_rvc and opcode == 0x6F:
            saw_cfi = True
            recorder.mark("bpu_basic_pred_type", "direct_jmp", cycle, {"pc": pc, "instr": instr})
        elif not is_rvc and opcode == 0x63:
            saw_cfi = True
            branch_bin = "cond_taken" if pred_taken else "cond_nt"
            recorder.mark(
                "bpu_basic_pred_type",
                branch_bin,
                cycle,
                {"pc": pc, "instr": instr, "pred_taken": pred_taken},
            )

        recorder._sample_exception_slot(dut, base, slot, pc, cycle, fetch_path)

    if valid_slots and not saw_cfi:
        recorder.mark("bpu_basic_pred_type", "seq_no_cfi", cycle, {"slot_count": len(valid_slots)})

    if valid_slots and recorder._reset_release_cycle is not None and not recorder._boot_recorded:
        recorder.mark(
            "reset_boot_path",
            "seen",
            cycle,
            {"reset_release_cycle": recorder._reset_release_cycle, "slot_count": len(valid_slots)},
        )
        recorder._boot_recorded = True

    recent_dual = recorder._two_fetch_last_dual_cycle is not None and (
        int(cycle) - int(recorder._two_fetch_last_dual_cycle)
    ) <= 64
    unique_ftq_ptrs = []
    for entry in cf_entries:
        if entry["ftq_ptr"] not in unique_ftq_ptrs:
            unique_ftq_ptrs.append(entry["ftq_ptr"])
    if recent_dual and len(unique_ftq_ptrs) >= 2:
        evidence = {
            "event": "backend_cfvec_two_ftq_sources",
            "ftq_ptrs": [list(ptr) for ptr in unique_ftq_ptrs],
            "entries": cf_entries,
        }
        recorder.mark("two_fetch_ifu_source", "two_ftq_sources", cycle, evidence)
        recorder.mark("two_fetch_delivery", "two_ftq_entries_same_cycle", cycle, evidence)
        if {entry["is_rvc"] for entry in cf_entries} == {0, 1}:
            recorder.mark("two_fetch_cross_block", "mixed_rvc_rvi", cycle, evidence)

        for before, after in zip(cf_entries, cf_entries[1:]):
            if before["ftq_ptr"] != after["ftq_ptr"] and before["is_rvc"] == 1:
                recorder.mark(
                    "two_fetch_cross_block",
                    "rvc_independent",
                    cycle,
                    {**evidence, "boundary_slots": [before["slot"], after["slot"]]},
                )
                break


def sample_ftq_coverage(recorder, env, cycle: int) -> None:
    if recorder._reset_release_cycle is None:
        return

    backend_cfg = getattr(getattr(env, "config", None), "backend", None)
    ftq_size = int(getattr(backend_cfg, "ftq_size", 64) or 64)
    bpu_flag = _read_first(
        recorder,
        (
            "Frontend_top.Frontend.inner_ftq.bpuPtr_ptrs_0_flag",
            "Frontend_top.Frontend.ftq.bpuPtr_ptrs_0_flag",
        ),
    )
    bpu_value = _read_first(
        recorder,
        (
            "Frontend_top.Frontend.inner_ftq.bpuPtr_ptrs_0_value",
            "Frontend_top.Frontend.ftq.bpuPtr_ptrs_0_value",
        ),
    )
    commit_flag = _read_first(
        recorder,
        (
            "Frontend_top.Frontend.inner_ftq.commitPtr_ptrs_0_flag",
            "Frontend_top.Frontend.ftq.commitPtr_ptrs_0_flag",
        ),
    )
    commit_value = _read_first(
        recorder,
        (
            "Frontend_top.Frontend.inner_ftq.commitPtr_ptrs_0_value",
            "Frontend_top.Frontend.ftq.commitPtr_ptrs_0_value",
        ),
    )
    if None in (bpu_flag, bpu_value, commit_flag, commit_value):
        return

    occupancy = recorder._circular_distance(
        int(bpu_flag),
        int(bpu_value),
        int(commit_flag),
        int(commit_value),
        int(ftq_size),
    )
    evidence = {
        "event": "ftq_pointer_distance",
        "occupancy": int(occupancy),
        "size": int(ftq_size),
        "bpu_ptr": [int(bpu_flag), int(bpu_value)],
        "commit_ptr": [int(commit_flag), int(commit_value)],
    }
    if occupancy <= 0:
        recorder._mark_ftq_state("empty", cycle, evidence)
    if occupancy >= (ftq_size * 3) // 4:
        recorder._mark_ftq_state("near_full", cycle, evidence)
    if occupancy >= ftq_size - 1:
        recorder._mark_ftq_state("full", cycle, evidence)


def sample_backend_redirect_coverage(recorder, env, cycle: int) -> None:
    target = _read(recorder, "io_backend_toFtq_redirect_bits_target", 0)
    offset = _read(recorder, "io_backend_toFtq_redirect_bits_ftqOffset", 0)
    evidence = {"event": "backend_redirect", "target_pc": int(target), "ftq_offset": int(offset)}

    recorder.mark(
        "bpu_backend_redirect_target_align",
        "word" if (int(target) & 0x3) == 0 else "halfword_only",
        cycle,
        evidence,
    )
    if (int(target) & 0x1) == 0:
        recorder.mark("bpu_backend_redirect_target_align", "halfword", cycle, evidence)

    if int(offset) <= 0:
        offset_bin = "head"
    elif int(offset) >= 7:
        offset_bin = "tail"
    else:
        offset_bin = "mid"
    recorder.mark("bpu_backend_redirect_offset", offset_bin, cycle, evidence)


def sample_bpu_to_ftq_coverage(recorder, env, cycle: int) -> None:
    valid = _read_first(
        recorder,
        (
            "Frontend_top.Frontend.inner_bpu_to_ftq_valid",
            "Frontend_top.Frontend.inner_bpu.io_out_valid",
        ),
    )
    ready = _read_first(
        recorder,
        (
            "Frontend_top.Frontend.inner_bpu_to_ftq_ready",
            "Frontend_top.Frontend.inner_ftq.io_fromBpu_ready",
        ),
    )
    if valid != 1 or ready != 1:
        return

    offset = _read_first(
        recorder,
        (
            "Frontend_top.Frontend.inner_bpu_to_ftq_bits_takenCfiOffset",
            "Frontend_top.Frontend.inner_bpu.io_out_bits_takenCfiOffset",
        ),
    )
    target = _read_first(
        recorder,
        (
            "Frontend_top.Frontend.inner_bpu_to_ftq_bits_target",
            "Frontend_top.Frontend.inner_bpu.io_out_bits_target",
        ),
    )
    evidence = {"event": "bpu_to_ftq", "offset": offset, "target": target}
    if offset is not None:
        if int(offset) <= 0:
            recorder.mark("bpu_internal_prediction_offset", "head", cycle, evidence)
        elif int(offset) >= 7:
            recorder.mark("bpu_internal_prediction_offset", "tail", cycle, evidence)
        else:
            recorder.mark("bpu_internal_prediction_offset", "mid", cycle, evidence)
    if target is not None:
        recorder.mark(
            "bpu_internal_prediction_target_align",
            "word" if (int(target) & 0x3) == 0 else "halfword_only",
            cycle,
            evidence,
        )
        if (int(target) & 0x1) == 0:
            recorder.mark("bpu_internal_prediction_target_align", "halfword", cycle, evidence)


def sample_bpu_v3_basic_prediction_coverage(recorder, env, cycle: int) -> None:
    s3_valid = _read_first(
        recorder,
        (
            "Frontend_top.Frontend.inner_bpu.s3_valid",
            "Frontend_top.Frontend.bpu.s3_valid",
        ),
    )
    if s3_valid != 1:
        return

    taken = _read_first(
        recorder,
        (
            "Frontend_top.Frontend.inner_bpu.s3_s1Prediction_taken",
            "Frontend_top.Frontend.bpu.s3_s1Prediction_taken",
        ),
    )
    cfi_pos = _read_first(
        recorder,
        (
            "Frontend_top.Frontend.inner_bpu.s3_prediction_cfiPosition",
            "Frontend_top.Frontend.bpu.s3_prediction_cfiPosition",
        ),
    )
    if taken is None:
        return

    evidence = {"event": "bpu_v3_s3", "taken": int(taken)}
    if int(taken) == 0:
        recorder.mark("bpu_v3_basic_flow", "no_cfi", cycle, evidence)
    elif cfi_pos is not None:
        cfi_slot = int(cfi_pos) >> 2
        evidence = {**evidence, "cfi_pos": int(cfi_pos), "cfi_slot": cfi_slot}
        recorder.mark("bpu_v3_basic_flow", "has_cfi", cycle, evidence)
        recorder.mark("bpu_v3_taken_slot_half", _slot_half_region(cfi_slot), cycle, evidence)
        recorder.mark(
            "bpu_v3_cfi_offset_region",
            "head" if int(cfi_pos) == 0 else "tail" if int(cfi_pos) >= 15 else "mid",
            cycle,
            evidence,
        )


def sample_bpu_subpredictor_coverage(recorder, env, cycle: int) -> None:
    ubtb_hit = _read_first(
        recorder,
        (
            "Frontend_top.Frontend.inner_bpu.ubtb.t1_hit",
            "Frontend_top.Frontend.bpu.ubtb.t1_hit",
        ),
    )
    if ubtb_hit is not None:
        recorder.mark("bpu_subpred_ubtb_hit", "hit" if int(ubtb_hit) else "miss", cycle, {"event": "ubtb_t1"})
