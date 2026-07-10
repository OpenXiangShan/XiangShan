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
        ex_sum = (
            recorder._read_dut_signal(dut, base + "bits_exceptionVec_1", 0)
            + recorder._read_dut_signal(dut, base + "bits_exceptionVec_2", 0)
            + recorder._read_dut_signal(dut, base + "bits_exceptionVec_12", 0)
            + recorder._read_dut_signal(dut, base + "bits_exceptionVec_19", 0)
            + recorder._read_dut_signal(dut, base + "bits_exceptionVec_20", 0)
        )
        instr = recorder._recover_unavailable_instr(env, int(pc), int(instr), bool(is_rvc), int(ex_sum))
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
