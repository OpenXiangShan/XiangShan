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

    taken_bits = [
        _read_first(
            recorder,
            (
                f"Frontend_top.Frontend.inner_bpu.s3_firstTakenBranchOH_{idx}",
                f"Frontend_top.Frontend.bpu.s3_firstTakenBranchOH_{idx}",
            ),
        )
        for idx in range(8)
    ]
    taken_count = _count_truthy(taken_bits)
    evidence = {"event": "bpu_v3_s3", "taken_bits": [int(v or 0) for v in taken_bits]}
    recorder.mark("bpu_v3_basic_flow", "has_cfi" if taken_count else "no_cfi", cycle, evidence)
    if taken_count:
        first_idx = next(idx for idx, value in enumerate(taken_bits) if int(value or 0) != 0)
        recorder.mark("bpu_v3_taken_slot_half", "front" if first_idx < 4 else "back", cycle, evidence)
        recorder.mark(
            "bpu_v3_cfi_offset_region",
            "head" if first_idx == 0 else "tail" if first_idx >= 7 else "mid",
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

