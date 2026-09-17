"""Real reset/re-entry must not reuse backend history or drive old actions."""

import os

import pytest


def _assert_no_old_actions(record, new_pc):
    assert record["redirect"] == 0 and not any(record["resolves"]), record
    # callRetCommit carries every semantic commit, including non-CFIs with
    # rasAction=0. Check its actual action and fresh DUT-published FTQ owner,
    # rather than incorrectly requiring all commit-valid lanes to stay zero.
    for payload in record["call_ret_payloads"]:
        assert payload["ras_action"] == 0, payload
        assert payload["ftq_start_pc"] is not None and payload["ftq_start_pc"] >= new_pc, payload
    if record["commit"]:
        assert record["commit_start_pc"] is not None and record["commit_start_pc"] >= new_pc, record


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires real Frontend DUT")
def test_backend_reset_starts_fresh_ftq_epoch_and_cancels_old_redirect(env):
    old_pc, new_pc, abandoned_pc = 0x80000000, 0x80010000, 0x80008000
    new_instr = 0x00100393  # ADDI x7,x0,1, independently encoded.
    env.load_program((0x13).to_bytes(4, "little") * 4096, old_pc)
    env.load_program(new_instr.to_bytes(4, "little") * 16384, new_pc)
    env.initialize(reset_vector=old_pc, bare_mode=True)
    for _ in range(2000):
        if env.backend_model.commit_count >= 4:
            break
        env.step(1)
    model = env.backend_model
    assert model.commit_count >= 4 and model._ftq_start_pc_by_value
    assert not env.get_errors()
    previous = {
        "commit_count": model.get_stats()["commit_count"],
        "commit_ptr": [model.commit_ptr_flag, model.commit_ptr_value],
        "pc_entries": dict(model._ftq_start_pc_by_value),
        "reset_epoch": model.hardware_reset_count,
        "monitor_observations": len(env.monitor.observations),
    }
    # Queue a not-yet-driven testbench request. Hardware reset must discard
    # it, not silently retarget its stale FTQ context to the new program.
    model.inject_redirect(abandoned_pc, "ctrl_redirect", delay_cycles=96)
    assert model.pending_events
    queued_cycle = env.current_cycle
    records, paths, ftq_starts = [], {}, {}

    def ftq_value(stem):
        readable = {}
        for prefix in ("Frontend_top.Frontend.inner_ftq.", "Frontend_top.Frontend.inner_ftq.__Vtogcov__"):
            path = prefix + stem
            value = env.functional_coverage._try_read_dut_signal(env.dut, path)
            if value is not None:
                readable[path] = int(value)
        assert readable, {"missing_reset_probe": stem}
        assert len(set(readable.values())) == 1, {"alias_disagreement": readable}
        paths[stem] = list(readable)
        return next(iter(readable.values()))

    def capture(cycle, active_env):
        ctrl = active_env.backend_agent._drive_if
        from_ftq = model.from_ftq_if
        if int(active_env.dut.reset.value):
            ftq_starts.clear()
        elif int(from_ftq.io_backend_fromFtq_wen.value):
            ftq_starts[int(from_ftq.io_backend_fromFtq_ftqIdx.value)] = int(from_ftq.io_backend_fromFtq_startPc_addr.value) << 1
        records.append({
            "cycle": int(cycle), "reset": int(active_env.dut.reset.value),
            "software_commit_ptr": [model.commit_ptr_flag, model.commit_ptr_value],
            "hardware_commit_ptr": [ftq_value("commitPtr_ptrs_0_flag"), ftq_value("commitPtr_ptrs_0_value")],
            "commit": int(ctrl.commit_valid.value), "redirect": int(ctrl.redirect_valid.value),
            "ahead": int(ctrl.ftq_idx_ahead_valid.value),
            "resolves": [int(s.value) for s in ctrl.resolve_valid],
            "call_ret": [int(s.value) for s in ctrl.call_ret_commit_valid],
            "call_ret_payloads": [{
                "lane": i, "ras_action": int(ctrl.call_ret_commit_bits_ras_action[i].value),
                "ftq_value": int(ctrl.call_ret_commit_bits_ftq_ptr_value[i].value),
                "ftq_start_pc": ftq_starts.get(int(ctrl.call_ret_commit_bits_ftq_ptr_value[i].value)),
            } for i in range(8) if int(ctrl.call_ret_commit_valid[i].value)],
            "commit_start_pc": ftq_starts.get(int(ctrl.commit_bits_value.value)),
            "resolve_payloads": [{
                "channel": i, "pc": int(ctrl.resolve_bits_pc_addr[i].value) << 1,
                "target": int(ctrl.resolve_bits_target_addr[i].value) << 1,
                "ftq": [int(ctrl.resolve_bits_ftq_idx_flag[i].value), int(ctrl.resolve_bits_ftq_idx_value[i].value)],
                "branch_type": int(ctrl.resolve_bits_attribute_branch_type[i].value),
                "ras_action": int(ctrl.resolve_bits_attribute_ras_action[i].value),
            } for i in range(3) if int(ctrl.resolve_valid[i].value)],
            "pending_work": model.pending_work_count(),
            "pc_entries": len(model._ftq_start_pc_by_value),
        })

    env.register_cycle_observer(capture)
    env.dut.io_reset_vector_addr.value = new_pc >> 1
    env.reset(20)
    released_at = env.current_cycle
    assert model.hardware_reset_count == previous["reset_epoch"] + 1
    assert model.get_stats()["commit_count"] == previous["commit_count"]
    assert len(env.monitor.observations) == previous["monitor_observations"]
    reset_records = [r for r in records if r["reset"]]
    assert len(reset_records) == 20
    for r in reset_records:
        assert r["software_commit_ptr"] == [0, 0]
        assert r["pending_work"] == r["pc_entries"] == 0
        assert r["commit"] == r["redirect"] == r["ahead"] == 0
        assert not any(r["resolves"]) and not any(r["call_ret"])
    # The first observer edge may precede synchronous hardware update.
    assert all(r["hardware_commit_ptr"] == [0, 0] for r in reset_records[1:])
    for _ in range(2000):
        if len(env.monitor.observations) >= previous["monitor_observations"] + 32:
            break
        env.step(1)
    # Exceed the old event's ready cycle; no delayed stale redirect may escape.
    env.step(max(150, queued_cycle + 120 - env.current_cycle))
    fresh = env.monitor.observations[previous["monitor_observations"]:]
    assert len(fresh) >= 32
    assert [o.pc for o in fresh] == [new_pc + 4 * i for i in range(len(fresh))]
    assert all(o.instr == new_instr and not o.is_rvc for o in fresh)
    for r in records:
        _assert_no_old_actions(r, new_pc)
    assert model.commit_count > 0
    assert model.get_stats()["commit_count"] == previous["commit_count"] + model.commit_count
    assert all(pc >= new_pc for pc in model._ftq_start_pc_by_value.values())
    assert not env.get_errors()
    env._emit_event("backend.reset_epoch_checkpoint", {
        "previous": previous, "release_cycle": released_at,
        "reset_records": reset_records, "signal_paths": paths,
        "recovered_instructions": len(fresh), "post_reset_records": records[-8:],
        "final_backend_stats": model.get_stats(), "abandoned_target": abandoned_pc,
    })
