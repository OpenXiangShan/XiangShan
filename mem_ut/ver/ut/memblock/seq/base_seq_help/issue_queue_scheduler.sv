//=========================================================
//File name    : issue_queue_scheduler.sv
//Author       : OpenAI_Codex
//Module name  : issue_queue_scheduler
//Discribution : lightweight load/STA/STD issue queue router and selector
//Date         : 2026-05-18
//=========================================================
`ifndef ISSUE_QUEUE_SCHEDULER__SV
`define ISSUE_QUEUE_SCHEDULER__SV

class issue_queue_scheduler extends uvm_object;

    common_data_transaction data;

    `uvm_object_utils(issue_queue_scheduler)

    function new(string name = "issue_queue_scheduler");
        super.new(name);
        data = common_data_transaction::get();
    endfunction:new

    function void ensure_data();
        if (data == null) begin
            data = common_data_transaction::get();
        end
    endfunction:ensure_data

    function void prepare_issue_route_for_uid(input memblock_uid_t uid);
        status_transaction status;

        ensure_data();
        status = data.get_status(uid);
        if (!status.active || !status.enq) begin
            `uvm_fatal("ISSUE_Q", $sformatf("prepare_issue_route_for_uid uid=%0d requires active enqueued status", uid))
        end
        data.set_status_field(uid, MEMBLOCK_STATUS_ISSUE_READY, 1'b1);
        route_uid(uid);
    endfunction:prepare_issue_route_for_uid

    function memblock_issue_q_item_t make_empty_item();
        memblock_issue_q_item_t item;

        item.uid         = 0;
        item.rob_key     = '{default:'0};
        item.target      = MEMBLOCK_ISSUE_TARGET_NONE;
        item.send_pri    = 0;
        item.ready_cycle = 0;
        item.replay_seq  = 0;
        item.has_lqIdx   = 1'b0;
        item.lq_key      = '{default:'0};
        item.has_sqIdx   = 1'b0;
        item.sq_key      = '{default:'0};
        item.numLsElem   = '0;
        item.uop_index   = 0;
        item.uop_count   = 0;
        return item;
    endfunction:make_empty_item

    function memblock_issue_q_item_t make_issue_item(input memblock_uid_t uid,
                                                     input memblock_issue_target_e target,
                                                     input memblock_op_behavior_t behavior);
        main_control_transaction main_tr;
        status_transaction       status;
        memblock_issue_q_item_t  item;

        ensure_data();
        main_tr = data.get_main_transaction(uid);
        status  = data.get_status(uid);
        item = make_empty_item();
        item.uid         = uid;
        item.rob_key     = main_tr.get_rob_key();
        item.target      = target;
        item.send_pri    = (target == MEMBLOCK_ISSUE_TARGET_STD) ? main_tr.send_pri_std : main_tr.send_pri;
        item.ready_cycle = main_tr.delay;
        item.replay_seq  = status.replay_seq;
        item.has_lqIdx   = status.active_lq_mapped;
        item.lq_key.flag = status.lqIdx_flag;
        item.lq_key.value = status.lqIdx_value;
        item.has_sqIdx   = status.active_sq_mapped;
        item.sq_key.flag = status.sqIdx_flag;
        item.sq_key.value = status.sqIdx_value;
        item.numLsElem   = behavior.num_ls_elem;
        item.uop_index   = 0;
        item.uop_count   = 1;
        if (behavior.is_atomic && target == MEMBLOCK_ISSUE_TARGET_STA) begin
            item.uop_count = behavior.atomic_sta_uop_count;
        end else if (behavior.is_atomic && target == MEMBLOCK_ISSUE_TARGET_STD) begin
            item.uop_count = behavior.atomic_data_uop_count;
        end
        return item;
    endfunction:make_issue_item

    function bit is_uid_route_ready(input memblock_uid_t uid);
        status_transaction status;

        ensure_data();
        if (data.issue_blocked_by_global_flush()) begin
            return 1'b0;
        end
        status = data.get_status(uid);
        if (status.active &&
            status.enq &&
            status.issue_ready &&
            status.replay_pending &&
            !status.flushed &&
            !status.redirect_pending &&
            !status.exception_pending) begin
            return 1'b1;
        end
        return status.active &&
               status.enq &&
               status.issue_ready &&
               !status.flushed &&
               !status.redirect_pending &&
               !status.exception_pending &&
               !status.replay_pending;
    endfunction:is_uid_route_ready

    function bit target_already_queued_or_done(input status_transaction status,
                                               input memblock_issue_target_e target);
        if (status == null) begin
            `uvm_fatal("ISSUE_Q", "target_already_queued_or_done got null status")
        end
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: return status.queued_load || status.load_dispatched || status.writeback || status.pass;
            MEMBLOCK_ISSUE_TARGET_STA:  return status.queued_sta  || status.sta_dispatched;
            MEMBLOCK_ISSUE_TARGET_STD:  return status.queued_std  || status.std_dispatched;
            default: begin
                `uvm_fatal("ISSUE_Q", $sformatf("target_already_queued_or_done got target=%0d", target))
            end
        endcase
        return 1'b1;
    endfunction:target_already_queued_or_done

    function void set_target_queued(input memblock_uid_t uid,
                                    input memblock_issue_target_e target,
                                    input bit value);
        ensure_data();
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: data.set_status_field(uid, MEMBLOCK_STATUS_QUEUED_LOAD, value);
            MEMBLOCK_ISSUE_TARGET_STA:  data.set_status_field(uid, MEMBLOCK_STATUS_QUEUED_STA, value);
            MEMBLOCK_ISSUE_TARGET_STD:  data.set_status_field(uid, MEMBLOCK_STATUS_QUEUED_STD, value);
            default: begin
                `uvm_fatal("ISSUE_Q", $sformatf("set_target_queued got target=%0d", target))
            end
        endcase
    endfunction:set_target_queued

    function void set_target_dispatched(input memblock_uid_t uid,
                                        input memblock_issue_target_e target,
                                        input bit value);
        ensure_data();
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: data.set_status_field(uid, MEMBLOCK_STATUS_LOAD_DISPATCHED, value);
            MEMBLOCK_ISSUE_TARGET_STA:  data.set_status_field(uid, MEMBLOCK_STATUS_STA_DISPATCHED, value);
            MEMBLOCK_ISSUE_TARGET_STD:  data.set_status_field(uid, MEMBLOCK_STATUS_STD_DISPATCHED, value);
            default: begin
                `uvm_fatal("ISSUE_Q", $sformatf("set_target_dispatched got target=%0d", target))
            end
        endcase
    endfunction:set_target_dispatched

    function void route_target(input memblock_uid_t uid,
                               input memblock_issue_target_e target,
                               input memblock_op_behavior_t behavior);
        status_transaction      status;
        memblock_issue_q_item_t item;

        ensure_data();
        status = data.get_status(uid);
        if (target_already_queued_or_done(status, target)) begin
            return;
        end
        if (status.replay_pending &&
            !data.replay_target_requested(status, target)) begin
            return;
        end
        data.delete_issue_queue_entry(target, uid, status.replay_seq, 1'b0);
        item = make_issue_item(uid, target, behavior);
        data.push_issue_queue_item(item);
        set_target_queued(uid, target, 1'b1);
    endfunction:route_target

    function void route_uid(input memblock_uid_t uid);
        main_control_transaction main_tr;
        memblock_op_behavior_t   behavior;

        ensure_data();
        if (!is_uid_route_ready(uid)) begin
            return;
        end
        main_tr  = data.get_main_transaction(uid);
        behavior = lsq_ctrl_model::derive_op_behavior(main_tr);
        if (behavior.route_load) begin
            route_target(uid, MEMBLOCK_ISSUE_TARGET_LOAD, behavior);
        end
        if (behavior.route_sta) begin
            route_target(uid, MEMBLOCK_ISSUE_TARGET_STA, behavior);
        end
        if (behavior.route_std) begin
            route_target(uid, MEMBLOCK_ISSUE_TARGET_STD, behavior);
        end
    endfunction:route_uid

    function void route_all_ready_uids();
        int unsigned scanned;
        int unsigned scan_limit;
        memblock_uid_t uid;
        memblock_uid_t begin_uid;
        memblock_uid_t end_uid;

        ensure_data();
        if (data.issue_blocked_by_global_flush()) begin
            return;
        end

        data.advance_terminal_done_uid();
        begin_uid = data.get_active_scan_begin_uid();
        end_uid   = data.get_active_scan_end_uid();
        scan_limit = seq_csr_common::get_real_lsq_enq_max();
        scanned = 0;
        // route只在公共活跃窗口内做有限扫描，避免10万笔请求每拍全表遍历。
        for (uid = begin_uid;
             uid < end_uid && scanned < scan_limit;
             uid++) begin
            route_uid(uid);
            scanned++;
        end
    endfunction:route_all_ready_uids

    function void advance_issue_queue_delays();
        ensure_data();
        foreach (data.load_issue_q[idx]) begin
            if (data.load_issue_q[idx].ready_cycle > 0) begin
                data.load_issue_q[idx].ready_cycle--;
            end
        end
        foreach (data.sta_issue_q[idx]) begin
            if (data.sta_issue_q[idx].ready_cycle > 0) begin
                data.sta_issue_q[idx].ready_cycle--;
            end
        end
        foreach (data.std_issue_q[idx]) begin
            if (data.std_issue_q[idx].ready_cycle > 0) begin
                data.std_issue_q[idx].ready_cycle--;
            end
        end
    endfunction:advance_issue_queue_delays

    function bit is_issue_item_eligible(input memblock_issue_q_item_t item);
        ensure_data();
        if (data.issue_blocked_by_global_flush()) begin
            return 1'b0;
        end
        return is_issue_item_state_eligible(item);
    endfunction:is_issue_item_eligible

    function bit is_issue_item_state_eligible(input memblock_issue_q_item_t item);
        status_transaction status;

        ensure_data();
        if (item.target == MEMBLOCK_ISSUE_TARGET_NONE || !data.is_valid_uid(item.uid)) begin
            return 1'b0;
        end
        status = data.get_status(item.uid);
        if (!status.active || !status.enq || !status.issue_ready) begin
            return 1'b0;
        end
        if (status.flushed || status.redirect_pending || status.exception_pending ||
            status.issue_killed) begin
            return 1'b0;
        end
        if (!data.target_replay_seq_match(status, item.target, item.replay_seq) ||
            item.ready_cycle != 0) begin
            return 1'b0;
        end
        case (item.target)
            MEMBLOCK_ISSUE_TARGET_LOAD: return !status.load_dispatched && !status.writeback && !status.pass;
            MEMBLOCK_ISSUE_TARGET_STA:  return !status.sta_dispatched;
            MEMBLOCK_ISSUE_TARGET_STD:  return !status.std_dispatched;
            default: return 1'b0;
        endcase
    endfunction:is_issue_item_state_eligible

    function bit item_is_older(input memblock_issue_q_item_t left,
                               input memblock_issue_q_item_t right);
        if (left.rob_key.flag == right.rob_key.flag && left.rob_key.value == right.rob_key.value) begin
            return left.uid < right.uid;
        end
        return rob_order_util::rob_is_after(right.rob_key, left.rob_key);
    endfunction:item_is_older

    function bit item_is_better(input memblock_issue_q_item_t candidate,
                                input memblock_issue_q_item_t best,
                                input bit compare_pri);
        if (compare_pri) begin
            if (candidate.send_pri > best.send_pri) begin
                return 1'b1;
            end
            if (candidate.send_pri < best.send_pri) begin
                return 1'b0;
            end
        end
        return item_is_older(candidate, best);
    endfunction:item_is_better

    function bit index_already_selected(input int idx, input int selected_indices[$]);
        foreach (selected_indices[i]) begin
            if (selected_indices[i] == idx) begin
                return 1'b1;
            end
        end
        return 1'b0;
    endfunction:index_already_selected

    function int get_target_queue_size(input memblock_issue_target_e target);
        ensure_data();
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: return data.load_issue_q.size();
            MEMBLOCK_ISSUE_TARGET_STA:  return data.sta_issue_q.size();
            MEMBLOCK_ISSUE_TARGET_STD:  return data.std_issue_q.size();
            default: begin
                `uvm_fatal("ISSUE_Q", $sformatf("get_target_queue_size got target=%0d", target))
            end
        endcase
        return 0;
    endfunction:get_target_queue_size

    function memblock_issue_q_item_t get_target_queue_item(input memblock_issue_target_e target,
                                                           input int idx);
        ensure_data();
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: return data.load_issue_q[idx];
            MEMBLOCK_ISSUE_TARGET_STA:  return data.sta_issue_q[idx];
            MEMBLOCK_ISSUE_TARGET_STD:  return data.std_issue_q[idx];
            default: begin
                `uvm_fatal("ISSUE_Q", $sformatf("get_target_queue_item got target=%0d", target))
            end
        endcase
        return make_empty_item();
    endfunction:get_target_queue_item

    function bit find_global_max_send_pri(output int unsigned max_pri);
        memblock_issue_q_item_t item;
        bit found;

        ensure_data();
        max_pri = 0;
        found = 1'b0;
        foreach (data.load_issue_q[idx]) begin
            item = data.load_issue_q[idx];
            if (is_issue_item_eligible(item) && (!found || item.send_pri > max_pri)) begin
                max_pri = item.send_pri;
                found = 1'b1;
            end
        end
        foreach (data.sta_issue_q[idx]) begin
            item = data.sta_issue_q[idx];
            if (is_issue_item_eligible(item) && (!found || item.send_pri > max_pri)) begin
                max_pri = item.send_pri;
                found = 1'b1;
            end
        end
        foreach (data.std_issue_q[idx]) begin
            item = data.std_issue_q[idx];
            if (is_issue_item_eligible(item) && (!found || item.send_pri > max_pri)) begin
                max_pri = item.send_pri;
                found = 1'b1;
            end
        end
        return found;
    endfunction:find_global_max_send_pri

    function void select_target_candidates(input memblock_issue_target_e target,
                                           input int unsigned max_count,
                                           input bit compare_pri,
                                           input bit use_global_pri,
                                           input int unsigned global_pri,
                                           output memblock_issue_q_item_t selected[$]);
        int selected_indices[$];

        selected.delete();
        if (max_count == 0) begin
            return;
        end

        while (selected.size() < max_count) begin
            int best_idx;
            memblock_issue_q_item_t best_item;
            bit found;

            best_idx = -1;
            best_item = make_empty_item();
            found = 1'b0;
            for (int idx = 0; idx < get_target_queue_size(target); idx++) begin
                memblock_issue_q_item_t item;

                if (index_already_selected(idx, selected_indices)) begin
                    continue;
                end
                item = get_target_queue_item(target, idx);
                if (!is_issue_item_eligible(item)) begin
                    continue;
                end
                if (use_global_pri && item.send_pri != global_pri) begin
                    continue;
                end
                if (!found || item_is_better(item, best_item, compare_pri)) begin
                    best_idx = idx;
                    best_item = item;
                    found = 1'b1;
                end
            end

            if (!found) begin
                break;
            end
            selected.push_back(best_item);
            selected_indices.push_back(best_idx);
        end
    endfunction:select_target_candidates

    function void select_issue_candidates(output memblock_issue_q_item_t load_items[$],
                                          output memblock_issue_q_item_t sta_items[$],
                                          output memblock_issue_q_item_t std_items[$]);
        int unsigned global_pri;
        bit          compare_pri;
        bit          use_global_pri;

        ensure_data();
        load_items.delete();
        sta_items.delete();
        std_items.delete();
        if (data.issue_blocked_by_global_flush()) begin
            data.issue_freeze_ack = 1'b1;
            return;
        end

        compare_pri = seq_csr_common::get_send_pri_mode_en();
        use_global_pri = compare_pri && seq_csr_common::sample_global_send_pri_en();
        if (use_global_pri && !find_global_max_send_pri(global_pri)) begin
            use_global_pri = 1'b0;
        end
        select_target_candidates(MEMBLOCK_ISSUE_TARGET_LOAD,
                                 seq_csr_common::sample_load_pip_num(),
                                 compare_pri,
                                 use_global_pri,
                                 global_pri,
                                 load_items);
        select_target_candidates(MEMBLOCK_ISSUE_TARGET_STA,
                                 seq_csr_common::sample_sta_pip_num(),
                                 compare_pri,
                                 use_global_pri,
                                 global_pri,
                                 sta_items);
        select_target_candidates(MEMBLOCK_ISSUE_TARGET_STD,
                                 seq_csr_common::sample_std_pip_num(),
                                 compare_pri,
                                 use_global_pri,
                                 global_pri,
                                 std_items);
    endfunction:select_issue_candidates

    function bit mark_issue_fire(input memblock_issue_q_item_t item);
        int unsigned issue_epoch;

        ensure_data();
        if (data.issue_blocked_by_global_flush()) begin
            return 1'b0;
        end
        if (!is_issue_item_eligible(item)) begin
            return 1'b0;
        end
        issue_epoch = data.alloc_issue_epoch();
        data.mark_issue_snapshot(item.uid, item.target, issue_epoch);
        data.delete_issue_queue_entry(item.target, item.uid, item.replay_seq, 1'b1);
        set_target_queued(item.uid, item.target, 1'b0);
        set_target_dispatched(item.uid, item.target, 1'b1);
        data.clear_replay_target_after_fire(item.uid, item.target);
        return 1'b1;
    endfunction:mark_issue_fire

    function bit mark_issue_fire_already_accepted(input memblock_issue_q_item_t item);
        int unsigned issue_epoch;

        ensure_data();
        if (!is_issue_item_state_eligible(item)) begin
            return 1'b0;
        end
        issue_epoch = data.alloc_issue_epoch();
        data.mark_issue_snapshot(item.uid, item.target, issue_epoch);
        data.delete_issue_queue_entry(item.target, item.uid, item.replay_seq, 1'b1);
        set_target_queued(item.uid, item.target, 1'b0);
        set_target_dispatched(item.uid, item.target, 1'b1);
        data.clear_replay_target_after_fire(item.uid, item.target);
        return 1'b1;
    endfunction:mark_issue_fire_already_accepted

endclass:issue_queue_scheduler

`endif
