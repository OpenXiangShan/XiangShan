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
        main_control_transaction main_tr;

        ensure_data();
        status = data.get_status(uid);
        main_tr = data.get_main_transaction(uid);
        if (!status.active || !status.enq) begin
            `uvm_fatal("ISSUE_Q", $sformatf("prepare_issue_route_for_uid uid=%0d requires active enqueued status", uid))
        end
        data.set_status_field(uid, MEMBLOCK_STATUS_ISSUE_READY, 1'b1);
        // 中文注释：控制 UID 仅借用公共 admission 可见性，不能创建 load/STA/STD
        // issue item；完成条件由 control service 与专用 commit 分支维护。
        if (is_control_op_class(main_tr.op_class)) begin
            return;
        end
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
        item.dynamic_epoch = 0;
        item.uncache_retry_boost = 1'b0;
        item.uncache_retry_count = 0;
        item.uncache_retry_identity_valid = 1'b0;
        item.uncache_retry_has_dynamic_epoch = 1'b0;
        item.uncache_retry_dynamic_epoch = 0;
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
        item.dynamic_epoch = status.dynamic_epoch;
        item.uncache_retry_count = status.uncache_retry_count;
        item.uncache_retry_identity_valid = status.uncache_retry_identity_valid &&
                                            status.uncache_retry_has_dynamic_epoch &&
                                            status.uncache_retry_rob_key == item.rob_key &&
                                            status.uncache_retry_dynamic_epoch == status.dynamic_epoch;
        item.uncache_retry_boost = status.uncache_retry_boost &&
                                   item.uncache_retry_identity_valid;
        item.uncache_retry_has_dynamic_epoch = status.uncache_retry_has_dynamic_epoch;
        item.uncache_retry_dynamic_epoch = status.uncache_retry_dynamic_epoch;
        if (item.uncache_retry_boost) begin
            // The original randomized dispatch delay belongs to the first
            // issue attempt.  Reloading it after every Uncache-full redirect
            // leaves the boosted owner ineligible while younger requests can
            // refill all entries, recreating the starvation loop.
            item.ready_cycle = 0;
        end
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
        if (item.uncache_retry_boost) begin
            `uvm_info("UNCACHE_RETRY_BOOST",
                      $sformatf("issue item snapshot uid=%0d target=%0d boost=%0d count=%0d dyn=%0d rob=%0d/%0d",
                                uid, target, item.uncache_retry_boost,
                                item.uncache_retry_count, item.dynamic_epoch,
                                item.rob_key.flag, item.rob_key.value), UVM_HIGH)
        end
        return item;
    endfunction:make_issue_item

    function bit is_uid_route_ready(input memblock_uid_t uid);
        status_transaction status;

        ensure_data();
        if (data.issue_blocked_by_global_flush()) begin
            return 1'b0;
        end
        if (!memblock_sync_pkg::pbmt_rf_issue_gate(uid)) begin
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
        // 中文注释：STA/STD 只有在 live SQ owner 存在时才能 route；SQ deq 后的
        // 未 fire item 属于 stale 状态，不允许重新创建、重路由或发射。
        if ((target == MEMBLOCK_ISSUE_TARGET_STA && !status.active_sq_mapped) ||
            (target == MEMBLOCK_ISSUE_TARGET_STD && !status.active_sq_mapped)) begin
            return;
        end
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
        main_tr = data.get_main_transaction(uid);
        if (is_control_op_class(main_tr.op_class)) begin
            return;
        end
        if (!is_uid_route_ready(uid)) begin
            return;
        end
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
        scan_limit = MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM;
        scanned = 0;
        // route只在公共活跃窗口内做有限扫描，避免10万笔请求每拍全表遍历。
        for (uid = begin_uid;
             uid < end_uid && scanned < scan_limit;
             uid++) begin
            route_uid(uid);
            scanned++;
        end
    endfunction:route_all_ready_uids

    function bit has_pending_issue_work();
        ensure_data();
        if (data == null) begin
            `uvm_fatal("ISSUE_Q", "has_pending_issue_work got null common data")
        end
        return data.load_issue_q.size() != 0 ||
               data.sta_issue_q.size()  != 0 ||
               data.std_issue_q.size()  != 0;
    endfunction:has_pending_issue_work

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
        bit                eligible;
        ensure_data();
        if (item.target == MEMBLOCK_ISSUE_TARGET_NONE || !data.is_valid_uid(item.uid)) begin
            return 1'b0;
        end
        status = data.get_status(item.uid);
        if (!status.active || !status.enq || !status.issue_ready) begin
            return 1'b0;
        end
        if (status.dynamic_epoch != item.dynamic_epoch) begin
            return 1'b0;
        end
        if (status.flushed || status.redirect_pending || status.issue_killed) begin
            return 1'b0;
        end
        // 真实 V2 的 STA/STD 分属独立 IQ。STA fault 已观测但 fault redirect
        // 尚未生效时，仅同一 store 的未发 STD 仍可能合法 fire；其它 target
        // 继续被 exception_pending 阻塞。
        if (status.exception_pending &&
            !(item.target == MEMBLOCK_ISSUE_TARGET_STD && status.sta_fault)) begin
            return 1'b0;
        end
        if (!data.target_replay_seq_match(status, item.target, item.replay_seq) ||
            item.ready_cycle != 0) begin
            return 1'b0;
        end
        case (item.target)
            MEMBLOCK_ISSUE_TARGET_LOAD: eligible = !status.load_dispatched && !status.writeback && !status.pass;
            MEMBLOCK_ISSUE_TARGET_STA:  eligible = !status.sta_dispatched;
            MEMBLOCK_ISSUE_TARGET_STD:  eligible = !status.std_dispatched &&
                                                    status.queued_std &&
                                                    !status.std_writeback &&
                                                    !status.std_fault &&
                                                    status.active_sq_mapped &&
                                                    item.has_sqIdx &&
                                                    item.sq_key.flag == status.sqIdx_flag &&
                                                    item.sq_key.value == status.sqIdx_value;
            default: eligible = 1'b0;
        endcase
        if (item.uid == 31 && item.target == MEMBLOCK_ISSUE_TARGET_STD && !eligible) begin
            `uvm_info("UID31_STD_TRACE",
                      $sformatf("STD eligibility=0 item dyn/rob/sq=%0d/%0d/%0d/%0d/%0d status active/enq/ready/queued/dispatched/wb/fault/exc/killed/active_sq=%0d/%0d/%0d/%0d/%0d/%0d/%0d/%0d/%0d/%0d",
                                item.dynamic_epoch, item.rob_key.flag, item.rob_key.value,
                                item.sq_key.flag, item.sq_key.value,
                                status.active, status.enq, status.issue_ready,
                                status.queued_std, status.std_dispatched, status.std_writeback,
                                status.std_fault, status.exception_pending, status.issue_killed,
                                status.active_sq_mapped), UVM_LOW)
        end
        return eligible;
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

    function bit item_has_live_uncache_retry_boost(
        input memblock_issue_q_item_t item
    );
        status_transaction status;

        ensure_data();
        if (!item.uncache_retry_boost || !item.uncache_retry_identity_valid ||
            !item.uncache_retry_has_dynamic_epoch ||
            !data.is_valid_uid(item.uid)) begin
            return 1'b0;
        end
        status = data.get_status(item.uid);
        return status.uncache_retry_boost &&
               status.uncache_retry_identity_valid &&
               status.uncache_retry_has_dynamic_epoch &&
               status.uncache_retry_rob_key == item.rob_key &&
               status.uncache_retry_dynamic_epoch == item.dynamic_epoch &&
               item.uncache_retry_dynamic_epoch == item.dynamic_epoch;
    endfunction:item_has_live_uncache_retry_boost

    function bit same_issue_item_identity(input memblock_issue_q_item_t left,
                                          input memblock_issue_q_item_t right);
        return left.uid == right.uid &&
               left.target == right.target &&
               left.rob_key == right.rob_key &&
               left.replay_seq == right.replay_seq &&
               left.dynamic_epoch == right.dynamic_epoch;
    endfunction:same_issue_item_identity

    function bit find_oldest_uncache_retry_boost(
        output memblock_issue_q_item_t winner
    );
        memblock_issue_q_item_t item;
        bit found;

        ensure_data();
        winner = make_empty_item();
        found = 1'b0;
        foreach (data.load_issue_q[idx]) begin
            item = data.load_issue_q[idx];
            if (is_issue_item_eligible(item) &&
                item_has_live_uncache_retry_boost(item) &&
                (!found || item_is_older(item, winner))) begin
                winner = item;
                found = 1'b1;
            end
        end
        foreach (data.sta_issue_q[idx]) begin
            item = data.sta_issue_q[idx];
            if (is_issue_item_eligible(item) &&
                item_has_live_uncache_retry_boost(item) &&
                (!found || item_is_older(item, winner))) begin
                winner = item;
                found = 1'b1;
            end
        end
        foreach (data.std_issue_q[idx]) begin
            item = data.std_issue_q[idx];
            if (is_issue_item_eligible(item) &&
                item_has_live_uncache_retry_boost(item) &&
                (!found || item_is_older(item, winner))) begin
                winner = item;
                found = 1'b1;
            end
        end
        return found;
    endfunction:find_oldest_uncache_retry_boost

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
                                           input bit boost_winner_valid,
                                           input memblock_issue_q_item_t boost_winner,
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
                if (boost_winner_valid &&
                    !same_issue_item_identity(item, boost_winner)) begin
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
        int unsigned load_max_count;
        int unsigned sta_max_count;
        int unsigned std_max_count;
        bit          compare_pri;
        bit          use_global_pri;
        bit          boost_winner_valid;
        memblock_issue_q_item_t boost_winner;

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
        load_max_count = seq_csr_common::sample_load_pip_num();
        sta_max_count = seq_csr_common::sample_sta_pip_num();
        std_max_count = seq_csr_common::sample_std_pip_num();
        boost_winner_valid = find_oldest_uncache_retry_boost(boost_winner);
        global_pri = 0;
        if (boost_winner_valid) begin
            // One live retry token owns this cycle across LOAD/STA/STD.  The
            // ordinary global-priority filter must not discard that owner.
            use_global_pri = 1'b0;
            load_max_count = (boost_winner.target == MEMBLOCK_ISSUE_TARGET_LOAD) ? 1 : 0;
            sta_max_count = (boost_winner.target == MEMBLOCK_ISSUE_TARGET_STA) ? 1 : 0;
            std_max_count = (boost_winner.target == MEMBLOCK_ISSUE_TARGET_STD) ? 1 : 0;
            `uvm_info("UNCACHE_RETRY_BOOST",
                      $sformatf("global winner uid=%0d target=%0d count=%0d dyn=%0d rob=%0d/%0d send_pri=%0d ready=%0d replay=%0d",
                                boost_winner.uid, boost_winner.target,
                                boost_winner.uncache_retry_count,
                                boost_winner.dynamic_epoch,
                                boost_winner.rob_key.flag,
                                boost_winner.rob_key.value,
                                boost_winner.send_pri,
                                boost_winner.ready_cycle,
                                boost_winner.replay_seq), UVM_HIGH)
        end else if (use_global_pri && !find_global_max_send_pri(global_pri)) begin
            use_global_pri = 1'b0;
        end
        select_target_candidates(MEMBLOCK_ISSUE_TARGET_LOAD,
                                 load_max_count,
                                 compare_pri,
                                 use_global_pri,
                                 global_pri,
                                 boost_winner_valid,
                                 boost_winner,
                                 load_items);
        select_target_candidates(MEMBLOCK_ISSUE_TARGET_STA,
                                 sta_max_count,
                                 compare_pri,
                                 use_global_pri,
                                 global_pri,
                                 boost_winner_valid,
                                 boost_winner,
                                 sta_items);
        select_target_candidates(MEMBLOCK_ISSUE_TARGET_STD,
                                 std_max_count,
                                 compare_pri,
                                 use_global_pri,
                                 global_pri,
                                 boost_winner_valid,
                                 boost_winner,
                                 std_items);
        if (boost_winner_valid) begin
            `uvm_info("UNCACHE_RETRY_BOOST",
                      $sformatf("local winners load=%0d sta=%0d std=%0d selected_uid=%0d target=%0d",
                                load_items.size(), sta_items.size(), std_items.size(),
                                boost_winner.uid, boost_winner.target), UVM_HIGH)
            if (load_items.size() + sta_items.size() + std_items.size() != 1) begin
                `uvm_fatal("UNCACHE_RETRY_BOOST",
                           $sformatf("single winner token selected %0d candidates",
                                     load_items.size() + sta_items.size() + std_items.size()))
            end
        end
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
        if (!data.mark_issue_snapshot(item.uid, item.target, issue_epoch)) begin
            return 1'b0;
        end
        data.delete_issue_queue_entry(item.target, item.uid, item.replay_seq, 1'b1);
        set_target_queued(item.uid, item.target, 1'b0);
        set_target_dispatched(item.uid, item.target, 1'b1);
        data.clear_replay_target_after_fire(item.uid, item.target);
        void'(data.consume_uncache_retry_boost_after_issue(item));
        return 1'b1;
    endfunction:mark_issue_fire

    function bit mark_issue_fire_already_accepted(input memblock_issue_q_item_t item);
        int unsigned issue_epoch;

        ensure_data();
        if (!is_issue_item_state_eligible(item)) begin
            return 1'b0;
        end
        if (!data.mark_issue_snapshot(item.uid, item.target, issue_epoch)) begin
            return 1'b0;
        end
        data.delete_issue_queue_entry(item.target, item.uid, item.replay_seq, 1'b1);
        set_target_queued(item.uid, item.target, 1'b0);
        set_target_dispatched(item.uid, item.target, 1'b1);
        data.clear_replay_target_after_fire(item.uid, item.target);
        void'(data.consume_uncache_retry_boost_after_issue(item));
        return 1'b1;
    endfunction:mark_issue_fire_already_accepted

    // 中文注释：普通 issue eligibility 会在 fault 后阻止新的 candidate，但不能否认
    // fault 发生前已经冻结并由 driver 真实 fire 的 candidate。该函数只做身份/生命周期
    // 校验，不修改公共状态；真正的状态转换由 mark_issue_fire_from_frozen_candidate() 完成。
    function bit is_frozen_issue_item_fire_acceptable(input memblock_issue_q_item_t item);
        status_transaction       status;
        main_control_transaction main_tr;

        ensure_data();
        if (data.issue_blocked_by_global_flush() ||
            item.target == MEMBLOCK_ISSUE_TARGET_NONE ||
            !data.is_valid_uid(item.uid)) begin
            return 1'b0;
        end

        status = data.get_status(item.uid);
        if (!status.active || !status.enq || !status.issue_ready ||
            status.terminal_done || status.flushed || status.redirect_pending ||
            status.issue_killed || !status.fault || !status.exception_pending ||
            status.dynamic_epoch != item.dynamic_epoch || item.ready_cycle != 0 ||
            !data.target_replay_seq_match(status, item.target, item.replay_seq)) begin
            return 1'b0;
        end

        main_tr = data.get_main_transaction(item.uid);
        if (main_tr.get_rob_key() != item.rob_key || status.get_rob_key() != item.rob_key) begin
            return 1'b0;
        end

        case (item.target)
            MEMBLOCK_ISSUE_TARGET_LOAD: begin
                if (!status.queued_load || status.load_dispatched || status.load_writeback ||
                    status.load_pass || status.load_fault || !status.active_lq_mapped ||
                    item.has_lqIdx != status.active_lq_mapped ||
                    item.lq_key.flag != status.lqIdx_flag ||
                    item.lq_key.value != status.lqIdx_value) begin
                    return 1'b0;
                end
            end
            MEMBLOCK_ISSUE_TARGET_STA: begin
                if (!status.queued_sta || status.sta_dispatched || status.sta_writeback ||
                    status.sta_pass || status.sta_fault || !status.active_sq_mapped ||
                    item.has_sqIdx != status.active_sq_mapped ||
                    item.sq_key.flag != status.sqIdx_flag ||
                    item.sq_key.value != status.sqIdx_value) begin
                    return 1'b0;
                end
            end
            MEMBLOCK_ISSUE_TARGET_STD: begin
                if (!status.queued_std || status.std_dispatched || status.std_writeback ||
                    status.std_pass || status.std_fault || !status.active_sq_mapped ||
                    item.has_sqIdx != status.active_sq_mapped ||
                    item.sq_key.flag != status.sqIdx_flag ||
                    item.sq_key.value != status.sqIdx_value) begin
                    return 1'b0;
                end
            end
            default: return 1'b0;
        endcase
        return 1'b1;
    endfunction:is_frozen_issue_item_fire_acceptable

    // 中文注释：只给已经真实 fire、但在 fire 前后恰好遇到 fault 的冻结 candidate
    // 建立 issue snapshot。它不放宽正常仲裁，也不允许 global flush/redirect 后的旧实例复用。
    function bit mark_issue_fire_from_frozen_candidate(input memblock_issue_q_item_t item);
        int unsigned issue_epoch;

        ensure_data();
        if (!is_frozen_issue_item_fire_acceptable(item)) begin
            return 1'b0;
        end
        if (!data.mark_issue_snapshot(item.uid, item.target, issue_epoch)) begin
            return 1'b0;
        end
        data.delete_issue_queue_entry(item.target, item.uid, item.replay_seq, 1'b1);
        set_target_queued(item.uid, item.target, 1'b0);
        set_target_dispatched(item.uid, item.target, 1'b1);
        data.clear_replay_target_after_fire(item.uid, item.target);
        void'(data.consume_uncache_retry_boost_after_issue(item));
        `uvm_info("ISSUE_Q",
                  $sformatf("accepted fault-inflight issue uid=%0d target=%0d dynamic_epoch=%0d issue_epoch=%0d",
                            item.uid, item.target, item.dynamic_epoch, issue_epoch),
                  UVM_HIGH)
        return 1'b1;
    endfunction:mark_issue_fire_from_frozen_candidate

endclass:issue_queue_scheduler

`endif
