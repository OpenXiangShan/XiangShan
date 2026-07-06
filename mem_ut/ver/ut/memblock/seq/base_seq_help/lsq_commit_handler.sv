//=========================================================
//File name    : lsq_commit_handler.sv
//Author       : OpenAI_Codex
//Module name  : lsq_commit_handler
//Discribution : LSQ commit stimulus and deq status helper
//Date         : 2026-05-18
//=========================================================
`ifndef LSQ_COMMIT_HANDLER__SV
`define LSQ_COMMIT_HANDLER__SV

class lsq_commit_handler extends uvm_object;

    common_data_transaction data;
    lsq_ctrl_model          lsq_ctrl;

    memblock_uid_t     commit_cursor_uid;
    memblock_rob_key_t last_pending_ptr;

    `uvm_object_utils(lsq_commit_handler)

    function new(string name = "lsq_commit_handler");
        super.new(name);
        data              = common_data_transaction::get();
        lsq_ctrl          = null;
        commit_cursor_uid = 0;
        last_pending_ptr  = '{default:'0};
    endfunction:new

    function void bind_lsq_ctrl(input lsq_ctrl_model ctrl);
        if (ctrl == null) begin
            `uvm_fatal("LSQ_COMMIT", "bind_lsq_ctrl got null lsq_ctrl")
        end
        lsq_ctrl = ctrl;
    endfunction:bind_lsq_ctrl

    function void ensure_handles();
        if (data == null) begin
            data = common_data_transaction::get();
        end
        if (lsq_ctrl == null) begin
            lsq_ctrl = lsq_ctrl_model::get();
        end
    endfunction:ensure_handles

    function void report_deq_mismatch(input string msg);
        if (seq_csr_common::is_initialized() &&
            seq_csr_common::get_lsq_resync_on_mismatch()) begin
            `uvm_warning("LSQ_COMMIT", msg)
        end else begin
            `uvm_fatal("LSQ_COMMIT", msg)
        end
    endfunction:report_deq_mismatch

    function bit uid_is_normal_commit_candidate(input memblock_uid_t uid);
        status_transaction status;

        ensure_handles();
        status = data.get_status(uid);
        return status.active &&
               status.writeback &&
               status.pass &&
               data.required_targets_done(uid) &&
               !status.rob_commit &&
               !status.fault &&
               !status.exception_pending &&
               !status.replay_pending &&
               !status.redirect_pending &&
               !status.flushed &&
               !status.issue_killed;
    endfunction:uid_is_normal_commit_candidate

    function bit uid_is_fault_terminal_candidate(input memblock_uid_t uid);
        status_transaction status;

        ensure_handles();
        status = data.get_status(uid);
        if (!status.active || status.rob_commit ||
            status.replay_pending || status.redirect_pending ||
            status.flushed || status.issue_killed) begin
            return 1'b0;
        end
        if (!status.writeback &&
            !status.load_fault && !status.sta_fault && !status.std_fault) begin
            return 1'b0;
        end
        return status.fault ||
               status.exception_pending ||
               status.load_fault ||
               status.sta_fault ||
               status.std_fault;
    endfunction:uid_is_fault_terminal_candidate

    function bit uid_is_commit_candidate(input memblock_uid_t uid);
        ensure_handles();
        if (data.issue_blocked_by_global_flush()) begin
            return 1'b0;
        end
        return uid_is_normal_commit_candidate(uid) ||
               uid_is_fault_terminal_candidate(uid);
    endfunction:uid_is_commit_candidate

    function void advance_commit_cursor_past_done();
        ensure_handles();
        while (commit_cursor_uid < data.main_trans_num) begin
            status_transaction status;

            status = data.get_status(commit_cursor_uid);
            // flushed不是终态，不能被commit cursor当作完成项跳过；它必须先redirect reissue并最终terminal_done。
            if (status.terminal_done) begin
                commit_cursor_uid++;
            end else begin
                break;
            end
        end
    endfunction:advance_commit_cursor_past_done

    function void select_rob_commit_batch(output memblock_uid_t uids[$]);
        memblock_uid_t uid;

        ensure_handles();
        uids.delete();
        if (data.issue_blocked_by_global_flush()) begin
            return;
        end
        advance_commit_cursor_past_done();
        uid = commit_cursor_uid;
        while (uid < data.main_trans_num && uids.size() < MEMBLOCK_COMMIT_WIDTH) begin
            if (data.get_status(uid).terminal_done) begin
                commit_cursor_uid = uid + 1;
                uid++;
                continue;
            end
            if (uid_is_normal_commit_candidate(uid)) begin
                uids.push_back(uid);
                uid++;
                continue;
            end
            if (uid_is_fault_terminal_candidate(uid)) begin
                uids.push_back(uid);
                break;
            end
            break;
        end
    endfunction:select_rob_commit_batch

    function void clear_lsqcommit_xaction(input lsqcommit_agent_agent_xaction tr);
        if (tr == null) begin
            `uvm_fatal("LSQ_COMMIT", "clear_lsqcommit_xaction got null transaction")
        end
        tr.io_ooo_to_mem_lsqio_pendingPtr_flag  = last_pending_ptr.flag;
        tr.io_ooo_to_mem_lsqio_pendingPtr_value = last_pending_ptr.value;
        tr.io_ooo_to_mem_flushSb                = 1'b0;
    endfunction:clear_lsqcommit_xaction

    function void build_lsqcommit_xaction(output lsqcommit_agent_agent_xaction tr,
                                          output memblock_uid_t commit_uids[$],
                                          output bit has_commit);
        memblock_rob_key_t pending_ptr;

        ensure_handles();
        select_rob_commit_batch(commit_uids);
        has_commit = commit_uids.size() != 0;
        tr = lsqcommit_agent_agent_xaction::type_id::create("lsqcommit_dispatch_tr");
        if (tr == null) begin
            `uvm_fatal("LSQ_COMMIT", "failed to create lsqcommit xaction")
        end
        clear_lsqcommit_xaction(tr);

        if (has_commit) begin
            pending_ptr = data.get_status(commit_uids[commit_uids.size() - 1]).get_rob_key();
            last_pending_ptr = pending_ptr;
            tr.io_ooo_to_mem_lsqio_pendingPtr_flag  = pending_ptr.flag;
            tr.io_ooo_to_mem_lsqio_pendingPtr_value = pending_ptr.value;
        end
    endfunction:build_lsqcommit_xaction

    function void mark_rob_commit_uid(input memblock_uid_t uid);
        status_transaction status;
        bit                fault_candidate;

        ensure_handles();
        status = data.get_status(uid);
        if (data.issue_blocked_by_global_flush()) begin
            `uvm_info("LSQ_COMMIT", $sformatf("skip ROB commit uid=%0d because redirect/flush is in progress", uid), UVM_LOW)
            return;
        end
        if (!uid_is_commit_candidate(uid)) begin
            `uvm_info("LSQ_COMMIT",
                      $sformatf("skip non-eligible ROB commit uid=%0d active=%0d wb=%0d pass=%0d fault=%0d exc=%0d load/sta/std_fault=%0d/%0d/%0d replay=%0d redirect=%0d flushed=%0d killed=%0d terminal_done=%0d",
                                uid,
                                status.active,
                                status.writeback,
                                status.pass,
                                status.fault,
                                status.exception_pending,
                                status.load_fault,
                                status.sta_fault,
                                status.std_fault,
                                status.replay_pending,
                                status.redirect_pending,
                                status.flushed,
                                status.issue_killed,
                                status.terminal_done),
                      UVM_LOW)
            return;
        end
        fault_candidate = uid_is_fault_terminal_candidate(uid);
        status.rob_commit       = 1'b1;
        status.last_event_cycle = $time;
        if (!status.active_lq_mapped && !status.active_sq_mapped) begin
            status.lsq_deq = 1'b1;
        end
        `uvm_info("LSQ_COMMIT",
                  $sformatf("rob commit uid=%0d rob=%0d/%0d lq_mapped=%0d sq_mapped=%0d fault_candidate=%0d",
                            uid,
                            status.robIdx_flag,
                            status.robIdx_value,
                            status.active_lq_mapped,
                            status.active_sq_mapped,
                            fault_candidate),
                  UVM_LOW)
        data.try_retire_committed_uid(uid);
        advance_commit_cursor_past_done();
    endfunction:mark_rob_commit_uid

    function void mark_rob_commit_batch(input memblock_uid_t uids[$]);
        foreach (uids[idx]) begin
            mark_rob_commit_uid(uids[idx]);
        end
    endfunction:mark_rob_commit_batch

    function memblock_lq_key_t lq_deq_start_key(input memblock_lq_key_t deq_ptr,
                                                input int unsigned count,
                                                input bit ptr_is_next);
        if (ptr_is_next) begin
            return lsq_ctrl_model::rewind_lq_key(deq_ptr, count);
        end
        return deq_ptr;
    endfunction:lq_deq_start_key

    function memblock_sq_key_t sq_deq_start_key(input memblock_sq_key_t deq_ptr,
                                                input int unsigned count,
                                                input bit ptr_is_next);
        if (ptr_is_next) begin
            return lsq_ctrl_model::rewind_sq_key(deq_ptr, count);
        end
        return deq_ptr;
    endfunction:sq_deq_start_key

    function void apply_dut_lq_deq(input int unsigned count,
                                   input memblock_lq_key_t deq_ptr,
                                   input bit ptr_is_next = 1'b1);
        memblock_lq_key_t start_key;
        memblock_uid_t    deq_uids[$];

        ensure_handles();
        if (count == 0) begin
            return;
        end
        start_key = lq_deq_start_key(deq_ptr, count, ptr_is_next);
        if (start_key != lsq_ctrl.lq_deq_ptr) begin
            report_deq_mismatch($sformatf("DUT lqDeq start flag=%0d value=%0d mismatches software LQ head flag=%0d value=%0d count=%0d",
                                          start_key.flag,
                                          start_key.value,
                                          lsq_ctrl.lq_deq_ptr.flag,
                                          lsq_ctrl.lq_deq_ptr.value,
                                          count));
            return;
        end
        for (int unsigned idx = 0; idx < count; idx++) begin
            memblock_lq_key_t key;
            memblock_uid_t    uid;

            key = lsq_ctrl_model::advance_lq_key(start_key, idx);
            if (data.lookup_active_uid_by_lq(key, uid)) begin
                deq_uids.push_back(uid);
                `uvm_info("LSQ_COMMIT",
                          $sformatf("dut lqDeq accept idx=%0d/%0d uid=%0d lq=%0d/%0d ptr_next=%0d",
                                    idx + 1,
                                    count,
                                    uid,
                                    key.flag,
                                    key.value,
                                    ptr_is_next),
                          UVM_LOW)
            end else begin
                report_deq_mismatch($sformatf("stale DUT lqDeq count=%0d key flag=%0d value=%0d has no active uid",
                                              count, key.flag, key.value));
                return;
            end
        end
        lsq_ctrl.release_lq(count);
        foreach (deq_uids[idx]) begin
            data.release_uid_lq_mapping(deq_uids[idx]);
            data.try_retire_committed_uid(deq_uids[idx]);
        end
    endfunction:apply_dut_lq_deq

    function void apply_dut_sq_deq(input int unsigned count,
                                   input memblock_sq_key_t deq_ptr,
                                   input bit ptr_is_next = 1'b1);
        memblock_sq_key_t start_key;
        memblock_uid_t    deq_uids[$];

        ensure_handles();
        if (count == 0) begin
            return;
        end
        start_key = sq_deq_start_key(deq_ptr, count, ptr_is_next);
        if (start_key != lsq_ctrl.sq_deq_ptr) begin
            report_deq_mismatch($sformatf("DUT sqDeq start flag=%0d value=%0d mismatches software SQ head flag=%0d value=%0d count=%0d",
                                          start_key.flag,
                                          start_key.value,
                                          lsq_ctrl.sq_deq_ptr.flag,
                                          lsq_ctrl.sq_deq_ptr.value,
                                          count));
            return;
        end
        for (int unsigned idx = 0; idx < count; idx++) begin
            memblock_sq_key_t key;
            memblock_uid_t    uid;

            key = lsq_ctrl_model::advance_sq_key(start_key, idx);
            if (data.lookup_active_uid_by_sq(key, uid)) begin
                deq_uids.push_back(uid);
                `uvm_info("LSQ_COMMIT",
                          $sformatf("dut sqDeq accept idx=%0d/%0d uid=%0d sq=%0d/%0d ptr_next=%0d",
                                    idx + 1,
                                    count,
                                    uid,
                                    key.flag,
                                    key.value,
                                    ptr_is_next),
                          UVM_LOW)
            end else begin
                report_deq_mismatch($sformatf("stale DUT sqDeq count=%0d key flag=%0d value=%0d has no active uid",
                                              count, key.flag, key.value));
                return;
            end
        end
        lsq_ctrl.release_sq(count);
        foreach (deq_uids[idx]) begin
            data.release_uid_sq_mapping(deq_uids[idx]);
            data.try_retire_committed_uid(deq_uids[idx]);
        end
    endfunction:apply_dut_sq_deq

    function void apply_raw_ctrl_deq(input int unsigned lq_count,
                                     input memblock_lq_key_t lq_ptr,
                                     input int unsigned sq_count,
                                     input memblock_sq_key_t sq_ptr,
                                     input bit ptr_is_next = 1'b1);
        apply_dut_lq_deq(lq_count, lq_ptr, ptr_is_next);
        apply_dut_sq_deq(sq_count, sq_ptr, ptr_is_next);
    endfunction:apply_raw_ctrl_deq

endclass:lsq_commit_handler

`endif
