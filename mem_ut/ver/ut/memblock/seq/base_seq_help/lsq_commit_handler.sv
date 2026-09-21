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

    static lsq_commit_handler m_inst;

    common_data_transaction data;
    lsq_ctrl_model          lsq_ctrl;

    memblock_uid_t     commit_cursor_uid;
    // 中文注释：modeled_rob_deq_ptr 是当前 ROB head 的完整 key；它只能从
    // status[commit_cursor_uid] 读取，不能由 commit batch tail 或 key+1 推导。
    memblock_rob_key_t modeled_rob_deq_ptr;
    bit                modeled_rob_deq_ptr_initialized;
    bit                modeled_head_valid;
    // 中文注释：最后一个已成功提交 batch 的 tail key 只作为 DUT 的保守
    // pendingPtr watermark。它不是新的 ROB head，也不参与 pending* 判定。
    memblock_rob_key_t committed_rob_watermark;
    bit                committed_rob_watermark_valid;
    bit                fault_head_waiting;
    memblock_uid_t     fault_head_uid;
    int unsigned       fault_head_dynamic_epoch;
    // fault terminal 后 modeled head 已可能移动，redirect 必须使用建 token 时的权威 ROB key。
    memblock_rob_key_t fault_head_rob_key;
    // 已有 older/equal redirect 覆盖 terminal fault 时，避免它完成后再发一次 fault redirect。
    bit                fault_head_redirect_covered;
    bit                latched_is_store_exception;

    `uvm_object_utils(lsq_commit_handler)

    function new(string name = "lsq_commit_handler");
        super.new(name);
        data              = common_data_transaction::get();
        lsq_ctrl          = null;
        commit_cursor_uid = 0;
        modeled_rob_deq_ptr = '{default:'0};
        modeled_rob_deq_ptr_initialized = 1'b0;
        modeled_head_valid = 1'b0;
        committed_rob_watermark = '{default:'0};
        committed_rob_watermark_valid = 1'b0;
        fault_head_waiting = 1'b0;
        fault_head_uid = 0;
        fault_head_dynamic_epoch = 0;
        fault_head_rob_key = '{default:'0};
        fault_head_redirect_covered = 1'b0;
        latched_is_store_exception = 1'b0;
    endfunction:new

    static function lsq_commit_handler get();
        if (m_inst == null) begin
            m_inst = lsq_commit_handler::type_id::create("lsq_commit_handler_singleton");
        end
        return m_inst;
    endfunction:get

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

    function void reset_lsqcommit_runtime_state();
        // 中文注释：只清 commit handler 私有游标；不触碰公共 status、active map 或
        // lsq_ctrl pointer。main table 每轮重建前由 sequence 调用一次。
        commit_cursor_uid = 0;
        modeled_rob_deq_ptr = '{default:'0};
        modeled_rob_deq_ptr_initialized = 1'b0;
        modeled_head_valid = 1'b0;
        committed_rob_watermark = '{default:'0};
        committed_rob_watermark_valid = 1'b0;
        fault_head_waiting = 1'b0;
        fault_head_uid = 0;
        fault_head_dynamic_epoch = 0;
        fault_head_rob_key = '{default:'0};
        fault_head_redirect_covered = 1'b0;
        latched_is_store_exception = 1'b0;
    endfunction:reset_lsqcommit_runtime_state

    // 只清 fault token 私有生命周期，不改 status、cursor、LQ/SQ 或 exception sideband latch。
    function void clear_fault_head_token();
        fault_head_waiting = 1'b0;
        fault_head_uid = 0;
        fault_head_dynamic_epoch = 0;
        fault_head_rob_key = '{default:'0};
        fault_head_redirect_covered = 1'b0;
    endfunction:clear_fault_head_token

    // 仅在 fault token terminal 等待期间扫描 recovery queue，保证未激活的真实 redirect
    // 先由 exception handler 按 oldest 规则仲裁，避免与 fault redirect 竞争 cancel record。
    function bit has_pending_monitor_redirect();
        ensure_handles();
        foreach (data.exception_event_q[idx]) begin
            if (data.feedback_event_is_redirect(data.exception_event_q[idx])) begin
                return 1'b1;
            end
        end
        return 1'b0;
    endfunction:has_pending_monitor_redirect

    // 将已经 terminal 的 real-dispatch fault 转交既有 redirect owner；调用者负责 token/cursor。
    function bit request_fault_head_redirect();
        memblock_redirect_payload_t redirect;

        ensure_handles();
        if (!memblock_sync_pkg::dispatch_real_smoke_active) begin
            `uvm_fatal("LSQ_COMMIT", "fault redirect requested outside real dispatch topology")
        end
        if (!seq_csr_common::is_initialized() || !seq_csr_common::get_redirect_seq_en()) begin
            `uvm_fatal("LSQ_COMMIT",
                       "real-dispatch fault recovery requires MEMBLOCK_REDIRECT_SEQ_EN=1")
        end
        if (data.redirect_deleted_owner_window_pending()) begin
            return 1'b0;
        end
        if (data.active_redirect.valid || data.has_pending_redirect_drive() ||
            data.issue_blocked_by_global_flush() || has_pending_monitor_redirect()) begin
            return 1'b0;
        end

        redirect = '{default:'0};
        redirect.valid = 1'b1;
        redirect.rob_key = fault_head_rob_key;
        redirect.flush_itself = 1'b1;
        redirect.level = 1'b1;
        redirect.is_vls_exception = 1'b0;
        data.request_redirect_flush(redirect);
        data.push_redirect_drive(redirect);
        `uvm_info("LSQ_COMMIT",
                  $sformatf("request scalar fault redirect uid=%0d rob=%0d/%0d level=%0d flush_itself=%0d",
                            fault_head_uid,
                            fault_head_rob_key.flag,
                            fault_head_rob_key.value,
                            redirect.level,
                            redirect.flush_itself),
                  UVM_LOW)
        return 1'b1;
    endfunction:request_fault_head_redirect

    // fault terminal 已确定后的统一收口。该函数不创建 redirect；调用者已经完成仲裁。
    function void finish_fault_head_terminal();
        if (data.fault_redirect_resource_matches(fault_head_uid,
                                                 fault_head_dynamic_epoch)) begin
            data.complete_fault_redirect_resource(fault_head_uid);
        end
        commit_cursor_uid = fault_head_uid + 1;
        clear_fault_head_token();
        modeled_head_valid = 1'b0;
        rebase_framework_head_from_commit_cursor();
    endfunction:finish_fault_head_terminal

    // 抽象职责：已有redirect覆盖fault head且其LQ/SQ owner均已释放后，清除旧
    // recovery gate并完成异常terminal。该路径只收口软件状态，不生成新的DUT
    // redirect/deq，也不把fault UID重新路由。
    function bit force_fault_head_terminal_after_redirect();
        status_transaction status;

        ensure_handles();
        if (!fault_head_waiting || data.active_redirect.valid ||
            data.issue_blocked_by_global_flush()) begin
            return 1'b0;
        end
        status = data.get_status(fault_head_uid);
        if (!status.active || status.dynamic_epoch != fault_head_dynamic_epoch ||
            !status.rob_commit || status.active_lq_mapped || status.active_sq_mapped) begin
            return 1'b0;
        end
        if (data.fault_redirect_resource_blocks_retire(fault_head_uid)) begin
            return 1'b0;
        end
        data.quiesce_fault_uid_pending_work(fault_head_uid, 1'b0);
        status.replay_pending  = 1'b0;
        status.redirect_pending = 1'b0;
        status.flushed          = 1'b0;
        status.issue_killed     = 1'b0;
        data.consume_fault_retire(fault_head_uid);
        `uvm_info("LSQ_COMMIT",
                  $sformatf("force fault terminal uid=%0d after covered redirect",
                            fault_head_uid),
                  UVM_LOW)
        return 1'b1;
    endfunction:force_fault_head_terminal_after_redirect

    function void ensure_modeled_rob_deq_ptr_initialized();
        ensure_handles();
        if (modeled_rob_deq_ptr_initialized) begin
            return;
        end
        if (!data.main_table_ready) begin
            `uvm_fatal("LSQ_COMMIT", "cannot initialize modeled ROB head before main table is ready")
        end
        if (fault_head_waiting) begin
            `uvm_fatal("LSQ_COMMIT", "cannot initialize modeled ROB head while fault token is waiting")
        end
        rebase_framework_head_from_commit_cursor();
        modeled_rob_deq_ptr_initialized = 1'b1;
    endfunction:ensure_modeled_rob_deq_ptr_initialized

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
        if (is_control_op_class(data.get_main_transaction(uid).op_class)) begin
            return 1'b0;
        end
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
        if (is_control_op_class(data.get_main_transaction(uid).op_class)) begin
            return 1'b0;
        end
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
               uid_is_fault_terminal_candidate(uid) ||
               uid_is_control_commit_candidate(uid);
    endfunction:uid_is_commit_candidate

    // 抽象职责：判断当前 active ROB head 是否已被 control service 完成其专用动作。
    // 它不读取普通 writeback/pass/target 状态，也不推进 cursor。
    function bit uid_is_control_commit_candidate(input memblock_uid_t uid);
        status_transaction status;

        ensure_handles();
        if (!is_control_op_class(data.get_main_transaction(uid).op_class)) begin
            return 1'b0;
        end
        status = data.get_status(uid);
        return status.active && status.enq && status.issue_ready &&
               status.control_state == MEMBLOCK_CONTROL_STATE_CONTROL_COMMIT_READY &&
               !status.rob_commit && !status.redirect_pending && !status.flushed &&
               !status.issue_killed && !status.terminal_done;
    endfunction:uid_is_control_commit_candidate

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

    function void rebase_framework_head_from_commit_cursor();
        status_transaction status;

        ensure_handles();
        if (fault_head_waiting) begin
            `uvm_fatal("LSQ_COMMIT", "head rebase is not allowed while fault token is waiting")
        end
        advance_commit_cursor_past_done();
        if (commit_cursor_uid > data.main_trans_num) begin
            `uvm_fatal("LSQ_COMMIT", "commit cursor moved beyond main table")
        end
        if (commit_cursor_uid == data.main_trans_num) begin
            modeled_head_valid = 1'b0;
            modeled_rob_deq_ptr = '{default:'0};
            return;
        end
        status = data.get_status(commit_cursor_uid);
        modeled_rob_deq_ptr = status.get_rob_key();
        modeled_head_valid = 1'b1;
    endfunction:rebase_framework_head_from_commit_cursor

    function bit resolve_sideband_head_uid(output memblock_uid_t uid);
        ensure_modeled_rob_deq_ptr_initialized();
        if (!fault_head_waiting) begin
            rebase_framework_head_from_commit_cursor();
        end
        return modeled_head_matches_active_uid(uid);
    endfunction:resolve_sideband_head_uid

    function bit modeled_head_matches_active_uid(output memblock_uid_t uid);
        status_transaction status;
        memblock_uid_t resolved_uid;

        uid = 0;
        if (!modeled_head_valid || commit_cursor_uid >= data.main_trans_num) begin
            return 1'b0;
        end
        resolved_uid = 0;
        if (!data.lookup_active_uid_by_rob(modeled_rob_deq_ptr, resolved_uid) ||
            resolved_uid != commit_cursor_uid) begin
            return 1'b0;
        end
        status = data.get_status(resolved_uid);
        if (!status.active || status.terminal_done || status.flushed || status.issue_killed ||
            status.get_rob_key() != modeled_rob_deq_ptr) begin
            return 1'b0;
        end
        uid = resolved_uid;
        return 1'b1;
    endfunction:modeled_head_matches_active_uid

    function bit committed_watermark_publishable();
        ensure_handles();
        return data.main_table_ready &&
               committed_rob_watermark_valid &&
               modeled_rob_deq_ptr_initialized &&
               !modeled_head_valid &&
               !fault_head_waiting &&
               commit_cursor_uid == data.main_trans_num;
    endfunction:committed_watermark_publishable

    function void select_rob_commit_batch(output memblock_uid_t uids[$]);
        memblock_uid_t uid;

        ensure_handles();
        uids.delete();
        ensure_modeled_rob_deq_ptr_initialized();
        if (data.issue_blocked_by_global_flush() || fault_head_waiting) begin
            return;
        end
        rebase_framework_head_from_commit_cursor();
        uid = commit_cursor_uid;
        while (uid < data.main_trans_num && uids.size() < MEMBLOCK_COMMIT_WIDTH) begin
            if (data.get_status(uid).terminal_done) begin
                `uvm_fatal("LSQ_COMMIT", "normal commit selector saw terminal uid before head rebase")
            end
            if (uid_is_normal_commit_candidate(uid)) begin
                uids.push_back(uid);
                uid++;
                continue;
            end
            break;
        end
    endfunction:select_rob_commit_batch

    function bit select_fault_head_candidate(output memblock_uid_t uid);
        status_transaction status;
        memblock_uid_t resolved_uid;
        memblock_rob_key_t status_rob_key;
        bit map_hit;
        uid = 0;
        ensure_handles();
        ensure_modeled_rob_deq_ptr_initialized();
        if (data.issue_blocked_by_global_flush() || fault_head_waiting) begin
            if (commit_cursor_uid < data.main_trans_num) begin
                status = data.get_status(commit_cursor_uid);
                if (status.fault || status.exception_pending || status.load_fault ||
                    status.sta_fault || status.std_fault) begin
                    `uvm_info("LSQ_COMMIT_DIAG",
                              $sformatf("fault selector gated cursor=%0d fault_waiting=%0d active_redirect=%0d flush_in_progress=%0d freeze_ack=%0d pending_drive=%0d sync_flush=%0d owner_window=%0d status active=%0d rob_commit=%0d replay=%0d redirect=%0d flushed=%0d killed=%0d epoch=%0d",
                                        commit_cursor_uid, fault_head_waiting,
                                        data.active_redirect.valid, data.flush_in_progress,
                                        data.issue_freeze_ack, data.has_pending_redirect_drive(),
                                        memblock_sync_pkg::dispatch_flush_in_progress,
                                        data.redirect_deleted_owner_window_pending(),
                                        status.active, status.rob_commit,
                                        status.replay_pending, status.redirect_pending,
                                        status.flushed, status.issue_killed,
                                        status.dynamic_epoch),
                              UVM_LOW)
                end
            end
            return 1'b0;
        end
        rebase_framework_head_from_commit_cursor();
        if (!modeled_head_valid || commit_cursor_uid >= data.main_trans_num) begin
            return 1'b0;
        end
        uid = commit_cursor_uid;
        status = data.get_status(uid);
        status_rob_key = status.get_rob_key();
        if (!uid_is_fault_terminal_candidate(uid)) begin
            if (status.fault || status.exception_pending || status.load_fault ||
                status.sta_fault || status.std_fault) begin
                `uvm_info("LSQ_COMMIT_DIAG",
                          $sformatf("fault selector rejected uid=%0d active=%0d enq=%0d issue_ready=%0d wb=%0d fault/exc/load/sta/std=%0d/%0d/%0d/%0d/%0d rob_commit=%0d replay=%0d redirect=%0d flushed=%0d killed=%0d terminal=%0d epoch=%0d cursor=%0d modeled_valid=%0d modeled_rob=%0d/%0d active_redirect=%0d flush=%0d owner_window=%0d",
                                    uid, status.active, status.enq, status.issue_ready,
                                    status.writeback, status.fault, status.exception_pending,
                                    status.load_fault, status.sta_fault, status.std_fault,
                                    status.rob_commit, status.replay_pending,
                                    status.redirect_pending, status.flushed, status.issue_killed,
                                    status.terminal_done, status.dynamic_epoch,
                                    commit_cursor_uid, modeled_head_valid,
                                    modeled_rob_deq_ptr.flag, modeled_rob_deq_ptr.value,
                                    data.active_redirect.valid, data.flush_in_progress,
                                    data.redirect_deleted_owner_window_pending()),
                          UVM_LOW)
            end
            return 1'b0;
        end
        map_hit = data.lookup_active_uid_by_rob(modeled_rob_deq_ptr, resolved_uid);
        if (!resolve_sideband_head_uid(uid) || uid != commit_cursor_uid) begin
            `uvm_info("LSQ_COMMIT_DIAG",
                      $sformatf("fault selector head mismatch uid=%0d resolved=%0d map_hit=%0d map_uid=%0d cursor=%0d modeled=%0d/%0d status_rob=%0d/%0d active=%0d terminal=%0d flushed=%0d killed=%0d active_redirect=%0d flush=%0d owner_window=%0d",
                                uid, uid, map_hit, resolved_uid, commit_cursor_uid,
                                modeled_rob_deq_ptr.flag, modeled_rob_deq_ptr.value,
                                status_rob_key.flag, status_rob_key.value,
                                status.active, status.terminal_done, status.flushed,
                                status.issue_killed, data.active_redirect.valid,
                                data.flush_in_progress,
                                data.redirect_deleted_owner_window_pending()),
                      UVM_LOW)
            return 1'b0;
        end
        return data.get_status(uid).get_rob_key() == modeled_rob_deq_ptr;
    endfunction:select_fault_head_candidate

    // 抽象职责：只选择正位于 modeled ROB head 的 control UID；普通 normal/fault
    // selector 都不会把该 UID 当作访存 commit 候选。
    function bit select_control_head_candidate(output memblock_uid_t uid);
        memblock_uid_t resolved_uid;

        uid = 0;
        ensure_handles();
        ensure_modeled_rob_deq_ptr_initialized();
        if (data.issue_blocked_by_global_flush() || fault_head_waiting) begin
            return 1'b0;
        end
        rebase_framework_head_from_commit_cursor();
        if (!modeled_head_valid || commit_cursor_uid >= data.main_trans_num) begin
            return 1'b0;
        end
        uid = commit_cursor_uid;
        if (!uid_is_control_commit_candidate(uid)) begin
            return 1'b0;
        end
        if (!resolve_sideband_head_uid(resolved_uid) || resolved_uid != uid ||
            data.get_status(uid).get_rob_key() != modeled_rob_deq_ptr) begin
            return 1'b0;
        end
        return 1'b1;
    endfunction:select_control_head_candidate

    // 根据 fault UID 的权威主表操作分类生成 ROB exception commit type 的 store bit。
    // 该 helper 不修改 handler、status 或 LSQ 状态。
    function bit fault_uid_is_store_exception(input memblock_uid_t uid);
        main_control_transaction main_tr;
        memblock_op_behavior_t   behavior;

        ensure_handles();
        if (!data.main_table_ready || uid >= data.main_trans_num) begin
            `uvm_fatal("LSQ_COMMIT",
                       $sformatf("cannot classify fault store bit for uid=%0d ready=%0d main_trans_num=%0d",
                                 uid, data.main_table_ready, data.main_trans_num))
        end
        main_tr = data.get_main_transaction(uid);
        if (main_tr == null) begin
            `uvm_fatal("LSQ_COMMIT", $sformatf("fault uid=%0d has null main transaction", uid))
        end
        if (is_control_op_class(main_tr.op_class)) begin
            return 1'b0;
        end
        behavior = lsq_ctrl_model::derive_op_behavior(main_tr);
        return memblock_op_behavior_util::is_scalar_rob_store_commit(behavior);
    endfunction:fault_uid_is_store_exception

    function void clear_lsqcommit_xaction(input lsqcommit_agent_agent_xaction tr);
        if (tr == null) begin
            `uvm_fatal("LSQ_COMMIT", "clear_lsqcommit_xaction got null transaction")
        end
        ensure_modeled_rob_deq_ptr_initialized();
        // 中文注释：modeled head 有效时无条件发布其完整 key；active-map 命中只由
        // build_lsqcommit_xaction() 用于 pendingst/pendingMMIOld，不能反向决定 pendingPtr。
        // modeled head 无效时才允许发布 final committed watermark。
        if (modeled_head_valid) begin
            tr.io_ooo_to_mem_lsqio_pendingPtr_flag  = modeled_rob_deq_ptr.flag;
            tr.io_ooo_to_mem_lsqio_pendingPtr_value = modeled_rob_deq_ptr.value;
        end else if (committed_watermark_publishable()) begin
            tr.io_ooo_to_mem_lsqio_pendingPtr_flag  = committed_rob_watermark.flag;
            tr.io_ooo_to_mem_lsqio_pendingPtr_value = committed_rob_watermark.value;
        end else begin
            tr.io_ooo_to_mem_lsqio_pendingPtr_flag  = 1'b0;
            tr.io_ooo_to_mem_lsqio_pendingPtr_value = '0;
        end
        tr.io_ooo_to_mem_lsqio_pendingst        = 1'b0;
        tr.io_ooo_to_mem_lsqio_pendingMMIOld    = 1'b0;
        tr.io_ooo_to_mem_lsqio_scommit          = '0;
        tr.io_ooo_to_mem_flushSb                = 1'b0;
        tr.io_ooo_to_mem_isStoreException       = latched_is_store_exception;
    endfunction:clear_lsqcommit_xaction

    function void build_lsqcommit_xaction(output lsqcommit_agent_agent_xaction tr,
                                          output memblock_uid_t commit_uids[$],
                                          output bit has_commit,
                                          output bit has_fault_head,
                                          output memblock_uid_t fault_uid);
        memblock_uid_t head_uid;
        bit has_head;

        ensure_handles();
        ensure_modeled_rob_deq_ptr_initialized();
        sync_modeled_head_after_fault_terminal();
        has_head = resolve_sideband_head_uid(head_uid);
        select_rob_commit_batch(commit_uids);
        has_commit = commit_uids.size() != 0;
        has_fault_head = 1'b0;
        fault_uid = 0;
        if (!has_commit) begin
            has_fault_head = select_fault_head_candidate(fault_uid);
        end
        tr = lsqcommit_agent_agent_xaction::type_id::create("lsqcommit_dispatch_tr");
        if (tr == null) begin
            `uvm_fatal("LSQ_COMMIT", "failed to create lsqcommit xaction")
        end
        clear_lsqcommit_xaction(tr);
        if (has_fault_head) begin
            tr.io_ooo_to_mem_isStoreException =
                fault_uid_is_store_exception(fault_uid);
        end
        // Fault head 的 pendingPtr 仍需保持，但 fault token 不是 normal commit，
        // 不得把该指令解释成 pending store/MMIO load。
        if (has_head && !has_fault_head && !fault_head_waiting &&
            !is_control_op_class(data.get_main_transaction(head_uid).op_class)) begin
            memblock_op_behavior_t head_behavior;

            head_behavior = lsq_ctrl_model::derive_op_behavior(data.get_main_transaction(head_uid));
            tr.io_ooo_to_mem_lsqio_pendingst =
                memblock_op_behavior_util::is_scalar_rob_store_commit(head_behavior);
            tr.io_ooo_to_mem_lsqio_pendingMMIOld =
                head_behavior.commit_is_load && data.uid_is_mmio_load(head_uid);
        end
        foreach (commit_uids[idx]) begin
            memblock_op_behavior_t behavior;

            behavior = lsq_ctrl_model::derive_op_behavior(data.get_main_transaction(commit_uids[idx]));
            if (memblock_op_behavior_util::is_scalar_rob_store_commit(behavior)) begin
                tr.io_ooo_to_mem_lsqio_scommit++;
            end
        end
    endfunction:build_lsqcommit_xaction

    function bit mark_rob_commit_uid(input memblock_uid_t uid);
        status_transaction status;

        ensure_handles();
        status = data.get_status(uid);
        if (data.issue_blocked_by_global_flush()) begin
            `uvm_info("LSQ_COMMIT", $sformatf("skip ROB commit uid=%0d because redirect/flush is in progress", uid), UVM_LOW)
            return 1'b0;
        end
        if (!uid_is_normal_commit_candidate(uid)) begin
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
            return 1'b0;
        end
        status.rob_commit       = 1'b1;
        status.last_event_cycle = $time;
        if (!status.active_lq_mapped && !status.active_sq_mapped) begin
            status.lsq_deq = 1'b1;
        end
        `uvm_info("LSQ_COMMIT",
                  $sformatf("normal rob commit uid=%0d rob=%0d/%0d lq_mapped=%0d sq_mapped=%0d",
                            uid,
                            status.robIdx_flag,
                            status.robIdx_value,
                            status.active_lq_mapped,
                            status.active_sq_mapped),
                  UVM_LOW)
        data.try_retire_committed_uid(uid);
        return 1'b1;
    endfunction:mark_rob_commit_uid

    function void mark_rob_commit_batch(input memblock_uid_t uids[$]);
        memblock_rob_key_t batch_tail_rob_key;
        memblock_rob_key_t previous_rob_key;

        if (uids.size() == 0) begin
            return;
        end
        ensure_modeled_rob_deq_ptr_initialized();
        if (fault_head_waiting || uids[0] != commit_cursor_uid ||
            data.get_status(uids[0]).get_rob_key() != modeled_rob_deq_ptr) begin
            `uvm_fatal("LSQ_COMMIT", "normal commit batch does not start at the modeled ROB head")
        end
        foreach (uids[idx]) begin
            memblock_rob_key_t current_rob_key;

            current_rob_key = data.get_status(uids[idx]).get_rob_key();
            rob_order_util::check_rob_key(current_rob_key, "mark_rob_commit_batch");
            if (uids[idx] != commit_cursor_uid + idx ||
                !uid_is_normal_commit_candidate(uids[idx]) ||
                uid_is_fault_terminal_candidate(uids[idx])) begin
                `uvm_fatal("LSQ_COMMIT",
                           $sformatf("normal commit batch preflight failed idx=%0d uid=%0d cursor=%0d",
                                     idx, uids[idx], commit_cursor_uid))
            end
            if (idx != 0 && !rob_order_util::rob_is_after(current_rob_key, previous_rob_key)) begin
                `uvm_fatal("LSQ_COMMIT",
                           $sformatf("normal commit batch ROB keys are not monotonic idx=%0d prev=%0d/%0d current=%0d/%0d",
                                     idx,
                                     previous_rob_key.flag,
                                     previous_rob_key.value,
                                     current_rob_key.flag,
                                     current_rob_key.value))
            end
            previous_rob_key = current_rob_key;
        end
        foreach (uids[idx]) begin
            if (!mark_rob_commit_uid(uids[idx])) begin
                `uvm_fatal("LSQ_COMMIT", $sformatf("normal commit uid=%0d was not applied", uids[idx]))
            end
        end
        // 中文注释：保存已知的 batch tail，而不是算术推导下一个 ROB key。
        // 若后续仍有 active UID，rebase 会以权威 status key 覆盖它；若本批
        // 是最后一批，则该 key 作为 pendingPtr watermark 继续发布。
        batch_tail_rob_key = data.get_status(uids[uids.size() - 1]).get_rob_key();
        committed_rob_watermark = batch_tail_rob_key;
        committed_rob_watermark_valid = 1'b1;
        commit_cursor_uid = uids[uids.size() - 1] + 1;
        rebase_framework_head_from_commit_cursor();
    endfunction:mark_rob_commit_batch

    // 抽象职责：记录一个 control ROB head 的提交并推进公共 commit cursor。它不生成
    // pending store/MMIO 或 scommit 语义，terminal 条件完全由 control state 决定。
    function bit mark_control_rob_commit_uid(input memblock_uid_t uid);
        status_transaction status;
        memblock_uid_t resolved_uid;
        memblock_rob_key_t control_rob_key;

        ensure_handles();
        ensure_modeled_rob_deq_ptr_initialized();
        if (data.issue_blocked_by_global_flush()) begin
            return 1'b0;
        end
        if (fault_head_waiting || uid != commit_cursor_uid ||
            !uid_is_control_commit_candidate(uid)) begin
            `uvm_fatal("CONTROL_COMMIT",
                       $sformatf("invalid control commit uid=%0d cursor=%0d fault_waiting=%0d",
                                 uid, commit_cursor_uid, fault_head_waiting))
        end
        if (!resolve_sideband_head_uid(resolved_uid) || resolved_uid != uid ||
            data.get_status(uid).get_rob_key() != modeled_rob_deq_ptr) begin
            `uvm_fatal("CONTROL_COMMIT",
                       $sformatf("control uid=%0d does not match modeled ROB head", uid))
        end
        status = data.get_status(uid);
        control_rob_key = status.get_rob_key();
        status.rob_commit = 1'b1;
        status.lsq_deq = 1'b1;
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        data.try_retire_control_committed_uid(uid);
        if (!status.terminal_done || status.active) begin
            `uvm_fatal("CONTROL_COMMIT",
                       $sformatf("control uid=%0d failed to reach terminal_done after commit", uid))
        end
        committed_rob_watermark = control_rob_key;
        committed_rob_watermark_valid = 1'b1;
        commit_cursor_uid = uid + 1;
        rebase_framework_head_from_commit_cursor();
        return 1'b1;
    endfunction:mark_control_rob_commit_uid

    function bit mark_fault_rob_commit_uid(input memblock_uid_t uid);
        status_transaction status;
        memblock_uid_t resolved_uid;
        bit fault_is_store_exception;

        ensure_handles();
        ensure_modeled_rob_deq_ptr_initialized();
        if (data.issue_blocked_by_global_flush()) begin
            return 1'b0;
        end
        if (fault_head_waiting || uid != commit_cursor_uid ||
            !uid_is_fault_terminal_candidate(uid)) begin
            `uvm_fatal("LSQ_COMMIT",
                       $sformatf("invalid fault head token uid=%0d cursor=%0d waiting=%0d",
                                 uid, commit_cursor_uid, fault_head_waiting))
        end
        if (!resolve_sideband_head_uid(resolved_uid) || resolved_uid != uid ||
            data.get_status(uid).get_rob_key() != modeled_rob_deq_ptr) begin
            `uvm_fatal("LSQ_COMMIT", "fault token uid does not match modeled ROB head")
        end
        fault_is_store_exception = fault_uid_is_store_exception(uid);
        status = data.get_status(uid);
        status.rob_commit = 1'b1;
        status.last_event_cycle = $time;
        // fault commit 固化 RM 对比时机，但不是 DUT STD IQ 的 redirect flush
        // 边界；保留尚未 fire 的 STD，直到 fault redirect 真正生效。
        data.quiesce_fault_uid_pending_work(uid, 1'b0);
        fault_head_waiting = 1'b1;
        fault_head_uid = uid;
        fault_head_dynamic_epoch = status.dynamic_epoch;
        fault_head_rob_key = status.get_rob_key();
        fault_head_redirect_covered = 1'b0;
        data.try_retire_committed_uid(uid);
        sync_modeled_head_after_fault_terminal();
        latched_is_store_exception = fault_is_store_exception;
        return 1'b1;
    endfunction:mark_fault_rob_commit_uid

    function bit sync_modeled_head_after_fault_terminal();
        status_transaction status;
        bit                terminal_ready;

        ensure_modeled_rob_deq_ptr_initialized();
        if (!fault_head_waiting) begin
            if (commit_cursor_uid < data.main_trans_num &&
                data.get_status(commit_cursor_uid).terminal_done) begin
                rebase_framework_head_from_commit_cursor();
            end
            return 1'b0;
        end
        status = data.get_status(fault_head_uid);
        if (status.dynamic_epoch != fault_head_dynamic_epoch ||
            status.flushed || status.issue_killed ||
            !status.rob_commit) begin
            // Redirect 已经杀掉旧 fault 动态实例；保持 cursor 在同一 uid 等待 reissue。
            clear_fault_head_token();
            modeled_head_valid = 1'b0;
            rebase_framework_head_from_commit_cursor();
            return 1'b0;
        end
        // Fault entry的LQ/SQ deq可能与active redirect重叠。deq handler会先释放
        // mapping，但按统一规则暂不retire；redirect解除后由fault token补做终态收口。
        if (status.active && !status.active_lq_mapped &&
            !status.active_sq_mapped && !data.active_redirect.valid &&
            !data.issue_blocked_by_global_flush()) begin
            data.try_retire_committed_uid(fault_head_uid);
            status = data.get_status(fault_head_uid);
        end
        terminal_ready = status.terminal_done && status.lsq_deq &&
                         !status.active && !status.success && status.fault &&
                         !status.active_lq_mapped && !status.active_sq_mapped;

        // 当前fault已经由自己的redirect或更老redirect覆盖，且异常资源和
        // terminal状态均已收口时，不能再被其它年轻redirect token饿死。先
        // 完成当前fault token并推进commit cursor，后续redirect继续按原规则仲裁。
        if (terminal_ready && fault_head_redirect_covered) begin
            finish_fault_head_terminal();
            return 1'b1;
        end

        if (!memblock_sync_pkg::dispatch_real_smoke_active) begin
            // software-only fault sequence 没有 redirect responder/monitor，保持旧 terminal rebase。
            if (terminal_ready) begin
                finish_fault_head_terminal();
                return 1'b1;
            end
            return 1'b0;
        end
        if (has_pending_monitor_redirect()) begin
            // 让 recovery handler 先仲裁真实 monitor redirect，避免并发分配 cancel record。
            return 1'b0;
        end
        if (data.active_redirect.valid) begin
            if (rob_order_util::rob_need_flush(fault_head_rob_key, data.active_redirect)) begin
                fault_head_redirect_covered = 1'b1;
            end
            return 1'b0;
        end
        if (data.has_pending_redirect_drive() || data.issue_blocked_by_global_flush()) begin
            return 1'b0;
        end
        if (fault_head_redirect_covered) begin
            // 已完成的覆盖 redirect 已负责年轻 UID 的 cancel/reissue，fault 本身仍只做 terminal skip。
            void'(force_fault_head_terminal_after_redirect());
            status = data.get_status(fault_head_uid);
            if (status.active && !status.active_lq_mapped &&
                !status.active_sq_mapped && !data.active_redirect.valid &&
                !data.issue_blocked_by_global_flush()) begin
                data.try_retire_committed_uid(fault_head_uid);
                status = data.get_status(fault_head_uid);
            end
            terminal_ready = status.terminal_done && status.lsq_deq &&
                             !status.active && !status.success && status.fault &&
                             !status.active_lq_mapped && !status.active_sq_mapped;
            if (terminal_ready) begin
                finish_fault_head_terminal();
                return 1'b1;
            end
            return 1'b0;
        end
        if (!request_fault_head_redirect()) begin
            return 1'b0;
        end
        // Fault UID可能仍持有LQ/SQ owner。保持token到redirect扫描把该UID按
        // exception terminal回收；禁止在此处提前推进cursor或把fault UID重发。
        fault_head_redirect_covered = 1'b1;
        return 1'b0;
    endfunction:sync_modeled_head_after_fault_terminal

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

    function bit preflight_dut_lq_deq(input int unsigned count,
                                      input memblock_lq_key_t deq_ptr,
                                      input bit ptr_is_next,
                                      output memblock_lq_deq_apply_item_t items[$]);
        memblock_lq_key_t start_key;

        ensure_handles();
        items.delete();
        if (count == 0) begin
            return 1'b1;
        end
        if (count > (MEMBLOCK_LQ_SIZE - lsq_ctrl.lq_free_count)) begin
            report_deq_mismatch($sformatf("DUT lqDeq count=%0d exceeds software allocated count=%0d",
                                          count, MEMBLOCK_LQ_SIZE - lsq_ctrl.lq_free_count));
            return 1'b0;
        end
        start_key = lq_deq_start_key(deq_ptr, count, ptr_is_next);
        if (start_key != lsq_ctrl.lq_deq_ptr) begin
            report_deq_mismatch($sformatf("DUT lqDeq start flag=%0d value=%0d mismatches software LQ head flag=%0d value=%0d count=%0d",
                                          start_key.flag,
                                          start_key.value,
                                          lsq_ctrl.lq_deq_ptr.flag,
                                          lsq_ctrl.lq_deq_ptr.value,
                                          count));
            return 1'b0;
        end
        for (int unsigned idx = 0; idx < count; idx++) begin
            memblock_lq_key_t key;
            memblock_uid_t    uid;
            status_transaction status;
            memblock_redirect_deleted_owner_t deleted_owner;
            memblock_lq_deq_apply_item_t item;
            bit active_hit;
            bit deleted_hit;
            bit uid_seen;

            key = lsq_ctrl_model::advance_lq_key(start_key, idx);
            active_hit = data.lookup_active_uid_by_lq(key, uid);
            deleted_hit = data.lookup_redirect_deleted_lq_owner(key, deleted_owner);
            if (active_hit && deleted_hit) begin
                report_deq_mismatch($sformatf("DUT lqDeq key=%0d/%0d has both live and redirect-deleted owners",
                                              key.flag, key.value));
                items.delete();
                return 1'b0;
            end
            item = '{
                key: '{default:'0},
                owner_kind: MEMBLOCK_DEQ_OWNER_LIVE,
                uid: '0,
                old_dynamic_epoch: 0,
                redirect_epoch: 0,
                cancel_record_id: 0
            };
            item.key = key;
            if (active_hit) begin
                status = data.get_status(uid);
                uid_seen = 1'b0;
                foreach (items[seen_idx]) begin
                    if (items[seen_idx].owner_kind == MEMBLOCK_DEQ_OWNER_LIVE &&
                        items[seen_idx].uid == uid) begin
                        uid_seen = 1'b1;
                    end
                end
                if (uid_seen || !status.active || !status.active_lq_mapped ||
                    status.lqIdx_flag != key.flag || status.lqIdx_value != key.value) begin
                    report_deq_mismatch($sformatf("DUT lqDeq owner mismatch uid=%0d key=%0d/%0d active=%0d mapped=%0d status_key=%0d/%0d duplicate=%0d",
                                                  uid, key.flag, key.value,
                                                  status.active, status.active_lq_mapped,
                                                  status.lqIdx_flag, status.lqIdx_value,
                                                  uid_seen));
                    items.delete();
                    return 1'b0;
                end
                item.owner_kind = MEMBLOCK_DEQ_OWNER_LIVE;
                item.uid = uid;
                item.old_dynamic_epoch = status.dynamic_epoch;
                items.push_back(item);
                `uvm_info("LSQ_COMMIT",
                          $sformatf("dut lqDeq preflight live idx=%0d/%0d uid=%0d lq=%0d/%0d ptr_next=%0d",
                                    idx + 1,
                                    count,
                                    uid,
                                    key.flag,
                                    key.value,
                                    ptr_is_next),
                          UVM_LOW)
            end else if (deleted_hit) begin
                item.owner_kind = MEMBLOCK_DEQ_OWNER_REDIRECT_DELETED;
                item.uid = deleted_owner.uid;
                item.old_dynamic_epoch = deleted_owner.old_dynamic_epoch;
                item.redirect_epoch = deleted_owner.redirect_epoch;
                item.cancel_record_id = deleted_owner.cancel_record_id;
                items.push_back(item);
                `uvm_info("LSQ_COMMIT",
                          $sformatf("dut lqDeq preflight redirect-deleted idx=%0d/%0d uid=%0d epoch=%0d lq=%0d/%0d",
                                    idx + 1, count, deleted_owner.uid,
                                    deleted_owner.old_dynamic_epoch,
                                    key.flag, key.value),
                          UVM_LOW)
            end else begin
                report_deq_mismatch($sformatf("stale DUT lqDeq count=%0d key flag=%0d value=%0d has no active uid",
                                              count, key.flag, key.value));
                items.delete();
                return 1'b0;
            end
        end
        return 1'b1;
    endfunction:preflight_dut_lq_deq

    function void commit_dut_lq_deq(input int unsigned count,
                                    input memblock_lq_deq_apply_item_t items[$]);
        if (count == 0) begin
            return;
        end
        if (items.size() != count) begin
            `uvm_fatal("LSQ_COMMIT", "LQ deq commit list size does not match count")
        end
        lsq_ctrl.release_lq(count);
        foreach (items[idx]) begin
            if (items[idx].owner_kind == MEMBLOCK_DEQ_OWNER_LIVE) begin
                data.release_uid_lq_mapping(items[idx].uid);
            end
        end
    endfunction:commit_dut_lq_deq

    function bit preflight_dut_sq_deq_from_start(input int unsigned count,
                                                 input memblock_sq_key_t start_key,
                                                 input bit ptr_is_next,
                                                 output memblock_sq_deq_apply_item_t items[$]);
        ensure_handles();
        items.delete();
        if (count == 0) begin
            return 1'b1;
        end
        if (count > MEMBLOCK_DUT_ENSBUFFER_WIDTH) begin
            `uvm_fatal("LSQ_COMMIT",
                       $sformatf("sqDeq count=%0d exceeds EnsbufferWidth=%0d",
                                 count, MEMBLOCK_DUT_ENSBUFFER_WIDTH))
        end
        if (count > (MEMBLOCK_SQ_SIZE - lsq_ctrl.sq_free_count)) begin
            report_deq_mismatch($sformatf("DUT sqDeq count=%0d exceeds software allocated count=%0d",
                                          count, MEMBLOCK_SQ_SIZE - lsq_ctrl.sq_free_count));
            return 1'b0;
        end
        if (start_key != lsq_ctrl.sq_deq_ptr) begin
            report_deq_mismatch($sformatf("DUT sqDeq start flag=%0d value=%0d mismatches software SQ head flag=%0d value=%0d count=%0d",
                                          start_key.flag,
                                          start_key.value,
                                          lsq_ctrl.sq_deq_ptr.flag,
                                          lsq_ctrl.sq_deq_ptr.value,
                                          count));
            return 1'b0;
        end
        for (int unsigned idx = 0; idx < count; idx++) begin
            memblock_sq_key_t key;
            memblock_uid_t    uid;
            status_transaction status;
            memblock_redirect_deleted_owner_t deleted_owner;
            memblock_sq_deq_apply_item_t item;
            bit active_hit;
            bit deleted_hit;
            bit uid_seen;

            key = lsq_ctrl_model::advance_sq_key(start_key, idx);
            active_hit = data.lookup_active_uid_by_sq(key, uid);
            deleted_hit = data.lookup_redirect_deleted_sq_owner(key, deleted_owner);
            if (active_hit && deleted_hit) begin
                report_deq_mismatch($sformatf("DUT sqDeq key=%0d/%0d has both live and redirect-deleted owners",
                                              key.flag, key.value));
                items.delete();
                return 1'b0;
            end
            item = '{
                key: '{default:'0},
                owner_kind: MEMBLOCK_DEQ_OWNER_LIVE,
                uid: '0,
                old_dynamic_epoch: 0,
                redirect_epoch: 0,
                cancel_record_id: 0
            };
            item.key = key;
            if (active_hit) begin
                status = data.get_status(uid);
                uid_seen = 1'b0;
                foreach (items[seen_idx]) begin
                    if (items[seen_idx].owner_kind == MEMBLOCK_DEQ_OWNER_LIVE &&
                        items[seen_idx].uid == uid) begin
                        uid_seen = 1'b1;
                    end
                end
                if (uid_seen || !status.active || !status.active_sq_mapped ||
                    status.sqIdx_flag != key.flag || status.sqIdx_value != key.value) begin
                    report_deq_mismatch($sformatf("DUT sqDeq owner mismatch uid=%0d key=%0d/%0d active=%0d mapped=%0d status_key=%0d/%0d duplicate=%0d",
                                                  uid, key.flag, key.value,
                                                  status.active, status.active_sq_mapped,
                                                  status.sqIdx_flag, status.sqIdx_value,
                                                  uid_seen));
                    items.delete();
                    return 1'b0;
                end
                item.owner_kind = MEMBLOCK_DEQ_OWNER_LIVE;
                item.uid = uid;
                item.old_dynamic_epoch = status.dynamic_epoch;
                items.push_back(item);
                `uvm_info("LSQ_COMMIT",
                          $sformatf("dut sqDeq preflight live idx=%0d/%0d uid=%0d sq=%0d/%0d ptr_next=%0d",
                                    idx + 1,
                                    count,
                                    uid,
                                    key.flag,
                                    key.value,
                                    ptr_is_next),
                          UVM_LOW)
            end else if (deleted_hit) begin
                item.owner_kind = MEMBLOCK_DEQ_OWNER_REDIRECT_DELETED;
                item.uid = deleted_owner.uid;
                item.old_dynamic_epoch = deleted_owner.old_dynamic_epoch;
                item.redirect_epoch = deleted_owner.redirect_epoch;
                item.cancel_record_id = deleted_owner.cancel_record_id;
                items.push_back(item);
                `uvm_info("LSQ_COMMIT",
                          $sformatf("dut sqDeq preflight redirect-deleted idx=%0d/%0d uid=%0d epoch=%0d sq=%0d/%0d",
                                    idx + 1, count, deleted_owner.uid,
                                    deleted_owner.old_dynamic_epoch,
                                    key.flag, key.value),
                          UVM_LOW)
            end else begin
                report_deq_mismatch($sformatf("stale DUT sqDeq count=%0d key flag=%0d value=%0d has no active uid",
                                              count, key.flag, key.value));
                items.delete();
                return 1'b0;
            end
        end
        return 1'b1;
    endfunction:preflight_dut_sq_deq_from_start

    function bit preflight_dut_sq_deq(input int unsigned count,
                                      input memblock_sq_key_t deq_ptr,
                                      input bit ptr_is_next,
                                      output memblock_sq_deq_apply_item_t items[$]);
        memblock_sq_key_t start_key;

        if (count == 0 || count > MEMBLOCK_DUT_ENSBUFFER_WIDTH) begin
            return preflight_dut_sq_deq_from_start(count, deq_ptr, ptr_is_next, items);
        end
        start_key = sq_deq_start_key(deq_ptr, count, ptr_is_next);
        return preflight_dut_sq_deq_from_start(count, start_key, ptr_is_next, items);
    endfunction:preflight_dut_sq_deq

    function bit preflight_dut_sq_deq_count_only(input int unsigned count,
                                                 output memblock_sq_deq_apply_item_t items[$]);
        ensure_handles();
        // 中文注释：V2 sqDeq raw 只携带 entry count；软件 SQ deq head 是
        // 唯一起点，capability 宏不参与 count 宽度或起点推导。
        return preflight_dut_sq_deq_from_start(count,
                                               lsq_ctrl.sq_deq_ptr,
                                               1'b0,
                                               items);
    endfunction:preflight_dut_sq_deq_count_only

    function void commit_dut_sq_deq(input int unsigned count,
                                    input memblock_sq_deq_apply_item_t items[$]);
        if (count == 0) begin
            return;
        end
        if (items.size() != count) begin
            `uvm_fatal("LSQ_COMMIT", "SQ deq commit list size does not match count")
        end
        lsq_ctrl.release_sq(count);
        foreach (items[idx]) begin
            if (items[idx].owner_kind == MEMBLOCK_DEQ_OWNER_LIVE) begin
                data.release_uid_sq_mapping(items[idx].uid);
            end
        end
    endfunction:commit_dut_sq_deq

    function void apply_dut_lq_deq(input int unsigned count,
                                   input memblock_lq_key_t deq_ptr,
                                   input bit ptr_is_next = 1'b1);
        memblock_lq_deq_apply_item_t lq_items[$];
        memblock_sq_deq_apply_item_t sq_items[$];
        int unsigned deleted_lq_count;
        int unsigned deleted_sq_count;

        if (!preflight_dut_lq_deq(count, deq_ptr, ptr_is_next, lq_items) ||
            !data.preflight_redirect_deleted_deq(lq_items, sq_items,
                                                 deleted_lq_count,
                                                 deleted_sq_count)) begin
            return;
        end
        commit_dut_lq_deq(count, lq_items);
        data.commit_redirect_deleted_deq(lq_items, sq_items,
                                         deleted_lq_count,
                                         deleted_sq_count);
        foreach (lq_items[idx]) begin
            if (lq_items[idx].owner_kind == MEMBLOCK_DEQ_OWNER_LIVE) begin
                data.try_retire_committed_uid(lq_items[idx].uid);
            end
        end
        sync_modeled_head_after_fault_terminal();
    endfunction:apply_dut_lq_deq

    function void apply_dut_sq_deq(input int unsigned count,
                                   input memblock_sq_key_t deq_ptr,
                                   input bit ptr_is_next = 1'b1);
        memblock_lq_deq_apply_item_t lq_items[$];
        memblock_sq_deq_apply_item_t sq_items[$];
        int unsigned deleted_lq_count;
        int unsigned deleted_sq_count;

        if (MEMBLOCK_DUT_HAS_SQ_DEQ_PTR) begin
            if (!preflight_dut_sq_deq(count, deq_ptr, ptr_is_next, sq_items)) begin
                return;
            end
        end else begin
            if (!preflight_dut_sq_deq_count_only(count, sq_items)) begin
                return;
            end
        end
        if (!data.preflight_redirect_deleted_deq(lq_items, sq_items,
                                                 deleted_lq_count,
                                                 deleted_sq_count)) begin
            return;
        end
        commit_dut_sq_deq(count, sq_items);
        data.commit_redirect_deleted_deq(lq_items, sq_items,
                                         deleted_lq_count,
                                         deleted_sq_count);
        foreach (sq_items[idx]) begin
            if (sq_items[idx].owner_kind == MEMBLOCK_DEQ_OWNER_LIVE) begin
                data.try_retire_committed_uid(sq_items[idx].uid);
            end
        end
        sync_modeled_head_after_fault_terminal();
    endfunction:apply_dut_sq_deq

    function void apply_dut_sq_deq_count_only(input int unsigned count);
        memblock_lq_deq_apply_item_t lq_items[$];
        memblock_sq_deq_apply_item_t sq_items[$];
        int unsigned deleted_lq_count;
        int unsigned deleted_sq_count;

        ensure_handles();
        if (count == 0) begin
            return;
        end
        if (!preflight_dut_sq_deq_count_only(count, sq_items) ||
            !data.preflight_redirect_deleted_deq(lq_items, sq_items,
                                                 deleted_lq_count,
                                                 deleted_sq_count)) begin
            return;
        end
        commit_dut_sq_deq(count, sq_items);
        data.commit_redirect_deleted_deq(lq_items, sq_items,
                                         deleted_lq_count,
                                         deleted_sq_count);
        foreach (sq_items[idx]) begin
            if (sq_items[idx].owner_kind == MEMBLOCK_DEQ_OWNER_LIVE) begin
                data.try_retire_committed_uid(sq_items[idx].uid);
            end
        end
        sync_modeled_head_after_fault_terminal();
    endfunction:apply_dut_sq_deq_count_only

    // 返回值表示 full raw 的 LQ/SQ 预检和提交是否完成。resync 模式下 mismatch
    // 只返回失败，调用者必须保留 raw 队首并在后续 service tick 重试。
    function bit apply_raw_ctrl_deq(input memblock_sync_pkg::dispatch_raw_ctrl_t raw);
        memblock_lq_deq_apply_item_t lq_items[$];
        memblock_sq_deq_apply_item_t sq_items[$];
        memblock_lq_key_t lq_ptr;
        memblock_sq_key_t sq_ptr;
        int unsigned deleted_lq_count;
        int unsigned deleted_sq_count;

        ensure_handles();
        data.update_sb_is_empty(raw);
        if (raw.sq_deq == 0 && raw.sq_deq_ptr_valid) begin
            `uvm_fatal("LSQ_COMMIT", "sqDeq pointer is valid while sqDeq count is zero")
        end
        if (!MEMBLOCK_DUT_HAS_SQ_DEQ_PTR && raw.sq_deq_ptr_valid) begin
            `uvm_fatal("LSQ_COMMIT", "V2 count-only SQ deq raw unexpectedly carries a pointer")
        end
        if (MEMBLOCK_DUT_HAS_SQ_DEQ_PTR && raw.sq_deq != 0 && !raw.sq_deq_ptr_valid) begin
            `uvm_fatal("LSQ_COMMIT", "pointer-capable SQ deq raw is missing its pointer")
        end

        lq_ptr.flag = raw.lq_deq_ptr_flag;
        lq_ptr.value = raw.lq_deq_ptr_value;
        sq_ptr.flag = raw.sq_deq_ptr_flag;
        sq_ptr.value = raw.sq_deq_ptr_value;

        // 中文注释：同一 ctrl raw 的 LQ/SQ release 先联合预检；任一侧失败时
        // 不允许另一侧先推进 pointer/free count，避免 deferred raw 部分成功。
        if (!preflight_dut_lq_deq(raw.lq_deq, lq_ptr, 1'b1, lq_items)) begin
            return 1'b0;
        end
        if (MEMBLOCK_DUT_HAS_SQ_DEQ_PTR) begin
            if (!preflight_dut_sq_deq(raw.sq_deq, sq_ptr, 1'b1, sq_items)) begin
                return 1'b0;
            end
        end else begin
            // 中文注释：full raw 仍先联合预检 LQ/SQ；V2 SQ 侧显式按 count-only
            // 语义从软件 head 解析 owner，不能调用会立即提交的独立 wrapper。
            if (!preflight_dut_sq_deq_count_only(raw.sq_deq, sq_items)) begin
                return 1'b0;
            end
        end
        if (!data.preflight_redirect_deleted_deq(lq_items, sq_items,
                                                 deleted_lq_count,
                                                 deleted_sq_count)) begin
            report_deq_mismatch("DUT deq redirect-deleted provenance preflight failed");
            return 1'b0;
        end
        commit_dut_lq_deq(raw.lq_deq, lq_items);
        commit_dut_sq_deq(raw.sq_deq, sq_items);
        data.commit_redirect_deleted_deq(lq_items, sq_items,
                                         deleted_lq_count,
                                         deleted_sq_count);
        foreach (lq_items[idx]) begin
            if (lq_items[idx].owner_kind == MEMBLOCK_DEQ_OWNER_LIVE) begin
                data.try_retire_committed_uid(lq_items[idx].uid);
            end
        end
        foreach (sq_items[idx]) begin
            if (sq_items[idx].owner_kind == MEMBLOCK_DEQ_OWNER_LIVE) begin
                data.try_retire_committed_uid(sq_items[idx].uid);
            end
        end
        sync_modeled_head_after_fault_terminal();
        return 1'b1;
    endfunction:apply_raw_ctrl_deq

endclass:lsq_commit_handler

`endif
