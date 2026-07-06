//=========================================================
//File name    : common_data_transaction.sv
//Author       : OpenAI_Codex
//Module name  : common_data_transaction
//Discribution : shared dispatch framework data owner
//Date         : 2026-05-18
//=========================================================
`ifndef COMMON_DATA_TRANSACTION__SV
`define COMMON_DATA_TRANSACTION__SV

class common_data_transaction extends uvm_object;

    static common_data_transaction m_inst;

    int unsigned   main_trans_num;
    memblock_uid_t next_uid;
    bit            main_table_ready;
    // global_stop_requested由顶层orchestration在所有主表transaction最终terminal_done后置位。
    // 子sequence只读该标志进入收尾退出阶段，避免各自重复维护completion退出条件。
    bit            global_stop_requested;
    // dispatch公共进度：所有admission/route/redirect扫描共享同一组边界，避免10万笔场景全表扫描。
    memblock_dispatch_progress_t dispatch_progress;

    main_control_transaction main_table_by_uid[];
    status_transaction       status_by_uid[];
    memblock_tlb_entry       tlb_entry_by_key[memblock_tlb_lookup_key_t];
    memblock_uid_tlb_record  uid_tlb_record_by_uid[memblock_uid_t];
    mmu_csr_runtime_state    mmu_csr_state;
    memblock_issue_q_item_t  load_issue_q[$];
    memblock_issue_q_item_t  sta_issue_q[$];
    memblock_issue_q_item_t  std_issue_q[$];
    memblock_wb_event_t      exception_event_q[$];
    memblock_redirect_payload_t pending_redirect_drive_q[$];
    memblock_ptw_wait_replay_t  ptw_wait_replay_q[$];
    int unsigned               last_applied_raw_csr_seq;
    int unsigned               pending_lq_cancel_count;
    int unsigned               pending_sq_cancel_count;

    memblock_uid_t uid_by_active_rob[memblock_rob_map_key_t];
    memblock_uid_t uid_by_lq[memblock_lq_map_key_t];
    memblock_uid_t uid_by_sq[memblock_sq_map_key_t];

    bit                         flush_in_progress;
    memblock_redirect_payload_t active_redirect;
    memblock_redirect_phase_e   redirect_phase;
    memblock_redirect_payload_t redirect_drive_inflight_payload;
    bit                         redirect_drive_inflight;
    int unsigned                redirect_drive_done_epoch;
    longint unsigned            redirect_drive_done_cycle;
    longint unsigned            redirect_freeze_cycle;
    int unsigned                global_issue_epoch;
    bit                         issue_freeze_ack;
    // flushSb待处理请求队列。所有producer只入队，LSQ commit sequence是唯一consumer。
    memblock_flushsb_req_t      flushsb_req_q[$];
    // 当前已经随lsqcommit xaction drive到DUT、正在等待sbIsEmpty的请求备份。
    memblock_flushsb_req_t      active_flushsb_req;
    bit                         active_flushsb_req_valid;
    int unsigned                next_flushsb_req_id;
    bit                         flushsb_waiting_empty;
    longint unsigned            flushsb_start_cycle;
    bit                         last_sb_is_empty;
    bit                         flushsb_timeout_warned;

    `uvm_object_utils(common_data_transaction)

    function new(string name = "common_data_transaction");
        super.new(name);
        main_trans_num      = 0;
        next_uid            = 0;
        main_table_ready    = 1'b0;
        global_stop_requested = 1'b0;
        dispatch_progress   = '{default:'0};
        flush_in_progress   = 1'b0;
        active_redirect     = '{default:'0};
        redirect_phase      = MEMBLOCK_REDIRECT_PHASE_IDLE;
        redirect_drive_inflight_payload = '{default:'0};
        redirect_drive_inflight = 1'b0;
        redirect_drive_done_epoch = 0;
        redirect_drive_done_cycle = 0;
        redirect_freeze_cycle = 0;
        global_issue_epoch  = 0;
        issue_freeze_ack    = 1'b0;
        flushsb_req_q.delete();
        active_flushsb_req  = '{default:'0};
        active_flushsb_req_valid = 1'b0;
        next_flushsb_req_id = 0;
        flushsb_waiting_empty = 1'b0;
        flushsb_start_cycle = 0;
        last_sb_is_empty    = 1'b0;
        flushsb_timeout_warned = 1'b0;
        last_applied_raw_csr_seq = 0;
        pending_lq_cancel_count = 0;
        pending_sq_cancel_count = 0;
        mmu_csr_state       = mmu_csr_runtime_state::type_id::create("mmu_csr_state");
        mmu_csr_state.reset();
    endfunction:new

    static function common_data_transaction get();
        if (m_inst == null) begin
            m_inst = new();
        end
        return m_inst;
    endfunction:get

    function void reset_all_tables(input int unsigned main_trans_num_i);
        int unsigned uid;

        if (main_trans_num_i == 0) begin
            `uvm_fatal("COMMON_DATA", "reset_all_tables requires non-zero main_trans_num")
        end

        main_trans_num      = main_trans_num_i;
        next_uid            = 0;
        main_table_ready    = 1'b0;
        global_stop_requested = 1'b0;
        dispatch_progress.terminal_done_uid      = 0;
        dispatch_progress.max_enqueued_uid       = 0;
        dispatch_progress.max_enqueued_uid_valid = 1'b0;
        flush_in_progress   = 1'b0;
        memblock_sync_pkg::dispatch_flush_in_progress = 1'b0;
        memblock_sync_pkg::dispatch_flushsb_waiting_empty = 1'b0;
        memblock_sync_pkg::dispatch_flush_epoch = 0;
        memblock_sync_pkg::clear_raw_monitor_queues();
        memblock_sync_pkg::dispatch_monitor_capture_en = 1'b1;
        active_redirect     = '{default:'0};
        redirect_phase      = MEMBLOCK_REDIRECT_PHASE_IDLE;
        redirect_drive_inflight_payload = '{default:'0};
        redirect_drive_inflight = 1'b0;
        redirect_drive_done_epoch = 0;
        redirect_drive_done_cycle = 0;
        redirect_freeze_cycle = 0;
        global_issue_epoch  = 0;
        issue_freeze_ack    = 1'b0;
        flushsb_req_q.delete();
        active_flushsb_req  = '{default:'0};
        active_flushsb_req_valid = 1'b0;
        next_flushsb_req_id = 0;
        flushsb_waiting_empty = 1'b0;
        flushsb_start_cycle = 0;
        last_sb_is_empty    = 1'b0;
        flushsb_timeout_warned = 1'b0;
        last_applied_raw_csr_seq = 0;
        pending_lq_cancel_count = 0;
        pending_sq_cancel_count = 0;
        main_table_by_uid = new[main_trans_num_i];
        status_by_uid     = new[main_trans_num_i];
        tlb_entry_by_key.delete();
        uid_tlb_record_by_uid.delete();
        clear_issue_queues();
        clear_feedback_events();
        clear_redirect_drive_queue();
        clear_ptw_wait_replay_queue();
        uid_by_active_rob.delete();
        uid_by_lq.delete();
        uid_by_sq.delete();
        if (mmu_csr_state == null) begin
            mmu_csr_state = mmu_csr_runtime_state::type_id::create("mmu_csr_state");
        end
        mmu_csr_state.reset();
        for (uid = 0; uid < main_trans_num_i; uid++) begin
            status_by_uid[uid] = status_transaction::type_id::create($sformatf("status_uid_%0d", uid));
            status_by_uid[uid].reset(uid);
        end
    endfunction:reset_all_tables

    function memblock_uid_t alloc_uid();
        memblock_uid_t uid;

        if (main_trans_num == 0) begin
            `uvm_fatal("COMMON_DATA", "alloc_uid called before reset_all_tables")
        end
        if (next_uid >= main_trans_num) begin
            `uvm_fatal("COMMON_DATA", $sformatf("alloc_uid overflow: next_uid=%0d main_trans_num=%0d", next_uid, main_trans_num))
        end

        uid = next_uid;
        next_uid++;
        return uid;
    endfunction:alloc_uid

    function bit is_valid_uid(input memblock_uid_t uid);
        return (main_trans_num != 0) && (uid < main_trans_num);
    endfunction:is_valid_uid

    function bit is_valid_lq_key(input memblock_lq_key_t key);
        return key.value < MEMBLOCK_LQ_SIZE;
    endfunction:is_valid_lq_key

    function bit is_valid_sq_key(input memblock_sq_key_t key);
        return key.value < MEMBLOCK_SQ_SIZE;
    endfunction:is_valid_sq_key

    function void check_uid(input memblock_uid_t uid, input string caller);
        if (!is_valid_uid(uid)) begin
            `uvm_fatal("COMMON_DATA", $sformatf("%s got invalid uid=%0d main_trans_num=%0d", caller, uid, main_trans_num))
        end
    endfunction:check_uid

    function void set_main_transaction(input memblock_uid_t uid, input main_control_transaction tr);
        check_uid(uid, "set_main_transaction");
        if (tr == null) begin
            `uvm_fatal("COMMON_DATA", "set_main_transaction got null transaction")
        end
        if (status_by_uid[uid] != null && status_by_uid[uid].active) begin
            `uvm_fatal("COMMON_DATA", $sformatf("set_main_transaction must not overwrite active uid=%0d", uid))
        end
        tr.uid = uid;
        main_table_by_uid[uid] = tr;
    endfunction:set_main_transaction

    function main_control_transaction get_main_transaction(input memblock_uid_t uid);
        check_uid(uid, "get_main_transaction");
        if (main_table_by_uid[uid] == null) begin
            `uvm_fatal("COMMON_DATA", $sformatf("main_table_by_uid[%0d] is null", uid))
        end
        return main_table_by_uid[uid];
    endfunction:get_main_transaction

    function void ensure_status_exists(input memblock_uid_t uid, input string caller);
        check_uid(uid, caller);
        if (status_by_uid[uid] == null) begin
            status_by_uid[uid] = status_transaction::type_id::create($sformatf("status_uid_%0d", uid));
            status_by_uid[uid].reset(uid);
        end
    endfunction:ensure_status_exists

    function status_transaction init_status_for_uid(input memblock_uid_t uid);
        status_transaction status;
        check_uid(uid, "init_status_for_uid");
        if (status_by_uid[uid] == null) begin
            status = status_transaction::type_id::create($sformatf("status_uid_%0d", uid));
        end else begin
            status = status_by_uid[uid];
        end
        if (status.active) begin
            `uvm_fatal("COMMON_DATA", $sformatf("init_status_for_uid must not reset active uid=%0d", uid))
        end
        status.reset(uid);
        if (main_table_by_uid[uid] != null) begin
            status.snapshot_from_main(main_table_by_uid[uid]);
        end
        status_by_uid[uid] = status;
        return status;
    endfunction:init_status_for_uid

    function status_transaction get_status(input memblock_uid_t uid);
        check_uid(uid, "get_status");
        if (status_by_uid[uid] == null) begin
            `uvm_fatal("COMMON_DATA", $sformatf("status_by_uid[%0d] is null", uid))
        end
        return status_by_uid[uid];
    endfunction:get_status

    function void mark_uid_enqueued(input memblock_uid_t uid);
        check_uid(uid, "mark_uid_enqueued");
        if (!dispatch_progress.max_enqueued_uid_valid) begin
            if (uid != 0) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("first LSQ admission must be uid0, got uid=%0d", uid))
            end
            dispatch_progress.max_enqueued_uid       = uid;
            dispatch_progress.max_enqueued_uid_valid = 1'b1;
            return;
        end
        if (uid != dispatch_progress.max_enqueued_uid + 1) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("LSQ admission must be sequential: uid=%0d expected=%0d max_enqueued_uid=%0d",
                                 uid,
                                 dispatch_progress.max_enqueued_uid + 1,
                                 dispatch_progress.max_enqueued_uid))
        end
        dispatch_progress.max_enqueued_uid = uid;
    endfunction:mark_uid_enqueued

    function void rollback_max_enqueued_uid(input memblock_uid_t oldest_flushed_uid);
        check_uid(oldest_flushed_uid, "rollback_max_enqueued_uid");
        if (oldest_flushed_uid == 0) begin
            dispatch_progress.max_enqueued_uid       = 0;
            dispatch_progress.max_enqueued_uid_valid = 1'b0;
            return;
        end
        dispatch_progress.max_enqueued_uid       = oldest_flushed_uid - 1;
        dispatch_progress.max_enqueued_uid_valid = 1'b1;
    endfunction:rollback_max_enqueued_uid

    function void advance_terminal_done_uid();
        status_transaction status;

        while (dispatch_progress.terminal_done_uid < main_trans_num) begin
            status = get_status(dispatch_progress.terminal_done_uid);
            if (!status.terminal_done) begin
                break;
            end
            dispatch_progress.terminal_done_uid++;
        end
    endfunction:advance_terminal_done_uid

    function bit transaction_done();
        advance_terminal_done_uid();
        return dispatch_progress.terminal_done_uid >= main_trans_num;
    endfunction:transaction_done

    function void request_global_stop_if_done();
        if (transaction_done()) begin
            global_stop_requested = 1'b1;
        end
    endfunction:request_global_stop_if_done

    function bit is_global_stop_requested();
        return global_stop_requested;
    endfunction:is_global_stop_requested

    function memblock_uid_t get_active_scan_begin_uid();
        return dispatch_progress.terminal_done_uid;
    endfunction:get_active_scan_begin_uid

    function memblock_uid_t get_active_scan_end_uid();
        if (!dispatch_progress.max_enqueued_uid_valid) begin
            return dispatch_progress.terminal_done_uid;
        end
        return dispatch_progress.max_enqueued_uid + 1;
    endfunction:get_active_scan_end_uid

    function memblock_uid_t get_next_new_admit_uid();
        if (!dispatch_progress.max_enqueued_uid_valid) begin
            return 0;
        end
        return dispatch_progress.max_enqueued_uid + 1;
    endfunction:get_next_new_admit_uid

    function void set_status_field(input memblock_uid_t uid,
                                   input memblock_status_field_e field,
                                   input bit value);
        status_transaction status;
        bit old_value;

        ensure_status_exists(uid, "set_status_field");
        status = status_by_uid[uid];
        case (field)
            MEMBLOCK_STATUS_ACTIVE: begin
                `uvm_fatal("COMMON_DATA", "set_status_field must not update active directly; use activate_uid/retire_active_uid")
            end
            MEMBLOCK_STATUS_ENQ: begin
                old_value = status.enq;
                status.enq = value;
                if (value && !old_value) begin
                    mark_uid_enqueued(uid);
                    // redirect reissue重新admission成功后，旧动态实例的flush标志不再阻塞route/commit。
                    if (status.redirect_pending || status.flushed) begin
                        status.redirect_pending = 1'b0;
                        status.flushed          = 1'b0;
                        status.issue_killed     = 1'b0;
                    end
                end
            end
            MEMBLOCK_STATUS_ISSUE_READY:       status.issue_ready       = value;
            MEMBLOCK_STATUS_TLB_MAPPED:        status.tlb_mapped        = value;
            MEMBLOCK_STATUS_QUEUED_LOAD:       status.queued_load       = value;
            MEMBLOCK_STATUS_QUEUED_STA:        status.queued_sta        = value;
            MEMBLOCK_STATUS_QUEUED_STD:        status.queued_std        = value;
            MEMBLOCK_STATUS_LOAD_DISPATCHED:   status.load_dispatched   = value;
            MEMBLOCK_STATUS_STA_DISPATCHED:    status.sta_dispatched    = value;
            MEMBLOCK_STATUS_STD_DISPATCHED:    status.std_dispatched    = value;
            MEMBLOCK_STATUS_WRITEBACK:         status.writeback         = value;
            MEMBLOCK_STATUS_PASS:              status.pass              = value;
            MEMBLOCK_STATUS_FAULT:             status.fault             = value;
            MEMBLOCK_STATUS_LOAD_WRITEBACK:    status.load_writeback    = value;
            MEMBLOCK_STATUS_STA_WRITEBACK:     status.sta_writeback     = value;
            MEMBLOCK_STATUS_STD_WRITEBACK:     status.std_writeback     = value;
            MEMBLOCK_STATUS_LOAD_PASS:         status.load_pass         = value;
            MEMBLOCK_STATUS_STA_PASS:          status.sta_pass          = value;
            MEMBLOCK_STATUS_STD_PASS:          status.std_pass          = value;
            MEMBLOCK_STATUS_LOAD_FAULT:        status.load_fault        = value;
            MEMBLOCK_STATUS_STA_FAULT:         status.sta_fault         = value;
            MEMBLOCK_STATUS_STD_FAULT:         status.std_fault         = value;
            MEMBLOCK_STATUS_EXCEPTION_PENDING: status.exception_pending = value;
            MEMBLOCK_STATUS_REPLAY_PENDING:    status.replay_pending    = value;
            MEMBLOCK_STATUS_REDIRECT_PENDING:  status.redirect_pending  = value;
            MEMBLOCK_STATUS_FLUSHED:           status.flushed           = value;
            MEMBLOCK_STATUS_ROB_COMMIT:        status.rob_commit        = value;
            MEMBLOCK_STATUS_LSQ_DEQ:           status.lsq_deq           = value;
            MEMBLOCK_STATUS_SUCCESS: begin
                status.success = value;
            end
            MEMBLOCK_STATUS_TERMINAL_DONE: begin
                status.terminal_done = value;
                if (value) begin
                    advance_terminal_done_uid();
                end
            end
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("unknown status field=%0d", field))
            end
        endcase
    endfunction:set_status_field

    function bit get_status_field(input memblock_uid_t uid,
                                  input memblock_status_field_e field);
        status_transaction status;

        status = get_status(uid);
        case (field)
            MEMBLOCK_STATUS_ACTIVE:            return status.active;
            MEMBLOCK_STATUS_ENQ:               return status.enq;
            MEMBLOCK_STATUS_ISSUE_READY:       return status.issue_ready;
            MEMBLOCK_STATUS_TLB_MAPPED:        return status.tlb_mapped;
            MEMBLOCK_STATUS_QUEUED_LOAD:       return status.queued_load;
            MEMBLOCK_STATUS_QUEUED_STA:        return status.queued_sta;
            MEMBLOCK_STATUS_QUEUED_STD:        return status.queued_std;
            MEMBLOCK_STATUS_LOAD_DISPATCHED:   return status.load_dispatched;
            MEMBLOCK_STATUS_STA_DISPATCHED:    return status.sta_dispatched;
            MEMBLOCK_STATUS_STD_DISPATCHED:    return status.std_dispatched;
            MEMBLOCK_STATUS_WRITEBACK:         return status.writeback;
            MEMBLOCK_STATUS_PASS:              return status.pass;
            MEMBLOCK_STATUS_FAULT:             return status.fault;
            MEMBLOCK_STATUS_LOAD_WRITEBACK:    return status.load_writeback;
            MEMBLOCK_STATUS_STA_WRITEBACK:     return status.sta_writeback;
            MEMBLOCK_STATUS_STD_WRITEBACK:     return status.std_writeback;
            MEMBLOCK_STATUS_LOAD_PASS:         return status.load_pass;
            MEMBLOCK_STATUS_STA_PASS:          return status.sta_pass;
            MEMBLOCK_STATUS_STD_PASS:          return status.std_pass;
            MEMBLOCK_STATUS_LOAD_FAULT:        return status.load_fault;
            MEMBLOCK_STATUS_STA_FAULT:         return status.sta_fault;
            MEMBLOCK_STATUS_STD_FAULT:         return status.std_fault;
            MEMBLOCK_STATUS_EXCEPTION_PENDING: return status.exception_pending;
            MEMBLOCK_STATUS_REPLAY_PENDING:    return status.replay_pending;
            MEMBLOCK_STATUS_REDIRECT_PENDING:  return status.redirect_pending;
            MEMBLOCK_STATUS_FLUSHED:           return status.flushed;
            MEMBLOCK_STATUS_ROB_COMMIT:        return status.rob_commit;
            MEMBLOCK_STATUS_LSQ_DEQ:           return status.lsq_deq;
            MEMBLOCK_STATUS_SUCCESS:           return status.success;
            MEMBLOCK_STATUS_TERMINAL_DONE:     return status.terminal_done;
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("unknown status field=%0d", field))
            end
        endcase
        return 1'b0;
    endfunction:get_status_field

    function int unsigned alloc_issue_epoch();
        global_issue_epoch++;
        return global_issue_epoch;
    endfunction:alloc_issue_epoch

    function void mark_issue_snapshot(input memblock_uid_t uid,
                                      input memblock_issue_target_e issue_target,
                                      input int unsigned issue_epoch);
        status_transaction status;

        ensure_status_exists(uid, "mark_issue_snapshot");
        status = status_by_uid[uid];
        status.set_target_issue_epoch(issue_target, issue_epoch);
        status.issue_killed = 1'b0;
        register_uid_tlb_record_on_issue(uid);
    endfunction:mark_issue_snapshot

    function memblock_status_field_e target_writeback_field(input memblock_issue_target_e target);
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: return MEMBLOCK_STATUS_LOAD_WRITEBACK;
            MEMBLOCK_ISSUE_TARGET_STA:  return MEMBLOCK_STATUS_STA_WRITEBACK;
            MEMBLOCK_ISSUE_TARGET_STD:  return MEMBLOCK_STATUS_STD_WRITEBACK;
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("target_writeback_field got target=%0d", target))
            end
        endcase
        return MEMBLOCK_STATUS_WRITEBACK;
    endfunction:target_writeback_field

    function memblock_status_field_e target_pass_field(input memblock_issue_target_e target);
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: return MEMBLOCK_STATUS_LOAD_PASS;
            MEMBLOCK_ISSUE_TARGET_STA:  return MEMBLOCK_STATUS_STA_PASS;
            MEMBLOCK_ISSUE_TARGET_STD:  return MEMBLOCK_STATUS_STD_PASS;
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("target_pass_field got target=%0d", target))
            end
        endcase
        return MEMBLOCK_STATUS_PASS;
    endfunction:target_pass_field

    function memblock_status_field_e target_fault_field(input memblock_issue_target_e target);
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: return MEMBLOCK_STATUS_LOAD_FAULT;
            MEMBLOCK_ISSUE_TARGET_STA:  return MEMBLOCK_STATUS_STA_FAULT;
            MEMBLOCK_ISSUE_TARGET_STD:  return MEMBLOCK_STATUS_STD_FAULT;
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("target_fault_field got target=%0d", target))
            end
        endcase
        return MEMBLOCK_STATUS_FAULT;
    endfunction:target_fault_field

    function bit target_entry_done(input status_transaction status,
                                   input memblock_issue_target_e target);
        if (status == null) begin
            `uvm_fatal("COMMON_DATA", "target_entry_done got null status")
        end
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: return status.load_pass || status.load_fault;
            MEMBLOCK_ISSUE_TARGET_STA:  return status.sta_pass  || status.sta_fault;
            MEMBLOCK_ISSUE_TARGET_STD:  return status.std_pass  || status.std_fault;
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("target_entry_done got target=%0d", target))
            end
        endcase
        return 1'b0;
    endfunction:target_entry_done

    function bit target_dispatched(input status_transaction status,
                                   input memblock_issue_target_e target);
        if (status == null) begin
            `uvm_fatal("COMMON_DATA", "target_dispatched got null status")
        end
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: return status.load_dispatched;
            MEMBLOCK_ISSUE_TARGET_STA:  return status.sta_dispatched;
            MEMBLOCK_ISSUE_TARGET_STD:  return status.std_dispatched;
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("target_dispatched got target=%0d", target))
            end
        endcase
        return 1'b0;
    endfunction:target_dispatched

    function bit target_replay_seq_match(input status_transaction status,
                                         input memblock_issue_target_e target,
                                         input int unsigned replay_seq);
        if (status == null) begin
            `uvm_fatal("COMMON_DATA", "target_replay_seq_match got null status")
        end
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD,
            MEMBLOCK_ISSUE_TARGET_STA: return status.replay_seq == replay_seq;
            MEMBLOCK_ISSUE_TARGET_STD: return 1'b1;
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("target_replay_seq_match got target=%0d", target))
            end
        endcase
        return 1'b0;
    endfunction:target_replay_seq_match

    function bit required_targets_done(input memblock_uid_t uid);
        status_transaction       status;
        main_control_transaction main_tr;

        status   = get_status(uid);
        main_tr  = get_main_transaction(uid);
        if (main_tr.fuType == MEMBLOCK_FUTYPE_LDU) begin
            return target_entry_done(status, MEMBLOCK_ISSUE_TARGET_LOAD);
        end
        if (main_tr.fuType == MEMBLOCK_FUTYPE_STU || main_tr.fuType == MEMBLOCK_FUTYPE_MOU) begin
            return target_entry_done(status, MEMBLOCK_ISSUE_TARGET_STA) &&
                   target_entry_done(status, MEMBLOCK_ISSUE_TARGET_STD);
        end
        `uvm_fatal("COMMON_DATA", $sformatf("required_targets_done uid=%0d got unsupported fuType=0x%0h", uid, main_tr.fuType))
        return 1'b0;
    endfunction:required_targets_done

    function bit conditional_set_target_status_field(input memblock_uid_t uid,
                                                     input memblock_status_field_e field,
                                                     input bit value,
                                                     input memblock_issue_target_e target,
                                                     input int unsigned issue_epoch,
                                                     input int unsigned replay_seq);
        status_transaction status;

        status = get_status(uid);
        if (!status.active || status.issue_killed ||
            !target_dispatched(status, target)) begin
            return 1'b0;
        end
        if (status.get_target_issue_epoch(target) != issue_epoch ||
            !target_replay_seq_match(status, target, replay_seq)) begin
            return 1'b0;
        end
        set_status_field(uid, field, value);
        return 1'b1;
    endfunction:conditional_set_target_status_field

    function bit mark_target_normal_pass(input memblock_uid_t uid,
                                         input memblock_issue_target_e target,
                                         input int unsigned issue_epoch,
                                         input int unsigned replay_seq,
                                         input longint unsigned cycle);
        status_transaction status;

        status = get_status(uid);
        if (status.fault || status.exception_pending ||
            status.redirect_pending ||
            target_entry_done(status, target)) begin
            return 1'b0;
        end
        if (status.replay_pending && replay_target_requested(status, target)) begin
            return 1'b0;
        end
        if (!conditional_set_target_status_field(uid, target_writeback_field(target), 1'b1, target, issue_epoch, replay_seq)) begin
            return 1'b0;
        end
        if (!conditional_set_target_status_field(uid, target_pass_field(target), 1'b1, target, issue_epoch, replay_seq)) begin
            return 1'b0;
        end
        status = get_status(uid);
        status.last_event_cycle = cycle;
        if (required_targets_done(uid) && !status.fault &&
            !status.exception_pending && !status.replay_pending && !status.redirect_pending) begin
            status.writeback = 1'b1;
            status.pass      = 1'b1;
        end
        return 1'b1;
    endfunction:mark_target_normal_pass

    function bit mark_issue_feedback_success(input memblock_uid_t uid,
                                             input memblock_issue_target_e target,
                                             input int unsigned issue_epoch,
                                             input int unsigned replay_seq,
                                             input longint unsigned cycle);
        status_transaction status;

        status = get_status(uid);
        // 中文注释：IssueQueue feedback success 只证明本次 issue response finalSuccess。
        // 这里复用 active/issue_killed/target_dispatched/issue_epoch/replay_seq 检查，过滤 replay/redirect 后迟到的旧 feedback；
        // 通过后仅设置 *_issue_feedback_success，不设置 *_writeback 或 *_pass，真实完成仍等待 real writeback。
        if (!status.active || status.issue_killed ||
            !target_dispatched(status, target) ||
            status.get_target_issue_epoch(target) != issue_epoch ||
            !target_replay_seq_match(status, target, replay_seq)) begin
            return 1'b0;
        end
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: status.load_issue_feedback_success = 1'b1;
            MEMBLOCK_ISSUE_TARGET_STA:  status.sta_issue_feedback_success  = 1'b1;
            MEMBLOCK_ISSUE_TARGET_STD:  status.std_issue_feedback_success  = 1'b1;
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("mark_issue_feedback_success got target=%0d", target))
            end
        endcase
        status.last_event_cycle = cycle;
        return 1'b1;
    endfunction:mark_issue_feedback_success

    function bit mark_target_fault(input memblock_uid_t uid,
                                   input memblock_issue_target_e target,
                                   input int unsigned issue_epoch,
                                   input int unsigned replay_seq,
                                   input bit [23:0] exception_vec,
                                   input longint unsigned cycle);
        status_transaction status;

        if (!conditional_set_target_status_field(uid, target_writeback_field(target), 1'b1, target, issue_epoch, replay_seq)) begin
            return 1'b0;
        end
        if (!conditional_set_target_status_field(uid, target_fault_field(target), 1'b1, target, issue_epoch, replay_seq)) begin
            return 1'b0;
        end
        status = get_status(uid);
        status.fault             = 1'b1;
        status.exception_pending = 1'b1;
        status.exception_vec     = exception_vec;
        status.pass              = 1'b0;
        status.success           = 1'b0;
        status.terminal_done     = 1'b0;
        status.last_event_cycle  = cycle;
        return 1'b1;
    endfunction:mark_target_fault

    function void set_replay_target_mask(input status_transaction status,
                                         input bit replay_load,
                                         input bit replay_sta,
                                         input bit replay_std);
        if (status == null) begin
            `uvm_fatal("COMMON_DATA", "set_replay_target_mask got null status")
        end
        status.replay_target_load = replay_load;
        status.replay_target_sta  = replay_sta;
        status.replay_target_std  = replay_std;
    endfunction:set_replay_target_mask

    function bit mark_replay_pending(input memblock_uid_t uid,
                                     input memblock_issue_target_e target,
                                     input int unsigned issue_epoch,
                                     input int unsigned replay_seq,
                                     input longint unsigned cycle);
        status_transaction status;

        status = get_status(uid);
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD,
            MEMBLOCK_ISSUE_TARGET_STA: begin
            end
            MEMBLOCK_ISSUE_TARGET_STD: begin
                `uvm_warning("COMMON_DATA",
                             $sformatf("ignore STD replay request uid=%0d: MemBlock has no backend STD replay feedback path", uid))
                return 1'b0;
            end
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("mark_replay_pending got target=%0d", target))
            end
        endcase
        if (!status.active || status.issue_killed ||
            !target_dispatched(status, target) ||
            status.get_target_issue_epoch(target) != issue_epoch ||
            !target_replay_seq_match(status, target, replay_seq)) begin
            return 1'b0;
        end
        delete_issue_queue_entry(target, uid, 0, 1'b0);
        status.replay_pending = 1'b1;
        status.writeback      = 1'b0;
        status.pass           = 1'b0;
        status.success        = 1'b0;
        status.terminal_done  = 1'b0;
        status.last_event_cycle = cycle;
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: begin
                status.load_dispatched = 1'b0;
                status.load_writeback  = 1'b0;
                status.load_issue_feedback_success = 1'b0;
                status.load_pass       = 1'b0;
                status.queued_load     = 1'b0;
                status.replay_target_load = 1'b1;
            end
            MEMBLOCK_ISSUE_TARGET_STA: begin
                status.sta_dispatched = 1'b0;
                status.sta_writeback  = 1'b0;
                status.sta_issue_feedback_success = 1'b0;
                status.sta_pass       = 1'b0;
                status.queued_sta      = 1'b0;
                status.replay_target_sta = 1'b1;
            end
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("mark_replay_pending got target=%0d", target))
            end
        endcase
        bump_replay_seq(uid);
        return 1'b1;
    endfunction:mark_replay_pending

    function bit replay_target_requested(input status_transaction status,
                                         input memblock_issue_target_e target);
        if (status == null) begin
            `uvm_fatal("COMMON_DATA", "replay_target_requested got null status")
        end
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: return status.replay_target_load;
            MEMBLOCK_ISSUE_TARGET_STA:  return status.replay_target_sta;
            MEMBLOCK_ISSUE_TARGET_STD:  return status.replay_target_std;
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("replay_target_requested got target=%0d", target))
            end
        endcase
        return 1'b0;
    endfunction:replay_target_requested

    function bit replay_targets_empty(input status_transaction status);
        if (status == null) begin
            `uvm_fatal("COMMON_DATA", "replay_targets_empty got null status")
        end
        return !status.replay_target_load &&
               !status.replay_target_sta &&
               !status.replay_target_std;
    endfunction:replay_targets_empty

    function void clear_replay_target_after_fire(input memblock_uid_t uid,
                                                 input memblock_issue_target_e target);
        status_transaction status;

        status = get_status(uid);
        if (!status.replay_pending) begin
            return;
        end
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: status.replay_target_load = 1'b0;
            MEMBLOCK_ISSUE_TARGET_STA:  status.replay_target_sta  = 1'b0;
            MEMBLOCK_ISSUE_TARGET_STD:  status.replay_target_std  = 1'b0;
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("clear_replay_target_after_fire got target=%0d", target))
            end
        endcase
        if (replay_targets_empty(status)) begin
            status.replay_pending = 1'b0;
        end
    endfunction:clear_replay_target_after_fire

    function void bump_replay_seq(input memblock_uid_t uid);
        status_transaction status;

        status = get_status(uid);
        status.replay_seq++;
    endfunction:bump_replay_seq

    function void clear_uid_dispatch_result(input memblock_uid_t uid);
        status_transaction status;

        status = get_status(uid);
        status.enq             = 1'b0;
        status.issue_ready     = 1'b0;
        status.queued_load     = 1'b0;
        status.queued_sta      = 1'b0;
        status.queued_std      = 1'b0;
        status.load_dispatched = 1'b0;
        status.sta_dispatched  = 1'b0;
        status.std_dispatched  = 1'b0;
        status.load_writeback  = 1'b0;
        status.sta_writeback   = 1'b0;
        status.std_writeback   = 1'b0;
        status.load_issue_feedback_success = 1'b0;
        status.sta_issue_feedback_success = 1'b0;
        status.std_issue_feedback_success = 1'b0;
        status.load_pass       = 1'b0;
        status.sta_pass        = 1'b0;
        status.std_pass        = 1'b0;
        status.writeback       = 1'b0;
        status.pass            = 1'b0;
        status.issue_killed    = 1'b1;
        status.exception_pending = 1'b0;
        status.replay_pending  = 1'b0;
        status.replay_target_load = 1'b0;
        status.replay_target_sta = 1'b0;
        status.replay_target_std = 1'b0;
        status.redirect_pending = 1'b0;
        status.rob_commit      = 1'b0;
        status.lsq_deq         = 1'b0;
        status.success         = 1'b0;
        status.terminal_done   = 1'b0;
        status.fault           = 1'b0;
        status.load_fault      = 1'b0;
        status.sta_fault       = 1'b0;
        status.std_fault       = 1'b0;
        status.exception_vec   = '0;
        status.exception_vaddr = '0;
        status.exception_gpaddr = '0;
    endfunction:clear_uid_dispatch_result

    function void prepare_uid_for_redirect_reissue(input memblock_uid_t uid,
                                                   input memblock_redirect_payload_t redirect);
        status_transaction status;
        main_control_transaction main_tr;
        bit had_lq_mapping;
        bit had_sq_mapping;

        if (!redirect.valid) begin
            `uvm_fatal("COMMON_DATA", "prepare_uid_for_redirect_reissue requires valid redirect")
        end
        status = get_status(uid);
        if (status.terminal_done) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("redirect tries to flush already terminal_done uid=%0d", uid))
        end

        main_tr = get_main_transaction(uid);
        had_lq_mapping = status.active_lq_mapped;
        had_sq_mapping = status.active_sq_mapped;
        // redirect命中的旧动态实例不再等待writeback/commit；清queue/map后等待同uid重新admission。
        if (status.active) begin
            retire_active_uid(uid);
        end else begin
            remove_uid_from_issue_queues(uid);
        end
        // 记录需要回退的软件LSQ admission镜像；LSQ sequence恢复后统一调用cancel_lq/cancel_sq消费。
        if (had_lq_mapping) begin
            pending_lq_cancel_count += main_tr.numLsElem;
        end
        if (had_sq_mapping) begin
            pending_sq_cancel_count += main_tr.numLsElem;
        end
        clear_uid_dispatch_result(uid);
        status.redirect_pending = 1'b1;
        status.flushed          = 1'b1;
        status.dynamic_epoch++;
        status.active           = 1'b0;
        status.success          = 1'b0;
        status.terminal_done    = 1'b0;
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    endfunction:prepare_uid_for_redirect_reissue

    function void request_redirect_flush(input memblock_redirect_payload_t redirect);
        if (!redirect.valid) begin
            `uvm_fatal("COMMON_DATA", "request_redirect_flush requires valid redirect")
        end
        redirect_phase    = MEMBLOCK_REDIRECT_PHASE_DETECTED;
        flush_in_progress = 1'b1;
        memblock_sync_pkg::dispatch_flush_in_progress = 1'b1;
        memblock_sync_pkg::dispatch_flush_epoch++;
        issue_freeze_ack  = 1'b1;
        active_redirect   = redirect;
        redirect_phase    = MEMBLOCK_REDIRECT_PHASE_FREEZE_REQUESTED;
        redirect_freeze_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    endfunction:request_redirect_flush

    function bit redirect_payload_equal(input memblock_redirect_payload_t left,
                                        input memblock_redirect_payload_t right);
        return left.valid == right.valid &&
               left.flush_itself == right.flush_itself &&
               left.level == right.level &&
               left.rob_key.flag == right.rob_key.flag &&
               left.rob_key.value == right.rob_key.value;
    endfunction:redirect_payload_equal

    function void push_redirect_drive(input memblock_redirect_payload_t payload);
        if (!payload.valid) begin
            `uvm_fatal("COMMON_DATA", "push_redirect_drive requires valid payload")
        end
        pending_redirect_drive_q.push_back(payload);
    endfunction:push_redirect_drive

    function bit try_pop_redirect_drive(output memblock_redirect_payload_t payload);
        if (pending_redirect_drive_q.size() == 0 || redirect_drive_inflight) begin
            payload = '{default:'0};
            return 1'b0;
        end
        payload = pending_redirect_drive_q.pop_front();
        redirect_drive_inflight_payload = payload;
        redirect_drive_inflight = 1'b1;
        return 1'b1;
    endfunction:try_pop_redirect_drive

    function void mark_redirect_drive_done(input memblock_redirect_payload_t payload);
        if (!payload.valid) begin
            `uvm_fatal("COMMON_DATA", "mark_redirect_drive_done requires valid payload")
        end
        if (redirect_drive_inflight &&
            !redirect_payload_equal(payload, redirect_drive_inflight_payload)) begin
            `uvm_fatal("COMMON_DATA", "mark_redirect_drive_done got payload that does not match inflight redirect")
        end
        redirect_drive_inflight = 1'b0;
        redirect_drive_inflight_payload = '{default:'0};
        redirect_drive_done_epoch++;
        redirect_drive_done_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        if (active_redirect.valid && redirect_payload_equal(payload, active_redirect)) begin
            redirect_phase = MEMBLOCK_REDIRECT_PHASE_REDIRECT_DRIVEN;
        end
    endfunction:mark_redirect_drive_done

    function bit has_pending_redirect_drive();
        return pending_redirect_drive_q.size() != 0 || redirect_drive_inflight;
    endfunction:has_pending_redirect_drive

    function bit redirect_drive_done_for(input memblock_redirect_payload_t payload);
        if (!payload.valid) begin
            return 1'b0;
        end
        if (redirect_drive_inflight && redirect_payload_equal(payload, redirect_drive_inflight_payload)) begin
            return 1'b0;
        end
        foreach (pending_redirect_drive_q[idx]) begin
            if (redirect_payload_equal(payload, pending_redirect_drive_q[idx])) begin
                return 1'b0;
            end
        end
        return redirect_drive_done_epoch != 0 &&
               redirect_phase >= MEMBLOCK_REDIRECT_PHASE_REDIRECT_DRIVEN &&
               memblock_sync_pkg::get_dispatch_service_cycle() > redirect_drive_done_cycle;
    endfunction:redirect_drive_done_for

    function void clear_redirect_drive_queue();
        pending_redirect_drive_q.delete();
        redirect_drive_inflight = 1'b0;
        redirect_drive_inflight_payload = '{default:'0};
    endfunction:clear_redirect_drive_queue

    function bit issue_blocked_by_global_flush();
        return flush_in_progress ||
               active_redirect.valid ||
               issue_freeze_ack ||
               has_pending_redirect_drive() ||
               memblock_sync_pkg::dispatch_flush_in_progress;
    endfunction:issue_blocked_by_global_flush

    function void apply_redirect_flush_range(input memblock_redirect_payload_t redirect);
        memblock_uid_t begin_uid;
        memblock_uid_t end_uid;
        memblock_uid_t oldest_flushed_uid;
        bit found_flushed;

        if (!redirect.valid) begin
            `uvm_fatal("COMMON_DATA", "apply_redirect_flush_range requires valid redirect")
        end
        advance_terminal_done_uid();
        begin_uid = get_active_scan_begin_uid();
        end_uid   = get_active_scan_end_uid();
        found_flushed = 1'b0;

        // redirect flush只扫描已admission的活跃窗口；真正flush判断仍由ROB顺序语义决定。
        for (memblock_uid_t uid = begin_uid; uid < end_uid; uid++) begin
            status_transaction status;
            memblock_rob_key_t rob_key;

            status = get_status(uid);
            if (status.terminal_done || (!status.active && !status.writeback && !status.pass)) begin
                continue;
            end
            rob_key = status.get_rob_key();
            if (rob_order_util::rob_need_flush(rob_key, redirect)) begin
                if (!found_flushed || uid < oldest_flushed_uid) begin
                    oldest_flushed_uid = uid;
                    found_flushed = 1'b1;
                end
                prepare_uid_for_redirect_reissue(uid, redirect);
            end
        end
        if (found_flushed) begin
            rollback_max_enqueued_uid(oldest_flushed_uid);
        end
    endfunction:apply_redirect_flush_range

    function void apply_redirect_flush(input memblock_redirect_payload_t redirect);
        if (!redirect.valid) begin
            `uvm_fatal("COMMON_DATA", "apply_redirect_flush requires valid redirect")
        end
        apply_redirect_flush_range(redirect);
        clear_ptw_wait_replay_by_redirect(redirect);
        clear_redirect_drive_queue();
        redirect_phase = MEMBLOCK_REDIRECT_PHASE_STATE_FLUSH_APPLIED;
        flush_in_progress  = 1'b0;
        memblock_sync_pkg::dispatch_flush_in_progress = 1'b0;
        issue_freeze_ack   = 1'b0;
        active_redirect    = '{default:'0};
        redirect_phase     = MEMBLOCK_REDIRECT_PHASE_IDLE;
    endfunction:apply_redirect_flush

    function memblock_wb_event_t make_empty_wb_event();
        memblock_wb_event_t wb_event;

        wb_event.valid             = 1'b0;
        wb_event.source            = MEMBLOCK_WB_EVENT_SOURCE_NONE;
        wb_event.port_id           = 0;
        wb_event.target            = MEMBLOCK_ISSUE_TARGET_NONE;
        wb_event.uid               = 0;
        wb_event.has_uid           = 1'b0;
        wb_event.rob_key           = '{default:'0};
        wb_event.has_rob           = 1'b0;
        wb_event.lq_key            = '{default:'0};
        wb_event.has_lq            = 1'b0;
        wb_event.sq_key            = '{default:'0};
        wb_event.has_sq            = 1'b0;
        wb_event.issue_epoch       = 0;
        wb_event.has_issue_epoch   = 1'b0;
        wb_event.replay_seq        = 0;
        wb_event.has_replay_seq    = 1'b0;
        wb_event.real_wb_valid      = 1'b0;
        wb_event.has_exception     = 1'b0;
        wb_event.exception_vec     = '0;
        wb_event.iq_feedback_valid = 1'b0;
        wb_event.iq_feedback_hit   = 1'b0;
        wb_event.iq_feedback_failed = 1'b0;
        wb_event.iq_feedback_flush_state = 1'b0;
        wb_event.replay_valid      = 1'b0;
        wb_event.redirect_valid    = 1'b0;
        wb_event.redirect          = '{default:'0};
        wb_event.ptw_back_replay   = 1'b0;
        wb_event.vector_ls         = 1'b0;
        wb_event.uop_index         = 0;
        wb_event.cycle             = 0;
        return wb_event;
    endfunction:make_empty_wb_event

    function bit feedback_event_is_redirect(input memblock_wb_event_t wb_event);
        if (wb_event.redirect_valid !== wb_event.redirect.valid) begin
            `uvm_fatal("COMMON_DATA", $sformatf("redirect valid mismatch: redirect_valid=%0b redirect.valid=%0b source=%0d",
                                                wb_event.redirect_valid, wb_event.redirect.valid, wb_event.source))
        end
        return wb_event.redirect.valid;
    endfunction:feedback_event_is_redirect

    function bit feedback_event_is_replay(input memblock_wb_event_t wb_event);
        return wb_event.replay_valid;
    endfunction:feedback_event_is_replay

    function bit feedback_event_has_fault(input memblock_wb_event_t wb_event);
        return wb_event.has_exception || wb_event.exception_vec != '0;
    endfunction:feedback_event_has_fault

    function bit feedback_event_has_action(input memblock_wb_event_t wb_event);
        return feedback_event_is_redirect(wb_event) ||
               feedback_event_is_replay(wb_event) ||
               feedback_event_has_fault(wb_event) ||
               wb_event.real_wb_valid ||
               wb_event.iq_feedback_valid;
    endfunction:feedback_event_has_action

    function bit feedback_event_target_is_valid(input memblock_issue_target_e target);
        return target == MEMBLOCK_ISSUE_TARGET_LOAD ||
               target == MEMBLOCK_ISSUE_TARGET_STA ||
               target == MEMBLOCK_ISSUE_TARGET_STD;
    endfunction:feedback_event_target_is_valid

    function bit normalize_feedback_event(input memblock_wb_event_t wb_event,
                                          output memblock_wb_event_t normalized_event);
        memblock_uid_t     uid;
        status_transaction status;

        normalized_event = wb_event;
        if (!normalized_event.valid || !feedback_event_has_action(normalized_event)) begin
            normalized_event = make_empty_wb_event();
            return 1'b0;
        end
        if (normalized_event.redirect.valid && !normalized_event.has_rob) begin
            normalized_event.rob_key = normalized_event.redirect.rob_key;
            normalized_event.has_rob = 1'b1;
        end
        if (!resolve_uid_for_event(normalized_event, uid)) begin
            normalized_event = make_empty_wb_event();
            return 1'b0;
        end
        status = get_status(uid);
        normalized_event.uid     = uid;
        normalized_event.has_uid = 1'b1;
        if (!normalized_event.has_rob) begin
            normalized_event.rob_key = status.get_rob_key();
            normalized_event.has_rob = 1'b1;
        end
        if (!feedback_event_is_redirect(normalized_event)) begin
            if (!feedback_event_target_is_valid(normalized_event.target)) begin
                `uvm_fatal("COMMON_DATA", $sformatf("normalize_feedback_event got unsupported target=%0d", normalized_event.target))
            end
            if (status.replay_seq != 0 &&
                ((!normalized_event.has_issue_epoch &&
                  normalized_event.target != MEMBLOCK_ISSUE_TARGET_STD) ||
                 (!normalized_event.has_replay_seq &&
                  normalized_event.target != MEMBLOCK_ISSUE_TARGET_STD))) begin
                `uvm_warning("COMMON_DATA",
                             $sformatf("drop feedback wb_event uid=%0d target=%0d replay_seq=%0d because issue_epoch/replay_seq snapshot is missing after replay",
                                       uid, normalized_event.target, status.replay_seq))
                normalized_event = make_empty_wb_event();
                return 1'b0;
            end
            if (!normalized_event.has_issue_epoch) begin
                if (!target_dispatched(status, normalized_event.target)) begin
                    `uvm_warning("COMMON_DATA",
                                 $sformatf("drop feedback wb_event uid=%0d target=%0d because issue_epoch snapshot is missing before target dispatched",
                                           uid, normalized_event.target))
                    normalized_event = make_empty_wb_event();
                    return 1'b0;
                end
                normalized_event.issue_epoch = status.get_target_issue_epoch(normalized_event.target);
                normalized_event.has_issue_epoch = 1'b1;
            end
        end
        if (!normalized_event.has_replay_seq) begin
            normalized_event.replay_seq = status.replay_seq;
            normalized_event.has_replay_seq = 1'b1;
        end
        return 1'b1;
    endfunction:normalize_feedback_event

    function void push_feedback_event(input memblock_wb_event_t wb_event);
        memblock_wb_event_t normalized_event;

        if (!normalize_feedback_event(wb_event, normalized_event)) begin
            return;
        end
        exception_event_q.push_back(normalized_event);
    endfunction:push_feedback_event

    function bit pop_feedback_event(output memblock_wb_event_t wb_event);
        if (exception_event_q.size() == 0) begin
            wb_event = make_empty_wb_event();
            return 1'b0;
        end
        wb_event = exception_event_q.pop_front();
        return 1'b1;
    endfunction:pop_feedback_event

    function void clear_feedback_events();
        exception_event_q.delete();
    endfunction:clear_feedback_events

    function bit resolve_uid_for_event(input memblock_wb_event_t wb_event,
                                       output memblock_uid_t uid);
        memblock_uid_t rob_uid;
        memblock_uid_t lq_uid;
        memblock_uid_t sq_uid;
        bit            have_uid;

        uid = 0;
        have_uid = 1'b0;
        if (wb_event.has_uid) begin
            check_uid(wb_event.uid, "resolve_uid_for_event");
            if (status_by_uid[wb_event.uid] == null || !status_by_uid[wb_event.uid].active) begin
                return 1'b0;
            end
            uid = wb_event.uid;
            have_uid = 1'b1;
        end
        if (wb_event.has_rob) begin
            if (!lookup_active_uid_by_rob(wb_event.rob_key, rob_uid)) begin
                return 1'b0;
            end
            if (have_uid && uid != rob_uid) begin
                `uvm_fatal("COMMON_DATA", $sformatf("WB_UID_MISMATCH uid=%0d rob_uid=%0d", uid, rob_uid))
            end
            uid = rob_uid;
            have_uid = 1'b1;
        end
        if (wb_event.has_lq) begin
            if (!lookup_active_uid_by_lq(wb_event.lq_key, lq_uid)) begin
                return 1'b0;
            end
            if (have_uid && uid != lq_uid) begin
                `uvm_fatal("COMMON_DATA", $sformatf("WB_UID_MISMATCH uid=%0d lq_uid=%0d", uid, lq_uid))
            end
            uid = lq_uid;
            have_uid = 1'b1;
        end
        if (wb_event.has_sq) begin
            if (!lookup_active_uid_by_sq(wb_event.sq_key, sq_uid)) begin
                return 1'b0;
            end
            if (have_uid && uid != sq_uid) begin
                `uvm_fatal("COMMON_DATA", $sformatf("WB_UID_MISMATCH uid=%0d sq_uid=%0d", uid, sq_uid))
            end
            uid = sq_uid;
            have_uid = 1'b1;
        end
        return have_uid;
    endfunction:resolve_uid_for_event

    function int unsigned get_event_issue_epoch(input memblock_wb_event_t wb_event,
                                                input memblock_uid_t uid);
        status_transaction status;

        if (wb_event.has_issue_epoch) begin
            return wb_event.issue_epoch;
        end
        status = get_status(uid);
        return status.get_target_issue_epoch(wb_event.target);
    endfunction:get_event_issue_epoch

    function int unsigned get_event_replay_seq(input memblock_wb_event_t wb_event,
                                               input memblock_uid_t uid);
        if (wb_event.has_replay_seq) begin
            return wb_event.replay_seq;
        end
        return get_status(uid).replay_seq;
    endfunction:get_event_replay_seq

    function void activate_uid(input memblock_uid_t uid,
                               input bit map_lq = 1'b0,
                               input bit map_sq = 1'b0);
        status_transaction       status;
        main_control_transaction main_tr;
        memblock_rob_key_t       rob_key;
        memblock_lq_key_t        lq_key;
        memblock_sq_key_t        sq_key;
        memblock_rob_map_key_t   rob_map_key;
        memblock_lq_map_key_t    lq_map_key;
        memblock_sq_map_key_t    sq_map_key;

        main_tr = get_main_transaction(uid);
        ensure_status_exists(uid, "activate_uid");
        status = status_by_uid[uid];
        if (status.terminal_done) begin
            `uvm_fatal("COMMON_DATA", $sformatf("activate_uid got terminal_done uid=%0d", uid))
        end
        if (status.active) begin
            `uvm_fatal("COMMON_DATA", $sformatf("activate_uid got already active uid=%0d", uid))
        end
        status.snapshot_from_main(main_tr);

        rob_key = main_tr.get_rob_key();
        rob_map_key = rob_order_util::rob_to_map_key(rob_key);
        if (uid_by_active_rob.exists(rob_map_key)) begin
            `uvm_fatal("COMMON_DATA", $sformatf("robIdx already active: uid=%0d existing_uid=%0d", uid, uid_by_active_rob[rob_map_key]))
        end

        if (map_lq) begin
            lq_key = main_tr.get_lq_key();
            if (!is_valid_lq_key(lq_key)) begin
                `uvm_fatal("COMMON_DATA", $sformatf("activate_uid uid=%0d got invalid lqIdx flag=%0d value=%0d", uid, lq_key.flag, lq_key.value))
            end
            lq_map_key = rob_order_util::lq_to_map_key(lq_key);
            if (uid_by_lq.exists(lq_map_key)) begin
                `uvm_fatal("COMMON_DATA", $sformatf("lqIdx already active: uid=%0d existing_uid=%0d", uid, uid_by_lq[lq_map_key]))
            end
            uid_by_lq[lq_map_key] = uid;
            status.active_lq_mapped = 1'b1;
        end

        if (map_sq) begin
            sq_key = main_tr.get_sq_key();
            if (!is_valid_sq_key(sq_key)) begin
                `uvm_fatal("COMMON_DATA", $sformatf("activate_uid uid=%0d got invalid sqIdx flag=%0d value=%0d", uid, sq_key.flag, sq_key.value))
            end
            sq_map_key = rob_order_util::sq_to_map_key(sq_key);
            if (uid_by_sq.exists(sq_map_key)) begin
                `uvm_fatal("COMMON_DATA", $sformatf("sqIdx already active: uid=%0d existing_uid=%0d", uid, uid_by_sq[sq_map_key]))
            end
            uid_by_sq[sq_map_key] = uid;
            status.active_sq_mapped = 1'b1;
        end

        uid_by_active_rob[rob_map_key] = uid;
        status.active = 1'b1;
    endfunction:activate_uid

    function void activate_uid_by_behavior(input memblock_uid_t uid,
                                           input memblock_op_behavior_t behavior);
        activate_uid(uid, behavior.uses_lq, behavior.uses_sq);
    endfunction:activate_uid_by_behavior

    function bit lookup_active_uid_by_rob(input memblock_rob_key_t rob_key,
                                          output memblock_uid_t uid);
        memblock_rob_map_key_t rob_map_key;

        rob_map_key = rob_order_util::rob_to_map_key(rob_key);
        if (!uid_by_active_rob.exists(rob_map_key)) begin
            return 1'b0;
        end
        uid = uid_by_active_rob[rob_map_key];
        if (!is_valid_uid(uid) || status_by_uid[uid] == null || !status_by_uid[uid].active) begin
            `uvm_fatal("COMMON_DATA", $sformatf("stale active rob map for uid=%0d", uid))
        end
        return 1'b1;
    endfunction:lookup_active_uid_by_rob

    function bit lookup_active_uid_by_lq(input memblock_lq_key_t lq_key,
                                         output memblock_uid_t uid);
        memblock_lq_map_key_t lq_map_key;

        if (!is_valid_lq_key(lq_key)) begin
            return 1'b0;
        end
        lq_map_key = rob_order_util::lq_to_map_key(lq_key);
        if (!uid_by_lq.exists(lq_map_key)) begin
            return 1'b0;
        end
        uid = uid_by_lq[lq_map_key];
        if (!is_valid_uid(uid) || status_by_uid[uid] == null || !status_by_uid[uid].active) begin
            `uvm_fatal("COMMON_DATA", $sformatf("stale active lq map for uid=%0d", uid))
        end
        return 1'b1;
    endfunction:lookup_active_uid_by_lq

    function bit lookup_active_uid_by_sq(input memblock_sq_key_t sq_key,
                                         output memblock_uid_t uid);
        memblock_sq_map_key_t sq_map_key;

        if (!is_valid_sq_key(sq_key)) begin
            return 1'b0;
        end
        sq_map_key = rob_order_util::sq_to_map_key(sq_key);
        if (!uid_by_sq.exists(sq_map_key)) begin
            return 1'b0;
        end
        uid = uid_by_sq[sq_map_key];
        if (!is_valid_uid(uid) || status_by_uid[uid] == null || !status_by_uid[uid].active) begin
            `uvm_fatal("COMMON_DATA", $sformatf("stale active sq map for uid=%0d", uid))
        end
        return 1'b1;
    endfunction:lookup_active_uid_by_sq

    function memblock_uid_t get_active_uid_by_rob(input memblock_rob_key_t rob_key);
        memblock_uid_t uid;

        if (!lookup_active_uid_by_rob(rob_key, uid)) begin
            `uvm_fatal("COMMON_DATA", $sformatf("no active uid for robIdx flag=%0d value=%0d", rob_key.flag, rob_key.value))
        end
        return uid;
    endfunction:get_active_uid_by_rob

    function void retire_active_uid(input memblock_uid_t uid);
        status_transaction     status;
        memblock_rob_key_t     rob_key;
        memblock_lq_key_t      lq_key;
        memblock_sq_key_t      sq_key;
        memblock_rob_map_key_t rob_map_key;
        memblock_lq_map_key_t  lq_map_key;
        memblock_sq_map_key_t  sq_map_key;

        status = get_status(uid);
        if (!status.active) begin
            `uvm_fatal("COMMON_DATA", $sformatf("retire_active_uid got inactive uid=%0d", uid))
        end
        remove_uid_from_issue_queues(uid);

        rob_key = status.get_rob_key();
        rob_map_key = rob_order_util::rob_to_map_key(rob_key);
        if (!uid_by_active_rob.exists(rob_map_key) || uid_by_active_rob[rob_map_key] != uid) begin
            `uvm_fatal("COMMON_DATA", $sformatf("retire_active_uid uid=%0d has inconsistent active rob mapping", uid))
        end
        uid_by_active_rob.delete(rob_map_key);

        lq_key.flag  = status.lqIdx_flag;
        lq_key.value = status.lqIdx_value;
        if (status.active_lq_mapped) begin
            if (!is_valid_lq_key(lq_key)) begin
                `uvm_fatal("COMMON_DATA", $sformatf("retire_active_uid uid=%0d has invalid mapped lqIdx", uid))
            end
            lq_map_key = rob_order_util::lq_to_map_key(lq_key);
            if (!uid_by_lq.exists(lq_map_key) || uid_by_lq[lq_map_key] != uid) begin
                `uvm_fatal("COMMON_DATA", $sformatf("retire_active_uid uid=%0d has inconsistent lq mapping", uid))
            end
            uid_by_lq.delete(lq_map_key);
            status.active_lq_mapped = 1'b0;
        end

        sq_key.flag  = status.sqIdx_flag;
        sq_key.value = status.sqIdx_value;
        if (status.active_sq_mapped) begin
            if (!is_valid_sq_key(sq_key)) begin
                `uvm_fatal("COMMON_DATA", $sformatf("retire_active_uid uid=%0d has invalid mapped sqIdx", uid))
            end
            sq_map_key = rob_order_util::sq_to_map_key(sq_key);
            if (!uid_by_sq.exists(sq_map_key) || uid_by_sq[sq_map_key] != uid) begin
                `uvm_fatal("COMMON_DATA", $sformatf("retire_active_uid uid=%0d has inconsistent sq mapping", uid))
            end
            uid_by_sq.delete(sq_map_key);
            status.active_sq_mapped = 1'b0;
        end

        `uvm_info("COMMON_DATA",
                  $sformatf("retire active uid=%0d success=%0d terminal_done=%0d rob=%0d/%0d lq_mapped_now=%0d sq_mapped_now=%0d",
                            uid,
                            status.success,
                            status.terminal_done,
                            status.robIdx_flag,
                            status.robIdx_value,
                            status.active_lq_mapped,
                            status.active_sq_mapped),
                  UVM_LOW)
        status.active = 1'b0;
    endfunction:retire_active_uid

    function void consume_fault_retire(input memblock_uid_t uid);
        status_transaction status;

        status = get_status(uid);
        if (!status.fault && !status.exception_pending &&
            !status.load_fault && !status.sta_fault && !status.std_fault) begin
            `uvm_fatal("COMMON_DATA", $sformatf("consume_fault_retire called for non-fault uid=%0d", uid))
        end
        status.exception_pending = 1'b0;
        set_status_field(uid, MEMBLOCK_STATUS_SUCCESS, 1'b0);
        set_status_field(uid, MEMBLOCK_STATUS_TERMINAL_DONE, 1'b1);
        `uvm_info("COMMON_DATA",
                  $sformatf("fault retire uid=%0d terminal_done=%0d fault=%0d load/sta/std_fault=%0d/%0d/%0d exception_vec=0x%0h",
                            uid,
                            status.terminal_done,
                            status.fault,
                            status.load_fault,
                            status.sta_fault,
                            status.std_fault,
                            status.exception_vec),
                  UVM_LOW)
        retire_active_uid(uid);
    endfunction:consume_fault_retire

    function void release_uid_lq_mapping(input memblock_uid_t uid);
        status_transaction     status;
        memblock_lq_key_t      lq_key;
        memblock_lq_map_key_t  lq_map_key;

        status = get_status(uid);
        if (!status.active_lq_mapped) begin
            return;
        end
        lq_key.flag  = status.lqIdx_flag;
        lq_key.value = status.lqIdx_value;
        if (!is_valid_lq_key(lq_key)) begin
            `uvm_fatal("COMMON_DATA", $sformatf("release_uid_lq_mapping uid=%0d has invalid mapped lqIdx", uid))
        end
        lq_map_key = rob_order_util::lq_to_map_key(lq_key);
        if (!uid_by_lq.exists(lq_map_key) || uid_by_lq[lq_map_key] != uid) begin
            `uvm_fatal("COMMON_DATA", $sformatf("release_uid_lq_mapping uid=%0d has inconsistent lq mapping", uid))
        end
        uid_by_lq.delete(lq_map_key);
        status.active_lq_mapped = 1'b0;
        status.lsq_deq = !status.active_lq_mapped && !status.active_sq_mapped;
        `uvm_info("COMMON_DATA",
                  $sformatf("release lq mapping uid=%0d lq=%0d/%0d lsq_deq=%0d",
                            uid,
                            lq_key.flag,
                            lq_key.value,
                            status.lsq_deq),
                  UVM_LOW)
    endfunction:release_uid_lq_mapping

    function void release_uid_sq_mapping(input memblock_uid_t uid);
        status_transaction     status;
        memblock_sq_key_t      sq_key;
        memblock_sq_map_key_t  sq_map_key;

        status = get_status(uid);
        if (!status.active_sq_mapped) begin
            return;
        end
        sq_key.flag  = status.sqIdx_flag;
        sq_key.value = status.sqIdx_value;
        if (!is_valid_sq_key(sq_key)) begin
            `uvm_fatal("COMMON_DATA", $sformatf("release_uid_sq_mapping uid=%0d has invalid mapped sqIdx", uid))
        end
        sq_map_key = rob_order_util::sq_to_map_key(sq_key);
        if (!uid_by_sq.exists(sq_map_key) || uid_by_sq[sq_map_key] != uid) begin
            `uvm_fatal("COMMON_DATA", $sformatf("release_uid_sq_mapping uid=%0d has inconsistent sq mapping", uid))
        end
        uid_by_sq.delete(sq_map_key);
        status.active_sq_mapped = 1'b0;
        status.lsq_deq = !status.active_lq_mapped && !status.active_sq_mapped;
        `uvm_info("COMMON_DATA",
                  $sformatf("release sq mapping uid=%0d sq=%0d/%0d lsq_deq=%0d",
                            uid,
                            sq_key.flag,
                            sq_key.value,
                            status.lsq_deq),
                  UVM_LOW)
    endfunction:release_uid_sq_mapping

    function void try_retire_committed_uid(input memblock_uid_t uid);
        status_transaction status;

        status = get_status(uid);
        if (!status.active || !status.rob_commit) begin
            return;
        end
        if (status.active_lq_mapped || status.active_sq_mapped) begin
            return;
        end
        if (active_redirect.valid &&
            rob_order_util::rob_need_flush(status.get_rob_key(), active_redirect)) begin
            prepare_uid_for_redirect_reissue(uid, active_redirect);
            return;
        end
        if (status.replay_pending || status.redirect_pending || status.flushed ||
            status.issue_killed) begin
            return;
        end
        if (status.fault || status.exception_pending ||
            status.load_fault || status.sta_fault || status.std_fault) begin
            consume_fault_retire(uid);
            return;
        end
        if (!status.pass || !required_targets_done(uid)) begin
            return;
        end
        set_status_field(uid, MEMBLOCK_STATUS_SUCCESS, 1'b1);
        set_status_field(uid, MEMBLOCK_STATUS_TERMINAL_DONE, 1'b1);
        `uvm_info("COMMON_DATA",
                  $sformatf("try retire committed uid=%0d success=%0d terminal_done=%0d rob_commit=%0d lsq_deq=%0d",
                            uid,
                            status.success,
                            status.terminal_done,
                            status.rob_commit,
                            status.lsq_deq),
                  UVM_LOW)
        retire_active_uid(uid);
    endfunction:try_retire_committed_uid

    function memblock_tlb_lookup_key_t make_tlb_key_by_req(input bit [37:0] vpn,
                                                           input bit [1:0] s2xlate);
        if (mmu_csr_state == null) begin
            mmu_csr_state = mmu_csr_runtime_state::type_id::create("mmu_csr_state");
            mmu_csr_state.reset();
        end
        return mmu_csr_state.make_lookup_key({26'b0, vpn}, s2xlate);
    endfunction:make_tlb_key_by_req

    function bit has_tlb_entry(input memblock_tlb_lookup_key_t key);
        return tlb_entry_by_key.exists(key) && tlb_entry_by_key[key] != null;
    endfunction:has_tlb_entry

    function memblock_tlb_entry get_tlb_entry(input memblock_tlb_lookup_key_t key);
        if (!has_tlb_entry(key)) begin
            `uvm_fatal("COMMON_DATA", $sformatf("tlb_entry_by_key miss vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d",
                                                key.vpn, key.asid, key.vmid, key.s2xlate))
        end
        return tlb_entry_by_key[key];
    endfunction:get_tlb_entry

    function void insert_tlb_entry(input memblock_tlb_lookup_key_t key,
                                   input memblock_tlb_entry entry);
        if (entry == null) begin
            `uvm_fatal("COMMON_DATA", "insert_tlb_entry got null entry")
        end
        entry.lookup_key = key;
        entry.asid       = key.asid;
        entry.vmid       = key.vmid;
        entry.s2xlate    = key.s2xlate;
        tlb_entry_by_key[key] = entry;
    endfunction:insert_tlb_entry

    function bit get_or_create_tlb_entry_by_req(input bit [37:0] vpn,
                                                input bit [1:0] s2xlate,
                                                output memblock_tlb_lookup_key_t key,
                                                output memblock_tlb_entry entry,
                                                output bit created);
        key = make_tlb_key_by_req(vpn, s2xlate);
        if (has_tlb_entry(key)) begin
            entry = tlb_entry_by_key[key];
            entry.last_hit_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            created = 1'b0;
            return 1'b1;
        end
        entry = build_tlb_entry_for_key(key);
        insert_tlb_entry(key, entry);
        created = 1'b1;
        return 1'b1;
    endfunction:get_or_create_tlb_entry_by_req

    function memblock_sfence_payload_t decode_raw_sfence(input memblock_sync_pkg::dispatch_raw_sfence_t raw);
        memblock_sfence_payload_t payload;

        payload = '{default:'0};
        payload.valid       = raw.valid;
        payload.ignore_addr = raw.rs1;
        payload.ignore_id   = raw.rs2;
        payload.addr        = raw.addr;
        payload.id          = raw.id;
        payload.hv          = raw.hv;
        payload.hg          = raw.hg;
        payload.cycle       = raw.cycle;
        return payload;
    endfunction:decode_raw_sfence

    function bit sfence_vpn_match(input bit [51:0] entry_vpn,
                                  input bit [1:0] entry_level,
                                  input bit [49:0] addr);
        bit [51:0] addr_vpn;

        addr_vpn = {14'b0, addr[49:12]};
        case (entry_level)
            2'd0: return entry_vpn[37:0]  == addr_vpn[37:0];
            2'd1: return entry_vpn[37:9]  == addr_vpn[37:9];
            2'd2: return entry_vpn[37:18] == addr_vpn[37:18];
            default: return entry_vpn[37:27] == addr_vpn[37:27];
        endcase
    endfunction:sfence_vpn_match

    function bit sfence_match_entry(input memblock_sfence_payload_t payload,
                                    input memblock_tlb_lookup_key_t key,
                                    input memblock_tlb_entry entry);
        if (!payload.valid) begin
            return 1'b0;
        end
        if (entry == null) begin
            `uvm_fatal("COMMON_DATA", "sfence_match_entry got null entry")
        end
        if (!payload.ignore_addr && !sfence_vpn_match(key.vpn, entry.level, payload.addr)) begin
            return 1'b0;
        end

        if (payload.hg) begin
            if (!(key.s2xlate == 2'd2 || key.s2xlate == 2'd3)) begin
                return 1'b0;
            end
            if (!payload.ignore_id && key.vmid != payload.id) begin
                return 1'b0;
            end
            return 1'b1;
        end

        if (payload.hv) begin
            if (!(key.s2xlate == 2'd1 || key.s2xlate == 2'd3)) begin
                return 1'b0;
            end
            if (key.s2xlate == 2'd3 &&
                mmu_csr_state != null &&
                key.vmid != mmu_csr_state.hgatp_vmid) begin
                return 1'b0;
            end
            if (!payload.ignore_id) begin
                if (entry.pte_g) begin
                    return 1'b0;
                end
                if (key.asid != payload.id) begin
                    return 1'b0;
                end
            end
            return 1'b1;
        end

        if (key.s2xlate == 2'd2) begin
            return 1'b0;
        end
        if (!payload.ignore_id) begin
            if (entry.pte_g) begin
                return 1'b0;
            end
            if (key.asid != payload.id) begin
                return 1'b0;
            end
        end
        return 1'b1;
    endfunction:sfence_match_entry

    function int unsigned apply_sfence_invalidate(input memblock_sfence_payload_t payload);
        memblock_tlb_lookup_key_t delete_keys[$];

        if (!payload.valid) begin
            return 0;
        end
        foreach (tlb_entry_by_key[key]) begin
            if (sfence_match_entry(payload, key, tlb_entry_by_key[key])) begin
                delete_keys.push_back(key);
            end
        end
        foreach (delete_keys[idx]) begin
            tlb_entry_by_key.delete(delete_keys[idx]);
        end
        if (delete_keys.size() != 0) begin
            `uvm_info("COMMON_DATA",
                      $sformatf("sfence invalidate deleted %0d TLB entries hv=%0d hg=%0d ignore_addr=%0d ignore_id=%0d addr=0x%0h id=0x%0h cycle=%0d",
                                delete_keys.size(),
                                payload.hv,
                                payload.hg,
                                payload.ignore_addr,
                                payload.ignore_id,
                                payload.addr,
                                payload.id,
                                payload.cycle),
                      UVM_LOW)
        end
        return delete_keys.size();
    endfunction:apply_sfence_invalidate

    function int unsigned apply_raw_sfence(input memblock_sync_pkg::dispatch_raw_sfence_t raw);
        return apply_sfence_invalidate(decode_raw_sfence(raw));
    endfunction:apply_raw_sfence

    function memblock_tlb_entry build_tlb_entry_for_key(input memblock_tlb_lookup_key_t key);
        memblock_tlb_entry entry;
        tlb_map_builder    builder;

        builder = tlb_map_builder::type_id::create("tlb_builder_by_key");
        if (builder == null) begin
            `uvm_fatal("COMMON_DATA", "failed to create tlb_map_builder")
        end
        entry = builder.build_tlb_entry_for_req(key.vpn[37:0], key.s2xlate, mmu_csr_state);
        entry.lookup_key = key;
        entry.asid = key.asid;
        entry.vmid = key.vmid;
        entry.s2xlate = key.s2xlate;
        return entry;
    endfunction:build_tlb_entry_for_key

    function void get_mmu_csr_snapshot(output mmu_csr_runtime_state snapshot);
        if (mmu_csr_state == null) begin
            mmu_csr_state = mmu_csr_runtime_state::type_id::create("mmu_csr_state");
            mmu_csr_state.reset();
        end
        snapshot = mmu_csr_runtime_state::type_id::create("mmu_csr_snapshot");
        snapshot.copy_from(mmu_csr_state);
    endfunction:get_mmu_csr_snapshot

    function bit is_hypervisor_tlb_inst(input main_control_transaction main_tr);
        if (main_tr == null) begin
            `uvm_fatal("COMMON_DATA", "is_hypervisor_tlb_inst got null transaction")
        end
        return main_tr.fuOpType[4] &&
               !main_tr.fuOpType[5] &&
               (main_tr.fuOpType[8:7] == 2'b00);
    endfunction:is_hypervisor_tlb_inst

    function void register_uid_tlb_record_on_issue(input memblock_uid_t uid);
        main_control_transaction main_tr;
        mmu_csr_runtime_state    snapshot;
        bit [51:0]               vpn;
        bit [1:0]                s2xlate;
        bit                      is_hypervisor_inst;

        check_uid(uid, "register_uid_tlb_record_on_issue");
        if (uid_tlb_record_by_uid.exists(uid) &&
            uid_tlb_record_by_uid[uid] != null &&
            uid_tlb_record_by_uid[uid].pte_valid) begin
            return;
        end
        main_tr = get_main_transaction(uid);
        get_mmu_csr_snapshot(snapshot);
        vpn = {14'b0, main_tr.vaddr[49:12]};
        is_hypervisor_inst = is_hypervisor_tlb_inst(main_tr);
        s2xlate = snapshot.expected_s2xlate(is_hypervisor_inst);
        update_uid_tlb_record_context(uid, vpn, s2xlate, is_hypervisor_inst, snapshot);
    endfunction:register_uid_tlb_record_on_issue

    function void update_uid_tlb_record_context(input memblock_uid_t uid,
                                                input bit [51:0] vpn,
                                                input bit [1:0] s2xlate,
                                                input bit is_hypervisor_inst,
                                                input mmu_csr_runtime_state csr_snapshot);
        memblock_uid_tlb_record record;

        check_uid(uid, "update_uid_tlb_record_context");
        if (csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "update_uid_tlb_record_context got null csr_snapshot")
        end
        if (!uid_tlb_record_by_uid.exists(uid) || uid_tlb_record_by_uid[uid] == null) begin
            record = memblock_uid_tlb_record::type_id::create($sformatf("uid_tlb_record_%0d", uid));
            uid_tlb_record_by_uid[uid] = record;
        end else begin
            record = uid_tlb_record_by_uid[uid];
        end
        record.init_context(uid, vpn, s2xlate, is_hypervisor_inst, csr_snapshot);
    endfunction:update_uid_tlb_record_context

    function int unsigned update_uid_tlb_records_by_entry(input memblock_tlb_lookup_key_t key,
                                                          input memblock_tlb_entry entry);
        int unsigned match_count;

        if (entry == null) begin
            `uvm_fatal("COMMON_DATA", "update_uid_tlb_records_by_entry got null entry")
        end
        match_count = 0;
        foreach (uid_tlb_record_by_uid[uid]) begin
            memblock_uid_tlb_record record;

            record = uid_tlb_record_by_uid[uid];
            if (record == null || !record.record_valid || record.pte_valid) begin
                continue;
            end
            if (record.vpn == key.vpn &&
                record.s2xlate == key.s2xlate &&
                record.asid == key.asid &&
                record.vmid == key.vmid) begin
                record.copy_entry_fields(entry);
                set_status_field(record.uid, MEMBLOCK_STATUS_TLB_MAPPED, 1'b1);
                match_count++;
            end
        end
        if (match_count == 0) begin
            `uvm_error("COMMON_DATA",
                       $sformatf("no pending uid_tlb_record matches TLB key vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d",
                                 key.vpn, key.asid, key.vmid, key.s2xlate))
        end
        return match_count;
    endfunction:update_uid_tlb_records_by_entry

    function memblock_uid_tlb_record get_uid_tlb_record(input memblock_uid_t uid);
        check_uid(uid, "get_uid_tlb_record");
        if (!uid_tlb_record_by_uid.exists(uid) || uid_tlb_record_by_uid[uid] == null) begin
            `uvm_fatal("COMMON_DATA", $sformatf("uid_tlb_record_by_uid[%0d] is null", uid))
        end
        return uid_tlb_record_by_uid[uid];
    endfunction:get_uid_tlb_record

    function bit tlb_entry_ready_for_uid(input memblock_uid_t uid);
        check_uid(uid, "tlb_entry_ready_for_uid");
        return uid_tlb_record_by_uid.exists(uid) &&
               uid_tlb_record_by_uid[uid] != null &&
               uid_tlb_record_by_uid[uid].record_valid &&
               uid_tlb_record_by_uid[uid].pte_valid;
    endfunction:tlb_entry_ready_for_uid

    function void apply_raw_csr_runtime(input memblock_sync_pkg::dispatch_raw_csr_t raw,
                                        input int unsigned raw_csr_seq);
        if (!raw.valid) begin
            return;
        end
        if (raw_csr_seq == last_applied_raw_csr_seq) begin
            return;
        end
        if (mmu_csr_state == null) begin
            mmu_csr_state = mmu_csr_runtime_state::type_id::create("mmu_csr_state");
            mmu_csr_state.reset();
        end
        mmu_csr_state.update_from_raw_csr(raw);
        last_applied_raw_csr_seq = raw_csr_seq;
    endfunction:apply_raw_csr_runtime

    function void clear_issue_queues();
        load_issue_q.delete();
        sta_issue_q.delete();
        std_issue_q.delete();
    endfunction:clear_issue_queues

    function bit issue_queue_contains(input memblock_issue_target_e target,
                                      input memblock_uid_t uid,
                                      input int unsigned replay_seq);
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: begin
                foreach (load_issue_q[idx]) begin
                    if (load_issue_q[idx].uid == uid && load_issue_q[idx].replay_seq == replay_seq) begin
                        return 1'b1;
                    end
                end
            end
            MEMBLOCK_ISSUE_TARGET_STA: begin
                foreach (sta_issue_q[idx]) begin
                    if (sta_issue_q[idx].uid == uid && sta_issue_q[idx].replay_seq == replay_seq) begin
                        return 1'b1;
                    end
                end
            end
            MEMBLOCK_ISSUE_TARGET_STD: begin
                foreach (std_issue_q[idx]) begin
                    if (std_issue_q[idx].uid == uid && std_issue_q[idx].replay_seq == replay_seq) begin
                        return 1'b1;
                    end
                end
            end
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("issue_queue_contains got unsupported target=%0d", target))
            end
        endcase
        return 1'b0;
    endfunction:issue_queue_contains

    function void delete_issue_queue_entry(input memblock_issue_target_e target,
                                           input memblock_uid_t uid,
                                           input int unsigned replay_seq,
                                           input bit match_replay_seq = 1'b1);
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: begin
                for (int idx = load_issue_q.size(); idx > 0; idx--) begin
                    if (load_issue_q[idx - 1].uid == uid &&
                        (!match_replay_seq || load_issue_q[idx - 1].replay_seq == replay_seq)) begin
                        load_issue_q.delete(idx - 1);
                    end
                end
            end
            MEMBLOCK_ISSUE_TARGET_STA: begin
                for (int idx = sta_issue_q.size(); idx > 0; idx--) begin
                    if (sta_issue_q[idx - 1].uid == uid &&
                        (!match_replay_seq || sta_issue_q[idx - 1].replay_seq == replay_seq)) begin
                        sta_issue_q.delete(idx - 1);
                    end
                end
            end
            MEMBLOCK_ISSUE_TARGET_STD: begin
                for (int idx = std_issue_q.size(); idx > 0; idx--) begin
                    if (std_issue_q[idx - 1].uid == uid &&
                        (!match_replay_seq || std_issue_q[idx - 1].replay_seq == replay_seq)) begin
                        std_issue_q.delete(idx - 1);
                    end
                end
            end
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("delete_issue_queue_entry got unsupported target=%0d", target))
            end
        endcase
    endfunction:delete_issue_queue_entry

    function void remove_uid_from_issue_queues(input memblock_uid_t uid);
        delete_issue_queue_entry(MEMBLOCK_ISSUE_TARGET_LOAD, uid, 0, 1'b0);
        delete_issue_queue_entry(MEMBLOCK_ISSUE_TARGET_STA, uid, 0, 1'b0);
        delete_issue_queue_entry(MEMBLOCK_ISSUE_TARGET_STD, uid, 0, 1'b0);
        if (is_valid_uid(uid) && status_by_uid[uid] != null) begin
            status_by_uid[uid].queued_load = 1'b0;
            status_by_uid[uid].queued_sta  = 1'b0;
            status_by_uid[uid].queued_std  = 1'b0;
        end
    endfunction:remove_uid_from_issue_queues

    function void push_ptw_wait_replay(input memblock_uid_t uid,
                                       input memblock_issue_target_e target,
                                       input int unsigned issue_epoch,
                                       input int unsigned replay_seq,
                                       input longint unsigned start_cycle);
        memblock_ptw_wait_replay_t wait_item;

        check_uid(uid, "push_ptw_wait_replay");
        foreach (ptw_wait_replay_q[idx]) begin
            if (ptw_wait_replay_q[idx].valid &&
                ptw_wait_replay_q[idx].uid == uid &&
                ptw_wait_replay_q[idx].target == target &&
                ptw_wait_replay_q[idx].replay_seq == replay_seq) begin
                return;
            end
        end
        wait_item.valid       = 1'b1;
        wait_item.uid         = uid;
        wait_item.target      = target;
        wait_item.issue_epoch = issue_epoch;
        wait_item.replay_seq  = replay_seq;
        wait_item.start_cycle = start_cycle;
        ptw_wait_replay_q.push_back(wait_item);
    endfunction:push_ptw_wait_replay

    function bit pop_ready_ptw_wait_replay(input int unsigned timeout,
                                           output memblock_ptw_wait_replay_t wait_item,
                                           output bit timed_out);
        wait_item.valid       = 1'b0;
        wait_item.uid         = '0;
        wait_item.target      = MEMBLOCK_ISSUE_TARGET_NONE;
        wait_item.issue_epoch = 0;
        wait_item.replay_seq  = 0;
        wait_item.start_cycle = 0;
        timed_out = 1'b0;
        for (int idx = 0; idx < ptw_wait_replay_q.size(); idx++) begin
            bit ready;
            longint unsigned age;

            if (!ptw_wait_replay_q[idx].valid) begin
                continue;
            end
            ready = tlb_entry_ready_for_uid(ptw_wait_replay_q[idx].uid);
            age = (memblock_sync_pkg::get_dispatch_service_cycle() >= ptw_wait_replay_q[idx].start_cycle) ?
                  (memblock_sync_pkg::get_dispatch_service_cycle() - ptw_wait_replay_q[idx].start_cycle) : 0;
            if (ready || (timeout != 0 && age >= timeout)) begin
                wait_item = ptw_wait_replay_q[idx];
                timed_out = !ready;
                ptw_wait_replay_q.delete(idx);
                return 1'b1;
            end
        end
        return 1'b0;
    endfunction:pop_ready_ptw_wait_replay

    function void release_ptw_wait_replay(input memblock_uid_t uid);
        for (int idx = ptw_wait_replay_q.size(); idx > 0; idx--) begin
            if (ptw_wait_replay_q[idx - 1].uid == uid) begin
                ptw_wait_replay_q.delete(idx - 1);
            end
        end
    endfunction:release_ptw_wait_replay

    function void clear_ptw_wait_replay_by_redirect(input memblock_redirect_payload_t redirect);
        for (int idx = ptw_wait_replay_q.size(); idx > 0; idx--) begin
            status_transaction status;

            if (!is_valid_uid(ptw_wait_replay_q[idx - 1].uid)) begin
                ptw_wait_replay_q.delete(idx - 1);
                continue;
            end
            status = get_status(ptw_wait_replay_q[idx - 1].uid);
            if (!status.active ||
                rob_order_util::rob_need_flush(status.get_rob_key(), redirect)) begin
                ptw_wait_replay_q.delete(idx - 1);
            end
        end
    endfunction:clear_ptw_wait_replay_by_redirect

    function void clear_ptw_wait_replay_queue();
        ptw_wait_replay_q.delete();
    endfunction:clear_ptw_wait_replay_queue

    function void push_flushsb_request(input int unsigned source = 0);
        memblock_flushsb_req_t req;

        req.req_id        = next_flushsb_req_id;
        req.enqueue_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        req.source        = source;
        next_flushsb_req_id++;
        flushsb_req_q.push_back(req);
        `uvm_info("COMMON_DATA",
                  $sformatf("push flushSb request: req_id=%0d source=%0d enqueue_cycle=%0d queue_size=%0d",
                            req.req_id,
                            req.source,
                            req.enqueue_cycle,
                            flushsb_req_q.size()),
                  UVM_LOW)
    endfunction:push_flushsb_request

    function bit has_pending_flushsb_request();
        return flushsb_req_q.size() != 0;
    endfunction:has_pending_flushsb_request

    function bit flushsb_busy();
        return flushsb_waiting_empty;
    endfunction:flushsb_busy

    function bit flushsb_request_pending();
        return has_pending_flushsb_request() ||
               flushsb_busy() ||
               active_flushsb_req_valid;
    endfunction:flushsb_request_pending

    function bit try_pop_flushsb_request(output memblock_flushsb_req_t req);
        req = '{default:'0};
        if (flushsb_busy()) begin
            return 1'b0;
        end
        if (issue_blocked_by_global_flush()) begin
            return 1'b0;
        end
        if (!has_pending_flushsb_request()) begin
            return 1'b0;
        end
        req = flushsb_req_q.pop_front();
        return 1'b1;
    endfunction:try_pop_flushsb_request

    function void mark_flushsb_driven(input memblock_flushsb_req_t req,
                                      input longint unsigned cycle);
        active_flushsb_req       = req;
        active_flushsb_req_valid = 1'b1;
        flushsb_waiting_empty    = 1'b1;
        flushsb_start_cycle      = cycle;
        last_sb_is_empty         = 1'b0;
        flushsb_timeout_warned   = 1'b0;
        memblock_sync_pkg::dispatch_flushsb_waiting_empty = 1'b1;
        `uvm_info("COMMON_DATA",
                  $sformatf("drive flushSb request: req_id=%0d source=%0d enqueue_cycle=%0d start_cycle=%0d queue_size=%0d",
                            req.req_id,
                            req.source,
                            req.enqueue_cycle,
                            cycle,
                            flushsb_req_q.size()),
                  UVM_LOW)
    endfunction:mark_flushsb_driven

    function void update_sb_is_empty(input bit sb_is_empty);
        last_sb_is_empty = sb_is_empty;
        if (flushsb_waiting_empty && sb_is_empty) begin
            `uvm_info("COMMON_DATA",
                      $sformatf("flushSb request completed: req_id=%0d source=%0d start_cycle=%0d done_cycle=%0d",
                                active_flushsb_req.req_id,
                                active_flushsb_req.source,
                                flushsb_start_cycle,
                                memblock_sync_pkg::get_dispatch_service_cycle()),
                      UVM_LOW)
            flushsb_waiting_empty    = 1'b0;
            active_flushsb_req       = '{default:'0};
            active_flushsb_req_valid = 1'b0;
            flushsb_start_cycle      = 0;
            flushsb_timeout_warned   = 1'b0;
            memblock_sync_pkg::dispatch_flushsb_waiting_empty = 1'b0;
        end
    endfunction:update_sb_is_empty

    function void warn_flushsb_timeout_if_needed(input int unsigned timeout);
        longint unsigned age;

        if (!flushsb_waiting_empty || timeout == 0 || flushsb_timeout_warned) begin
            return;
        end
        age = (memblock_sync_pkg::get_dispatch_service_cycle() >= flushsb_start_cycle) ?
              (memblock_sync_pkg::get_dispatch_service_cycle() - flushsb_start_cycle) : 0;
        if (age >= timeout) begin
            `uvm_warning("COMMON_DATA",
                         $sformatf("flushSb request timeout warning: req_id=%0d source=%0d age=%0d timeout=%0d start_cycle=%0d last_sb_is_empty=%0d",
                                   active_flushsb_req.req_id,
                                   active_flushsb_req.source,
                                   age,
                                   timeout,
                                   flushsb_start_cycle,
                                   last_sb_is_empty))
            flushsb_timeout_warned = 1'b1;
        end
    endfunction:warn_flushsb_timeout_if_needed

    function void push_issue_queue_item(input memblock_issue_q_item_t item);
        check_uid(item.uid, "push_issue_queue_item");
        if (item.target == MEMBLOCK_ISSUE_TARGET_NONE) begin
            `uvm_fatal("COMMON_DATA", $sformatf("push_issue_queue_item uid=%0d got target NONE", item.uid))
        end
        if (issue_queue_contains(item.target, item.uid, item.replay_seq)) begin
            return;
        end
        case (item.target)
            MEMBLOCK_ISSUE_TARGET_LOAD: load_issue_q.push_back(item);
            MEMBLOCK_ISSUE_TARGET_STA:  sta_issue_q.push_back(item);
            MEMBLOCK_ISSUE_TARGET_STD:  std_issue_q.push_back(item);
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("push_issue_queue_item got unsupported target=%0d", item.target))
            end
        endcase
    endfunction:push_issue_queue_item

    function int unsigned get_issue_queue_size(input memblock_issue_target_e target);
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: return load_issue_q.size();
            MEMBLOCK_ISSUE_TARGET_STA:  return sta_issue_q.size();
            MEMBLOCK_ISSUE_TARGET_STD:  return std_issue_q.size();
            default: begin
                `uvm_fatal("COMMON_DATA", $sformatf("get_issue_queue_size got unsupported target=%0d", target))
            end
        endcase
        return 0;
    endfunction:get_issue_queue_size

    function void check_main_table_complete();
        int unsigned uid;

        if (main_trans_num == 0) begin
            `uvm_fatal("COMMON_DATA", "check_main_table_complete called before reset_all_tables")
        end
        if (next_uid != main_trans_num) begin
            `uvm_fatal("COMMON_DATA", $sformatf("uid allocation mismatch: next_uid=%0d main_trans_num=%0d", next_uid, main_trans_num))
        end
        for (uid = 0; uid < main_trans_num; uid++) begin
            if (main_table_by_uid[uid] == null) begin
                `uvm_fatal("COMMON_DATA", $sformatf("main_table_by_uid[%0d] is null after main table build", uid))
            end
            if (status_by_uid[uid] == null) begin
                `uvm_fatal("COMMON_DATA", $sformatf("status_by_uid[%0d] is null after main table build", uid))
            end
        end
        main_table_ready = 1'b1;
    endfunction:check_main_table_complete

    function void end_test_check();
        int unsigned uid;

        memblock_sync_pkg::dispatch_monitor_capture_en = 1'b0;
        if (memblock_sync_pkg::raw_monitor_queue_size() != 0) begin
            `uvm_error("COMMON_DATA",
                         $sformatf("clear %0d raw monitor events at end_test_check",
                                   memblock_sync_pkg::raw_monitor_queue_size()))
            memblock_sync_pkg::clear_raw_monitor_queues();
        end
        if (main_trans_num == 0) begin
            return;
        end
        if (next_uid != main_trans_num) begin
            `uvm_error("COMMON_DATA", $sformatf("uid allocation mismatch: next_uid=%0d main_trans_num=%0d", next_uid, main_trans_num))
        end
        for (uid = 0; uid < main_trans_num; uid++) begin
            if (status_by_uid[uid] == null) begin
                `uvm_fatal("COMMON_DATA", $sformatf("status_by_uid[%0d] is null at end_test_check", uid))
            end
            if (!status_by_uid[uid].terminal_done) begin
                `uvm_error("COMMON_DATA", $sformatf("uid=%0d is not terminal_done at end_test_check", uid))
            end
            if (status_by_uid[uid].active ||
                status_by_uid[uid].exception_pending ||
                status_by_uid[uid].replay_pending ||
                status_by_uid[uid].redirect_pending) begin
                `uvm_error("COMMON_DATA", $sformatf("uid=%0d has unfinished status at end_test_check", uid))
            end
            if (status_by_uid[uid].terminal_done &&
                (status_by_uid[uid].flushed ||
                 status_by_uid[uid].issue_killed)) begin
                `uvm_error("COMMON_DATA", $sformatf("uid=%0d has terminal_done with stale intermediate state", uid))
            end
        end
        if (uid_by_active_rob.num() != 0 || uid_by_lq.num() != 0 || uid_by_sq.num() != 0) begin
            `uvm_error("COMMON_DATA", "active ROB/LQ/SQ mapping is not empty at end_test_check")
        end
        if (load_issue_q.size() != 0 || sta_issue_q.size() != 0 || std_issue_q.size() != 0) begin
            `uvm_error("COMMON_DATA", "issue queues are not empty at end_test_check")
        end
        if (flush_in_progress || active_redirect.valid || issue_freeze_ack) begin
            `uvm_error("COMMON_DATA", "global flush/redirect control state is not idle at end_test_check")
        end
        if (has_pending_redirect_drive() || redirect_phase != MEMBLOCK_REDIRECT_PHASE_IDLE) begin
            `uvm_error("COMMON_DATA", "redirect drive queue/state is not idle at end_test_check")
        end
        if (flushsb_request_pending()) begin
            `uvm_error("COMMON_DATA", "flushSb state is not idle at end_test_check")
        end
        if (ptw_wait_replay_q.size() != 0) begin
            `uvm_error("COMMON_DATA", "ptw_wait_replay queue is not empty at end_test_check")
        end
    endfunction:end_test_check

endclass:common_data_transaction

`endif
