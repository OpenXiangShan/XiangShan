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
    // 中文注释：cancel record 的唯一递增 ID；epoch 用于 DUT/recovery 语义，ID 用于
    // active redirect 与 delayed snapshot 绑定，避免重复 payload 误合并。
    int unsigned               next_cancel_record_id;
    bit                         active_cancel_record_id_valid;
    int unsigned               active_cancel_record_id;
    // 中文注释：最近已由主 service drain 的 monitor sample watermark；negedge readiness
    // 只读该值，不能用递增型 sample getter 伪造一个更晚的 DUT sample。
    longint unsigned            latest_drained_cancel_sample_seq;
    bit                         cancel_held_baseline_valid;
    bit [`MEMBLOCK_LQ_CANCEL_COUNT_W-1:0] cancel_held_lq_count;
    bit [`MEMBLOCK_SQ_CANCEL_COUNT_W-1:0] cancel_held_sq_count;
    // 中文注释：仅用于真实 directed cancel flow 的诊断计数，不参与状态/退出判断。
    int unsigned               cancel_reconcile_match_count;
    int unsigned               cancel_reconcile_lq_nonzero_match_count;
    int unsigned               cancel_reconcile_sq_nonzero_match_count;
    memblock_lsq_cancel_record_t cancel_record_q[$];
    memblock_sync_pkg::dispatch_raw_cancel_snapshot_t cancel_snapshot_history_q[$];
    memblock_sync_pkg::dispatch_raw_redirect_anchor_t redirect_anchor_history_q[$];

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
        next_cancel_record_id = 0;
        active_cancel_record_id_valid = 1'b0;
        active_cancel_record_id = 0;
        latest_drained_cancel_sample_seq = 0;
        cancel_held_baseline_valid = 1'b0;
        cancel_held_lq_count = '0;
        cancel_held_sq_count = '0;
        cancel_reconcile_match_count = 0;
        cancel_reconcile_lq_nonzero_match_count = 0;
        cancel_reconcile_sq_nonzero_match_count = 0;
        cancel_record_q.delete();
        cancel_snapshot_history_q.delete();
        redirect_anchor_history_q.delete();
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
        // The shared L2TLB sample coordinator is initialized by top_tb at
        // time 0. This table reset only clears testcase-owned software state;
        // it must not rewind a sample already published by CSR monitor.
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
        next_cancel_record_id = 0;
        active_cancel_record_id_valid = 1'b0;
        active_cancel_record_id = 0;
        latest_drained_cancel_sample_seq = 0;
        cancel_held_baseline_valid = 1'b0;
        cancel_held_lq_count = '0;
        cancel_held_sq_count = '0;
        cancel_reconcile_match_count = 0;
        cancel_reconcile_lq_nonzero_match_count = 0;
        cancel_reconcile_sq_nonzero_match_count = 0;
        cancel_record_q.delete();
        cancel_snapshot_history_q.delete();
        redirect_anchor_history_q.delete();
        main_table_by_uid = new[main_trans_num_i];
        status_by_uid     = new[main_trans_num_i];
        cancel_waiting_uid_tlb_records("reset_all_tables");
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

    // 中文注释：canonical MMIO tag 的唯一清理入口。redirect/reissue 在
    // dynamic_epoch 递增前调用；普通 terminal retire 不复用该动态实例。
    function void clear_uid_mmio_tag(input memblock_uid_t uid);
        status_transaction status;

        status = get_status(uid);
        status.mmio_tag_valid = 1'b0;
        status.is_mmio_load = 1'b0;
        status.is_mmio_store = 1'b0;
        status.mmio_tag_source = MEMBLOCK_MMIO_TAG_NONE;
        status.mmio_tag_dynamic_epoch = 0;
    endfunction:clear_uid_mmio_tag

    // 中文注释：预检和提交共用同一入口。apply_update=0 只验证 active provenance、
    // op kind、dynamic epoch 和既有 tag 冲突；全 raw 预检成功后才允许置 1 提交。
    function void set_uid_mmio_tag(input memblock_uid_t uid,
                                   input memblock_mmio_kind_e kind,
                                   input memblock_mmio_tag_source_e source,
                                   input bit apply_update = 1'b1);
        status_transaction       status;
        memblock_op_behavior_t  behavior;
        memblock_mmio_tag_source_e next_source;

        status = get_status(uid);
        if (kind != MEMBLOCK_MMIO_KIND_LOAD && kind != MEMBLOCK_MMIO_KIND_STORE) begin
            `uvm_fatal("MMIO_TAG", $sformatf("uid=%0d got invalid MMIO kind=%0d", uid, kind))
        end
        if (source != MEMBLOCK_MMIO_TAG_DIRECTED &&
            source != MEMBLOCK_MMIO_TAG_MONITOR) begin
            `uvm_fatal("MMIO_TAG", $sformatf("uid=%0d got invalid MMIO source=%0d", uid, source))
        end
        if (!status.active || status.terminal_done || status.flushed ||
            status.issue_killed || status.redirect_pending) begin
            `uvm_fatal("MMIO_TAG",
                       $sformatf("uid=%0d MMIO tag requires current active instance: active=%0d terminal=%0d flushed=%0d killed=%0d redirect=%0d",
                                 uid, status.active, status.terminal_done, status.flushed,
                                 status.issue_killed, status.redirect_pending))
        end
        if (!status.active_instance_flush_epoch_valid) begin
            `uvm_fatal("MMIO_TAG", $sformatf("uid=%0d has no activation flush-epoch provenance", uid))
        end

        behavior = memblock_op_behavior_util::derive_op_behavior(get_main_transaction(uid));
        if (kind == MEMBLOCK_MMIO_KIND_LOAD &&
            (behavior.kind != MEMBLOCK_OP_BEHAVIOR_LOAD || !behavior.commit_is_load)) begin
            `uvm_fatal("MMIO_TAG",
                       $sformatf("uid=%0d cannot receive LOAD MMIO tag for behavior=%0d",
                                 uid, behavior.kind))
        end
        if (kind == MEMBLOCK_MMIO_KIND_STORE &&
            (behavior.kind != MEMBLOCK_OP_BEHAVIOR_STORE || !behavior.commit_is_store)) begin
            `uvm_fatal("MMIO_TAG",
                       $sformatf("uid=%0d cannot receive STORE MMIO tag for behavior=%0d",
                                 uid, behavior.kind))
        end

        next_source = source;
        if (status.mmio_tag_valid) begin
            if (status.mmio_tag_dynamic_epoch != status.dynamic_epoch) begin
                `uvm_fatal("MMIO_TAG",
                           $sformatf("uid=%0d carries stale MMIO tag epoch=%0d current=%0d",
                                     uid, status.mmio_tag_dynamic_epoch, status.dynamic_epoch))
            end
            if (status.is_mmio_load == status.is_mmio_store) begin
                `uvm_fatal("MMIO_TAG",
                           $sformatf("uid=%0d canonical tag has invalid load/store bits=%0d/%0d",
                                     uid, status.is_mmio_load, status.is_mmio_store))
            end
            if ((kind == MEMBLOCK_MMIO_KIND_LOAD && !status.is_mmio_load) ||
                (kind == MEMBLOCK_MMIO_KIND_STORE && !status.is_mmio_store)) begin
                `uvm_fatal("MMIO_TAG",
                           $sformatf("uid=%0d MMIO kind conflict existing load/store=%0d/%0d incoming=%0d",
                                     uid, status.is_mmio_load, status.is_mmio_store, kind))
            end
            if (status.mmio_tag_source != MEMBLOCK_MMIO_TAG_DIRECTED &&
                status.mmio_tag_source != MEMBLOCK_MMIO_TAG_MONITOR) begin
                `uvm_fatal("MMIO_TAG",
                           $sformatf("uid=%0d existing MMIO source=%0d is invalid",
                                     uid, status.mmio_tag_source))
            end
            // monitor 是真实 DUT fact，允许覆盖同 kind directed 来源；反向调用不降级。
            if (status.mmio_tag_source == MEMBLOCK_MMIO_TAG_MONITOR ||
                source == MEMBLOCK_MMIO_TAG_MONITOR) begin
                next_source = MEMBLOCK_MMIO_TAG_MONITOR;
            end else begin
                next_source = MEMBLOCK_MMIO_TAG_DIRECTED;
            end
        end

        if (apply_update) begin
            status.mmio_tag_valid = 1'b1;
            status.is_mmio_load = kind == MEMBLOCK_MMIO_KIND_LOAD;
            status.is_mmio_store = kind == MEMBLOCK_MMIO_KIND_STORE;
            status.mmio_tag_source = next_source;
            status.mmio_tag_dynamic_epoch = status.dynamic_epoch;
        end
    endfunction:set_uid_mmio_tag

    function bit uid_is_mmio_load(input memblock_uid_t uid);
        status_transaction status;

        status = get_status(uid);
        if (!status.mmio_tag_valid) begin
            return 1'b0;
        end
        if (status.is_mmio_load == status.is_mmio_store) begin
            `uvm_fatal("MMIO_TAG", $sformatf("uid=%0d has non-canonical load/store tag", uid))
        end
        if (status.mmio_tag_dynamic_epoch != status.dynamic_epoch) begin
            `uvm_fatal("MMIO_TAG",
                       $sformatf("uid=%0d query observed stale tag epoch=%0d current=%0d",
                                 uid, status.mmio_tag_dynamic_epoch, status.dynamic_epoch))
        end
        if (!status.active) begin
            return 1'b0;
        end
        if (!status.active_instance_flush_epoch_valid) begin
            `uvm_fatal("MMIO_TAG", $sformatf("uid=%0d active MMIO query lacks activation provenance", uid))
        end
        return status.is_mmio_load && !status.is_mmio_store;
    endfunction:uid_is_mmio_load

    function bit uid_is_mmio_store(input memblock_uid_t uid);
        status_transaction status;

        status = get_status(uid);
        if (!status.mmio_tag_valid) begin
            return 1'b0;
        end
        if (status.is_mmio_load == status.is_mmio_store) begin
            `uvm_fatal("MMIO_TAG", $sformatf("uid=%0d has non-canonical load/store tag", uid))
        end
        if (status.mmio_tag_dynamic_epoch != status.dynamic_epoch) begin
            `uvm_fatal("MMIO_TAG",
                       $sformatf("uid=%0d query observed stale tag epoch=%0d current=%0d",
                                 uid, status.mmio_tag_dynamic_epoch, status.dynamic_epoch))
        end
        if (!status.active) begin
            return 1'b0;
        end
        if (!status.active_instance_flush_epoch_valid) begin
            `uvm_fatal("MMIO_TAG", $sformatf("uid=%0d active MMIO query lacks activation provenance", uid))
        end
        return status.is_mmio_store && !status.is_mmio_load;
    endfunction:uid_is_mmio_store

    // 中文注释：只 probe 同一 ROB value 的两个完整 wrap key，不扫描主表。
    // 返回 CURRENT 时 uid 唯一且 op/dispatch/provenance 已验证；只有可证明旧 raw
    // 时返回 STALE_DROP，其余 value-only 歧义直接 fatal。LOAD 还要检查
    // LoadQueueUncache s1 后一拍脉冲是否与未完成 redirect 的 sample anchor 重叠。
    function memblock_mmio_resolve_result_e resolve_mmio_uid_by_rob_value(
        input bit [MEMBLOCK_ROB_VALUE_W-1:0] rob_value,
        input memblock_mmio_kind_e expected_kind,
        input int unsigned raw_sample_flush_epoch,
        input longint unsigned raw_sample_seq,
        output memblock_uid_t uid,
        output string stale_reason
    );
        int unsigned current_candidate_count;
        int unsigned active_candidate_count;
        int unsigned newer_candidate_count;
        bit load_overlap_observed;
        int unsigned overlap_redirect_match_count;
        memblock_redirect_payload_t overlap_redirect;
        int unsigned overlap_redirect_epoch;
        longint unsigned overlap_redirect_sample_seq;
        int unsigned overlap_old_covered_count;
        int unsigned overlap_new_candidate_count;
        int unsigned overlap_uncovered_count;
        int unsigned overlap_incompatible_count;
        memblock_rob_key_t overlap_old_key;

        uid = 0;
        stale_reason = "";
        current_candidate_count = 0;
        active_candidate_count = 0;
        newer_candidate_count = 0;
        load_overlap_observed = 1'b0;
        overlap_redirect_match_count = 0;
        overlap_redirect = '{default:'0};
        overlap_redirect_epoch = 0;
        overlap_redirect_sample_seq = 0;
        overlap_old_covered_count = 0;
        overlap_new_candidate_count = 0;
        overlap_uncovered_count = 0;
        overlap_incompatible_count = 0;
        overlap_old_key = '{default:'0};
        if (expected_kind != MEMBLOCK_MMIO_KIND_LOAD &&
            expected_kind != MEMBLOCK_MMIO_KIND_STORE) begin
            `uvm_fatal("MMIO_RESOLVE",
                       $sformatf("ROB value=%0d got invalid expected kind=%0d",
                                 rob_value, expected_kind))
        end
        if (raw_sample_flush_epoch > memblock_sync_pkg::dispatch_flush_epoch) begin
            `uvm_fatal("MMIO_RESOLVE",
                       $sformatf("future raw epoch=%0d current=%0d ROB value=%0d",
                                 raw_sample_flush_epoch,
                                 memblock_sync_pkg::dispatch_flush_epoch,
                                 rob_value))
        end
        if (raw_sample_seq == 0) begin
            `uvm_fatal("MMIO_RESOLVE",
                       $sformatf("ROB value=%0d kind=%0d has no MMIO sample provenance",
                                 rob_value, expected_kind))
            return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
        end
        if (raw_sample_seq > memblock_sync_pkg::peek_current_dut_global_sample()) begin
            `uvm_fatal("MMIO_RESOLVE",
                       $sformatf("future MMIO sample sequence=%0d latest=%0d ROB value=%0d",
                                 raw_sample_seq,
                                 memblock_sync_pkg::peek_current_dut_global_sample(),
                                 rob_value))
            return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
        end

        // 中文注释：扫描全部未完成 redirect provenance，而不是只看当前 observation
        // epoch。已绑定 record 直接提供完整 redirect；未绑定 anchor 按 FIFO 序号与
        // 未绑定 record 配对。队列深度受 MEMBLOCK_CANCEL_RECORD_MAX_DEPTH 限制。
        if (expected_kind == MEMBLOCK_MMIO_KIND_LOAD) begin
            foreach (cancel_record_q[record_probe_idx]) begin
                if (!cancel_record_q[record_probe_idx].valid ||
                    !cancel_record_q[record_probe_idx].redirect_anchor_valid ||
                    (raw_sample_seq != cancel_record_q[record_probe_idx].redirect_sample_seq &&
                     raw_sample_seq != cancel_record_q[record_probe_idx].redirect_sample_seq + 1)) begin
                    continue;
                end
                load_overlap_observed = 1'b1;
                overlap_redirect_match_count++;
                if (overlap_redirect_match_count > 1) begin
                    `uvm_fatal("MMIO_RESOLVE",
                               $sformatf("LOAD MMIO sample=%0d overlaps multiple unfinished redirect records",
                                         raw_sample_seq))
                    return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
                end
                overlap_redirect = cancel_record_q[record_probe_idx].redirect;
                overlap_redirect_epoch = cancel_record_q[record_probe_idx].redirect_epoch;
                overlap_redirect_sample_seq = cancel_record_q[record_probe_idx].redirect_sample_seq;
            end

            foreach (redirect_anchor_history_q[anchor_idx]) begin
                int record_idx_for_anchor;
                int unsigned unanchored_seen;

                if (!redirect_anchor_history_q[anchor_idx].valid ||
                    redirect_anchor_history_q[anchor_idx].sample_seq == 0 ||
                    (raw_sample_seq != redirect_anchor_history_q[anchor_idx].sample_seq &&
                     raw_sample_seq != redirect_anchor_history_q[anchor_idx].sample_seq + 1)) begin
                    continue;
                end
                load_overlap_observed = 1'b1;
                overlap_redirect_match_count++;
                if (overlap_redirect_match_count > 1) begin
                    `uvm_fatal("MMIO_RESOLVE",
                               $sformatf("LOAD MMIO sample=%0d overlaps multiple redirect anchors/records",
                                         raw_sample_seq))
                    return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
                end

                record_idx_for_anchor = -1;
                unanchored_seen = 0;
                foreach (cancel_record_q[record_probe_idx2]) begin
                    if (cancel_record_q[record_probe_idx2].valid &&
                        !cancel_record_q[record_probe_idx2].redirect_anchor_valid) begin
                        if (unanchored_seen == anchor_idx) begin
                            record_idx_for_anchor = record_probe_idx2;
                            break;
                        end
                        unanchored_seen++;
                    end
                end
                if (record_idx_for_anchor < 0) begin
                    `uvm_fatal("MMIO_RESOLVE",
                               $sformatf("LOAD MMIO sample=%0d has anchor without unfinished redirect record",
                                         raw_sample_seq))
                    return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
                end
                if (redirect_anchor_history_q[anchor_idx].level !=
                        cancel_record_q[record_idx_for_anchor].redirect.level ||
                    redirect_anchor_history_q[anchor_idx].rob_flag !=
                        cancel_record_q[record_idx_for_anchor].redirect.rob_key.flag ||
                    redirect_anchor_history_q[anchor_idx].rob_value !=
                        cancel_record_q[record_idx_for_anchor].redirect.rob_key.value) begin
                    `uvm_fatal("MMIO_RESOLVE",
                               $sformatf("LOAD MMIO anchor FIFO mismatch sample=%0d record=%0d",
                                         raw_sample_seq,
                                         cancel_record_q[record_idx_for_anchor].cancel_record_id))
                    return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
                end
                overlap_redirect = cancel_record_q[record_idx_for_anchor].redirect;
                overlap_redirect_epoch = cancel_record_q[record_idx_for_anchor].redirect_epoch;
                overlap_redirect_sample_seq = redirect_anchor_history_q[anchor_idx].sample_seq;
            end

            if (load_overlap_observed && overlap_redirect_epoch == 0) begin
                `uvm_fatal("MMIO_RESOLVE",
                           $sformatf("LOAD MMIO sample=%0d overlap has invalid redirect epoch",
                                     raw_sample_seq))
                return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
            end
        end

        for (int unsigned flag_idx = 0; flag_idx < 2; flag_idx++) begin
            memblock_rob_key_t key;
            memblock_uid_t candidate_uid;
            status_transaction status;
            memblock_op_behavior_t behavior;

            key.flag = flag_idx[0];
            key.value = rob_value;
            if (!lookup_active_uid_by_rob(key, candidate_uid)) begin
                continue;
            end
            active_candidate_count++;
            status = get_status(candidate_uid);
            if (!status.active_instance_flush_epoch_valid) begin
                `uvm_fatal("MMIO_RESOLVE",
                           $sformatf("active uid=%0d ROB=%0d/%0d lacks activation provenance",
                                     candidate_uid, key.flag, key.value))
            end
            if (status.active_instance_flush_epoch >
                memblock_sync_pkg::dispatch_flush_epoch) begin
                `uvm_fatal("MMIO_RESOLVE",
                           $sformatf("uid=%0d activation epoch=%0d is newer than current=%0d",
                                     candidate_uid, status.active_instance_flush_epoch,
                                     memblock_sync_pkg::dispatch_flush_epoch))
            end
            if (load_overlap_observed) begin
                // overlap stale-drop 仍必须先证明命中的是已 dispatch scalar load；
                // 旧 store、prefetch或尚未 dispatch 的 load 都属于无法证明，不得静默 drop。
                behavior = memblock_op_behavior_util::derive_op_behavior(
                    get_main_transaction(candidate_uid));
                if (behavior.kind != MEMBLOCK_OP_BEHAVIOR_LOAD ||
                    !behavior.commit_is_load || !status.load_dispatched) begin
                    overlap_incompatible_count++;
                    continue;
                end
                // redirect epoch 之前建立且完整 ROB key 被覆盖的兼容 load owner 才能证明为
                // 被杀旧请求；同 epoch或更晚的 owner 属于新动态实例。
                if (rob_order_util::rob_need_flush(key, overlap_redirect) &&
                    status.active_instance_flush_epoch < overlap_redirect_epoch) begin
                    overlap_old_covered_count++;
                    overlap_old_key = key;
                end else if (status.active_instance_flush_epoch >= overlap_redirect_epoch) begin
                    overlap_new_candidate_count++;
                end else begin
                    overlap_uncovered_count++;
                end
                continue;
            end
            if (raw_sample_flush_epoch < status.active_instance_flush_epoch) begin
                newer_candidate_count++;
                continue;
            end

            behavior = memblock_op_behavior_util::derive_op_behavior(
                get_main_transaction(candidate_uid));
            if (expected_kind == MEMBLOCK_MMIO_KIND_LOAD) begin
                if (behavior.kind != MEMBLOCK_OP_BEHAVIOR_LOAD ||
                    !behavior.commit_is_load || !status.load_dispatched) begin
                    `uvm_fatal("MMIO_RESOLVE",
                               $sformatf("LOAD raw ROB=%0d/%0d maps to incompatible uid=%0d behavior=%0d load_dispatched=%0d",
                                         key.flag, key.value, candidate_uid,
                                         behavior.kind, status.load_dispatched))
                end
            end else begin
                if (behavior.kind != MEMBLOCK_OP_BEHAVIOR_STORE ||
                    !behavior.commit_is_store ||
                    !status.sta_dispatched || !status.std_dispatched) begin
                    `uvm_fatal("MMIO_RESOLVE",
                               $sformatf("STORE raw ROB=%0d/%0d maps to incompatible uid=%0d behavior=%0d sta/std_dispatched=%0d/%0d",
                                         key.flag, key.value, candidate_uid,
                                         behavior.kind, status.sta_dispatched,
                                         status.std_dispatched))
                end
            end
            current_candidate_count++;
            uid = candidate_uid;
        end

        if (load_overlap_observed) begin
            if (active_candidate_count == 1 &&
                overlap_old_covered_count == 1 &&
                overlap_new_candidate_count == 0 &&
                overlap_uncovered_count == 0 &&
                overlap_incompatible_count == 0) begin
                stale_reason = $sformatf(
                    "loadMmio sample=%0d overlaps redirect sample=%0d and old active ROB=%0d/%0d is covered",
                    raw_sample_seq, overlap_redirect_sample_seq,
                    overlap_old_key.flag, overlap_old_key.value);
                return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
            end
            stale_reason = $sformatf(
                "cannot prove LOAD MMIO stale ownership sample=%0d redirect_sample=%0d active=%0d old_covered=%0d new=%0d uncovered=%0d incompatible=%0d",
                raw_sample_seq, overlap_redirect_sample_seq,
                active_candidate_count, overlap_old_covered_count,
                overlap_new_candidate_count, overlap_uncovered_count,
                overlap_incompatible_count);
            `uvm_fatal("MMIO_RESOLVE", stale_reason)
            return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
        end

        if (current_candidate_count > 1) begin
            `uvm_fatal("MMIO_RESOLVE",
                       $sformatf("ROB value=%0d kind=%0d has multiple current active candidates",
                                 rob_value, expected_kind))
        end
        if (current_candidate_count == 1) begin
            return MEMBLOCK_MMIO_RESOLVE_CURRENT;
        end
        if (raw_sample_flush_epoch < memblock_sync_pkg::dispatch_flush_epoch &&
            (active_candidate_count == 0 ||
             newer_candidate_count == active_candidate_count)) begin
            stale_reason = active_candidate_count == 0 ?
                "old raw has no active ROB-key owner" :
                "old raw predates every active ROB-key instance";
            return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
        end

        `uvm_fatal("MMIO_RESOLVE",
                   $sformatf("cannot prove MMIO raw ownership ROB value=%0d kind=%0d raw_epoch=%0d current_epoch=%0d active=%0d newer=%0d",
                             rob_value, expected_kind, raw_sample_flush_epoch,
                             memblock_sync_pkg::dispatch_flush_epoch,
                             active_candidate_count, newer_candidate_count))
        return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
    endfunction:resolve_mmio_uid_by_rob_value

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

    function bit cancel_reconcile_pending();
        return cancel_record_q.size() != 0;
    endfunction:cancel_reconcile_pending

    function bit redirect_sample_anchor_pending();
        return redirect_anchor_history_q.size() != 0 ||
               active_cancel_record_id_valid;
    endfunction:redirect_sample_anchor_pending

    function bit cancel_snapshot_buffer_pending();
        return cancel_snapshot_history_q.size() != 0;
    endfunction:cancel_snapshot_buffer_pending

    // 中文注释：主动 flow 的统一运行期 drain 判定。该函数只读取 queue size、
    // associative-map count 和控制位，不扫描 main/status 主表；所有异步 producer、
    // recovery、LSQ cancel 与 flushSb 生命周期收敛后才返回 1。
    function bit runtime_drain_complete();
        return memblock_sync_pkg::raw_monitor_queue_size() == 0 &&
               exception_event_q.size() == 0 &&
               load_issue_q.size() == 0 &&
               sta_issue_q.size() == 0 &&
               std_issue_q.size() == 0 &&
               uid_by_active_rob.num() == 0 &&
               uid_by_lq.num() == 0 &&
               uid_by_sq.num() == 0 &&
               !has_pending_redirect_drive() &&
               !flush_in_progress &&
               !active_redirect.valid &&
               redirect_phase == MEMBLOCK_REDIRECT_PHASE_IDLE &&
               !issue_freeze_ack &&
               !memblock_sync_pkg::dispatch_flush_in_progress &&
               ptw_wait_replay_q.size() == 0 &&
               !flushsb_request_pending() &&
               !cancel_reconcile_pending() &&
               !has_pending_lsq_cancel_apply() &&
               pending_lq_cancel_count == 0 &&
               pending_sq_cancel_count == 0 &&
               !redirect_sample_anchor_pending() &&
               !cancel_snapshot_buffer_pending() &&
               memblock_sync_pkg::lsq_timing_sideband_queue_size() == 0;
    endfunction:runtime_drain_complete

    function void request_global_stop_if_done();
        if (transaction_done() && runtime_drain_complete()) begin
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
        status.set_target_instance_flush_epoch(
            issue_target,
            memblock_sync_pkg::dispatch_flush_epoch
        );
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

    function int unsigned begin_lsq_reservation_launch(input memblock_uid_t uid);
        status_transaction status;

        check_uid(uid, "begin_lsq_reservation_launch");
        status = get_status(uid);
        if (!status.active || (!status.active_lq_mapped && !status.active_sq_mapped)) begin
            `uvm_fatal("LSQ_RESERVATION",
                       $sformatf("uid=%0d launch has no active LSQ mapping", uid))
        end
        if (status.lsq_reservation_state != MEMBLOCK_LSQ_RESERVATION_NONE) begin
            `uvm_fatal("LSQ_RESERVATION",
                       $sformatf("uid=%0d reservation already exists state=%0d epoch=%0d",
                                 uid, status.lsq_reservation_state,
                                 status.lsq_reservation_launch_epoch))
        end
        status.lsq_reservation_launch_epoch++;
        if (status.lsq_reservation_launch_epoch == 0) begin
            `uvm_fatal("LSQ_RESERVATION", "reservation epoch wrapped")
        end
        status.lsq_reservation_state = MEMBLOCK_LSQ_RESERVATION_LAUNCHED_PENDING_SAMPLE;
        status.lsq_reservation_sample_valid = 1'b0;
        status.lsq_reservation_sample_seq = 0;
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        return status.lsq_reservation_launch_epoch;
    endfunction:begin_lsq_reservation_launch

    function void mark_lsq_reservation_sampled(input memblock_uid_t uid,
                                               input int unsigned launch_epoch,
                                               input longint unsigned sample_seq);
        status_transaction status;

        check_uid(uid, "mark_lsq_reservation_sampled");
        status = get_status(uid);
        if (launch_epoch == 0 || sample_seq == 0) begin
            `uvm_fatal("LSQ_RESERVATION",
                       $sformatf("uid=%0d invalid sample token epoch=%0d sample_seq=%0d",
                                 uid, launch_epoch, sample_seq))
        end
        if (status.lsq_reservation_launch_epoch != launch_epoch ||
            status.lsq_reservation_state != MEMBLOCK_LSQ_RESERVATION_LAUNCHED_PENDING_SAMPLE) begin
            `uvm_fatal("LSQ_RESERVATION",
                       $sformatf("uid=%0d sample token mismatch expected epoch=%0d/state=%0d got epoch=%0d/state=%0d",
                                 uid, status.lsq_reservation_launch_epoch,
                                 status.lsq_reservation_state, launch_epoch,
                                 MEMBLOCK_LSQ_RESERVATION_LAUNCHED_PENDING_SAMPLE))
        end
        status.lsq_reservation_sample_seq = sample_seq;
        status.lsq_reservation_sample_valid = 1'b1;
        status.lsq_reservation_state = MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE;
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    endfunction:mark_lsq_reservation_sampled

    function bit uid_lsq_reservation_visible(input memblock_uid_t uid);
        status_transaction status;

        check_uid(uid, "uid_lsq_reservation_visible");
        status = get_status(uid);
        return status.lsq_reservation_state == MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE &&
               status.lsq_reservation_sample_valid;
    endfunction:uid_lsq_reservation_visible

    function void clear_uid_dispatch_result(input memblock_uid_t uid);
        status_transaction status;

        status = get_status(uid);
        clear_uid_mmio_tag(uid);
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
        status.active_instance_flush_epoch_valid = 1'b0;
        status.active_instance_flush_epoch = 0;
        status.clear_target_instance_flush_epochs();
        status.clear_lsq_reservation_visibility();
    endfunction:clear_uid_dispatch_result

    function int find_cancel_record_index(input int unsigned redirect_epoch);
        foreach (cancel_record_q[idx]) begin
            if (cancel_record_q[idx].valid &&
                cancel_record_q[idx].redirect_epoch == redirect_epoch) begin
                return idx;
            end
        end
        return -1;
    endfunction:find_cancel_record_index

    function int find_cancel_record_index_by_id(input int unsigned record_id);
        foreach (cancel_record_q[idx]) begin
            if (cancel_record_q[idx].valid &&
                cancel_record_q[idx].cancel_record_id == record_id) begin
                return idx;
            end
        end
        return -1;
    endfunction:find_cancel_record_index_by_id

    function int find_oldest_unanchored_cancel_record_index();
        foreach (cancel_record_q[idx]) begin
            if (cancel_record_q[idx].valid &&
                !cancel_record_q[idx].redirect_anchor_valid) begin
                return idx;
            end
        end
        return -1;
    endfunction:find_oldest_unanchored_cancel_record_index

    function int find_oldest_observation_pending_record_index();
        foreach (cancel_record_q[idx]) begin
            if (cancel_record_q[idx].valid &&
                cancel_record_q[idx].redirect_anchor_valid &&
                !cancel_record_q[idx].observed_valid) begin
                return idx;
            end
        end
        return -1;
    endfunction:find_oldest_observation_pending_record_index

    function void check_cancel_record_capacity();
        if (cancel_record_q.size() > MEMBLOCK_CANCEL_RECORD_MAX_DEPTH) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("cancel record depth=%0d exceeds compile bound=%0d",
                                 cancel_record_q.size(), MEMBLOCK_CANCEL_RECORD_MAX_DEPTH))
        end
    endfunction:check_cancel_record_capacity

    function void check_cancel_pending_aggregate();
        int unsigned expected_lq;
        int unsigned expected_sq;

        expected_lq = 0;
        expected_sq = 0;
        foreach (cancel_record_q[idx]) begin
            if (cancel_record_q[idx].valid &&
                !cancel_record_q[idx].software_applied) begin
                expected_lq += cancel_record_q[idx].software_cancel_lq_count;
                expected_sq += cancel_record_q[idx].software_cancel_sq_count;
            end
        end
        if (pending_lq_cancel_count != expected_lq ||
            pending_sq_cancel_count != expected_sq) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("pending cancel aggregate drift stored=%0d/%0d records=%0d/%0d",
                                 pending_lq_cancel_count, pending_sq_cancel_count,
                                 expected_lq, expected_sq))
        end
    endfunction:check_cancel_pending_aggregate

    function void note_lsq_cancel_for_uid(input memblock_uid_t uid,
                                          input int unsigned redirect_epoch);
        status_transaction status;
        main_control_transaction main_tr;
        int record_idx;

        check_uid(uid, "note_lsq_cancel_for_uid");
        record_idx = find_cancel_record_index(redirect_epoch);
        if (record_idx < 0) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("uid=%0d has no cancel record for redirect epoch=%0d",
                                 uid, redirect_epoch))
        end
        status = get_status(uid);
        if (!active_cancel_record_id_valid ||
            cancel_record_q[record_idx].cancel_record_id != active_cancel_record_id) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("uid=%0d redirect epoch=%0d does not own active cancel record",
                                 uid, redirect_epoch))
        end
        if (!cancel_record_q[record_idx].redirect_anchor_valid ||
            cancel_record_q[record_idx].software_count_finalized) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("uid=%0d cancel record epoch=%0d is not open for scan",
                                 uid, redirect_epoch))
        end
        if (status.lsq_cancel_accounted_epoch == redirect_epoch) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("uid=%0d counted twice for redirect epoch=%0d", uid, redirect_epoch))
        end
        if (!status.active_lq_mapped && !status.active_sq_mapped) begin
            status.lsq_cancel_accounted_epoch = redirect_epoch;
            status.lsq_reservation_state = MEMBLOCK_LSQ_RESERVATION_CANCEL_ACCOUNTED;
            check_cancel_pending_aggregate();
            return;
        end
        main_tr = get_main_transaction(uid);
        if (main_tr.numLsElem != 1) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("uid=%0d scalar cancel requires numLsElem=1, got %0d",
                                 uid, main_tr.numLsElem))
        end
        begin
            if (status.lsq_reservation_state != MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE ||
                !status.lsq_reservation_sample_valid) begin
                `uvm_fatal("LSQ_CANCEL",
                           $sformatf("uid=%0d mapped cancel lacks DUT-visible reservation state=%0d valid=%0d",
                                     uid, status.lsq_reservation_state,
                                     status.lsq_reservation_sample_valid))
            end
            if (status.lsq_reservation_sample_seq >
                cancel_record_q[record_idx].redirect_lsq_sample_seq) begin
                `uvm_fatal("LSQ_CANCEL",
                           $sformatf("uid=%0d reservation sample=%0d is later than redirect LSQ cutoff=%0d",
                                     uid, status.lsq_reservation_sample_seq,
                                     cancel_record_q[record_idx].redirect_lsq_sample_seq))
            end
        end
        if (status.active_lq_mapped) begin
            cancel_record_q[record_idx].software_cancel_lq_count += main_tr.numLsElem;
            pending_lq_cancel_count += main_tr.numLsElem;
            if (cancel_record_q[record_idx].software_cancel_lq_count > MEMBLOCK_LQ_SIZE) begin
                `uvm_fatal("LSQ_CANCEL", "software LQ cancel count exceeds LQ capacity")
            end
        end
        if (status.active_sq_mapped) begin
            cancel_record_q[record_idx].software_cancel_sq_count += main_tr.numLsElem;
            pending_sq_cancel_count += main_tr.numLsElem;
            if (cancel_record_q[record_idx].software_cancel_sq_count > MEMBLOCK_SQ_SIZE) begin
                `uvm_fatal("LSQ_CANCEL", "software SQ cancel count exceeds SQ capacity")
            end
        end
        status.lsq_cancel_accounted_epoch = redirect_epoch;
        status.lsq_reservation_state = MEMBLOCK_LSQ_RESERVATION_CANCEL_ACCOUNTED;
        check_cancel_pending_aggregate();
    endfunction:note_lsq_cancel_for_uid

    function void add_cancel_snapshot(input memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot);
        if (snapshot.sample_seq == 0) begin
            `uvm_fatal("LSQ_CANCEL", "cancel snapshot has zero sample sequence")
        end
        if (snapshot.lq_cancel_count > MEMBLOCK_LQ_SIZE ||
            snapshot.sq_cancel_count > MEMBLOCK_SQ_SIZE) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("DUT cancel snapshot exceeds capacity sample=%0d count=%0d/%0d",
                                 snapshot.sample_seq,
                                 snapshot.lq_cancel_count,
                                 snapshot.sq_cancel_count))
        end
        if (latest_drained_cancel_sample_seq != 0 &&
            snapshot.sample_seq <= latest_drained_cancel_sample_seq) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("cancel snapshot sample sequence is not strictly increasing previous=%0d current=%0d",
                                 latest_drained_cancel_sample_seq, snapshot.sample_seq))
        end
        latest_drained_cancel_sample_seq = snapshot.sample_seq;
        cancel_snapshot_history_q.push_back(snapshot);
        if (cancel_snapshot_history_q.size() > MEMBLOCK_CANCEL_SNAPSHOT_QUEUE_MAX_DEPTH) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("local cancel snapshot depth=%0d exceeds compile bound=%0d",
                                 cancel_snapshot_history_q.size(),
                                 MEMBLOCK_CANCEL_SNAPSHOT_QUEUE_MAX_DEPTH))
        end
    endfunction:add_cancel_snapshot

    function void add_redirect_anchor(input memblock_sync_pkg::dispatch_raw_redirect_anchor_t anchor);
        if (!anchor.valid || anchor.sample_seq == 0) begin
            `uvm_fatal("LSQ_CANCEL", "redirect anchor must be valid with non-zero sample sequence")
        end
        if (redirect_anchor_history_q.size() != 0 &&
            anchor.sample_seq <= redirect_anchor_history_q[$].sample_seq) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("redirect anchor sequence is not strictly increasing previous=%0d current=%0d",
                                 redirect_anchor_history_q[$].sample_seq, anchor.sample_seq))
        end
        redirect_anchor_history_q.push_back(anchor);
        if (redirect_anchor_history_q.size() > MEMBLOCK_CANCEL_RECORD_MAX_DEPTH) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("redirect anchor depth=%0d exceeds compile bound=%0d",
                                 redirect_anchor_history_q.size(), MEMBLOCK_CANCEL_RECORD_MAX_DEPTH))
        end
    endfunction:add_redirect_anchor

    function void bind_redirect_anchors_to_cancel_records();
        while (redirect_anchor_history_q.size() != 0) begin
            int record_idx;
            memblock_sync_pkg::dispatch_raw_redirect_anchor_t anchor;

            record_idx = find_oldest_unanchored_cancel_record_index();
            if (record_idx < 0) begin
                `uvm_fatal("LSQ_CANCEL", "redirect anchor has no unanchored framework record")
            end
            anchor = redirect_anchor_history_q.pop_front();
            if (anchor.level != cancel_record_q[record_idx].redirect.level ||
                anchor.rob_flag != cancel_record_q[record_idx].redirect.rob_key.flag ||
                anchor.rob_value != cancel_record_q[record_idx].redirect.rob_key.value) begin
                `uvm_fatal("LSQ_CANCEL",
                           $sformatf("redirect anchor FIFO mismatch record=%0d expected level/rob=%0d/%0d/%0d observed=%0d/%0d/%0d",
                                     cancel_record_q[record_idx].cancel_record_id,
                                     cancel_record_q[record_idx].redirect.level,
                                     cancel_record_q[record_idx].redirect.rob_key.flag,
                                     cancel_record_q[record_idx].redirect.rob_key.value,
                                     anchor.level, anchor.rob_flag, anchor.rob_value))
            end
            if (record_idx > 0 &&
                cancel_record_q[record_idx - 1].valid &&
                cancel_record_q[record_idx - 1].redirect_anchor_valid &&
                anchor.sample_seq <= cancel_record_q[record_idx - 1].redirect_sample_seq) begin
                `uvm_fatal("LSQ_CANCEL", "redirect anchor sample sequence does not preserve record FIFO order")
            end
            cancel_record_q[record_idx].redirect_anchor_valid = 1'b1;
            cancel_record_q[record_idx].redirect_sample_seq = anchor.sample_seq;
            cancel_record_q[record_idx].redirect_lsq_sample_seq =
                anchor.sample_seq + MEMBLOCK_DUT_REDIRECT_TO_LSQ_LATENCY;
            cancel_record_q[record_idx].dut_cancel_update_sample_seq =
                anchor.sample_seq + MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY;
            cancel_record_q[record_idx].compare_snapshot_sample_seq =
                anchor.sample_seq + MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY;
            cancel_record_q[record_idx].deadline_sample_seq =
                cancel_record_q[record_idx].compare_snapshot_sample_seq + 1;
        end
    endfunction:bind_redirect_anchors_to_cancel_records

    function void check_cancel_baseline_snapshot(
        input memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot);
        if (!cancel_held_baseline_valid) begin
            if (snapshot.lq_cancel_count != 0 || snapshot.sq_cancel_count != 0) begin
                `uvm_fatal("LSQ_CANCEL",
                           $sformatf("non-zero cancel level before first redirect target sample=%0d count=%0d/%0d",
                                     snapshot.sample_seq,
                                     snapshot.lq_cancel_count,
                                     snapshot.sq_cancel_count))
            end
            cancel_held_baseline_valid = 1'b1;
            cancel_held_lq_count = snapshot.lq_cancel_count;
            cancel_held_sq_count = snapshot.sq_cancel_count;
            return;
        end
        if (snapshot.lq_cancel_count != cancel_held_lq_count ||
            snapshot.sq_cancel_count != cancel_held_sq_count) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("cancel held level changed outside target sample=%0d baseline=%0d/%0d observed=%0d/%0d",
                                 snapshot.sample_seq,
                                 cancel_held_lq_count, cancel_held_sq_count,
                                 snapshot.lq_cancel_count, snapshot.sq_cancel_count))
        end
    endfunction:check_cancel_baseline_snapshot

    function void cleanup_completed_cancel_records();
        while (cancel_record_q.size() != 0 &&
               cancel_record_q[0].valid &&
               cancel_record_q[0].software_applied &&
               cancel_record_q[0].observed_valid) begin
            void'(cancel_record_q.pop_front());
        end
        check_cancel_pending_aggregate();
    endfunction:cleanup_completed_cancel_records

    function void service_cancel_reconcile();
        bind_redirect_anchors_to_cancel_records();
        foreach (cancel_record_q[idx]) begin
            if (cancel_record_q[idx].valid &&
                cancel_record_q[idx].redirect_drive_done_valid &&
                !cancel_record_q[idx].redirect_anchor_valid &&
                memblock_sync_pkg::get_dispatch_service_cycle() >
                    cancel_record_q[idx].anchor_deadline_service_cycle) begin
                `uvm_fatal("LSQ_CANCEL_RECONCILE",
                           $sformatf("redirect record=%0d missed monitor anchor deadline=%0d current=%0d",
                                     cancel_record_q[idx].cancel_record_id,
                                     cancel_record_q[idx].anchor_deadline_service_cycle,
                                     memblock_sync_pkg::get_dispatch_service_cycle()))
            end
        end

        while (cancel_snapshot_history_q.size() != 0) begin
            memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot;
            int record_idx;

            snapshot = cancel_snapshot_history_q[0];
            record_idx = find_oldest_observation_pending_record_index();
            if (record_idx < 0) begin
                // 中文注释：只要仍有 record，队首 snapshot 就可能属于尚未到达的
                // redirect anchor；此时保留整个有界队列等待绑定，不能误作 baseline 消费。
                if (cancel_record_q.size() != 0) begin
                    break;
                end
                check_cancel_baseline_snapshot(snapshot);
                void'(cancel_snapshot_history_q.pop_front());
                continue;
            end
            if (snapshot.sample_seq <
                cancel_record_q[record_idx].compare_snapshot_sample_seq) begin
                check_cancel_baseline_snapshot(snapshot);
                void'(cancel_snapshot_history_q.pop_front());
                continue;
            end
            if (snapshot.sample_seq >
                cancel_record_q[record_idx].compare_snapshot_sample_seq) begin
                `uvm_fatal("LSQ_CANCEL_RECONCILE",
                           $sformatf("missing exact cancel target snapshot record=%0d target=%0d next=%0d",
                                     cancel_record_q[record_idx].cancel_record_id,
                                     cancel_record_q[record_idx].compare_snapshot_sample_seq,
                                     snapshot.sample_seq))
            end
            if (!cancel_record_q[record_idx].software_count_finalized) begin
                break;
            end
            if (snapshot.lq_cancel_count !=
                    cancel_record_q[record_idx].software_cancel_lq_count ||
                snapshot.sq_cancel_count !=
                    cancel_record_q[record_idx].software_cancel_sq_count) begin
                `uvm_fatal("LSQ_CANCEL_RECONCILE",
                           $sformatf("cancel mismatch record=%0d epoch=%0d software=%0d/%0d dut=%0d/%0d",
                                     cancel_record_q[record_idx].cancel_record_id,
                                     cancel_record_q[record_idx].redirect_epoch,
                                     cancel_record_q[record_idx].software_cancel_lq_count,
                                     cancel_record_q[record_idx].software_cancel_sq_count,
                                     snapshot.lq_cancel_count,
                                     snapshot.sq_cancel_count))
            end
            cancel_record_q[record_idx].observed_cancel_lq_count = snapshot.lq_cancel_count;
            cancel_record_q[record_idx].observed_cancel_sq_count = snapshot.sq_cancel_count;
            cancel_record_q[record_idx].observed_valid = 1'b1;
            cancel_reconcile_match_count++;
            if (snapshot.lq_cancel_count != 0) begin
                cancel_reconcile_lq_nonzero_match_count++;
            end
            if (snapshot.sq_cancel_count != 0) begin
                cancel_reconcile_sq_nonzero_match_count++;
            end
            cancel_held_baseline_valid = 1'b1;
            cancel_held_lq_count = snapshot.lq_cancel_count;
            cancel_held_sq_count = snapshot.sq_cancel_count;
            void'(cancel_snapshot_history_q.pop_front());
        end

        begin
            int record_idx;

            record_idx = find_oldest_observation_pending_record_index();
            if (record_idx >= 0 &&
                cancel_record_q[record_idx].software_count_finalized &&
                latest_drained_cancel_sample_seq >
                    cancel_record_q[record_idx].deadline_sample_seq) begin
                `uvm_fatal("LSQ_CANCEL_RECONCILE",
                           $sformatf("cancel compare deadline expired record=%0d deadline=%0d watermark=%0d",
                                     cancel_record_q[record_idx].cancel_record_id,
                                     cancel_record_q[record_idx].deadline_sample_seq,
                                     latest_drained_cancel_sample_seq))
            end
        end
        cleanup_completed_cancel_records();
    endfunction:service_cancel_reconcile

    function bit cancel_redirect_scan_ready(input memblock_redirect_payload_t redirect);
        int record_idx;

        if (!redirect.valid) begin
            return 1'b0;
        end
        if (!active_cancel_record_id_valid) begin
            return 1'b0;
        end
        record_idx = find_cancel_record_index_by_id(active_cancel_record_id);
        if (record_idx < 0 ||
            redirect.valid != cancel_record_q[record_idx].redirect.valid ||
            redirect.flush_itself != cancel_record_q[record_idx].redirect.flush_itself ||
            redirect.level != cancel_record_q[record_idx].redirect.level ||
            redirect.rob_key != cancel_record_q[record_idx].redirect.rob_key ||
            !cancel_record_q[record_idx].redirect_drive_done_valid ||
            !cancel_record_q[record_idx].redirect_anchor_valid) begin
            return 1'b0;
        end
        return memblock_sync_pkg::peek_current_dut_global_sample() >=
                   cancel_record_q[record_idx].redirect_lsq_sample_seq &&
               latest_drained_cancel_sample_seq >=
                   cancel_record_q[record_idx].redirect_lsq_sample_seq;
    endfunction:cancel_redirect_scan_ready

    function bit has_pending_cancel_reconcile();
        foreach (cancel_record_q[idx]) begin
            if (cancel_record_q[idx].valid && !cancel_record_q[idx].observed_valid) begin
                return 1'b1;
            end
        end
        return 1'b0;
    endfunction:has_pending_cancel_reconcile

    function bit has_pending_lsq_cancel_apply();
        foreach (cancel_record_q[idx]) begin
            if (cancel_record_q[idx].valid && cancel_record_q[idx].software_count_finalized &&
                !cancel_record_q[idx].software_applied) begin
                return 1'b1;
            end
        end
        return 1'b0;
    endfunction:has_pending_lsq_cancel_apply

    function void mark_cancel_record_applied(input int unsigned redirect_epoch);
        int record_idx;

        record_idx = find_cancel_record_index(redirect_epoch);
        if (record_idx < 0) begin
            `uvm_fatal("LSQ_CANCEL", $sformatf("unknown cancel epoch=%0d", redirect_epoch))
        end
        if (!cancel_record_q[record_idx].software_count_finalized ||
            cancel_record_q[record_idx].software_applied) begin
            `uvm_fatal("LSQ_CANCEL", $sformatf("cancel epoch=%0d is not ready or already applied", redirect_epoch))
        end
        cancel_record_q[record_idx].software_applied = 1'b1;
        check_cancel_pending_aggregate();
    endfunction:mark_cancel_record_applied

    function void prepare_uid_for_redirect_reissue(input memblock_uid_t uid,
                                                   input memblock_redirect_payload_t redirect);
        status_transaction status;

        if (!redirect.valid) begin
            `uvm_fatal("COMMON_DATA", "prepare_uid_for_redirect_reissue requires valid redirect")
        end
        status = get_status(uid);
        if (status.terminal_done) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("redirect tries to flush already terminal_done uid=%0d", uid))
        end

        // 中文伪代码：在 retire_active_uid 清除 mapping 前登记软件 cancel，
        // 否则 scan 只能看到已经释放的 active_lq/sq 标志。
        note_lsq_cancel_for_uid(uid, memblock_sync_pkg::dispatch_flush_epoch);
        cancel_waiting_uid_tlb_record_for_uid(uid, "redirect_flush");
        // redirect命中的旧动态实例不再等待writeback/commit；清queue/map后等待同uid重新admission。
        if (status.active) begin
            retire_active_uid(uid);
        end else begin
            remove_uid_from_issue_queues(uid);
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
        if (active_redirect.valid || active_cancel_record_id_valid) begin
            `uvm_fatal("COMMON_DATA", "request_redirect_flush called while another redirect is active")
        end
        if (cancel_record_q.size() >= MEMBLOCK_CANCEL_RECORD_MAX_DEPTH) begin
            `uvm_fatal("LSQ_CANCEL", "cancel record FIFO is full before redirect allocation")
        end
        redirect_phase    = MEMBLOCK_REDIRECT_PHASE_DETECTED;
        flush_in_progress = 1'b1;
        memblock_sync_pkg::dispatch_flush_in_progress = 1'b1;
        memblock_sync_pkg::dispatch_flush_epoch++;
        begin
            memblock_lsq_cancel_record_t record;

            record = '{default:'0};
            next_cancel_record_id++;
            if (next_cancel_record_id == 0) begin
                `uvm_fatal("LSQ_CANCEL", "cancel record id wrapped")
            end
            record.valid = 1'b1;
            record.redirect_epoch = memblock_sync_pkg::dispatch_flush_epoch;
            record.cancel_record_id = next_cancel_record_id;
            record.redirect = redirect;
            record.redirect_service_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            cancel_record_q.push_back(record);
            active_cancel_record_id = record.cancel_record_id;
            active_cancel_record_id_valid = 1'b1;
            check_cancel_record_capacity();
        end
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
        int record_idx;

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
        if (!active_cancel_record_id_valid) begin
            `uvm_fatal("LSQ_CANCEL", "redirect drive completed without an active cancel record")
        end
        record_idx = find_cancel_record_index_by_id(active_cancel_record_id);
        if (record_idx < 0 ||
            !redirect_payload_equal(payload, cancel_record_q[record_idx].redirect)) begin
            `uvm_fatal("LSQ_CANCEL", "redirect drive payload does not match active cancel record")
        end
        if (cancel_record_q[record_idx].redirect_drive_done_valid) begin
            `uvm_fatal("LSQ_CANCEL", "redirect drive completion was recorded twice")
        end
        cancel_record_q[record_idx].redirect_drive_done_valid = 1'b1;
        cancel_record_q[record_idx].redirect_drive_done_service_cycle =
            memblock_sync_pkg::get_dispatch_service_cycle();
        cancel_record_q[record_idx].anchor_deadline_service_cycle =
            memblock_sync_pkg::get_dispatch_service_cycle() + 2;
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
        int record_idx;

        if (!redirect.valid) begin
            `uvm_fatal("COMMON_DATA", "apply_redirect_flush_range requires valid redirect")
        end
        if (!active_cancel_record_id_valid) begin
            `uvm_fatal("LSQ_CANCEL", "redirect flush scan has no active cancel record")
        end
        record_idx = find_cancel_record_index_by_id(active_cancel_record_id);
        if (record_idx < 0 ||
            !redirect_payload_equal(redirect, cancel_record_q[record_idx].redirect) ||
            !cancel_record_q[record_idx].redirect_anchor_valid ||
            memblock_sync_pkg::peek_current_dut_global_sample() <
                cancel_record_q[record_idx].redirect_lsq_sample_seq ||
            latest_drained_cancel_sample_seq <
                cancel_record_q[record_idx].redirect_lsq_sample_seq) begin
            `uvm_fatal("LSQ_CANCEL", "redirect flush scan started before its anchored LSQ sample boundary")
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
        if (cancel_record_q[record_idx].software_cancel_lq_count > MEMBLOCK_LQ_SIZE ||
            cancel_record_q[record_idx].software_cancel_sq_count > MEMBLOCK_SQ_SIZE) begin
            `uvm_fatal("LSQ_CANCEL", "finalized software cancel count exceeds LSQ capacity")
        end
        cancel_record_q[record_idx].active_scan_done = 1'b1;
        cancel_record_q[record_idx].software_count_finalized = 1'b1;
        cancel_record_q[record_idx].state_flush_applied_service_cycle =
            memblock_sync_pkg::get_dispatch_service_cycle();
        active_cancel_record_id_valid = 1'b0;
        active_cancel_record_id = 0;
        check_cancel_pending_aggregate();
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
        if (status.active_instance_flush_epoch_valid) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("activate_uid uid=%0d still carries activation epoch=%0d",
                                 uid, status.active_instance_flush_epoch))
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
        status.active_instance_flush_epoch_valid = 1'b1;
        status.active_instance_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
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
        if (status.lsq_deq) begin
            status.clear_lsq_reservation_visibility();
        end
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
        if (status.lsq_deq) begin
            status.clear_lsq_reservation_visibility();
        end
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
            // 中文注释：redirect 命中的 active uid 只能由
            // apply_redirect_flush_range() 统一扫描、记账和清理；这里不能提前删除
            // mapping，否则 cancel record 会失去 reservation/sample 事实。
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

    function bit tlb_lookup_key_equal(input memblock_tlb_lookup_key_t left,
                                      input memblock_tlb_lookup_key_t right);
        return left.vpn == right.vpn &&
               left.asid == right.asid &&
               left.vmid == right.vmid &&
               left.s2xlate == right.s2xlate;
    endfunction:tlb_lookup_key_equal

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
        return get_or_create_tlb_entry_by_req_with_snapshot(vpn,
                                                             s2xlate,
                                                             mmu_csr_state,
                                                             key,
                                                             entry,
                                                             created);
    endfunction:get_or_create_tlb_entry_by_req

    // 中文注释：L2TLB request fire必须用该笔request冻结的CSR生成key和新entry。
    // 不能在CSR变更边界回退到common_data的live mmu_csr_state；命中旧表项时仍复用同一by-key存储。
    function bit get_or_create_tlb_entry_by_req_with_snapshot(
        input bit [37:0] vpn,
        input bit [1:0] s2xlate,
        input mmu_csr_runtime_state csr_snapshot,
        output memblock_tlb_lookup_key_t key,
        output memblock_tlb_entry entry,
        output bit created);
        if (csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "get_or_create_tlb_entry_by_req_with_snapshot got null csr_snapshot")
        end
        key = csr_snapshot.make_lookup_key({26'b0, vpn}, s2xlate);
        if (has_tlb_entry(key)) begin
            entry = tlb_entry_by_key[key];
            entry.last_hit_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            created = 1'b0;
            return 1'b1;
        end
        entry = build_tlb_entry_for_key_with_csr(key, csr_snapshot);
        insert_tlb_entry(key, entry);
        created = 1'b1;
        return 1'b1;
    endfunction:get_or_create_tlb_entry_by_req_with_snapshot

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

    // Abstract responsibility: clear only the software-owned live L2TLB
    // entry/range state at a runtime reset, while preserving the main table and
    // UID history owned by other dispatch flows.
    function void clear_dispatch_l2tlb_live_entries();
        tlb_entry_by_key.delete();
    endfunction:clear_dispatch_l2tlb_live_entries

    function memblock_tlb_entry build_tlb_entry_for_key_with_csr(
        input memblock_tlb_lookup_key_t key,
        input mmu_csr_runtime_state csr_snapshot);
        memblock_tlb_entry entry;
        tlb_map_builder    builder;

        if (csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "build_tlb_entry_for_key_with_csr got null csr_snapshot")
        end
        builder = tlb_map_builder::type_id::create("tlb_builder_by_key");
        if (builder == null) begin
            `uvm_fatal("COMMON_DATA", "failed to create tlb_map_builder")
        end
        entry = builder.build_tlb_entry_for_req(key.vpn[37:0], key.s2xlate, csr_snapshot);
        entry.lookup_key = key;
        entry.asid = key.asid;
        entry.vmid = key.vmid;
        entry.s2xlate = key.s2xlate;
        return entry;
    endfunction:build_tlb_entry_for_key_with_csr

    function memblock_tlb_entry build_tlb_entry_for_key(input memblock_tlb_lookup_key_t key);
        if (mmu_csr_state == null) begin
            mmu_csr_state = mmu_csr_runtime_state::type_id::create("mmu_csr_state");
            mmu_csr_state.reset();
        end
        return build_tlb_entry_for_key_with_csr(key, mmu_csr_state);
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

    // 中文注释：UID TLB lifecycle helper 只维护 request/response/cancel 账本，
    // 不直接修改主表 pass/fail/terminal，也不重新解释 lookup key。
    function int unsigned mark_uid_tlb_record_request_fire(input memblock_tlb_lookup_key_t key,
                                                           input longint unsigned sample_seq);
        int unsigned marked_count;

        if (sample_seq == 0) begin
            `uvm_fatal("COMMON_DATA", "mark_uid_tlb_record_request_fire requires non-zero sample_seq")
        end
        if (sample_seq > memblock_sync_pkg::peek_current_dut_global_sample()) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("mark_uid_tlb_record_request_fire got future sample=%0d latest=%0d key vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d",
                                 sample_seq,
                                 memblock_sync_pkg::peek_current_dut_global_sample(),
                                 key.vpn, key.asid, key.vmid, key.s2xlate))
        end

        marked_count = 0;
        foreach (uid_tlb_record_by_uid[uid]) begin
            memblock_uid_tlb_record record;

            record = uid_tlb_record_by_uid[uid];
            if (record == null || !record.record_valid ||
                record.vpn != key.vpn || record.s2xlate != key.s2xlate) begin
                continue;
            end
            if (record.lifecycle_state ==
                    memblock_uid_tlb_record::MEMBLOCK_UID_TLB_RECORD_STATE_UNBOUND) begin
                // One DTLB/L2TLB request may be shared by several same-key
                // PTW filter waiters.  The marker proves a real request fire;
                // it does not establish a token-to-UID ownership relation.
                record.mark_request_fire(sample_seq);
                marked_count++;
            end
        end

        if (marked_count == 0) begin
            `uvm_info("COMMON_DATA",
                      $sformatf("no UNBOUND uid_tlb_record matches L2TLB request fire key vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d sample=%0d; allow duplicate/prefetch/no-UID request",
                                key.vpn, key.asid, key.vmid, key.s2xlate, sample_seq),
                      UVM_LOW)
        end
        return marked_count;
    endfunction:mark_uid_tlb_record_request_fire

    function int unsigned cancel_waiting_uid_tlb_record_for_uid(input memblock_uid_t uid,
                                                                input string reason = "");
        memblock_uid_tlb_record record;

        check_uid(uid, "cancel_waiting_uid_tlb_record_for_uid");
        if (!uid_tlb_record_by_uid.exists(uid) || uid_tlb_record_by_uid[uid] == null) begin
            return 0;
        end
        record = uid_tlb_record_by_uid[uid];
        if (!record.is_waiting()) begin
            return 0;
        end
        record.mark_canceled();
        `uvm_info("COMMON_DATA",
                  $sformatf("cancel WAITING uid_tlb_record uid=%0d reason=%s key vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d sample=%0d",
                            uid, reason,
                            record.lookup_key.vpn,
                            record.lookup_key.asid,
                            record.lookup_key.vmid,
                            record.lookup_key.s2xlate,
                            record.request_fire_sample_seq),
                  UVM_LOW)
        return 1;
    endfunction:cancel_waiting_uid_tlb_record_for_uid

    function int unsigned cancel_waiting_uid_tlb_records(input string reason = "");
        int unsigned cancel_count;

        cancel_count = 0;
        foreach (uid_tlb_record_by_uid[uid]) begin
            if (uid_tlb_record_by_uid[uid] != null &&
                uid_tlb_record_by_uid[uid].is_waiting()) begin
                uid_tlb_record_by_uid[uid].mark_canceled();
                cancel_count++;
            end
        end
        if (cancel_count != 0) begin
            `uvm_info("COMMON_DATA",
                      $sformatf("cancel WAITING uid_tlb_records reason=%s count=%0d",
                                reason, cancel_count),
                      UVM_LOW)
        end
        return cancel_count;
    endfunction:cancel_waiting_uid_tlb_records

    function int unsigned cancel_waiting_uid_tlb_records_through_sample(
        input longint unsigned anchor_sample_seq,
        input string reason = "");
        int unsigned cancel_count;

        cancel_count = 0;
        if (anchor_sample_seq == 0) begin
            return 0;
        end
        foreach (uid_tlb_record_by_uid[uid]) begin
            memblock_uid_tlb_record record;

            record = uid_tlb_record_by_uid[uid];
            if (record == null || !record.is_waiting() ||
                !record.request_fire_valid ||
                record.request_fire_sample_seq == 0 ||
                record.request_fire_sample_seq > anchor_sample_seq) begin
                continue;
            end
            record.mark_canceled();
            cancel_count++;
        end
        if (cancel_count != 0) begin
            `uvm_info("COMMON_DATA",
                      $sformatf("cancel WAITING uid_tlb_records through sample reason=%s anchor=%0d count=%0d",
                                reason, anchor_sample_seq, cancel_count),
                      UVM_LOW)
        end
        return cancel_count;
    endfunction:cancel_waiting_uid_tlb_records_through_sample

    function bit has_waiting_uid_tlb_record();
        foreach (uid_tlb_record_by_uid[uid]) begin
            if (uid_tlb_record_by_uid[uid] != null &&
                uid_tlb_record_by_uid[uid].is_waiting()) begin
                return 1'b1;
            end
        end
        return 1'b0;
    endfunction:has_waiting_uid_tlb_record

    // Abstract responsibility: reproduce the V2 PtwRespS2.hit() raw address,
    // stage, sector, ASID/VMID and global matching rules for one response.
    // This helper is intentionally independent of UID state.
    function bit entry_matches_request_raw(
        input memblock_tlb_entry entry,
        input bit [51:0] request_vpn,
        input bit [1:0] request_s2xlate,
        input mmu_csr_runtime_state response_filter_csr_snapshot);
        bit level_hit;
        bit addr_low_hit;
        bit asid_hit;
        bit vmid_hit;
        bit napot_or_super;
        bit [15:0] response_asid;
        bit [15:0] response_vmid;

        if (entry == null || response_filter_csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "entry_matches_request_raw got null input")
        end
        if (entry.s2xlate != request_s2xlate) begin
            return 1'b0;
        end

        response_asid = response_filter_csr_snapshot.current_asid(request_s2xlate);
        response_vmid = response_filter_csr_snapshot.current_vmid(request_s2xlate);
        napot_or_super = (entry.level != 2'd0) || entry.pte_n;

        // MMUBundle.scala's level_match checks all VPN portions above the
        // effective page level.  The no-S2 sector path removes VPN[2:0]
        // before applying valididx; the two-stage combined path and HPTW use
        // the reconstructed full level-0 anchor.
        if (request_s2xlate == 2'd0) begin
            if (entry.pte_n && entry.level == 2'd0) begin
                level_hit = entry.lookup_key.vpn[51:4] == request_vpn[51:4];
            end
            else begin
                case (entry.level)
                    2'd0: level_hit = entry.lookup_key.vpn[51:3] == request_vpn[51:3];
                    2'd1: level_hit = entry.lookup_key.vpn[51:9] == request_vpn[51:9];
                    2'd2: level_hit = entry.lookup_key.vpn[51:18] == request_vpn[51:18];
                    default: level_hit = entry.lookup_key.vpn[51:27] == request_vpn[51:27];
                endcase
            end
        end
        else if (entry.pte_n && entry.level == 2'd0) begin
            level_hit = entry.lookup_key.vpn[51:4] == request_vpn[51:4];
        end
        else begin
            case (entry.level)
                2'd0: level_hit = entry.lookup_key.vpn == request_vpn;
                2'd1: level_hit = entry.lookup_key.vpn[51:9] == request_vpn[51:9];
                2'd2: level_hit = entry.lookup_key.vpn[51:18] == request_vpn[51:18];
                default: level_hit = entry.lookup_key.vpn[51:27] == request_vpn[51:27];
            endcase
        end

        // PtwSectorResp.hit() is the no-S2 path.  A normal 4-KB sector still
        // requires the response VPN's low sector bit to be valid; superpages
        // and NAPOT entries cover all sectors.
        addr_low_hit = napot_or_super || entry.valididx[request_vpn[2:0]];

        if (request_s2xlate == 2'd0) begin
            asid_hit = (entry.asid[15:0] == response_asid) || entry.pte_g;
            vmid_hit = 1'b1;
            return asid_hit && level_hit && addr_low_hit;
        end

        if (request_s2xlate == 2'd2) begin
            // HptwResp.hit() uses only VMID and G-stage tag/level.  Its raw
            // response has no ASID/global override.
            vmid_hit = entry.vmid[15:0] == response_vmid;
            return vmid_hit && level_hit;
        end

        // onlyStage1/allStage use PtwRespS2's combined S1 anchor.  The
        // response's S1 global bit allows an ASID change; VMID remains part of
        // the two-stage matcher.
        asid_hit = (entry.asid[15:0] == response_asid) || entry.pte_g;
        vmid_hit = entry.vmid[15:0] == response_vmid;
        return asid_hit && vmid_hit && level_hit;
    endfunction:entry_matches_request_raw

    // Abstract responsibility: decide whether a response payload is visible to
    // one UID under the CSR context that the DUT filter sees on this response
    // sample.  It does not mutate either the record or the live TLB entry.
    function bit entry_matches_uid_at_response(
        input memblock_tlb_entry entry,
        input memblock_uid_tlb_record record,
        input mmu_csr_runtime_state response_filter_csr_snapshot);
        if (entry == null || record == null ||
            response_filter_csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "entry_matches_uid_at_response got null input")
        end
        if (!record.is_waiting() || !record.request_fire_valid ||
            record.request_fire_sample_seq == 0) begin
            return 1'b0;
        end
        return entry_matches_request_raw(entry, record.vpn,
                                         record.s2xlate,
                                         response_filter_csr_snapshot);
    endfunction:entry_matches_uid_at_response

    // Abstract responsibility: multicast one observed L2TLB response to all
    // real-fire UID waiters whose raw key matches under response-visible C-2
    // CSR.  The token remains complete even when this returns zero.
    function int unsigned complete_waiting_uid_records_by_response(
        input memblock_tlb_entry entry,
        input mmu_csr_runtime_state response_filter_csr_snapshot);
        int unsigned match_count;

        if (entry == null || response_filter_csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "complete_waiting_uid_records_by_response got null input")
        end
        match_count = 0;
        foreach (uid_tlb_record_by_uid[uid]) begin
            memblock_uid_tlb_record record;

            record = uid_tlb_record_by_uid[uid];
            if (record == null || record.pte_valid ||
                !entry_matches_uid_at_response(entry, record,
                                               response_filter_csr_snapshot)) begin
                continue;
            end
            record.copy_entry_fields(entry);
            record.mark_completed();
            set_status_field(record.uid, MEMBLOCK_STATUS_TLB_MAPPED, 1'b1);
            match_count++;
        end
        if (match_count == 0) begin
            `uvm_info("COMMON_DATA",
                      $sformatf("no WAITING uid_tlb_record matches L2TLB response key vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d under response-visible CSR; allow prefetch/old-context response",
                                entry.lookup_key.vpn, entry.lookup_key.asid,
                                entry.lookup_key.vmid, entry.lookup_key.s2xlate),
                      UVM_LOW)
        end
        return match_count;
    endfunction:complete_waiting_uid_records_by_response

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
                       $sformatf("raw monitor queues are not drained at end_test_check: size=%0d",
                                 memblock_sync_pkg::raw_monitor_queue_size()))
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
        if (cancel_reconcile_pending() || has_pending_lsq_cancel_apply() ||
            pending_lq_cancel_count != 0 || pending_sq_cancel_count != 0 ||
            redirect_sample_anchor_pending() || cancel_snapshot_buffer_pending()) begin
            `uvm_error("COMMON_DATA",
                       $sformatf("LSQ cancel lifecycle is not drained: records=%0d apply=%0d pending=%0d/%0d anchor=%0d snapshot=%0d",
                                 cancel_record_q.size(), has_pending_lsq_cancel_apply(),
                                 pending_lq_cancel_count, pending_sq_cancel_count,
                                 redirect_anchor_history_q.size(),
                                 cancel_snapshot_history_q.size()))
        end
        if (!runtime_drain_complete()) begin
            `uvm_error("COMMON_DATA", "runtime drain predicate is not complete at end_test_check")
        end
    endfunction:end_test_check

endclass:common_data_transaction

`endif
