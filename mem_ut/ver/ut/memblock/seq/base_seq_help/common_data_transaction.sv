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

    typedef bit [53:0] memblock_uid_tlb_wait_shape_key_t;
    localparam bit [3:0] MEMBLOCK_SV39_MODE = 4'd8;
    localparam bit [3:0] MEMBLOCK_SV48_MODE = 4'd9;

    typedef struct {
        memblock_sfence_payload_t payload;
        longint unsigned          anchor_sample_seq;
        longint unsigned          due_sample_seq;
        longint unsigned          reset_epoch;
        longint unsigned          lifecycle_event_seq;
    } memblock_pending_sfence_invalidate_t;

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
    // 中文注释：secondary index 只将一个 raw range shape 映射到有限个
    // canonical anchor key。payload 仍只保存在 tlb_entry_by_key，查询时必须
    // 回到 canonical table 复核，不能把 payload 或 pending handle 存进 index。
    memblock_tlb_lookup_key_t
        tlb_anchor_keys_by_range_key[memblock_tlb_range_index_key_t][$];
    // 中文注释：adapter 成功消费 raw fence 后只在此登记 C4 删除工作。
    // 该队列不拥有 L2TLB token/UID；runtime reset 或 C4 delete 后由本类清除。
    memblock_pending_sfence_invalidate_t sfence_invalidate_pending_q[$];
    // 中文注释：live entry 的单调身份；普通 reset/flush 只清 table，不回退该计数器。
    longint unsigned         next_tlb_entry_generation;
    memblock_uid_tlb_record  uid_tlb_record_by_uid[memblock_uid_t];
    // WAITING UID 的 request-fire 候选索引。key 只包含 DTLB request 可见的
    // vpn/s2xlate；每个 bucket 受 DTLB filter 物理容量限制。
    memblock_uid_t uid_waiting_by_vpn_s2xlate[memblock_uid_tlb_wait_shape_key_t][$];
    mmu_csr_runtime_state    mmu_csr_state;
    // 中文注释：PMA/PMP model 是整个 testcase 的唯一运行期表 owner。表项只由
    // CSR monitor 的 raw write 回放更新；UID request-fire 冻结 generation 后，RM
    // 只能通过只读 API 查询对应历史快照，不能读取当前可变表。
    memblock_pma_pmp_model   pma_pmp_model;
    // 中文注释：已被 PMA/PMP model 回放的最后一个 generic CSR write sample。
    // 只允许单调增加；同 edge request 使用旧表，因此 sample 相等的写入保留给
    // 下一笔 request 才消费。
    longint unsigned         pma_pmp_last_applied_csr_sample;
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

    // 中文注释：控制标记只允许存在一个静态屏障。它在 control admission 时建立，
    // 在该 UID terminal_done 后由 control service 解除；普通 LSQ admission 不得越过它。
    bit            active_control_barrier_valid;
    memblock_uid_t active_control_barrier_uid;
    // 中文注释：当前唯一控制 SFence 的 C0/C4 adapter observation。写入：已有
    // L2TLB adapter 在 schedule/apply 边界；读取/清除：control barrier service。
    // 这两个槽不保存 generic fence，避免普通 SFence 干扰控制 owner。
    memblock_control_sfence_observation_t control_sfence_c0_observation;
    memblock_control_sfence_observation_t control_sfence_effective_observation;

    // 中文注释：控制 worker 的持久工作项和唤醒事件。service 是唯一 producer，
    // CSR/Fence worker 分别是唯一 consumer；event 只用于唤醒，queue 才是动作真源。
    memblock_csr_control_action_t      csr_control_action_q[$];
    memblock_sfence_control_action_t   sfence_control_action_q[$];
    memblock_l2_flush_release_request_t l2_flush_release_request_q[$];
    event                               csr_control_action_available_ev;
    event                               sfence_control_action_available_ev;
    event                               control_worker_shutdown_ev;
    // 中文注释：shutdown 由 service 在所有 control action drain 后请求；两个 worker
    // 分别确认退出，global stop 必须等确认齐备，避免空 worker 与终止条件循环依赖。
    bit                                 control_workers_shutdown_requested;
    bit                                 csr_control_worker_exited;
    bit                                 sfence_control_worker_exited;

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
    // 中文注释：attached 表示请求已经绑定到尚未 finish_item 的 lsqcommit xaction；
    // active 表示该 xaction 已由 driver sendover，才允许 sbIsEmpty 完成。
    memblock_flushsb_req_t      attached_flushsb_req;
    bit                         attached_flushsb_req_valid;
    // 当前已经随lsqcommit xaction drive到DUT、正在等待sbIsEmpty的请求备份。
    memblock_flushsb_req_t      active_flushsb_req;
    bit                         active_flushsb_req_valid;
    // 中文注释：owner 请求的 completed slot 有界保存一条 immutable completion；
    // service 必须按 owner+req_id 消费后清空，防止旧 high observation 误匹配下一请求。
    memblock_flushsb_completion_t flushsb_completed;
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
        attached_flushsb_req = '{default:'0};
        attached_flushsb_req_valid = 1'b0;
        active_flushsb_req  = '{default:'0};
        active_flushsb_req_valid = 1'b0;
        flushsb_completed = '{default:'0};
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
        pma_pmp_model = memblock_pma_pmp_model::get();
        pma_pmp_model.reset_and_init_v2_profile();
        pma_pmp_last_applied_csr_sample = 0;
        next_tlb_entry_generation = 0;
        sfence_invalidate_pending_q.delete();
        uid_waiting_by_vpn_s2xlate.delete();
        active_control_barrier_valid = 1'b0;
        active_control_barrier_uid = 0;
        control_sfence_c0_observation = '{default:'0};
        control_sfence_effective_observation = '{default:'0};
        csr_control_action_q.delete();
        sfence_control_action_q.delete();
        l2_flush_release_request_q.delete();
        control_workers_shutdown_requested = 1'b0;
        csr_control_worker_exited = 1'b0;
        sfence_control_worker_exited = 1'b0;
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
        attached_flushsb_req = '{default:'0};
        attached_flushsb_req_valid = 1'b0;
        active_flushsb_req  = '{default:'0};
        active_flushsb_req_valid = 1'b0;
        flushsb_completed = '{default:'0};
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
        tlb_anchor_keys_by_range_key.delete();
        sfence_invalidate_pending_q.delete();
        uid_tlb_record_by_uid.delete();
        clear_issue_queues();
        clear_feedback_events();
        clear_redirect_drive_queue();
        clear_ptw_wait_replay_queue();
        uid_by_active_rob.delete();
        uid_by_lq.delete();
        uid_by_sq.delete();
        active_control_barrier_valid = 1'b0;
        active_control_barrier_uid = 0;
        control_sfence_c0_observation = '{default:'0};
        control_sfence_effective_observation = '{default:'0};
        csr_control_action_q.delete();
        sfence_control_action_q.delete();
        l2_flush_release_request_q.delete();
        control_workers_shutdown_requested = 1'b0;
        csr_control_worker_exited = 1'b0;
        sfence_control_worker_exited = 1'b0;
        if (mmu_csr_state == null) begin
            mmu_csr_state = mmu_csr_runtime_state::type_id::create("mmu_csr_state");
        end
        mmu_csr_state.reset();
        if (pma_pmp_model == null) begin
            pma_pmp_model = memblock_pma_pmp_model::get();
        end
        pma_pmp_model.reset_and_init_v2_profile();
        pma_pmp_last_applied_csr_sample = 0;
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
                    redirect_anchor_history_q[anchor_idx].is_vls_exception !=
                        cancel_record_q[record_idx_for_anchor].redirect.is_vls_exception ||
                    redirect_anchor_history_q[anchor_idx].effective_level !=
                        memblock_redirect_effective_level(
                            cancel_record_q[record_idx_for_anchor].redirect) ||
                    redirect_anchor_history_q[anchor_idx].rob_flag !=
                        cancel_record_q[record_idx_for_anchor].redirect.rob_key.flag ||
                    redirect_anchor_history_q[anchor_idx].rob_value !=
                        cancel_record_q[record_idx_for_anchor].redirect.rob_key.value) begin
                    `uvm_fatal("MMIO_RESOLVE",
                               $sformatf("LOAD MMIO anchor FIFO mismatch sample=%0d record=%0d expected raw/effective/vls/rob=%0d/%0d/%0d/%0d/%0d observed=%0d/%0d/%0d/%0d/%0d",
                                         raw_sample_seq,
                                         cancel_record_q[record_idx_for_anchor].cancel_record_id,
                                         cancel_record_q[record_idx_for_anchor].redirect.level,
                                         memblock_redirect_effective_level(
                                             cancel_record_q[record_idx_for_anchor].redirect),
                                         cancel_record_q[record_idx_for_anchor].redirect.is_vls_exception,
                                         cancel_record_q[record_idx_for_anchor].redirect.rob_key.flag,
                                         cancel_record_q[record_idx_for_anchor].redirect.rob_key.value,
                                         redirect_anchor_history_q[anchor_idx].level,
                                         redirect_anchor_history_q[anchor_idx].effective_level,
                                         redirect_anchor_history_q[anchor_idx].is_vls_exception,
                                         redirect_anchor_history_q[anchor_idx].rob_flag,
                                         redirect_anchor_history_q[anchor_idx].rob_value))
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
    // recovery、LSQ cancel、flushSb 以及 active topology 的 worker shutdown 生命周期
    // 都收敛后才返回 1。
    function bit runtime_drain_complete();
        return memblock_sync_pkg::raw_monitor_queue_size() == 0 &&
               exception_event_q.size() == 0 &&
               load_issue_q.size() == 0 &&
               sta_issue_q.size() == 0 &&
               std_issue_q.size() == 0 &&
               uid_by_active_rob.num() == 0 &&
               uid_by_lq.num() == 0 &&
               uid_by_sq.num() == 0 &&
               !active_control_barrier_valid &&
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
               memblock_sync_pkg::lsq_timing_sideband_queue_size() == 0 &&
               (!memblock_sync_pkg::uses_control_barrier_topology() ||
                (control_action_drain_complete() &&
                 control_workers_shutdown_complete()));
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

    // 抽象职责：识别主表中的控制标记。它只读取静态 op_class，不推导普通访存
    // behavior，也不修改 admission、ROB 或状态表。
    function bit uid_is_control_marker(input memblock_uid_t uid);
        check_uid(uid, "uid_is_control_marker");
        return is_control_op_class(get_main_transaction(uid).op_class);
    endfunction:uid_is_control_marker

    // 抽象职责：控制静态屏障阻止其后的 UID admission；屏障自身用于 redirect 后
    // 恢复连续 admission 前缀时仍可被重新经过一次。
    function bit control_barrier_blocks_admission(input memblock_uid_t uid);
        if (!active_control_barrier_valid) begin
            return 1'b0;
        end
        return uid > active_control_barrier_uid;
    endfunction:control_barrier_blocks_admission

    // 抽象职责：为 CSR/SFence/check_store 建立不占 LSQ 的 active ROB 实例与静态屏障。
    // 它只复用公共 ROB map/admission prefix；不分配 LQ/SQ、不产生 issue work。
    function void activate_control_uid(input memblock_uid_t uid);
        main_control_transaction main_tr;
        status_transaction       status;
        memblock_control_kind_e  expected_kind;

        check_uid(uid, "activate_control_uid");
        if (get_next_new_admit_uid() != uid) begin
            `uvm_fatal("CONTROL_ADMISSION",
                       $sformatf("control admission uid=%0d is not the next prefix uid=%0d",
                                 uid, get_next_new_admit_uid()))
        end
        if (active_control_barrier_valid) begin
            `uvm_fatal("CONTROL_ADMISSION",
                       $sformatf("cannot admit control uid=%0d while barrier uid=%0d is active",
                                 uid, active_control_barrier_uid))
        end
        main_tr = get_main_transaction(uid);
        status = get_status(uid);
        if (!is_control_op_class(main_tr.op_class) || status.active || status.enq ||
            status.terminal_done) begin
            `uvm_fatal("CONTROL_ADMISSION",
                       $sformatf("invalid fresh control admission uid=%0d op=%0d active/enq/done=%0d/%0d/%0d",
                                 uid, main_tr.op_class, status.active, status.enq,
                                 status.terminal_done))
        end
        expected_kind = control_kind_from_op_class(main_tr.op_class);
        if (status.control_kind != expected_kind ||
            status.control_state != MEMBLOCK_CONTROL_STATE_NONE ||
            main_tr.lsq_flow != MEMBLOCK_LSQ_FLOW_NONE || main_tr.numLsElem != 0 ||
            main_tr.lqIdx_flag || main_tr.lqIdx_value != '0 ||
            main_tr.sqIdx_flag || main_tr.sqIdx_value != '0) begin
            `uvm_fatal("CONTROL_ADMISSION",
                       $sformatf("control uid=%0d has non-neutral static state kind/state=%0d/%0d",
                                 uid, status.control_kind, status.control_state))
        end

        activate_uid(uid, 1'b0, 1'b0);
        set_status_field(uid, MEMBLOCK_STATUS_ENQ, 1'b1);
        set_status_field(uid, MEMBLOCK_STATUS_ISSUE_READY, 1'b1);
        set_status_field(uid, MEMBLOCK_STATUS_LSQ_DEQ, 1'b1);
        status.control_state = MEMBLOCK_CONTROL_STATE_WAIT_OLDER_ROB_COMMIT;
        status.control_owner.valid = 1'b0;
        status.control_owner.uid = 0;
        status.control_owner.dynamic_epoch = 0;
        status.control_owner.action_generation = 0;
        status.control_owner.kind = MEMBLOCK_CONTROL_KIND_NONE;
        status.control_action_generation = 0;
        status.control_action_enqueued = 1'b0;
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        active_control_barrier_uid = uid;
        active_control_barrier_valid = 1'b1;
    endfunction:activate_control_uid

    // 抽象职责：redirect 回退后重新跨过已保留的静态控制标记，只恢复 admission
    // prefix，不重建 active ROB、不分配新 owner，也不修改 dynamic_epoch。
    function void restore_control_admission_prefix(input memblock_uid_t uid);
        status_transaction status;

        check_uid(uid, "restore_control_admission_prefix");
        status = get_status(uid);
        if (!active_control_barrier_valid || active_control_barrier_uid != uid ||
            get_next_new_admit_uid() != uid || !uid_is_control_marker(uid) ||
            !status.active || !status.enq || status.terminal_done ||
            status.control_state != MEMBLOCK_CONTROL_STATE_WAIT_OLDER_ROB_COMMIT ||
            status.control_action_enqueued || status.control_owner.valid) begin
            `uvm_fatal("CONTROL_ADMISSION",
                       $sformatf("cannot restore static control prefix uid=%0d barrier=%0d/%0d active/enq/done=%0d/%0d/%0d state=%0d",
                                 uid, active_control_barrier_valid,
                                 active_control_barrier_uid, status.active, status.enq,
                                 status.terminal_done, status.control_state))
        end
        mark_uid_enqueued(uid);
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    endfunction:restore_control_admission_prefix

    // 抽象职责：确认 redirect 命中的是尚未拥有动作实例的静态控制标记。该 helper
    // 不清状态、不撤 ROB map、不计入 LSQ cancel，也不推进 dynamic_epoch。
    function void preserve_static_control_marker_on_redirect(
        input memblock_uid_t uid,
        input memblock_redirect_payload_t redirect
    );
        status_transaction status;

        check_uid(uid, "preserve_static_control_marker_on_redirect");
        status = get_status(uid);
        if (!redirect.valid || !uid_is_control_marker(uid) ||
            !active_control_barrier_valid || active_control_barrier_uid != uid ||
            !status.active || !status.enq || status.terminal_done ||
            status.control_state != MEMBLOCK_CONTROL_STATE_WAIT_OLDER_ROB_COMMIT ||
            status.control_action_enqueued || status.control_owner.valid) begin
            `uvm_fatal("CONTROL_REDIRECT",
                       $sformatf("redirect cannot preserve control uid=%0d state=%0d owner_valid=%0d action_enqueued=%0d",
                                 uid, status.control_state, status.control_owner.valid,
                                 status.control_action_enqueued))
        end
    endfunction:preserve_static_control_marker_on_redirect

    // 抽象职责：控制 worker 在 terminal_done 已被公共 commit/retire 固化后解除 barrier，
    // 使后续 UID 恢复 admission；它不负责 commit 或状态推进。
    function void release_control_barrier_after_terminal(input memblock_uid_t uid);
        status_transaction status;

        check_uid(uid, "release_control_barrier_after_terminal");
        status = get_status(uid);
        if (!active_control_barrier_valid || active_control_barrier_uid != uid ||
            !uid_is_control_marker(uid) || !status.terminal_done || status.active) begin
            `uvm_fatal("CONTROL_BARRIER",
                       $sformatf("cannot release barrier uid=%0d active_barrier=%0d/%0d terminal/active=%0d/%0d",
                                 uid, active_control_barrier_valid,
                                 active_control_barrier_uid, status.terminal_done,
                                 status.active))
        end
        active_control_barrier_valid = 1'b0;
        active_control_barrier_uid = 0;
    endfunction:release_control_barrier_after_terminal

    // 抽象职责：清除本 testcase 尚未交付的 control worker 工作项和 shutdown 回执。
    // 建表或 control runtime reset 的唯一调用者使用它建立新的控制代际；它不改主表、
    // status、ROB 或 flushSb normal request，避免把运行期动作清理误当成重建主表。
    function void reset_control_action_runtime();
        csr_control_action_q.delete();
        sfence_control_action_q.delete();
        l2_flush_release_request_q.delete();
        control_sfence_c0_observation = '{default:'0};
        control_sfence_effective_observation = '{default:'0};
        control_workers_shutdown_requested = 1'b0;
        csr_control_worker_exited = 1'b0;
        sfence_control_worker_exited = 1'b0;
    endfunction:reset_control_action_runtime

    // 抽象职责：持久化一个已绑定 owner 的 CSR 工作项，再唤醒 CSR worker。
    // event 丢失不会丢动作，因为 worker 每次醒来都重新检查 queue；本函数不推进
    // status，调用者必须先完成 owner/state 校验和状态写入。
    function void enqueue_csr_control_action(input memblock_csr_control_action_t action);
        if (!action.owner.valid) begin
            `uvm_fatal("CONTROL_ACTION", "enqueue_csr_control_action got invalid owner")
        end
        if (control_workers_shutdown_requested || csr_control_worker_exited) begin
            `uvm_fatal("CONTROL_ACTION", "cannot enqueue CSR action after worker shutdown/exited")
        end
        csr_control_action_q.push_back(action);
        ->csr_control_action_available_ev;
    endfunction:enqueue_csr_control_action

    // 抽象职责：CSR worker 从 FIFO 取出唯一待交付 action；为空时只返回 0，
    // 不等待 event，避免把 queue state 与 event 唤醒语义混为一体。
    function bit try_pop_csr_control_action(output memblock_csr_control_action_t action);
        if (csr_control_action_q.size() == 0) begin
            action = '{default:'0};
            return 1'b0;
        end
        action = csr_control_action_q.pop_front();
        return 1'b1;
    endfunction:try_pop_csr_control_action

    // 抽象职责：记录 CSR worker 的 driver sendover，而非 monitor 完成。该 helper
    // 归档本 action 的 expected runtime 字段与 drive 前 snapshot 序号，供 service 在
    // 后续 monitor sample 中验证；它不提前写 runtime completion snapshot。
    function void mark_csr_control_sendover(input memblock_csr_control_action_t action);
        status_transaction status;

        if (!action.owner.valid ||
            action.completion_profile != MEMBLOCK_CONTROL_COMPLETION_RUNTIME_CSR_SNAPSHOT ||
            !action.expected_runtime_csr_valid) begin
            `uvm_fatal("CONTROL_ACTION", "invalid CSR action at sendover")
        end
        status = get_status(action.owner.uid);
        if (!memblock_control_owner_equal(status.control_owner, action.owner) ||
            status.control_state != MEMBLOCK_CONTROL_STATE_CSR_CONFIG_PENDING ||
            !status.control_action_enqueued) begin
            `uvm_fatal("CONTROL_ACTION",
                       $sformatf("CSR sendover owner/state mismatch uid=%0d state=%0d enqueued=%0d",
                                 action.owner.uid, status.control_state,
                                 status.control_action_enqueued))
        end
        status.control_expected_runtime_csr_valid = 1'b1;
        status.control_expected_runtime_csr = action.expected_runtime_csr;
        status.control_runtime_snapshot_seq_before_drive =
            action.runtime_snapshot_seq_before_drive;
        status.control_action_enqueued = 1'b0;
        status.control_state = MEMBLOCK_CONTROL_STATE_CSR_SENDOVER;
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    endfunction:mark_csr_control_sendover

    // 抽象职责：在 check_store ASSERT 的 finish_item() 返回后冻结 done observation
    // 下界，并把状态交给 service 等待 monitor 已观察到的新 high。driver 已经建立
    // 私有 high hold；这里不读取或修改 driver 状态，也不把 sendover 当作 done-high。
    function void mark_l2_flush_assert_sendover(
        input memblock_csr_control_action_t action
    );
        status_transaction status;
        memblock_sync_pkg::memblock_control_level_observation_t observation;

        if (!action.owner.valid || !action.csr_baseline_valid ||
            action.completion_profile != MEMBLOCK_CONTROL_COMPLETION_L2_FLUSH_LEVEL ||
            action.l2_flush_phase != MEMBLOCK_L2_FLUSH_PHASE_ASSERT ||
            action.control_reset_epoch == 0 ||
            !memblock_sync_pkg::get_latest_control_l2_flush_done_observation(observation)) begin
            `uvm_fatal("CONTROL_ACTION", "invalid L2 flush ASSERT sendover")
        end
        status = get_status(action.owner.uid);
        if (!memblock_control_owner_equal(status.control_owner, action.owner) ||
            status.control_state != MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_ASSERT ||
            !status.control_action_enqueued ||
            status.control_reset_epoch != action.control_reset_epoch) begin
            `uvm_fatal("CONTROL_ACTION",
                       $sformatf("L2 ASSERT sendover owner/state mismatch uid=%0d state=%0d enqueued=%0d",
                                 action.owner.uid, status.control_state,
                                 status.control_action_enqueued))
        end
        status.control_l2_csr_baseline_valid = 1'b1;
        status.control_l2_csr_baseline = action.csr_baseline;
        status.control_assert_done_baseline_seq = observation.observation_seq;
        status.control_action_enqueued = 1'b0;
        status.control_state = MEMBLOCK_CONTROL_STATE_WAIT_L2_FLUSH_DONE;
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    endfunction:mark_l2_flush_assert_sendover

    // 抽象职责：在 check_store RELEASE 的 finish_item() 返回后冻结 done-low 的
    // observation 下界。只有 service 后续观察到更新后的低电平才能完成 control marker。
    function void mark_l2_flush_release_sendover(
        input memblock_l2_flush_release_request_t request
    );
        status_transaction status;
        memblock_sync_pkg::memblock_control_level_observation_t observation;

        if (!request.owner.valid || request.control_reset_epoch == 0 ||
            !memblock_sync_pkg::get_latest_control_l2_flush_done_observation(observation)) begin
            `uvm_fatal("CONTROL_ACTION", "invalid L2 flush RELEASE sendover")
        end
        status = get_status(request.owner.uid);
        if (!memblock_control_owner_equal(status.control_owner, request.owner) ||
            status.control_state != MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_RELEASE ||
            !status.control_l2_csr_baseline_valid ||
            status.control_reset_epoch != request.control_reset_epoch) begin
            `uvm_fatal("CONTROL_ACTION",
                       $sformatf("L2 RELEASE sendover owner/state mismatch uid=%0d state=%0d",
                                 request.owner.uid, status.control_state))
        end
        status.control_release_done_baseline_seq = observation.observation_seq;
        status.control_state = MEMBLOCK_CONTROL_STATE_WAIT_L2_FLUSH_IDLE;
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    endfunction:mark_l2_flush_release_sendover

    // 抽象职责：持久化 SFence action 并唤醒唯一 fence worker。token 含 C0 arm
    // 前的基线字段；本函数不读取 monitor event，也不把 event 当作 action 真源。
    function void enqueue_sfence_control_action(input memblock_sfence_control_action_t action);
        if (!action.owner.valid) begin
            `uvm_fatal("CONTROL_ACTION", "enqueue_sfence_control_action got invalid owner")
        end
        if (control_workers_shutdown_requested || sfence_control_worker_exited) begin
            `uvm_fatal("CONTROL_ACTION", "cannot enqueue SFence action after worker shutdown/exited")
        end
        sfence_control_action_q.push_back(action);
        ->sfence_control_action_available_ev;
    endfunction:enqueue_sfence_control_action

    function bit try_pop_sfence_control_action(output memblock_sfence_control_action_t action);
        if (sfence_control_action_q.size() == 0) begin
            action = '{default:'0};
            return 1'b0;
        end
        action = sfence_control_action_q.pop_front();
        return 1'b1;
    endfunction:try_pop_sfence_control_action

    // 抽象职责：在 SFence start_item() 之前登记 C0 匹配资格和 immutable event baseline。
    // Fence monitor 与 driver 可同拍运行，因此不得把这一步放到 finish_item() 返回后。
    function void arm_sfence_control_c0_match(
        input memblock_sfence_control_action_t action
    );
        status_transaction status;

        if (!action.owner.valid || !action.sfence_c0_match_armed) begin
            `uvm_fatal("CONTROL_ACTION", "cannot arm invalid SFence action")
        end
        status = get_status(action.owner.uid);
        if (!memblock_control_owner_equal(status.control_owner, action.owner) ||
            status.control_state != MEMBLOCK_CONTROL_STATE_SFENCE_REQ ||
            !status.control_action_enqueued) begin
            `uvm_fatal("CONTROL_ACTION",
                       $sformatf("SFence C0 arm owner/state mismatch uid=%0d state=%0d enqueued=%0d",
                                 action.owner.uid, status.control_state,
                                 status.control_action_enqueued))
        end
        status.control_expected_sfence_valid = 1'b1;
        status.control_expected_sfence = action.expected_fence;
        status.control_sfence_c0_armed = 1'b1;
        status.control_sfence_pre_drive_event_seq = action.pre_drive_event_seq;
        status.control_l2tlb_reset_epoch_at_arm = action.l2tlb_reset_epoch_at_arm;
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    endfunction:arm_sfence_control_c0_match

    // 抽象职责：记录 SFence driver sendover。C0/C4 仍由 adapter/monitor 事实推进，
    // 此处仅把状态从已 arm 的 SFENCE_REQ 转成可消费 C0 的 SFENCE_SENDOVER。
    function void mark_sfence_control_sendover(
        input memblock_sfence_control_action_t action
    );
        status_transaction status;

        if (!action.owner.valid || !action.sfence_c0_match_armed) begin
            `uvm_fatal("CONTROL_ACTION", "invalid SFence action at sendover")
        end
        status = get_status(action.owner.uid);
        if (!memblock_control_owner_equal(status.control_owner, action.owner) ||
            status.control_state != MEMBLOCK_CONTROL_STATE_SFENCE_REQ ||
            !status.control_action_enqueued) begin
            `uvm_fatal("CONTROL_ACTION",
                       $sformatf("SFence sendover owner/state mismatch uid=%0d state=%0d enqueued=%0d",
                                 action.owner.uid, status.control_state,
                                 status.control_action_enqueued))
        end
        status.control_action_enqueued = 1'b0;
        status.control_state = MEMBLOCK_CONTROL_STATE_SFENCE_SENDOVER;
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    endfunction:mark_sfence_control_sendover

    // 抽象职责：把 check_store 的 done-high 转换为 owner 化 RELEASE 请求。CSR worker
    // 优先消费该 queue，确保同一 sequencer 在 high hold 期间不会误交付普通 CSR action。
    function void enqueue_l2_flush_release_request(
        input memblock_l2_flush_release_request_t request
    );
        if (!request.owner.valid || request.control_reset_epoch == 0) begin
            `uvm_fatal("CONTROL_ACTION", "enqueue_l2_flush_release_request got invalid request")
        end
        if (l2_flush_release_request_q.size() != 0) begin
            `uvm_fatal("CONTROL_ACTION", "only one active L2 flush RELEASE request is supported")
        end
        l2_flush_release_request_q.push_back(request);
        ->csr_control_action_available_ev;
    endfunction:enqueue_l2_flush_release_request

    function bit try_pop_l2_flush_release_request(
        output memblock_l2_flush_release_request_t request
    );
        if (l2_flush_release_request_q.size() == 0) begin
            request = '{default:'0};
            return 1'b0;
        end
        request = l2_flush_release_request_q.pop_front();
        return 1'b1;
    endfunction:try_pop_l2_flush_release_request

    // 抽象职责：service 在 terminal prefix 和 control action drain 均完成后请求两个
    // worker 退出；worker 醒来后还会复查 queue，避免 shutdown/event 与最后 token 竞态。
    function void request_control_worker_shutdown();
        if (control_workers_shutdown_requested) begin
            return;
        end
        control_workers_shutdown_requested = 1'b1;
        ->control_worker_shutdown_ev;
        ->csr_control_action_available_ev;
        ->sfence_control_action_available_ev;
    endfunction:request_control_worker_shutdown

    // 抽象职责：给 global stop 前的 worker shutdown 提供不含 worker ack 的控制动作
    // 收敛谓词。它不扫描 status 表；terminal 前缀由调用者先证明，这里只确认没有
    // barrier、action、owner flushSb 或 RELEASE 仍可能唤醒 worker。
    function bit control_action_drain_complete();
        return !active_control_barrier_valid &&
               csr_control_action_q.size() == 0 &&
               sfence_control_action_q.size() == 0 &&
               l2_flush_release_request_q.size() == 0 &&
               !flushsb_request_pending() &&
               !flushsb_completed.valid;
    endfunction:control_action_drain_complete

    function bit control_worker_can_exit(input bit is_csr_worker);
        if (!control_workers_shutdown_requested) begin
            return 1'b0;
        end
        if (is_csr_worker) begin
            return csr_control_action_q.size() == 0 &&
                   l2_flush_release_request_q.size() == 0;
        end
        return sfence_control_action_q.size() == 0;
    endfunction:control_worker_can_exit

    function void mark_control_worker_exited(input bit is_csr_worker);
        if (!control_workers_shutdown_requested) begin
            `uvm_fatal("CONTROL_ACTION", "worker exited before shutdown was requested")
        end
        if (is_csr_worker) begin
            csr_control_worker_exited = 1'b1;
        end else begin
            sfence_control_worker_exited = 1'b1;
        end
    endfunction:mark_control_worker_exited

    function bit control_workers_shutdown_complete();
        return control_workers_shutdown_requested &&
               csr_control_worker_exited && sfence_control_worker_exited;
    endfunction:control_workers_shutdown_complete

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
                anchor.is_vls_exception !=
                    cancel_record_q[record_idx].redirect.is_vls_exception ||
                anchor.effective_level !=
                    memblock_redirect_effective_level(cancel_record_q[record_idx].redirect) ||
                anchor.rob_flag != cancel_record_q[record_idx].redirect.rob_key.flag ||
                anchor.rob_value != cancel_record_q[record_idx].redirect.rob_key.value) begin
                `uvm_fatal("LSQ_CANCEL",
                           $sformatf("redirect anchor FIFO mismatch record=%0d expected raw/effective/vls/rob=%0d/%0d/%0d/%0d/%0d observed=%0d/%0d/%0d/%0d/%0d",
                                     cancel_record_q[record_idx].cancel_record_id,
                                     cancel_record_q[record_idx].redirect.level,
                                     memblock_redirect_effective_level(cancel_record_q[record_idx].redirect),
                                     cancel_record_q[record_idx].redirect.is_vls_exception,
                                     cancel_record_q[record_idx].redirect.rob_key.flag,
                                     cancel_record_q[record_idx].redirect.rob_key.value,
                                     anchor.level, anchor.effective_level, anchor.is_vls_exception,
                                     anchor.rob_flag, anchor.rob_value))
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
            redirect.is_vls_exception != cancel_record_q[record_idx].redirect.is_vls_exception ||
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
        if (uid_is_control_marker(uid)) begin
            `uvm_fatal("CONTROL_REDIRECT",
                       $sformatf("control uid=%0d must use static preserve or fatal redirect handling", uid))
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
               left.is_vls_exception == right.is_vls_exception &&
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
                if (uid_is_control_marker(uid)) begin
                    if (status.control_state ==
                        MEMBLOCK_CONTROL_STATE_WAIT_OLDER_ROB_COMMIT) begin
                        preserve_static_control_marker_on_redirect(uid, redirect);
                        continue;
                    end
                    `uvm_fatal("CONTROL_REDIRECT",
                               $sformatf("redirect covers started control uid=%0d state=%0d; the barrier forbids this recovery",
                                         uid, status.control_state))
                end
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

        if (uid_is_control_marker(uid)) begin
            `uvm_fatal("CONTROL_COMMIT",
                       $sformatf("control uid=%0d must use try_retire_control_committed_uid", uid))
        end
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

    // 抽象职责：完成已获 control commit 的无 LSQ 标记。它不检查普通 writeback、
    // pass 或 issue target；控制 service 先把状态推进到 CONTROL_COMMIT_READY。
    function void try_retire_control_committed_uid(input memblock_uid_t uid);
        status_transaction status;

        check_uid(uid, "try_retire_control_committed_uid");
        if (!uid_is_control_marker(uid)) begin
            `uvm_fatal("CONTROL_COMMIT",
                       $sformatf("control retire got ordinary uid=%0d", uid))
        end
        status = get_status(uid);
        if (!status.active || !status.rob_commit ||
            status.control_state != MEMBLOCK_CONTROL_STATE_CONTROL_COMMIT_READY) begin
            return;
        end
        if (!active_control_barrier_valid || active_control_barrier_uid != uid ||
            status.active_lq_mapped || status.active_sq_mapped ||
            status.redirect_pending || status.flushed || status.issue_killed ||
            active_redirect.valid) begin
            `uvm_fatal("CONTROL_COMMIT",
                       $sformatf("invalid control retire uid=%0d barrier=%0d/%0d lq/sq=%0d/%0d redirect=%0d/%0d/%0d",
                                 uid, active_control_barrier_valid,
                                 active_control_barrier_uid,
                                 status.active_lq_mapped,
                                 status.active_sq_mapped,
                                 status.redirect_pending, status.flushed,
                                 active_redirect.valid))
        end
        set_status_field(uid, MEMBLOCK_STATUS_SUCCESS, 1'b1);
        set_status_field(uid, MEMBLOCK_STATUS_TERMINAL_DONE, 1'b1);
        retire_active_uid(uid);
    endfunction:try_retire_control_committed_uid

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
        entry.s2xlate    = key.s2xlate;
        tlb_entry_by_key[key] = entry;
    endfunction:insert_tlb_entry

    // Abstract responsibility: make one secondary-index key from a raw
    // response shape.  It applies exactly the level/NAPOT mask consumed by
    // the ordinary raw matcher and does not access either table.
    function memblock_tlb_range_index_key_t make_tlb_range_index_key(
        input memblock_tlb_range_kind_e range_kind,
        input bit [1:0] s2xlate,
        input bit asid_global,
        input bit [15:0] asid,
        input bit [15:0] vmid,
        input bit [1:0] level,
        input bit napot,
        input bit [51:0] vpn);
        memblock_tlb_range_index_key_t key;

        if (napot && level != 2'd0) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("range index cannot use NAPOT with non-zero level=%0d", level))
        end
        key = '{default:'0};
        key.range_kind = range_kind;
        key.s2xlate = s2xlate;
        key.asid_global = asid_global;
        key.asid = asid_global ? '0 : asid;
        key.vmid = vmid;
        key.level = level;
        key.napot = napot;
        key.normalized_vpn = vpn;
        if (napot) begin
            key.normalized_vpn[3:0] = '0;
        end
        else begin
            case (level)
                2'd0: begin end
                2'd1: key.normalized_vpn[8:0] = '0;
                2'd2: key.normalized_vpn[17:0] = '0;
                2'd3: key.normalized_vpn[26:0] = '0;
                default: begin
                    `uvm_fatal("COMMON_DATA", "invalid L2TLB range level")
                end
            endcase
        end
        return key;
    endfunction:make_tlb_range_index_key

    // Abstract responsibility: enumerate the finite raw-hit shape keys owned
    // by one completed canonical entry.  It is pure with respect to table and
    // index state; registration and deletion are handled by separate helpers.
    function void build_entry_range_index_keys(
        input memblock_tlb_entry entry,
        output memblock_tlb_range_index_key_t keys[$]);
        bit [51:0] raw_anchor_vpn;
        bit [1:0]  effective_level;
        bit        effective_napot;
        bit        use_napot;
        bit        asid_global;
        bit [15:0] indexed_asid;
        bit [15:0] indexed_vmid;

        keys.delete();
        if (entry == null || entry.entry_generation == 0) begin
            `uvm_fatal("COMMON_DATA", "build_entry_range_index_keys got invalid entry")
        end
        entry.check_inactive_stage_defaults("RANGE_INDEX_BUILD");
        entry.validate_s1_sector_payload_consistency("RANGE_INDEX_BUILD");
        case (entry.s2xlate)
            2'd0: begin
                if (!entry.s1_stage_active || entry.s2_stage_active) begin
                    `uvm_fatal("COMMON_DATA", "noS2xlate range entry has invalid stage shape")
                end
                asid_global = entry.s1_pte_g;
                indexed_asid = entry.s1_asid;
                indexed_vmid = '0;
                if (entry.s1_level == 2'd0 && !entry.s1_pte_n) begin
                    foreach (entry.s1_valididx[idx]) begin
                        if (entry.s1_valididx[idx]) begin
                            raw_anchor_vpn = {14'b0, entry.s1_tag, idx[2:0]};
                            keys.push_back(make_tlb_range_index_key(
                                MEMBLOCK_TLB_RANGE_KIND_S1, entry.s2xlate,
                                asid_global, indexed_asid, indexed_vmid,
                                2'd0, 1'b0, raw_anchor_vpn));
                        end
                    end
                end
                else begin
                    raw_anchor_vpn = {14'b0, entry.s1_tag, 3'b000};
                    use_napot = entry.s1_level == 2'd0 && entry.s1_pte_n;
                    keys.push_back(make_tlb_range_index_key(
                        MEMBLOCK_TLB_RANGE_KIND_S1, entry.s2xlate,
                        asid_global, indexed_asid, indexed_vmid,
                        entry.s1_level, use_napot, raw_anchor_vpn));
                end
            end
            2'd1: begin
                if (!entry.s1_stage_active || entry.s2_stage_active) begin
                    `uvm_fatal("COMMON_DATA", "onlyStage1 range entry has invalid stage shape")
                end
                raw_anchor_vpn = {14'b0, entry.s1_tag, entry.s1_addr_low};
                use_napot = entry.s1_level == 2'd0 && entry.s1_pte_n;
                keys.push_back(make_tlb_range_index_key(
                    MEMBLOCK_TLB_RANGE_KIND_S1, entry.s2xlate,
                    entry.s1_pte_g, entry.s1_asid, entry.s1_vmid,
                    entry.s1_level, use_napot, raw_anchor_vpn));
            end
            2'd2: begin
                if (entry.s1_stage_active || !entry.s2_stage_active) begin
                    `uvm_fatal("COMMON_DATA", "onlyStage2 range entry has invalid stage shape")
                end
                raw_anchor_vpn = {14'b0, entry.s2_tag};
                use_napot = entry.s2_level == 2'd0 && entry.s2_pte_n;
                keys.push_back(make_tlb_range_index_key(
                    MEMBLOCK_TLB_RANGE_KIND_S2, entry.s2xlate,
                    1'b0, '0, entry.s2_vmid, entry.s2_level,
                    use_napot, raw_anchor_vpn));
            end
            2'd3: begin
                if (!entry.s1_stage_active || !entry.s2_stage_active) begin
                    `uvm_fatal("COMMON_DATA", "allStage range entry has invalid stage shape")
                end
                derive_allstage_lookup_shape(entry, effective_level, effective_napot);
                raw_anchor_vpn = {14'b0, entry.s1_tag, entry.s1_addr_low};
                use_napot = effective_level == 2'd0 && effective_napot;
                keys.push_back(make_tlb_range_index_key(
                    MEMBLOCK_TLB_RANGE_KIND_ALLSTAGE, entry.s2xlate,
                    entry.s1_pte_g, entry.s1_asid, entry.s1_vmid,
                    effective_level, use_napot, raw_anchor_vpn));
            end
            default: begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("unsupported range entry s2xlate=%0d", entry.s2xlate))
            end
        endcase
        if (keys.size() == 0) begin
            `uvm_fatal("COMMON_DATA", "canonical entry produced no range index key")
        end
    endfunction:build_entry_range_index_keys

    // Abstract responsibility: validate the normal-leaf NAPOT encoding before
    // a new canonical entry becomes discoverable through the range index.  It
    // never repairs raw fields and deliberately leaves fault passthrough alone.
    function void validate_normal_napot_payload(
        input memblock_tlb_lookup_key_t anchor_key,
        input memblock_tlb_entry entry);
        bit [3:0] s1_napot_low;

        if (entry == null || entry.lookup_key != anchor_key) begin
            `uvm_fatal("COMMON_DATA", "validate_normal_napot_payload got inconsistent anchor entry")
        end
        if (entry.has_effective_fault()) begin
            return;
        end
        if (entry.s1_stage_active && entry.s1_pte_n) begin
            entry.validate_s1_sector_payload_consistency("NAPOT_VALIDATE");
            if (entry.s1_level != 2'd0) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("S1 normal NAPOT has non-zero level=%0d anchor vpn=0x%0h",
                                     entry.s1_level, anchor_key.vpn))
            end
            s1_napot_low = {entry.s1_entry_ppn_raw[0],
                            entry.s1_ppn_low[entry.s1_addr_low]};
            if (entry.s1_pte_mode_at_build ==
                    memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL &&
                s1_napot_low != 4'b1000) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("LEGAL S1 NAPOT encoding invalid anchor vpn=0x%0h low=0x%0h",
                                     anchor_key.vpn, s1_napot_low))
            end
        end
        if (entry.s2_stage_active && entry.s2_pte_n) begin
            if (entry.s2_level != 2'd0) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("S2 normal NAPOT has non-zero level=%0d anchor vpn=0x%0h",
                                     entry.s2_level, anchor_key.vpn))
            end
            if (entry.s2_pte_mode_at_build ==
                    memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL &&
                entry.s2_entry_ppn_raw[3:0] != 4'b1000) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("LEGAL S2 NAPOT encoding invalid anchor vpn=0x%0h low=0x%0h",
                                     anchor_key.vpn, entry.s2_entry_ppn_raw[3:0]))
            end
        end
    endfunction:validate_normal_napot_payload

    // Abstract responsibility: atomically publish all finite range buckets for
    // one canonical entry after the entry is already in the live table.
    function bit register_tlb_range_index(
        input memblock_tlb_lookup_key_t anchor_key,
        input memblock_tlb_entry entry);
        memblock_tlb_range_index_key_t keys[$];
        int key_idx;
        int prior_idx;
        int bucket_idx;
        bit duplicate_anchor;

        if (entry == null || !tlb_entry_by_key.exists(anchor_key) ||
            tlb_entry_by_key[anchor_key] != entry ||
            entry.lookup_key != anchor_key || entry.entry_generation == 0) begin
            `uvm_fatal("COMMON_DATA", "range-index registration got non-canonical entry")
        end
        if (entry.range_index_keys.size() != 0) begin
            `uvm_fatal("COMMON_DATA", "range-index registration repeated for canonical entry")
        end
        validate_normal_napot_payload(anchor_key, entry);
        build_entry_range_index_keys(entry, keys);

        // Validate the complete publication set before changing any bucket, so
        // a failure cannot leave a half-registered live entry behind.
        foreach (keys[key_idx]) begin
            for (prior_idx = 0; prior_idx < key_idx; prior_idx++) begin
                if (keys[prior_idx] == keys[key_idx]) begin
                    `uvm_fatal("COMMON_DATA", "canonical entry produced duplicate range index key")
                end
            end
            duplicate_anchor = 1'b0;
            if (tlb_anchor_keys_by_range_key.exists(keys[key_idx])) begin
                if (tlb_anchor_keys_by_range_key[keys[key_idx]].size() >=
                    MEMBLOCK_TLB_RANGE_CANDIDATE_MAX) begin
                    `uvm_fatal("COMMON_DATA",
                               $sformatf("range index bucket exceeds max=%0d", MEMBLOCK_TLB_RANGE_CANDIDATE_MAX))
                end
                foreach (tlb_anchor_keys_by_range_key[keys[key_idx]][bucket_idx]) begin
                    if (tlb_lookup_key_equal(
                            tlb_anchor_keys_by_range_key[keys[key_idx]][bucket_idx],
                            anchor_key)) begin
                        duplicate_anchor = 1'b1;
                    end
                end
            end
            if (duplicate_anchor) begin
                `uvm_fatal("COMMON_DATA", "canonical anchor is already registered in range index bucket")
            end
        end
        foreach (keys[key_idx]) begin
            tlb_anchor_keys_by_range_key[keys[key_idx]].push_back(anchor_key);
            entry.range_index_keys.push_back(keys[key_idx]);
        end
        return 1'b1;
    endfunction:register_tlb_range_index

    // Abstract responsibility: remove exactly the buckets previously published
    // by one canonical entry.  It uses the entry-owned key list rather than
    // re-deriving shapes or scanning the live table during deletion.
    function void unregister_tlb_range_index(
        input memblock_tlb_lookup_key_t anchor_key,
        input memblock_tlb_entry entry);
        int key_idx;
        int bucket_idx;
        int unsigned removed_count;
        memblock_tlb_range_index_key_t range_key;

        if (entry == null || entry.lookup_key != anchor_key ||
            entry.range_index_keys.size() == 0) begin
            `uvm_fatal("COMMON_DATA", "range-index unregistration got invalid canonical entry")
        end
        foreach (entry.range_index_keys[key_idx]) begin
            range_key = entry.range_index_keys[key_idx];
            if (!tlb_anchor_keys_by_range_key.exists(range_key)) begin
                `uvm_fatal("COMMON_DATA", "range-index unregistration lost bucket")
            end
            removed_count = 0;
            for (bucket_idx = int'(tlb_anchor_keys_by_range_key[range_key].size()) - 1;
                 bucket_idx >= 0;
                 bucket_idx--) begin
                if (tlb_lookup_key_equal(
                        tlb_anchor_keys_by_range_key[range_key][bucket_idx],
                        anchor_key)) begin
                    tlb_anchor_keys_by_range_key[range_key].delete(bucket_idx);
                    removed_count++;
                end
            end
            if (removed_count != 1) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("range-index unregistration expected one anchor got=%0d", removed_count))
            end
            if (tlb_anchor_keys_by_range_key[range_key].size() == 0) begin
                tlb_anchor_keys_by_range_key.delete(range_key);
            end
        end
        entry.range_index_keys.delete();
    endfunction:unregister_tlb_range_index

    // Abstract responsibility: enumerate the bounded set of secondary-index
    // buckets that could raw-hit one exact-miss request.  It does not inspect
    // the table or arbitrate candidates; it only mirrors matcher granularity.
    function void build_tlb_range_query_keys(
        input memblock_tlb_lookup_key_t request_key,
        output memblock_tlb_range_index_key_t keys[$]);
        memblock_tlb_range_kind_e range_kind;
        bit [1:0]                  level;
        bit                        napot;
        bit [15:0]                 vmid;

        keys.delete();
        case (request_key.s2xlate)
            2'd0: begin
                range_kind = MEMBLOCK_TLB_RANGE_KIND_S1;
                vmid = '0;
            end
            2'd1: begin
                range_kind = MEMBLOCK_TLB_RANGE_KIND_S1;
                vmid = request_key.vmid;
            end
            2'd2: begin
                range_kind = MEMBLOCK_TLB_RANGE_KIND_S2;
                vmid = request_key.vmid;
            end
            2'd3: begin
                range_kind = MEMBLOCK_TLB_RANGE_KIND_ALLSTAGE;
                vmid = request_key.vmid;
            end
            default: begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("unsupported range request s2xlate=%0d", request_key.s2xlate))
            end
        endcase
        for (int unsigned shape_idx = 0; shape_idx < 5; shape_idx++) begin
            level = 2'd0;
            napot = 1'b0;
            case (shape_idx)
                0: begin end
                1: napot = 1'b1;
                2: level = 2'd1;
                3: level = 2'd2;
                4: level = 2'd3;
                default: begin
                    `uvm_fatal("COMMON_DATA", "invalid L2TLB range query shape")
                end
            endcase
            if (request_key.s2xlate == 2'd2) begin
                keys.push_back(make_tlb_range_index_key(
                    range_kind, request_key.s2xlate,
                    1'b0, '0, vmid, level, napot, request_key.vpn));
            end
            else begin
                // S1 raw hit allows either the request ASID bucket or a
                // global mapping bucket; both must be considered explicitly.
                keys.push_back(make_tlb_range_index_key(
                    range_kind, request_key.s2xlate,
                    1'b0, request_key.asid, vmid, level, napot,
                    request_key.vpn));
                keys.push_back(make_tlb_range_index_key(
                    range_kind, request_key.s2xlate,
                    1'b1, '0, vmid, level, napot, request_key.vpn));
            end
        end
    endfunction:build_tlb_range_query_keys

    // Abstract responsibility: return the raw coverage rank for an entry that
    // has already passed the exact V2 raw matcher.  It is a deterministic
    // overlap policy input and never changes the entry or response payload.
    function memblock_tlb_range_coverage_rank_e get_tlb_range_match_coverage_rank(
        input memblock_tlb_entry entry);
        bit [1:0] level;
        bit       napot;

        if (entry == null) begin
            `uvm_fatal("COMMON_DATA", "get_tlb_range_match_coverage_rank got null entry")
        end
        case (entry.s2xlate)
            2'd0, 2'd1: begin
                if (!entry.s1_stage_active || entry.s2_stage_active) begin
                    `uvm_fatal("COMMON_DATA", "S1 range rank got invalid stage shape")
                end
                level = entry.s1_level;
                napot = level == 2'd0 && entry.s1_pte_n;
            end
            2'd2: begin
                if (entry.s1_stage_active || !entry.s2_stage_active) begin
                    `uvm_fatal("COMMON_DATA", "S2 range rank got invalid stage shape")
                end
                level = entry.s2_level;
                napot = level == 2'd0 && entry.s2_pte_n;
            end
            2'd3: begin
                if (!entry.s1_stage_active || !entry.s2_stage_active) begin
                    `uvm_fatal("COMMON_DATA", "allStage range rank got invalid stage shape")
                end
                derive_allstage_lookup_shape(entry, level, napot);
                napot = level == 2'd0 && napot;
            end
            default: begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("unsupported range rank s2xlate=%0d", entry.s2xlate))
            end
        endcase
        case (level)
            2'd0: return napot ? MEMBLOCK_TLB_COVERAGE_64K : MEMBLOCK_TLB_COVERAGE_4K;
            2'd1: return MEMBLOCK_TLB_COVERAGE_2M;
            2'd2: return MEMBLOCK_TLB_COVERAGE_1G;
            2'd3: return MEMBLOCK_TLB_COVERAGE_512G;
            default: begin
                `uvm_fatal("COMMON_DATA", "invalid range-rank level")
            end
        endcase
        return MEMBLOCK_TLB_COVERAGE_4K;
    endfunction:get_tlb_range_match_coverage_rank

    // Abstract responsibility: use only request-relevant secondary buckets to
    // find a canonical raw payload for an exact-miss request.  Bucket hit is a
    // candidate filter; entry_matches_request_raw() remains the final truth.
    function bit find_tlb_range_hit_by_req(
        input memblock_tlb_lookup_key_t request_key,
        input mmu_csr_runtime_state request_csr_snapshot,
        output memblock_tlb_lookup_key_t anchor_key,
        output memblock_tlb_entry entry);
        memblock_tlb_range_index_key_t query_keys[$];
        memblock_tlb_lookup_key_t seen_anchor_keys[$];
        memblock_tlb_lookup_key_t candidate_anchor_keys[$];
        memblock_tlb_range_index_key_t candidate_query_keys[$];
        memblock_tlb_range_coverage_rank_e candidate_ranks[$];
        memblock_tlb_entry candidate_entries[$];
        memblock_tlb_lookup_key_t candidate_anchor;
        memblock_tlb_entry candidate_entry;
        memblock_tlb_range_coverage_rank_e max_rank;
        int query_idx;
        int bucket_idx;
        int seen_idx;
        int candidate_idx;
        int selected_idx;
        int unsigned max_rank_count;
        bit seen;
        string candidate_summary;

        anchor_key = '{default:'0};
        entry = null;
        if (request_csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "find_tlb_range_hit_by_req got null CSR snapshot")
        end
        build_tlb_range_query_keys(request_key, query_keys);
        foreach (query_keys[query_idx]) begin
            if (!tlb_anchor_keys_by_range_key.exists(query_keys[query_idx])) begin
                continue;
            end
            foreach (tlb_anchor_keys_by_range_key[query_keys[query_idx]][bucket_idx]) begin
                candidate_anchor =
                    tlb_anchor_keys_by_range_key[query_keys[query_idx]][bucket_idx];
                seen = 1'b0;
                foreach (seen_anchor_keys[seen_idx]) begin
                    if (tlb_lookup_key_equal(seen_anchor_keys[seen_idx], candidate_anchor)) begin
                        seen = 1'b1;
                    end
                end
                if (seen) begin
                    continue;
                end
                seen_anchor_keys.push_back(candidate_anchor);
                if (!has_tlb_entry(candidate_anchor)) begin
                    `uvm_fatal("COMMON_DATA",
                               "range index points to a missing canonical TLB entry")
                end
                candidate_entry = get_tlb_entry(candidate_anchor);
                if (candidate_entry.lookup_key != candidate_anchor ||
                    candidate_entry.range_index_keys.size() == 0) begin
                    `uvm_fatal("COMMON_DATA",
                               "range index points to an inconsistent canonical TLB entry")
                end
                if (!entry_matches_request_raw(candidate_entry, request_key.vpn,
                                               request_key.s2xlate,
                                               request_csr_snapshot)) begin
                    `uvm_fatal("COMMON_DATA",
                               $sformatf("range index/raw matcher disagreement request vpn=0x%0h anchor vpn=0x%0h s2xlate=%0d",
                                         request_key.vpn, candidate_anchor.vpn,
                                         request_key.s2xlate))
                end
                candidate_anchor_keys.push_back(candidate_anchor);
                candidate_entries.push_back(candidate_entry);
                candidate_ranks.push_back(
                    get_tlb_range_match_coverage_rank(candidate_entry));
                candidate_query_keys.push_back(query_keys[query_idx]);
            end
        end
        if (candidate_entries.size() == 0) begin
            return 1'b0;
        end
        if (candidate_entries.size() == 1) begin
            anchor_key = candidate_anchor_keys[0];
            entry = candidate_entries[0];
            return 1'b1;
        end

        max_rank = MEMBLOCK_TLB_COVERAGE_4K;
        foreach (candidate_ranks[candidate_idx]) begin
            if (candidate_ranks[candidate_idx] > max_rank) begin
                max_rank = candidate_ranks[candidate_idx];
            end
        end
        max_rank_count = 0;
        selected_idx = -1;
        candidate_summary = "";
        foreach (candidate_entries[candidate_idx]) begin
            candidate_summary = {candidate_summary,
                $sformatf(" [anchor vpn=0x%0h asid=0x%0h vmid=0x%0h gen=%0d level=%0d napot=%0d rank=%0d qkind=%0d]",
                          candidate_anchor_keys[candidate_idx].vpn,
                          candidate_anchor_keys[candidate_idx].asid,
                          candidate_anchor_keys[candidate_idx].vmid,
                          candidate_entries[candidate_idx].entry_generation,
                          candidate_query_keys[candidate_idx].level,
                          candidate_query_keys[candidate_idx].napot,
                          candidate_ranks[candidate_idx],
                          candidate_query_keys[candidate_idx].range_kind)};
            if (candidate_ranks[candidate_idx] == max_rank) begin
                max_rank_count++;
                selected_idx = candidate_idx;
            end
        end
        if (max_rank_count != 1 || selected_idx < 0) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("ambiguous L2TLB range hit request vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d max_rank=%0d candidates:%s",
                                 request_key.vpn, request_key.asid,
                                 request_key.vmid, request_key.s2xlate,
                                 max_rank, candidate_summary))
        end
        anchor_key = candidate_anchor_keys[selected_idx];
        entry = candidate_entries[selected_idx];
        `uvm_info("COMMON_DATA",
                  $sformatf("overlapping L2TLB range hit selects widest raw coverage request vpn=0x%0h s2xlate=%0d selected vpn=0x%0h gen=%0d rank=%0d candidates:%s",
                            request_key.vpn, request_key.s2xlate,
                            anchor_key.vpn, entry.entry_generation,
                            max_rank, candidate_summary), UVM_LOW)
        return 1'b1;
    endfunction:find_tlb_range_hit_by_req

    function bit get_or_create_tlb_entry_by_req(input bit [37:0] vpn,
                                                input bit [1:0] s2xlate,
                                                output memblock_tlb_lookup_key_t key,
                                                output memblock_tlb_entry entry,
                                                output bit created);
        memblock_tlb_lookup_key_t entry_anchor_key;
        memblock_tlb_lookup_result_e lookup_result;

        key = make_tlb_key_by_req(vpn, s2xlate);
        return get_or_create_tlb_entry_by_req_with_snapshot(vpn,
                                                             s2xlate,
                                                             mmu_csr_state,
                                                             key,
                                                             entry_anchor_key,
                                                             lookup_result,
                                                             entry,
                                                             created);
    endfunction:get_or_create_tlb_entry_by_req

    // 中文注释：L2TLB request fire必须用该笔request冻结的CSR生成key和新entry。
    // 不能在CSR变更边界回退到common_data的live mmu_csr_state；命中旧表项时仍复用同一by-key存储。
    function bit get_or_create_tlb_entry_by_req_with_snapshot(
        input bit [37:0] vpn,
        input bit [1:0] s2xlate,
        input mmu_csr_runtime_state csr_snapshot,
        output memblock_tlb_lookup_key_t request_key,
        output memblock_tlb_lookup_key_t entry_anchor_key,
        output memblock_tlb_lookup_result_e lookup_result,
        output memblock_tlb_entry entry,
        output bit created);
        if (csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "get_or_create_tlb_entry_by_req_with_snapshot got null csr_snapshot")
        end
        request_key = csr_snapshot.make_lookup_key({26'b0, vpn}, s2xlate);
        entry_anchor_key = '{default:'0};
        lookup_result = MEMBLOCK_TLB_LOOKUP_MISS_BUILD;
        if (has_tlb_entry(request_key)) begin
            entry = tlb_entry_by_key[request_key];
            if (entry.range_index_keys.size() == 0) begin
                `uvm_fatal("COMMON_DATA", "exact L2TLB hit has no registered range index ownership")
            end
            entry.last_hit_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            entry_anchor_key = request_key;
            lookup_result = MEMBLOCK_TLB_LOOKUP_EXACT_HIT;
            created = 1'b0;
            return 1'b1;
        end
        if (find_tlb_range_hit_by_req(request_key, csr_snapshot,
                                      entry_anchor_key, entry)) begin
            entry.last_hit_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            lookup_result = MEMBLOCK_TLB_LOOKUP_RANGE_HIT;
            created = 1'b0;
            return 1'b1;
        end
        entry = build_tlb_entry_for_key_with_csr(request_key, csr_snapshot);
        insert_tlb_entry(request_key, entry);
        if (!register_tlb_range_index(request_key, entry)) begin
            tlb_entry_by_key.delete(request_key);
            `uvm_fatal("COMMON_DATA", "failed to register new canonical TLB entry in range index")
        end
        entry_anchor_key = request_key;
        lookup_result = MEMBLOCK_TLB_LOOKUP_MISS_BUILD;
        created = 1'b1;
        return 1'b1;
    endfunction:get_or_create_tlb_entry_by_req_with_snapshot

    // Abstract responsibility: translate one context-bound raw fence into the
    // immutable stage-specific payload used by the C4 live-entry deleter. It
    // does not inspect the live table or modify L2TLB request token state.
    function memblock_sfence_payload_t decode_raw_sfence(
        input memblock_sync_pkg::dispatch_raw_sfence_t raw);
        memblock_sfence_payload_t payload;

        if (!raw.valid || raw.sample_seq == 0 || !raw.context_valid ||
            raw.csr_sample_seq != raw.sample_seq ||
            raw.context_reset_epoch != raw.reset_epoch ||
            raw.reset_epoch != memblock_sync_pkg::get_l2tlb_current_reset_epoch()) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("invalid raw SFENCE context valid=%0d sample=%0d csr_sample=%0d raw_epoch=%0d context_epoch=%0d current_epoch=%0d",
                                 raw.valid, raw.sample_seq, raw.csr_sample_seq,
                                 raw.reset_epoch, raw.context_reset_epoch,
                                 memblock_sync_pkg::get_l2tlb_current_reset_epoch()))
        end
        if (raw.hv && raw.hg) begin
            `uvm_fatal("COMMON_DATA", "SFENCE raw cannot assert hv and hg together")
        end

        payload = '{default:'0};
        payload.valid       = 1'b1;
        payload.ignore_addr = raw.rs1;
        payload.ignore_id   = raw.rs2;
        payload.addr        = raw.addr;
        payload.id          = raw.id;
        payload.hv          = raw.hv;
        payload.hg          = raw.hg;
        payload.priv_virt_at_sample = raw.priv_virt_at_sample;
        payload.hgatp_vmid_at_sample = raw.hgatp_vmid_at_sample;
        payload.satp_mode_at_sample = raw.satp_mode_at_sample;
        payload.vsatp_mode_at_sample = raw.vsatp_mode_at_sample;
        payload.hgatp_mode_at_sample = raw.hgatp_mode_at_sample;
        payload.sample_seq = raw.sample_seq;
        payload.reset_epoch = raw.reset_epoch;
        payload.lifecycle_event_seq = raw.lifecycle_event_seq;
        payload.cycle       = raw.cycle;

        if (raw.hg) begin
            payload.target_stage = MEMBLOCK_SFENCE_TARGET_G_S2;
            // HFENCE.GVMA carries GPA >> 2. Recover the GVPN only once here.
            payload.s2_gvpn = {12'b0, raw.addr[49:10]};
        end
        else begin
            payload.s1_vpn = raw.addr[49:12];
            if (raw.hv || raw.priv_virt_at_sample) begin
                payload.target_stage = MEMBLOCK_SFENCE_TARGET_VS_S1;
            end
            else begin
                payload.target_stage = MEMBLOCK_SFENCE_TARGET_HS_S1;
            end
        end
        return payload;
    endfunction:decode_raw_sfence

    // Abstract responsibility: reject a live entry whose frozen translation
    // stage/mode/level cannot represent the stage matcher about to consume it.
    // It is a pure structural check and never repairs fields from current CSR.
    function void validate_frozen_stage_level(
        input memblock_sfence_target_stage_e target_stage,
        input memblock_tlb_entry entry);
        bit stage_active;
        bit [3:0] mode;
        bit [1:0] level;
        bit stage_shape_valid;

        if (entry == null) begin
            `uvm_fatal("COMMON_DATA", "validate_frozen_stage_level got null entry")
        end
        case (target_stage)
            MEMBLOCK_SFENCE_TARGET_HS_S1: begin
                stage_active = entry.s1_stage_active;
                mode = entry.s1_translation_mode_at_build;
                level = entry.s1_level;
                stage_shape_valid = entry.s2xlate == 2'd0;
            end
            MEMBLOCK_SFENCE_TARGET_VS_S1: begin
                stage_active = entry.s1_stage_active;
                mode = entry.s1_translation_mode_at_build;
                level = entry.s1_level;
                stage_shape_valid = entry.s2xlate == 2'd1 || entry.s2xlate == 2'd3;
            end
            MEMBLOCK_SFENCE_TARGET_G_S2: begin
                stage_active = entry.s2_stage_active;
                mode = entry.s2_translation_mode_at_build;
                level = entry.s2_level;
                stage_shape_valid = entry.s2xlate == 2'd2 || entry.s2xlate == 2'd3;
            end
            default: begin
                `uvm_fatal("COMMON_DATA", "unknown SFENCE target stage")
                return;
            end
        endcase
        if (!stage_active || !stage_shape_valid ||
            !(mode inside {MEMBLOCK_SV39_MODE, MEMBLOCK_SV48_MODE}) ||
            (mode == MEMBLOCK_SV39_MODE && level == 2'd3)) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("invalid frozen SFENCE stage target=%0d s2xlate=%0d active=%0d mode=%0d level=%0d generation=%0d",
                                 target_stage, entry.s2xlate, stage_active,
                                 mode, level, entry.entry_generation))
        end
    endfunction:validate_frozen_stage_level

    // Abstract responsibility: decide whether a frozen S1 live entry covers
    // the S1 VPN carried by an HS/VS fence. It only reads S1 fields and does
    // not use S2 level, current CSR, PPN split data, or pteidx as an address.
    function bit sfence_s1_addr_match(
        input memblock_tlb_entry entry,
        input bit [37:0] fence_vpn,
        input memblock_sfence_target_stage_e target_stage);
        bit [37:0] anchor_vpn;

        validate_frozen_stage_level(target_stage, entry);
        if (!tlb_request_s1_vpn_fits_mode({14'b0, fence_vpn},
                                          entry.s1_translation_mode_at_build)) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("S1 fence VPN=0x%0h exceeds frozen mode=%0d generation=%0d",
                                 fence_vpn, entry.s1_translation_mode_at_build,
                                 entry.entry_generation))
        end
        if (!entry.s1_pte_n && entry.s1_level == 2'd0) begin
            return entry.s1_tag == fence_vpn[37:3] &&
                   entry.s1_valididx[fence_vpn[2:0]];
        end
        anchor_vpn = {entry.s1_tag, 3'b000};
        return raw_l2tlb_vpn_matches_level(anchor_vpn, {14'b0, fence_vpn},
                                           entry.s1_level, entry.s1_pte_n);
    endfunction:sfence_s1_addr_match

    // Abstract responsibility: decide whether a frozen S2 live entry covers
    // the GVPN recovered from HFENCE.GVMA. It only reads S2 fields and never
    // derives a replacement GVPN from request VPN, S1 PPN, or current CSR.
    function bit sfence_s2_addr_match(input memblock_tlb_entry entry,
                                      input bit [51:0] fence_gvpn);
        if (|fence_gvpn[51:44] ||
            !tlb_request_s2_gvpn_fits_mode(fence_gvpn[43:0],
                                            entry.s2_translation_mode_at_build)) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("S2 fence GVPN=0x%0h exceeds frozen mode=%0d generation=%0d",
                                 fence_gvpn, entry.s2_translation_mode_at_build,
                                 entry.entry_generation))
        end
        validate_frozen_stage_level(MEMBLOCK_SFENCE_TARGET_G_S2, entry);
        return raw_l2tlb_vpn_matches_level(entry.s2_tag, fence_gvpn,
                                           entry.s2_level, entry.s2_pte_n);
    endfunction:sfence_s2_addr_match

    // Abstract responsibility: test one live entry against one already
    // decoded fence. It returns only match/not-match; C4 deletion is owned by
    // apply_due_sfence_invalidate(), and it never reads mutable mmu_csr_state.
    function bit sfence_match_entry(input memblock_sfence_payload_t payload,
                                    input memblock_tlb_lookup_key_t key,
                                    input memblock_tlb_entry entry);
        bit addr_ok;
        bit id_ok;
        bit vmid_ok;

        if (!payload.valid) begin
            return 1'b0;
        end
        if (entry == null || entry.entry_generation == 0 ||
            key.s2xlate != entry.s2xlate) begin
            `uvm_fatal("COMMON_DATA", "sfence_match_entry got inconsistent live entry")
        end
        entry.check_inactive_stage_defaults("SFENCE_MATCH");
        case (payload.target_stage)
            MEMBLOCK_SFENCE_TARGET_HS_S1: begin
                if (key.s2xlate != 2'd0) begin
                    return 1'b0;
                end
                validate_frozen_stage_level(MEMBLOCK_SFENCE_TARGET_HS_S1,
                                            entry);
                addr_ok = payload.ignore_addr ||
                          sfence_s1_addr_match(entry, payload.s1_vpn,
                                               MEMBLOCK_SFENCE_TARGET_HS_S1);
                id_ok = payload.ignore_id ||
                        (!entry.s1_pte_g && entry.s1_asid == payload.id);
                return addr_ok && id_ok;
            end
            MEMBLOCK_SFENCE_TARGET_VS_S1: begin
                if (!(key.s2xlate inside {2'd1, 2'd3})) begin
                    return 1'b0;
                end
                validate_frozen_stage_level(MEMBLOCK_SFENCE_TARGET_VS_S1,
                                            entry);
                addr_ok = payload.ignore_addr ||
                          sfence_s1_addr_match(entry, payload.s1_vpn,
                                               MEMBLOCK_SFENCE_TARGET_VS_S1);
                // VS-stage always retains the sampled VMID, including rs2=x0.
                vmid_ok = entry.s1_vmid == payload.hgatp_vmid_at_sample;
                id_ok = payload.ignore_id ||
                        (!entry.s1_pte_g && entry.s1_asid == payload.id);
                return addr_ok && vmid_ok && id_ok;
            end
            MEMBLOCK_SFENCE_TARGET_G_S2: begin
                if (!(key.s2xlate inside {2'd2, 2'd3})) begin
                    return 1'b0;
                end
                validate_frozen_stage_level(MEMBLOCK_SFENCE_TARGET_G_S2,
                                            entry);
                addr_ok = payload.ignore_addr ||
                          sfence_s2_addr_match(entry, payload.s2_gvpn);
                id_ok = payload.ignore_id ||
                        (entry.s2_vmid[13:0] == payload.id[13:0]);
                return addr_ok && id_ok;
            end
            default: begin
                `uvm_fatal("COMMON_DATA", "sfence_match_entry got unknown target stage")
            end
        endcase
        return 1'b0;
    endfunction:sfence_match_entry

    // 抽象职责：比较控制 worker 预约的 canonical SFence 与 adapter 从 raw fence
    // 解码的 payload。只比较 DUT 语义字段，sample/event/reset provenance 由调用者
    // 单独校验，避免把同一事件的 transport 元数据误作 payload 差异。
    function bit control_sfence_payload_matches(
        input memblock_sfence_payload_t expected,
        input memblock_sfence_payload_t observed
    );
        return expected.valid && observed.valid &&
               expected.ignore_addr == observed.ignore_addr &&
               expected.ignore_id == observed.ignore_id &&
               expected.addr == observed.addr && expected.id == observed.id &&
               expected.hv == observed.hv && expected.hg == observed.hg &&
               expected.target_stage == observed.target_stage;
    endfunction:control_sfence_payload_matches

    // 抽象职责：adapter 在 C0 schedule 边界把匹配的 raw fence 固化为 owner 化
    // observation。它不改 control_state；service 仅在 worker sendover 后消费该槽。
    function void record_control_sfence_c0_observation(
        input memblock_sfence_payload_t payload,
        input longint unsigned reset_epoch,
        input longint unsigned lifecycle_event_seq,
        input longint unsigned anchor_sample_seq
    );
        status_transaction status;

        if (!active_control_barrier_valid) begin
            return;
        end
        status = get_status(active_control_barrier_uid);
        if (status.control_kind != MEMBLOCK_CONTROL_KIND_SFENCE ||
            !status.control_owner.valid || !status.control_expected_sfence_valid ||
            !status.control_sfence_c0_armed ||
            !(status.control_state inside {MEMBLOCK_CONTROL_STATE_SFENCE_REQ,
                                            MEMBLOCK_CONTROL_STATE_SFENCE_SENDOVER,
                                            MEMBLOCK_CONTROL_STATE_WAIT_L2TLB_FLUSH_EFFECTIVE}) ||
            lifecycle_event_seq <= status.control_sfence_pre_drive_event_seq ||
            reset_epoch != status.control_l2tlb_reset_epoch_at_arm ||
            !control_sfence_payload_matches(status.control_expected_sfence, payload)) begin
            return;
        end
        if (control_sfence_c0_observation.valid) begin
            if (control_sfence_c0_observation.lifecycle_event_seq == lifecycle_event_seq &&
                memblock_control_owner_equal(control_sfence_c0_observation.owner,
                                             status.control_owner)) begin
                return;
            end
            `uvm_fatal("CONTROL_SFENCE",
                       "second control SFence C0 observation arrived before prior record was consumed")
        end
        control_sfence_c0_observation.valid = 1'b1;
        control_sfence_c0_observation.owner = status.control_owner;
        control_sfence_c0_observation.payload = payload;
        control_sfence_c0_observation.lifecycle_event_seq = lifecycle_event_seq;
        control_sfence_c0_observation.reset_epoch = reset_epoch;
        control_sfence_c0_observation.anchor_sample_seq = anchor_sample_seq;
        control_sfence_c0_observation.due_sample_seq = 0;
    endfunction:record_control_sfence_c0_observation

    // 抽象职责：adapter 在既有 C4 delete 已实际执行后，发布同一 C0 event 的
    // effective observation。即使没有匹配 TLB entry 需要删除，C4 已生效仍必须可见。
    function void record_control_sfence_effective_observation(
        input memblock_pending_sfence_invalidate_t pending
    );
        if (!control_sfence_c0_observation.valid ||
            control_sfence_c0_observation.lifecycle_event_seq !=
                pending.lifecycle_event_seq ||
            control_sfence_c0_observation.reset_epoch != pending.reset_epoch) begin
            return;
        end
        if (control_sfence_effective_observation.valid) begin
            `uvm_fatal("CONTROL_SFENCE",
                       "duplicate control SFence effective observation")
        end
        control_sfence_effective_observation = control_sfence_c0_observation;
        control_sfence_effective_observation.due_sample_seq = pending.due_sample_seq;
    endfunction:record_control_sfence_effective_observation

    function bit get_control_sfence_c0_observation(
        input memblock_control_owner_t owner,
        output memblock_control_sfence_observation_t observation
    );
        observation = '{default:'0};
        if (!control_sfence_c0_observation.valid ||
            !memblock_control_owner_equal(control_sfence_c0_observation.owner, owner)) begin
            return 1'b0;
        end
        observation = control_sfence_c0_observation;
        return 1'b1;
    endfunction:get_control_sfence_c0_observation

    function bit try_consume_control_sfence_effective_observation(
        input memblock_control_owner_t owner,
        input longint unsigned lifecycle_event_seq,
        input longint unsigned reset_epoch,
        output memblock_control_sfence_observation_t observation
    );
        observation = '{default:'0};
        if (!control_sfence_effective_observation.valid ||
            !memblock_control_owner_equal(control_sfence_effective_observation.owner, owner) ||
            control_sfence_effective_observation.lifecycle_event_seq != lifecycle_event_seq ||
            control_sfence_effective_observation.reset_epoch != reset_epoch) begin
            return 1'b0;
        end
        observation = control_sfence_effective_observation;
        control_sfence_effective_observation = '{default:'0};
        control_sfence_c0_observation = '{default:'0};
        return 1'b1;
    endfunction:try_consume_control_sfence_effective_observation

    // Abstract responsibility: record one C0 fence as a future C4 destructive
    // action after the adapter has accepted its raw FIFO item. It does not
    // scan live entries or affect pending response token/UID ownership.
    function bit schedule_sfence_invalidate(
        input memblock_sfence_payload_t payload,
        input longint unsigned anchor_sample_seq,
        input longint unsigned reset_epoch,
        input longint unsigned lifecycle_event_seq);
        memblock_pending_sfence_invalidate_t pending;

        if (!payload.valid || anchor_sample_seq == 0 ||
            payload.sample_seq != anchor_sample_seq ||
            payload.reset_epoch != reset_epoch ||
            reset_epoch != memblock_sync_pkg::get_l2tlb_current_reset_epoch() ||
            lifecycle_event_seq == memblock_sync_pkg::MEMBLOCK_L2TLB_EVENT_SEQ_NONE ||
            lifecycle_event_seq > memblock_sync_pkg::last_allocated_l2tlb_event_seq) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("invalid SFENCE schedule anchor=%0d payload_sample=%0d reset=%0d/%0d event=%0d",
                                 anchor_sample_seq, payload.sample_seq,
                                 reset_epoch,
                                 memblock_sync_pkg::get_l2tlb_current_reset_epoch(),
                                 lifecycle_event_seq))
        end
        pending.payload = payload;
        pending.anchor_sample_seq = anchor_sample_seq;
        pending.due_sample_seq = anchor_sample_seq +
                                 MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES;
        pending.reset_epoch = reset_epoch;
        pending.lifecycle_event_seq = lifecycle_event_seq;
        if (sfence_invalidate_pending_q.size() != 0 &&
            pending.due_sample_seq <
                sfence_invalidate_pending_q[$].due_sample_seq) begin
            `uvm_fatal("COMMON_DATA", "SFENCE invalidate queue due order regressed")
        end
        sfence_invalidate_pending_q.push_back(pending);
        record_control_sfence_c0_observation(payload, reset_epoch,
                                              lifecycle_event_seq,
                                              anchor_sample_seq);
        return 1'b1;
    endfunction:schedule_sfence_invalidate

    // Abstract responsibility: remove one canonical live entry through the
    // sole delete API. It first unregisters all entry-owned range buckets, so
    // no future request can observe an anchor whose raw payload was deleted.
    function void delete_live_tlb_entry_by_anchor_key(
        input memblock_tlb_lookup_key_t key,
        input string reason);
        memblock_tlb_entry entry;

        if (!tlb_entry_by_key.exists(key) || tlb_entry_by_key[key] == null) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("live TLB delete lost canonical key reason=%s", reason))
        end
        entry = tlb_entry_by_key[key];
        `uvm_info("COMMON_DATA",
                  $sformatf("delete live TLB entry reason=%s generation=%0d vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d",
                            reason, entry.entry_generation, key.vpn, key.asid,
                            key.vmid, key.s2xlate), UVM_LOW)
        unregister_tlb_range_index(key, entry);
        tlb_entry_by_key.delete(key);
    endfunction:delete_live_tlb_entry_by_anchor_key

    // Abstract responsibility: perform only C4-or-later live-entry deletion
    // for queued fences in the current reset epoch. It scans the bounded live
    // entry map, never main/status tables or L2TLB pending response tokens.
    function int unsigned apply_due_sfence_invalidate(
        input longint unsigned dut_sample_seq,
        input longint unsigned current_reset_epoch);
        memblock_pending_sfence_invalidate_t pending;
        memblock_tlb_lookup_key_t delete_keys[$];
        int unsigned deleted_count;

        deleted_count = 0;
        while (sfence_invalidate_pending_q.size() != 0 &&
               sfence_invalidate_pending_q[0].due_sample_seq <= dut_sample_seq) begin
            pending = sfence_invalidate_pending_q.pop_front();
            if (pending.reset_epoch < current_reset_epoch) begin
                `uvm_info("COMMON_DATA",
                          $sformatf("drop stale SFENCE invalidate anchor=%0d epoch=%0d current=%0d",
                                    pending.anchor_sample_seq, pending.reset_epoch,
                                    current_reset_epoch), UVM_LOW)
                continue;
            end
            if (pending.reset_epoch > current_reset_epoch ||
                pending.payload.reset_epoch != pending.reset_epoch) begin
                `uvm_fatal("COMMON_DATA",
                           "future/inconsistent SFENCE invalidate reached due sample")
            end
            delete_keys.delete();
            foreach (tlb_entry_by_key[key]) begin
                if (sfence_match_entry(pending.payload, key,
                                       tlb_entry_by_key[key])) begin
                    delete_keys.push_back(key);
                end
            end
            foreach (delete_keys[idx]) begin
                delete_live_tlb_entry_by_anchor_key(delete_keys[idx],
                                                    "SFENCE/HFENCE C4");
            end
            record_control_sfence_effective_observation(pending);
            deleted_count += delete_keys.size();
            `uvm_info("COMMON_DATA",
                      $sformatf("apply due SFENCE/HFENCE target=%0d anchor=%0d due=%0d event=%0d deleted=%0d",
                                pending.payload.target_stage,
                                pending.anchor_sample_seq,
                                pending.due_sample_seq,
                                pending.lifecycle_event_seq,
                                delete_keys.size()), UVM_LOW)
        end
        return deleted_count;
    endfunction:apply_due_sfence_invalidate

    function bit has_pending_sfence_invalidate();
        return sfence_invalidate_pending_q.size() != 0;
    endfunction:has_pending_sfence_invalidate

    // Abstract responsibility: clear only the software-owned live L2TLB
    // entry/invalidate state at a runtime reset, while preserving the main
    // table and UID history owned by other dispatch flows.
    function void clear_dispatch_l2tlb_live_entries();
        sfence_invalidate_pending_q.delete();
        tlb_anchor_keys_by_range_key.delete();
        tlb_entry_by_key.delete();
    endfunction:clear_dispatch_l2tlb_live_entries

    function longint unsigned allocate_tlb_entry_generation();
        if (next_tlb_entry_generation == '1 || next_tlb_entry_generation + 1 == 0)
            `uvm_fatal("COMMON_DATA", "TLB entry generation overflow");
        next_tlb_entry_generation++;
        return next_tlb_entry_generation;
    endfunction:allocate_tlb_entry_generation

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
        entry = builder.build_payload_for_key_with_csr(key, csr_snapshot);
        if (entry == null) begin
            `uvm_fatal("COMMON_DATA", "builder returned null payload entry")
        end
        entry.lookup_key = key;
        entry.s2xlate = key.s2xlate;
        entry.entry_generation = allocate_tlb_entry_generation();
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

    function memblock_uid_tlb_wait_shape_key_t make_uid_tlb_wait_shape_key(
        input bit [51:0] vpn,
        input bit [1:0] s2xlate);
        return {vpn, s2xlate};
    endfunction:make_uid_tlb_wait_shape_key

    // UID registration is sealed when the owner has requested release, before
    // the later driver-side transport cutoff confirms admission closed.
    function void check_l2tlb_uid_registration_open(input string caller);
        if (memblock_sync_pkg::l2tlb_release_admission_close_requested ||
            memblock_sync_pkg::l2tlb_release_admission_closed ||
            memblock_sync_pkg::l2tlb_release_closing) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("%s called after L2TLB release admission seal requested=%0d closed=%0d closing=%0d",
                                 caller,
                                 memblock_sync_pkg::l2tlb_release_admission_close_requested,
                                 memblock_sync_pkg::l2tlb_release_admission_closed,
                                 memblock_sync_pkg::l2tlb_release_closing))
        end
    endfunction:check_l2tlb_uid_registration_open

    // This bounded cleanup never changes a valid WAITING record.  It only
    // removes a stale index handle after that record has already become
    // invalid, COMPLETED, or CANCELED.
    function void prune_uid_waiting_index_bucket(
        input memblock_uid_tlb_wait_shape_key_t shape_key,
        input string caller);
        int                         idx;
        memblock_uid_t              candidate_uid;
        memblock_uid_tlb_record     candidate_record;

        if (!uid_waiting_by_vpn_s2xlate.exists(shape_key)) begin
            return;
        end
        for (idx = int'(uid_waiting_by_vpn_s2xlate[shape_key].size()) - 1;
             idx >= 0;
             idx--) begin
            candidate_uid = uid_waiting_by_vpn_s2xlate[shape_key][idx];
            if (!uid_tlb_record_by_uid.exists(candidate_uid) ||
                uid_tlb_record_by_uid[candidate_uid] == null ||
                !uid_tlb_record_by_uid[candidate_uid].record_valid ||
                !uid_tlb_record_by_uid[candidate_uid].is_waiting()) begin
                `uvm_info("COMMON_DATA",
                          $sformatf("prune stale L2TLB WAITING index uid=%0d caller=%s",
                                    candidate_uid, caller),
                          UVM_LOW)
                uid_waiting_by_vpn_s2xlate[shape_key].delete(idx);
                continue;
            end
            candidate_record = uid_tlb_record_by_uid[candidate_uid];
            if (candidate_record.uid != candidate_uid ||
                make_uid_tlb_wait_shape_key(candidate_record.vpn,
                                             candidate_record.s2xlate) != shape_key) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("L2TLB WAITING index corruption uid=%0d caller=%s",
                                     candidate_uid, caller))
            end
        end
        if (!uid_waiting_by_vpn_s2xlate.exists(shape_key)) begin
            return;
        end
        if (uid_waiting_by_vpn_s2xlate[shape_key].size() == 0) begin
            uid_waiting_by_vpn_s2xlate.delete(shape_key);
        end else if (uid_waiting_by_vpn_s2xlate[shape_key].size() >
                     MEMBLOCK_DUT_L2TLB_DFILTER_SIZE) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("L2TLB WAITING index bucket exceeds DTLB filter capacity caller=%s size=%0d max=%0d",
                                 caller,
                                 uid_waiting_by_vpn_s2xlate[shape_key].size(),
                                 MEMBLOCK_DUT_L2TLB_DFILTER_SIZE))
        end
    endfunction:prune_uid_waiting_index_bucket

    function void add_waiting_uid_to_index(input memblock_uid_t uid,
                                            input memblock_uid_tlb_record record);
        memblock_uid_tlb_wait_shape_key_t shape_key;
        int                                idx;

        if (record == null || !record.is_waiting() || record.uid != uid ||
            record.lookup_key.vpn != record.vpn ||
            record.lookup_key.s2xlate != record.s2xlate) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("invalid WAITING UID registration uid=%0d", uid))
        end
        shape_key = make_uid_tlb_wait_shape_key(record.vpn, record.s2xlate);
        prune_uid_waiting_index_bucket(shape_key, "add_waiting_uid_to_index");
        if (uid_waiting_by_vpn_s2xlate.exists(shape_key)) begin
            for (idx = 0; idx < int'(uid_waiting_by_vpn_s2xlate[shape_key].size());
                 idx++) begin
                if (uid_waiting_by_vpn_s2xlate[shape_key][idx] == uid) begin
                    `uvm_fatal("COMMON_DATA",
                               $sformatf("duplicate L2TLB WAITING index uid=%0d", uid))
                end
            end
            if (uid_waiting_by_vpn_s2xlate[shape_key].size() >=
                MEMBLOCK_DUT_L2TLB_DFILTER_SIZE) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("L2TLB WAITING index capacity overflow uid=%0d size=%0d max=%0d",
                                     uid,
                                     uid_waiting_by_vpn_s2xlate[shape_key].size(),
                                     MEMBLOCK_DUT_L2TLB_DFILTER_SIZE))
            end
        end
        uid_waiting_by_vpn_s2xlate[shape_key].push_back(uid);
    endfunction:add_waiting_uid_to_index

    function void check_waiting_uid_index_membership(
        input memblock_uid_t uid,
        input memblock_uid_tlb_record record,
        input string caller);
        memblock_uid_tlb_wait_shape_key_t shape_key;
        int                                member_count;
        int                                idx;

        if (record == null || !record.is_waiting() || record.uid != uid) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("%s got invalid WAITING UID record uid=%0d",
                                 caller, uid))
        end
        shape_key = make_uid_tlb_wait_shape_key(record.vpn, record.s2xlate);
        prune_uid_waiting_index_bucket(shape_key, caller);
        member_count = 0;
        if (uid_waiting_by_vpn_s2xlate.exists(shape_key)) begin
            for (idx = 0; idx < int'(uid_waiting_by_vpn_s2xlate[shape_key].size());
                 idx++) begin
                if (uid_waiting_by_vpn_s2xlate[shape_key][idx] == uid) begin
                    member_count++;
                end
            end
        end
        if (member_count != 1) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("%s expected one L2TLB WAITING index member uid=%0d got=%0d",
                                 caller, uid, member_count))
        end
    endfunction:check_waiting_uid_index_membership

    function void remove_waiting_uid_from_index(
        input memblock_uid_t uid,
        input memblock_uid_tlb_record record,
        input string caller);
        memblock_uid_tlb_wait_shape_key_t shape_key;
        int                                idx;
        int unsigned                       removed_count;

        if (record == null || !record.record_valid || record.uid != uid) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("%s got invalid UID record for L2TLB index removal uid=%0d",
                                 caller, uid))
        end
        shape_key = make_uid_tlb_wait_shape_key(record.vpn, record.s2xlate);
        if (!uid_waiting_by_vpn_s2xlate.exists(shape_key)) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("%s missing L2TLB WAITING index bucket uid=%0d",
                                 caller, uid))
        end
        removed_count = 0;
        for (idx = int'(uid_waiting_by_vpn_s2xlate[shape_key].size()) - 1;
             idx >= 0;
             idx--) begin
            if (uid_waiting_by_vpn_s2xlate[shape_key][idx] == uid) begin
                uid_waiting_by_vpn_s2xlate[shape_key].delete(idx);
                removed_count++;
            end
        end
        if (removed_count != 1) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("%s expected one L2TLB WAITING index removal uid=%0d got=%0d",
                                 caller, uid, removed_count))
        end
        if (uid_waiting_by_vpn_s2xlate[shape_key].size() == 0) begin
            uid_waiting_by_vpn_s2xlate.delete(shape_key);
        end else begin
            prune_uid_waiting_index_bucket(shape_key, caller);
        end
    endfunction:remove_waiting_uid_from_index

    function void clear_uid_waiting_by_vpn_s2xlate();
        uid_waiting_by_vpn_s2xlate.delete();
    endfunction:clear_uid_waiting_by_vpn_s2xlate

    function bit uid_tlb_waiting_context_matches(
        input memblock_uid_t uid,
        input memblock_uid_tlb_record record,
        input bit [51:0] vpn,
        input bit [1:0] s2xlate,
        input bit is_hypervisor_inst,
        input mmu_csr_runtime_state csr_snapshot);
        memblock_tlb_lookup_key_t expected_key;

        if (csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA",
                       "uid_tlb_waiting_context_matches got null csr_snapshot")
        end
        if (record == null || !record.is_waiting() || record.csr_snapshot == null) begin
            return 1'b0;
        end
        expected_key = csr_snapshot.make_lookup_key(vpn, s2xlate);
        return record.uid == uid &&
               record.vpn == vpn &&
               record.s2xlate == s2xlate &&
               record.is_hypervisor_inst == is_hypervisor_inst &&
               record.lookup_key == expected_key &&
               record.csr_snapshot.update_seq == csr_snapshot.update_seq;
    endfunction:uid_tlb_waiting_context_matches

    function void register_uid_tlb_record_on_issue(input memblock_uid_t uid);
        main_control_transaction main_tr;
        status_transaction       status;
        mmu_csr_runtime_state    snapshot;
        bit [51:0]               vpn;
        bit [1:0]                s2xlate;
        bit                      is_hypervisor_inst;
        longint unsigned         current_sample;

        check_uid(uid, "register_uid_tlb_record_on_issue");
        check_l2tlb_uid_registration_open("register_uid_tlb_record_on_issue");
        current_sample = memblock_sync_pkg::peek_current_dut_global_sample();
        if (current_sample == 0) begin
            `uvm_fatal("COMMON_DATA",
                       "UID TLB registration requires a published DUT global sample")
        end
        main_tr = get_main_transaction(uid);
        status = get_status(uid);
        get_mmu_csr_snapshot(snapshot);
        // 中文注释：PMA/PMP context 必须在首次真实 issue 边界冻结。TLB hit
        // 可能没有 L2TLB request-fire，因此不能只在 responder 回调中建立该快照。
        // 先回放严格早于本 sample 的 CSR 写，再用当前 dynamic_epoch 保存表代数。
        if (seq_csr_common::get_pma_pmp_model_en()) begin
            apply_pma_pmp_csr_writes_before_request(current_sample);
            if (pma_pmp_model == null ||
                !pma_pmp_model.capture_uid_context(
                    uid,
                    status.dynamic_epoch,
                    snapshot.priv_dmode,
                    snapshot.priv_debug,
                    1'b0,
                    1'b0,
                    '0,
                    snapshot.update_seq,
                    current_sample)) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("failed to freeze PMA/PMP issue context uid=%0d epoch=%0d sample=%0d",
                                     uid, status.dynamic_epoch, current_sample))
            end
        end
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
        longint unsigned        current_sample;

        check_uid(uid, "update_uid_tlb_record_context");
        check_l2tlb_uid_registration_open("update_uid_tlb_record_context");
        if (csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "update_uid_tlb_record_context got null csr_snapshot")
        end
        current_sample = memblock_sync_pkg::peek_current_dut_global_sample();
        if (current_sample == 0) begin
            `uvm_fatal("COMMON_DATA",
                       "update_uid_tlb_record_context requires a published DUT global sample")
        end
        if (!uid_tlb_record_by_uid.exists(uid) || uid_tlb_record_by_uid[uid] == null) begin
            record = memblock_uid_tlb_record::type_id::create($sformatf("uid_tlb_record_%0d", uid));
            uid_tlb_record_by_uid[uid] = record;
        end else begin
            record = uid_tlb_record_by_uid[uid];
        end
        if (record.record_valid) begin
            case (record.uid_tlb_wait_state)
                memblock_uid_tlb_record::MEMBLOCK_UID_TLB_WAITING: begin
                    if (record.csr_snapshot == null) begin
                        `uvm_fatal("COMMON_DATA",
                                   $sformatf("WAITING uid=%0d has null CSR snapshot", uid))
                    end
                    if (uid_tlb_waiting_context_matches(uid, record, vpn, s2xlate,
                                                        is_hypervisor_inst,
                                                        csr_snapshot)) begin
                        check_waiting_uid_index_membership(
                            uid, record, "update_uid_tlb_record_context");
                        return;
                    end
                    `uvm_fatal("COMMON_DATA",
                               $sformatf("uid=%0d attempts to replace an active L2TLB WAITING instance old_vpn=0x%0h old_asid=0x%0h old_vmid=0x%0h old_s2xlate=%0d old_csr_seq=%0d new_vpn=0x%0h new_s2xlate=%0d new_csr_seq=%0d",
                                         uid,
                                         record.lookup_key.vpn,
                                         record.lookup_key.asid,
                                         record.lookup_key.vmid,
                                         record.lookup_key.s2xlate,
                                         record.csr_snapshot.update_seq,
                                         vpn,
                                         s2xlate,
                                         csr_snapshot.update_seq))
                end
                memblock_uid_tlb_record::MEMBLOCK_UID_TLB_COMPLETED: begin
                    if (!record.pte_valid) begin
                        `uvm_fatal("COMMON_DATA",
                                   $sformatf("COMPLETED uid=%0d has pte_valid=0", uid))
                    end
                    // A real reissue starts a fresh WAITING epoch below.
                    // init_context() resets only the new-attempt context and
                    // derived fields; record.payload remains historical data.
                end
                memblock_uid_tlb_record::MEMBLOCK_UID_TLB_CANCELED: begin
                    if (record.pte_valid) begin
                        `uvm_fatal("COMMON_DATA",
                                   $sformatf("CANCELED uid=%0d has pte_valid=1", uid))
                    end
                end
                default: begin
                    `uvm_fatal("COMMON_DATA",
                               $sformatf("uid=%0d has invalid L2TLB wait state=%0d",
                                         uid, record.uid_tlb_wait_state))
                end
            endcase
        end
        record.init_context(uid, vpn, s2xlate, is_hypervisor_inst, csr_snapshot,
                            current_sample);
        add_waiting_uid_to_index(uid, record);
    endfunction:update_uid_tlb_record_context

    // 抽象职责：回放严格早于 request-fire 的 CSR write，使该 request 使用与
    // DUT 同边沿顺序一致的旧/新 PMA/PMP generation。该函数不扫描主表，只消费
    // monitor FIFO 的连续前缀；同 sample 写入留给后续 request。
    function void apply_pma_pmp_csr_writes_before_request(
        input longint unsigned request_sample_seq
    );
        memblock_sync_pkg::dispatch_raw_pma_pmp_csr_write_t write_event;

        if (request_sample_seq == 0) begin
            `uvm_fatal("COMMON_DATA", "PMA/PMP CSR replay requires non-zero request sample")
        end
        if (pma_pmp_model == null) begin
            `uvm_fatal("COMMON_DATA", "PMA/PMP model is unavailable during CSR replay")
        end
        while (memblock_sync_pkg::peek_raw_pma_pmp_csr_write(write_event) &&
               write_event.sample_seq < request_sample_seq) begin
            if (!memblock_sync_pkg::pop_raw_pma_pmp_csr_write(write_event)) begin
                `uvm_fatal("COMMON_DATA", "PMA/PMP CSR FIFO peek/pop lost an event")
            end
            if (!write_event.valid || write_event.sample_seq == 0 ||
                write_event.sample_seq <= pma_pmp_last_applied_csr_sample) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("PMA/PMP CSR write sample is not strictly monotonic event=%0d applied=%0d",
                                     write_event.sample_seq,
                                     pma_pmp_last_applied_csr_sample))
            end
            pma_pmp_model.apply_csr_write(write_event.addr, write_event.data,
                                          write_event.sample_seq);
            pma_pmp_last_applied_csr_sample = write_event.sample_seq;
        end
    endfunction:apply_pma_pmp_csr_writes_before_request

    // 抽象职责：向 readonly façade 提供 UID 在真实 request-fire 时冻结的
    // PMA/PMP context。该 helper 不创建 singleton、也不补写缺失 context。
    function bit read_pma_pmp_uid_context(
        input memblock_uid_t uid,
        output pma_pmp_uid_context_t context_view
    );
        context_view = '{default:'0};
        return pma_pmp_model != null &&
               pma_pmp_model.read_uid_context(uid, context_view);
    endfunction:read_pma_pmp_uid_context

    // 抽象职责：读取指定 dynamic epoch 的冻结 PMA/PMP context。旧 redirect 实例
    // 不能借用同 UID 的新 context，避免延迟 writeback 读取到错误 generation。
    function bit read_pma_pmp_uid_context_for_epoch(
        input memblock_uid_t uid,
        input int unsigned dynamic_epoch,
        output pma_pmp_uid_context_t context_view
    );
        context_view = '{default:'0};
        return pma_pmp_model != null &&
               pma_pmp_model.read_uid_context_for_epoch(uid, dynamic_epoch,
                                                        context_view);
    endfunction:read_pma_pmp_uid_context_for_epoch

    // 抽象职责：用已经冻结的 UID context 评估完整 post-TLB PMA/PMP AF；
    // 该查询不读取 live CSR、不修改表，也不会把 TLB fault 后的访问误判为有效。
    function bit evaluate_pma_pmp_for_uid(
        input memblock_uid_t uid,
        input bit translation_success,
        input bit [47:0] paddr,
        input int unsigned size_bytes,
        input pma_pmp_cmd_e cmd,
        output pma_pmp_eval_t result
    );
        result = '{default:'0};
        return pma_pmp_model != null &&
               pma_pmp_model.evaluate_for_uid(uid, translation_success, paddr,
                                              size_bytes, cmd, result);
    endfunction:evaluate_pma_pmp_for_uid

    // 抽象职责：用指定 dynamic epoch 的上下文评估 post-TLB PMA/PMP。该路径是
    // RM 延迟在 LDA/STA sample 查询时的唯一入口，绝不回读当前 live CSR 表。
    function bit evaluate_pma_pmp_for_uid_epoch(
        input memblock_uid_t uid,
        input int unsigned dynamic_epoch,
        input bit translation_success,
        input bit [47:0] paddr,
        input int unsigned size_bytes,
        input pma_pmp_cmd_e cmd,
        output pma_pmp_eval_t result
    );
        result = '{default:'0};
        return pma_pmp_model != null &&
               pma_pmp_model.evaluate_for_uid_epoch(uid, dynamic_epoch,
                                                    translation_success, paddr,
                                                    size_bytes, cmd, result);
    endfunction:evaluate_pma_pmp_for_uid_epoch

    // Abstract responsibility: use the bounded shape index after a real
    // DTLB->L2TLB request fire.  It records only fire provenance and does not
    // create a token-to-UID ownership relation.
    function int unsigned mark_waiting_uid_records_on_request_fire(
        input memblock_tlb_lookup_key_t key,
        input longint unsigned sample_seq);
        int unsigned marked_count;
        int          idx;
        memblock_uid_t uid;
        memblock_uid_tlb_record record;
        memblock_uid_tlb_wait_shape_key_t shape_key;
        memblock_tlb_lookup_key_t request_key;
        memblock_tlb_lookup_key_t request_candidate_key;
        memblock_tlb_lookup_key_t candidate_key;
        memblock_sync_pkg::dispatch_raw_csr_t request_raw_csr;
        mmu_csr_runtime_state request_csr_snapshot;

        check_l2tlb_uid_registration_open(
            "mark_waiting_uid_records_on_request_fire");
        if (sample_seq == 0) begin
            `uvm_fatal("COMMON_DATA",
                       "mark_waiting_uid_records_on_request_fire requires non-zero sample_seq")
        end
        if (sample_seq > memblock_sync_pkg::peek_current_dut_global_sample()) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("mark_waiting_uid_records_on_request_fire got future sample=%0d latest=%0d key vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d",
                                 sample_seq,
                                 memblock_sync_pkg::peek_current_dut_global_sample(),
                                 key.vpn, key.asid, key.vmid, key.s2xlate))
        end

        // Rebuild the exact C-2 request context rather than consulting the
        // mutable runtime latest.  key must be the same key captured by the
        // pending request at this real fire boundary.
        if (!memblock_sync_pkg::get_l2tlb_request_csr_history(sample_seq,
                                                               request_raw_csr) ||
            !request_raw_csr.valid) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("missing valid L2TLB request C-2 CSR for request-fire sample=%0d",
                                 sample_seq))
        end
        request_csr_snapshot = mmu_csr_runtime_state::type_id::create(
            $sformatf("uid_request_fire_csr_%0d", sample_seq));
        if (request_csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA",
                       "failed to allocate L2TLB request-fire C-2 CSR snapshot")
        end
        request_csr_snapshot.reset();
        request_csr_snapshot.update_from_raw_csr(request_raw_csr);
        request_key = request_csr_snapshot.make_lookup_key({12'b0, key.vpn},
                                                            key.s2xlate);
        if (request_key != key) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("L2TLB request-fire key is not from its C-2 CSR sample=%0d key vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d C-2 asid=0x%0h vmid=0x%0h",
                                 sample_seq, key.vpn, key.asid, key.vmid,
                                 key.s2xlate, request_key.asid,
                                 request_key.vmid))
        end

        apply_pma_pmp_csr_writes_before_request(sample_seq);

        shape_key = make_uid_tlb_wait_shape_key(key.vpn, key.s2xlate);
        prune_uid_waiting_index_bucket(
            shape_key, "mark_waiting_uid_records_on_request_fire");
        marked_count = 0;
        if (uid_waiting_by_vpn_s2xlate.exists(shape_key)) begin
            for (idx = 0; idx < int'(uid_waiting_by_vpn_s2xlate[shape_key].size());
                 idx++) begin
                uid = uid_waiting_by_vpn_s2xlate[shape_key][idx];
                if (!uid_tlb_record_by_uid.exists(uid) ||
                    uid_tlb_record_by_uid[uid] == null ||
                    !uid_tlb_record_by_uid[uid].record_valid ||
                    !uid_tlb_record_by_uid[uid].is_waiting()) begin
                    `uvm_fatal("COMMON_DATA",
                               $sformatf("L2TLB WAITING index changed during request-fire marking uid=%0d",
                                         uid))
                end
                record = uid_tlb_record_by_uid[uid];
                if (record.uid != uid || record.vpn != key.vpn ||
                    record.s2xlate != key.s2xlate) begin
                    `uvm_fatal("COMMON_DATA",
                               $sformatf("L2TLB WAITING index shape mismatch uid=%0d",
                                         uid))
                end
                if (record.csr_snapshot == null) begin
                    `uvm_fatal("COMMON_DATA",
                               $sformatf("WAITING uid=%0d has null request CSR snapshot",
                                         uid))
                end
                // The bounded shape bucket is only a candidate filter.  First
                // replay the pending request's C-2 context, then require the
                // UID's own frozen request context to denote the same key.
                request_candidate_key = request_csr_snapshot.make_lookup_key(
                    {12'b0, record.vpn}, record.s2xlate);
                candidate_key = record.csr_snapshot.make_lookup_key(
                    {12'b0, record.vpn}, record.s2xlate);
                if (request_candidate_key != key || candidate_key != key) begin
                    continue;
                end
                if (record.uid_tlb_first_request_fire_sample_seq == 0) begin
                    // One request fire may mark several waiting UIDs.  The
                    // record remains independent of this request token.
                    record.mark_request_fire(sample_seq);
                    if (status_by_uid[uid] == null ||
                        !pma_pmp_model.capture_uid_context(
                            uid,
                            status_by_uid[uid].dynamic_epoch,
                            request_csr_snapshot.priv_dmode,
                            request_csr_snapshot.priv_debug,
                            1'b0,
                            1'b0,
                            '0,
                            request_csr_snapshot.update_seq,
                            sample_seq)) begin
                        `uvm_fatal("COMMON_DATA",
                                   $sformatf("failed to freeze PMA/PMP context uid=%0d sample=%0d",
                                             uid, sample_seq))
                    end
                    marked_count++;
                end
            end
        end

        if (marked_count == 0) begin
            `uvm_info("COMMON_DATA",
                      $sformatf("no unmarked WAITING UID matches L2TLB request fire key vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d sample=%0d; allow duplicate/prefetch/no-UID request",
                                key.vpn, key.asid, key.vmid, key.s2xlate, sample_seq),
                      UVM_LOW)
        end
        return marked_count;
    endfunction:mark_waiting_uid_records_on_request_fire

    // Compatibility API for the existing responder call site.  key is frozen
    // from that request's C-2 CSR snapshot before this helper is invoked.
    function int unsigned mark_uid_tlb_record_request_fire(
        input memblock_tlb_lookup_key_t key,
        input longint unsigned sample_seq);
        return mark_waiting_uid_records_on_request_fire(key, sample_seq);
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
        remove_waiting_uid_from_index(
            uid, record, "cancel_waiting_uid_tlb_record_for_uid");
        `uvm_info("COMMON_DATA",
                  $sformatf("cancel WAITING uid_tlb_record uid=%0d reason=%s key vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d sample=%0d",
                            uid, reason,
                            record.lookup_key.vpn,
                            record.lookup_key.asid,
                            record.lookup_key.vmid,
                            record.lookup_key.s2xlate,
                            record.uid_tlb_first_request_fire_sample_seq),
                  UVM_LOW)
        return 1;
    endfunction:cancel_waiting_uid_tlb_record_for_uid

    function int unsigned cancel_waiting_uid_tlb_records(input string reason = "");
        int unsigned cancel_count;

        cancel_count = 0;
        foreach (uid_tlb_record_by_uid[uid]) begin
            memblock_uid_tlb_record record;

            record = uid_tlb_record_by_uid[uid];
            if (record != null && record.is_waiting()) begin
                record.mark_canceled();
                cancel_count++;
            end
        end
        // Runtime reset owns every WAITING instance: state is transitioned
        // above before the whole secondary index is discarded.
        clear_uid_waiting_by_vpn_s2xlate();
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
                record.uid_tlb_first_request_fire_sample_seq == 0 ||
                record.uid_tlb_first_request_fire_sample_seq > anchor_sample_seq) begin
                continue;
            end
            record.mark_canceled();
            remove_waiting_uid_from_index(
                uid, record, "cancel_waiting_uid_tlb_records_through_sample");
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

    // Abstract responsibility: close UID TLB candidates that were registered
    // at issue time but never became an actual DTLB->L2TLB request before the
    // owner closed admission.  This is a proven no-request outcome, not a
    // substitute for draining a real response.
    function int unsigned cancel_unbound_uid_tlb_records_at_release(
        input string reason = "");
        int unsigned cancel_count;

        cancel_count = 0;
        foreach (uid_tlb_record_by_uid[uid]) begin
            memblock_uid_tlb_record record;

            record = uid_tlb_record_by_uid[uid];
            if (record == null || !record.is_waiting() ||
                record.uid_tlb_first_request_fire_sample_seq != 0) begin
                continue;
            end
            record.mark_canceled();
            remove_waiting_uid_from_index(
                uid, record, "cancel_unbound_uid_tlb_records_at_release");
            cancel_count++;
            `uvm_info("COMMON_DATA",
                      $sformatf("cancel unbound L2TLB UID candidate uid=%0d wait_epoch=%0d reason=%s key vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d",
                                uid,
                                record.uid_tlb_wait_epoch,
                                reason,
                                record.lookup_key.vpn,
                                record.lookup_key.asid,
                                record.lookup_key.vmid,
                                record.lookup_key.s2xlate),
                      UVM_LOW)
        end
        return cancel_count;
    endfunction:cancel_unbound_uid_tlb_records_at_release

    function bit has_waiting_uid_tlb_record();
        foreach (uid_tlb_record_by_uid[uid]) begin
            if (uid_tlb_record_by_uid[uid] != null &&
                uid_tlb_record_by_uid[uid].is_waiting()) begin
                return 1'b1;
            end
        end
        return 1'b0;
    endfunction:has_waiting_uid_tlb_record

    // Abstract responsibility: provide the normal-release caller a complete
    // diagnostic count.  This low-frequency scan deliberately does not alter
    // UID state or clean the index, because WAITING must block release.
    function void check_l2tlb_release_uid_waiting(
        output int unsigned waiting_count);
        memblock_uid_tlb_record record;

        waiting_count = 0;
        foreach (uid_tlb_record_by_uid[uid]) begin
            record = uid_tlb_record_by_uid[uid];
            if (record == null || !record.record_valid ||
                record.uid_tlb_wait_state !=
                    memblock_uid_tlb_record::MEMBLOCK_UID_TLB_WAITING) begin
                continue;
            end
            waiting_count++;
            `uvm_info("COMMON_DATA",
                      $sformatf("L2TLB release blocked by WAITING uid=%0d wait_epoch=%0d wait_start_sample=%0d key vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d pte_valid=%0d",
                                uid,
                                record.uid_tlb_wait_epoch,
                                record.uid_wait_start_sample_seq,
                                record.lookup_key.vpn,
                                record.lookup_key.asid,
                                record.lookup_key.vmid,
                                record.lookup_key.s2xlate,
                                record.pte_valid),
                      UVM_LOW)
        end
    endfunction:check_l2tlb_release_uid_waiting

    function bit tlb_request_s1_vpn_fits_mode(
        input bit [51:0] vpn,
        input bit [3:0] mode);
        case (mode)
            MEMBLOCK_SV39_MODE: return !(|vpn[51:27]);
            MEMBLOCK_SV48_MODE: return !(|vpn[51:36]);
            default: return 1'b0;
        endcase
    endfunction:tlb_request_s1_vpn_fits_mode

    function bit tlb_request_s2_gvpn_fits_mode(
        input bit [43:0] gvpn,
        input bit [3:0] mode);
        case (mode)
            MEMBLOCK_SV39_MODE: return !(|gvpn[43:29]);
            MEMBLOCK_SV48_MODE: return !(|gvpn[43:38]);
            default: return 1'b0;
        endcase
    endfunction:tlb_request_s2_gvpn_fits_mode

    function bit [43:0] resolve_tlb_request_ppn_from_raw(
        input bit [43:0] canonical_ppn,
        input bit [43:0] request_vpn,
        input bit [1:0] level,
        input bit pte_n);
        case (level)
            2'd3: return {canonical_ppn[43:27], request_vpn[26:0]};
            2'd2: return {canonical_ppn[43:18], request_vpn[17:0]};
            2'd1: return {canonical_ppn[43:9], request_vpn[8:0]};
            default: begin
                if (pte_n) begin
                    return {canonical_ppn[43:4], request_vpn[3:0]};
                end
                return canonical_ppn;
            end
        endcase
    endfunction:resolve_tlb_request_ppn_from_raw

    function bit tlb_request_napot_ppn_is_resolvable(
        input bit s1,
        input bit pte_n,
        input bit [43:0] canonical_ppn,
        input bit [3:0] pte_mode,
        input bit [51:0] request_vpn,
        input bit [1:0] request_s2xlate);
        if (!pte_n || canonical_ppn[3:0] == 4'b1000) begin
            return 1'b1;
        end
        if (pte_mode ==
            memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL) begin
            `uvm_fatal("L2TLB_PAYLOAD_NAPOT",
                       $sformatf("LEGAL %s NAPOT raw PPN=0x%0h is not encoded vpn=0x%0h s2xlate=%0d",
                                 s1 ? "S1" : "S2", canonical_ppn,
                                 request_vpn, request_s2xlate))
        end
        `uvm_info("L2TLB_PAYLOAD_NAPOT",
                  $sformatf("non-LEGAL %s NAPOT raw PPN=0x%0h leaves derived fields invalid vpn=0x%0h s2xlate=%0d",
                            s1 ? "S1" : "S2", canonical_ppn,
                            request_vpn, request_s2xlate), UVM_LOW)
        return 1'b0;
    endfunction:tlb_request_napot_ppn_is_resolvable

    function void note_tlb_request_derived_invalid(
        input memblock_tlb_entry entry,
        input bit [51:0] request_vpn,
        input bit [1:0] request_s2xlate,
        input string reason);
        `uvm_info("COMMON_DATA",
                  $sformatf("leave response-derived fields invalid generation=%0d vpn=0x%0h s2xlate=%0d reason=%s",
                            entry.entry_generation, request_vpn,
                            request_s2xlate, reason), UVM_LOW)
    endfunction:note_tlb_request_derived_invalid

    // Abstract responsibility: derive request-specific normal PPN/GVPN from
    // one frozen raw response and the caller's VPN.  It neither mutates the
    // entry nor uses a later runtime CSR, so pending capture and UID multicast
    // can share it without copying an anchor's derived fields.
    function void derive_tlb_request_fields(
        input memblock_tlb_entry entry,
        input bit [51:0] request_vpn,
        input bit [1:0] request_s2xlate,
        input mmu_csr_runtime_state response_csr,
        output bit valid,
        output bit [43:0] s1_ppn,
        output bit [43:0] s2_ppn,
        output bit [51:0] gvpn);
        bit s1_expected;
        bit s2_expected;
        bit [3:0] s1_response_mode;
        bit [3:0] s2_response_mode;
        bit [43:0] s1_canonical_ppn;
        bit [43:0] s2_canonical_ppn;
        bit [43:0] resolved_s1_ppn;
        bit [43:0] resolved_s2_ppn;
        bit [43:0] s2_input_gvpn;

        valid = 1'b0;
        s1_ppn = '0;
        s2_ppn = '0;
        gvpn = '0;
        if (entry == null || response_csr == null) begin
            `uvm_fatal("COMMON_DATA",
                       "derive_tlb_request_fields requires entry and response C-2 CSR")
        end
        if (entry.s2xlate != request_s2xlate) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("derived request s2xlate mismatch entry=%0d request=%0d",
                                 entry.s2xlate, request_s2xlate))
        end
        if (entry.has_effective_fault()) begin
            return;
        end

        s1_expected = 1'b0;
        s2_expected = 1'b0;
        s1_response_mode = '0;
        s2_response_mode = '0;
        case (request_s2xlate)
            2'd0: begin
                s1_expected = 1'b1;
                s1_response_mode = response_csr.satp_mode;
            end
            2'd1: begin
                s1_expected = 1'b1;
                s1_response_mode = response_csr.vsatp_mode;
            end
            2'd2: begin
                s2_expected = 1'b1;
                s2_response_mode = response_csr.hgatp_mode;
            end
            2'd3: begin
                s1_expected = 1'b1;
                s2_expected = 1'b1;
                s1_response_mode = response_csr.vsatp_mode;
                s2_response_mode = response_csr.hgatp_mode;
            end
            default: begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("unsupported derived request s2xlate=%0d",
                                     request_s2xlate))
            end
        endcase
        if (entry.s1_stage_active != s1_expected ||
            entry.s2_stage_active != s2_expected) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("derived request stage shape mismatch s2xlate=%0d",
                                 request_s2xlate))
        end
        entry.validate_s1_sector_payload_consistency("REQUEST_DERIVED");
        if (s1_expected &&
            ((s1_response_mode != MEMBLOCK_SV39_MODE &&
              s1_response_mode != MEMBLOCK_SV48_MODE) ||
             s1_response_mode != entry.s1_translation_mode_at_build)) begin
            note_tlb_request_derived_invalid(entry, request_vpn,
                request_s2xlate,
                "response-visible S1 mode is unsupported or differs from raw payload");
            return;
        end
        if (s2_expected &&
            ((s2_response_mode != MEMBLOCK_SV39_MODE &&
              s2_response_mode != MEMBLOCK_SV48_MODE) ||
             s2_response_mode != entry.s2_translation_mode_at_build)) begin
            note_tlb_request_derived_invalid(entry, request_vpn,
                request_s2xlate,
                "response-visible S2 mode is unsupported or differs from raw payload");
            return;
        end

        // Derived fields are only meaningful for an active normal leaf.  V2
        // has no persisted S2 V bit, so S2 leaf-ness is established by R/W/X.
        if (s1_expected &&
            (!entry.s1_pte_v ||
             !(entry.s1_pte_r || entry.s1_pte_w || entry.s1_pte_x))) begin
            note_tlb_request_derived_invalid(entry, request_vpn,
                request_s2xlate,
                "active S1 payload is fake or a valid non-leaf");
            return;
        end
        if (s2_expected &&
            !(entry.s2_pte_r || entry.s2_pte_w || entry.s2_pte_x)) begin
            note_tlb_request_derived_invalid(entry, request_vpn,
                request_s2xlate,
                "active S2 payload is fake or a non-leaf");
            return;
        end

        resolved_s1_ppn = '0;
        resolved_s2_ppn = '0;
        if (s1_expected) begin
            if (!tlb_request_s1_vpn_fits_mode(request_vpn,
                                               s1_response_mode)) begin
                note_tlb_request_derived_invalid(entry, request_vpn,
                    request_s2xlate,
                    "UID VPN does not fit response-visible S1 mode");
                return;
            end
            s1_canonical_ppn = {entry.s1_entry_ppn_raw,
                                entry.s1_ppn_low[request_vpn[2:0]]};
            if (!tlb_request_napot_ppn_is_resolvable(
                    1'b1, entry.s1_pte_n, s1_canonical_ppn,
                    entry.s1_pte_mode_at_build,
                    request_vpn, request_s2xlate)) begin
                return;
            end
            resolved_s1_ppn = resolve_tlb_request_ppn_from_raw(
                s1_canonical_ppn, {6'b0, request_vpn[37:0]},
                entry.s1_level, entry.s1_pte_n);
        end

        if (s2_expected) begin
            if (request_s2xlate == 2'd2) begin
                if (|request_vpn[51:38]) begin
                    note_tlb_request_derived_invalid(entry, request_vpn,
                        request_s2xlate,
                        "onlyStage2 UID VPN exceeds raw GVPN width");
                    return;
                end
                s2_input_gvpn = {6'b0, request_vpn[37:0]};
            end else begin
                s2_input_gvpn = resolved_s1_ppn;
            end
            if (!tlb_request_s2_gvpn_fits_mode(s2_input_gvpn,
                                                s2_response_mode)) begin
                note_tlb_request_derived_invalid(entry, request_vpn,
                    request_s2xlate,
                    "UID GVPN does not fit response-visible S2 mode");
                return;
            end
            s2_canonical_ppn = {6'b0, entry.s2_entry_ppn_raw};
            if (!tlb_request_napot_ppn_is_resolvable(
                    1'b0, entry.s2_pte_n, s2_canonical_ppn,
                    entry.s2_pte_mode_at_build,
                    request_vpn, request_s2xlate)) begin
                return;
            end
            resolved_s2_ppn = resolve_tlb_request_ppn_from_raw(
                s2_canonical_ppn, s2_input_gvpn, entry.s2_level,
                entry.s2_pte_n);
        end

        if (request_s2xlate == 2'd3) begin
            gvpn = {8'b0, resolved_s1_ppn};
        end else begin
            gvpn = {14'b0, request_vpn[37:0]};
        end
        s1_ppn = resolved_s1_ppn;
        s2_ppn = resolved_s2_ppn;
        valid = 1'b1;
    endfunction:derive_tlb_request_fields

    // Abstract responsibility: copy the generic request-specific derived
    // result into one UID record after raw payload copy and before completion.
    function void populate_uid_record_derived(
        input memblock_uid_tlb_record record,
        input memblock_tlb_entry entry,
        input mmu_csr_runtime_state response_filter_csr_snapshot);
        bit derived_valid;
        bit [43:0] derived_s1_ppn;
        bit [43:0] derived_s2_ppn;
        bit [51:0] derived_gvpn;

        if (record == null || entry == null ||
            response_filter_csr_snapshot == null || record.payload == null ||
            !record.is_waiting()) begin
            `uvm_fatal("COMMON_DATA",
                       "populate_uid_record_derived requires copied WAITING UID payload and response C-2 CSR")
        end
        if (record.s2xlate != entry.s2xlate ||
            record.payload.s2xlate != entry.s2xlate ||
            record.payload.entry_generation != entry.entry_generation) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("UID payload copy provenance mismatch uid=%0d", record.uid))
        end
        record.payload.validate_s1_sector_payload_consistency("UID_DERIVED",
                                                               entry);
        derive_tlb_request_fields(record.payload, record.vpn, record.s2xlate,
                                  response_filter_csr_snapshot, derived_valid,
                                  derived_s1_ppn, derived_s2_ppn, derived_gvpn);
        record.request_derived_valid = derived_valid;
        record.request_s1_resolved_ppn = derived_s1_ppn;
        record.request_s2_resolved_ppn = derived_s2_ppn;
        record.request_gvpn = derived_gvpn;
    endfunction:populate_uid_record_derived

    // PtwRespS2.hit() compares the response's raw VPN anchor rather than the
    // framework lookup key.  This helper implements its level/NAPOT prefix
    // comparison for the 38-bit V2 request VPN/GVPN width.
    function bit raw_l2tlb_vpn_matches_level(
        input bit [37:0] response_anchor_vpn,
        input bit [51:0] request_vpn,
        input bit [1:0] level,
        input bit pte_n);
        if (|request_vpn[51:38]) begin
            return 1'b0;
        end
        case (level)
            2'd0: begin
                if (pte_n) begin
                    return response_anchor_vpn[37:4] == request_vpn[37:4];
                end
                return response_anchor_vpn == request_vpn[37:0];
            end
            2'd1: return response_anchor_vpn[37:9] == request_vpn[37:9];
            2'd2: return response_anchor_vpn[37:18] == request_vpn[37:18];
            default: return response_anchor_vpn[37:27] == request_vpn[37:27];
        endcase
    endfunction:raw_l2tlb_vpn_matches_level

    // PtwSectorResp.hit() is used only by noS2xlate.  A normal level-0
    // response matches its tag and the request-selected valididx bit; its
    // addr_low is not an address-match input in that path.
    function bit s1_sector_response_matches_request(
        input memblock_tlb_entry entry,
        input bit [51:0] request_vpn);
        bit [37:0] sector_base_vpn;

        if (entry == null) begin
            `uvm_fatal("COMMON_DATA",
                       "s1_sector_response_matches_request got null entry")
        end
        if (|request_vpn[51:38]) begin
            return 1'b0;
        end
        if (entry.s1_level == 2'd0 && !entry.s1_pte_n) begin
            return entry.s1_tag == request_vpn[37:3] &&
                   entry.s1_valididx[request_vpn[2:0]];
        end
        sector_base_vpn = {entry.s1_tag, 3'b000};
        return raw_l2tlb_vpn_matches_level(sector_base_vpn, request_vpn,
                                           entry.s1_level, entry.s1_pte_n);
    endfunction:s1_sector_response_matches_request

    // allStage accepts at the smaller of the two raw page sizes.  Its NAPOT
    // condition is valid only when the other stage does not reduce coverage.
    function void derive_allstage_lookup_shape(
        input memblock_tlb_entry entry,
        output bit [1:0] effective_level,
        output bit effective_n);
        if (entry == null || !entry.s1_stage_active ||
            !entry.s2_stage_active) begin
            `uvm_fatal("COMMON_DATA",
                       "derive_allstage_lookup_shape requires active S1 and S2")
        end
        effective_level = (entry.s1_level < entry.s2_level) ?
                          entry.s1_level : entry.s2_level;
        effective_n = (entry.s1_pte_n && entry.s2_level != 2'd0) ||
                      (entry.s2_pte_n && entry.s1_level != 2'd0) ||
                      (entry.s1_pte_n && entry.s2_pte_n);
    endfunction:derive_allstage_lookup_shape

    // Abstract responsibility: reproduce V2 PtwRespS2.hit() exactly for one
    // response payload.  It is a pure raw match: no lookup-key fallback,
    // UID ownership, payload mutation, or timing side effect is permitted.
    function bit entry_matches_request_raw(
        input memblock_tlb_entry entry,
        input bit [51:0] request_vpn,
        input bit [1:0] request_s2xlate,
        input mmu_csr_runtime_state response_filter_csr_snapshot);
        bit [37:0] s1_response_anchor_vpn;
        bit [1:0]  allstage_level;
        bit        allstage_n;
        bit [15:0] response_asid;
        bit [15:0] response_vmid;

        if (entry == null || response_filter_csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "entry_matches_request_raw got null input")
        end
        if (entry.s2xlate != request_s2xlate) begin
            return 1'b0;
        end

        case (request_s2xlate)
            2'd0: begin
                if (!entry.s1_stage_active || entry.s2_stage_active) begin
                    `uvm_fatal("COMMON_DATA",
                               "noS2xlate entry has invalid stage-active shape")
                end
                response_asid =
                    response_filter_csr_snapshot.current_asid(request_s2xlate);
                return ((entry.s1_asid == response_asid) || entry.s1_pte_g) &&
                       s1_sector_response_matches_request(entry, request_vpn);
            end
            2'd1: begin
                if (!entry.s1_stage_active || entry.s2_stage_active) begin
                    `uvm_fatal("COMMON_DATA",
                               "onlyStage1 entry has invalid stage-active shape")
                end
                response_asid =
                    response_filter_csr_snapshot.current_asid(request_s2xlate);
                response_vmid =
                    response_filter_csr_snapshot.current_vmid(request_s2xlate);
                s1_response_anchor_vpn = {entry.s1_tag, entry.s1_addr_low};
                return ((entry.s1_asid == response_asid) || entry.s1_pte_g) &&
                       (entry.s1_vmid == response_vmid) &&
                       raw_l2tlb_vpn_matches_level(s1_response_anchor_vpn,
                                                   request_vpn,
                                                   entry.s1_level,
                                                   entry.s1_pte_n);
            end
            2'd2: begin
                if (entry.s1_stage_active || !entry.s2_stage_active) begin
                    `uvm_fatal("COMMON_DATA",
                               "onlyStage2 entry has invalid stage-active shape")
                end
                response_vmid =
                    response_filter_csr_snapshot.current_vmid(request_s2xlate);
                return (entry.s2_vmid == response_vmid) &&
                       raw_l2tlb_vpn_matches_level(entry.s2_tag, request_vpn,
                                                   entry.s2_level,
                                                   entry.s2_pte_n);
            end
            2'd3: begin
                if (!entry.s1_stage_active || !entry.s2_stage_active) begin
                    `uvm_fatal("COMMON_DATA",
                               "allStage entry has invalid stage-active shape")
                end
                response_asid =
                    response_filter_csr_snapshot.current_asid(request_s2xlate);
                response_vmid =
                    response_filter_csr_snapshot.current_vmid(request_s2xlate);
                derive_allstage_lookup_shape(entry, allstage_level, allstage_n);
                s1_response_anchor_vpn = {entry.s1_tag, entry.s1_addr_low};
                return ((entry.s1_asid == response_asid) || entry.s1_pte_g) &&
                       (entry.s1_vmid == response_vmid) &&
                       raw_l2tlb_vpn_matches_level(s1_response_anchor_vpn,
                                                   request_vpn,
                                                   allstage_level,
                                                   allstage_n);
            end
            default: begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("unsupported L2TLB s2xlate=%0d",
                                     request_s2xlate))
            end
        endcase
        return 1'b0;
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
        if (!record.is_waiting()) begin
            return 1'b0;
        end
        return entry_matches_request_raw(entry, record.vpn,
                                         record.s2xlate,
                                         response_filter_csr_snapshot);
    endfunction:entry_matches_uid_at_response

    // Abstract responsibility: multicast one observed L2TLB response to all
    // WAITING UID records whose raw key matches under response-visible C-2
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
            if (record == null || !record.is_waiting() ||
                !entry_matches_uid_at_response(entry, record,
                                               response_filter_csr_snapshot)) begin
                continue;
            end
            record.copy_entry_fields(entry);
            populate_uid_record_derived(record, entry,
                                        response_filter_csr_snapshot);
            record.mark_completed();
            remove_waiting_uid_from_index(
                record.uid, record,
                "complete_waiting_uid_records_by_response");
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
        req.owner_valid   = 1'b0;
        req.owner         = '{default:'0};
        req.sb_is_empty_observation_seq_at_sendover = 0;
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

    // 抽象职责：为控制屏障生成带唯一 owner 的 flushSb 请求。它和周期性请求共用
    // 原有 FIFO/LSQ commit consumer；不会直接驱动接口，也不会把 owner 写进 monitor。
    function void push_owner_flushsb_request(
        input memblock_control_owner_t owner,
        output memblock_flushsb_req_t req
    );
        if (!owner.valid) begin
            `uvm_fatal("CONTROL_FLUSHSB", "owner flushSb request has invalid owner")
        end
        if ((attached_flushsb_req_valid && attached_flushsb_req.owner_valid) ||
            (active_flushsb_req_valid && active_flushsb_req.owner_valid) ||
            flushsb_completed.valid) begin
            `uvm_fatal("CONTROL_FLUSHSB",
                       "cannot enqueue a second owner flushSb request before prior completion is consumed")
        end
        foreach (flushsb_req_q[idx]) begin
            if (flushsb_req_q[idx].owner_valid) begin
                `uvm_fatal("CONTROL_FLUSHSB",
                           "owner flushSb request already exists in pending FIFO")
            end
        end
        req = '{default:'0};
        req.req_id        = next_flushsb_req_id;
        req.enqueue_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        req.source        = 0;
        req.owner_valid   = 1'b1;
        req.owner         = owner;
        req.sb_is_empty_observation_seq_at_sendover = 0;
        next_flushsb_req_id++;
        flushsb_req_q.push_back(req);
        `uvm_info("COMMON_DATA",
                  $sformatf("push owner flushSb request: req_id=%0d uid=%0d epoch=%0d gen=%0d kind=%0d queue_size=%0d",
                            req.req_id, owner.uid, owner.dynamic_epoch,
                            owner.action_generation, owner.kind,
                            flushsb_req_q.size()),
                  UVM_LOW)
    endfunction:push_owner_flushsb_request

    function bit has_pending_flushsb_request();
        return flushsb_req_q.size() != 0;
    endfunction:has_pending_flushsb_request

    function bit flushsb_busy();
        return attached_flushsb_req_valid || flushsb_waiting_empty ||
               active_flushsb_req_valid;
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

    // 抽象职责：记录 flushSb 已附着到尚未 finish_item 的 LSQ commit xaction。
    // 该阶段不能开启 sbIsEmpty 完成捕获，避免旧 high 在 driver 尚未实际交付前完成请求。
    function void mark_flushsb_request_attached_to_lsqcommit_xaction(
        input memblock_flushsb_req_t req,
        input longint unsigned cycle
    );
        if (attached_flushsb_req_valid || active_flushsb_req_valid ||
            flushsb_waiting_empty) begin
            `uvm_fatal("CONTROL_FLUSHSB", "flushSb attach while another request is active")
        end
        attached_flushsb_req = req;
        attached_flushsb_req_valid = 1'b1;
        flushsb_start_cycle = cycle;
        last_sb_is_empty = 1'b0;
        flushsb_timeout_warned = 1'b0;
    endfunction:mark_flushsb_request_attached_to_lsqcommit_xaction

    // 抽象职责：在同一 LSQ commit item 的 finish_item() 返回后登记真实 driver
    // sendover，并冻结 latest sbIsEmpty observation 作为新鲜完成的下界。
    function void mark_flushsb_request_driver_sendover(
        input memblock_flushsb_req_t req,
        input longint unsigned cycle
    );
        memblock_sync_pkg::memblock_control_level_observation_t observation;

        if (!attached_flushsb_req_valid ||
            attached_flushsb_req.req_id != req.req_id ||
            active_flushsb_req_valid || flushsb_waiting_empty) begin
            `uvm_fatal("CONTROL_FLUSHSB", "flushSb sendover does not match attached request")
        end
        void'(memblock_sync_pkg::get_latest_control_sb_is_empty_observation(observation));
        active_flushsb_req = attached_flushsb_req;
        active_flushsb_req.sb_is_empty_observation_seq_at_sendover =
            observation.observation_seq;
        active_flushsb_req_valid = 1'b1;
        attached_flushsb_req = '{default:'0};
        attached_flushsb_req_valid = 1'b0;
        flushsb_waiting_empty = 1'b1;
        flushsb_start_cycle = cycle;
        last_sb_is_empty = 1'b0;
        flushsb_timeout_warned = 1'b0;
        memblock_sync_pkg::dispatch_flushsb_waiting_empty = 1'b1;
        `uvm_info("COMMON_DATA",
                  $sformatf("flushSb driver sendover: req_id=%0d source=%0d owner=%0d uid=%0d baseline_obs=%0d",
                            active_flushsb_req.req_id, active_flushsb_req.source,
                            active_flushsb_req.owner_valid,
                            active_flushsb_req.owner.uid,
                            active_flushsb_req.sb_is_empty_observation_seq_at_sendover),
                  UVM_LOW)
    endfunction:mark_flushsb_request_driver_sendover

    // 抽象职责：消费 immutable ctrl raw 的 sbIsEmpty 采样，并只完成已经 sendover
    // 且 observation 序号更新的 active request。owner request 的完成事实保留到
    // control service 按 req_id+owner 确认，普通请求仍沿用完成后直接清 active 的语义。
    function void update_sb_is_empty(
        input memblock_sync_pkg::dispatch_raw_ctrl_t raw
    );
        last_sb_is_empty = raw.sb_is_empty;
        if (flushsb_waiting_empty && raw.sb_is_empty &&
            raw.sb_is_empty_observation_seq >
                active_flushsb_req.sb_is_empty_observation_seq_at_sendover) begin
            `uvm_info("COMMON_DATA",
                      $sformatf("flushSb request completed: req_id=%0d source=%0d start_cycle=%0d done_cycle=%0d",
                                active_flushsb_req.req_id,
                                active_flushsb_req.source,
                                flushsb_start_cycle,
                                memblock_sync_pkg::get_dispatch_service_cycle()),
                      UVM_LOW)
            if (active_flushsb_req.owner_valid) begin
                if (flushsb_completed.valid) begin
                    `uvm_fatal("CONTROL_FLUSHSB",
                               "owner flushSb completion slot was not consumed before next completion")
                end
                flushsb_completed.valid = 1'b1;
                flushsb_completed.req_id = active_flushsb_req.req_id;
                flushsb_completed.owner = active_flushsb_req.owner;
                flushsb_completed.observation_seq = raw.sb_is_empty_observation_seq;
                flushsb_completed.cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            end
            flushsb_waiting_empty    = 1'b0;
            active_flushsb_req       = '{default:'0};
            active_flushsb_req_valid = 1'b0;
            flushsb_start_cycle      = 0;
            flushsb_timeout_warned   = 1'b0;
            memblock_sync_pkg::dispatch_flushsb_waiting_empty = 1'b0;
        end
    endfunction:update_sb_is_empty

    // 抽象职责：让 control service 判断 owner 请求是否已经越过 driver sendover。
    // completed slot 也代表该事实已经发生，避免 monitor 完成过快时 service 漏掉中间 active 状态。
    function bit control_flushsb_sendover_seen(
        input memblock_control_owner_t owner,
        input int unsigned req_id
    );
        return (active_flushsb_req_valid && active_flushsb_req.owner_valid &&
                active_flushsb_req.req_id == req_id &&
                memblock_control_owner_equal(active_flushsb_req.owner, owner)) ||
               (flushsb_completed.valid && flushsb_completed.req_id == req_id &&
                memblock_control_owner_equal(flushsb_completed.owner, owner));
    endfunction:control_flushsb_sendover_seen

    // 抽象职责：由当前 control owner 取得并清除自己的 sbIsEmpty 完成事实。
    // 非匹配 owner/req_id 只返回 0，调用者保持等待；不会误消费周期性 flushSb。
    function bit try_consume_control_flushsb_completion(
        input memblock_control_owner_t owner,
        input int unsigned req_id,
        output memblock_flushsb_completion_t completion
    );
        completion = '{default:'0};
        if (!flushsb_completed.valid || flushsb_completed.req_id != req_id ||
            !memblock_control_owner_equal(flushsb_completed.owner, owner)) begin
            return 1'b0;
        end
        completion = flushsb_completed;
        flushsb_completed = '{default:'0};
        return 1'b1;
    endfunction:try_consume_control_flushsb_completion

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
