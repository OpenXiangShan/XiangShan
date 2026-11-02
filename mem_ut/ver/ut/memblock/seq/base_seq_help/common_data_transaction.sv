//=========================================================
//File name    : common_data_transaction.sv
//Author       : OpenAI_Codex
//Module name  : common_data_transaction
//Discribution : shared dispatch framework data owner
//Date         : 2026-05-18
//=========================================================
`ifndef COMMON_DATA_TRANSACTION__SV
`define COMMON_DATA_TRANSACTION__SV

// 成熟 raw 前缀的离线提交凭据；只含本 replay 状态，不包含 issue residency。
// prepare 冻结，commit 恰好消费一次；reset/model/raw revision 变化使其失效。
class memblock_pma_pmp_replay_ticket;
    bit consumed;
    longint unsigned reset_epoch;
    longint unsigned service_revision;
    longint unsigned raw_revision;
    memblock_sync_pkg::dispatch_raw_pma_pmp_csr_write_t events[$];
    pma_pmp_csr_replay_ticket model_ticket;
endclass:memblock_pma_pmp_replay_ticket

class common_data_transaction extends uvm_object;

    typedef enum int unsigned {
        MEMBLOCK_CONTROL_AUDIT_CREATION,
        MEMBLOCK_CONTROL_AUDIT_OWNER_BIND,
        MEMBLOCK_CONTROL_AUDIT_CSR_ENQUEUE,
        MEMBLOCK_CONTROL_AUDIT_CSR_START,
        MEMBLOCK_CONTROL_AUDIT_CSR_COMPLETION,
        MEMBLOCK_CONTROL_AUDIT_SFENCE_ENQUEUE,
        MEMBLOCK_CONTROL_AUDIT_SFENCE_START,
        MEMBLOCK_CONTROL_AUDIT_SFENCE_COMPLETION,
        MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_ENQUEUE,
        MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_START,
        MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_COMPLETION,
        MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_ENQUEUE,
        MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_START,
        MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_COMPLETION,
        MEMBLOCK_CONTROL_AUDIT_TERMINAL,
        MEMBLOCK_CONTROL_AUDIT_BARRIER_RELEASE
    } memblock_control_audit_event_e;

    typedef struct {
        bit                            valid;
        memblock_uid_t                 uid;
        memblock_control_audit_event_e event_kind;
        int unsigned                   generation;
    } memblock_control_lifecycle_ticket_t;

    typedef bit [53:0] memblock_uid_tlb_wait_shape_key_t;
    localparam bit [3:0] MEMBLOCK_SV39_MODE = 4'd8;
    localparam bit [3:0] MEMBLOCK_SV48_MODE = 4'd9;
    localparam int unsigned MEMBLOCK_STA_LATE_FAULT_TOMBSTONE_MAX =
        `MEMBLOCK_DUT_SQ_SIZE;
    // 中文注释：V2 LoadQueueUncache 的 loadMmio 相对外部 redirect monitor
    // anchor 最迟可在 R+2 出现。该常量只定义 overlap 候选窗口；R+2 仍需
    // redirect-deleted owner 与 status 历史共同唯一证明后才能 stale-drop。
    localparam longint unsigned MEMBLOCK_LOAD_MMIO_REDIRECT_MAX_SAMPLE_OFFSET = 2;
    localparam int unsigned MEMBLOCK_TLB_FALLBACK_WARNING_LIMIT = 4;
    // 中文注释：主表/公共队列已经收敛后，memory responder 仍允许接收尾请求的最短静默窗口。
    // 该值是 global stop 的固定框架时序，不是 testcase 可改的 plus 参数；DCache/SBuffer
    // 任一观察到新 activity 都会重新起算该窗口，避免在已知尾流量中途提交 global stop。
    localparam time MEMBLOCK_GLOBAL_STOP_PREPARE_QUIET_TIME = 1us;

    typedef struct {
        memblock_sfence_payload_t payload;
        longint unsigned          anchor_sample_seq;
        longint unsigned          due_sample_seq;
        longint unsigned          reset_epoch;
        longint unsigned          lifecycle_event_seq;
    } memblock_pending_sfence_invalidate_t;

    typedef struct {
        bit                 ppn_valid;
        bit [43:0]          ppn;
        bit [1:0]           s2xlate;
        longint unsigned    response_token;
        longint unsigned    complete_sample_seq;
    } memblock_l2tlb_ppn_history_record_t;

    // 中文注释：普通 scalar store 的 target fault 在 STA/STD 两个真实 writeback
    // 都到达前只保存在这里；它不是 fault terminal，也不拥有 ROB/LSQ 生命周期。
    typedef struct {
        bit                 valid;
        memblock_uid_t      uid;
        int unsigned        dynamic_epoch;
        int unsigned        sta_issue_epoch;
        int unsigned        std_issue_epoch;
        int unsigned        sta_replay_seq;
        int unsigned        std_replay_seq;
        bit                 sta_fault_seen;
        bit                 std_fault_seen;
        bit [23:0]          sta_exception_vec;
        bit [23:0]          std_exception_vec;
        longint unsigned    cycle;
    } memblock_pending_store_fault_t;

    static common_data_transaction m_inst;

    int unsigned   main_trans_num;
    memblock_uid_t next_uid;
    bit            main_table_ready;
    // global_stop_requested 只在 stop-prepare 静默窗口完成后由顶层 orchestration 置位。
    // 子sequence只读该标志进入收尾退出阶段，避免各自重复维护 completion 退出条件。
    bit            global_stop_requested;
    // 中文注释：global stop 的两阶段 prepare 状态。置位：所有 UID terminal 且公共 runtime
    // 队列收敛时；清零：prepare 期间公共 runtime work 再次出现、reset 或真正 global stop 前重建。
    // DCache/SBuffer 观察到 A/response/probe 等尾部 activity 时只刷新 last_activity_time，不清该位。
    // 作用：为 memory-facing responder 留出连续静默窗口，避免其已知尾请求被 global stop 截断。
    bit            global_stop_prepare_requested;
    time           global_stop_prepare_last_activity_time;
    // 中文注释：DCache/SBuffer 在 prepare 期间每个真实 responder sample 都回报自己的
    // 时间。stop commit 除了等待 1us 无 activity，还必须等两边各完成一次 deadline 之后的
    // sample，避免主 service 的 negedge 恰好先于同一仿真时刻 responder drv_cb 而漏看尾请求。
    time           global_stop_prepare_dcache_last_sample_time;
    time           global_stop_prepare_sbuffer_last_sample_time;
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
    // 中文注释：parent occupancy count记录一个上下文父区域中已有多少个更小
    // canonical live entry。真正miss只读count，不扫描live表或owner集合。
    int unsigned
        tlb_parent_occupancy_count[memblock_tlb_parent_occupancy_key_t];
    // 中文注释：owner set把父区域key与canonical anchor绑定。publish写入、统一
    // delete删除；只用于重复注册检查、精确撤销和fatal诊断。
    bit tlb_parent_owner_set[memblock_tlb_parent_owner_key_t];
    longint unsigned parent_lookup_count;
    longint unsigned parent_block_64k_count;
    longint unsigned parent_block_2m_count;
    longint unsigned parent_block_1g_count;
    longint unsigned parent_block_512g_count;
    longint unsigned pte_n_override_count;
    longint unsigned pte_g_override_count;
    longint unsigned forced_4k_fallback_count;
    int unsigned     parent_owner_peak_count;
    // Only a real sampled L2TLB response publishes this cache.  It is kept
    // separate from the live canonical table because live entries exist while
    // a miss is still pending and therefore are not completion evidence.
    memblock_tlb_entry completed_tlb_entry_by_key[memblock_tlb_lookup_key_t];
    memblock_tlb_lookup_key_t
        completed_tlb_anchor_keys_by_range_key[memblock_tlb_range_index_key_t][$];
    longint unsigned
        completed_tlb_response_token_by_key[memblock_tlb_lookup_key_t];
    longint unsigned
        completed_tlb_response_sample_by_key[memblock_tlb_lookup_key_t];
    // 中文注释：adapter 成功消费 raw fence 后只在此登记 C4 删除工作。
    // 该队列不拥有 L2TLB token/UID；runtime reset 或 C4 delete 后由本类清除。
    memblock_pending_sfence_invalidate_t sfence_invalidate_pending_q[$];
    // 中文注释：只保存已被 DUT sample 到的 L2TLB response 的最近 PPN 记录。
    // EN=1 的 responder completion 写入，reset_all_tables/runtime reset 清空；SFENCE/HFENCE live-entry 删除不得清空。
    memblock_l2tlb_ppn_history_record_t l2tlb_ppn_history_q[$];
    // 中文注释：live entry 的单调身份；普通 reset/flush 只清 table，不回退该计数器。
    longint unsigned         next_tlb_entry_generation;
    memblock_uid_tlb_record  uid_tlb_record_by_uid[memblock_uid_t];
    // Later-page scalar Load split provenance is owned by the architectural
    // UID instance, not by a reusable translation key.  A completed record is
    // retained through RM commit and invalidated on redirect/reset.
    memblock_later_split_tlb_record
        later_split_tlb_record_by_uid[memblock_uid_t];
    // 中文注释：active L2TLB owner完成全部split observer后推进的连续消费边界。
    // constructor清0，reset以epoch隔离；缺口冻结，RM不得把尚未消费的高页当作不存在。
    longint unsigned split_observed_reset_epoch;
    longint unsigned split_observed_sample_seq;
    // WAITING UID 的 request-fire 候选索引。key 只包含 DTLB request 可见的
    // vpn/s2xlate；每个 bucket 受 DTLB filter 物理容量限制。
    memblock_uid_t uid_waiting_by_vpn_s2xlate[memblock_uid_tlb_wait_shape_key_t][$];
    mmu_csr_runtime_state    mmu_csr_state;
    // CSR 专项场景的唯一已提交 level。初始 sequence 写入并发布，动态 child
    // sequence 只从这里复制/更新；普通 topology 保持该句柄无效。
    csr_ctrl_agent_agent_xaction csr_committed_state;
    bit                          csr_committed_state_valid;
    bit                          csr_initial_config_done;
    event                        csr_initial_config_done_ev;
    memblock_csr_pmp_pma_profile_t csr_committed_pmp_pma_profile;
    // 中文注释：当前 CSR control owner 尚未被 monitor/model 确认的 candidate。
    // 置位：动态 child sequence 在 sendover 前登记完整 level、PMP/PMA profile 和
    // drive 前 DUT sample；清零：control service 同时确认完整 CSR snapshot 与
    // PMP/PMA model 后提交，或 control runtime reset 清理。有效时下一动态 action
    // 不得覆盖它，避免 candidate 与 committed state 混用。
    csr_ctrl_agent_agent_xaction csr_dynamic_candidate_state;
    bit                          csr_dynamic_candidate_valid;
    memblock_control_owner_t     csr_dynamic_candidate_owner;
    memblock_csr_pmp_pma_profile_t csr_dynamic_candidate_profile;
    longint unsigned             csr_dynamic_candidate_after_sample;
    bit                          csr_dynamic_candidate_pmp_pma_required;
    // 当前 dynamic action 预期捕获/成熟的累计 write 数；stage 设置、提交/reset 清零。
    longint unsigned csr_dynamic_candidate_expected_write_count;
    // 中文注释：PMA/PMP model 是整个 testcase 的唯一运行期表 owner。表项只由
    // CSR monitor 的 raw write 回放更新；UID request-fire 冻结 generation 后，RM
    // 只能通过只读 API 查询对应历史快照，不能读取当前可变表。
    memblock_pma_pmp_model   pma_pmp_model;
    // 中文注释：只有窄窗口main在发布ready之前登记；运行期builder只读，reset清空。
    memblock_routed_ppn_intent_t routed_ppn_intent_by_vpn[memblock_routed_ppn_vpn_t];
    // 中文注释：已被 PMA/PMP model 回放的最后一个 generic CSR write sample。
    // 只允许单调增加；同 edge request 使用旧表，因此 sample 相等的写入保留给
    // 下一笔 request 才消费。
    longint unsigned         pma_pmp_last_applied_csr_sample;
    // replay 所属 reset epoch、提交版本和已消费 generic write 数，独立于 model enable。
    longint unsigned pma_pmp_csr_replay_reset_epoch;
    longint unsigned pma_pmp_csr_replay_service_revision;
    longint unsigned pma_pmp_csr_applied_count;
    pma_pmp_entry_t          pbmt_rf_pmp_baseline[32];
    pma_pmp_entry_t          pbmt_rf_pma_baseline[32];
    longint unsigned         pbmt_rf_pmp_generation_baseline;
    longint unsigned         pbmt_rf_pma_generation_baseline;
    bit                      pbmt_rf_pma_pmp_baseline_valid;
    memblock_issue_q_item_t  load_issue_q[$];
    memblock_issue_q_item_t  sta_issue_q[$];
    memblock_issue_q_item_t  std_issue_q[$];
    // 中文注释：以 UID 为索引的有限 pending fault 表；动态实例失效、terminal 或 reset
    // 时清除，正式 fault 状态只能由 finalize_pending_store_fault() 写入。
    memblock_pending_store_fault_t pending_store_fault_by_uid[memblock_uid_t];
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
    bit            rob_admission_blocked_log_valid;
    memblock_uid_t rob_admission_blocked_candidate_uid;
    memblock_uid_t rob_admission_blocked_frontier_uid;
    // Fault redirect has one commit-head owner.  Its terminal path is not
    // inferred from map deletion: it must be proven by cancel apply or deq.
    memblock_fault_redirect_resource_t fault_redirect_resource;

    memblock_uid_t uid_by_active_rob[memblock_rob_map_key_t];
    memblock_uid_t uid_by_lq[memblock_lq_map_key_t];
    memblock_uid_t uid_by_sq[memblock_sq_map_key_t];
    // 中文注释：key 为完整 {rob_flag,rob_value}。记录只在 active owner 删除后的
    // 固定短窗口内存在，不能替代 active ROB owner 或 redirect-deleted LQ/SQ owner。
    memblock_std_late_raw_tombstone_t std_late_raw_tombstone_by_rob[memblock_rob_map_key_t];

    // 中文注释：单代 redirect 删除 owner 的固定保护窗口。redirect scan 在删除
    // active map 前写入；迟到 deq 消费或剩余 cancel 应用完成后清除。
    // 窗口有效期间禁止新的 LSQ allocating admission 和下一次 redirect。
    localparam int unsigned MEMBLOCK_REDIRECT_DELETED_OWNER_HOLD_SAMPLES = 4;
    memblock_redirect_deleted_owner_t redirect_deleted_lq_owner_by_key[memblock_lq_map_key_t];
    memblock_redirect_deleted_owner_t redirect_deleted_sq_owner_by_key[memblock_sq_map_key_t];
    bit              redirect_deleted_owner_window_active;
    int unsigned     redirect_deleted_owner_epoch;
    int unsigned     redirect_deleted_owner_record_id;
    longint unsigned redirect_deleted_owner_expire_sample;

    // 中文注释：控制标记只允许存在一个静态屏障。它在 control admission 时建立，
    // 在该 UID terminal_done 后由 control service 解除；普通 LSQ admission 不得越过它。
    bit            active_control_barrier_valid;
    memblock_uid_t active_control_barrier_uid;
    bit                         control_barrier_prefix_external_provenance_valid;
    memblock_uid_t              control_barrier_prefix_external_provenance_uid;
    memblock_redirect_payload_t control_barrier_prefix_external_origin_redirect;
    memblock_uid_t              control_barrier_prefix_external_origin_old_end_uid;
    memblock_uid_t              control_barrier_prefix_external_origin_new_end_uid;
    // 中文注释：当前唯一控制 SFence 的 C0/C4 adapter observation。写入：已有
    // L2TLB adapter 在 schedule/apply 边界；读取/清除：control barrier service。
    // CSR mapping子流程也以原CSR owner预约/消费两槽；不保存未预约generic fence。
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
        split_observed_reset_epoch = 0;
        split_observed_sample_seq = 0;
        global_stop_requested = 1'b0;
        global_stop_prepare_requested = 1'b0;
        global_stop_prepare_last_activity_time = 0;
        global_stop_prepare_dcache_last_sample_time = 0;
        global_stop_prepare_sbuffer_last_sample_time = 0;
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
        redirect_deleted_lq_owner_by_key.delete();
        redirect_deleted_sq_owner_by_key.delete();
        std_late_raw_tombstone_by_rob.delete();
        pending_store_fault_by_uid.delete();
        redirect_deleted_owner_window_active = 1'b0;
        redirect_deleted_owner_epoch = 0;
        redirect_deleted_owner_record_id = 0;
        redirect_deleted_owner_expire_sample = 0;
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
        fault_redirect_resource = '{default:'0};
        cancel_snapshot_history_q.delete();
        redirect_anchor_history_q.delete();
        rob_admission_blocked_log_valid = 1'b0;
        rob_admission_blocked_candidate_uid = 0;
        rob_admission_blocked_frontier_uid = 0;
        mmu_csr_state       = mmu_csr_runtime_state::type_id::create("mmu_csr_state");
        mmu_csr_state.reset();
        csr_committed_state = null;
        csr_committed_state_valid = 1'b0;
        csr_initial_config_done = 1'b0;
        csr_committed_pmp_pma_profile = '{default:'0};
        csr_dynamic_candidate_state = null;
        csr_dynamic_candidate_valid = 1'b0;
        csr_dynamic_candidate_expected_write_count = 0;
        csr_dynamic_candidate_owner = '{default:'0};
        csr_dynamic_candidate_profile = '{default:'0};
        csr_dynamic_candidate_after_sample = 0;
        csr_dynamic_candidate_pmp_pma_required = 1'b0;
        pma_pmp_model = memblock_pma_pmp_model::get();
        pma_pmp_model.reset_and_init_v2_profile();
        pma_pmp_last_applied_csr_sample = 0;
        pma_pmp_csr_replay_reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
        pma_pmp_csr_replay_service_revision = 1;
        pma_pmp_csr_applied_count = 0;
        pbmt_rf_pmp_generation_baseline = 0;
        pbmt_rf_pma_generation_baseline = 0;
        pbmt_rf_pma_pmp_baseline_valid = 1'b0;
        next_tlb_entry_generation = 0;
        parent_lookup_count = 0;
        parent_block_64k_count = 0;
        parent_block_2m_count = 0;
        parent_block_1g_count = 0;
        parent_block_512g_count = 0;
        pte_n_override_count = 0;
        pte_g_override_count = 0;
        forced_4k_fallback_count = 0;
        parent_owner_peak_count = 0;
        sfence_invalidate_pending_q.delete();
        l2tlb_ppn_history_q.delete();
        uid_waiting_by_vpn_s2xlate.delete();
        active_control_barrier_valid = 1'b0;
        active_control_barrier_uid = 0;
        clear_control_barrier_prefix_external_provenance();
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

        if (!memblock_sync_pkg::pma_pmp_csr_epoch_is_current(
                memblock_sync_pkg::get_l2tlb_current_reset_epoch())) begin
            `uvm_fatal("COMMON_DATA", "reset_all_tables requires an initialized current PMA/PMP reset domain")
        end
        if (pma_pmp_model == null) begin
            pma_pmp_model = memblock_pma_pmp_model::get();
        end
        if (!precheck_pma_pmp_csr_replay_reset(
                1'b0, memblock_sync_pkg::get_l2tlb_current_reset_epoch())) begin
            `uvm_fatal("COMMON_DATA", "PMA/PMP testcase reset revision precheck failed")
        end

        if (control_barrier_prefix_external_provenance_valid) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("reset_all_tables rejects residual control provenance uid=%0d origin_old/new=%0d/%0d",
                                 control_barrier_prefix_external_provenance_uid,
                                 control_barrier_prefix_external_origin_old_end_uid,
                                 control_barrier_prefix_external_origin_new_end_uid))
            return;
        end

        if (main_trans_num_i == 0) begin
            `uvm_fatal("COMMON_DATA", "reset_all_tables requires non-zero main_trans_num")
            return;
        end

        main_trans_num      = main_trans_num_i;
        next_uid            = 0;
        main_table_ready    = 1'b0;
        global_stop_requested = 1'b0;
        global_stop_prepare_requested = 1'b0;
        global_stop_prepare_last_activity_time = 0;
        global_stop_prepare_dcache_last_sample_time = 0;
        global_stop_prepare_sbuffer_last_sample_time = 0;
        dispatch_progress.terminal_done_uid      = 0;
        dispatch_progress.rob_commit_frontier_uid = 0;
        dispatch_progress.max_enqueued_uid       = 0;
        dispatch_progress.max_enqueued_uid_valid = 1'b0;
        flush_in_progress   = 1'b0;
        memblock_sync_pkg::dispatch_flush_in_progress = 1'b0;
        memblock_sync_pkg::dispatch_flushsb_waiting_empty = 1'b0;
        memblock_sync_pkg::dispatch_flush_epoch = 0;
        // 两个 done 标志属于本 testcase 的 responder 生命周期，不能继承上一次
        // scenario 的 terminal idle 结论；每个新主表都要求本轮 DCache/SBuffer 重做
        // 自然 drain 和最终 audit。
        memblock_sync_pkg::dcache_responder_done = 1'b0;
        memblock_sync_pkg::sbuffer_responder_done = 1'b0;
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
        redirect_deleted_lq_owner_by_key.delete();
        redirect_deleted_sq_owner_by_key.delete();
        std_late_raw_tombstone_by_rob.delete();
        pending_store_fault_by_uid.delete();
        redirect_deleted_owner_window_active = 1'b0;
        redirect_deleted_owner_epoch = 0;
        redirect_deleted_owner_record_id = 0;
        redirect_deleted_owner_expire_sample = 0;
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
        fault_redirect_resource = '{default:'0};
        cancel_snapshot_history_q.delete();
        redirect_anchor_history_q.delete();
        rob_admission_blocked_log_valid = 1'b0;
        rob_admission_blocked_candidate_uid = 0;
        rob_admission_blocked_frontier_uid = 0;
        main_table_by_uid = new[main_trans_num_i];
        status_by_uid     = new[main_trans_num_i];
        cancel_waiting_uid_tlb_records("reset_all_tables");
        routed_ppn_intent_by_vpn.delete();
        tlb_entry_by_key.delete();
        tlb_anchor_keys_by_range_key.delete();
        tlb_parent_occupancy_count.delete();
        tlb_parent_owner_set.delete();
        parent_lookup_count = 0;
        parent_block_64k_count = 0;
        parent_block_2m_count = 0;
        parent_block_1g_count = 0;
        parent_block_512g_count = 0;
        pte_n_override_count = 0;
        pte_g_override_count = 0;
        forced_4k_fallback_count = 0;
        parent_owner_peak_count = 0;
        completed_tlb_entry_by_key.delete();
        completed_tlb_anchor_keys_by_range_key.delete();
        completed_tlb_response_token_by_key.delete();
        completed_tlb_response_sample_by_key.delete();
        sfence_invalidate_pending_q.delete();
        l2tlb_ppn_history_q.delete();
        uid_tlb_record_by_uid.delete();
        later_split_tlb_record_by_uid.delete();
        clear_issue_queues();
        clear_feedback_events();
        clear_redirect_drive_queue();
        clear_ptw_wait_replay_queue();
        uid_by_active_rob.delete();
        uid_by_lq.delete();
        uid_by_sq.delete();
        active_control_barrier_valid = 1'b0;
        active_control_barrier_uid = 0;
        clear_control_barrier_prefix_external_provenance();
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
        csr_committed_state = null;
        csr_committed_state_valid = 1'b0;
        csr_initial_config_done = 1'b0;
        csr_committed_pmp_pma_profile = '{default:'0};
        csr_dynamic_candidate_state = null;
        csr_dynamic_candidate_valid = 1'b0;
        csr_dynamic_candidate_expected_write_count = 0;
        csr_dynamic_candidate_owner = '{default:'0};
        csr_dynamic_candidate_profile = '{default:'0};
        csr_dynamic_candidate_after_sample = 0;
        csr_dynamic_candidate_pmp_pma_required = 1'b0;
        pma_pmp_model.reset_and_init_v2_profile();
        memblock_sync_pkg::reset_raw_pma_pmp_csr_writes();
        pma_pmp_last_applied_csr_sample = 0;
        pma_pmp_csr_replay_reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
        pma_pmp_csr_replay_service_revision = 1;
        pma_pmp_csr_applied_count = 0;
        pbmt_rf_pmp_generation_baseline = 0;
        pbmt_rf_pma_generation_baseline = 0;
        pbmt_rf_pma_pmp_baseline_valid = 1'b0;
        for (uid = 0; uid < main_trans_num_i; uid++) begin
            status_by_uid[uid] = status_transaction::type_id::create($sformatf("status_uid_%0d", uid));
            status_by_uid[uid].reset(uid);
        end
    endfunction:reset_all_tables

    // CSR 专项状态只接受完整、已净化的 level item；调用者必须在 monitor
    // confirmation 后调用，避免把 candidate 当作 committed state。
    function void publish_csr_committed_state(input csr_ctrl_agent_agent_xaction source);
        if (source == null) begin
            `uvm_fatal("COMMON_DATA", "publish_csr_committed_state got null source")
        end
        if (csr_committed_state == null) begin
            csr_committed_state = csr_ctrl_agent_agent_xaction::type_id::create("csr_committed_state");
        end
        csr_committed_state.copy(source);
        csr_committed_state.io_ooo_to_mem_tlbCsr_satp_changed = 1'b0;
        csr_committed_state.io_ooo_to_mem_tlbCsr_vsatp_changed = 1'b0;
        csr_committed_state.io_ooo_to_mem_tlbCsr_hgatp_changed = 1'b0;
        csr_committed_state.io_ooo_to_mem_tlbCsr_priv_virt_changed = 1'b0;
        csr_committed_state.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b0;
        csr_committed_state.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_addr = '0;
        csr_committed_state.io_ooo_to_mem_csrCtrl_distribute_csr_w_bits_data = '0;
        csr_committed_state.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid = 1'b0;
        csr_committed_state.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid = 1'b0;
        csr_committed_state.io_ooo_to_mem_tlbCsr_priv_debug = 1'b0;
        csr_committed_state_valid = 1'b1;
    endfunction:publish_csr_committed_state

    function bit get_csr_committed_state(output csr_ctrl_agent_agent_xaction target);
        if (!csr_committed_state_valid || csr_committed_state == null) begin
            target = null;
            return 1'b0;
        end
        target = csr_committed_state;
        return 1'b1;
    endfunction:get_csr_committed_state

    // 中文注释：登记动态 CSR candidate，不把尚未观察到的 target 提前发布为 committed。
    // 调用者是动态 CSR child；control service 随后用 owner/sample 边界读取候选并确认。
    function void stage_csr_dynamic_candidate(
        input memblock_control_owner_t owner,
        input csr_ctrl_agent_agent_xaction source,
        input memblock_csr_pmp_pma_profile_t profile,
        input longint unsigned after_sample,
        input bit pmp_pma_required,
        input longint unsigned expected_write_count
    );
        if (!owner.valid || source == null || !profile.valid || after_sample == 0) begin
            `uvm_fatal("COMMON_DATA", "invalid CSR dynamic candidate staging request")
        end
        if (csr_dynamic_candidate_valid) begin
            `uvm_fatal("COMMON_DATA", "a CSR dynamic candidate is already pending")
        end
        if (csr_dynamic_candidate_state == null) begin
            csr_dynamic_candidate_state = csr_ctrl_agent_agent_xaction::type_id::create(
                "csr_dynamic_candidate_state");
        end
        csr_dynamic_candidate_state.copy(source);
        memblock_csr_config_state::clear_protocol_metadata(csr_dynamic_candidate_state);
        csr_dynamic_candidate_owner = owner;
        csr_dynamic_candidate_profile = profile;
        csr_dynamic_candidate_after_sample = after_sample;
        csr_dynamic_candidate_pmp_pma_required = pmp_pma_required;
        csr_dynamic_candidate_expected_write_count = expected_write_count;
        csr_dynamic_candidate_valid = 1'b1;
    endfunction:stage_csr_dynamic_candidate

    // 中文注释：control service 的唯一动态 candidate 确认入口。它先消费 monitor
    // 的 PMA/PMP write FIFO，再检查完整 CSR payload 是否跨过 drive sample；只有两类
    // 事实同时成立才提交，返回 0 时 status 继续停在 WAIT_CSR_RUNTIME_SNAPSHOT。
    function bit commit_csr_dynamic_candidate_if_observed(
        input memblock_control_owner_t owner
    );
        memblock_sync_pkg::memblock_csr_full_snapshot_t observed;
        bit [memblock_sync_pkg::MEMBLOCK_CSR_FULL_PAYLOAD_BITS-1:0] expected_payload;
        if (!csr_dynamic_candidate_valid)
            `uvm_fatal("COMMON_DATA", "special CSR completion has no staged dynamic candidate")
        if (!memblock_control_owner_equal(csr_dynamic_candidate_owner, owner))
            `uvm_fatal("COMMON_DATA", "special CSR completion owner does not match staged candidate")
        if (!service_csr_pmp_pma_write_observations()) return 1'b0;
        if (!csr_pmp_pma_writes_observed(csr_dynamic_candidate_expected_write_count)) begin
            return 1'b0;
        end
        if (!memblock_sync_pkg::get_latest_csr_full_snapshot(observed) ||
            observed.sample_seq <= csr_dynamic_candidate_after_sample) begin
            return 1'b0;
        end
        expected_payload = memblock_csr_config_state::pack_full_payload(
            csr_dynamic_candidate_state);
        if (observed.payload != expected_payload) begin
            return 1'b0;
        end
        if (csr_dynamic_candidate_pmp_pma_required) begin
            memblock_csr_pmp_pma_write_plan plan;
            plan = memblock_csr_pmp_pma_write_plan::type_id::create(
                "csr_dynamic_candidate_observation_plan");
            if (!plan.model_matches(pma_pmp_model, csr_dynamic_candidate_profile)) begin
                return 1'b0;
            end
        end
        publish_csr_committed_state(csr_dynamic_candidate_state);
        publish_csr_pmp_pma_profile(csr_dynamic_candidate_profile);
        csr_dynamic_candidate_valid = 1'b0;
        csr_dynamic_candidate_expected_write_count = 0;
        csr_dynamic_candidate_owner = '{default:'0};
        csr_dynamic_candidate_profile = '{default:'0};
        csr_dynamic_candidate_after_sample = 0;
        csr_dynamic_candidate_pmp_pma_required = 1'b0;
        return 1'b1;
    endfunction:commit_csr_dynamic_candidate_if_observed

    function void mark_csr_initial_config_done();
        csr_initial_config_done = 1'b1;
        ->csr_initial_config_done_ev;
    endfunction:mark_csr_initial_config_done

    function void publish_csr_pmp_pma_profile(
        input memblock_csr_pmp_pma_profile_t profile
    );
        if (!profile.valid) begin
            `uvm_fatal("COMMON_DATA", "cannot publish an invalid CSR PMP/PMA profile")
        end
        csr_committed_pmp_pma_profile = profile;
    endfunction:publish_csr_pmp_pma_profile

    function bit get_csr_pmp_pma_profile(
        output memblock_csr_pmp_pma_profile_t profile
    );
        profile = csr_committed_pmp_pma_profile;
        return profile.valid;
    endfunction:get_csr_pmp_pma_profile

    function bit csr_pmp_pma_writes_observed(input longint unsigned expected_count);
        return memblock_sync_pkg::pma_pmp_csr_epoch_is_current(
                   pma_pmp_csr_replay_reset_epoch) &&
               pma_pmp_csr_applied_count >= expected_count &&
               pma_pmp_csr_applied_count == memblock_sync_pkg::raw_pma_pmp_csr_write_produced_count &&
               memblock_sync_pkg::raw_pma_pmp_csr_write_size() == 0;
    endfunction:csr_pmp_pma_writes_observed


    function bit precheck_pma_pmp_csr_end_state(
        output longint unsigned produced_count,
        output longint unsigned applied_count,
        output longint unsigned pending_count
    );
        produced_count = memblock_sync_pkg::raw_pma_pmp_csr_write_produced_count;
        applied_count = pma_pmp_csr_applied_count;
        pending_count = $unsigned(memblock_sync_pkg::raw_pma_pmp_csr_write_size());
        return produced_count == applied_count && pending_count == 0;
    endfunction:precheck_pma_pmp_csr_end_state

    function bit precheck_pma_pmp_csr_replay_reset(
        input bit require_runtime_reset,
        input longint unsigned epoch
    );
        if (pma_pmp_model == null ||
            !pma_pmp_model.can_reset_csr_replay_state() ||
            !memblock_sync_pkg::pma_pmp_csr_revision_can_advance(
                memblock_sync_pkg::get_raw_pma_pmp_csr_write_revision()) ||
            !memblock_sync_pkg::pma_pmp_csr_revision_can_advance(
                pma_pmp_csr_replay_service_revision)) begin
            return 1'b0;
        end
        if (require_runtime_reset) begin
            return memblock_sync_pkg::l2tlb_reset_active() && epoch != 0 &&
                   epoch == memblock_sync_pkg::get_l2tlb_current_reset_epoch();
        end
        return memblock_sync_pkg::pma_pmp_csr_epoch_is_current(epoch);
    endfunction:precheck_pma_pmp_csr_replay_reset

    function void reset_pma_pmp_csr_replay_for_runtime_epoch();
        longint unsigned epoch;
        epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
        if (!memblock_sync_pkg::l2tlb_reset_active() || epoch == 0 ||
            pma_pmp_csr_replay_reset_epoch == epoch) return;
        if (!precheck_pma_pmp_csr_replay_reset(1'b1, epoch)) begin
            `uvm_fatal("COMMON_DATA", "PMA/PMP runtime replay reset precheck failed")
            return;
        end
        pma_pmp_model.reset_and_init_v2_profile();
        memblock_sync_pkg::reset_raw_pma_pmp_csr_writes();
        pma_pmp_csr_replay_reset_epoch = epoch;
        pma_pmp_last_applied_csr_sample = 0;
        pma_pmp_csr_applied_count = 0;
        pma_pmp_csr_replay_service_revision++;
    endfunction:reset_pma_pmp_csr_replay_for_runtime_epoch


    function bit service_csr_pmp_pma_write_observations();
        memblock_pma_pmp_replay_ticket ticket;
        longint unsigned published;
        if (memblock_sync_pkg::l2tlb_reset_active()) return 1'b1;
        published = memblock_sync_pkg::get_l2tlb_lifecycle_event_published_seq();
        if (published == 0 || memblock_sync_pkg::raw_pma_pmp_csr_write_size() == 0) return 1'b1;
        if (!prepare_pma_pmp_csr_replay(published, ticket)) begin
            `uvm_fatal("COMMON_DATA", "PMA/PMP CSR replay prepare failed")
            return 1'b0;
        end
        if (ticket.events.size() == 0) return 1'b1;
        if (!commit_pma_pmp_csr_replay(ticket)) begin
            `uvm_fatal("COMMON_DATA", "PMA/PMP CSR replay commit precheck failed")
            return 1'b0;
        end
        return 1'b1;
    endfunction:service_csr_pmp_pma_write_observations

    function longint unsigned get_pma_pmp_last_applied_csr_sample();
        return pma_pmp_last_applied_csr_sample;
    endfunction:get_pma_pmp_last_applied_csr_sample

    function void capture_pbmt_rf_pma_pmp_baseline();
        if (!memblock_sync_pkg::is_pbmt_rf_scenario_active() ||
            pma_pmp_model == null || !pma_pmp_model.is_ready()) begin
            `uvm_fatal("COMMON_DATA",
                       "PBMT RF baseline capture requires active scenario and ready model")
        end
        if (pbmt_rf_pma_pmp_baseline_valid) begin
            `uvm_fatal("COMMON_DATA", "PBMT RF PMA/PMP baseline captured twice")
        end
        for (int unsigned idx = 0; idx < 32; idx++) begin
            if (!pma_pmp_model.read_entry(1'b0, idx,
                                          pbmt_rf_pmp_baseline[idx]) ||
                !pma_pmp_model.read_entry(1'b1, idx,
                                          pbmt_rf_pma_baseline[idx])) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("failed to capture PBMT RF PMA/PMP baseline idx=%0d",
                                     idx))
            end
        end
        pma_pmp_model.read_generations(pbmt_rf_pmp_generation_baseline,
                                       pbmt_rf_pma_generation_baseline);
        pbmt_rf_pma_pmp_baseline_valid = 1'b1;
    endfunction:capture_pbmt_rf_pma_pmp_baseline

    function bit pbmt_rf_bootstrap_matches();
        pma_pmp_entry_t pmp_entry;
        pma_pmp_entry_t pma_entry;
        longint unsigned pmp_generation;
        longint unsigned pma_generation;

        if (!pbmt_rf_pma_pmp_baseline_valid || pma_pmp_model == null) begin
            return 1'b0;
        end
        pma_pmp_model.read_generations(pmp_generation, pma_generation);
        if (pmp_generation != pbmt_rf_pmp_generation_baseline + 2 ||
            pma_generation != pbmt_rf_pma_generation_baseline) begin
            return 1'b0;
        end
        for (int unsigned idx = 0; idx < 32; idx++) begin
            if (!pma_pmp_model.read_entry(1'b0, idx, pmp_entry) ||
                !pma_pmp_model.read_entry(1'b1, idx, pma_entry)) begin
                return 1'b0;
            end
            if (pma_entry != pbmt_rf_pma_baseline[idx]) begin
                return 1'b0;
            end
            if (idx == 0) begin
                if (!pmp_entry.valid || pmp_entry.index != 0 ||
                    pmp_entry.lock || pmp_entry.c || pmp_entry.atomic ||
                    pmp_entry.a != PMA_PMP_A_TOR ||
                    !pmp_entry.x || !pmp_entry.w || !pmp_entry.r ||
                    pmp_entry.addr_raw !=
                        memblock_sync_pkg::PBMT_RF_PMPADDR0_TOR_TOP[45:0] ||
                    pmp_entry.origin != 2'd2) begin
                    return 1'b0;
                end
            end
            else if (idx < 8) begin
                if (pmp_entry.valid || pmp_entry.index != idx ||
                    pmp_entry.lock || pmp_entry.c || pmp_entry.atomic ||
                    pmp_entry.a != PMA_PMP_A_OFF ||
                    pmp_entry.x || pmp_entry.w || pmp_entry.r ||
                    pmp_entry.addr_raw != pbmt_rf_pmp_baseline[idx].addr_raw ||
                    pmp_entry.origin != 2'd2) begin
                    return 1'b0;
                end
            end
            else if (pmp_entry != pbmt_rf_pmp_baseline[idx]) begin
                return 1'b0;
            end
        end
        return 1'b1;
    endfunction:pbmt_rf_bootstrap_matches

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

    // 中文注释：仅登记输入生成约束；不创建TLB条目、不设置RM预期结果。
    function bit register_routed_ppn_intent(input bit [37:0] vpn,
                                            input memblock_routed_ppn_intent_t intent);
        bit [63:0] first, limit;
        if (!intent.valid || intent.first_offset > intent.last_offset ||
            seq_csr_common::get_main_mem_ranges_en() ||
            seq_csr_common::get_l2tlb_ppn_reuse_en()) return 1'b0;
        first = {8'b0, intent.ppn, 12'b0} + intent.first_offset;
        limit = {8'b0, intent.ppn, 12'b0} + intent.last_offset + 1;
        if (!memblock_routed_ppn_policy::is_external_span(first, limit)) return 1'b0;
        if (routed_ppn_intent_by_vpn.exists(vpn) &&
            routed_ppn_intent_by_vpn[vpn] != intent) return 1'b0;
        routed_ppn_intent_by_vpn[vpn] = intent;
        return 1'b1;
    endfunction:register_routed_ppn_intent

    // 中文注释：只向builder传递已提交的固定版本。raw写消费仍由已有request/service owner完成。
    function void configure_builder_routed_context(input tlb_map_builder builder,
        input memblock_tlb_lookup_key_t key, input mmu_csr_runtime_state csr,
        input longint unsigned query_sample = 0);
        longint unsigned pmp_gen, pma_gen;
        memblock_routed_ppn_intent_t intent;
        if (seq_csr_common::get_main_mem_ranges_en() ||
            memblock_sync_pkg::csr_special_sequence_active) return;
        if (builder == null || csr == null || pma_pmp_model == null || !pma_pmp_model.is_ready())
            `uvm_fatal("L2TLB_ROUTED_PPN", "routed construction has no initialized PMA owner")
        if (query_sample != 0) begin
            if (!pma_pmp_model.get_generations_before_sample(query_sample, pmp_gen, pma_gen))
                `uvm_fatal("L2TLB_ROUTED_PPN", "no effective PMA generation for request sample")
        end else pma_pmp_model.get_current_generations(pmp_gen, pma_gen);
        intent = '{default:'0};
        if (routed_ppn_intent_by_vpn.exists(key.vpn[37:0]))
            intent = routed_ppn_intent_by_vpn[key.vpn[37:0]];
        builder.configure_routed_ppn(pma_pmp_model, pma_gen, csr.priv_debug, intent);
    endfunction:configure_builder_routed_context

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

    // 中文注释：只在 MMIO owner 最终无法证明的失败路径扫描主表/status。
    // 该诊断不修改任何 map、queue、status 或 redirect 状态；全表扫描只服务 fatal debug。
    function void dump_mmio_resolve_failure_context(
        input bit [MEMBLOCK_ROB_VALUE_W-1:0] rob_value,
        input memblock_mmio_kind_e expected_kind,
        input int unsigned raw_sample_flush_epoch,
        input longint unsigned raw_sample_seq,
        input longint unsigned raw_sample_cycle
    );
        int unsigned static_kind_match_count;
        int unsigned lifecycle_match_count;
        memblock_rob_key_t raw_key_flag0;
        memblock_rob_key_t raw_key_flag1;

        static_kind_match_count = 0;
        lifecycle_match_count = 0;
        raw_key_flag0.flag = 1'b0;
        raw_key_flag0.value = rob_value;
        raw_key_flag1.flag = 1'b1;
        raw_key_flag1.value = rob_value;
        `uvm_info("MMIO_RESOLVE_DIAG",
                  $sformatf("raw cycle=%0d sample=%0d epoch=%0d kind=%0d rob_value=%0d current_sample=%0d current_service_cycle=%0d current_epoch=%0d active_redirect(valid/flush_itself/level/vls/rob)=%0d/%0d/%0d/%0d/%0d/%0d redirect_phase=%0d flush_in_progress=%0d cancel_records=%0d redirect_anchors=%0d",
                            raw_sample_cycle, raw_sample_seq, raw_sample_flush_epoch,
                            expected_kind, rob_value,
                            memblock_sync_pkg::peek_current_dut_global_sample(),
                            memblock_sync_pkg::get_dispatch_service_cycle(),
                            memblock_sync_pkg::dispatch_flush_epoch,
                            active_redirect.valid, active_redirect.flush_itself,
                            active_redirect.level, active_redirect.is_vls_exception,
                            active_redirect.rob_key.flag, active_redirect.rob_key.value,
                            redirect_phase, flush_in_progress, cancel_record_q.size(),
                            redirect_anchor_history_q.size()),
                  UVM_NONE)
        `uvm_info("MMIO_RESOLVE_DIAG",
                  $sformatf("redirect drive inflight=%0d rob=%0d/%0d done_epoch=%0d done_cycle=%0d deleted_owner_window(active/epoch/record/expire_sample/lq/sq)=%0d/%0d/%0d/%0d/%0d/%0d fault_resource(valid/uid/dynamic_epoch/record/disposition)=%0d/%0d/%0d/%0d/%0d",
                            redirect_drive_inflight,
                            redirect_drive_inflight_payload.rob_key.flag,
                            redirect_drive_inflight_payload.rob_key.value,
                            redirect_drive_done_epoch, redirect_drive_done_cycle,
                            redirect_deleted_owner_window_active,
                            redirect_deleted_owner_epoch,
                            redirect_deleted_owner_record_id,
                            redirect_deleted_owner_expire_sample,
                            redirect_deleted_lq_owner_by_key.num(),
                            redirect_deleted_sq_owner_by_key.num(),
                            fault_redirect_resource.valid,
                            fault_redirect_resource.uid,
                            fault_redirect_resource.dynamic_epoch,
                            fault_redirect_resource.cancel_record_id,
                            fault_redirect_resource.disposition),
                  UVM_NONE)

        foreach (cancel_record_q[record_idx]) begin
            `uvm_info("MMIO_RESOLVE_DIAG",
                      $sformatf("cancel record idx=%0d id=%0d valid=%0d epoch=%0d redirect(valid/flush_itself/level/vls/rob)=%0d/%0d/%0d/%0d/%0d/%0d service_cycle=%0d drive_done=%0d/%0d anchor=%0d sample=%0d lsq_sample=%0d cancel_sample=%0d compare_sample=%0d scan/final/applied/observed=%0d/%0d/%0d/%0d sw_lq/sq=%0d/%0d dut_lq/sq=%0d/%0d raw_rob_covered_flag0/flag1=%0d/%0d",
                                record_idx,
                                cancel_record_q[record_idx].cancel_record_id,
                                cancel_record_q[record_idx].valid,
                                cancel_record_q[record_idx].redirect_epoch,
                                cancel_record_q[record_idx].redirect.valid,
                                cancel_record_q[record_idx].redirect.flush_itself,
                                cancel_record_q[record_idx].redirect.level,
                                cancel_record_q[record_idx].redirect.is_vls_exception,
                                cancel_record_q[record_idx].redirect.rob_key.flag,
                                cancel_record_q[record_idx].redirect.rob_key.value,
                                cancel_record_q[record_idx].redirect_service_cycle,
                                cancel_record_q[record_idx].redirect_drive_done_valid,
                                cancel_record_q[record_idx].redirect_drive_done_service_cycle,
                                cancel_record_q[record_idx].redirect_anchor_valid,
                                cancel_record_q[record_idx].redirect_sample_seq,
                                cancel_record_q[record_idx].redirect_lsq_sample_seq,
                                cancel_record_q[record_idx].dut_cancel_update_sample_seq,
                                cancel_record_q[record_idx].compare_snapshot_sample_seq,
                                cancel_record_q[record_idx].active_scan_done,
                                cancel_record_q[record_idx].software_count_finalized,
                                cancel_record_q[record_idx].software_applied,
                                cancel_record_q[record_idx].observed_valid,
                                cancel_record_q[record_idx].software_cancel_lq_count,
                                cancel_record_q[record_idx].software_cancel_sq_count,
                                cancel_record_q[record_idx].observed_cancel_lq_count,
                                cancel_record_q[record_idx].observed_cancel_sq_count,
                                rob_order_util::rob_need_flush(
                                    raw_key_flag0,
                                    cancel_record_q[record_idx].redirect),
                                rob_order_util::rob_need_flush(
                                    raw_key_flag1,
                                    cancel_record_q[record_idx].redirect)),
                      UVM_NONE)
        end
        foreach (redirect_anchor_history_q[anchor_idx]) begin
            `uvm_info("MMIO_RESOLVE_DIAG",
                      $sformatf("unbound redirect anchor idx=%0d valid=%0d sample=%0d cycle=%0d raw/effective/vls/rob=%0d/%0d/%0d/%0d/%0d",
                                anchor_idx,
                                redirect_anchor_history_q[anchor_idx].valid,
                                redirect_anchor_history_q[anchor_idx].sample_seq,
                                redirect_anchor_history_q[anchor_idx].cycle,
                                redirect_anchor_history_q[anchor_idx].level,
                                redirect_anchor_history_q[anchor_idx].effective_level,
                                redirect_anchor_history_q[anchor_idx].is_vls_exception,
                                redirect_anchor_history_q[anchor_idx].rob_flag,
                                redirect_anchor_history_q[anchor_idx].rob_value),
                      UVM_NONE)
        end

        for (memblock_uid_t candidate_uid = 0;
             candidate_uid < main_trans_num;
             candidate_uid++) begin
            main_control_transaction main_tr;
            status_transaction status;
            memblock_op_behavior_t behavior;
            bit kind_match;
            bit lifecycle_observed;
            bit active_redirect_covers;

            if (main_table_by_uid[candidate_uid] == null ||
                status_by_uid[candidate_uid] == null) begin
                continue;
            end
            main_tr = main_table_by_uid[candidate_uid];
            status = status_by_uid[candidate_uid];
            if (main_tr.robIdx_value != rob_value) begin
                continue;
            end
            behavior = memblock_op_behavior_util::derive_op_behavior(main_tr);
            kind_match = (expected_kind == MEMBLOCK_MMIO_KIND_LOAD &&
                          behavior.kind == MEMBLOCK_OP_BEHAVIOR_LOAD &&
                          behavior.commit_is_load) ||
                         (expected_kind == MEMBLOCK_MMIO_KIND_STORE &&
                          behavior.kind == MEMBLOCK_OP_BEHAVIOR_STORE &&
                          behavior.commit_is_store);
            if (!kind_match) begin
                continue;
            end
            static_kind_match_count++;
            lifecycle_observed = status.last_event_cycle != 0 || status.active ||
                                 status.enq || status.flushed || status.terminal_done ||
                                 status.load_issue_epoch != 0 ||
                                 status.sta_issue_epoch != 0 ||
                                 status.std_issue_epoch != 0;
            if (!lifecycle_observed) begin
                continue;
            end
            lifecycle_match_count++;
            active_redirect_covers = active_redirect.valid &&
                rob_order_util::rob_need_flush(status.get_rob_key(), active_redirect);
            `uvm_info("MMIO_RESOLVE_DIAG",
                      $sformatf("owner candidate uid=%0d rob=%0d/%0d behavior=%0d dynamic_epoch=%0d active=%0d enq=%0d flushed=%0d terminal_done=%0d killed=%0d redirect_pending=%0d rob_commit=%0d pass=%0d fault=%0d last_event_cycle=%0d active_instance_epoch(valid/value)=%0d/%0d target_dispatched(load/sta/std)=%0d/%0d/%0d target_issue_epoch(load/sta/std)=%0d/%0d/%0d target_flush_epoch_valid(load/sta/std)=%0d/%0d/%0d target_flush_epoch(load/sta/std)=%0d/%0d/%0d mmio_tag(valid/load/store/source/epoch)=%0d/%0d/%0d/%0d/%0d active_redirect_covers=%0d",
                                candidate_uid,
                                status.robIdx_flag, status.robIdx_value,
                                behavior.kind, status.dynamic_epoch,
                                status.active, status.enq, status.flushed,
                                status.terminal_done, status.issue_killed,
                                status.redirect_pending, status.rob_commit,
                                status.pass, status.fault, status.last_event_cycle,
                                status.active_instance_flush_epoch_valid,
                                status.active_instance_flush_epoch,
                                status.load_dispatched, status.sta_dispatched,
                                status.std_dispatched,
                                status.load_issue_epoch, status.sta_issue_epoch,
                                status.std_issue_epoch,
                                status.load_instance_flush_epoch_valid,
                                status.sta_instance_flush_epoch_valid,
                                status.std_instance_flush_epoch_valid,
                                status.load_instance_flush_epoch,
                                status.sta_instance_flush_epoch,
                                status.std_instance_flush_epoch,
                                status.mmio_tag_valid, status.is_mmio_load,
                                status.is_mmio_store, status.mmio_tag_source,
                                status.mmio_tag_dynamic_epoch,
                                active_redirect_covers),
                      UVM_NONE)
            foreach (cancel_record_q[candidate_record_idx]) begin
                `uvm_info("MMIO_RESOLVE_DIAG",
                          $sformatf("owner candidate uid=%0d cancel_record=%0d redirect_epoch=%0d covered=%0d",
                                    candidate_uid,
                                    cancel_record_q[candidate_record_idx].cancel_record_id,
                                    cancel_record_q[candidate_record_idx].redirect_epoch,
                                    rob_order_util::rob_need_flush(
                                        status.get_rob_key(),
                                        cancel_record_q[candidate_record_idx].redirect)),
                          UVM_NONE)
            end
        end
        `uvm_info("MMIO_RESOLVE_DIAG",
                  $sformatf("owner scan summary rob_value=%0d kind=%0d static_kind_matches=%0d lifecycle_matches=%0d",
                            rob_value, expected_kind, static_kind_match_count,
                            lifecycle_match_count),
                  UVM_NONE)
    endfunction:dump_mmio_resolve_failure_context

    // 抽象职责：只在 Load MMIO 与 redirect 的R..R+2窗口重叠且active ROB owner
    // 已消失时，使用现有
    // status/main table 与单代 redirect-deleted LQ owner证明旧动态实例归属。
    // 本函数只读公共状态，不写 tag、map、queue、redirect 或 cancel 生命周期。
    function bit find_unique_redirect_deleted_load_mmio_owner(
        input bit [MEMBLOCK_ROB_VALUE_W-1:0] rob_value,
        input memblock_redirect_payload_t overlap_redirect,
        input int unsigned overlap_redirect_epoch,
        input int unsigned overlap_cancel_record_id,
        output memblock_uid_t matched_uid,
        output memblock_rob_key_t matched_rob_key,
        output int unsigned matched_count,
        output string proof_detail
    );
        int record_idx;

        matched_uid = 0;
        matched_rob_key = '{default:'0};
        matched_count = 0;
        proof_detail = "";
        if (!overlap_redirect.valid || overlap_redirect_epoch == 0 ||
            overlap_cancel_record_id == 0 ||
            !redirect_deleted_owner_window_active ||
            redirect_deleted_owner_epoch != overlap_redirect_epoch ||
            redirect_deleted_owner_record_id != overlap_cancel_record_id) begin
            proof_detail = $sformatf(
                "redirect-deleted window mismatch active=%0d window_epoch/record=%0d/%0d overlap_epoch/record=%0d/%0d",
                redirect_deleted_owner_window_active,
                redirect_deleted_owner_epoch,
                redirect_deleted_owner_record_id,
                overlap_redirect_epoch,
                overlap_cancel_record_id);
            return 1'b0;
        end
        record_idx = find_cancel_record_index_by_id(overlap_cancel_record_id);
        if (record_idx < 0 || !cancel_record_q[record_idx].valid ||
            !cancel_record_q[record_idx].redirect_anchor_valid ||
            !cancel_record_q[record_idx].active_scan_done ||
            !redirect_payload_equal(cancel_record_q[record_idx].redirect,
                                    overlap_redirect)) begin
            proof_detail = $sformatf(
                "redirect record is unavailable or incompatible record=%0d index=%0d",
                overlap_cancel_record_id, record_idx);
            return 1'b0;
        end

        for (memblock_uid_t candidate_uid = 0;
             candidate_uid < main_trans_num;
             candidate_uid++) begin
            main_control_transaction main_tr;
            status_transaction status;
            memblock_op_behavior_t behavior;
            memblock_rob_key_t candidate_rob_key;
            memblock_lq_key_t candidate_lq_key;
            memblock_redirect_deleted_owner_t deleted_owner;

            if (main_table_by_uid[candidate_uid] == null ||
                status_by_uid[candidate_uid] == null) begin
                continue;
            end
            main_tr = main_table_by_uid[candidate_uid];
            status = status_by_uid[candidate_uid];
            if (main_tr.robIdx_value != rob_value ||
                status.robIdx_value != rob_value) begin
                continue;
            end
            behavior = memblock_op_behavior_util::derive_op_behavior(main_tr);
            if (behavior.kind != MEMBLOCK_OP_BEHAVIOR_LOAD ||
                !behavior.commit_is_load || !behavior.uses_lq ||
                status.active || !status.flushed || !status.issue_killed ||
                !status.redirect_pending || status.load_dispatched ||
                status.load_issue_epoch == 0 || status.dynamic_epoch == 0 ||
                status.active_instance_flush_epoch_valid) begin
                continue;
            end
            candidate_rob_key = status.get_rob_key();
            if (candidate_rob_key != main_tr.get_rob_key() ||
                !rob_order_util::rob_need_flush(candidate_rob_key,
                                                overlap_redirect)) begin
                continue;
            end
            candidate_lq_key.flag = status.lqIdx_flag;
            candidate_lq_key.value = status.lqIdx_value;
            if (!is_valid_lq_key(candidate_lq_key) ||
                !lookup_redirect_deleted_lq_owner(candidate_lq_key,
                                                  deleted_owner) ||
                deleted_owner.uid != candidate_uid ||
                deleted_owner.old_dynamic_epoch + 1 != status.dynamic_epoch ||
                deleted_owner.old_activation_flush_epoch >= overlap_redirect_epoch ||
                deleted_owner.redirect_epoch != overlap_redirect_epoch ||
                deleted_owner.cancel_record_id != overlap_cancel_record_id) begin
                continue;
            end
            matched_count++;
            matched_uid = candidate_uid;
            matched_rob_key = candidate_rob_key;
        end
        proof_detail = $sformatf(
            "redirect-deleted scalar Load matches=%0d epoch=%0d record=%0d",
            matched_count, overlap_redirect_epoch, overlap_cancel_record_id);
        return matched_count == 1;
    endfunction:find_unique_redirect_deleted_load_mmio_owner

    // 中文注释：只 probe 同一 ROB value 的两个完整 wrap key，不扫描主表。
    // 返回 CURRENT 时 uid 唯一且 op/dispatch/provenance 已验证；只有可证明旧 raw
    // 时返回 STALE_DROP，其余 value-only 歧义直接 fatal。LOAD 还要检查
    // LoadQueueUncache s1 后一拍脉冲是否与未完成 redirect 的 sample anchor 重叠。
    function memblock_mmio_resolve_result_e resolve_mmio_uid_by_rob_value(
        input bit [MEMBLOCK_ROB_VALUE_W-1:0] rob_value,
        input memblock_mmio_kind_e expected_kind,
        input int unsigned raw_sample_flush_epoch,
        input longint unsigned raw_sample_seq,
        input longint unsigned raw_sample_cycle,
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
        int unsigned overlap_cancel_record_id;
        longint unsigned overlap_redirect_sample_seq;
        int unsigned overlap_old_covered_count;
        int unsigned overlap_new_candidate_count;
        int unsigned overlap_uncovered_count;
        int unsigned overlap_incompatible_count;
        memblock_rob_key_t overlap_old_key;
        memblock_uid_t overlap_surviving_uid;
        memblock_rob_key_t overlap_surviving_key;

        uid = 0;
        stale_reason = "";
        current_candidate_count = 0;
        active_candidate_count = 0;
        newer_candidate_count = 0;
        load_overlap_observed = 1'b0;
        overlap_redirect_match_count = 0;
        overlap_redirect = '{default:'0};
        overlap_redirect_epoch = 0;
        overlap_cancel_record_id = 0;
        overlap_redirect_sample_seq = 0;
        overlap_old_covered_count = 0;
        overlap_new_candidate_count = 0;
        overlap_uncovered_count = 0;
        overlap_incompatible_count = 0;
        overlap_old_key = '{default:'0};
        overlap_surviving_uid = 0;
        overlap_surviving_key = '{default:'0};
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
                    raw_sample_seq < cancel_record_q[record_probe_idx].redirect_sample_seq ||
                    raw_sample_seq > cancel_record_q[record_probe_idx].redirect_sample_seq +
                        MEMBLOCK_LOAD_MMIO_REDIRECT_MAX_SAMPLE_OFFSET) begin
                    continue;
                end
                load_overlap_observed = 1'b1;
                overlap_redirect_match_count++;
                if (overlap_redirect_match_count > 1) begin
                    dump_mmio_resolve_failure_context(rob_value, expected_kind,
                                                      raw_sample_flush_epoch,
                                                      raw_sample_seq,
                                                      raw_sample_cycle);
                    `uvm_fatal("MMIO_RESOLVE",
                               $sformatf("LOAD MMIO sample=%0d overlaps multiple unfinished redirect records",
                                         raw_sample_seq))
                    return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
                end
                overlap_redirect = cancel_record_q[record_probe_idx].redirect;
                overlap_redirect_epoch = cancel_record_q[record_probe_idx].redirect_epoch;
                overlap_cancel_record_id =
                    cancel_record_q[record_probe_idx].cancel_record_id;
                overlap_redirect_sample_seq = cancel_record_q[record_probe_idx].redirect_sample_seq;
            end

            foreach (redirect_anchor_history_q[anchor_idx]) begin
                int record_idx_for_anchor;
                int unsigned unanchored_seen;

                if (!redirect_anchor_history_q[anchor_idx].valid ||
                    redirect_anchor_history_q[anchor_idx].sample_seq == 0 ||
                    raw_sample_seq < redirect_anchor_history_q[anchor_idx].sample_seq ||
                    raw_sample_seq > redirect_anchor_history_q[anchor_idx].sample_seq +
                        MEMBLOCK_LOAD_MMIO_REDIRECT_MAX_SAMPLE_OFFSET) begin
                    continue;
                end
                load_overlap_observed = 1'b1;
                overlap_redirect_match_count++;
                if (overlap_redirect_match_count > 1) begin
                    dump_mmio_resolve_failure_context(rob_value, expected_kind,
                                                      raw_sample_flush_epoch,
                                                      raw_sample_seq,
                                                      raw_sample_cycle);
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
                    dump_mmio_resolve_failure_context(rob_value, expected_kind,
                                                      raw_sample_flush_epoch,
                                                      raw_sample_seq,
                                                      raw_sample_cycle);
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
                    dump_mmio_resolve_failure_context(rob_value, expected_kind,
                                                      raw_sample_flush_epoch,
                                                      raw_sample_seq,
                                                      raw_sample_cycle);
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
                overlap_cancel_record_id =
                    cancel_record_q[record_idx_for_anchor].cancel_record_id;
                overlap_redirect_sample_seq = redirect_anchor_history_q[anchor_idx].sample_seq;
            end

            if (load_overlap_observed && overlap_redirect_epoch == 0) begin
                dump_mmio_resolve_failure_context(rob_value, expected_kind,
                                                  raw_sample_flush_epoch,
                                                  raw_sample_seq,
                                                  raw_sample_cycle);
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
                dump_mmio_resolve_failure_context(rob_value, expected_kind,
                                                  raw_sample_flush_epoch,
                                                  raw_sample_seq,
                                                  raw_sample_cycle);
                `uvm_fatal("MMIO_RESOLVE",
                           $sformatf("active uid=%0d ROB=%0d/%0d lacks activation provenance",
                                     candidate_uid, key.flag, key.value))
            end
            if (status.active_instance_flush_epoch >
                memblock_sync_pkg::dispatch_flush_epoch) begin
                dump_mmio_resolve_failure_context(rob_value, expected_kind,
                                                  raw_sample_flush_epoch,
                                                  raw_sample_seq,
                                                  raw_sample_cycle);
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
                    overlap_surviving_uid = candidate_uid;
                    overlap_surviving_key = key;
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
            // 在完整R..R+2 overlap窗口内，只有唯一active scalar Load明确早于
            // redirect、完整ROB key不被覆盖，且不存在任何covered/new/incompatible
            // 混合候选时，value-only raw才可归给survivor。ROB admission保证相同
            // value的相反flag未提交owner不能同时active；已提交但仍active的owner
            // 仍会被双flag probe计入active_candidate_count并阻止该分支。
            if (raw_sample_seq >= overlap_redirect_sample_seq &&
                raw_sample_seq <= overlap_redirect_sample_seq +
                                  MEMBLOCK_LOAD_MMIO_REDIRECT_MAX_SAMPLE_OFFSET &&
                active_candidate_count == 1 &&
                overlap_uncovered_count == 1 &&
                overlap_old_covered_count == 0 &&
                overlap_new_candidate_count == 0 &&
                overlap_incompatible_count == 0) begin
                uid = overlap_surviving_uid;
                stale_reason = $sformatf(
                    "loadMmio sample=%0d overlaps redirect sample=%0d and uniquely matches surviving uid=%0d ROB=%0d/%0d epoch=%0d record=%0d",
                    raw_sample_seq, overlap_redirect_sample_seq,
                    overlap_surviving_uid, overlap_surviving_key.flag,
                    overlap_surviving_key.value, overlap_redirect_epoch,
                    overlap_cancel_record_id);
                `uvm_info("MMIO_RESOLVE", stale_reason, UVM_LOW)
                return MEMBLOCK_MMIO_RESOLVE_CURRENT;
            end
            if (raw_sample_seq >= overlap_redirect_sample_seq &&
                raw_sample_seq <= overlap_redirect_sample_seq +
                                  MEMBLOCK_LOAD_MMIO_REDIRECT_MAX_SAMPLE_OFFSET &&
                active_candidate_count == 0) begin
                memblock_uid_t deleted_uid;
                memblock_rob_key_t deleted_rob_key;
                int unsigned deleted_match_count;
                string deleted_proof_detail;

                if (find_unique_redirect_deleted_load_mmio_owner(
                        rob_value,
                        overlap_redirect,
                        overlap_redirect_epoch,
                        overlap_cancel_record_id,
                        deleted_uid,
                        deleted_rob_key,
                        deleted_match_count,
                        deleted_proof_detail)) begin
                    uid = deleted_uid;
                    stale_reason = $sformatf(
                        "loadMmio sample=%0d overlaps redirect sample=%0d at offset=%0d and uniquely matches deleted uid=%0d ROB=%0d/%0d epoch=%0d record=%0d",
                        raw_sample_seq, overlap_redirect_sample_seq,
                        raw_sample_seq - overlap_redirect_sample_seq,
                        deleted_uid, deleted_rob_key.flag,
                        deleted_rob_key.value, overlap_redirect_epoch,
                        overlap_cancel_record_id);
                    return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
                end
                stale_reason = $sformatf(
                    "cannot prove LOAD MMIO stale ownership in redirect overlap sample=%0d redirect_sample=%0d offset=%0d %s",
                    raw_sample_seq, overlap_redirect_sample_seq,
                    raw_sample_seq - overlap_redirect_sample_seq,
                    deleted_proof_detail);
                dump_mmio_resolve_failure_context(rob_value, expected_kind,
                                                  raw_sample_flush_epoch,
                                                  raw_sample_seq,
                                                  raw_sample_cycle);
                `uvm_fatal("MMIO_RESOLVE", stale_reason)
                return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
            end
            stale_reason = $sformatf(
                "cannot prove LOAD MMIO stale ownership sample=%0d redirect_sample=%0d redirect_record=%0d active=%0d old_covered=%0d new=%0d uncovered=%0d incompatible=%0d",
                raw_sample_seq, overlap_redirect_sample_seq,
                overlap_cancel_record_id,
                active_candidate_count, overlap_old_covered_count,
                overlap_new_candidate_count, overlap_uncovered_count,
                overlap_incompatible_count);
            dump_mmio_resolve_failure_context(rob_value, expected_kind,
                                              raw_sample_flush_epoch,
                                              raw_sample_seq,
                                              raw_sample_cycle);
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

        dump_mmio_resolve_failure_context(rob_value, expected_kind,
                                          raw_sample_flush_epoch,
                                          raw_sample_seq,
                                          raw_sample_cycle);
        `uvm_fatal("MMIO_RESOLVE",
                   $sformatf("cannot prove MMIO raw ownership ROB value=%0d kind=%0d raw_epoch=%0d current_epoch=%0d active=%0d newer=%0d",
                             rob_value, expected_kind, raw_sample_flush_epoch,
                             memblock_sync_pkg::dispatch_flush_epoch,
                             active_candidate_count, newer_candidate_count))
        return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
    endfunction:resolve_mmio_uid_by_rob_value

    // 抽象职责：commit handler 单点发布下一条尚未释放 ROB slot 的 UID。
    // 发布只允许前进，并验证被跨过的 UID 已经 ROB commit 或进入 terminal。
    function void publish_rob_commit_frontier(input memblock_uid_t new_frontier,
                                              input string caller);
        memblock_uid_t old_frontier;

        old_frontier = dispatch_progress.rob_commit_frontier_uid;
        if (new_frontier < old_frontier || new_frontier > main_trans_num) begin
            `uvm_fatal("ROB_ADMISSION_FRONTIER",
                       $sformatf("%s invalid ROB frontier old=%0d new=%0d main_trans_num=%0d",
                                 caller, old_frontier, new_frontier, main_trans_num))
            return;
        end
        for (memblock_uid_t uid = old_frontier; uid < new_frontier; uid++) begin
            status_transaction status;

            status = get_status(uid);
            if (!status.rob_commit && !status.terminal_done) begin
                `uvm_fatal("ROB_ADMISSION_FRONTIER",
                           $sformatf("%s skips uncommitted uid=%0d old=%0d new=%0d",
                                     caller, uid, old_frontier, new_frontier))
                return;
            end
        end
        dispatch_progress.rob_commit_frontier_uid = new_frontier;
        rob_admission_blocked_log_valid = 1'b0;
    endfunction:publish_rob_commit_frontier

    // 抽象职责：normal admission gate。窗口满时返回0形成 backpressure；
    // UID顺序或静态 ROB key 漂移属于框架损坏，直接 fatal。
    function bit rob_admission_window_has_room(input memblock_uid_t candidate_uid,
                                               input string caller);
        memblock_uid_t frontier_uid;
        longint unsigned distance;
        memblock_rob_key_t frontier_rob_key;
        memblock_rob_key_t candidate_rob_key;
        memblock_rob_key_t expected_rob_key;

        if (!main_table_ready || candidate_uid >= main_trans_num) begin
            `uvm_fatal("ROB_ADMISSION_WINDOW",
                       $sformatf("%s invalid candidate uid=%0d ready=%0d main_trans_num=%0d",
                                 caller, candidate_uid, main_table_ready, main_trans_num))
            return 1'b0;
        end
        frontier_uid = dispatch_progress.rob_commit_frontier_uid;
        if (frontier_uid > main_trans_num || candidate_uid < frontier_uid) begin
            `uvm_fatal("ROB_ADMISSION_WINDOW",
                       $sformatf("%s candidate precedes ROB frontier candidate=%0d frontier=%0d main_trans_num=%0d",
                                 caller, candidate_uid, frontier_uid, main_trans_num))
            return 1'b0;
        end
        distance = longint'(candidate_uid) - longint'(frontier_uid);
        if (frontier_uid < main_trans_num) begin
            frontier_rob_key = get_main_transaction(frontier_uid).get_rob_key();
            candidate_rob_key = get_main_transaction(candidate_uid).get_rob_key();
            expected_rob_key = rob_order_util::rob_advance(frontier_rob_key,
                                                            int'(distance));
            if (candidate_rob_key != expected_rob_key) begin
                `uvm_fatal("ROB_ADMISSION_WINDOW",
                           $sformatf("%s static ROB order mismatch candidate=%0d rob=%0d/%0d frontier=%0d rob=%0d/%0d distance=%0d expected=%0d/%0d",
                                     caller, candidate_uid,
                                     candidate_rob_key.flag, candidate_rob_key.value,
                                     frontier_uid,
                                     frontier_rob_key.flag, frontier_rob_key.value,
                                     distance,
                                     expected_rob_key.flag, expected_rob_key.value))
                return 1'b0;
            end
        end
        if (distance >= MEMBLOCK_ROB_SIZE) begin
            if (!rob_admission_blocked_log_valid ||
                rob_admission_blocked_candidate_uid != candidate_uid ||
                rob_admission_blocked_frontier_uid != frontier_uid) begin
                candidate_rob_key = get_main_transaction(candidate_uid).get_rob_key();
                `uvm_info("ROB_ADMISSION_WINDOW",
                          $sformatf("ROB admission blocked caller=%s candidate=%0d rob=%0d/%0d frontier=%0d distance=%0d limit=%0d max_enqueued_valid/uid=%0d/%0d terminal_done_uid=%0d",
                                    caller, candidate_uid,
                                    candidate_rob_key.flag, candidate_rob_key.value,
                                    frontier_uid, distance, MEMBLOCK_ROB_SIZE,
                                    dispatch_progress.max_enqueued_uid_valid,
                                    dispatch_progress.max_enqueued_uid,
                                    dispatch_progress.terminal_done_uid),
                          UVM_LOW)
                rob_admission_blocked_log_valid = 1'b1;
                rob_admission_blocked_candidate_uid = candidate_uid;
                rob_admission_blocked_frontier_uid = frontier_uid;
            end
            return 1'b0;
        end
        return 1'b1;
    endfunction:rob_admission_window_has_room

    // 返回值让 expected-fatal 定向测试在捕获 fatal 后仍能阻止 map 写入。
    function bit assert_uid_within_rob_admission_window(input memblock_uid_t uid,
                                                        input string caller);
        if (rob_admission_window_has_room(uid, caller)) begin
            return 1'b1;
        end
        `uvm_fatal("ROB_ADMISSION_WINDOW",
                   $sformatf("%s attempted activation outside ROB admission window uid=%0d frontier=%0d limit=%0d",
                             caller, uid,
                             dispatch_progress.rob_commit_frontier_uid,
                             MEMBLOCK_ROB_SIZE))
        return 1'b0;
    endfunction:assert_uid_within_rob_admission_window

    // redirect 等低频事件入口使用有限 active ROB map 审计，不扫描完整主表。
    function bit audit_active_rob_window(input string caller);
        memblock_uid_t frontier_uid;

        frontier_uid = dispatch_progress.rob_commit_frontier_uid;
        foreach (uid_by_active_rob[map_key]) begin
            memblock_uid_t uid;
            status_transaction status;
            memblock_rob_key_t rob_key;

            uid = uid_by_active_rob[map_key];
            if (!is_valid_uid(uid) || status_by_uid[uid] == null ||
                main_table_by_uid[uid] == null) begin
                `uvm_fatal("ROB_ADMISSION_WINDOW",
                           $sformatf("%s active ROB map references invalid uid=%0d map_key=%0h",
                                     caller, uid, map_key))
                return 1'b0;
            end
            status = status_by_uid[uid];
            rob_key = status.get_rob_key();
            if (!status.active || rob_order_util::rob_to_map_key(rob_key) != map_key ||
                get_main_transaction(uid).get_rob_key() != rob_key) begin
                `uvm_fatal("ROB_ADMISSION_WINDOW",
                           $sformatf("%s active ROB map identity mismatch uid=%0d map_key=%0h active=%0d rob=%0d/%0d",
                                     caller, uid, map_key, status.active,
                                     rob_key.flag, rob_key.value))
                return 1'b0;
            end
            if (!status.rob_commit) begin
                if (!assert_uid_within_rob_admission_window(uid, caller)) begin
                    return 1'b0;
                end
            end else if (uid > frontier_uid) begin
                `uvm_fatal("ROB_ADMISSION_FRONTIER",
                           $sformatf("%s committed active owner is younger than frontier uid=%0d frontier=%0d",
                                     caller, uid, frontier_uid))
                return 1'b0;
            end
        end
        return 1'b1;
    endfunction:audit_active_rob_window

    // redirect 完成后只验证 prefix containment；发现 prefix 外 owner 报错，不补扫。
    function bit assert_active_owners_within_admission_prefix(
        input memblock_redirect_payload_t redirect,
        input string caller);
        memblock_uid_t new_end_uid;
        int unsigned provenance_match_count;

        new_end_uid = get_active_scan_end_uid();
        provenance_match_count = 0;
        foreach (uid_by_active_rob[map_key]) begin
            memblock_uid_t uid;
            status_transaction status;
            memblock_rob_key_t rob_key;

            uid = uid_by_active_rob[map_key];
            if (uid < new_end_uid) begin
                continue;
            end
            if (!is_valid_uid(uid) || status_by_uid[uid] == null ||
                main_table_by_uid[uid] == null) begin
                `uvm_fatal("ROB_ADMISSION_PREFIX",
                           $sformatf("%s prefix scan references invalid uid=%0d", caller, uid))
                return 1'b0;
            end
            status = status_by_uid[uid];
            rob_key = status.get_rob_key();
            if (control_barrier_prefix_external_provenance_valid &&
                uid == control_barrier_prefix_external_provenance_uid &&
                active_control_barrier_valid && active_control_barrier_uid == uid &&
                is_control_op_class(main_table_by_uid[uid].op_class) &&
                status.active && status.enq && !status.terminal_done &&
                status.control_state == MEMBLOCK_CONTROL_STATE_WAIT_OLDER_ROB_COMMIT &&
                !status.control_owner.valid && !status.control_action_enqueued &&
                !status.active_lq_mapped && !status.active_sq_mapped &&
                rob_order_util::rob_need_flush(rob_key, redirect)) begin
                provenance_match_count++;
                continue;
            end
            `uvm_fatal("ROB_ADMISSION_PREFIX",
                       $sformatf("%s found prefix-external active owner uid=%0d rob=%0d/%0d redirect=%0d/%0d new_end=%0d frontier=%0d distance=%0d lq/sq_mapped=%0d/%0d provenance=%0d/%0d barrier=%0d/%0d",
                                 caller, uid, rob_key.flag, rob_key.value,
                                 redirect.rob_key.flag, redirect.rob_key.value,
                                 new_end_uid,
                                 dispatch_progress.rob_commit_frontier_uid,
                                 uid - dispatch_progress.rob_commit_frontier_uid,
                                 status.active_lq_mapped,
                                 status.active_sq_mapped,
                                 control_barrier_prefix_external_provenance_valid,
                                 control_barrier_prefix_external_provenance_uid,
                                 active_control_barrier_valid,
                                 active_control_barrier_uid))
            return 1'b0;
        end
        if (control_barrier_prefix_external_provenance_valid && provenance_match_count != 1) begin
            `uvm_fatal("ROB_ADMISSION_PREFIX",
                       $sformatf("%s provenance uid=%0d matched prefix-external owners %0d times new_end=%0d",
                                 caller, control_barrier_prefix_external_provenance_uid,
                                 provenance_match_count, new_end_uid))
            return 1'b0;
        end
        return 1'b1;
    endfunction:assert_active_owners_within_admission_prefix

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

    function bit rollback_max_enqueued_uid(input memblock_uid_t oldest_flushed_uid);
        if (!is_valid_uid(oldest_flushed_uid)) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("rollback_max_enqueued_uid got invalid uid=%0d main_trans_num=%0d",
                                 oldest_flushed_uid, main_trans_num))
            return 1'b0;
        end
        if (oldest_flushed_uid == 0) begin
            dispatch_progress.max_enqueued_uid       = 0;
            dispatch_progress.max_enqueued_uid_valid = 1'b0;
            return 1'b1;
        end
        dispatch_progress.max_enqueued_uid       = oldest_flushed_uid - 1;
        dispatch_progress.max_enqueued_uid_valid = 1'b1;
        return 1'b1;
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
               !redirect_deleted_owner_window_active &&
               redirect_deleted_lq_owner_by_key.num() == 0 &&
               redirect_deleted_sq_owner_by_key.num() == 0 &&
               !active_control_barrier_valid &&
               !control_barrier_prefix_external_provenance_valid &&
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

    // 抽象职责：撤销尚未提交的 global-stop prepare。调用者只在公共框架 work 再次出现
    // 或 testcase reset 时使用；它不清任何 responder 本地 queue，也不改 UID terminal 状态。
    function void reset_global_stop_prepare();
        global_stop_prepare_requested = 1'b0;
        global_stop_prepare_last_activity_time = 0;
        global_stop_prepare_dcache_last_sample_time = 0;
        global_stop_prepare_sbuffer_last_sample_time = 0;
    endfunction:reset_global_stop_prepare

    // 抽象职责：由 DCache/SBuffer responder 报告其在 prepare 期间观察到的真实尾部 activity。
    // 该函数只刷新静默窗口的起点；不会重新开放已经提交的 global stop，也不会改变 responder
    // 的 request/response owner 或本地 queue。调用应覆盖 A.valid，不能只等待 A.fire。
    function void note_global_stop_prepare_activity();
        if (!global_stop_requested && global_stop_prepare_requested) begin
            global_stop_prepare_last_activity_time = $time;
        end
    endfunction:note_global_stop_prepare_activity

    // 抽象职责：记录 prepare 期间 DCache/SBuffer 已到达的真实 clocking-block sample。
    // 该时间戳不是 activity，不重置 1us 静默窗口；它只为 commit 侧提供跨采样边界的
    // settle proof。is_dcache=1 表示 DCache，0 表示 SBuffer。
    function void note_global_stop_prepare_responder_sample(input bit is_dcache);
        if (!global_stop_requested && global_stop_prepare_requested) begin
            if (is_dcache) begin
                global_stop_prepare_dcache_last_sample_time = $time;
            end
            else begin
                global_stop_prepare_sbuffer_last_sample_time = $time;
            end
        end
    endfunction:note_global_stop_prepare_responder_sample

    // 抽象职责：判断两个 memory responder 是否均已跨过当前 quiet deadline 的真实
    // sample。仅 real-dispatch 生命周期启用该约束；software-only directed sequence
    // 没有 DCache/SBuffer responder，必须保留既有的立即 stop 语义。
    function bit global_stop_prepare_responder_samples_settled();
        time quiet_deadline;

        if (!memblock_sync_pkg::dispatch_real_smoke_active) begin
            return 1'b1;
        end
        quiet_deadline = global_stop_prepare_last_activity_time +
                         MEMBLOCK_GLOBAL_STOP_PREPARE_QUIET_TIME;
        return global_stop_prepare_dcache_last_sample_time >= quiet_deadline &&
               global_stop_prepare_sbuffer_last_sample_time >= quiet_deadline;
    endfunction:global_stop_prepare_responder_samples_settled

    function bit is_global_stop_prepare_requested();
        return global_stop_prepare_requested;
    endfunction:is_global_stop_prepare_requested

    // 抽象职责：把主表完成转换为两阶段 global stop。第一阶段只启动 1us 静默窗口；
    // 第二阶段确认 UID/公共队列仍收敛且 DCache/SBuffer 未报告新 activity 后才提交 stop。
    // 这样 responder 可以在 prepare 期间继续接收 DUT 内部尾请求，不能在此函数中等待或清队列。
    function void request_global_stop_if_done();
        if (global_stop_requested) begin
            return;
        end

        if (transaction_done() && control_barrier_prefix_external_provenance_valid) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("transaction complete with residual control provenance uid=%0d origin_old/new=%0d/%0d",
                                 control_barrier_prefix_external_provenance_uid,
                                 control_barrier_prefix_external_origin_old_end_uid,
                                 control_barrier_prefix_external_origin_new_end_uid))
            return;
        end

        if (!transaction_done() || !runtime_drain_complete()) begin
            reset_global_stop_prepare();
            return;
        end

        // 软件定向 sequence 不启动 memory-facing responder；对该拓扑保留原先在
        // transaction/runtime drain 同拍提交 global stop 的行为，避免把无消费者的
        // 1us settle 窗口变成永久等待。
        if (!memblock_sync_pkg::dispatch_real_smoke_active) begin
            global_stop_requested = 1'b1;
            return;
        end

        if (!global_stop_prepare_requested) begin
            global_stop_prepare_requested = 1'b1;
            global_stop_prepare_last_activity_time = $time;
            global_stop_prepare_dcache_last_sample_time = 0;
            global_stop_prepare_sbuffer_last_sample_time = 0;
            `uvm_info("COMMON_DATA",
                      $sformatf("global stop prepare started at time=%0t; require %0t quiet memory-responder window",
                                $time, MEMBLOCK_GLOBAL_STOP_PREPARE_QUIET_TIME),
                      UVM_LOW)
            return;
        end

        if (($time - global_stop_prepare_last_activity_time) >=
                MEMBLOCK_GLOBAL_STOP_PREPARE_QUIET_TIME &&
            global_stop_prepare_responder_samples_settled()) begin
            global_stop_requested = 1'b1;
            `uvm_info("COMMON_DATA",
                      $sformatf("global stop committed after %0t quiet memory-responder window",
                                MEMBLOCK_GLOBAL_STOP_PREPARE_QUIET_TIME),
                      UVM_LOW)
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
        memblock_control_lifecycle_ticket_t audit_ticket;

        check_uid(uid, "activate_control_uid");
        if (get_next_new_admit_uid() != uid) begin
            `uvm_fatal("CONTROL_ADMISSION",
                       $sformatf("control admission uid=%0d is not the next prefix uid=%0d",
                                 uid, get_next_new_admit_uid()))
            return;
        end
        if (active_control_barrier_valid) begin
            `uvm_fatal("CONTROL_ADMISSION",
                       $sformatf("cannot admit control uid=%0d while barrier uid=%0d is active",
                                 uid, active_control_barrier_uid))
            return;
        end
        if (!assert_uid_within_rob_admission_window(uid, "activate_control_uid")) begin
            return;
        end
        main_tr = get_main_transaction(uid);
        status = get_status(uid);
        if (!is_control_op_class(main_tr.op_class) || status.active || status.enq ||
            status.terminal_done) begin
            `uvm_fatal("CONTROL_ADMISSION",
                       $sformatf("invalid fresh control admission uid=%0d op=%0d active/enq/done=%0d/%0d/%0d",
                                 uid, main_tr.op_class, status.active, status.enq,
                                 status.terminal_done))
            return;
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
            return;
        end
        if (!precheck_control_lifecycle_event(
                uid, MEMBLOCK_CONTROL_AUDIT_CREATION, audit_ticket)) begin
            return;
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
        record_control_lifecycle_event(audit_ticket);
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
            return;
        end
        if (!control_barrier_prefix_external_provenance_valid ||
            control_barrier_prefix_external_provenance_uid != uid) begin
            `uvm_fatal("CONTROL_ADMISSION_PROVENANCE",
                       $sformatf("restore uid=%0d lacks matching provenance valid/uid=%0d/%0d",
                                 uid, control_barrier_prefix_external_provenance_valid,
                                 control_barrier_prefix_external_provenance_uid))
            return;
        end
        mark_uid_enqueued(uid);
        clear_control_barrier_prefix_external_provenance();
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    endfunction:restore_control_admission_prefix

    // 抽象职责：确认 redirect 命中的是尚未拥有动作实例的静态控制标记。该 helper
    // 不清状态、不撤 ROB map、不计入 LSQ cancel，也不推进 dynamic_epoch。
    function bit preserve_static_control_marker_on_redirect(
        input memblock_uid_t uid,
        input memblock_redirect_payload_t redirect
    );
        status_transaction status;

        if (!is_valid_uid(uid) || status_by_uid[uid] == null ||
            main_table_by_uid[uid] == null) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("preserve_static_control_marker_on_redirect invalid uid=%0d", uid))
            return 1'b0;
        end
        status = status_by_uid[uid];
        if (!redirect.valid || !is_control_op_class(main_table_by_uid[uid].op_class) ||
            !active_control_barrier_valid || active_control_barrier_uid != uid ||
            !status.active || !status.enq || status.terminal_done ||
            status.control_state != MEMBLOCK_CONTROL_STATE_WAIT_OLDER_ROB_COMMIT ||
            status.control_action_enqueued || status.control_owner.valid) begin
            `uvm_fatal("CONTROL_REDIRECT",
                       $sformatf("redirect cannot preserve control uid=%0d state=%0d owner_valid=%0d action_enqueued=%0d",
                                 uid, status.control_state, status.control_owner.valid,
                                 status.control_action_enqueued))
            return 1'b0;
        end
        return 1'b1;
    endfunction:preserve_static_control_marker_on_redirect

    // 抽象职责：控制 worker 在 terminal_done 已被公共 commit/retire 固化后解除 barrier，
    // 使后续 UID 恢复 admission；它不负责 commit 或状态推进。
    function void release_control_barrier_after_terminal(input memblock_uid_t uid);
        status_transaction status;
        memblock_control_lifecycle_ticket_t audit_ticket;
        bit matching_csr_action;
        bit matching_sfence_action;
        bit matching_l2_release_request;

        check_uid(uid, "release_control_barrier_after_terminal");
        status = get_status(uid);
        if (control_barrier_prefix_external_provenance_valid) begin
            `uvm_fatal("CONTROL_BARRIER",
                       $sformatf("cannot release uid=%0d with residual provenance uid=%0d",
                                 uid, control_barrier_prefix_external_provenance_uid))
            return;
        end
        if (!active_control_barrier_valid || active_control_barrier_uid != uid ||
            !uid_is_control_marker(uid) || !status.terminal_done || status.active) begin
            `uvm_fatal("CONTROL_BARRIER",
                       $sformatf("cannot release barrier uid=%0d active_barrier=%0d/%0d terminal/active=%0d/%0d",
                                 uid, active_control_barrier_valid,
                                 active_control_barrier_uid, status.terminal_done,
                                 status.active))
            return;
        end
        matching_csr_action = 1'b0;
        foreach (csr_control_action_q[idx]) begin
            if (memblock_control_owner_equal(csr_control_action_q[idx].owner,
                                             status.control_owner)) begin
                matching_csr_action = 1'b1;
            end
        end
        matching_sfence_action = 1'b0;
        foreach (sfence_control_action_q[idx]) begin
            if (memblock_control_owner_equal(sfence_control_action_q[idx].owner,
                                             status.control_owner)) begin
                matching_sfence_action = 1'b1;
            end
        end
        matching_l2_release_request = 1'b0;
        foreach (l2_flush_release_request_q[idx]) begin
            if (memblock_control_owner_equal(l2_flush_release_request_q[idx].owner,
                                             status.control_owner)) begin
                matching_l2_release_request = 1'b1;
            end
        end
        if (status.control_action_enqueued || matching_csr_action ||
            matching_sfence_action || matching_l2_release_request ||
            status.control_flushsb_request_queued ||
            (attached_flushsb_req_valid && attached_flushsb_req.owner_valid &&
             memblock_control_owner_equal(attached_flushsb_req.owner, status.control_owner)) ||
            (active_flushsb_req_valid && active_flushsb_req.owner_valid &&
             memblock_control_owner_equal(active_flushsb_req.owner, status.control_owner)) ||
            (flushsb_completed.valid &&
             memblock_control_owner_equal(flushsb_completed.owner, status.control_owner))) begin
            `uvm_fatal("CONTROL_LIFECYCLE_AUDIT",
                       $sformatf("release uid=%0d has residual action/queue/flushSb", uid))
            return;
        end
        if (!precheck_control_lifecycle_event(
                uid, MEMBLOCK_CONTROL_AUDIT_BARRIER_RELEASE, audit_ticket)) begin
            return;
        end
        active_control_barrier_valid = 1'b0;
        active_control_barrier_uid = 0;
        status.control_owner = '{default:'0};
        status.control_action_enqueued = 1'b0;
        status.control_flushsb_request_queued = 1'b0;
        record_control_lifecycle_event(audit_ticket);
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
        csr_dynamic_candidate_valid = 1'b0;
        csr_dynamic_candidate_expected_write_count = 0;
        csr_dynamic_candidate_owner = '{default:'0};
        csr_dynamic_candidate_profile = '{default:'0};
        csr_dynamic_candidate_after_sample = 0;
        csr_dynamic_candidate_pmp_pma_required = 1'b0;
    endfunction:reset_control_action_runtime

    // 抽象职责：持久化一个已绑定 owner 的 CSR 工作项，再唤醒 CSR worker。
    // event 丢失不会丢动作，因为 worker 每次醒来都重新检查 queue；本函数不推进
    // status，调用者必须先完成 owner/state 校验和状态写入。
    function void enqueue_csr_control_action(input memblock_csr_control_action_t action);
        memblock_control_lifecycle_ticket_t audit_ticket;
        status_transaction status;
        memblock_control_audit_event_e event_kind;

        if (!action.owner.valid) begin
            `uvm_fatal("CONTROL_ACTION", "enqueue_csr_control_action got invalid owner")
            return;
        end
        if (control_workers_shutdown_requested || csr_control_worker_exited) begin
            `uvm_fatal("CONTROL_ACTION", "cannot enqueue CSR action after worker shutdown/exited")
            return;
        end
        if (!is_valid_uid(action.owner.uid) || status_by_uid[action.owner.uid] == null) begin
            `uvm_fatal("CONTROL_ACTION", "enqueue CSR action references invalid owner uid")
            return;
        end
        status = status_by_uid[action.owner.uid];
        if (!memblock_control_owner_equal(status.control_owner, action.owner)) begin
            `uvm_fatal("CONTROL_ACTION", "enqueue CSR action owner mismatch")
            return;
        end
        if (action.completion_profile == MEMBLOCK_CONTROL_COMPLETION_RUNTIME_CSR_SNAPSHOT &&
            action.l2_flush_phase == MEMBLOCK_L2_FLUSH_PHASE_NONE) begin
            event_kind = MEMBLOCK_CONTROL_AUDIT_CSR_ENQUEUE;
        end else if (action.completion_profile == MEMBLOCK_CONTROL_COMPLETION_L2_FLUSH_LEVEL &&
                     action.l2_flush_phase == MEMBLOCK_L2_FLUSH_PHASE_ASSERT) begin
            event_kind = MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_ENQUEUE;
        end else begin
            `uvm_fatal("CONTROL_ACTION", "enqueue CSR action has unsupported audit profile")
            return;
        end
        if (!precheck_control_lifecycle_event(action.owner.uid, event_kind, audit_ticket)) begin
            return;
        end
        commit_csr_control_action(action, audit_ticket);
    endfunction:enqueue_csr_control_action

    // 抽象职责：CSR worker 从 FIFO 取出唯一待交付 action；为空时只返回 0，
    // 不等待 event，避免把 queue state 与 event 唤醒语义混为一体。
    function bit try_pop_csr_control_action(output memblock_csr_control_action_t action);
        memblock_control_lifecycle_ticket_t audit_ticket;
        memblock_control_audit_event_e event_kind;

        if (csr_control_action_q.size() == 0) begin
            action = '{default:'0};
            return 1'b0;
        end
        action = csr_control_action_q[0];
        if (action.completion_profile == MEMBLOCK_CONTROL_COMPLETION_RUNTIME_CSR_SNAPSHOT &&
            action.l2_flush_phase == MEMBLOCK_L2_FLUSH_PHASE_NONE) begin
            event_kind = MEMBLOCK_CONTROL_AUDIT_CSR_START;
        end else if (action.completion_profile == MEMBLOCK_CONTROL_COMPLETION_L2_FLUSH_LEVEL &&
                     action.l2_flush_phase == MEMBLOCK_L2_FLUSH_PHASE_ASSERT) begin
            event_kind = MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_START;
        end else begin
            `uvm_fatal("CONTROL_ACTION", "CSR queue front has unsupported audit profile")
            action = '{default:'0};
            return 1'b0;
        end
        if (!precheck_control_lifecycle_event(action.owner.uid, event_kind, audit_ticket)) begin
            action = '{default:'0};
            return 1'b0;
        end
        action = csr_control_action_q.pop_front();
        record_control_lifecycle_event(audit_ticket);
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
        if (status.control_sfence_c0_armed || status.control_expected_sfence_valid) begin
            `uvm_fatal("CONTROL_ACTION", "CSR sendover precedes mapping fence completion")
            return;
        end
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
        memblock_control_lifecycle_ticket_t audit_ticket;
        status_transaction status;

        if (!action.owner.valid) begin
            `uvm_fatal("CONTROL_ACTION", "enqueue_sfence_control_action got invalid owner")
            return;
        end
        if (control_workers_shutdown_requested || sfence_control_worker_exited) begin
            `uvm_fatal("CONTROL_ACTION", "cannot enqueue SFence action after worker shutdown/exited")
            return;
        end
        if (!is_valid_uid(action.owner.uid) || status_by_uid[action.owner.uid] == null) begin
            `uvm_fatal("CONTROL_ACTION", "enqueue SFence action references invalid owner uid")
            return;
        end
        status = status_by_uid[action.owner.uid];
        if (!memblock_control_owner_equal(status.control_owner, action.owner)) begin
            `uvm_fatal("CONTROL_ACTION", "enqueue SFence action owner mismatch")
            return;
        end
        if (!precheck_control_lifecycle_event(
                action.owner.uid, MEMBLOCK_CONTROL_AUDIT_SFENCE_ENQUEUE,
                audit_ticket)) begin
            return;
        end
        sfence_control_action_q.push_back(action);
        status.control_action_enqueued = 1'b1;
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        record_control_lifecycle_event(audit_ticket);
        ->sfence_control_action_available_ev;
    endfunction:enqueue_sfence_control_action

    function bit try_pop_sfence_control_action(output memblock_sfence_control_action_t action);
        memblock_control_lifecycle_ticket_t audit_ticket;

        if (sfence_control_action_q.size() == 0) begin
            action = '{default:'0};
            return 1'b0;
        end
        action = sfence_control_action_q[0];
        if (!precheck_control_lifecycle_event(
                action.owner.uid, MEMBLOCK_CONTROL_AUDIT_SFENCE_START,
                audit_ticket)) begin
            action = '{default:'0};
            return 1'b0;
        end
        action = sfence_control_action_q.pop_front();
        record_control_lifecycle_event(audit_ticket);
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
        memblock_control_lifecycle_ticket_t audit_ticket;
        status_transaction status;

        if (!request.owner.valid || request.control_reset_epoch == 0) begin
            `uvm_fatal("CONTROL_ACTION", "enqueue_l2_flush_release_request got invalid request")
            return;
        end
        if (l2_flush_release_request_q.size() != 0) begin
            `uvm_fatal("CONTROL_ACTION", "only one active L2 flush RELEASE request is supported")
            return;
        end
        if (!is_valid_uid(request.owner.uid) || status_by_uid[request.owner.uid] == null) begin
            `uvm_fatal("CONTROL_ACTION", "enqueue L2 release references invalid owner uid")
            return;
        end
        status = status_by_uid[request.owner.uid];
        if (!memblock_control_owner_equal(status.control_owner, request.owner) ||
            !precheck_control_lifecycle_event(
                request.owner.uid, MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_ENQUEUE,
                audit_ticket)) begin
            return;
        end
        l2_flush_release_request_q.push_back(request);
        status.control_state = MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_RELEASE;
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        record_control_lifecycle_event(audit_ticket);
        ->csr_control_action_available_ev;
    endfunction:enqueue_l2_flush_release_request

    function bit try_pop_l2_flush_release_request(
        output memblock_l2_flush_release_request_t request
    );
        memblock_control_lifecycle_ticket_t audit_ticket;

        if (l2_flush_release_request_q.size() == 0) begin
            request = '{default:'0};
            return 1'b0;
        end
        request = l2_flush_release_request_q[0];
        if (!precheck_control_lifecycle_event(
                request.owner.uid, MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_START,
                audit_ticket)) begin
            request = '{default:'0};
            return 1'b0;
        end
        request = l2_flush_release_request_q.pop_front();
        record_control_lifecycle_event(audit_ticket);
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
                    clear_uncache_retry_boost(uid, "terminal status");
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

    // 保留给专项直接构造历史issue epoch；正式scheduler使用
    // mark_issue_snapshot()在全部precheck成功后分配epoch。
    function int unsigned alloc_issue_epoch();
        global_issue_epoch++;
        return global_issue_epoch;
    endfunction:alloc_issue_epoch

    function bit mark_issue_snapshot(input memblock_uid_t uid,
                                     input memblock_issue_target_e issue_target,
                                     output int unsigned issue_epoch);
        status_transaction status;

        issue_epoch = 0;
        if (!is_valid_uid(uid) || status_by_uid[uid] == null ||
            !(issue_target inside {MEMBLOCK_ISSUE_TARGET_LOAD,
                                   MEMBLOCK_ISSUE_TARGET_STA,
                                   MEMBLOCK_ISSUE_TARGET_STD}) ||
            global_issue_epoch == '1) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("mark_issue_snapshot precheck failed uid=%0d target=%0d epoch=%0d",
                                 uid, issue_target, global_issue_epoch))
            return 1'b0;
        end
        if (!register_uid_tlb_record_on_issue(uid)) begin
            return 1'b0;
        end
        global_issue_epoch++;
        issue_epoch = global_issue_epoch;
        status = status_by_uid[uid];
        status.set_target_issue_epoch(issue_target, issue_epoch);
        status.set_target_instance_flush_epoch(
            issue_target,
            memblock_sync_pkg::dispatch_flush_epoch
        );
        status.issue_killed = 1'b0;
        return 1'b1;
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

    function bit is_scalar_store_uid(input memblock_uid_t uid);
        main_control_transaction main_tr;

        main_tr = get_main_transaction(uid);
        return main_tr.fuType == MEMBLOCK_FUTYPE_STU ||
               main_tr.fuType == MEMBLOCK_FUTYPE_MOU;
    endfunction:is_scalar_store_uid

    function bit store_real_writebacks_done(input memblock_uid_t uid);
        status_transaction status;

        status = get_status(uid);
        if (is_scalar_store_uid(uid)) begin
            return status.sta_writeback && status.std_writeback;
        end
        if (get_main_transaction(uid).fuType == MEMBLOCK_FUTYPE_LDU) begin
            return status.load_writeback;
        end
        `uvm_fatal("COMMON_DATA", $sformatf("store_real_writebacks_done unsupported fuType uid=%0d fuType=0x%0h",
                                             uid, get_main_transaction(uid).fuType))
        return 1'b0;
    endfunction:store_real_writebacks_done

    function bit pending_store_fault_matches(input memblock_uid_t uid);
        status_transaction status;

        if (!pending_store_fault_by_uid.exists(uid) ||
            !pending_store_fault_by_uid[uid].valid) begin
            return 1'b0;
        end
        status = get_status(uid);
        return status.active &&
               is_scalar_store_uid(uid) &&
               !status.redirect_pending &&
               !status.flushed &&
               !status.issue_killed &&
               pending_store_fault_by_uid[uid].uid == uid &&
               pending_store_fault_by_uid[uid].dynamic_epoch == status.dynamic_epoch;
    endfunction:pending_store_fault_matches

    // 抽象职责：判断当前动态实例是否仍处于 store-fault 的双 real-WB 终态屏障。
    // 该判断只认与 status.dynamic_epoch 相同的 pending record；旧实例被 redirect
    // 清理后，即使迟到 WB 仍带有同 UID，也不能重新建立终态等待。
    function bit store_fault_real_wb_pending(input memblock_uid_t uid);
        return pending_store_fault_matches(uid) &&
               !store_real_writebacks_done(uid);
    endfunction:store_fault_real_wb_pending

    // 抽象职责：pendingPtr 只在缺失的另一侧已经真正 dispatch 后才回退。STA
    // fault 可能早于 STD dispatch 到达；此时回退 pendingPtr 会阻塞 STD 自己入队，
    // 形成 uid130 场景的自锁。terminal/commit 仍使用上面的严格屏障，不会提前提交。
    function bit store_fault_blocks_pending_ptr(input memblock_uid_t uid);
        status_transaction             status;
        memblock_pending_store_fault_t pending;

        if (!store_fault_real_wb_pending(uid)) begin
            return 1'b0;
        end
        status = get_status(uid);
        pending = pending_store_fault_by_uid[uid];
        if (pending.sta_fault_seen && !status.std_dispatched) begin
            return 1'b0;
        end
        if (pending.std_fault_seen && !status.sta_dispatched) begin
            return 1'b0;
        end
        return 1'b1;
    endfunction:store_fault_blocks_pending_ptr

    // SQ deq/release is the other half of the store-fault barrier. The
    // missing target may still be dispatched, but the physical SQ owner
    // remains mapped until both real-WB events arrive.
    function bit store_fault_blocks_sq_deq(input memblock_uid_t uid);
        return store_fault_real_wb_pending(uid);
    endfunction:store_fault_blocks_sq_deq

    function bit clear_pending_store_fault(input memblock_uid_t uid);
        if (pending_store_fault_by_uid.exists(uid)) begin
            pending_store_fault_by_uid.delete(uid);
            return 1'b1;
        end
        return 1'b0;
    endfunction:clear_pending_store_fault

    function bit capture_pending_store_fault(input memblock_uid_t uid,
                                             input memblock_issue_target_e target,
                                             input int unsigned issue_epoch,
                                             input int unsigned replay_seq,
                                             input bit [23:0] exception_vec,
                                             input longint unsigned cycle);
        status_transaction              status;
        memblock_pending_store_fault_t  pending;

        status = get_status(uid);
        if (!status.active || !is_scalar_store_uid(uid) ||
            (target != MEMBLOCK_ISSUE_TARGET_STA && target != MEMBLOCK_ISSUE_TARGET_STD)) begin
            return 1'b0;
        end
        if (!pending_store_fault_matches(uid)) begin
            pending = '{default:'0};
            pending.valid = 1'b1;
            pending.uid = uid;
            pending.dynamic_epoch = status.dynamic_epoch;
            pending_store_fault_by_uid[uid] = pending;
        end
        pending = pending_store_fault_by_uid[uid];
        if (target == MEMBLOCK_ISSUE_TARGET_STA) begin
            if (pending.sta_fault_seen &&
                (pending.sta_issue_epoch != issue_epoch || pending.sta_replay_seq != replay_seq ||
                 pending.sta_exception_vec != exception_vec)) begin
                `uvm_fatal("STORE_FAULT_BARRIER", $sformatf("STA pending fault identity mismatch uid=%0d", uid))
            end
            pending.sta_fault_seen = 1'b1;
            pending.sta_issue_epoch = issue_epoch;
            pending.sta_replay_seq = replay_seq;
            pending.sta_exception_vec = exception_vec;
        end else begin
            if (pending.std_fault_seen &&
                (pending.std_issue_epoch != issue_epoch || pending.std_replay_seq != replay_seq ||
                 pending.std_exception_vec != exception_vec)) begin
                `uvm_fatal("STORE_FAULT_BARRIER", $sformatf("STD pending fault identity mismatch uid=%0d", uid))
            end
            pending.std_fault_seen = 1'b1;
            pending.std_issue_epoch = issue_epoch;
            pending.std_replay_seq = replay_seq;
            pending.std_exception_vec = exception_vec;
        end
        pending.cycle = cycle;
        pending_store_fault_by_uid[uid] = pending;
        return 1'b1;
    endfunction:capture_pending_store_fault

    function bit finalize_pending_store_fault(input memblock_uid_t uid);
        status_transaction              status;
        memblock_pending_store_fault_t  pending;
        bit [23:0]                      exception_vec;

        if (!pending_store_fault_matches(uid)) begin
            return 1'b0;
        end
        status = get_status(uid);
        if (!status.sta_writeback || !status.std_writeback) begin
            return 1'b0;
        end
        pending = pending_store_fault_by_uid[uid];
        exception_vec = pending.sta_fault_seen ? pending.sta_exception_vec : pending.std_exception_vec;
        status.sta_fault = status.sta_fault || pending.sta_fault_seen;
        status.std_fault = status.std_fault || pending.std_fault_seen;
        status.fault = 1'b1;
        status.exception_pending = 1'b1;
        status.exception_vec = exception_vec;
        status.pass = 1'b0;
        status.success = 1'b0;
        status.terminal_done = 1'b0;
        // 中文注释：双 real-WB 屏障通过后才进入 fault 生命周期；redirect 前仍保留
        // 尚未 fire 的 STD，避免把异常落表误当成 DUT redirect flush 边界。
        quiesce_fault_uid_pending_work(uid, 1'b0);
        release_ptw_wait_replay(uid);
        status.clear_sta_late_fault_tombstones();
        void'(clear_pending_store_fault(uid));
        status.last_event_cycle = pending.cycle;
        `uvm_info("STORE_FAULT_BARRIER",
                  $sformatf("finalize store fault uid=%0d sta_wb=%0d std_wb=%0d sta_fault=%0d std_fault=%0d",
                            uid, status.sta_writeback, status.std_writeback,
                            status.sta_fault, status.std_fault), UVM_LOW)
        return 1'b1;
    endfunction:finalize_pending_store_fault

    function void finalize_pending_store_faults();
        memblock_uid_t uid;
        memblock_uid_t pending_uids[$];

        foreach (pending_store_fault_by_uid[uid]) begin
            pending_uids.push_back(uid);
        end
        foreach (pending_uids[idx]) begin
            uid = pending_uids[idx];
            if (!pending_store_fault_matches(uid)) begin
                void'(clear_pending_store_fault(uid));
                continue;
            end
            void'(finalize_pending_store_fault(uid));
        end
    endfunction:finalize_pending_store_faults

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
        if (((status.fault || status.exception_pending) &&
             !(target == MEMBLOCK_ISSUE_TARGET_STD && pending_store_fault_matches(uid))) ||
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
        if (required_targets_done(uid) && store_real_writebacks_done(uid) &&
            !pending_store_fault_matches(uid) && !status.fault &&
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
        if (!status.active || status.issue_killed || status.fault ||
            status.exception_pending || status.terminal_done ||
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

    // 中文注释：STA IQ hit=0 时冻结旧 issue identity，但不等待 raw；后续 recovery
    // 立即调用既有 mark_replay_pending()。只有 late fault 可以读取这个 history。
    function bit capture_sta_late_fault_tombstone(input memblock_uid_t uid,
                                                   input int unsigned issue_epoch,
                                                   input int unsigned replay_seq,
                                                   input longint unsigned cycle,
                                                   output bit created_new);
        status_transaction                    status;
        memblock_sta_late_fault_tombstone_t  tombstone;
        memblock_rob_key_t                    rob_key;
        memblock_sq_key_t                     sq_key;
        memblock_uid_t                        mapped_uid;
        int unsigned                          target_flush_epoch;

        created_new = 1'b0;
        status = get_status(uid);
        if (!status.active || status.issue_killed || status.fault ||
            status.exception_pending || status.redirect_pending || status.terminal_done ||
            !status.sta_dispatched || status.sta_writeback || status.sta_pass ||
            status.sta_fault || issue_epoch == 0 ||
            status.sta_issue_epoch != issue_epoch ||
            !target_replay_seq_match(status, MEMBLOCK_ISSUE_TARGET_STA, replay_seq)) begin
            return 1'b0;
        end
        if (!status.active_sq_mapped ||
            !status.get_target_instance_flush_epoch(MEMBLOCK_ISSUE_TARGET_STA,
                                                    target_flush_epoch)) begin
            `uvm_fatal("STA_LATE_TOMBSTONE",
                       $sformatf("current STA snapshot lacks SQ/flush identity uid=%0d", uid))
        end
        rob_key = status.get_rob_key();
        sq_key.flag = status.sqIdx_flag;
        sq_key.value = status.sqIdx_value;
        if (!is_valid_sq_key(sq_key) ||
            !lookup_active_uid_by_rob(rob_key, mapped_uid) || mapped_uid != uid ||
            !lookup_active_uid_by_sq(sq_key, mapped_uid) || mapped_uid != uid) begin
            `uvm_fatal("STA_LATE_TOMBSTONE",
                       $sformatf("current STA mapping mismatch uid=%0d rob=%0d/%0d sq=%0d/%0d",
                                 uid, rob_key.flag, rob_key.value, sq_key.flag, sq_key.value))
        end
        foreach (status.sta_late_fault_tombstone_q[idx]) begin
            tombstone = status.sta_late_fault_tombstone_q[idx];
            if (!tombstone.valid) begin
                `uvm_fatal("STA_LATE_TOMBSTONE", $sformatf("invalid tombstone uid=%0d idx=%0d", uid, idx))
            end
            if (tombstone.dynamic_epoch == status.dynamic_epoch &&
                tombstone.issue_epoch == issue_epoch &&
                tombstone.replay_seq == replay_seq) begin
                if (tombstone.rob_key != rob_key || tombstone.sq_key != sq_key ||
                    tombstone.target_flush_epoch != target_flush_epoch) begin
                    `uvm_fatal("STA_LATE_TOMBSTONE",
                               $sformatf("duplicate tombstone identity mismatch uid=%0d epoch=%0d replay=%0d",
                                         uid, issue_epoch, replay_seq))
                end
                return 1'b1;
            end
            // 中文注释：V2 scalar STA feedback 只携带 SQ 与 hit，TLB miss 和跨 16B
            // misalign replay 都可能持续报告 hit=0。对于同一动态实例、ROB 和 SQ，
            // late-fault reader 本来只选择最早 record；后续 replay 不能再占用 history 槽位。
            // 当前 target flush epoch 可以因无关年轻 redirect 前进，但不得倒退到首条
            // record 之前，否则说明 active STA snapshot 生命周期发生了错误回退。
            if (tombstone.dynamic_epoch == status.dynamic_epoch &&
                tombstone.rob_key == rob_key && tombstone.sq_key == sq_key) begin
                if (target_flush_epoch < tombstone.target_flush_epoch) begin
                    `uvm_fatal("STA_LATE_TOMBSTONE",
                               $sformatf("coalesced tombstone flush epoch regressed uid=%0d retained=%0d current=%0d",
                                         uid, tombstone.target_flush_epoch, target_flush_epoch))
                end
                `uvm_info("STA_LATE_TOMBSTONE",
                          $sformatf("coalesce uid=%0d dynamic_epoch=%0d retained_issue_epoch=%0d retained_replay_seq=%0d current_issue_epoch=%0d current_replay_seq=%0d history=%0d",
                                    uid, status.dynamic_epoch,
                                    tombstone.issue_epoch, tombstone.replay_seq,
                                    issue_epoch, replay_seq,
                                    status.sta_late_fault_tombstone_q.size()),
                          UVM_LOW)
                return 1'b1;
            end
        end
        if (status.sta_late_fault_tombstone_q.size() >=
            MEMBLOCK_STA_LATE_FAULT_TOMBSTONE_MAX) begin
            `uvm_fatal("STA_LATE_TOMBSTONE",
                       $sformatf("tombstone history overflow uid=%0d dynamic_epoch=%0d size=%0d max=%0d",
                                 uid, status.dynamic_epoch,
                                 status.sta_late_fault_tombstone_q.size(),
                                 MEMBLOCK_STA_LATE_FAULT_TOMBSTONE_MAX))
        end
        tombstone = '{default:'0};
        tombstone.valid = 1'b1;
        tombstone.rob_key = rob_key;
        tombstone.sq_key = sq_key;
        tombstone.issue_epoch = issue_epoch;
        tombstone.replay_seq = replay_seq;
        tombstone.dynamic_epoch = status.dynamic_epoch;
        tombstone.target_flush_epoch = target_flush_epoch;
        tombstone.create_cycle = cycle;
        status.sta_late_fault_tombstone_q.push_back(tombstone);
        created_new = 1'b1;
        status.last_event_cycle = cycle;
        `uvm_info("STA_LATE_TOMBSTONE",
                  $sformatf("capture uid=%0d dynamic_epoch=%0d issue_epoch=%0d replay_seq=%0d flush_epoch=%0d history=%0d",
                            uid, tombstone.dynamic_epoch, issue_epoch, replay_seq,
                            target_flush_epoch, status.sta_late_fault_tombstone_q.size()),
                  UVM_LOW)
        return 1'b1;
    endfunction:capture_sta_late_fault_tombstone

    // 中文注释：adapter 已以 raw ROB 找到 active UID 后，只读取该 UID 的有界 history。
    // raw 不带 SQ/epoch/generation，故以 active status 的 SQ/dynamic identity 和 flush 下界复核。
    function bit read_sta_late_fault_tombstone(
        input memblock_uid_t uid,
        input memblock_rob_key_t raw_rob_key,
        input int unsigned raw_sample_flush_epoch,
        output memblock_sta_late_fault_tombstone_t tombstone
    );
        status_transaction                    status;
        memblock_sta_late_fault_tombstone_t  candidate;
        memblock_sq_key_t                     sq_key;
        memblock_uid_t                        mapped_uid;
        bit                                   found;

        tombstone = '{default:'0};
        status = get_status(uid);
        if (raw_sample_flush_epoch > memblock_sync_pkg::dispatch_flush_epoch) begin
            `uvm_fatal("STA_LATE_TOMBSTONE",
                       $sformatf("late fault sample epoch is from future uid=%0d sample=%0d current=%0d",
                                 uid, raw_sample_flush_epoch,
                                 memblock_sync_pkg::dispatch_flush_epoch))
        end
        if (!status.active || status.fault || status.exception_pending ||
            status.terminal_done || status.redirect_pending || status.flushed ||
            !status.active_sq_mapped || status.get_rob_key() != raw_rob_key) begin
            return 1'b0;
        end
        sq_key.flag = status.sqIdx_flag;
        sq_key.value = status.sqIdx_value;
        if (!is_valid_sq_key(sq_key) ||
            !lookup_active_uid_by_rob(raw_rob_key, mapped_uid) || mapped_uid != uid ||
            !lookup_active_uid_by_sq(sq_key, mapped_uid) || mapped_uid != uid) begin
            `uvm_fatal("STA_LATE_TOMBSTONE",
                       $sformatf("late fault active mapping mismatch uid=%0d rob=%0d/%0d sq=%0d/%0d",
                                 uid, raw_rob_key.flag, raw_rob_key.value,
                                 sq_key.flag, sq_key.value))
        end
        found = 1'b0;
        foreach (status.sta_late_fault_tombstone_q[idx]) begin
            candidate = status.sta_late_fault_tombstone_q[idx];
            if (!candidate.valid) begin
                `uvm_fatal("STA_LATE_TOMBSTONE", $sformatf("invalid tombstone uid=%0d idx=%0d", uid, idx))
            end
            if (candidate.dynamic_epoch != status.dynamic_epoch ||
                candidate.rob_key != raw_rob_key || candidate.sq_key != sq_key) begin
                continue;
            end
            if (raw_sample_flush_epoch < candidate.target_flush_epoch) begin
                continue;
            end
            if (!found || candidate.create_cycle < tombstone.create_cycle) begin
                tombstone = candidate;
                found = 1'b1;
            end
        end
        return found;
    endfunction:read_sta_late_fault_tombstone

    // 中文注释：该 helper 只由 mark_target_fault() 调用。history 命中后将同 UID
    // 终止为 fault，撤销待发射/待 PTW replay；不直接 retire active map。
    function bit mark_sta_late_fault_from_tombstone(input memblock_uid_t uid,
                                                     input int unsigned issue_epoch,
                                                     input int unsigned replay_seq,
                                                     input bit [23:0] exception_vec,
                                                     input longint unsigned cycle);
        status_transaction                    status;
        memblock_sta_late_fault_tombstone_t  tombstone;
        memblock_rob_key_t                    rob_key;
        memblock_sq_key_t                     sq_key;
        bit                                   found;

        status = get_status(uid);
        if (!status.active || status.fault || status.exception_pending ||
            status.terminal_done || status.redirect_pending || status.flushed ||
            !status.active_sq_mapped) begin
            return 1'b0;
        end
        rob_key = status.get_rob_key();
        sq_key.flag = status.sqIdx_flag;
        sq_key.value = status.sqIdx_value;
        found = 1'b0;
        foreach (status.sta_late_fault_tombstone_q[idx]) begin
            tombstone = status.sta_late_fault_tombstone_q[idx];
            if (tombstone.valid && tombstone.dynamic_epoch == status.dynamic_epoch &&
                tombstone.issue_epoch == issue_epoch && tombstone.replay_seq == replay_seq &&
                tombstone.rob_key == rob_key && tombstone.sq_key == sq_key) begin
                found = 1'b1;
                break;
            end
        end
        if (!found) begin
            return 1'b0;
        end
        remove_uid_from_issue_queues(uid);
        release_ptw_wait_replay(uid);
        status.replay_pending = 1'b0;
        status.replay_target_load = 1'b0;
        status.replay_target_sta = 1'b0;
        status.replay_target_std = 1'b0;
        status.sta_dispatched = 1'b0;
        status.sta_writeback = 1'b1;
        status.sta_issue_feedback_success = 1'b0;
        status.sta_pass = 1'b0;
        // target-level late fault 只作为 pending record 的证据，UID 级 fault 由 finalize
        // helper 在 STA/STD 双 real-WB 屏障满足后统一设置。
        status.sta_fault = 1'b0;
        status.writeback = 1'b0;
        status.pass = 1'b0;
        status.success = 1'b0;
        status.terminal_done = 1'b0;
        status.fault = 1'b0;
        status.exception_pending = 1'b0;
        status.exception_vec = '0;
        status.clear_sta_late_fault_tombstones();
        status.last_event_cycle = cycle;
        if (!capture_pending_store_fault(uid, MEMBLOCK_ISSUE_TARGET_STA,
                                         issue_epoch, replay_seq, exception_vec, cycle)) begin
            return 1'b0;
        end
        void'(finalize_pending_store_fault(uid));
        `uvm_info("STA_LATE_TOMBSTONE",
                  $sformatf("consume late fault uid=%0d issue_epoch=%0d replay_seq=%0d exception_vec=0x%0h",
                            uid, issue_epoch, replay_seq, exception_vec),
                  UVM_LOW)
        return 1'b1;
    endfunction:mark_sta_late_fault_from_tombstone

    function bit mark_target_fault(input memblock_uid_t uid,
                                   input memblock_issue_target_e target,
                                   input int unsigned issue_epoch,
                                   input int unsigned replay_seq,
                                   input bit [23:0] exception_vec,
                                   input longint unsigned cycle);
        status_transaction status;

        status = get_status(uid);
        if (status.fault || status.exception_pending || status.terminal_done) begin
            `uvm_info("WB_STATUS",
                      $sformatf("drop duplicate fault uid=%0d target=%0d exception_vec=0x%0h",
                                uid, target, exception_vec),
                      UVM_LOW)
            return 1'b0;
        end
        if (target == MEMBLOCK_ISSUE_TARGET_STA &&
            mark_sta_late_fault_from_tombstone(uid, issue_epoch, replay_seq,
                                                exception_vec, cycle)) begin
            return 1'b1;
        end
        if (!conditional_set_target_status_field(uid, target_writeback_field(target), 1'b1, target, issue_epoch, replay_seq)) begin
            return 1'b0;
        end
        status = get_status(uid);
        if (is_scalar_store_uid(uid) &&
            (target == MEMBLOCK_ISSUE_TARGET_STA || target == MEMBLOCK_ISSUE_TARGET_STD)) begin
            if (!capture_pending_store_fault(uid, target, issue_epoch, replay_seq,
                                             exception_vec, cycle)) begin
                return 1'b0;
            end
            // 中文注释：target fault/writeback 已记录，但 UID 级 fault 必须等两个
            // real-WB bit 都成立后再 finalize，避免 STA-only 或 STD-only 异常提前进入 commit。
            void'(finalize_pending_store_fault(uid));
            return 1'b1;
        end
        if (!conditional_set_target_status_field(uid, target_fault_field(target), 1'b1, target, issue_epoch, replay_seq)) begin
            return 1'b0;
        end
        status.fault             = 1'b1;
        status.exception_pending = 1'b1;
        status.exception_vec     = exception_vec;
        status.pass              = 1'b0;
        status.success           = 1'b0;
        status.terminal_done     = 1'b0;
        quiesce_fault_uid_pending_work(uid, 1'b0);
        if (target == MEMBLOCK_ISSUE_TARGET_STA) begin
            release_ptw_wait_replay(uid);
            status.clear_sta_late_fault_tombstones();
        end
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
        // 中文注释：late fault 或其它终态已经锁定 UID 结果时，队列中可能仍有同一
        // UID 的旧 feedback。直接过滤，避免 stale replay 再次进入 issue queue。
        if (!status.active || status.issue_killed || status.fault ||
            status.exception_pending || status.terminal_done ||
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

    function bit clear_uid_dispatch_result(input memblock_uid_t uid);
        status_transaction status;
        bit                dropped_pending_draft;

        status = get_status(uid);
        // 草稿删除只服务旧动态实例作废。finalize 直接调用
        // clear_pending_store_fault()，不经过这里，因此不会打出 reissue 日志。
        dropped_pending_draft = clear_pending_store_fault(uid);
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
        status.clear_sta_late_fault_tombstones();
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
        return dropped_pending_draft;
    endfunction:clear_uid_dispatch_result

    function void clear_uncache_retry_boost(input memblock_uid_t uid,
                                            input string reason);
        status_transaction status;

        check_uid(uid, "clear_uncache_retry_boost");
        status = get_status(uid);
        if (status.uncache_retry_boost || status.uncache_retry_count != 0 ||
            status.uncache_retry_identity_valid) begin
            `uvm_info("UNCACHE_RETRY_BOOST",
                      $sformatf("clear uid=%0d reason=%s count=%0d dyn=%0d rob=%0d/%0d",
                                uid, reason, status.uncache_retry_count,
                                status.uncache_retry_dynamic_epoch,
                                status.uncache_retry_rob_key.flag,
                                status.uncache_retry_rob_key.value), UVM_LOW)
        end
        status.uncache_retry_boost = 1'b0;
        status.uncache_retry_count = 0;
        status.uncache_retry_identity_valid = 1'b0;
        status.uncache_retry_rob_key = '{default:'0};
        status.uncache_retry_has_dynamic_epoch = 1'b0;
        status.uncache_retry_dynamic_epoch = 0;
    endfunction:clear_uncache_retry_boost

    function bit apply_uncache_retry_boost(input memblock_wb_event_t wb_event);
        status_transaction status;
        memblock_rob_key_t status_rob_key;

        if (!wb_event.valid ||
            wb_event.source != MEMBLOCK_WB_EVENT_SOURCE_MEMORY_VIOLATION ||
            wb_event.rollback_subtype != MEMBLOCK_ROLLBACK_SUBTYPE_UNCACHE_NACK ||
            !wb_event.redirect.valid || !wb_event.redirect.flush_itself ||
            wb_event.redirect.is_vls_exception ||
            !wb_event.has_uid || !wb_event.has_rob || !wb_event.has_dynamic_epoch ||
            !is_valid_uid(wb_event.uid)) begin
            return 1'b0;
        end
        status = get_status(wb_event.uid);
        status_rob_key = status.get_rob_key();
        if (!status.active || status.terminal_done || status.flushed ||
            status.redirect_pending || status_rob_key != wb_event.rob_key ||
            status.dynamic_epoch != wb_event.dynamic_epoch) begin
            `uvm_info("UNCACHE_RETRY_BOOST",
                      $sformatf("reject stale selected uid=%0d event_dyn=%0d status_dyn=%0d event_rob=%0d/%0d status_rob=%0d/%0d active=%0d terminal=%0d flushed=%0d redirect_pending=%0d",
                                wb_event.uid, wb_event.dynamic_epoch, status.dynamic_epoch,
                                wb_event.rob_key.flag, wb_event.rob_key.value,
                                status_rob_key.flag, status_rob_key.value,
                                status.active, status.terminal_done, status.flushed,
                                status.redirect_pending), UVM_LOW)
            return 1'b0;
        end
        status.uncache_retry_boost = 1'b1;
        status.uncache_retry_count++;
        if (status.uncache_retry_count == 0) begin
            `uvm_fatal("UNCACHE_RETRY_BOOST", "uncache retry count wrapped")
        end
        status.uncache_retry_identity_valid = 1'b1;
        status.uncache_retry_rob_key = wb_event.rob_key;
        status.uncache_retry_has_dynamic_epoch = 1'b1;
        status.uncache_retry_dynamic_epoch = wb_event.dynamic_epoch;
        `uvm_info("UNCACHE_RETRY_BOOST",
                  $sformatf("set uid=%0d count=%0d dyn=%0d rob=%0d/%0d subtype=%0d flush_itself=%0d",
                            wb_event.uid, status.uncache_retry_count,
                            wb_event.dynamic_epoch, wb_event.rob_key.flag,
                            wb_event.rob_key.value, wb_event.rollback_subtype,
                            wb_event.redirect.flush_itself), UVM_LOW)
        return 1'b1;
    endfunction:apply_uncache_retry_boost

    function bit consume_uncache_retry_boost_after_issue(
        input memblock_issue_q_item_t item
    );
        status_transaction status;

        if (!item.uncache_retry_boost || !item.uncache_retry_identity_valid ||
            !item.uncache_retry_has_dynamic_epoch ||
            !is_valid_uid(item.uid)) begin
            return 1'b0;
        end
        status = get_status(item.uid);
        if (!status.uncache_retry_boost ||
            !status.uncache_retry_identity_valid ||
            !status.uncache_retry_has_dynamic_epoch ||
            status.uncache_retry_rob_key != item.rob_key ||
            status.uncache_retry_dynamic_epoch != item.dynamic_epoch ||
            item.uncache_retry_dynamic_epoch != item.dynamic_epoch) begin
            return 1'b0;
        end
        `uvm_info("UNCACHE_RETRY_BOOST",
                  $sformatf("consume after issue uid=%0d target=%0d count=%0d dyn=%0d rob=%0d/%0d",
                            item.uid, item.target, status.uncache_retry_count,
                            item.dynamic_epoch, item.rob_key.flag,
                            item.rob_key.value), UVM_LOW)
        // Keep retry_count as a cumulative diagnostic until terminal/reset.
        // A subsequent selected subtype5 event explicitly re-arms the token.
        status.uncache_retry_boost = 1'b0;
        status.uncache_retry_identity_valid = 1'b0;
        status.uncache_retry_rob_key = '{default:'0};
        status.uncache_retry_has_dynamic_epoch = 1'b0;
        status.uncache_retry_dynamic_epoch = 0;
        return 1'b1;
    endfunction:consume_uncache_retry_boost_after_issue

    function void inherit_uncache_retry_boost_for_reissue(
        input memblock_uid_t uid,
        input memblock_rob_key_t old_rob_key,
        input int unsigned old_dynamic_epoch,
        input int unsigned new_dynamic_epoch
    );
        status_transaction status;

        check_uid(uid, "inherit_uncache_retry_boost_for_reissue");
        status = get_status(uid);
        if (!status.uncache_retry_boost ||
            !status.uncache_retry_identity_valid ||
            status.uncache_retry_rob_key != old_rob_key ||
            status.uncache_retry_dynamic_epoch != old_dynamic_epoch ||
            status.get_rob_key() != old_rob_key ||
            new_dynamic_epoch != old_dynamic_epoch + 1) begin
            if (status.uncache_retry_boost || status.uncache_retry_identity_valid) begin
                clear_uncache_retry_boost(uid, "stale redirect reissue identity");
            end
            return;
        end
        status.uncache_retry_dynamic_epoch = new_dynamic_epoch;
        status.uncache_retry_has_dynamic_epoch = 1'b1;
        `uvm_info("UNCACHE_RETRY_BOOST",
                  $sformatf("inherit uid=%0d count=%0d E=%0d->E+1=%0d rob=%0d/%0d",
                            uid, status.uncache_retry_count, old_dynamic_epoch,
                            new_dynamic_epoch, old_rob_key.flag, old_rob_key.value), UVM_LOW)
    endfunction:inherit_uncache_retry_boost_for_reissue

    function void reset_uid_after_older_redirect(input memblock_uid_t uid);
        status_transaction status;
        memblock_rob_key_t old_rob_key;
        int unsigned old_dynamic_epoch;

        status = get_status(uid);
        old_rob_key = status.get_rob_key();
        old_dynamic_epoch = status.dynamic_epoch;
        if (status.rob_commit || status.terminal_done) begin
            `uvm_fatal("REDIRECT_REISSUE",
                       $sformatf("reset pending-fault uid=%0d with rob_commit=%0d terminal_done=%0d",
                                 uid, status.rob_commit, status.terminal_done))
        end
        if (pending_store_fault_by_uid.exists(uid)) begin
            pending_store_fault_by_uid.delete(uid);
        end
        void'(clear_uid_dispatch_result(uid));
        status.first_split_access = '{default:'0};
        status.fault           = 1'b0;
        status.exception_pending = 1'b0;
        status.sta_fault       = 1'b0;
        status.std_fault       = 1'b0;
        status.exception_vec   = '0;
        status.redirect_pending = 1'b1;
        status.flushed          = 1'b1;
        status.dynamic_epoch++;
        status.active           = 1'b0;
        status.success         = 1'b0;
        status.terminal_done   = 1'b0;
        inherit_uncache_retry_boost_for_reissue(uid, old_rob_key,
                                                old_dynamic_epoch,
                                                status.dynamic_epoch);
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        `uvm_info("REDIRECT_REISSUE",
                  $sformatf("reset older-redirect pending-fault uid=%0d new_dynamic_epoch=%0d; next admission remains uid=%0d",
                            uid, status.dynamic_epoch, uid), UVM_LOW)
    endfunction:reset_uid_after_older_redirect

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

    function bit fault_redirect_resource_matches(input memblock_uid_t uid,
                                                 input int unsigned dynamic_epoch);
        return fault_redirect_resource.valid &&
               fault_redirect_resource.uid == uid &&
               fault_redirect_resource.dynamic_epoch == dynamic_epoch;
    endfunction:fault_redirect_resource_matches

    function bit fault_redirect_resource_terminal_ready(input memblock_uid_t uid);
        if (!fault_redirect_resource_matches(uid, get_status(uid).dynamic_epoch)) begin
            return 1'b0;
        end
        return fault_redirect_resource.disposition == MEMBLOCK_FAULT_LSQ_DISPOSITION_CANCEL_APPLIED ||
               fault_redirect_resource.disposition == MEMBLOCK_FAULT_LSQ_DISPOSITION_LATE_DEQ ||
               fault_redirect_resource.disposition == MEMBLOCK_FAULT_LSQ_DISPOSITION_SQ_DEQED;
    endfunction:fault_redirect_resource_terminal_ready

    function bit fault_redirect_resource_blocks_retire(input memblock_uid_t uid);
        return fault_redirect_resource_matches(uid, get_status(uid).dynamic_epoch) &&
               !fault_redirect_resource_terminal_ready(uid);
    endfunction:fault_redirect_resource_blocks_retire

    function bit fault_sq_candidate_snapshot_pending(input int unsigned redirect_epoch);
        return fault_redirect_resource.valid && !fault_redirect_resource.is_lq &&
               fault_redirect_resource.redirect_epoch == redirect_epoch &&
               fault_redirect_resource.disposition == MEMBLOCK_FAULT_LSQ_DISPOSITION_SQ_CANDIDATE;
    endfunction:fault_sq_candidate_snapshot_pending

    function void complete_fault_redirect_resource(input memblock_uid_t uid);
        if (!fault_redirect_resource_matches(uid, get_status(uid).dynamic_epoch)) begin
            `uvm_fatal("LSQ_CANCEL", $sformatf("fault resource completion mismatch uid=%0d", uid))
        end
        if (!fault_redirect_resource_terminal_ready(uid)) begin
            `uvm_fatal("LSQ_CANCEL", $sformatf("fault resource uid=%0d is not terminal disposition=%0d",
                                                  uid, fault_redirect_resource.disposition))
        end
        fault_redirect_resource = '{default:'0};
    endfunction:complete_fault_redirect_resource

    function void detach_fault_sq_mapping(input memblock_uid_t uid);
        status_transaction status;
        memblock_sq_key_t sq_key;
        memblock_sq_map_key_t sq_map_key;

        status = get_status(uid);
        if (!status.active_sq_mapped) begin
            return;
        end
        sq_key.flag = status.sqIdx_flag;
        sq_key.value = status.sqIdx_value;
        sq_map_key = rob_order_util::sq_to_map_key(sq_key);
        if (!uid_by_sq.exists(sq_map_key) || uid_by_sq[sq_map_key] != uid) begin
            `uvm_fatal("LSQ_CANCEL", $sformatf("fault SQ detach mapping mismatch uid=%0d", uid))
        end
        uid_by_sq.delete(sq_map_key);
        status.active_sq_mapped = 1'b0;
        status.lsq_deq = !status.active_lq_mapped && !status.active_sq_mapped;
    endfunction:detach_fault_sq_mapping

    function void arm_fault_redirect_resource_candidate(input memblock_uid_t uid,
                                                        input int unsigned redirect_epoch,
                                                        input int unsigned record_id);
        status_transaction status;
        if (fault_redirect_resource.valid) begin
            if (!fault_redirect_resource_matches(uid, get_status(uid).dynamic_epoch)) begin
                `uvm_fatal("LSQ_CANCEL", $sformatf("second fault resource candidate uid=%0d existing_uid=%0d",
                                                     uid, fault_redirect_resource.uid))
            end
            if (fault_redirect_resource.redirect_epoch != redirect_epoch ||
                fault_redirect_resource.cancel_record_id != record_id) begin
                `uvm_fatal("LSQ_CANCEL", $sformatf("fault resource uid=%0d changed cancel record", uid))
            end
            return;
        end
        status = get_status(uid);
        fault_redirect_resource = '{default:'0};
        fault_redirect_resource.valid = 1'b1;
        fault_redirect_resource.uid = uid;
        fault_redirect_resource.dynamic_epoch = status.dynamic_epoch;
        fault_redirect_resource.redirect_epoch = redirect_epoch;
        fault_redirect_resource.cancel_record_id = record_id;
        if (status.active_sq_mapped) begin
            fault_redirect_resource.is_lq = 1'b0;
            fault_redirect_resource.sq_key.flag = status.sqIdx_flag;
            fault_redirect_resource.sq_key.value = status.sqIdx_value;
            fault_redirect_resource.disposition = MEMBLOCK_FAULT_LSQ_DISPOSITION_SQ_CANDIDATE;
        end else if (status.active_lq_mapped) begin
            fault_redirect_resource.is_lq = 1'b1;
            fault_redirect_resource.lq_key.flag = status.lqIdx_flag;
            fault_redirect_resource.lq_key.value = status.lqIdx_value;
            fault_redirect_resource.disposition = MEMBLOCK_FAULT_LSQ_DISPOSITION_LQ_CANCEL_PENDING;
        end else begin
            fault_redirect_resource.valid = 1'b0;
        end
        if (fault_redirect_resource.valid) begin
            `uvm_info("LSQ_CANCEL",
                      $sformatf("arm fault resource uid=%0d epoch=%0d record=%0d side=%s key=%0d/%0d",
                                uid, status.dynamic_epoch, record_id,
                                fault_redirect_resource.is_lq ? "LQ" : "SQ",
                                fault_redirect_resource.is_lq ? fault_redirect_resource.lq_key.flag : fault_redirect_resource.sq_key.flag,
                                fault_redirect_resource.is_lq ? fault_redirect_resource.lq_key.value : fault_redirect_resource.sq_key.value),
                      UVM_LOW)
        end
    endfunction:arm_fault_redirect_resource_candidate

    function void resolve_fault_sq_candidate_from_snapshot(input int record_idx,
                                                            input memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot);
        int unsigned ordinary_sq_count;
        if (!fault_redirect_resource.valid || fault_redirect_resource.is_lq ||
            fault_redirect_resource.cancel_record_id != cancel_record_q[record_idx].cancel_record_id) begin
            return;
        end
        ordinary_sq_count = cancel_record_q[record_idx].software_cancel_sq_count;
        case (fault_redirect_resource.disposition)
            MEMBLOCK_FAULT_LSQ_DISPOSITION_SQ_CANDIDATE: begin
                if (snapshot.sq_cancel_count == ordinary_sq_count + 1) begin
                    cancel_record_q[record_idx].software_cancel_sq_count++;
                    pending_sq_cancel_count++;
                    detach_fault_sq_mapping(fault_redirect_resource.uid);
                    fault_redirect_resource.disposition =
                        MEMBLOCK_FAULT_LSQ_DISPOSITION_CANCEL_APPLIED_WAIT_RECORD_APPLY;
                    `uvm_info("LSQ_CANCEL",
                              $sformatf("fault SQ snapshot->cancel uid=%0d record=%0d ordinary=%0d dut=%0d",
                                        fault_redirect_resource.uid,
                                        fault_redirect_resource.cancel_record_id,
                                        ordinary_sq_count, snapshot.sq_cancel_count), UVM_LOW)
                end else if (snapshot.sq_cancel_count == ordinary_sq_count) begin
                    fault_redirect_resource.disposition = MEMBLOCK_FAULT_LSQ_DISPOSITION_WAIT_SQ_DEQ;
                    `uvm_info("LSQ_CANCEL",
                              $sformatf("fault SQ snapshot->wait_deq uid=%0d record=%0d ordinary=%0d",
                                        fault_redirect_resource.uid,
                                        fault_redirect_resource.cancel_record_id,
                                        ordinary_sq_count), UVM_LOW)
                end else begin
                    `uvm_fatal("LSQ_CANCEL_RECONCILE",
                               $sformatf("fault SQ candidate delta invalid record=%0d ordinary=%0d dut=%0d",
                                         cancel_record_q[record_idx].cancel_record_id,
                                         ordinary_sq_count, snapshot.sq_cancel_count))
                end
            end
            MEMBLOCK_FAULT_LSQ_DISPOSITION_SQ_DEQED,
            MEMBLOCK_FAULT_LSQ_DISPOSITION_WAIT_SQ_DEQ: begin
                if (snapshot.sq_cancel_count != ordinary_sq_count) begin
                    `uvm_fatal("LSQ_CANCEL_RECONCILE",
                               $sformatf("fault SQ deq path has unexpected cancel record=%0d ordinary=%0d dut=%0d",
                                         cancel_record_q[record_idx].cancel_record_id,
                                         ordinary_sq_count, snapshot.sq_cancel_count))
                end
            end
            MEMBLOCK_FAULT_LSQ_DISPOSITION_CANCEL_APPLIED_WAIT_RECORD_APPLY,
            MEMBLOCK_FAULT_LSQ_DISPOSITION_CANCEL_APPLIED: begin
                if (snapshot.sq_cancel_count != ordinary_sq_count) begin
                    `uvm_fatal("LSQ_CANCEL_RECONCILE",
                               $sformatf("fault SQ cancel path changed after attribution record=%0d ordinary=%0d dut=%0d",
                                         cancel_record_q[record_idx].cancel_record_id,
                                         ordinary_sq_count, snapshot.sq_cancel_count))
                end
            end
            default: begin
                `uvm_fatal("LSQ_CANCEL_RECONCILE", "invalid fault SQ candidate disposition")
            end
        endcase
    endfunction:resolve_fault_sq_candidate_from_snapshot

    function void note_fault_sq_deq(input memblock_uid_t uid,
                                    input memblock_sq_key_t key);
        if (!fault_redirect_resource.valid || fault_redirect_resource.is_lq ||
            fault_redirect_resource.uid != uid || fault_redirect_resource.sq_key != key) begin
            return;
        end
        if (fault_redirect_resource.disposition == MEMBLOCK_FAULT_LSQ_DISPOSITION_CANCEL_APPLIED ||
            fault_redirect_resource.disposition == MEMBLOCK_FAULT_LSQ_DISPOSITION_CANCEL_APPLIED_WAIT_RECORD_APPLY) begin
            `uvm_fatal("LSQ_CANCEL", $sformatf("sqDeq arrived after fault SQ cancel uid=%0d", uid))
        end
        fault_redirect_resource.disposition = MEMBLOCK_FAULT_LSQ_DISPOSITION_SQ_DEQED;
        `uvm_info("LSQ_CANCEL", $sformatf("fault SQ deq uid=%0d", uid), UVM_LOW)
    endfunction:note_fault_sq_deq

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
            if (fault_redirect_resource.valid &&
                fault_redirect_resource.cancel_record_id == cancel_record_q[0].cancel_record_id) begin
                break;
            end
            // 中文注释：窗口 service 仍需用关联 record 判断 cancel apply 完成。
            // 在临时 owner 清理前禁止 reconcile 提前 pop，避免跨 sequence 查空。
            if (redirect_deleted_owner_window_active &&
                cancel_record_q[0].cancel_record_id ==
                    redirect_deleted_owner_record_id) begin
                break;
            end
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
                // redirect anchor；仅在存在未锚定 record 时保留。已 observed 的 pinned
                // fault record 不能阻止后续周期性 baseline snapshot 被消费。
                if (find_oldest_unanchored_cancel_record_index() >= 0) begin
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
            resolve_fault_sq_candidate_from_snapshot(record_idx, snapshot);
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
        if (fault_redirect_resource.valid &&
            fault_redirect_resource.cancel_record_id == cancel_record_q[record_idx].cancel_record_id &&
            fault_redirect_resource.disposition ==
                MEMBLOCK_FAULT_LSQ_DISPOSITION_CANCEL_APPLIED_WAIT_RECORD_APPLY) begin
            fault_redirect_resource.disposition = MEMBLOCK_FAULT_LSQ_DISPOSITION_CANCEL_APPLIED;
        end
        if (fault_redirect_resource.valid &&
            fault_redirect_resource.cancel_record_id == cancel_record_q[record_idx].cancel_record_id &&
            fault_redirect_resource.disposition ==
                MEMBLOCK_FAULT_LSQ_DISPOSITION_LQ_CANCEL_PENDING) begin
            fault_redirect_resource.disposition = MEMBLOCK_FAULT_LSQ_DISPOSITION_CANCEL_APPLIED;
        end
        check_cancel_pending_aggregate();
    endfunction:mark_cancel_record_applied

    function bit redirect_deleted_owner_window_pending();
        return redirect_deleted_owner_window_active;
    endfunction:redirect_deleted_owner_window_pending

    // 中文注释：redirect 删除 active map 前冻结旧动态实例的物理 LSQ 身份。
    // 本函数不修改 cancel count 或资源 pointer；后续 note/retire 保持原职责。
    function void capture_redirect_deleted_owner(input memblock_uid_t uid,
                                                  input int unsigned redirect_epoch);
        status_transaction status;
        int record_idx;
        memblock_redirect_deleted_owner_t owner;

        check_uid(uid, "capture_redirect_deleted_owner");
        if (redirect_deleted_owner_window_active) begin
            `uvm_fatal("REDIRECT_DELETED_OWNER",
                       "capture attempted while previous owner window is active")
        end
        record_idx = find_cancel_record_index(redirect_epoch);
        if (record_idx < 0 || !active_cancel_record_id_valid ||
            cancel_record_q[record_idx].cancel_record_id != active_cancel_record_id ||
            !cancel_record_q[record_idx].redirect_anchor_valid ||
            cancel_record_q[record_idx].software_count_finalized) begin
            `uvm_fatal("REDIRECT_DELETED_OWNER",
                       $sformatf("uid=%0d redirect epoch=%0d has no open anchored cancel record",
                                 uid, redirect_epoch))
        end
        status = get_status(uid);
        if (!status.active || !status.active_instance_flush_epoch_valid ||
            status.active_instance_flush_epoch >= redirect_epoch) begin
            `uvm_fatal("REDIRECT_DELETED_OWNER",
                       $sformatf("uid=%0d activation epoch valid/value=%0d/%0d is not older than redirect epoch=%0d",
                                 uid, status.active_instance_flush_epoch_valid,
                                 status.active_instance_flush_epoch, redirect_epoch))
        end
        owner = '{default:'0};
        owner.valid = 1'b1;
        owner.uid = uid;
        owner.old_dynamic_epoch = status.dynamic_epoch;
        owner.old_activation_flush_epoch = status.active_instance_flush_epoch;
        owner.redirect_epoch = redirect_epoch;
        owner.cancel_record_id = cancel_record_q[record_idx].cancel_record_id;
        if (status.active_lq_mapped) begin
            memblock_lq_key_t lq_key;
            memblock_lq_map_key_t map_key;

            lq_key.flag = status.lqIdx_flag;
            lq_key.value = status.lqIdx_value;
            map_key = rob_order_util::lq_to_map_key(lq_key);
            if (redirect_deleted_lq_owner_by_key.exists(map_key)) begin
                `uvm_fatal("REDIRECT_DELETED_OWNER",
                           $sformatf("duplicate redirect-deleted LQ key=%0d/%0d",
                                     lq_key.flag, lq_key.value))
            end
            redirect_deleted_lq_owner_by_key[map_key] = owner;
        end
        if (status.active_sq_mapped) begin
            memblock_sq_key_t sq_key;
            memblock_sq_map_key_t map_key;

            sq_key.flag = status.sqIdx_flag;
            sq_key.value = status.sqIdx_value;
            map_key = rob_order_util::sq_to_map_key(sq_key);
            if (redirect_deleted_sq_owner_by_key.exists(map_key)) begin
                `uvm_fatal("REDIRECT_DELETED_OWNER",
                           $sformatf("duplicate redirect-deleted SQ key=%0d/%0d",
                                     sq_key.flag, sq_key.value))
            end
            redirect_deleted_sq_owner_by_key[map_key] = owner;
        end
    endfunction:capture_redirect_deleted_owner

    function bit lookup_redirect_deleted_lq_owner(
        input memblock_lq_key_t key,
        output memblock_redirect_deleted_owner_t owner);
        memblock_lq_map_key_t map_key;

        owner = '{default:'0};
        if (!redirect_deleted_owner_window_active) begin
            return 1'b0;
        end
        map_key = rob_order_util::lq_to_map_key(key);
        if (!redirect_deleted_lq_owner_by_key.exists(map_key)) begin
            return 1'b0;
        end
        owner = redirect_deleted_lq_owner_by_key[map_key];
        return owner.valid &&
               owner.redirect_epoch == redirect_deleted_owner_epoch &&
               owner.cancel_record_id == redirect_deleted_owner_record_id;
    endfunction:lookup_redirect_deleted_lq_owner

    // 中文注释：只为 DUT 内仍存活的 LoadQueueUncache slot 提供单代 redirect
    // fallback。调用者必须先按完整 ROB 查询 active owner；仅 active lookup miss
    // 时才能使用本函数。函数只读当前 redirect-deleted LQ owner window，不消费记录。
    function bit find_unique_redirect_deleted_load_owner_by_rob(
        input memblock_rob_key_t rob_key,
        output memblock_redirect_deleted_owner_t matched_owner,
        output memblock_lq_key_t matched_lq_key,
        output int unsigned matched_count);
        memblock_lq_map_key_t map_key;

        matched_owner = '{default:'0};
        matched_lq_key = '{default:'0};
        matched_count = 0;
        if (!redirect_deleted_owner_window_active) begin
            return 1'b0;
        end
        foreach (redirect_deleted_lq_owner_by_key[map_key]) begin
            memblock_redirect_deleted_owner_t candidate_owner;
            main_control_transaction main_tr;
            status_transaction status;
            memblock_lq_key_t candidate_lq_key;

            candidate_owner = redirect_deleted_lq_owner_by_key[map_key];
            if (!candidate_owner.valid ||
                candidate_owner.redirect_epoch != redirect_deleted_owner_epoch ||
                candidate_owner.cancel_record_id != redirect_deleted_owner_record_id ||
                !is_valid_uid(candidate_owner.uid) ||
                main_table_by_uid[candidate_owner.uid] == null ||
                status_by_uid[candidate_owner.uid] == null) begin
                continue;
            end
            main_tr = main_table_by_uid[candidate_owner.uid];
            status = status_by_uid[candidate_owner.uid];
            candidate_lq_key = memblock_lq_key_t'(map_key);
            if (main_tr.get_rob_key() != rob_key ||
                main_tr.get_lq_key() != candidate_lq_key ||
                status.dynamic_epoch != candidate_owner.old_dynamic_epoch + 1) begin
                continue;
            end
            matched_count++;
            matched_owner = candidate_owner;
            matched_lq_key = candidate_lq_key;
        end
        return matched_count == 1;
    endfunction:find_unique_redirect_deleted_load_owner_by_rob

    function bit lookup_redirect_deleted_sq_owner(
        input memblock_sq_key_t key,
        output memblock_redirect_deleted_owner_t owner);
        memblock_sq_map_key_t map_key;

        owner = '{default:'0};
        if (!redirect_deleted_owner_window_active) begin
            return 1'b0;
        end
        map_key = rob_order_util::sq_to_map_key(key);
        if (!redirect_deleted_sq_owner_by_key.exists(map_key)) begin
            return 1'b0;
        end
        owner = redirect_deleted_sq_owner_by_key[map_key];
        return owner.valid &&
               owner.redirect_epoch == redirect_deleted_owner_epoch &&
               owner.cancel_record_id == redirect_deleted_owner_record_id;
    endfunction:lookup_redirect_deleted_sq_owner

    function void start_redirect_deleted_owner_window(input int record_idx);
        if (redirect_deleted_owner_window_active ||
            (redirect_deleted_lq_owner_by_key.num() == 0 &&
             redirect_deleted_sq_owner_by_key.num() == 0)) begin
            `uvm_fatal("REDIRECT_DELETED_OWNER",
                       "start window requires idle state and at least one saved owner")
        end
        if (record_idx < 0 || record_idx >= cancel_record_q.size() ||
            !cancel_record_q[record_idx].valid ||
            !cancel_record_q[record_idx].redirect_anchor_valid ||
            !cancel_record_q[record_idx].active_scan_done ||
            cancel_record_q[record_idx].software_count_finalized ||
            cancel_record_q[record_idx].software_applied) begin
            `uvm_fatal("REDIRECT_DELETED_OWNER",
                       "start window got an invalid cancel record lifecycle")
        end
        redirect_deleted_owner_window_active = 1'b1;
        redirect_deleted_owner_epoch = cancel_record_q[record_idx].redirect_epoch;
        redirect_deleted_owner_record_id = cancel_record_q[record_idx].cancel_record_id;
        redirect_deleted_owner_expire_sample =
            cancel_record_q[record_idx].redirect_sample_seq +
            MEMBLOCK_REDIRECT_DELETED_OWNER_HOLD_SAMPLES;
    endfunction:start_redirect_deleted_owner_window

    function bit preflight_redirect_deleted_deq(
        input memblock_lq_deq_apply_item_t lq_items[$],
        input memblock_sq_deq_apply_item_t sq_items[$],
        output int unsigned deleted_lq_count,
        output int unsigned deleted_sq_count);
        int record_idx;

        deleted_lq_count = 0;
        deleted_sq_count = 0;
        foreach (lq_items[idx]) begin
            if (lq_items[idx].owner_kind == MEMBLOCK_DEQ_OWNER_REDIRECT_DELETED) begin
                memblock_redirect_deleted_owner_t owner;
                if (!lookup_redirect_deleted_lq_owner(lq_items[idx].key, owner) ||
                    owner.uid != lq_items[idx].uid ||
                    owner.old_dynamic_epoch != lq_items[idx].old_dynamic_epoch ||
                    owner.redirect_epoch != lq_items[idx].redirect_epoch ||
                    owner.cancel_record_id != lq_items[idx].cancel_record_id) begin
                    return 1'b0;
                end
                deleted_lq_count++;
            end
        end
        foreach (sq_items[idx]) begin
            if (sq_items[idx].owner_kind == MEMBLOCK_DEQ_OWNER_REDIRECT_DELETED) begin
                memblock_redirect_deleted_owner_t owner;
                if (!lookup_redirect_deleted_sq_owner(sq_items[idx].key, owner) ||
                    owner.uid != sq_items[idx].uid ||
                    owner.old_dynamic_epoch != sq_items[idx].old_dynamic_epoch ||
                    owner.redirect_epoch != sq_items[idx].redirect_epoch ||
                    owner.cancel_record_id != sq_items[idx].cancel_record_id) begin
                    return 1'b0;
                end
                deleted_sq_count++;
            end
        end
        if (deleted_lq_count == 0 && deleted_sq_count == 0) begin
            return 1'b1;
        end
        if (!redirect_deleted_owner_window_active) begin
            return 1'b0;
        end
        record_idx = find_cancel_record_index_by_id(redirect_deleted_owner_record_id);
        if (record_idx < 0 ||
            cancel_record_q[record_idx].software_count_finalized ||
            cancel_record_q[record_idx].software_applied ||
            cancel_record_q[record_idx].software_cancel_lq_count < deleted_lq_count ||
            cancel_record_q[record_idx].software_cancel_sq_count < deleted_sq_count ||
            pending_lq_cancel_count < deleted_lq_count ||
            pending_sq_cancel_count < deleted_sq_count) begin
            return 1'b0;
        end
        return 1'b1;
    endfunction:preflight_redirect_deleted_deq

    // 中文注释：只在 LQ/SQ 联合预检和资源容量检查全部成功后提交。
    // 此阶段不重新选择 provenance，保证同一 raw 不发生部分成功。
    function void commit_redirect_deleted_deq(
        input memblock_lq_deq_apply_item_t lq_items[$],
        input memblock_sq_deq_apply_item_t sq_items[$],
        input int unsigned deleted_lq_count,
        input int unsigned deleted_sq_count);
        int record_idx;

        // Live fault SQ deq has already released its active map in the caller.
        // Record its provenance even when this raw does not include a deleted owner.
        foreach (sq_items[idx]) begin
            if (sq_items[idx].owner_kind == MEMBLOCK_DEQ_OWNER_LIVE) begin
                note_fault_sq_deq(sq_items[idx].uid, sq_items[idx].key);
            end
        end
        if (deleted_lq_count == 0 && deleted_sq_count == 0) begin
            return;
        end
        record_idx = find_cancel_record_index_by_id(redirect_deleted_owner_record_id);
        if (record_idx < 0 || cancel_record_q[record_idx].software_count_finalized ||
            cancel_record_q[record_idx].software_applied) begin
            `uvm_fatal("REDIRECT_DELETED_OWNER",
                       "commit redirect-deleted deq lost its provisional cancel record")
        end
        cancel_record_q[record_idx].software_cancel_lq_count -= deleted_lq_count;
        cancel_record_q[record_idx].software_cancel_sq_count -= deleted_sq_count;
        pending_lq_cancel_count -= deleted_lq_count;
        pending_sq_cancel_count -= deleted_sq_count;
        foreach (lq_items[idx]) begin
            if (lq_items[idx].owner_kind == MEMBLOCK_DEQ_OWNER_REDIRECT_DELETED) begin
                redirect_deleted_lq_owner_by_key.delete(
                    rob_order_util::lq_to_map_key(lq_items[idx].key));
                if (fault_redirect_resource.valid && fault_redirect_resource.is_lq &&
                    fault_redirect_resource.uid == lq_items[idx].uid &&
                    fault_redirect_resource.dynamic_epoch == lq_items[idx].old_dynamic_epoch &&
                    fault_redirect_resource.cancel_record_id == lq_items[idx].cancel_record_id &&
                    fault_redirect_resource.lq_key == lq_items[idx].key) begin
                    fault_redirect_resource.disposition = MEMBLOCK_FAULT_LSQ_DISPOSITION_LATE_DEQ;
                end
            end
        end
        foreach (sq_items[idx]) begin
            if (sq_items[idx].owner_kind == MEMBLOCK_DEQ_OWNER_REDIRECT_DELETED) begin
                redirect_deleted_sq_owner_by_key.delete(
                    rob_order_util::sq_to_map_key(sq_items[idx].key));
            end
        end
        check_cancel_pending_aggregate();
    endfunction:commit_redirect_deleted_deq

    function void service_redirect_deleted_owner_window(
        input longint unsigned current_sample,
        input bit deferred_deq_blocks);
        int record_idx;

        if (!redirect_deleted_owner_window_active) begin
            return;
        end
        record_idx = find_cancel_record_index_by_id(redirect_deleted_owner_record_id);
        if (record_idx < 0 || !cancel_record_q[record_idx].valid) begin
            `uvm_fatal("REDIRECT_DELETED_OWNER",
                       "active owner window lost its cancel record")
        end
        if (!cancel_record_q[record_idx].software_count_finalized &&
            current_sample >= redirect_deleted_owner_expire_sample) begin
            // 中文注释：只等待仍可能扣减本记录取消数的延迟出队；无关旧store
            // 等STD不能阻塞当前redirect冻结。依赖由唯一LSQ owner只读推导。
            if (deferred_deq_blocks) begin
                return;
            end
            cancel_record_q[record_idx].software_count_finalized = 1'b1;
            check_cancel_pending_aggregate();
        end
        if (!cancel_record_q[record_idx].software_count_finalized ||
            !cancel_record_q[record_idx].software_applied) begin
            return;
        end
        redirect_deleted_lq_owner_by_key.delete();
        redirect_deleted_sq_owner_by_key.delete();
        redirect_deleted_owner_window_active = 1'b0;
        redirect_deleted_owner_epoch = 0;
        redirect_deleted_owner_record_id = 0;
        redirect_deleted_owner_expire_sample = 0;
        cleanup_completed_cancel_records();
    endfunction:service_redirect_deleted_owner_window

    // Fault ROB head已经完成异常提交后仍可能持有RTL中的committed LQ/SQ entry。
    // Redirect只flush年轻实例；该head保持active owner，等待正常deq完成异常终态。

    function bit preserve_fault_uid_during_redirect(
        input memblock_uid_t uid,
        input memblock_redirect_payload_t redirect);
        status_transaction status;

        if (!precheck_uid_redirect_mutation(uid, redirect, 1'b1,
                                            "preserve_fault_uid_during_redirect")) begin
            return 1'b0;
        end
        status = status_by_uid[uid];
        if (!status.active || !status.rob_commit || status.terminal_done ||
            status.get_rob_key() != redirect.rob_key ||
            (!status.fault && !status.exception_pending &&
             !status.load_fault && !status.sta_fault && !status.std_fault)) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("uid=%0d is not the active committed fault at redirect head",
                                 uid))
            return 1'b0;
        end
        commit_preserve_fault_uid_during_redirect(uid, redirect);
        return 1'b1;
    endfunction:preserve_fault_uid_during_redirect

    function bit prepare_uid_for_redirect_reissue(input memblock_uid_t uid,
                                                   input memblock_redirect_payload_t redirect);
        status_transaction status;

        if (!precheck_uid_redirect_mutation(uid, redirect, 1'b0,
                                            "prepare_uid_for_redirect_reissue")) begin
            return 1'b0;
        end
        status = status_by_uid[uid];
        if (status.terminal_done) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("redirect tries to flush already terminal_done uid=%0d", uid))
            return 1'b0;
        end
        commit_uid_for_redirect_reissue(uid);
        return 1'b1;
    endfunction:prepare_uid_for_redirect_reissue

    function void request_redirect_flush(input memblock_redirect_payload_t redirect);
        if (!redirect.valid) begin
            `uvm_fatal("COMMON_DATA", "request_redirect_flush requires valid redirect")
        end
        if (redirect_deleted_owner_window_active ||
            redirect_deleted_lq_owner_by_key.num() != 0 ||
            redirect_deleted_sq_owner_by_key.num() != 0) begin
            `uvm_fatal("REDIRECT_DELETED_OWNER",
                       "new redirect attempted before single-generation owner window was cleared")
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

    function bit apply_redirect_flush_range(input memblock_redirect_payload_t redirect);
        memblock_uid_t begin_uid;
        memblock_uid_t end_uid;
        memblock_uid_t planned_terminal_done_uid;
        memblock_uid_t new_end_uid;
        memblock_uid_t oldest_flushed_uid;
        memblock_uid_t fault_head_uids[$];
        memblock_uid_t inactive_cleanup_uids[$];
        memblock_uid_t reissue_uids[$];
        bit found_flushed;
        bit preserve_observation_valid;
        memblock_uid_t preserve_observation_uid;
        bit establish_provenance;
        bit start_deleted_owner_window;
        int unsigned planned_cancel_lq_count;
        int unsigned planned_cancel_sq_count;
        int unsigned planned_deleted_lq_owner_count;
        int unsigned planned_deleted_sq_owner_count;
        int record_idx;

        if (!redirect.valid) begin
            `uvm_fatal("COMMON_DATA", "apply_redirect_flush_range requires valid redirect")
            return 1'b0;
        end
        if (!active_cancel_record_id_valid) begin
            `uvm_fatal("LSQ_CANCEL", "redirect flush scan has no active cancel record")
            return 1'b0;
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
            return 1'b0;
        end
        if (!audit_active_rob_window("apply_redirect_flush_range:before_scan")) begin
            return 1'b0;
        end
        if (!plan_terminal_done_uid_for_redirect_range(planned_terminal_done_uid)) begin
            return 1'b0;
        end
        begin_uid = planned_terminal_done_uid;
        end_uid   = get_active_scan_end_uid();
        found_flushed = 1'b0;
        preserve_observation_valid = 1'b0;
        preserve_observation_uid = 0;
        establish_provenance = 1'b0;
        planned_cancel_lq_count = cancel_record_q[record_idx].software_cancel_lq_count;
        planned_cancel_sq_count = cancel_record_q[record_idx].software_cancel_sq_count;
        planned_deleted_lq_owner_count = 0;
        planned_deleted_sq_owner_count = 0;

        // Phase 1: read-only plan/precheck.  No public state may change until
        // every UID, marker, post-prefix and cancel/window condition is approved.
        for (memblock_uid_t uid = begin_uid; uid < end_uid; uid++) begin
            status_transaction status;
            memblock_rob_key_t rob_key;
            bit terminal_fault_head;

            if (!is_valid_uid(uid) || status_by_uid[uid] == null ||
                main_table_by_uid[uid] == null) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("redirect scan references invalid uid=%0d", uid))
                return 1'b0;
            end
            status = status_by_uid[uid];
            terminal_fault_head = status.active && status.rob_commit &&
                                  !status.terminal_done &&
                                  status.get_rob_key() == redirect.rob_key &&
                                  (status.fault || status.exception_pending ||
                                   status.load_fault || status.sta_fault ||
                                   status.std_fault);
            if (terminal_fault_head) begin
                if (!precheck_uid_redirect_mutation(
                        uid, redirect, 1'b1,
                        "apply_redirect_flush_range:plan_fault_head")) begin
                    return 1'b0;
                end
                fault_head_uids.push_back(uid);
                if (status.active_lq_mapped) begin
                    planned_cancel_lq_count += main_table_by_uid[uid].numLsElem;
                    planned_deleted_lq_owner_count++;
                end
                continue;
            end
            if (status.terminal_done || (!status.active && !status.writeback && !status.pass)) begin
                inactive_cleanup_uids.push_back(uid);
                continue;
            end
            rob_key = status.get_rob_key();
            if (rob_order_util::rob_need_flush(rob_key, redirect)) begin
                if (uid_is_control_marker(uid)) begin
                    if (status.control_state ==
                        MEMBLOCK_CONTROL_STATE_WAIT_OLDER_ROB_COMMIT) begin
                        if (!preserve_static_control_marker_on_redirect(uid, redirect)) begin
                            return 1'b0;
                        end
                        if (preserve_observation_valid) begin
                            `uvm_fatal("CONTROL_REDIRECT",
                                       "redirect observed more than one static control marker")
                            return 1'b0;
                        end
                        preserve_observation_valid = 1'b1;
                        preserve_observation_uid = uid;
                        continue;
                    end
                    `uvm_fatal("CONTROL_REDIRECT",
                               $sformatf("redirect covers started control uid=%0d state=%0d; the barrier forbids this recovery",
                                         uid, status.control_state))
                    return 1'b0;
                end
                if (!found_flushed || uid < oldest_flushed_uid) begin
                    oldest_flushed_uid = uid;
                    found_flushed = 1'b1;
                end
                if (!precheck_uid_redirect_mutation(
                        uid, redirect, 1'b0,
                        "apply_redirect_flush_range:plan_reissue")) begin
                    return 1'b0;
                end
                reissue_uids.push_back(uid);
                if (status.active_lq_mapped) begin
                    planned_cancel_lq_count += main_table_by_uid[uid].numLsElem;
                    planned_deleted_lq_owner_count++;
                end
                if (status.active_sq_mapped) begin
                    planned_cancel_sq_count += main_table_by_uid[uid].numLsElem;
                    planned_deleted_sq_owner_count++;
                end
            end
        end
        if (found_flushed) begin
            if (!is_valid_uid(oldest_flushed_uid)) begin
                `uvm_fatal("COMMON_DATA", "redirect plan produced invalid rollback uid")
                return 1'b0;
            end
            new_end_uid = oldest_flushed_uid;
            if (!control_barrier_prefix_external_provenance_valid &&
                preserve_observation_valid &&
                preserve_observation_uid >= new_end_uid) begin
                if (!precheck_control_barrier_prefix_external_provenance(
                        preserve_observation_uid, redirect, end_uid, new_end_uid)) begin
                    return 1'b0;
                end
                establish_provenance = 1'b1;
            end
        end else begin
            new_end_uid = end_uid;
        end
        if (!precheck_redirect_post_state_invariant(
                redirect, new_end_uid, reissue_uids,
                establish_provenance, preserve_observation_uid)) begin
            return 1'b0;
        end
        if (planned_cancel_lq_count > MEMBLOCK_LQ_SIZE ||
            planned_cancel_sq_count > MEMBLOCK_SQ_SIZE) begin
            `uvm_fatal("LSQ_CANCEL", "planned software cancel count exceeds LSQ capacity")
            return 1'b0;
        end
        if (!check_cancel_pending_aggregate_checked(
                "apply_redirect_flush_range:before_finalize")) begin
            return 1'b0;
        end
        if (!precheck_cancel_pending_aggregate_after_redirect_plan(
                record_idx, planned_cancel_lq_count, planned_cancel_sq_count,
                "apply_redirect_flush_range:planned_aggregate")) begin
            return 1'b0;
        end
        start_deleted_owner_window = planned_deleted_lq_owner_count != 0 ||
                                     planned_deleted_sq_owner_count != 0;
        if (start_deleted_owner_window &&
            !precheck_start_redirect_deleted_owner_window_plan(
                record_idx, planned_deleted_lq_owner_count,
                planned_deleted_sq_owner_count)) begin
            return 1'b0;
        end
        if (!start_deleted_owner_window &&
            (planned_cancel_lq_count != cancel_record_q[record_idx].software_cancel_lq_count ||
             planned_cancel_sq_count != cancel_record_q[record_idx].software_cancel_sq_count)) begin
            `uvm_fatal("REDIRECT_DELETED_OWNER",
                       "redirect plan has cancel increments without saved deleted owners")
            return 1'b0;
        end

        // Phase 2: commit only.  Every helper below is mutation-only and has no
        // catchable report checks; all possible failures were approved above.
        dispatch_progress.terminal_done_uid = planned_terminal_done_uid;
        foreach (inactive_cleanup_uids[idx]) begin
            commit_remove_uid_from_issue_queues_for_redirect_range(
                inactive_cleanup_uids[idx]);
        end
        foreach (fault_head_uids[idx]) begin
            commit_preserve_fault_uid_during_redirect(fault_head_uids[idx], redirect);
        end
        foreach (reissue_uids[idx]) begin
            commit_uid_for_redirect_reissue(reissue_uids[idx]);
        end
        if (found_flushed) begin
            if (oldest_flushed_uid == 0) begin
                dispatch_progress.max_enqueued_uid = 0;
                dispatch_progress.max_enqueued_uid_valid = 1'b0;
            end else begin
                dispatch_progress.max_enqueued_uid = oldest_flushed_uid - 1;
                dispatch_progress.max_enqueued_uid_valid = 1'b1;
            end
            if (establish_provenance) begin
                commit_control_barrier_prefix_external_provenance(
                    preserve_observation_uid, redirect, end_uid, new_end_uid);
            end
        end
        cancel_record_q[record_idx].active_scan_done = 1'b1;
        cancel_record_q[record_idx].state_flush_applied_service_cycle =
            memblock_sync_pkg::get_dispatch_service_cycle();
        if (start_deleted_owner_window) begin
            commit_start_redirect_deleted_owner_window(record_idx);
        end else begin
            cancel_record_q[record_idx].software_count_finalized = 1'b1;
        end
        active_cancel_record_id_valid = 1'b0;
        active_cancel_record_id = 0;
        return 1'b1;
    endfunction:apply_redirect_flush_range

    function bit apply_redirect_flush(input memblock_redirect_payload_t redirect);
        if (!redirect.valid) begin
            `uvm_fatal("COMMON_DATA", "apply_redirect_flush requires valid redirect")
            return 1'b0;
        end
        if (!apply_redirect_flush_range(redirect)) begin
            return 1'b0;
        end
        clear_ptw_wait_replay_by_redirect(redirect);
        clear_redirect_drive_queue();
        redirect_phase = MEMBLOCK_REDIRECT_PHASE_STATE_FLUSH_APPLIED;
        flush_in_progress  = 1'b0;
        memblock_sync_pkg::dispatch_flush_in_progress = 1'b0;
        issue_freeze_ack   = 1'b0;
        active_redirect    = '{default:'0};
        redirect_phase     = MEMBLOCK_REDIRECT_PHASE_IDLE;
        return 1'b1;
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
        wb_event.rollback_diag_valid = 1'b0;
        wb_event.rollback_diag_complete = 1'b0;
        wb_event.rollback_selected_input = 0;
        wb_event.rollback_subtype = MEMBLOCK_ROLLBACK_SUBTYPE_NONE;
        wb_event.has_dynamic_epoch = 1'b0;
        wb_event.dynamic_epoch = 0;
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
        memblock_uid_t     rob_uid;
        memblock_uid_t     lq_uid;
        memblock_uid_t     sq_uid;
        status_transaction resolved_status;
        memblock_lq_key_t  resolved_lq_key;
        bit                use_dequeued_load_lq_identity;
        bit                have_uid;

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
            use_dequeued_load_lq_identity = 1'b0;
            if (have_uid && wb_event.target == MEMBLOCK_ISSUE_TARGET_LOAD) begin
                resolved_status = get_status(uid);
                resolved_lq_key.flag  = resolved_status.lqIdx_flag;
                resolved_lq_key.value = resolved_status.lqIdx_value;
                // 中文注释：Load WB 已先由 UID/ROB 归属后，真实 lqDeq 可以让该物理
                // LQ slot 被后续 UID 复用。此时 event LQ key 仅校验当前 UID 的历史 identity，
                // 不能再查询 uid_by_lq 并把新 owner 误判为这笔 WB 的 owner。
                use_dequeued_load_lq_identity = !resolved_status.active_lq_mapped &&
                                                 resolved_status.lsq_deq &&
                                                 resolved_lq_key.flag == wb_event.lq_key.flag &&
                                                 resolved_lq_key.value == wb_event.lq_key.value;
            end
            if (!use_dequeued_load_lq_identity) begin
                if (!lookup_active_uid_by_lq(wb_event.lq_key, lq_uid)) begin
                    return 1'b0;
                end
                if (have_uid && uid != lq_uid) begin
                    `uvm_fatal("COMMON_DATA", $sformatf("WB_UID_MISMATCH uid=%0d lq_uid=%0d", uid, lq_uid))
                end
                uid = lq_uid;
                have_uid = 1'b1;
            end
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
        if (!assert_uid_within_rob_admission_window(uid, "activate_uid")) begin
            return;
        end
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

    function void cancel_later_split_tlb_record_for_uid(
        input memblock_uid_t uid,
        input int unsigned dynamic_epoch,
        input string reason = "");
        memblock_later_split_tlb_record record;

        if (!later_split_tlb_record_by_uid.exists(uid) ||
            later_split_tlb_record_by_uid[uid] == null) begin
            return;
        end
        record = later_split_tlb_record_by_uid[uid];
        if (record.dynamic_epoch != dynamic_epoch ||
            record.state == memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_CANCELED) begin
            return;
        end
        record.state = memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_CANCELED;
        `uvm_info("LATER_SPLIT_TLB",
                  $sformatf("cancel uid=%0d dynamic_epoch=%0d vpn=0x%0h token=%0d reason=%s",
                            uid, dynamic_epoch, record.split_vpn,
                            record.response_token, reason), UVM_LOW)
    endfunction:cancel_later_split_tlb_record_for_uid

    function void cancel_all_later_split_tlb_records(input string reason = "");
        foreach (later_split_tlb_record_by_uid[uid]) begin
            memblock_later_split_tlb_record record;
            record = later_split_tlb_record_by_uid[uid];
            if (record != null &&
                record.state != memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_CANCELED) begin
                record.state = memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_CANCELED;
            end
        end
        later_split_tlb_record_by_uid.delete();
        `uvm_info("LATER_SPLIT_TLB",
                  $sformatf("clear all later-split records reason=%s", reason),
                  UVM_LOW)
    endfunction:cancel_all_later_split_tlb_records

    // 中文注释：只绑定mabuf真实请求，不生成翻译或改变调度；高页标记跨低段重放保持。
    function void observe_first_split_request_sample(
        input bit request_fire,
        input memblock_rob_key_t rob_key,
        input bit [63:0] vaddr,
        input longint unsigned sample_seq,
        input longint unsigned reset_epoch);
        memblock_uid_t uid;
        status_transaction status;
        main_control_transaction main_tr;
        memblock_first_split_access_context_t context_value;
        bit same_instance;

        if (!request_fire) return;
        if (sample_seq == 0 ||
            reset_epoch != memblock_sync_pkg::get_l2tlb_current_reset_epoch() ||
            !lookup_active_uid_by_rob(rob_key, uid)) begin
            `uvm_fatal("FIRST_SPLIT_ACCESS", "split request lacks current sample/reset/active ROB")
            return;
        end
        status = get_status(uid);
        main_tr = get_main_transaction(uid);
        if (main_tr.fuType != MEMBLOCK_FUTYPE_LDU ||
            !memblock_op_behavior_util::is_load_fuoptype(main_tr.fuOpType)) begin
            `uvm_fatal("FIRST_SPLIT_ACCESS", "split request is not a scalar Load")
            return;
        end
        context_value = status.first_split_access;
        same_instance = context_value.valid && context_value.uid == uid &&
                        context_value.dynamic_epoch == status.dynamic_epoch &&
                        context_value.reset_epoch == reset_epoch &&
                        context_value.rob_key == rob_key;
        if (vaddr[63:12] != main_tr.vaddr[63:12]) begin
            if (!same_instance) begin
                `uvm_fatal("FIRST_SPLIT_ACCESS", "later-page split lacks first-page context")
                return;
            end
            status.first_split_access.later_page_seen = 1'b1;
            return;
        end
        if (same_instance &&
            (context_value.response_sample_seq == 0 ||
             context_value.response_sample_seq >= sample_seq)) begin
            `uvm_fatal("FIRST_SPLIT_ACCESS",
                       $sformatf("overlapping low split uid=%0d epoch=%0d old_request=%0d old_response=%0d new_request=%0d",
                                 uid, status.dynamic_epoch, context_value.request_sample_seq,
                                 context_value.response_sample_seq, sample_seq))
            return;
        end
        context_value = '{default:'0};
        context_value.valid = 1'b1;
        context_value.uid = uid;
        context_value.dynamic_epoch = status.dynamic_epoch;
        context_value.reset_epoch = reset_epoch;
        context_value.rob_key = rob_key;
        context_value.original_vaddr = main_tr.vaddr;
        context_value.request_vaddr = vaddr;
        context_value.request_sample_seq = sample_seq;
        context_value.later_page_seen = same_instance && status.first_split_access.later_page_seen;
        status.first_split_access = context_value;
    endfunction:observe_first_split_request_sample

    // 中文注释：记录实际S2取数，不以response减拍猜时刻；仅同attempt能更新。
    function void observe_first_split_data_sample(
        input bit data_valid,
        input memblock_rob_key_t rob_key,
        input bit [63:0] vaddr,
        input bit cache_eligible,
        input longint unsigned sample_seq,
        input longint unsigned reset_epoch);
        memblock_uid_t uid;
        status_transaction status;
        memblock_first_split_access_context_t context_value;

        if (!data_valid || !lookup_active_uid_by_rob(rob_key, uid)) return;
        status = get_status(uid);
        context_value = status.first_split_access;
        if (!context_value.valid || context_value.uid != uid ||
            context_value.dynamic_epoch != status.dynamic_epoch ||
            context_value.reset_epoch != reset_epoch ||
            reset_epoch != memblock_sync_pkg::get_l2tlb_current_reset_epoch() ||
            context_value.rob_key != rob_key || context_value.request_vaddr != vaddr ||
            context_value.request_sample_seq >= sample_seq ||
            context_value.data_sample_seq >= sample_seq || context_value.response_sample_seq != 0) return;
        // LU内部fast replay可以不产生新的mabuf请求；响应前最后一次S2才是返回数据的取数点。
        status.first_split_access.data_sample_seq = sample_seq;
        status.first_split_access.data_cache_eligible = cache_eligible;
    endfunction:observe_first_split_data_sample

    // 中文注释：返回只提供身份/路径/重放证据，不解码DUT异常用于预测。
    function void observe_first_split_response_sample(
        input bit response_valid,
        input memblock_rob_key_t rob_key,
        input bit [63:0] vaddr,
        input bit nc,
        input bit mmio,
        input bit need_replay,
        input longint unsigned sample_seq,
        input longint unsigned reset_epoch);
        memblock_uid_t uid;
        status_transaction status;
        memblock_first_split_access_context_t context_value;

        if (!response_valid || !lookup_active_uid_by_rob(rob_key, uid)) return;
        status = get_status(uid);
        context_value = status.first_split_access;
        if (!context_value.valid || context_value.uid != uid ||
            context_value.dynamic_epoch != status.dynamic_epoch ||
            context_value.reset_epoch != reset_epoch ||
            reset_epoch != memblock_sync_pkg::get_l2tlb_current_reset_epoch() ||
            context_value.rob_key != rob_key || context_value.request_vaddr != vaddr ||
            context_value.data_sample_seq == 0 || context_value.data_sample_seq >= sample_seq ||
            context_value.response_sample_seq != 0) return;
        status.first_split_access.response_sample_seq = sample_seq;
        status.first_split_access.response_vaddr = vaddr;
        status.first_split_access.response_nc = nc;
        status.first_split_access.response_mmio = mmio;
        status.first_split_access.response_need_replay = need_replay;
    endfunction:observe_first_split_response_sample

    function void observe_later_split_tlb_sample(
        input bit split_fire,
        input memblock_rob_key_t split_rob_key,
        input bit [63:0] split_fullva,
        input bit lu1_tlb_valid,
        input bit lu1_frm_mabuf,
        input bit lu1_kill,
        input memblock_rob_key_t lu1_rob_key,
        input bit [63:0] lu1_fullva,
        input bit lu1_ptw_valid,
        input bit [37:0] lu1_ptw_vpn,
        input bit [1:0] lu1_ptw_s2xlate,
        input longint unsigned sample_seq);
        memblock_uid_t uid;
        status_transaction status;
        main_control_transaction main_tr;
        memblock_later_split_tlb_record record;
        memblock_uid_t candidate_uid;
        int unsigned candidate_count;
        bit split_is_later_page;
        bit lu1_is_later_page;
        bit retain_completed_owner;

        if (sample_seq == 0) begin
            `uvm_fatal("LATER_SPLIT_TLB", "sample sequence must be non-zero")
            return;
        end

        // Phase 1: a cross-page misaligned load emits both its original-page
        // split and its later-page split through this interface.  Only the
        // later-page request needs a second translation response, so the
        // original-page handshake is deliberately not a ledger owner.
        if (split_fire) begin
            retain_completed_owner = 1'b0;
            if (!lookup_active_uid_by_rob(split_rob_key, uid)) begin
                `uvm_fatal("LATER_SPLIT_TLB",
                           $sformatf("split fire has no active UID sample=%0d rob=%0d/%0d",
                                     sample_seq, split_rob_key.flag,
                                     split_rob_key.value))
                return;
            end
            status = get_status(uid);
            main_tr = get_main_transaction(uid);
            split_is_later_page =
                split_fullva[63:12] != main_tr.vaddr[63:12];
            if (split_is_later_page) begin
                if (lu1_tlb_valid && lu1_frm_mabuf && !lu1_kill &&
                    (split_rob_key != lu1_rob_key ||
                     split_fullva != lu1_fullva)) begin
                    `uvm_fatal("LATER_SPLIT_TLB",
                               $sformatf("same-sample split/LU1 binding conflict sample=%0d split_rob=%0d/%0d lu1_rob=%0d/%0d split_va=0x%0h lu1_va=0x%0h",
                                         sample_seq,
                                         split_rob_key.flag, split_rob_key.value,
                                         lu1_rob_key.flag, lu1_rob_key.value,
                                         split_fullva, lu1_fullva))
                    return;
                end
                if (later_split_tlb_record_by_uid.exists(uid) &&
                    later_split_tlb_record_by_uid[uid] != null) begin
                    record = later_split_tlb_record_by_uid[uid];
                    if (record.dynamic_epoch == status.dynamic_epoch &&
                        record.state != memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_CANCELED) begin
                        if (record.state inside {
                                memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_LU1_PTW,
                                memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_L2_TOKEN,
                                memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_RESPONSE} &&
                            record.rob_key == split_rob_key &&
                            record.split_vaddr == split_fullva &&
                            record.mabuf_response_sample_seq != 0 &&
                            record.mabuf_response_sample_seq >= record.lu1_request_sample_seq &&
                            record.mabuf_response_sample_seq < sample_seq &&
                            record.mabuf_response_need_replay &&
                            !record.mabuf_response_nc &&
                            !record.mabuf_response_mmio) begin
                            `uvm_info("LATER_SPLIT_TLB",
                                      $sformatf("restart exact later-page split after replay response uid=%0d epoch=%0d rob=%0d/%0d va=0x%0h response_sample=%0d replay_split_sample=%0d",
                                                uid, status.dynamic_epoch,
                                                split_rob_key.flag,
                                                split_rob_key.value,
                                                split_fullva,
                                                record.mabuf_response_sample_seq,
                                                sample_seq), UVM_LOW)
                        end else if (record.state == memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_COMPLETED &&
                            record.rob_key == split_rob_key &&
                            record.split_vaddr == split_fullva) begin
                            `uvm_info("LATER_SPLIT_TLB",
                                      $sformatf("retain completed later-page response across exact replay uid=%0d epoch=%0d rob=%0d/%0d va=0x%0h token=%0d response_sample=%0d replay_split_sample=%0d",
                                                uid, status.dynamic_epoch,
                                                split_rob_key.flag,
                                                split_rob_key.value,
                                                split_fullva,
                                                record.response_token,
                                                record.response_sample_seq,
                                                sample_seq), UVM_LOW)
                            retain_completed_owner = 1'b1;
                        end else begin
                            `uvm_fatal("LATER_SPLIT_TLB",
                                       $sformatf("new later-page split conflicts with existing owner uid=%0d epoch=%0d state=%0d old_rob=%0d/%0d new_rob=%0d/%0d old_va=0x%0h new_va=0x%0h",
                                                 uid, status.dynamic_epoch,
                                                 record.state,
                                                 record.rob_key.flag,
                                                 record.rob_key.value,
                                                 split_rob_key.flag,
                                                 split_rob_key.value,
                                                 record.split_vaddr,
                                                 split_fullva))
                            return;
                        end
                    end
                end
                if (!retain_completed_owner) begin
                    record = memblock_later_split_tlb_record::type_id::create(
                        $sformatf("later_split_uid_%0d_epoch_%0d", uid,
                                  status.dynamic_epoch));
                    if (record == null) begin
                        `uvm_fatal("LATER_SPLIT_TLB", "failed to allocate split owner")
                        return;
                    end
                    record.uid = uid;
                    record.dynamic_epoch = status.dynamic_epoch;
                    record.rob_key = split_rob_key;
                    record.split_vaddr = split_fullva;
                    record.split_vpn = split_fullva[63:12];
                    record.split_fire_sample_seq = sample_seq;
                    record.state = memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_LU1_REQUEST;
                    later_split_tlb_record_by_uid[uid] = record;
                    `uvm_info("LATER_SPLIT_TLB",
                              $sformatf("capture split owner uid=%0d epoch=%0d rob=%0d/%0d va=0x%0h sample=%0d",
                                        uid, status.dynamic_epoch,
                                        split_rob_key.flag, split_rob_key.value,
                                        split_fullva, sample_seq), UVM_LOW)
                end
            end
        end

        // Phase 2: only a non-killed LoadUnit1 request sourced from mabuf may
        // advance the owner.  A request without a prior split owner is ignored;
        // it is negative provenance, not a new owner source.
        if (lu1_tlb_valid && lu1_frm_mabuf && !lu1_kill) begin
            if (lookup_active_uid_by_rob(lu1_rob_key, uid)) begin
                main_tr = get_main_transaction(uid);
                lu1_is_later_page =
                    lu1_fullva[63:12] != main_tr.vaddr[63:12];
                if (lu1_is_later_page) begin
                    if (!later_split_tlb_record_by_uid.exists(uid) ||
                        later_split_tlb_record_by_uid[uid] == null) begin
                        `uvm_fatal("LATER_SPLIT_TLB",
                                   $sformatf("later-page LU1 frm_mabuf request has no prior split fire uid=%0d rob=%0d/%0d va=0x%0h sample=%0d",
                                             uid, lu1_rob_key.flag,
                                             lu1_rob_key.value, lu1_fullva,
                                             sample_seq))
                        return;
                    end
                    status = get_status(uid);
                    record = later_split_tlb_record_by_uid[uid];
                    if (record.dynamic_epoch == status.dynamic_epoch &&
                        record.state == memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_LU1_REQUEST) begin
                        if (record.rob_key != lu1_rob_key ||
                            record.split_vaddr != lu1_fullva) begin
                            `uvm_fatal("LATER_SPLIT_TLB",
                                       $sformatf("split/LU1 binding conflict uid=%0d epoch=%0d split_rob=%0d/%0d lu1_rob=%0d/%0d split_va=0x%0h lu1_va=0x%0h",
                                                 uid, status.dynamic_epoch,
                                                 record.rob_key.flag,
                                                 record.rob_key.value,
                                                 lu1_rob_key.flag,
                                                 lu1_rob_key.value,
                                                 record.split_vaddr, lu1_fullva))
                            return;
                        end
                        record.lu1_request_sample_seq = sample_seq;
                        record.state = memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_LU1_PTW;
                        `uvm_info("LATER_SPLIT_TLB",
                                  $sformatf("bind LU1 frm_mabuf uid=%0d epoch=%0d vpn=0x%0h sample=%0d",
                                            uid, record.dynamic_epoch,
                                            record.split_vpn, sample_seq), UVM_LOW)
                    end
                end
            end
        end

        // Phase 3: direct requestor-1 PTW evidence is matched only against
        // owners that already observed the exact LU1 frm_mabuf request.
        if (!lu1_ptw_valid) begin
            return;
        end
        candidate_count = 0;
        candidate_uid = 0;
        foreach (later_split_tlb_record_by_uid[scan_uid]) begin
            record = later_split_tlb_record_by_uid[scan_uid];
            if (record != null &&
                record.state == memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_LU1_REQUEST &&
                record.split_vpn[37:0] == lu1_ptw_vpn &&
                status_by_uid[scan_uid] != null &&
                status_by_uid[scan_uid].active &&
                status_by_uid[scan_uid].dynamic_epoch == record.dynamic_epoch) begin
                `uvm_fatal("LATER_SPLIT_TLB",
                           $sformatf("LU1 PTW request arrived before frm_mabuf request uid=%0d epoch=%0d vpn=0x%0h sample=%0d",
                                     scan_uid, record.dynamic_epoch,
                                     lu1_ptw_vpn, sample_seq))
                return;
            end
            if (record != null &&
                record.state == memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_LU1_PTW &&
                record.split_vpn[37:0] == lu1_ptw_vpn &&
                status_by_uid[scan_uid] != null &&
                status_by_uid[scan_uid].active &&
                status_by_uid[scan_uid].dynamic_epoch == record.dynamic_epoch) begin
                candidate_uid = scan_uid;
                candidate_count++;
            end
        end
        if (candidate_count > 1) begin
            `uvm_fatal("LATER_SPLIT_TLB",
                       $sformatf("LU1 PTW request has ambiguous split owners vpn=0x%0h s2xlate=%0d sample=%0d candidates=%0d",
                                 lu1_ptw_vpn, lu1_ptw_s2xlate,
                                 sample_seq, candidate_count))
            return;
        end
        if (candidate_count == 0) begin
            return;
        end
        record = later_split_tlb_record_by_uid[candidate_uid];
        record.s2xlate = lu1_ptw_s2xlate;
        record.lu1_ptw_sample_seq = sample_seq;
        record.state = memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_L2_TOKEN;
        `uvm_info("LATER_SPLIT_TLB",
                  $sformatf("bind LU1 PTW owner uid=%0d epoch=%0d vpn=0x%0h s2xlate=%0d sample=%0d",
                            candidate_uid, record.dynamic_epoch,
                            lu1_ptw_vpn, lu1_ptw_s2xlate,
                            sample_seq), UVM_LOW)
    endfunction:observe_later_split_tlb_sample

    function void observe_later_split_load_response_sample(
        input bit response_valid,
        input memblock_rob_key_t response_rob_key,
        input bit [63:0] response_vaddr,
        input bit response_nc,
        input bit response_mmio,
        input bit response_need_replay,
        input longint unsigned sample_seq);
        memblock_uid_t uid;
        status_transaction status;
        main_control_transaction main_tr;
        memblock_later_split_tlb_record record;

        if (!response_valid) begin
            return;
        end
        if (sample_seq == 0) begin
            `uvm_fatal("LATER_SPLIT_TLB", "mabuf response requires non-zero sample")
            return;
        end
        if (!lookup_active_uid_by_rob(response_rob_key, uid)) begin
            `uvm_fatal("LATER_SPLIT_TLB",
                       $sformatf("mabuf response has no active UID sample=%0d rob=%0d/%0d va=0x%0h",
                                 sample_seq, response_rob_key.flag,
                                 response_rob_key.value, response_vaddr))
            return;
        end
        status = get_status(uid);
        main_tr = get_main_transaction(uid);
        if (response_vaddr[63:12] == main_tr.vaddr[63:12]) begin
            return;
        end
        if (!later_split_tlb_record_by_uid.exists(uid) ||
            later_split_tlb_record_by_uid[uid] == null) begin
            `uvm_fatal("LATER_SPLIT_TLB",
                       $sformatf("later-page mabuf response has no split owner uid=%0d epoch=%0d rob=%0d/%0d va=0x%0h sample=%0d",
                                 uid, status.dynamic_epoch,
                                 response_rob_key.flag,
                                 response_rob_key.value,
                                 response_vaddr, sample_seq))
            return;
        end
        record = later_split_tlb_record_by_uid[uid];
        if (record.dynamic_epoch != status.dynamic_epoch ||
            record.rob_key != response_rob_key ||
            record.split_vaddr != response_vaddr ||
            record.state == memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_CANCELED) begin
            `uvm_fatal("LATER_SPLIT_TLB",
                       $sformatf("later-page mabuf response provenance mismatch uid=%0d epoch=%0d owner_epoch=%0d state=%0d owner_rob=%0d/%0d response_rob=%0d/%0d owner_va=0x%0h response_va=0x%0h sample=%0d",
                                 uid, status.dynamic_epoch,
                                 record.dynamic_epoch, record.state,
                                 record.rob_key.flag, record.rob_key.value,
                                 response_rob_key.flag, response_rob_key.value,
                                 record.split_vaddr, response_vaddr,
                                 sample_seq))
            return;
        end
        record.mabuf_response_sample_seq = sample_seq;
        record.mabuf_response_vaddr = response_vaddr;
        record.mabuf_response_nc = response_nc;
        record.mabuf_response_mmio = response_mmio;
        record.mabuf_response_need_replay = response_need_replay;
        `uvm_info("LATER_SPLIT_TLB",
                  $sformatf("capture mabuf response uid=%0d epoch=%0d rob=%0d/%0d va=0x%0h nc=%0d mmio=%0d need_replay=%0d sample=%0d state=%0d",
                            uid, status.dynamic_epoch,
                            response_rob_key.flag, response_rob_key.value,
                            response_vaddr, response_nc, response_mmio,
                            response_need_replay,
                            sample_seq, record.state), UVM_LOW)
    endfunction:observe_later_split_load_response_sample

    function memblock_later_split_tlb_record bind_later_split_l2_request_token(
        input memblock_tlb_lookup_key_t request_key,
        input longint unsigned request_token,
        input longint unsigned sample_seq);
        memblock_later_split_tlb_record record;
        memblock_uid_t candidate_uid;
        int unsigned candidate_count;

        if (sample_seq == 0) begin
            `uvm_fatal("LATER_SPLIT_TLB", "L2 token bind requires non-zero sample")
            return null;
        end
        candidate_count = 0;
        candidate_uid = 0;
        foreach (later_split_tlb_record_by_uid[uid]) begin
            record = later_split_tlb_record_by_uid[uid];
            if (record != null &&
                record.state inside {
                    memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_RESPONSE,
                    memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_COMPLETED} &&
                record.response_token == request_token) begin
                `uvm_fatal("LATER_SPLIT_TLB",
                           $sformatf("L2 token=%0d already owned by uid=%0d epoch=%0d",
                                     request_token, record.uid,
                                     record.dynamic_epoch))
                return null;
            end
            if (record != null &&
                record.state == memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_L2_TOKEN &&
                record.split_vpn == request_key.vpn &&
                record.s2xlate == request_key.s2xlate &&
                status_by_uid[uid] != null && status_by_uid[uid].active &&
                status_by_uid[uid].dynamic_epoch == record.dynamic_epoch) begin
                candidate_uid = uid;
                candidate_count++;
            end
        end
        if (candidate_count > 1) begin
            `uvm_fatal("LATER_SPLIT_TLB",
                       $sformatf("aggregate L2 request has ambiguous split owners vpn=0x%0h s2xlate=%0d token=%0d sample=%0d candidates=%0d",
                                 request_key.vpn, request_key.s2xlate,
                                 request_token, sample_seq, candidate_count))
            return null;
        end
        if (candidate_count == 0) begin
            return null;
        end
        record = later_split_tlb_record_by_uid[candidate_uid];
        record.request_lookup_key = request_key;
        record.response_token = request_token;
        record.l2_request_sample_seq = sample_seq;
        record.state = memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_RESPONSE;
        `uvm_info("LATER_SPLIT_TLB",
                  $sformatf("bind exact L2 token uid=%0d epoch=%0d vpn=0x%0h token=%0d sample=%0d",
                            candidate_uid, record.dynamic_epoch,
                            request_key.vpn, request_token, sample_seq), UVM_LOW)
        return record;
    endfunction:bind_later_split_l2_request_token

    function void complete_later_split_l2_response(
        input memblock_later_split_tlb_record owner,
        input longint unsigned request_token,
        input memblock_tlb_lookup_key_t request_key,
        input memblock_tlb_lookup_key_t entry_anchor_key,
        input longint unsigned pending_entry_generation,
        input memblock_tlb_entry entry,
        input longint unsigned complete_sample_seq);
        memblock_later_split_tlb_record canonical;

        if (owner == null || entry == null || complete_sample_seq == 0) begin
            `uvm_fatal("LATER_SPLIT_TLB", "completion has invalid owner handle")
            return;
        end
        if (owner.state ==
                memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_CANCELED &&
            (!is_valid_uid(owner.uid) ||
             status_by_uid[owner.uid] == null ||
             !status_by_uid[owner.uid].active ||
             status_by_uid[owner.uid].dynamic_epoch != owner.dynamic_epoch)) begin
            `uvm_info("LATER_SPLIT_TLB",
                      $sformatf("ignore canceled stale completion uid=%0d epoch=%0d token=%0d sample=%0d",
                                owner.uid, owner.dynamic_epoch,
                                request_token, complete_sample_seq), UVM_LOW)
            return;
        end
        if (!later_split_tlb_record_by_uid.exists(owner.uid)) begin
            `uvm_fatal("LATER_SPLIT_TLB", "completion owner is missing from canonical map")
            return;
        end
        canonical = later_split_tlb_record_by_uid[owner.uid];
        if (canonical != owner) begin
            if (canonical != null &&
                canonical.uid == owner.uid &&
                canonical.dynamic_epoch == owner.dynamic_epoch &&
                canonical.rob_key == owner.rob_key &&
                canonical.split_vaddr == owner.split_vaddr &&
                canonical.split_fire_sample_seq > owner.mabuf_response_sample_seq &&
                owner.response_token == request_token &&
                owner.request_lookup_key == request_key &&
                owner.mabuf_response_sample_seq != 0 &&
                owner.mabuf_response_sample_seq >= owner.lu1_request_sample_seq &&
                owner.mabuf_response_need_replay &&
                !owner.mabuf_response_nc && !owner.mabuf_response_mmio) begin
                `uvm_info("LATER_SPLIT_TLB",
                          $sformatf("ignore superseded replay-attempt completion uid=%0d epoch=%0d token=%0d old_split_sample=%0d replay_response_sample=%0d new_split_sample=%0d complete_sample=%0d",
                                    owner.uid, owner.dynamic_epoch, request_token,
                                    owner.split_fire_sample_seq,
                                    owner.mabuf_response_sample_seq,
                                    canonical.split_fire_sample_seq,
                                    complete_sample_seq), UVM_LOW)
                return;
            end
            `uvm_fatal("LATER_SPLIT_TLB", "completion has stale owner handle without replay provenance")
            return;
        end
        if (canonical.state != memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_RESPONSE ||
            status_by_uid[canonical.uid] == null ||
            !status_by_uid[canonical.uid].active ||
            status_by_uid[canonical.uid].dynamic_epoch != canonical.dynamic_epoch ||
            main_table_by_uid[canonical.uid] == null ||
            main_table_by_uid[canonical.uid].get_rob_key() != canonical.rob_key ||
            canonical.response_token != request_token ||
            canonical.request_lookup_key != request_key ||
            canonical.split_vpn != request_key.vpn ||
            canonical.s2xlate != request_key.s2xlate ||
            entry.lookup_key != entry_anchor_key ||
            entry.entry_generation == 0 ||
            pending_entry_generation != entry.entry_generation) begin
            `uvm_fatal("LATER_SPLIT_TLB",
                       $sformatf("completion provenance mismatch uid=%0d epoch=%0d state=%0d owner_token=%0d response_token=%0d owner_vpn=0x%0h request_vpn=0x%0h owner_s2=%0d request_s2=%0d pending_gen=%0d entry_gen=%0d",
                                 canonical.uid, canonical.dynamic_epoch,
                                 canonical.state, canonical.response_token,
                                 request_token, canonical.split_vpn,
                                 request_key.vpn, canonical.s2xlate,
                                 request_key.s2xlate,
                                 pending_entry_generation,
                                 entry.entry_generation))
            return;
        end
        canonical.entry_anchor_key = entry_anchor_key;
        canonical.payload.copy_from(entry);
        canonical.response_sample_seq = complete_sample_seq;
        canonical.entry_generation = entry.entry_generation;
        canonical.state = memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_COMPLETED;
        `uvm_info("LATER_SPLIT_TLB",
                  $sformatf("complete exact owner uid=%0d epoch=%0d request_vpn=0x%0h anchor_vpn=0x%0h token=%0d sample=%0d generation=%0d",
                            canonical.uid, canonical.dynamic_epoch,
                            canonical.request_lookup_key.vpn,
                            canonical.entry_anchor_key.vpn, request_token,
                            complete_sample_seq,
                            canonical.entry_generation), UVM_LOW)
    endfunction:complete_later_split_l2_response

    function void cancel_later_split_l2_token(
        input longint unsigned request_token,
        input string reason = "");
        foreach (later_split_tlb_record_by_uid[uid]) begin
            memblock_later_split_tlb_record record;
            record = later_split_tlb_record_by_uid[uid];
            if (record != null &&
                record.state == memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_RESPONSE &&
                record.response_token == request_token) begin
                record.state = memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_CANCELED;
                `uvm_info("LATER_SPLIT_TLB",
                          $sformatf("cancel token owner uid=%0d epoch=%0d token=%0d reason=%s",
                                    uid, record.dynamic_epoch,
                                    request_token, reason), UVM_LOW)
                return;
            end
        end
    endfunction:cancel_later_split_l2_token

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

    // 抽象职责：在 fault STD 的 active ROB owner 即将删除前，保存一个固定四
    // service-sample 的 value-only raw 归属证明。该记录不拥有 issue、LSQ 或终态状态。
    function void capture_std_late_raw_tombstone(input memblock_uid_t uid);
        status_transaction                 status;
        memblock_rob_key_t                 rob_key;
        memblock_rob_map_key_t             rob_map_key;
        memblock_std_late_raw_tombstone_t  tombstone;

        check_uid(uid, "capture_std_late_raw_tombstone");
        status = get_status(uid);
        if (!status.active || !status.std_dispatched ||
            !(status.fault || status.exception_pending || status.sta_fault)) begin
            return;
        end
        rob_key = status.get_rob_key();
        rob_map_key = rob_order_util::rob_to_map_key(rob_key);
        if (std_late_raw_tombstone_by_rob.exists(rob_map_key)) begin
            tombstone = std_late_raw_tombstone_by_rob[rob_map_key];
            if (!tombstone.valid || tombstone.uid != uid ||
                tombstone.dynamic_epoch != status.dynamic_epoch) begin
                `uvm_fatal("STD_LATE_TOMBSTONE",
                           $sformatf("ROB tombstone collision rob=%0d/%0d old_uid=%0d old_epoch=%0d uid=%0d epoch=%0d",
                                     rob_key.flag, rob_key.value, tombstone.uid,
                                     tombstone.dynamic_epoch, uid, status.dynamic_epoch))
            end
            return;
        end
        tombstone = '{default:'0};
        tombstone.valid = 1'b1;
        tombstone.uid = uid;
        tombstone.dynamic_epoch = status.dynamic_epoch;
        tombstone.redirect_epoch = memblock_sync_pkg::dispatch_flush_epoch;
        tombstone.expire_service_sample =
            memblock_sync_pkg::get_dispatch_service_cycle() +
            MEMBLOCK_REDIRECT_DELETED_OWNER_HOLD_SAMPLES;
        std_late_raw_tombstone_by_rob[rob_map_key] = tombstone;
        `uvm_info("STD_LATE_TOMBSTONE",
                  $sformatf("capture uid=%0d epoch=%0d rob=%0d/%0d redirect_epoch=%0d expire_sample=%0d",
                            uid, tombstone.dynamic_epoch, rob_key.flag, rob_key.value,
                            tombstone.redirect_epoch, tombstone.expire_service_sample),
                  UVM_LOW)
    endfunction:capture_std_late_raw_tombstone

    // 抽象职责：维护短时 STD raw 归属表。仅删除已过期记录，不读取 main table，
    // 因而可放在每个 dispatch service sample 的中频收尾路径。
    function void service_std_late_raw_tombstones();
        memblock_rob_map_key_t             rob_map_key;
        memblock_std_late_raw_tombstone_t  tombstone;

        foreach (std_late_raw_tombstone_by_rob[rob_map_key]) begin
            tombstone = std_late_raw_tombstone_by_rob[rob_map_key];
            if (!tombstone.valid ||
                memblock_sync_pkg::get_dispatch_service_cycle() >=
                tombstone.expire_service_sample) begin
                std_late_raw_tombstone_by_rob.delete(rob_map_key);
            end
        end
    endfunction:service_std_late_raw_tombstones

    // 抽象职责：为 adapter 查询完整 ROB key 是否属于已删除 fault STD 的短时
    // pipeline 残留。过期记录立即清除；命中不改变 status、issue 或物理资源。
    function bit read_std_late_raw_tombstone(
        input memblock_rob_key_t rob_key,
        output memblock_std_late_raw_tombstone_t tombstone
    );
        memblock_rob_map_key_t rob_map_key;

        tombstone = '{default:'0};
        rob_map_key = rob_order_util::rob_to_map_key(rob_key);
        if (!std_late_raw_tombstone_by_rob.exists(rob_map_key)) begin
            return 1'b0;
        end
        tombstone = std_late_raw_tombstone_by_rob[rob_map_key];
        if (!tombstone.valid ||
            memblock_sync_pkg::get_dispatch_service_cycle() >=
            tombstone.expire_service_sample) begin
            std_late_raw_tombstone_by_rob.delete(rob_map_key);
            tombstone = '{default:'0};
            return 1'b0;
        end
        return 1'b1;
    endfunction:read_std_late_raw_tombstone

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
        if (status.terminal_done) begin
            clear_uncache_retry_boost(uid, "terminal retire");
        end
        if (!status.active) begin
            `uvm_fatal("COMMON_DATA", $sformatf("retire_active_uid got inactive uid=%0d", uid))
        end
        // 已 fire 的 fault STD 在 owner 删除后仍可能从 DUT pipeline 迟到一拍。
        // 必须在 active ROB map 删除前保存短时身份；正常 owner 始终优先于该记录。
        capture_std_late_raw_tombstone(uid);
        remove_uid_from_issue_queues(uid);
        release_ptw_wait_replay(uid);
        status.clear_sta_late_fault_tombstones();

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
        quiesce_fault_uid_pending_work(uid);
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

    function void release_uid_sq_mapping(input memblock_uid_t uid,
                                         input bit allow_fault_redirect_cleanup = 1'b0);
        status_transaction     status;
        memblock_sq_key_t      sq_key;
        memblock_sq_map_key_t  sq_map_key;

        status = get_status(uid);
        if (!status.active_sq_mapped) begin
            return;
        end
        if (store_fault_blocks_sq_deq(uid) && !allow_fault_redirect_cleanup) begin
            `uvm_fatal("STORE_FAULT_BARRIER",
                       $sformatf("SQ mapping release bypassed deq preflight uid=%0d epoch=%0d sta_wb=%0d std_wb=%0d",
                                 uid, status.dynamic_epoch,
                                 status.sta_writeback, status.std_writeback))
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
        if (is_scalar_store_uid(uid) && !status.std_writeback) begin
            `uvm_info("STORE_FAULT_BARRIER",
                      $sformatf("SQ deq before STD real-WB uid=%0d epoch=%0d sta_dispatch/wb=%0d/%0d std_dispatch/queued/wb/pass=%0d/%0d/%0d/%0d sta_fault=%0d pending_fault=%0d",
                                uid, status.dynamic_epoch,
                                status.sta_dispatched, status.sta_writeback,
                                status.std_dispatched, status.queued_std,
                                status.std_writeback, status.std_pass,
                                status.sta_fault, pending_store_fault_matches(uid)),
                      UVM_LOW)
        end
        // 中文注释：SQ deq 后统一删除未 fire 的 STD item，避免无 owner 的 stale item
        // 被重新路由或发射；若 pending fault 仍未完成双 WB，保留诊断并等待上游时序修正。
        delete_issue_queue_entry(MEMBLOCK_ISSUE_TARGET_STD, uid, 0, 1'b0);
        status.queued_std = 1'b0;
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
            if (store_fault_real_wb_pending(uid)) begin
                return;
            end
            if (fault_redirect_resource_blocks_retire(uid)) begin
                return;
            end
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
        memblock_control_lifecycle_ticket_t audit_ticket;
        memblock_uid_t planned_terminal_done_uid;

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
            return;
        end
        if (control_barrier_prefix_external_provenance_valid) begin
            `uvm_fatal("CONTROL_COMMIT",
                       $sformatf("cannot terminal uid=%0d with residual provenance uid=%0d",
                                 uid, control_barrier_prefix_external_provenance_uid))
            return;
        end
        if (!precheck_control_lifecycle_event(
                uid, MEMBLOCK_CONTROL_AUDIT_TERMINAL, audit_ticket)) begin
            return;
        end
        if (!precheck_control_terminal_retire(uid, planned_terminal_done_uid)) begin
            return;
        end
        commit_control_terminal_retire(uid, planned_terminal_done_uid);
        record_control_lifecycle_event(audit_ticket);
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
        if (entry == null || entry.entry_generation == 0 ||
            entry.payload_phase !=
                memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_FINALIZED) begin
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
                memblock_tlb_entry::derive_allstage_lookup_shape(
                    entry.s1_level, entry.s1_pte_n,
                    entry.s2_level, entry.s2_pte_n,
                    effective_level, effective_napot);
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

    // 中文注释：新canonical live entry的唯一生产发布入口。所有range/parent
    // key和冲突条件先检查，再在同一个零时间function内连续更新主表和两个索引。
    function void publish_live_tlb_entry(
        input memblock_tlb_lookup_key_t anchor_key,
        input memblock_tlb_entry entry,
        input mmu_csr_runtime_state csr_snapshot);
        memblock_tlb_lookup_key_t expected_key;
        memblock_tlb_range_index_key_t range_keys[$];
        memblock_tlb_parent_occupancy_key_t parent_keys[$];
        memblock_tlb_parent_owner_key_t owner_key;

        if (entry == null || csr_snapshot == null || has_tlb_entry(anchor_key) ||
            entry.lookup_key != anchor_key || entry.s2xlate != anchor_key.s2xlate ||
            entry.entry_generation == 0 || entry.range_index_keys.size() != 0 ||
            entry.parent_occupancy_keys.size() != 0 ||
            entry.payload_phase !=
                memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_FINALIZED) begin
            `uvm_fatal("COMMON_DATA", "publish_live_tlb_entry got invalid detached entry")
        end
        expected_key = csr_snapshot.make_lookup_key(anchor_key.vpn,
                                                     anchor_key.s2xlate);
        if (!tlb_lookup_key_equal(expected_key, anchor_key)) begin
            `uvm_fatal("COMMON_DATA", "publish key disagrees with frozen CSR snapshot")
        end
        validate_normal_napot_payload(anchor_key, entry);
        void'(validate_no_live_tlb_overlap(anchor_key, entry));
        build_entry_range_index_keys(entry, range_keys);
        build_tlb_parent_occupancy_keys(entry, parent_keys);

        foreach (range_keys[key_idx]) begin
            for (int prior_idx = 0; prior_idx < key_idx; prior_idx++) begin
                if (range_keys[prior_idx] == range_keys[key_idx]) begin
                    `uvm_fatal("COMMON_DATA", "publish produced duplicate range key")
                end
            end
            if (tlb_anchor_keys_by_range_key.exists(range_keys[key_idx])) begin
                if (tlb_anchor_keys_by_range_key[range_keys[key_idx]].size() >=
                    MEMBLOCK_TLB_RANGE_CANDIDATE_MAX) begin
                    `uvm_fatal("COMMON_DATA", "publish range bucket capacity overflow")
                end
                foreach (tlb_anchor_keys_by_range_key[range_keys[key_idx]][bucket_idx]) begin
                    if (tlb_lookup_key_equal(
                            tlb_anchor_keys_by_range_key[range_keys[key_idx]][bucket_idx],
                            anchor_key)) begin
                        `uvm_fatal("COMMON_DATA", "publish found duplicate range anchor")
                    end
                end
            end
        end
        foreach (parent_keys[key_idx]) begin
            for (int prior_idx = 0; prior_idx < key_idx; prior_idx++) begin
                if (parent_keys[prior_idx] == parent_keys[key_idx]) begin
                    `uvm_fatal("COMMON_DATA", "publish produced duplicate parent key")
                end
            end
            owner_key.parent_key = parent_keys[key_idx];
            owner_key.anchor_key = anchor_key;
            if (tlb_parent_owner_set.exists(owner_key) ||
                (tlb_parent_occupancy_count.exists(parent_keys[key_idx]) &&
                 tlb_parent_occupancy_count[parent_keys[key_idx]] == '1)) begin
                `uvm_fatal("COMMON_DATA", "publish parent owner duplicate/count overflow")
            end
        end

        tlb_entry_by_key[anchor_key] = entry;
        foreach (range_keys[key_idx]) begin
            tlb_anchor_keys_by_range_key[range_keys[key_idx]].push_back(anchor_key);
            entry.range_index_keys.push_back(range_keys[key_idx]);
        end
        foreach (parent_keys[key_idx]) begin
            owner_key.parent_key = parent_keys[key_idx];
            owner_key.anchor_key = anchor_key;
            if (!tlb_parent_occupancy_count.exists(parent_keys[key_idx])) begin
                tlb_parent_occupancy_count[parent_keys[key_idx]] = 0;
            end
            tlb_parent_occupancy_count[parent_keys[key_idx]]++;
            tlb_parent_owner_set[owner_key] = 1'b1;
            entry.parent_occupancy_keys.push_back(parent_keys[key_idx]);
        end
        if (tlb_parent_owner_set.num() > parent_owner_peak_count) begin
            parent_owner_peak_count = tlb_parent_owner_set.num();
        end
    endfunction:publish_live_tlb_entry

    function void unregister_tlb_parent_occupancy_index(
        input memblock_tlb_lookup_key_t anchor_key,
        input memblock_tlb_entry entry);
        memblock_tlb_parent_owner_key_t owner_key;

        if (entry == null || entry.lookup_key != anchor_key) begin
            `uvm_fatal("COMMON_DATA", "parent unregistration got invalid canonical entry")
        end
        foreach (entry.parent_occupancy_keys[key_idx]) begin
            owner_key.parent_key = entry.parent_occupancy_keys[key_idx];
            owner_key.anchor_key = anchor_key;
            if (!tlb_parent_owner_set.exists(owner_key) ||
                !tlb_parent_occupancy_count.exists(owner_key.parent_key) ||
                tlb_parent_occupancy_count[owner_key.parent_key] == 0) begin
                `uvm_fatal("COMMON_DATA", "parent owner/count lifecycle is inconsistent")
            end
            tlb_parent_owner_set.delete(owner_key);
            tlb_parent_occupancy_count[owner_key.parent_key]--;
            if (tlb_parent_occupancy_count[owner_key.parent_key] == 0) begin
                tlb_parent_occupancy_count.delete(owner_key.parent_key);
            end
        end
        entry.parent_occupancy_keys.delete();
    endfunction:unregister_tlb_parent_occupancy_index

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
                memblock_tlb_entry::derive_allstage_lookup_shape(
                    entry.s1_level, entry.s1_pte_n,
                    entry.s2_level, entry.s2_pte_n, level, napot);
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

    function memblock_tlb_range_kind_e get_tlb_range_kind_by_s2xlate(
        input bit [1:0] s2xlate);
        case (s2xlate)
            2'd0, 2'd1: return MEMBLOCK_TLB_RANGE_KIND_S1;
            2'd2: return MEMBLOCK_TLB_RANGE_KIND_S2;
            2'd3: return MEMBLOCK_TLB_RANGE_KIND_ALLSTAGE;
            default: begin
                `uvm_fatal("COMMON_DATA", "unsupported s2xlate for parent occupancy")
            end
        endcase
        return MEMBLOCK_TLB_RANGE_KIND_S1;
    endfunction:get_tlb_range_kind_by_s2xlate

    function bit [51:0] normalize_tlb_parent_vpn(
        input bit [51:0] vpn,
        input memblock_tlb_range_coverage_rank_e parent_region);
        bit [51:0] normalized_vpn;

        normalized_vpn = vpn;
        case (parent_region)
            MEMBLOCK_TLB_COVERAGE_4K:   begin end
            MEMBLOCK_TLB_COVERAGE_64K:  normalized_vpn[3:0] = '0;
            MEMBLOCK_TLB_COVERAGE_2M:   normalized_vpn[8:0] = '0;
            MEMBLOCK_TLB_COVERAGE_1G:   normalized_vpn[17:0] = '0;
            MEMBLOCK_TLB_COVERAGE_512G: normalized_vpn[26:0] = '0;
            default: begin
                `uvm_fatal("COMMON_DATA", "invalid shape cannot be a parent region")
            end
        endcase
        return normalized_vpn;
    endfunction:normalize_tlb_parent_vpn

    function memblock_tlb_parent_occupancy_key_t make_tlb_parent_occupancy_key(
        input bit [51:0] vpn,
        input memblock_tlb_entry context_entry,
        input memblock_tlb_range_coverage_rank_e owner_shape,
        input memblock_tlb_range_coverage_rank_e parent_region,
        input memblock_tlb_context_scope_e context_scope,
        input bit [15:0] asid);
        memblock_tlb_parent_occupancy_key_t parent_key;

        if (context_entry == null) begin
            `uvm_fatal("COMMON_DATA", "parent occupancy key got null context entry")
        end
        parent_key = '{default:'0};
        parent_key.range_kind = get_tlb_range_kind_by_s2xlate(
            context_entry.s2xlate);
        parent_key.s2xlate = context_entry.s2xlate;
        parent_key.owner_shape = owner_shape;
        parent_key.parent_region = parent_region;
        parent_key.context_scope = context_scope;
        parent_key.normalized_vpn = normalize_tlb_parent_vpn(
            vpn, parent_region);
        if (context_entry.s2xlate == 2'd2) begin
            parent_key.vmid = context_entry.s2_vmid;
            parent_key.asid = '0;
        end
        else begin
            parent_key.vmid = context_entry.s2xlate == 2'd0 ? '0 :
                              context_entry.s1_vmid;
            parent_key.asid = context_scope == MEMBLOCK_TLB_CONTEXT_EXACT_ASID ?
                              asid : '0;
        end
        return parent_key;
    endfunction:make_tlb_parent_occupancy_key

    // 中文注释：注册key表达已有entry对父区域的所有权。S1 non-global同时
    // 注册EXACT_ASID和ANY_ASID；global注册GLOBAL和ANY_ASID；S2-only只注册VMID。
    function void build_tlb_parent_registration_keys_for_region(
        input bit [51:0] vpn,
        input memblock_tlb_entry entry,
        input memblock_tlb_range_coverage_rank_e owner_shape,
        input memblock_tlb_range_coverage_rank_e parent_region,
        output memblock_tlb_parent_occupancy_key_t keys[$]);
        bit context_global;

        keys.delete();
        if (entry == null) begin
            `uvm_fatal("COMMON_DATA", "parent registration got null entry")
        end
        if (entry.s2xlate == 2'd2) begin
            keys.push_back(make_tlb_parent_occupancy_key(
                vpn, entry, owner_shape, parent_region,
                MEMBLOCK_TLB_CONTEXT_S2_ONLY, '0));
            return;
        end
        context_global = entry.s1_stage_active && entry.s1_pte_g;
        keys.push_back(make_tlb_parent_occupancy_key(
            vpn, entry, owner_shape, parent_region,
            context_global ? MEMBLOCK_TLB_CONTEXT_GLOBAL :
                             MEMBLOCK_TLB_CONTEXT_EXACT_ASID,
            entry.s1_asid));
        keys.push_back(make_tlb_parent_occupancy_key(
            vpn, entry, owner_shape, parent_region,
            MEMBLOCK_TLB_CONTEXT_ANY_ASID, '0));
    endfunction:build_tlb_parent_registration_keys_for_region

    // 中文注释：候选大页查询与注册scope互补。non-global查询当前ASID和GLOBAL，
    // global查询ANY_ASID，确保任一方global时冲突；S2-only只按VMID查询。
    function void build_tlb_parent_query_keys_for_region(
        input bit [51:0] vpn,
        input memblock_tlb_entry prepared_entry,
        input memblock_tlb_range_coverage_rank_e owner_shape,
        input memblock_tlb_range_coverage_rank_e parent_region,
        output memblock_tlb_parent_occupancy_key_t keys[$]);
        bit context_global;

        keys.delete();
        if (prepared_entry == null) begin
            `uvm_fatal("COMMON_DATA", "parent query got null prepared entry")
        end
        if (prepared_entry.s2xlate == 2'd2) begin
            keys.push_back(make_tlb_parent_occupancy_key(
                vpn, prepared_entry, owner_shape, parent_region,
                MEMBLOCK_TLB_CONTEXT_S2_ONLY, '0));
            return;
        end
        context_global = prepared_entry.s1_stage_active &&
                         prepared_entry.s1_pte_g;
        if (context_global) begin
            keys.push_back(make_tlb_parent_occupancy_key(
                vpn, prepared_entry, owner_shape, parent_region,
                MEMBLOCK_TLB_CONTEXT_ANY_ASID, '0));
        end
        else begin
            keys.push_back(make_tlb_parent_occupancy_key(
                vpn, prepared_entry, owner_shape, parent_region,
                MEMBLOCK_TLB_CONTEXT_EXACT_ASID,
                prepared_entry.s1_asid));
            keys.push_back(make_tlb_parent_occupancy_key(
                vpn, prepared_entry, owner_shape, parent_region,
                MEMBLOCK_TLB_CONTEXT_GLOBAL, '0));
        end
    endfunction:build_tlb_parent_query_keys_for_region

    function void build_tlb_parent_occupancy_keys(
        input memblock_tlb_entry entry,
        output memblock_tlb_parent_occupancy_key_t keys[$]);
        memblock_tlb_range_coverage_rank_e entry_shape;
        memblock_tlb_parent_occupancy_key_t region_keys[$];

        keys.delete();
        if (entry == null || entry.entry_generation == 0 ||
            entry.payload_phase !=
                memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_FINALIZED) begin
            `uvm_fatal("COMMON_DATA", "parent occupancy build got invalid entry")
        end
        entry_shape = get_tlb_range_match_coverage_rank(entry);
        // Register the entry's own shape as well as every larger shape.  This
        // is required when a later global entry is built under another ASID:
        // same-shape and larger non-global entries must remain discoverable.
        for (int unsigned region_idx = int'(entry_shape);
             region_idx <= int'(MEMBLOCK_TLB_COVERAGE_512G);
             region_idx++) begin
            build_tlb_parent_registration_keys_for_region(
                entry.lookup_key.vpn, entry, entry_shape,
                memblock_tlb_range_coverage_rank_e'(region_idx), region_keys);
            foreach (region_keys[idx]) keys.push_back(region_keys[idx]);
        end
    endfunction:build_tlb_parent_occupancy_keys

    // 中文注释：候选shape的冲突查询分成两组。更小/同尺寸owner只查询
    // 候选自身区域；更大owner只查询其实际shape区域。owner_shape字段避免
    // 同一更大区域内互不重叠的小页被误认为覆盖当前候选。
    function void build_tlb_overlap_query_keys(
        input bit [51:0] request_vpn,
        input memblock_tlb_entry prepared_entry,
        input memblock_tlb_range_coverage_rank_e candidate_shape,
        output memblock_tlb_parent_occupancy_key_t keys[$]);
        memblock_tlb_parent_occupancy_key_t shape_keys[$];

        keys.delete();
        for (int unsigned owner_idx = int'(MEMBLOCK_TLB_COVERAGE_4K);
             owner_idx <= int'(candidate_shape); owner_idx++) begin
            build_tlb_parent_query_keys_for_region(
                request_vpn, prepared_entry,
                memblock_tlb_range_coverage_rank_e'(owner_idx),
                candidate_shape, shape_keys);
            foreach (shape_keys[idx]) keys.push_back(shape_keys[idx]);
        end
        for (int unsigned owner_idx = int'(candidate_shape) + 1;
             owner_idx <= int'(MEMBLOCK_TLB_COVERAGE_512G); owner_idx++) begin
            build_tlb_parent_query_keys_for_region(
                request_vpn, prepared_entry,
                memblock_tlb_range_coverage_rank_e'(owner_idx),
                memblock_tlb_range_coverage_rank_e'(owner_idx), shape_keys);
            foreach (shape_keys[idx]) keys.push_back(shape_keys[idx]);
        end
    endfunction:build_tlb_overlap_query_keys

    function bit tlb_shape_supported_by_active_modes(
        input memblock_tlb_entry entry,
        input memblock_tlb_range_coverage_rank_e shape);
        if (entry == null) begin
            `uvm_fatal("COMMON_DATA", "shape mode check got null entry")
        end
        if (shape != MEMBLOCK_TLB_COVERAGE_512G) begin
            return 1'b1;
        end
        return (!entry.s1_stage_active ||
                entry.s1_translation_mode_at_build == MEMBLOCK_SV48_MODE) &&
               (!entry.s2_stage_active ||
                entry.s2_translation_mode_at_build == MEMBLOCK_SV48_MODE);
    endfunction:tlb_shape_supported_by_active_modes

    // 中文注释：真正miss在builder finalize前调用。每个shape只做固定1到2次
    // count查询，已有child时屏蔽对应大页，不扫描owner或canonical live表。
    function bit [4:0] derive_allowed_tlb_shape_mask(
        input bit [51:0] request_vpn,
        input memblock_tlb_entry prepared_entry);
        bit [4:0] allowed_shape_mask;
        memblock_tlb_parent_occupancy_key_t query_keys[$];

        allowed_shape_mask = '0;
        for (int unsigned shape_idx = 0;
             shape_idx <= int'(MEMBLOCK_TLB_COVERAGE_512G);
             shape_idx++) begin
            allowed_shape_mask[shape_idx] = tlb_shape_supported_by_active_modes(
                prepared_entry,
                memblock_tlb_range_coverage_rank_e'(shape_idx));
        end
        allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K] = 1'b1;
        for (int unsigned region_idx = int'(MEMBLOCK_TLB_COVERAGE_4K);
             region_idx <= int'(MEMBLOCK_TLB_COVERAGE_512G);
             region_idx++) begin
            bit blocked;

            if (!allowed_shape_mask[region_idx]) continue;
            blocked = 1'b0;
            build_tlb_overlap_query_keys(
                request_vpn, prepared_entry,
                memblock_tlb_range_coverage_rank_e'(region_idx), query_keys);
            foreach (query_keys[idx]) begin
                parent_lookup_count++;
                if (tlb_parent_occupancy_count.exists(query_keys[idx]) &&
                    tlb_parent_occupancy_count[query_keys[idx]] != 0) begin
                    blocked = 1'b1;
                    break;
                end
            end
            if (blocked) begin
                allowed_shape_mask[region_idx] = 1'b0;
                case (memblock_tlb_range_coverage_rank_e'(region_idx))
                    MEMBLOCK_TLB_COVERAGE_64K:  parent_block_64k_count++;
                    MEMBLOCK_TLB_COVERAGE_2M:   parent_block_2m_count++;
                    MEMBLOCK_TLB_COVERAGE_1G:   parent_block_1g_count++;
                    MEMBLOCK_TLB_COVERAGE_512G: parent_block_512g_count++;
                    default: begin end
                endcase
            end
        end
        return allowed_shape_mask;
    endfunction:derive_allowed_tlb_shape_mask

    // 中文注释：global prepared entry若连4KB都被其他ASID的non-global
    // owner挡住，说明随机G位使候选扩大了可见上下文。prepared entry尚未
    // publish，可清G后按当前ASID重新计算；真实同ASID/global冲突仍保持阻塞。
    function bit [4:0] derive_allowed_tlb_shape_mask_with_global_fallback(
        input bit [51:0] request_vpn,
        input memblock_tlb_entry prepared_entry,
        output bit pte_g_overridden);
        bit [4:0] allowed_shape_mask;

        if (prepared_entry == null || prepared_entry.entry_generation != 0 ||
            prepared_entry.range_index_keys.size() != 0 ||
            prepared_entry.parent_occupancy_keys.size() != 0 ||
            prepared_entry.payload_phase !=
                memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_PREPARED) begin
            `uvm_fatal("COMMON_DATA",
                       "global fallback requires a detached prepared entry")
        end
        pte_g_overridden = 1'b0;
        allowed_shape_mask = derive_allowed_tlb_shape_mask(
            request_vpn, prepared_entry);
        if (!allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K] &&
            prepared_entry.s1_stage_active && prepared_entry.s1_pte_g) begin
            prepared_entry.s1_pte_g = 1'b0;
            pte_g_overridden = 1'b1;
            allowed_shape_mask = derive_allowed_tlb_shape_mask(
                request_vpn, prepared_entry);
        end
        return allowed_shape_mask;
    endfunction:derive_allowed_tlb_shape_mask_with_global_fallback

    function bit validate_no_live_tlb_overlap(
        input memblock_tlb_lookup_key_t anchor_key,
        input memblock_tlb_entry entry);
        memblock_tlb_range_coverage_rank_e final_shape;
        memblock_tlb_parent_occupancy_key_t query_keys[$];
        string witness;

        if (entry == null || entry.lookup_key != anchor_key) begin
            `uvm_fatal("COMMON_DATA", "live overlap validation got invalid entry")
        end
        final_shape = get_tlb_range_match_coverage_rank(entry);
        build_tlb_overlap_query_keys(anchor_key.vpn, entry, final_shape,
                                     query_keys);
        foreach (query_keys[idx]) begin
            parent_lookup_count++;
            if (tlb_parent_occupancy_count.exists(query_keys[idx]) &&
                tlb_parent_occupancy_count[query_keys[idx]] != 0) begin
                witness = "";
                foreach (tlb_parent_owner_set[owner_key]) begin
                    if (owner_key.parent_key == query_keys[idx]) begin
                        witness = $sformatf(" witness_vpn=0x%0h witness_asid=0x%0h witness_vmid=0x%0h",
                                            owner_key.anchor_key.vpn,
                                            owner_key.anchor_key.asid,
                                            owner_key.anchor_key.vmid);
                        break;
                    end
                end
                `uvm_fatal("COMMON_DATA",
                           $sformatf("new L2TLB live entry overlaps existing owner vpn=0x%0h shape=%0d owner_shape=%0d region=%0d count=%0d%s",
                                     anchor_key.vpn, final_shape,
                                     query_keys[idx].owner_shape,
                                     query_keys[idx].parent_region,
                                     tlb_parent_occupancy_count[query_keys[idx]],
                                     witness))
            end
        end
        return 1'b1;
    endfunction:validate_no_live_tlb_overlap

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
        memblock_tlb_entry candidate_entries[$];
        memblock_tlb_lookup_key_t candidate_anchor;
        memblock_tlb_entry candidate_entry;
        int query_idx;
        int bucket_idx;
        int seen_idx;
        int candidate_idx;
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
                          get_tlb_range_match_coverage_rank(
                              candidate_entries[candidate_idx]),
                          candidate_query_keys[candidate_idx].range_kind)};
        end
        `uvm_fatal("COMMON_DATA",
                   $sformatf("illegal overlapping L2TLB live hit request vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d candidates=%0d:%s",
                             request_key.vpn, request_key.asid,
                             request_key.vmid, request_key.s2xlate,
                             candidate_entries.size(), candidate_summary))
        return 1'b0;
    endfunction:find_tlb_range_hit_by_req

    function bit find_existing_l2tlb_entry_by_req_with_snapshot(
        input bit [37:0] vpn,
        input bit [1:0] s2xlate,
        input mmu_csr_runtime_state csr_snapshot,
        output memblock_tlb_lookup_key_t request_key,
        output memblock_tlb_lookup_key_t entry_anchor_key,
        output memblock_tlb_lookup_result_e lookup_result,
        output memblock_tlb_entry entry);
        request_key = '{default:'0};
        entry_anchor_key = '{default:'0};
        lookup_result = MEMBLOCK_TLB_LOOKUP_MISS_BUILD;
        entry = null;
        if (csr_snapshot == null) begin
            return 1'b0;
        end
        request_key = csr_snapshot.make_lookup_key({26'b0, vpn}, s2xlate);
        if (has_tlb_entry(request_key)) begin
            entry = tlb_entry_by_key[request_key];
            if (entry == null || entry.range_index_keys.size() == 0) begin
                return 1'b0;
            end
            entry_anchor_key = request_key;
            lookup_result = MEMBLOCK_TLB_LOOKUP_EXACT_HIT;
            return 1'b1;
        end
        if (find_tlb_range_hit_by_req(request_key, csr_snapshot,
                                      entry_anchor_key, entry)) begin
            lookup_result = MEMBLOCK_TLB_LOOKUP_RANGE_HIT;
            return entry != null;
        end
        return 1'b0;
    endfunction:find_existing_l2tlb_entry_by_req_with_snapshot

    function void unregister_completed_tlb_range_index(
        input memblock_tlb_lookup_key_t anchor_key,
        input memblock_tlb_entry entry);
        int idx;
        int bucket_idx;
        int unsigned removed_count;
        memblock_tlb_range_index_key_t range_key;

        if (entry == null || entry.lookup_key != anchor_key ||
            entry.range_index_keys.size() == 0) begin
            `uvm_fatal("COMMON_DATA", "completed TLB range unregistration got invalid entry")
        end
        foreach (entry.range_index_keys[idx]) begin
            range_key = entry.range_index_keys[idx];
            if (!completed_tlb_anchor_keys_by_range_key.exists(range_key)) begin
                `uvm_fatal("COMMON_DATA", "completed TLB range index lost bucket")
            end
            removed_count = 0;
            for (bucket_idx = int'(completed_tlb_anchor_keys_by_range_key[range_key].size()) - 1;
                 bucket_idx >= 0; bucket_idx--) begin
                if (tlb_lookup_key_equal(
                        completed_tlb_anchor_keys_by_range_key[range_key][bucket_idx],
                        anchor_key)) begin
                    completed_tlb_anchor_keys_by_range_key[range_key].delete(bucket_idx);
                    removed_count++;
                end
            end
            if (removed_count != 1) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("completed TLB range removal expected one anchor got=%0d",
                                     removed_count))
            end
            if (completed_tlb_anchor_keys_by_range_key[range_key].size() == 0) begin
                completed_tlb_anchor_keys_by_range_key.delete(range_key);
            end
        end
        entry.range_index_keys.delete();
    endfunction:unregister_completed_tlb_range_index

    function void delete_completed_tlb_entry_by_anchor_key(
        input memblock_tlb_lookup_key_t anchor_key,
        input string reason);
        memblock_tlb_entry entry;

        if (!completed_tlb_entry_by_key.exists(anchor_key)) begin
            return;
        end
        entry = completed_tlb_entry_by_key[anchor_key];
        if (entry == null ||
            !completed_tlb_response_token_by_key.exists(anchor_key) ||
            !completed_tlb_response_sample_by_key.exists(anchor_key)) begin
            `uvm_fatal("COMMON_DATA", "completed TLB cache provenance is incomplete")
        end
        unregister_completed_tlb_range_index(anchor_key, entry);
        completed_tlb_entry_by_key.delete(anchor_key);
        completed_tlb_response_token_by_key.delete(anchor_key);
        completed_tlb_response_sample_by_key.delete(anchor_key);
        `uvm_info("COMMON_DATA",
                  $sformatf("delete completed TLB response reason=%s vpn=0x%0h generation=%0d",
                            reason, anchor_key.vpn, entry.entry_generation),
                  UVM_LOW)
    endfunction:delete_completed_tlb_entry_by_anchor_key

    function void record_completed_tlb_response(
        input memblock_tlb_entry entry,
        input longint unsigned response_token,
        input longint unsigned response_sample_seq);
        memblock_tlb_entry snapshot;
        memblock_tlb_lookup_key_t anchor_key;
        memblock_tlb_range_index_key_t keys[$];
        int idx;
        int bucket_idx;

        // request_token is zero-based; cache/table presence is its validity.
        if (entry == null || entry.entry_generation == 0 ||
            response_sample_seq == 0) begin
            `uvm_fatal("COMMON_DATA", "completed TLB response cache requires valid provenance")
        end
        anchor_key = entry.lookup_key;
        delete_completed_tlb_entry_by_anchor_key(anchor_key,
                                                  "newer sampled response");
        snapshot = memblock_tlb_entry::type_id::create(
            $sformatf("completed_tlb_response_%0d", response_token));
        if (snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "failed to allocate completed TLB response snapshot")
        end
        snapshot.copy_from(entry);
        build_entry_range_index_keys(snapshot, keys);
        foreach (keys[idx]) begin
            if (completed_tlb_anchor_keys_by_range_key.exists(keys[idx]) &&
                completed_tlb_anchor_keys_by_range_key[keys[idx]].size() >=
                    MEMBLOCK_TLB_RANGE_CANDIDATE_MAX) begin
                `uvm_fatal("COMMON_DATA", "completed TLB range index bucket overflow")
            end
            foreach (completed_tlb_anchor_keys_by_range_key[keys[idx]][bucket_idx]) begin
                if (tlb_lookup_key_equal(
                        completed_tlb_anchor_keys_by_range_key[keys[idx]][bucket_idx],
                        anchor_key)) begin
                    `uvm_fatal("COMMON_DATA", "completed TLB range index duplicate anchor")
                end
            end
        end
        foreach (keys[idx]) begin
            completed_tlb_anchor_keys_by_range_key[keys[idx]].push_back(anchor_key);
            snapshot.range_index_keys.push_back(keys[idx]);
        end
        completed_tlb_entry_by_key[anchor_key] = snapshot;
        completed_tlb_response_token_by_key[anchor_key] = response_token;
        completed_tlb_response_sample_by_key[anchor_key] = response_sample_seq;
    endfunction:record_completed_tlb_response

    function bit find_completed_tlb_response_for_issue(
        input memblock_tlb_lookup_key_t request_key,
        input mmu_csr_runtime_state request_csr_snapshot,
        input longint unsigned issue_boundary_sample,
        output memblock_tlb_entry entry,
        output longint unsigned response_token,
        output longint unsigned response_sample_seq);
        memblock_tlb_range_index_key_t query_keys[$];
        memblock_tlb_lookup_key_t seen_keys[$];
        memblock_tlb_lookup_key_t candidate_keys[$];
        memblock_tlb_entry candidate_entries[$];
        longint unsigned candidate_tokens[$];
        longint unsigned candidate_samples[$];
        memblock_tlb_lookup_key_t anchor_key;
        longint unsigned latest_sample;
        bit seen;
        int qidx;
        int bidx;
        int sidx;
        int cidx;
        int selected_idx;
        int unsigned latest_sample_count;

        entry = null;
        response_token = 0;
        response_sample_seq = 0;
        if (request_csr_snapshot == null || issue_boundary_sample == 0) begin
            `uvm_fatal("COMMON_DATA", "completed TLB issue lookup got null CSR snapshot")
        end
        build_tlb_range_query_keys(request_key, query_keys);
        foreach (query_keys[qidx]) begin
            if (!completed_tlb_anchor_keys_by_range_key.exists(query_keys[qidx])) begin
                continue;
            end
            foreach (completed_tlb_anchor_keys_by_range_key[query_keys[qidx]][bidx]) begin
                anchor_key = completed_tlb_anchor_keys_by_range_key[query_keys[qidx]][bidx];
                seen = 1'b0;
                foreach (seen_keys[sidx]) begin
                    if (tlb_lookup_key_equal(seen_keys[sidx], anchor_key)) seen = 1'b1;
                end
                if (seen) continue;
                seen_keys.push_back(anchor_key);
                if (!completed_tlb_entry_by_key.exists(anchor_key) ||
                    completed_tlb_entry_by_key[anchor_key] == null ||
                    !completed_tlb_response_token_by_key.exists(anchor_key) ||
                    !completed_tlb_response_sample_by_key.exists(anchor_key)) begin
                    `uvm_fatal("COMMON_DATA", "completed TLB range index has incomplete provenance")
                end
                if (completed_tlb_response_sample_by_key[anchor_key] >
                    issue_boundary_sample) begin
                    continue;
                end
                if (!entry_matches_request_raw(completed_tlb_entry_by_key[anchor_key],
                                               request_key.vpn,
                                               request_key.s2xlate,
                                               request_csr_snapshot)) begin
                    `uvm_fatal("COMMON_DATA", "completed TLB range index/raw mismatch")
                end
                candidate_keys.push_back(anchor_key);
                candidate_entries.push_back(completed_tlb_entry_by_key[anchor_key]);
                candidate_tokens.push_back(
                    completed_tlb_response_token_by_key[anchor_key]);
                candidate_samples.push_back(
                    completed_tlb_response_sample_by_key[anchor_key]);
            end
        end
        if (candidate_entries.size() == 0) return 1'b0;
        latest_sample = candidate_samples[0];
        foreach (candidate_samples[cidx]) begin
            if (candidate_samples[cidx] > latest_sample) begin
                latest_sample = candidate_samples[cidx];
            end
        end
        selected_idx = -1;
        latest_sample_count = 0;
        foreach (candidate_samples[cidx]) begin
            if (candidate_samples[cidx] == latest_sample) begin
                selected_idx = cidx;
                latest_sample_count++;
            end
        end
        if (latest_sample_count != 1 || selected_idx < 0) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("ambiguous same-sample completed TLB issue visibility vpn=0x%0h sample=%0d candidates=%0d",
                                 request_key.vpn, latest_sample,
                                 latest_sample_count))
        end
        anchor_key = candidate_keys[selected_idx];
        entry = candidate_entries[selected_idx];
        response_token = candidate_tokens[selected_idx];
        response_sample_seq = candidate_samples[selected_idx];
        return 1'b1;
    endfunction:find_completed_tlb_response_for_issue

    function bit capture_uid_issue_hit_snapshot(input memblock_uid_tlb_record record);
        memblock_tlb_entry entry;
        longint unsigned response_token;
        longint unsigned response_sample_seq;
        bit derived_valid;
        bit [43:0] derived_s1_ppn;
        bit [43:0] derived_s2_ppn;
        bit [51:0] derived_gvpn;

        if (record == null || !record.is_waiting() || record.csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "issue-hit capture requires a WAITING UID record")
        end
        if (!find_completed_tlb_response_for_issue(record.lookup_key,
                                                   record.csr_snapshot,
                                                   record.uid_wait_start_sample_seq,
                                                   entry,
                                                   response_token,
                                                   response_sample_seq)) begin
            return 1'b0;
        end
        record.set_issue_hit_snapshot(entry, response_token,
                                      response_sample_seq);
        derive_tlb_request_fields(entry, record.vpn, record.s2xlate,
                                  record.csr_snapshot, derived_valid,
                                  derived_s1_ppn, derived_s2_ppn,
                                  derived_gvpn);
        record.request_derived_valid = derived_valid;
        record.request_s1_resolved_ppn = derived_s1_ppn;
        record.request_s2_resolved_ppn = derived_s2_ppn;
        record.request_gvpn = derived_gvpn;
        `uvm_info("COMMON_DATA",
                  $sformatf("freeze UID issue-hit snapshot uid=%0d vpn=0x%0h token=%0d response_sample=%0d generation=%0d",
                            record.uid, record.vpn, response_token,
                            response_sample_seq, entry.entry_generation),
                  UVM_LOW)
        return 1'b1;
    endfunction:capture_uid_issue_hit_snapshot

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
        return get_or_create_l2tlb_entry_by_req_with_snapshot(
            vpn, s2xlate, csr_snapshot, 1'b0, 0, request_key, entry_anchor_key,
            lookup_result, entry, created);
    endfunction:get_or_create_tlb_entry_by_req_with_snapshot

    function bit check_routed_ppn_intent_hit(input memblock_tlb_lookup_key_t key,
                                               input memblock_tlb_entry entry);
        bit [43:0] actual_ppn;
        if (!routed_ppn_intent_by_vpn.exists(key.vpn[37:0])) return 1'b1;
        if (entry == null || entry.s2_stage_active || entry.s1_level != 0 || entry.s1_pte_n)
            `uvm_fatal("L2TLB_ROUTED_INTENT", "narrow intent conflicts with existing translation shape")
        actual_ppn = entry.s1_resolved_ppn;
        if (!entry.s1_resolved_ppn_valid ||
            actual_ppn != routed_ppn_intent_by_vpn[key.vpn[37:0]].ppn)
            `uvm_fatal("L2TLB_ROUTED_INTENT", "narrow intent conflicts with existing physical translation")
        return 1'b1;
    endfunction:check_routed_ppn_intent_hit

    // 中文注释：L2TLB responder 专用 lookup。exact/range hit 保持完全只读，
    // 只有 miss build 在 canonical entry 插表前可按已冻结的 reuse policy 覆写最终 PPN。
    function bit get_or_create_l2tlb_entry_by_req_with_snapshot(
        input bit [37:0] vpn,
        input bit [1:0] s2xlate,
        input mmu_csr_runtime_state csr_snapshot,
        input bit ppn_reuse_en,
        input int unsigned ppn_reuse_wt,
        output memblock_tlb_lookup_key_t request_key,
        output memblock_tlb_lookup_key_t entry_anchor_key,
        output memblock_tlb_lookup_result_e lookup_result,
        output memblock_tlb_entry entry,
        output bit created,
        input longint unsigned pma_query_sample = 0);
        bit reused_ppn;

        if (csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "get_or_create_l2tlb_entry_by_req_with_snapshot got null csr_snapshot")
        end
        if (pma_query_sample != 0 &&
            !apply_pma_pmp_csr_writes_before_request(pma_query_sample)) return 1'b0;
        request_key = csr_snapshot.make_lookup_key({26'b0, vpn}, s2xlate);
        entry_anchor_key = '{default:'0};
        lookup_result = MEMBLOCK_TLB_LOOKUP_MISS_BUILD;
        if (has_tlb_entry(request_key)) begin
            entry = tlb_entry_by_key[request_key];
            if (entry.range_index_keys.size() == 0) begin
                `uvm_fatal("COMMON_DATA", "exact L2TLB hit has no registered range index ownership")
            end
            if (!check_routed_ppn_intent_hit(request_key, entry)) return 1'b0;
            entry.last_hit_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            entry_anchor_key = request_key;
            lookup_result = MEMBLOCK_TLB_LOOKUP_EXACT_HIT;
            created = 1'b0;
            return 1'b1;
        end
        if (find_tlb_range_hit_by_req(request_key, csr_snapshot,
                                      entry_anchor_key, entry)) begin
            if (!check_routed_ppn_intent_hit(request_key, entry)) return 1'b0;
            entry.last_hit_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            lookup_result = MEMBLOCK_TLB_LOOKUP_RANGE_HIT;
            created = 1'b0;
            return 1'b1;
        end
        entry = build_tlb_entry_for_key_with_csr(request_key, csr_snapshot, pma_query_sample);
        reused_ppn = 1'b0;
        if (ppn_reuse_en) begin
            if (routed_ppn_intent_by_vpn.exists(vpn))
                `uvm_fatal("L2TLB_ROUTED_INTENT", "narrow PPN intent cannot use completed PPN reuse")
            try_apply_l2tlb_ppn_reuse_to_new_entry(entry, ppn_reuse_wt, reused_ppn,
                                                csr_snapshot, pma_query_sample);
        end
        publish_live_tlb_entry(request_key, entry, csr_snapshot);
        entry_anchor_key = request_key;
        lookup_result = MEMBLOCK_TLB_LOOKUP_MISS_BUILD;
        created = 1'b1;
        if (reused_ppn) begin
            `uvm_info("COMMON_DATA",
                      $sformatf("L2TLB miss reuses completed PPN vpn=0x%0h s2xlate=%0d generation=%0d",
                                request_key.vpn, request_key.s2xlate,
                                entry.entry_generation), UVM_LOW)
        end
        return 1'b1;
    endfunction:get_or_create_l2tlb_entry_by_req_with_snapshot

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

    // 中文注释：预约同CSR owner的全HS fence，复用既有观测槽；所有校验先于状态修改。
    function bit arm_csr_mapping_fence(
        input memblock_control_owner_t owner,
        input memblock_sfence_payload_t payload);
        status_transaction status;
        if (!owner.valid || !active_control_barrier_valid ||
            active_control_barrier_uid != owner.uid || !is_valid_uid(owner.uid)) begin
            `uvm_fatal("CSR_MAPPING_FENCE", "mapping fence has no active CSR barrier")
            return 1'b0;
        end
        status = get_status(owner.uid);
        if (!memblock_sync_pkg::csr_special_sequence_active ||
            !memblock_control_owner_equal(status.control_owner, owner) ||
            status.control_kind != MEMBLOCK_CONTROL_KIND_CSR ||
            status.control_state != MEMBLOCK_CONTROL_STATE_CSR_CONFIG_PENDING ||
            !status.control_action_enqueued || status.control_sfence_c0_armed ||
            control_sfence_c0_observation.valid || control_sfence_effective_observation.valid ||
            !payload.valid || !payload.ignore_addr || !payload.ignore_id ||
            payload.hv || payload.hg || payload.target_stage != MEMBLOCK_SFENCE_TARGET_HS_S1 ||
            memblock_sync_pkg::l2tlb_reset_active()) begin
            `uvm_fatal("CSR_MAPPING_FENCE", "mapping fence arm owner/state/payload mismatch")
            return 1'b0;
        end
        status.control_expected_sfence_valid = 1'b1;
        status.control_expected_sfence = payload;
        status.control_sfence_c0_armed = 1'b1;
        status.control_sfence_pre_drive_event_seq = memblock_sync_pkg::last_allocated_l2tlb_event_seq;
        status.control_l2tlb_reset_epoch_at_arm = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
        return 1'b1;
    endfunction:arm_csr_mapping_fence

    function bit finish_csr_mapping_fence(
        input memblock_control_owner_t owner,
        input longint unsigned event_seq,
        input longint unsigned reset_epoch,
        output memblock_control_sfence_observation_t observation);
        status_transaction status;
        observation = '{default:'0};
        if (!owner.valid || !active_control_barrier_valid ||
            active_control_barrier_uid != owner.uid || !is_valid_uid(owner.uid)) return 1'b0;
        status = get_status(owner.uid);
        if (!memblock_control_owner_equal(status.control_owner, owner) ||
            status.control_kind != MEMBLOCK_CONTROL_KIND_CSR ||
            status.control_state != MEMBLOCK_CONTROL_STATE_CSR_CONFIG_PENDING ||
            !status.control_sfence_c0_armed ||
            reset_epoch != status.control_l2tlb_reset_epoch_at_arm ||
            reset_epoch != memblock_sync_pkg::get_l2tlb_current_reset_epoch() ||
            event_seq <= status.control_sfence_pre_drive_event_seq) return 1'b0;
        if (!try_consume_control_sfence_effective_observation(
                owner, event_seq, reset_epoch, observation)) return 1'b0;
        status.control_expected_sfence_valid = 1'b0;
        status.control_expected_sfence = '{default:'0};
        status.control_sfence_c0_armed = 1'b0;
        status.control_sfence_pre_drive_event_seq = 0;
        status.control_sfence_c0_event_seq = 0;
        status.control_l2tlb_reset_epoch_at_arm = 0;
        return 1'b1;
    endfunction:finish_csr_mapping_fence

    // 抽象职责：adapter在C0固化匹配的owner观测，不改control_state。
    // 普通SFENCE由service消费；CSR mapping由child等待C4后消费并清预约再sendover。
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
        if (!((status.control_kind == MEMBLOCK_CONTROL_KIND_SFENCE &&
               status.control_state inside {MEMBLOCK_CONTROL_STATE_SFENCE_REQ,
                   MEMBLOCK_CONTROL_STATE_SFENCE_SENDOVER,
                   MEMBLOCK_CONTROL_STATE_WAIT_L2TLB_FLUSH_EFFECTIVE}) ||
              (status.control_kind == MEMBLOCK_CONTROL_KIND_CSR &&
               memblock_sync_pkg::csr_special_sequence_active &&
               status.control_state == MEMBLOCK_CONTROL_STATE_CSR_CONFIG_PENDING)) ||
            !status.control_owner.valid || !status.control_expected_sfence_valid ||
            !status.control_sfence_c0_armed ||
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
        unregister_tlb_parent_occupancy_index(key, entry);
        tlb_entry_by_key.delete(key);
        delete_completed_tlb_entry_by_anchor_key(key, reason);
    endfunction:delete_live_tlb_entry_by_anchor_key

    // Abstract responsibility: perform only C4-or-later live-entry deletion
    // for queued fences in the current reset epoch. It scans the bounded live
    // entry map, never main/status tables or L2TLB pending response tokens.
    function int unsigned apply_due_sfence_invalidate(
        input longint unsigned dut_sample_seq,
        input longint unsigned current_reset_epoch);
        memblock_pending_sfence_invalidate_t pending;
        memblock_tlb_lookup_key_t delete_keys[$];
        memblock_tlb_lookup_key_t completed_delete_keys[$];
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
            completed_delete_keys.delete();
            foreach (completed_tlb_entry_by_key[key]) begin
                if (sfence_match_entry(pending.payload, key,
                                       completed_tlb_entry_by_key[key])) begin
                    completed_delete_keys.push_back(key);
                end
            end
            foreach (completed_delete_keys[idx]) begin
                delete_completed_tlb_entry_by_anchor_key(
                    completed_delete_keys[idx], "SFENCE/HFENCE C4");
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
        cancel_all_later_split_tlb_records("runtime_reset");
        sfence_invalidate_pending_q.delete();
        tlb_anchor_keys_by_range_key.delete();
        tlb_entry_by_key.delete();
        tlb_parent_occupancy_count.delete();
        tlb_parent_owner_set.delete();
        completed_tlb_anchor_keys_by_range_key.delete();
        completed_tlb_entry_by_key.delete();
        completed_tlb_response_token_by_key.delete();
        completed_tlb_response_sample_by_key.delete();
    endfunction:clear_dispatch_l2tlb_live_entries

    // 中文注释：清除 completed-response PPN FIFO，不触碰 live entry、range index、UID 或 token。
    // 调用者只能是 testcase table reset 或已去重的 runtime reset epoch；普通 SFENCE/HFENCE 不调用本函数。
    function void clear_l2tlb_ppn_history();
        l2tlb_ppn_history_q.delete();
    endfunction:clear_l2tlb_ppn_history

    // 中文注释：在真实 response completion 边界记录一个 FIFO 位置。
    // fault/PMA AF/无法推导最终 PPN 仍写 invalid record，确保 history 表示最近 M 次 completion 而非仅成功翻译。
    function void record_l2tlb_completed_ppn_history(
        input bit [1:0] s2xlate,
        input bit request_derived_valid,
        input bit [43:0] request_s1_resolved_ppn,
        input bit [43:0] request_s2_resolved_ppn,
        input memblock_tlb_entry entry,
        input longint unsigned response_token,
        input longint unsigned complete_sample_seq,
        input int unsigned history_size);
        memblock_l2tlb_ppn_history_record_t record;

        // request_token 从 0 开始分配；0 是首个真实 request 的合法 provenance，
        // 不能把它当成未初始化标记。completion sample 和容量仍必须非零。
        if (entry == null || complete_sample_seq == 0 || history_size == 0) begin
            `uvm_fatal("COMMON_DATA",
                       "PPN history record requires entry, non-zero sample and non-zero capacity")
        end
        if (entry.s2xlate != s2xlate) begin
            `uvm_fatal("COMMON_DATA", "PPN history record s2xlate does not match frozen entry")
        end
        record = '{default:'0};
        record.s2xlate = s2xlate;
        record.response_token = response_token;
        record.complete_sample_seq = complete_sample_seq;
        if (!entry.has_effective_fault() && !entry.pmaAF && request_derived_valid) begin
            case (s2xlate)
                2'd0,
                2'd1: begin
                    record.ppn = request_s1_resolved_ppn;
                    record.ppn_valid = 1'b1;
                end
                2'd2,
                2'd3: begin
                    record.ppn = request_s2_resolved_ppn;
                    record.ppn_valid = 1'b1;
                end
                default: begin
                    `uvm_fatal("COMMON_DATA",
                               $sformatf("unsupported PPN history s2xlate=%0d", s2xlate))
                end
            endcase
        end
        l2tlb_ppn_history_q.push_back(record);
        while (l2tlb_ppn_history_q.size() > history_size) begin
            void'(l2tlb_ppn_history_q.pop_front());
        end
        `uvm_info("COMMON_DATA",
                  $sformatf("record L2TLB completed PPN token=%0d sample=%0d s2xlate=%0d valid=%0d ppn=0x%0h history=%0d/%0d",
                            response_token, complete_sample_seq, s2xlate,
                            record.ppn_valid, record.ppn,
                            l2tlb_ppn_history_q.size(), history_size), UVM_LOW)
    endfunction:record_l2tlb_completed_ppn_history

    // 中文注释：仅在 miss build 的新 entry 尚未插表时扫描有界 history FIFO。
    // 该函数不读取主表，也不改已有 entry；WT=0 和无候选路径不消耗新的随机数。
    function void try_apply_l2tlb_ppn_reuse_to_new_entry(
        ref memblock_tlb_entry entry,
        input int unsigned ppn_reuse_wt,
        output bit reused_ppn,
        input mmu_csr_runtime_state csr_snapshot = null,
        input longint unsigned pma_query_sample = 0);
        tlb_map_builder builder;
        int unsigned candidate_indices[$];
        int unsigned candidate_count;
        int unsigned candidate_choice;
        bit choose_reuse;

        reused_ppn = 1'b0;
        if (entry == null) begin
            `uvm_fatal("COMMON_DATA", "PPN reuse got null new entry")
        end
        if (ppn_reuse_wt > 100) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("PPN reuse weight=%0d is outside [0:100]", ppn_reuse_wt))
        end
        if (l2tlb_ppn_history_q.size() == 0 || ppn_reuse_wt == 0) begin
            return;
        end
        builder = tlb_map_builder::type_id::create("tlb_builder_ppn_reuse");
        if (builder == null) begin
            `uvm_fatal("COMMON_DATA", "failed to create PPN reuse builder")
        end
        if (csr_snapshot != null)
            configure_builder_routed_context(builder, entry.lookup_key, csr_snapshot, pma_query_sample);
        if (!builder.can_apply_reused_final_ppn(entry, '0)) begin
            return;
        end
        foreach (l2tlb_ppn_history_q[idx]) begin
            if (l2tlb_ppn_history_q[idx].ppn_valid &&
                builder.can_apply_reused_final_ppn(entry,
                                                   l2tlb_ppn_history_q[idx].ppn) &&
                builder.check_reused_routed_ppn(entry,
                                                l2tlb_ppn_history_q[idx].ppn)) begin
                candidate_indices.push_back(idx);
            end
        end
        candidate_count = candidate_indices.size();
        if (candidate_count == 0) begin
            return;
        end
        choose_reuse = 1'b1;
        if (ppn_reuse_wt < 100) begin
            if (!std::randomize(choose_reuse) with {
                    choose_reuse dist {1'b1 := ppn_reuse_wt,
                                       1'b0 := 100 - ppn_reuse_wt};
                }) begin
                `uvm_fatal("COMMON_DATA", "failed to randomize L2TLB PPN reuse decision")
            end
        end
        if (!choose_reuse) begin
            return;
        end
        candidate_choice = 0;
        if (candidate_count > 1 && !std::randomize(candidate_choice) with {
                candidate_choice < candidate_count;
            }) begin
            `uvm_fatal("COMMON_DATA", "failed to randomize L2TLB PPN reuse candidate")
        end
        builder.apply_reused_final_ppn(
            l2tlb_ppn_history_q[candidate_indices[candidate_choice]].ppn, entry);
        reused_ppn = 1'b1;
    endfunction:try_apply_l2tlb_ppn_reuse_to_new_entry

    function longint unsigned allocate_tlb_entry_generation();
        if (next_tlb_entry_generation == '1 || next_tlb_entry_generation + 1 == 0)
            `uvm_fatal("COMMON_DATA", "TLB entry generation overflow");
        next_tlb_entry_generation++;
        return next_tlb_entry_generation;
    endfunction:allocate_tlb_entry_generation

    function memblock_tlb_entry build_tlb_entry_for_key_with_csr(
        input memblock_tlb_lookup_key_t key,
        input mmu_csr_runtime_state csr_snapshot,
        input longint unsigned pma_query_sample = 0);
        memblock_tlb_entry entry;
        memblock_tlb_entry prepared_entry;
        tlb_map_builder    builder;
        bit [4:0]          allowed_shape_mask;
        bit                pte_n_overridden;
        bit                pte_g_overridden;
        bit                forced_4k;

        if (csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "build_tlb_entry_for_key_with_csr got null csr_snapshot")
        end
        builder = tlb_map_builder::type_id::create("tlb_builder_by_key");
        if (builder == null) begin
            `uvm_fatal("COMMON_DATA", "failed to create tlb_map_builder")
        end
        configure_builder_routed_context(builder, key, csr_snapshot, pma_query_sample);
        prepared_entry = builder.prepare_tlb_payload_context(key, csr_snapshot);
        allowed_shape_mask = derive_allowed_tlb_shape_mask_with_global_fallback(
            key.vpn, prepared_entry, pte_g_overridden);
        entry = builder.finalize_tlb_payload_shape_and_payload(
            prepared_entry, key, csr_snapshot, allowed_shape_mask,
            pte_n_overridden, forced_4k, 1'b0,
            MEMBLOCK_TLB_COVERAGE_4K);
        if (entry == null) begin
            `uvm_fatal("COMMON_DATA", "builder returned null payload entry")
        end
        entry.lookup_key = key;
        entry.s2xlate = key.s2xlate;
        entry.entry_generation = allocate_tlb_entry_generation();
        if (pte_n_overridden) pte_n_override_count++;
        if (pte_g_overridden) pte_g_override_count++;
        if (forced_4k) begin
            forced_4k_fallback_count++;
            if (forced_4k_fallback_count <= MEMBLOCK_TLB_FALLBACK_WARNING_LIMIT) begin
                `uvm_warning("L2TLB_SHAPE_FALLBACK",
                             $sformatf("force ordinary 4KB after occupancy/address filtering vpn=0x%0h s2xlate=%0d occurrence=%0d",
                                       key.vpn, key.s2xlate,
                                       forced_4k_fallback_count))
            end
        end
        return entry;
    endfunction:build_tlb_entry_for_key_with_csr

    function void report_l2tlb_parent_occupancy_stats();
        `uvm_info("L2TLB_PARENT_OCCUPANCY_STATS",
                  $sformatf("lookup=%0d block_64k=%0d block_2m=%0d block_1g=%0d block_512g=%0d n_override=%0d g_override=%0d forced_4k=%0d owner_peak=%0d",
                            parent_lookup_count,
                            parent_block_64k_count,
                            parent_block_2m_count,
                            parent_block_1g_count,
                            parent_block_512g_count,
                            pte_n_override_count,
                            pte_g_override_count,
                            forced_4k_fallback_count,
                            parent_owner_peak_count),
                  UVM_LOW)
    endfunction:report_l2tlb_parent_occupancy_stats

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
    function bit check_l2tlb_uid_registration_open(input string caller);
        if (memblock_sync_pkg::l2tlb_release_admission_close_requested ||
            memblock_sync_pkg::l2tlb_release_admission_closed ||
            memblock_sync_pkg::l2tlb_release_closing) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("%s called after L2TLB release admission seal requested=%0d closed=%0d closing=%0d",
                                 caller,
                                 memblock_sync_pkg::l2tlb_release_admission_close_requested,
                                 memblock_sync_pkg::l2tlb_release_admission_closed,
                                 memblock_sync_pkg::l2tlb_release_closing))
            return 1'b0;
        end
        return 1'b1;
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
                     MEMBLOCK_ROB_SIZE) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("L2TLB WAITING logical UID bucket exceeds ROB active-window capacity caller=%s size=%0d max=%0d",
                                 caller,
                                 uid_waiting_by_vpn_s2xlate[shape_key].size(),
                                 MEMBLOCK_ROB_SIZE))
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
                MEMBLOCK_ROB_SIZE) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("L2TLB WAITING logical UID capacity overflow uid=%0d size=%0d max=%0d",
                                     uid,
                                     uid_waiting_by_vpn_s2xlate[shape_key].size(),
                                     MEMBLOCK_ROB_SIZE))
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

    function bit register_uid_tlb_record_on_issue(input memblock_uid_t uid);
        main_control_transaction main_tr;
        status_transaction       status;
        mmu_csr_runtime_state    snapshot;
        memblock_uid_tlb_record  planned_record;
        bit                      record_needs_commit;
        pma_pmp_uid_context_plan_t context_plan;
        bit [51:0]               vpn;
        bit [1:0]                s2xlate;
        bit                      is_hypervisor_inst;
        longint unsigned         current_sample;

        if (!is_valid_uid(uid)) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("register_uid_tlb_record_on_issue got invalid uid=%0d main_trans_num=%0d",
                                 uid, main_trans_num))
            return 1'b0;
        end
        if (!check_l2tlb_uid_registration_open(
                "register_uid_tlb_record_on_issue")) begin
            return 1'b0;
        end
        current_sample = memblock_sync_pkg::peek_current_dut_global_sample();
        if (current_sample == 0) begin
            `uvm_fatal("COMMON_DATA",
                       "UID TLB registration requires a published DUT global sample")
            return 1'b0;
        end
        if (main_table_by_uid[uid] == null || status_by_uid[uid] == null) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("issue registration missing transaction/status uid=%0d",
                                 uid))
            return 1'b0;
        end
        main_tr = main_table_by_uid[uid];
        status = status_by_uid[uid];
        if (mmu_csr_state == null) begin
            `uvm_fatal("COMMON_DATA",
                       "issue registration requires initialized MMU CSR state")
            return 1'b0;
        end
        snapshot = mmu_csr_runtime_state::type_id::create(
            $sformatf("issue_csr_snapshot_uid_%0d", uid));
        if (snapshot == null) begin
            `uvm_fatal("COMMON_DATA", "failed to allocate issue CSR snapshot")
            return 1'b0;
        end
        snapshot.copy_from(mmu_csr_state);
        vpn = {14'b0, main_tr.vaddr[49:12]};
        is_hypervisor_inst = is_hypervisor_tlb_inst(main_tr);
        s2xlate = snapshot.expected_s2xlate(is_hypervisor_inst);
        if (!plan_uid_tlb_record_context(uid, vpn, s2xlate,
                                         is_hypervisor_inst, snapshot,
                                         planned_record,
                                         record_needs_commit)) begin
            return 1'b0;
        end
        // 中文注释：PMA/PMP context 必须在首次真实 issue 边界冻结。TLB hit
        // 可能没有 L2TLB request-fire，因此不能只在 responder 回调中建立该快照。
        // 先回放严格早于本 sample 的 CSR 写，再用当前 dynamic_epoch 保存表代数。
        if (seq_csr_common::get_pma_pmp_model_en() &&
            !memblock_sync_pkg::is_pbmt_rf_scenario_active()) begin
            if (pma_pmp_model == null ||
                !pma_pmp_model.plan_uid_context_capture(
                    uid,
                    status.dynamic_epoch,
                    snapshot.priv_dmode,
                    snapshot.priv_debug,
                    1'b0,
                    1'b0,
                    '0,
                    snapshot.update_seq,
                    current_sample,
                    context_plan)) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("failed to plan PMA/PMP issue context uid=%0d epoch=%0d sample=%0d",
                                     uid, status.dynamic_epoch, current_sample))
                return 1'b0;
            end
            if (!apply_pma_pmp_csr_writes_before_request(current_sample)) return 1'b0;
            // Replay may change the generation frozen in the context, so
            // rebuild the plan after the replay and before any commit.
            if (!pma_pmp_model.plan_uid_context_capture(
                    uid, status.dynamic_epoch,
                    snapshot.priv_dmode, snapshot.priv_debug,
                    1'b0, 1'b0, '0, snapshot.update_seq,
                    current_sample, context_plan)) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("failed to finalize PMA/PMP issue context uid=%0d epoch=%0d sample=%0d",
                                     uid, status.dynamic_epoch, current_sample))
                return 1'b0;
            end
        end
        if (record_needs_commit) begin
            commit_uid_tlb_record_context_prechecked(uid, planned_record);
        end
        if (seq_csr_common::get_pma_pmp_model_en() &&
            !memblock_sync_pkg::is_pbmt_rf_scenario_active()) begin
            pma_pmp_model.commit_uid_context_plan_prechecked(context_plan);
        end
        return 1'b1;
    endfunction:register_uid_tlb_record_on_issue

    function bit plan_uid_tlb_record_context(
        input memblock_uid_t uid,
        input bit [51:0] vpn,
        input bit [1:0] s2xlate,
        input bit is_hypervisor_inst,
        input mmu_csr_runtime_state csr_snapshot,
        output memblock_uid_tlb_record planned_record,
        output bit record_needs_commit);
        memblock_uid_tlb_record record;
        memblock_uid_t candidate_uid;
        memblock_uid_tlb_record candidate_record;
        memblock_uid_tlb_wait_shape_key_t shape_key;
        longint unsigned current_sample;
        int unsigned member_count;

        planned_record = null;
        record_needs_commit = 1'b0;
        if (!is_valid_uid(uid)) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("plan_uid_tlb_record_context invalid uid=%0d main_trans_num=%0d",
                                 uid, main_trans_num))
            return 1'b0;
        end
        if (!check_l2tlb_uid_registration_open(
                "plan_uid_tlb_record_context")) return 1'b0;
        if (csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA",
                       "plan_uid_tlb_record_context got null CSR snapshot")
            return 1'b0;
        end
        current_sample = memblock_sync_pkg::peek_current_dut_global_sample();
        if (current_sample == 0) begin
            `uvm_fatal("COMMON_DATA",
                       "plan_uid_tlb_record_context requires a published DUT sample")
            return 1'b0;
        end
        record = null;
        if (uid_tlb_record_by_uid.exists(uid)) record = uid_tlb_record_by_uid[uid];
        if (record != null && record.record_valid) begin
            case (record.uid_tlb_wait_state)
                memblock_uid_tlb_record::MEMBLOCK_UID_TLB_WAITING: begin
                    if (record.csr_snapshot == null ||
                        !uid_tlb_waiting_context_matches(
                            uid, record, vpn, s2xlate,
                            is_hypervisor_inst, csr_snapshot)) begin
                        `uvm_fatal("COMMON_DATA",
                                   $sformatf("uid=%0d attempts to replace active L2TLB WAITING instance",
                                             uid))
                        return 1'b0;
                    end
                    shape_key = make_uid_tlb_wait_shape_key(record.vpn,
                                                             record.s2xlate);
                    member_count = 0;
                    if (uid_waiting_by_vpn_s2xlate.exists(shape_key)) begin
                        foreach (uid_waiting_by_vpn_s2xlate[shape_key][idx]) begin
                            candidate_uid = uid_waiting_by_vpn_s2xlate[shape_key][idx];
                            if (!uid_tlb_record_by_uid.exists(candidate_uid) ||
                                uid_tlb_record_by_uid[candidate_uid] == null ||
                                !uid_tlb_record_by_uid[candidate_uid].record_valid ||
                                !uid_tlb_record_by_uid[candidate_uid].is_waiting()) begin
                                `uvm_fatal("COMMON_DATA",
                                           "stale WAITING index during issue planning")
                                return 1'b0;
                            end
                            candidate_record = uid_tlb_record_by_uid[candidate_uid];
                            if (make_uid_tlb_wait_shape_key(candidate_record.vpn,
                                                           candidate_record.s2xlate) !=
                                shape_key) begin
                                `uvm_fatal("COMMON_DATA",
                                           "WAITING index shape mismatch during issue planning")
                                return 1'b0;
                            end
                            if (candidate_uid == uid) member_count++;
                        end
                    end
                    if (member_count != 1) begin
                        `uvm_fatal("COMMON_DATA",
                                   $sformatf("expected one WAITING index member uid=%0d got=%0d",
                                             uid, member_count))
                        return 1'b0;
                    end
                    planned_record = record;
                    return 1'b1;
                end
                memblock_uid_tlb_record::MEMBLOCK_UID_TLB_COMPLETED: begin
                    if (!record.pte_valid) begin
                        `uvm_fatal("COMMON_DATA",
                                   $sformatf("COMPLETED uid=%0d has pte_valid=0", uid))
                        return 1'b0;
                    end
                end
                memblock_uid_tlb_record::MEMBLOCK_UID_TLB_CANCELED: begin
                    if (record.pte_valid) begin
                        `uvm_fatal("COMMON_DATA",
                                   $sformatf("CANCELED uid=%0d has pte_valid=1", uid))
                        return 1'b0;
                    end
                end
                default: begin
                    `uvm_fatal("COMMON_DATA",
                               $sformatf("uid=%0d invalid L2TLB wait state=%0d",
                                         uid, record.uid_tlb_wait_state))
                    return 1'b0;
                end
            endcase
        end
        if (record != null && record.uid_tlb_wait_epoch == '1) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("uid=%0d L2TLB wait epoch overflow", uid))
            return 1'b0;
        end
        shape_key = make_uid_tlb_wait_shape_key(vpn, s2xlate);
        member_count = 0;
        if (uid_waiting_by_vpn_s2xlate.exists(shape_key)) begin
            foreach (uid_waiting_by_vpn_s2xlate[shape_key][idx]) begin
                candidate_uid = uid_waiting_by_vpn_s2xlate[shape_key][idx];
                if (!uid_tlb_record_by_uid.exists(candidate_uid) ||
                    uid_tlb_record_by_uid[candidate_uid] == null ||
                    !uid_tlb_record_by_uid[candidate_uid].record_valid ||
                    !uid_tlb_record_by_uid[candidate_uid].is_waiting()) begin
                    `uvm_fatal("COMMON_DATA",
                               "stale WAITING index during context planning")
                    return 1'b0;
                end
                candidate_record = uid_tlb_record_by_uid[candidate_uid];
                if (make_uid_tlb_wait_shape_key(candidate_record.vpn,
                                                candidate_record.s2xlate) !=
                    shape_key) begin
                    `uvm_fatal("COMMON_DATA",
                               "WAITING index shape mismatch during context planning")
                    return 1'b0;
                end
                if (candidate_uid == uid) member_count++;
            end
            if (member_count != 0 ||
                uid_waiting_by_vpn_s2xlate[shape_key].size() >=
                    MEMBLOCK_ROB_SIZE) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("cannot add WAITING index uid=%0d members=%0d size=%0d",
                                     uid, member_count,
                                     uid_waiting_by_vpn_s2xlate[shape_key].size()))
                return 1'b0;
            end
        end
        planned_record = memblock_uid_tlb_record::type_id::create(
            $sformatf("planned_uid_tlb_record_%0d", uid));
        if (planned_record == null) begin
            `uvm_fatal("COMMON_DATA", "failed to allocate planned UID TLB record")
            return 1'b0;
        end
        if (record != null) planned_record.uid_tlb_wait_epoch =
            record.uid_tlb_wait_epoch;
        planned_record.init_context(uid, vpn, s2xlate, is_hypervisor_inst,
                                    csr_snapshot, current_sample);
        if (!capture_uid_issue_hit_snapshot(planned_record) &&
            planned_record.issue_hit_snapshot_valid) begin
            `uvm_fatal("COMMON_DATA", "invalid planned issue-hit snapshot state")
            return 1'b0;
        end
        record_needs_commit = 1'b1;
        return 1'b1;
    endfunction:plan_uid_tlb_record_context

    function void commit_uid_tlb_record_context_prechecked(
        input memblock_uid_t uid,
        input memblock_uid_tlb_record planned_record);
        memblock_uid_tlb_record record;
        memblock_uid_tlb_wait_shape_key_t shape_key;

        if (!uid_tlb_record_by_uid.exists(uid) ||
            uid_tlb_record_by_uid[uid] == null) begin
            record = planned_record;
            uid_tlb_record_by_uid[uid] = record;
        end else begin
            record = uid_tlb_record_by_uid[uid];
            record.commit_context_prechecked(planned_record);
        end
        shape_key = make_uid_tlb_wait_shape_key(record.vpn, record.s2xlate);
        uid_waiting_by_vpn_s2xlate[shape_key].push_back(uid);
    endfunction:commit_uid_tlb_record_context_prechecked

    function bit update_uid_tlb_record_context(input memblock_uid_t uid,
                                               input bit [51:0] vpn,
                                               input bit [1:0] s2xlate,
                                               input bit is_hypervisor_inst,
                                               input mmu_csr_runtime_state csr_snapshot);
        memblock_uid_tlb_record planned_record;
        bit record_needs_commit;

        if (!plan_uid_tlb_record_context(uid, vpn, s2xlate,
                                         is_hypervisor_inst, csr_snapshot,
                                         planned_record,
                                         record_needs_commit)) begin
            return 1'b0;
        end
        if (record_needs_commit) begin
            commit_uid_tlb_record_context_prechecked(uid, planned_record);
        end
        return 1'b1;
    endfunction:update_uid_tlb_record_context

    // 抽象职责：回放严格早于 request-fire 的 CSR write，使该 request 使用与
    // DUT 同边沿顺序一致的旧/新 PMA/PMP generation。该函数不扫描主表，只消费
    // monitor FIFO 的连续前缀；同 sample 写入留给后续 request。
    function bit prepare_pma_pmp_csr_replay(
        input longint unsigned published_sample,
        output memblock_pma_pmp_replay_ticket ticket
    );
        memblock_sync_pkg::dispatch_raw_pma_pmp_csr_write_t event_item;
        memblock_sync_pkg::dispatch_raw_pma_pmp_csr_write_t effective_events[$];
        longint unsigned previous_sample;
        longint unsigned raw_size64;
        ticket = null;
        if (published_sample == 0 || pma_pmp_model == null ||
            !memblock_sync_pkg::pma_pmp_csr_epoch_is_current(
                pma_pmp_csr_replay_reset_epoch)) return 1'b0;
        ticket = new();
        ticket.reset_epoch = pma_pmp_csr_replay_reset_epoch;
        ticket.service_revision = pma_pmp_csr_replay_service_revision;
        ticket.raw_revision = memblock_sync_pkg::get_raw_pma_pmp_csr_write_revision();
        previous_sample = pma_pmp_last_applied_csr_sample;
        raw_size64 = $unsigned(memblock_sync_pkg::raw_pma_pmp_csr_write_size());
        // 高频路径仅扫描 pending 成熟前缀和第一个 future head，不扫描 UID/history。
        for (longint unsigned idx = 0; idx < raw_size64; idx++) begin
            if (!memblock_sync_pkg::peek_raw_pma_pmp_csr_write_at(idx, event_item) ||
                !event_item.valid || event_item.sample_seq == 0 || event_item.sample_time == 0 ||
                !memblock_sync_pkg::pma_pmp_csr_epoch_is_current(event_item.reset_epoch) ||
                event_item.reset_epoch != ticket.reset_epoch ||
                event_item.sample_seq <= previous_sample ||
                event_item.sample_seq > memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - 2) return 1'b0;
            if (published_sample < event_item.sample_seq ||
                published_sample - event_item.sample_seq < 2) break;
            ticket.events.push_back(event_item);
            previous_sample = event_item.sample_seq;
            // model history 使用 RTL effective sample，raw/audit 保留外部 capture sample。
            event_item.sample_seq += 2;
            effective_events.push_back(event_item);
        end
        if (ticket.events.size() == 0) return 1'b1;
        return pma_pmp_model.prepare_csr_replay_batch(effective_events, ticket.model_ticket);
    endfunction:prepare_pma_pmp_csr_replay


    function bit commit_pma_pmp_csr_replay(input memblock_pma_pmp_replay_ticket ticket);
        longint unsigned event_count64;
        if (ticket == null) return 1'b0;
        event_count64 = $unsigned(ticket.events.size());
        if (ticket == null || ticket.consumed || ticket.events.size() == 0 ||
            ticket.reset_epoch != pma_pmp_csr_replay_reset_epoch ||
            !memblock_sync_pkg::pma_pmp_csr_epoch_is_current(ticket.reset_epoch) ||
            ticket.service_revision != pma_pmp_csr_replay_service_revision ||
            !memblock_sync_pkg::pma_pmp_csr_revision_can_advance(
                pma_pmp_csr_replay_service_revision) ||
            pma_pmp_csr_applied_count >
                memblock_sync_pkg::MEMBLOCK_PMA_PMP_MAX_U64 - event_count64) return 1'b0;
        if (!memblock_sync_pkg::precheck_raw_pma_pmp_csr_write_prefix(ticket.raw_revision, ticket.events) ||
            !pma_pmp_model.precheck_csr_replay_commit(ticket.model_ticket)) return 1'b0;
        // 此后无等待、无可失败检查；model snapshot 已在 prepare 分配。
        pma_pmp_model.commit_csr_replay_batch(ticket.model_ticket);
        memblock_sync_pkg::commit_raw_pma_pmp_csr_write_prefix(event_count64);
        pma_pmp_last_applied_csr_sample = ticket.events[$].sample_seq;
        pma_pmp_csr_applied_count += event_count64;
        pma_pmp_csr_replay_service_revision++;
        ticket.consumed = 1'b1;
        foreach (ticket.events[idx]) begin
            `uvm_info("PMA_PMP_CSR_REPLAY",
                $sformatf("capture=%0d effective=%0d epoch=%0d addr=0x%0h data=0x%0h outcome=%0d pmp=%0d pma=%0d applied=%0d",
                    ticket.events[idx].sample_seq, ticket.events[idx].sample_seq + 2,
                    ticket.reset_epoch, ticket.events[idx].addr, ticket.events[idx].data,
                    ticket.model_ticket.audit_rows[idx].outcome,
                    ticket.model_ticket.audit_rows[idx].pmp_generation_after,
                    ticket.model_ticket.audit_rows[idx].pma_generation_after,
                    pma_pmp_csr_applied_count), UVM_LOW)
        end
        return 1'b1;
    endfunction:commit_pma_pmp_csr_replay

    function bit apply_pma_pmp_csr_writes_before_request(
        input longint unsigned request_sample_seq
    );
        memblock_pma_pmp_replay_ticket ticket;
        if (request_sample_seq == 0 ||
            !prepare_pma_pmp_csr_replay(request_sample_seq, ticket)) begin
            `uvm_fatal("COMMON_DATA", "PMA/PMP request-bound replay prepare failed")
            return 1'b0;
        end
        if (ticket.events.size() == 0) return 1'b1;
        if (!commit_pma_pmp_csr_replay(ticket)) begin
            `uvm_fatal("COMMON_DATA", "PMA/PMP request-bound replay commit failed")
            return 1'b0;
        end
        return 1'b1;
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
    function bit mark_waiting_uid_records_on_request_fire(
        input memblock_tlb_lookup_key_t key,
        input longint unsigned sample_seq,
        output int unsigned marked_count);
        int          idx;
        memblock_uid_t uid;
        memblock_uid_t candidate_uids[$];
        memblock_uid_tlb_record record;
        memblock_uid_tlb_wait_shape_key_t shape_key;
        memblock_tlb_lookup_key_t request_key;
        memblock_tlb_lookup_key_t request_candidate_key;
        memblock_tlb_lookup_key_t candidate_key;
        memblock_sync_pkg::dispatch_raw_csr_t request_raw_csr;
        mmu_csr_runtime_state request_csr_snapshot;
        pma_pmp_uid_context_plan_t context_plans[$];
        pma_pmp_uid_context_plan_t context_plan;
        longint unsigned reserved_access_seq;

        marked_count = 0;
        if (memblock_sync_pkg::l2tlb_release_admission_close_requested ||
            memblock_sync_pkg::l2tlb_release_admission_closed ||
            memblock_sync_pkg::l2tlb_release_closing) begin
            `uvm_fatal("COMMON_DATA",
                       "mark_waiting_uid_records_on_request_fire called after L2TLB release seal")
            return 1'b0;
        end
        if (sample_seq == 0) begin
            `uvm_fatal("COMMON_DATA",
                       "mark_waiting_uid_records_on_request_fire requires non-zero sample_seq")
            return 1'b0;
        end
        if (sample_seq > memblock_sync_pkg::peek_current_dut_global_sample()) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("mark_waiting_uid_records_on_request_fire got future sample=%0d latest=%0d key vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d",
                                 sample_seq,
                                 memblock_sync_pkg::peek_current_dut_global_sample(),
                                 key.vpn, key.asid, key.vmid, key.s2xlate))
            return 1'b0;
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
            return 1'b0;
        end
        request_csr_snapshot = mmu_csr_runtime_state::type_id::create(
            $sformatf("uid_request_fire_csr_%0d", sample_seq));
        if (request_csr_snapshot == null) begin
            `uvm_fatal("COMMON_DATA",
                       "failed to allocate L2TLB request-fire C-2 CSR snapshot")
            return 1'b0;
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
            return 1'b0;
        end

        if (!apply_pma_pmp_csr_writes_before_request(sample_seq)) return 1'b0;

        reserved_access_seq = pma_pmp_model.next_access_seq;
        shape_key = make_uid_tlb_wait_shape_key(key.vpn, key.s2xlate);
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
                    return 1'b0;
                end
                record = uid_tlb_record_by_uid[uid];
                if (record.uid != uid || record.vpn != key.vpn ||
                    record.s2xlate != key.s2xlate) begin
                    `uvm_fatal("COMMON_DATA",
                               $sformatf("L2TLB WAITING index shape mismatch uid=%0d",
                                         uid))
                    return 1'b0;
                end
                if (record.csr_snapshot == null) begin
                    `uvm_fatal("COMMON_DATA",
                               $sformatf("WAITING uid=%0d has null request CSR snapshot",
                                         uid))
                    return 1'b0;
                end
                // The ROB-bounded logical-UID bucket is only a candidate
                // filter. First
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
                    if (status_by_uid[uid] == null || pma_pmp_model == null ||
                        !pma_pmp_model.plan_uid_context_capture_at_seq(
                            uid, status_by_uid[uid].dynamic_epoch,
                            request_csr_snapshot.priv_dmode,
                            request_csr_snapshot.priv_debug,
                            1'b0, 1'b0, '0,
                            request_csr_snapshot.update_seq, sample_seq,
                            reserved_access_seq, context_plan)) begin
                        `uvm_fatal("COMMON_DATA",
                                   $sformatf("cannot freeze PMA/PMP context uid=%0d sample=%0d",
                                             uid, sample_seq))
                        return 1'b0;
                    end
                    candidate_uids.push_back(uid);
                    context_plans.push_back(context_plan);
                    reserved_access_seq = context_plan.next_access_seq_after;
                end
            end
        end

        // Mutation commit begins only after every candidate can freeze its
        // PMA/PMP context. No fatal-capable validation remains below.
        foreach (candidate_uids[candidate_idx]) begin
            uid = candidate_uids[candidate_idx];
            record = uid_tlb_record_by_uid[uid];
            record.mark_request_fire(sample_seq);
            pma_pmp_model.commit_uid_context_plan_prechecked(
                context_plans[candidate_idx]);
            marked_count++;
        end

        if (marked_count == 0) begin
            `uvm_info("COMMON_DATA",
                      $sformatf("no unmarked WAITING UID matches L2TLB request fire key vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d sample=%0d; allow duplicate/prefetch/no-UID request",
                                key.vpn, key.asid, key.vmid, key.s2xlate, sample_seq),
                      UVM_LOW)
        end
        return 1'b1;
    endfunction:mark_waiting_uid_records_on_request_fire

    // Compatibility API for the existing responder call site.  key is frozen
    // from that request's C-2 CSR snapshot before this helper is invoked.
    function bit mark_uid_tlb_record_request_fire(
        input memblock_tlb_lookup_key_t key,
        input longint unsigned sample_seq,
        output int unsigned marked_count);
        return mark_waiting_uid_records_on_request_fire(
            key, sample_seq, marked_count);
    endfunction:mark_uid_tlb_record_request_fire

    function bit resolve_and_plan_pbmt_rf_request_fire(
        input memblock_tlb_lookup_key_t key,
        input longint unsigned sample_seq,
        input memblock_pbmt_rf_audit_slot_t prepared_slot,
        ref memblock_pbmt_rf_audit_slot_t audit[2],
        output memblock_uid_t resolved_uid,
        output mmu_csr_runtime_state request_csr_snapshot,
        output memblock_uid_tlb_record resolved_record,
        output pma_pmp_uid_context_plan_t context_plan);
        memblock_uid_t candidates[$];
        memblock_uid_t uid;
        memblock_uid_t expected_uid;
        memblock_uid_tlb_record record;
        memblock_uid_tlb_wait_shape_key_t shape_key;
        memblock_tlb_lookup_key_t request_key;
        memblock_tlb_lookup_key_t candidate_key;
        memblock_sync_pkg::dispatch_raw_csr_t request_raw_csr;
        status_transaction status;
        main_control_transaction main_tr;
        int slot_index;

        resolved_uid = '0;
        request_csr_snapshot = null;
        resolved_record = null;
        context_plan = '{default:'0};
        if (!memblock_sync_pkg::is_pbmt_rf_scenario_active() ||
            sample_seq == 0 || !prepared_slot.valid) begin
            return 1'b0;
        end
        if (!memblock_sync_pkg::get_l2tlb_request_csr_history(
                sample_seq, request_raw_csr) || !request_raw_csr.valid) begin
            return 1'b0;
        end
        request_csr_snapshot = mmu_csr_runtime_state::type_id::create(
            $sformatf("pbmt_rf_request_csr_%0d", sample_seq));
        if (request_csr_snapshot == null) begin
            return 1'b0;
        end
        request_csr_snapshot.reset();
        request_csr_snapshot.update_from_raw_csr(request_raw_csr);
        request_key = request_csr_snapshot.make_lookup_key({12'b0, key.vpn},
                                                            key.s2xlate);
        if (request_key != key) begin
            return 1'b0;
        end

        shape_key = make_uid_tlb_wait_shape_key(key.vpn, key.s2xlate);
        if (uid_waiting_by_vpn_s2xlate.exists(shape_key)) begin
            foreach (uid_waiting_by_vpn_s2xlate[shape_key][idx]) begin
                uid = uid_waiting_by_vpn_s2xlate[shape_key][idx];
                if (!uid_tlb_record_by_uid.exists(uid) ||
                    uid_tlb_record_by_uid[uid] == null) begin
                    continue;
                end
                record = uid_tlb_record_by_uid[uid];
                if (!record.record_valid || !record.is_waiting() ||
                    record.uid != uid || record.vpn != key.vpn ||
                    record.s2xlate != key.s2xlate ||
                    record.uid_tlb_first_request_fire_sample_seq != 0 ||
                    record.csr_snapshot == null) begin
                    continue;
                end
                candidate_key = record.csr_snapshot.make_lookup_key(
                    record.vpn, record.s2xlate);
                if (candidate_key == key) begin
                    candidates.push_back(uid);
                end
            end
        end
        if (candidates.size() != 1) begin
            `uvm_fatal("PBMT_RF_BIND",
                       $sformatf("strict request-fire expected one UID key vpn=0x%0h sample=%0d candidates=%0d",
                                 key.vpn, sample_seq, candidates.size()))
            return 1'b0;
        end

        resolved_uid = candidates[0];
        expected_uid = prepared_slot.slot == PBMT_RF_SLOT_B ? 0 : 1;
        slot_index = int'(prepared_slot.slot);
        record = uid_tlb_record_by_uid[resolved_uid];
        status = get_status(resolved_uid);
        main_tr = get_main_transaction(resolved_uid);
        if (resolved_uid != expected_uid || prepared_slot.uid != expected_uid ||
            prepared_slot.request_key != key || audit[slot_index].valid ||
            !status.active || status.terminal_done ||
            !pma_pmp_model.can_capture_uid_context(resolved_uid,
                                                    status.dynamic_epoch)) begin
            return 1'b0;
        end
        if ((prepared_slot.slot == PBMT_RF_SLOT_B &&
             (main_tr.op_class != MEMBLOCK_OP_CLASS_STORE ||
              memblock_sync_pkg::get_pbmt_rf_phase() != memblock_sync_pkg::PBMT_RF_B_RUNTIME_READY ||
              sample_seq < memblock_sync_pkg::get_pbmt_rf_b_ready_sample())) ||
            (prepared_slot.slot == PBMT_RF_SLOT_C &&
             (main_tr.op_class != MEMBLOCK_OP_CLASS_INT_LOAD ||
              memblock_sync_pkg::get_pbmt_rf_phase() != memblock_sync_pkg::PBMT_RF_C_C2_READY_ACK ||
              sample_seq <= memblock_sync_pkg::get_pbmt_rf_c_ready_sample()))) begin
            return 1'b0;
        end
        for (int idx = 0; idx < 2; idx++) begin
            if (audit[idx].valid &&
                (audit[idx].uid == resolved_uid ||
                 audit[idx].request_token == prepared_slot.request_token)) begin
                return 1'b0;
            end
        end

        if (record != uid_tlb_record_by_uid[resolved_uid] ||
            !record.is_waiting() ||
            record.uid_tlb_first_request_fire_sample_seq != 0 ||
            !pma_pmp_model.plan_uid_context_capture(
                resolved_uid, status.dynamic_epoch,
                request_csr_snapshot.priv_dmode,
                request_csr_snapshot.priv_debug,
                1'b0, 1'b0, '0,
                request_csr_snapshot.update_seq, sample_seq,
                context_plan)) begin
            return 1'b0;
        end
        resolved_record = record;
        return 1'b1;
    endfunction:resolve_and_plan_pbmt_rf_request_fire

    function void commit_pbmt_rf_request_fire_prechecked(
        input memblock_uid_tlb_record resolved_record,
        input longint unsigned sample_seq,
        input pma_pmp_uid_context_plan_t context_plan,
        input memblock_pbmt_rf_audit_slot_t prepared_slot,
        ref memblock_pbmt_rf_audit_slot_t audit[2]);
        resolved_record.commit_request_fire_prechecked(sample_seq);
        pma_pmp_model.commit_uid_context_plan_prechecked(context_plan);
        audit[int'(prepared_slot.slot)] = prepared_slot;
    endfunction:commit_pbmt_rf_request_fire_prechecked

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
                memblock_tlb_entry::derive_allstage_lookup_shape(
                    entry.s1_level, entry.s1_pte_n,
                    entry.s2_level, entry.s2_pte_n,
                    allstage_level, allstage_n);
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
        if (uid == 31 && target == MEMBLOCK_ISSUE_TARGET_STD) begin
            `uvm_info("UID31_STD_TRACE",
                      $sformatf("delete STD replay=%0d match=%0d queue_before=%0d queued/dispatched/wb/active_sq=%0d/%0d/%0d/%0d",
                                replay_seq, match_replay_seq, std_issue_q.size(),
                                status_by_uid[uid].queued_std, status_by_uid[uid].std_dispatched,
                                status_by_uid[uid].std_writeback, status_by_uid[uid].active_sq_mapped), UVM_LOW)
        end
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
        if (uid == 31) begin
            `uvm_info("UID31_STD_TRACE",
                      $sformatf("remove UID queues load/sta/std=%0d/%0d/%0d queued/dispatched/wb/active_sq=%0d/%0d/%0d/%0d",
                                load_issue_q.size(), sta_issue_q.size(), std_issue_q.size(),
                                status_by_uid[uid].queued_std, status_by_uid[uid].std_dispatched,
                                status_by_uid[uid].std_writeback, status_by_uid[uid].active_sq_mapped), UVM_LOW)
        end
        delete_issue_queue_entry(MEMBLOCK_ISSUE_TARGET_LOAD, uid, 0, 1'b0);
        delete_issue_queue_entry(MEMBLOCK_ISSUE_TARGET_STA, uid, 0, 1'b0);
        delete_issue_queue_entry(MEMBLOCK_ISSUE_TARGET_STD, uid, 0, 1'b0);
        if (is_valid_uid(uid) && status_by_uid[uid] != null) begin
            status_by_uid[uid].queued_load = 1'b0;
            status_by_uid[uid].queued_sta  = 1'b0;
            status_by_uid[uid].queued_std  = 1'b0;
        end
    endfunction:remove_uid_from_issue_queues

    // 抽象职责：fault UID进入异常提交边界后，撤销该动态实例尚未真正发射的
    // issue/replay工作，但保留其LQ/SQ owner、fault证据和“不重发”语义。该helper
    // 可在commit、fault redirect和terminal retire三个边界幂等调用；它不修改LSQ
    // cancel计数、owner map或dynamic_epoch。
    function void quiesce_fault_uid_pending_work(input memblock_uid_t uid,
                                                  input bit quiesce_std = 1'b1);
        status_transaction status;
        int unsigned issue_q_before;
        int unsigned issue_q_after;

        check_uid(uid, "quiesce_fault_uid_pending_work");
        status = get_status(uid);
        if (!status.fault && !status.exception_pending &&
            !status.load_fault && !status.sta_fault && !status.std_fault) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("quiesce_fault_uid_pending_work got non-fault uid=%0d", uid))
        end
        issue_q_before = load_issue_q.size() + sta_issue_q.size() + std_issue_q.size();
        delete_issue_queue_entry(MEMBLOCK_ISSUE_TARGET_LOAD, uid, 0, 1'b0);
        delete_issue_queue_entry(MEMBLOCK_ISSUE_TARGET_STA, uid, 0, 1'b0);
        status.queued_load = 1'b0;
        status.queued_sta  = 1'b0;
        if (quiesce_std) begin
            delete_issue_queue_entry(MEMBLOCK_ISSUE_TARGET_STD, uid, 0, 1'b0);
            status.queued_std = 1'b0;
        end
        release_ptw_wait_replay(uid);
        status.replay_pending      = 1'b0;
        status.replay_target_load  = 1'b0;
        status.replay_target_sta   = 1'b0;
        status.replay_target_std   = 1'b0;
        issue_q_after = load_issue_q.size() + sta_issue_q.size() + std_issue_q.size();
        if (issue_q_before != issue_q_after) begin
            `uvm_info("COMMON_DATA",
                      $sformatf("quiesce fault uid=%0d quiesce_std=%0d removed pending issue items=%0d load_q=%0d sta_q=%0d std_q=%0d",
                                uid,
                                quiesce_std,
                                issue_q_before - issue_q_after,
                                load_issue_q.size(),
                                sta_issue_q.size(),
                                std_issue_q.size()),
                      UVM_LOW)
        end
    endfunction:quiesce_fault_uid_pending_work

    function void push_ptw_wait_replay(input memblock_uid_t uid,
                                       input memblock_issue_target_e target,
                                       input int unsigned issue_epoch,
                                       input int unsigned replay_seq,
                                       input longint unsigned start_cycle);
        memblock_ptw_wait_replay_t wait_item;
        status_transaction          status;

        check_uid(uid, "push_ptw_wait_replay");
        status = get_status(uid);
        // 中文注释：fault/redirect 与同 batch 的旧 replay 可能交错到达。先在
        // PTW wait queue 入口做当前 identity 校验，避免 mark_replay_pending() 尚未
        // 执行前把 stale item 再插回队列，进而阻塞 terminal drain。
        if (!status.active || status.issue_killed || status.fault ||
            status.exception_pending || status.terminal_done ||
            status.redirect_pending || status.flushed ||
            !target_dispatched(status, target) ||
            status.get_target_issue_epoch(target) != issue_epoch ||
            !target_replay_seq_match(status, target, replay_seq)) begin
            `uvm_info("PTW_WAIT_REPLAY",
                      $sformatf("drop stale PTW wait uid=%0d target=%0d issue_epoch=%0d replay_seq=%0d",
                                uid, target, issue_epoch, replay_seq),
                      UVM_LOW)
            return;
        end
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
        bit effective_flush_itself;

        effective_flush_itself = redirect.is_vls_exception ?
                                 memblock_redirect_effective_level(redirect) :
                                 redirect.flush_itself;
        for (int idx = ptw_wait_replay_q.size(); idx > 0; idx--) begin
            status_transaction status;
            memblock_uid_t uid;
            memblock_rob_key_t rob_key;
            bit same_rob;
            bit different_flag;
            bit compare_value;
            bit need_flush;

            uid = ptw_wait_replay_q[idx - 1].uid;
            if (!is_valid_uid(uid) || status_by_uid[uid] == null) begin
                ptw_wait_replay_q.delete(idx - 1);
                continue;
            end
            // Redirect range precheck has already approved all mutable state.
            // Keep this wrapper-tail cleanup commit-only: do not call get_status()
            // or ROB helpers with report-capable key validation after range
            // mutation has started.  The range precheck already validated every
            // active UID/key, so reproduce rob_need_flush() with pure bit logic.
            status = status_by_uid[uid];
            rob_key.flag = status.robIdx_flag;
            rob_key.value = status.robIdx_value;
            same_rob = {rob_key.flag, rob_key.value} ==
                       {redirect.rob_key.flag, redirect.rob_key.value};
            different_flag = rob_key.flag ^ redirect.rob_key.flag;
            compare_value = rob_key.value > redirect.rob_key.value;
            need_flush = (effective_flush_itself && same_rob) ||
                         (different_flag ^ compare_value);
            if (!status.active || need_flush) begin
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
        longint unsigned pma_pmp_produced_count;
        longint unsigned pma_pmp_applied_count;
        longint unsigned pma_pmp_pending_count;

        if (control_barrier_prefix_external_provenance_valid) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("end_test_check residual control provenance uid=%0d origin_old/new=%0d/%0d",
                                 control_barrier_prefix_external_provenance_uid,
                                 control_barrier_prefix_external_origin_old_end_uid,
                                 control_barrier_prefix_external_origin_new_end_uid))
            return;
        end

        if (memblock_sync_pkg::raw_monitor_queue_size() != 0) begin
            `uvm_error("COMMON_DATA",
                       $sformatf("raw monitor queues are not drained at end_test_check: size=%0d",
                                 memblock_sync_pkg::raw_monitor_queue_size()))
        end
        // PMA/PMP CSR FIFO 由逐拍成熟 service 维护；结束检查只审计守恒和残留，禁止补消费。
        void'(precheck_pma_pmp_csr_end_state(
            pma_pmp_produced_count, pma_pmp_applied_count,
            pma_pmp_pending_count));
        if (pma_pmp_applied_count != pma_pmp_produced_count) begin
            `uvm_error("COMMON_DATA", $sformatf("PMA/PMP CSR conservation failed produced=%0d applied=%0d",
                pma_pmp_produced_count, pma_pmp_applied_count))
        end
        if (pma_pmp_pending_count != 0) begin
            `uvm_error("COMMON_DATA",
                       $sformatf("PMA/PMP CSR write FIFO is not drained at end_test_check: size=%0d",
                                 pma_pmp_pending_count))
        end
        if (main_trans_num == 0) begin
            memblock_sync_pkg::dispatch_monitor_capture_en = 1'b0;
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
            if (status_by_uid[uid].uncache_retry_boost ||
                status_by_uid[uid].uncache_retry_count != 0 ||
                status_by_uid[uid].uncache_retry_identity_valid ||
                status_by_uid[uid].uncache_retry_has_dynamic_epoch) begin
                `uvm_error("UNCACHE_RETRY_BOOST",
                           $sformatf("uid=%0d leaked retry boost at end_test_check boost=%0d count=%0d identity=%0d has_dyn=%0d dyn=%0d rob=%0d/%0d",
                                     uid,
                                     status_by_uid[uid].uncache_retry_boost,
                                     status_by_uid[uid].uncache_retry_count,
                                     status_by_uid[uid].uncache_retry_identity_valid,
                                     status_by_uid[uid].uncache_retry_has_dynamic_epoch,
                                     status_by_uid[uid].uncache_retry_dynamic_epoch,
                                     status_by_uid[uid].uncache_retry_rob_key.flag,
                                     status_by_uid[uid].uncache_retry_rob_key.value))
            end
            if (main_table_by_uid[uid] != null &&
                is_control_op_class(main_table_by_uid[uid].op_class) &&
                !assert_control_lifecycle_audit_complete(
                    uid, 1'b1, "end_test_check")) begin
                return;
            end
        end
        if (uid_by_active_rob.num() != 0 || uid_by_lq.num() != 0 || uid_by_sq.num() != 0) begin
            `uvm_error("COMMON_DATA", "active ROB/LQ/SQ mapping is not empty at end_test_check")
        end
        if (dispatch_progress.rob_commit_frontier_uid != main_trans_num) begin
            `uvm_error("COMMON_DATA",
                       $sformatf("ROB admission frontier did not reach main_trans_num: frontier=%0d main_trans_num=%0d",
                                 dispatch_progress.rob_commit_frontier_uid,
                                 main_trans_num))
        end
        if (redirect_deleted_owner_window_active ||
            redirect_deleted_lq_owner_by_key.num() != 0 ||
            redirect_deleted_sq_owner_by_key.num() != 0) begin
            `uvm_error("COMMON_DATA",
                       $sformatf("redirect-deleted owner window is not drained: active=%0d lq=%0d sq=%0d",
                                 redirect_deleted_owner_window_active,
                                 redirect_deleted_lq_owner_by_key.num(),
                                 redirect_deleted_sq_owner_by_key.num()))
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
        // 中文注释：最终检查必须先读取所有 raw queue；关闭 capture 只发生在全部
        // responder/drain 已完成之后，不能让最后一拍 monitor event 被静默丢弃。
        memblock_sync_pkg::dispatch_monitor_capture_en = 1'b0;
    endfunction:end_test_check


    function bit assert_control_lifecycle_audit_complete(
        input memblock_uid_t uid,
        input bit expect_released,
        input string caller
    );
        status_transaction status;

        if (!is_valid_uid(uid) || status_by_uid[uid] == null) begin
            `uvm_fatal("CONTROL_LIFECYCLE_AUDIT",
                       $sformatf("%s invalid uid=%0d", caller, uid))
            return 1'b0;
        end
        status = status_by_uid[uid];
        if (!control_lifecycle_action_matrix_complete(status) ||
            status.control_audit_terminal_count != 1 ||
            status.control_audit_barrier_release_count != expect_released ||
            status.control_action_generation != 1 ||
            (expect_released &&
             (status.active || status.control_owner.valid ||
              status.control_action_enqueued ||
              (active_control_barrier_valid && active_control_barrier_uid == uid)))) begin
            `uvm_fatal("CONTROL_LIFECYCLE_AUDIT",
                       $sformatf("%s incomplete uid=%0d kind=%0d generation=%0d create/bind=%0d/%0d csr=%0d/%0d/%0d sfence=%0d/%0d/%0d l2_assert=%0d/%0d/%0d l2_release=%0d/%0d/%0d terminal/release=%0d/%0d active/owner/action=%0d/%0d/%0d",
                                 caller, uid, status.control_kind,
                                 status.control_action_generation,
                                 status.control_audit_creation_count,
                                 status.control_audit_owner_bind_count,
                                 status.control_audit_csr_enqueue_count,
                                 status.control_audit_csr_start_count,
                                 status.control_audit_csr_completion_count,
                                 status.control_audit_sfence_enqueue_count,
                                 status.control_audit_sfence_start_count,
                                 status.control_audit_sfence_completion_count,
                                 status.control_audit_l2_assert_enqueue_count,
                                 status.control_audit_l2_assert_start_count,
                                 status.control_audit_l2_assert_completion_count,
                                 status.control_audit_l2_release_enqueue_count,
                                 status.control_audit_l2_release_start_count,
                                 status.control_audit_l2_release_completion_count,
                                 status.control_audit_terminal_count,
                                 status.control_audit_barrier_release_count,
                                 status.active, status.control_owner.valid,
                                 status.control_action_enqueued))
            return 1'b0;
        end
        if (expect_released) begin
            `uvm_info("CONTROL_LIFECYCLE_AUDIT",
                      $sformatf("event=final uid=%0d kind=%0d generation=%0d create/bind=1/1 csr=%0d/%0d/%0d sfence=%0d/%0d/%0d l2_assert=%0d/%0d/%0d l2_release=%0d/%0d/%0d terminal/release=1/1",
                                uid, status.control_kind,
                                status.control_action_generation,
                                status.control_audit_csr_enqueue_count,
                                status.control_audit_csr_start_count,
                                status.control_audit_csr_completion_count,
                                status.control_audit_sfence_enqueue_count,
                                status.control_audit_sfence_start_count,
                                status.control_audit_sfence_completion_count,
                                status.control_audit_l2_assert_enqueue_count,
                                status.control_audit_l2_assert_start_count,
                                status.control_audit_l2_assert_completion_count,
                                status.control_audit_l2_release_enqueue_count,
                                status.control_audit_l2_release_start_count,
                                status.control_audit_l2_release_completion_count), UVM_LOW)
        end
        return 1'b1;
    endfunction:assert_control_lifecycle_audit_complete



    function bit check_cancel_pending_aggregate_checked(input string caller);
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
                       $sformatf("%s pending cancel aggregate drift stored=%0d/%0d records=%0d/%0d",
                                 caller, pending_lq_cancel_count,
                                 pending_sq_cancel_count, expected_lq, expected_sq))
            return 1'b0;
        end
        return 1'b1;
    endfunction:check_cancel_pending_aggregate_checked



    function void clear_control_barrier_prefix_external_provenance();
        control_barrier_prefix_external_provenance_valid = 1'b0;
        control_barrier_prefix_external_provenance_uid = 0;
        control_barrier_prefix_external_origin_redirect = '{default:'0};
        control_barrier_prefix_external_origin_old_end_uid = 0;
        control_barrier_prefix_external_origin_new_end_uid = 0;
    endfunction:clear_control_barrier_prefix_external_provenance



    function void commit_cancel_waiting_uid_tlb_record_for_redirect_range(
        input memblock_uid_t uid
    );
        memblock_uid_tlb_record record;
        memblock_uid_tlb_wait_shape_key_t shape_key;

        if (!uid_tlb_record_by_uid.exists(uid) || uid_tlb_record_by_uid[uid] == null ||
            !uid_tlb_record_by_uid[uid].is_waiting()) begin
            return;
        end
        record = uid_tlb_record_by_uid[uid];
        shape_key = make_uid_tlb_wait_shape_key(record.vpn, record.s2xlate);
        record.mark_canceled();
        for (int idx = int'(uid_waiting_by_vpn_s2xlate[shape_key].size()) - 1;
             idx >= 0; idx--) begin
            if (uid_waiting_by_vpn_s2xlate[shape_key][idx] == uid) begin
                uid_waiting_by_vpn_s2xlate[shape_key].delete(idx);
            end
        end
        if (uid_waiting_by_vpn_s2xlate[shape_key].size() == 0) begin
            uid_waiting_by_vpn_s2xlate.delete(shape_key);
        end
    endfunction:commit_cancel_waiting_uid_tlb_record_for_redirect_range



    function void commit_capture_redirect_deleted_owner_for_redirect_range(
        input memblock_uid_t uid,
        input int record_idx
    );
        status_transaction status;
        memblock_redirect_deleted_owner_t owner;
        memblock_lq_key_t lq_key;
        memblock_sq_key_t sq_key;

        status = status_by_uid[uid];
        owner = '{default:'0};
        owner.valid = 1'b1;
        owner.uid = uid;
        owner.old_dynamic_epoch = status.dynamic_epoch;
        owner.old_activation_flush_epoch = status.active_instance_flush_epoch;
        owner.redirect_epoch = cancel_record_q[record_idx].redirect_epoch;
        owner.cancel_record_id = cancel_record_q[record_idx].cancel_record_id;
        if (status.active_lq_mapped) begin
            lq_key.flag = status.lqIdx_flag;
            lq_key.value = status.lqIdx_value;
            redirect_deleted_lq_owner_by_key[
                rob_order_util::lq_to_map_key(lq_key)] = owner;
        end
        if (status.active_sq_mapped) begin
            sq_key.flag = status.sqIdx_flag;
            sq_key.value = status.sqIdx_value;
            redirect_deleted_sq_owner_by_key[
                rob_order_util::sq_to_map_key(sq_key)] = owner;
        end
    endfunction:commit_capture_redirect_deleted_owner_for_redirect_range



    function void commit_control_barrier_prefix_external_provenance(
        input memblock_uid_t uid,
        input memblock_redirect_payload_t redirect,
        input memblock_uid_t old_end_uid,
        input memblock_uid_t new_end_uid
    );
        control_barrier_prefix_external_provenance_uid = uid;
        control_barrier_prefix_external_origin_redirect = redirect;
        control_barrier_prefix_external_origin_old_end_uid = old_end_uid;
        control_barrier_prefix_external_origin_new_end_uid = new_end_uid;
        control_barrier_prefix_external_provenance_valid = 1'b1;
    endfunction:commit_control_barrier_prefix_external_provenance


    function void commit_control_terminal_retire(
        input memblock_uid_t uid,
        input memblock_uid_t planned_terminal_done_uid
    );
        status_transaction     status;
        memblock_rob_key_t     rob_key;
        memblock_rob_map_key_t rob_map_key;

        status = status_by_uid[uid];
        rob_key.flag = status.robIdx_flag;
        rob_key.value = status.robIdx_value;
        rob_map_key = rob_order_util::rob_to_map_key(rob_key);

        status.uncache_retry_boost = 1'b0;
        status.uncache_retry_count = 0;
        status.uncache_retry_identity_valid = 1'b0;
        status.uncache_retry_rob_key = '{default:'0};
        status.uncache_retry_has_dynamic_epoch = 1'b0;
        status.uncache_retry_dynamic_epoch = 0;
        for (int idx = load_issue_q.size(); idx > 0; idx--) begin
            if (load_issue_q[idx - 1].uid == uid) load_issue_q.delete(idx - 1);
        end
        for (int idx = sta_issue_q.size(); idx > 0; idx--) begin
            if (sta_issue_q[idx - 1].uid == uid) sta_issue_q.delete(idx - 1);
        end
        for (int idx = std_issue_q.size(); idx > 0; idx--) begin
            if (std_issue_q[idx - 1].uid == uid) std_issue_q.delete(idx - 1);
        end
        status.queued_load = 1'b0;
        status.queued_sta = 1'b0;
        status.queued_std = 1'b0;
        for (int idx = ptw_wait_replay_q.size(); idx > 0; idx--) begin
            if (ptw_wait_replay_q[idx - 1].uid == uid) ptw_wait_replay_q.delete(idx - 1);
        end
        status.sta_late_fault_tombstone_q.delete();
        std_late_raw_tombstone_by_rob.delete(rob_map_key);
        uid_by_active_rob.delete(rob_map_key);
        status.success = 1'b1;
        status.terminal_done = 1'b1;
        dispatch_progress.terminal_done_uid = planned_terminal_done_uid;
        status.active = 1'b0;
        `uvm_info("COMMON_DATA",
                  $sformatf("control terminal retire uid=%0d success=%0d terminal_done=%0d rob=%0d/%0d frontier=%0d",
                            uid, status.success, status.terminal_done,
                            status.robIdx_flag, status.robIdx_value,
                            dispatch_progress.terminal_done_uid),
                  UVM_LOW)
    endfunction:commit_control_terminal_retire


    function void commit_csr_control_action(
        input memblock_csr_control_action_t action,
        input memblock_control_lifecycle_ticket_t audit_ticket
    );
        status_transaction status;

        status = status_by_uid[action.owner.uid];
        csr_control_action_q.push_back(action);
        status.control_action_enqueued = 1'b1;
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        record_control_lifecycle_event(audit_ticket);
        ->csr_control_action_available_ev;
    endfunction:commit_csr_control_action



    function void commit_l2_flush_release_request(
        input memblock_l2_flush_release_request_t request,
        input memblock_control_lifecycle_ticket_t audit_ticket
    );
        status_transaction status;

        status = status_by_uid[request.owner.uid];
        l2_flush_release_request_q.push_back(request);
        status.control_state = MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_RELEASE;
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        record_control_lifecycle_event(audit_ticket);
        ->csr_control_action_available_ev;
    endfunction:commit_l2_flush_release_request



    function void commit_note_lsq_cancel_for_redirect_range(
        input memblock_uid_t uid,
        input int record_idx
    );
        status_transaction status;
        memblock_lq_key_t lq_key;
        main_control_transaction main_tr;

        status = status_by_uid[uid];
        main_tr = main_table_by_uid[uid];
        if (status.active_lq_mapped) begin
            cancel_record_q[record_idx].software_cancel_lq_count += main_tr.numLsElem;
            pending_lq_cancel_count += main_tr.numLsElem;
        end
        if (status.active_sq_mapped) begin
            cancel_record_q[record_idx].software_cancel_sq_count += main_tr.numLsElem;
            pending_sq_cancel_count += main_tr.numLsElem;
        end
        status.lsq_cancel_accounted_epoch = cancel_record_q[record_idx].redirect_epoch;
        status.lsq_reservation_state = MEMBLOCK_LSQ_RESERVATION_CANCEL_ACCOUNTED;
    endfunction:commit_note_lsq_cancel_for_redirect_range


    function void commit_preserve_fault_uid_during_redirect(
        input memblock_uid_t uid,
        input memblock_redirect_payload_t redirect);
        status_transaction status;
        int record_idx;

        status = status_by_uid[uid];
        record_idx = find_cancel_record_index_by_id(active_cancel_record_id);
        commit_quiesce_fault_uid_for_redirect_range(uid);
        if (status.active_lq_mapped) begin
            commit_capture_redirect_deleted_owner_for_redirect_range(uid, record_idx);
            commit_note_lsq_cancel_for_redirect_range(uid, record_idx);
        end
        if (!fault_redirect_resource.valid) begin
            memblock_lq_key_t lq_key;
            memblock_sq_key_t sq_key;

            fault_redirect_resource = '{default:'0};
            fault_redirect_resource.valid = status.active_lq_mapped || status.active_sq_mapped;
            fault_redirect_resource.uid = uid;
            fault_redirect_resource.dynamic_epoch = status.dynamic_epoch;
            fault_redirect_resource.redirect_epoch = cancel_record_q[record_idx].redirect_epoch;
            fault_redirect_resource.cancel_record_id = cancel_record_q[record_idx].cancel_record_id;
            if (status.active_sq_mapped) begin
                sq_key.flag = status.sqIdx_flag;
                sq_key.value = status.sqIdx_value;
                fault_redirect_resource.is_lq = 1'b0;
                fault_redirect_resource.sq_key = sq_key;
                fault_redirect_resource.disposition = MEMBLOCK_FAULT_LSQ_DISPOSITION_SQ_CANDIDATE;
            end else if (status.active_lq_mapped) begin
                lq_key.flag = status.lqIdx_flag;
                lq_key.value = status.lqIdx_value;
                fault_redirect_resource.is_lq = 1'b1;
                fault_redirect_resource.lq_key = lq_key;
                fault_redirect_resource.disposition = MEMBLOCK_FAULT_LSQ_DISPOSITION_LQ_CANCEL_PENDING;
            end
        end
        if (status.active_lq_mapped) begin
            commit_release_uid_lq_mapping_for_redirect_range(uid);
        end
    endfunction:commit_preserve_fault_uid_during_redirect



    function void commit_quiesce_fault_uid_for_redirect_range(
        input memblock_uid_t uid
    );
        status_transaction status;

        status = status_by_uid[uid];
        commit_remove_uid_from_issue_queues_for_redirect_range(uid);
        release_ptw_wait_replay(uid);
        status.replay_pending = 1'b0;
        status.replay_target_load = 1'b0;
        status.replay_target_sta = 1'b0;
        status.replay_target_std = 1'b0;
    endfunction:commit_quiesce_fault_uid_for_redirect_range



    function void commit_release_uid_lq_mapping_for_redirect_range(
        input memblock_uid_t uid
    );
        status_transaction status;
        memblock_lq_key_t lq_key;

        status = status_by_uid[uid];
        if (!status.active_lq_mapped) begin
            return;
        end
        lq_key.flag = status.lqIdx_flag;
        lq_key.value = status.lqIdx_value;
        uid_by_lq.delete(rob_order_util::lq_to_map_key(lq_key));
        status.active_lq_mapped = 1'b0;
        status.lsq_deq = !status.active_sq_mapped;
        if (status.lsq_deq) begin
            status.clear_lsq_reservation_visibility();
        end
    endfunction:commit_release_uid_lq_mapping_for_redirect_range



    function void commit_release_uid_sq_mapping_for_redirect_range(
        input memblock_uid_t uid
    );
        status_transaction status;
        memblock_sq_key_t sq_key;

        status = status_by_uid[uid];
        if (!status.active_sq_mapped) begin
            return;
        end
        for (int idx = std_issue_q.size(); idx > 0; idx--) begin
            if (std_issue_q[idx - 1].uid == uid) begin
                std_issue_q.delete(idx - 1);
            end
        end
        status.queued_std = 1'b0;
        sq_key.flag = status.sqIdx_flag;
        sq_key.value = status.sqIdx_value;
        uid_by_sq.delete(rob_order_util::sq_to_map_key(sq_key));
        status.active_sq_mapped = 1'b0;
        status.lsq_deq = !status.active_lq_mapped;
        if (status.lsq_deq) begin
            status.clear_lsq_reservation_visibility();
        end
    endfunction:commit_release_uid_sq_mapping_for_redirect_range



    function void commit_remove_uid_from_issue_queues_for_redirect_range(
        input memblock_uid_t uid
    );
        for (int idx = load_issue_q.size(); idx > 0; idx--) begin
            if (load_issue_q[idx - 1].uid == uid) begin
                load_issue_q.delete(idx - 1);
            end
        end
        for (int idx = sta_issue_q.size(); idx > 0; idx--) begin
            if (sta_issue_q[idx - 1].uid == uid) begin
                sta_issue_q.delete(idx - 1);
            end
        end
        for (int idx = std_issue_q.size(); idx > 0; idx--) begin
            if (std_issue_q[idx - 1].uid == uid) begin
                std_issue_q.delete(idx - 1);
            end
        end
        status_by_uid[uid].queued_load = 1'b0;
        status_by_uid[uid].queued_sta = 1'b0;
        status_by_uid[uid].queued_std = 1'b0;
    endfunction:commit_remove_uid_from_issue_queues_for_redirect_range



    function void commit_reset_uid_after_older_redirect_for_redirect_range(
        input memblock_uid_t uid
    );
        status_transaction status;
        memblock_rob_key_t old_rob_key;
        int unsigned old_dynamic_epoch;
        bit inherit_retry_boost;

        status = status_by_uid[uid];
        old_rob_key = status.get_rob_key();
        old_dynamic_epoch = status.dynamic_epoch;
        inherit_retry_boost = status.uncache_retry_boost &&
                              status.uncache_retry_identity_valid &&
                              status.uncache_retry_rob_key == old_rob_key &&
                              status.uncache_retry_dynamic_epoch == old_dynamic_epoch;
        void'(clear_pending_store_fault(uid));
        status.mmio_tag_valid = 1'b0;
        status.is_mmio_load = 1'b0;
        status.is_mmio_store = 1'b0;
        status.mmio_tag_source = MEMBLOCK_MMIO_TAG_NONE;
        status.mmio_tag_dynamic_epoch = 0;
        status.enq = 1'b0;
        status.issue_ready = 1'b0;
        status.queued_load = 1'b0;
        status.queued_sta = 1'b0;
        status.queued_std = 1'b0;
        status.load_dispatched = 1'b0;
        status.sta_dispatched = 1'b0;
        status.std_dispatched = 1'b0;
        status.load_writeback = 1'b0;
        status.sta_writeback = 1'b0;
        status.std_writeback = 1'b0;
        status.load_issue_feedback_success = 1'b0;
        status.sta_issue_feedback_success = 1'b0;
        status.std_issue_feedback_success = 1'b0;
        status.clear_sta_late_fault_tombstones();
        status.first_split_access = '{default:'0};
        status.load_pass = 1'b0;
        status.sta_pass = 1'b0;
        status.std_pass = 1'b0;
        status.writeback = 1'b0;
        status.pass = 1'b0;
        status.issue_killed = 1'b1;
        status.exception_pending = 1'b0;
        status.replay_pending = 1'b0;
        status.replay_target_load = 1'b0;
        status.replay_target_sta = 1'b0;
        status.replay_target_std = 1'b0;
        status.rob_commit = 1'b0;
        status.lsq_deq = 1'b0;
        status.fault = 1'b0;
        status.load_fault = 1'b0;
        status.sta_fault = 1'b0;
        status.std_fault = 1'b0;
        status.exception_vec = '0;
        status.exception_vaddr = '0;
        status.exception_gpaddr = '0;
        status.active_instance_flush_epoch_valid = 1'b0;
        status.active_instance_flush_epoch = 0;
        status.clear_target_instance_flush_epochs();
        status.clear_lsq_reservation_visibility();
        status.redirect_pending = 1'b1;
        status.flushed = 1'b1;
        status.dynamic_epoch++;
        status.active = 1'b0;
        status.success = 1'b0;
        status.terminal_done = 1'b0;
        if (inherit_retry_boost) begin
            status.uncache_retry_dynamic_epoch = status.dynamic_epoch;
            status.uncache_retry_has_dynamic_epoch = 1'b1;
        end else begin
            status.uncache_retry_boost = 1'b0;
            status.uncache_retry_identity_valid = 1'b0;
            status.uncache_retry_rob_key = '{default:'0};
            status.uncache_retry_has_dynamic_epoch = 1'b0;
            status.uncache_retry_dynamic_epoch = 0;
        end
        status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    endfunction:commit_reset_uid_after_older_redirect_for_redirect_range



    function void commit_retire_active_uid_for_redirect_range(
        input memblock_uid_t uid
    );
        status_transaction status;
        memblock_rob_map_key_t rob_map_key;

        status = status_by_uid[uid];
        if (status.std_dispatched &&
            (status.fault || status.exception_pending || status.sta_fault)) begin
            memblock_std_late_raw_tombstone_t tombstone;
            rob_map_key = rob_order_util::rob_to_map_key(status.get_rob_key());
            if (!std_late_raw_tombstone_by_rob.exists(rob_map_key)) begin
                tombstone = '{default:'0};
                tombstone.valid = 1'b1;
                tombstone.uid = uid;
                tombstone.dynamic_epoch = status.dynamic_epoch;
                tombstone.redirect_epoch = memblock_sync_pkg::dispatch_flush_epoch;
                tombstone.expire_service_sample =
                    memblock_sync_pkg::get_dispatch_service_cycle() +
                    MEMBLOCK_REDIRECT_DELETED_OWNER_HOLD_SAMPLES;
                std_late_raw_tombstone_by_rob[rob_map_key] = tombstone;
            end
        end
        commit_remove_uid_from_issue_queues_for_redirect_range(uid);
        release_ptw_wait_replay(uid);
        status.clear_sta_late_fault_tombstones();
        uid_by_active_rob.delete(rob_order_util::rob_to_map_key(status.get_rob_key()));
        commit_release_uid_lq_mapping_for_redirect_range(uid);
        commit_release_uid_sq_mapping_for_redirect_range(uid);
        status.active = 1'b0;
    endfunction:commit_retire_active_uid_for_redirect_range



    function void commit_start_redirect_deleted_owner_window(input int record_idx);
        redirect_deleted_owner_window_active = 1'b1;
        redirect_deleted_owner_epoch = cancel_record_q[record_idx].redirect_epoch;
        redirect_deleted_owner_record_id = cancel_record_q[record_idx].cancel_record_id;
        redirect_deleted_owner_expire_sample =
            cancel_record_q[record_idx].redirect_sample_seq +
            MEMBLOCK_REDIRECT_DELETED_OWNER_HOLD_SAMPLES;
    endfunction:commit_start_redirect_deleted_owner_window



    function void commit_uid_for_redirect_reissue(input memblock_uid_t uid);
        status_transaction status;
        memblock_rob_key_t old_rob_key;
        int unsigned old_dynamic_epoch;
        int record_idx;

        status = status_by_uid[uid];
        old_rob_key = status.get_rob_key();
        old_dynamic_epoch = status.dynamic_epoch;
        record_idx = find_cancel_record_index_by_id(active_cancel_record_id);
        commit_capture_redirect_deleted_owner_for_redirect_range(uid, record_idx);
        commit_note_lsq_cancel_for_redirect_range(uid, record_idx);
        // 旧 pending response 仍持有记录句柄，必须在递增 epoch 前标记取消，供迟到 completion 精确丢弃。
        cancel_later_split_tlb_record_for_uid(uid, old_dynamic_epoch, "redirect_reissue");
        commit_cancel_waiting_uid_tlb_record_for_redirect_range(uid);
        // redirect命中的旧动态实例不再等待writeback/commit；pending fault 未 commit 时
        // 必须完整清理 fault 证据与所有 target 状态，之后从同一 UID 重新 admission。
        if ((status.fault || status.exception_pending || status.sta_fault || status.std_fault) &&
            !status.rob_commit) begin
            commit_release_uid_lq_mapping_for_redirect_range(uid);
            commit_release_uid_sq_mapping_for_redirect_range(uid);
            uid_by_active_rob.delete(rob_order_util::rob_to_map_key(status.get_rob_key()));
            status.active = 1'b0;
            commit_remove_uid_from_issue_queues_for_redirect_range(uid);
            commit_reset_uid_after_older_redirect_for_redirect_range(uid);
            return;
        end
        commit_retire_active_uid_for_redirect_range(uid);
        commit_reset_uid_after_older_redirect_for_redirect_range(uid);
    endfunction:commit_uid_for_redirect_reissue



    function bit control_lifecycle_action_matrix_complete(
        input status_transaction status
    );
        if (status.control_audit_creation_count != 1 ||
            status.control_audit_owner_bind_count != 1 ||
            status.control_action_generation != 1) begin
            return 1'b0;
        end
        case (status.control_kind)
            MEMBLOCK_CONTROL_KIND_CSR:
                return status.control_audit_csr_enqueue_count == 1 &&
                       status.control_audit_csr_start_count == 1 &&
                       status.control_audit_csr_completion_count == 1 &&
                       status.control_audit_sfence_enqueue_count == 0 &&
                       status.control_audit_sfence_start_count == 0 &&
                       status.control_audit_sfence_completion_count == 0 &&
                       status.control_audit_l2_assert_enqueue_count == 0 &&
                       status.control_audit_l2_assert_start_count == 0 &&
                       status.control_audit_l2_assert_completion_count == 0 &&
                       status.control_audit_l2_release_enqueue_count == 0 &&
                       status.control_audit_l2_release_start_count == 0 &&
                       status.control_audit_l2_release_completion_count == 0;
            MEMBLOCK_CONTROL_KIND_SFENCE:
                return status.control_audit_csr_enqueue_count == 0 &&
                       status.control_audit_csr_start_count == 0 &&
                       status.control_audit_csr_completion_count == 0 &&
                       status.control_audit_sfence_enqueue_count == 1 &&
                       status.control_audit_sfence_start_count == 1 &&
                       status.control_audit_sfence_completion_count == 1 &&
                       status.control_audit_l2_assert_enqueue_count == 0 &&
                       status.control_audit_l2_assert_start_count == 0 &&
                       status.control_audit_l2_assert_completion_count == 0 &&
                       status.control_audit_l2_release_enqueue_count == 0 &&
                       status.control_audit_l2_release_start_count == 0 &&
                       status.control_audit_l2_release_completion_count == 0;
            MEMBLOCK_CONTROL_KIND_CHECK_STORE:
                return status.control_audit_csr_enqueue_count == 0 &&
                       status.control_audit_csr_start_count == 0 &&
                       status.control_audit_csr_completion_count == 0 &&
                       status.control_audit_sfence_enqueue_count == 0 &&
                       status.control_audit_sfence_start_count == 0 &&
                       status.control_audit_sfence_completion_count == 0 &&
                       status.control_audit_l2_assert_enqueue_count == 1 &&
                       status.control_audit_l2_assert_start_count == 1 &&
                       status.control_audit_l2_assert_completion_count == 1 &&
                       status.control_audit_l2_release_enqueue_count == 1 &&
                       status.control_audit_l2_release_start_count == 1 &&
                       status.control_audit_l2_release_completion_count == 1;
            default: return 1'b0;
        endcase
    endfunction:control_lifecycle_action_matrix_complete



    function string control_lifecycle_event_name(
        input memblock_control_audit_event_e event_kind
    );
        case (event_kind)
            MEMBLOCK_CONTROL_AUDIT_CREATION: return "creation";
            MEMBLOCK_CONTROL_AUDIT_OWNER_BIND: return "owner_bind";
            MEMBLOCK_CONTROL_AUDIT_CSR_ENQUEUE: return "csr_enqueue";
            MEMBLOCK_CONTROL_AUDIT_CSR_START: return "csr_start";
            MEMBLOCK_CONTROL_AUDIT_CSR_COMPLETION: return "csr_completion";
            MEMBLOCK_CONTROL_AUDIT_SFENCE_ENQUEUE: return "sfence_enqueue";
            MEMBLOCK_CONTROL_AUDIT_SFENCE_START: return "sfence_start";
            MEMBLOCK_CONTROL_AUDIT_SFENCE_COMPLETION: return "sfence_completion";
            MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_ENQUEUE: return "l2_assert_enqueue";
            MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_START: return "l2_assert_start";
            MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_COMPLETION: return "l2_assert_completion";
            MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_ENQUEUE: return "l2_release_enqueue";
            MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_START: return "l2_release_start";
            MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_COMPLETION: return "l2_release_completion";
            MEMBLOCK_CONTROL_AUDIT_TERMINAL: return "terminal";
            MEMBLOCK_CONTROL_AUDIT_BARRIER_RELEASE: return "barrier_release";
            default: return "unknown";
        endcase
    endfunction:control_lifecycle_event_name



    function bit establish_control_barrier_prefix_external_provenance(
        input memblock_uid_t uid,
        input memblock_redirect_payload_t redirect,
        input memblock_uid_t old_end_uid,
        input memblock_uid_t new_end_uid
    );
        if (!precheck_control_barrier_prefix_external_provenance(
                uid, redirect, old_end_uid, new_end_uid)) begin
            return 1'b0;
        end
        commit_control_barrier_prefix_external_provenance(
            uid, redirect, old_end_uid, new_end_uid);
        return 1'b1;
    endfunction:establish_control_barrier_prefix_external_provenance



    function int unsigned get_control_lifecycle_event_count(
        input status_transaction status,
        input memblock_control_audit_event_e event_kind
    );
        case (event_kind)
            MEMBLOCK_CONTROL_AUDIT_CREATION: return status.control_audit_creation_count;
            MEMBLOCK_CONTROL_AUDIT_OWNER_BIND: return status.control_audit_owner_bind_count;
            MEMBLOCK_CONTROL_AUDIT_CSR_ENQUEUE: return status.control_audit_csr_enqueue_count;
            MEMBLOCK_CONTROL_AUDIT_CSR_START: return status.control_audit_csr_start_count;
            MEMBLOCK_CONTROL_AUDIT_CSR_COMPLETION: return status.control_audit_csr_completion_count;
            MEMBLOCK_CONTROL_AUDIT_SFENCE_ENQUEUE: return status.control_audit_sfence_enqueue_count;
            MEMBLOCK_CONTROL_AUDIT_SFENCE_START: return status.control_audit_sfence_start_count;
            MEMBLOCK_CONTROL_AUDIT_SFENCE_COMPLETION: return status.control_audit_sfence_completion_count;
            MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_ENQUEUE: return status.control_audit_l2_assert_enqueue_count;
            MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_START: return status.control_audit_l2_assert_start_count;
            MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_COMPLETION: return status.control_audit_l2_assert_completion_count;
            MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_ENQUEUE: return status.control_audit_l2_release_enqueue_count;
            MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_START: return status.control_audit_l2_release_start_count;
            MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_COMPLETION: return status.control_audit_l2_release_completion_count;
            MEMBLOCK_CONTROL_AUDIT_TERMINAL: return status.control_audit_terminal_count;
            MEMBLOCK_CONTROL_AUDIT_BARRIER_RELEASE: return status.control_audit_barrier_release_count;
            default: return '1;
        endcase
    endfunction:get_control_lifecycle_event_count



    function bit peek_control_sfence_effective_observation(
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
        return 1'b1;
    endfunction:peek_control_sfence_effective_observation



    function bit plan_terminal_done_uid_for_redirect_range(
        output memblock_uid_t planned_terminal_done_uid
    );
        memblock_uid_t uid;

        uid = dispatch_progress.terminal_done_uid;
        planned_terminal_done_uid = uid;
        while (uid < main_trans_num) begin
            if (status_by_uid[uid] == null) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("redirect terminal frontier references null status uid=%0d", uid))
                return 1'b0;
            end
            if (!status_by_uid[uid].terminal_done) begin
                break;
            end
            uid++;
        end
        planned_terminal_done_uid = uid;
        return 1'b1;
    endfunction:plan_terminal_done_uid_for_redirect_range



    function bit precheck_cancel_pending_aggregate_after_redirect_plan(
        input int record_idx,
        input int unsigned planned_record_lq_count,
        input int unsigned planned_record_sq_count,
        input string caller
    );
        int unsigned expected_lq;
        int unsigned expected_sq;
        int unsigned planned_pending_lq;
        int unsigned planned_pending_sq;

        if (record_idx < 0 || record_idx >= cancel_record_q.size() ||
            planned_record_lq_count < cancel_record_q[record_idx].software_cancel_lq_count ||
            planned_record_sq_count < cancel_record_q[record_idx].software_cancel_sq_count) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("%s invalid redirect cancel plan record=%0d planned=%0d/%0d current=%0d/%0d",
                                 caller, record_idx,
                                 planned_record_lq_count, planned_record_sq_count,
                                 record_idx >= 0 && record_idx < cancel_record_q.size() ?
                                     cancel_record_q[record_idx].software_cancel_lq_count : 0,
                                 record_idx >= 0 && record_idx < cancel_record_q.size() ?
                                     cancel_record_q[record_idx].software_cancel_sq_count : 0))
            return 1'b0;
        end
        expected_lq = 0;
        expected_sq = 0;
        foreach (cancel_record_q[idx]) begin
            if (cancel_record_q[idx].valid && !cancel_record_q[idx].software_applied) begin
                expected_lq += idx == record_idx ? planned_record_lq_count :
                                                  cancel_record_q[idx].software_cancel_lq_count;
                expected_sq += idx == record_idx ? planned_record_sq_count :
                                                  cancel_record_q[idx].software_cancel_sq_count;
            end
        end
        planned_pending_lq = pending_lq_cancel_count + planned_record_lq_count -
                             cancel_record_q[record_idx].software_cancel_lq_count;
        planned_pending_sq = pending_sq_cancel_count + planned_record_sq_count -
                             cancel_record_q[record_idx].software_cancel_sq_count;
        if (planned_pending_lq != expected_lq || planned_pending_sq != expected_sq) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("%s planned pending cancel aggregate drift planned=%0d/%0d records=%0d/%0d",
                                 caller, planned_pending_lq, planned_pending_sq,
                                 expected_lq, expected_sq))
            return 1'b0;
        end
        return 1'b1;
    endfunction:precheck_cancel_pending_aggregate_after_redirect_plan



    function bit precheck_control_barrier_prefix_external_provenance(
        input memblock_uid_t uid,
        input memblock_redirect_payload_t redirect,
        input memblock_uid_t old_end_uid,
        input memblock_uid_t new_end_uid
    );
        status_transaction status;
        memblock_rob_key_t rob_key;

        if (!is_valid_uid(uid) || status_by_uid[uid] == null ||
            main_table_by_uid[uid] == null) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("precheck control provenance invalid uid=%0d", uid))
            return 1'b0;
        end
        status = status_by_uid[uid];
        rob_key = status.get_rob_key();
        if (control_barrier_prefix_external_provenance_valid || old_end_uid == 0 ||
            uid != old_end_uid - 1 || uid < new_end_uid ||
            !active_control_barrier_valid || active_control_barrier_uid != uid ||
            !is_control_op_class(main_table_by_uid[uid].op_class) ||
            !status.active || !status.enq || status.terminal_done ||
            status.control_state != MEMBLOCK_CONTROL_STATE_WAIT_OLDER_ROB_COMMIT ||
            status.control_owner.valid || status.control_action_enqueued ||
            status.active_lq_mapped || status.active_sq_mapped ||
            !rob_order_util::rob_need_flush(rob_key, redirect)) begin
            `uvm_fatal("CONTROL_ADMISSION_PROVENANCE",
                       $sformatf("cannot establish control provenance uid=%0d old/new=%0d/%0d barrier=%0d/%0d state=%0d owner/action=%0d/%0d lq/sq=%0d/%0d",
                                 uid, old_end_uid, new_end_uid,
                                 active_control_barrier_valid, active_control_barrier_uid,
                                 status.control_state, status.control_owner.valid,
                                 status.control_action_enqueued,
                                 status.active_lq_mapped, status.active_sq_mapped))
            return 1'b0;
        end
        return 1'b1;
    endfunction:precheck_control_barrier_prefix_external_provenance



    function bit precheck_control_lifecycle_event(
        input memblock_uid_t uid,
        input memblock_control_audit_event_e event_kind,
        output memblock_control_lifecycle_ticket_t ticket
    );
        status_transaction status;
        int unsigned event_count;
        bit predecessor_ok;

        ticket = '{default:'0};
        if (!is_valid_uid(uid) || status_by_uid[uid] == null ||
            main_table_by_uid[uid] == null ||
            !is_control_op_class(main_table_by_uid[uid].op_class)) begin
            `uvm_fatal("CONTROL_LIFECYCLE_AUDIT",
                       $sformatf("event=%0s invalid control uid=%0d",
                                 control_lifecycle_event_name(event_kind), uid))
            return 1'b0;
        end
        status = status_by_uid[uid];
        event_count = get_control_lifecycle_event_count(status, event_kind);
        if (event_count != 0) begin
            `uvm_fatal("CONTROL_LIFECYCLE_AUDIT",
                       $sformatf("duplicate event=%0s uid=%0d kind=%0d generation=%0d count=%0d",
                                 control_lifecycle_event_name(event_kind), uid,
                                 status.control_kind, status.control_action_generation,
                                 event_count))
            return 1'b0;
        end
        predecessor_ok = 1'b0;
        case (event_kind)
            MEMBLOCK_CONTROL_AUDIT_CREATION:
                predecessor_ok = !status.active && !status.enq &&
                    status.control_audit_owner_bind_count == 0 &&
                    status.control_action_generation == 0 &&
                    !status.control_owner.valid;
            MEMBLOCK_CONTROL_AUDIT_OWNER_BIND:
                predecessor_ok = status.control_audit_creation_count == 1 &&
                    status.control_action_generation == 0 &&
                    !status.control_owner.valid && !status.control_action_enqueued &&
                    status.control_state == MEMBLOCK_CONTROL_STATE_WAIT_OLDER_ROB_COMMIT &&
                    !control_barrier_prefix_external_provenance_valid;
            MEMBLOCK_CONTROL_AUDIT_CSR_ENQUEUE:
                predecessor_ok = status.control_kind == MEMBLOCK_CONTROL_KIND_CSR &&
                    status.control_audit_owner_bind_count == 1 &&
                    status.control_state == MEMBLOCK_CONTROL_STATE_CSR_CONFIG_PENDING &&
                    status.control_owner.valid && !status.control_action_enqueued;
            MEMBLOCK_CONTROL_AUDIT_CSR_START:
                predecessor_ok = status.control_kind == MEMBLOCK_CONTROL_KIND_CSR &&
                    status.control_audit_csr_enqueue_count == 1 &&
                    status.control_owner.valid && status.control_action_enqueued;
            MEMBLOCK_CONTROL_AUDIT_CSR_COMPLETION:
                predecessor_ok = status.control_audit_csr_start_count == 1 &&
                    status.control_state == MEMBLOCK_CONTROL_STATE_WAIT_CSR_RUNTIME_SNAPSHOT;
            MEMBLOCK_CONTROL_AUDIT_SFENCE_ENQUEUE:
                predecessor_ok = status.control_kind == MEMBLOCK_CONTROL_KIND_SFENCE &&
                    status.control_audit_owner_bind_count == 1 &&
                    status.control_state == MEMBLOCK_CONTROL_STATE_SFENCE_REQ &&
                    status.control_owner.valid && !status.control_action_enqueued;
            MEMBLOCK_CONTROL_AUDIT_SFENCE_START:
                predecessor_ok = status.control_kind == MEMBLOCK_CONTROL_KIND_SFENCE &&
                    status.control_audit_sfence_enqueue_count == 1 &&
                    status.control_owner.valid && status.control_action_enqueued;
            MEMBLOCK_CONTROL_AUDIT_SFENCE_COMPLETION:
                predecessor_ok = status.control_audit_sfence_start_count == 1 &&
                    status.control_state == MEMBLOCK_CONTROL_STATE_WAIT_L2TLB_FLUSH_EFFECTIVE;
            MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_ENQUEUE:
                predecessor_ok = status.control_kind == MEMBLOCK_CONTROL_KIND_CHECK_STORE &&
                    status.control_audit_owner_bind_count == 1 &&
                    status.control_state == MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_ASSERT &&
                    status.control_owner.valid && !status.control_action_enqueued;
            MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_START:
                predecessor_ok = status.control_audit_l2_assert_enqueue_count == 1 &&
                    status.control_owner.valid && status.control_action_enqueued;
            MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_COMPLETION:
                predecessor_ok = status.control_audit_l2_assert_start_count == 1 &&
                    status.control_state == MEMBLOCK_CONTROL_STATE_WAIT_L2_FLUSH_DONE;
            MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_ENQUEUE:
                predecessor_ok = status.control_audit_l2_assert_completion_count == 1 &&
                    status.control_state == MEMBLOCK_CONTROL_STATE_WAIT_L2_FLUSH_DONE;
            MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_START:
                predecessor_ok = status.control_audit_l2_release_enqueue_count == 1 &&
                    status.control_state == MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_RELEASE;
            MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_COMPLETION:
                predecessor_ok = status.control_audit_l2_release_start_count == 1 &&
                    status.control_state == MEMBLOCK_CONTROL_STATE_WAIT_L2_FLUSH_IDLE;
            MEMBLOCK_CONTROL_AUDIT_TERMINAL:
                predecessor_ok = control_lifecycle_action_matrix_complete(status) &&
                    status.control_state == MEMBLOCK_CONTROL_STATE_CONTROL_COMMIT_READY &&
                    status.active && !status.terminal_done;
            MEMBLOCK_CONTROL_AUDIT_BARRIER_RELEASE:
                predecessor_ok = control_lifecycle_action_matrix_complete(status) &&
                    status.control_audit_terminal_count == 1 &&
                    status.terminal_done && !status.active &&
                    active_control_barrier_valid && active_control_barrier_uid == uid &&
                    !control_barrier_prefix_external_provenance_valid;
            default: predecessor_ok = 1'b0;
        endcase
        if (!predecessor_ok) begin
            `uvm_fatal("CONTROL_LIFECYCLE_AUDIT",
                       $sformatf("event=%0s predecessor mismatch uid=%0d kind=%0d state=%0d generation=%0d create/bind=%0d/%0d terminal/release=%0d/%0d",
                                 control_lifecycle_event_name(event_kind), uid,
                                 status.control_kind, status.control_state,
                                 status.control_action_generation,
                                 status.control_audit_creation_count,
                                 status.control_audit_owner_bind_count,
                                 status.control_audit_terminal_count,
                                 status.control_audit_barrier_release_count))
            return 1'b0;
        end
        ticket.valid = 1'b1;
        ticket.uid = uid;
        ticket.event_kind = event_kind;
        ticket.generation = (event_kind == MEMBLOCK_CONTROL_AUDIT_OWNER_BIND) ?
            1 : status.control_action_generation;
        return 1'b1;
    endfunction:precheck_control_lifecycle_event


    function bit precheck_control_terminal_retire(
        input memblock_uid_t uid,
        output memblock_uid_t planned_terminal_done_uid
    );
        status_transaction     status;
        memblock_rob_key_t     rob_key;
        memblock_rob_map_key_t rob_map_key;
        memblock_rob_map_key_t scan_rob_map_key;
        memblock_lq_map_key_t  scan_lq_map_key;
        memblock_sq_map_key_t  scan_sq_map_key;
        memblock_uid_t         scan_uid;

        planned_terminal_done_uid = dispatch_progress.terminal_done_uid;
        if (!is_valid_uid(uid) || status_by_uid[uid] == null) begin
            `uvm_fatal("CONTROL_COMMIT",
                       $sformatf("control terminal retire invalid uid=%0d", uid))
            return 1'b0;
        end
        status = status_by_uid[uid];
        if (!status.active || status.terminal_done ||
            status.active_lq_mapped || status.active_sq_mapped) begin
            `uvm_fatal("CONTROL_COMMIT",
                       $sformatf("control terminal retire status mismatch uid=%0d active/terminal=%0d/%0d lq/sq=%0d/%0d",
                                 uid, status.active, status.terminal_done,
                                 status.active_lq_mapped, status.active_sq_mapped))
            return 1'b0;
        end

        rob_key.flag = status.robIdx_flag;
        rob_key.value = status.robIdx_value;
        rob_map_key = rob_order_util::rob_to_map_key(rob_key);
        if (!uid_by_active_rob.exists(rob_map_key) ||
            uid_by_active_rob[rob_map_key] != uid) begin
            `uvm_fatal("CONTROL_COMMIT",
                       $sformatf("control terminal retire uid=%0d has inconsistent active ROB mapping rob=%0d/%0d",
                                 uid, rob_key.flag, rob_key.value))
            return 1'b0;
        end
        foreach (uid_by_active_rob[scan_rob_map_key]) begin
            if (uid_by_active_rob[scan_rob_map_key] == uid &&
                scan_rob_map_key != rob_map_key) begin
                `uvm_fatal("CONTROL_COMMIT",
                           $sformatf("control terminal retire uid=%0d has duplicate active ROB mapping",
                                     uid))
                return 1'b0;
            end
        end
        foreach (uid_by_lq[scan_lq_map_key]) begin
            if (uid_by_lq[scan_lq_map_key] == uid) begin
                `uvm_fatal("CONTROL_COMMIT",
                           $sformatf("control terminal retire uid=%0d has residual LQ mapping", uid))
                return 1'b0;
            end
        end
        foreach (uid_by_sq[scan_sq_map_key]) begin
            if (uid_by_sq[scan_sq_map_key] == uid) begin
                `uvm_fatal("CONTROL_COMMIT",
                           $sformatf("control terminal retire uid=%0d has residual SQ mapping", uid))
                return 1'b0;
            end
        end
        foreach (load_issue_q[idx]) begin
            if (load_issue_q[idx].uid == uid) begin
                `uvm_fatal("CONTROL_COMMIT",
                           $sformatf("control terminal retire uid=%0d has residual load issue item", uid))
                return 1'b0;
            end
        end
        foreach (sta_issue_q[idx]) begin
            if (sta_issue_q[idx].uid == uid) begin
                `uvm_fatal("CONTROL_COMMIT",
                           $sformatf("control terminal retire uid=%0d has residual STA issue item", uid))
                return 1'b0;
            end
        end
        foreach (std_issue_q[idx]) begin
            if (std_issue_q[idx].uid == uid) begin
                `uvm_fatal("CONTROL_COMMIT",
                           $sformatf("control terminal retire uid=%0d has residual STD issue item", uid))
                return 1'b0;
            end
        end
        foreach (ptw_wait_replay_q[idx]) begin
            if (ptw_wait_replay_q[idx].uid == uid) begin
                `uvm_fatal("CONTROL_COMMIT",
                           $sformatf("control terminal retire uid=%0d has residual PTW replay item", uid))
                return 1'b0;
            end
        end
        if (status.sta_late_fault_tombstone_q.size() != 0 ||
            std_late_raw_tombstone_by_rob.exists(rob_map_key)) begin
            `uvm_fatal("CONTROL_COMMIT",
                       $sformatf("control terminal retire uid=%0d has residual tombstone sta/std=%0d/%0d",
                                 uid, status.sta_late_fault_tombstone_q.size(),
                                 std_late_raw_tombstone_by_rob.exists(rob_map_key)))
            return 1'b0;
        end

        if (dispatch_progress.terminal_done_uid > main_trans_num) begin
            `uvm_fatal("CONTROL_COMMIT",
                       $sformatf("control terminal retire invalid frontier=%0d main_trans_num=%0d",
                                 dispatch_progress.terminal_done_uid, main_trans_num))
            return 1'b0;
        end
        scan_uid = dispatch_progress.terminal_done_uid;
        while (scan_uid < main_trans_num) begin
            if (status_by_uid[scan_uid] == null) begin
                `uvm_fatal("CONTROL_COMMIT",
                           $sformatf("control terminal retire frontier references null status uid=%0d",
                                     scan_uid))
                return 1'b0;
            end
            if (scan_uid != uid && !status_by_uid[scan_uid].terminal_done) begin
                break;
            end
            scan_uid++;
        end
        planned_terminal_done_uid = scan_uid;
        return 1'b1;
    endfunction:precheck_control_terminal_retire



    function bit precheck_l2_assert_completion_release_enqueue(
        input memblock_uid_t uid,
        output memblock_control_lifecycle_ticket_t assert_ticket,
        output memblock_control_lifecycle_ticket_t release_ticket
    );
        status_transaction status;

        assert_ticket = '{default:'0};
        release_ticket = '{default:'0};
        if (!precheck_control_lifecycle_event(
                uid, MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_COMPLETION,
                assert_ticket)) begin
            return 1'b0;
        end
        status = status_by_uid[uid];
        if (status.control_audit_l2_release_enqueue_count != 0 ||
            status.control_kind != MEMBLOCK_CONTROL_KIND_CHECK_STORE ||
            status.control_audit_l2_assert_start_count != 1 ||
            status.control_audit_l2_assert_completion_count != 0 ||
            status.control_state != MEMBLOCK_CONTROL_STATE_WAIT_L2_FLUSH_DONE ||
            !status.control_owner.valid || status.control_action_enqueued ||
            l2_flush_release_request_q.size() != 0) begin
            `uvm_fatal("CONTROL_LIFECYCLE_AUDIT",
                       $sformatf("combined L2 ASSERT completion/RELEASE enqueue precheck mismatch uid=%0d state=%0d generation=%0d assert=%0d/%0d release_enqueue=%0d owner/action=%0d/%0d queue=%0d",
                                 uid, status.control_state,
                                 status.control_action_generation,
                                 status.control_audit_l2_assert_start_count,
                                 status.control_audit_l2_assert_completion_count,
                                 status.control_audit_l2_release_enqueue_count,
                                 status.control_owner.valid,
                                 status.control_action_enqueued,
                                 l2_flush_release_request_q.size()))
            assert_ticket = '{default:'0};
            return 1'b0;
        end
        release_ticket.valid = 1'b1;
        release_ticket.uid = uid;
        release_ticket.event_kind = MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_ENQUEUE;
        release_ticket.generation = status.control_action_generation;
        return 1'b1;
    endfunction:precheck_l2_assert_completion_release_enqueue



    function bit precheck_redirect_post_state_invariant(
        input memblock_redirect_payload_t redirect,
        input memblock_uid_t planned_new_end_uid,
        input memblock_uid_t reissue_uids[$],
        input bit establish_provenance,
        input memblock_uid_t planned_provenance_uid
    );
        int unsigned provenance_match_count;

        provenance_match_count = 0;
        foreach (uid_by_active_rob[map_key]) begin
            memblock_uid_t uid;
            status_transaction status;
            memblock_rob_key_t rob_key;
            bit planned_removed;
            bit provenance_valid;
            memblock_uid_t provenance_uid;

            uid = uid_by_active_rob[map_key];
            planned_removed = 1'b0;
            foreach (reissue_uids[idx]) begin
                if (reissue_uids[idx] == uid) begin
                    planned_removed = 1'b1;
                end
            end
            if (planned_removed || uid < planned_new_end_uid) begin
                continue;
            end
            if (!is_valid_uid(uid) || status_by_uid[uid] == null ||
                main_table_by_uid[uid] == null) begin
                `uvm_fatal("ROB_ADMISSION_PREFIX",
                           $sformatf("redirect post-state plan references invalid uid=%0d", uid))
                return 1'b0;
            end
            status = status_by_uid[uid];
            rob_key = status.get_rob_key();
            provenance_valid = control_barrier_prefix_external_provenance_valid ||
                               establish_provenance;
            provenance_uid = control_barrier_prefix_external_provenance_valid ?
                control_barrier_prefix_external_provenance_uid :
                planned_provenance_uid;
            if (provenance_valid && uid == provenance_uid &&
                active_control_barrier_valid && active_control_barrier_uid == uid &&
                is_control_op_class(main_table_by_uid[uid].op_class) &&
                status.active && status.enq && !status.terminal_done &&
                status.control_state == MEMBLOCK_CONTROL_STATE_WAIT_OLDER_ROB_COMMIT &&
                !status.control_owner.valid && !status.control_action_enqueued &&
                !status.active_lq_mapped && !status.active_sq_mapped &&
                rob_order_util::rob_need_flush(rob_key, redirect)) begin
                provenance_match_count++;
                continue;
            end
            `uvm_fatal("ROB_ADMISSION_PREFIX",
                       $sformatf("redirect planned post-state has prefix-external owner uid=%0d new_end=%0d provenance=%0d/%0d",
                                 uid, planned_new_end_uid,
                                 provenance_valid, provenance_uid))
            return 1'b0;
        end
        if ((control_barrier_prefix_external_provenance_valid || establish_provenance) &&
            provenance_match_count != 1) begin
            `uvm_fatal("ROB_ADMISSION_PREFIX",
                       $sformatf("redirect planned provenance uid=%0d matched owners=%0d new_end=%0d",
                                 control_barrier_prefix_external_provenance_valid ?
                                     control_barrier_prefix_external_provenance_uid :
                                     planned_provenance_uid,
                                 provenance_match_count, planned_new_end_uid))
            return 1'b0;
        end
        return 1'b1;
    endfunction:precheck_redirect_post_state_invariant



    function bit precheck_start_redirect_deleted_owner_window(input int record_idx);
        if (redirect_deleted_owner_window_active ||
            (redirect_deleted_lq_owner_by_key.num() == 0 &&
             redirect_deleted_sq_owner_by_key.num() == 0)) begin
            `uvm_fatal("REDIRECT_DELETED_OWNER",
                       "start window requires idle state and at least one saved owner")
            return 1'b0;
        end
        if (record_idx < 0 || record_idx >= cancel_record_q.size() ||
            !cancel_record_q[record_idx].valid ||
            !cancel_record_q[record_idx].redirect_anchor_valid ||
            cancel_record_q[record_idx].active_scan_done ||
            cancel_record_q[record_idx].software_count_finalized ||
            cancel_record_q[record_idx].software_applied) begin
            `uvm_fatal("REDIRECT_DELETED_OWNER",
                       "start window precheck got an invalid cancel record lifecycle")
            return 1'b0;
        end
        return 1'b1;
    endfunction:precheck_start_redirect_deleted_owner_window



    function bit precheck_start_redirect_deleted_owner_window_plan(
        input int record_idx,
        input int unsigned planned_lq_owner_count,
        input int unsigned planned_sq_owner_count
    );
        if (planned_lq_owner_count == 0 && planned_sq_owner_count == 0) begin
            `uvm_fatal("REDIRECT_DELETED_OWNER",
                       "planned deleted-owner window requires at least one owner")
            return 1'b0;
        end
        if (redirect_deleted_owner_window_active ||
            redirect_deleted_lq_owner_by_key.num() != 0 ||
            redirect_deleted_sq_owner_by_key.num() != 0) begin
            `uvm_fatal("REDIRECT_DELETED_OWNER",
                       "planned deleted-owner window requires empty current owner state")
            return 1'b0;
        end
        if (record_idx < 0 || record_idx >= cancel_record_q.size() ||
            !cancel_record_q[record_idx].valid ||
            !cancel_record_q[record_idx].redirect_anchor_valid ||
            cancel_record_q[record_idx].active_scan_done ||
            cancel_record_q[record_idx].software_count_finalized ||
            cancel_record_q[record_idx].software_applied) begin
            `uvm_fatal("REDIRECT_DELETED_OWNER",
                       "planned deleted-owner window got invalid cancel record lifecycle")
            return 1'b0;
        end
        return 1'b1;
    endfunction:precheck_start_redirect_deleted_owner_window_plan



    function bit precheck_uid_redirect_mutation(
        input memblock_uid_t uid,
        input memblock_redirect_payload_t redirect,
        input bit require_fault_head,
        input string caller
    );
        status_transaction status;
        main_control_transaction main_tr;
        memblock_rob_map_key_t rob_map_key;
        int record_idx;

        if (!redirect.valid || !is_valid_uid(uid) || status_by_uid[uid] == null ||
            main_table_by_uid[uid] == null) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("%s invalid redirect/uid valid=%0d uid=%0d",
                                 caller, redirect.valid, uid))
            return 1'b0;
        end
        status = status_by_uid[uid];
        main_tr = main_table_by_uid[uid];
        if (is_control_op_class(main_tr.op_class) || status.terminal_done || !status.active) begin
            `uvm_fatal("CONTROL_REDIRECT",
                       $sformatf("%s rejects control/terminal/inactive uid=%0d op=%0d terminal/active=%0d/%0d",
                                 caller, uid, main_tr.op_class, status.terminal_done,
                                 status.active))
            return 1'b0;
        end
        if (require_fault_head &&
            (!status.active || !status.rob_commit ||
             status.get_rob_key() != redirect.rob_key ||
             (!status.fault && !status.exception_pending && !status.load_fault &&
              !status.sta_fault && !status.std_fault))) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("%s uid=%0d is not active committed fault head", caller, uid))
            return 1'b0;
        end
        record_idx = find_cancel_record_index_by_id(active_cancel_record_id);
        if (!active_cancel_record_id_valid || record_idx < 0 ||
            !cancel_record_q[record_idx].valid ||
            !cancel_record_q[record_idx].redirect_anchor_valid ||
            cancel_record_q[record_idx].software_count_finalized ||
            cancel_record_q[record_idx].software_applied ||
            !redirect_payload_equal(redirect, cancel_record_q[record_idx].redirect) ||
            cancel_record_q[record_idx].redirect_epoch !=
                memblock_sync_pkg::dispatch_flush_epoch) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("%s uid=%0d has no open active cancel record", caller, uid))
            return 1'b0;
        end
        if (status.lsq_cancel_accounted_epoch == cancel_record_q[record_idx].redirect_epoch) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("%s uid=%0d was already cancel-accounted", caller, uid))
            return 1'b0;
        end
        if ((status.active_lq_mapped || status.active_sq_mapped) &&
            (main_tr.numLsElem != 1 ||
             status.lsq_reservation_state != MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE ||
             !status.lsq_reservation_sample_valid ||
             status.lsq_reservation_sample_seq > cancel_record_q[record_idx].redirect_lsq_sample_seq)) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("%s uid=%0d invalid mapped cancel reservation", caller, uid))
            return 1'b0;
        end
        if (!check_cancel_pending_aggregate_checked(caller)) begin
            return 1'b0;
        end
        if (cancel_record_q[record_idx].software_cancel_lq_count +
                (status.active_lq_mapped ? main_tr.numLsElem : 0) > MEMBLOCK_LQ_SIZE ||
            cancel_record_q[record_idx].software_cancel_sq_count +
                (status.active_sq_mapped ? main_tr.numLsElem : 0) > MEMBLOCK_SQ_SIZE) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("%s uid=%0d cancel count would exceed capacity", caller, uid))
            return 1'b0;
        end
        if (redirect_deleted_owner_window_active) begin
            `uvm_fatal("REDIRECT_DELETED_OWNER",
                       $sformatf("%s uid=%0d previous owner window is active", caller, uid))
            return 1'b0;
        end
        if (status.active) begin
            rob_map_key = rob_order_util::rob_to_map_key(status.get_rob_key());
            if (!uid_by_active_rob.exists(rob_map_key) || uid_by_active_rob[rob_map_key] != uid) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("%s uid=%0d has inconsistent active ROB map", caller, uid))
                return 1'b0;
            end
            if (!status.active_instance_flush_epoch_valid ||
                status.active_instance_flush_epoch >=
                    cancel_record_q[record_idx].redirect_epoch) begin
                `uvm_fatal("REDIRECT_DELETED_OWNER",
                           $sformatf("%s uid=%0d activation epoch valid/value=%0d/%0d is not older than redirect epoch=%0d",
                                     caller, uid,
                                     status.active_instance_flush_epoch_valid,
                                     status.active_instance_flush_epoch,
                                     cancel_record_q[record_idx].redirect_epoch))
                return 1'b0;
            end
        end
        if (status.active_lq_mapped) begin
            memblock_lq_key_t key;
            memblock_lq_map_key_t map_key;
            key.flag = status.lqIdx_flag;
            key.value = status.lqIdx_value;
            map_key = rob_order_util::lq_to_map_key(key);
            if (!is_valid_lq_key(key) || !uid_by_lq.exists(map_key) ||
                uid_by_lq[map_key] != uid || redirect_deleted_lq_owner_by_key.exists(map_key)) begin
                `uvm_fatal("REDIRECT_DELETED_OWNER",
                           $sformatf("%s uid=%0d has invalid/duplicate LQ owner", caller, uid))
                return 1'b0;
            end
        end
        if (status.active_sq_mapped) begin
            memblock_sq_key_t key;
            memblock_sq_map_key_t map_key;
            key.flag = status.sqIdx_flag;
            key.value = status.sqIdx_value;
            map_key = rob_order_util::sq_to_map_key(key);
            if (!is_valid_sq_key(key) || !uid_by_sq.exists(map_key) ||
                uid_by_sq[map_key] != uid || redirect_deleted_sq_owner_by_key.exists(map_key)) begin
                `uvm_fatal("REDIRECT_DELETED_OWNER",
                           $sformatf("%s uid=%0d has invalid/duplicate SQ owner", caller, uid))
                return 1'b0;
            end
        end
        if (require_fault_head && fault_redirect_resource.valid &&
            (!fault_redirect_resource_matches(uid, status.dynamic_epoch) ||
             fault_redirect_resource.redirect_epoch != cancel_record_q[record_idx].redirect_epoch ||
             fault_redirect_resource.cancel_record_id != cancel_record_q[record_idx].cancel_record_id)) begin
            `uvm_fatal("LSQ_CANCEL",
                       $sformatf("%s uid=%0d conflicts with active fault resource", caller, uid))
            return 1'b0;
        end
        if (uid_tlb_record_by_uid.exists(uid) && uid_tlb_record_by_uid[uid] != null &&
            uid_tlb_record_by_uid[uid].is_waiting()) begin
            memblock_uid_tlb_record record;
            memblock_uid_tlb_wait_shape_key_t shape_key;
            int unsigned match_count;

            record = uid_tlb_record_by_uid[uid];
            if (!record.record_valid || record.uid != uid) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("%s uid=%0d has invalid waiting TLB record", caller, uid))
                return 1'b0;
            end
            shape_key = make_uid_tlb_wait_shape_key(record.vpn, record.s2xlate);
            match_count = 0;
            if (uid_waiting_by_vpn_s2xlate.exists(shape_key)) begin
                foreach (uid_waiting_by_vpn_s2xlate[shape_key][idx]) begin
                    if (uid_waiting_by_vpn_s2xlate[shape_key][idx] == uid) begin
                        match_count++;
                    end
                end
            end
            if (match_count != 1) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("%s uid=%0d waiting TLB index match_count=%0d",
                                     caller, uid, match_count))
                return 1'b0;
            end
            if (uid_waiting_by_vpn_s2xlate[shape_key].size() > MEMBLOCK_ROB_SIZE) begin
                `uvm_fatal("COMMON_DATA",
                           $sformatf("%s uid=%0d waiting TLB index bucket size=%0d exceeds ROB capacity=%0d",
                                     caller, uid,
                                     uid_waiting_by_vpn_s2xlate[shape_key].size(),
                                     MEMBLOCK_ROB_SIZE))
                return 1'b0;
            end
            foreach (uid_waiting_by_vpn_s2xlate[shape_key][idx]) begin
                memblock_uid_t candidate_uid;
                memblock_uid_tlb_record candidate_record;

                candidate_uid = uid_waiting_by_vpn_s2xlate[shape_key][idx];
                if (!uid_tlb_record_by_uid.exists(candidate_uid) ||
                    uid_tlb_record_by_uid[candidate_uid] == null ||
                    !uid_tlb_record_by_uid[candidate_uid].record_valid ||
                    !uid_tlb_record_by_uid[candidate_uid].is_waiting()) begin
                    continue;
                end
                candidate_record = uid_tlb_record_by_uid[candidate_uid];
                if (candidate_record.uid != candidate_uid ||
                    make_uid_tlb_wait_shape_key(candidate_record.vpn,
                                                candidate_record.s2xlate) != shape_key) begin
                    `uvm_fatal("COMMON_DATA",
                               $sformatf("%s uid=%0d waiting TLB index contains corrupt candidate=%0d",
                                         caller, uid, candidate_uid))
                    return 1'b0;
                end
            end
        end
        if (status.active && status.std_dispatched &&
            (status.fault || status.exception_pending || status.sta_fault)) begin
            memblock_std_late_raw_tombstone_t tombstone;
            rob_map_key = rob_order_util::rob_to_map_key(status.get_rob_key());
            if (std_late_raw_tombstone_by_rob.exists(rob_map_key)) begin
                tombstone = std_late_raw_tombstone_by_rob[rob_map_key];
                if (!tombstone.valid || tombstone.uid != uid ||
                    tombstone.dynamic_epoch != status.dynamic_epoch) begin
                    `uvm_fatal("STD_LATE_TOMBSTONE",
                               $sformatf("%s uid=%0d conflicts with existing tombstone",
                                         caller, uid))
                    return 1'b0;
                end
            end
        end
        return 1'b1;
    endfunction:precheck_uid_redirect_mutation



    function void record_control_lifecycle_event(
        input memblock_control_lifecycle_ticket_t ticket
    );
        status_transaction status;

        if (!ticket.valid) begin
            return;
        end
        status = status_by_uid[ticket.uid];
        case (ticket.event_kind)
            MEMBLOCK_CONTROL_AUDIT_CREATION: status.control_audit_creation_count = 1;
            MEMBLOCK_CONTROL_AUDIT_OWNER_BIND: status.control_audit_owner_bind_count = 1;
            MEMBLOCK_CONTROL_AUDIT_CSR_ENQUEUE: status.control_audit_csr_enqueue_count = 1;
            MEMBLOCK_CONTROL_AUDIT_CSR_START: status.control_audit_csr_start_count = 1;
            MEMBLOCK_CONTROL_AUDIT_CSR_COMPLETION: status.control_audit_csr_completion_count = 1;
            MEMBLOCK_CONTROL_AUDIT_SFENCE_ENQUEUE: status.control_audit_sfence_enqueue_count = 1;
            MEMBLOCK_CONTROL_AUDIT_SFENCE_START: status.control_audit_sfence_start_count = 1;
            MEMBLOCK_CONTROL_AUDIT_SFENCE_COMPLETION: status.control_audit_sfence_completion_count = 1;
            MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_ENQUEUE: status.control_audit_l2_assert_enqueue_count = 1;
            MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_START: status.control_audit_l2_assert_start_count = 1;
            MEMBLOCK_CONTROL_AUDIT_L2_ASSERT_COMPLETION: status.control_audit_l2_assert_completion_count = 1;
            MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_ENQUEUE: status.control_audit_l2_release_enqueue_count = 1;
            MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_START: status.control_audit_l2_release_start_count = 1;
            MEMBLOCK_CONTROL_AUDIT_L2_RELEASE_COMPLETION: status.control_audit_l2_release_completion_count = 1;
            MEMBLOCK_CONTROL_AUDIT_TERMINAL: status.control_audit_terminal_count = 1;
            MEMBLOCK_CONTROL_AUDIT_BARRIER_RELEASE: status.control_audit_barrier_release_count = 1;
            default: return;
        endcase
        `uvm_info("CONTROL_LIFECYCLE_AUDIT",
                  $sformatf("event=%0s uid=%0d kind=%0d generation=%0d create/bind=%0d/%0d csr=%0d/%0d/%0d sfence=%0d/%0d/%0d l2_assert=%0d/%0d/%0d l2_release=%0d/%0d/%0d terminal/release=%0d/%0d",
                            control_lifecycle_event_name(ticket.event_kind), ticket.uid,
                            status.control_kind, status.control_action_generation,
                            status.control_audit_creation_count,
                            status.control_audit_owner_bind_count,
                            status.control_audit_csr_enqueue_count,
                            status.control_audit_csr_start_count,
                            status.control_audit_csr_completion_count,
                            status.control_audit_sfence_enqueue_count,
                            status.control_audit_sfence_start_count,
                            status.control_audit_sfence_completion_count,
                            status.control_audit_l2_assert_enqueue_count,
                            status.control_audit_l2_assert_start_count,
                            status.control_audit_l2_assert_completion_count,
                            status.control_audit_l2_release_enqueue_count,
                            status.control_audit_l2_release_start_count,
                            status.control_audit_l2_release_completion_count,
                            status.control_audit_terminal_count,
                            status.control_audit_barrier_release_count), UVM_LOW)
    endfunction:record_control_lifecycle_event



    function bit remove_uid_from_issue_queues_for_redirect_range_checked(
        input memblock_uid_t uid
    );
        if (!is_valid_uid(uid) || status_by_uid[uid] == null) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("redirect issue-queue cleanup invalid uid=%0d", uid))
            return 1'b0;
        end
        remove_uid_from_issue_queues(uid);
        return 1'b1;
    endfunction:remove_uid_from_issue_queues_for_redirect_range_checked



endclass:common_data_transaction

`endif
