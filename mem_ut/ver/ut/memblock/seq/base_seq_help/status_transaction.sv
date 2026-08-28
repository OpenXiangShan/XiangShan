//=========================================================
//File name    : status_transaction.sv
//Author       : OpenAI_Codex
//Module name  : status_transaction
//Discribution : dispatch framework per-uid runtime status
//Date         : 2026-05-18
//=========================================================
`ifndef STATUS_TRANSACTION__SV
`define STATUS_TRANSACTION__SV

// 中文注释：STA IQ hit=0 后保存的旧 issue identity。raw STA 只有 ROB 和异常信息，
// 因此 adapter 只能在 current snapshot 已失效且 raw 为 fault 时，用本记录恢复旧身份。
typedef struct {
    bit                  valid;
    memblock_rob_key_t   rob_key;
    memblock_sq_key_t    sq_key;
    int unsigned         issue_epoch;
    int unsigned         replay_seq;
    int unsigned         dynamic_epoch;
    int unsigned         target_flush_epoch;
    longint unsigned     create_cycle;
} memblock_sta_late_fault_tombstone_t;

class status_transaction extends uvm_object;

    memblock_uid_t uid;

    bit active;
    bit enq;
    bit issue_ready;
    bit tlb_mapped;
    bit queued_load;
    bit queued_sta;
    bit queued_std;
    bit load_dispatched;
    bit sta_dispatched;
    bit std_dispatched;
    bit writeback;
    bit pass;
    bit fault;
    bit load_writeback;
    bit sta_writeback;
    bit std_writeback;
    // 中文注释：各 target 的 IssueQueue feedback success 状态。
    // 置位：IQ feedback hit 经 mark_issue_feedback_success() 通过 epoch/replay 检查后置位。
    // 清零：status reset、redirect/replay 清理对应 target 发射结果时清零。为1只说明 issue response finalSuccess，不代表真实 writeback/pass。
    bit load_issue_feedback_success;
    bit sta_issue_feedback_success;
    bit std_issue_feedback_success;
    // 中文注释：每 UID 的 STA late-fault history。IQ hit=0 创建，replay 不清除；
    // STA fault、redirect/flush、terminal retire 或 reset 清除。仅 fault raw 可读取。
    memblock_sta_late_fault_tombstone_t sta_late_fault_tombstone_q[$];
    bit load_pass;
    bit sta_pass;
    bit std_pass;
    bit load_fault;
    bit sta_fault;
    bit std_fault;
    bit exception_pending;
    bit replay_pending;
    bit replay_target_load;
    bit replay_target_sta;
    bit replay_target_std;
    bit redirect_pending;
    bit flushed;
    bit rob_commit;
    bit lsq_deq;
    bit success;
    bit terminal_done;
    bit active_lq_mapped;
    bit active_sq_mapped;

    // 中文注释：控制标记的静态类别、阶段和唯一 owner。
    // 控制 admission/service/已验证 worker sendover 写入；terminal_done 后由 service 清 barrier。
    // 普通访存保持 KIND_NONE/STATE_NONE，不得借此字段进入控制流程。
    memblock_control_kind_e   control_kind;
    memblock_control_state_e  control_state;
    memblock_control_owner_t  control_owner;
    int unsigned              control_action_generation;
    bit                       control_action_enqueued;
    int unsigned              control_reset_epoch;
    // 中文注释：CSR 完成时归档 monitor 已观察到的 runtime snapshot，而不是已发送 xaction。
    bit                                     control_runtime_csr_snapshot_valid;
    memblock_sync_pkg::dispatch_raw_csr_t  control_runtime_csr_snapshot;
    int unsigned                            control_runtime_csr_snapshot_seq;
    int unsigned                            control_runtime_snapshot_seq_before_drive;
    // 中文注释：CSR action 的 monitor 期望值与 check_store 的稳定 CSR baseline。
    // 设置：control worker 配置/交付边界；读取：control service 的 runtime snapshot、
    // L2 ASSERT/RELEASE 分支；terminal/reset 时随 status 生命周期清零。
    bit                                     control_expected_runtime_csr_valid;
    memblock_sync_pkg::dispatch_raw_csr_t  control_expected_runtime_csr;
    bit                                     control_expected_sfence_valid;
    memblock_sfence_payload_t              control_expected_sfence;
    bit                                     control_l2_csr_baseline_valid;
    memblock_sync_pkg::dispatch_raw_csr_t  control_l2_csr_baseline;
    // 中文注释：SFence/check_store completion 的 request/event/done 基线。
    // 每个字段由对应 service 分支写入，owner 变化或 status reset 时清零，防止旧 sample 误完成。
    int unsigned              control_flushsb_req_id;
    bit                       control_flushsb_request_queued;
    // 中文注释：C0 arm 在 start_item() 前冻结 event/reset baseline；C0/C4 只能匹配
    // 同一 owner、同一 L2TLB reset epoch 且 event_seq 严格大于该 pre-drive 序号的记录。
    bit                       control_sfence_c0_armed;
    longint unsigned          control_sfence_pre_drive_event_seq;
    longint unsigned          control_sfence_c0_event_seq;
    longint unsigned          control_l2tlb_reset_epoch_at_arm;
    longint unsigned          control_assert_done_baseline_seq;
    longint unsigned          control_release_done_baseline_seq;

    // 中文注释：这些字段只描述 LSQ 动态实例是否已经跨过 DUT sample 边界，
    // 不改变 pass/fail/terminal 语义，也不替代 batch flush epoch。
    memblock_lsq_reservation_state_e lsq_reservation_state;
    int unsigned                    lsq_reservation_launch_epoch;
    longint unsigned                lsq_reservation_sample_seq;
    bit                             lsq_reservation_sample_valid;
    int unsigned                    lsq_cancel_accounted_epoch;
    // 中文注释：当前 dynamic_epoch 是否有且只有一个 canonical MMIO tag。
    // 置位/更新只允许走 common_data_transaction::set_uid_mmio_tag()；
    // redirect/reissue 由 clear_uid_mmio_tag() 清零，普通 terminal retire 不复用实例。
    bit                             mmio_tag_valid;
    // 中文注释：有效 tag 的互斥 kind。load/store 不得同时为 1；query 还会
    // 校验 mmio_tag_dynamic_epoch，防止旧实例 tag 被新实例观察。
    bit                             is_mmio_load;
    bit                             is_mmio_store;
    memblock_mmio_tag_source_e      mmio_tag_source;
    int unsigned                    mmio_tag_dynamic_epoch;

    bit                 robIdx_flag;
    bit [MEMBLOCK_ROB_VALUE_W-1:0] robIdx_value;
    bit                 lqIdx_flag;
    bit [MEMBLOCK_LQ_VALUE_W-1:0] lqIdx_value;
    bit                 sqIdx_flag;
    bit [MEMBLOCK_SQ_VALUE_W-1:0] sqIdx_value;
    int unsigned        load_issue_epoch;
    int unsigned        sta_issue_epoch;
    int unsigned        std_issue_epoch;
    // 中文注释：active_instance_flush_epoch 记录 activate_uid() 建立当前动态实例时
    // 的 dispatch flush epoch。activate_uid() 置位，clear_uid_dispatch_result() 清零；
    // MMIO value-only resolver 用它证明 raw 属于当前实例或旧实例。
    bit                 active_instance_flush_epoch_valid;
    int unsigned        active_instance_flush_epoch;
    // 记录每个 target 最近一次真实 issue 所属的 dispatch flush epoch，供 WB/feedback
    // 归属校验；它们不拥有 active-instance activation provenance。
    bit                 load_instance_flush_epoch_valid;
    bit                 sta_instance_flush_epoch_valid;
    bit                 std_instance_flush_epoch_valid;
    int unsigned        load_instance_flush_epoch;
    int unsigned        sta_instance_flush_epoch;
    int unsigned        std_instance_flush_epoch;
    // 同一uid被redirect reissue后产生新动态实例；递增后可区分旧实例事件。
    int unsigned        dynamic_epoch;
    int unsigned        replay_seq;
    bit                 issue_killed;
    bit [23:0]          exception_vec;
    bit [63:0]          exception_vaddr;
    bit [63:0]          exception_gpaddr;
    longint unsigned    last_event_cycle;

    `uvm_object_utils(status_transaction)

    function new(string name = "status_transaction");
        super.new(name);
        reset(0);
    endfunction:new

    function void clear_sta_late_fault_tombstones();
        sta_late_fault_tombstone_q.delete();
    endfunction:clear_sta_late_fault_tombstones

    function void reset(input memblock_uid_t uid_i);
        uid               = uid_i;
        active            = 1'b0;
        enq               = 1'b0;
        issue_ready       = 1'b0;
        tlb_mapped        = 1'b0;
        queued_load       = 1'b0;
        queued_sta        = 1'b0;
        queued_std        = 1'b0;
        load_dispatched   = 1'b0;
        sta_dispatched    = 1'b0;
        std_dispatched    = 1'b0;
        writeback         = 1'b0;
        pass              = 1'b0;
        fault             = 1'b0;
        load_writeback    = 1'b0;
        sta_writeback     = 1'b0;
        std_writeback     = 1'b0;
        load_issue_feedback_success = 1'b0;
        sta_issue_feedback_success = 1'b0;
        std_issue_feedback_success = 1'b0;
        clear_sta_late_fault_tombstones();
        load_pass         = 1'b0;
        sta_pass          = 1'b0;
        std_pass          = 1'b0;
        load_fault        = 1'b0;
        sta_fault         = 1'b0;
        std_fault         = 1'b0;
        exception_pending = 1'b0;
        replay_pending    = 1'b0;
        replay_target_load = 1'b0;
        replay_target_sta = 1'b0;
        replay_target_std = 1'b0;
        redirect_pending  = 1'b0;
        flushed           = 1'b0;
        rob_commit        = 1'b0;
        lsq_deq           = 1'b0;
        success           = 1'b0;
        terminal_done     = 1'b0;
        active_lq_mapped  = 1'b0;
        active_sq_mapped  = 1'b0;
        control_kind = MEMBLOCK_CONTROL_KIND_NONE;
        control_state = MEMBLOCK_CONTROL_STATE_NONE;
        control_owner.valid = 1'b0;
        control_owner.uid = 0;
        control_owner.dynamic_epoch = 0;
        control_owner.action_generation = 0;
        control_owner.kind = MEMBLOCK_CONTROL_KIND_NONE;
        control_action_generation = 0;
        control_action_enqueued = 1'b0;
        control_reset_epoch = 0;
        control_runtime_csr_snapshot_valid = 1'b0;
        control_runtime_csr_snapshot = '{default:'0};
        control_runtime_csr_snapshot_seq = 0;
        control_runtime_snapshot_seq_before_drive = 0;
        control_expected_runtime_csr_valid = 1'b0;
        control_expected_runtime_csr = '{default:'0};
        control_expected_sfence_valid = 1'b0;
        control_expected_sfence = '{
            default: '0,
            target_stage: MEMBLOCK_SFENCE_TARGET_HS_S1
        };
        control_l2_csr_baseline_valid = 1'b0;
        control_l2_csr_baseline = '{default:'0};
        control_flushsb_req_id = 0;
        control_flushsb_request_queued = 1'b0;
        control_sfence_c0_armed = 1'b0;
        control_sfence_pre_drive_event_seq = 0;
        control_sfence_c0_event_seq = 0;
        control_l2tlb_reset_epoch_at_arm = 0;
        control_assert_done_baseline_seq = 0;
        control_release_done_baseline_seq = 0;
        lsq_reservation_state = MEMBLOCK_LSQ_RESERVATION_NONE;
        lsq_reservation_launch_epoch = 0;
        lsq_reservation_sample_seq = 0;
        lsq_reservation_sample_valid = 1'b0;
        lsq_cancel_accounted_epoch = 0;
        mmio_tag_valid = 1'b0;
        is_mmio_load = 1'b0;
        is_mmio_store = 1'b0;
        mmio_tag_source = MEMBLOCK_MMIO_TAG_NONE;
        mmio_tag_dynamic_epoch = 0;
        robIdx_flag       = 1'b0;
        robIdx_value      = '0;
        lqIdx_flag        = 1'b0;
        lqIdx_value       = '0;
        sqIdx_flag        = 1'b0;
        sqIdx_value       = '0;
        load_issue_epoch  = 0;
        sta_issue_epoch   = 0;
        std_issue_epoch   = 0;
        active_instance_flush_epoch_valid = 1'b0;
        active_instance_flush_epoch       = 0;
        load_instance_flush_epoch_valid   = 1'b0;
        sta_instance_flush_epoch_valid    = 1'b0;
        std_instance_flush_epoch_valid    = 1'b0;
        load_instance_flush_epoch         = 0;
        sta_instance_flush_epoch          = 0;
        std_instance_flush_epoch          = 0;
        dynamic_epoch     = 0;
        replay_seq        = 0;
        issue_killed      = 1'b0;
        exception_vec     = '0;
        exception_vaddr   = '0;
        exception_gpaddr  = '0;
        last_event_cycle  = 0;
    endfunction:reset

    function void snapshot_from_main(input main_control_transaction tr);
        if (tr == null) begin
            `uvm_fatal("STATUS_TR", "snapshot_from_main got null transaction")
        end
        uid          = tr.uid;
        robIdx_flag  = tr.robIdx_flag;
        robIdx_value = tr.robIdx_value;
        lqIdx_flag   = tr.lqIdx_flag;
        lqIdx_value  = tr.lqIdx_value;
        sqIdx_flag   = tr.sqIdx_flag;
        sqIdx_value  = tr.sqIdx_value;
    endfunction:snapshot_from_main

    function memblock_rob_key_t get_rob_key();
        memblock_rob_key_t key;
        key.flag  = robIdx_flag;
        key.value = robIdx_value;
        return key;
    endfunction:get_rob_key

    function int unsigned get_target_issue_epoch(input memblock_issue_target_e target);
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: return load_issue_epoch;
            MEMBLOCK_ISSUE_TARGET_STA:  return sta_issue_epoch;
            MEMBLOCK_ISSUE_TARGET_STD:  return std_issue_epoch;
            default: begin
                `uvm_fatal("STATUS_TR", $sformatf("get_target_issue_epoch got target=%0d", target))
            end
        endcase
        return 0;
    endfunction:get_target_issue_epoch

    function void set_target_issue_epoch(input memblock_issue_target_e target,
                                         input int unsigned issue_epoch_i);
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: load_issue_epoch = issue_epoch_i;
            MEMBLOCK_ISSUE_TARGET_STA:  sta_issue_epoch  = issue_epoch_i;
            MEMBLOCK_ISSUE_TARGET_STD:  std_issue_epoch  = issue_epoch_i;
            default: begin
                `uvm_fatal("STATUS_TR", $sformatf("set_target_issue_epoch got target=%0d", target))
            end
        endcase
    endfunction:set_target_issue_epoch

    function bit get_target_instance_flush_epoch(
        input memblock_issue_target_e target,
        output int unsigned instance_flush_epoch
    );
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: begin
                instance_flush_epoch = load_instance_flush_epoch;
                return load_instance_flush_epoch_valid;
            end
            MEMBLOCK_ISSUE_TARGET_STA: begin
                instance_flush_epoch = sta_instance_flush_epoch;
                return sta_instance_flush_epoch_valid;
            end
            MEMBLOCK_ISSUE_TARGET_STD: begin
                instance_flush_epoch = std_instance_flush_epoch;
                return std_instance_flush_epoch_valid;
            end
            default: begin
                `uvm_fatal("STATUS_TR", $sformatf("get_target_instance_flush_epoch got target=%0d", target))
            end
        endcase
        instance_flush_epoch = 0;
        return 1'b0;
    endfunction:get_target_instance_flush_epoch

    function void set_target_instance_flush_epoch(
        input memblock_issue_target_e target,
        input int unsigned instance_flush_epoch_i
    );
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: begin
                load_instance_flush_epoch_valid = 1'b1;
                load_instance_flush_epoch = instance_flush_epoch_i;
            end
            MEMBLOCK_ISSUE_TARGET_STA: begin
                sta_instance_flush_epoch_valid = 1'b1;
                sta_instance_flush_epoch = instance_flush_epoch_i;
            end
            MEMBLOCK_ISSUE_TARGET_STD: begin
                std_instance_flush_epoch_valid = 1'b1;
                std_instance_flush_epoch = instance_flush_epoch_i;
            end
            default: begin
                `uvm_fatal("STATUS_TR", $sformatf("set_target_instance_flush_epoch got target=%0d", target))
            end
        endcase
    endfunction:set_target_instance_flush_epoch

    function void clear_target_instance_flush_epochs();
        load_instance_flush_epoch_valid = 1'b0;
        sta_instance_flush_epoch_valid = 1'b0;
        std_instance_flush_epoch_valid = 1'b0;
        load_instance_flush_epoch = 0;
        sta_instance_flush_epoch = 0;
        std_instance_flush_epoch = 0;
    endfunction:clear_target_instance_flush_epochs

    function void clear_lsq_reservation_visibility();
        // 中文伪代码：真实 deq/redirect 清除当前动态实例可见性，但保留单调 launch epoch，
        // 使同 UID 的晚到 callback 不能命中新实例。
        lsq_reservation_state = MEMBLOCK_LSQ_RESERVATION_NONE;
        lsq_reservation_sample_seq = 0;
        lsq_reservation_sample_valid = 1'b0;
    endfunction:clear_lsq_reservation_visibility

endclass:status_transaction

`endif
