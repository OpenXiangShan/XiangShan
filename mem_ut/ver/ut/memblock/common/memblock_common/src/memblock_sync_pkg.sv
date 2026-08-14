//=========================================================
//File name    : memblock_sync_pkg.sv
//Author       : OpenAI_Codex
//Module name  : memblock_sync_pkg
//Discribution : shared sync state for memblock tb/uvm
//Date         : 2026-04-14
//=========================================================
`ifndef MEMBLOCK_SYNC_PKG__SV
`define MEMBLOCK_SYNC_PKG__SV

`include "memblock_compile_params.svh"

package memblock_sync_pkg;

    import uvm_pkg::*;

    // 中文注释：控制屏障的静态拓扑模式。plus 解析后仅允许初始化一次；
    // sequence/worker/service 只读此快照，不能通过 testcase 或 VSEQ 推断/改写模式。
    typedef enum int unsigned {
        MEMBLOCK_CONTROL_TOPOLOGY_DISABLED             = 0,
        MEMBLOCK_CONTROL_TOPOLOGY_AUTO_MAIN_TABLE      = 1,
        MEMBLOCK_CONTROL_TOPOLOGY_MANUAL_MAIN_TABLE    = 2,
        MEMBLOCK_CONTROL_TOPOLOGY_MANUAL_CONTROL_TABLE = 3
    } memblock_control_worker_topology_mode_e;

    bit control_worker_topology_initialized = 1'b0;
    memblock_control_worker_topology_mode_e control_worker_topology_mode =
        MEMBLOCK_CONTROL_TOPOLOGY_DISABLED;
    bit control_worker_topology_active = 1'b0;
    string control_worker_topology_initializer = "";

    // 抽象职责：把已经校验的公共 plus mode 冻结为本 testcase 的唯一 runtime topology。
    // 同一 mode 的重复调用是幂等的；不同 mode 的第二写者直接 fatal，避免场景入口竞争。
    function void initialize_control_worker_topology(
        input memblock_control_worker_topology_mode_e mode,
        input string initializer
    );
        if (control_worker_topology_initialized) begin
            if (control_worker_topology_mode != mode) begin
                `uvm_fatal("MEMBLOCK_CONTROL_TOPOLOGY",
                           $sformatf("topology already frozen to %0d by %0s, cannot rewrite to %0d by %0s",
                                     control_worker_topology_mode,
                                     control_worker_topology_initializer,
                                     mode,
                                     initializer))
            end
            return;
        end
        control_worker_topology_mode = mode;
        control_worker_topology_active =
            mode == MEMBLOCK_CONTROL_TOPOLOGY_AUTO_MAIN_TABLE ||
            mode == MEMBLOCK_CONTROL_TOPOLOGY_MANUAL_CONTROL_TABLE;
        control_worker_topology_initializer = initializer;
        control_worker_topology_initialized = 1'b1;
    endfunction:initialize_control_worker_topology

    function memblock_control_worker_topology_mode_e get_control_worker_topology_mode();
        if (!control_worker_topology_initialized) begin
            `uvm_fatal("MEMBLOCK_CONTROL_TOPOLOGY", "control worker topology read before initialization")
        end
        return control_worker_topology_mode;
    endfunction:get_control_worker_topology_mode

    function bit uses_control_barrier_topology();
        return get_control_worker_topology_mode() == MEMBLOCK_CONTROL_TOPOLOGY_AUTO_MAIN_TABLE ||
               get_control_worker_topology_mode() == MEMBLOCK_CONTROL_TOPOLOGY_MANUAL_CONTROL_TABLE;
    endfunction:uses_control_barrier_topology

    function bit uses_auto_control_barrier_topology();
        return get_control_worker_topology_mode() == MEMBLOCK_CONTROL_TOPOLOGY_AUTO_MAIN_TABLE;
    endfunction:uses_auto_control_barrier_topology

    bit reset_backend_done = 1'b0;
    // Runtime reset is a shared lifecycle boundary.  The CSR monitor is the
    // only publisher; consumers copy this state into their frozen sample and
    // never infer an epoch from a transaction or the current owner.
    // Testcase startup begins from the virtual epoch-0 baseline.  The first
    // physical/runtime reset call creates epoch 1 and its acknowledgement
    // mask; epoch 0 must never wait for component callbacks.
    bit l2tlb_runtime_reset_active = 1'b0;
    longint unsigned l2tlb_current_reset_epoch = 0;
    bit dispatch_flush_in_progress = 1'b0;
    bit dispatch_monitor_capture_en = 1'b0;
    bit l2tlb_responder_active = 1'b0;
    bit dispatch_real_smoke_active = 1'b0;
    // L2TLB connect takeover only describes whether the testbench owns the
    // wires.  Testcase lifecycle chooses separately whether a responder and
    // dispatch live-entry service are actually present.
    typedef enum bit {
        MEMBLOCK_L2TLB_RESPONDER_DISABLED,
        MEMBLOCK_L2TLB_RESPONDER_ENABLED
    } memblock_l2tlb_responder_mode_e;
    typedef enum bit {
        MEMBLOCK_L2TLB_TOPOLOGY_NO_DISPATCH,
        MEMBLOCK_L2TLB_TOPOLOGY_DISPATCH_ACTIVE
    } memblock_l2tlb_dispatch_topology_e;
    typedef enum bit [1:0] {
        MEMBLOCK_L2TLB_START_DISABLED,
        MEMBLOCK_L2TLB_START_DEFAULT,
        MEMBLOCK_L2TLB_START_EXPLICIT
    } memblock_l2tlb_start_mode_e;
    bit                                l2tlb_testcase_lifecycle_initialized = 1'b0;
    memblock_l2tlb_responder_mode_e    l2tlb_testcase_responder_mode =
                                        MEMBLOCK_L2TLB_RESPONDER_DISABLED;
    memblock_l2tlb_dispatch_topology_e l2tlb_testcase_dispatch_topology =
                                        MEMBLOCK_L2TLB_TOPOLOGY_NO_DISPATCH;
    memblock_l2tlb_start_mode_e        l2tlb_testcase_start_mode =
                                        MEMBLOCK_L2TLB_START_DISABLED;
    bit                                l2tlb_testcase_needs_response = 1'b0;
    string                             l2tlb_testcase_topology_name = "";
    // This tracks an actually running dispatch adapter service.  Startup reset
    // happens before UVM sequences begin, so an inactive service is N/A for
    // that reset epoch rather than an acknowledgement that can never arrive.
    bit                                l2tlb_adapter_service_active = 1'b0;
    string                             l2tlb_adapter_service_owner_name = "";
    // 中文注释：DCache responder 已在 global stop 后发送 terminal idle 并自然返回。
    // legacy testcase 用它保持 phase objection，避免 responder 被 phase 提前杀掉。
    bit dcache_responder_done = 1'b0;
    bit dispatch_flushsb_waiting_empty = 1'b0;
    int unsigned dispatch_flush_epoch = 0;
    longint unsigned dispatch_service_cycle = 0;
    int unsigned raw_csr_rearm_epoch = 0;
    // 中文注释：同一时刻唯一的L2TLB responder生命周期owner。
    // enable sequence在开放ready前claim，最终inactive item完成后release；DUT reset不清owner。
    bit l2tlb_lifecycle_owner_claimed = 1'b0;
    string l2tlb_lifecycle_owner_name = "";
    bit l2tlb_owner_claimed_once = 1'b0;
    longint unsigned l2tlb_owner_admission_settled_sample_seq = 0;
    bit l2tlb_release_admission_close_requested = 1'b0;
    string l2tlb_release_admission_request_owner_name = "";
    longint unsigned l2tlb_release_admission_close_request_sample_seq = 0;
    longint unsigned l2tlb_release_admission_close_reset_epoch = 0;
    longint unsigned l2tlb_release_admission_close_generation = 0;
    bit l2tlb_release_admission_closed = 1'b0;
    string l2tlb_release_admission_owner_name = "";
    longint unsigned l2tlb_release_admission_closed_generation = 0;
    longint unsigned l2tlb_release_admission_cutoff_sample_seq = 0;
    bit l2tlb_response_drain_done = 1'b0;
    string l2tlb_response_drain_owner_name = "";
    longint unsigned l2tlb_response_drain_generation = 0;
    bit l2tlb_release_final_inactive_item_done = 1'b0;
    longint unsigned l2tlb_release_final_inactive_generation = 0;
    longint unsigned l2tlb_release_final_inactive_transport_sample_seq = 0;
    longint unsigned l2tlb_transport_sample_recycle_done_seq = 0;
    bit l2tlb_monitor_final_sample_settled_valid = 1'b0;
    longint unsigned l2tlb_monitor_final_sample_settled_epoch = 0;
    longint unsigned l2tlb_monitor_final_sample_settled_transport_sample_seq = 0;
    bit l2tlb_release_closing = 1'b0;
    string l2tlb_release_closing_owner_name = "";
    longint unsigned l2tlb_release_closing_generation = 0;
    bit l2tlb_release_granted = 1'b0;
    string l2tlb_release_grant_owner_name = "";
    longint unsigned l2tlb_release_grant_reset_epoch = 0;
    longint unsigned l2tlb_release_grant_generation = 0;
    // Release proofs owned by the adapter/fence producer are kept in the
    // shared package so the parent and the responder use one gate.  A proof
    // is valid only for the exact current reset epoch and close generation;
    // an empty queue alone is not a producer-close proof.
    bit l2tlb_adapter_drain_done = 1'b0;
    longint unsigned l2tlb_adapter_drain_epoch = 0;
    longint unsigned l2tlb_adapter_drain_generation = 0;
    longint unsigned l2tlb_raw_fence_producer_settled_sample_seq = 0;
    bit l2tlb_raw_fence_intake_closed = 1'b0;
    longint unsigned l2tlb_raw_fence_intake_closed_reset_epoch = 0;
    longint unsigned l2tlb_raw_fence_intake_closed_generation = 0;
    longint unsigned l2tlb_raw_fence_intake_cutoff_sample_seq = 0;
    // The final transport sample is the only mailbox state relevant to the
    // release gate.  Driver-side final confirmation makes it non-empty;
    // driver-side recycle returns it to empty.
    bit l2tlb_transport_sample_mailbox_empty_state = 1'b1;
    uvm_event l2tlb_release_state_changed_ev;

    // Runtime reset uses a small acknowledgement mask.  The coordinator owns
    // only the epoch and grant invalidation; each bit is written by the
    // component that owns the corresponding runtime state.
    parameter bit [4:0] MEMBLOCK_L2TLB_RESET_ACK_CSR      = 5'b00001;
    parameter bit [4:0] MEMBLOCK_L2TLB_RESET_ACK_FENCE    = 5'b00010;
    parameter bit [4:0] MEMBLOCK_L2TLB_RESET_ACK_ADAPTER  = 5'b00100;
    parameter bit [4:0] MEMBLOCK_L2TLB_RESET_ACK_MONITOR  = 5'b01000;
    parameter bit [4:0] MEMBLOCK_L2TLB_RESET_ACK_RESPONSE = 5'b10000;
    bit [4:0] l2tlb_runtime_reset_required_ack_mask = '0;
    bit [4:0] l2tlb_runtime_reset_ack_mask = '0;
    longint unsigned l2tlb_response_owner_reset_done_epoch = 0;
    // Each direct writer records the epoch for which its cleanup function has
    // already run.  The guard is separate from the ack mask so a repeated
    // monitor/service tick cannot repeat a cleanup or ack side effect.
    longint unsigned l2tlb_csr_reset_done_epoch = 0;
    longint unsigned l2tlb_fence_reset_done_epoch = 0;
    longint unsigned l2tlb_adapter_reset_done_epoch = 0;
    longint unsigned l2tlb_driver_reset_done_epoch = 0;
    longint unsigned l2tlb_monitor_reset_done_epoch = 0;
    // Driver-owned proof that the current epoch has crossed one inactive
    // post-reset baseline sample.  Stop/final items may not bypass it.
    bit l2tlb_post_reset_baseline_done_valid = 1'b0;
    longint unsigned l2tlb_post_reset_baseline_done_epoch = 0;
    longint unsigned l2tlb_post_reset_baseline_done_sample_seq = 0;
    string l2tlb_runtime_reset_ack_provenance[0:4];

    // L2TLB monitor reset proof is a transport tuple, not a level.  The
    // monitor writes the pending/processed/ack fields; the coordinator only
    // consumes the ack bit when deciding whether reset may end.
    longint unsigned l2tlb_monitor_reset_pending_epoch = 0;
    longint unsigned l2tlb_monitor_reset_processed_epoch = 0;
    longint unsigned l2tlb_monitor_reset_processed_transport_sample_seq = 0;
    longint unsigned l2tlb_monitor_reset_ack_floor_transport_sample_seq = 0;
    longint unsigned l2tlb_monitor_reset_ack_epoch = 0;
    longint unsigned l2tlb_monitor_reset_ack_transport_sample_seq = 0;

    typedef enum bit [1:0] {
        // 无有效 V2 int-WB 来源，只用于 empty raw 的中性默认值。
        MEMBLOCK_INT_WB_SOURCE_INVALID    = 2'd0,
        // V2 标量 load-address writebackLda_0/1/2，port_id 为 kind 内 lane。
        MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA = 2'd1,
        // V2 store-address writebackSta_0/1，port_id 为 kind 内 lane。
        MEMBLOCK_INT_WB_SOURCE_STA        = 2'd2,
        // V2 store-data writebackStd_0/1，port_id 为 kind 内 lane。
        MEMBLOCK_INT_WB_SOURCE_STD        = 2'd3
    } memblock_int_wb_source_kind_e;

    typedef struct {
        bit                           valid;
        // source_kind 区分 split writeback 类别，port_id 只表示该类别内的物理 lane。
        memblock_int_wb_source_kind_e source_kind;
        int unsigned                  port_id;
        // monitor 在 payload 同拍冻结 flush epoch，adapter 不得用消费拍状态回填来源信息。
        int unsigned                  sample_flush_epoch;
        // 缺 key 标志由 monitor 按真实端口能力设置，后续 adapter 负责查询并补齐。
        bit                           key_needs_state_lookup;
        bit                           rob_value_only_without_flag;
        bit                           rob_valid;
        bit                           rob_flag;
        bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] rob_value;
        bit                           lq_valid;
        bit                           lq_flag;
        bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] lq_value;
        bit                           sq_valid;
        bit                           sq_flag;
        bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] sq_value;
        // metadata valid 明确区分“端口不存在该字段”和“真实字段采样值为 0”。
        bit                           replay_inst_valid;
        bit                           flush_pipe_valid;
        bit                           trigger_valid;
        bit                           replay_inst;
        bit                           flush_pipe;
        bit [3:0]                     trigger;
        // STA0 的 trigger=0 只有在 uncache/CBO provenance 下才可视为中性值。
        // monitor 按真实 debug sideband 设置；adapter 只读，不据此直接推进 pass/fault。
        bit                           debug_is_mmio;
        bit                           debug_is_ncio;
        bit [23:0]                    exception_vec;
        longint unsigned              cycle;
    } dispatch_raw_int_wb_t;

    typedef struct {
        bit               valid;
        int unsigned      port_id;
        bit               is_sta;
        bit               is_std;
        bit               rob_valid;
        bit               rob_flag;
        bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] rob_value;
        bit               lq_valid;
        bit               lq_flag;
        bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] lq_value;
        bit               sq_valid;
        bit               sq_flag;
        bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] sq_value;
        bit               hit;
        bit               flush_state;
        bit [3:0]         source_type;
        bit               vector_feedback;
        longint unsigned  cycle;
    } dispatch_raw_iq_feedback_t;

    typedef struct {
        bit               valid;
        bit [3:0]         lq_deq;
        bit [`MEMBLOCK_SQ_DEQ_COUNT_W-1:0] sq_deq;
        bit               lq_deq_ptr_flag;
        bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] lq_deq_ptr_value;
        // SQ pointer capability 与 payload 分离。V2 count-only profile 恒为 0；
        // pointer-capable profile 只在 sq_deq 非零且真实 pointer 被采样时置 1。
        bit               sq_deq_ptr_valid;
        bit               sq_deq_ptr_flag;
        bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] sq_deq_ptr_value;
        // ctrl monitor 是 MMIO output 的唯一 producer。valid 为 1 的 lane 才允许
        // adapter 读取对应 ROB value；mmio_flush_epoch 固定为采样拍的 flush epoch。
        bit [`MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM-1:0] load_mmio_valid;
        bit [`MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM-1:0][`MEMBLOCK_DUT_ROB_VALUE_W-1:0]
                          load_mmio_rob_value;
        bit               store_mmio_valid;
        bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] store_mmio_rob_value;
        int unsigned      mmio_flush_epoch;
        // 中文注释：MMIO output 由 LoadQueueUncache 的 s1 后一拍脉冲产生；该序号
        // 是 ctrl monitor 采样该脉冲的 DUT sample provenance，不等同于 flush epoch。
        // 只有 monitor 在 MMIO valid 时写入，adapter 不得用当前 sample 重新推导。
        longint unsigned  mmio_sample_seq;
        bit               memory_violation_valid;
        bit               memory_violation_rob_valid;
        bit               memory_violation_rob_flag;
        bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] memory_violation_rob_value;
        bit               memory_violation_ftq_flag;
        bit [`MEMBLOCK_DUT_FTQ_PTR_VALUE_W-1:0] memory_violation_ftq_value;
        bit [`MEMBLOCK_DUT_FTQ_OFFSET_W-1:0] memory_violation_ftq_offset;
        bit               memory_violation_is_rvc;
        bit [49:0]        memory_violation_target;
        bit               memory_violation_level;
        bit               sb_is_empty;
        longint unsigned  cycle;
    } dispatch_raw_ctrl_t;

    // 中文伪代码：ctrl monitor 每个 post-reset sample 都记录一次 held cancel level，
    // 即使两个 count 都为 0 也入队；该队列不属于 semantic ctrl raw batch。
    typedef struct {
        bit [`MEMBLOCK_LQ_CANCEL_COUNT_W-1:0] lq_cancel_count;
        bit [`MEMBLOCK_SQ_CANCEL_COUNT_W-1:0] sq_cancel_count;
        longint unsigned                       sample_seq;
        longint unsigned                       cycle;
    } dispatch_raw_cancel_snapshot_t;

    // 中文伪代码：redirect monitor 只记录 DUT 实际采样到的输入投影和 sample 序号，
    // 不把该 sideband 重新包装成 recovery event。
    typedef struct {
        bit                                      valid;
        // level 保留顶层原始输入；effective_level 按 VLS 规则保存 DUT 有效值。
        bit                                      level;
        bit                                      is_vls_exception;
        bit                                      effective_level;
        bit                                      rob_flag;
        bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0]     rob_value;
        longint unsigned                         sample_seq;
        longint unsigned                         cycle;
    } dispatch_raw_redirect_anchor_t;

    typedef struct {
        bit               valid;
        bit [3:0]         satp_mode;
        bit [15:0]        satp_asid;
        bit [43:0]        satp_ppn;
        bit               satp_changed;
        bit [3:0]         vsatp_mode;
        bit [15:0]        vsatp_asid;
        bit [43:0]        vsatp_ppn;
        bit               vsatp_changed;
        bit [3:0]         hgatp_mode;
        bit [15:0]        hgatp_vmid;
        bit [43:0]        hgatp_ppn;
        bit               hgatp_changed;
        bit               priv_mxr;
        bit               priv_sum;
        bit               priv_vmxr;
        bit               priv_vsum;
        bit               priv_virt;
        bit               priv_virt_changed;
        bit               priv_spvp;
        bit [1:0]         priv_imode;
        bit [1:0]         priv_dmode;
        bit               hd_misalign_ld_enable;
        bit               hd_misalign_st_enable;
        bit               priv_debug;
        bit               m_pbmt_en;
        bit               h_pbmt_en;
        longint unsigned  cycle;
    } dispatch_raw_csr_t;

    typedef struct {
        bit               valid;
        bit               rs1;
        bit               rs2;
        bit [49:0]        addr;
        bit [15:0]        id;
        bit               hv;
        bit               hg;
        longint unsigned  sample_seq;
        time              sample_time;
        // 中文注释：raw fence 必须绑定产生它的 runtime reset epoch 与 CSR context。
        // adapter 只消费同 epoch、同 sample 已绑定的 item，避免旧 fence 删除新 entry。
        longint unsigned  reset_epoch;
        longint unsigned  lifecycle_event_seq;
        bit               context_valid;
        longint unsigned  context_reset_epoch;
        bit               priv_virt_at_sample;
        bit [15:0]        hgatp_vmid_at_sample;
        bit [3:0]         satp_mode_at_sample;
        bit [3:0]         vsatp_mode_at_sample;
        bit [3:0]         hgatp_mode_at_sample;
        longint unsigned  csr_sample_seq;
        longint unsigned  cycle;
    } dispatch_raw_sfence_t;

    // L2TLB lifecycle reason bits. A CSR change and a fence observed in the
    // same DUT sample share one event record rather than creating two barriers.
    localparam bit [1:0] MEMBLOCK_L2TLB_REASON_CSR   = 2'b01;
    localparam bit [1:0] MEMBLOCK_L2TLB_REASON_FENCE = 2'b10;
    localparam longint unsigned MEMBLOCK_L2TLB_EVENT_SEQ_NONE = 0;

    typedef enum int unsigned {
        MEMBLOCK_L2TLB_ITEM_NORMAL,
        MEMBLOCK_L2TLB_ITEM_RELEASE_STOP,
        MEMBLOCK_L2TLB_ITEM_RELEASE_FINAL_INACTIVE
    } memblock_l2tlb_release_item_kind_e;

    typedef enum int unsigned {
        MEMBLOCK_L2TLB_SAMPLE_NOT_READY,
        MEMBLOCK_L2TLB_SAMPLE_READY
    } memblock_l2tlb_sample_ready_result_e;

    typedef enum int unsigned {
        MEMBLOCK_L2TLB_SAMPLE_CONSUMED,
        MEMBLOCK_L2TLB_SAMPLE_DROPPED
    } memblock_l2tlb_transport_terminal_e;

    typedef struct {
        longint unsigned event_seq;
        bit [1:0]         reason_mask;
        longint unsigned anchor_sample_seq;
        time              sample_time;
    } memblock_l2tlb_event_record_t;

    typedef struct {
        longint unsigned sample_seq;
        dispatch_raw_csr_t payload;
        bit                 valid;
    } memblock_l2tlb_csr_history_entry_t;

    typedef struct {
        bit               valid;
        longint unsigned  sample_seq;
        time              sample_time;
        longint unsigned  reset_epoch;
        bit               priv_virt_at_sample;
        bit [15:0]        hgatp_vmid_at_sample;
        bit [3:0]         satp_mode_at_sample;
        bit [3:0]         vsatp_mode_at_sample;
        bit [3:0]         hgatp_mode_at_sample;
    } memblock_l2tlb_sfence_csr_context_t;

    dispatch_raw_int_wb_t      raw_int_wb_q[$];
    dispatch_raw_iq_feedback_t raw_iq_feedback_q[$];
    dispatch_raw_ctrl_t        raw_ctrl_q[$];
    // ctrl raw 已完成 semantic conversion、但尚未完成 LSQ owner apply 的持久 FIFO。
    // 不能放在一次 service task 的 automatic queue 中，否则 resync warning 后会丢失。
    dispatch_raw_ctrl_t        deferred_raw_ctrl_q[$];
    dispatch_raw_cancel_snapshot_t raw_cancel_snapshot_q[$];
    dispatch_raw_redirect_anchor_t raw_redirect_anchor_q[$];
    dispatch_raw_sfence_t      raw_sfence_q[$];
    // 中文注释：CSR monitor 每个 DUT sample 发布一份短生命周期 context。
    // 同拍 fence 先到时 raw 留在 FIFO 等待绑定；下拍仍未绑定即为 monitor 时序错误。
    memblock_l2tlb_sfence_csr_context_t l2tlb_sfence_csr_context;
    dispatch_raw_csr_t         latest_raw_csr;
    bit                        latest_raw_csr_valid;
    int unsigned               latest_raw_csr_seq;
    // 中文注释：CSR monitor独立发布的post-reset runtime latest视图。
    // payload变化时序号单调递增；clear semantic raw queue和DUT reset均不清该公共快照。
    dispatch_raw_csr_t         runtime_csr_snapshot;
    bit                        runtime_csr_snapshot_valid;
    int unsigned               runtime_csr_snapshot_seq;
    // 中文注释：CSR changed或sfence monitor事件的non-destructive history。
    // producer只追加/合并，唯一response owner按连续cursor回收前缀。
    longint unsigned           l2tlb_flush_event_seq;
    time                       l2tlb_flush_sample_time;
    bit                        l2tlb_flush_event_valid;
    bit [1:0]                   l2tlb_flush_event_reason_mask;
    longint unsigned            last_allocated_l2tlb_event_seq;
    memblock_l2tlb_event_record_t l2tlb_flush_event_history[$];
    longint unsigned            response_owner_event_cursor;
    longint unsigned            lifecycle_event_published_seq;
    longint unsigned            sample_producer_active_seq;
    bit [1:0]                    sample_producer_done_mask;
    bit [1:0]                    sample_producer_reason_mask;
    bit                          dispatch_l2tlb_lookup_active = 1'b0;
    bit                          l2tlb_sample_anchor_valid;
    longint unsigned           dut_sample_seq;
    longint unsigned           dut_sample_time;
    bit                        dut_sample_time_valid;
    longint unsigned            csr_history_published_seq;
    memblock_l2tlb_csr_history_entry_t l2tlb_csr_history[0:2];

    function dispatch_raw_int_wb_t make_empty_raw_int_wb();
        dispatch_raw_int_wb_t item;
        item.valid                       = 1'b0;
        item.source_kind                 = MEMBLOCK_INT_WB_SOURCE_INVALID;
        item.port_id                     = 0;
        item.sample_flush_epoch          = 0;
        item.key_needs_state_lookup      = 1'b0;
        item.rob_value_only_without_flag = 1'b0;
        item.rob_valid                   = 1'b0;
        item.rob_flag                    = 1'b0;
        item.rob_value                   = '0;
        item.lq_valid                    = 1'b0;
        item.lq_flag                     = 1'b0;
        item.lq_value                    = '0;
        item.sq_valid                    = 1'b0;
        item.sq_flag                     = 1'b0;
        item.sq_value                    = '0;
        item.replay_inst_valid           = 1'b0;
        item.flush_pipe_valid            = 1'b0;
        item.trigger_valid               = 1'b0;
        item.replay_inst                 = 1'b0;
        item.flush_pipe                  = 1'b0;
        item.trigger                     = 4'hf;
        item.debug_is_mmio                = 1'b0;
        item.debug_is_ncio                = 1'b0;
        item.exception_vec               = '0;
        item.cycle                       = 0;
        return item;
    endfunction:make_empty_raw_int_wb

    function dispatch_raw_iq_feedback_t make_empty_raw_iq_feedback();
        dispatch_raw_iq_feedback_t item;
        item.valid           = 1'b0;
        item.port_id         = 0;
        item.is_sta          = 1'b0;
        item.is_std          = 1'b0;
        item.rob_valid       = 1'b0;
        item.rob_flag        = 1'b0;
        item.rob_value       = '0;
        item.lq_valid        = 1'b0;
        item.lq_flag         = 1'b0;
        item.lq_value        = '0;
        item.sq_valid        = 1'b0;
        item.sq_flag         = 1'b0;
        item.sq_value        = '0;
        item.hit             = 1'b0;
        item.flush_state     = 1'b0;
        item.source_type     = '0;
        item.vector_feedback = 1'b0;
        item.cycle           = 0;
        return item;
    endfunction:make_empty_raw_iq_feedback

    function dispatch_raw_ctrl_t make_empty_raw_ctrl();
        dispatch_raw_ctrl_t item;
        item.valid                      = 1'b0;
        item.lq_deq                     = '0;
        item.sq_deq                     = '0;
        item.lq_deq_ptr_flag            = 1'b0;
        item.lq_deq_ptr_value           = '0;
        item.sq_deq_ptr_valid           = 1'b0;
        item.sq_deq_ptr_flag            = 1'b0;
        item.sq_deq_ptr_value           = '0;
        item.load_mmio_valid            = '0;
        item.load_mmio_rob_value        = '0;
        item.store_mmio_valid           = 1'b0;
        item.store_mmio_rob_value       = '0;
        item.mmio_flush_epoch           = 0;
        item.mmio_sample_seq            = 0;
        item.memory_violation_valid     = 1'b0;
        item.memory_violation_rob_valid = 1'b0;
        item.memory_violation_rob_flag  = 1'b0;
        item.memory_violation_rob_value = '0;
        item.memory_violation_ftq_flag  = 1'b0;
        item.memory_violation_ftq_value = '0;
        item.memory_violation_ftq_offset = '0;
        item.memory_violation_is_rvc    = 1'b0;
        item.memory_violation_target    = '0;
        item.memory_violation_level     = 1'b0;
        item.sb_is_empty                 = 1'b0;
        item.cycle                      = 0;
        return item;
    endfunction:make_empty_raw_ctrl

    function dispatch_raw_cancel_snapshot_t make_empty_raw_cancel_snapshot();
        dispatch_raw_cancel_snapshot_t item;
        item.lq_cancel_count = '0;
        item.sq_cancel_count = '0;
        item.sample_seq = 0;
        item.cycle = 0;
        return item;
    endfunction:make_empty_raw_cancel_snapshot

    function dispatch_raw_redirect_anchor_t make_empty_raw_redirect_anchor();
        dispatch_raw_redirect_anchor_t item;
        item.valid = 1'b0;
        item.level = 1'b0;
        item.is_vls_exception = 1'b0;
        item.effective_level = 1'b0;
        item.rob_flag = 1'b0;
        item.rob_value = '0;
        item.sample_seq = 0;
        item.cycle = 0;
        return item;
    endfunction:make_empty_raw_redirect_anchor

    function dispatch_raw_csr_t make_empty_raw_csr();
        dispatch_raw_csr_t item;
        item.valid             = 1'b0;
        item.satp_mode         = '0;
        item.satp_asid         = '0;
        item.satp_ppn          = '0;
        item.satp_changed      = 1'b0;
        item.vsatp_mode        = '0;
        item.vsatp_asid        = '0;
        item.vsatp_ppn         = '0;
        item.vsatp_changed     = 1'b0;
        item.hgatp_mode        = '0;
        item.hgatp_vmid        = '0;
        item.hgatp_ppn         = '0;
        item.hgatp_changed     = 1'b0;
        item.priv_mxr          = 1'b0;
        item.priv_sum          = 1'b0;
        item.priv_vmxr         = 1'b0;
        item.priv_vsum         = 1'b0;
        item.priv_virt         = 1'b0;
        item.priv_virt_changed = 1'b0;
        item.priv_spvp         = 1'b0;
        item.priv_imode        = '0;
        item.priv_dmode        = '0;
        item.hd_misalign_ld_enable = 1'b1;
        item.hd_misalign_st_enable = 1'b1;
        item.priv_debug        = 1'b0;
        item.m_pbmt_en         = 1'b0;
        item.h_pbmt_en         = 1'b0;
        item.cycle             = 0;
        return item;
    endfunction:make_empty_raw_csr

    function dispatch_raw_sfence_t make_empty_raw_sfence();
        dispatch_raw_sfence_t item;
        item.valid = 1'b0;
        item.rs1   = 1'b0;
        item.rs2   = 1'b0;
        item.addr  = '0;
        item.id    = '0;
        item.hv    = 1'b0;
        item.hg    = 1'b0;
        item.sample_seq = 0;
        item.sample_time = 0;
        item.reset_epoch = 0;
        item.lifecycle_event_seq = MEMBLOCK_L2TLB_EVENT_SEQ_NONE;
        item.context_valid = 1'b0;
        item.context_reset_epoch = 0;
        item.priv_virt_at_sample = 1'b0;
        item.hgatp_vmid_at_sample = '0;
        item.satp_mode_at_sample = '0;
        item.vsatp_mode_at_sample = '0;
        item.hgatp_mode_at_sample = '0;
        item.csr_sample_seq = 0;
        item.cycle = 0;
        return item;
    endfunction:make_empty_raw_sfence

    function memblock_l2tlb_sfence_csr_context_t make_empty_l2tlb_sfence_csr_context();
        memblock_l2tlb_sfence_csr_context_t context;

        context.valid = 1'b0;
        context.sample_seq = 0;
        context.sample_time = 0;
        context.reset_epoch = 0;
        context.priv_virt_at_sample = 1'b0;
        context.hgatp_vmid_at_sample = '0;
        context.satp_mode_at_sample = '0;
        context.vsatp_mode_at_sample = '0;
        context.hgatp_mode_at_sample = '0;
        return context;
    endfunction:make_empty_l2tlb_sfence_csr_context

    function bit raw_csr_payload_changed(input dispatch_raw_csr_t prev,
                                         input dispatch_raw_csr_t cur);
        return
            prev.satp_mode         != cur.satp_mode         ||
            prev.satp_asid         != cur.satp_asid         ||
            prev.satp_ppn          != cur.satp_ppn          ||
            prev.vsatp_mode        != cur.vsatp_mode        ||
            prev.vsatp_asid        != cur.vsatp_asid        ||
            prev.vsatp_ppn         != cur.vsatp_ppn         ||
            prev.hgatp_mode        != cur.hgatp_mode        ||
            prev.hgatp_vmid        != cur.hgatp_vmid        ||
            prev.hgatp_ppn         != cur.hgatp_ppn         ||
            prev.priv_mxr          != cur.priv_mxr          ||
            prev.priv_sum          != cur.priv_sum          ||
            prev.priv_vmxr         != cur.priv_vmxr         ||
            prev.priv_vsum         != cur.priv_vsum         ||
            prev.priv_virt         != cur.priv_virt         ||
            prev.priv_spvp         != cur.priv_spvp         ||
            prev.priv_imode        != cur.priv_imode        ||
            prev.priv_dmode        != cur.priv_dmode        ||
            prev.hd_misalign_ld_enable != cur.hd_misalign_ld_enable ||
            prev.hd_misalign_st_enable != cur.hd_misalign_st_enable ||
            prev.priv_debug        != cur.priv_debug        ||
            prev.m_pbmt_en         != cur.m_pbmt_en         ||
            prev.h_pbmt_en         != cur.h_pbmt_en         ||
            (cur.satp_changed      && !prev.satp_changed)   ||
            (cur.vsatp_changed     && !prev.vsatp_changed)  ||
            (cur.hgatp_changed     && !prev.hgatp_changed)  ||
            (cur.priv_virt_changed && !prev.priv_virt_changed);
    endfunction:raw_csr_payload_changed

    // Abstract responsibility: freeze the testcase-level L2TLB topology before
    // reset/sample processing begins.  It chooses which direct writers can
    // exist; it does not start a sequence or alter response ownership.
    function void initialize_l2tlb_testcase_lifecycle(
        input memblock_l2tlb_responder_mode_e responder_mode,
        input memblock_l2tlb_dispatch_topology_e dispatch_topology,
        input memblock_l2tlb_start_mode_e start_mode,
        input bit needs_response,
        input bit connect_takeover_active,
        input string topology_name);
        if (topology_name == "") begin
            `uvm_fatal("MEMBLOCK_L2TLB_TOPOLOGY",
                       "L2TLB testcase topology requires a non-empty owner name")
        end
        if (connect_takeover_active != l2tlb_responder_active) begin
            `uvm_fatal("MEMBLOCK_L2TLB_TOPOLOGY",
                       $sformatf("L2TLB connect capability mismatch input=%0d observed=%0d topology=%s",
                                 connect_takeover_active, l2tlb_responder_active,
                                 topology_name))
        end
        if (responder_mode == MEMBLOCK_L2TLB_RESPONDER_DISABLED) begin
            if (dispatch_topology != MEMBLOCK_L2TLB_TOPOLOGY_NO_DISPATCH ||
                start_mode != MEMBLOCK_L2TLB_START_DISABLED ||
                needs_response) begin
                `uvm_fatal("MEMBLOCK_L2TLB_TOPOLOGY",
                           $sformatf("invalid disabled L2TLB topology=%s dispatch=%0d start=%0d needs_response=%0d",
                                     topology_name, dispatch_topology, start_mode,
                                     needs_response))
            end
        end
        else begin
            if (!connect_takeover_active ||
                dispatch_topology != MEMBLOCK_L2TLB_TOPOLOGY_DISPATCH_ACTIVE ||
                !(start_mode inside {MEMBLOCK_L2TLB_START_DEFAULT,
                                     MEMBLOCK_L2TLB_START_EXPLICIT}) ||
                !needs_response) begin
                `uvm_fatal("MEMBLOCK_L2TLB_TOPOLOGY",
                           $sformatf("invalid enabled L2TLB topology=%s connect=%0d dispatch=%0d start=%0d needs_response=%0d",
                                     topology_name, connect_takeover_active,
                                     dispatch_topology, start_mode,
                                     needs_response))
            end
        end
        if (l2tlb_testcase_lifecycle_initialized) begin
            if (l2tlb_testcase_responder_mode != responder_mode ||
                l2tlb_testcase_dispatch_topology != dispatch_topology ||
                l2tlb_testcase_start_mode != start_mode ||
                l2tlb_testcase_needs_response != needs_response ||
                l2tlb_testcase_topology_name != topology_name) begin
                `uvm_fatal("MEMBLOCK_L2TLB_TOPOLOGY",
                           $sformatf("L2TLB testcase topology changed old=%s new=%s",
                                     l2tlb_testcase_topology_name, topology_name))
            end
            return;
        end
        l2tlb_testcase_responder_mode = responder_mode;
        l2tlb_testcase_dispatch_topology = dispatch_topology;
        l2tlb_testcase_start_mode = start_mode;
        l2tlb_testcase_needs_response = needs_response;
        l2tlb_testcase_topology_name = topology_name;
        dispatch_l2tlb_lookup_active =
            (dispatch_topology == MEMBLOCK_L2TLB_TOPOLOGY_DISPATCH_ACTIVE);
        l2tlb_testcase_lifecycle_initialized = 1'b1;
    endfunction:initialize_l2tlb_testcase_lifecycle

    function bit l2tlb_responder_enabled();
        return l2tlb_testcase_lifecycle_initialized &&
               l2tlb_testcase_responder_mode == MEMBLOCK_L2TLB_RESPONDER_ENABLED;
    endfunction:l2tlb_responder_enabled

    function bit l2tlb_dispatch_active();
        return l2tlb_testcase_lifecycle_initialized &&
               l2tlb_testcase_dispatch_topology ==
                   MEMBLOCK_L2TLB_TOPOLOGY_DISPATCH_ACTIVE;
    endfunction:l2tlb_dispatch_active

    // Abstract responsibility: record the unique service that can clear
    // adapter-owned raw/live state on later runtime resets.  This is separate
    // from connect takeover and does not modify the fixed testcase topology.
    function void register_l2tlb_adapter_service(input string owner_name);
        if (!l2tlb_testcase_lifecycle_initialized ||
            !l2tlb_dispatch_active() || owner_name == "") begin
            `uvm_fatal("MEMBLOCK_L2TLB_TOPOLOGY",
                       $sformatf("invalid L2TLB adapter service registration owner=%s initialized=%0d dispatch=%0d",
                                 owner_name, l2tlb_testcase_lifecycle_initialized,
                                 l2tlb_dispatch_active()))
        end
        if (l2tlb_adapter_service_active &&
            l2tlb_adapter_service_owner_name != owner_name) begin
            `uvm_fatal("MEMBLOCK_L2TLB_TOPOLOGY",
                       $sformatf("duplicate L2TLB adapter service old=%s new=%s",
                                 l2tlb_adapter_service_owner_name, owner_name))
        end
        l2tlb_adapter_service_active = 1'b1;
        l2tlb_adapter_service_owner_name = owner_name;
    endfunction:register_l2tlb_adapter_service

    function void unregister_l2tlb_adapter_service(input string owner_name);
        if (!l2tlb_adapter_service_active) begin
            return;
        end
        if (l2tlb_adapter_service_owner_name != owner_name) begin
            `uvm_fatal("MEMBLOCK_L2TLB_TOPOLOGY",
                       $sformatf("L2TLB adapter service unregister owner mismatch old=%s new=%s",
                                 l2tlb_adapter_service_owner_name, owner_name))
        end
        if (l2tlb_reset_active()) begin
            `uvm_fatal("MEMBLOCK_L2TLB_TOPOLOGY",
                       "cannot remove L2TLB adapter service during runtime reset")
        end
        l2tlb_adapter_service_active = 1'b0;
        l2tlb_adapter_service_owner_name = "";
    endfunction:unregister_l2tlb_adapter_service

    function bit try_claim_l2tlb_lifecycle_owner(input string owner_name,
                                                  output string current_owner);
        current_owner = l2tlb_lifecycle_owner_name;
        if (l2tlb_runtime_reset_active || reset_backend_done !== 1'b1) begin
            // A lifecycle owner must not be created while reset is still
            // being coordinated.  The caller waits for reset release and
            // retries; this function never silently claims a reset epoch.
            return 1'b0;
        end
        if (l2tlb_lifecycle_owner_claimed) begin
            return 1'b0;
        end
        if (l2tlb_owner_claimed_once) begin
            current_owner = l2tlb_lifecycle_owner_name;
            return 1'b0;
        end
        l2tlb_lifecycle_owner_claimed = 1'b1;
        l2tlb_lifecycle_owner_name = owner_name;
        l2tlb_owner_claimed_once = 1'b1;
        current_owner = owner_name;
        return 1'b1;
    endfunction:try_claim_l2tlb_lifecycle_owner

    function bit try_release_l2tlb_lifecycle_owner(input string owner_name,
                                                    output string current_owner);
        current_owner = l2tlb_lifecycle_owner_name;
        if (!l2tlb_lifecycle_owner_claimed ||
            l2tlb_lifecycle_owner_name != owner_name) begin
            return 1'b0;
        end
        if (!l2tlb_release_granted ||
            l2tlb_release_grant_owner_name != owner_name ||
            !release_grantable(owner_name,
                               l2tlb_release_grant_reset_epoch)) begin
            return 1'b0;
        end
        l2tlb_lifecycle_owner_claimed = 1'b0;
        l2tlb_lifecycle_owner_name = "";
        l2tlb_owner_admission_settled_sample_seq = 0;
        l2tlb_release_admission_close_requested = 1'b0;
        l2tlb_release_admission_request_owner_name = "";
        l2tlb_release_admission_close_request_sample_seq = 0;
        l2tlb_release_admission_close_reset_epoch = 0;
        l2tlb_release_closing = 1'b0;
        l2tlb_release_closing_owner_name = "";
        l2tlb_response_drain_done = 1'b0;
        l2tlb_response_drain_owner_name = "";
        current_owner = "";
        return 1'b1;
    endfunction:try_release_l2tlb_lifecycle_owner

    function uvm_event get_l2tlb_release_state_changed_ev();
        if (l2tlb_release_state_changed_ev == null) begin
            l2tlb_release_state_changed_ev = new("l2tlb_release_state_changed_ev");
        end
        return l2tlb_release_state_changed_ev;
    endfunction:get_l2tlb_release_state_changed_ev

    function bit l2tlb_reset_active();
        return l2tlb_runtime_reset_active;
    endfunction:l2tlb_reset_active

    function longint unsigned get_l2tlb_current_reset_epoch();
        return l2tlb_current_reset_epoch;
    endfunction:get_l2tlb_current_reset_epoch

    // The coordinator only publishes the reset boundary and invalidates a
    // pending grant.  Each direct owner clears its own queues/proofs.
    function void begin_l2tlb_runtime_reset();
        if (!l2tlb_runtime_reset_active) begin
            l2tlb_current_reset_epoch++;
            if (l2tlb_current_reset_epoch == 0) begin
                `uvm_fatal("MEMBLOCK_L2TLB_RESET", "L2TLB reset epoch wrapped to zero")
            end

            // The fixed topology determines which roles may exist.  The
            // adapter acknowledgement is required only after its service has
            // actually registered: initial physical reset precedes UVM
            // sequence startup and therefore has no adapter-owned state.
            // A response owner is likewise required only after claim.
            l2tlb_runtime_reset_required_ack_mask =
                MEMBLOCK_L2TLB_RESET_ACK_CSR |
                MEMBLOCK_L2TLB_RESET_ACK_FENCE |
                MEMBLOCK_L2TLB_RESET_ACK_MONITOR;
            if (l2tlb_adapter_service_active) begin
                l2tlb_runtime_reset_required_ack_mask |=
                    MEMBLOCK_L2TLB_RESET_ACK_ADAPTER;
            end
            if (l2tlb_lifecycle_owner_claimed) begin
                l2tlb_runtime_reset_required_ack_mask |=
                    MEMBLOCK_L2TLB_RESET_ACK_RESPONSE;
            end
            l2tlb_runtime_reset_ack_mask = '0;
            foreach (l2tlb_runtime_reset_ack_provenance[idx]) begin
                l2tlb_runtime_reset_ack_provenance[idx] = "";
            end

            // Grant provenance belongs to the coordinator.  Other release
            // proof, token, mailbox, CSR and raw-fence state is deliberately
            // left to its direct writer below.
            l2tlb_release_granted = 1'b0;
            l2tlb_release_grant_owner_name = "";
            l2tlb_release_grant_reset_epoch = 0;
            l2tlb_release_grant_generation = 0;
            l2tlb_response_owner_reset_done_epoch = 0;
        end
        l2tlb_runtime_reset_active = 1'b1;
        get_l2tlb_release_state_changed_ev().trigger();
    endfunction:begin_l2tlb_runtime_reset

    // Abstract responsibility: record completion by one reset direct writer.
    // The ack contains no cleanup side effect and is valid only for the live
    // reset epoch, preventing a delayed old callback from re-arming reset.
    function void acknowledge_l2tlb_runtime_reset(
        input bit [4:0] owner_ack_bit,
        input longint unsigned reset_epoch,
        input string writer_name);
        int unsigned ack_idx;
        if (!l2tlb_runtime_reset_active || reset_epoch == 0 ||
            reset_epoch != l2tlb_current_reset_epoch ||
            owner_ack_bit == '0 ||
            (owner_ack_bit & (owner_ack_bit - 1'b1)) != '0 ||
            (owner_ack_bit & l2tlb_runtime_reset_required_ack_mask) != owner_ack_bit ||
            writer_name == "") begin
            `uvm_fatal("MEMBLOCK_L2TLB_RESET",
                       $sformatf("invalid L2TLB reset ack bit=0x%0h epoch=%0d current=%0d active=%0d required=0x%0h writer=%s",
                                 owner_ack_bit, reset_epoch,
                                 l2tlb_current_reset_epoch,
                                 l2tlb_runtime_reset_active,
                                 l2tlb_runtime_reset_required_ack_mask,
                                 writer_name))
        end
        case (owner_ack_bit)
            MEMBLOCK_L2TLB_RESET_ACK_CSR:      ack_idx = 0;
            MEMBLOCK_L2TLB_RESET_ACK_FENCE:    ack_idx = 1;
            MEMBLOCK_L2TLB_RESET_ACK_ADAPTER:  ack_idx = 2;
            MEMBLOCK_L2TLB_RESET_ACK_MONITOR:  ack_idx = 3;
            MEMBLOCK_L2TLB_RESET_ACK_RESPONSE: ack_idx = 4;
            default: begin
                `uvm_fatal("MEMBLOCK_L2TLB_RESET",
                           $sformatf("unmapped L2TLB reset ack bit=0x%0h", owner_ack_bit))
                ack_idx = 0;
            end
        endcase
        if ((l2tlb_runtime_reset_ack_mask & owner_ack_bit) != '0) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RESET",
                       $sformatf("duplicate L2TLB reset ack bit=0x%0h epoch=%0d writer=%s previous_writer=%s",
                                 owner_ack_bit, reset_epoch, writer_name,
                                 l2tlb_runtime_reset_ack_provenance[ack_idx]))
        end
        l2tlb_runtime_reset_ack_mask |= owner_ack_bit;
        l2tlb_runtime_reset_ack_provenance[ack_idx] = writer_name;
        get_l2tlb_release_state_changed_ev().trigger();
    endfunction:acknowledge_l2tlb_runtime_reset

    function bit l2tlb_runtime_reset_acks_complete();
        return (l2tlb_runtime_reset_ack_mask &
                l2tlb_runtime_reset_required_ack_mask) ==
               l2tlb_runtime_reset_required_ack_mask;
    endfunction:l2tlb_runtime_reset_acks_complete

    // CSR monitor direct-writer reset.  The allocator remains monotonic so
    // post-reset records cannot be confused with old history.
    function void reset_l2tlb_csr_runtime_state(
        input longint unsigned reset_epoch);
        if (!l2tlb_runtime_reset_active ||
            reset_epoch != l2tlb_current_reset_epoch) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RESET", "CSR reset direct-writer epoch mismatch")
        end
        if (l2tlb_csr_reset_done_epoch == reset_epoch) begin
            return;
        end
        l2tlb_flush_event_history.delete();
        l2tlb_flush_event_seq = MEMBLOCK_L2TLB_EVENT_SEQ_NONE;
        l2tlb_flush_event_reason_mask = 2'b00;
        l2tlb_flush_sample_time = 0;
        l2tlb_flush_event_valid = 1'b0;
        sample_producer_active_seq = 0;
        sample_producer_done_mask = 2'b00;
        sample_producer_reason_mask = 2'b00;
        l2tlb_sfence_csr_context = make_empty_l2tlb_sfence_csr_context();
        lifecycle_event_published_seq = 0;
        csr_history_published_seq = 0;
        foreach (l2tlb_csr_history[idx]) begin
            l2tlb_csr_history[idx].sample_seq = 0;
            l2tlb_csr_history[idx].payload = make_empty_raw_csr();
            l2tlb_csr_history[idx].valid = 1'b0;
        end
        l2tlb_csr_reset_done_epoch = reset_epoch;
        acknowledge_l2tlb_runtime_reset(MEMBLOCK_L2TLB_RESET_ACK_CSR,
                                        reset_epoch,
                                        "csr_ctrl_agent_agent_monitor");
    endfunction:reset_l2tlb_csr_runtime_state

    // Fence monitor direct-writer reset.  It does not touch adapter raw FIFO.
    function void reset_l2tlb_fence_runtime_state(
        input longint unsigned reset_epoch);
        if (!l2tlb_runtime_reset_active ||
            reset_epoch != l2tlb_current_reset_epoch) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RESET", "fence reset direct-writer epoch mismatch")
        end
        if (l2tlb_fence_reset_done_epoch == reset_epoch) begin
            return;
        end
        l2tlb_raw_fence_producer_settled_sample_seq = 0;
        l2tlb_raw_fence_intake_closed = 1'b0;
        l2tlb_raw_fence_intake_closed_reset_epoch = 0;
        l2tlb_raw_fence_intake_closed_generation = 0;
        l2tlb_raw_fence_intake_cutoff_sample_seq = 0;
        l2tlb_fence_reset_done_epoch = reset_epoch;
        acknowledge_l2tlb_runtime_reset(MEMBLOCK_L2TLB_RESET_ACK_FENCE,
                                        reset_epoch,
                                        "fence_agent_agent_monitor");
    endfunction:reset_l2tlb_fence_runtime_state

    // Adapter direct-writer reset. This helper clears package-owned raw state
    // only; CSR monitor remains the sole writer that clears the short-lived
    // SFENCE CSR context. The adapter sends its ACK after clearing its live
    // table and range index.
    function void reset_l2tlb_adapter_runtime_state(
        input longint unsigned reset_epoch);
        if (!l2tlb_runtime_reset_active ||
            reset_epoch != l2tlb_current_reset_epoch) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RESET", "adapter reset direct-writer epoch mismatch")
        end
        if (l2tlb_adapter_reset_done_epoch == reset_epoch) begin
            return;
        end
        raw_sfence_q.delete();
        l2tlb_adapter_drain_done = 1'b0;
        l2tlb_adapter_drain_epoch = 0;
        l2tlb_adapter_drain_generation = 0;
        l2tlb_adapter_reset_done_epoch = reset_epoch;
    endfunction:reset_l2tlb_adapter_runtime_state

    // Response owner direct-writer reset.  Token/UID cancellation is still
    // performed by the sequence before it calls this helper.
    function void reset_l2tlb_response_owner_runtime_state(
        input string owner_name,
        input longint unsigned reset_epoch);
        if (!l2tlb_runtime_reset_active ||
            reset_epoch != l2tlb_current_reset_epoch ||
            !l2tlb_lifecycle_owner_claimed ||
            l2tlb_lifecycle_owner_name != owner_name) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RESET", "response-owner reset direct-writer mismatch")
        end
        if (l2tlb_response_owner_reset_done_epoch == reset_epoch) begin
            return;
        end
        l2tlb_owner_admission_settled_sample_seq = 0;
        l2tlb_release_admission_close_requested = 1'b0;
        l2tlb_release_admission_request_owner_name = "";
        l2tlb_release_admission_close_request_sample_seq = 0;
        l2tlb_release_admission_close_reset_epoch = 0;
        l2tlb_release_admission_closed = 1'b0;
        l2tlb_release_admission_owner_name = "";
        l2tlb_release_admission_closed_generation = 0;
        l2tlb_release_admission_cutoff_sample_seq = 0;
        l2tlb_response_drain_done = 1'b0;
        l2tlb_response_drain_owner_name = "";
        l2tlb_response_drain_generation = 0;
        l2tlb_release_closing = 1'b0;
        l2tlb_release_closing_owner_name = "";
        l2tlb_release_closing_generation = 0;
        response_owner_event_cursor = last_allocated_l2tlb_event_seq;
        // The response owner only proves that its token/UID state is reset.
        // The driver writes the RESPONSE ack after its transport slot and
        // local metadata have also become quiescent.
        l2tlb_response_owner_reset_done_epoch = reset_epoch;
    endfunction:reset_l2tlb_response_owner_runtime_state

    // Driver and monitor each clear only their own transport proof state.
    function void reset_l2tlb_driver_runtime_state(
        input longint unsigned reset_epoch);
        if (!l2tlb_runtime_reset_active ||
            reset_epoch != l2tlb_current_reset_epoch) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RESET", "driver reset direct-writer epoch mismatch")
        end
        if (l2tlb_driver_reset_done_epoch == reset_epoch) begin
            return;
        end
        l2tlb_release_final_inactive_item_done = 1'b0;
        l2tlb_release_final_inactive_generation = 0;
        l2tlb_release_final_inactive_transport_sample_seq = 0;
        l2tlb_transport_sample_recycle_done_seq = 0;
        l2tlb_post_reset_baseline_done_valid = 1'b0;
        l2tlb_post_reset_baseline_done_epoch = 0;
        l2tlb_post_reset_baseline_done_sample_seq = 0;
        l2tlb_driver_reset_done_epoch = reset_epoch;
    endfunction:reset_l2tlb_driver_runtime_state

    // Abstract responsibility: publish the driver proof that one NORMAL
    // inactive item crossed a later real sample in this reset epoch.  It
    // prevents a newly re-armed owner from confirming release stop/final
    // metadata before the transport baseline exists.
    function void mark_l2tlb_post_reset_baseline_done(
        input longint unsigned reset_epoch,
        input longint unsigned sample_seq);
        if (reset_epoch == 0 || sample_seq == 0 ||
            reset_epoch != get_l2tlb_current_reset_epoch() ||
            l2tlb_reset_active()) begin
            `uvm_fatal("MEMBLOCK_L2TLB_BASELINE",
                       $sformatf("invalid post-reset baseline proof epoch=%0d current=%0d sample=%0d reset=%0d",
                                 reset_epoch, get_l2tlb_current_reset_epoch(),
                                 sample_seq, l2tlb_reset_active()))
        end
        if (l2tlb_post_reset_baseline_done_valid) begin
            if (l2tlb_post_reset_baseline_done_epoch != reset_epoch ||
                l2tlb_post_reset_baseline_done_sample_seq != sample_seq) begin
                `uvm_fatal("MEMBLOCK_L2TLB_BASELINE",
                           $sformatf("post-reset baseline proof changed old=%0d/%0d new=%0d/%0d",
                                     l2tlb_post_reset_baseline_done_epoch,
                                     l2tlb_post_reset_baseline_done_sample_seq,
                                     reset_epoch, sample_seq))
            end
            return;
        end
        l2tlb_post_reset_baseline_done_valid = 1'b1;
        l2tlb_post_reset_baseline_done_epoch = reset_epoch;
        l2tlb_post_reset_baseline_done_sample_seq = sample_seq;
    endfunction:mark_l2tlb_post_reset_baseline_done

    function bit l2tlb_post_reset_baseline_done(
        input longint unsigned reset_epoch);
        return l2tlb_post_reset_baseline_done_valid &&
               l2tlb_post_reset_baseline_done_epoch == reset_epoch &&
               l2tlb_post_reset_baseline_done_sample_seq != 0;
    endfunction:l2tlb_post_reset_baseline_done

    // Abstract responsibility: close the response reset handshake only after
    // both direct owners have proved their state and the transport mailbox is
    // empty.  This helper is called by the driver at a real drv_cb boundary;
    // it never clears the mailbox or substitutes for its recycle operation.
    function void acknowledge_l2tlb_response_reset_if_quiescent(
        input string owner_name,
        input longint unsigned reset_epoch);
        if (!l2tlb_runtime_reset_active ||
            !(l2tlb_runtime_reset_required_ack_mask &
              MEMBLOCK_L2TLB_RESET_ACK_RESPONSE)) begin
            return;
        end
        if (reset_epoch != l2tlb_current_reset_epoch ||
            !l2tlb_lifecycle_owner_claimed ||
            l2tlb_lifecycle_owner_name != owner_name) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RESET",
                       $sformatf("response reset quiescent owner/epoch mismatch owner=%s current=%s epoch=%0d current_epoch=%0d",
                                 owner_name, l2tlb_lifecycle_owner_name,
                                 reset_epoch, l2tlb_current_reset_epoch))
        end
        if (l2tlb_response_owner_reset_done_epoch != reset_epoch ||
            l2tlb_driver_reset_done_epoch != reset_epoch ||
            !l2tlb_transport_sample_mailbox_empty()) begin
            return;
        end
        if (l2tlb_runtime_reset_ack_mask & MEMBLOCK_L2TLB_RESET_ACK_RESPONSE) begin
            return;
        end
        acknowledge_l2tlb_runtime_reset(MEMBLOCK_L2TLB_RESET_ACK_RESPONSE,
                                        reset_epoch,
                                        "L2tlb_agent_agent_driver");
    endfunction:acknowledge_l2tlb_response_reset_if_quiescent

    function void reset_l2tlb_monitor_runtime_state(
        input longint unsigned reset_epoch);
        if (!l2tlb_runtime_reset_active ||
            reset_epoch != l2tlb_current_reset_epoch) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RESET", "monitor reset direct-writer epoch mismatch")
        end
        if (l2tlb_monitor_reset_done_epoch == reset_epoch) begin
            return;
        end
        l2tlb_monitor_final_sample_settled_valid = 1'b0;
        l2tlb_monitor_final_sample_settled_epoch = 0;
        l2tlb_monitor_final_sample_settled_transport_sample_seq = 0;
        l2tlb_monitor_reset_pending_epoch = reset_epoch;
        l2tlb_monitor_reset_processed_epoch = 0;
        l2tlb_monitor_reset_processed_transport_sample_seq = 0;
        l2tlb_monitor_reset_ack_floor_transport_sample_seq =
            l2tlb_monitor_reset_ack_transport_sample_seq;
        l2tlb_monitor_reset_done_epoch = reset_epoch;
    endfunction:reset_l2tlb_monitor_runtime_state

    // Abstract responsibility: publish the L2TLB monitor's reset tuple only
    // after the matching frozen reset-active transport sample has been fully
    // processed.  A normal sample or a repeated callback cannot substitute
    // for that tuple.
    function void acknowledge_l2tlb_monitor_reset(
        input longint unsigned reset_epoch,
        input longint unsigned reset_sample_transport_seq,
        input longint unsigned processed_epoch,
        input longint unsigned processed_transport_sample_seq,
        input string writer_name);
        if (!l2tlb_runtime_reset_active ||
            reset_epoch != l2tlb_current_reset_epoch ||
            l2tlb_monitor_reset_pending_epoch != reset_epoch ||
            processed_epoch != reset_epoch ||
            processed_transport_sample_seq != reset_sample_transport_seq ||
            reset_sample_transport_seq <=
                l2tlb_monitor_reset_ack_floor_transport_sample_seq ||
            (l2tlb_runtime_reset_ack_mask &
             MEMBLOCK_L2TLB_RESET_ACK_MONITOR) != '0) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RESET",
                       $sformatf("invalid L2TLB monitor reset tuple epoch=%0d/current=%0d pending=%0d sample=%0d processed=%0d/%0d floor=%0d ack=0x%0h",
                                 reset_epoch, l2tlb_current_reset_epoch,
                                 l2tlb_monitor_reset_pending_epoch,
                                 reset_sample_transport_seq,
                                 processed_epoch,
                                 processed_transport_sample_seq,
                                 l2tlb_monitor_reset_ack_floor_transport_sample_seq,
                                 l2tlb_runtime_reset_ack_mask))
        end
        l2tlb_monitor_reset_ack_epoch = reset_epoch;
        l2tlb_monitor_reset_ack_transport_sample_seq =
            reset_sample_transport_seq;
        acknowledge_l2tlb_runtime_reset(
            MEMBLOCK_L2TLB_RESET_ACK_MONITOR,
            reset_epoch,
            writer_name);
        l2tlb_monitor_reset_pending_epoch = 0;
    endfunction:acknowledge_l2tlb_monitor_reset

    function void end_l2tlb_runtime_reset();
        if (!l2tlb_runtime_reset_active) begin
            return;
        end
        if (!l2tlb_runtime_reset_acks_complete()) begin
            return;
        end
        l2tlb_runtime_reset_active = 1'b0;
        get_l2tlb_release_state_changed_ev().trigger();
    endfunction:end_l2tlb_runtime_reset

    function void mark_l2tlb_owner_admission_settled(input string owner_name,
                                                     input longint unsigned sample_seq);
        if (!l2tlb_lifecycle_owner_claimed ||
            l2tlb_lifecycle_owner_name != owner_name ||
            sample_seq == 0) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                       $sformatf("invalid admission settled owner=%s current=%s claimed=%0d sample=%0d",
                                 owner_name, l2tlb_lifecycle_owner_name,
                                 l2tlb_lifecycle_owner_claimed, sample_seq))
        end
        if (l2tlb_owner_admission_settled_sample_seq > sample_seq) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                       $sformatf("admission settled sample regressed old=%0d new=%0d",
                                 l2tlb_owner_admission_settled_sample_seq,
                                 sample_seq))
        end
        l2tlb_owner_admission_settled_sample_seq = sample_seq;
    endfunction:mark_l2tlb_owner_admission_settled

    function longint unsigned close_l2tlb_admission_for_release(
        input string owner_name,
        input longint unsigned current_sample);
        if (!l2tlb_lifecycle_owner_claimed ||
            l2tlb_lifecycle_owner_name != owner_name ||
            current_sample == 0) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                       $sformatf("invalid close request owner=%s current=%s claimed=%0d sample=%0d",
                                 owner_name, l2tlb_lifecycle_owner_name,
                                 l2tlb_lifecycle_owner_claimed, current_sample))
        end
        if (l2tlb_owner_admission_settled_sample_seq != current_sample) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                       $sformatf("close request before admission settled owner=%s settled=%0d current=%0d",
                                 owner_name,
                                 l2tlb_owner_admission_settled_sample_seq,
                                 current_sample))
        end
        if (get_l2tlb_current_reset_epoch() != 0 &&
            !l2tlb_post_reset_baseline_done(get_l2tlb_current_reset_epoch())) begin
            `uvm_fatal("MEMBLOCK_L2TLB_BASELINE",
                       $sformatf("close request before post-reset transport baseline proof owner=%s epoch=%0d sample=%0d",
                                 owner_name,
                                 get_l2tlb_current_reset_epoch(),
                                 current_sample))
        end
        if (l2tlb_release_admission_close_requested) begin
            if (l2tlb_release_admission_request_owner_name != owner_name) begin
                `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                           $sformatf("close request owner mismatch old=%s new=%s",
                                     l2tlb_release_admission_request_owner_name,
                                     owner_name))
            end
            return l2tlb_release_admission_close_generation;
        end
        l2tlb_release_admission_close_generation++;
        if (l2tlb_release_admission_close_generation == 0) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE", "close generation wrapped to zero")
        end
        l2tlb_release_admission_close_requested = 1'b1;
        l2tlb_release_admission_request_owner_name = owner_name;
        l2tlb_release_admission_close_request_sample_seq = current_sample;
        l2tlb_release_admission_close_reset_epoch =
            get_l2tlb_current_reset_epoch();
        l2tlb_release_admission_closed = 1'b0;
        l2tlb_release_admission_owner_name = "";
        l2tlb_release_admission_closed_generation = 0;
        l2tlb_release_admission_cutoff_sample_seq = 0;
        l2tlb_response_drain_done = 1'b0;
        l2tlb_response_drain_owner_name = "";
        l2tlb_response_drain_generation = 0;
        l2tlb_release_final_inactive_item_done = 1'b0;
        l2tlb_release_final_inactive_generation = 0;
        l2tlb_release_final_inactive_transport_sample_seq = 0;
        l2tlb_transport_sample_recycle_done_seq = 0;
        l2tlb_monitor_final_sample_settled_valid = 1'b0;
        l2tlb_release_closing = 1'b0;
        l2tlb_release_closing_owner_name = "";
        l2tlb_release_closing_generation = 0;
        l2tlb_release_granted = 1'b0;
        l2tlb_release_grant_owner_name = "";
        l2tlb_release_grant_reset_epoch = 0;
        l2tlb_release_grant_generation = 0;
        l2tlb_response_owner_reset_done_epoch = 0;
        return l2tlb_release_admission_close_generation;
    endfunction:close_l2tlb_admission_for_release

    function void confirm_l2tlb_admission_closed_at_drv_cb(
        input string owner_name,
        input longint unsigned dut_sample_seq_i,
        input longint unsigned sampled_reset_epoch,
        input string sampled_item_owner_name,
        input memblock_l2tlb_release_item_kind_e sampled_item_kind,
        input longint unsigned sampled_item_generation,
        input longint unsigned sampled_item_reset_epoch,
        input bit sampled_req_fire,
        input logic sampled_req_ready);
        if (sampled_item_kind != MEMBLOCK_L2TLB_ITEM_RELEASE_STOP) begin
            return;
        end
        if (sampled_item_owner_name != owner_name ||
            !l2tlb_release_admission_close_requested ||
            l2tlb_release_admission_request_owner_name != owner_name ||
            sampled_item_generation != l2tlb_release_admission_close_generation ||
            sampled_item_reset_epoch != sampled_reset_epoch ||
            dut_sample_seq_i <= l2tlb_release_admission_close_request_sample_seq ||
            sampled_req_ready !== 1'b0 ||
            sampled_req_fire) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                       $sformatf("invalid RELEASE_STOP sample owner=%s item_owner=%s gen=%0d close_gen=%0d item_epoch=%0d sample_epoch=%0d sample=%0d close_sample=%0d ready=%b fire=%0d",
                                 owner_name, sampled_item_owner_name,
                                 sampled_item_generation,
                                 l2tlb_release_admission_close_generation,
                                 sampled_item_reset_epoch, sampled_reset_epoch,
                                 dut_sample_seq_i,
                                 l2tlb_release_admission_close_request_sample_seq,
                                 sampled_req_ready, sampled_req_fire))
        end
        if (l2tlb_release_admission_closed) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE", "duplicate admission closed confirm")
        end
        l2tlb_release_admission_closed = 1'b1;
        l2tlb_release_admission_owner_name = owner_name;
        l2tlb_release_admission_closed_generation =
            l2tlb_release_admission_close_generation;
        l2tlb_release_admission_cutoff_sample_seq = dut_sample_seq_i;
    endfunction:confirm_l2tlb_admission_closed_at_drv_cb

    function void mark_l2tlb_response_drain_done(input string owner_name);
        if (!l2tlb_release_admission_closed ||
            l2tlb_release_admission_owner_name != owner_name) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                       $sformatf("response drain before admission closed owner=%s closed_owner=%s closed=%0d",
                                 owner_name,
                                 l2tlb_release_admission_owner_name,
                                 l2tlb_release_admission_closed))
        end
        l2tlb_response_drain_done = 1'b1;
        l2tlb_response_drain_owner_name = owner_name;
        l2tlb_response_drain_generation =
            l2tlb_release_admission_close_generation;
    endfunction:mark_l2tlb_response_drain_done

    function void mark_l2tlb_final_inactive_at_drv_cb(
        input string owner_name,
        input longint unsigned dut_sample_seq_i,
        input longint unsigned transport_sample_seq,
        input longint unsigned sampled_reset_epoch,
        input string sampled_item_owner_name,
        input memblock_l2tlb_release_item_kind_e sampled_item_kind,
        input longint unsigned sampled_item_generation,
        input longint unsigned sampled_item_reset_epoch,
        input logic sampled_req_ready,
        input bit sampled_req_fire,
        input logic sampled_resp_valid);
        if (sampled_item_kind != MEMBLOCK_L2TLB_ITEM_RELEASE_FINAL_INACTIVE) begin
            return;
        end
        if (sampled_item_owner_name != owner_name ||
            !l2tlb_release_admission_closed ||
            sampled_item_generation != l2tlb_release_admission_close_generation ||
            sampled_item_reset_epoch != sampled_reset_epoch ||
            dut_sample_seq_i <= l2tlb_release_admission_cutoff_sample_seq ||
            sampled_req_ready !== 1'b0 ||
            sampled_req_fire ||
            sampled_resp_valid !== 1'b0) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                       $sformatf("invalid RELEASE_FINAL_INACTIVE sample owner=%s item_owner=%s gen=%0d close_gen=%0d sample=%0d cutoff=%0d ready=%b fire=%0d resp_valid=%b",
                                 owner_name, sampled_item_owner_name,
                                 sampled_item_generation,
                                 l2tlb_release_admission_close_generation,
                                 dut_sample_seq_i,
                                 l2tlb_release_admission_cutoff_sample_seq,
                                 sampled_req_ready, sampled_req_fire,
                                 sampled_resp_valid))
        end
        if (!l2tlb_response_drain_done ||
            l2tlb_response_drain_owner_name != owner_name ||
            l2tlb_response_drain_generation != l2tlb_release_admission_close_generation) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE", "final inactive before response drain")
        end
        if (l2tlb_release_final_inactive_item_done) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE", "duplicate final inactive confirm")
        end
        l2tlb_release_final_inactive_item_done = 1'b1;
        l2tlb_release_final_inactive_generation =
            l2tlb_release_admission_close_generation;
        l2tlb_release_final_inactive_transport_sample_seq =
            transport_sample_seq;
        // This only records that the final item crossed the physical driver
        // boundary.  The final transport sample is not PUBLISHED until the
        // driver reserves the sequencer slot later in the same callback.
        // Setting mailbox-empty false here would suppress that publication
        // and strand the owner before it can acknowledge the final sample.
    endfunction:mark_l2tlb_final_inactive_at_drv_cb

    function void mark_l2tlb_monitor_final_sample_settled(
        input longint unsigned reset_epoch,
        input longint unsigned transport_sample_seq);
        l2tlb_monitor_final_sample_settled_valid = 1'b1;
        l2tlb_monitor_final_sample_settled_epoch = reset_epoch;
        l2tlb_monitor_final_sample_settled_transport_sample_seq =
            transport_sample_seq;
    endfunction:mark_l2tlb_monitor_final_sample_settled

    function bit monitor_final_sample_settled(input longint unsigned reset_epoch,
                                              input longint unsigned transport_sample_seq);
        return l2tlb_monitor_final_sample_settled_valid &&
               l2tlb_monitor_final_sample_settled_epoch == reset_epoch &&
               l2tlb_monitor_final_sample_settled_transport_sample_seq ==
                   transport_sample_seq;
    endfunction:monitor_final_sample_settled

    function void begin_l2tlb_release_closing(input string owner_name);
        if (!l2tlb_lifecycle_owner_claimed ||
            l2tlb_lifecycle_owner_name != owner_name ||
            !l2tlb_response_drain_done ||
            !l2tlb_release_final_inactive_item_done ||
            l2tlb_reset_active() ||
            l2tlb_release_admission_close_reset_epoch !=
                get_l2tlb_current_reset_epoch() ||
            !monitor_final_sample_settled(get_l2tlb_current_reset_epoch(),
                l2tlb_release_final_inactive_transport_sample_seq)) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                       $sformatf("release closing not ready owner=%s current=%s drain=%0d final=%0d monitor=%0d",
                                 owner_name, l2tlb_lifecycle_owner_name,
                                 l2tlb_response_drain_done,
                                 l2tlb_release_final_inactive_item_done,
                                 monitor_final_sample_settled(
                                     get_l2tlb_current_reset_epoch(),
                                     l2tlb_release_final_inactive_transport_sample_seq)))
        end
        if (l2tlb_release_closing) begin
            if (l2tlb_release_closing_owner_name != owner_name ||
                l2tlb_release_closing_generation !=
                    l2tlb_release_admission_close_generation) begin
                `uvm_fatal("MEMBLOCK_L2TLB_RELEASE", "release closing owner/generation mismatch")
            end
            return;
        end
        l2tlb_release_closing = 1'b1;
        l2tlb_release_closing_owner_name = owner_name;
        l2tlb_release_closing_generation =
            l2tlb_release_admission_close_generation;
    endfunction:begin_l2tlb_release_closing

    function void mark_l2tlb_transport_sample_recycled(
        input longint unsigned transport_sample_seq);
        if (transport_sample_seq == 0 ||
            transport_sample_seq != l2tlb_release_final_inactive_transport_sample_seq) begin
            return;
        end
        l2tlb_transport_sample_recycle_done_seq = transport_sample_seq;
        l2tlb_transport_sample_mailbox_empty_state = 1'b1;
    endfunction:mark_l2tlb_transport_sample_recycled

    // Abstract function: record that the adapter has drained all work that
    // can delete or update a live L2TLB entry for one release generation.
    // The raw-fence input queue is checked here as an additional cheap guard;
    // adapter-local pending state must be represented by the proof itself.
    function void mark_l2tlb_adapter_drain_done(
        input longint unsigned reset_epoch,
        input longint unsigned generation);
        if (!dispatch_l2tlb_lookup_active) begin
            return;
        end
        if (l2tlb_reset_active() ||
            reset_epoch != get_l2tlb_current_reset_epoch() ||
            !l2tlb_release_admission_close_requested ||
            generation == 0 ||
            generation != l2tlb_release_admission_close_generation) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                       $sformatf("invalid adapter drain proof epoch=%0d current=%0d generation=%0d current_generation=%0d close=%0d reset=%0d",
                                 reset_epoch, get_l2tlb_current_reset_epoch(),
                                 generation, l2tlb_release_admission_close_generation,
                                 l2tlb_release_admission_close_requested,
                                 l2tlb_reset_active()))
        end
        if (raw_sfence_q.size() != 0) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                       $sformatf("adapter drain proof published with raw fence queue size=%0d",
                                 raw_sfence_q.size()))
        end
        l2tlb_adapter_drain_done = 1'b1;
        l2tlb_adapter_drain_epoch = reset_epoch;
        l2tlb_adapter_drain_generation = generation;
    endfunction:mark_l2tlb_adapter_drain_done

    // Abstract function: expose the adapter's release-safe drain proof.  In
    // no-dispatch topology there is no adapter-owned live-entry work.
    function bit dispatch_l2tlb_live_entry_drain_done();
        if (!dispatch_l2tlb_lookup_active) begin
            return 1'b1;
        end
        return l2tlb_adapter_drain_done &&
               l2tlb_adapter_drain_epoch == get_l2tlb_current_reset_epoch() &&
               l2tlb_adapter_drain_generation ==
                   l2tlb_release_admission_close_generation &&
               raw_sfence_q.size() == 0;
    endfunction:dispatch_l2tlb_live_entry_drain_done

    // Abstract function: publish the fence monitor's per-sample producer
    // watermark.  It does not consume the raw-fence queue.
    function void mark_l2tlb_raw_fence_producer_settled(
        input longint unsigned sample_seq);
        if (sample_seq == 0 ||
            (dut_sample_time_valid && sample_seq > dut_sample_seq) ||
            sample_seq < l2tlb_raw_fence_producer_settled_sample_seq) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                       $sformatf("invalid raw fence producer watermark sample=%0d current=%0d previous=%0d",
                                 sample_seq, dut_sample_seq,
                                 l2tlb_raw_fence_producer_settled_sample_seq))
        end
        l2tlb_raw_fence_producer_settled_sample_seq = sample_seq;
    endfunction:mark_l2tlb_raw_fence_producer_settled

    // Abstract function: close future raw-fence admission after the fence
    // monitor has completed a full sample strictly after the owner close
    // request.  It is a producer proof, not an adapter drain operation.
    function void close_dispatch_raw_fence_intake_for_release(
        input longint unsigned sample_seq);
        if (!dispatch_l2tlb_lookup_active ||
            !l2tlb_release_admission_close_requested ||
            l2tlb_reset_active()) begin
            return;
        end
        if (l2tlb_release_admission_close_reset_epoch !=
                get_l2tlb_current_reset_epoch()) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                       $sformatf("raw fence close epoch mismatch close_epoch=%0d current=%0d",
                                 l2tlb_release_admission_close_reset_epoch,
                                 get_l2tlb_current_reset_epoch()))
        end
        if (sample_seq <= l2tlb_release_admission_close_request_sample_seq) begin
            return;
        end
        if (l2tlb_raw_fence_producer_settled_sample_seq != sample_seq) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                       $sformatf("raw fence close before producer settled sample=%0d settled=%0d",
                                 sample_seq,
                                 l2tlb_raw_fence_producer_settled_sample_seq))
        end
        if (l2tlb_raw_fence_intake_closed) begin
            if (l2tlb_raw_fence_intake_closed_reset_epoch !=
                    get_l2tlb_current_reset_epoch() ||
                l2tlb_raw_fence_intake_closed_generation !=
                    l2tlb_release_admission_close_generation) begin
                `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                           "raw fence intake close provenance changed")
            end
            return;
        end
        l2tlb_raw_fence_intake_closed = 1'b1;
        l2tlb_raw_fence_intake_closed_reset_epoch =
            get_l2tlb_current_reset_epoch();
        l2tlb_raw_fence_intake_closed_generation =
            l2tlb_release_admission_close_generation;
        l2tlb_raw_fence_intake_cutoff_sample_seq = sample_seq;
    endfunction:close_dispatch_raw_fence_intake_for_release

    // Abstract function: check the exact producer-close proof required by the
    // release gate.  No-dispatch has no raw-fence producer to close.
    function bit dispatch_raw_fence_intake_closed_for_release(
        input longint unsigned generation,
        input longint unsigned reset_epoch);
        if (!dispatch_l2tlb_lookup_active) begin
            return 1'b1;
        end
        return l2tlb_raw_fence_intake_closed &&
               l2tlb_raw_fence_intake_closed_reset_epoch == reset_epoch &&
               l2tlb_raw_fence_intake_closed_generation == generation &&
               reset_epoch == get_l2tlb_current_reset_epoch() &&
               l2tlb_raw_fence_intake_cutoff_sample_seq >
                   l2tlb_release_admission_close_request_sample_seq;
    endfunction:dispatch_raw_fence_intake_closed_for_release

    // Abstract function: report whether the single transport sample slot is
    // reusable.  The driver is the only component that may publish/recycle
    // this proof; the package never infers it from a UVM item handle.
    function bit l2tlb_transport_sample_mailbox_empty();
        return l2tlb_transport_sample_mailbox_empty_state;
    endfunction:l2tlb_transport_sample_mailbox_empty

    function void mark_l2tlb_transport_sample_mailbox_nonempty();
        l2tlb_transport_sample_mailbox_empty_state = 1'b0;
    endfunction:mark_l2tlb_transport_sample_mailbox_nonempty

    function void mark_l2tlb_transport_sample_mailbox_empty();
        l2tlb_transport_sample_mailbox_empty_state = 1'b1;
    endfunction:mark_l2tlb_transport_sample_mailbox_empty

    function bit l2tlb_transport_monitor_drain_done(
        input longint unsigned reset_epoch,
        input longint unsigned transport_sample_seq);
        return monitor_final_sample_settled(reset_epoch, transport_sample_seq);
    endfunction:l2tlb_transport_monitor_drain_done

    function bit release_grantable(input string owner_name,
                                   input longint unsigned reset_epoch);
        longint unsigned generation;

        if (!l2tlb_lifecycle_owner_claimed ||
            l2tlb_lifecycle_owner_name != owner_name ||
            reset_epoch != get_l2tlb_current_reset_epoch() ||
            l2tlb_reset_active()) begin
            return 1'b0;
        end
        if (reset_epoch != 0 &&
            !l2tlb_post_reset_baseline_done(reset_epoch)) begin
            return 1'b0;
        end
        generation = l2tlb_release_admission_close_generation;
        if (!l2tlb_release_admission_close_requested ||
            generation == 0 ||
            l2tlb_release_admission_request_owner_name != owner_name ||
            l2tlb_release_admission_close_reset_epoch != reset_epoch ||
            !l2tlb_release_admission_closed ||
            l2tlb_release_admission_owner_name != owner_name ||
            l2tlb_release_admission_closed_generation != generation ||
            !l2tlb_response_drain_done ||
            l2tlb_response_drain_owner_name != owner_name ||
            l2tlb_response_drain_generation != generation ||
            !l2tlb_release_final_inactive_item_done ||
            l2tlb_release_final_inactive_generation != generation ||
            l2tlb_release_final_inactive_transport_sample_seq == 0 ||
            !l2tlb_transport_monitor_drain_done(
                reset_epoch, l2tlb_release_final_inactive_transport_sample_seq) ||
            !l2tlb_release_closing ||
            l2tlb_release_closing_owner_name != owner_name ||
            l2tlb_release_closing_generation != generation ||
            l2tlb_transport_sample_recycle_done_seq !=
                l2tlb_release_final_inactive_transport_sample_seq ||
            !l2tlb_transport_sample_mailbox_empty() ||
            !dispatch_l2tlb_live_entry_drain_done() ||
            !dispatch_raw_fence_intake_closed_for_release(
                generation, reset_epoch)) begin
            return 1'b0;
        end
        return 1'b1;
    endfunction:release_grantable

    function bit grant_l2tlb_final_release();
        string owner_name;
        longint unsigned current_epoch;
        longint unsigned current_generation;

        if (!l2tlb_lifecycle_owner_claimed) begin
            // No owner means there is no grant to issue.  This is not a
            // successful release and must not wake a future owner.
            return 1'b0;
        end
        if (l2tlb_reset_active()) begin
            return 1'b0;
        end
        owner_name = l2tlb_lifecycle_owner_name;
        current_epoch = get_l2tlb_current_reset_epoch();
        current_generation = l2tlb_release_admission_close_generation;
        if (l2tlb_release_granted) begin
            if (l2tlb_release_grant_owner_name != owner_name ||
                l2tlb_release_grant_reset_epoch != current_epoch ||
                l2tlb_release_grant_generation != current_generation ||
                !release_grantable(owner_name, current_epoch)) begin
                `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                           $sformatf("existing release grant is no longer valid owner=%s grant_owner=%s epoch=%0d/%0d generation=%0d/%0d",
                                     owner_name, l2tlb_release_grant_owner_name,
                                     current_epoch, l2tlb_release_grant_reset_epoch,
                                     current_generation, l2tlb_release_grant_generation))
            end
            return 1'b1;
        end
        if (!release_grantable(owner_name, current_epoch)) begin
            return 1'b0;
        end
        l2tlb_release_granted = 1'b1;
        l2tlb_release_grant_owner_name = owner_name;
        l2tlb_release_grant_reset_epoch = current_epoch;
        l2tlb_release_grant_generation = current_generation;
        get_l2tlb_release_state_changed_ev().trigger();
        // A return value of 1 means that a valid grant is now available.  A
        // repeated call is idempotent and also returns 1 without re-triggering.
        return 1'b1;
    endfunction:grant_l2tlb_final_release

    task automatic wait_for_l2tlb_release_grant_or_reset(
        input string owner_name,
        input longint unsigned expected_reset_epoch,
        input longint unsigned expected_generation,
        output bit granted);
        granted = 1'b0;
        forever begin
            // Reset has priority over a stale grant.  The reset publisher
            // triggers the same event, so this branch also guarantees that a
            // waiter cannot remain blocked after grant invalidation.
            if (l2tlb_reset_active() ||
                get_l2tlb_current_reset_epoch() != expected_reset_epoch) begin
                granted = 1'b0;
                return;
            end
            if (l2tlb_release_granted) begin
                if (l2tlb_release_grant_owner_name != owner_name ||
                    l2tlb_release_grant_reset_epoch != expected_reset_epoch ||
                    l2tlb_release_grant_generation != expected_generation ||
                    !release_grantable(owner_name, expected_reset_epoch)) begin
                    `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                               $sformatf("invalid release grant owner=%s grant_owner=%s epoch=%0d/%0d gen=%0d/%0d",
                                         owner_name,
                                         l2tlb_release_grant_owner_name,
                                         expected_reset_epoch,
                                         l2tlb_release_grant_reset_epoch,
                                         expected_generation,
                                         l2tlb_release_grant_generation))
                end
                granted = 1'b1;
                return;
            end
            get_l2tlb_release_state_changed_ev().wait_ptrigger();
        end
    endtask:wait_for_l2tlb_release_grant_or_reset

    function longint unsigned note_l2tlb_flush_event(input time sample_time,
                                                      input bit [1:0] reason_mask = MEMBLOCK_L2TLB_REASON_FENCE);
        memblock_l2tlb_event_record_t event_record;

        if (!dut_sample_time_valid || dut_sample_seq == 0) begin
            `uvm_fatal("MEMBLOCK_L2TLB_SAMPLE",
                       "cannot record an L2TLB event before the CSR sample anchor")
        end
        if (reason_mask == 2'b00 ||
            (reason_mask & ~(MEMBLOCK_L2TLB_REASON_CSR |
                             MEMBLOCK_L2TLB_REASON_FENCE)) != 2'b00) begin
            `uvm_fatal("MEMBLOCK_L2TLB_EVENT",
                       $sformatf("invalid L2TLB event reason mask=0x%0h", reason_mask))
        end
        // The first producer may call this function before publishing its
        // producer-done bit.  Establish the reason ledger for this sample at
        // the event boundary, while preserving the missed-barrier check.
        if (sample_producer_active_seq != dut_sample_seq) begin
            if (sample_producer_active_seq != 0 &&
                sample_producer_done_mask != 2'b11) begin
                `uvm_fatal("MEMBLOCK_L2TLB_EVENT",
                           $sformatf("flush reason advanced before producer barrier closed sample=%0d mask=0x%0h new_sample=%0d reason=0x%0h",
                                     sample_producer_active_seq,
                                     sample_producer_done_mask,
                                     dut_sample_seq,
                                     reason_mask))
            end
            sample_producer_active_seq = dut_sample_seq;
            sample_producer_done_mask = 2'b00;
            sample_producer_reason_mask = 2'b00;
        end
        if ((sample_producer_reason_mask & reason_mask) != 2'b00) begin
            `uvm_fatal("MEMBLOCK_L2TLB_EVENT",
                       $sformatf("duplicate L2TLB flush reason sample=%0d old_reason=0x%0h new_reason=0x%0h",
                                 dut_sample_seq,
                                 sample_producer_reason_mask,
                                 reason_mask))
        end
        if (l2tlb_flush_event_history.size() != 0 &&
            l2tlb_flush_event_history[$].anchor_sample_seq == dut_sample_seq) begin
            l2tlb_flush_event_history[$].reason_mask |= reason_mask;
            l2tlb_flush_event_history[$].sample_time = sample_time;
            l2tlb_flush_event_seq = l2tlb_flush_event_history[$].event_seq;
            l2tlb_flush_event_reason_mask = l2tlb_flush_event_history[$].reason_mask;
            l2tlb_flush_sample_time = sample_time;
            l2tlb_flush_event_valid = 1'b1;
            sample_producer_reason_mask |= reason_mask;
            return l2tlb_flush_event_seq;
        end
        if (!dispatch_l2tlb_lookup_active) begin
            // No-dispatch topology has no response-side event history. Keep
            // the producer reason/watermark only for the sample barrier.
            sample_producer_reason_mask |= reason_mask;
            return MEMBLOCK_L2TLB_EVENT_SEQ_NONE;
        end
        if (l2tlb_flush_event_history.size() >= `MEMBLOCK_L2TLB_EVENT_HISTORY_MAX_DEPTH) begin
            `uvm_fatal("MEMBLOCK_L2TLB_EVENT",
                       $sformatf("L2TLB event history exceeds bound=%0d",
                                 `MEMBLOCK_L2TLB_EVENT_HISTORY_MAX_DEPTH))
        end
        last_allocated_l2tlb_event_seq++;
        if (last_allocated_l2tlb_event_seq == MEMBLOCK_L2TLB_EVENT_SEQ_NONE) begin
            `uvm_fatal("MEMBLOCK_L2TLB_EVENT", "L2TLB event sequence wrapped to NONE")
        end
        event_record.event_seq = last_allocated_l2tlb_event_seq;
        event_record.reason_mask = reason_mask;
        event_record.anchor_sample_seq = dut_sample_seq;
        event_record.sample_time = sample_time;
        l2tlb_flush_event_history.push_back(event_record);
        l2tlb_flush_event_seq = event_record.event_seq;
        l2tlb_flush_event_reason_mask = reason_mask;
        l2tlb_flush_sample_time = sample_time;
        l2tlb_flush_event_valid = 1'b1;
        sample_producer_reason_mask |= reason_mask;
        return event_record.event_seq;
    endfunction:note_l2tlb_flush_event

    function void get_latest_l2tlb_flush_event(output longint unsigned event_seq,
                                                output time sample_time,
                                                output bit valid);
        event_seq = l2tlb_flush_event_seq;
        sample_time = l2tlb_flush_sample_time;
        valid = l2tlb_flush_event_valid;
    endfunction:get_latest_l2tlb_flush_event

    function bit get_l2tlb_event_after(input longint unsigned cursor,
                                       output memblock_l2tlb_event_record_t event_record);
        longint unsigned expected_seq;

        event_record = '{default:'0};
        expected_seq = cursor + 1;
        foreach (l2tlb_flush_event_history[idx]) begin
            if (l2tlb_flush_event_history[idx].event_seq > cursor) begin
                if (l2tlb_flush_event_history[idx].event_seq != expected_seq) begin
                    `uvm_fatal("MEMBLOCK_L2TLB_EVENT",
                               $sformatf("L2TLB event sequence gap cursor=%0d expected=%0d actual=%0d history_index=%0d",
                                         cursor,
                                         expected_seq,
                                         l2tlb_flush_event_history[idx].event_seq,
                                         idx))
                end
                event_record = l2tlb_flush_event_history[idx];
                return 1'b1;
            end
        end
        return 1'b0;
    endfunction:get_l2tlb_event_after

    function void retire_l2tlb_event_history_prefix(input longint unsigned cursor);
        if (cursor > last_allocated_l2tlb_event_seq) begin
            `uvm_fatal("MEMBLOCK_L2TLB_EVENT",
                       $sformatf("event cursor=%0d exceeds last allocated=%0d",
                                 cursor, last_allocated_l2tlb_event_seq))
        end
        while (l2tlb_flush_event_history.size() != 0 &&
               l2tlb_flush_event_history[0].event_seq <= cursor) begin
            l2tlb_flush_event_history.pop_front();
        end
        response_owner_event_cursor = cursor;
    endfunction:retire_l2tlb_event_history_prefix

    function void mark_l2tlb_sample_producer_done(input longint unsigned sample_seq,
                                                   input bit [1:0] producer_kind);
        bit [1:0] required_mask;
        required_mask = 2'b11;
        if (!dut_sample_time_valid || sample_seq != dut_sample_seq) begin
            `uvm_fatal("MEMBLOCK_L2TLB_SAMPLE",
                       $sformatf("producer sample mismatch producer=0x%0h sample=%0d current=%0d",
                                 producer_kind, sample_seq, dut_sample_seq))
        end
        if (producer_kind == 2'b00 ||
            (producer_kind & ~required_mask) != 2'b00 ||
            (producer_kind & (producer_kind - 1'b1)) != 2'b00) begin
            `uvm_fatal("MEMBLOCK_L2TLB_SAMPLE",
                       $sformatf("invalid producer kind=0x%0h", producer_kind))
        end
        if (producer_kind == 2'b10) begin
            // The fence monitor is the caller of this producer barrier. Its
            // completion is therefore also the raw-fence producer watermark;
            // no second sample writer is introduced here.
            mark_l2tlb_raw_fence_producer_settled(sample_seq);
            close_dispatch_raw_fence_intake_for_release(sample_seq);
        end
        if (sample_producer_active_seq != sample_seq) begin
            if (sample_producer_active_seq != 0 &&
                sample_producer_done_mask != required_mask) begin
                `uvm_fatal("MEMBLOCK_L2TLB_SAMPLE",
                           $sformatf("producer barrier missed sample=%0d mask=0x%0h",
                                     sample_producer_active_seq, sample_producer_done_mask))
            end
            sample_producer_active_seq = sample_seq;
            sample_producer_done_mask = 2'b00;
            sample_producer_reason_mask = 2'b00;
        end
        if ((sample_producer_done_mask & producer_kind) != 2'b00) begin
            `uvm_fatal("MEMBLOCK_L2TLB_SAMPLE",
                       $sformatf("producer kind repeated sample=%0d kind=0x%0h",
                                 sample_seq, producer_kind))
        end
        sample_producer_done_mask |= producer_kind;
        if (sample_producer_done_mask == required_mask) begin
            lifecycle_event_published_seq = sample_seq;
        end
    endfunction:mark_l2tlb_sample_producer_done

    function bit l2tlb_sample_ready(input longint unsigned sample_seq);
        return sample_seq != 0 &&
               csr_history_published_seq >= sample_seq &&
               lifecycle_event_published_seq >= sample_seq;
    endfunction:l2tlb_sample_ready

    function void publish_runtime_csr_snapshot(input dispatch_raw_csr_t item,
                                               input bit payload_changed);
        if (item.valid && payload_changed) begin
            runtime_csr_snapshot = item;
            runtime_csr_snapshot_valid = 1'b1;
            runtime_csr_snapshot_seq++;
        end
    endfunction:publish_runtime_csr_snapshot

    function bit get_latest_runtime_csr_snapshot(output dispatch_raw_csr_t item,
                                                  output int unsigned seq);
        seq = runtime_csr_snapshot_seq;
        if (!runtime_csr_snapshot_valid) begin
            item = make_empty_raw_csr();
            return 1'b0;
        end
        item = runtime_csr_snapshot;
        return 1'b1;
    endfunction:get_latest_runtime_csr_snapshot

    function void push_raw_int_wb(input dispatch_raw_int_wb_t item);
        if (dispatch_monitor_capture_en && item.valid) begin
            raw_int_wb_q.push_back(item);
        end
    endfunction:push_raw_int_wb

    function bit pop_raw_int_wb(output dispatch_raw_int_wb_t item);
        if (raw_int_wb_q.size() == 0) begin
            item = make_empty_raw_int_wb();
            return 1'b0;
        end
        item = raw_int_wb_q.pop_front();
        return 1'b1;
    endfunction:pop_raw_int_wb

    function void push_raw_iq_feedback(input dispatch_raw_iq_feedback_t item);
        if (dispatch_monitor_capture_en && item.valid) begin
            raw_iq_feedback_q.push_back(item);
        end
    endfunction:push_raw_iq_feedback

    function bit pop_raw_iq_feedback(output dispatch_raw_iq_feedback_t item);
        if (raw_iq_feedback_q.size() == 0) begin
            item = make_empty_raw_iq_feedback();
            return 1'b0;
        end
        item = raw_iq_feedback_q.pop_front();
        return 1'b1;
    endfunction:pop_raw_iq_feedback

    function void push_raw_ctrl(input dispatch_raw_ctrl_t item);
        if (dispatch_monitor_capture_en && item.valid) begin
            raw_ctrl_q.push_back(item);
        end
    endfunction:push_raw_ctrl

    function bit pop_raw_ctrl(output dispatch_raw_ctrl_t item);
        if (raw_ctrl_q.size() == 0) begin
            item = make_empty_raw_ctrl();
            return 1'b0;
        end
        item = raw_ctrl_q.pop_front();
        return 1'b1;
    endfunction:pop_raw_ctrl

    function void push_deferred_raw_ctrl(input dispatch_raw_ctrl_t item);
        // 该 raw 已经在 monitor capture 开启时进入 deferred 阶段；后续即使
        // capture 开关变化，也不能丢失等待重试的事实。
        if (item.valid) begin
            deferred_raw_ctrl_q.push_back(item);
        end
    endfunction:push_deferred_raw_ctrl

    function bit peek_deferred_raw_ctrl(output dispatch_raw_ctrl_t item);
        if (deferred_raw_ctrl_q.size() == 0) begin
            item = make_empty_raw_ctrl();
            return 1'b0;
        end
        item = deferred_raw_ctrl_q[0];
        return 1'b1;
    endfunction:peek_deferred_raw_ctrl

    function bit pop_deferred_raw_ctrl(output dispatch_raw_ctrl_t item);
        if (deferred_raw_ctrl_q.size() == 0) begin
            item = make_empty_raw_ctrl();
            return 1'b0;
        end
        item = deferred_raw_ctrl_q.pop_front();
        return 1'b1;
    endfunction:pop_deferred_raw_ctrl

    function longint unsigned advance_dut_global_sample(input longint unsigned sample_time);
        if (!dut_sample_time_valid) begin
            dut_sample_time = sample_time;
            dut_sample_time_valid = 1'b1;
            dut_sample_seq = 1;
        end else if (sample_time <= dut_sample_time) begin
            `uvm_fatal("MEMBLOCK_SAMPLE_SEQ",
                       $sformatf("CSR monitor advanced sample at non-increasing time: previous=%0d current=%0d",
                                 dut_sample_time, sample_time))
        end else begin
            if (sample_producer_active_seq != 0 &&
                sample_producer_active_seq == dut_sample_seq &&
                sample_producer_done_mask != 2'b11) begin
                `uvm_fatal("MEMBLOCK_L2TLB_SAMPLE",
                           $sformatf("CSR monitor advanced before producer barrier closed sample=%0d mask=0x%0h",
                                     dut_sample_seq, sample_producer_done_mask))
            end
            dut_sample_time = sample_time;
            dut_sample_seq++;
        end
        l2tlb_sample_anchor_valid = 1'b1;
        return dut_sample_seq;
    endfunction:advance_dut_global_sample

    // Non-CSR consumers must use this side-effect-free accessor. Keeping the
    // old name out of the package prevents accidental reintroduction of a
    // second sample writer.
    function longint unsigned peek_current_dut_global_sample();
        return dut_sample_seq;
    endfunction:peek_current_dut_global_sample

    task automatic wait_for_l2tlb_sample_anchor(input longint unsigned sample_time,
                                                 output longint unsigned sample_seq);
        sample_seq = 0;
        for (int unsigned delta = 0;
             delta <= `MEMBLOCK_L2TLB_SAMPLE_PROBE_MAX_DELTA;
             delta++) begin
            if (dut_sample_time_valid && dut_sample_time == sample_time) begin
                sample_seq = dut_sample_seq;
                return;
            end
            // Clocking-block producers may publish the anchor from the NBA
            // region of the same edge.  Visit that bounded region before the
            // next delta check; never wait for another clock here.
            uvm_wait_for_nba_region();
            #0;
        end
        `uvm_fatal("MEMBLOCK_SAMPLE_SEQ",
                   $sformatf("no CSR sample anchor at time=%0t after bounded probe=%0d valid=%0d anchor=%0d stored_time=%0t stored_seq=%0d reset_active=%0d reset_backend_done=%0d now=%0t",
                             sample_time, `MEMBLOCK_L2TLB_SAMPLE_PROBE_MAX_DELTA,
                             dut_sample_time_valid, l2tlb_sample_anchor_valid,
                             dut_sample_time, dut_sample_seq,
                             l2tlb_runtime_reset_active, reset_backend_done,
                             $realtime))
        sample_seq = 0;
    endtask:wait_for_l2tlb_sample_anchor

    function void publish_l2tlb_csr_history(input dispatch_raw_csr_t item,
                                             input longint unsigned sample_seq);
        int unsigned slot;

        if (!item.valid || sample_seq == 0 || sample_seq != dut_sample_seq) begin
            `uvm_fatal("MEMBLOCK_L2TLB_CSR",
                       $sformatf("invalid CSR history publish sample=%0d current=%0d valid=%0d",
                                 sample_seq, dut_sample_seq, item.valid))
        end
        slot = sample_seq % 3;
        l2tlb_csr_history[slot].sample_seq = sample_seq;
        l2tlb_csr_history[slot].payload = item;
        l2tlb_csr_history[slot].valid = 1'b1;
        csr_history_published_seq = sample_seq;
    endfunction:publish_l2tlb_csr_history

    function bit get_l2tlb_request_csr_history(input longint unsigned sample_seq,
                                               output dispatch_raw_csr_t item);
        longint signed target_sample;
        int unsigned slot;

        item = make_empty_raw_csr();
        if (sample_seq <= `MEMBLOCK_DUT_L2TLB_CSR_PIPE_STAGES) begin
            return 1'b0;
        end
        target_sample = longint'(sample_seq) -
                        longint'(`MEMBLOCK_DUT_L2TLB_CSR_PIPE_STAGES);
        slot = target_sample % 3;
        if (!l2tlb_csr_history[slot].valid ||
            l2tlb_csr_history[slot].sample_seq != target_sample) begin
            return 1'b0;
        end
        item = l2tlb_csr_history[slot].payload;
        return item.valid;
    endfunction:get_l2tlb_request_csr_history

    function void push_raw_cancel_snapshot(input dispatch_raw_cancel_snapshot_t item);
        if (dispatch_monitor_capture_en) begin
            if (raw_cancel_snapshot_q.size() >= `MEMBLOCK_CANCEL_SNAPSHOT_QUEUE_MAX_DEPTH) begin
                `uvm_fatal("MEMBLOCK_SYNC", $sformatf("raw cancel snapshot queue exceeds compile bound=%0d",
                                                        `MEMBLOCK_CANCEL_SNAPSHOT_QUEUE_MAX_DEPTH))
            end
            raw_cancel_snapshot_q.push_back(item);
        end
    endfunction:push_raw_cancel_snapshot

    function bit pop_raw_cancel_snapshot(output dispatch_raw_cancel_snapshot_t item);
        if (raw_cancel_snapshot_q.size() == 0) begin
            item = make_empty_raw_cancel_snapshot();
            return 1'b0;
        end
        item = raw_cancel_snapshot_q.pop_front();
        return 1'b1;
    endfunction:pop_raw_cancel_snapshot

    function void push_raw_redirect_anchor(input dispatch_raw_redirect_anchor_t item);
        if (dispatch_monitor_capture_en && item.valid) begin
            if (raw_redirect_anchor_q.size() >= `MEMBLOCK_CANCEL_RECORD_MAX_DEPTH) begin
                `uvm_fatal("MEMBLOCK_SYNC", $sformatf("raw redirect anchor queue exceeds compile bound=%0d",
                                                        `MEMBLOCK_CANCEL_RECORD_MAX_DEPTH))
            end
            raw_redirect_anchor_q.push_back(item);
        end
    endfunction:push_raw_redirect_anchor

    function bit pop_raw_redirect_anchor(output dispatch_raw_redirect_anchor_t item);
        if (raw_redirect_anchor_q.size() == 0) begin
            item = make_empty_raw_redirect_anchor();
            return 1'b0;
        end
        item = raw_redirect_anchor_q.pop_front();
        return 1'b1;
    endfunction:pop_raw_redirect_anchor

    function void push_raw_csr(input dispatch_raw_csr_t item);
        if (dispatch_monitor_capture_en && item.valid &&
            runtime_csr_snapshot_valid &&
            (!latest_raw_csr_valid ||
             latest_raw_csr_seq != runtime_csr_snapshot_seq)) begin
            latest_raw_csr = item;
            latest_raw_csr_valid = 1'b1;
            latest_raw_csr_seq = runtime_csr_snapshot_seq;
        end
    endfunction:push_raw_csr

    function bit get_latest_raw_csr(output dispatch_raw_csr_t item,
                                    output int unsigned seq);
        seq = latest_raw_csr_seq;
        if (!latest_raw_csr_valid) begin
            item = make_empty_raw_csr();
            return 1'b0;
        end
        item = latest_raw_csr;
        return 1'b1;
    endfunction:get_latest_raw_csr

    // Abstract responsibility: copy one immutable CSR sample into a raw fence
    // from the same DUT sample/reset epoch. It never changes FIFO order and
    // does not decide whether the fence should invalidate an entry.
    function dispatch_raw_sfence_t bind_raw_sfence_context(
        input dispatch_raw_sfence_t item,
        input memblock_l2tlb_sfence_csr_context_t context);
        dispatch_raw_sfence_t bound_item;

        bound_item = item;
        if (!context.valid || item.sample_seq != context.sample_seq) begin
            return bound_item;
        end
        if (item.reset_epoch != context.reset_epoch) begin
            `uvm_fatal("MEMBLOCK_L2TLB_SFENCE_CONTEXT",
                       $sformatf("raw/context reset epoch mismatch raw=%0d context=%0d sample=%0d",
                                 item.reset_epoch, context.reset_epoch,
                                 item.sample_seq))
        end
        if (item.context_valid) begin
            if (item.context_reset_epoch != context.reset_epoch ||
                item.csr_sample_seq != context.sample_seq ||
                item.priv_virt_at_sample != context.priv_virt_at_sample ||
                item.hgatp_vmid_at_sample != context.hgatp_vmid_at_sample ||
                item.satp_mode_at_sample != context.satp_mode_at_sample ||
                item.vsatp_mode_at_sample != context.vsatp_mode_at_sample ||
                item.hgatp_mode_at_sample != context.hgatp_mode_at_sample) begin
                `uvm_fatal("MEMBLOCK_L2TLB_SFENCE_CONTEXT",
                           $sformatf("raw fence context changed sample=%0d", item.sample_seq))
            end
            return bound_item;
        end
        bound_item.context_valid = 1'b1;
        bound_item.context_reset_epoch = context.reset_epoch;
        bound_item.priv_virt_at_sample = context.priv_virt_at_sample;
        bound_item.hgatp_vmid_at_sample = context.hgatp_vmid_at_sample;
        bound_item.satp_mode_at_sample = context.satp_mode_at_sample;
        bound_item.vsatp_mode_at_sample = context.vsatp_mode_at_sample;
        bound_item.hgatp_mode_at_sample = context.hgatp_mode_at_sample;
        bound_item.csr_sample_seq = context.sample_seq;
        return bound_item;
    endfunction:bind_raw_sfence_context

    // Abstract responsibility: publish the CSR context that interprets raw
    // SFENCE/HFENCE bits for one DUT sample. The CSR monitor is its sole
    // caller; a later adapter only reads the frozen fields on the raw item.
    function void publish_l2tlb_sfence_csr_context(
        input dispatch_raw_csr_t raw_csr,
        input longint unsigned sample_seq,
        input longint unsigned reset_epoch,
        input time sample_time);
        memblock_l2tlb_sfence_csr_context_t context;

        if (!raw_csr.valid || !dut_sample_time_valid || sample_seq == 0 ||
            sample_seq != dut_sample_seq || sample_time != dut_sample_time) begin
            `uvm_fatal("MEMBLOCK_L2TLB_SFENCE_CONTEXT",
                       $sformatf("invalid CSR context publication sample=%0d current=%0d time=%0t/%0t valid=%0d",
                                 sample_seq, dut_sample_seq, sample_time,
                                 dut_sample_time, raw_csr.valid))
        end
        if (l2tlb_runtime_reset_active || reset_epoch != l2tlb_current_reset_epoch) begin
            `uvm_fatal("MEMBLOCK_L2TLB_SFENCE_CONTEXT",
                       $sformatf("CSR context reset epoch mismatch sample=%0d context=%0d current=%0d reset_active=%0d",
                                 sample_seq, reset_epoch,
                                 l2tlb_current_reset_epoch,
                                 l2tlb_runtime_reset_active))
        end
        context = make_empty_l2tlb_sfence_csr_context();
        context.valid = 1'b1;
        context.sample_seq = sample_seq;
        context.sample_time = sample_time;
        context.reset_epoch = reset_epoch;
        context.priv_virt_at_sample = raw_csr.priv_virt;
        context.hgatp_vmid_at_sample = raw_csr.hgatp_vmid;
        context.satp_mode_at_sample = raw_csr.satp_mode;
        context.vsatp_mode_at_sample = raw_csr.vsatp_mode;
        context.hgatp_mode_at_sample = raw_csr.hgatp_mode;

        if (l2tlb_sfence_csr_context.valid &&
            l2tlb_sfence_csr_context.sample_seq == sample_seq &&
            l2tlb_sfence_csr_context.reset_epoch == reset_epoch) begin
            if (l2tlb_sfence_csr_context.priv_virt_at_sample != context.priv_virt_at_sample ||
                l2tlb_sfence_csr_context.hgatp_vmid_at_sample != context.hgatp_vmid_at_sample ||
                l2tlb_sfence_csr_context.satp_mode_at_sample != context.satp_mode_at_sample ||
                l2tlb_sfence_csr_context.vsatp_mode_at_sample != context.vsatp_mode_at_sample ||
                l2tlb_sfence_csr_context.hgatp_mode_at_sample != context.hgatp_mode_at_sample) begin
                `uvm_fatal("MEMBLOCK_L2TLB_SFENCE_CONTEXT",
                           $sformatf("CSR context duplicate changed payload sample=%0d", sample_seq))
            end
        end
        else begin
            if (l2tlb_sfence_csr_context.valid &&
                l2tlb_sfence_csr_context.sample_seq >= sample_seq) begin
                `uvm_fatal("MEMBLOCK_L2TLB_SFENCE_CONTEXT",
                           $sformatf("CSR context sample did not advance previous=%0d current=%0d",
                                     l2tlb_sfence_csr_context.sample_seq,
                                     sample_seq))
            end
            l2tlb_sfence_csr_context = context;
        end

        // 同一接口每个 sample 最多产生一条 fence，FIFO 也按 sample 顺序入队。
        // 因而只需检查/绑定当前 sample 的队尾，不能每拍扫描积压 raw FIFO。
        if (raw_sfence_q.size() != 0) begin
            if (!raw_sfence_q[$].context_valid &&
                raw_sfence_q[$].sample_seq < sample_seq) begin
                `uvm_fatal("MEMBLOCK_L2TLB_SFENCE_CONTEXT",
                           $sformatf("raw fence missed same-sample CSR context raw_sample=%0d publish_sample=%0d",
                                     raw_sfence_q[$].sample_seq, sample_seq))
            end
            if (raw_sfence_q[$].sample_seq > sample_seq) begin
                `uvm_fatal("MEMBLOCK_L2TLB_SFENCE_CONTEXT",
                           $sformatf("raw fence sample advanced ahead of CSR context raw_sample=%0d publish_sample=%0d",
                                     raw_sfence_q[$].sample_seq, sample_seq))
            end
            if (raw_sfence_q[$].sample_seq == sample_seq) begin
                raw_sfence_q[$] = bind_raw_sfence_context(raw_sfence_q[$], context);
            end
        end
    endfunction:publish_l2tlb_sfence_csr_context

    function void push_raw_sfence(input dispatch_raw_sfence_t item);
        dispatch_raw_sfence_t bound_item;

        if (!item.valid) begin
            return;
        end
        if (!l2tlb_testcase_lifecycle_initialized) begin
            `uvm_fatal("MEMBLOCK_L2TLB_TOPOLOGY",
                       "raw fence observed before testcase L2TLB topology was initialized")
        end
        if (item.sample_seq == 0 || item.reset_epoch > l2tlb_current_reset_epoch) begin
            `uvm_fatal("MEMBLOCK_L2TLB_SFENCE_CONTEXT",
                       $sformatf("invalid/future raw fence sample=%0d raw_epoch=%0d current_epoch=%0d",
                                 item.sample_seq, item.reset_epoch,
                                 l2tlb_current_reset_epoch))
        end
        if (item.reset_epoch < l2tlb_current_reset_epoch) begin
            `uvm_info("MEMBLOCK_L2TLB_SFENCE_CONTEXT",
                      $sformatf("drop stale raw fence sample=%0d raw_epoch=%0d current_epoch=%0d",
                                item.sample_seq, item.reset_epoch,
                                l2tlb_current_reset_epoch), UVM_LOW)
            return;
        end
        if (!dispatch_l2tlb_lookup_active) begin
            if (item.lifecycle_event_seq != MEMBLOCK_L2TLB_EVENT_SEQ_NONE) begin
                `uvm_fatal("MEMBLOCK_L2TLB_TOPOLOGY",
                           $sformatf("no-dispatch raw fence has response event seq=%0d",
                                     item.lifecycle_event_seq))
            end
            return;
        end
        if (l2tlb_raw_fence_intake_closed) begin
            `uvm_fatal("MEMBLOCK_L2TLB_RELEASE",
                       $sformatf("raw fence arrived after intake close sample=%0d close_sample=%0d epoch=%0d generation=%0d",
                                 item.sample_seq,
                                 l2tlb_raw_fence_intake_cutoff_sample_seq,
                                 l2tlb_raw_fence_intake_closed_reset_epoch,
                                 l2tlb_raw_fence_intake_closed_generation))
        end
        bound_item = item;
        if (l2tlb_sfence_csr_context.valid &&
            l2tlb_sfence_csr_context.sample_seq == item.sample_seq) begin
            bound_item = bind_raw_sfence_context(item, l2tlb_sfence_csr_context);
        end
        else if (l2tlb_sfence_csr_context.valid &&
                 l2tlb_sfence_csr_context.sample_seq > item.sample_seq) begin
            `uvm_fatal("MEMBLOCK_L2TLB_SFENCE_CONTEXT",
                       $sformatf("raw fence arrived after its CSR context expired raw_sample=%0d latest_context=%0d",
                                 item.sample_seq,
                                 l2tlb_sfence_csr_context.sample_seq))
        end
        raw_sfence_q.push_back(bound_item);
    endfunction:push_raw_sfence

    function bit peek_raw_sfence(output dispatch_raw_sfence_t item);
        if (raw_sfence_q.size() == 0) begin
            item = make_empty_raw_sfence();
            return 1'b0;
        end
        item = raw_sfence_q[0];
        return 1'b1;
    endfunction:peek_raw_sfence

    function bit pop_raw_sfence(output dispatch_raw_sfence_t item);
        if (raw_sfence_q.size() == 0) begin
            item = make_empty_raw_sfence();
            return 1'b0;
        end
        item = raw_sfence_q.pop_front();
        return 1'b1;
    endfunction:pop_raw_sfence

    function void initialize_l2tlb_sample_coordinator();
        // This helper is testcase-start only.  Runtime reset uses the
        // owner-specific reset path and must not silently re-arm a second
        // responder, rewrite the testcase topology, or discard a registered
        // adapter service.  Testcase topology is initialized by the testcase
        // lifecycle owner, not by this sample-state helper.
        if (l2tlb_testcase_lifecycle_initialized ||
            l2tlb_lifecycle_owner_claimed ||
            l2tlb_adapter_service_active) begin
            `uvm_fatal("MEMBLOCK_L2TLB_TOPOLOGY",
                       "sample coordinator initialized after L2TLB testcase lifecycle became active")
        end
        l2tlb_lifecycle_owner_claimed = 1'b0;
        l2tlb_lifecycle_owner_name = "";
        dut_sample_seq = 0;
        dut_sample_time = 0;
        dut_sample_time_valid = 1'b0;
        l2tlb_sample_anchor_valid = 1'b0;
        l2tlb_owner_claimed_once = 1'b0;
        l2tlb_owner_admission_settled_sample_seq = 0;
        l2tlb_release_admission_close_requested = 1'b0;
        l2tlb_release_admission_request_owner_name = "";
        l2tlb_release_admission_close_request_sample_seq = 0;
        l2tlb_release_admission_close_reset_epoch = 0;
        l2tlb_release_admission_close_generation = 0;
        l2tlb_release_admission_closed = 1'b0;
        l2tlb_release_admission_owner_name = "";
        l2tlb_release_admission_closed_generation = 0;
        l2tlb_release_admission_cutoff_sample_seq = 0;
        l2tlb_response_drain_done = 1'b0;
        l2tlb_response_drain_owner_name = "";
        l2tlb_response_drain_generation = 0;
        l2tlb_release_final_inactive_item_done = 1'b0;
        l2tlb_release_final_inactive_generation = 0;
        l2tlb_release_final_inactive_transport_sample_seq = 0;
        l2tlb_transport_sample_recycle_done_seq = 0;
        l2tlb_monitor_final_sample_settled_valid = 1'b0;
        l2tlb_monitor_final_sample_settled_epoch = 0;
        l2tlb_monitor_final_sample_settled_transport_sample_seq = 0;
        l2tlb_release_closing = 1'b0;
        l2tlb_release_closing_owner_name = "";
        l2tlb_release_closing_generation = 0;
        l2tlb_release_granted = 1'b0;
        l2tlb_release_grant_owner_name = "";
        l2tlb_release_grant_reset_epoch = 0;
        l2tlb_release_grant_generation = 0;
        l2tlb_adapter_drain_done = 1'b0;
        l2tlb_adapter_drain_epoch = 0;
        l2tlb_adapter_drain_generation = 0;
        l2tlb_raw_fence_producer_settled_sample_seq = 0;
        l2tlb_raw_fence_intake_closed = 1'b0;
        l2tlb_raw_fence_intake_closed_reset_epoch = 0;
        l2tlb_raw_fence_intake_closed_generation = 0;
        l2tlb_raw_fence_intake_cutoff_sample_seq = 0;
        l2tlb_transport_sample_mailbox_empty_state = 1'b1;
        l2tlb_runtime_reset_active = 1'b0;
        l2tlb_current_reset_epoch = 0;
        l2tlb_runtime_reset_required_ack_mask = '0;
        l2tlb_runtime_reset_ack_mask = '0;
        l2tlb_response_owner_reset_done_epoch = 0;
        l2tlb_csr_reset_done_epoch = 0;
        l2tlb_fence_reset_done_epoch = 0;
        l2tlb_adapter_reset_done_epoch = 0;
        l2tlb_driver_reset_done_epoch = 0;
        l2tlb_monitor_reset_done_epoch = 0;
        l2tlb_post_reset_baseline_done_valid = 1'b0;
        l2tlb_post_reset_baseline_done_epoch = 0;
        l2tlb_post_reset_baseline_done_sample_seq = 0;
        foreach (l2tlb_runtime_reset_ack_provenance[idx]) begin
            l2tlb_runtime_reset_ack_provenance[idx] = "";
        end
        l2tlb_monitor_reset_pending_epoch = 0;
        l2tlb_monitor_reset_processed_epoch = 0;
        l2tlb_monitor_reset_processed_transport_sample_seq = 0;
        l2tlb_monitor_reset_ack_floor_transport_sample_seq = 0;
        l2tlb_monitor_reset_ack_epoch = 0;
        l2tlb_monitor_reset_ack_transport_sample_seq = 0;
        if (l2tlb_release_state_changed_ev == null) begin
            l2tlb_release_state_changed_ev = new("l2tlb_release_state_changed_ev");
        end
        csr_history_published_seq = 0;
        foreach (l2tlb_csr_history[idx]) begin
            l2tlb_csr_history[idx].sample_seq = 0;
            l2tlb_csr_history[idx].payload = make_empty_raw_csr();
            l2tlb_csr_history[idx].valid = 1'b0;
        end
        l2tlb_flush_event_history.delete();
        l2tlb_flush_event_seq = MEMBLOCK_L2TLB_EVENT_SEQ_NONE;
        l2tlb_flush_event_reason_mask = 2'b00;
        l2tlb_flush_sample_time = 0;
        l2tlb_flush_event_valid = 1'b0;
        last_allocated_l2tlb_event_seq = MEMBLOCK_L2TLB_EVENT_SEQ_NONE;
        response_owner_event_cursor = MEMBLOCK_L2TLB_EVENT_SEQ_NONE;
        lifecycle_event_published_seq = 0;
        sample_producer_active_seq = 0;
        sample_producer_done_mask = 2'b00;
        sample_producer_reason_mask = 2'b00;
        l2tlb_sfence_csr_context = make_empty_l2tlb_sfence_csr_context();
    endfunction:initialize_l2tlb_sample_coordinator

    function void clear_raw_monitor_queues();
        raw_int_wb_q.delete();
        raw_iq_feedback_q.delete();
        raw_ctrl_q.delete();
        deferred_raw_ctrl_q.delete();
        raw_cancel_snapshot_q.delete();
        raw_redirect_anchor_q.delete();
        raw_sfence_q.delete();
        l2tlb_sfence_csr_context = make_empty_l2tlb_sfence_csr_context();
        latest_raw_csr = make_empty_raw_csr();
        latest_raw_csr_valid = 1'b0;
        latest_raw_csr_seq = 0;
        // Runtime/raw queue clearing must not rewind the DUT global sample or
        // CSR/event history. Testcase-start calls the explicit coordinator
        // initializer; runtime reset has its own direct-writer contract.
        raw_csr_rearm_epoch++;
        dispatch_service_cycle = 0;
    endfunction:clear_raw_monitor_queues

    function void tick_dispatch_service_cycle();
        dispatch_service_cycle++;
    endfunction:tick_dispatch_service_cycle

    function longint unsigned get_dispatch_service_cycle();
        return dispatch_service_cycle;
    endfunction:get_dispatch_service_cycle

    function int unsigned raw_monitor_queue_size();
        return raw_int_wb_q.size() +
               raw_iq_feedback_q.size() +
               raw_ctrl_q.size() +
               deferred_raw_ctrl_q.size() +
               raw_cancel_snapshot_q.size() +
               raw_redirect_anchor_q.size() +
               raw_sfence_q.size();
    endfunction:raw_monitor_queue_size

    function int unsigned lsq_timing_sideband_queue_size();
        return raw_cancel_snapshot_q.size() + raw_redirect_anchor_q.size();
    endfunction:lsq_timing_sideband_queue_size
endpackage

`endif
