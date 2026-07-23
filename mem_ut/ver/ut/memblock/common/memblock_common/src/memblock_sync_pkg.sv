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
    bit reset_backend_done = 1'b0;
    bit dispatch_flush_in_progress = 1'b0;
    bit dispatch_monitor_capture_en = 1'b0;
    bit l2tlb_responder_active = 1'b0;
    bit dispatch_real_smoke_active = 1'b0;
    bit dispatch_flushsb_waiting_empty = 1'b0;
    int unsigned dispatch_flush_epoch = 0;
    longint unsigned dispatch_service_cycle = 0;
    int unsigned raw_csr_rearm_epoch = 0;
    // 中文注释：同一时刻唯一的L2TLB responder生命周期owner。
    // enable sequence在开放ready前claim，最终inactive item完成后release；DUT reset不清owner。
    bit l2tlb_lifecycle_owner_claimed = 1'b0;
    string l2tlb_lifecycle_owner_name = "";

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
        bit [1:0]         sq_deq;
        bit               lq_deq_ptr_flag;
        bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] lq_deq_ptr_value;
        bit               sq_deq_ptr_flag;
        bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] sq_deq_ptr_value;
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
        longint unsigned  cycle;
    } dispatch_raw_sfence_t;

    dispatch_raw_int_wb_t      raw_int_wb_q[$];
    dispatch_raw_iq_feedback_t raw_iq_feedback_q[$];
    dispatch_raw_ctrl_t        raw_ctrl_q[$];
    dispatch_raw_sfence_t      raw_sfence_q[$];
    dispatch_raw_csr_t         latest_raw_csr;
    bit                        latest_raw_csr_valid;
    int unsigned               latest_raw_csr_seq;

    // 中文注释：CSR monitor独立发布的post-reset runtime latest视图。
    // payload变化时序号单调递增；clear semantic raw queue和DUT reset均不清该公共快照。
    dispatch_raw_csr_t         runtime_csr_snapshot;
    bit                        runtime_csr_snapshot_valid;
    int unsigned               runtime_csr_snapshot_seq;
    // 中文注释：CSR changed或sfence monitor事件的non-destructive latest视图。
    // monitor每次真实采样递增event_seq；L2TLB sequence只保存本地last_seen，不pop raw queue。
    longint unsigned           l2tlb_flush_event_seq;
    time                       l2tlb_flush_sample_time;
    bit                        l2tlb_flush_event_valid;
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
        item.sq_deq_ptr_flag            = 1'b0;
        item.sq_deq_ptr_value           = '0;
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
        item.cycle = 0;
        return item;
    endfunction:make_empty_raw_sfence

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

    function bit try_claim_l2tlb_lifecycle_owner(input string owner_name,
                                                  output string current_owner);
        current_owner = l2tlb_lifecycle_owner_name;
        if (l2tlb_lifecycle_owner_claimed) begin
            return 1'b0;
        end
        l2tlb_lifecycle_owner_claimed = 1'b1;
        l2tlb_lifecycle_owner_name = owner_name;
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
        l2tlb_lifecycle_owner_claimed = 1'b0;
        l2tlb_lifecycle_owner_name = "";
        current_owner = "";
        return 1'b1;
    endfunction:try_release_l2tlb_lifecycle_owner

    function void note_l2tlb_flush_event(input time sample_time);
        l2tlb_flush_event_seq++;
        l2tlb_flush_sample_time = sample_time;
        l2tlb_flush_event_valid = 1'b1;
    endfunction:note_l2tlb_flush_event

    function void get_latest_l2tlb_flush_event(output longint unsigned event_seq,
                                                output time sample_time,
                                                output bit valid);
        event_seq = l2tlb_flush_event_seq;
        sample_time = l2tlb_flush_sample_time;
        valid = l2tlb_flush_event_valid;
    endfunction:get_latest_l2tlb_flush_event

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

    function void push_raw_sfence(input dispatch_raw_sfence_t item);
        if (dispatch_monitor_capture_en && item.valid) begin
            raw_sfence_q.push_back(item);
        end
    endfunction:push_raw_sfence

    function bit pop_raw_sfence(output dispatch_raw_sfence_t item);
        if (raw_sfence_q.size() == 0) begin
            item = make_empty_raw_sfence();
            return 1'b0;
        end
        item = raw_sfence_q.pop_front();
        return 1'b1;
    endfunction:pop_raw_sfence

    function void clear_raw_monitor_queues();
        raw_int_wb_q.delete();
        raw_iq_feedback_q.delete();
        raw_ctrl_q.delete();
        raw_sfence_q.delete();
        latest_raw_csr = make_empty_raw_csr();
        latest_raw_csr_valid = 1'b0;
        latest_raw_csr_seq = 0;
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
               raw_sfence_q.size();
    endfunction:raw_monitor_queue_size
endpackage

`endif
