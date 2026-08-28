//=========================================================
//File name    : memblock_rm_readonly_api.sv
//Author       : OpenAI_Codex
//Module name  : memblock_rm_readonly_api
//Discribution : Read-only value views for a future reference model
//Date         : 2026-08-11
//=========================================================
`ifndef MEMBLOCK_RM_READONLY_API__SV
`define MEMBLOCK_RM_READONLY_API__SV

`include "memblock_rm_observer_api.sv"

// 抽象职责：该 class 是测试框架状态到后续 RM 的唯一只读 façade。
// 它只探测已经存在的 owner、表项和 memory map，并把结果复制成值型 view；
// 不创建 singleton/table entry，不触发懒分配，也不返回任何内部 object/queue handle。
class memblock_rm_readonly_api extends uvm_object;

    typedef memblock_rm_dut_writeback_record_t dut_writeback_view_t;
    typedef mem_access_base_sequence::mem_addr_t      mem_addr_t;
    typedef mem_access_base_sequence::mem_line_addr_t mem_line_addr_t;
    typedef mem_access_base_sequence::mem_line_data_t mem_line_data_t;
    typedef mem_access_base_sequence::mem_line_mask_t mem_line_mask_t;
    // 中文注释：RM 只拿到 PMA/PMP 的 Access-Fault 视图；mmio/cacheable/
    // atomic_allowed 等属性留在模型内部，避免把分类误当成 RM 比较条件。
    typedef pma_pmp_af_view_t pma_pmp_af_view_for_rm_t;

    typedef struct packed {
        bit                  valid;
        memblock_uid_t       uid;
        int unsigned         op_class;
        bit                  check_store;
        int unsigned         boundary_profile;
        int unsigned         boundary_size_bytes;
        int unsigned         lsq_flow;
        bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] fu_type;
        bit [8:0]            fu_op_type;
        bit [63:0]           src_0;
        bit [63:0]           imm;
        bit [63:0]           vaddr;
        bit                  rob_idx_flag;
        bit [MEMBLOCK_ROB_VALUE_W-1:0] rob_idx_value;
        bit                  lq_idx_flag;
        bit [MEMBLOCK_LQ_VALUE_W-1:0] lq_idx_value;
        bit                  sq_idx_flag;
        bit [MEMBLOCK_SQ_VALUE_W-1:0] sq_idx_value;
        memblock_num_ls_elem_t num_ls_elem;
        bit                  tlb_af;
        bit                  tlb_pf;
        bit                  tlb_gpf;
        bit [1:0]            pbmt;
        bit                  pma_af;
        bit                  corrupt;
        bit                  denied;
        int unsigned         delay;
        int unsigned         send_pri;
        int unsigned         send_pri_std;
    } main_transaction_view_t;

    typedef struct packed {
        bit                  valid;
        memblock_uid_t       uid;
        bit                  active;
        bit                  enq;
        bit                  issue_ready;
        bit                  tlb_mapped;
        bit                  queued_load;
        bit                  queued_sta;
        bit                  queued_std;
        bit                  load_dispatched;
        bit                  sta_dispatched;
        bit                  std_dispatched;
        bit                  writeback;
        bit                  pass;
        bit                  fault;
        bit                  load_writeback;
        bit                  sta_writeback;
        bit                  std_writeback;
        bit                  load_pass;
        bit                  sta_pass;
        bit                  std_pass;
        bit                  load_fault;
        bit                  sta_fault;
        bit                  std_fault;
        bit                  exception_pending;
        bit                  replay_pending;
        bit                  redirect_pending;
        bit                  flushed;
        bit                  rob_commit;
        bit                  lsq_deq;
        bit                  success;
        bit                  terminal_done;
        bit                  rob_idx_flag;
        bit [MEMBLOCK_ROB_VALUE_W-1:0] rob_idx_value;
        bit                  lq_idx_flag;
        bit [MEMBLOCK_LQ_VALUE_W-1:0] lq_idx_value;
        bit                  sq_idx_flag;
        bit [MEMBLOCK_SQ_VALUE_W-1:0] sq_idx_value;
        bit                  active_lq_mapped;
        bit                  active_sq_mapped;
        int unsigned         dynamic_epoch;
        int unsigned         load_issue_epoch;
        bit                  load_instance_flush_epoch_valid;
        int unsigned         load_instance_flush_epoch;
        int unsigned         replay_seq;
        bit                  issue_killed;
        bit                  mmio_tag_valid;
        bit                  is_mmio_load;
        bit                  is_mmio_store;
        int unsigned         mmio_tag_dynamic_epoch;
        bit [23:0]           exception_vec;
        bit [63:0]           exception_vaddr;
        bit [63:0]           exception_gpaddr;
        longint unsigned     last_event_cycle;
    } status_view_t;

    typedef struct packed {
        bit                  valid;
        bit                  found;
        bit                  queued_load;
        bit                  queued_sta;
        bit                  queued_std;
        bit                  load_dispatched;
        bit                  sta_dispatched;
        bit                  std_dispatched;
    } issue_membership_view_t;

    typedef struct packed {
        bit                  valid;
        memblock_tlb_lookup_key_t lookup_key;
        bit [1:0]            s2xlate;
        bit [3:0]            s1_translation_mode_at_build;
        bit [3:0]            s2_translation_mode_at_build;
        bit [1:0]            s1_pte_mode_at_build;
        bit [1:0]            s2_pte_mode_at_build;
        bit [43:0]           s1_root_ppn_at_build;
        bit [43:0]           s2_root_ppn_at_build;
        int unsigned         csr_context_seq_at_build;
        longint unsigned     entry_generation;
        bit                  s1_stage_active;
        bit                  s2_stage_active;
        bit                  fault_raw_s1_pf;
        bit                  fault_raw_s1_af;
        bit                  fault_raw_s2_gpf;
        bit                  fault_raw_s2_gaf;
        bit                  fault_effective_s1_pf;
        bit                  fault_effective_s1_af;
        bit                  fault_effective_s2_gpf;
        bit                  fault_effective_s2_gaf;
        bit [1:0]            fault_stage_selected;
        bit [40:0]           s1_entry_ppn_raw;
        bit                  s1_resolved_ppn_valid;
        bit [43:0]           s1_resolved_ppn;
        bit [1:0]            s1_level;
        bit                  s1_pte_n;
        bit [1:0]            s1_entry_pbmt;
        bit                  s1_pte_r;
        bit                  s1_pte_w;
        bit                  s1_pte_x;
        bit                  s1_pte_u;
        bit                  s1_pte_g;
        bit                  s1_pte_a;
        bit                  s1_pte_d;
        bit                  s1_pte_v;
        bit [2:0]            s1_addr_low;
        bit [37:0]           s2_entry_ppn_raw;
        bit                  s2_resolved_ppn_valid;
        bit [43:0]           s2_resolved_ppn;
        bit [1:0]            s2_level;
        bit                  s2_pte_n;
        bit [1:0]            s2_entry_pbmt;
        bit                  s2_pte_r;
        bit                  s2_pte_w;
        bit                  s2_pte_x;
        bit                  s2_pte_u;
        bit                  s2_pte_g;
        bit                  s2_pte_a;
        bit                  s2_pte_d;
        bit                  fault;
        bit                  pma_af;
        longint unsigned     create_cycle;
        longint unsigned     last_hit_cycle;
    } tlb_entry_view_t;

    // 只读请求上下文。entry_key 仅解析 tlb_entry_by_key 的 canonical key；
    // 表项内容必须另行调用 read_tlb_entry_for_rm() 取得。
    typedef struct packed {
        bit                  valid;
        memblock_uid_t       uid;
        bit [51:0]           request_vpn;
        bit [1:0]            s2xlate;
        bit                  is_hypervisor_inst;
        memblock_tlb_lookup_key_t request_key;
        memblock_tlb_lookup_key_t entry_key;
        bit                  range_hit;
        bit                  request_translation_valid;
        bit [43:0]           request_s1_resolved_ppn;
        bit [43:0]           request_s2_resolved_ppn;
        bit [51:0]           request_gvpn;
        bit [3:0]            satp_mode;
        bit [15:0]           satp_asid;
        bit [43:0]           satp_ppn;
        bit [3:0]            vsatp_mode;
        bit [15:0]           vsatp_asid;
        bit [43:0]           vsatp_ppn;
        bit [3:0]            hgatp_mode;
        bit [15:0]           hgatp_vmid;
        bit [43:0]           hgatp_ppn;
        bit                  priv_virt;
        bit                  priv_spvp;
        bit [1:0]            priv_imode;
        bit [1:0]            priv_dmode;
        bit                  priv_mxr;
        bit                  priv_sum;
        bit                  priv_vmxr;
        bit                  priv_vsum;
        bit                  m_pbmt_en;
        bit                  h_pbmt_en;
        bit                  hd_misalign_ld_enable;
        bit                  hd_misalign_st_enable;
        bit                  priv_debug;
        int unsigned         csr_update_seq;
    } tlb_request_context_view_t;

    typedef struct packed {
        bit                  valid;
        memblock_uid_t       uid;
        bit                  record_valid;
        bit                  pte_valid;
        bit [51:0]           vpn;
        bit [1:0]            s2xlate;
        bit                  is_hypervisor_inst;
        memblock_tlb_lookup_key_t lookup_key;
        int unsigned         wait_epoch;
        int unsigned         wait_state;
        longint unsigned     uid_wait_start_sample_seq;
        longint unsigned     first_request_fire_sample_seq;
        longint unsigned     pte_update_cycle;
        bit                  payload_valid;
        bit [43:0]           request_s1_resolved_ppn;
        bit [43:0]           request_s2_resolved_ppn;
        bit [51:0]           request_gvpn;
        int unsigned         csr_update_seq;
    } uid_tlb_view_t;

    typedef struct packed {
        bit                  valid;
        bit                  corrupt;
        bit                  data_valid;
        mem_line_data_t      data;
        mem_line_mask_t      byte_valid;
        mem_line_mask_t      corrupt_byte_mask;
    } memory_read_view_t;

    typedef struct packed {
        bit                  valid;
        bit                  ready;
    } dcache_overlay_readiness_view_t;

    // 中文注释：RM 读取的 L2 D-error 值型快照。valid 表示账本已经完成 testcase
    // 初始化；state/activation 来自指定 query sample，绝不暴露 live associative map。
    typedef struct packed {
        bit                  valid;
        bit                  sticky_enabled;
        bit                  denied;
        bit                  corrupt;
        mem_access_base_sequence::l2_d_error_state_e state;
        longint unsigned     generation;
        longint unsigned     corrupt_activation_sample;
        longint unsigned     denied_activation_sample;
        bit                  source_valid;
        bit [9:0]            source;
    } l2_d_error_view_t;

    // 中文注释：dispatch 上下文是 RM 关联 UID 表项、当前采样和 flush/replay
    // 生命周期的只读标量快照。它不暴露或消费任一内部 queue；其中 pending 计数
    // 只表示调用瞬间已有 PTW-wait replay 项数，不能用于驱动或推进 replay。
    typedef struct packed {
        bit                  valid;
        bit                  main_table_ready;
        int unsigned         main_trans_num;
        memblock_uid_t       next_uid;
        memblock_uid_t       terminal_done_uid;
        bit                  global_stop_requested;
        bit                  reset_backend_done;
        bit                  flush_in_progress;
        int unsigned         redirect_phase;
        bit                  redirect_drive_inflight;
        int unsigned         global_issue_epoch;
        int unsigned         dispatch_flush_epoch;
        bit                  dispatch_flush_in_progress;
        int unsigned         ptw_wait_replay_count;
        longint unsigned     current_dut_sample_seq;
        longint unsigned     latest_drained_cancel_sample_seq;
    } framework_context_view_t;

    static memblock_rm_readonly_api m_inst;
    static memblock_rm_observer_cache m_observer;
    static memblock_rm_dut_writeback_observer m_dut_writeback_observer;

    `uvm_object_utils(memblock_rm_readonly_api)

    extern function new(string name = "memblock_rm_readonly_api");
    extern static function memblock_rm_readonly_api get();
    extern static function void bind_observer(memblock_rm_observer_cache observer);
    extern static function void bind_dut_writeback_observer_vif_for_rm(
        virtual io_mem_to_ooo_int_wb_agent_agent_interface.mon_mp vif
    );
    extern task run_dut_writeback_observer_for_rm();
    extern function bit read_dut_load_writeback_for_rm(
        input memblock_rob_key_t key,
        output dut_writeback_view_t view
    );
    extern function bit read_observer_capability_for_rm(
        output memblock_rm_observer_cache::capability_view_t view
    );
    extern function bit read_load_actual_for_rm(
        input memblock_uid_t uid,
        output memblock_rm_observer_cache::load_actual_view_t view
    );
    extern function bit read_store_input_for_rm(
        input memblock_uid_t uid,
        output memblock_rm_observer_cache::store_input_view_t view
    );
    extern function bit read_translation_snapshot_for_rm(
        input memblock_uid_t uid,
        output memblock_rm_observer_cache::translation_snapshot_view_t view
    );
    extern function bit read_commit_rob_for_rm(
        input int unsigned cursor,
        output memblock_rm_observer_cache::commit_rob_view_t view
    );
    extern function bit read_store_final_for_rm(
        input longint unsigned pa,
        output memblock_rm_observer_cache::store_final_entry_view_t view
    );
    extern function int unsigned read_commit_count_for_rm();
    extern function int unsigned read_store_final_count_for_rm();
    extern function bit read_store_final_by_index_for_rm(
        input int unsigned index,
        output memblock_rm_observer_cache::store_final_entry_view_t view
    );
    extern function bit read_main_transaction_for_rm(
        input  memblock_uid_t uid,
        output main_transaction_view_t view
    );
    extern function bit read_status_for_rm(
        input  memblock_uid_t uid,
        output status_view_t view
    );
    extern function bit read_issue_membership_for_rm(
        input  memblock_uid_t uid,
        output issue_membership_view_t view
    );
    extern function bit read_framework_context_for_rm(
        output framework_context_view_t view
    );
    extern function bit framework_ready_for_rm();
    extern function bit read_uid_by_rob_for_rm(
        input  memblock_rob_key_t key,
        output memblock_uid_t uid
    );
    extern function bit read_uid_by_lq_for_rm(
        input  memblock_lq_key_t key,
        output memblock_uid_t uid
    );
    extern function bit read_uid_by_sq_for_rm(
        input  memblock_sq_key_t key,
        output memblock_uid_t uid
    );
    extern function bit read_tlb_entry_for_rm(
        input  memblock_tlb_lookup_key_t key,
        output tlb_entry_view_t view
    );
    extern function bit read_tlb_request_context_for_rm(
        input  memblock_uid_t uid,
        input  bit [51:0] request_vpn,
        output tlb_request_context_view_t view
    );
    extern function bit resolve_tlb_entry_key_for_rm(
        input  memblock_uid_t uid,
        input  bit [51:0] request_vpn,
        output tlb_request_context_view_t view
    );
    extern function bit read_uid_tlb_for_rm(
        input  memblock_uid_t uid,
        output uid_tlb_view_t view
    );
    extern function bit read_initialized_backing_for_rm(
        input  mem_addr_t      addr,
        input  mem_line_mask_t byte_mask,
        output memory_read_view_t view
    );
    extern function bit try_read_initialized_backing_for_rm(
        input  mem_addr_t      addr,
        input  mem_line_mask_t byte_mask,
        output memory_read_view_t view
    );
    extern function bit read_committed_overlay_for_rm(
        input  mem_addr_t      addr,
        input  mem_line_mask_t byte_mask,
        output memory_read_view_t view
    );
    extern function bit try_read_committed_overlay_for_rm(
        input  mem_addr_t      addr,
        input  mem_line_mask_t byte_mask,
        output memory_read_view_t view
    );
    extern function bit get_dcache_overlay_readiness_for_rm(
        output dcache_overlay_readiness_view_t view
    );
    extern function bit read_l2_d_error_for_rm(
        input  mem_addr_t          line_addr,
        input  longint unsigned    sample,
        output l2_d_error_view_t   view
    );
    extern function bit read_pma_pmp_af_for_rm(
        input memblock_uid_t            uid,
        input int unsigned              dynamic_epoch,
        input bit                       translation_success,
        input bit [47:0]                paddr,
        input int unsigned              size_bytes,
        input pma_pmp_cmd_e             cmd,
        output pma_pmp_af_view_for_rm_t view
    );

    extern function bit try_get_common_data(output common_data_transaction data);
    extern function bit report_query_miss(input string query_name, input string detail);
    extern function bit read_memory_map(
        input  bit              overlay,
        input  bit              report_miss,
        input  mem_addr_t       addr,
        input  mem_line_mask_t  byte_mask,
        output memory_read_view_t view
    );
    extern function void copy_main_transaction(
        input  main_control_transaction source,
        output main_transaction_view_t view
    );
    extern function void copy_status(
        input  status_transaction source,
        output status_view_t view
    );
    extern function void copy_tlb_entry(
        input  memblock_tlb_entry source,
        output tlb_entry_view_t view
    );
    extern function void copy_uid_tlb_record(
        input  memblock_uid_tlb_record source,
        output uid_tlb_view_t view
    );

endclass:memblock_rm_readonly_api

function memblock_rm_readonly_api::new(string name = "memblock_rm_readonly_api");
    super.new(name);
endfunction:new

function memblock_rm_readonly_api memblock_rm_readonly_api::get();
    if (m_inst == null) begin
        m_inst = new("memblock_rm_readonly_api_singleton");
    end
    if (m_dut_writeback_observer == null) begin
        m_dut_writeback_observer = new("memblock_rm_dut_writeback_observer_singleton");
    end
    return m_inst;
endfunction:get

function void memblock_rm_readonly_api::bind_observer(memblock_rm_observer_cache observer);
    m_observer = observer;
endfunction:bind_observer

function void memblock_rm_readonly_api::bind_dut_writeback_observer_vif_for_rm(
    virtual io_mem_to_ooo_int_wb_agent_agent_interface.mon_mp vif
);
    memblock_rm_readonly_api api;
    api = get();
    m_dut_writeback_observer.bind_vif(vif);
endfunction:bind_dut_writeback_observer_vif_for_rm

task memblock_rm_readonly_api::run_dut_writeback_observer_for_rm();
    void'(get());
    m_dut_writeback_observer.run();
endtask:run_dut_writeback_observer_for_rm

function bit memblock_rm_readonly_api::read_dut_load_writeback_for_rm(
    input memblock_rob_key_t key,
    output dut_writeback_view_t view
);
    view = '{default:'0};
    void'(get());
    return m_dut_writeback_observer.read_latest_load_by_rob(key, view);
endfunction:read_dut_load_writeback_for_rm

function bit memblock_rm_readonly_api::read_observer_capability_for_rm(
    output memblock_rm_observer_cache::capability_view_t view
);
    view = '{default:'0};
    // Observer facts arrive asynchronously.  Returning zero here is a normal
    // not-ready condition; the RM records a semantic failure only on commit.
    return m_observer != null && m_observer.read_capability(view);
endfunction:read_observer_capability_for_rm

function bit memblock_rm_readonly_api::read_load_actual_for_rm(
    input memblock_uid_t uid,
    output memblock_rm_observer_cache::load_actual_view_t view
);
    view = '{default:'0};
    return m_observer != null && m_observer.read_load_actual(uid, view);
endfunction:read_load_actual_for_rm

function bit memblock_rm_readonly_api::read_store_input_for_rm(
    input memblock_uid_t uid,
    output memblock_rm_observer_cache::store_input_view_t view
);
    view = '{default:'0};
    return m_observer != null && m_observer.read_store_input(uid, view);
endfunction:read_store_input_for_rm

function bit memblock_rm_readonly_api::read_translation_snapshot_for_rm(
    input memblock_uid_t uid,
    output memblock_rm_observer_cache::translation_snapshot_view_t view
);
    view = '{default:'0};
    return m_observer != null && m_observer.read_translation(uid, view);
endfunction:read_translation_snapshot_for_rm

function bit memblock_rm_readonly_api::read_commit_rob_for_rm(
    input int unsigned cursor,
    output memblock_rm_observer_cache::commit_rob_view_t view
);
    view = '{default:'0};
    return m_observer != null && m_observer.read_commit(cursor, view);
endfunction:read_commit_rob_for_rm

function bit memblock_rm_readonly_api::read_store_final_for_rm(
    input longint unsigned pa,
    output memblock_rm_observer_cache::store_final_entry_view_t view
);
    mem_line_mask_t   byte_mask;
    memory_read_view_t memory_view;

    // 按 RM 给出的最终 PA 复制框架 Mem_overlay 的单 byte 结果；不依赖 observer
    // producer，不创建 overlay line，也不修改 shared-memory 生命周期。
    view = '{default:'0};
    if (pa > 64'h0000_ffff_ffff_ffff) return 1'b0;

    byte_mask = '0;
    byte_mask[0] = 1'b1;
    if (!read_memory_map(1'b1, 1'b0, mem_addr_t'(pa), byte_mask, memory_view)) begin
        return 1'b0;
    end

    view.valid          = memory_view.valid;
    view.pa             = pa;
    view.value          = memory_view.data[7:0];
    view.byte_valid     = memory_view.byte_valid[0];
    view.corrupt        = memory_view.corrupt_byte_mask[0];
    view.observed_cycle = memblock_sync_pkg::peek_current_dut_global_sample();
    return view.valid && (view.byte_valid || view.corrupt);
endfunction:read_store_final_for_rm

function int unsigned memblock_rm_readonly_api::read_commit_count_for_rm();
    if (m_observer == null) return 0;
    return m_observer.commit_count();
endfunction:read_commit_count_for_rm

function int unsigned memblock_rm_readonly_api::read_store_final_count_for_rm();
    if (m_observer == null) return 0;
    return m_observer.final_store_count();
endfunction:read_store_final_count_for_rm

function bit memblock_rm_readonly_api::read_store_final_by_index_for_rm(
    input int unsigned index,
    output memblock_rm_observer_cache::store_final_entry_view_t view
);
    view = '{default:'0};
    return m_observer != null && m_observer.read_store_final_by_index(index, view);
endfunction:read_store_final_by_index_for_rm

function bit memblock_rm_readonly_api::try_get_common_data(output common_data_transaction data);
    data = common_data_transaction::m_inst;
    return data != null;
endfunction:try_get_common_data

function bit memblock_rm_readonly_api::framework_ready_for_rm();
    return common_data_transaction::m_inst != null;
endfunction:framework_ready_for_rm

function bit memblock_rm_readonly_api::report_query_miss(input string query_name, input string detail);
    `uvm_error("RM_READONLY_API", $sformatf("%s query miss: %s", query_name, detail))
    return 1'b0;
endfunction:report_query_miss

function void memblock_rm_readonly_api::copy_main_transaction(
    input  main_control_transaction source,
    output main_transaction_view_t view
);
    view = '{default:'0};
    view.valid              = 1'b1;
    view.uid                = source.uid;
    view.op_class           = source.op_class;
    view.check_store        = source.op_class == MEMBLOCK_OP_CLASS_CHECK_STORE;
    view.boundary_profile   = source.boundary_profile;
    view.boundary_size_bytes = source.boundary_size_bytes;
    view.lsq_flow           = source.lsq_flow;
    view.fu_type            = source.fuType;
    view.fu_op_type         = source.fuOpType;
    view.src_0              = source.src_0;
    view.imm                = source.imm;
    view.vaddr              = source.vaddr;
    view.rob_idx_flag       = source.robIdx_flag;
    view.rob_idx_value      = source.robIdx_value;
    view.lq_idx_flag        = source.lqIdx_flag;
    view.lq_idx_value       = source.lqIdx_value;
    view.sq_idx_flag        = source.sqIdx_flag;
    view.sq_idx_value       = source.sqIdx_value;
    view.num_ls_elem        = source.numLsElem;
    view.tlb_af             = source.tlbAF;
    view.tlb_pf             = source.tlbPF;
    view.tlb_gpf            = source.tlbGPF;
    view.pbmt               = source.PBMT;
    view.pma_af             = source.pmaAF;
    view.corrupt            = source.corrupt;
    view.denied             = source.denied;
    view.delay              = source.delay;
    view.send_pri           = source.send_pri;
    view.send_pri_std       = source.send_pri_std;
endfunction:copy_main_transaction

function void memblock_rm_readonly_api::copy_status(
    input  status_transaction source,
    output status_view_t view
);
    view = '{default:'0};
    view.valid              = 1'b1;
    view.uid                = source.uid;
    view.active             = source.active;
    view.enq                = source.enq;
    view.issue_ready        = source.issue_ready;
    view.tlb_mapped         = source.tlb_mapped;
    view.queued_load        = source.queued_load;
    view.queued_sta         = source.queued_sta;
    view.queued_std         = source.queued_std;
    view.load_dispatched    = source.load_dispatched;
    view.sta_dispatched     = source.sta_dispatched;
    view.std_dispatched     = source.std_dispatched;
    view.writeback          = source.writeback;
    view.pass               = source.pass;
    view.fault              = source.fault;
    view.load_writeback     = source.load_writeback;
    view.sta_writeback      = source.sta_writeback;
    view.std_writeback      = source.std_writeback;
    view.load_pass          = source.load_pass;
    view.sta_pass           = source.sta_pass;
    view.std_pass           = source.std_pass;
    view.load_fault         = source.load_fault;
    view.sta_fault          = source.sta_fault;
    view.std_fault          = source.std_fault;
    view.exception_pending  = source.exception_pending;
    view.replay_pending     = source.replay_pending;
    view.redirect_pending   = source.redirect_pending;
    view.flushed            = source.flushed;
    view.rob_commit         = source.rob_commit;
    view.lsq_deq            = source.lsq_deq;
    view.success            = source.success;
    view.terminal_done      = source.terminal_done;
    view.rob_idx_flag       = source.robIdx_flag;
    view.rob_idx_value      = source.robIdx_value;
    view.lq_idx_flag        = source.lqIdx_flag;
    view.lq_idx_value       = source.lqIdx_value;
    view.sq_idx_flag        = source.sqIdx_flag;
    view.sq_idx_value       = source.sqIdx_value;
    view.active_lq_mapped   = source.active_lq_mapped;
    view.active_sq_mapped   = source.active_sq_mapped;
    view.dynamic_epoch      = source.dynamic_epoch;
    view.load_issue_epoch   = source.load_issue_epoch;
    view.load_instance_flush_epoch_valid = source.load_instance_flush_epoch_valid;
    view.load_instance_flush_epoch = source.load_instance_flush_epoch;
    view.replay_seq         = source.replay_seq;
    view.issue_killed       = source.issue_killed;
    view.mmio_tag_valid     = source.mmio_tag_valid;
    view.is_mmio_load       = source.is_mmio_load;
    view.is_mmio_store      = source.is_mmio_store;
    view.mmio_tag_dynamic_epoch = source.mmio_tag_dynamic_epoch;
    view.exception_vec      = source.exception_vec;
    view.exception_vaddr    = source.exception_vaddr;
    view.exception_gpaddr   = source.exception_gpaddr;
    view.last_event_cycle   = source.last_event_cycle;
endfunction:copy_status

function void memblock_rm_readonly_api::copy_tlb_entry(
    input  memblock_tlb_entry source,
    output tlb_entry_view_t view
);
    view = '{default:'0};
    view.valid                  = 1'b1;
    view.lookup_key             = source.lookup_key;
    view.s2xlate                = source.s2xlate;
    view.s1_translation_mode_at_build = source.s1_translation_mode_at_build;
    view.s2_translation_mode_at_build = source.s2_translation_mode_at_build;
    view.s1_pte_mode_at_build   = source.s1_pte_mode_at_build;
    view.s2_pte_mode_at_build   = source.s2_pte_mode_at_build;
    view.s1_root_ppn_at_build   = source.s1_root_ppn_at_build;
    view.s2_root_ppn_at_build   = source.s2_root_ppn_at_build;
    view.csr_context_seq_at_build = source.csr_context_seq_at_build;
    view.entry_generation       = source.entry_generation;
    view.s1_stage_active        = source.s1_stage_active;
    view.s2_stage_active        = source.s2_stage_active;
    view.fault_raw_s1_pf        = source.fault_raw_s1_pf;
    view.fault_raw_s1_af        = source.fault_raw_s1_af;
    view.fault_raw_s2_gpf       = source.fault_raw_s2_gpf;
    view.fault_raw_s2_gaf       = source.fault_raw_s2_gaf;
    view.fault_effective_s1_pf  = source.fault_effective_s1_pf;
    view.fault_effective_s1_af  = source.fault_effective_s1_af;
    view.fault_effective_s2_gpf = source.fault_effective_s2_gpf;
    view.fault_effective_s2_gaf = source.fault_effective_s2_gaf;
    view.fault_stage_selected   = source.fault_stage_selected;
    view.s1_entry_ppn_raw       = source.s1_entry_ppn_raw;
    view.s1_resolved_ppn_valid  = source.s1_resolved_ppn_valid;
    view.s1_resolved_ppn        = source.s1_resolved_ppn;
    view.s1_level               = source.s1_level;
    view.s1_pte_n               = source.s1_pte_n;
    view.s1_entry_pbmt          = source.s1_entry_pbmt;
    view.s1_pte_r               = source.s1_pte_r;
    view.s1_pte_w               = source.s1_pte_w;
    view.s1_pte_x               = source.s1_pte_x;
    view.s1_pte_u               = source.s1_pte_u;
    view.s1_pte_g               = source.s1_pte_g;
    view.s1_pte_a               = source.s1_pte_a;
    view.s1_pte_d               = source.s1_pte_d;
    view.s1_pte_v               = source.s1_pte_v;
    view.s1_addr_low            = source.s1_addr_low;
    view.s2_entry_ppn_raw       = source.s2_entry_ppn_raw;
    view.s2_resolved_ppn_valid  = source.s2_resolved_ppn_valid;
    view.s2_resolved_ppn        = source.s2_resolved_ppn;
    view.s2_level               = source.s2_level;
    view.s2_pte_n               = source.s2_pte_n;
    view.s2_entry_pbmt          = source.s2_entry_pbmt;
    view.s2_pte_r               = source.s2_pte_r;
    view.s2_pte_w               = source.s2_pte_w;
    view.s2_pte_x               = source.s2_pte_x;
    view.s2_pte_u               = source.s2_pte_u;
    view.s2_pte_g               = source.s2_pte_g;
    view.s2_pte_a               = source.s2_pte_a;
    view.s2_pte_d               = source.s2_pte_d;
    view.fault                  = source.has_effective_fault();
    view.pma_af                 = source.pmaAF;
    view.create_cycle           = source.create_cycle;
    view.last_hit_cycle         = source.last_hit_cycle;
endfunction:copy_tlb_entry

function void memblock_rm_readonly_api::copy_uid_tlb_record(
    input  memblock_uid_tlb_record source,
    output uid_tlb_view_t view
);
    view = '{default:'0};
    view.valid                       = 1'b1;
    view.uid                         = source.uid;
    view.record_valid                = source.record_valid;
    view.pte_valid                   = source.pte_valid;
    view.vpn                         = source.vpn;
    view.s2xlate                     = source.s2xlate;
    view.is_hypervisor_inst          = source.is_hypervisor_inst;
    view.lookup_key                  = source.lookup_key;
    view.wait_epoch                  = source.uid_tlb_wait_epoch;
    view.wait_state                  = source.uid_tlb_wait_state;
    view.uid_wait_start_sample_seq  = source.uid_wait_start_sample_seq;
    view.first_request_fire_sample_seq = source.uid_tlb_first_request_fire_sample_seq;
    view.pte_update_cycle            = source.pte_update_cycle;
    view.payload_valid               = source.payload != null;
    view.request_s1_resolved_ppn     = source.request_s1_resolved_ppn;
    view.request_s2_resolved_ppn     = source.request_s2_resolved_ppn;
    view.request_gvpn                = source.request_gvpn;
    if (source.csr_snapshot != null) begin
        view.csr_update_seq = source.csr_snapshot.update_seq;
    end
endfunction:copy_uid_tlb_record

function bit memblock_rm_readonly_api::read_main_transaction_for_rm(
    input  memblock_uid_t uid,
    output main_transaction_view_t view
);
    common_data_transaction data;
    view = '{default:'0};
    if (!try_get_common_data(data)) begin
        return report_query_miss("main_transaction", "common data owner is not initialized");
    end
    if (uid >= data.main_trans_num || data.main_table_by_uid.size() <= uid) begin
        return report_query_miss("main_transaction", $sformatf("uid=%0d is outside the initialized table", uid));
    end
    if (data.main_table_by_uid[uid] == null) begin
        return report_query_miss("main_transaction", $sformatf("uid=%0d has no table entry", uid));
    end
    copy_main_transaction(data.main_table_by_uid[uid], view);
    return 1'b1;
endfunction:read_main_transaction_for_rm

function bit memblock_rm_readonly_api::read_status_for_rm(
    input  memblock_uid_t uid,
    output status_view_t view
);
    common_data_transaction data;
    view = '{default:'0};
    if (!try_get_common_data(data)) begin
        return report_query_miss("status", "common data owner is not initialized");
    end
    if (uid >= data.main_trans_num || data.status_by_uid.size() <= uid) begin
        return report_query_miss("status", $sformatf("uid=%0d is outside the initialized table", uid));
    end
    if (data.status_by_uid[uid] == null) begin
        return report_query_miss("status", $sformatf("uid=%0d has no status entry", uid));
    end
    copy_status(data.status_by_uid[uid], view);
    return 1'b1;
endfunction:read_status_for_rm

function bit memblock_rm_readonly_api::read_issue_membership_for_rm(
    input  memblock_uid_t uid,
    output issue_membership_view_t view
);
    status_view_t status_view;
    view = '{default:'0};
    if (!read_status_for_rm(uid, status_view)) begin
        return 1'b0;
    end
    view.valid           = 1'b1;
    view.found           = status_view.active || status_view.queued_load ||
                           status_view.queued_sta || status_view.queued_std ||
                           status_view.load_dispatched || status_view.sta_dispatched ||
                           status_view.std_dispatched;
    view.queued_load     = status_view.queued_load;
    view.queued_sta      = status_view.queued_sta;
    view.queued_std      = status_view.queued_std;
    view.load_dispatched = status_view.load_dispatched;
    view.sta_dispatched  = status_view.sta_dispatched;
    view.std_dispatched  = status_view.std_dispatched;
    return 1'b1;
endfunction:read_issue_membership_for_rm

function bit memblock_rm_readonly_api::read_framework_context_for_rm(
    output framework_context_view_t view
);
    common_data_transaction data;

    view = '{default:'0};
    if (!try_get_common_data(data)) begin
        return report_query_miss("framework_context", "common data owner is not initialized");
    end
    view.valid                            = 1'b1;
    view.main_table_ready                 = data.main_table_ready;
    view.main_trans_num                   = data.main_trans_num;
    view.next_uid                         = data.next_uid;
    view.terminal_done_uid                = data.dispatch_progress.terminal_done_uid;
    view.global_stop_requested            = data.global_stop_requested;
    view.reset_backend_done               = memblock_sync_pkg::reset_backend_done;
    view.flush_in_progress                = data.flush_in_progress;
    view.redirect_phase                   = data.redirect_phase;
    view.redirect_drive_inflight          = data.redirect_drive_inflight;
    view.global_issue_epoch               = data.global_issue_epoch;
    view.dispatch_flush_epoch             = memblock_sync_pkg::dispatch_flush_epoch;
    view.dispatch_flush_in_progress       = memblock_sync_pkg::dispatch_flush_in_progress;
    view.ptw_wait_replay_count            = data.ptw_wait_replay_q.size();
    view.current_dut_sample_seq           = memblock_sync_pkg::peek_current_dut_global_sample();
    view.latest_drained_cancel_sample_seq = data.latest_drained_cancel_sample_seq;
    return 1'b1;
endfunction:read_framework_context_for_rm

function bit memblock_rm_readonly_api::read_uid_by_rob_for_rm(
    input  memblock_rob_key_t key,
    output memblock_uid_t uid
);
    common_data_transaction data;
    memblock_rob_map_key_t map_key;
    uid = '0;
    if (!try_get_common_data(data)) begin
        return report_query_miss("uid_by_rob", "common data owner is not initialized");
    end
    map_key = {key.flag, key.value};
    if (!data.uid_by_active_rob.exists(map_key)) begin
        return report_query_miss("uid_by_rob", $sformatf("ROB key flag=%0d value=%0d is absent", key.flag, key.value));
    end
    uid = data.uid_by_active_rob[map_key];
    return 1'b1;
endfunction:read_uid_by_rob_for_rm

function bit memblock_rm_readonly_api::read_uid_by_lq_for_rm(
    input  memblock_lq_key_t key,
    output memblock_uid_t uid
);
    common_data_transaction data;
    memblock_lq_map_key_t map_key;
    uid = '0;
    if (!try_get_common_data(data)) begin
        return report_query_miss("uid_by_lq", "common data owner is not initialized");
    end
    map_key = {key.flag, key.value};
    if (!data.uid_by_lq.exists(map_key)) begin
        return report_query_miss("uid_by_lq", $sformatf("LQ key flag=%0d value=%0d is absent", key.flag, key.value));
    end
    uid = data.uid_by_lq[map_key];
    return 1'b1;
endfunction:read_uid_by_lq_for_rm

function bit memblock_rm_readonly_api::read_uid_by_sq_for_rm(
    input  memblock_sq_key_t key,
    output memblock_uid_t uid
);
    common_data_transaction data;
    memblock_sq_map_key_t map_key;
    uid = '0;
    if (!try_get_common_data(data)) begin
        return report_query_miss("uid_by_sq", "common data owner is not initialized");
    end
    map_key = {key.flag, key.value};
    if (!data.uid_by_sq.exists(map_key)) begin
        return report_query_miss("uid_by_sq", $sformatf("SQ key flag=%0d value=%0d is absent", key.flag, key.value));
    end
    uid = data.uid_by_sq[map_key];
    return 1'b1;
endfunction:read_uid_by_sq_for_rm

function bit memblock_rm_readonly_api::read_tlb_entry_for_rm(
    input  memblock_tlb_lookup_key_t key,
    output tlb_entry_view_t view
);
    common_data_transaction data;
    view = '{default:'0};
    if (!try_get_common_data(data)) begin
        return report_query_miss("tlb_entry", "common data owner is not initialized");
    end
    if (!data.tlb_entry_by_key.exists(key) || data.tlb_entry_by_key[key] == null) begin
        return report_query_miss("tlb_entry", "requested lookup key is absent");
    end
    copy_tlb_entry(data.tlb_entry_by_key[key], view);
    return 1'b1;
endfunction:read_tlb_entry_for_rm

function bit memblock_rm_readonly_api::read_tlb_request_context_for_rm(
    input  memblock_uid_t uid,
    input  bit [51:0] request_vpn,
    output tlb_request_context_view_t view
);
    common_data_transaction data;
    memblock_uid_tlb_record record;
    mmu_csr_runtime_state csr;

    view = '{default:'0};
    if (!try_get_common_data(data)) begin
        return report_query_miss("tlb_request_context", "common data owner is not initialized");
    end
    if (!data.uid_tlb_record_by_uid.exists(uid) ||
        data.uid_tlb_record_by_uid[uid] == null ||
        !data.uid_tlb_record_by_uid[uid].record_valid) begin
        return report_query_miss("tlb_request_context", $sformatf("uid=%0d request context is unavailable", uid));
    end
    record = data.uid_tlb_record_by_uid[uid];
    csr = record.csr_snapshot;
    if (csr == null) begin
        return report_query_miss("tlb_request_context", $sformatf("uid=%0d CSR snapshot is unavailable", uid));
    end

    view.valid            = 1'b1;
    view.uid              = uid;
    view.request_vpn      = request_vpn;
    view.s2xlate          = record.s2xlate;
    view.is_hypervisor_inst = record.is_hypervisor_inst;
    view.request_key      = csr.make_lookup_key(request_vpn, record.s2xlate);
    view.entry_key        = view.request_key;
    view.satp_mode        = csr.satp_mode;
    view.satp_asid        = csr.satp_asid;
    view.satp_ppn         = csr.satp_ppn;
    view.vsatp_mode       = csr.vsatp_mode;
    view.vsatp_asid       = csr.vsatp_asid;
    view.vsatp_ppn        = csr.vsatp_ppn;
    view.hgatp_mode       = csr.hgatp_mode;
    view.hgatp_vmid       = csr.hgatp_vmid;
    view.hgatp_ppn        = csr.hgatp_ppn;
    view.priv_virt        = csr.priv_virt;
    view.priv_spvp        = csr.priv_spvp;
    view.priv_imode       = csr.priv_imode;
    view.priv_dmode       = csr.priv_dmode;
    view.priv_mxr         = csr.priv_mxr;
    view.priv_sum         = csr.priv_sum;
    view.priv_vmxr        = csr.priv_vmxr;
    view.priv_vsum        = csr.priv_vsum;
    view.m_pbmt_en        = csr.m_pbmt_en;
    view.h_pbmt_en        = csr.h_pbmt_en;
    view.hd_misalign_ld_enable = csr.hd_misalign_ld_enable;
    view.hd_misalign_st_enable = csr.hd_misalign_st_enable;
    view.priv_debug       = csr.priv_debug;
    view.csr_update_seq   = csr.update_seq;
    return 1'b1;
endfunction:read_tlb_request_context_for_rm

function bit memblock_rm_readonly_api::resolve_tlb_entry_key_for_rm(
    input  memblock_uid_t uid,
    input  bit [51:0] request_vpn,
    output tlb_request_context_view_t view
);
    common_data_transaction data;
    memblock_uid_tlb_record record;
    mmu_csr_runtime_state csr;
    memblock_tlb_entry entry;

    if (!read_tlb_request_context_for_rm(uid, request_vpn, view)) return 1'b0;
    if (!try_get_common_data(data) ||
        !data.uid_tlb_record_by_uid.exists(uid) ||
        data.uid_tlb_record_by_uid[uid] == null) begin
        return report_query_miss("tlb_request_context",
                                 $sformatf("uid=%0d context disappeared during query", uid));
    end
    record = data.uid_tlb_record_by_uid[uid];
    csr = record.csr_snapshot;

    if (data.tlb_entry_by_key.exists(view.request_key) &&
        data.tlb_entry_by_key[view.request_key] != null) begin
        entry = data.tlb_entry_by_key[view.request_key];
    end else if (data.find_tlb_range_hit_by_req(view.request_key, csr,
                                                view.entry_key, entry)) begin
        view.range_hit = 1'b1;
    end else begin
        return report_query_miss("tlb_entry_exact_or_range",
                                 $sformatf("uid=%0d vpn=0x%0h asid=0x%0h vmid=0x%0h s2xlate=%0d",
                                           uid, request_vpn, view.request_key.asid,
                                           view.request_key.vmid, view.request_key.s2xlate));
    end
    data.derive_tlb_request_fields(entry, request_vpn, record.s2xlate, csr,
                                   view.request_translation_valid,
                                   view.request_s1_resolved_ppn,
                                   view.request_s2_resolved_ppn,
                                   view.request_gvpn);
    return 1'b1;
endfunction:resolve_tlb_entry_key_for_rm

function bit memblock_rm_readonly_api::read_uid_tlb_for_rm(
    input  memblock_uid_t uid,
    output uid_tlb_view_t view
);
    common_data_transaction data;
    view = '{default:'0};
    if (!try_get_common_data(data)) begin
        return report_query_miss("uid_tlb", "common data owner is not initialized");
    end
    if (uid >= data.main_trans_num || !data.uid_tlb_record_by_uid.exists(uid) ||
        data.uid_tlb_record_by_uid[uid] == null) begin
        return report_query_miss("uid_tlb", $sformatf("uid=%0d has no UID-TLB record", uid));
    end
    copy_uid_tlb_record(data.uid_tlb_record_by_uid[uid], view);
    return 1'b1;
endfunction:read_uid_tlb_for_rm

function bit memblock_rm_readonly_api::read_memory_map(
    input  bit              overlay,
    input  bit              report_miss,
    input  mem_addr_t       addr,
    input  mem_line_mask_t  byte_mask,
    output memory_read_view_t view
);
    mem_addr_t      byte_addr;
    mem_line_addr_t line_addr;
    bit [9:0]        byte_offset;
    bit              miss;
    bit              corrupt_hit;

    view = '{default:'0};
    if (!mem_access_base_sequence::is_shared_memory_lifecycle_initialized()) begin
        if (report_miss) begin
            return report_query_miss(overlay ? "committed_overlay" : "initialized_backing",
                                     "shared-memory lifecycle is not initialized");
        end
        return 1'b0;
    end
    miss = 1'b0;
    corrupt_hit = 1'b0;
    foreach (byte_mask[i]) begin
        if (!byte_mask[i]) begin
            continue;
        end
        if (mem_addr_t'(i) > 48'hffff_ffff_ffff - addr) begin
            miss = 1'b1;
            continue;
        end
        byte_addr   = addr + mem_addr_t'(i);
        line_addr   = byte_addr[47:10];
        byte_offset = byte_addr[9:0];
        if (!overlay) begin
            // 只探测已有 backing line；这里禁止调用 ensure_main_line，避免 RM 查询改变主表。
            if (!mem_access_base_sequence::main_mem.exists(line_addr)) begin
                miss = 1'b1;
                continue;
            end
            view.data[(i * 8) +: 8] =
                mem_access_base_sequence::main_mem[line_addr][(byte_offset * 8) +: 8];
            view.byte_valid[i] = 1'b1;
        end
        else begin
            if (mem_access_base_sequence::write_overlay_corrupt_byte_mask.exists(line_addr) &&
                mem_access_base_sequence::write_overlay_corrupt_byte_mask[line_addr][byte_offset]) begin
                corrupt_hit = 1'b1;
                view.corrupt_byte_mask[i] = 1'b1;
            end
            else if (mem_access_base_sequence::write_overlay_byte_valid.exists(line_addr) &&
                     mem_access_base_sequence::write_overlay_byte_valid[line_addr][byte_offset]) begin
                view.data[(i * 8) +: 8] =
                    mem_access_base_sequence::write_overlay_mem[line_addr][(byte_offset * 8) +: 8];
                view.byte_valid[i] = 1'b1;
            end
            else begin
                miss = 1'b1;
            end
        end
    end

    if (miss && !corrupt_hit) begin
        if (report_miss) begin
            return report_query_miss(overlay ? "committed_overlay" : "initialized_backing",
                                     $sformatf("addr=0x%0h byte_mask=0x%0h has an unavailable byte",
                                               addr, byte_mask));
        end
        return 1'b0;
    end
    view.valid      = 1'b1;
    view.corrupt    = corrupt_hit;
    view.data_valid = !corrupt_hit && !miss;
    return 1'b1;
endfunction:read_memory_map

function bit memblock_rm_readonly_api::read_initialized_backing_for_rm(
    input  mem_addr_t       addr,
    input  mem_line_mask_t  byte_mask,
    output memory_read_view_t view
);
    return read_memory_map(1'b0, 1'b1, addr, byte_mask, view);
endfunction:read_initialized_backing_for_rm

function bit memblock_rm_readonly_api::try_read_initialized_backing_for_rm(
    input  mem_addr_t       addr,
    input  mem_line_mask_t  byte_mask,
    output memory_read_view_t view
);
    return read_memory_map(1'b0, 1'b0, addr, byte_mask, view);
endfunction:try_read_initialized_backing_for_rm

function bit memblock_rm_readonly_api::read_committed_overlay_for_rm(
    input  mem_addr_t       addr,
    input  mem_line_mask_t  byte_mask,
    output memory_read_view_t view
);
    return read_memory_map(1'b1, 1'b1, addr, byte_mask, view);
endfunction:read_committed_overlay_for_rm

function bit memblock_rm_readonly_api::try_read_committed_overlay_for_rm(
    input  mem_addr_t       addr,
    input  mem_line_mask_t  byte_mask,
    output memory_read_view_t view
);
    return read_memory_map(1'b1, 1'b0, addr, byte_mask, view);
endfunction:try_read_committed_overlay_for_rm

function bit memblock_rm_readonly_api::get_dcache_overlay_readiness_for_rm(
    output dcache_overlay_readiness_view_t view
);
    mem_access_base_sequence::dcache_aggregate_snapshot_t snapshot;

    view = '{default:'0};
    if (mem_access_base_sequence::peek_dcache_aggregate_snapshot(snapshot)) begin
        view.valid = 1'b1;
        view.ready = snapshot.dcache_overlay_read_ready;
        return 1'b1;
    end
    // owner release invalidates the live aggregate in the same drain-complete
    // flow, while committed overlay bytes intentionally remain readable.
    if (memblock_sync_pkg::dcache_responder_done) begin
        view.valid = 1'b1;
        view.ready = 1'b1;
        return 1'b1;
    end
    return 1'b0;
endfunction:get_dcache_overlay_readiness_for_rm

function bit memblock_rm_readonly_api::read_l2_d_error_for_rm(
    input  mem_addr_t          line_addr,
    input  longint unsigned    sample,
    output l2_d_error_view_t   view
);
    mem_access_base_sequence::l2_d_error_line_record_t record;
    bit sticky_enabled;

    view = '{
        valid: 1'b0,
        sticky_enabled: 1'b0,
        denied: 1'b0,
        corrupt: 1'b0,
        state: mem_access_base_sequence::L2_D_ERROR_NONE,
        generation: 0,
        corrupt_activation_sample: 0,
        denied_activation_sample: 0,
        source_valid: 1'b0,
        source: '0
    };
    if (!mem_access_base_sequence::query_l2_d_error_at_sample(
            line_addr, sample, sticky_enabled, record)) begin
        return 1'b0;
    end
    view.valid          = 1'b1;
    view.sticky_enabled = sticky_enabled;
    view.state          = record.valid ? record.state :
                          mem_access_base_sequence::L2_D_ERROR_NONE;
    view.denied         = record.valid &&
                          record.state == mem_access_base_sequence::L2_D_ERROR_DENIED;
    view.corrupt        = record.valid &&
                          (record.state == mem_access_base_sequence::L2_D_ERROR_CORRUPT ||
                           record.state == mem_access_base_sequence::L2_D_ERROR_DENIED);
    view.generation                 = record.generation;
    view.corrupt_activation_sample  = record.corrupt_activation_sample;
    view.denied_activation_sample   = record.denied_activation_sample;
    view.source_valid               = record.source_valid;
    view.source                     = record.source;
    return 1'b1;
endfunction:read_l2_d_error_for_rm

function bit memblock_rm_readonly_api::read_pma_pmp_af_for_rm(
    input memblock_uid_t            uid,
    input int unsigned              dynamic_epoch,
    input bit                       translation_success,
    input bit [47:0]                paddr,
    input int unsigned              size_bytes,
    input pma_pmp_cmd_e             cmd,
    output pma_pmp_af_view_for_rm_t view
);
    common_data_transaction data;
    pma_pmp_eval_t          result;

    view = '{default:'0};
    // 中文注释：模型关闭时保持既有兼容语义；模型开启后 context 或 generation
    // 缺失必须报告 query miss，不能静默把访问当作允许。
    if (!seq_csr_common::get_pma_pmp_model_en()) begin
        view.valid = translation_success;
        view.translation_eligible = translation_success;
        view.af_decided = translation_success;
        return 1'b1;
    end
    if (!translation_success) begin
        // 翻译失败时 PMA/PMP 结果未定义，调用者不得把它解释为 fault 或 allow。
        return 1'b1;
    end
    if (!try_get_common_data(data) || data == null) begin
        return report_query_miss("pma_pmp", "common data owner is not initialized");
    end
    if (!data.evaluate_pma_pmp_for_uid_epoch(uid, dynamic_epoch,
                                             translation_success, paddr,
                                             size_bytes, cmd, result)) begin
        return report_query_miss(
            "pma_pmp",
            $sformatf("uid=%0d dynamic_epoch=%0d has no frozen PMA/PMP context or generation snapshot",
                      uid, dynamic_epoch));
    end
    data.pma_pmp_model.make_base_af_view(result, view);
    if (!view.valid || !view.translation_eligible) begin
        return report_query_miss(
            "pma_pmp",
            $sformatf("uid=%0d dynamic_epoch=%0d PMA/PMP result is not translation-eligible",
                      uid, dynamic_epoch));
    end
    return 1'b1;
endfunction:read_pma_pmp_af_for_rm

`endif
