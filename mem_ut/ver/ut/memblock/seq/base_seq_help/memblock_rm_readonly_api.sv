//=========================================================
//File name    : memblock_rm_readonly_api.sv
//Author       : OpenAI_Codex
//Module name  : memblock_rm_readonly_api
//Discribution : Read-only value views for a future reference model
//Date         : 2026-08-11
//=========================================================
`ifndef MEMBLOCK_RM_READONLY_API__SV
`define MEMBLOCK_RM_READONLY_API__SV

// 抽象职责：该 class 是测试框架状态到后续 RM 的唯一只读 façade。
// 它只探测已经存在的 owner、表项和 memory map，并把结果复制成值型 view；
// 不创建 singleton/table entry，不触发懒分配，也不返回任何内部 object/queue handle。
class memblock_rm_readonly_api extends uvm_object;

    typedef mem_access_base_sequence::mem_addr_t      mem_addr_t;
    typedef mem_access_base_sequence::mem_line_addr_t mem_line_addr_t;
    typedef mem_access_base_sequence::mem_line_data_t mem_line_data_t;
    typedef mem_access_base_sequence::mem_line_mask_t mem_line_mask_t;

    typedef struct packed {
        bit                  valid;
        memblock_uid_t       uid;
        int unsigned         op_class;
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
        bit                  active_lq_mapped;
        bit                  active_sq_mapped;
        int unsigned         dynamic_epoch;
        int unsigned         replay_seq;
        bit                  issue_killed;
        bit                  mmio_tag_valid;
        bit                  is_mmio_load;
        bit                  is_mmio_store;
        int unsigned         mmio_tag_dynamic_epoch;
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
        longint unsigned     entry_generation;
        bit                  s1_stage_active;
        bit                  s2_stage_active;
        bit                  s1_resolved_ppn_valid;
        bit [43:0]           s1_resolved_ppn;
        bit                  s2_resolved_ppn_valid;
        bit [43:0]           s2_resolved_ppn;
        bit                  fault;
        bit                  pma_af;
        longint unsigned     create_cycle;
        longint unsigned     last_hit_cycle;
    } tlb_entry_view_t;

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

    static memblock_rm_readonly_api m_inst;

    `uvm_object_utils(memblock_rm_readonly_api)

    extern function new(string name = "memblock_rm_readonly_api");
    extern static function memblock_rm_readonly_api get();
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
    extern function bit read_uid_tlb_for_rm(
        input  memblock_uid_t uid,
        output uid_tlb_view_t view
    );
    extern function bit read_initialized_backing_for_rm(
        input  mem_addr_t      addr,
        input  mem_line_mask_t byte_mask,
        output memory_read_view_t view
    );
    extern function bit read_committed_overlay_for_rm(
        input  mem_addr_t      addr,
        input  mem_line_mask_t byte_mask,
        output memory_read_view_t view
    );
    extern function bit get_dcache_overlay_readiness_for_rm(
        output dcache_overlay_readiness_view_t view
    );

    extern function bit try_get_common_data(output common_data_transaction data);
    extern function bit report_query_miss(input string query_name, input string detail);
    extern function bit read_memory_map(
        input  bit              overlay,
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
    return m_inst;
endfunction:get

function bit memblock_rm_readonly_api::try_get_common_data(output common_data_transaction data);
    data = common_data_transaction::m_inst;
    return data != null;
endfunction:try_get_common_data

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
    view.active_lq_mapped   = source.active_lq_mapped;
    view.active_sq_mapped   = source.active_sq_mapped;
    view.dynamic_epoch      = source.dynamic_epoch;
    view.replay_seq         = source.replay_seq;
    view.issue_killed       = source.issue_killed;
    view.mmio_tag_valid     = source.mmio_tag_valid;
    view.is_mmio_load       = source.is_mmio_load;
    view.is_mmio_store      = source.is_mmio_store;
    view.mmio_tag_dynamic_epoch = source.mmio_tag_dynamic_epoch;
    view.last_event_cycle   = source.last_event_cycle;
endfunction:copy_status

function void memblock_rm_readonly_api::copy_tlb_entry(
    input  memblock_tlb_entry source,
    output tlb_entry_view_t view
);
    view = '{default:'0};
    view.valid                  = 1'b1;
    view.lookup_key             = source.lookup_key;
    view.entry_generation       = source.entry_generation;
    view.s1_stage_active        = source.s1_stage_active;
    view.s2_stage_active        = source.s2_stage_active;
    view.s1_resolved_ppn_valid  = source.s1_resolved_ppn_valid;
    view.s1_resolved_ppn        = source.s1_resolved_ppn;
    view.s2_resolved_ppn_valid  = source.s2_resolved_ppn_valid;
    view.s2_resolved_ppn        = source.s2_resolved_ppn;
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
        return report_query_miss(overlay ? "committed_overlay" : "initialized_backing",
                                 $sformatf("addr=0x%0h byte_mask=0x%0h has an unavailable byte",
                                           addr, byte_mask));
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
    return read_memory_map(1'b0, addr, byte_mask, view);
endfunction:read_initialized_backing_for_rm

function bit memblock_rm_readonly_api::read_committed_overlay_for_rm(
    input  mem_addr_t       addr,
    input  mem_line_mask_t  byte_mask,
    output memory_read_view_t view
);
    return read_memory_map(1'b1, addr, byte_mask, view);
endfunction:read_committed_overlay_for_rm

function bit memblock_rm_readonly_api::get_dcache_overlay_readiness_for_rm(
    output dcache_overlay_readiness_view_t view
);
    mem_access_base_sequence::dcache_aggregate_snapshot_t snapshot;

    view = '{default:'0};
    if (!mem_access_base_sequence::peek_dcache_aggregate_snapshot(snapshot)) begin
        return report_query_miss("dcache_overlay_readiness",
                                 "DCache owner or observer snapshot is not published");
    end
    view.valid = 1'b1;
    view.ready = snapshot.dcache_overlay_read_ready;
    return 1'b1;
endfunction:get_dcache_overlay_readiness_for_rm

`endif
