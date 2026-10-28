//=========================================================
//File name    : memblock_main_dispatch_pbmt_response_fault_sequence.sv
//Module name  : memblock_main_dispatch_pbmt_response_fault_sequence
//Discribution : real DUT PBMT response-fault directed main-table sequence
//=========================================================
`ifndef MEMBLOCK_MAIN_DISPATCH_PBMT_RESPONSE_FAULT_SEQUENCE__SV
`define MEMBLOCK_MAIN_DISPATCH_PBMT_RESPONSE_FAULT_SEQUENCE__SV

class memblock_main_dispatch_pbmt_response_fault_sequence extends
    memblock_main_dispatch_manual_main_table_sequence;

    localparam bit [63:0] PBMT_B_VADDR = 64'h0000_0000_8000_1000;
    localparam bit [63:0] PBMT_C_VADDR = 64'h0000_0000_8000_2000;

    memblock_tlb_lookup_key_t pbmt_rf_anchor_key;
    longint unsigned          pbmt_rf_anchor_generation;
    bit [1:0]                 pbmt_rf_anchor_pbmt;
    bit                       pbmt_rf_anchor_raw_s1_pf;
    bit                       pbmt_rf_anchor_raw_s1_af;
    bit                       pbmt_rf_anchor_raw_s2_gpf;
    bit                       pbmt_rf_anchor_raw_s2_gaf;
    bit                       pbmt_rf_anchor_effective_s1_pf;
    bit                       pbmt_rf_anchor_effective_s1_af;
    bit                       pbmt_rf_anchor_effective_s2_gpf;
    bit                       pbmt_rf_anchor_effective_s2_gaf;

    `uvm_object_utils(memblock_main_dispatch_pbmt_response_fault_sequence)

    extern function new(string name = "memblock_main_dispatch_pbmt_response_fault_sequence");
    extern virtual task body();
    extern virtual task build_directed_mixed_main_table();
    extern virtual function void validate_pbmt_rf_cfg_snapshot();
    extern virtual function void build_and_publish_pbmt_anchor_entry();
    extern virtual function bit check_pbmt_response_fault_results(
        input memblock_l2tlb_base_sequence l2tlb_seq);
endclass:memblock_main_dispatch_pbmt_response_fault_sequence

function memblock_main_dispatch_pbmt_response_fault_sequence::new(
    string name = "memblock_main_dispatch_pbmt_response_fault_sequence");
    super.new(name);
    pbmt_rf_anchor_key = '{default:'0};
    pbmt_rf_anchor_generation = 0;
endfunction:new

task memblock_main_dispatch_pbmt_response_fault_sequence::body();
    if (memblock_sync_pkg::get_control_worker_topology_mode() !=
        memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_DISABLED) begin
        `uvm_fatal(get_type_name(),
                   "PBMT response-fault main sequence requires disabled control topology")
        return;
    end
    ensure_dispatch_runtime_helpers();
    build_directed_mixed_main_table();
    check_main_table_control_policy(get_type_name());
    validate_pbmt_rf_cfg_snapshot();
    data.capture_pbmt_rf_pma_pmp_baseline();
    build_and_publish_pbmt_anchor_entry();
    if (!memblock_sync_pkg::publish_pbmt_rf_phase(
            memblock_sync_pkg::PBMT_RF_RESET,
            memblock_sync_pkg::PBMT_RF_ANCHOR_PUBLISHED, this)) return;
    service_real_dispatch_flow();
    if (memblock_sync_pkg::get_pbmt_rf_phase() != memblock_sync_pkg::PBMT_RF_C_COMPLETED ||
        !data.get_status(1).terminal_done) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("PBMT RF main stopped before C terminal phase=%0d terminal=%0d",
                             memblock_sync_pkg::get_pbmt_rf_phase(),
                             data.get_status(1).terminal_done))
        return;
    end
    void'(memblock_sync_pkg::publish_pbmt_rf_phase(
        memblock_sync_pkg::PBMT_RF_C_COMPLETED,
        memblock_sync_pkg::PBMT_RF_C_TERMINAL, this));
endtask:body

task memblock_main_dispatch_pbmt_response_fault_sequence::build_directed_mixed_main_table();
    clear_manual_main_table();
    set_manual_main_transaction(0, make_directed_transaction(
        "pbmt_disabled_store_b", MEMBLOCK_OP_CLASS_STORE, 0, PBMT_B_VADDR));
    set_manual_main_transaction(1, make_directed_transaction(
        "pbmt_enabled_load_c", MEMBLOCK_OP_CLASS_INT_LOAD, 1, PBMT_C_VADDR));
    import_manual_main_table();
endtask:build_directed_mixed_main_table

function void memblock_main_dispatch_pbmt_response_fault_sequence::validate_pbmt_rf_cfg_snapshot();
    if (seq_csr_common::get_main_trans_num() != 2 ||
        seq_csr_common::get_mmu_sv39_m_pbmte_en() ||
        seq_csr_common::get_mmu_sv39_h_pbmte_en() ||
        !seq_csr_common::get_l2tlb_level_weight_en() ||
        seq_csr_common::get_l2tlb_level_wt(1'b1, 0) != 0 ||
        seq_csr_common::get_l2tlb_level_wt(1'b1, 1) != 100 ||
        seq_csr_common::get_l2tlb_level_wt(1'b1, 2) != 0 ||
        seq_csr_common::get_l2tlb_level_wt(1'b1, 3) != 0 ||
        seq_csr_common::get_l2tlb_pbmt_wt(1'b1, 0) != 0 ||
        seq_csr_common::get_l2tlb_pbmt_wt(1'b1, 1) != 100 ||
        seq_csr_common::get_l2tlb_pbmt_wt(1'b1, 2) != 0 ||
        seq_csr_common::get_l2tlb_fault_1_wt(1'b1, 0) != 0 ||
        seq_csr_common::get_l2tlb_fault_1_wt(1'b1, 1) != 0 ||
        seq_csr_common::get_l2tlb_fault_1_wt(1'b0, 0) != 0 ||
        seq_csr_common::get_l2tlb_fault_1_wt(1'b0, 1) != 0 ||
        !seq_csr_common::get_pma_pmp_model_en() ||
        seq_csr_common::get_main_mem_ranges_en() ||
        seq_csr_common::get_l2tlb_max_outstanding() != 1 ||
        seq_csr_common::get_l2tlb_resp_reorder_en() ||
        seq_csr_common::get_l2tlb_resp_1c_wt() != 100 ||
        seq_csr_common::get_l2tlb_resp_mid_wt() != 0 ||
        seq_csr_common::get_l2tlb_resp_long_wt() != 0) begin
        `uvm_fatal(get_type_name(), "PBMT RF cfg snapshot does not match directed contract")
    end
endfunction:validate_pbmt_rf_cfg_snapshot

function void memblock_main_dispatch_pbmt_response_fault_sequence::build_and_publish_pbmt_anchor_entry();
    mmu_csr_runtime_state csr;
    memblock_tlb_entry entry;
    bit [63:0] paddr_base;

    csr = mmu_csr_runtime_state::type_id::create("pbmt_rf_anchor_csr");
    csr.reset();
    paddr_base = seq_csr_common::get_paddr_base();
    csr.satp_mode = 4'd8;
    csr.satp_asid = '0;
    csr.satp_ppn = paddr_base[55:12];
    csr.priv_imode = 2'd0;
    csr.priv_dmode = 2'd0;
    csr.priv_virt = 1'b0;
    csr.m_pbmt_en = 1'b1;
    csr.h_pbmt_en = 1'b0;
    pbmt_rf_anchor_key = csr.make_lookup_key(
        {26'b0, PBMT_B_VADDR[49:12]}, 2'd0);
    entry = data.build_tlb_entry_for_key_with_csr(pbmt_rf_anchor_key, csr);
    if (entry == null || entry.s1_level != 2'd1 || entry.s1_entry_pbmt != 2'd1 ||
        entry.fault_raw_s1_pf || entry.fault_raw_s1_af ||
        entry.fault_raw_s2_gpf || entry.fault_raw_s2_gaf ||
        entry.has_effective_fault() || entry.pmaAF) begin
        `uvm_fatal(get_type_name(), "PBMT RF canonical anchor builder contract failed")
        return;
    end
    data.publish_live_tlb_entry(pbmt_rf_anchor_key, entry, csr);
    pbmt_rf_anchor_generation = entry.entry_generation;
    pbmt_rf_anchor_pbmt = entry.s1_entry_pbmt;
    pbmt_rf_anchor_raw_s1_pf = entry.fault_raw_s1_pf;
    pbmt_rf_anchor_raw_s1_af = entry.fault_raw_s1_af;
    pbmt_rf_anchor_raw_s2_gpf = entry.fault_raw_s2_gpf;
    pbmt_rf_anchor_raw_s2_gaf = entry.fault_raw_s2_gaf;
    pbmt_rf_anchor_effective_s1_pf = entry.fault_effective_s1_pf;
    pbmt_rf_anchor_effective_s1_af = entry.fault_effective_s1_af;
    pbmt_rf_anchor_effective_s2_gpf = entry.fault_effective_s2_gpf;
    pbmt_rf_anchor_effective_s2_gaf = entry.fault_effective_s2_gaf;
endfunction:build_and_publish_pbmt_anchor_entry

function bit memblock_main_dispatch_pbmt_response_fault_sequence::check_pbmt_response_fault_results(
    input memblock_l2tlb_base_sequence l2tlb_seq);
    memblock_pbmt_rf_audit_slot_t b;
    memblock_pbmt_rf_audit_slot_t c;
    memblock_tlb_entry raw_entry;
    status_transaction b_status;
    status_transaction c_status;

    if (l2tlb_seq == null ||
        memblock_sync_pkg::get_pbmt_rf_phase() != memblock_sync_pkg::PBMT_RF_AUDIT_READY) begin
        `uvm_fatal(get_type_name(), "PBMT RF final checker called outside AUDIT_READY")
        return 1'b0;
    end
    if (!l2tlb_seq.get_pbmt_rf_audit_slot(PBMT_RF_SLOT_B, b) ||
        !l2tlb_seq.get_pbmt_rf_audit_slot(PBMT_RF_SLOT_C, c)) begin
        `uvm_fatal(get_type_name(), "PBMT RF final checker missing audit slot")
        return 1'b0;
    end
    if (!b.complete_valid || b.canceled || !c.complete_valid || c.canceled ||
        b.uid != 0 || c.uid != 1 || b.requestor != PBMT_RF_REQ_STORE ||
        c.requestor != PBMT_RF_REQ_LOAD || b.request_token == c.request_token ||
        b.request_token != 0 || c.request_token != 1) begin
        `uvm_fatal(get_type_name(), "PBMT RF slot identity/lifecycle mismatch")
        return 1'b0;
    end
    if (l2tlb_seq.get_l2tlb_accepted_count() != 2 ||
        l2tlb_seq.get_l2tlb_completed_count() != 2 ||
        l2tlb_seq.get_l2tlb_canceled_count() != 0 ||
        l2tlb_seq.outstanding_count() != 0) begin
        `uvm_fatal(get_type_name(), "PBMT RF lifecycle counters are not 2/2/0/0")
        return 1'b0;
    end
    if (b.request_vpn != PBMT_B_VADDR[49:12] ||
        c.request_vpn != PBMT_C_VADDR[49:12] ||
        b.lookup_kind != MEMBLOCK_TLB_LOOKUP_EXACT_HIT ||
        c.lookup_kind != MEMBLOCK_TLB_LOOKUP_RANGE_HIT ||
        b.anchor_key != pbmt_rf_anchor_key || c.anchor_key != pbmt_rf_anchor_key ||
        b.entry_generation != pbmt_rf_anchor_generation ||
        c.entry_generation != pbmt_rf_anchor_generation) begin
        `uvm_fatal(get_type_name(), "PBMT RF canonical lookup provenance mismatch")
        return 1'b0;
    end
    if (b.accept_dut_sample_seq < memblock_sync_pkg::get_pbmt_rf_b_ready_sample() ||
        b.accept_csr_history_sample_seq == 0 || b.complete_csr_history_sample_seq == 0 ||
        b.accept_m_pbmt_en || b.accept_h_pbmt_en ||
        b.complete_m_pbmt_en || b.complete_h_pbmt_en ||
        b.raw_s1_pf || b.raw_s1_af || b.raw_s2_gpf || b.raw_s2_gaf ||
        !b.effective_s1_pf || b.effective_s1_af || b.effective_s2_gpf ||
        b.effective_s2_gaf || b.pma_af || !b.forced_s1_pf || b.forced_s2_gpf) begin
        `uvm_fatal(get_type_name(), "PBMT RF B fault/CSR audit mismatch")
        return 1'b0;
    end
    if (c.accept_dut_sample_seq <= memblock_sync_pkg::get_pbmt_rf_c_ready_sample() ||
        c.accept_csr_history_sample_seq == 0 || c.complete_csr_history_sample_seq == 0 ||
        !c.accept_m_pbmt_en || c.accept_h_pbmt_en ||
        !c.complete_m_pbmt_en || c.complete_h_pbmt_en ||
        c.raw_s1_pf || c.raw_s1_af || c.raw_s2_gpf || c.raw_s2_gaf ||
        c.effective_s1_pf || c.effective_s1_af || c.effective_s2_gpf ||
        c.effective_s2_gaf || c.pma_af || c.forced_s1_pf || c.forced_s2_gpf) begin
        `uvm_fatal(get_type_name(), "PBMT RF C fault/CSR audit mismatch")
        return 1'b0;
    end
    b_status = data.get_status(0);
    c_status = data.get_status(1);
    if (b_status.exception_vec != 24'h008000 ||
        c_status.exception_vec != 24'h000000) begin
        `uvm_fatal(get_type_name(), "PBMT RF DUT exception mismatch")
        return 1'b0;
    end
    raw_entry = data.get_tlb_entry(pbmt_rf_anchor_key);
    if (raw_entry.entry_generation != pbmt_rf_anchor_generation ||
        raw_entry.s1_entry_pbmt != pbmt_rf_anchor_pbmt ||
        raw_entry.fault_raw_s1_pf != pbmt_rf_anchor_raw_s1_pf ||
        raw_entry.fault_raw_s1_af != pbmt_rf_anchor_raw_s1_af ||
        raw_entry.fault_raw_s2_gpf != pbmt_rf_anchor_raw_s2_gpf ||
        raw_entry.fault_raw_s2_gaf != pbmt_rf_anchor_raw_s2_gaf ||
        raw_entry.fault_effective_s1_pf != pbmt_rf_anchor_effective_s1_pf ||
        raw_entry.fault_effective_s1_af != pbmt_rf_anchor_effective_s1_af ||
        raw_entry.fault_effective_s2_gpf != pbmt_rf_anchor_effective_s2_gpf ||
        raw_entry.fault_effective_s2_gaf != pbmt_rf_anchor_effective_s2_gaf) begin
        `uvm_fatal(get_type_name(), "PBMT RF canonical raw entry changed")
        return 1'b0;
    end
    return 1'b1;
endfunction:check_pbmt_response_fault_results

`endif
