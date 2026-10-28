//=========================================================
//File name    : memblock_l2tlb_pbmt_response_fault_vseq.sv
//Module name  : memblock_l2tlb_pbmt_response_fault_vseq
//Discribution : real DUT PBMT dynamic response-fault virtual sequence
//=========================================================
`ifndef MEMBLOCK_L2TLB_PBMT_RESPONSE_FAULT_VSEQ__SV
`define MEMBLOCK_L2TLB_PBMT_RESPONSE_FAULT_VSEQ__SV

class memblock_l2tlb_pbmt_response_fault_vseq extends memblock_dispatch_real_smoke_vseq;
    `uvm_object_utils(memblock_l2tlb_pbmt_response_fault_vseq)
    extern function new(string name = "memblock_l2tlb_pbmt_response_fault_vseq");
    extern virtual task start_core_dispatch_flow();
endclass:memblock_l2tlb_pbmt_response_fault_vseq

function memblock_l2tlb_pbmt_response_fault_vseq::new(
    string name = "memblock_l2tlb_pbmt_response_fault_vseq");
    super.new(name);
endfunction:new

task memblock_l2tlb_pbmt_response_fault_vseq::start_core_dispatch_flow();
    memblock_lsqenq_dispatch_base_sequence              lsqenq_seq;
    memblock_issue_dispatch_base_sequence               issue_seq;
    memblock_lsqcommit_dispatch_base_sequence           lsqcommit_seq;
    memblock_l2tlb_base_sequence                        l2tlb_seq;
    memblock_main_dispatch_pbmt_response_fault_sequence main_seq;
    memblock_l2tlb_pbmt_toggle_csr_sequence             csr_seq;

    if (memblock_sync_pkg::get_control_worker_topology_mode() !=
        memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_DISABLED) begin
        `uvm_fatal(get_type_name(), "PBMT RF VSEQ requires disabled control topology")
        return;
    end
    lsqenq_seq = memblock_lsqenq_dispatch_base_sequence::type_id::create("lsqenq_seq");
    issue_seq = memblock_issue_dispatch_base_sequence::type_id::create("issue_seq");
    lsqcommit_seq = memblock_lsqcommit_dispatch_base_sequence::type_id::create("lsqcommit_seq");
    l2tlb_seq = memblock_l2tlb_base_sequence::type_id::create("l2tlb_seq");
    main_seq = memblock_main_dispatch_pbmt_response_fault_sequence::type_id::create("main_seq");
    csr_seq = memblock_l2tlb_pbmt_toggle_csr_sequence::type_id::create("csr_seq");
    if (lsqenq_seq == null || issue_seq == null || lsqcommit_seq == null ||
        l2tlb_seq == null || main_seq == null || csr_seq == null) begin
        `uvm_fatal(get_type_name(), "PBMT RF child sequence allocation failed")
        return;
    end

    if (!memblock_sync_pkg::reset_pbmt_rf_shared_state(this)) return;
    l2tlb_seq.reset_pbmt_rf_audit_ledger();
    if (!memblock_sync_pkg::activate_pbmt_rf_scenario(
            this, main_seq, l2tlb_seq, csr_seq)) return;

    fork
        lsqenq_seq.start(p_sequencer.lsqenq_sqr);
        begin
            wait_for_explicit_l2tlb_start_barrier();
            issue_seq.start(p_sequencer.lintsissue_sqr);
        end
        lsqcommit_seq.start(p_sequencer.lsqcommit_sqr);
        begin
            wait_for_explicit_l2tlb_start_barrier();
            l2tlb_seq.start(p_sequencer.L2tlb_sqr);
        end
        main_seq.start(p_sequencer);
        csr_seq.start(p_sequencer.csr_ctrl_sqr);
    join

    if (memblock_sync_pkg::get_pbmt_rf_phase() != memblock_sync_pkg::PBMT_RF_C_TERMINAL) begin
        void'(memblock_sync_pkg::release_pbmt_rf_csr_level_hold(this));
        `uvm_fatal(get_type_name(),
                   $sformatf("PBMT RF children joined at phase=%0d",
                             memblock_sync_pkg::get_pbmt_rf_phase()))
        return;
    end
    if (!memblock_sync_pkg::publish_pbmt_rf_phase(
            memblock_sync_pkg::PBMT_RF_C_TERMINAL,
            memblock_sync_pkg::PBMT_RF_AUDIT_READY, this)) return;
    if (!main_seq.check_pbmt_response_fault_results(l2tlb_seq)) begin
        void'(memblock_sync_pkg::release_pbmt_rf_csr_level_hold(this));
        return;
    end
    if (!l2tlb_seq.check_l2tlb_lifecycle_accounting(
            "pbmt_response_fault_final")) begin
        void'(memblock_sync_pkg::release_pbmt_rf_csr_level_hold(this));
        return;
    end
    if (!memblock_sync_pkg::publish_pbmt_rf_phase(
            memblock_sync_pkg::PBMT_RF_AUDIT_READY,
            memblock_sync_pkg::PBMT_RF_AUDIT_PASSED, this)) return;
    l2tlb_seq.cleanup_pbmt_rf_audit_ledger_after_final_read();
    void'(memblock_sync_pkg::cleanup_pbmt_rf_shared_state(this));
endtask:start_core_dispatch_flow

`endif
