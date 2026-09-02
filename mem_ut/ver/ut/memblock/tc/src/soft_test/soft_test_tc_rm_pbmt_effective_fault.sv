//=========================================================
//File name    : soft_test_tc_rm_pbmt_effective_fault.sv
//Module name  : soft_test_tc_rm_pbmt_effective_fault
//Discribution : software-only RM PBMT/PBMTE effective-fault test
//=========================================================
`ifndef SOFT_TEST_TC_RM_PBMT_EFFECTIVE_FAULT__SV
`define SOFT_TEST_TC_RM_PBMT_EFFECTIVE_FAULT__SV

class soft_test_tc_rm_pbmt_effective_fault extends soft_test_tc_dispatch_smoke;

    `uvm_component_utils(soft_test_tc_rm_pbmt_effective_fault)

    function new(string name = "soft_test_tc_rm_pbmt_effective_fault",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

    virtual task run_dispatch_smoke_sequence();
        soft_test_rm_pbmt_effective_fault_sequence pbmt_seq;
        memblock_rm_readonly_api::tlb_entry_view_t entry;
        memblock_rm_readonly_api::tlb_request_context_view_t tlb_context;
        bit force_s1_pf;
        bit force_s2_gpf;

        pbmt_seq = soft_test_rm_pbmt_effective_fault_sequence::type_id::create(
            "rm_pbmt_effective_fault_seq");
        if (pbmt_seq == null) begin
            `uvm_fatal(get_type_name(),
                       "failed to create soft_test_rm_pbmt_effective_fault_sequence")
        end
        pbmt_seq.start(null);

        // Call the actual RM component wrapper once as a smoke guard; the
        // sequence above exercises the same pure readonly implementation.
        if (env == null || env.rm == null) begin
            `uvm_fatal(get_type_name(), "RM component is unavailable for PBMT helper smoke")
        end
        entry = '{default:'0};
        tlb_context = '{default:'0};
        entry.valid = 1'b1;
        entry.s2xlate = 2'd0;
        entry.s1_stage_active = 1'b1;
        entry.s1_entry_pbmt = 2'd1;
        tlb_context.valid = 1'b1;
        tlb_context.s2xlate = 2'd0;
        tlb_context.m_pbmt_en = 1'b0;
        tlb_context.h_pbmt_en = 1'b1;
        if (!env.rm.observer_eval_pbmt_fault_overlay(
                entry, tlb_context, 1'b1, 1'b0,
                force_s1_pf, force_s2_gpf) ||
            !force_s1_pf || force_s2_gpf) begin
            `uvm_fatal(get_type_name(),
                       "RM component did not apply disabled mPBMTE S1 PF overlay")
        end
    endtask:run_dispatch_smoke_sequence

endclass:soft_test_tc_rm_pbmt_effective_fault

class tc_rm_pbmt_effective_fault extends soft_test_tc_rm_pbmt_effective_fault;

    `uvm_component_utils(tc_rm_pbmt_effective_fault)

    function new(string name = "tc_rm_pbmt_effective_fault",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

endclass:tc_rm_pbmt_effective_fault

`endif
