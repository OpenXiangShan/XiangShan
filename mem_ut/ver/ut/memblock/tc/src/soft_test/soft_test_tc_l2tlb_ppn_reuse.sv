//=========================================================
//File name    : soft_test_tc_l2tlb_ppn_reuse.sv
//Module name  : soft_test_tc_l2tlb_ppn_reuse
//Discribution : software-only V2 L2TLB PPN reuse test
//=========================================================
`ifndef SOFT_TEST_TC_L2TLB_PPN_REUSE__SV
`define SOFT_TEST_TC_L2TLB_PPN_REUSE__SV

class soft_test_tc_l2tlb_ppn_reuse extends soft_test_tc_dispatch_smoke;

    `uvm_component_utils(soft_test_tc_l2tlb_ppn_reuse)

    function new(string name = "soft_test_tc_l2tlb_ppn_reuse",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

    virtual task run_dispatch_smoke_sequence();
        soft_test_l2tlb_ppn_reuse_sequence ppn_reuse_seq;

        ppn_reuse_seq = soft_test_l2tlb_ppn_reuse_sequence::type_id::create(
            "ppn_reuse_seq");
        if (ppn_reuse_seq == null) begin
            `uvm_fatal(get_type_name(),
                       "failed to create soft_test_l2tlb_ppn_reuse_sequence")
        end
        ppn_reuse_seq.start(null);
    endtask:run_dispatch_smoke_sequence

endclass:soft_test_tc_l2tlb_ppn_reuse

class tc_l2tlb_ppn_reuse_smoke extends soft_test_tc_l2tlb_ppn_reuse;

    `uvm_component_utils(tc_l2tlb_ppn_reuse_smoke)

    function new(string name = "tc_l2tlb_ppn_reuse_smoke",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

endclass:tc_l2tlb_ppn_reuse_smoke

`endif
