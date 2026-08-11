//=========================================================
//File name    : soft_test_tc_l2tlb_range_lookup.sv
//Module name  : soft_test_tc_l2tlb_range_lookup
//Discribution : software-only V2 L2TLB range lookup test
//=========================================================
`ifndef SOFT_TEST_TC_L2TLB_RANGE_LOOKUP__SV
`define SOFT_TEST_TC_L2TLB_RANGE_LOOKUP__SV

class soft_test_tc_l2tlb_range_lookup extends soft_test_tc_dispatch_smoke;

    `uvm_component_utils(soft_test_tc_l2tlb_range_lookup)

    function new(string name = "soft_test_tc_l2tlb_range_lookup",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

    virtual task run_dispatch_smoke_sequence();
        soft_test_l2tlb_range_lookup_sequence range_lookup_seq;

        range_lookup_seq = soft_test_l2tlb_range_lookup_sequence::type_id::create(
            "range_lookup_seq");
        if (range_lookup_seq == null) begin
            `uvm_fatal(get_type_name(),
                       "failed to create soft_test_l2tlb_range_lookup_sequence")
        end
        range_lookup_seq.start(null);
    endtask:run_dispatch_smoke_sequence

endclass:soft_test_tc_l2tlb_range_lookup

class tc_l2tlb_range_lookup_smoke extends soft_test_tc_l2tlb_range_lookup;

    `uvm_component_utils(tc_l2tlb_range_lookup_smoke)

    function new(string name = "tc_l2tlb_range_lookup_smoke",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

endclass:tc_l2tlb_range_lookup_smoke

`endif
