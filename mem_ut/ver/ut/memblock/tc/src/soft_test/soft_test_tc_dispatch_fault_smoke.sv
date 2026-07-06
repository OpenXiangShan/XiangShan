//=========================================================
//File name    : soft_test_tc_dispatch_fault_smoke.sv
//Author       : OpenAI_Codex
//Module name  : soft_test_tc_dispatch_fault_smoke
//Discribution : software-only dispatch fault terminal_done smoke test
//Date         : 2026-06-28
//=========================================================
`ifndef SOFT_TEST_TC_DISPATCH_FAULT_SMOKE__SV
`define SOFT_TEST_TC_DISPATCH_FAULT_SMOKE__SV

class soft_test_tc_dispatch_fault_smoke extends soft_test_tc_dispatch_smoke;

    `uvm_component_utils(soft_test_tc_dispatch_fault_smoke)

    function new(string name = "soft_test_tc_dispatch_fault_smoke", uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

    virtual task run_dispatch_smoke_sequence();
        soft_test_memblock_dispatch_fault_smoke_sequence fault_smoke_seq;

        `uvm_info(get_type_name(), "Starting software dispatch fault smoke", UVM_MEDIUM)
        fault_smoke_seq = soft_test_memblock_dispatch_fault_smoke_sequence::type_id::create("fault_smoke_seq");
        if (fault_smoke_seq == null) begin
            `uvm_fatal(get_type_name(), "failed to create soft_test_memblock_dispatch_fault_smoke_sequence")
        end
        fault_smoke_seq.start(null);
        `uvm_info(get_type_name(), "soft_test_tc_dispatch_fault_smoke completed", UVM_MEDIUM)
    endtask:run_dispatch_smoke_sequence

endclass:soft_test_tc_dispatch_fault_smoke

class tc_dispatch_fault_smoke extends soft_test_tc_dispatch_fault_smoke;

    `uvm_component_utils(tc_dispatch_fault_smoke)

    function new(string name = "tc_dispatch_fault_smoke", uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

endclass:tc_dispatch_fault_smoke

`endif
