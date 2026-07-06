//=========================================================
//File name    : tc_dispatch_real_mixed_wb_smoke.sv
//Author       : OpenAI_Codex
//Module name  : tc_dispatch_real_mixed_wb_smoke
//Discribution : real DUT dispatch load/store writeback smoke test
//Date         : 2026-05-19
//=========================================================
`ifndef TC_DISPATCH_REAL_MIXED_WB_SMOKE__SV
`define TC_DISPATCH_REAL_MIXED_WB_SMOKE__SV

class tc_dispatch_real_mixed_wb_smoke extends tc_dispatch_real_store_wb_smoke;

    `uvm_component_utils(tc_dispatch_real_mixed_wb_smoke)

    function new(string name = "tc_dispatch_real_mixed_wb_smoke", uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

    virtual task run_real_smoke_sequence();
        memblock_main_dispatch_manual_main_table_sequence mixed_smoke_seq;

        `uvm_info(get_type_name(), "Starting real mixed dispatch smoke", UVM_MEDIUM)
        mixed_smoke_seq = memblock_main_dispatch_manual_main_table_sequence::type_id::create("mixed_smoke_seq");
        if (mixed_smoke_seq == null) begin
            `uvm_fatal(get_type_name(), "failed to create memblock_main_dispatch_manual_main_table_sequence")
        end
        mixed_smoke_seq.start(null);
        `uvm_info(get_type_name(), "tc_dispatch_real_mixed_wb_smoke completed", UVM_MEDIUM)
    endtask:run_real_smoke_sequence

endclass:tc_dispatch_real_mixed_wb_smoke

`endif
