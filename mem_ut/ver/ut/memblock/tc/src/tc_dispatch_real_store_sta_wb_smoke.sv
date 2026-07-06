//=========================================================
//File name    : tc_dispatch_real_store_sta_wb_smoke.sv
//Author       : OpenAI_Codex
//Module name  : tc_dispatch_real_store_sta_wb_smoke
//Discribution : real DUT dispatch scalar store STA/STD writeback smoke test
//Date         : 2026-05-19
//=========================================================
`ifndef TC_DISPATCH_REAL_STORE_STA_WB_SMOKE__SV
`define TC_DISPATCH_REAL_STORE_STA_WB_SMOKE__SV

class tc_dispatch_real_store_sta_wb_smoke extends tc_dispatch_real_store_wb_smoke;

    `uvm_component_utils(tc_dispatch_real_store_sta_wb_smoke)

    function new(string name = "tc_dispatch_real_store_sta_wb_smoke", uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

endclass:tc_dispatch_real_store_sta_wb_smoke

`endif
