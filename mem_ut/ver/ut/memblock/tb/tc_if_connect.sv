//=========================================================
//File name    : tc_if_connect.sv
//Author       : OpenAI_Codex
//Module name  : tc_if_connect
//Discribution : tc_if_connect : tc virtual connection for force/probe
//Date         : 2026-04-12
//=========================================================
`ifndef TC_IF_CONNECT__SV
`define TC_IF_CONNECT__SV

tc_if tc_if(clk);
initial begin
    uvm_config_db#(virtual tc_if)::set(null, "uvm_test_top", "vif", tc_if);
    uvm_config_db#(virtual tc_if)::set(null, "uvm_test_top*.rm", "vif", tc_if);
end

`endif
