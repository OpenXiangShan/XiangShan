//=========================================================
//File name    : int_sink_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : int_sink_agent_agent_interface
//Discribution : int_sink_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef INT_SINK_AGENT_AGENT_INTERFACE__SV
`define INT_SINK_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface int_sink_agent_agent_interface  (input bit clk,input bit rst_n);

    logic auto_inner_beu_local_int_sink_in_0;
    logic auto_inner_nmi_int_sink_in_0 ;
    logic auto_inner_nmi_int_sink_in_1 ;
    logic auto_inner_plic_int_sink_in_1_0;
    logic auto_inner_plic_int_sink_in_0_0;
    logic auto_inner_clint_int_sink_in_0;
    logic auto_inner_clint_int_sink_in_1;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        output auto_inner_beu_local_int_sink_in_0;
        output auto_inner_nmi_int_sink_in_0;
        output auto_inner_nmi_int_sink_in_1;
        output auto_inner_plic_int_sink_in_1_0;
        output auto_inner_plic_int_sink_in_0_0;
        output auto_inner_clint_int_sink_in_0;
        output auto_inner_clint_int_sink_in_1;

    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  auto_inner_beu_local_int_sink_in_0;
        input  auto_inner_nmi_int_sink_in_0;
        input  auto_inner_nmi_int_sink_in_1;
        input  auto_inner_plic_int_sink_in_1_0;
        input  auto_inner_plic_int_sink_in_0_0;
        input  auto_inner_clint_int_sink_in_0;
        input  auto_inner_clint_int_sink_in_1;

    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:int_sink_agent_agent_interface

`endif
