//=========================================================
//File name    : lsqcommit_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : lsqcommit_agent_agent_interface
//Discribution : lsqcommit_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef LSQCOMMIT_AGENT_AGENT_INTERFACE__SV
`define LSQCOMMIT_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface lsqcommit_agent_agent_interface  (input bit clk,input bit rst_n);

    logic io_ooo_to_mem_lsqio_pendingPtr_flag;
    logic [8:0] io_ooo_to_mem_lsqio_pendingPtr_value;
    logic io_ooo_to_mem_flushSb        ;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        output io_ooo_to_mem_lsqio_pendingPtr_flag;
        output io_ooo_to_mem_lsqio_pendingPtr_value;
        output io_ooo_to_mem_flushSb;

    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_ooo_to_mem_lsqio_pendingPtr_flag;
        input  io_ooo_to_mem_lsqio_pendingPtr_value;
        input  io_ooo_to_mem_flushSb;

    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:lsqcommit_agent_agent_interface

`endif
