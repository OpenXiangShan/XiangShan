//=========================================================
//File name    : fence_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : fence_agent_agent_interface
//Discribution : fence_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef FENCE_AGENT_AGENT_INTERFACE__SV
`define FENCE_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface fence_agent_agent_interface  (input bit clk,input bit rst_n);

    logic io_ooo_to_mem_sfence_valid   ;
    logic io_ooo_to_mem_sfence_bits_rs1;
    logic io_ooo_to_mem_sfence_bits_rs2;
    logic [49:0] io_ooo_to_mem_sfence_bits_addr;
    logic [15:0] io_ooo_to_mem_sfence_bits_id;
    logic io_ooo_to_mem_sfence_bits_hv ;
    logic io_ooo_to_mem_sfence_bits_hg ;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        output io_ooo_to_mem_sfence_valid;
        output io_ooo_to_mem_sfence_bits_rs1;
        output io_ooo_to_mem_sfence_bits_rs2;
        output io_ooo_to_mem_sfence_bits_addr;
        output io_ooo_to_mem_sfence_bits_id;
        output io_ooo_to_mem_sfence_bits_hv;
        output io_ooo_to_mem_sfence_bits_hg;

    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_ooo_to_mem_sfence_valid;
        input  io_ooo_to_mem_sfence_bits_rs1;
        input  io_ooo_to_mem_sfence_bits_rs2;
        input  io_ooo_to_mem_sfence_bits_addr;
        input  io_ooo_to_mem_sfence_bits_id;
        input  io_ooo_to_mem_sfence_bits_hv;
        input  io_ooo_to_mem_sfence_bits_hg;

    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:fence_agent_agent_interface

`endif
