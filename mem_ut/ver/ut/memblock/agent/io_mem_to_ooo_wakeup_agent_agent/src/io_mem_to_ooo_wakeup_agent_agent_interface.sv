//=========================================================
//File name    : io_mem_to_ooo_wakeup_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_wakeup_agent_agent_interface
//Discribution : io_mem_to_ooo_wakeup_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_WAKEUP_AGENT_AGENT_INTERFACE__SV
`define IO_MEM_TO_OOO_WAKEUP_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface io_mem_to_ooo_wakeup_agent_agent_interface  (input bit clk,input bit rst_n);

    logic io_mem_to_ooo_wakeup_0_valid ;
    logic io_mem_to_ooo_wakeup_0_bits_rfWen;
    logic io_mem_to_ooo_wakeup_0_bits_fpWen;
    logic io_mem_to_ooo_wakeup_0_bits_vecWen;
    logic io_mem_to_ooo_wakeup_0_bits_v0Wen;
    logic io_mem_to_ooo_wakeup_0_bits_vlWen;
    logic [7:0] io_mem_to_ooo_wakeup_0_bits_pdest;
    logic io_mem_to_ooo_wakeup_1_valid ;
    logic io_mem_to_ooo_wakeup_1_bits_rfWen;
    logic io_mem_to_ooo_wakeup_1_bits_fpWen;
    logic io_mem_to_ooo_wakeup_1_bits_vecWen;
    logic io_mem_to_ooo_wakeup_1_bits_v0Wen;
    logic io_mem_to_ooo_wakeup_1_bits_vlWen;
    logic [7:0] io_mem_to_ooo_wakeup_1_bits_pdest;
    logic io_mem_to_ooo_wakeup_2_valid ;
    logic io_mem_to_ooo_wakeup_2_bits_rfWen;
    logic io_mem_to_ooo_wakeup_2_bits_fpWen;
    logic io_mem_to_ooo_wakeup_2_bits_vecWen;
    logic io_mem_to_ooo_wakeup_2_bits_v0Wen;
    logic io_mem_to_ooo_wakeup_2_bits_vlWen;
    logic [7:0] io_mem_to_ooo_wakeup_2_bits_pdest;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_mem_to_ooo_wakeup_0_valid;
        input  io_mem_to_ooo_wakeup_0_bits_rfWen;
        input  io_mem_to_ooo_wakeup_0_bits_fpWen;
        input  io_mem_to_ooo_wakeup_0_bits_vecWen;
        input  io_mem_to_ooo_wakeup_0_bits_v0Wen;
        input  io_mem_to_ooo_wakeup_0_bits_vlWen;
        input  io_mem_to_ooo_wakeup_0_bits_pdest;
        input  io_mem_to_ooo_wakeup_1_valid;
        input  io_mem_to_ooo_wakeup_1_bits_rfWen;
        input  io_mem_to_ooo_wakeup_1_bits_fpWen;
        input  io_mem_to_ooo_wakeup_1_bits_vecWen;
        input  io_mem_to_ooo_wakeup_1_bits_v0Wen;
        input  io_mem_to_ooo_wakeup_1_bits_vlWen;
        input  io_mem_to_ooo_wakeup_1_bits_pdest;
        input  io_mem_to_ooo_wakeup_2_valid;
        input  io_mem_to_ooo_wakeup_2_bits_rfWen;
        input  io_mem_to_ooo_wakeup_2_bits_fpWen;
        input  io_mem_to_ooo_wakeup_2_bits_vecWen;
        input  io_mem_to_ooo_wakeup_2_bits_v0Wen;
        input  io_mem_to_ooo_wakeup_2_bits_vlWen;
        input  io_mem_to_ooo_wakeup_2_bits_pdest;

    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_mem_to_ooo_wakeup_0_valid;
        input  io_mem_to_ooo_wakeup_0_bits_rfWen;
        input  io_mem_to_ooo_wakeup_0_bits_fpWen;
        input  io_mem_to_ooo_wakeup_0_bits_vecWen;
        input  io_mem_to_ooo_wakeup_0_bits_v0Wen;
        input  io_mem_to_ooo_wakeup_0_bits_vlWen;
        input  io_mem_to_ooo_wakeup_0_bits_pdest;
        input  io_mem_to_ooo_wakeup_1_valid;
        input  io_mem_to_ooo_wakeup_1_bits_rfWen;
        input  io_mem_to_ooo_wakeup_1_bits_fpWen;
        input  io_mem_to_ooo_wakeup_1_bits_vecWen;
        input  io_mem_to_ooo_wakeup_1_bits_v0Wen;
        input  io_mem_to_ooo_wakeup_1_bits_vlWen;
        input  io_mem_to_ooo_wakeup_1_bits_pdest;
        input  io_mem_to_ooo_wakeup_2_valid;
        input  io_mem_to_ooo_wakeup_2_bits_rfWen;
        input  io_mem_to_ooo_wakeup_2_bits_fpWen;
        input  io_mem_to_ooo_wakeup_2_bits_vecWen;
        input  io_mem_to_ooo_wakeup_2_bits_v0Wen;
        input  io_mem_to_ooo_wakeup_2_bits_vlWen;
        input  io_mem_to_ooo_wakeup_2_bits_pdest;

    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:io_mem_to_ooo_wakeup_agent_agent_interface

`endif
