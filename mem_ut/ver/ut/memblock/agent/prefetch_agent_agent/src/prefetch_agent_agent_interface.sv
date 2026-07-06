//=========================================================
//File name    : prefetch_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : prefetch_agent_agent_interface
//Discribution : prefetch_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef PREFETCH_AGENT_AGENT_INTERFACE__SV
`define PREFETCH_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface prefetch_agent_agent_interface  (input bit clk,input bit rst_n);

    logic [63:0] auto_inner_l3_pf_sender_out_addr;
    logic auto_inner_l3_pf_sender_out_addr_valid;
    logic auto_inner_l3_pf_sender_out_l2_pf_en;
    logic [63:0] auto_inner_l2_pf_sender_out_addr;
    logic [4:0] auto_inner_l2_pf_sender_out_pf_source;
    logic auto_inner_l2_pf_sender_out_addr_valid;
    logic auto_inner_l2_pf_sender_out_l2_pf_en;
    logic io_ifetchPrefetch_0_valid    ;
    logic [49:0] io_ifetchPrefetch_0_bits_vaddr;
    logic io_ifetchPrefetch_1_valid    ;
    logic [49:0] io_ifetchPrefetch_1_bits_vaddr;
    logic io_ifetchPrefetch_2_valid    ;
    logic [49:0] io_ifetchPrefetch_2_bits_vaddr;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  auto_inner_l3_pf_sender_out_addr;
        input  auto_inner_l3_pf_sender_out_addr_valid;
        input  auto_inner_l3_pf_sender_out_l2_pf_en;
        input  auto_inner_l2_pf_sender_out_addr;
        input  auto_inner_l2_pf_sender_out_pf_source;
        input  auto_inner_l2_pf_sender_out_addr_valid;
        input  auto_inner_l2_pf_sender_out_l2_pf_en;
        input  io_ifetchPrefetch_0_valid;
        input  io_ifetchPrefetch_0_bits_vaddr;
        input  io_ifetchPrefetch_1_valid;
        input  io_ifetchPrefetch_1_bits_vaddr;
        input  io_ifetchPrefetch_2_valid;
        input  io_ifetchPrefetch_2_bits_vaddr;

    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  auto_inner_l3_pf_sender_out_addr;
        input  auto_inner_l3_pf_sender_out_addr_valid;
        input  auto_inner_l3_pf_sender_out_l2_pf_en;
        input  auto_inner_l2_pf_sender_out_addr;
        input  auto_inner_l2_pf_sender_out_pf_source;
        input  auto_inner_l2_pf_sender_out_addr_valid;
        input  auto_inner_l2_pf_sender_out_l2_pf_en;
        input  io_ifetchPrefetch_0_valid;
        input  io_ifetchPrefetch_0_bits_vaddr;
        input  io_ifetchPrefetch_1_valid;
        input  io_ifetchPrefetch_1_bits_vaddr;
        input  io_ifetchPrefetch_2_valid;
        input  io_ifetchPrefetch_2_bits_vaddr;

    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:prefetch_agent_agent_interface

`endif
