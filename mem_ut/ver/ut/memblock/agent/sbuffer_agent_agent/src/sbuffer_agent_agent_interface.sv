//=========================================================
//File name    : sbuffer_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : sbuffer_agent_agent_interface
//Discribution : sbuffer_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef SBUFFER_AGENT_AGENT_INTERFACE__SV
`define SBUFFER_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface sbuffer_agent_agent_interface  (input bit clk,input bit rst_n);

    logic auto_inner_buffers_out_a_ready;
    logic auto_inner_buffers_out_a_valid;
    logic [3:0] auto_inner_buffers_out_a_bits_opcode;
    logic [2:0] auto_inner_buffers_out_a_bits_param;
    logic [2:0] auto_inner_buffers_out_a_bits_size;
    logic [3:0] auto_inner_buffers_out_a_bits_source;
    logic [47:0] auto_inner_buffers_out_a_bits_address;
    logic auto_inner_buffers_out_a_bits_user_memBackType_MM;
    logic auto_inner_buffers_out_a_bits_user_memPageType_NC;
    logic [7:0] auto_inner_buffers_out_a_bits_mask;
    logic [63:0] auto_inner_buffers_out_a_bits_data;
    logic auto_inner_buffers_out_a_bits_corrupt;
    logic auto_inner_buffers_out_d_ready;
    logic auto_inner_buffers_out_d_valid;
    logic [3:0] auto_inner_buffers_out_d_bits_opcode;
    logic [1:0] auto_inner_buffers_out_d_bits_param;
    logic [2:0] auto_inner_buffers_out_d_bits_size;
    logic [3:0] auto_inner_buffers_out_d_bits_source;
    logic auto_inner_buffers_out_d_bits_sink;
    logic auto_inner_buffers_out_d_bits_denied;
    logic [63:0] auto_inner_buffers_out_d_bits_data;
    logic auto_inner_buffers_out_d_bits_corrupt;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        output auto_inner_buffers_out_a_ready;
        input  auto_inner_buffers_out_a_valid;
        input  auto_inner_buffers_out_a_bits_opcode;
        input  auto_inner_buffers_out_a_bits_param;
        input  auto_inner_buffers_out_a_bits_size;
        input  auto_inner_buffers_out_a_bits_source;
        input  auto_inner_buffers_out_a_bits_address;
        input  auto_inner_buffers_out_a_bits_user_memBackType_MM;
        input  auto_inner_buffers_out_a_bits_user_memPageType_NC;
        input  auto_inner_buffers_out_a_bits_mask;
        input  auto_inner_buffers_out_a_bits_data;
        input  auto_inner_buffers_out_a_bits_corrupt;
        input  auto_inner_buffers_out_d_ready;
        output auto_inner_buffers_out_d_valid;
        output auto_inner_buffers_out_d_bits_opcode;
        output auto_inner_buffers_out_d_bits_param;
        output auto_inner_buffers_out_d_bits_size;
        output auto_inner_buffers_out_d_bits_source;
        output auto_inner_buffers_out_d_bits_sink;
        output auto_inner_buffers_out_d_bits_denied;
        output auto_inner_buffers_out_d_bits_data;
        output auto_inner_buffers_out_d_bits_corrupt;

    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  auto_inner_buffers_out_a_ready;
        input  auto_inner_buffers_out_a_valid;
        input  auto_inner_buffers_out_a_bits_opcode;
        input  auto_inner_buffers_out_a_bits_param;
        input  auto_inner_buffers_out_a_bits_size;
        input  auto_inner_buffers_out_a_bits_source;
        input  auto_inner_buffers_out_a_bits_address;
        input  auto_inner_buffers_out_a_bits_user_memBackType_MM;
        input  auto_inner_buffers_out_a_bits_user_memPageType_NC;
        input  auto_inner_buffers_out_a_bits_mask;
        input  auto_inner_buffers_out_a_bits_data;
        input  auto_inner_buffers_out_a_bits_corrupt;
        input  auto_inner_buffers_out_d_ready;
        input  auto_inner_buffers_out_d_valid;
        input  auto_inner_buffers_out_d_bits_opcode;
        input  auto_inner_buffers_out_d_bits_param;
        input  auto_inner_buffers_out_d_bits_size;
        input  auto_inner_buffers_out_d_bits_source;
        input  auto_inner_buffers_out_d_bits_sink;
        input  auto_inner_buffers_out_d_bits_denied;
        input  auto_inner_buffers_out_d_bits_data;
        input  auto_inner_buffers_out_d_bits_corrupt;

    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:sbuffer_agent_agent_interface

`endif
