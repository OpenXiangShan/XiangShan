//=========================================================
//File name    : dcache_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : dcache_agent_agent_interface
//Discribution : dcache_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef DCACHE_AGENT_AGENT_INTERFACE__SV
`define DCACHE_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface dcache_agent_agent_interface  (input bit clk,input bit rst_n);

    logic auto_inner_dcache_client_out_a_ready;
    logic auto_inner_dcache_client_out_a_valid;
    logic [3:0] auto_inner_dcache_client_out_a_bits_opcode;
    logic [2:0] auto_inner_dcache_client_out_a_bits_param;
    logic [2:0] auto_inner_dcache_client_out_a_bits_size;
    logic [5:0] auto_inner_dcache_client_out_a_bits_source;
    logic [47:0] auto_inner_dcache_client_out_a_bits_address;
    logic [1:0] auto_inner_dcache_client_out_a_bits_user_alias;
    logic auto_inner_dcache_client_out_a_bits_user_memPageType_NC;
    logic auto_inner_dcache_client_out_a_bits_user_memBackType_MM;
    logic [43:0] auto_inner_dcache_client_out_a_bits_user_vaddr;
    logic [4:0] auto_inner_dcache_client_out_a_bits_user_reqSource;
    logic auto_inner_dcache_client_out_a_bits_user_needHint;
    logic auto_inner_dcache_client_out_a_bits_echo_isKeyword;
    logic [31:0] auto_inner_dcache_client_out_a_bits_mask;
    logic [255:0] auto_inner_dcache_client_out_a_bits_data;
    logic auto_inner_dcache_client_out_a_bits_corrupt;
    logic auto_inner_dcache_client_out_b_ready;
    logic auto_inner_dcache_client_out_b_valid;
    logic [2:0] auto_inner_dcache_client_out_b_bits_opcode;
    logic [1:0] auto_inner_dcache_client_out_b_bits_param;
    logic [2:0] auto_inner_dcache_client_out_b_bits_size;
    logic [5:0] auto_inner_dcache_client_out_b_bits_source;
    logic [47:0] auto_inner_dcache_client_out_b_bits_address;
    logic [31:0] auto_inner_dcache_client_out_b_bits_mask;
    logic [255:0] auto_inner_dcache_client_out_b_bits_data;
    logic auto_inner_dcache_client_out_b_bits_corrupt;
    logic auto_inner_dcache_client_out_c_ready;
    logic auto_inner_dcache_client_out_c_valid;
    logic [2:0] auto_inner_dcache_client_out_c_bits_opcode;
    logic [2:0] auto_inner_dcache_client_out_c_bits_param;
    logic [2:0] auto_inner_dcache_client_out_c_bits_size;
    logic [5:0] auto_inner_dcache_client_out_c_bits_source;
    logic [47:0] auto_inner_dcache_client_out_c_bits_address;
    logic [1:0] auto_inner_dcache_client_out_c_bits_user_alias;
    logic auto_inner_dcache_client_out_c_bits_user_memPageType_NC;
    logic auto_inner_dcache_client_out_c_bits_user_memBackType_MM;
    logic [43:0] auto_inner_dcache_client_out_c_bits_user_vaddr;
    logic [4:0] auto_inner_dcache_client_out_c_bits_user_reqSource;
    logic auto_inner_dcache_client_out_c_bits_user_needHint;
    logic auto_inner_dcache_client_out_c_bits_echo_isKeyword;
    logic [255:0] auto_inner_dcache_client_out_c_bits_data;
    logic auto_inner_dcache_client_out_c_bits_corrupt;
    logic auto_inner_dcache_client_out_d_ready;
    logic auto_inner_dcache_client_out_d_valid;
    logic [3:0] auto_inner_dcache_client_out_d_bits_opcode;
    logic [1:0] auto_inner_dcache_client_out_d_bits_param;
    logic [2:0] auto_inner_dcache_client_out_d_bits_size;
    logic [5:0] auto_inner_dcache_client_out_d_bits_source;
    logic [9:0] auto_inner_dcache_client_out_d_bits_sink;
    logic auto_inner_dcache_client_out_d_bits_denied;
    logic auto_inner_dcache_client_out_d_bits_echo_isKeyword;
    logic [255:0] auto_inner_dcache_client_out_d_bits_data;
    logic auto_inner_dcache_client_out_d_bits_corrupt;
    logic auto_inner_dcache_client_out_e_ready;
    logic auto_inner_dcache_client_out_e_valid;
    logic [9:0] auto_inner_dcache_client_out_e_bits_sink;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        output auto_inner_dcache_client_out_a_ready;
        input  auto_inner_dcache_client_out_a_valid;
        input  auto_inner_dcache_client_out_a_bits_opcode;
        input  auto_inner_dcache_client_out_a_bits_param;
        input  auto_inner_dcache_client_out_a_bits_size;
        input  auto_inner_dcache_client_out_a_bits_source;
        input  auto_inner_dcache_client_out_a_bits_address;
        input  auto_inner_dcache_client_out_a_bits_user_alias;
        input  auto_inner_dcache_client_out_a_bits_user_memPageType_NC;
        input  auto_inner_dcache_client_out_a_bits_user_memBackType_MM;
        input  auto_inner_dcache_client_out_a_bits_user_vaddr;
        input  auto_inner_dcache_client_out_a_bits_user_reqSource;
        input  auto_inner_dcache_client_out_a_bits_user_needHint;
        input  auto_inner_dcache_client_out_a_bits_echo_isKeyword;
        input  auto_inner_dcache_client_out_a_bits_mask;
        input  auto_inner_dcache_client_out_a_bits_data;
        input  auto_inner_dcache_client_out_a_bits_corrupt;
        input  auto_inner_dcache_client_out_b_ready;
        output auto_inner_dcache_client_out_b_valid;
        output auto_inner_dcache_client_out_b_bits_opcode;
        output auto_inner_dcache_client_out_b_bits_param;
        output auto_inner_dcache_client_out_b_bits_size;
        output auto_inner_dcache_client_out_b_bits_source;
        output auto_inner_dcache_client_out_b_bits_address;
        output auto_inner_dcache_client_out_b_bits_mask;
        output auto_inner_dcache_client_out_b_bits_data;
        output auto_inner_dcache_client_out_b_bits_corrupt;
        output auto_inner_dcache_client_out_c_ready;
        input  auto_inner_dcache_client_out_c_valid;
        input  auto_inner_dcache_client_out_c_bits_opcode;
        input  auto_inner_dcache_client_out_c_bits_param;
        input  auto_inner_dcache_client_out_c_bits_size;
        input  auto_inner_dcache_client_out_c_bits_source;
        input  auto_inner_dcache_client_out_c_bits_address;
        input  auto_inner_dcache_client_out_c_bits_user_alias;
        input  auto_inner_dcache_client_out_c_bits_user_memPageType_NC;
        input  auto_inner_dcache_client_out_c_bits_user_memBackType_MM;
        input  auto_inner_dcache_client_out_c_bits_user_vaddr;
        input  auto_inner_dcache_client_out_c_bits_user_reqSource;
        input  auto_inner_dcache_client_out_c_bits_user_needHint;
        input  auto_inner_dcache_client_out_c_bits_echo_isKeyword;
        input  auto_inner_dcache_client_out_c_bits_data;
        input  auto_inner_dcache_client_out_c_bits_corrupt;
        input  auto_inner_dcache_client_out_d_ready;
        output auto_inner_dcache_client_out_d_valid;
        output auto_inner_dcache_client_out_d_bits_opcode;
        output auto_inner_dcache_client_out_d_bits_param;
        output auto_inner_dcache_client_out_d_bits_size;
        output auto_inner_dcache_client_out_d_bits_source;
        output auto_inner_dcache_client_out_d_bits_sink;
        output auto_inner_dcache_client_out_d_bits_denied;
        output auto_inner_dcache_client_out_d_bits_echo_isKeyword;
        output auto_inner_dcache_client_out_d_bits_data;
        output auto_inner_dcache_client_out_d_bits_corrupt;
        output auto_inner_dcache_client_out_e_ready;
        input  auto_inner_dcache_client_out_e_valid;
        input  auto_inner_dcache_client_out_e_bits_sink;

    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  auto_inner_dcache_client_out_a_ready;
        input  auto_inner_dcache_client_out_a_valid;
        input  auto_inner_dcache_client_out_a_bits_opcode;
        input  auto_inner_dcache_client_out_a_bits_param;
        input  auto_inner_dcache_client_out_a_bits_size;
        input  auto_inner_dcache_client_out_a_bits_source;
        input  auto_inner_dcache_client_out_a_bits_address;
        input  auto_inner_dcache_client_out_a_bits_user_alias;
        input  auto_inner_dcache_client_out_a_bits_user_memPageType_NC;
        input  auto_inner_dcache_client_out_a_bits_user_memBackType_MM;
        input  auto_inner_dcache_client_out_a_bits_user_vaddr;
        input  auto_inner_dcache_client_out_a_bits_user_reqSource;
        input  auto_inner_dcache_client_out_a_bits_user_needHint;
        input  auto_inner_dcache_client_out_a_bits_echo_isKeyword;
        input  auto_inner_dcache_client_out_a_bits_mask;
        input  auto_inner_dcache_client_out_a_bits_data;
        input  auto_inner_dcache_client_out_a_bits_corrupt;
        input  auto_inner_dcache_client_out_b_ready;
        input  auto_inner_dcache_client_out_b_valid;
        input  auto_inner_dcache_client_out_b_bits_opcode;
        input  auto_inner_dcache_client_out_b_bits_param;
        input  auto_inner_dcache_client_out_b_bits_size;
        input  auto_inner_dcache_client_out_b_bits_source;
        input  auto_inner_dcache_client_out_b_bits_address;
        input  auto_inner_dcache_client_out_b_bits_mask;
        input  auto_inner_dcache_client_out_b_bits_data;
        input  auto_inner_dcache_client_out_b_bits_corrupt;
        input  auto_inner_dcache_client_out_c_ready;
        input  auto_inner_dcache_client_out_c_valid;
        input  auto_inner_dcache_client_out_c_bits_opcode;
        input  auto_inner_dcache_client_out_c_bits_param;
        input  auto_inner_dcache_client_out_c_bits_size;
        input  auto_inner_dcache_client_out_c_bits_source;
        input  auto_inner_dcache_client_out_c_bits_address;
        input  auto_inner_dcache_client_out_c_bits_user_alias;
        input  auto_inner_dcache_client_out_c_bits_user_memPageType_NC;
        input  auto_inner_dcache_client_out_c_bits_user_memBackType_MM;
        input  auto_inner_dcache_client_out_c_bits_user_vaddr;
        input  auto_inner_dcache_client_out_c_bits_user_reqSource;
        input  auto_inner_dcache_client_out_c_bits_user_needHint;
        input  auto_inner_dcache_client_out_c_bits_echo_isKeyword;
        input  auto_inner_dcache_client_out_c_bits_data;
        input  auto_inner_dcache_client_out_c_bits_corrupt;
        input  auto_inner_dcache_client_out_d_ready;
        input  auto_inner_dcache_client_out_d_valid;
        input  auto_inner_dcache_client_out_d_bits_opcode;
        input  auto_inner_dcache_client_out_d_bits_param;
        input  auto_inner_dcache_client_out_d_bits_size;
        input  auto_inner_dcache_client_out_d_bits_source;
        input  auto_inner_dcache_client_out_d_bits_sink;
        input  auto_inner_dcache_client_out_d_bits_denied;
        input  auto_inner_dcache_client_out_d_bits_echo_isKeyword;
        input  auto_inner_dcache_client_out_d_bits_data;
        input  auto_inner_dcache_client_out_d_bits_corrupt;
        input  auto_inner_dcache_client_out_e_ready;
        input  auto_inner_dcache_client_out_e_valid;
        input  auto_inner_dcache_client_out_e_bits_sink;

    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:dcache_agent_agent_interface

`endif
