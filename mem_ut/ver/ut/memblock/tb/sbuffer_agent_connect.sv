//=========================================================
//File name    : sbuffer_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : sbuffer_agent_connect
//Discribution : sbuffer_agent_connect : sbuffer_agent Interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef SBUFFER_AGENT_CONNECT__SV
`define SBUFFER_AGENT_CONNECT__SV

`define MEMBLOCK__SBUFFER_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    sbuffer_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    initial begin \
        uvm_config_db#(virtual sbuffer_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    `ifdef MEMBLOCK_UT \
    initial begin \
        force RTL_PATH.auto_inner_buffers_out_a_ready = U_IF_NAME.auto_inner_buffers_out_a_ready; \
        force U_IF_NAME.auto_inner_buffers_out_a_valid = RTL_PATH.auto_inner_buffers_out_a_valid; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_opcode = RTL_PATH.auto_inner_buffers_out_a_bits_opcode; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_param = RTL_PATH.auto_inner_buffers_out_a_bits_param; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_size = RTL_PATH.auto_inner_buffers_out_a_bits_size; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_source = RTL_PATH.auto_inner_buffers_out_a_bits_source; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_address = RTL_PATH.auto_inner_buffers_out_a_bits_address; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_user_memBackType_MM = RTL_PATH.auto_inner_buffers_out_a_bits_user_memBackType_MM; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_user_memPageType_NC = RTL_PATH.auto_inner_buffers_out_a_bits_user_memPageType_NC; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_mask = RTL_PATH.auto_inner_buffers_out_a_bits_mask; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_data = RTL_PATH.auto_inner_buffers_out_a_bits_data; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_corrupt = RTL_PATH.auto_inner_buffers_out_a_bits_corrupt; \
        force U_IF_NAME.auto_inner_buffers_out_d_ready = RTL_PATH.auto_inner_buffers_out_d_ready; \
        force RTL_PATH.auto_inner_buffers_out_d_valid = U_IF_NAME.auto_inner_buffers_out_d_valid; \
        force RTL_PATH.auto_inner_buffers_out_d_bits_opcode = U_IF_NAME.auto_inner_buffers_out_d_bits_opcode; \
        force RTL_PATH.auto_inner_buffers_out_d_bits_param = U_IF_NAME.auto_inner_buffers_out_d_bits_param; \
        force RTL_PATH.auto_inner_buffers_out_d_bits_size = U_IF_NAME.auto_inner_buffers_out_d_bits_size; \
        force RTL_PATH.auto_inner_buffers_out_d_bits_source = U_IF_NAME.auto_inner_buffers_out_d_bits_source; \
        force RTL_PATH.auto_inner_buffers_out_d_bits_sink = U_IF_NAME.auto_inner_buffers_out_d_bits_sink; \
        force RTL_PATH.auto_inner_buffers_out_d_bits_denied = U_IF_NAME.auto_inner_buffers_out_d_bits_denied; \
        force RTL_PATH.auto_inner_buffers_out_d_bits_data = U_IF_NAME.auto_inner_buffers_out_d_bits_data; \
        force RTL_PATH.auto_inner_buffers_out_d_bits_corrupt = U_IF_NAME.auto_inner_buffers_out_d_bits_corrupt; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.auto_inner_buffers_out_a_ready = RTL_PATH.auto_inner_buffers_out_a_ready; \
        force U_IF_NAME.auto_inner_buffers_out_a_valid = RTL_PATH.auto_inner_buffers_out_a_valid; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_opcode = RTL_PATH.auto_inner_buffers_out_a_bits_opcode; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_param = RTL_PATH.auto_inner_buffers_out_a_bits_param; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_size = RTL_PATH.auto_inner_buffers_out_a_bits_size; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_source = RTL_PATH.auto_inner_buffers_out_a_bits_source; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_address = RTL_PATH.auto_inner_buffers_out_a_bits_address; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_user_memBackType_MM = RTL_PATH.auto_inner_buffers_out_a_bits_user_memBackType_MM; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_user_memPageType_NC = RTL_PATH.auto_inner_buffers_out_a_bits_user_memPageType_NC; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_mask = RTL_PATH.auto_inner_buffers_out_a_bits_mask; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_data = RTL_PATH.auto_inner_buffers_out_a_bits_data; \
        force U_IF_NAME.auto_inner_buffers_out_a_bits_corrupt = RTL_PATH.auto_inner_buffers_out_a_bits_corrupt; \
        force U_IF_NAME.auto_inner_buffers_out_d_ready = RTL_PATH.auto_inner_buffers_out_d_ready; \
        force U_IF_NAME.auto_inner_buffers_out_d_valid = RTL_PATH.auto_inner_buffers_out_d_valid; \
        force U_IF_NAME.auto_inner_buffers_out_d_bits_opcode = RTL_PATH.auto_inner_buffers_out_d_bits_opcode; \
        force U_IF_NAME.auto_inner_buffers_out_d_bits_param = RTL_PATH.auto_inner_buffers_out_d_bits_param; \
        force U_IF_NAME.auto_inner_buffers_out_d_bits_size = RTL_PATH.auto_inner_buffers_out_d_bits_size; \
        force U_IF_NAME.auto_inner_buffers_out_d_bits_source = RTL_PATH.auto_inner_buffers_out_d_bits_source; \
        force U_IF_NAME.auto_inner_buffers_out_d_bits_sink = RTL_PATH.auto_inner_buffers_out_d_bits_sink; \
        force U_IF_NAME.auto_inner_buffers_out_d_bits_denied = RTL_PATH.auto_inner_buffers_out_d_bits_denied; \
        force U_IF_NAME.auto_inner_buffers_out_d_bits_data = RTL_PATH.auto_inner_buffers_out_d_bits_data; \
        force U_IF_NAME.auto_inner_buffers_out_d_bits_corrupt = RTL_PATH.auto_inner_buffers_out_d_bits_corrupt; \
    end \
    `endif

`endif
