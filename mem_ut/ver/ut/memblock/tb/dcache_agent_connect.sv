//=========================================================
//File name    : dcache_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : dcache_agent_connect
//Discribution : dcache_agent_connect : dcache_agent Interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef DCACHE_AGENT_CONNECT__SV
`define DCACHE_AGENT_CONNECT__SV

`define MEMBLOCK__DCACHE_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    dcache_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    initial begin \
        uvm_config_db#(virtual dcache_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    `ifdef MEMBLOCK_UT \
    initial begin \
        force RTL_PATH.auto_inner_dcache_client_out_a_ready = U_IF_NAME.auto_inner_dcache_client_out_a_ready; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_valid = RTL_PATH.auto_inner_dcache_client_out_a_valid; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_opcode = RTL_PATH.auto_inner_dcache_client_out_a_bits_opcode; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_param = RTL_PATH.auto_inner_dcache_client_out_a_bits_param; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_size = RTL_PATH.auto_inner_dcache_client_out_a_bits_size; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_source = RTL_PATH.auto_inner_dcache_client_out_a_bits_source; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_address = RTL_PATH.auto_inner_dcache_client_out_a_bits_address; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_user_alias = RTL_PATH.auto_inner_dcache_client_out_a_bits_user_alias; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_user_memPageType_NC = RTL_PATH.auto_inner_dcache_client_out_a_bits_user_memPageType_NC; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_user_memBackType_MM = RTL_PATH.auto_inner_dcache_client_out_a_bits_user_memBackType_MM; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_user_vaddr = RTL_PATH.auto_inner_dcache_client_out_a_bits_user_vaddr; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_user_reqSource = RTL_PATH.auto_inner_dcache_client_out_a_bits_user_reqSource; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_user_needHint = RTL_PATH.auto_inner_dcache_client_out_a_bits_user_needHint; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_echo_isKeyword = RTL_PATH.auto_inner_dcache_client_out_a_bits_echo_isKeyword; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_mask = RTL_PATH.auto_inner_dcache_client_out_a_bits_mask; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_data = RTL_PATH.auto_inner_dcache_client_out_a_bits_data; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_corrupt = RTL_PATH.auto_inner_dcache_client_out_a_bits_corrupt; \
        force U_IF_NAME.auto_inner_dcache_client_out_b_ready = RTL_PATH.auto_inner_dcache_client_out_b_ready; \
        force RTL_PATH.auto_inner_dcache_client_out_b_valid = U_IF_NAME.auto_inner_dcache_client_out_b_valid; \
        force RTL_PATH.auto_inner_dcache_client_out_b_bits_opcode = U_IF_NAME.auto_inner_dcache_client_out_b_bits_opcode; \
        force RTL_PATH.auto_inner_dcache_client_out_b_bits_param = U_IF_NAME.auto_inner_dcache_client_out_b_bits_param; \
        force RTL_PATH.auto_inner_dcache_client_out_b_bits_size = U_IF_NAME.auto_inner_dcache_client_out_b_bits_size; \
        force RTL_PATH.auto_inner_dcache_client_out_b_bits_source = U_IF_NAME.auto_inner_dcache_client_out_b_bits_source; \
        force RTL_PATH.auto_inner_dcache_client_out_b_bits_address = U_IF_NAME.auto_inner_dcache_client_out_b_bits_address; \
        force RTL_PATH.auto_inner_dcache_client_out_b_bits_mask = U_IF_NAME.auto_inner_dcache_client_out_b_bits_mask; \
        force RTL_PATH.auto_inner_dcache_client_out_b_bits_data = U_IF_NAME.auto_inner_dcache_client_out_b_bits_data; \
        force RTL_PATH.auto_inner_dcache_client_out_b_bits_corrupt = U_IF_NAME.auto_inner_dcache_client_out_b_bits_corrupt; \
        force RTL_PATH.auto_inner_dcache_client_out_c_ready = U_IF_NAME.auto_inner_dcache_client_out_c_ready; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_valid = RTL_PATH.auto_inner_dcache_client_out_c_valid; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_opcode = RTL_PATH.auto_inner_dcache_client_out_c_bits_opcode; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_param = RTL_PATH.auto_inner_dcache_client_out_c_bits_param; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_size = RTL_PATH.auto_inner_dcache_client_out_c_bits_size; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_source = RTL_PATH.auto_inner_dcache_client_out_c_bits_source; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_address = RTL_PATH.auto_inner_dcache_client_out_c_bits_address; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_user_alias = RTL_PATH.auto_inner_dcache_client_out_c_bits_user_alias; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_user_memPageType_NC = RTL_PATH.auto_inner_dcache_client_out_c_bits_user_memPageType_NC; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_user_memBackType_MM = RTL_PATH.auto_inner_dcache_client_out_c_bits_user_memBackType_MM; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_user_vaddr = RTL_PATH.auto_inner_dcache_client_out_c_bits_user_vaddr; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_user_reqSource = RTL_PATH.auto_inner_dcache_client_out_c_bits_user_reqSource; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_user_needHint = RTL_PATH.auto_inner_dcache_client_out_c_bits_user_needHint; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_echo_isKeyword = RTL_PATH.auto_inner_dcache_client_out_c_bits_echo_isKeyword; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_data = RTL_PATH.auto_inner_dcache_client_out_c_bits_data; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_corrupt = RTL_PATH.auto_inner_dcache_client_out_c_bits_corrupt; \
        force U_IF_NAME.auto_inner_dcache_client_out_d_ready = RTL_PATH.auto_inner_dcache_client_out_d_ready; \
        force RTL_PATH.auto_inner_dcache_client_out_d_valid = U_IF_NAME.auto_inner_dcache_client_out_d_valid; \
        force RTL_PATH.auto_inner_dcache_client_out_d_bits_opcode = U_IF_NAME.auto_inner_dcache_client_out_d_bits_opcode; \
        force RTL_PATH.auto_inner_dcache_client_out_d_bits_param = U_IF_NAME.auto_inner_dcache_client_out_d_bits_param; \
        force RTL_PATH.auto_inner_dcache_client_out_d_bits_size = U_IF_NAME.auto_inner_dcache_client_out_d_bits_size; \
        force RTL_PATH.auto_inner_dcache_client_out_d_bits_source = U_IF_NAME.auto_inner_dcache_client_out_d_bits_source; \
        force RTL_PATH.auto_inner_dcache_client_out_d_bits_sink = U_IF_NAME.auto_inner_dcache_client_out_d_bits_sink; \
        force RTL_PATH.auto_inner_dcache_client_out_d_bits_denied = U_IF_NAME.auto_inner_dcache_client_out_d_bits_denied; \
        force RTL_PATH.auto_inner_dcache_client_out_d_bits_echo_isKeyword = U_IF_NAME.auto_inner_dcache_client_out_d_bits_echo_isKeyword; \
        force RTL_PATH.auto_inner_dcache_client_out_d_bits_data = U_IF_NAME.auto_inner_dcache_client_out_d_bits_data; \
        force RTL_PATH.auto_inner_dcache_client_out_d_bits_corrupt = U_IF_NAME.auto_inner_dcache_client_out_d_bits_corrupt; \
        force RTL_PATH.auto_inner_dcache_client_out_e_ready = U_IF_NAME.auto_inner_dcache_client_out_e_ready; \
        force U_IF_NAME.auto_inner_dcache_client_out_e_valid = RTL_PATH.auto_inner_dcache_client_out_e_valid; \
        force U_IF_NAME.auto_inner_dcache_client_out_e_bits_sink = RTL_PATH.auto_inner_dcache_client_out_e_bits_sink; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.auto_inner_dcache_client_out_a_ready = RTL_PATH.auto_inner_dcache_client_out_a_ready; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_valid = RTL_PATH.auto_inner_dcache_client_out_a_valid; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_opcode = RTL_PATH.auto_inner_dcache_client_out_a_bits_opcode; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_param = RTL_PATH.auto_inner_dcache_client_out_a_bits_param; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_size = RTL_PATH.auto_inner_dcache_client_out_a_bits_size; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_source = RTL_PATH.auto_inner_dcache_client_out_a_bits_source; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_address = RTL_PATH.auto_inner_dcache_client_out_a_bits_address; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_user_alias = RTL_PATH.auto_inner_dcache_client_out_a_bits_user_alias; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_user_memPageType_NC = RTL_PATH.auto_inner_dcache_client_out_a_bits_user_memPageType_NC; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_user_memBackType_MM = RTL_PATH.auto_inner_dcache_client_out_a_bits_user_memBackType_MM; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_user_vaddr = RTL_PATH.auto_inner_dcache_client_out_a_bits_user_vaddr; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_user_reqSource = RTL_PATH.auto_inner_dcache_client_out_a_bits_user_reqSource; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_user_needHint = RTL_PATH.auto_inner_dcache_client_out_a_bits_user_needHint; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_echo_isKeyword = RTL_PATH.auto_inner_dcache_client_out_a_bits_echo_isKeyword; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_mask = RTL_PATH.auto_inner_dcache_client_out_a_bits_mask; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_data = RTL_PATH.auto_inner_dcache_client_out_a_bits_data; \
        force U_IF_NAME.auto_inner_dcache_client_out_a_bits_corrupt = RTL_PATH.auto_inner_dcache_client_out_a_bits_corrupt; \
        force U_IF_NAME.auto_inner_dcache_client_out_b_ready = RTL_PATH.auto_inner_dcache_client_out_b_ready; \
        force U_IF_NAME.auto_inner_dcache_client_out_b_valid = RTL_PATH.auto_inner_dcache_client_out_b_valid; \
        force U_IF_NAME.auto_inner_dcache_client_out_b_bits_opcode = RTL_PATH.auto_inner_dcache_client_out_b_bits_opcode; \
        force U_IF_NAME.auto_inner_dcache_client_out_b_bits_param = RTL_PATH.auto_inner_dcache_client_out_b_bits_param; \
        force U_IF_NAME.auto_inner_dcache_client_out_b_bits_size = RTL_PATH.auto_inner_dcache_client_out_b_bits_size; \
        force U_IF_NAME.auto_inner_dcache_client_out_b_bits_source = RTL_PATH.auto_inner_dcache_client_out_b_bits_source; \
        force U_IF_NAME.auto_inner_dcache_client_out_b_bits_address = RTL_PATH.auto_inner_dcache_client_out_b_bits_address; \
        force U_IF_NAME.auto_inner_dcache_client_out_b_bits_mask = RTL_PATH.auto_inner_dcache_client_out_b_bits_mask; \
        force U_IF_NAME.auto_inner_dcache_client_out_b_bits_data = RTL_PATH.auto_inner_dcache_client_out_b_bits_data; \
        force U_IF_NAME.auto_inner_dcache_client_out_b_bits_corrupt = RTL_PATH.auto_inner_dcache_client_out_b_bits_corrupt; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_ready = RTL_PATH.auto_inner_dcache_client_out_c_ready; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_valid = RTL_PATH.auto_inner_dcache_client_out_c_valid; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_opcode = RTL_PATH.auto_inner_dcache_client_out_c_bits_opcode; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_param = RTL_PATH.auto_inner_dcache_client_out_c_bits_param; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_size = RTL_PATH.auto_inner_dcache_client_out_c_bits_size; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_source = RTL_PATH.auto_inner_dcache_client_out_c_bits_source; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_address = RTL_PATH.auto_inner_dcache_client_out_c_bits_address; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_user_alias = RTL_PATH.auto_inner_dcache_client_out_c_bits_user_alias; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_user_memPageType_NC = RTL_PATH.auto_inner_dcache_client_out_c_bits_user_memPageType_NC; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_user_memBackType_MM = RTL_PATH.auto_inner_dcache_client_out_c_bits_user_memBackType_MM; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_user_vaddr = RTL_PATH.auto_inner_dcache_client_out_c_bits_user_vaddr; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_user_reqSource = RTL_PATH.auto_inner_dcache_client_out_c_bits_user_reqSource; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_user_needHint = RTL_PATH.auto_inner_dcache_client_out_c_bits_user_needHint; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_echo_isKeyword = RTL_PATH.auto_inner_dcache_client_out_c_bits_echo_isKeyword; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_data = RTL_PATH.auto_inner_dcache_client_out_c_bits_data; \
        force U_IF_NAME.auto_inner_dcache_client_out_c_bits_corrupt = RTL_PATH.auto_inner_dcache_client_out_c_bits_corrupt; \
        force U_IF_NAME.auto_inner_dcache_client_out_d_ready = RTL_PATH.auto_inner_dcache_client_out_d_ready; \
        force U_IF_NAME.auto_inner_dcache_client_out_d_valid = RTL_PATH.auto_inner_dcache_client_out_d_valid; \
        force U_IF_NAME.auto_inner_dcache_client_out_d_bits_opcode = RTL_PATH.auto_inner_dcache_client_out_d_bits_opcode; \
        force U_IF_NAME.auto_inner_dcache_client_out_d_bits_param = RTL_PATH.auto_inner_dcache_client_out_d_bits_param; \
        force U_IF_NAME.auto_inner_dcache_client_out_d_bits_size = RTL_PATH.auto_inner_dcache_client_out_d_bits_size; \
        force U_IF_NAME.auto_inner_dcache_client_out_d_bits_source = RTL_PATH.auto_inner_dcache_client_out_d_bits_source; \
        force U_IF_NAME.auto_inner_dcache_client_out_d_bits_sink = RTL_PATH.auto_inner_dcache_client_out_d_bits_sink; \
        force U_IF_NAME.auto_inner_dcache_client_out_d_bits_denied = RTL_PATH.auto_inner_dcache_client_out_d_bits_denied; \
        force U_IF_NAME.auto_inner_dcache_client_out_d_bits_echo_isKeyword = RTL_PATH.auto_inner_dcache_client_out_d_bits_echo_isKeyword; \
        force U_IF_NAME.auto_inner_dcache_client_out_d_bits_data = RTL_PATH.auto_inner_dcache_client_out_d_bits_data; \
        force U_IF_NAME.auto_inner_dcache_client_out_d_bits_corrupt = RTL_PATH.auto_inner_dcache_client_out_d_bits_corrupt; \
        force U_IF_NAME.auto_inner_dcache_client_out_e_ready = RTL_PATH.auto_inner_dcache_client_out_e_ready; \
        force U_IF_NAME.auto_inner_dcache_client_out_e_valid = RTL_PATH.auto_inner_dcache_client_out_e_valid; \
        force U_IF_NAME.auto_inner_dcache_client_out_e_bits_sink = RTL_PATH.auto_inner_dcache_client_out_e_bits_sink; \
    end \
    `endif

`endif
