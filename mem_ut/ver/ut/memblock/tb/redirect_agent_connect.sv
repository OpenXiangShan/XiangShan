//=========================================================
//File name    : redirect_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : redirect_agent_connect
//Discribution : redirect_agent_connect : redirect_agent Interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef REDIRECT_AGENT_CONNECT__SV
`define REDIRECT_AGENT_CONNECT__SV

`define MEMBLOCK__REDIRECT_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    redirect_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    initial begin \
        uvm_config_db#(virtual redirect_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    `ifdef MEMBLOCK_UT \
    initial begin \
        force RTL_PATH.io_redirect_valid = U_IF_NAME.io_redirect_valid; \
        force RTL_PATH.io_redirect_bits_level = U_IF_NAME.io_redirect_bits_level; \
        force RTL_PATH.io_redirect_bits_robIdx_flag = U_IF_NAME.io_redirect_bits_robIdx_flag; \
        force RTL_PATH.io_redirect_bits_robIdx_value = U_IF_NAME.io_redirect_bits_robIdx_value; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.io_redirect_valid = RTL_PATH.io_redirect_valid; \
        force U_IF_NAME.io_redirect_bits_level = RTL_PATH.io_redirect_bits_level; \
        force U_IF_NAME.io_redirect_bits_robIdx_flag = RTL_PATH.io_redirect_bits_robIdx_flag; \
        force U_IF_NAME.io_redirect_bits_robIdx_value = RTL_PATH.io_redirect_bits_robIdx_value; \
    end \
    `endif

`endif
