//=========================================================
//File name    : fence_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : fence_agent_connect
//Discribution : fence_agent_connect : fence_agent Interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef FENCE_AGENT_CONNECT__SV
`define FENCE_AGENT_CONNECT__SV

`define MEMBLOCK__FENCE_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    fence_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    initial begin \
        uvm_config_db#(virtual fence_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    `ifdef MEMBLOCK_UT \
    initial begin \
        force RTL_PATH.io_ooo_to_mem_sfence_valid = U_IF_NAME.io_ooo_to_mem_sfence_valid; \
        force RTL_PATH.io_ooo_to_mem_sfence_bits_rs1 = U_IF_NAME.io_ooo_to_mem_sfence_bits_rs1; \
        force RTL_PATH.io_ooo_to_mem_sfence_bits_rs2 = U_IF_NAME.io_ooo_to_mem_sfence_bits_rs2; \
        force RTL_PATH.io_ooo_to_mem_sfence_bits_addr = U_IF_NAME.io_ooo_to_mem_sfence_bits_addr; \
        force RTL_PATH.io_ooo_to_mem_sfence_bits_id = U_IF_NAME.io_ooo_to_mem_sfence_bits_id; \
        force RTL_PATH.io_ooo_to_mem_sfence_bits_hv = U_IF_NAME.io_ooo_to_mem_sfence_bits_hv; \
        force RTL_PATH.io_ooo_to_mem_sfence_bits_hg = U_IF_NAME.io_ooo_to_mem_sfence_bits_hg; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.io_ooo_to_mem_sfence_valid = RTL_PATH.io_ooo_to_mem_sfence_valid; \
        force U_IF_NAME.io_ooo_to_mem_sfence_bits_rs1 = RTL_PATH.io_ooo_to_mem_sfence_bits_rs1; \
        force U_IF_NAME.io_ooo_to_mem_sfence_bits_rs2 = RTL_PATH.io_ooo_to_mem_sfence_bits_rs2; \
        force U_IF_NAME.io_ooo_to_mem_sfence_bits_addr = RTL_PATH.io_ooo_to_mem_sfence_bits_addr; \
        force U_IF_NAME.io_ooo_to_mem_sfence_bits_id = RTL_PATH.io_ooo_to_mem_sfence_bits_id; \
        force U_IF_NAME.io_ooo_to_mem_sfence_bits_hv = RTL_PATH.io_ooo_to_mem_sfence_bits_hv; \
        force U_IF_NAME.io_ooo_to_mem_sfence_bits_hg = RTL_PATH.io_ooo_to_mem_sfence_bits_hg; \
    end \
    `endif

`endif
