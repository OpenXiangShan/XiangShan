//=========================================================
//File name    : io_mem_to_ooo_wakeup_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_wakeup_agent_connect
//Discribution : io_mem_to_ooo_wakeup_agent_connect : io_mem_to_ooo_wakeup_agent Interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_WAKEUP_AGENT_CONNECT__SV
`define IO_MEM_TO_OOO_WAKEUP_AGENT_CONNECT__SV

`define MEMBLOCK__IO_MEM_TO_OOO_WAKEUP_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    io_mem_to_ooo_wakeup_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    initial begin \
        uvm_config_db#(virtual io_mem_to_ooo_wakeup_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    `ifdef MEMBLOCK_UT \
    initial begin \
        force U_IF_NAME.io_mem_to_ooo_wakeup_0_valid = RTL_PATH.io_mem_to_ooo_wakeup_0_valid; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_0_bits_rfWen = RTL_PATH.io_mem_to_ooo_wakeup_0_bits_rfWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_0_bits_fpWen = RTL_PATH.io_mem_to_ooo_wakeup_0_bits_fpWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_0_bits_vecWen = RTL_PATH.io_mem_to_ooo_wakeup_0_bits_vecWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_0_bits_v0Wen = RTL_PATH.io_mem_to_ooo_wakeup_0_bits_v0Wen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_0_bits_vlWen = RTL_PATH.io_mem_to_ooo_wakeup_0_bits_vlWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_0_bits_pdest = RTL_PATH.io_mem_to_ooo_wakeup_0_bits_pdest; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_1_valid = RTL_PATH.io_mem_to_ooo_wakeup_1_valid; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_1_bits_rfWen = RTL_PATH.io_mem_to_ooo_wakeup_1_bits_rfWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_1_bits_fpWen = RTL_PATH.io_mem_to_ooo_wakeup_1_bits_fpWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_1_bits_vecWen = RTL_PATH.io_mem_to_ooo_wakeup_1_bits_vecWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_1_bits_v0Wen = RTL_PATH.io_mem_to_ooo_wakeup_1_bits_v0Wen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_1_bits_vlWen = RTL_PATH.io_mem_to_ooo_wakeup_1_bits_vlWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_1_bits_pdest = RTL_PATH.io_mem_to_ooo_wakeup_1_bits_pdest; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_2_valid = RTL_PATH.io_mem_to_ooo_wakeup_2_valid; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_2_bits_rfWen = RTL_PATH.io_mem_to_ooo_wakeup_2_bits_rfWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_2_bits_fpWen = RTL_PATH.io_mem_to_ooo_wakeup_2_bits_fpWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_2_bits_vecWen = RTL_PATH.io_mem_to_ooo_wakeup_2_bits_vecWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_2_bits_v0Wen = RTL_PATH.io_mem_to_ooo_wakeup_2_bits_v0Wen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_2_bits_vlWen = RTL_PATH.io_mem_to_ooo_wakeup_2_bits_vlWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_2_bits_pdest = RTL_PATH.io_mem_to_ooo_wakeup_2_bits_pdest; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.io_mem_to_ooo_wakeup_0_valid = RTL_PATH.io_mem_to_ooo_wakeup_0_valid; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_0_bits_rfWen = RTL_PATH.io_mem_to_ooo_wakeup_0_bits_rfWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_0_bits_fpWen = RTL_PATH.io_mem_to_ooo_wakeup_0_bits_fpWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_0_bits_vecWen = RTL_PATH.io_mem_to_ooo_wakeup_0_bits_vecWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_0_bits_v0Wen = RTL_PATH.io_mem_to_ooo_wakeup_0_bits_v0Wen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_0_bits_vlWen = RTL_PATH.io_mem_to_ooo_wakeup_0_bits_vlWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_0_bits_pdest = RTL_PATH.io_mem_to_ooo_wakeup_0_bits_pdest; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_1_valid = RTL_PATH.io_mem_to_ooo_wakeup_1_valid; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_1_bits_rfWen = RTL_PATH.io_mem_to_ooo_wakeup_1_bits_rfWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_1_bits_fpWen = RTL_PATH.io_mem_to_ooo_wakeup_1_bits_fpWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_1_bits_vecWen = RTL_PATH.io_mem_to_ooo_wakeup_1_bits_vecWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_1_bits_v0Wen = RTL_PATH.io_mem_to_ooo_wakeup_1_bits_v0Wen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_1_bits_vlWen = RTL_PATH.io_mem_to_ooo_wakeup_1_bits_vlWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_1_bits_pdest = RTL_PATH.io_mem_to_ooo_wakeup_1_bits_pdest; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_2_valid = RTL_PATH.io_mem_to_ooo_wakeup_2_valid; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_2_bits_rfWen = RTL_PATH.io_mem_to_ooo_wakeup_2_bits_rfWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_2_bits_fpWen = RTL_PATH.io_mem_to_ooo_wakeup_2_bits_fpWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_2_bits_vecWen = RTL_PATH.io_mem_to_ooo_wakeup_2_bits_vecWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_2_bits_v0Wen = RTL_PATH.io_mem_to_ooo_wakeup_2_bits_v0Wen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_2_bits_vlWen = RTL_PATH.io_mem_to_ooo_wakeup_2_bits_vlWen; \
        force U_IF_NAME.io_mem_to_ooo_wakeup_2_bits_pdest = RTL_PATH.io_mem_to_ooo_wakeup_2_bits_pdest; \
    end \
    `endif

`endif
