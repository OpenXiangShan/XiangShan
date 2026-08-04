//=========================================================
//File name    : lsqcommit_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : lsqcommit_agent_connect
//Discribution : lsqcommit_agent_connect : lsqcommit_agent Interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef LSQCOMMIT_AGENT_CONNECT__SV
`define LSQCOMMIT_AGENT_CONNECT__SV

`define MEMBLOCK__LSQCOMMIT_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    lsqcommit_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    initial begin \
        uvm_config_db#(virtual lsqcommit_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    `ifdef MEMBLOCK_UT \
    initial begin \
        force RTL_PATH.io_ooo_to_mem_lsqio_pendingPtr_flag = U_IF_NAME.io_ooo_to_mem_lsqio_pendingPtr_flag; \
        force RTL_PATH.io_ooo_to_mem_lsqio_pendingPtr_value = U_IF_NAME.io_ooo_to_mem_lsqio_pendingPtr_value; \
        force RTL_PATH.io_ooo_to_mem_flushSb = U_IF_NAME.io_ooo_to_mem_flushSb; \
        force RTL_PATH.io_ooo_to_mem_lsqio_pendingMMIOld = U_IF_NAME.io_ooo_to_mem_lsqio_pendingMMIOld; \
        force RTL_PATH.io_ooo_to_mem_lsqio_pendingst = U_IF_NAME.io_ooo_to_mem_lsqio_pendingst; \
        force RTL_PATH.io_ooo_to_mem_lsqio_scommit = U_IF_NAME.io_ooo_to_mem_lsqio_scommit; \
        force RTL_PATH.io_ooo_to_mem_isStoreException = U_IF_NAME.io_ooo_to_mem_isStoreException; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.io_ooo_to_mem_lsqio_pendingPtr_flag = RTL_PATH.io_ooo_to_mem_lsqio_pendingPtr_flag; \
        force U_IF_NAME.io_ooo_to_mem_lsqio_pendingPtr_value = RTL_PATH.io_ooo_to_mem_lsqio_pendingPtr_value; \
        force U_IF_NAME.io_ooo_to_mem_flushSb = RTL_PATH.io_ooo_to_mem_flushSb; \
        force U_IF_NAME.io_ooo_to_mem_lsqio_pendingMMIOld = RTL_PATH.io_ooo_to_mem_lsqio_pendingMMIOld; \
        force U_IF_NAME.io_ooo_to_mem_lsqio_pendingst = RTL_PATH.io_ooo_to_mem_lsqio_pendingst; \
        force U_IF_NAME.io_ooo_to_mem_lsqio_scommit = RTL_PATH.io_ooo_to_mem_lsqio_scommit; \
        force U_IF_NAME.io_ooo_to_mem_isStoreException = RTL_PATH.io_ooo_to_mem_isStoreException; \
    end \
    `endif

`endif
