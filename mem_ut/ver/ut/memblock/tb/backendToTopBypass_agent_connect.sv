//=========================================================
//File name    : backendToTopBypass_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : backendToTopBypass_agent_connect
//Discribution : backendToTopBypass_agent_connect : backendToTopBypass_agent Interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef BACKENDTOTOPBYPASS_AGENT_CONNECT__SV
`define BACKENDTOTOPBYPASS_AGENT_CONNECT__SV

`define MEMBLOCK__BACKENDTOTOPBYPASS_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    backendToTopBypass_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    initial begin \
        uvm_config_db#(virtual backendToTopBypass_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    `ifdef MEMBLOCK_UT \
    initial begin \
        force RTL_PATH.io_ooo_to_mem_backendToTopBypass_cpuWfi = U_IF_NAME.io_ooo_to_mem_backendToTopBypass_cpuWfi; \
        force RTL_PATH.io_ooo_to_mem_backendToTopBypass_cpuCriticalError = U_IF_NAME.io_ooo_to_mem_backendToTopBypass_cpuCriticalError; \
        force RTL_PATH.io_ooo_to_mem_backendToTopBypass_msiAck = U_IF_NAME.io_ooo_to_mem_backendToTopBypass_msiAck; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.io_ooo_to_mem_backendToTopBypass_cpuWfi = RTL_PATH.io_ooo_to_mem_backendToTopBypass_cpuWfi; \
        force U_IF_NAME.io_ooo_to_mem_backendToTopBypass_cpuCriticalError = RTL_PATH.io_ooo_to_mem_backendToTopBypass_cpuCriticalError; \
        force U_IF_NAME.io_ooo_to_mem_backendToTopBypass_msiAck = RTL_PATH.io_ooo_to_mem_backendToTopBypass_msiAck; \
    end \
    `endif

`endif
