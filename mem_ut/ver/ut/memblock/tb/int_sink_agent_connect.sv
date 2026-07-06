//=========================================================
//File name    : int_sink_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : int_sink_agent_connect
//Discribution : int_sink_agent_connect : int_sink_agent Interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef INT_SINK_AGENT_CONNECT__SV
`define INT_SINK_AGENT_CONNECT__SV

`define MEMBLOCK__INT_SINK_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    int_sink_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    initial begin \
        uvm_config_db#(virtual int_sink_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    `ifdef MEMBLOCK_UT \
    initial begin \
        force RTL_PATH.auto_inner_beu_local_int_sink_in_0 = U_IF_NAME.auto_inner_beu_local_int_sink_in_0; \
        force RTL_PATH.auto_inner_nmi_int_sink_in_0 = U_IF_NAME.auto_inner_nmi_int_sink_in_0; \
        force RTL_PATH.auto_inner_nmi_int_sink_in_1 = U_IF_NAME.auto_inner_nmi_int_sink_in_1; \
        force RTL_PATH.auto_inner_plic_int_sink_in_1_0 = U_IF_NAME.auto_inner_plic_int_sink_in_1_0; \
        force RTL_PATH.auto_inner_plic_int_sink_in_0_0 = U_IF_NAME.auto_inner_plic_int_sink_in_0_0; \
        force RTL_PATH.auto_inner_clint_int_sink_in_0 = U_IF_NAME.auto_inner_clint_int_sink_in_0; \
        force RTL_PATH.auto_inner_clint_int_sink_in_1 = U_IF_NAME.auto_inner_clint_int_sink_in_1; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.auto_inner_beu_local_int_sink_in_0 = RTL_PATH.auto_inner_beu_local_int_sink_in_0; \
        force U_IF_NAME.auto_inner_nmi_int_sink_in_0 = RTL_PATH.auto_inner_nmi_int_sink_in_0; \
        force U_IF_NAME.auto_inner_nmi_int_sink_in_1 = RTL_PATH.auto_inner_nmi_int_sink_in_1; \
        force U_IF_NAME.auto_inner_plic_int_sink_in_1_0 = RTL_PATH.auto_inner_plic_int_sink_in_1_0; \
        force U_IF_NAME.auto_inner_plic_int_sink_in_0_0 = RTL_PATH.auto_inner_plic_int_sink_in_0_0; \
        force U_IF_NAME.auto_inner_clint_int_sink_in_0 = RTL_PATH.auto_inner_clint_int_sink_in_0; \
        force U_IF_NAME.auto_inner_clint_int_sink_in_1 = RTL_PATH.auto_inner_clint_int_sink_in_1; \
    end \
    `endif

`endif
