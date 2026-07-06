//=========================================================
//File name    : prefetch_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : prefetch_agent_connect
//Discribution : prefetch_agent_connect : prefetch_agent Interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef PREFETCH_AGENT_CONNECT__SV
`define PREFETCH_AGENT_CONNECT__SV

`define MEMBLOCK__PREFETCH_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    prefetch_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    initial begin \
        uvm_config_db#(virtual prefetch_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    `ifdef MEMBLOCK_UT \
    initial begin \
        force U_IF_NAME.auto_inner_l3_pf_sender_out_addr = RTL_PATH.auto_inner_l3_pf_sender_out_addr; \
        force U_IF_NAME.auto_inner_l3_pf_sender_out_addr_valid = RTL_PATH.auto_inner_l3_pf_sender_out_addr_valid; \
        force U_IF_NAME.auto_inner_l3_pf_sender_out_l2_pf_en = RTL_PATH.auto_inner_l3_pf_sender_out_l2_pf_en; \
        force U_IF_NAME.auto_inner_l2_pf_sender_out_addr = RTL_PATH.auto_inner_l2_pf_sender_out_addr; \
        force U_IF_NAME.auto_inner_l2_pf_sender_out_pf_source = RTL_PATH.auto_inner_l2_pf_sender_out_pf_source; \
        force U_IF_NAME.auto_inner_l2_pf_sender_out_addr_valid = RTL_PATH.auto_inner_l2_pf_sender_out_addr_valid; \
        force U_IF_NAME.auto_inner_l2_pf_sender_out_l2_pf_en = RTL_PATH.auto_inner_l2_pf_sender_out_l2_pf_en; \
        force U_IF_NAME.io_ifetchPrefetch_0_valid = RTL_PATH.io_ifetchPrefetch_0_valid; \
        force U_IF_NAME.io_ifetchPrefetch_0_bits_vaddr = RTL_PATH.io_ifetchPrefetch_0_bits_vaddr; \
        force U_IF_NAME.io_ifetchPrefetch_1_valid = RTL_PATH.io_ifetchPrefetch_1_valid; \
        force U_IF_NAME.io_ifetchPrefetch_1_bits_vaddr = RTL_PATH.io_ifetchPrefetch_1_bits_vaddr; \
        force U_IF_NAME.io_ifetchPrefetch_2_valid = RTL_PATH.io_ifetchPrefetch_2_valid; \
        force U_IF_NAME.io_ifetchPrefetch_2_bits_vaddr = RTL_PATH.io_ifetchPrefetch_2_bits_vaddr; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.auto_inner_l3_pf_sender_out_addr = RTL_PATH.auto_inner_l3_pf_sender_out_addr; \
        force U_IF_NAME.auto_inner_l3_pf_sender_out_addr_valid = RTL_PATH.auto_inner_l3_pf_sender_out_addr_valid; \
        force U_IF_NAME.auto_inner_l3_pf_sender_out_l2_pf_en = RTL_PATH.auto_inner_l3_pf_sender_out_l2_pf_en; \
        force U_IF_NAME.auto_inner_l2_pf_sender_out_addr = RTL_PATH.auto_inner_l2_pf_sender_out_addr; \
        force U_IF_NAME.auto_inner_l2_pf_sender_out_pf_source = RTL_PATH.auto_inner_l2_pf_sender_out_pf_source; \
        force U_IF_NAME.auto_inner_l2_pf_sender_out_addr_valid = RTL_PATH.auto_inner_l2_pf_sender_out_addr_valid; \
        force U_IF_NAME.auto_inner_l2_pf_sender_out_l2_pf_en = RTL_PATH.auto_inner_l2_pf_sender_out_l2_pf_en; \
        force U_IF_NAME.io_ifetchPrefetch_0_valid = RTL_PATH.io_ifetchPrefetch_0_valid; \
        force U_IF_NAME.io_ifetchPrefetch_0_bits_vaddr = RTL_PATH.io_ifetchPrefetch_0_bits_vaddr; \
        force U_IF_NAME.io_ifetchPrefetch_1_valid = RTL_PATH.io_ifetchPrefetch_1_valid; \
        force U_IF_NAME.io_ifetchPrefetch_1_bits_vaddr = RTL_PATH.io_ifetchPrefetch_1_bits_vaddr; \
        force U_IF_NAME.io_ifetchPrefetch_2_valid = RTL_PATH.io_ifetchPrefetch_2_valid; \
        force U_IF_NAME.io_ifetchPrefetch_2_bits_vaddr = RTL_PATH.io_ifetchPrefetch_2_bits_vaddr; \
    end \
    `endif

`endif
