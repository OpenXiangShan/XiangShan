//=========================================================
//File name    : itlb_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : itlb_agent_connect
//Discribution : itlb_agent_connect : itlb_agent Interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef ITLB_AGENT_CONNECT__SV
`define ITLB_AGENT_CONNECT__SV

`define MEMBLOCK__ITLB_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    itlb_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    initial begin \
        uvm_config_db#(virtual itlb_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    `ifdef MEMBLOCK_UT \
    initial begin \
        force U_IF_NAME.io_fetch_to_mem_itlb_req_0_ready = RTL_PATH.io_fetch_to_mem_itlb_req_0_ready; \
        force RTL_PATH.io_fetch_to_mem_itlb_req_0_valid = U_IF_NAME.io_fetch_to_mem_itlb_req_0_valid; \
        force RTL_PATH.io_fetch_to_mem_itlb_req_0_bits_vpn = U_IF_NAME.io_fetch_to_mem_itlb_req_0_bits_vpn; \
        force RTL_PATH.io_fetch_to_mem_itlb_req_0_bits_s2xlate = U_IF_NAME.io_fetch_to_mem_itlb_req_0_bits_s2xlate; \
        force RTL_PATH.io_fetch_to_mem_itlb_resp_ready = U_IF_NAME.io_fetch_to_mem_itlb_resp_ready; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_valid = RTL_PATH.io_fetch_to_mem_itlb_resp_valid; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2xlate = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2xlate; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_tag = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_tag; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_asid = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_asid; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_n = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_n; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_level = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_level; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_v = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_v; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_addr_low = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_addr_low; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_0 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_0; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_1 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_1; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_2 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_2; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_3 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_3; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_4 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_4; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_5 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_5; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_6 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_6; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_7 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_7; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pf = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pf; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_af = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_af; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_tag = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_tag; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_asid = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_asid; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_n = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_n; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_level = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_level; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_v = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_v; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_gpf = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_gpf; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_gaf = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_gaf; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.io_fetch_to_mem_itlb_req_0_ready = RTL_PATH.io_fetch_to_mem_itlb_req_0_ready; \
        force U_IF_NAME.io_fetch_to_mem_itlb_req_0_valid = RTL_PATH.io_fetch_to_mem_itlb_req_0_valid; \
        force U_IF_NAME.io_fetch_to_mem_itlb_req_0_bits_vpn = RTL_PATH.io_fetch_to_mem_itlb_req_0_bits_vpn; \
        force U_IF_NAME.io_fetch_to_mem_itlb_req_0_bits_s2xlate = RTL_PATH.io_fetch_to_mem_itlb_req_0_bits_s2xlate; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_ready = RTL_PATH.io_fetch_to_mem_itlb_resp_ready; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_valid = RTL_PATH.io_fetch_to_mem_itlb_resp_valid; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2xlate = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2xlate; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_tag = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_tag; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_asid = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_asid; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_n = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_n; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_level = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_level; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_v = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_v; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_addr_low = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_addr_low; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_0 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_0; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_1 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_1; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_2 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_2; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_3 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_3; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_4 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_4; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_5 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_5; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_6 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_6; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_valididx_7 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_valididx_7; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7 = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_pf = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_pf; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s1_af = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s1_af; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_tag = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_tag; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_asid = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_asid; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_n = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_n; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_level = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_level; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_entry_v = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_entry_v; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_gpf = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_gpf; \
        force U_IF_NAME.io_fetch_to_mem_itlb_resp_bits_s2_gaf = RTL_PATH.io_fetch_to_mem_itlb_resp_bits_s2_gaf; \
    end \
    `endif

`endif
