//=========================================================
//File name    : L2tlb_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : L2tlb_agent_connect
//Discribution : L2tlb_agent_connect : L2TLB/PTW interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef L2TLB_AGENT_CONNECT__SV
`define L2TLB_AGENT_CONNECT__SV

`define MEMBLOCK__L2TLB_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    L2tlb_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    bit U_IF_NAME``_l2tlb_active; \
    initial begin \
        uvm_config_db#(virtual L2tlb_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    initial begin \
        U_IF_NAME``_l2tlb_active = (`MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN != 0); \
        memblock_sync_pkg::l2tlb_responder_active = U_IF_NAME``_l2tlb_active; \
        U_IF_NAME.io_ptw_req_0_ready = '0; \
        U_IF_NAME.io_ptw_resp_valid = '0; \
        U_IF_NAME.io_ptw_resp_bits_s2xlate = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_entry_tag = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_entry_asid = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_entry_vmid = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_entry_n = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_entry_pbmt = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_d = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_a = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_g = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_u = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_x = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_w = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_r = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_entry_level = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_entry_v = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_entry_ppn = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_addr_low = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_0 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_1 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_2 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_3 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_4 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_5 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_6 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_7 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_valididx_0 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_valididx_1 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_valididx_2 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_valididx_3 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_valididx_4 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_valididx_5 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_valididx_6 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_valididx_7 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_pteidx_0 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_pteidx_1 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_pteidx_2 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_pteidx_3 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_pteidx_4 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_pteidx_5 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_pteidx_6 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_pteidx_7 = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_pf = '0; \
        U_IF_NAME.io_ptw_resp_bits_s1_af = '0; \
        U_IF_NAME.io_ptw_resp_bits_s2_entry_tag = '0; \
        U_IF_NAME.io_ptw_resp_bits_s2_entry_vmid = '0; \
        U_IF_NAME.io_ptw_resp_bits_s2_entry_n = '0; \
        U_IF_NAME.io_ptw_resp_bits_s2_entry_pbmt = '0; \
        U_IF_NAME.io_ptw_resp_bits_s2_entry_ppn = '0; \
        U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_d = '0; \
        U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_a = '0; \
        U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_x = '0; \
        U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_w = '0; \
        U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_r = '0; \
        U_IF_NAME.io_ptw_resp_bits_s2_entry_level = '0; \
        U_IF_NAME.io_ptw_resp_bits_s2_gpf = '0; \
        U_IF_NAME.io_ptw_resp_bits_s2_gaf = '0; \
        $display("[L2TLB_CONNECT] active=%0d MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN=%0d", \
                 U_IF_NAME``_l2tlb_active, \
                 `MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN); \
        force U_IF_NAME.io_ptw_req_0_valid = RTL_PATH._inner_dtlbRepeater_io_ptw_req_0_valid; \
        force U_IF_NAME.io_ptw_req_0_bits_vpn = RTL_PATH._inner_dtlbRepeater_io_ptw_req_0_bits_vpn; \
        force U_IF_NAME.io_ptw_req_0_bits_s2xlate = RTL_PATH._inner_dtlbRepeater_io_ptw_req_0_bits_s2xlate; \
        if(U_IF_NAME``_l2tlb_active) begin \
            force RTL_PATH._inner_ptw_io_tlb_1_req_0_ready = U_IF_NAME.io_ptw_req_0_ready; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_valid = U_IF_NAME.io_ptw_resp_valid; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2xlate = U_IF_NAME.io_ptw_resp_bits_s2xlate; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_tag = U_IF_NAME.io_ptw_resp_bits_s1_entry_tag; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_asid = U_IF_NAME.io_ptw_resp_bits_s1_entry_asid; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_vmid = U_IF_NAME.io_ptw_resp_bits_s1_entry_vmid; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_n = U_IF_NAME.io_ptw_resp_bits_s1_entry_n; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_pbmt = U_IF_NAME.io_ptw_resp_bits_s1_entry_pbmt; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_d = U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_d; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_a = U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_a; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_g = U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_g; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_u = U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_u; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_x = U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_x; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_w = U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_w; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_r = U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_r; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_level = U_IF_NAME.io_ptw_resp_bits_s1_entry_level; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_v = U_IF_NAME.io_ptw_resp_bits_s1_entry_v; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_ppn = U_IF_NAME.io_ptw_resp_bits_s1_entry_ppn; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_addr_low = U_IF_NAME.io_ptw_resp_bits_s1_addr_low; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_0 = U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_0; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_1 = U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_1; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_2 = U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_2; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_3 = U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_3; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_4 = U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_4; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_5 = U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_5; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_6 = U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_6; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_7 = U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_7; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_0 = U_IF_NAME.io_ptw_resp_bits_s1_valididx_0; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_1 = U_IF_NAME.io_ptw_resp_bits_s1_valididx_1; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_2 = U_IF_NAME.io_ptw_resp_bits_s1_valididx_2; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_3 = U_IF_NAME.io_ptw_resp_bits_s1_valididx_3; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_4 = U_IF_NAME.io_ptw_resp_bits_s1_valididx_4; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_5 = U_IF_NAME.io_ptw_resp_bits_s1_valididx_5; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_6 = U_IF_NAME.io_ptw_resp_bits_s1_valididx_6; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_7 = U_IF_NAME.io_ptw_resp_bits_s1_valididx_7; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_0 = U_IF_NAME.io_ptw_resp_bits_s1_pteidx_0; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_1 = U_IF_NAME.io_ptw_resp_bits_s1_pteidx_1; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_2 = U_IF_NAME.io_ptw_resp_bits_s1_pteidx_2; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_3 = U_IF_NAME.io_ptw_resp_bits_s1_pteidx_3; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_4 = U_IF_NAME.io_ptw_resp_bits_s1_pteidx_4; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_5 = U_IF_NAME.io_ptw_resp_bits_s1_pteidx_5; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_6 = U_IF_NAME.io_ptw_resp_bits_s1_pteidx_6; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_7 = U_IF_NAME.io_ptw_resp_bits_s1_pteidx_7; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pf = U_IF_NAME.io_ptw_resp_bits_s1_pf; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_af = U_IF_NAME.io_ptw_resp_bits_s1_af; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_tag = U_IF_NAME.io_ptw_resp_bits_s2_entry_tag; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_vmid = U_IF_NAME.io_ptw_resp_bits_s2_entry_vmid; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_n = U_IF_NAME.io_ptw_resp_bits_s2_entry_n; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_pbmt = U_IF_NAME.io_ptw_resp_bits_s2_entry_pbmt; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_ppn = U_IF_NAME.io_ptw_resp_bits_s2_entry_ppn; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_d = U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_d; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_a = U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_a; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_x = U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_x; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_w = U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_w; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_r = U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_r; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_level = U_IF_NAME.io_ptw_resp_bits_s2_entry_level; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_gpf = U_IF_NAME.io_ptw_resp_bits_s2_gpf; \
            force RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_gaf = U_IF_NAME.io_ptw_resp_bits_s2_gaf; \
        end else begin \
            force U_IF_NAME.io_ptw_req_0_ready = RTL_PATH._inner_ptw_io_tlb_1_req_0_ready; \
            force U_IF_NAME.io_ptw_resp_valid = RTL_PATH._inner_ptw_io_tlb_1_resp_valid; \
            force U_IF_NAME.io_ptw_resp_bits_s2xlate = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2xlate; \
            force U_IF_NAME.io_ptw_resp_bits_s1_entry_tag = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_tag; \
            force U_IF_NAME.io_ptw_resp_bits_s1_entry_asid = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_asid; \
            force U_IF_NAME.io_ptw_resp_bits_s1_entry_vmid = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_vmid; \
            force U_IF_NAME.io_ptw_resp_bits_s1_entry_n = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_n; \
            force U_IF_NAME.io_ptw_resp_bits_s1_entry_pbmt = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_pbmt; \
            force U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_d = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_d; \
            force U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_a = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_a; \
            force U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_g = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_g; \
            force U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_u = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_u; \
            force U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_x = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_x; \
            force U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_w = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_w; \
            force U_IF_NAME.io_ptw_resp_bits_s1_entry_perm_r = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_perm_r; \
            force U_IF_NAME.io_ptw_resp_bits_s1_entry_level = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_level; \
            force U_IF_NAME.io_ptw_resp_bits_s1_entry_v = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_v; \
            force U_IF_NAME.io_ptw_resp_bits_s1_entry_ppn = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_entry_ppn; \
            force U_IF_NAME.io_ptw_resp_bits_s1_addr_low = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_addr_low; \
            force U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_0 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_0; \
            force U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_1 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_1; \
            force U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_2 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_2; \
            force U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_3 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_3; \
            force U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_4 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_4; \
            force U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_5 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_5; \
            force U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_6 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_6; \
            force U_IF_NAME.io_ptw_resp_bits_s1_ppn_low_7 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_ppn_low_7; \
            force U_IF_NAME.io_ptw_resp_bits_s1_valididx_0 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_0; \
            force U_IF_NAME.io_ptw_resp_bits_s1_valididx_1 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_1; \
            force U_IF_NAME.io_ptw_resp_bits_s1_valididx_2 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_2; \
            force U_IF_NAME.io_ptw_resp_bits_s1_valididx_3 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_3; \
            force U_IF_NAME.io_ptw_resp_bits_s1_valididx_4 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_4; \
            force U_IF_NAME.io_ptw_resp_bits_s1_valididx_5 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_5; \
            force U_IF_NAME.io_ptw_resp_bits_s1_valididx_6 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_6; \
            force U_IF_NAME.io_ptw_resp_bits_s1_valididx_7 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_valididx_7; \
            force U_IF_NAME.io_ptw_resp_bits_s1_pteidx_0 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_0; \
            force U_IF_NAME.io_ptw_resp_bits_s1_pteidx_1 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_1; \
            force U_IF_NAME.io_ptw_resp_bits_s1_pteidx_2 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_2; \
            force U_IF_NAME.io_ptw_resp_bits_s1_pteidx_3 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_3; \
            force U_IF_NAME.io_ptw_resp_bits_s1_pteidx_4 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_4; \
            force U_IF_NAME.io_ptw_resp_bits_s1_pteidx_5 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_5; \
            force U_IF_NAME.io_ptw_resp_bits_s1_pteidx_6 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_6; \
            force U_IF_NAME.io_ptw_resp_bits_s1_pteidx_7 = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pteidx_7; \
            force U_IF_NAME.io_ptw_resp_bits_s1_pf = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_pf; \
            force U_IF_NAME.io_ptw_resp_bits_s1_af = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s1_af; \
            force U_IF_NAME.io_ptw_resp_bits_s2_entry_tag = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_tag; \
            force U_IF_NAME.io_ptw_resp_bits_s2_entry_vmid = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_vmid; \
            force U_IF_NAME.io_ptw_resp_bits_s2_entry_n = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_n; \
            force U_IF_NAME.io_ptw_resp_bits_s2_entry_pbmt = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_pbmt; \
            force U_IF_NAME.io_ptw_resp_bits_s2_entry_ppn = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_ppn; \
            force U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_d = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_d; \
            force U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_a = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_a; \
            force U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_x = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_x; \
            force U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_w = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_w; \
            force U_IF_NAME.io_ptw_resp_bits_s2_entry_perm_r = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_r; \
            force U_IF_NAME.io_ptw_resp_bits_s2_entry_level = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_entry_level; \
            force U_IF_NAME.io_ptw_resp_bits_s2_gpf = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_gpf; \
            force U_IF_NAME.io_ptw_resp_bits_s2_gaf = RTL_PATH._inner_ptw_io_tlb_1_resp_bits_s2_gaf; \
        end \
    end

`endif
