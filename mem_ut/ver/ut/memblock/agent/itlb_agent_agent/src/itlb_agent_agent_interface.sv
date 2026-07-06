//=========================================================
//File name    : itlb_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : itlb_agent_agent_interface
//Discribution : itlb_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef ITLB_AGENT_AGENT_INTERFACE__SV
`define ITLB_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface itlb_agent_agent_interface  (input bit clk,input bit rst_n);

    logic io_fetch_to_mem_itlb_req_0_ready;
    logic io_fetch_to_mem_itlb_req_0_valid;
    logic [37:0] io_fetch_to_mem_itlb_req_0_bits_vpn;
    logic [1:0] io_fetch_to_mem_itlb_req_0_bits_s2xlate;
    logic io_fetch_to_mem_itlb_resp_ready;
    logic io_fetch_to_mem_itlb_resp_valid;
    logic [1:0] io_fetch_to_mem_itlb_resp_bits_s2xlate;
    logic [34:0] io_fetch_to_mem_itlb_resp_bits_s1_entry_tag;
    logic [15:0] io_fetch_to_mem_itlb_resp_bits_s1_entry_asid;
    logic [13:0] io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid;
    logic io_fetch_to_mem_itlb_resp_bits_s1_entry_n;
    logic [1:0] io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt;
    logic io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d;
    logic io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a;
    logic io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g;
    logic io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u;
    logic io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x;
    logic io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w;
    logic io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r;
    logic [1:0] io_fetch_to_mem_itlb_resp_bits_s1_entry_level;
    logic io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch;
    logic io_fetch_to_mem_itlb_resp_bits_s1_entry_v;
    logic [40:0] io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn;
    logic [2:0] io_fetch_to_mem_itlb_resp_bits_s1_addr_low;
    logic [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0;
    logic [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1;
    logic [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2;
    logic [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3;
    logic [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4;
    logic [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5;
    logic [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6;
    logic [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7;
    logic io_fetch_to_mem_itlb_resp_bits_s1_valididx_0;
    logic io_fetch_to_mem_itlb_resp_bits_s1_valididx_1;
    logic io_fetch_to_mem_itlb_resp_bits_s1_valididx_2;
    logic io_fetch_to_mem_itlb_resp_bits_s1_valididx_3;
    logic io_fetch_to_mem_itlb_resp_bits_s1_valididx_4;
    logic io_fetch_to_mem_itlb_resp_bits_s1_valididx_5;
    logic io_fetch_to_mem_itlb_resp_bits_s1_valididx_6;
    logic io_fetch_to_mem_itlb_resp_bits_s1_valididx_7;
    logic io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0;
    logic io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1;
    logic io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2;
    logic io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3;
    logic io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4;
    logic io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5;
    logic io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6;
    logic io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7;
    logic io_fetch_to_mem_itlb_resp_bits_s1_pf;
    logic io_fetch_to_mem_itlb_resp_bits_s1_af;
    logic [37:0] io_fetch_to_mem_itlb_resp_bits_s2_entry_tag;
    logic [15:0] io_fetch_to_mem_itlb_resp_bits_s2_entry_asid;
    logic [13:0] io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid;
    logic io_fetch_to_mem_itlb_resp_bits_s2_entry_n;
    logic [1:0] io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt;
    logic [37:0] io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn;
    logic io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d;
    logic io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a;
    logic io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g;
    logic io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u;
    logic io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x;
    logic io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w;
    logic io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r;
    logic [1:0] io_fetch_to_mem_itlb_resp_bits_s2_entry_level;
    logic io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch;
    logic io_fetch_to_mem_itlb_resp_bits_s2_entry_v;
    logic io_fetch_to_mem_itlb_resp_bits_s2_gpf;
    logic io_fetch_to_mem_itlb_resp_bits_s2_gaf;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_fetch_to_mem_itlb_req_0_ready;
        output io_fetch_to_mem_itlb_req_0_valid;
        output io_fetch_to_mem_itlb_req_0_bits_vpn;
        output io_fetch_to_mem_itlb_req_0_bits_s2xlate;
        output io_fetch_to_mem_itlb_resp_ready;
        input  io_fetch_to_mem_itlb_resp_valid;
        input  io_fetch_to_mem_itlb_resp_bits_s2xlate;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_tag;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_asid;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_n;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_level;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_v;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn;
        input  io_fetch_to_mem_itlb_resp_bits_s1_addr_low;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_0;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_1;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_2;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_3;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_4;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_5;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_6;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_7;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pf;
        input  io_fetch_to_mem_itlb_resp_bits_s1_af;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_tag;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_asid;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_n;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_level;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_v;
        input  io_fetch_to_mem_itlb_resp_bits_s2_gpf;
        input  io_fetch_to_mem_itlb_resp_bits_s2_gaf;

    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_fetch_to_mem_itlb_req_0_ready;
        input  io_fetch_to_mem_itlb_req_0_valid;
        input  io_fetch_to_mem_itlb_req_0_bits_vpn;
        input  io_fetch_to_mem_itlb_req_0_bits_s2xlate;
        input  io_fetch_to_mem_itlb_resp_ready;
        input  io_fetch_to_mem_itlb_resp_valid;
        input  io_fetch_to_mem_itlb_resp_bits_s2xlate;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_tag;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_asid;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_n;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_level;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_v;
        input  io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn;
        input  io_fetch_to_mem_itlb_resp_bits_s1_addr_low;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6;
        input  io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_0;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_1;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_2;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_3;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_4;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_5;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_6;
        input  io_fetch_to_mem_itlb_resp_bits_s1_valididx_7;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7;
        input  io_fetch_to_mem_itlb_resp_bits_s1_pf;
        input  io_fetch_to_mem_itlb_resp_bits_s1_af;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_tag;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_asid;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_n;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_level;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch;
        input  io_fetch_to_mem_itlb_resp_bits_s2_entry_v;
        input  io_fetch_to_mem_itlb_resp_bits_s2_gpf;
        input  io_fetch_to_mem_itlb_resp_bits_s2_gaf;

    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:itlb_agent_agent_interface

`endif
