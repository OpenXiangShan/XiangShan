//=========================================================
//File name    : itlb_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : itlb_agent_agent_xaction
//Discribution : itlb_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef ITLB_AGENT_AGENT_XACTION__SV
`define ITLB_AGENT_AGENT_XACTION__SV

class itlb_agent_agent_xaction  extends tcnt_data_base;
    // ITLB/PTW interaction template. The response payload is intentionally kept
    // broad but basic structural legality (levels and booleans) is enforced.
    rand bit io_fetch_to_mem_itlb_req_0_ready;
    rand bit io_fetch_to_mem_itlb_req_0_valid;
    rand bit [37:0] io_fetch_to_mem_itlb_req_0_bits_vpn;
    rand bit [1:0] io_fetch_to_mem_itlb_req_0_bits_s2xlate;
    rand bit io_fetch_to_mem_itlb_resp_ready;
    rand bit io_fetch_to_mem_itlb_resp_valid;
    rand bit [1:0] io_fetch_to_mem_itlb_resp_bits_s2xlate;
    rand bit [34:0] io_fetch_to_mem_itlb_resp_bits_s1_entry_tag;
    rand bit [15:0] io_fetch_to_mem_itlb_resp_bits_s1_entry_asid;
    rand bit [13:0] io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_entry_n;
    rand bit [1:0] io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r;
    rand bit [1:0] io_fetch_to_mem_itlb_resp_bits_s1_entry_level;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_entry_v;
    rand bit [40:0] io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn;
    rand bit [2:0] io_fetch_to_mem_itlb_resp_bits_s1_addr_low;
    rand bit [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0;
    rand bit [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1;
    rand bit [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2;
    rand bit [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3;
    rand bit [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4;
    rand bit [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5;
    rand bit [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6;
    rand bit [2:0] io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_valididx_0;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_valididx_1;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_valididx_2;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_valididx_3;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_valididx_4;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_valididx_5;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_valididx_6;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_valididx_7;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_pf;
    rand bit io_fetch_to_mem_itlb_resp_bits_s1_af;
    rand bit [37:0] io_fetch_to_mem_itlb_resp_bits_s2_entry_tag;
    rand bit [15:0] io_fetch_to_mem_itlb_resp_bits_s2_entry_asid;
    rand bit [13:0] io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid;
    rand bit io_fetch_to_mem_itlb_resp_bits_s2_entry_n;
    rand bit [1:0] io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt;
    rand bit [37:0] io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn;
    rand bit io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d;
    rand bit io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a;
    rand bit io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g;
    rand bit io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u;
    rand bit io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x;
    rand bit io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w;
    rand bit io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r;
    rand bit [1:0] io_fetch_to_mem_itlb_resp_bits_s2_entry_level;
    rand bit io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch;
    rand bit io_fetch_to_mem_itlb_resp_bits_s2_entry_v;
    rand bit io_fetch_to_mem_itlb_resp_bits_s2_gpf;
    rand bit io_fetch_to_mem_itlb_resp_bits_s2_gaf;

    extern constraint default_io_fetch_to_mem_itlb_req_0_ready_cons;
    extern constraint default_io_fetch_to_mem_itlb_req_0_valid_cons;
    extern constraint default_io_fetch_to_mem_itlb_req_0_bits_vpn_cons;
    extern constraint default_io_fetch_to_mem_itlb_req_0_bits_s2xlate_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_ready_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_valid_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2xlate_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_tag_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_asid_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_n_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_level_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_v_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_addr_low_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_0_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_1_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_2_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_3_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_4_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_5_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_6_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_7_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_pf_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s1_af_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_tag_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_asid_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_n_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_level_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_entry_v_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_gpf_cons;
    extern constraint default_io_fetch_to_mem_itlb_resp_bits_s2_gaf_cons;

    extern function new(string name="itlb_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(itlb_agent_agent_xaction)
        `uvm_field_int(io_fetch_to_mem_itlb_req_0_ready, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_req_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_req_0_bits_vpn, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_req_0_bits_s2xlate, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_ready, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_valid, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2xlate, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_tag, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_asid, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_n, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_level, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_v, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_addr_low, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_valididx_0, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_valididx_1, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_valididx_2, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_valididx_3, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_valididx_4, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_valididx_5, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_valididx_6, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_valididx_7, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_pf, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s1_af, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_tag, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_asid, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_n, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_level, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_entry_v, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_gpf, UVM_ALL_ON);
        `uvm_field_int(io_fetch_to_mem_itlb_resp_bits_s2_gaf, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:itlb_agent_agent_xaction

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_req_0_ready_cons{
    io_fetch_to_mem_itlb_req_0_ready inside {1'b0, 1'b1};
}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_req_0_valid_cons{
    io_fetch_to_mem_itlb_req_0_valid inside {1'b0, 1'b1};
}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_req_0_bits_vpn_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_req_0_bits_s2xlate_cons{
    io_fetch_to_mem_itlb_req_0_bits_s2xlate inside {[2'd0:2'd3]};
}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_ready_cons{
    io_fetch_to_mem_itlb_resp_ready inside {1'b0, 1'b1};
}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_valid_cons{
    io_fetch_to_mem_itlb_resp_valid inside {1'b0, 1'b1};
}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2xlate_cons{
    io_fetch_to_mem_itlb_resp_bits_s2xlate inside {[2'd0:2'd3]};
}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_tag_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_asid_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_n_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_level_cons{
    io_fetch_to_mem_itlb_resp_bits_s1_entry_level inside {[2'd0:2'd3]};
}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_v_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_addr_low_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_0_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_1_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_2_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_3_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_4_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_5_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_6_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_valididx_7_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_pf_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s1_af_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_tag_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_asid_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_n_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_level_cons{
    io_fetch_to_mem_itlb_resp_bits_s2_entry_level inside {[2'd0:2'd3]};
}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_entry_v_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_gpf_cons{

}

constraint itlb_agent_agent_xaction::default_io_fetch_to_mem_itlb_resp_bits_s2_gaf_cons{

}

function itlb_agent_agent_xaction::new(string name = "itlb_agent_agent_xaction");
    super.new();
endfunction:new

function void itlb_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void itlb_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void itlb_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void itlb_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string itlb_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_req_0_ready = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_req_0_ready);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_req_0_valid = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_req_0_valid);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_req_0_bits_vpn = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_req_0_bits_vpn);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_req_0_bits_s2xlate = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_req_0_bits_s2xlate);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_ready = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_ready);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_valid = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_valid);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2xlate = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2xlate);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_tag = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_tag);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_asid = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_asid);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_vmid = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_n = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_n);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_level = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_level);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_v = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_v);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_entry_ppn = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_addr_low = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_addr_low);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_valididx_0 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_0);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_valididx_1 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_1);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_valididx_2 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_2);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_valididx_3 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_3);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_valididx_4 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_4);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_valididx_5 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_5);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_valididx_6 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_6);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_valididx_7 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_7);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_pteidx_0 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_pteidx_1 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_pteidx_2 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_pteidx_3 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_pteidx_4 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_pteidx_5 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_pteidx_6 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_pteidx_7 = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_pf = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_pf);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s1_af = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s1_af);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_tag = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_tag);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_asid = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_asid);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_vmid = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_n = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_n);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_ppn = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_level = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_level);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_entry_v = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_entry_v);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_gpf = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_gpf);
    pkt_str = $sformatf("%sio_fetch_to_mem_itlb_resp_bits_s2_gaf = 0x%0h ",pkt_str,this.io_fetch_to_mem_itlb_resp_bits_s2_gaf);

    return pkt_str;
endfunction:psdisplay

function bit itlb_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    itlb_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a itlb_agent_agent_xaction or its extend"))
    end
    super_result = super.compare(rhs_,comparer);
    if(super_result==0) begin
        super_result = 1;
        //foreach(this.pload_q[i]) begin
        //    if(this.pload_q[i]!=rhs_.pload_q[i]) begin
        //        super_result = 0;
        //        `uvm_info(get_type_name(),$sformatf("compare fail for this.pload[%0d]=0x%2h while the rhs_.pload[%0d]=0x%2h",i,this.pload_q[i],i,rhs_.pload_q[i]),UVM_NONE)
        //    end
        //end

        if(this.io_fetch_to_mem_itlb_req_0_ready!=rhs_.io_fetch_to_mem_itlb_req_0_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_req_0_ready=0x%0h while the rhs_.io_fetch_to_mem_itlb_req_0_ready=0x%0h",this.io_fetch_to_mem_itlb_req_0_ready,rhs_.io_fetch_to_mem_itlb_req_0_ready),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_req_0_valid!=rhs_.io_fetch_to_mem_itlb_req_0_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_req_0_valid=0x%0h while the rhs_.io_fetch_to_mem_itlb_req_0_valid=0x%0h",this.io_fetch_to_mem_itlb_req_0_valid,rhs_.io_fetch_to_mem_itlb_req_0_valid),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_req_0_bits_vpn!=rhs_.io_fetch_to_mem_itlb_req_0_bits_vpn) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_req_0_bits_vpn=0x%0h while the rhs_.io_fetch_to_mem_itlb_req_0_bits_vpn=0x%0h",this.io_fetch_to_mem_itlb_req_0_bits_vpn,rhs_.io_fetch_to_mem_itlb_req_0_bits_vpn),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_req_0_bits_s2xlate!=rhs_.io_fetch_to_mem_itlb_req_0_bits_s2xlate) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_req_0_bits_s2xlate=0x%0h while the rhs_.io_fetch_to_mem_itlb_req_0_bits_s2xlate=0x%0h",this.io_fetch_to_mem_itlb_req_0_bits_s2xlate,rhs_.io_fetch_to_mem_itlb_req_0_bits_s2xlate),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_ready!=rhs_.io_fetch_to_mem_itlb_resp_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_ready=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_ready=0x%0h",this.io_fetch_to_mem_itlb_resp_ready,rhs_.io_fetch_to_mem_itlb_resp_ready),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_valid!=rhs_.io_fetch_to_mem_itlb_resp_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_valid=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_valid=0x%0h",this.io_fetch_to_mem_itlb_resp_valid,rhs_.io_fetch_to_mem_itlb_resp_valid),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2xlate!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2xlate) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2xlate=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2xlate=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2xlate,rhs_.io_fetch_to_mem_itlb_resp_bits_s2xlate),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_tag!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_tag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_tag=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_tag=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_tag,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_tag),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_asid!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_asid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_asid=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_asid=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_asid,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_asid),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_vmid),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_n!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_n) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_n=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_n=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_n,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_n),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_pbmt),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_d),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_a),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_g),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_u),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_x),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_w),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_perm_r),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_level!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_level) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_level=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_level=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_level,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_level),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_prefetch),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_v!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_v) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_v=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_v=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_v,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_v),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_entry_ppn),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_addr_low!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_addr_low) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_addr_low=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_addr_low=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_addr_low,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_addr_low),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_0),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_1),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_2),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_3),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_4),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_5),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_6),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_ppn_low_7),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_0!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_0=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_0=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_0,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_0),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_1!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_1) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_1=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_1=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_1,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_1),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_2!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_2) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_2=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_2=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_2,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_2),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_3!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_3) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_3=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_3=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_3,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_3),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_4!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_4) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_4=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_4=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_4,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_4),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_5!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_5) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_5=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_5=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_5,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_5),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_6!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_6) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_6=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_6=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_6,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_6),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_7!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_7) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_7=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_7=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_valididx_7,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_valididx_7),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_0),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_1),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_2),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_3),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_4),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_5),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_6),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pteidx_7),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_pf!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pf) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_pf=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pf=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_pf,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_pf),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s1_af!=rhs_.io_fetch_to_mem_itlb_resp_bits_s1_af) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s1_af=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s1_af=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s1_af,rhs_.io_fetch_to_mem_itlb_resp_bits_s1_af),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_tag!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_tag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_tag=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_tag=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_tag,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_tag),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_asid!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_asid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_asid=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_asid=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_asid,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_asid),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_vmid),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_n!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_n) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_n=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_n=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_n,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_n),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_pbmt),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_ppn),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_d),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_a),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_g),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_u),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_x),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_w),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_perm_r),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_level!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_level) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_level=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_level=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_level,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_level),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_prefetch),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_entry_v!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_v) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_entry_v=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_v=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_entry_v,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_entry_v),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_gpf!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_gpf) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_gpf=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_gpf=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_gpf,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_gpf),UVM_NONE)
        end

        if(this.io_fetch_to_mem_itlb_resp_bits_s2_gaf!=rhs_.io_fetch_to_mem_itlb_resp_bits_s2_gaf) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_fetch_to_mem_itlb_resp_bits_s2_gaf=0x%0h while the rhs_.io_fetch_to_mem_itlb_resp_bits_s2_gaf=0x%0h",this.io_fetch_to_mem_itlb_resp_bits_s2_gaf,rhs_.io_fetch_to_mem_itlb_resp_bits_s2_gaf),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
