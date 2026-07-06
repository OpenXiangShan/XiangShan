//=========================================================
//File name    : dcache_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : dcache_agent_agent_xaction
//Discribution : dcache_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef DCACHE_AGENT_AGENT_XACTION__SV
`define DCACHE_AGENT_AGENT_XACTION__SV

class dcache_agent_agent_xaction  extends tcnt_data_base;
    // This transaction mixes DUT-driven request channels and environment-driven
    // response/backpressure channels. Base constraints only keep protocol
    // template fields inside conservative TileLink-style ranges.
    rand bit auto_inner_dcache_client_out_a_ready;
    rand bit auto_inner_dcache_client_out_a_valid;
    rand bit [3:0] auto_inner_dcache_client_out_a_bits_opcode;
    rand bit [2:0] auto_inner_dcache_client_out_a_bits_param;
    rand bit [2:0] auto_inner_dcache_client_out_a_bits_size;
    rand bit [5:0] auto_inner_dcache_client_out_a_bits_source;
    rand bit [47:0] auto_inner_dcache_client_out_a_bits_address;
    rand bit [1:0] auto_inner_dcache_client_out_a_bits_user_alias;
    rand bit auto_inner_dcache_client_out_a_bits_user_memPageType_NC;
    rand bit auto_inner_dcache_client_out_a_bits_user_memBackType_MM;
    rand bit [43:0] auto_inner_dcache_client_out_a_bits_user_vaddr;
    rand bit [4:0] auto_inner_dcache_client_out_a_bits_user_reqSource;
    rand bit auto_inner_dcache_client_out_a_bits_user_needHint;
    rand bit auto_inner_dcache_client_out_a_bits_echo_isKeyword;
    rand bit [31:0] auto_inner_dcache_client_out_a_bits_mask;
    rand bit [255:0] auto_inner_dcache_client_out_a_bits_data;
    rand bit auto_inner_dcache_client_out_a_bits_corrupt;
    rand bit auto_inner_dcache_client_out_b_ready;
    rand bit auto_inner_dcache_client_out_b_valid;
    rand bit [2:0] auto_inner_dcache_client_out_b_bits_opcode;
    rand bit [1:0] auto_inner_dcache_client_out_b_bits_param;
    rand bit [2:0] auto_inner_dcache_client_out_b_bits_size;
    rand bit [5:0] auto_inner_dcache_client_out_b_bits_source;
    rand bit [47:0] auto_inner_dcache_client_out_b_bits_address;
    rand bit [31:0] auto_inner_dcache_client_out_b_bits_mask;
    rand bit [255:0] auto_inner_dcache_client_out_b_bits_data;
    rand bit auto_inner_dcache_client_out_b_bits_corrupt;
    rand bit auto_inner_dcache_client_out_c_ready;
    rand bit auto_inner_dcache_client_out_c_valid;
    rand bit [2:0] auto_inner_dcache_client_out_c_bits_opcode;
    rand bit [2:0] auto_inner_dcache_client_out_c_bits_param;
    rand bit [2:0] auto_inner_dcache_client_out_c_bits_size;
    rand bit [5:0] auto_inner_dcache_client_out_c_bits_source;
    rand bit [47:0] auto_inner_dcache_client_out_c_bits_address;
    rand bit [1:0] auto_inner_dcache_client_out_c_bits_user_alias;
    rand bit auto_inner_dcache_client_out_c_bits_user_memPageType_NC;
    rand bit auto_inner_dcache_client_out_c_bits_user_memBackType_MM;
    rand bit [43:0] auto_inner_dcache_client_out_c_bits_user_vaddr;
    rand bit [4:0] auto_inner_dcache_client_out_c_bits_user_reqSource;
    rand bit auto_inner_dcache_client_out_c_bits_user_needHint;
    rand bit auto_inner_dcache_client_out_c_bits_echo_isKeyword;
    rand bit [255:0] auto_inner_dcache_client_out_c_bits_data;
    rand bit auto_inner_dcache_client_out_c_bits_corrupt;
    rand bit auto_inner_dcache_client_out_d_ready;
    rand bit auto_inner_dcache_client_out_d_valid;
    rand bit [3:0] auto_inner_dcache_client_out_d_bits_opcode;
    rand bit [1:0] auto_inner_dcache_client_out_d_bits_param;
    rand bit [2:0] auto_inner_dcache_client_out_d_bits_size;
    rand bit [5:0] auto_inner_dcache_client_out_d_bits_source;
    rand bit [9:0] auto_inner_dcache_client_out_d_bits_sink;
    rand bit auto_inner_dcache_client_out_d_bits_denied;
    rand bit auto_inner_dcache_client_out_d_bits_echo_isKeyword;
    rand bit [255:0] auto_inner_dcache_client_out_d_bits_data;
    rand bit auto_inner_dcache_client_out_d_bits_corrupt;
    rand bit auto_inner_dcache_client_out_e_ready;
    rand bit auto_inner_dcache_client_out_e_valid;
    rand bit [9:0] auto_inner_dcache_client_out_e_bits_sink;

    extern constraint default_auto_inner_dcache_client_out_a_ready_cons;
    extern constraint default_auto_inner_dcache_client_out_a_valid_cons;
    extern constraint default_auto_inner_dcache_client_out_a_bits_opcode_cons;
    extern constraint default_auto_inner_dcache_client_out_a_bits_param_cons;
    extern constraint default_auto_inner_dcache_client_out_a_bits_size_cons;
    extern constraint default_auto_inner_dcache_client_out_a_bits_source_cons;
    extern constraint default_auto_inner_dcache_client_out_a_bits_address_cons;
    extern constraint default_auto_inner_dcache_client_out_a_bits_user_alias_cons;
    extern constraint default_auto_inner_dcache_client_out_a_bits_user_memPageType_NC_cons;
    extern constraint default_auto_inner_dcache_client_out_a_bits_user_memBackType_MM_cons;
    extern constraint default_auto_inner_dcache_client_out_a_bits_user_vaddr_cons;
    extern constraint default_auto_inner_dcache_client_out_a_bits_user_reqSource_cons;
    extern constraint default_auto_inner_dcache_client_out_a_bits_user_needHint_cons;
    extern constraint default_auto_inner_dcache_client_out_a_bits_echo_isKeyword_cons;
    extern constraint default_auto_inner_dcache_client_out_a_bits_mask_cons;
    extern constraint default_auto_inner_dcache_client_out_a_bits_data_cons;
    extern constraint default_auto_inner_dcache_client_out_a_bits_corrupt_cons;
    extern constraint default_auto_inner_dcache_client_out_b_ready_cons;
    extern constraint default_auto_inner_dcache_client_out_b_valid_cons;
    extern constraint default_auto_inner_dcache_client_out_b_bits_opcode_cons;
    extern constraint default_auto_inner_dcache_client_out_b_bits_param_cons;
    extern constraint default_auto_inner_dcache_client_out_b_bits_size_cons;
    extern constraint default_auto_inner_dcache_client_out_b_bits_source_cons;
    extern constraint default_auto_inner_dcache_client_out_b_bits_address_cons;
    extern constraint default_auto_inner_dcache_client_out_b_bits_mask_cons;
    extern constraint default_auto_inner_dcache_client_out_b_bits_data_cons;
    extern constraint default_auto_inner_dcache_client_out_b_bits_corrupt_cons;
    extern constraint default_auto_inner_dcache_client_out_c_ready_cons;
    extern constraint default_auto_inner_dcache_client_out_c_valid_cons;
    extern constraint default_auto_inner_dcache_client_out_c_bits_opcode_cons;
    extern constraint default_auto_inner_dcache_client_out_c_bits_param_cons;
    extern constraint default_auto_inner_dcache_client_out_c_bits_size_cons;
    extern constraint default_auto_inner_dcache_client_out_c_bits_source_cons;
    extern constraint default_auto_inner_dcache_client_out_c_bits_address_cons;
    extern constraint default_auto_inner_dcache_client_out_c_bits_user_alias_cons;
    extern constraint default_auto_inner_dcache_client_out_c_bits_user_memPageType_NC_cons;
    extern constraint default_auto_inner_dcache_client_out_c_bits_user_memBackType_MM_cons;
    extern constraint default_auto_inner_dcache_client_out_c_bits_user_vaddr_cons;
    extern constraint default_auto_inner_dcache_client_out_c_bits_user_reqSource_cons;
    extern constraint default_auto_inner_dcache_client_out_c_bits_user_needHint_cons;
    extern constraint default_auto_inner_dcache_client_out_c_bits_echo_isKeyword_cons;
    extern constraint default_auto_inner_dcache_client_out_c_bits_data_cons;
    extern constraint default_auto_inner_dcache_client_out_c_bits_corrupt_cons;
    extern constraint default_auto_inner_dcache_client_out_d_ready_cons;
    extern constraint default_auto_inner_dcache_client_out_d_valid_cons;
    extern constraint default_auto_inner_dcache_client_out_d_bits_opcode_cons;
    extern constraint default_auto_inner_dcache_client_out_d_bits_param_cons;
    extern constraint default_auto_inner_dcache_client_out_d_bits_size_cons;
    extern constraint default_auto_inner_dcache_client_out_d_bits_source_cons;
    extern constraint default_auto_inner_dcache_client_out_d_bits_sink_cons;
    extern constraint default_auto_inner_dcache_client_out_d_bits_denied_cons;
    extern constraint default_auto_inner_dcache_client_out_d_bits_echo_isKeyword_cons;
    extern constraint default_auto_inner_dcache_client_out_d_bits_data_cons;
    extern constraint default_auto_inner_dcache_client_out_d_bits_corrupt_cons;
    extern constraint default_auto_inner_dcache_client_out_e_ready_cons;
    extern constraint default_auto_inner_dcache_client_out_e_valid_cons;
    extern constraint default_auto_inner_dcache_client_out_e_bits_sink_cons;

    extern function new(string name="dcache_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(dcache_agent_agent_xaction)
        `uvm_field_int(auto_inner_dcache_client_out_a_ready, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_valid, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_bits_opcode, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_bits_param, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_bits_size, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_bits_source, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_bits_address, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_bits_user_alias, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_bits_user_memPageType_NC, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_bits_user_memBackType_MM, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_bits_user_vaddr, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_bits_user_reqSource, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_bits_user_needHint, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_bits_echo_isKeyword, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_bits_mask, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_bits_data, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_a_bits_corrupt, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_b_ready, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_b_valid, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_b_bits_opcode, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_b_bits_param, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_b_bits_size, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_b_bits_source, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_b_bits_address, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_b_bits_mask, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_b_bits_data, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_b_bits_corrupt, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_ready, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_valid, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_bits_opcode, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_bits_param, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_bits_size, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_bits_source, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_bits_address, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_bits_user_alias, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_bits_user_memPageType_NC, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_bits_user_memBackType_MM, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_bits_user_vaddr, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_bits_user_reqSource, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_bits_user_needHint, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_bits_echo_isKeyword, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_bits_data, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_c_bits_corrupt, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_d_ready, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_d_valid, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_d_bits_opcode, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_d_bits_param, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_d_bits_size, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_d_bits_source, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_d_bits_sink, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_d_bits_denied, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_d_bits_echo_isKeyword, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_d_bits_data, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_d_bits_corrupt, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_e_ready, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_e_valid, UVM_ALL_ON);
        `uvm_field_int(auto_inner_dcache_client_out_e_bits_sink, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:dcache_agent_agent_xaction

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_ready_cons{
    auto_inner_dcache_client_out_a_ready inside {1'b0, 1'b1};
}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_valid_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_bits_opcode_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_bits_param_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_bits_size_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_bits_source_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_bits_address_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_bits_user_alias_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_bits_user_memPageType_NC_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_bits_user_memBackType_MM_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_bits_user_vaddr_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_bits_user_reqSource_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_bits_user_needHint_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_bits_echo_isKeyword_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_bits_mask_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_bits_data_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_a_bits_corrupt_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_b_ready_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_b_valid_cons{
    auto_inner_dcache_client_out_b_valid inside {1'b0, 1'b1};
}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_b_bits_opcode_cons{
    auto_inner_dcache_client_out_b_bits_opcode inside {[3'd0:3'd7]};
}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_b_bits_param_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_b_bits_size_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_b_bits_source_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_b_bits_address_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_b_bits_mask_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_b_bits_data_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_b_bits_corrupt_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_ready_cons{
    auto_inner_dcache_client_out_c_ready inside {1'b0, 1'b1};
}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_valid_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_bits_opcode_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_bits_param_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_bits_size_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_bits_source_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_bits_address_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_bits_user_alias_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_bits_user_memPageType_NC_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_bits_user_memBackType_MM_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_bits_user_vaddr_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_bits_user_reqSource_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_bits_user_needHint_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_bits_echo_isKeyword_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_bits_data_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_c_bits_corrupt_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_d_ready_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_d_valid_cons{
    auto_inner_dcache_client_out_d_valid inside {1'b0, 1'b1};
}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_d_bits_opcode_cons{
    auto_inner_dcache_client_out_d_bits_opcode inside {[4'd0:4'd15]};
}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_d_bits_param_cons{
    auto_inner_dcache_client_out_d_bits_param inside {[2'd0:2'd3]};
}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_d_bits_size_cons{
    auto_inner_dcache_client_out_d_bits_size inside {[3'd0:3'd7]};
}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_d_bits_source_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_d_bits_sink_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_d_bits_denied_cons{
    auto_inner_dcache_client_out_d_bits_denied inside {1'b0, 1'b1};
}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_d_bits_echo_isKeyword_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_d_bits_data_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_d_bits_corrupt_cons{
    auto_inner_dcache_client_out_d_bits_corrupt inside {1'b0, 1'b1};
}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_e_ready_cons{
    auto_inner_dcache_client_out_e_ready inside {1'b0, 1'b1};
}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_e_valid_cons{

}

constraint dcache_agent_agent_xaction::default_auto_inner_dcache_client_out_e_bits_sink_cons{

}

function dcache_agent_agent_xaction::new(string name = "dcache_agent_agent_xaction");
    super.new();
endfunction:new

function void dcache_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void dcache_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void dcache_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void dcache_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string dcache_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_ready = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_ready);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_valid = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_valid);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_bits_opcode = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_bits_opcode);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_bits_param = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_bits_param);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_bits_size = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_bits_size);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_bits_source = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_bits_source);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_bits_address = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_bits_address);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_bits_user_alias = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_bits_user_alias);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_bits_user_memPageType_NC = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_bits_user_memPageType_NC);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_bits_user_memBackType_MM = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_bits_user_memBackType_MM);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_bits_user_vaddr = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_bits_user_vaddr);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_bits_user_reqSource = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_bits_user_reqSource);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_bits_user_needHint = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_bits_user_needHint);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_bits_echo_isKeyword = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_bits_echo_isKeyword);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_bits_mask = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_bits_mask);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_bits_data = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_bits_data);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_a_bits_corrupt = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_a_bits_corrupt);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_b_ready = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_b_ready);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_b_valid = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_b_valid);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_b_bits_opcode = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_b_bits_opcode);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_b_bits_param = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_b_bits_param);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_b_bits_size = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_b_bits_size);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_b_bits_source = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_b_bits_source);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_b_bits_address = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_b_bits_address);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_b_bits_mask = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_b_bits_mask);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_b_bits_data = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_b_bits_data);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_b_bits_corrupt = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_b_bits_corrupt);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_ready = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_ready);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_valid = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_valid);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_bits_opcode = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_bits_opcode);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_bits_param = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_bits_param);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_bits_size = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_bits_size);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_bits_source = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_bits_source);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_bits_address = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_bits_address);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_bits_user_alias = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_bits_user_alias);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_bits_user_memPageType_NC = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_bits_user_memPageType_NC);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_bits_user_memBackType_MM = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_bits_user_memBackType_MM);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_bits_user_vaddr = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_bits_user_vaddr);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_bits_user_reqSource = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_bits_user_reqSource);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_bits_user_needHint = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_bits_user_needHint);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_bits_echo_isKeyword = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_bits_echo_isKeyword);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_bits_data = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_bits_data);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_c_bits_corrupt = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_c_bits_corrupt);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_d_ready = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_d_ready);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_d_valid = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_d_valid);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_d_bits_opcode = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_d_bits_opcode);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_d_bits_param = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_d_bits_param);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_d_bits_size = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_d_bits_size);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_d_bits_source = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_d_bits_source);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_d_bits_sink = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_d_bits_sink);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_d_bits_denied = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_d_bits_denied);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_d_bits_echo_isKeyword = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_d_bits_echo_isKeyword);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_d_bits_data = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_d_bits_data);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_d_bits_corrupt = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_d_bits_corrupt);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_e_ready = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_e_ready);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_e_valid = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_e_valid);
    pkt_str = $sformatf("%sauto_inner_dcache_client_out_e_bits_sink = 0x%0h ",pkt_str,this.auto_inner_dcache_client_out_e_bits_sink);

    return pkt_str;
endfunction:psdisplay

function bit dcache_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    dcache_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a dcache_agent_agent_xaction or its extend"))
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

        if(this.auto_inner_dcache_client_out_a_ready!=rhs_.auto_inner_dcache_client_out_a_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_ready=0x%0h while the rhs_.auto_inner_dcache_client_out_a_ready=0x%0h",this.auto_inner_dcache_client_out_a_ready,rhs_.auto_inner_dcache_client_out_a_ready),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_valid!=rhs_.auto_inner_dcache_client_out_a_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_valid=0x%0h while the rhs_.auto_inner_dcache_client_out_a_valid=0x%0h",this.auto_inner_dcache_client_out_a_valid,rhs_.auto_inner_dcache_client_out_a_valid),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_bits_opcode!=rhs_.auto_inner_dcache_client_out_a_bits_opcode) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_bits_opcode=0x%0h while the rhs_.auto_inner_dcache_client_out_a_bits_opcode=0x%0h",this.auto_inner_dcache_client_out_a_bits_opcode,rhs_.auto_inner_dcache_client_out_a_bits_opcode),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_bits_param!=rhs_.auto_inner_dcache_client_out_a_bits_param) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_bits_param=0x%0h while the rhs_.auto_inner_dcache_client_out_a_bits_param=0x%0h",this.auto_inner_dcache_client_out_a_bits_param,rhs_.auto_inner_dcache_client_out_a_bits_param),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_bits_size!=rhs_.auto_inner_dcache_client_out_a_bits_size) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_bits_size=0x%0h while the rhs_.auto_inner_dcache_client_out_a_bits_size=0x%0h",this.auto_inner_dcache_client_out_a_bits_size,rhs_.auto_inner_dcache_client_out_a_bits_size),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_bits_source!=rhs_.auto_inner_dcache_client_out_a_bits_source) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_bits_source=0x%0h while the rhs_.auto_inner_dcache_client_out_a_bits_source=0x%0h",this.auto_inner_dcache_client_out_a_bits_source,rhs_.auto_inner_dcache_client_out_a_bits_source),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_bits_address!=rhs_.auto_inner_dcache_client_out_a_bits_address) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_bits_address=0x%0h while the rhs_.auto_inner_dcache_client_out_a_bits_address=0x%0h",this.auto_inner_dcache_client_out_a_bits_address,rhs_.auto_inner_dcache_client_out_a_bits_address),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_bits_user_alias!=rhs_.auto_inner_dcache_client_out_a_bits_user_alias) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_bits_user_alias=0x%0h while the rhs_.auto_inner_dcache_client_out_a_bits_user_alias=0x%0h",this.auto_inner_dcache_client_out_a_bits_user_alias,rhs_.auto_inner_dcache_client_out_a_bits_user_alias),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_bits_user_memPageType_NC!=rhs_.auto_inner_dcache_client_out_a_bits_user_memPageType_NC) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_bits_user_memPageType_NC=0x%0h while the rhs_.auto_inner_dcache_client_out_a_bits_user_memPageType_NC=0x%0h",this.auto_inner_dcache_client_out_a_bits_user_memPageType_NC,rhs_.auto_inner_dcache_client_out_a_bits_user_memPageType_NC),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_bits_user_memBackType_MM!=rhs_.auto_inner_dcache_client_out_a_bits_user_memBackType_MM) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_bits_user_memBackType_MM=0x%0h while the rhs_.auto_inner_dcache_client_out_a_bits_user_memBackType_MM=0x%0h",this.auto_inner_dcache_client_out_a_bits_user_memBackType_MM,rhs_.auto_inner_dcache_client_out_a_bits_user_memBackType_MM),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_bits_user_vaddr!=rhs_.auto_inner_dcache_client_out_a_bits_user_vaddr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_bits_user_vaddr=0x%0h while the rhs_.auto_inner_dcache_client_out_a_bits_user_vaddr=0x%0h",this.auto_inner_dcache_client_out_a_bits_user_vaddr,rhs_.auto_inner_dcache_client_out_a_bits_user_vaddr),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_bits_user_reqSource!=rhs_.auto_inner_dcache_client_out_a_bits_user_reqSource) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_bits_user_reqSource=0x%0h while the rhs_.auto_inner_dcache_client_out_a_bits_user_reqSource=0x%0h",this.auto_inner_dcache_client_out_a_bits_user_reqSource,rhs_.auto_inner_dcache_client_out_a_bits_user_reqSource),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_bits_user_needHint!=rhs_.auto_inner_dcache_client_out_a_bits_user_needHint) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_bits_user_needHint=0x%0h while the rhs_.auto_inner_dcache_client_out_a_bits_user_needHint=0x%0h",this.auto_inner_dcache_client_out_a_bits_user_needHint,rhs_.auto_inner_dcache_client_out_a_bits_user_needHint),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_bits_echo_isKeyword!=rhs_.auto_inner_dcache_client_out_a_bits_echo_isKeyword) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_bits_echo_isKeyword=0x%0h while the rhs_.auto_inner_dcache_client_out_a_bits_echo_isKeyword=0x%0h",this.auto_inner_dcache_client_out_a_bits_echo_isKeyword,rhs_.auto_inner_dcache_client_out_a_bits_echo_isKeyword),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_bits_mask!=rhs_.auto_inner_dcache_client_out_a_bits_mask) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_bits_mask=0x%0h while the rhs_.auto_inner_dcache_client_out_a_bits_mask=0x%0h",this.auto_inner_dcache_client_out_a_bits_mask,rhs_.auto_inner_dcache_client_out_a_bits_mask),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_bits_data!=rhs_.auto_inner_dcache_client_out_a_bits_data) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_bits_data=0x%0h while the rhs_.auto_inner_dcache_client_out_a_bits_data=0x%0h",this.auto_inner_dcache_client_out_a_bits_data,rhs_.auto_inner_dcache_client_out_a_bits_data),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_a_bits_corrupt!=rhs_.auto_inner_dcache_client_out_a_bits_corrupt) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_a_bits_corrupt=0x%0h while the rhs_.auto_inner_dcache_client_out_a_bits_corrupt=0x%0h",this.auto_inner_dcache_client_out_a_bits_corrupt,rhs_.auto_inner_dcache_client_out_a_bits_corrupt),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_b_ready!=rhs_.auto_inner_dcache_client_out_b_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_b_ready=0x%0h while the rhs_.auto_inner_dcache_client_out_b_ready=0x%0h",this.auto_inner_dcache_client_out_b_ready,rhs_.auto_inner_dcache_client_out_b_ready),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_b_valid!=rhs_.auto_inner_dcache_client_out_b_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_b_valid=0x%0h while the rhs_.auto_inner_dcache_client_out_b_valid=0x%0h",this.auto_inner_dcache_client_out_b_valid,rhs_.auto_inner_dcache_client_out_b_valid),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_b_bits_opcode!=rhs_.auto_inner_dcache_client_out_b_bits_opcode) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_b_bits_opcode=0x%0h while the rhs_.auto_inner_dcache_client_out_b_bits_opcode=0x%0h",this.auto_inner_dcache_client_out_b_bits_opcode,rhs_.auto_inner_dcache_client_out_b_bits_opcode),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_b_bits_param!=rhs_.auto_inner_dcache_client_out_b_bits_param) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_b_bits_param=0x%0h while the rhs_.auto_inner_dcache_client_out_b_bits_param=0x%0h",this.auto_inner_dcache_client_out_b_bits_param,rhs_.auto_inner_dcache_client_out_b_bits_param),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_b_bits_size!=rhs_.auto_inner_dcache_client_out_b_bits_size) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_b_bits_size=0x%0h while the rhs_.auto_inner_dcache_client_out_b_bits_size=0x%0h",this.auto_inner_dcache_client_out_b_bits_size,rhs_.auto_inner_dcache_client_out_b_bits_size),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_b_bits_source!=rhs_.auto_inner_dcache_client_out_b_bits_source) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_b_bits_source=0x%0h while the rhs_.auto_inner_dcache_client_out_b_bits_source=0x%0h",this.auto_inner_dcache_client_out_b_bits_source,rhs_.auto_inner_dcache_client_out_b_bits_source),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_b_bits_address!=rhs_.auto_inner_dcache_client_out_b_bits_address) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_b_bits_address=0x%0h while the rhs_.auto_inner_dcache_client_out_b_bits_address=0x%0h",this.auto_inner_dcache_client_out_b_bits_address,rhs_.auto_inner_dcache_client_out_b_bits_address),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_b_bits_mask!=rhs_.auto_inner_dcache_client_out_b_bits_mask) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_b_bits_mask=0x%0h while the rhs_.auto_inner_dcache_client_out_b_bits_mask=0x%0h",this.auto_inner_dcache_client_out_b_bits_mask,rhs_.auto_inner_dcache_client_out_b_bits_mask),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_b_bits_data!=rhs_.auto_inner_dcache_client_out_b_bits_data) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_b_bits_data=0x%0h while the rhs_.auto_inner_dcache_client_out_b_bits_data=0x%0h",this.auto_inner_dcache_client_out_b_bits_data,rhs_.auto_inner_dcache_client_out_b_bits_data),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_b_bits_corrupt!=rhs_.auto_inner_dcache_client_out_b_bits_corrupt) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_b_bits_corrupt=0x%0h while the rhs_.auto_inner_dcache_client_out_b_bits_corrupt=0x%0h",this.auto_inner_dcache_client_out_b_bits_corrupt,rhs_.auto_inner_dcache_client_out_b_bits_corrupt),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_ready!=rhs_.auto_inner_dcache_client_out_c_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_ready=0x%0h while the rhs_.auto_inner_dcache_client_out_c_ready=0x%0h",this.auto_inner_dcache_client_out_c_ready,rhs_.auto_inner_dcache_client_out_c_ready),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_valid!=rhs_.auto_inner_dcache_client_out_c_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_valid=0x%0h while the rhs_.auto_inner_dcache_client_out_c_valid=0x%0h",this.auto_inner_dcache_client_out_c_valid,rhs_.auto_inner_dcache_client_out_c_valid),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_bits_opcode!=rhs_.auto_inner_dcache_client_out_c_bits_opcode) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_bits_opcode=0x%0h while the rhs_.auto_inner_dcache_client_out_c_bits_opcode=0x%0h",this.auto_inner_dcache_client_out_c_bits_opcode,rhs_.auto_inner_dcache_client_out_c_bits_opcode),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_bits_param!=rhs_.auto_inner_dcache_client_out_c_bits_param) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_bits_param=0x%0h while the rhs_.auto_inner_dcache_client_out_c_bits_param=0x%0h",this.auto_inner_dcache_client_out_c_bits_param,rhs_.auto_inner_dcache_client_out_c_bits_param),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_bits_size!=rhs_.auto_inner_dcache_client_out_c_bits_size) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_bits_size=0x%0h while the rhs_.auto_inner_dcache_client_out_c_bits_size=0x%0h",this.auto_inner_dcache_client_out_c_bits_size,rhs_.auto_inner_dcache_client_out_c_bits_size),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_bits_source!=rhs_.auto_inner_dcache_client_out_c_bits_source) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_bits_source=0x%0h while the rhs_.auto_inner_dcache_client_out_c_bits_source=0x%0h",this.auto_inner_dcache_client_out_c_bits_source,rhs_.auto_inner_dcache_client_out_c_bits_source),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_bits_address!=rhs_.auto_inner_dcache_client_out_c_bits_address) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_bits_address=0x%0h while the rhs_.auto_inner_dcache_client_out_c_bits_address=0x%0h",this.auto_inner_dcache_client_out_c_bits_address,rhs_.auto_inner_dcache_client_out_c_bits_address),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_bits_user_alias!=rhs_.auto_inner_dcache_client_out_c_bits_user_alias) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_bits_user_alias=0x%0h while the rhs_.auto_inner_dcache_client_out_c_bits_user_alias=0x%0h",this.auto_inner_dcache_client_out_c_bits_user_alias,rhs_.auto_inner_dcache_client_out_c_bits_user_alias),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_bits_user_memPageType_NC!=rhs_.auto_inner_dcache_client_out_c_bits_user_memPageType_NC) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_bits_user_memPageType_NC=0x%0h while the rhs_.auto_inner_dcache_client_out_c_bits_user_memPageType_NC=0x%0h",this.auto_inner_dcache_client_out_c_bits_user_memPageType_NC,rhs_.auto_inner_dcache_client_out_c_bits_user_memPageType_NC),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_bits_user_memBackType_MM!=rhs_.auto_inner_dcache_client_out_c_bits_user_memBackType_MM) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_bits_user_memBackType_MM=0x%0h while the rhs_.auto_inner_dcache_client_out_c_bits_user_memBackType_MM=0x%0h",this.auto_inner_dcache_client_out_c_bits_user_memBackType_MM,rhs_.auto_inner_dcache_client_out_c_bits_user_memBackType_MM),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_bits_user_vaddr!=rhs_.auto_inner_dcache_client_out_c_bits_user_vaddr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_bits_user_vaddr=0x%0h while the rhs_.auto_inner_dcache_client_out_c_bits_user_vaddr=0x%0h",this.auto_inner_dcache_client_out_c_bits_user_vaddr,rhs_.auto_inner_dcache_client_out_c_bits_user_vaddr),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_bits_user_reqSource!=rhs_.auto_inner_dcache_client_out_c_bits_user_reqSource) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_bits_user_reqSource=0x%0h while the rhs_.auto_inner_dcache_client_out_c_bits_user_reqSource=0x%0h",this.auto_inner_dcache_client_out_c_bits_user_reqSource,rhs_.auto_inner_dcache_client_out_c_bits_user_reqSource),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_bits_user_needHint!=rhs_.auto_inner_dcache_client_out_c_bits_user_needHint) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_bits_user_needHint=0x%0h while the rhs_.auto_inner_dcache_client_out_c_bits_user_needHint=0x%0h",this.auto_inner_dcache_client_out_c_bits_user_needHint,rhs_.auto_inner_dcache_client_out_c_bits_user_needHint),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_bits_echo_isKeyword!=rhs_.auto_inner_dcache_client_out_c_bits_echo_isKeyword) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_bits_echo_isKeyword=0x%0h while the rhs_.auto_inner_dcache_client_out_c_bits_echo_isKeyword=0x%0h",this.auto_inner_dcache_client_out_c_bits_echo_isKeyword,rhs_.auto_inner_dcache_client_out_c_bits_echo_isKeyword),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_bits_data!=rhs_.auto_inner_dcache_client_out_c_bits_data) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_bits_data=0x%0h while the rhs_.auto_inner_dcache_client_out_c_bits_data=0x%0h",this.auto_inner_dcache_client_out_c_bits_data,rhs_.auto_inner_dcache_client_out_c_bits_data),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_c_bits_corrupt!=rhs_.auto_inner_dcache_client_out_c_bits_corrupt) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_c_bits_corrupt=0x%0h while the rhs_.auto_inner_dcache_client_out_c_bits_corrupt=0x%0h",this.auto_inner_dcache_client_out_c_bits_corrupt,rhs_.auto_inner_dcache_client_out_c_bits_corrupt),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_d_ready!=rhs_.auto_inner_dcache_client_out_d_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_d_ready=0x%0h while the rhs_.auto_inner_dcache_client_out_d_ready=0x%0h",this.auto_inner_dcache_client_out_d_ready,rhs_.auto_inner_dcache_client_out_d_ready),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_d_valid!=rhs_.auto_inner_dcache_client_out_d_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_d_valid=0x%0h while the rhs_.auto_inner_dcache_client_out_d_valid=0x%0h",this.auto_inner_dcache_client_out_d_valid,rhs_.auto_inner_dcache_client_out_d_valid),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_d_bits_opcode!=rhs_.auto_inner_dcache_client_out_d_bits_opcode) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_d_bits_opcode=0x%0h while the rhs_.auto_inner_dcache_client_out_d_bits_opcode=0x%0h",this.auto_inner_dcache_client_out_d_bits_opcode,rhs_.auto_inner_dcache_client_out_d_bits_opcode),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_d_bits_param!=rhs_.auto_inner_dcache_client_out_d_bits_param) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_d_bits_param=0x%0h while the rhs_.auto_inner_dcache_client_out_d_bits_param=0x%0h",this.auto_inner_dcache_client_out_d_bits_param,rhs_.auto_inner_dcache_client_out_d_bits_param),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_d_bits_size!=rhs_.auto_inner_dcache_client_out_d_bits_size) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_d_bits_size=0x%0h while the rhs_.auto_inner_dcache_client_out_d_bits_size=0x%0h",this.auto_inner_dcache_client_out_d_bits_size,rhs_.auto_inner_dcache_client_out_d_bits_size),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_d_bits_source!=rhs_.auto_inner_dcache_client_out_d_bits_source) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_d_bits_source=0x%0h while the rhs_.auto_inner_dcache_client_out_d_bits_source=0x%0h",this.auto_inner_dcache_client_out_d_bits_source,rhs_.auto_inner_dcache_client_out_d_bits_source),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_d_bits_sink!=rhs_.auto_inner_dcache_client_out_d_bits_sink) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_d_bits_sink=0x%0h while the rhs_.auto_inner_dcache_client_out_d_bits_sink=0x%0h",this.auto_inner_dcache_client_out_d_bits_sink,rhs_.auto_inner_dcache_client_out_d_bits_sink),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_d_bits_denied!=rhs_.auto_inner_dcache_client_out_d_bits_denied) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_d_bits_denied=0x%0h while the rhs_.auto_inner_dcache_client_out_d_bits_denied=0x%0h",this.auto_inner_dcache_client_out_d_bits_denied,rhs_.auto_inner_dcache_client_out_d_bits_denied),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_d_bits_echo_isKeyword!=rhs_.auto_inner_dcache_client_out_d_bits_echo_isKeyword) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_d_bits_echo_isKeyword=0x%0h while the rhs_.auto_inner_dcache_client_out_d_bits_echo_isKeyword=0x%0h",this.auto_inner_dcache_client_out_d_bits_echo_isKeyword,rhs_.auto_inner_dcache_client_out_d_bits_echo_isKeyword),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_d_bits_data!=rhs_.auto_inner_dcache_client_out_d_bits_data) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_d_bits_data=0x%0h while the rhs_.auto_inner_dcache_client_out_d_bits_data=0x%0h",this.auto_inner_dcache_client_out_d_bits_data,rhs_.auto_inner_dcache_client_out_d_bits_data),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_d_bits_corrupt!=rhs_.auto_inner_dcache_client_out_d_bits_corrupt) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_d_bits_corrupt=0x%0h while the rhs_.auto_inner_dcache_client_out_d_bits_corrupt=0x%0h",this.auto_inner_dcache_client_out_d_bits_corrupt,rhs_.auto_inner_dcache_client_out_d_bits_corrupt),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_e_ready!=rhs_.auto_inner_dcache_client_out_e_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_e_ready=0x%0h while the rhs_.auto_inner_dcache_client_out_e_ready=0x%0h",this.auto_inner_dcache_client_out_e_ready,rhs_.auto_inner_dcache_client_out_e_ready),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_e_valid!=rhs_.auto_inner_dcache_client_out_e_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_e_valid=0x%0h while the rhs_.auto_inner_dcache_client_out_e_valid=0x%0h",this.auto_inner_dcache_client_out_e_valid,rhs_.auto_inner_dcache_client_out_e_valid),UVM_NONE)
        end

        if(this.auto_inner_dcache_client_out_e_bits_sink!=rhs_.auto_inner_dcache_client_out_e_bits_sink) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_dcache_client_out_e_bits_sink=0x%0h while the rhs_.auto_inner_dcache_client_out_e_bits_sink=0x%0h",this.auto_inner_dcache_client_out_e_bits_sink,rhs_.auto_inner_dcache_client_out_e_bits_sink),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
