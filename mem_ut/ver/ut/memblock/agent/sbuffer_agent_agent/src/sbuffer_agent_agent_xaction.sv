//=========================================================
//File name    : sbuffer_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : sbuffer_agent_agent_xaction
//Discribution : sbuffer_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef SBUFFER_AGENT_AGENT_XACTION__SV
`define SBUFFER_AGENT_AGENT_XACTION__SV

class sbuffer_agent_agent_xaction  extends tcnt_data_base;
    // Similar to dcache_agent, this transaction is half DUT-driven and half
    // environment-driven. Base constraints here only keep response fields
    // inside conservative protocol ranges.
    rand bit auto_inner_buffers_out_a_ready;
    rand bit auto_inner_buffers_out_a_valid;
    rand bit [3:0] auto_inner_buffers_out_a_bits_opcode;
    rand bit [2:0] auto_inner_buffers_out_a_bits_param;
    rand bit [2:0] auto_inner_buffers_out_a_bits_size;
    rand bit [3:0] auto_inner_buffers_out_a_bits_source;
    rand bit [47:0] auto_inner_buffers_out_a_bits_address;
    rand bit auto_inner_buffers_out_a_bits_user_memBackType_MM;
    rand bit auto_inner_buffers_out_a_bits_user_memPageType_NC;
    rand bit [7:0] auto_inner_buffers_out_a_bits_mask;
    rand bit [63:0] auto_inner_buffers_out_a_bits_data;
    rand bit auto_inner_buffers_out_a_bits_corrupt;
    rand bit auto_inner_buffers_out_d_ready;
    rand bit auto_inner_buffers_out_d_valid;
    rand bit [3:0] auto_inner_buffers_out_d_bits_opcode;
    rand bit [1:0] auto_inner_buffers_out_d_bits_param;
    rand bit [2:0] auto_inner_buffers_out_d_bits_size;
    rand bit [3:0] auto_inner_buffers_out_d_bits_source;
    rand bit auto_inner_buffers_out_d_bits_sink;
    rand bit auto_inner_buffers_out_d_bits_denied;
    rand bit [63:0] auto_inner_buffers_out_d_bits_data;
    rand bit auto_inner_buffers_out_d_bits_corrupt;

    extern constraint default_auto_inner_buffers_out_a_ready_cons;
    extern constraint default_auto_inner_buffers_out_a_valid_cons;
    extern constraint default_auto_inner_buffers_out_a_bits_opcode_cons;
    extern constraint default_auto_inner_buffers_out_a_bits_param_cons;
    extern constraint default_auto_inner_buffers_out_a_bits_size_cons;
    extern constraint default_auto_inner_buffers_out_a_bits_source_cons;
    extern constraint default_auto_inner_buffers_out_a_bits_address_cons;
    extern constraint default_auto_inner_buffers_out_a_bits_user_memBackType_MM_cons;
    extern constraint default_auto_inner_buffers_out_a_bits_user_memPageType_NC_cons;
    extern constraint default_auto_inner_buffers_out_a_bits_mask_cons;
    extern constraint default_auto_inner_buffers_out_a_bits_data_cons;
    extern constraint default_auto_inner_buffers_out_a_bits_corrupt_cons;
    extern constraint default_auto_inner_buffers_out_d_ready_cons;
    extern constraint default_auto_inner_buffers_out_d_valid_cons;
    extern constraint default_auto_inner_buffers_out_d_bits_opcode_cons;
    extern constraint default_auto_inner_buffers_out_d_bits_param_cons;
    extern constraint default_auto_inner_buffers_out_d_bits_size_cons;
    extern constraint default_auto_inner_buffers_out_d_bits_source_cons;
    extern constraint default_auto_inner_buffers_out_d_bits_sink_cons;
    extern constraint default_auto_inner_buffers_out_d_bits_denied_cons;
    extern constraint default_auto_inner_buffers_out_d_bits_data_cons;
    extern constraint default_auto_inner_buffers_out_d_bits_corrupt_cons;

    extern function new(string name="sbuffer_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(sbuffer_agent_agent_xaction)
        `uvm_field_int(auto_inner_buffers_out_a_ready, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_a_valid, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_a_bits_opcode, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_a_bits_param, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_a_bits_size, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_a_bits_source, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_a_bits_address, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_a_bits_user_memBackType_MM, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_a_bits_user_memPageType_NC, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_a_bits_mask, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_a_bits_data, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_a_bits_corrupt, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_d_ready, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_d_valid, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_d_bits_opcode, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_d_bits_param, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_d_bits_size, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_d_bits_source, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_d_bits_sink, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_d_bits_denied, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_d_bits_data, UVM_ALL_ON);
        `uvm_field_int(auto_inner_buffers_out_d_bits_corrupt, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:sbuffer_agent_agent_xaction

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_a_ready_cons{
    auto_inner_buffers_out_a_ready inside {1'b0, 1'b1};
}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_a_valid_cons{

}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_a_bits_opcode_cons{

}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_a_bits_param_cons{

}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_a_bits_size_cons{

}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_a_bits_source_cons{

}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_a_bits_address_cons{

}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_a_bits_user_memBackType_MM_cons{

}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_a_bits_user_memPageType_NC_cons{

}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_a_bits_mask_cons{

}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_a_bits_data_cons{

}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_a_bits_corrupt_cons{

}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_d_ready_cons{

}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_d_valid_cons{
    auto_inner_buffers_out_d_valid inside {1'b0, 1'b1};
}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_d_bits_opcode_cons{
    auto_inner_buffers_out_d_bits_opcode inside {[4'd0:4'd15]};
}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_d_bits_param_cons{
    auto_inner_buffers_out_d_bits_param inside {[2'd0:2'd3]};
}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_d_bits_size_cons{
    auto_inner_buffers_out_d_bits_size inside {[3'd0:3'd7]};
}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_d_bits_source_cons{

}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_d_bits_sink_cons{

}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_d_bits_denied_cons{
    auto_inner_buffers_out_d_bits_denied inside {1'b0, 1'b1};
}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_d_bits_data_cons{

}

constraint sbuffer_agent_agent_xaction::default_auto_inner_buffers_out_d_bits_corrupt_cons{
    auto_inner_buffers_out_d_bits_corrupt inside {1'b0, 1'b1};
}

function sbuffer_agent_agent_xaction::new(string name = "sbuffer_agent_agent_xaction");
    super.new();
endfunction:new

function void sbuffer_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void sbuffer_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void sbuffer_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void sbuffer_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string sbuffer_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sauto_inner_buffers_out_a_ready = 0x%0h ",pkt_str,this.auto_inner_buffers_out_a_ready);
    pkt_str = $sformatf("%sauto_inner_buffers_out_a_valid = 0x%0h ",pkt_str,this.auto_inner_buffers_out_a_valid);
    pkt_str = $sformatf("%sauto_inner_buffers_out_a_bits_opcode = 0x%0h ",pkt_str,this.auto_inner_buffers_out_a_bits_opcode);
    pkt_str = $sformatf("%sauto_inner_buffers_out_a_bits_param = 0x%0h ",pkt_str,this.auto_inner_buffers_out_a_bits_param);
    pkt_str = $sformatf("%sauto_inner_buffers_out_a_bits_size = 0x%0h ",pkt_str,this.auto_inner_buffers_out_a_bits_size);
    pkt_str = $sformatf("%sauto_inner_buffers_out_a_bits_source = 0x%0h ",pkt_str,this.auto_inner_buffers_out_a_bits_source);
    pkt_str = $sformatf("%sauto_inner_buffers_out_a_bits_address = 0x%0h ",pkt_str,this.auto_inner_buffers_out_a_bits_address);
    pkt_str = $sformatf("%sauto_inner_buffers_out_a_bits_user_memBackType_MM = 0x%0h ",pkt_str,this.auto_inner_buffers_out_a_bits_user_memBackType_MM);
    pkt_str = $sformatf("%sauto_inner_buffers_out_a_bits_user_memPageType_NC = 0x%0h ",pkt_str,this.auto_inner_buffers_out_a_bits_user_memPageType_NC);
    pkt_str = $sformatf("%sauto_inner_buffers_out_a_bits_mask = 0x%0h ",pkt_str,this.auto_inner_buffers_out_a_bits_mask);
    pkt_str = $sformatf("%sauto_inner_buffers_out_a_bits_data = 0x%0h ",pkt_str,this.auto_inner_buffers_out_a_bits_data);
    pkt_str = $sformatf("%sauto_inner_buffers_out_a_bits_corrupt = 0x%0h ",pkt_str,this.auto_inner_buffers_out_a_bits_corrupt);
    pkt_str = $sformatf("%sauto_inner_buffers_out_d_ready = 0x%0h ",pkt_str,this.auto_inner_buffers_out_d_ready);
    pkt_str = $sformatf("%sauto_inner_buffers_out_d_valid = 0x%0h ",pkt_str,this.auto_inner_buffers_out_d_valid);
    pkt_str = $sformatf("%sauto_inner_buffers_out_d_bits_opcode = 0x%0h ",pkt_str,this.auto_inner_buffers_out_d_bits_opcode);
    pkt_str = $sformatf("%sauto_inner_buffers_out_d_bits_param = 0x%0h ",pkt_str,this.auto_inner_buffers_out_d_bits_param);
    pkt_str = $sformatf("%sauto_inner_buffers_out_d_bits_size = 0x%0h ",pkt_str,this.auto_inner_buffers_out_d_bits_size);
    pkt_str = $sformatf("%sauto_inner_buffers_out_d_bits_source = 0x%0h ",pkt_str,this.auto_inner_buffers_out_d_bits_source);
    pkt_str = $sformatf("%sauto_inner_buffers_out_d_bits_sink = 0x%0h ",pkt_str,this.auto_inner_buffers_out_d_bits_sink);
    pkt_str = $sformatf("%sauto_inner_buffers_out_d_bits_denied = 0x%0h ",pkt_str,this.auto_inner_buffers_out_d_bits_denied);
    pkt_str = $sformatf("%sauto_inner_buffers_out_d_bits_data = 0x%0h ",pkt_str,this.auto_inner_buffers_out_d_bits_data);
    pkt_str = $sformatf("%sauto_inner_buffers_out_d_bits_corrupt = 0x%0h ",pkt_str,this.auto_inner_buffers_out_d_bits_corrupt);

    return pkt_str;
endfunction:psdisplay

function bit sbuffer_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    sbuffer_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a sbuffer_agent_agent_xaction or its extend"))
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

        if(this.auto_inner_buffers_out_a_ready!=rhs_.auto_inner_buffers_out_a_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_a_ready=0x%0h while the rhs_.auto_inner_buffers_out_a_ready=0x%0h",this.auto_inner_buffers_out_a_ready,rhs_.auto_inner_buffers_out_a_ready),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_a_valid!=rhs_.auto_inner_buffers_out_a_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_a_valid=0x%0h while the rhs_.auto_inner_buffers_out_a_valid=0x%0h",this.auto_inner_buffers_out_a_valid,rhs_.auto_inner_buffers_out_a_valid),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_a_bits_opcode!=rhs_.auto_inner_buffers_out_a_bits_opcode) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_a_bits_opcode=0x%0h while the rhs_.auto_inner_buffers_out_a_bits_opcode=0x%0h",this.auto_inner_buffers_out_a_bits_opcode,rhs_.auto_inner_buffers_out_a_bits_opcode),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_a_bits_param!=rhs_.auto_inner_buffers_out_a_bits_param) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_a_bits_param=0x%0h while the rhs_.auto_inner_buffers_out_a_bits_param=0x%0h",this.auto_inner_buffers_out_a_bits_param,rhs_.auto_inner_buffers_out_a_bits_param),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_a_bits_size!=rhs_.auto_inner_buffers_out_a_bits_size) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_a_bits_size=0x%0h while the rhs_.auto_inner_buffers_out_a_bits_size=0x%0h",this.auto_inner_buffers_out_a_bits_size,rhs_.auto_inner_buffers_out_a_bits_size),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_a_bits_source!=rhs_.auto_inner_buffers_out_a_bits_source) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_a_bits_source=0x%0h while the rhs_.auto_inner_buffers_out_a_bits_source=0x%0h",this.auto_inner_buffers_out_a_bits_source,rhs_.auto_inner_buffers_out_a_bits_source),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_a_bits_address!=rhs_.auto_inner_buffers_out_a_bits_address) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_a_bits_address=0x%0h while the rhs_.auto_inner_buffers_out_a_bits_address=0x%0h",this.auto_inner_buffers_out_a_bits_address,rhs_.auto_inner_buffers_out_a_bits_address),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_a_bits_user_memBackType_MM!=rhs_.auto_inner_buffers_out_a_bits_user_memBackType_MM) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_a_bits_user_memBackType_MM=0x%0h while the rhs_.auto_inner_buffers_out_a_bits_user_memBackType_MM=0x%0h",this.auto_inner_buffers_out_a_bits_user_memBackType_MM,rhs_.auto_inner_buffers_out_a_bits_user_memBackType_MM),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_a_bits_user_memPageType_NC!=rhs_.auto_inner_buffers_out_a_bits_user_memPageType_NC) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_a_bits_user_memPageType_NC=0x%0h while the rhs_.auto_inner_buffers_out_a_bits_user_memPageType_NC=0x%0h",this.auto_inner_buffers_out_a_bits_user_memPageType_NC,rhs_.auto_inner_buffers_out_a_bits_user_memPageType_NC),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_a_bits_mask!=rhs_.auto_inner_buffers_out_a_bits_mask) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_a_bits_mask=0x%0h while the rhs_.auto_inner_buffers_out_a_bits_mask=0x%0h",this.auto_inner_buffers_out_a_bits_mask,rhs_.auto_inner_buffers_out_a_bits_mask),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_a_bits_data!=rhs_.auto_inner_buffers_out_a_bits_data) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_a_bits_data=0x%0h while the rhs_.auto_inner_buffers_out_a_bits_data=0x%0h",this.auto_inner_buffers_out_a_bits_data,rhs_.auto_inner_buffers_out_a_bits_data),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_a_bits_corrupt!=rhs_.auto_inner_buffers_out_a_bits_corrupt) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_a_bits_corrupt=0x%0h while the rhs_.auto_inner_buffers_out_a_bits_corrupt=0x%0h",this.auto_inner_buffers_out_a_bits_corrupt,rhs_.auto_inner_buffers_out_a_bits_corrupt),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_d_ready!=rhs_.auto_inner_buffers_out_d_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_d_ready=0x%0h while the rhs_.auto_inner_buffers_out_d_ready=0x%0h",this.auto_inner_buffers_out_d_ready,rhs_.auto_inner_buffers_out_d_ready),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_d_valid!=rhs_.auto_inner_buffers_out_d_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_d_valid=0x%0h while the rhs_.auto_inner_buffers_out_d_valid=0x%0h",this.auto_inner_buffers_out_d_valid,rhs_.auto_inner_buffers_out_d_valid),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_d_bits_opcode!=rhs_.auto_inner_buffers_out_d_bits_opcode) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_d_bits_opcode=0x%0h while the rhs_.auto_inner_buffers_out_d_bits_opcode=0x%0h",this.auto_inner_buffers_out_d_bits_opcode,rhs_.auto_inner_buffers_out_d_bits_opcode),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_d_bits_param!=rhs_.auto_inner_buffers_out_d_bits_param) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_d_bits_param=0x%0h while the rhs_.auto_inner_buffers_out_d_bits_param=0x%0h",this.auto_inner_buffers_out_d_bits_param,rhs_.auto_inner_buffers_out_d_bits_param),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_d_bits_size!=rhs_.auto_inner_buffers_out_d_bits_size) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_d_bits_size=0x%0h while the rhs_.auto_inner_buffers_out_d_bits_size=0x%0h",this.auto_inner_buffers_out_d_bits_size,rhs_.auto_inner_buffers_out_d_bits_size),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_d_bits_source!=rhs_.auto_inner_buffers_out_d_bits_source) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_d_bits_source=0x%0h while the rhs_.auto_inner_buffers_out_d_bits_source=0x%0h",this.auto_inner_buffers_out_d_bits_source,rhs_.auto_inner_buffers_out_d_bits_source),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_d_bits_sink!=rhs_.auto_inner_buffers_out_d_bits_sink) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_d_bits_sink=0x%0h while the rhs_.auto_inner_buffers_out_d_bits_sink=0x%0h",this.auto_inner_buffers_out_d_bits_sink,rhs_.auto_inner_buffers_out_d_bits_sink),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_d_bits_denied!=rhs_.auto_inner_buffers_out_d_bits_denied) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_d_bits_denied=0x%0h while the rhs_.auto_inner_buffers_out_d_bits_denied=0x%0h",this.auto_inner_buffers_out_d_bits_denied,rhs_.auto_inner_buffers_out_d_bits_denied),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_d_bits_data!=rhs_.auto_inner_buffers_out_d_bits_data) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_d_bits_data=0x%0h while the rhs_.auto_inner_buffers_out_d_bits_data=0x%0h",this.auto_inner_buffers_out_d_bits_data,rhs_.auto_inner_buffers_out_d_bits_data),UVM_NONE)
        end

        if(this.auto_inner_buffers_out_d_bits_corrupt!=rhs_.auto_inner_buffers_out_d_bits_corrupt) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_buffers_out_d_bits_corrupt=0x%0h while the rhs_.auto_inner_buffers_out_d_bits_corrupt=0x%0h",this.auto_inner_buffers_out_d_bits_corrupt,rhs_.auto_inner_buffers_out_d_bits_corrupt),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
