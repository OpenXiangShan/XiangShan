//=========================================================
//File name    : redirect_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : redirect_agent_agent_xaction
//Discribution : redirect_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef REDIRECT_AGENT_AGENT_XACTION__SV
`define REDIRECT_AGENT_AGENT_XACTION__SV

class redirect_agent_agent_xaction  extends tcnt_data_base;
    // Redirect is an input control interface to MemBlock. `level` follows
    // RedirectLevel in Scala: flushAfter=0, flush=1.
    rand bit io_redirect_valid         ;
    rand bit io_redirect_bits_level    ;
    rand bit io_redirect_bits_robIdx_flag;
    rand bit [8:0] io_redirect_bits_robIdx_value;
    extern constraint default_io_redirect_valid_cons;
    extern constraint default_io_redirect_bits_level_cons;
    extern constraint default_io_redirect_bits_robIdx_flag_cons;
    extern constraint default_io_redirect_bits_robIdx_value_cons;

    extern function new(string name="redirect_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(redirect_agent_agent_xaction)
        `uvm_field_int(io_redirect_valid, UVM_ALL_ON);
        `uvm_field_int(io_redirect_bits_level, UVM_ALL_ON);
        `uvm_field_int(io_redirect_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_redirect_bits_robIdx_value, UVM_ALL_ON);
    `uvm_object_utils_end

endclass:redirect_agent_agent_xaction

constraint redirect_agent_agent_xaction::default_io_redirect_valid_cons{
    io_redirect_valid inside {1'b0, 1'b1};
}

constraint redirect_agent_agent_xaction::default_io_redirect_bits_level_cons{
    io_redirect_bits_level inside {1'b0, 1'b1};
}

constraint redirect_agent_agent_xaction::default_io_redirect_bits_robIdx_flag_cons{
    io_redirect_bits_robIdx_flag inside {1'b0, 1'b1};
}

constraint redirect_agent_agent_xaction::default_io_redirect_bits_robIdx_value_cons{
    io_redirect_bits_robIdx_value inside {[9'd0:9'd351]};
}

function redirect_agent_agent_xaction::new(string name = "redirect_agent_agent_xaction");
    super.new();
endfunction:new

function void redirect_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void redirect_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void redirect_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void redirect_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string redirect_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sio_redirect_valid = 0x%0h ",pkt_str,this.io_redirect_valid);
    pkt_str = $sformatf("%sio_redirect_bits_level = 0x%0h ",pkt_str,this.io_redirect_bits_level);
    pkt_str = $sformatf("%sio_redirect_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_redirect_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_redirect_bits_robIdx_value = 0x%0h ",pkt_str,this.io_redirect_bits_robIdx_value);
    return pkt_str;
endfunction:psdisplay

function bit redirect_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    redirect_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a redirect_agent_agent_xaction or its extend"))
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

        if(this.io_redirect_valid!=rhs_.io_redirect_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_redirect_valid=0x%0h while the rhs_.io_redirect_valid=0x%0h",this.io_redirect_valid,rhs_.io_redirect_valid),UVM_NONE)
        end

        if(this.io_redirect_bits_level!=rhs_.io_redirect_bits_level) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_redirect_bits_level=0x%0h while the rhs_.io_redirect_bits_level=0x%0h",this.io_redirect_bits_level,rhs_.io_redirect_bits_level),UVM_NONE)
        end

        if(this.io_redirect_bits_robIdx_flag!=rhs_.io_redirect_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_redirect_bits_robIdx_flag=0x%0h while the rhs_.io_redirect_bits_robIdx_flag=0x%0h",this.io_redirect_bits_robIdx_flag,rhs_.io_redirect_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_redirect_bits_robIdx_value!=rhs_.io_redirect_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_redirect_bits_robIdx_value=0x%0h while the rhs_.io_redirect_bits_robIdx_value=0x%0h",this.io_redirect_bits_robIdx_value,rhs_.io_redirect_bits_robIdx_value),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
