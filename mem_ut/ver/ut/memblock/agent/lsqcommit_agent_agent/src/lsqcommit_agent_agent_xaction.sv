//=========================================================
//File name    : lsqcommit_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : lsqcommit_agent_agent_xaction
//Discribution : lsqcommit_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef LSQCOMMIT_AGENT_AGENT_XACTION__SV
`define LSQCOMMIT_AGENT_AGENT_XACTION__SV

class lsqcommit_agent_agent_xaction  extends tcnt_data_base;
    // This agent only models a subset of the full ROB->LSQ commit interface.
    // pendingPtr must remain inside current ROB capacity.
    rand bit io_ooo_to_mem_lsqio_pendingPtr_flag;
    rand bit [8:0] io_ooo_to_mem_lsqio_pendingPtr_value;
    rand bit io_ooo_to_mem_flushSb     ;

    extern constraint default_io_ooo_to_mem_lsqio_pendingPtr_flag_cons;
    extern constraint default_io_ooo_to_mem_lsqio_pendingPtr_value_cons;
    extern constraint default_io_ooo_to_mem_flushSb_cons;

    extern function new(string name="lsqcommit_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(lsqcommit_agent_agent_xaction)
        `uvm_field_int(io_ooo_to_mem_lsqio_pendingPtr_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_lsqio_pendingPtr_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_flushSb, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:lsqcommit_agent_agent_xaction

constraint lsqcommit_agent_agent_xaction::default_io_ooo_to_mem_lsqio_pendingPtr_flag_cons{
    io_ooo_to_mem_lsqio_pendingPtr_flag inside {1'b0, 1'b1};
}

constraint lsqcommit_agent_agent_xaction::default_io_ooo_to_mem_lsqio_pendingPtr_value_cons{
    io_ooo_to_mem_lsqio_pendingPtr_value inside {[9'd0:9'd351]};
}

constraint lsqcommit_agent_agent_xaction::default_io_ooo_to_mem_flushSb_cons{
    io_ooo_to_mem_flushSb inside {1'b0, 1'b1};
}

function lsqcommit_agent_agent_xaction::new(string name = "lsqcommit_agent_agent_xaction");
    super.new();
endfunction:new

function void lsqcommit_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void lsqcommit_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void lsqcommit_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void lsqcommit_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string lsqcommit_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sio_ooo_to_mem_lsqio_pendingPtr_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_lsqio_pendingPtr_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_lsqio_pendingPtr_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_lsqio_pendingPtr_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_flushSb = 0x%0h ",pkt_str,this.io_ooo_to_mem_flushSb);

    return pkt_str;
endfunction:psdisplay

function bit lsqcommit_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    lsqcommit_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a lsqcommit_agent_agent_xaction or its extend"))
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

        if(this.io_ooo_to_mem_lsqio_pendingPtr_flag!=rhs_.io_ooo_to_mem_lsqio_pendingPtr_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_lsqio_pendingPtr_flag=0x%0h while the rhs_.io_ooo_to_mem_lsqio_pendingPtr_flag=0x%0h",this.io_ooo_to_mem_lsqio_pendingPtr_flag,rhs_.io_ooo_to_mem_lsqio_pendingPtr_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_lsqio_pendingPtr_value!=rhs_.io_ooo_to_mem_lsqio_pendingPtr_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_lsqio_pendingPtr_value=0x%0h while the rhs_.io_ooo_to_mem_lsqio_pendingPtr_value=0x%0h",this.io_ooo_to_mem_lsqio_pendingPtr_value,rhs_.io_ooo_to_mem_lsqio_pendingPtr_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_flushSb!=rhs_.io_ooo_to_mem_flushSb) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_flushSb=0x%0h while the rhs_.io_ooo_to_mem_flushSb=0x%0h",this.io_ooo_to_mem_flushSb,rhs_.io_ooo_to_mem_flushSb),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
