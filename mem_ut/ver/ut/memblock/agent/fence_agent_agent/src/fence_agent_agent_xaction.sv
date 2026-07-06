//=========================================================
//File name    : fence_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : fence_agent_agent_xaction
//Discribution : fence_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef FENCE_AGENT_AGENT_XACTION__SV
`define FENCE_AGENT_AGENT_XACTION__SV

class fence_agent_agent_xaction  extends tcnt_data_base;
    // sfence/hfence are explicit control transactions. The payload stays
    // unconstrained except for basic two-state/range legality.
    rand bit io_ooo_to_mem_sfence_valid;
    rand bit io_ooo_to_mem_sfence_bits_rs1;
    rand bit io_ooo_to_mem_sfence_bits_rs2;
    rand bit [49:0] io_ooo_to_mem_sfence_bits_addr;
    rand bit [15:0] io_ooo_to_mem_sfence_bits_id;
    rand bit io_ooo_to_mem_sfence_bits_hv;
    rand bit io_ooo_to_mem_sfence_bits_hg;

    extern constraint default_io_ooo_to_mem_sfence_valid_cons;
    extern constraint default_io_ooo_to_mem_sfence_bits_rs1_cons;
    extern constraint default_io_ooo_to_mem_sfence_bits_rs2_cons;
    extern constraint default_io_ooo_to_mem_sfence_bits_addr_cons;
    extern constraint default_io_ooo_to_mem_sfence_bits_id_cons;
    extern constraint default_io_ooo_to_mem_sfence_bits_hv_cons;
    extern constraint default_io_ooo_to_mem_sfence_bits_hg_cons;

    extern function new(string name="fence_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(fence_agent_agent_xaction)
        `uvm_field_int(io_ooo_to_mem_sfence_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_sfence_bits_rs1, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_sfence_bits_rs2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_sfence_bits_addr, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_sfence_bits_id, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_sfence_bits_hv, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_sfence_bits_hg, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:fence_agent_agent_xaction

constraint fence_agent_agent_xaction::default_io_ooo_to_mem_sfence_valid_cons{
    io_ooo_to_mem_sfence_valid inside {1'b0, 1'b1};
}

constraint fence_agent_agent_xaction::default_io_ooo_to_mem_sfence_bits_rs1_cons{
    io_ooo_to_mem_sfence_bits_rs1 inside {1'b0, 1'b1};
}

constraint fence_agent_agent_xaction::default_io_ooo_to_mem_sfence_bits_rs2_cons{
    io_ooo_to_mem_sfence_bits_rs2 inside {1'b0, 1'b1};
}

constraint fence_agent_agent_xaction::default_io_ooo_to_mem_sfence_bits_addr_cons{
    io_ooo_to_mem_sfence_bits_addr inside {[50'd0:(50'd1<<49)-1]};
}

constraint fence_agent_agent_xaction::default_io_ooo_to_mem_sfence_bits_id_cons{
    io_ooo_to_mem_sfence_bits_id inside {[16'd0:16'hffff]};
}

constraint fence_agent_agent_xaction::default_io_ooo_to_mem_sfence_bits_hv_cons{
    io_ooo_to_mem_sfence_bits_hv inside {1'b0, 1'b1};
}

constraint fence_agent_agent_xaction::default_io_ooo_to_mem_sfence_bits_hg_cons{
    io_ooo_to_mem_sfence_bits_hg inside {1'b0, 1'b1};
}

function fence_agent_agent_xaction::new(string name = "fence_agent_agent_xaction");
    super.new();
endfunction:new

function void fence_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void fence_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void fence_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void fence_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string fence_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sio_ooo_to_mem_sfence_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_sfence_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_sfence_bits_rs1 = 0x%0h ",pkt_str,this.io_ooo_to_mem_sfence_bits_rs1);
    pkt_str = $sformatf("%sio_ooo_to_mem_sfence_bits_rs2 = 0x%0h ",pkt_str,this.io_ooo_to_mem_sfence_bits_rs2);
    pkt_str = $sformatf("%sio_ooo_to_mem_sfence_bits_addr = 0x%0h ",pkt_str,this.io_ooo_to_mem_sfence_bits_addr);
    pkt_str = $sformatf("%sio_ooo_to_mem_sfence_bits_id = 0x%0h ",pkt_str,this.io_ooo_to_mem_sfence_bits_id);
    pkt_str = $sformatf("%sio_ooo_to_mem_sfence_bits_hv = 0x%0h ",pkt_str,this.io_ooo_to_mem_sfence_bits_hv);
    pkt_str = $sformatf("%sio_ooo_to_mem_sfence_bits_hg = 0x%0h ",pkt_str,this.io_ooo_to_mem_sfence_bits_hg);

    return pkt_str;
endfunction:psdisplay

function bit fence_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    fence_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a fence_agent_agent_xaction or its extend"))
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

        if(this.io_ooo_to_mem_sfence_valid!=rhs_.io_ooo_to_mem_sfence_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_sfence_valid=0x%0h while the rhs_.io_ooo_to_mem_sfence_valid=0x%0h",this.io_ooo_to_mem_sfence_valid,rhs_.io_ooo_to_mem_sfence_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_sfence_bits_rs1!=rhs_.io_ooo_to_mem_sfence_bits_rs1) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_sfence_bits_rs1=0x%0h while the rhs_.io_ooo_to_mem_sfence_bits_rs1=0x%0h",this.io_ooo_to_mem_sfence_bits_rs1,rhs_.io_ooo_to_mem_sfence_bits_rs1),UVM_NONE)
        end

        if(this.io_ooo_to_mem_sfence_bits_rs2!=rhs_.io_ooo_to_mem_sfence_bits_rs2) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_sfence_bits_rs2=0x%0h while the rhs_.io_ooo_to_mem_sfence_bits_rs2=0x%0h",this.io_ooo_to_mem_sfence_bits_rs2,rhs_.io_ooo_to_mem_sfence_bits_rs2),UVM_NONE)
        end

        if(this.io_ooo_to_mem_sfence_bits_addr!=rhs_.io_ooo_to_mem_sfence_bits_addr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_sfence_bits_addr=0x%0h while the rhs_.io_ooo_to_mem_sfence_bits_addr=0x%0h",this.io_ooo_to_mem_sfence_bits_addr,rhs_.io_ooo_to_mem_sfence_bits_addr),UVM_NONE)
        end

        if(this.io_ooo_to_mem_sfence_bits_id!=rhs_.io_ooo_to_mem_sfence_bits_id) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_sfence_bits_id=0x%0h while the rhs_.io_ooo_to_mem_sfence_bits_id=0x%0h",this.io_ooo_to_mem_sfence_bits_id,rhs_.io_ooo_to_mem_sfence_bits_id),UVM_NONE)
        end

        if(this.io_ooo_to_mem_sfence_bits_hv!=rhs_.io_ooo_to_mem_sfence_bits_hv) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_sfence_bits_hv=0x%0h while the rhs_.io_ooo_to_mem_sfence_bits_hv=0x%0h",this.io_ooo_to_mem_sfence_bits_hv,rhs_.io_ooo_to_mem_sfence_bits_hv),UVM_NONE)
        end

        if(this.io_ooo_to_mem_sfence_bits_hg!=rhs_.io_ooo_to_mem_sfence_bits_hg) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_sfence_bits_hg=0x%0h while the rhs_.io_ooo_to_mem_sfence_bits_hg=0x%0h",this.io_ooo_to_mem_sfence_bits_hg,rhs_.io_ooo_to_mem_sfence_bits_hg),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
