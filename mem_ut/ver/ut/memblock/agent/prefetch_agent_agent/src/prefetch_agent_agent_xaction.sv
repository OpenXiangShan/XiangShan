//=========================================================
//File name    : prefetch_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : prefetch_agent_agent_xaction
//Discribution : prefetch_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef PREFETCH_AGENT_AGENT_XACTION__SV
`define PREFETCH_AGENT_AGENT_XACTION__SV

class prefetch_agent_agent_xaction  extends tcnt_data_base;
    // Prefetch-related outputs are primarily monitored. Base constraints here
    // only keep the payload values inside obvious width/source bounds.
    rand bit [63:0] auto_inner_l3_pf_sender_out_addr;
    rand bit auto_inner_l3_pf_sender_out_addr_valid;
    rand bit auto_inner_l3_pf_sender_out_l2_pf_en;
    rand bit [63:0] auto_inner_l2_pf_sender_out_addr;
    rand bit [4:0] auto_inner_l2_pf_sender_out_pf_source;
    rand bit auto_inner_l2_pf_sender_out_addr_valid;
    rand bit auto_inner_l2_pf_sender_out_l2_pf_en;
    rand bit io_ifetchPrefetch_0_valid ;
    rand bit [49:0] io_ifetchPrefetch_0_bits_vaddr;
    rand bit io_ifetchPrefetch_1_valid ;
    rand bit [49:0] io_ifetchPrefetch_1_bits_vaddr;
    rand bit io_ifetchPrefetch_2_valid ;
    rand bit [49:0] io_ifetchPrefetch_2_bits_vaddr;

    extern constraint default_auto_inner_l3_pf_sender_out_addr_cons;
    extern constraint default_auto_inner_l3_pf_sender_out_addr_valid_cons;
    extern constraint default_auto_inner_l3_pf_sender_out_l2_pf_en_cons;
    extern constraint default_auto_inner_l2_pf_sender_out_addr_cons;
    extern constraint default_auto_inner_l2_pf_sender_out_pf_source_cons;
    extern constraint default_auto_inner_l2_pf_sender_out_addr_valid_cons;
    extern constraint default_auto_inner_l2_pf_sender_out_l2_pf_en_cons;
    extern constraint default_io_ifetchPrefetch_0_valid_cons;
    extern constraint default_io_ifetchPrefetch_0_bits_vaddr_cons;
    extern constraint default_io_ifetchPrefetch_1_valid_cons;
    extern constraint default_io_ifetchPrefetch_1_bits_vaddr_cons;
    extern constraint default_io_ifetchPrefetch_2_valid_cons;
    extern constraint default_io_ifetchPrefetch_2_bits_vaddr_cons;

    extern function new(string name="prefetch_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(prefetch_agent_agent_xaction)
        `uvm_field_int(auto_inner_l3_pf_sender_out_addr, UVM_ALL_ON);
        `uvm_field_int(auto_inner_l3_pf_sender_out_addr_valid, UVM_ALL_ON);
        `uvm_field_int(auto_inner_l3_pf_sender_out_l2_pf_en, UVM_ALL_ON);
        `uvm_field_int(auto_inner_l2_pf_sender_out_addr, UVM_ALL_ON);
        `uvm_field_int(auto_inner_l2_pf_sender_out_pf_source, UVM_ALL_ON);
        `uvm_field_int(auto_inner_l2_pf_sender_out_addr_valid, UVM_ALL_ON);
        `uvm_field_int(auto_inner_l2_pf_sender_out_l2_pf_en, UVM_ALL_ON);
        `uvm_field_int(io_ifetchPrefetch_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_ifetchPrefetch_0_bits_vaddr, UVM_ALL_ON);
        `uvm_field_int(io_ifetchPrefetch_1_valid, UVM_ALL_ON);
        `uvm_field_int(io_ifetchPrefetch_1_bits_vaddr, UVM_ALL_ON);
        `uvm_field_int(io_ifetchPrefetch_2_valid, UVM_ALL_ON);
        `uvm_field_int(io_ifetchPrefetch_2_bits_vaddr, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:prefetch_agent_agent_xaction

constraint prefetch_agent_agent_xaction::default_auto_inner_l3_pf_sender_out_addr_cons{

}

constraint prefetch_agent_agent_xaction::default_auto_inner_l3_pf_sender_out_addr_valid_cons{
}

constraint prefetch_agent_agent_xaction::default_auto_inner_l3_pf_sender_out_l2_pf_en_cons{
}

constraint prefetch_agent_agent_xaction::default_auto_inner_l2_pf_sender_out_addr_cons{

}

constraint prefetch_agent_agent_xaction::default_auto_inner_l2_pf_sender_out_pf_source_cons{
}

constraint prefetch_agent_agent_xaction::default_auto_inner_l2_pf_sender_out_addr_valid_cons{
}

constraint prefetch_agent_agent_xaction::default_auto_inner_l2_pf_sender_out_l2_pf_en_cons{
}

constraint prefetch_agent_agent_xaction::default_io_ifetchPrefetch_0_valid_cons{
}

constraint prefetch_agent_agent_xaction::default_io_ifetchPrefetch_0_bits_vaddr_cons{

}

constraint prefetch_agent_agent_xaction::default_io_ifetchPrefetch_1_valid_cons{
    io_ifetchPrefetch_1_valid inside {1'b0, 1'b1};
}

constraint prefetch_agent_agent_xaction::default_io_ifetchPrefetch_1_bits_vaddr_cons{

}

constraint prefetch_agent_agent_xaction::default_io_ifetchPrefetch_2_valid_cons{
    io_ifetchPrefetch_2_valid inside {1'b0, 1'b1};
}

constraint prefetch_agent_agent_xaction::default_io_ifetchPrefetch_2_bits_vaddr_cons{

}

function prefetch_agent_agent_xaction::new(string name = "prefetch_agent_agent_xaction");
    super.new();
endfunction:new

function void prefetch_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void prefetch_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void prefetch_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void prefetch_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string prefetch_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sauto_inner_l3_pf_sender_out_addr = 0x%0h ",pkt_str,this.auto_inner_l3_pf_sender_out_addr);
    pkt_str = $sformatf("%sauto_inner_l3_pf_sender_out_addr_valid = 0x%0h ",pkt_str,this.auto_inner_l3_pf_sender_out_addr_valid);
    pkt_str = $sformatf("%sauto_inner_l3_pf_sender_out_l2_pf_en = 0x%0h ",pkt_str,this.auto_inner_l3_pf_sender_out_l2_pf_en);
    pkt_str = $sformatf("%sauto_inner_l2_pf_sender_out_addr = 0x%0h ",pkt_str,this.auto_inner_l2_pf_sender_out_addr);
    pkt_str = $sformatf("%sauto_inner_l2_pf_sender_out_pf_source = 0x%0h ",pkt_str,this.auto_inner_l2_pf_sender_out_pf_source);
    pkt_str = $sformatf("%sauto_inner_l2_pf_sender_out_addr_valid = 0x%0h ",pkt_str,this.auto_inner_l2_pf_sender_out_addr_valid);
    pkt_str = $sformatf("%sauto_inner_l2_pf_sender_out_l2_pf_en = 0x%0h ",pkt_str,this.auto_inner_l2_pf_sender_out_l2_pf_en);
    pkt_str = $sformatf("%sio_ifetchPrefetch_0_valid = 0x%0h ",pkt_str,this.io_ifetchPrefetch_0_valid);
    pkt_str = $sformatf("%sio_ifetchPrefetch_0_bits_vaddr = 0x%0h ",pkt_str,this.io_ifetchPrefetch_0_bits_vaddr);
    pkt_str = $sformatf("%sio_ifetchPrefetch_1_valid = 0x%0h ",pkt_str,this.io_ifetchPrefetch_1_valid);
    pkt_str = $sformatf("%sio_ifetchPrefetch_1_bits_vaddr = 0x%0h ",pkt_str,this.io_ifetchPrefetch_1_bits_vaddr);
    pkt_str = $sformatf("%sio_ifetchPrefetch_2_valid = 0x%0h ",pkt_str,this.io_ifetchPrefetch_2_valid);
    pkt_str = $sformatf("%sio_ifetchPrefetch_2_bits_vaddr = 0x%0h ",pkt_str,this.io_ifetchPrefetch_2_bits_vaddr);

    return pkt_str;
endfunction:psdisplay

function bit prefetch_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    prefetch_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a prefetch_agent_agent_xaction or its extend"))
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

        if(this.auto_inner_l3_pf_sender_out_addr!=rhs_.auto_inner_l3_pf_sender_out_addr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_l3_pf_sender_out_addr=0x%0h while the rhs_.auto_inner_l3_pf_sender_out_addr=0x%0h",this.auto_inner_l3_pf_sender_out_addr,rhs_.auto_inner_l3_pf_sender_out_addr),UVM_NONE)
        end

        if(this.auto_inner_l3_pf_sender_out_addr_valid!=rhs_.auto_inner_l3_pf_sender_out_addr_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_l3_pf_sender_out_addr_valid=0x%0h while the rhs_.auto_inner_l3_pf_sender_out_addr_valid=0x%0h",this.auto_inner_l3_pf_sender_out_addr_valid,rhs_.auto_inner_l3_pf_sender_out_addr_valid),UVM_NONE)
        end

        if(this.auto_inner_l3_pf_sender_out_l2_pf_en!=rhs_.auto_inner_l3_pf_sender_out_l2_pf_en) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_l3_pf_sender_out_l2_pf_en=0x%0h while the rhs_.auto_inner_l3_pf_sender_out_l2_pf_en=0x%0h",this.auto_inner_l3_pf_sender_out_l2_pf_en,rhs_.auto_inner_l3_pf_sender_out_l2_pf_en),UVM_NONE)
        end

        if(this.auto_inner_l2_pf_sender_out_addr!=rhs_.auto_inner_l2_pf_sender_out_addr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_l2_pf_sender_out_addr=0x%0h while the rhs_.auto_inner_l2_pf_sender_out_addr=0x%0h",this.auto_inner_l2_pf_sender_out_addr,rhs_.auto_inner_l2_pf_sender_out_addr),UVM_NONE)
        end

        if(this.auto_inner_l2_pf_sender_out_pf_source!=rhs_.auto_inner_l2_pf_sender_out_pf_source) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_l2_pf_sender_out_pf_source=0x%0h while the rhs_.auto_inner_l2_pf_sender_out_pf_source=0x%0h",this.auto_inner_l2_pf_sender_out_pf_source,rhs_.auto_inner_l2_pf_sender_out_pf_source),UVM_NONE)
        end

        if(this.auto_inner_l2_pf_sender_out_addr_valid!=rhs_.auto_inner_l2_pf_sender_out_addr_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_l2_pf_sender_out_addr_valid=0x%0h while the rhs_.auto_inner_l2_pf_sender_out_addr_valid=0x%0h",this.auto_inner_l2_pf_sender_out_addr_valid,rhs_.auto_inner_l2_pf_sender_out_addr_valid),UVM_NONE)
        end

        if(this.auto_inner_l2_pf_sender_out_l2_pf_en!=rhs_.auto_inner_l2_pf_sender_out_l2_pf_en) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_l2_pf_sender_out_l2_pf_en=0x%0h while the rhs_.auto_inner_l2_pf_sender_out_l2_pf_en=0x%0h",this.auto_inner_l2_pf_sender_out_l2_pf_en,rhs_.auto_inner_l2_pf_sender_out_l2_pf_en),UVM_NONE)
        end

        if(this.io_ifetchPrefetch_0_valid!=rhs_.io_ifetchPrefetch_0_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ifetchPrefetch_0_valid=0x%0h while the rhs_.io_ifetchPrefetch_0_valid=0x%0h",this.io_ifetchPrefetch_0_valid,rhs_.io_ifetchPrefetch_0_valid),UVM_NONE)
        end

        if(this.io_ifetchPrefetch_0_bits_vaddr!=rhs_.io_ifetchPrefetch_0_bits_vaddr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ifetchPrefetch_0_bits_vaddr=0x%0h while the rhs_.io_ifetchPrefetch_0_bits_vaddr=0x%0h",this.io_ifetchPrefetch_0_bits_vaddr,rhs_.io_ifetchPrefetch_0_bits_vaddr),UVM_NONE)
        end

        if(this.io_ifetchPrefetch_1_valid!=rhs_.io_ifetchPrefetch_1_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ifetchPrefetch_1_valid=0x%0h while the rhs_.io_ifetchPrefetch_1_valid=0x%0h",this.io_ifetchPrefetch_1_valid,rhs_.io_ifetchPrefetch_1_valid),UVM_NONE)
        end

        if(this.io_ifetchPrefetch_1_bits_vaddr!=rhs_.io_ifetchPrefetch_1_bits_vaddr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ifetchPrefetch_1_bits_vaddr=0x%0h while the rhs_.io_ifetchPrefetch_1_bits_vaddr=0x%0h",this.io_ifetchPrefetch_1_bits_vaddr,rhs_.io_ifetchPrefetch_1_bits_vaddr),UVM_NONE)
        end

        if(this.io_ifetchPrefetch_2_valid!=rhs_.io_ifetchPrefetch_2_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ifetchPrefetch_2_valid=0x%0h while the rhs_.io_ifetchPrefetch_2_valid=0x%0h",this.io_ifetchPrefetch_2_valid,rhs_.io_ifetchPrefetch_2_valid),UVM_NONE)
        end

        if(this.io_ifetchPrefetch_2_bits_vaddr!=rhs_.io_ifetchPrefetch_2_bits_vaddr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ifetchPrefetch_2_bits_vaddr=0x%0h while the rhs_.io_ifetchPrefetch_2_bits_vaddr=0x%0h",this.io_ifetchPrefetch_2_bits_vaddr,rhs_.io_ifetchPrefetch_2_bits_vaddr),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
