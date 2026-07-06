//=========================================================
//File name    : io_mem_to_ooo_wakeup_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_wakeup_agent_agent_xaction
//Discribution : io_mem_to_ooo_wakeup_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_WAKEUP_AGENT_AGENT_XACTION__SV
`define IO_MEM_TO_OOO_WAKEUP_AGENT_AGENT_XACTION__SV

class io_mem_to_ooo_wakeup_agent_agent_xaction  extends tcnt_data_base;
    // DUT wakeup outputs observed by the monitor. We only keep minimal validity
    // and pdest ranges here for debug/compare sanity.
    rand bit io_mem_to_ooo_wakeup_0_valid;
    rand bit io_mem_to_ooo_wakeup_0_bits_rfWen;
    rand bit io_mem_to_ooo_wakeup_0_bits_fpWen;
    rand bit io_mem_to_ooo_wakeup_0_bits_vecWen;
    rand bit io_mem_to_ooo_wakeup_0_bits_v0Wen;
    rand bit io_mem_to_ooo_wakeup_0_bits_vlWen;
    rand bit [7:0] io_mem_to_ooo_wakeup_0_bits_pdest;
    rand bit io_mem_to_ooo_wakeup_1_valid;
    rand bit io_mem_to_ooo_wakeup_1_bits_rfWen;
    rand bit io_mem_to_ooo_wakeup_1_bits_fpWen;
    rand bit io_mem_to_ooo_wakeup_1_bits_vecWen;
    rand bit io_mem_to_ooo_wakeup_1_bits_v0Wen;
    rand bit io_mem_to_ooo_wakeup_1_bits_vlWen;
    rand bit [7:0] io_mem_to_ooo_wakeup_1_bits_pdest;
    rand bit io_mem_to_ooo_wakeup_2_valid;
    rand bit io_mem_to_ooo_wakeup_2_bits_rfWen;
    rand bit io_mem_to_ooo_wakeup_2_bits_fpWen;
    rand bit io_mem_to_ooo_wakeup_2_bits_vecWen;
    rand bit io_mem_to_ooo_wakeup_2_bits_v0Wen;
    rand bit io_mem_to_ooo_wakeup_2_bits_vlWen;
    rand bit [7:0] io_mem_to_ooo_wakeup_2_bits_pdest;

    extern constraint default_io_mem_to_ooo_wakeup_0_valid_cons;
    extern constraint default_io_mem_to_ooo_wakeup_0_bits_rfWen_cons;
    extern constraint default_io_mem_to_ooo_wakeup_0_bits_fpWen_cons;
    extern constraint default_io_mem_to_ooo_wakeup_0_bits_vecWen_cons;
    extern constraint default_io_mem_to_ooo_wakeup_0_bits_v0Wen_cons;
    extern constraint default_io_mem_to_ooo_wakeup_0_bits_vlWen_cons;
    extern constraint default_io_mem_to_ooo_wakeup_0_bits_pdest_cons;
    extern constraint default_io_mem_to_ooo_wakeup_1_valid_cons;
    extern constraint default_io_mem_to_ooo_wakeup_1_bits_rfWen_cons;
    extern constraint default_io_mem_to_ooo_wakeup_1_bits_fpWen_cons;
    extern constraint default_io_mem_to_ooo_wakeup_1_bits_vecWen_cons;
    extern constraint default_io_mem_to_ooo_wakeup_1_bits_v0Wen_cons;
    extern constraint default_io_mem_to_ooo_wakeup_1_bits_vlWen_cons;
    extern constraint default_io_mem_to_ooo_wakeup_1_bits_pdest_cons;
    extern constraint default_io_mem_to_ooo_wakeup_2_valid_cons;
    extern constraint default_io_mem_to_ooo_wakeup_2_bits_rfWen_cons;
    extern constraint default_io_mem_to_ooo_wakeup_2_bits_fpWen_cons;
    extern constraint default_io_mem_to_ooo_wakeup_2_bits_vecWen_cons;
    extern constraint default_io_mem_to_ooo_wakeup_2_bits_v0Wen_cons;
    extern constraint default_io_mem_to_ooo_wakeup_2_bits_vlWen_cons;
    extern constraint default_io_mem_to_ooo_wakeup_2_bits_pdest_cons;

    extern function new(string name="io_mem_to_ooo_wakeup_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(io_mem_to_ooo_wakeup_agent_agent_xaction)
        `uvm_field_int(io_mem_to_ooo_wakeup_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_0_bits_rfWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_0_bits_fpWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_0_bits_vecWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_0_bits_v0Wen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_0_bits_vlWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_0_bits_pdest, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_1_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_1_bits_rfWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_1_bits_fpWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_1_bits_vecWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_1_bits_v0Wen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_1_bits_vlWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_1_bits_pdest, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_2_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_2_bits_rfWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_2_bits_fpWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_2_bits_vecWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_2_bits_v0Wen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_2_bits_vlWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_wakeup_2_bits_pdest, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:io_mem_to_ooo_wakeup_agent_agent_xaction

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_0_valid_cons{
}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_0_bits_rfWen_cons{

}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_0_bits_fpWen_cons{

}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_0_bits_vecWen_cons{

}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_0_bits_v0Wen_cons{

}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_0_bits_vlWen_cons{

}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_0_bits_pdest_cons{
}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_1_valid_cons{
}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_1_bits_rfWen_cons{

}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_1_bits_fpWen_cons{

}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_1_bits_vecWen_cons{

}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_1_bits_v0Wen_cons{

}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_1_bits_vlWen_cons{

}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_1_bits_pdest_cons{
    io_mem_to_ooo_wakeup_1_bits_pdest inside {[8'd0:8'd255]};
}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_2_valid_cons{
    io_mem_to_ooo_wakeup_2_valid inside {1'b0, 1'b1};
}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_2_bits_rfWen_cons{

}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_2_bits_fpWen_cons{

}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_2_bits_vecWen_cons{

}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_2_bits_v0Wen_cons{

}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_2_bits_vlWen_cons{

}

constraint io_mem_to_ooo_wakeup_agent_agent_xaction::default_io_mem_to_ooo_wakeup_2_bits_pdest_cons{
    io_mem_to_ooo_wakeup_2_bits_pdest inside {[8'd0:8'd255]};
}

function io_mem_to_ooo_wakeup_agent_agent_xaction::new(string name = "io_mem_to_ooo_wakeup_agent_agent_xaction");
    super.new();
endfunction:new

function void io_mem_to_ooo_wakeup_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void io_mem_to_ooo_wakeup_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void io_mem_to_ooo_wakeup_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void io_mem_to_ooo_wakeup_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string io_mem_to_ooo_wakeup_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_0_valid = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_0_valid);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_0_bits_rfWen = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_0_bits_rfWen);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_0_bits_fpWen = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_0_bits_fpWen);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_0_bits_vecWen = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_0_bits_vecWen);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_0_bits_v0Wen = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_0_bits_v0Wen);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_0_bits_vlWen = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_0_bits_vlWen);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_0_bits_pdest = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_0_bits_pdest);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_1_valid = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_1_valid);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_1_bits_rfWen = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_1_bits_rfWen);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_1_bits_fpWen = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_1_bits_fpWen);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_1_bits_vecWen = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_1_bits_vecWen);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_1_bits_v0Wen = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_1_bits_v0Wen);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_1_bits_vlWen = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_1_bits_vlWen);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_1_bits_pdest = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_1_bits_pdest);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_2_valid = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_2_valid);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_2_bits_rfWen = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_2_bits_rfWen);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_2_bits_fpWen = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_2_bits_fpWen);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_2_bits_vecWen = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_2_bits_vecWen);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_2_bits_v0Wen = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_2_bits_v0Wen);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_2_bits_vlWen = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_2_bits_vlWen);
    pkt_str = $sformatf("%sio_mem_to_ooo_wakeup_2_bits_pdest = 0x%0h ",pkt_str,this.io_mem_to_ooo_wakeup_2_bits_pdest);

    return pkt_str;
endfunction:psdisplay

function bit io_mem_to_ooo_wakeup_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    io_mem_to_ooo_wakeup_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a io_mem_to_ooo_wakeup_agent_agent_xaction or its extend"))
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

        if(this.io_mem_to_ooo_wakeup_0_valid!=rhs_.io_mem_to_ooo_wakeup_0_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_0_valid=0x%0h while the rhs_.io_mem_to_ooo_wakeup_0_valid=0x%0h",this.io_mem_to_ooo_wakeup_0_valid,rhs_.io_mem_to_ooo_wakeup_0_valid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_0_bits_rfWen!=rhs_.io_mem_to_ooo_wakeup_0_bits_rfWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_0_bits_rfWen=0x%0h while the rhs_.io_mem_to_ooo_wakeup_0_bits_rfWen=0x%0h",this.io_mem_to_ooo_wakeup_0_bits_rfWen,rhs_.io_mem_to_ooo_wakeup_0_bits_rfWen),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_0_bits_fpWen!=rhs_.io_mem_to_ooo_wakeup_0_bits_fpWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_0_bits_fpWen=0x%0h while the rhs_.io_mem_to_ooo_wakeup_0_bits_fpWen=0x%0h",this.io_mem_to_ooo_wakeup_0_bits_fpWen,rhs_.io_mem_to_ooo_wakeup_0_bits_fpWen),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_0_bits_vecWen!=rhs_.io_mem_to_ooo_wakeup_0_bits_vecWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_0_bits_vecWen=0x%0h while the rhs_.io_mem_to_ooo_wakeup_0_bits_vecWen=0x%0h",this.io_mem_to_ooo_wakeup_0_bits_vecWen,rhs_.io_mem_to_ooo_wakeup_0_bits_vecWen),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_0_bits_v0Wen!=rhs_.io_mem_to_ooo_wakeup_0_bits_v0Wen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_0_bits_v0Wen=0x%0h while the rhs_.io_mem_to_ooo_wakeup_0_bits_v0Wen=0x%0h",this.io_mem_to_ooo_wakeup_0_bits_v0Wen,rhs_.io_mem_to_ooo_wakeup_0_bits_v0Wen),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_0_bits_vlWen!=rhs_.io_mem_to_ooo_wakeup_0_bits_vlWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_0_bits_vlWen=0x%0h while the rhs_.io_mem_to_ooo_wakeup_0_bits_vlWen=0x%0h",this.io_mem_to_ooo_wakeup_0_bits_vlWen,rhs_.io_mem_to_ooo_wakeup_0_bits_vlWen),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_0_bits_pdest!=rhs_.io_mem_to_ooo_wakeup_0_bits_pdest) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_0_bits_pdest=0x%0h while the rhs_.io_mem_to_ooo_wakeup_0_bits_pdest=0x%0h",this.io_mem_to_ooo_wakeup_0_bits_pdest,rhs_.io_mem_to_ooo_wakeup_0_bits_pdest),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_1_valid!=rhs_.io_mem_to_ooo_wakeup_1_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_1_valid=0x%0h while the rhs_.io_mem_to_ooo_wakeup_1_valid=0x%0h",this.io_mem_to_ooo_wakeup_1_valid,rhs_.io_mem_to_ooo_wakeup_1_valid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_1_bits_rfWen!=rhs_.io_mem_to_ooo_wakeup_1_bits_rfWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_1_bits_rfWen=0x%0h while the rhs_.io_mem_to_ooo_wakeup_1_bits_rfWen=0x%0h",this.io_mem_to_ooo_wakeup_1_bits_rfWen,rhs_.io_mem_to_ooo_wakeup_1_bits_rfWen),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_1_bits_fpWen!=rhs_.io_mem_to_ooo_wakeup_1_bits_fpWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_1_bits_fpWen=0x%0h while the rhs_.io_mem_to_ooo_wakeup_1_bits_fpWen=0x%0h",this.io_mem_to_ooo_wakeup_1_bits_fpWen,rhs_.io_mem_to_ooo_wakeup_1_bits_fpWen),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_1_bits_vecWen!=rhs_.io_mem_to_ooo_wakeup_1_bits_vecWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_1_bits_vecWen=0x%0h while the rhs_.io_mem_to_ooo_wakeup_1_bits_vecWen=0x%0h",this.io_mem_to_ooo_wakeup_1_bits_vecWen,rhs_.io_mem_to_ooo_wakeup_1_bits_vecWen),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_1_bits_v0Wen!=rhs_.io_mem_to_ooo_wakeup_1_bits_v0Wen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_1_bits_v0Wen=0x%0h while the rhs_.io_mem_to_ooo_wakeup_1_bits_v0Wen=0x%0h",this.io_mem_to_ooo_wakeup_1_bits_v0Wen,rhs_.io_mem_to_ooo_wakeup_1_bits_v0Wen),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_1_bits_vlWen!=rhs_.io_mem_to_ooo_wakeup_1_bits_vlWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_1_bits_vlWen=0x%0h while the rhs_.io_mem_to_ooo_wakeup_1_bits_vlWen=0x%0h",this.io_mem_to_ooo_wakeup_1_bits_vlWen,rhs_.io_mem_to_ooo_wakeup_1_bits_vlWen),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_1_bits_pdest!=rhs_.io_mem_to_ooo_wakeup_1_bits_pdest) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_1_bits_pdest=0x%0h while the rhs_.io_mem_to_ooo_wakeup_1_bits_pdest=0x%0h",this.io_mem_to_ooo_wakeup_1_bits_pdest,rhs_.io_mem_to_ooo_wakeup_1_bits_pdest),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_2_valid!=rhs_.io_mem_to_ooo_wakeup_2_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_2_valid=0x%0h while the rhs_.io_mem_to_ooo_wakeup_2_valid=0x%0h",this.io_mem_to_ooo_wakeup_2_valid,rhs_.io_mem_to_ooo_wakeup_2_valid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_2_bits_rfWen!=rhs_.io_mem_to_ooo_wakeup_2_bits_rfWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_2_bits_rfWen=0x%0h while the rhs_.io_mem_to_ooo_wakeup_2_bits_rfWen=0x%0h",this.io_mem_to_ooo_wakeup_2_bits_rfWen,rhs_.io_mem_to_ooo_wakeup_2_bits_rfWen),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_2_bits_fpWen!=rhs_.io_mem_to_ooo_wakeup_2_bits_fpWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_2_bits_fpWen=0x%0h while the rhs_.io_mem_to_ooo_wakeup_2_bits_fpWen=0x%0h",this.io_mem_to_ooo_wakeup_2_bits_fpWen,rhs_.io_mem_to_ooo_wakeup_2_bits_fpWen),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_2_bits_vecWen!=rhs_.io_mem_to_ooo_wakeup_2_bits_vecWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_2_bits_vecWen=0x%0h while the rhs_.io_mem_to_ooo_wakeup_2_bits_vecWen=0x%0h",this.io_mem_to_ooo_wakeup_2_bits_vecWen,rhs_.io_mem_to_ooo_wakeup_2_bits_vecWen),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_2_bits_v0Wen!=rhs_.io_mem_to_ooo_wakeup_2_bits_v0Wen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_2_bits_v0Wen=0x%0h while the rhs_.io_mem_to_ooo_wakeup_2_bits_v0Wen=0x%0h",this.io_mem_to_ooo_wakeup_2_bits_v0Wen,rhs_.io_mem_to_ooo_wakeup_2_bits_v0Wen),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_2_bits_vlWen!=rhs_.io_mem_to_ooo_wakeup_2_bits_vlWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_2_bits_vlWen=0x%0h while the rhs_.io_mem_to_ooo_wakeup_2_bits_vlWen=0x%0h",this.io_mem_to_ooo_wakeup_2_bits_vlWen,rhs_.io_mem_to_ooo_wakeup_2_bits_vlWen),UVM_NONE)
        end

        if(this.io_mem_to_ooo_wakeup_2_bits_pdest!=rhs_.io_mem_to_ooo_wakeup_2_bits_pdest) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_wakeup_2_bits_pdest=0x%0h while the rhs_.io_mem_to_ooo_wakeup_2_bits_pdest=0x%0h",this.io_mem_to_ooo_wakeup_2_bits_pdest,rhs_.io_mem_to_ooo_wakeup_2_bits_pdest),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
