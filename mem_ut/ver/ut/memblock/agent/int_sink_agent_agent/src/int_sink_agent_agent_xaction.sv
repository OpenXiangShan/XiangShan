//=========================================================
//File name    : int_sink_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : int_sink_agent_agent_xaction
//Discribution : int_sink_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef INT_SINK_AGENT_AGENT_XACTION__SV
`define INT_SINK_AGENT_AGENT_XACTION__SV

class int_sink_agent_agent_xaction  extends tcnt_data_base;
    // Interrupt sink lines are simple one-bit sideband inputs. Keep them
    // explicit two-state to avoid X/Z style randomized traffic.
    rand bit auto_inner_beu_local_int_sink_in_0;
    rand bit auto_inner_nmi_int_sink_in_0;
    rand bit auto_inner_nmi_int_sink_in_1;
    rand bit auto_inner_plic_int_sink_in_1_0;
    rand bit auto_inner_plic_int_sink_in_0_0;
    rand bit auto_inner_clint_int_sink_in_0;
    rand bit auto_inner_clint_int_sink_in_1;

    extern constraint default_auto_inner_beu_local_int_sink_in_0_cons;
    extern constraint default_auto_inner_nmi_int_sink_in_0_cons;
    extern constraint default_auto_inner_nmi_int_sink_in_1_cons;
    extern constraint default_auto_inner_plic_int_sink_in_1_0_cons;
    extern constraint default_auto_inner_plic_int_sink_in_0_0_cons;
    extern constraint default_auto_inner_clint_int_sink_in_0_cons;
    extern constraint default_auto_inner_clint_int_sink_in_1_cons;

    extern function new(string name="int_sink_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(int_sink_agent_agent_xaction)
        `uvm_field_int(auto_inner_beu_local_int_sink_in_0, UVM_ALL_ON);
        `uvm_field_int(auto_inner_nmi_int_sink_in_0, UVM_ALL_ON);
        `uvm_field_int(auto_inner_nmi_int_sink_in_1, UVM_ALL_ON);
        `uvm_field_int(auto_inner_plic_int_sink_in_1_0, UVM_ALL_ON);
        `uvm_field_int(auto_inner_plic_int_sink_in_0_0, UVM_ALL_ON);
        `uvm_field_int(auto_inner_clint_int_sink_in_0, UVM_ALL_ON);
        `uvm_field_int(auto_inner_clint_int_sink_in_1, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:int_sink_agent_agent_xaction

constraint int_sink_agent_agent_xaction::default_auto_inner_beu_local_int_sink_in_0_cons{
    auto_inner_beu_local_int_sink_in_0 inside {1'b0, 1'b1};
}

constraint int_sink_agent_agent_xaction::default_auto_inner_nmi_int_sink_in_0_cons{
    auto_inner_nmi_int_sink_in_0 inside {1'b0, 1'b1};
}

constraint int_sink_agent_agent_xaction::default_auto_inner_nmi_int_sink_in_1_cons{
    auto_inner_nmi_int_sink_in_1 inside {1'b0, 1'b1};
}

constraint int_sink_agent_agent_xaction::default_auto_inner_plic_int_sink_in_1_0_cons{
    auto_inner_plic_int_sink_in_1_0 inside {1'b0, 1'b1};
}

constraint int_sink_agent_agent_xaction::default_auto_inner_plic_int_sink_in_0_0_cons{
    auto_inner_plic_int_sink_in_0_0 inside {1'b0, 1'b1};
}

constraint int_sink_agent_agent_xaction::default_auto_inner_clint_int_sink_in_0_cons{
    auto_inner_clint_int_sink_in_0 inside {1'b0, 1'b1};
}

constraint int_sink_agent_agent_xaction::default_auto_inner_clint_int_sink_in_1_cons{
    auto_inner_clint_int_sink_in_1 inside {1'b0, 1'b1};
}

function int_sink_agent_agent_xaction::new(string name = "int_sink_agent_agent_xaction");
    super.new();
endfunction:new

function void int_sink_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void int_sink_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void int_sink_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void int_sink_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string int_sink_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sauto_inner_beu_local_int_sink_in_0 = 0x%0h ",pkt_str,this.auto_inner_beu_local_int_sink_in_0);
    pkt_str = $sformatf("%sauto_inner_nmi_int_sink_in_0 = 0x%0h ",pkt_str,this.auto_inner_nmi_int_sink_in_0);
    pkt_str = $sformatf("%sauto_inner_nmi_int_sink_in_1 = 0x%0h ",pkt_str,this.auto_inner_nmi_int_sink_in_1);
    pkt_str = $sformatf("%sauto_inner_plic_int_sink_in_1_0 = 0x%0h ",pkt_str,this.auto_inner_plic_int_sink_in_1_0);
    pkt_str = $sformatf("%sauto_inner_plic_int_sink_in_0_0 = 0x%0h ",pkt_str,this.auto_inner_plic_int_sink_in_0_0);
    pkt_str = $sformatf("%sauto_inner_clint_int_sink_in_0 = 0x%0h ",pkt_str,this.auto_inner_clint_int_sink_in_0);
    pkt_str = $sformatf("%sauto_inner_clint_int_sink_in_1 = 0x%0h ",pkt_str,this.auto_inner_clint_int_sink_in_1);

    return pkt_str;
endfunction:psdisplay

function bit int_sink_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    int_sink_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a int_sink_agent_agent_xaction or its extend"))
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

        if(this.auto_inner_beu_local_int_sink_in_0!=rhs_.auto_inner_beu_local_int_sink_in_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_beu_local_int_sink_in_0=0x%0h while the rhs_.auto_inner_beu_local_int_sink_in_0=0x%0h",this.auto_inner_beu_local_int_sink_in_0,rhs_.auto_inner_beu_local_int_sink_in_0),UVM_NONE)
        end

        if(this.auto_inner_nmi_int_sink_in_0!=rhs_.auto_inner_nmi_int_sink_in_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_nmi_int_sink_in_0=0x%0h while the rhs_.auto_inner_nmi_int_sink_in_0=0x%0h",this.auto_inner_nmi_int_sink_in_0,rhs_.auto_inner_nmi_int_sink_in_0),UVM_NONE)
        end

        if(this.auto_inner_nmi_int_sink_in_1!=rhs_.auto_inner_nmi_int_sink_in_1) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_nmi_int_sink_in_1=0x%0h while the rhs_.auto_inner_nmi_int_sink_in_1=0x%0h",this.auto_inner_nmi_int_sink_in_1,rhs_.auto_inner_nmi_int_sink_in_1),UVM_NONE)
        end

        if(this.auto_inner_plic_int_sink_in_1_0!=rhs_.auto_inner_plic_int_sink_in_1_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_plic_int_sink_in_1_0=0x%0h while the rhs_.auto_inner_plic_int_sink_in_1_0=0x%0h",this.auto_inner_plic_int_sink_in_1_0,rhs_.auto_inner_plic_int_sink_in_1_0),UVM_NONE)
        end

        if(this.auto_inner_plic_int_sink_in_0_0!=rhs_.auto_inner_plic_int_sink_in_0_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_plic_int_sink_in_0_0=0x%0h while the rhs_.auto_inner_plic_int_sink_in_0_0=0x%0h",this.auto_inner_plic_int_sink_in_0_0,rhs_.auto_inner_plic_int_sink_in_0_0),UVM_NONE)
        end

        if(this.auto_inner_clint_int_sink_in_0!=rhs_.auto_inner_clint_int_sink_in_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_clint_int_sink_in_0=0x%0h while the rhs_.auto_inner_clint_int_sink_in_0=0x%0h",this.auto_inner_clint_int_sink_in_0,rhs_.auto_inner_clint_int_sink_in_0),UVM_NONE)
        end

        if(this.auto_inner_clint_int_sink_in_1!=rhs_.auto_inner_clint_int_sink_in_1) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.auto_inner_clint_int_sink_in_1=0x%0h while the rhs_.auto_inner_clint_int_sink_in_1=0x%0h",this.auto_inner_clint_int_sink_in_1,rhs_.auto_inner_clint_int_sink_in_1),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
