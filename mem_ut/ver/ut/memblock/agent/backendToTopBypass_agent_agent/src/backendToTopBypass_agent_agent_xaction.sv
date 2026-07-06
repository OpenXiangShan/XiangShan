//=========================================================
//File name    : backendToTopBypass_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : backendToTopBypass_agent_agent_xaction
//Discribution : backendToTopBypass_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef BACKENDTOTOPBYPASS_AGENT_AGENT_XACTION__SV
`define BACKENDTOTOPBYPASS_AGENT_AGENT_XACTION__SV

class backendToTopBypass_agent_agent_xaction  extends tcnt_data_base;
    // Simple backend-to-mem sideband controls. Keep them two-state and idle-low
    // by default; scenario sequences can selectively assert them.
    rand bit io_ooo_to_mem_backendToTopBypass_cpuWfi;
    rand bit io_ooo_to_mem_backendToTopBypass_cpuCriticalError;
    rand bit io_ooo_to_mem_backendToTopBypass_msiAck;

    extern constraint default_io_ooo_to_mem_backendToTopBypass_cpuWfi_cons;
    extern constraint default_io_ooo_to_mem_backendToTopBypass_cpuCriticalError_cons;
    extern constraint default_io_ooo_to_mem_backendToTopBypass_msiAck_cons;

    extern function new(string name="backendToTopBypass_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(backendToTopBypass_agent_agent_xaction)
        `uvm_field_int(io_ooo_to_mem_backendToTopBypass_cpuWfi, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_backendToTopBypass_cpuCriticalError, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_backendToTopBypass_msiAck, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:backendToTopBypass_agent_agent_xaction

constraint backendToTopBypass_agent_agent_xaction::default_io_ooo_to_mem_backendToTopBypass_cpuWfi_cons{
    io_ooo_to_mem_backendToTopBypass_cpuWfi inside {1'b0, 1'b1};
}

constraint backendToTopBypass_agent_agent_xaction::default_io_ooo_to_mem_backendToTopBypass_cpuCriticalError_cons{
    io_ooo_to_mem_backendToTopBypass_cpuCriticalError inside {1'b0, 1'b1};
}

constraint backendToTopBypass_agent_agent_xaction::default_io_ooo_to_mem_backendToTopBypass_msiAck_cons{
    io_ooo_to_mem_backendToTopBypass_msiAck inside {1'b0, 1'b1};
}

function backendToTopBypass_agent_agent_xaction::new(string name = "backendToTopBypass_agent_agent_xaction");
    super.new();
endfunction:new

function void backendToTopBypass_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void backendToTopBypass_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void backendToTopBypass_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void backendToTopBypass_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string backendToTopBypass_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sio_ooo_to_mem_backendToTopBypass_cpuWfi = 0x%0h ",pkt_str,this.io_ooo_to_mem_backendToTopBypass_cpuWfi);
    pkt_str = $sformatf("%sio_ooo_to_mem_backendToTopBypass_cpuCriticalError = 0x%0h ",pkt_str,this.io_ooo_to_mem_backendToTopBypass_cpuCriticalError);
    pkt_str = $sformatf("%sio_ooo_to_mem_backendToTopBypass_msiAck = 0x%0h ",pkt_str,this.io_ooo_to_mem_backendToTopBypass_msiAck);

    return pkt_str;
endfunction:psdisplay

function bit backendToTopBypass_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    backendToTopBypass_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a backendToTopBypass_agent_agent_xaction or its extend"))
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

        if(this.io_ooo_to_mem_backendToTopBypass_cpuWfi!=rhs_.io_ooo_to_mem_backendToTopBypass_cpuWfi) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_backendToTopBypass_cpuWfi=0x%0h while the rhs_.io_ooo_to_mem_backendToTopBypass_cpuWfi=0x%0h",this.io_ooo_to_mem_backendToTopBypass_cpuWfi,rhs_.io_ooo_to_mem_backendToTopBypass_cpuWfi),UVM_NONE)
        end

        if(this.io_ooo_to_mem_backendToTopBypass_cpuCriticalError!=rhs_.io_ooo_to_mem_backendToTopBypass_cpuCriticalError) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_backendToTopBypass_cpuCriticalError=0x%0h while the rhs_.io_ooo_to_mem_backendToTopBypass_cpuCriticalError=0x%0h",this.io_ooo_to_mem_backendToTopBypass_cpuCriticalError,rhs_.io_ooo_to_mem_backendToTopBypass_cpuCriticalError),UVM_NONE)
        end

        if(this.io_ooo_to_mem_backendToTopBypass_msiAck!=rhs_.io_ooo_to_mem_backendToTopBypass_msiAck) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_backendToTopBypass_msiAck=0x%0h while the rhs_.io_ooo_to_mem_backendToTopBypass_msiAck=0x%0h",this.io_ooo_to_mem_backendToTopBypass_msiAck,rhs_.io_ooo_to_mem_backendToTopBypass_msiAck),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
