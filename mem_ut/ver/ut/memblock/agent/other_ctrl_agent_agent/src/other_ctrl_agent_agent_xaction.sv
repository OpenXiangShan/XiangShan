//=========================================================
//File name    : other_ctrl_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : other_ctrl_agent_agent_xaction
//Discribution : other_ctrl_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef OTHER_CTRL_AGENT_AGENT_XACTION__SV
`define OTHER_CTRL_AGENT_AGENT_XACTION__SV

class other_ctrl_agent_agent_xaction  extends tcnt_data_base;
    rand bit [5:0] io_hartId           ;
    rand bit io_dcacheError_ecc_error_valid;
    rand bit [47:0] io_dcacheError_ecc_error_bits;
    rand bit io_uncacheError_ecc_error_valid;
    rand bit [47:0] io_uncacheError_ecc_error_bits;
    rand bit io_memInfo_sqFull         ;
    rand bit io_memInfo_lqFull         ;
    rand bit io_memInfo_dcacheMSHRFull ;
    rand bit [5:0] io_inner_hartId     ;
    rand bit [47:0] io_inner_reset_vector;
    rand bit [47:0] io_outer_reset_vector;
    rand bit io_outer_cpu_wfi          ;
    rand bit io_outer_l2_flush_en      ;
    rand bit io_outer_power_down_en    ;
    rand bit io_outer_cpu_critical_error;
    rand bit io_outer_msi_ack          ;
    rand bit io_inner_beu_errors_icache_ecc_error_valid;
    rand bit [47:0] io_inner_beu_errors_icache_ecc_error_bits;
    rand bit io_outer_beu_errors_icache_ecc_error_valid;
    rand bit [47:0] io_outer_beu_errors_icache_ecc_error_bits;
    rand bit io_reset_backend          ;

    extern constraint default_io_hartId_cons;
    extern constraint default_io_dcacheError_ecc_error_valid_cons;
    extern constraint default_io_dcacheError_ecc_error_bits_cons;
    extern constraint default_io_uncacheError_ecc_error_valid_cons;
    extern constraint default_io_uncacheError_ecc_error_bits_cons;
    extern constraint default_io_memInfo_sqFull_cons;
    extern constraint default_io_memInfo_lqFull_cons;
    extern constraint default_io_memInfo_dcacheMSHRFull_cons;
    extern constraint default_io_inner_hartId_cons;
    extern constraint default_io_inner_reset_vector_cons;
    extern constraint default_io_outer_reset_vector_cons;
    extern constraint default_io_outer_cpu_wfi_cons;
    extern constraint default_io_outer_l2_flush_en_cons;
    extern constraint default_io_outer_power_down_en_cons;
    extern constraint default_io_outer_cpu_critical_error_cons;
    extern constraint default_io_outer_msi_ack_cons;
    extern constraint default_io_inner_beu_errors_icache_ecc_error_valid_cons;
    extern constraint default_io_inner_beu_errors_icache_ecc_error_bits_cons;
    extern constraint default_io_outer_beu_errors_icache_ecc_error_valid_cons;
    extern constraint default_io_outer_beu_errors_icache_ecc_error_bits_cons;
    extern constraint default_io_reset_backend_cons;

    extern function new(string name="other_ctrl_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(other_ctrl_agent_agent_xaction)
        `uvm_field_int(io_hartId, UVM_ALL_ON);
        `uvm_field_int(io_dcacheError_ecc_error_valid, UVM_ALL_ON);
        `uvm_field_int(io_dcacheError_ecc_error_bits, UVM_ALL_ON);
        `uvm_field_int(io_uncacheError_ecc_error_valid, UVM_ALL_ON);
        `uvm_field_int(io_uncacheError_ecc_error_bits, UVM_ALL_ON);
        `uvm_field_int(io_memInfo_sqFull, UVM_ALL_ON);
        `uvm_field_int(io_memInfo_lqFull, UVM_ALL_ON);
        `uvm_field_int(io_memInfo_dcacheMSHRFull, UVM_ALL_ON);
        `uvm_field_int(io_inner_hartId, UVM_ALL_ON);
        `uvm_field_int(io_inner_reset_vector, UVM_ALL_ON);
        `uvm_field_int(io_outer_reset_vector, UVM_ALL_ON);
        `uvm_field_int(io_outer_cpu_wfi, UVM_ALL_ON);
        `uvm_field_int(io_outer_l2_flush_en, UVM_ALL_ON);
        `uvm_field_int(io_outer_power_down_en, UVM_ALL_ON);
        `uvm_field_int(io_outer_cpu_critical_error, UVM_ALL_ON);
        `uvm_field_int(io_outer_msi_ack, UVM_ALL_ON);
        `uvm_field_int(io_inner_beu_errors_icache_ecc_error_valid, UVM_ALL_ON);
        `uvm_field_int(io_inner_beu_errors_icache_ecc_error_bits, UVM_ALL_ON);
        `uvm_field_int(io_outer_beu_errors_icache_ecc_error_valid, UVM_ALL_ON);
        `uvm_field_int(io_outer_beu_errors_icache_ecc_error_bits, UVM_ALL_ON);
        `uvm_field_int(io_reset_backend, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:other_ctrl_agent_agent_xaction

constraint other_ctrl_agent_agent_xaction::default_io_hartId_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_dcacheError_ecc_error_valid_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_dcacheError_ecc_error_bits_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_uncacheError_ecc_error_valid_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_uncacheError_ecc_error_bits_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_memInfo_sqFull_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_memInfo_lqFull_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_memInfo_dcacheMSHRFull_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_inner_hartId_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_inner_reset_vector_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_outer_reset_vector_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_outer_cpu_wfi_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_outer_l2_flush_en_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_outer_power_down_en_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_outer_cpu_critical_error_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_outer_msi_ack_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_inner_beu_errors_icache_ecc_error_valid_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_inner_beu_errors_icache_ecc_error_bits_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_outer_beu_errors_icache_ecc_error_valid_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_outer_beu_errors_icache_ecc_error_bits_cons{

}

constraint other_ctrl_agent_agent_xaction::default_io_reset_backend_cons{

}

function other_ctrl_agent_agent_xaction::new(string name = "other_ctrl_agent_agent_xaction");
    super.new();
endfunction:new

function void other_ctrl_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void other_ctrl_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void other_ctrl_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void other_ctrl_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string other_ctrl_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sio_hartId = 0x%0h ",pkt_str,this.io_hartId);
    pkt_str = $sformatf("%sio_dcacheError_ecc_error_valid = 0x%0h ",pkt_str,this.io_dcacheError_ecc_error_valid);
    pkt_str = $sformatf("%sio_dcacheError_ecc_error_bits = 0x%0h ",pkt_str,this.io_dcacheError_ecc_error_bits);
    pkt_str = $sformatf("%sio_uncacheError_ecc_error_valid = 0x%0h ",pkt_str,this.io_uncacheError_ecc_error_valid);
    pkt_str = $sformatf("%sio_uncacheError_ecc_error_bits = 0x%0h ",pkt_str,this.io_uncacheError_ecc_error_bits);
    pkt_str = $sformatf("%sio_memInfo_sqFull = 0x%0h ",pkt_str,this.io_memInfo_sqFull);
    pkt_str = $sformatf("%sio_memInfo_lqFull = 0x%0h ",pkt_str,this.io_memInfo_lqFull);
    pkt_str = $sformatf("%sio_memInfo_dcacheMSHRFull = 0x%0h ",pkt_str,this.io_memInfo_dcacheMSHRFull);
    pkt_str = $sformatf("%sio_inner_hartId = 0x%0h ",pkt_str,this.io_inner_hartId);
    pkt_str = $sformatf("%sio_inner_reset_vector = 0x%0h ",pkt_str,this.io_inner_reset_vector);
    pkt_str = $sformatf("%sio_outer_reset_vector = 0x%0h ",pkt_str,this.io_outer_reset_vector);
    pkt_str = $sformatf("%sio_outer_cpu_wfi = 0x%0h ",pkt_str,this.io_outer_cpu_wfi);
    pkt_str = $sformatf("%sio_outer_l2_flush_en = 0x%0h ",pkt_str,this.io_outer_l2_flush_en);
    pkt_str = $sformatf("%sio_outer_power_down_en = 0x%0h ",pkt_str,this.io_outer_power_down_en);
    pkt_str = $sformatf("%sio_outer_cpu_critical_error = 0x%0h ",pkt_str,this.io_outer_cpu_critical_error);
    pkt_str = $sformatf("%sio_outer_msi_ack = 0x%0h ",pkt_str,this.io_outer_msi_ack);
    pkt_str = $sformatf("%sio_inner_beu_errors_icache_ecc_error_valid = 0x%0h ",pkt_str,this.io_inner_beu_errors_icache_ecc_error_valid);
    pkt_str = $sformatf("%sio_inner_beu_errors_icache_ecc_error_bits = 0x%0h ",pkt_str,this.io_inner_beu_errors_icache_ecc_error_bits);
    pkt_str = $sformatf("%sio_outer_beu_errors_icache_ecc_error_valid = 0x%0h ",pkt_str,this.io_outer_beu_errors_icache_ecc_error_valid);
    pkt_str = $sformatf("%sio_outer_beu_errors_icache_ecc_error_bits = 0x%0h ",pkt_str,this.io_outer_beu_errors_icache_ecc_error_bits);
    pkt_str = $sformatf("%sio_reset_backend = 0x%0h ",pkt_str,this.io_reset_backend);

    return pkt_str;
endfunction:psdisplay

function bit other_ctrl_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    other_ctrl_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a other_ctrl_agent_agent_xaction or its extend"))
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

        if(this.io_hartId!=rhs_.io_hartId) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_hartId=0x%0h while the rhs_.io_hartId=0x%0h",this.io_hartId,rhs_.io_hartId),UVM_NONE)
        end

        if(this.io_dcacheError_ecc_error_valid!=rhs_.io_dcacheError_ecc_error_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_dcacheError_ecc_error_valid=0x%0h while the rhs_.io_dcacheError_ecc_error_valid=0x%0h",this.io_dcacheError_ecc_error_valid,rhs_.io_dcacheError_ecc_error_valid),UVM_NONE)
        end

        if(this.io_dcacheError_ecc_error_bits!=rhs_.io_dcacheError_ecc_error_bits) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_dcacheError_ecc_error_bits=0x%0h while the rhs_.io_dcacheError_ecc_error_bits=0x%0h",this.io_dcacheError_ecc_error_bits,rhs_.io_dcacheError_ecc_error_bits),UVM_NONE)
        end

        if(this.io_uncacheError_ecc_error_valid!=rhs_.io_uncacheError_ecc_error_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_uncacheError_ecc_error_valid=0x%0h while the rhs_.io_uncacheError_ecc_error_valid=0x%0h",this.io_uncacheError_ecc_error_valid,rhs_.io_uncacheError_ecc_error_valid),UVM_NONE)
        end

        if(this.io_uncacheError_ecc_error_bits!=rhs_.io_uncacheError_ecc_error_bits) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_uncacheError_ecc_error_bits=0x%0h while the rhs_.io_uncacheError_ecc_error_bits=0x%0h",this.io_uncacheError_ecc_error_bits,rhs_.io_uncacheError_ecc_error_bits),UVM_NONE)
        end

        if(this.io_memInfo_sqFull!=rhs_.io_memInfo_sqFull) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_memInfo_sqFull=0x%0h while the rhs_.io_memInfo_sqFull=0x%0h",this.io_memInfo_sqFull,rhs_.io_memInfo_sqFull),UVM_NONE)
        end

        if(this.io_memInfo_lqFull!=rhs_.io_memInfo_lqFull) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_memInfo_lqFull=0x%0h while the rhs_.io_memInfo_lqFull=0x%0h",this.io_memInfo_lqFull,rhs_.io_memInfo_lqFull),UVM_NONE)
        end

        if(this.io_memInfo_dcacheMSHRFull!=rhs_.io_memInfo_dcacheMSHRFull) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_memInfo_dcacheMSHRFull=0x%0h while the rhs_.io_memInfo_dcacheMSHRFull=0x%0h",this.io_memInfo_dcacheMSHRFull,rhs_.io_memInfo_dcacheMSHRFull),UVM_NONE)
        end

        if(this.io_inner_hartId!=rhs_.io_inner_hartId) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_inner_hartId=0x%0h while the rhs_.io_inner_hartId=0x%0h",this.io_inner_hartId,rhs_.io_inner_hartId),UVM_NONE)
        end

        if(this.io_inner_reset_vector!=rhs_.io_inner_reset_vector) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_inner_reset_vector=0x%0h while the rhs_.io_inner_reset_vector=0x%0h",this.io_inner_reset_vector,rhs_.io_inner_reset_vector),UVM_NONE)
        end

        if(this.io_outer_reset_vector!=rhs_.io_outer_reset_vector) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_outer_reset_vector=0x%0h while the rhs_.io_outer_reset_vector=0x%0h",this.io_outer_reset_vector,rhs_.io_outer_reset_vector),UVM_NONE)
        end

        if(this.io_outer_cpu_wfi!=rhs_.io_outer_cpu_wfi) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_outer_cpu_wfi=0x%0h while the rhs_.io_outer_cpu_wfi=0x%0h",this.io_outer_cpu_wfi,rhs_.io_outer_cpu_wfi),UVM_NONE)
        end

        if(this.io_outer_l2_flush_en!=rhs_.io_outer_l2_flush_en) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_outer_l2_flush_en=0x%0h while the rhs_.io_outer_l2_flush_en=0x%0h",this.io_outer_l2_flush_en,rhs_.io_outer_l2_flush_en),UVM_NONE)
        end

        if(this.io_outer_power_down_en!=rhs_.io_outer_power_down_en) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_outer_power_down_en=0x%0h while the rhs_.io_outer_power_down_en=0x%0h",this.io_outer_power_down_en,rhs_.io_outer_power_down_en),UVM_NONE)
        end

        if(this.io_outer_cpu_critical_error!=rhs_.io_outer_cpu_critical_error) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_outer_cpu_critical_error=0x%0h while the rhs_.io_outer_cpu_critical_error=0x%0h",this.io_outer_cpu_critical_error,rhs_.io_outer_cpu_critical_error),UVM_NONE)
        end

        if(this.io_outer_msi_ack!=rhs_.io_outer_msi_ack) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_outer_msi_ack=0x%0h while the rhs_.io_outer_msi_ack=0x%0h",this.io_outer_msi_ack,rhs_.io_outer_msi_ack),UVM_NONE)
        end

        if(this.io_inner_beu_errors_icache_ecc_error_valid!=rhs_.io_inner_beu_errors_icache_ecc_error_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_inner_beu_errors_icache_ecc_error_valid=0x%0h while the rhs_.io_inner_beu_errors_icache_ecc_error_valid=0x%0h",this.io_inner_beu_errors_icache_ecc_error_valid,rhs_.io_inner_beu_errors_icache_ecc_error_valid),UVM_NONE)
        end

        if(this.io_inner_beu_errors_icache_ecc_error_bits!=rhs_.io_inner_beu_errors_icache_ecc_error_bits) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_inner_beu_errors_icache_ecc_error_bits=0x%0h while the rhs_.io_inner_beu_errors_icache_ecc_error_bits=0x%0h",this.io_inner_beu_errors_icache_ecc_error_bits,rhs_.io_inner_beu_errors_icache_ecc_error_bits),UVM_NONE)
        end

        if(this.io_outer_beu_errors_icache_ecc_error_valid!=rhs_.io_outer_beu_errors_icache_ecc_error_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_outer_beu_errors_icache_ecc_error_valid=0x%0h while the rhs_.io_outer_beu_errors_icache_ecc_error_valid=0x%0h",this.io_outer_beu_errors_icache_ecc_error_valid,rhs_.io_outer_beu_errors_icache_ecc_error_valid),UVM_NONE)
        end

        if(this.io_outer_beu_errors_icache_ecc_error_bits!=rhs_.io_outer_beu_errors_icache_ecc_error_bits) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_outer_beu_errors_icache_ecc_error_bits=0x%0h while the rhs_.io_outer_beu_errors_icache_ecc_error_bits=0x%0h",this.io_outer_beu_errors_icache_ecc_error_bits,rhs_.io_outer_beu_errors_icache_ecc_error_bits),UVM_NONE)
        end

        if(this.io_reset_backend!=rhs_.io_reset_backend) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_reset_backend=0x%0h while the rhs_.io_reset_backend=0x%0h",this.io_reset_backend,rhs_.io_reset_backend),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
