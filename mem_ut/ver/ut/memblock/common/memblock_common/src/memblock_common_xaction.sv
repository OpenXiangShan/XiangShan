//=========================================================
//File name    : memblock_common_xaction.sv
//Author       : OpenAI_Codex
//Module name  : memblock_common_xaction
//Discribution : memblock_common_xaction : common transaction
//Date         : 2026-04-12
//=========================================================
`ifndef MEMBLOCK_COMMON_XACTION__SV
`define MEMBLOCK_COMMON_XACTION__SV

class memblock_common_xaction  extends tcnt_data_base;
    backendToTopBypass_agent_agent_xaction backendToTopBypass_agent_tr;
    fence_agent_agent_xaction fence_agent_tr;
    csr_ctrl_agent_agent_xaction csr_ctrl_agent_tr;
    lsqcommit_agent_agent_xaction lsqcommit_agent_tr;
    lsqenq_agent_agent_xaction lsqenq_agent_tr;
    lintsissue_agent_agent_xaction lintsissue_agent_tr;
    vecissue_agent_agent_xaction vecissue_agent_tr;
    redirect_agent_agent_xaction redirect_agent_tr;
    sbuffer_agent_agent_xaction sbuffer_agent_tr;
    dcache_agent_agent_xaction dcache_agent_tr;
    int_sink_agent_agent_xaction int_sink_agent_tr;
    L2tlb_agent_agent_xaction L2tlb_agent_tr;
    itlb_agent_agent_xaction itlb_agent_tr;
    prefetch_agent_agent_xaction prefetch_agent_tr;
    io_mem_to_ooo_ctrl_agent_agent_xaction io_mem_to_ooo_ctrl_agent_tr;
    io_mem_to_ooo_int_wb_agent_agent_xaction io_mem_to_ooo_int_wb_agent_tr;
    io_mem_to_ooo_vec_wb_agent_agent_xaction io_mem_to_ooo_vec_wb_agent_tr;
    io_mem_to_ooo_wakeup_agent_agent_xaction io_mem_to_ooo_wakeup_agent_tr;
    io_mem_to_ooo_iq_feedback_agent_agent_xaction io_mem_to_ooo_iq_feedback_agent_tr;
    other_ctrl_agent_agent_xaction other_ctrl_agent_tr;

    extern function new(string name="memblock_common_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function void pack_backendToTopBypass_agent(uvm_object tr);
    extern function void pack_fence_agent(uvm_object tr);
    extern function void pack_csr_ctrl_agent(uvm_object tr);
    extern function void pack_lsqcommit_agent(uvm_object tr);
    extern function void pack_lsqenq_agent(uvm_object tr);
    extern function void pack_lintsissue_agent(uvm_object tr);
    extern function void pack_vecissue_agent(uvm_object tr);
    extern function void pack_redirect_agent(uvm_object tr);
    extern function void pack_sbuffer_agent(uvm_object tr);
    extern function void pack_dcache_agent(uvm_object tr);
    extern function void pack_int_sink_agent(uvm_object tr);
    extern function void pack_L2tlb_agent(uvm_object tr);
    extern function void pack_itlb_agent(uvm_object tr);
    extern function void pack_prefetch_agent(uvm_object tr);
    extern function void pack_io_mem_to_ooo_ctrl_agent(uvm_object tr);
    extern function void pack_io_mem_to_ooo_int_wb_agent(uvm_object tr);
    extern function void pack_io_mem_to_ooo_vec_wb_agent(uvm_object tr);
    extern function void pack_io_mem_to_ooo_wakeup_agent(uvm_object tr);
    extern function void pack_io_mem_to_ooo_iq_feedback_agent(uvm_object tr);
    extern function void pack_other_ctrl_agent(uvm_object tr);

    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(memblock_common_xaction)
        `uvm_field_object(backendToTopBypass_agent_tr, UVM_ALL_ON);
        `uvm_field_object(fence_agent_tr, UVM_ALL_ON);
        `uvm_field_object(csr_ctrl_agent_tr, UVM_ALL_ON);
        `uvm_field_object(lsqcommit_agent_tr, UVM_ALL_ON);
        `uvm_field_object(lsqenq_agent_tr, UVM_ALL_ON);
        `uvm_field_object(lintsissue_agent_tr, UVM_ALL_ON);
        `uvm_field_object(vecissue_agent_tr, UVM_ALL_ON);
        `uvm_field_object(redirect_agent_tr, UVM_ALL_ON);
        `uvm_field_object(sbuffer_agent_tr, UVM_ALL_ON);
        `uvm_field_object(dcache_agent_tr, UVM_ALL_ON);
        `uvm_field_object(int_sink_agent_tr, UVM_ALL_ON);
        `uvm_field_object(L2tlb_agent_tr, UVM_ALL_ON);
        `uvm_field_object(itlb_agent_tr, UVM_ALL_ON);
        `uvm_field_object(prefetch_agent_tr, UVM_ALL_ON);
        `uvm_field_object(io_mem_to_ooo_ctrl_agent_tr, UVM_ALL_ON);
        `uvm_field_object(io_mem_to_ooo_int_wb_agent_tr, UVM_ALL_ON);
        `uvm_field_object(io_mem_to_ooo_vec_wb_agent_tr, UVM_ALL_ON);
        `uvm_field_object(io_mem_to_ooo_wakeup_agent_tr, UVM_ALL_ON);
        `uvm_field_object(io_mem_to_ooo_iq_feedback_agent_tr, UVM_ALL_ON);
        `uvm_field_object(other_ctrl_agent_tr, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:memblock_common_xaction

function memblock_common_xaction::new(string name = "memblock_common_xaction");
    super.new();
endfunction:new

function void memblock_common_xaction::pack();
    super.pack();
endfunction:pack
function void memblock_common_xaction::unpack();
    super.unpack();
endfunction:unpack
function void memblock_common_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void memblock_common_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function void memblock_common_xaction::pack_backendToTopBypass_agent(uvm_object tr);
    backendToTopBypass_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a backendToTopBypass_agent_agent_xaction or its extend"));
    end
    this.backendToTopBypass_agent_tr = tr_;
endfunction:pack_backendToTopBypass_agent

function void memblock_common_xaction::pack_fence_agent(uvm_object tr);
    fence_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a fence_agent_agent_xaction or its extend"));
    end
    this.fence_agent_tr = tr_;
endfunction:pack_fence_agent

function void memblock_common_xaction::pack_csr_ctrl_agent(uvm_object tr);
    csr_ctrl_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a csr_ctrl_agent_agent_xaction or its extend"));
    end
    this.csr_ctrl_agent_tr = tr_;
endfunction:pack_csr_ctrl_agent

function void memblock_common_xaction::pack_lsqcommit_agent(uvm_object tr);
    lsqcommit_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a lsqcommit_agent_agent_xaction or its extend"));
    end
    this.lsqcommit_agent_tr = tr_;
endfunction:pack_lsqcommit_agent

function void memblock_common_xaction::pack_lsqenq_agent(uvm_object tr);
    lsqenq_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a lsqenq_agent_agent_xaction or its extend"));
    end
    this.lsqenq_agent_tr = tr_;
endfunction:pack_lsqenq_agent

function void memblock_common_xaction::pack_lintsissue_agent(uvm_object tr);
    lintsissue_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a lintsissue_agent_agent_xaction or its extend"));
    end
    this.lintsissue_agent_tr = tr_;
endfunction:pack_lintsissue_agent

function void memblock_common_xaction::pack_vecissue_agent(uvm_object tr);
    vecissue_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a vecissue_agent_agent_xaction or its extend"));
    end
    this.vecissue_agent_tr = tr_;
endfunction:pack_vecissue_agent

function void memblock_common_xaction::pack_redirect_agent(uvm_object tr);
    redirect_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a redirect_agent_agent_xaction or its extend"));
    end
    this.redirect_agent_tr = tr_;
endfunction:pack_redirect_agent

function void memblock_common_xaction::pack_sbuffer_agent(uvm_object tr);
    sbuffer_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a sbuffer_agent_agent_xaction or its extend"));
    end
    this.sbuffer_agent_tr = tr_;
endfunction:pack_sbuffer_agent

function void memblock_common_xaction::pack_dcache_agent(uvm_object tr);
    dcache_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a dcache_agent_agent_xaction or its extend"));
    end
    this.dcache_agent_tr = tr_;
endfunction:pack_dcache_agent

function void memblock_common_xaction::pack_int_sink_agent(uvm_object tr);
    int_sink_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a int_sink_agent_agent_xaction or its extend"));
    end
    this.int_sink_agent_tr = tr_;
endfunction:pack_int_sink_agent

function void memblock_common_xaction::pack_L2tlb_agent(uvm_object tr);
    L2tlb_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a L2tlb_agent_agent_xaction or its extend"));
    end
    this.L2tlb_agent_tr = tr_;
endfunction:pack_L2tlb_agent

function void memblock_common_xaction::pack_itlb_agent(uvm_object tr);
    itlb_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a itlb_agent_agent_xaction or its extend"));
    end
    this.itlb_agent_tr = tr_;
endfunction:pack_itlb_agent

function void memblock_common_xaction::pack_prefetch_agent(uvm_object tr);
    prefetch_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a prefetch_agent_agent_xaction or its extend"));
    end
    this.prefetch_agent_tr = tr_;
endfunction:pack_prefetch_agent

function void memblock_common_xaction::pack_io_mem_to_ooo_ctrl_agent(uvm_object tr);
    io_mem_to_ooo_ctrl_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a io_mem_to_ooo_ctrl_agent_agent_xaction or its extend"));
    end
    this.io_mem_to_ooo_ctrl_agent_tr = tr_;
endfunction:pack_io_mem_to_ooo_ctrl_agent

function void memblock_common_xaction::pack_io_mem_to_ooo_int_wb_agent(uvm_object tr);
    io_mem_to_ooo_int_wb_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a io_mem_to_ooo_int_wb_agent_agent_xaction or its extend"));
    end
    this.io_mem_to_ooo_int_wb_agent_tr = tr_;
endfunction:pack_io_mem_to_ooo_int_wb_agent

function void memblock_common_xaction::pack_io_mem_to_ooo_vec_wb_agent(uvm_object tr);
    io_mem_to_ooo_vec_wb_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a io_mem_to_ooo_vec_wb_agent_agent_xaction or its extend"));
    end
    this.io_mem_to_ooo_vec_wb_agent_tr = tr_;
endfunction:pack_io_mem_to_ooo_vec_wb_agent

function void memblock_common_xaction::pack_io_mem_to_ooo_wakeup_agent(uvm_object tr);
    io_mem_to_ooo_wakeup_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a io_mem_to_ooo_wakeup_agent_agent_xaction or its extend"));
    end
    this.io_mem_to_ooo_wakeup_agent_tr = tr_;
endfunction:pack_io_mem_to_ooo_wakeup_agent

function void memblock_common_xaction::pack_io_mem_to_ooo_iq_feedback_agent(uvm_object tr);
    io_mem_to_ooo_iq_feedback_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a io_mem_to_ooo_iq_feedback_agent_agent_xaction or its extend"));
    end
    this.io_mem_to_ooo_iq_feedback_agent_tr = tr_;
endfunction:pack_io_mem_to_ooo_iq_feedback_agent

function void memblock_common_xaction::pack_other_ctrl_agent(uvm_object tr);
    other_ctrl_agent_agent_xaction tr_;
    if(!$cast(tr_, tr)) begin
        `uvm_fatal(get_type_name(),$sformatf("tr is not a other_ctrl_agent_agent_xaction or its extend"));
    end
    this.other_ctrl_agent_tr = tr_;
endfunction:pack_other_ctrl_agent

function string memblock_common_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    if(channel_id == 0)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.backendToTopBypass_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 1)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.fence_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 2)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.csr_ctrl_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 3)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.lsqcommit_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 4)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.lsqenq_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 5)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.lintsissue_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 6)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.vecissue_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 7)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.redirect_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 8)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.sbuffer_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 9)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.dcache_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 10)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.int_sink_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 19)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.L2tlb_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 11)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.itlb_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 12)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.prefetch_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 13)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.io_mem_to_ooo_ctrl_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 14)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.io_mem_to_ooo_int_wb_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 15)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.io_mem_to_ooo_vec_wb_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 16)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.io_mem_to_ooo_wakeup_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 17)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.io_mem_to_ooo_iq_feedback_agent_tr.psdisplay(prefix));
    end
    if(channel_id == 18)begin
        pkt_str = $sformatf("%s%s",pkt_str,this.other_ctrl_agent_tr.psdisplay(prefix));
    end

    return pkt_str;
endfunction:psdisplay

function bit memblock_common_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    memblock_common_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a memblock_common_xaction or its extend"))
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
        if(channel_id == 0)begin
            super_result = this.backendToTopBypass_agent_tr.compare(rhs_.backendToTopBypass_agent_tr);
        end
        if(channel_id == 1)begin
            super_result = this.fence_agent_tr.compare(rhs_.fence_agent_tr);
        end
        if(channel_id == 2)begin
            super_result = this.csr_ctrl_agent_tr.compare(rhs_.csr_ctrl_agent_tr);
        end
        if(channel_id == 3)begin
            super_result = this.lsqcommit_agent_tr.compare(rhs_.lsqcommit_agent_tr);
        end
        if(channel_id == 4)begin
            super_result = this.lsqenq_agent_tr.compare(rhs_.lsqenq_agent_tr);
        end
        if(channel_id == 5)begin
            super_result = this.lintsissue_agent_tr.compare(rhs_.lintsissue_agent_tr);
        end
        if(channel_id == 6)begin
            super_result = this.vecissue_agent_tr.compare(rhs_.vecissue_agent_tr);
        end
        if(channel_id == 7)begin
            super_result = this.redirect_agent_tr.compare(rhs_.redirect_agent_tr);
        end
        if(channel_id == 8)begin
            super_result = this.sbuffer_agent_tr.compare(rhs_.sbuffer_agent_tr);
        end
        if(channel_id == 9)begin
            super_result = this.dcache_agent_tr.compare(rhs_.dcache_agent_tr);
        end
        if(channel_id == 10)begin
            super_result = this.int_sink_agent_tr.compare(rhs_.int_sink_agent_tr);
        end
        if(channel_id == 19)begin
            super_result = this.L2tlb_agent_tr.compare(rhs_.L2tlb_agent_tr);
        end
        if(channel_id == 11)begin
            super_result = this.itlb_agent_tr.compare(rhs_.itlb_agent_tr);
        end
        if(channel_id == 12)begin
            super_result = this.prefetch_agent_tr.compare(rhs_.prefetch_agent_tr);
        end
        if(channel_id == 13)begin
            super_result = this.io_mem_to_ooo_ctrl_agent_tr.compare(rhs_.io_mem_to_ooo_ctrl_agent_tr);
        end
        if(channel_id == 14)begin
            super_result = this.io_mem_to_ooo_int_wb_agent_tr.compare(rhs_.io_mem_to_ooo_int_wb_agent_tr);
        end
        if(channel_id == 15)begin
            super_result = this.io_mem_to_ooo_vec_wb_agent_tr.compare(rhs_.io_mem_to_ooo_vec_wb_agent_tr);
        end
        if(channel_id == 16)begin
            super_result = this.io_mem_to_ooo_wakeup_agent_tr.compare(rhs_.io_mem_to_ooo_wakeup_agent_tr);
        end
        if(channel_id == 17)begin
            super_result = this.io_mem_to_ooo_iq_feedback_agent_tr.compare(rhs_.io_mem_to_ooo_iq_feedback_agent_tr);
        end
        if(channel_id == 18)begin
            super_result = this.other_ctrl_agent_tr.compare(rhs_.other_ctrl_agent_tr);
        end

    end
    return super_result;
endfunction:compare

`endif
