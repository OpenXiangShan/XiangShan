//=========================================================
//File name    : vecissue_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : vecissue_agent_agent_xaction
//Discribution : vecissue_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef VECISSUE_AGENT_AGENT_XACTION__SV
`define VECISSUE_AGENT_AGENT_XACTION__SV

class vecissue_agent_agent_xaction  extends tcnt_data_base;
    // vecissue is a highly structured interface. Base constraints keep only the
    // stable vtype/index/capacity legality; scenario templates should still
    // refine fuOpType and vpu fields together.

    rand bit [4:0] io_ooo_to_mem_issueVldu_0_bits_flowNum;
    rand bit io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay;
    rand bit [127:0] io_ooo_to_mem_issueVldu_0_bits_src_0;
    rand bit [127:0] io_ooo_to_mem_issueVldu_0_bits_src_1;
    rand bit [127:0] io_ooo_to_mem_issueVldu_0_bits_src_2;
    rand bit [127:0] io_ooo_to_mem_issueVldu_0_bits_src_3;
    rand bit [127:0] io_ooo_to_mem_issueVldu_0_bits_src_4;
    rand bit [`MEMBLOCK_DUT_FTQ_OFFSET_W-1:0] io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset;
    rand bit io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag;
    rand bit [`MEMBLOCK_DUT_FTQ_PTR_VALUE_W-1:0] io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value;
    rand bit [8:0] io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType;
    rand bit [`MEMBLOCK_DUT_FUTYPE_W-1:0] io_ooo_to_mem_issueVldu_0_bits_uop_fuType;
    rand bit io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag;
    rand bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value;
    rand bit [7:0] io_ooo_to_mem_issueVldu_0_bits_uop_pdest;
    rand bit io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value;
    rand bit io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag;
    rand bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value;
    rand bit io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen;
    rand bit io_ooo_to_mem_issueVldu_0_bits_uop_vecWen;
    rand bit io_ooo_to_mem_issueVldu_0_bits_uop_vlWen;
    rand bit io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff;
    rand bit io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop;
    rand bit [2:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf;
    rand bit [1:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew;
    rand bit [2:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul;
    rand bit io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm;
    rand bit io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma;
    rand bit [127:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask;
    rand bit [1:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew;
    rand bit [7:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart;
    rand bit io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta;
    rand bit [6:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx;
    rand bit [15:0] io_ooo_to_mem_issueVldu_0_bits_vecReplayMask;
    rand bit [3:0] io_ooo_to_mem_issueVldu_0_bits_vecReplayMbIdx;
    rand bit io_ooo_to_mem_issueVldu_0_ready;
    rand bit io_ooo_to_mem_issueVldu_0_valid;
    rand bit [4:0] io_ooo_to_mem_issueVldu_1_bits_flowNum;
    rand bit io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay;
    rand bit [127:0] io_ooo_to_mem_issueVldu_1_bits_src_0;
    rand bit [127:0] io_ooo_to_mem_issueVldu_1_bits_src_1;
    rand bit [127:0] io_ooo_to_mem_issueVldu_1_bits_src_2;
    rand bit [127:0] io_ooo_to_mem_issueVldu_1_bits_src_3;
    rand bit [127:0] io_ooo_to_mem_issueVldu_1_bits_src_4;
    rand bit [`MEMBLOCK_DUT_FTQ_OFFSET_W-1:0] io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset;
    rand bit io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag;
    rand bit [`MEMBLOCK_DUT_FTQ_PTR_VALUE_W-1:0] io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value;
    rand bit [8:0] io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType;
    rand bit io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag;
    rand bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value;
    rand bit [7:0] io_ooo_to_mem_issueVldu_1_bits_uop_pdest;
    rand bit io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value;
    rand bit io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag;
    rand bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value;
    rand bit io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen;
    rand bit io_ooo_to_mem_issueVldu_1_bits_uop_vecWen;
    rand bit io_ooo_to_mem_issueVldu_1_bits_uop_vlWen;
    rand bit io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff;
    rand bit io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop;
    rand bit [2:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf;
    rand bit [1:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew;
    rand bit [2:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul;
    rand bit io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm;
    rand bit io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma;
    rand bit [127:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask;
    rand bit [1:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew;
    rand bit [7:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart;
    rand bit io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta;
    rand bit [6:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx;
    rand bit [15:0] io_ooo_to_mem_issueVldu_1_bits_vecReplayMask;
    rand bit [3:0] io_ooo_to_mem_issueVldu_1_bits_vecReplayMbIdx;
    rand bit io_ooo_to_mem_issueVldu_1_ready;
    rand bit io_ooo_to_mem_issueVldu_1_valid;

    extern function new(string name="vecissue_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(vecissue_agent_agent_xaction)

        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_flowNum, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_src_1, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_src_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_src_3, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_src_4, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_pdest, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_vecWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_vlWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_vecReplayMask, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_bits_vecReplayMbIdx, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_flowNum, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_src_1, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_src_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_src_3, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_src_4, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_pdest, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_vecWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_vlWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_vecReplayMask, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_bits_vecReplayMbIdx, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_issueVldu_1_valid, UVM_ALL_ON);
    `uvm_object_utils_end

endclass:vecissue_agent_agent_xaction
































































































































function vecissue_agent_agent_xaction::new(string name = "vecissue_agent_agent_xaction");
    super.new();
endfunction:new

function void vecissue_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void vecissue_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void vecissue_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void vecissue_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string vecissue_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end

    return pkt_str;
endfunction:psdisplay

function bit vecissue_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    vecissue_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a vecissue_agent_agent_xaction or its extend"))
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
































































































































    end
    return super_result;
endfunction:compare

`endif
