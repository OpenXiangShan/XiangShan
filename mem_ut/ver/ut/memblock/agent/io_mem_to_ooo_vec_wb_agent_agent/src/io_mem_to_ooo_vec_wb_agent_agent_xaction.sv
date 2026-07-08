//=========================================================
//File name    : io_mem_to_ooo_vec_wb_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_vec_wb_agent_agent_xaction
//Discribution : io_mem_to_ooo_vec_wb_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_VEC_WB_AGENT_AGENT_XACTION__SV
`define IO_MEM_TO_OOO_VEC_WB_AGENT_AGENT_XACTION__SV

class io_mem_to_ooo_vec_wb_agent_agent_xaction  extends tcnt_data_base;

    rand bit [127:0] io_mem_to_ooo_writebackVldu_0_bits_data;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_debug_isMMIO;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_debug_isNCIO;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_debug_isPerfCnt;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_13;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_15;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_19;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_21;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_23;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_4;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_5;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_6;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_7;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_flushPipe;
    rand bit [8:0] io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType;
    rand bit [7:0] io_mem_to_ooo_writebackVldu_0_bits_uop_pdest;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_replayInst;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_flag;
    rand bit [7:0] io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_value;
    rand bit [3:0] io_mem_to_ooo_writebackVldu_0_bits_uop_trigger;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_v0Wen;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_vecWen;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_vlWen;
    rand bit [2:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_nf;
    rand bit [1:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_veew;
    rand bit [7:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vl;
    rand bit [2:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vlmul;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vm;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vma;
    rand bit [127:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vmask;
    rand bit [1:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vsew;
    rand bit [7:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vstart;
    rand bit io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vta;
    rand bit [6:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vuopIdx;
    rand bit [2:0] io_mem_to_ooo_writebackVldu_0_bits_vdIdx;
    rand bit [2:0] io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField;
    rand bit io_mem_to_ooo_writebackVldu_0_valid;
    rand bit [127:0] io_mem_to_ooo_writebackVldu_1_bits_data;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_13;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_15;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_19;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_21;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_23;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_4;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_5;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_6;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_7;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_flushPipe;
    rand bit [8:0] io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType;
    rand bit [7:0] io_mem_to_ooo_writebackVldu_1_bits_uop_pdest;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_replayInst;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_flag;
    rand bit [7:0] io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_value;
    rand bit [3:0] io_mem_to_ooo_writebackVldu_1_bits_uop_trigger;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_v0Wen;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_vecWen;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_vlWen;
    rand bit [2:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_nf;
    rand bit [1:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_veew;
    rand bit [7:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vl;
    rand bit [2:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vlmul;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vm;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vma;
    rand bit [127:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vmask;
    rand bit [1:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vsew;
    rand bit [7:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vstart;
    rand bit io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vta;
    rand bit [6:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vuopIdx;
    rand bit [2:0] io_mem_to_ooo_writebackVldu_1_bits_vdIdx;
    rand bit [2:0] io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField;
    rand bit io_mem_to_ooo_writebackVldu_1_valid;

    extern function new(string name="io_mem_to_ooo_vec_wb_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(io_mem_to_ooo_vec_wb_agent_agent_xaction)

        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_data, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_debug_isMMIO, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_debug_isNCIO, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_debug_isPerfCnt, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_13, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_15, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_19, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_21, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_23, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_4, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_5, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_6, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_7, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_flushPipe, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_pdest, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_replayInst, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_trigger, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_v0Wen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_vecWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_vlWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_nf, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_veew, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vl, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vlmul, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vm, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vma, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vmask, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vsew, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vstart, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vta, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vuopIdx, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_vdIdx, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_data, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_13, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_15, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_19, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_21, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_23, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_4, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_5, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_6, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_7, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_flushPipe, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_pdest, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_replayInst, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_trigger, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_v0Wen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_vecWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_vlWen, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_nf, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_veew, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vl, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vlmul, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vm, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vma, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vmask, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vsew, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vstart, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vta, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vuopIdx, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_vdIdx, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_writebackVldu_1_valid, UVM_ALL_ON);
    `uvm_object_utils_end

endclass:io_mem_to_ooo_vec_wb_agent_agent_xaction















































































































































function io_mem_to_ooo_vec_wb_agent_agent_xaction::new(string name = "io_mem_to_ooo_vec_wb_agent_agent_xaction");
    super.new();
endfunction:new

function void io_mem_to_ooo_vec_wb_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void io_mem_to_ooo_vec_wb_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void io_mem_to_ooo_vec_wb_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void io_mem_to_ooo_vec_wb_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string io_mem_to_ooo_vec_wb_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end

    return pkt_str;
endfunction:psdisplay

function bit io_mem_to_ooo_vec_wb_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    io_mem_to_ooo_vec_wb_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a io_mem_to_ooo_vec_wb_agent_agent_xaction or its extend"))
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
