//=========================================================
//File name    : io_mem_to_ooo_vec_wb_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_vec_wb_agent_agent_monitor
//Discribution : io_mem_to_ooo_vec_wb_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_VEC_WB_AGENT_AGENT_MONITOR__SV
`define IO_MEM_TO_OOO_VEC_WB_AGENT_AGENT_MONITOR__SV

class io_mem_to_ooo_vec_wb_agent_agent_monitor  extends tcnt_monitor_base#(virtual io_mem_to_ooo_vec_wb_agent_agent_interface,io_mem_to_ooo_vec_wb_agent_agent_cfg,io_mem_to_ooo_vec_wb_agent_agent_xaction);

    `uvm_component_utils(io_mem_to_ooo_vec_wb_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:io_mem_to_ooo_vec_wb_agent_agent_monitor

function io_mem_to_ooo_vec_wb_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void io_mem_to_ooo_vec_wb_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task io_mem_to_ooo_vec_wb_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task io_mem_to_ooo_vec_wb_agent_agent_monitor::mon_data();


    logic [127:0] io_mem_to_ooo_writebackVldu_0_bits_data;
    logic io_mem_to_ooo_writebackVldu_0_bits_debug_isMMIO;
    logic io_mem_to_ooo_writebackVldu_0_bits_debug_isNCIO;
    logic io_mem_to_ooo_writebackVldu_0_bits_debug_isPerfCnt;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_13;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_15;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_19;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_21;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_23;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_4;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_5;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_6;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_7;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_flushPipe;
    logic [8:0] io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType;
    logic [7:0] io_mem_to_ooo_writebackVldu_0_bits_uop_pdest;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_replayInst;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_flag;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_value;
    logic [3:0] io_mem_to_ooo_writebackVldu_0_bits_uop_trigger;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_v0Wen;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_vecWen;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_vlWen;
    logic [2:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_nf;
    logic [1:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_veew;
    logic [7:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vl;
    logic [2:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vlmul;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vm;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vma;
    logic [127:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vmask;
    logic [1:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vsew;
    logic [7:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vstart;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vta;
    logic [6:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vuopIdx;
    logic [2:0] io_mem_to_ooo_writebackVldu_0_bits_vdIdx;
    logic [2:0] io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField;
    logic io_mem_to_ooo_writebackVldu_0_valid;
    logic [127:0] io_mem_to_ooo_writebackVldu_1_bits_data;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_13;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_15;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_19;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_21;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_23;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_4;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_5;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_6;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_7;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_flushPipe;
    logic [8:0] io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType;
    logic [7:0] io_mem_to_ooo_writebackVldu_1_bits_uop_pdest;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_replayInst;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_flag;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_value;
    logic [3:0] io_mem_to_ooo_writebackVldu_1_bits_uop_trigger;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_v0Wen;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_vecWen;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_vlWen;
    logic [2:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_nf;
    logic [1:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_veew;
    logic [7:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vl;
    logic [2:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vlmul;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vm;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vma;
    logic [127:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vmask;
    logic [1:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vsew;
    logic [7:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vstart;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vta;
    logic [6:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vuopIdx;
    logic [2:0] io_mem_to_ooo_writebackVldu_1_bits_vdIdx;
    logic [2:0] io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField;
    logic io_mem_to_ooo_writebackVldu_1_valid;
    io_mem_to_ooo_vec_wb_agent_agent_xaction  mon_tr;
    while(1) begin
        @this.vif.mon_mp.mon_cb;

        io_mem_to_ooo_writebackVldu_0_bits_data = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_data;
        io_mem_to_ooo_writebackVldu_0_bits_debug_isMMIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_debug_isMMIO;
        io_mem_to_ooo_writebackVldu_0_bits_debug_isNCIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_debug_isNCIO;
        io_mem_to_ooo_writebackVldu_0_bits_debug_isPerfCnt = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_debug_isPerfCnt;
        io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_13 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_13;
        io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_15 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_15;
        io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_19 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_19;
        io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_21 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_21;
        io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_23 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_23;
        io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3;
        io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_4 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_4;
        io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_5 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_5;
        io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_6 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_6;
        io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_7 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_7;
        io_mem_to_ooo_writebackVldu_0_bits_uop_flushPipe = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_flushPipe;
        io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType;
        io_mem_to_ooo_writebackVldu_0_bits_uop_pdest = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_pdest;
        io_mem_to_ooo_writebackVldu_0_bits_uop_replayInst = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_replayInst;
        io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_flag;
        io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_value;
        io_mem_to_ooo_writebackVldu_0_bits_uop_trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_trigger;
        io_mem_to_ooo_writebackVldu_0_bits_uop_v0Wen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_v0Wen;
        io_mem_to_ooo_writebackVldu_0_bits_uop_vecWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_vecWen;
        io_mem_to_ooo_writebackVldu_0_bits_uop_vlWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_vlWen;
        io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_nf = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_nf;
        io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_veew = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_veew;
        io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vl = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vl;
        io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vlmul = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vlmul;
        io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vm = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vm;
        io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vma = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vma;
        io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vmask = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vmask;
        io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vsew = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vsew;
        io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vstart = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vstart;
        io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vta = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vta;
        io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vuopIdx = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vuopIdx;
        io_mem_to_ooo_writebackVldu_0_bits_vdIdx = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_vdIdx;
        io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField;
        io_mem_to_ooo_writebackVldu_0_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_0_valid;
        io_mem_to_ooo_writebackVldu_1_bits_data = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_data;
        io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_13 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_13;
        io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_15 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_15;
        io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_19 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_19;
        io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_21 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_21;
        io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_23 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_23;
        io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3;
        io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_4 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_4;
        io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_5 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_5;
        io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_6 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_6;
        io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_7 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_7;
        io_mem_to_ooo_writebackVldu_1_bits_uop_flushPipe = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_flushPipe;
        io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType;
        io_mem_to_ooo_writebackVldu_1_bits_uop_pdest = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_pdest;
        io_mem_to_ooo_writebackVldu_1_bits_uop_replayInst = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_replayInst;
        io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_flag;
        io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_value;
        io_mem_to_ooo_writebackVldu_1_bits_uop_trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_trigger;
        io_mem_to_ooo_writebackVldu_1_bits_uop_v0Wen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_v0Wen;
        io_mem_to_ooo_writebackVldu_1_bits_uop_vecWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_vecWen;
        io_mem_to_ooo_writebackVldu_1_bits_uop_vlWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_vlWen;
        io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_nf = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_nf;
        io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_veew = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_veew;
        io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vl = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vl;
        io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vlmul = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vlmul;
        io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vm = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vm;
        io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vma = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vma;
        io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vmask = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vmask;
        io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vsew = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vsew;
        io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vstart = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vstart;
        io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vta = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vta;
        io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vuopIdx = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vuopIdx;
        io_mem_to_ooo_writebackVldu_1_bits_vdIdx = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_vdIdx;
        io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField;
        io_mem_to_ooo_writebackVldu_1_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackVldu_1_valid;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin

        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = io_mem_to_ooo_vec_wb_agent_agent_xaction::type_id::create("mon_tr");

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
