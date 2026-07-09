//=========================================================
//File name    : io_mem_to_ooo_int_wb_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_int_wb_agent_agent_monitor
//Discribution : io_mem_to_ooo_int_wb_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_INT_WB_AGENT_AGENT_MONITOR__SV
`define IO_MEM_TO_OOO_INT_WB_AGENT_AGENT_MONITOR__SV

class io_mem_to_ooo_int_wb_agent_agent_monitor  extends tcnt_monitor_base#(virtual io_mem_to_ooo_int_wb_agent_agent_interface,io_mem_to_ooo_int_wb_agent_agent_cfg,io_mem_to_ooo_int_wb_agent_agent_xaction);

    `uvm_component_utils(io_mem_to_ooo_int_wb_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:io_mem_to_ooo_int_wb_agent_agent_monitor

function io_mem_to_ooo_int_wb_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void io_mem_to_ooo_int_wb_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task io_mem_to_ooo_int_wb_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task io_mem_to_ooo_int_wb_agent_agent_monitor::mon_data();

    logic io_mem_to_ooo_intWriteback_6_0_valid;
    logic io_mem_to_ooo_intWriteback_6_0_bits_toRob_valid;
    logic [8:0] io_mem_to_ooo_intWriteback_6_0_bits_toRob_bits_robIdx_value;
    logic io_mem_to_ooo_intWriteback_5_0_valid;
    logic io_mem_to_ooo_intWriteback_5_0_bits_toRob_valid;
    logic [8:0] io_mem_to_ooo_intWriteback_5_0_bits_toRob_bits_robIdx_value;
    logic io_mem_to_ooo_intWriteback_4_0_valid;
    logic io_mem_to_ooo_intWriteback_4_0_bits_toRob_valid;
    logic io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_value;
    logic io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_3;
    logic io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_6;
    logic io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_7;
    logic io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_15;
    logic io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_19;
    logic io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_23;
    logic [3:0] io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_trigger;
    logic io_mem_to_ooo_intWriteback_3_0_valid;
    logic io_mem_to_ooo_intWriteback_3_0_bits_toRob_valid;
    logic io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_value;
    logic io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_3;
    logic io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_6;
    logic io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_7;
    logic io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_15;
    logic io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_19;
    logic io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_23;
    logic [3:0] io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_trigger;
    logic io_mem_to_ooo_intWriteback_2_0_valid;
    logic io_mem_to_ooo_intWriteback_2_0_bits_toRob_valid;
    logic io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_value;
    logic io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_3;
    logic io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_4;
    logic io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_5;
    logic [3:0] io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_trigger;
    logic [7:0] io_mem_to_ooo_intWriteback_2_0_bits_pdest;
    logic io_mem_to_ooo_intWriteback_2_0_bits_toIntRf_valid;
    logic [63:0] io_mem_to_ooo_intWriteback_2_0_bits_toIntRf_bits;
    logic io_mem_to_ooo_intWriteback_2_0_bits_toFpRf_valid;
    logic [63:0] io_mem_to_ooo_intWriteback_2_0_bits_toFpRf_bits;
    logic io_mem_to_ooo_intWriteback_1_0_valid;
    logic io_mem_to_ooo_intWriteback_1_0_bits_toRob_valid;
    logic io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_value;
    logic io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_3;
    logic io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_4;
    logic io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_5;
    logic [3:0] io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_trigger;
    logic [7:0] io_mem_to_ooo_intWriteback_1_0_bits_pdest;
    logic io_mem_to_ooo_intWriteback_1_0_bits_toIntRf_valid;
    logic [63:0] io_mem_to_ooo_intWriteback_1_0_bits_toIntRf_bits;
    logic io_mem_to_ooo_intWriteback_1_0_bits_toFpRf_valid;
    logic [63:0] io_mem_to_ooo_intWriteback_1_0_bits_toFpRf_bits;
    logic io_mem_to_ooo_intWriteback_0_0_valid;
    logic io_mem_to_ooo_intWriteback_0_0_bits_toRob_valid;
    logic io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_value;
    logic io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_3;
    logic io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_4;
    logic io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_5;
    logic io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_6;
    logic io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_7;
    logic [3:0] io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_trigger;
    logic [7:0] io_mem_to_ooo_intWriteback_0_0_bits_pdest;
    logic io_mem_to_ooo_intWriteback_0_0_bits_toIntRf_valid;
    logic [63:0] io_mem_to_ooo_intWriteback_0_0_bits_toIntRf_bits;
    logic io_mem_to_ooo_intWriteback_0_0_bits_toFpRf_valid;
    logic [63:0] io_mem_to_ooo_intWriteback_0_0_bits_toFpRf_bits;
    logic io_mem_to_ooo_intWriteback_0_0_bits_isFromLoadUnit;

    logic io_mem_to_ooo_writebackLda_0_bits_debug_isMMIO;
    logic io_mem_to_ooo_writebackLda_0_bits_debug_isNCIO;
    logic io_mem_to_ooo_writebackLda_0_bits_debug_isPerfCnt;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_flushPipe;
    logic io_mem_to_ooo_writebackLda_0_bits_uop_replayInst;
    logic io_mem_to_ooo_writebackLda_1_bits_debug_isMMIO;
    logic io_mem_to_ooo_writebackLda_1_bits_debug_isNCIO;
    logic io_mem_to_ooo_writebackLda_1_bits_debug_isPerfCnt;
    logic io_mem_to_ooo_writebackLda_1_bits_uop_flushPipe;
    logic io_mem_to_ooo_writebackLda_1_bits_uop_replayInst;
    logic io_mem_to_ooo_writebackLda_2_bits_debug_isMMIO;
    logic io_mem_to_ooo_writebackLda_2_bits_debug_isNCIO;
    logic io_mem_to_ooo_writebackLda_2_bits_debug_isPerfCnt;
    logic io_mem_to_ooo_writebackLda_2_bits_uop_flushPipe;
    logic io_mem_to_ooo_writebackLda_2_bits_uop_replayInst;
    logic io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO;
    logic io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_1;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_2;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_4;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_5;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_6;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_7;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_8;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_9;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_10;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_11;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_12;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_13;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_14;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_15;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_16;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_17;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_18;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_19;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_20;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_21;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_22;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_23;
    logic io_mem_to_ooo_writebackSta_0_bits_uop_flushPipe;
    logic io_mem_to_ooo_writebackSta_1_bits_debug_isMMIO;
    logic io_mem_to_ooo_writebackSta_1_bits_debug_isNCIO;
    io_mem_to_ooo_int_wb_agent_agent_xaction  mon_tr;
    memblock_sync_pkg::dispatch_raw_int_wb_t raw_int_wb;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        io_mem_to_ooo_intWriteback_6_0_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_6_0_valid;
        io_mem_to_ooo_intWriteback_6_0_bits_toRob_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_6_0_bits_toRob_valid;
        io_mem_to_ooo_intWriteback_6_0_bits_toRob_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_6_0_bits_toRob_bits_robIdx_value;
        io_mem_to_ooo_intWriteback_5_0_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_5_0_valid;
        io_mem_to_ooo_intWriteback_5_0_bits_toRob_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_5_0_bits_toRob_valid;
        io_mem_to_ooo_intWriteback_5_0_bits_toRob_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_5_0_bits_toRob_bits_robIdx_value;
        io_mem_to_ooo_intWriteback_4_0_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_4_0_valid;
        io_mem_to_ooo_intWriteback_4_0_bits_toRob_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_4_0_bits_toRob_valid;
        io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_flag;
        io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_value;
        io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_3 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_3;
        io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_6 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_6;
        io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_7 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_7;
        io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_15 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_15;
        io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_19 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_19;
        io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_23 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_23;
        io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_trigger;
        io_mem_to_ooo_intWriteback_3_0_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_3_0_valid;
        io_mem_to_ooo_intWriteback_3_0_bits_toRob_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_3_0_bits_toRob_valid;
        io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_flag;
        io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_value;
        io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_3 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_3;
        io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_6 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_6;
        io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_7 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_7;
        io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_15 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_15;
        io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_19 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_19;
        io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_23 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_23;
        io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_trigger;
        io_mem_to_ooo_intWriteback_2_0_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_2_0_valid;
        io_mem_to_ooo_intWriteback_2_0_bits_toRob_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_2_0_bits_toRob_valid;
        io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_flag;
        io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_value;
        io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_3 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_3;
        io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_4 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_4;
        io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_5 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_5;
        io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_trigger;
        io_mem_to_ooo_intWriteback_2_0_bits_pdest = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_2_0_bits_pdest;
        io_mem_to_ooo_intWriteback_2_0_bits_toIntRf_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_2_0_bits_toIntRf_valid;
        io_mem_to_ooo_intWriteback_2_0_bits_toIntRf_bits = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_2_0_bits_toIntRf_bits;
        io_mem_to_ooo_intWriteback_2_0_bits_toFpRf_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_2_0_bits_toFpRf_valid;
        io_mem_to_ooo_intWriteback_2_0_bits_toFpRf_bits = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_2_0_bits_toFpRf_bits;
        io_mem_to_ooo_intWriteback_1_0_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_1_0_valid;
        io_mem_to_ooo_intWriteback_1_0_bits_toRob_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_1_0_bits_toRob_valid;
        io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_flag;
        io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_value;
        io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_3 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_3;
        io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_4 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_4;
        io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_5 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_5;
        io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_trigger;
        io_mem_to_ooo_intWriteback_1_0_bits_pdest = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_1_0_bits_pdest;
        io_mem_to_ooo_intWriteback_1_0_bits_toIntRf_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_1_0_bits_toIntRf_valid;
        io_mem_to_ooo_intWriteback_1_0_bits_toIntRf_bits = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_1_0_bits_toIntRf_bits;
        io_mem_to_ooo_intWriteback_1_0_bits_toFpRf_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_1_0_bits_toFpRf_valid;
        io_mem_to_ooo_intWriteback_1_0_bits_toFpRf_bits = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_1_0_bits_toFpRf_bits;
        io_mem_to_ooo_intWriteback_0_0_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_valid;
        io_mem_to_ooo_intWriteback_0_0_bits_toRob_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_bits_toRob_valid;
        io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_flag;
        io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_value;
        io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_3 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_3;
        io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_4 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_4;
        io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_5 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_5;
        io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_6 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_6;
        io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_7 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_7;
        io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_trigger = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_trigger;
        io_mem_to_ooo_intWriteback_0_0_bits_pdest = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_bits_pdest;
        io_mem_to_ooo_intWriteback_0_0_bits_toIntRf_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_bits_toIntRf_valid;
        io_mem_to_ooo_intWriteback_0_0_bits_toIntRf_bits = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_bits_toIntRf_bits;
        io_mem_to_ooo_intWriteback_0_0_bits_toFpRf_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_bits_toFpRf_valid;
        io_mem_to_ooo_intWriteback_0_0_bits_toFpRf_bits = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_bits_toFpRf_bits;
        io_mem_to_ooo_intWriteback_0_0_bits_isFromLoadUnit = this.vif.mon_mp.mon_cb.io_mem_to_ooo_intWriteback_0_0_bits_isFromLoadUnit;

        io_mem_to_ooo_writebackLda_0_bits_debug_isMMIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_debug_isMMIO;
        io_mem_to_ooo_writebackLda_0_bits_debug_isNCIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_debug_isNCIO;
        io_mem_to_ooo_writebackLda_0_bits_debug_isPerfCnt = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_debug_isPerfCnt;
        io_mem_to_ooo_writebackLda_0_bits_uop_flushPipe = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_flushPipe;
        io_mem_to_ooo_writebackLda_0_bits_uop_replayInst = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_0_bits_uop_replayInst;
        io_mem_to_ooo_writebackLda_1_bits_debug_isMMIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_debug_isMMIO;
        io_mem_to_ooo_writebackLda_1_bits_debug_isNCIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_debug_isNCIO;
        io_mem_to_ooo_writebackLda_1_bits_debug_isPerfCnt = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_debug_isPerfCnt;
        io_mem_to_ooo_writebackLda_1_bits_uop_flushPipe = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_flushPipe;
        io_mem_to_ooo_writebackLda_1_bits_uop_replayInst = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_1_bits_uop_replayInst;
        io_mem_to_ooo_writebackLda_2_bits_debug_isMMIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_debug_isMMIO;
        io_mem_to_ooo_writebackLda_2_bits_debug_isNCIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_debug_isNCIO;
        io_mem_to_ooo_writebackLda_2_bits_debug_isPerfCnt = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_debug_isPerfCnt;
        io_mem_to_ooo_writebackLda_2_bits_uop_flushPipe = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_flushPipe;
        io_mem_to_ooo_writebackLda_2_bits_uop_replayInst = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackLda_2_bits_uop_replayInst;
        io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO;
        io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_0;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_1 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_1;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_2 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_2;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_4 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_4;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_5 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_5;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_6 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_6;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_7 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_7;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_8 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_8;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_9 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_9;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_10 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_10;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_11 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_11;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_12 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_12;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_13 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_13;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_14 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_14;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_15 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_15;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_16 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_16;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_17 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_17;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_18 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_18;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_19 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_19;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_20 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_20;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_21 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_21;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_22 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_22;
        io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_23 = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_23;
        io_mem_to_ooo_writebackSta_0_bits_uop_flushPipe = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_0_bits_uop_flushPipe;
        io_mem_to_ooo_writebackSta_1_bits_debug_isMMIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_debug_isMMIO;
        io_mem_to_ooo_writebackSta_1_bits_debug_isNCIO = this.vif.mon_mp.mon_cb.io_mem_to_ooo_writebackSta_1_bits_debug_isNCIO;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_6_0_valid,io_mem_to_ooo_intWriteback_6_0_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_6_0_bits_toRob_valid,io_mem_to_ooo_intWriteback_6_0_bits_toRob_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_6_0_bits_toRob_bits_robIdx_value,io_mem_to_ooo_intWriteback_6_0_bits_toRob_bits_robIdx_value,9);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_5_0_valid,io_mem_to_ooo_intWriteback_5_0_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_5_0_bits_toRob_valid,io_mem_to_ooo_intWriteback_5_0_bits_toRob_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_5_0_bits_toRob_bits_robIdx_value,io_mem_to_ooo_intWriteback_5_0_bits_toRob_bits_robIdx_value,9);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_4_0_valid,io_mem_to_ooo_intWriteback_4_0_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_4_0_bits_toRob_valid,io_mem_to_ooo_intWriteback_4_0_bits_toRob_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_flag,io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_value,io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_value,9);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_3,io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_3,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_6,io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_6,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_7,io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_7,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_15,io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_15,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_19,io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_19,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_23,io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_23,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_trigger,io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_trigger,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_3_0_valid,io_mem_to_ooo_intWriteback_3_0_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_3_0_bits_toRob_valid,io_mem_to_ooo_intWriteback_3_0_bits_toRob_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_flag,io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_value,io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_value,9);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_3,io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_3,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_6,io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_6,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_7,io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_7,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_15,io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_15,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_19,io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_19,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_23,io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_23,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_trigger,io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_trigger,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_2_0_valid,io_mem_to_ooo_intWriteback_2_0_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_2_0_bits_toRob_valid,io_mem_to_ooo_intWriteback_2_0_bits_toRob_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_flag,io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_value,io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_value,9);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_3,io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_3,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_4,io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_4,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_5,io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_5,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_trigger,io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_trigger,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_2_0_bits_pdest,io_mem_to_ooo_intWriteback_2_0_bits_pdest,8);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_2_0_bits_toIntRf_valid,io_mem_to_ooo_intWriteback_2_0_bits_toIntRf_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_2_0_bits_toIntRf_bits,io_mem_to_ooo_intWriteback_2_0_bits_toIntRf_bits,64);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_2_0_bits_toFpRf_valid,io_mem_to_ooo_intWriteback_2_0_bits_toFpRf_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_2_0_bits_toFpRf_bits,io_mem_to_ooo_intWriteback_2_0_bits_toFpRf_bits,64);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_1_0_valid,io_mem_to_ooo_intWriteback_1_0_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_1_0_bits_toRob_valid,io_mem_to_ooo_intWriteback_1_0_bits_toRob_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_flag,io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_value,io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_value,9);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_3,io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_3,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_4,io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_4,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_5,io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_5,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_trigger,io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_trigger,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_1_0_bits_pdest,io_mem_to_ooo_intWriteback_1_0_bits_pdest,8);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_1_0_bits_toIntRf_valid,io_mem_to_ooo_intWriteback_1_0_bits_toIntRf_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_1_0_bits_toIntRf_bits,io_mem_to_ooo_intWriteback_1_0_bits_toIntRf_bits,64);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_1_0_bits_toFpRf_valid,io_mem_to_ooo_intWriteback_1_0_bits_toFpRf_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_1_0_bits_toFpRf_bits,io_mem_to_ooo_intWriteback_1_0_bits_toFpRf_bits,64);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_valid,io_mem_to_ooo_intWriteback_0_0_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_bits_toRob_valid,io_mem_to_ooo_intWriteback_0_0_bits_toRob_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_flag,io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_value,io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_value,9);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_3,io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_3,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_4,io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_4,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_5,io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_5,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_6,io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_6,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_7,io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_7,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_trigger,io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_trigger,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_bits_pdest,io_mem_to_ooo_intWriteback_0_0_bits_pdest,8);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_bits_toIntRf_valid,io_mem_to_ooo_intWriteback_0_0_bits_toIntRf_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_bits_toIntRf_bits,io_mem_to_ooo_intWriteback_0_0_bits_toIntRf_bits,64);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_bits_toFpRf_valid,io_mem_to_ooo_intWriteback_0_0_bits_toFpRf_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_bits_toFpRf_bits,io_mem_to_ooo_intWriteback_0_0_bits_toFpRf_bits,64);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_intWriteback_0_0_bits_isFromLoadUnit,io_mem_to_ooo_intWriteback_0_0_bits_isFromLoadUnit,1);

        end
        if(this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            if (io_mem_to_ooo_intWriteback_0_0_valid &&
                io_mem_to_ooo_intWriteback_0_0_bits_toRob_valid) begin
                raw_int_wb = memblock_sync_pkg::make_empty_raw_int_wb();
                raw_int_wb.valid = 1'b1;
                raw_int_wb.port_id = 0;
                raw_int_wb.rob_valid = 1'b1;
                raw_int_wb.rob_flag = io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_flag;
                raw_int_wb.rob_value = io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_value;
                raw_int_wb.lq_valid = 1'b1;
                raw_int_wb.exception_vec[3] = io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_3;
                raw_int_wb.exception_vec[4] = io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_4;
                raw_int_wb.exception_vec[5] = io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_5;
                raw_int_wb.exception_vec[6] = io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_6;
                raw_int_wb.exception_vec[7] = io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_7;
                raw_int_wb.cycle = $time;
                memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
            end
            if (io_mem_to_ooo_intWriteback_1_0_valid &&
                io_mem_to_ooo_intWriteback_1_0_bits_toRob_valid) begin
                raw_int_wb = memblock_sync_pkg::make_empty_raw_int_wb();
                raw_int_wb.valid = 1'b1;
                raw_int_wb.port_id = 1;
                raw_int_wb.rob_valid = 1'b1;
                raw_int_wb.rob_flag = io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_flag;
                raw_int_wb.rob_value = io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_value;
                raw_int_wb.lq_valid = 1'b1;
                raw_int_wb.exception_vec[3] = io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_3;
                raw_int_wb.exception_vec[4] = io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_4;
                raw_int_wb.exception_vec[5] = io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_5;
                raw_int_wb.cycle = $time;
                memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
            end
            if (io_mem_to_ooo_intWriteback_2_0_valid &&
                io_mem_to_ooo_intWriteback_2_0_bits_toRob_valid) begin
                raw_int_wb = memblock_sync_pkg::make_empty_raw_int_wb();
                raw_int_wb.valid = 1'b1;
                raw_int_wb.port_id = 2;
                raw_int_wb.rob_valid = 1'b1;
                raw_int_wb.rob_flag = io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_flag;
                raw_int_wb.rob_value = io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_value;
                raw_int_wb.lq_valid = 1'b1;
                raw_int_wb.exception_vec[3] = io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_3;
                raw_int_wb.exception_vec[4] = io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_4;
                raw_int_wb.exception_vec[5] = io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_5;
                raw_int_wb.cycle = $time;
                memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
            end
            if (io_mem_to_ooo_intWriteback_3_0_valid &&
                io_mem_to_ooo_intWriteback_3_0_bits_toRob_valid) begin
                raw_int_wb = memblock_sync_pkg::make_empty_raw_int_wb();
                raw_int_wb.valid = 1'b1;
                raw_int_wb.port_id = 3;
                raw_int_wb.rob_valid = 1'b1;
                raw_int_wb.rob_flag = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_flag;
                raw_int_wb.rob_value = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_value;
                raw_int_wb.sq_valid = 1'b1;
                raw_int_wb.exception_vec[3] = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_3;
                raw_int_wb.exception_vec[6] = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_6;
                raw_int_wb.exception_vec[7] = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_7;
                raw_int_wb.exception_vec[15] = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_15;
                raw_int_wb.exception_vec[19] = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_19;
                raw_int_wb.exception_vec[23] = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_23;
                raw_int_wb.cycle = $time;
                memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
            end
            if (io_mem_to_ooo_intWriteback_4_0_valid &&
                io_mem_to_ooo_intWriteback_4_0_bits_toRob_valid) begin
                raw_int_wb = memblock_sync_pkg::make_empty_raw_int_wb();
                raw_int_wb.valid = 1'b1;
                raw_int_wb.port_id = 4;
                raw_int_wb.rob_valid = 1'b1;
                raw_int_wb.rob_flag = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_flag;
                raw_int_wb.rob_value = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_value;
                raw_int_wb.sq_valid = 1'b1;
                raw_int_wb.exception_vec[3] = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_3;
                raw_int_wb.exception_vec[6] = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_6;
                raw_int_wb.exception_vec[7] = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_7;
                raw_int_wb.exception_vec[15] = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_15;
                raw_int_wb.exception_vec[19] = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_19;
                raw_int_wb.exception_vec[23] = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_23;
                raw_int_wb.cycle = $time;
                memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
            end
            if (io_mem_to_ooo_intWriteback_5_0_valid &&
                io_mem_to_ooo_intWriteback_5_0_bits_toRob_valid) begin
                raw_int_wb = memblock_sync_pkg::make_empty_raw_int_wb();
                raw_int_wb.valid = 1'b1;
                raw_int_wb.port_id = 5;
                raw_int_wb.rob_valid = 1'b1;
                raw_int_wb.rob_value = io_mem_to_ooo_intWriteback_5_0_bits_toRob_bits_robIdx_value;
                raw_int_wb.cycle = $time;
                memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
            end
            if (io_mem_to_ooo_intWriteback_6_0_valid &&
                io_mem_to_ooo_intWriteback_6_0_bits_toRob_valid) begin
                raw_int_wb = memblock_sync_pkg::make_empty_raw_int_wb();
                raw_int_wb.valid = 1'b1;
                raw_int_wb.port_id = 6;
                raw_int_wb.rob_valid = 1'b1;
                raw_int_wb.rob_value = io_mem_to_ooo_intWriteback_6_0_bits_toRob_bits_robIdx_value;
                raw_int_wb.cycle = $time;
                memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
            end
        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = io_mem_to_ooo_int_wb_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.io_mem_to_ooo_intWriteback_6_0_valid = io_mem_to_ooo_intWriteback_6_0_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_6_0_bits_toRob_valid = io_mem_to_ooo_intWriteback_6_0_bits_toRob_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_6_0_bits_toRob_bits_robIdx_value = io_mem_to_ooo_intWriteback_6_0_bits_toRob_bits_robIdx_value;
        //    mon_tr.io_mem_to_ooo_intWriteback_5_0_valid = io_mem_to_ooo_intWriteback_5_0_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_5_0_bits_toRob_valid = io_mem_to_ooo_intWriteback_5_0_bits_toRob_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_5_0_bits_toRob_bits_robIdx_value = io_mem_to_ooo_intWriteback_5_0_bits_toRob_bits_robIdx_value;
        //    mon_tr.io_mem_to_ooo_intWriteback_4_0_valid = io_mem_to_ooo_intWriteback_4_0_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_4_0_bits_toRob_valid = io_mem_to_ooo_intWriteback_4_0_bits_toRob_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_flag = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_flag;
        //    mon_tr.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_value = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_robIdx_value;
        //    mon_tr.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_3 = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_3;
        //    mon_tr.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_6 = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_6;
        //    mon_tr.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_7 = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_7;
        //    mon_tr.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_15 = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_15;
        //    mon_tr.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_19 = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_19;
        //    mon_tr.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_23 = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_exceptionVec_23;
        //    mon_tr.io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_trigger = io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_trigger;
        //    mon_tr.io_mem_to_ooo_intWriteback_3_0_valid = io_mem_to_ooo_intWriteback_3_0_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_3_0_bits_toRob_valid = io_mem_to_ooo_intWriteback_3_0_bits_toRob_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_flag = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_flag;
        //    mon_tr.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_value = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_robIdx_value;
        //    mon_tr.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_3 = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_3;
        //    mon_tr.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_6 = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_6;
        //    mon_tr.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_7 = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_7;
        //    mon_tr.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_15 = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_15;
        //    mon_tr.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_19 = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_19;
        //    mon_tr.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_23 = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_exceptionVec_23;
        //    mon_tr.io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_trigger = io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_trigger;
        //    mon_tr.io_mem_to_ooo_intWriteback_2_0_valid = io_mem_to_ooo_intWriteback_2_0_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_2_0_bits_toRob_valid = io_mem_to_ooo_intWriteback_2_0_bits_toRob_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_flag = io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_flag;
        //    mon_tr.io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_value = io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_robIdx_value;
        //    mon_tr.io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_3 = io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_3;
        //    mon_tr.io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_4 = io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_4;
        //    mon_tr.io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_5 = io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_exceptionVec_5;
        //    mon_tr.io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_trigger = io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_trigger;
        //    mon_tr.io_mem_to_ooo_intWriteback_2_0_bits_pdest = io_mem_to_ooo_intWriteback_2_0_bits_pdest;
        //    mon_tr.io_mem_to_ooo_intWriteback_2_0_bits_toIntRf_valid = io_mem_to_ooo_intWriteback_2_0_bits_toIntRf_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_2_0_bits_toIntRf_bits = io_mem_to_ooo_intWriteback_2_0_bits_toIntRf_bits;
        //    mon_tr.io_mem_to_ooo_intWriteback_2_0_bits_toFpRf_valid = io_mem_to_ooo_intWriteback_2_0_bits_toFpRf_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_2_0_bits_toFpRf_bits = io_mem_to_ooo_intWriteback_2_0_bits_toFpRf_bits;
        //    mon_tr.io_mem_to_ooo_intWriteback_1_0_valid = io_mem_to_ooo_intWriteback_1_0_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_1_0_bits_toRob_valid = io_mem_to_ooo_intWriteback_1_0_bits_toRob_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_flag = io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_flag;
        //    mon_tr.io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_value = io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_robIdx_value;
        //    mon_tr.io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_3 = io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_3;
        //    mon_tr.io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_4 = io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_4;
        //    mon_tr.io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_5 = io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_exceptionVec_5;
        //    mon_tr.io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_trigger = io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_trigger;
        //    mon_tr.io_mem_to_ooo_intWriteback_1_0_bits_pdest = io_mem_to_ooo_intWriteback_1_0_bits_pdest;
        //    mon_tr.io_mem_to_ooo_intWriteback_1_0_bits_toIntRf_valid = io_mem_to_ooo_intWriteback_1_0_bits_toIntRf_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_1_0_bits_toIntRf_bits = io_mem_to_ooo_intWriteback_1_0_bits_toIntRf_bits;
        //    mon_tr.io_mem_to_ooo_intWriteback_1_0_bits_toFpRf_valid = io_mem_to_ooo_intWriteback_1_0_bits_toFpRf_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_1_0_bits_toFpRf_bits = io_mem_to_ooo_intWriteback_1_0_bits_toFpRf_bits;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_valid = io_mem_to_ooo_intWriteback_0_0_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_bits_toRob_valid = io_mem_to_ooo_intWriteback_0_0_bits_toRob_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_flag = io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_flag;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_value = io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_robIdx_value;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_3 = io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_3;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_4 = io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_4;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_5 = io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_5;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_6 = io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_6;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_7 = io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_exceptionVec_7;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_trigger = io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_trigger;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_bits_pdest = io_mem_to_ooo_intWriteback_0_0_bits_pdest;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_bits_toIntRf_valid = io_mem_to_ooo_intWriteback_0_0_bits_toIntRf_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_bits_toIntRf_bits = io_mem_to_ooo_intWriteback_0_0_bits_toIntRf_bits;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_bits_toFpRf_valid = io_mem_to_ooo_intWriteback_0_0_bits_toFpRf_valid;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_bits_toFpRf_bits = io_mem_to_ooo_intWriteback_0_0_bits_toFpRf_bits;
        //    mon_tr.io_mem_to_ooo_intWriteback_0_0_bits_isFromLoadUnit = io_mem_to_ooo_intWriteback_0_0_bits_isFromLoadUnit;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
