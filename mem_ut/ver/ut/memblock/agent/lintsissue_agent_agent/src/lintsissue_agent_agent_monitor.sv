//=========================================================
//File name    : lintsissue_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : lintsissue_agent_agent_monitor
//Discribution : lintsissue_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef LINTSISSUE_AGENT_AGENT_MONITOR__SV
`define LINTSISSUE_AGENT_AGENT_MONITOR__SV

class lintsissue_agent_agent_monitor  extends tcnt_monitor_base#(virtual lintsissue_agent_agent_interface,lintsissue_agent_agent_cfg,lintsissue_agent_agent_xaction);

    `uvm_component_utils(lintsissue_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:lintsissue_agent_agent_monitor

function lintsissue_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void lintsissue_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task lintsissue_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task lintsissue_agent_agent_monitor::mon_data();


    logic [63:0] io_ooo_to_mem_issueLda_0_bits_src_0;
    logic io_ooo_to_mem_issueLda_0_bits_uop_fpWen;
    logic [3:0] io_ooo_to_mem_issueLda_0_bits_uop_ftqOffset;
    logic io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_flag;
    logic [5:0] io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_value;
    logic [8:0] io_ooo_to_mem_issueLda_0_bits_uop_fuOpType;
    logic [31:0] io_ooo_to_mem_issueLda_0_bits_uop_imm;
    logic io_ooo_to_mem_issueLda_0_bits_uop_loadWaitBit;
    logic io_ooo_to_mem_issueLda_0_bits_uop_loadWaitStrict;
    logic io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_flag;
    logic [6:0] io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_value;
    logic [49:0] io_ooo_to_mem_issueLda_0_bits_uop_pc;
    logic [7:0] io_ooo_to_mem_issueLda_0_bits_uop_pdest;
    logic io_ooo_to_mem_issueLda_0_bits_uop_preDecodeInfo_isRVC;
    logic io_ooo_to_mem_issueLda_0_bits_uop_rfWen;
    logic io_ooo_to_mem_issueLda_0_bits_uop_robIdx_flag;
    logic [7:0] io_ooo_to_mem_issueLda_0_bits_uop_robIdx_value;
    logic io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_flag;
    logic [5:0] io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_value;
    logic io_ooo_to_mem_issueLda_0_bits_uop_storeSetHit;
    logic io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_flag;
    logic [7:0] io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_value;
    logic io_ooo_to_mem_issueLda_0_ready;
    logic io_ooo_to_mem_issueLda_0_valid;
    logic [63:0] io_ooo_to_mem_issueLda_1_bits_src_0;
    logic io_ooo_to_mem_issueLda_1_bits_uop_fpWen;
    logic [3:0] io_ooo_to_mem_issueLda_1_bits_uop_ftqOffset;
    logic io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_flag;
    logic [5:0] io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_value;
    logic [8:0] io_ooo_to_mem_issueLda_1_bits_uop_fuOpType;
    logic [31:0] io_ooo_to_mem_issueLda_1_bits_uop_imm;
    logic io_ooo_to_mem_issueLda_1_bits_uop_loadWaitBit;
    logic io_ooo_to_mem_issueLda_1_bits_uop_loadWaitStrict;
    logic io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_flag;
    logic [6:0] io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_value;
    logic [49:0] io_ooo_to_mem_issueLda_1_bits_uop_pc;
    logic [7:0] io_ooo_to_mem_issueLda_1_bits_uop_pdest;
    logic io_ooo_to_mem_issueLda_1_bits_uop_preDecodeInfo_isRVC;
    logic io_ooo_to_mem_issueLda_1_bits_uop_rfWen;
    logic io_ooo_to_mem_issueLda_1_bits_uop_robIdx_flag;
    logic [7:0] io_ooo_to_mem_issueLda_1_bits_uop_robIdx_value;
    logic io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_flag;
    logic [5:0] io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_value;
    logic io_ooo_to_mem_issueLda_1_bits_uop_storeSetHit;
    logic io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_flag;
    logic [7:0] io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_value;
    logic io_ooo_to_mem_issueLda_1_ready;
    logic io_ooo_to_mem_issueLda_1_valid;
    logic [63:0] io_ooo_to_mem_issueLda_2_bits_src_0;
    logic io_ooo_to_mem_issueLda_2_bits_uop_fpWen;
    logic [3:0] io_ooo_to_mem_issueLda_2_bits_uop_ftqOffset;
    logic io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_flag;
    logic [5:0] io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_value;
    logic [8:0] io_ooo_to_mem_issueLda_2_bits_uop_fuOpType;
    logic [31:0] io_ooo_to_mem_issueLda_2_bits_uop_imm;
    logic io_ooo_to_mem_issueLda_2_bits_uop_loadWaitBit;
    logic io_ooo_to_mem_issueLda_2_bits_uop_loadWaitStrict;
    logic io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_flag;
    logic [6:0] io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_value;
    logic [49:0] io_ooo_to_mem_issueLda_2_bits_uop_pc;
    logic [7:0] io_ooo_to_mem_issueLda_2_bits_uop_pdest;
    logic io_ooo_to_mem_issueLda_2_bits_uop_preDecodeInfo_isRVC;
    logic io_ooo_to_mem_issueLda_2_bits_uop_rfWen;
    logic io_ooo_to_mem_issueLda_2_bits_uop_robIdx_flag;
    logic [7:0] io_ooo_to_mem_issueLda_2_bits_uop_robIdx_value;
    logic io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_flag;
    logic [5:0] io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_value;
    logic io_ooo_to_mem_issueLda_2_bits_uop_storeSetHit;
    logic io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_flag;
    logic [7:0] io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_value;
    logic io_ooo_to_mem_issueLda_2_ready;
    logic io_ooo_to_mem_issueLda_2_valid;
    logic [63:0] io_ooo_to_mem_issueSta_0_bits_src_0;
    logic [8:0] io_ooo_to_mem_issueSta_0_bits_uop_fuOpType;
    logic [34:0] io_ooo_to_mem_issueSta_0_bits_uop_fuType;
    logic [31:0] io_ooo_to_mem_issueSta_0_bits_uop_imm;
    logic [7:0] io_ooo_to_mem_issueSta_0_bits_uop_pdest;
    logic io_ooo_to_mem_issueSta_0_bits_uop_rfWen;
    logic io_ooo_to_mem_issueSta_0_bits_uop_robIdx_flag;
    logic [7:0] io_ooo_to_mem_issueSta_0_bits_uop_robIdx_value;
    logic io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_flag;
    logic [5:0] io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_value;
    logic io_ooo_to_mem_issueSta_0_ready;
    logic io_ooo_to_mem_issueSta_0_valid;
    logic [63:0] io_ooo_to_mem_issueSta_1_bits_src_0;
    logic [8:0] io_ooo_to_mem_issueSta_1_bits_uop_fuOpType;
    logic [34:0] io_ooo_to_mem_issueSta_1_bits_uop_fuType;
    logic [31:0] io_ooo_to_mem_issueSta_1_bits_uop_imm;
    logic [7:0] io_ooo_to_mem_issueSta_1_bits_uop_pdest;
    logic io_ooo_to_mem_issueSta_1_bits_uop_rfWen;
    logic io_ooo_to_mem_issueSta_1_bits_uop_robIdx_flag;
    logic [7:0] io_ooo_to_mem_issueSta_1_bits_uop_robIdx_value;
    logic io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_flag;
    logic [5:0] io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_value;
    logic io_ooo_to_mem_issueSta_1_ready;
    logic io_ooo_to_mem_issueSta_1_valid;
    logic [63:0] io_ooo_to_mem_issueStd_0_bits_src_0;
    logic [8:0] io_ooo_to_mem_issueStd_0_bits_uop_fuOpType;
    logic [34:0] io_ooo_to_mem_issueStd_0_bits_uop_fuType;
    logic [7:0] io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value;
    logic io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_flag;
    logic [5:0] io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_value;
    logic io_ooo_to_mem_issueStd_0_ready;
    logic io_ooo_to_mem_issueStd_0_valid;
    logic [63:0] io_ooo_to_mem_issueStd_1_bits_src_0;
    logic [8:0] io_ooo_to_mem_issueStd_1_bits_uop_fuOpType;
    logic [34:0] io_ooo_to_mem_issueStd_1_bits_uop_fuType;
    logic [7:0] io_ooo_to_mem_issueStd_1_bits_uop_robIdx_value;
    logic io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_flag;
    logic [5:0] io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_value;
    logic io_ooo_to_mem_issueStd_1_ready;
    logic io_ooo_to_mem_issueStd_1_valid;
    lintsissue_agent_agent_xaction  mon_tr;
    while(1) begin
        @this.vif.mon_mp.mon_cb;

        io_ooo_to_mem_issueLda_0_bits_src_0 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_src_0;
        io_ooo_to_mem_issueLda_0_bits_uop_fpWen = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_fpWen;
        io_ooo_to_mem_issueLda_0_bits_uop_ftqOffset = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_ftqOffset;
        io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_flag;
        io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_value;
        io_ooo_to_mem_issueLda_0_bits_uop_fuOpType = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_fuOpType;
        io_ooo_to_mem_issueLda_0_bits_uop_imm = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_imm;
        io_ooo_to_mem_issueLda_0_bits_uop_loadWaitBit = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_loadWaitBit;
        io_ooo_to_mem_issueLda_0_bits_uop_loadWaitStrict = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_loadWaitStrict;
        io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_flag;
        io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_value;
        io_ooo_to_mem_issueLda_0_bits_uop_pc = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_pc;
        io_ooo_to_mem_issueLda_0_bits_uop_pdest = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_pdest;
        io_ooo_to_mem_issueLda_0_bits_uop_preDecodeInfo_isRVC = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_preDecodeInfo_isRVC;
        io_ooo_to_mem_issueLda_0_bits_uop_rfWen = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_rfWen;
        io_ooo_to_mem_issueLda_0_bits_uop_robIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_robIdx_flag;
        io_ooo_to_mem_issueLda_0_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_robIdx_value;
        io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_flag;
        io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_value;
        io_ooo_to_mem_issueLda_0_bits_uop_storeSetHit = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_storeSetHit;
        io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_flag;
        io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_value;
        io_ooo_to_mem_issueLda_0_ready = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_ready;
        io_ooo_to_mem_issueLda_0_valid = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_0_valid;
        io_ooo_to_mem_issueLda_1_bits_src_0 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_src_0;
        io_ooo_to_mem_issueLda_1_bits_uop_fpWen = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_fpWen;
        io_ooo_to_mem_issueLda_1_bits_uop_ftqOffset = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_ftqOffset;
        io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_flag;
        io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_value;
        io_ooo_to_mem_issueLda_1_bits_uop_fuOpType = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_fuOpType;
        io_ooo_to_mem_issueLda_1_bits_uop_imm = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_imm;
        io_ooo_to_mem_issueLda_1_bits_uop_loadWaitBit = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_loadWaitBit;
        io_ooo_to_mem_issueLda_1_bits_uop_loadWaitStrict = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_loadWaitStrict;
        io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_flag;
        io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_value;
        io_ooo_to_mem_issueLda_1_bits_uop_pc = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_pc;
        io_ooo_to_mem_issueLda_1_bits_uop_pdest = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_pdest;
        io_ooo_to_mem_issueLda_1_bits_uop_preDecodeInfo_isRVC = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_preDecodeInfo_isRVC;
        io_ooo_to_mem_issueLda_1_bits_uop_rfWen = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_rfWen;
        io_ooo_to_mem_issueLda_1_bits_uop_robIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_robIdx_flag;
        io_ooo_to_mem_issueLda_1_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_robIdx_value;
        io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_flag;
        io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_value;
        io_ooo_to_mem_issueLda_1_bits_uop_storeSetHit = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_storeSetHit;
        io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_flag;
        io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_value;
        io_ooo_to_mem_issueLda_1_ready = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_ready;
        io_ooo_to_mem_issueLda_1_valid = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_1_valid;
        io_ooo_to_mem_issueLda_2_bits_src_0 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_src_0;
        io_ooo_to_mem_issueLda_2_bits_uop_fpWen = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_fpWen;
        io_ooo_to_mem_issueLda_2_bits_uop_ftqOffset = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_ftqOffset;
        io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_flag;
        io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_value;
        io_ooo_to_mem_issueLda_2_bits_uop_fuOpType = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_fuOpType;
        io_ooo_to_mem_issueLda_2_bits_uop_imm = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_imm;
        io_ooo_to_mem_issueLda_2_bits_uop_loadWaitBit = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_loadWaitBit;
        io_ooo_to_mem_issueLda_2_bits_uop_loadWaitStrict = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_loadWaitStrict;
        io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_flag;
        io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_value;
        io_ooo_to_mem_issueLda_2_bits_uop_pc = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_pc;
        io_ooo_to_mem_issueLda_2_bits_uop_pdest = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_pdest;
        io_ooo_to_mem_issueLda_2_bits_uop_preDecodeInfo_isRVC = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_preDecodeInfo_isRVC;
        io_ooo_to_mem_issueLda_2_bits_uop_rfWen = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_rfWen;
        io_ooo_to_mem_issueLda_2_bits_uop_robIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_robIdx_flag;
        io_ooo_to_mem_issueLda_2_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_robIdx_value;
        io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_flag;
        io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_value;
        io_ooo_to_mem_issueLda_2_bits_uop_storeSetHit = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_storeSetHit;
        io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_flag;
        io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_value;
        io_ooo_to_mem_issueLda_2_ready = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_ready;
        io_ooo_to_mem_issueLda_2_valid = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueLda_2_valid;
        io_ooo_to_mem_issueSta_0_bits_src_0 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_0_bits_src_0;
        io_ooo_to_mem_issueSta_0_bits_uop_fuOpType = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_0_bits_uop_fuOpType;
        io_ooo_to_mem_issueSta_0_bits_uop_fuType = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_0_bits_uop_fuType;
        io_ooo_to_mem_issueSta_0_bits_uop_imm = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_0_bits_uop_imm;
        io_ooo_to_mem_issueSta_0_bits_uop_pdest = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_0_bits_uop_pdest;
        io_ooo_to_mem_issueSta_0_bits_uop_rfWen = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_0_bits_uop_rfWen;
        io_ooo_to_mem_issueSta_0_bits_uop_robIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_0_bits_uop_robIdx_flag;
        io_ooo_to_mem_issueSta_0_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_0_bits_uop_robIdx_value;
        io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_flag;
        io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_value;
        io_ooo_to_mem_issueSta_0_ready = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_0_ready;
        io_ooo_to_mem_issueSta_0_valid = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_0_valid;
        io_ooo_to_mem_issueSta_1_bits_src_0 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_1_bits_src_0;
        io_ooo_to_mem_issueSta_1_bits_uop_fuOpType = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_1_bits_uop_fuOpType;
        io_ooo_to_mem_issueSta_1_bits_uop_fuType = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_1_bits_uop_fuType;
        io_ooo_to_mem_issueSta_1_bits_uop_imm = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_1_bits_uop_imm;
        io_ooo_to_mem_issueSta_1_bits_uop_pdest = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_1_bits_uop_pdest;
        io_ooo_to_mem_issueSta_1_bits_uop_rfWen = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_1_bits_uop_rfWen;
        io_ooo_to_mem_issueSta_1_bits_uop_robIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_1_bits_uop_robIdx_flag;
        io_ooo_to_mem_issueSta_1_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_1_bits_uop_robIdx_value;
        io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_flag;
        io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_value;
        io_ooo_to_mem_issueSta_1_ready = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_1_ready;
        io_ooo_to_mem_issueSta_1_valid = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueSta_1_valid;
        io_ooo_to_mem_issueStd_0_bits_src_0 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_0_bits_src_0;
        io_ooo_to_mem_issueStd_0_bits_uop_fuOpType = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_0_bits_uop_fuOpType;
        io_ooo_to_mem_issueStd_0_bits_uop_fuType = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_0_bits_uop_fuType;
        io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value;
        io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_flag;
        io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_value;
        io_ooo_to_mem_issueStd_0_ready = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_0_ready;
        io_ooo_to_mem_issueStd_0_valid = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_0_valid;
        io_ooo_to_mem_issueStd_1_bits_src_0 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_1_bits_src_0;
        io_ooo_to_mem_issueStd_1_bits_uop_fuOpType = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_1_bits_uop_fuOpType;
        io_ooo_to_mem_issueStd_1_bits_uop_fuType = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_1_bits_uop_fuType;
        io_ooo_to_mem_issueStd_1_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_1_bits_uop_robIdx_value;
        io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_flag;
        io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_value;
        io_ooo_to_mem_issueStd_1_ready = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_1_ready;
        io_ooo_to_mem_issueStd_1_valid = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueStd_1_valid;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin

        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = lintsissue_agent_agent_xaction::type_id::create("mon_tr");

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
