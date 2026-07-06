//=========================================================
//File name    : io_mem_to_ooo_iq_feedback_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_iq_feedback_agent_agent_monitor
//Discribution : io_mem_to_ooo_iq_feedback_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_IQ_FEEDBACK_AGENT_AGENT_MONITOR__SV
`define IO_MEM_TO_OOO_IQ_FEEDBACK_AGENT_AGENT_MONITOR__SV

class io_mem_to_ooo_iq_feedback_agent_agent_monitor  extends tcnt_monitor_base#(virtual io_mem_to_ooo_iq_feedback_agent_agent_interface,io_mem_to_ooo_iq_feedback_agent_agent_cfg,io_mem_to_ooo_iq_feedback_agent_agent_xaction);

    `uvm_component_utils(io_mem_to_ooo_iq_feedback_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:io_mem_to_ooo_iq_feedback_agent_agent_monitor

function io_mem_to_ooo_iq_feedback_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void io_mem_to_ooo_iq_feedback_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task io_mem_to_ooo_iq_feedback_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task io_mem_to_ooo_iq_feedback_agent_agent_monitor::mon_data();

    logic io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid;
    logic io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_value;
    logic io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit;
    logic io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_flushState;
    logic [3:0] io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sourceType;
    logic io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
    logic [5:0] io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value;
    logic io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_flag;
    logic [6:0] io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_value;
    logic io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid;
    logic io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_value;
    logic io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit;
    logic io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_flushState;
    logic [3:0] io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sourceType;
    logic io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
    logic [5:0] io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value;
    logic io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_flag;
    logic [6:0] io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_value;
    logic io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid;
    logic io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_value;
    logic io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit;
    logic [3:0] io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sourceType;
    logic io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
    logic [5:0] io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value;
    logic io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag;
    logic [6:0] io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value;
    logic io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid;
    logic io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_value;
    logic io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit;
    logic [3:0] io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sourceType;
    logic io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
    logic [5:0] io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value;
    logic io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag;
    logic [6:0] io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value;
    logic io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_valid;
    logic io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_value;
    logic io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_hit;
    logic io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_flushState;
    logic [3:0] io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sourceType;
    logic io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
    logic [5:0] io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_value;
    logic io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_flag;
    logic [6:0] io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_value;
    logic io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_valid;
    logic io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_value;
    logic io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_hit;
    logic io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_flushState;
    logic [3:0] io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sourceType;
    logic io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
    logic [5:0] io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_value;
    logic io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_flag;
    logic [6:0] io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_value;

    io_mem_to_ooo_iq_feedback_agent_agent_xaction  mon_tr;
    memblock_sync_pkg::dispatch_raw_iq_feedback_t raw_iq_feedback;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid;
        io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_flag;
        io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_value;
        io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit;
        io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_flushState = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_flushState;
        io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sourceType = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sourceType;
        io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
        io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value;
        io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_flag;
        io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_value;
        io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid;
        io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_flag;
        io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_value;
        io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit;
        io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_flushState = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_flushState;
        io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sourceType = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sourceType;
        io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
        io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value;
        io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_flag;
        io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_value;
        io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid;
        io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_flag;
        io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_value;
        io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit;
        io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sourceType = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sourceType;
        io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
        io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value;
        io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag;
        io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value;
        io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid;
        io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_flag;
        io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_value;
        io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit;
        io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sourceType = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sourceType;
        io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
        io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value;
        io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag;
        io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value;
        io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_valid;
        io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_flag;
        io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_value;
        io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_hit = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_hit;
        io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_flushState = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_flushState;
        io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sourceType = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sourceType;
        io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
        io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_value;
        io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_flag;
        io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_value;
        io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_valid;
        io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_flag;
        io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_value;
        io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_hit = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_hit;
        io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_flushState = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_flushState;
        io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sourceType = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sourceType;
        io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
        io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_value;
        io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_flag = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_flag;
        io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_value = this.vif.mon_mp.mon_cb.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_value;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid,io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_flag,io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_value,io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_value,9);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit,io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_flushState,io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_flushState,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sourceType,io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sourceType,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag,io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value,io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value,6);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_flag,io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_value,io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_value,7);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid,io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_flag,io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_value,io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_value,9);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit,io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_flushState,io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_flushState,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sourceType,io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sourceType,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag,io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value,io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value,6);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_flag,io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_value,io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_value,7);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid,io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_flag,io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_value,io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_value,9);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit,io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sourceType,io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sourceType,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag,io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value,io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value,6);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag,io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value,io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value,7);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid,io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_flag,io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_value,io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_value,9);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit,io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sourceType,io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sourceType,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag,io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value,io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value,6);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag,io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value,io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value,7);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_valid,io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_flag,io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_value,io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_value,9);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_hit,io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_hit,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_flushState,io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_flushState,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sourceType,io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sourceType,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_flag,io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_value,io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_value,6);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_flag,io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_value,io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_value,7);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_valid,io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_flag,io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_value,io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_value,9);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_hit,io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_hit,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_flushState,io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_flushState,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sourceType,io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sourceType,4);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_flag,io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_value,io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_value,6);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_flag,io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_value,io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_value,7);

        end
        if(this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            if (io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid) begin
                raw_iq_feedback = memblock_sync_pkg::make_empty_raw_iq_feedback();
                raw_iq_feedback.valid = 1'b1;
                raw_iq_feedback.port_id = 0;
                raw_iq_feedback.is_sta = 1'b1;
                raw_iq_feedback.rob_valid = 1'b1;
                raw_iq_feedback.rob_flag = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_flag;
                raw_iq_feedback.rob_value = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_value;
                raw_iq_feedback.hit = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit;
                raw_iq_feedback.flush_state = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_flushState;
                raw_iq_feedback.source_type = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sourceType;
                raw_iq_feedback.sq_valid = 1'b1;
                raw_iq_feedback.sq_flag = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
                raw_iq_feedback.sq_value = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value;
                raw_iq_feedback.lq_valid = 1'b1;
                raw_iq_feedback.lq_flag = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_flag;
                raw_iq_feedback.lq_value = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_value;
                raw_iq_feedback.cycle = $time;
                memblock_sync_pkg::push_raw_iq_feedback(raw_iq_feedback);
            end
            if (io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid) begin
                raw_iq_feedback = memblock_sync_pkg::make_empty_raw_iq_feedback();
                raw_iq_feedback.valid = 1'b1;
                raw_iq_feedback.port_id = 1;
                raw_iq_feedback.is_sta = 1'b1;
                raw_iq_feedback.rob_valid = 1'b1;
                raw_iq_feedback.rob_flag = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_flag;
                raw_iq_feedback.rob_value = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_value;
                raw_iq_feedback.hit = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit;
                raw_iq_feedback.flush_state = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_flushState;
                raw_iq_feedback.source_type = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sourceType;
                raw_iq_feedback.sq_valid = 1'b1;
                raw_iq_feedback.sq_flag = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
                raw_iq_feedback.sq_value = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value;
                raw_iq_feedback.lq_valid = 1'b1;
                raw_iq_feedback.lq_flag = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_flag;
                raw_iq_feedback.lq_value = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_value;
                raw_iq_feedback.cycle = $time;
                memblock_sync_pkg::push_raw_iq_feedback(raw_iq_feedback);
            end
        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = io_mem_to_ooo_iq_feedback_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_flag = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_flag;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_value = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_value;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_flushState = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_flushState;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sourceType = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sourceType;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_flag = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_flag;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_value = io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_value;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_flag = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_flag;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_value = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_value;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_flushState = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_flushState;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sourceType = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sourceType;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_flag = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_flag;
        //    mon_tr.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_value = io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_value;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid = io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_flag = io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_flag;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_value = io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_value;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit = io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sourceType = io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sourceType;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag = io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value = io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag = io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value = io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid = io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_flag = io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_flag;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_value = io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_value;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit = io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sourceType = io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sourceType;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag = io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value = io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag = io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag;
        //    mon_tr.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value = io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_valid = io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_valid;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_flag = io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_flag;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_value = io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_value;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_hit = io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_hit;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_flushState = io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_flushState;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sourceType = io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sourceType;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_flag = io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_value = io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_value;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_flag = io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_flag;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_value = io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_value;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_valid = io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_valid;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_flag = io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_flag;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_value = io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_value;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_hit = io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_hit;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_flushState = io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_flushState;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sourceType = io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sourceType;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_flag = io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_value = io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_value;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_flag = io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_flag;
        //    mon_tr.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_value = io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_value;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
