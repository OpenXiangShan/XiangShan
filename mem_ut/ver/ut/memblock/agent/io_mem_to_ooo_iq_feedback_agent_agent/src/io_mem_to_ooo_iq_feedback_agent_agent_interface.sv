//=========================================================
//File name    : io_mem_to_ooo_iq_feedback_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_iq_feedback_agent_agent_interface
//Discribution : io_mem_to_ooo_iq_feedback_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_IQ_FEEDBACK_AGENT_AGENT_INTERFACE__SV
`define IO_MEM_TO_OOO_IQ_FEEDBACK_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface io_mem_to_ooo_iq_feedback_agent_agent_interface  (input bit clk,input bit rst_n);

    logic io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid;
    logic io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit;
    logic io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
    logic [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value;
    logic io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid;
    logic io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit;
    logic io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
    logic [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value;
    logic io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid;
    logic io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit;
    logic io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
    logic [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value;
    logic io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag;
    logic [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value;
    logic io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid;
    logic io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit;
    logic io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
    logic [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value;
    logic io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag;
    logic [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value;

    logic io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_isVecPartReplay;
    logic [15:0] io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMask;
    logic [3:0] io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMbIdx;
    logic io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_isVecPartReplay;
    logic [15:0] io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMask;
    logic [3:0] io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMbIdx;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid;
        input  io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit;
        input  io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
        input  io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value;
        input  io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid;
        input  io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit;
        input  io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
        input  io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value;

        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_isVecPartReplay;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMask;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMbIdx;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_isVecPartReplay;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMask;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMbIdx;
    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid;
        input  io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit;
        input  io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
        input  io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value;
        input  io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid;
        input  io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit;
        input  io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
        input  io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value;

        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_isVecPartReplay;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMask;
        input  io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMbIdx;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_isVecPartReplay;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMask;
        input  io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMbIdx;
    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:io_mem_to_ooo_iq_feedback_agent_agent_interface

`endif
