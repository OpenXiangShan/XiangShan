//=========================================================
//File name    : io_mem_to_ooo_iq_feedback_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_iq_feedback_agent_connect
//Discribution : io_mem_to_ooo_iq_feedback_agent_connect : io_mem_to_ooo_iq_feedback_agent Interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_IQ_FEEDBACK_AGENT_CONNECT__SV
`define IO_MEM_TO_OOO_IQ_FEEDBACK_AGENT_CONNECT__SV

`define MEMBLOCK__IO_MEM_TO_OOO_IQ_FEEDBACK_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    io_mem_to_ooo_iq_feedback_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    initial begin \
        uvm_config_db#(virtual io_mem_to_ooo_iq_feedback_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    `ifdef MEMBLOCK_UT \
    initial begin \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid = RTL_PATH.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit = RTL_PATH.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_flushState = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sourceType = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag = RTL_PATH.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value = RTL_PATH.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid = RTL_PATH.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit = RTL_PATH.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_flushState = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sourceType = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag = RTL_PATH.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value = RTL_PATH.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sourceType = '0; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sourceType = '0; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_valid = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_hit = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_flushState = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sourceType = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_valid = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_hit = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_flushState = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sourceType = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_value = '0; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid = RTL_PATH.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_valid; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_robIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit = RTL_PATH.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_hit; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_flushState = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sourceType = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag = RTL_PATH.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value = RTL_PATH.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_sqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_0_feedbackSlow_bits_lqIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid = RTL_PATH.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_valid; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_robIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit = RTL_PATH.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_hit; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_flushState = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sourceType = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag = RTL_PATH.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value = RTL_PATH.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_sqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_staIqFeedback_1_feedbackSlow_bits_lqIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_robIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sourceType = '0; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_sqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_lqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_robIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sourceType = '0; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_sqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value = RTL_PATH.io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_lqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_valid = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_robIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_hit = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_flushState = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sourceType = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_sqIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_0_feedbackSlow_bits_lqIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_valid = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_robIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_hit = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_flushState = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sourceType = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_sqIdx_value = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_flag = '0; \
        force U_IF_NAME.io_mem_to_ooo_vlduIqFeedback_1_feedbackSlow_bits_lqIdx_value = '0; \
    end \
    `endif

`endif
