//=========================================================
//File name    : lsqenq_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : lsqenq_agent_agent_xaction
//Discribution : lsqenq_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef LSQENQ_AGENT_AGENT_XACTION__SV
`define LSQENQ_AGENT_AGENT_XACTION__SV

`include "memblock_compile_params.svh"

`define MEMBLOCK_V2_LSQ_LOAD_OR_PREFETCH_FUOPTYPE_VALUES [9'd0:9'd6], 9'd8, 9'd9, 9'd10
`define MEMBLOCK_V2_LSQ_STORE_FUOPTYPE_VALUES [9'd0:9'd3]

class lsqenq_agent_agent_xaction  extends tcnt_data_base;
    // Base legality follows backend dispatch->LSQ handshake:
    // needAlloc: 0=no alloc, 1=load/vload, 2=store/vstore.
    // fuType and indices must later be refined per scenario.
    bit memblock_dispatch_wait_can_accept;
    int unsigned memblock_dispatch_ready_timeout;
    // V2 driver在launch前redirect时置abort；成功写VIF时只置launched，sequence据此决定是否预留资源。
    bit memblock_dispatch_aborted_by_redirect;
    bit memblock_dispatch_request_launched;
    int unsigned memblock_dispatch_flush_epoch;
    rand bit [1:0] io_ooo_to_mem_enqLsq_needAlloc_0;
    rand bit [1:0] io_ooo_to_mem_enqLsq_needAlloc_1;
    rand bit [1:0] io_ooo_to_mem_enqLsq_needAlloc_2;
    rand bit [1:0] io_ooo_to_mem_enqLsq_needAlloc_3;
    rand bit [1:0] io_ooo_to_mem_enqLsq_needAlloc_4;
    rand bit [1:0] io_ooo_to_mem_enqLsq_needAlloc_5;
    rand bit io_ooo_to_mem_enqLsq_req_0_valid;
    rand bit [`MEMBLOCK_DUT_FUTYPE_W-1:0] io_ooo_to_mem_enqLsq_req_0_bits_fuType;
    rand bit [`MEMBLOCK_DUT_UOP_IDX_W-1:0] io_ooo_to_mem_enqLsq_req_0_bits_uopIdx;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag;
    rand bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag;
    rand bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value;
    rand bit [`MEMBLOCK_DUT_NUM_LS_ELEM_W-1:0] io_ooo_to_mem_enqLsq_req_0_bits_numLsElem;
    rand bit io_ooo_to_mem_enqLsq_req_1_valid;
    rand bit [`MEMBLOCK_DUT_FUTYPE_W-1:0] io_ooo_to_mem_enqLsq_req_1_bits_fuType;
    rand bit [`MEMBLOCK_DUT_UOP_IDX_W-1:0] io_ooo_to_mem_enqLsq_req_1_bits_uopIdx;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag;
    rand bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag;
    rand bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value;
    rand bit [`MEMBLOCK_DUT_NUM_LS_ELEM_W-1:0] io_ooo_to_mem_enqLsq_req_1_bits_numLsElem;
    rand bit io_ooo_to_mem_enqLsq_req_2_valid;
    rand bit [`MEMBLOCK_DUT_FUTYPE_W-1:0] io_ooo_to_mem_enqLsq_req_2_bits_fuType;
    rand bit [`MEMBLOCK_DUT_UOP_IDX_W-1:0] io_ooo_to_mem_enqLsq_req_2_bits_uopIdx;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag;
    rand bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag;
    rand bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value;
    rand bit [`MEMBLOCK_DUT_NUM_LS_ELEM_W-1:0] io_ooo_to_mem_enqLsq_req_2_bits_numLsElem;
    rand bit io_ooo_to_mem_enqLsq_req_3_valid;
    rand bit [`MEMBLOCK_DUT_FUTYPE_W-1:0] io_ooo_to_mem_enqLsq_req_3_bits_fuType;
    rand bit [`MEMBLOCK_DUT_UOP_IDX_W-1:0] io_ooo_to_mem_enqLsq_req_3_bits_uopIdx;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag;
    rand bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag;
    rand bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value;
    rand bit [`MEMBLOCK_DUT_NUM_LS_ELEM_W-1:0] io_ooo_to_mem_enqLsq_req_3_bits_numLsElem;
    rand bit io_ooo_to_mem_enqLsq_req_4_valid;
    rand bit [`MEMBLOCK_DUT_FUTYPE_W-1:0] io_ooo_to_mem_enqLsq_req_4_bits_fuType;
    rand bit [`MEMBLOCK_DUT_UOP_IDX_W-1:0] io_ooo_to_mem_enqLsq_req_4_bits_uopIdx;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag;
    rand bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag;
    rand bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value;
    rand bit [`MEMBLOCK_DUT_NUM_LS_ELEM_W-1:0] io_ooo_to_mem_enqLsq_req_4_bits_numLsElem;
    rand bit io_ooo_to_mem_enqLsq_req_5_valid;
    rand bit [`MEMBLOCK_DUT_FUTYPE_W-1:0] io_ooo_to_mem_enqLsq_req_5_bits_fuType;
    rand bit [`MEMBLOCK_DUT_UOP_IDX_W-1:0] io_ooo_to_mem_enqLsq_req_5_bits_uopIdx;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag;
    rand bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag;
    rand bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag;
    rand bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value;
    rand bit [`MEMBLOCK_DUT_NUM_LS_ELEM_W-1:0] io_ooo_to_mem_enqLsq_req_5_bits_numLsElem;

    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_1;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_10;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_11;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_12;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_13;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_14;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_15;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_16;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_17;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_18;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_19;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_2;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_20;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_21;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_22;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_23;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_3;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_4;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_5;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_6;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_7;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_8;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_9;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_flushPipe;
    rand bit [8:0] io_ooo_to_mem_enqLsq_req_0_bits_fuOpType;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_lastUop;
    rand bit [3:0] io_ooo_to_mem_enqLsq_req_0_bits_trigger;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_0;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_1;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_10;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_11;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_12;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_13;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_14;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_15;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_16;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_17;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_18;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_19;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_2;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_20;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_21;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_22;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_23;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_3;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_4;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_5;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_6;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_7;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_8;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_9;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_flushPipe;
    rand bit [8:0] io_ooo_to_mem_enqLsq_req_1_bits_fuOpType;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_lastUop;
    rand bit [3:0] io_ooo_to_mem_enqLsq_req_1_bits_trigger;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_0;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_1;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_10;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_11;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_12;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_13;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_14;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_15;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_16;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_17;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_18;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_19;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_2;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_20;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_21;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_22;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_23;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_3;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_4;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_5;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_6;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_7;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_8;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_9;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_flushPipe;
    rand bit [8:0] io_ooo_to_mem_enqLsq_req_2_bits_fuOpType;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_lastUop;
    rand bit [3:0] io_ooo_to_mem_enqLsq_req_2_bits_trigger;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_0;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_1;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_10;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_11;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_12;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_13;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_14;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_15;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_16;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_17;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_18;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_19;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_2;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_20;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_21;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_22;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_23;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_3;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_4;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_5;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_6;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_7;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_8;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_9;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_flushPipe;
    rand bit [8:0] io_ooo_to_mem_enqLsq_req_3_bits_fuOpType;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_lastUop;
    rand bit [3:0] io_ooo_to_mem_enqLsq_req_3_bits_trigger;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_0;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_1;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_10;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_11;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_12;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_13;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_14;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_15;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_16;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_17;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_18;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_19;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_2;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_20;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_21;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_22;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_23;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_3;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_4;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_5;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_6;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_7;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_8;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_9;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_flushPipe;
    rand bit [8:0] io_ooo_to_mem_enqLsq_req_4_bits_fuOpType;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_lastUop;
    rand bit [3:0] io_ooo_to_mem_enqLsq_req_4_bits_trigger;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_0;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_1;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_10;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_11;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_12;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_13;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_14;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_15;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_16;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_17;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_18;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_19;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_2;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_20;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_21;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_22;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_23;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_3;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_4;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_5;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_6;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_7;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_8;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_9;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_flushPipe;
    rand bit [8:0] io_ooo_to_mem_enqLsq_req_5_bits_fuOpType;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_lastUop;
    rand bit [3:0] io_ooo_to_mem_enqLsq_req_5_bits_trigger;

    // V2 scalar LQ accepts ordinary load plus software-prefetch opcodes; SQ accepts ordinary stores only.
    static function bit is_supported_v2_load_or_prefetch_fuoptype(input bit [8:0] fu_op_type);
        return fu_op_type inside {`MEMBLOCK_V2_LSQ_LOAD_OR_PREFETCH_FUOPTYPE_VALUES};
    endfunction:is_supported_v2_load_or_prefetch_fuoptype

    static function bit is_supported_v2_store_fuoptype(input bit [8:0] fu_op_type);
        return fu_op_type inside {`MEMBLOCK_V2_LSQ_STORE_FUOPTYPE_VALUES};
    endfunction:is_supported_v2_store_fuoptype

    constraint v2_streaming_gap_cons {
        pre_pkt_gap == 0;
        post_pkt_gap == 0;
    }

    extern constraint default_io_ooo_to_mem_enqLsq_needAlloc_0_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_needAlloc_1_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_needAlloc_2_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_needAlloc_3_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_needAlloc_4_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_needAlloc_5_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_0_valid_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_0_bits_fuType_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_0_bits_uopIdx_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_0_bits_numLsElem_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_1_valid_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_1_bits_fuType_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_1_bits_uopIdx_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_1_bits_numLsElem_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_2_valid_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_2_bits_fuType_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_2_bits_uopIdx_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_2_bits_numLsElem_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_3_valid_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_3_bits_fuType_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_3_bits_uopIdx_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_3_bits_numLsElem_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_4_valid_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_4_bits_fuType_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_4_bits_uopIdx_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_4_bits_numLsElem_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_5_valid_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_5_bits_fuType_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_5_bits_uopIdx_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_5_bits_numLsElem_cons;

    constraint c_v2_scalar_request_contract {
        if (!io_ooo_to_mem_enqLsq_req_0_valid) {
            io_ooo_to_mem_enqLsq_needAlloc_0 == 2'b00;
            io_ooo_to_mem_enqLsq_req_0_bits_fuType == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_uopIdx == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_numLsElem == '0;
            {io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_23,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_22,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_21,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_20,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_19,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_18,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_17,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_16,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_15,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_14,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_13,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_12,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_11,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_10,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_9,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_8,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_7,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_6,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_5,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_4,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_3,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_2,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_1,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0} == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_trigger == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_fuOpType == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_flushPipe == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_lastUop == '0;
        } else {
            io_ooo_to_mem_enqLsq_needAlloc_0 inside {2'b01, 2'b10};
            (io_ooo_to_mem_enqLsq_needAlloc_0 == 2'b01) ->
                io_ooo_to_mem_enqLsq_req_0_bits_fuType == (1 << `MEMBLOCK_DUT_FUTYPE_LDU_BIT);
            (io_ooo_to_mem_enqLsq_needAlloc_0 == 2'b10) ->
                io_ooo_to_mem_enqLsq_req_0_bits_fuType == (1 << `MEMBLOCK_DUT_FUTYPE_STU_BIT);
            (io_ooo_to_mem_enqLsq_needAlloc_0 == 2'b01) ->
                io_ooo_to_mem_enqLsq_req_0_bits_fuOpType inside {`MEMBLOCK_V2_LSQ_LOAD_OR_PREFETCH_FUOPTYPE_VALUES};
            (io_ooo_to_mem_enqLsq_needAlloc_0 == 2'b10) ->
                io_ooo_to_mem_enqLsq_req_0_bits_fuOpType inside {`MEMBLOCK_V2_LSQ_STORE_FUOPTYPE_VALUES};
            io_ooo_to_mem_enqLsq_req_0_bits_uopIdx == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value inside {[0:`MEMBLOCK_DUT_ROB_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value inside {[0:`MEMBLOCK_DUT_LQ_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value inside {[0:`MEMBLOCK_DUT_SQ_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_0_bits_numLsElem == 1;
            {io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_23,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_22,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_21,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_20,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_19,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_18,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_17,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_16,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_15,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_14,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_13,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_12,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_11,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_10,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_9,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_8,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_7,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_6,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_5,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_4,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_3,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_2,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_1,
             io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0} == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_trigger == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_flushPipe == '0;
            io_ooo_to_mem_enqLsq_req_0_bits_lastUop == 1'b1;
        }

        if (!io_ooo_to_mem_enqLsq_req_1_valid) {
            io_ooo_to_mem_enqLsq_needAlloc_1 == 2'b00;
            io_ooo_to_mem_enqLsq_req_1_bits_fuType == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_uopIdx == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_numLsElem == '0;
            {io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_23,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_22,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_21,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_20,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_19,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_18,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_17,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_16,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_15,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_14,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_13,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_12,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_11,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_10,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_9,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_8,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_7,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_6,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_5,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_4,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_3,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_2,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_1,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_0} == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_trigger == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_fuOpType == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_flushPipe == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_lastUop == '0;
        } else {
            io_ooo_to_mem_enqLsq_needAlloc_1 inside {2'b01, 2'b10};
            (io_ooo_to_mem_enqLsq_needAlloc_1 == 2'b01) ->
                io_ooo_to_mem_enqLsq_req_1_bits_fuType == (1 << `MEMBLOCK_DUT_FUTYPE_LDU_BIT);
            (io_ooo_to_mem_enqLsq_needAlloc_1 == 2'b10) ->
                io_ooo_to_mem_enqLsq_req_1_bits_fuType == (1 << `MEMBLOCK_DUT_FUTYPE_STU_BIT);
            (io_ooo_to_mem_enqLsq_needAlloc_1 == 2'b01) ->
                io_ooo_to_mem_enqLsq_req_1_bits_fuOpType inside {`MEMBLOCK_V2_LSQ_LOAD_OR_PREFETCH_FUOPTYPE_VALUES};
            (io_ooo_to_mem_enqLsq_needAlloc_1 == 2'b10) ->
                io_ooo_to_mem_enqLsq_req_1_bits_fuOpType inside {`MEMBLOCK_V2_LSQ_STORE_FUOPTYPE_VALUES};
            io_ooo_to_mem_enqLsq_req_1_bits_uopIdx == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value inside {[0:`MEMBLOCK_DUT_ROB_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value inside {[0:`MEMBLOCK_DUT_LQ_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value inside {[0:`MEMBLOCK_DUT_SQ_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_1_bits_numLsElem == 1;
            {io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_23,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_22,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_21,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_20,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_19,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_18,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_17,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_16,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_15,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_14,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_13,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_12,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_11,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_10,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_9,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_8,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_7,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_6,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_5,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_4,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_3,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_2,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_1,
             io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_0} == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_trigger == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_flushPipe == '0;
            io_ooo_to_mem_enqLsq_req_1_bits_lastUop == 1'b1;
        }

        if (!io_ooo_to_mem_enqLsq_req_2_valid) {
            io_ooo_to_mem_enqLsq_needAlloc_2 == 2'b00;
            io_ooo_to_mem_enqLsq_req_2_bits_fuType == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_uopIdx == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_numLsElem == '0;
            {io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_23,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_22,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_21,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_20,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_19,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_18,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_17,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_16,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_15,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_14,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_13,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_12,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_11,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_10,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_9,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_8,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_7,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_6,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_5,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_4,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_3,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_2,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_1,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_0} == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_trigger == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_fuOpType == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_flushPipe == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_lastUop == '0;
        } else {
            io_ooo_to_mem_enqLsq_needAlloc_2 inside {2'b01, 2'b10};
            (io_ooo_to_mem_enqLsq_needAlloc_2 == 2'b01) ->
                io_ooo_to_mem_enqLsq_req_2_bits_fuType == (1 << `MEMBLOCK_DUT_FUTYPE_LDU_BIT);
            (io_ooo_to_mem_enqLsq_needAlloc_2 == 2'b10) ->
                io_ooo_to_mem_enqLsq_req_2_bits_fuType == (1 << `MEMBLOCK_DUT_FUTYPE_STU_BIT);
            (io_ooo_to_mem_enqLsq_needAlloc_2 == 2'b01) ->
                io_ooo_to_mem_enqLsq_req_2_bits_fuOpType inside {`MEMBLOCK_V2_LSQ_LOAD_OR_PREFETCH_FUOPTYPE_VALUES};
            (io_ooo_to_mem_enqLsq_needAlloc_2 == 2'b10) ->
                io_ooo_to_mem_enqLsq_req_2_bits_fuOpType inside {`MEMBLOCK_V2_LSQ_STORE_FUOPTYPE_VALUES};
            io_ooo_to_mem_enqLsq_req_2_bits_uopIdx == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value inside {[0:`MEMBLOCK_DUT_ROB_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value inside {[0:`MEMBLOCK_DUT_LQ_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value inside {[0:`MEMBLOCK_DUT_SQ_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_2_bits_numLsElem == 1;
            {io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_23,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_22,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_21,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_20,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_19,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_18,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_17,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_16,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_15,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_14,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_13,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_12,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_11,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_10,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_9,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_8,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_7,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_6,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_5,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_4,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_3,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_2,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_1,
             io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_0} == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_trigger == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_flushPipe == '0;
            io_ooo_to_mem_enqLsq_req_2_bits_lastUop == 1'b1;
        }

        if (!io_ooo_to_mem_enqLsq_req_3_valid) {
            io_ooo_to_mem_enqLsq_needAlloc_3 == 2'b00;
            io_ooo_to_mem_enqLsq_req_3_bits_fuType == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_uopIdx == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_numLsElem == '0;
            {io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_23,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_22,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_21,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_20,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_19,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_18,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_17,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_16,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_15,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_14,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_13,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_12,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_11,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_10,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_9,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_8,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_7,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_6,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_5,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_4,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_3,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_2,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_1,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_0} == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_trigger == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_fuOpType == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_flushPipe == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_lastUop == '0;
        } else {
            io_ooo_to_mem_enqLsq_needAlloc_3 inside {2'b01, 2'b10};
            (io_ooo_to_mem_enqLsq_needAlloc_3 == 2'b01) ->
                io_ooo_to_mem_enqLsq_req_3_bits_fuType == (1 << `MEMBLOCK_DUT_FUTYPE_LDU_BIT);
            (io_ooo_to_mem_enqLsq_needAlloc_3 == 2'b10) ->
                io_ooo_to_mem_enqLsq_req_3_bits_fuType == (1 << `MEMBLOCK_DUT_FUTYPE_STU_BIT);
            (io_ooo_to_mem_enqLsq_needAlloc_3 == 2'b01) ->
                io_ooo_to_mem_enqLsq_req_3_bits_fuOpType inside {`MEMBLOCK_V2_LSQ_LOAD_OR_PREFETCH_FUOPTYPE_VALUES};
            (io_ooo_to_mem_enqLsq_needAlloc_3 == 2'b10) ->
                io_ooo_to_mem_enqLsq_req_3_bits_fuOpType inside {`MEMBLOCK_V2_LSQ_STORE_FUOPTYPE_VALUES};
            io_ooo_to_mem_enqLsq_req_3_bits_uopIdx == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value inside {[0:`MEMBLOCK_DUT_ROB_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value inside {[0:`MEMBLOCK_DUT_LQ_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value inside {[0:`MEMBLOCK_DUT_SQ_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_3_bits_numLsElem == 1;
            {io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_23,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_22,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_21,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_20,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_19,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_18,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_17,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_16,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_15,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_14,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_13,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_12,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_11,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_10,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_9,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_8,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_7,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_6,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_5,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_4,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_3,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_2,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_1,
             io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_0} == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_trigger == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_flushPipe == '0;
            io_ooo_to_mem_enqLsq_req_3_bits_lastUop == 1'b1;
        }

        if (!io_ooo_to_mem_enqLsq_req_4_valid) {
            io_ooo_to_mem_enqLsq_needAlloc_4 == 2'b00;
            io_ooo_to_mem_enqLsq_req_4_bits_fuType == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_uopIdx == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_numLsElem == '0;
            {io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_23,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_22,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_21,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_20,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_19,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_18,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_17,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_16,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_15,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_14,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_13,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_12,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_11,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_10,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_9,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_8,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_7,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_6,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_5,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_4,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_3,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_2,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_1,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_0} == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_trigger == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_fuOpType == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_flushPipe == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_lastUop == '0;
        } else {
            io_ooo_to_mem_enqLsq_needAlloc_4 inside {2'b01, 2'b10};
            (io_ooo_to_mem_enqLsq_needAlloc_4 == 2'b01) ->
                io_ooo_to_mem_enqLsq_req_4_bits_fuType == (1 << `MEMBLOCK_DUT_FUTYPE_LDU_BIT);
            (io_ooo_to_mem_enqLsq_needAlloc_4 == 2'b10) ->
                io_ooo_to_mem_enqLsq_req_4_bits_fuType == (1 << `MEMBLOCK_DUT_FUTYPE_STU_BIT);
            (io_ooo_to_mem_enqLsq_needAlloc_4 == 2'b01) ->
                io_ooo_to_mem_enqLsq_req_4_bits_fuOpType inside {`MEMBLOCK_V2_LSQ_LOAD_OR_PREFETCH_FUOPTYPE_VALUES};
            (io_ooo_to_mem_enqLsq_needAlloc_4 == 2'b10) ->
                io_ooo_to_mem_enqLsq_req_4_bits_fuOpType inside {`MEMBLOCK_V2_LSQ_STORE_FUOPTYPE_VALUES};
            io_ooo_to_mem_enqLsq_req_4_bits_uopIdx == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value inside {[0:`MEMBLOCK_DUT_ROB_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value inside {[0:`MEMBLOCK_DUT_LQ_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value inside {[0:`MEMBLOCK_DUT_SQ_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_4_bits_numLsElem == 1;
            {io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_23,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_22,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_21,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_20,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_19,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_18,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_17,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_16,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_15,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_14,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_13,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_12,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_11,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_10,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_9,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_8,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_7,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_6,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_5,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_4,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_3,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_2,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_1,
             io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_0} == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_trigger == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_flushPipe == '0;
            io_ooo_to_mem_enqLsq_req_4_bits_lastUop == 1'b1;
        }

        if (!io_ooo_to_mem_enqLsq_req_5_valid) {
            io_ooo_to_mem_enqLsq_needAlloc_5 == 2'b00;
            io_ooo_to_mem_enqLsq_req_5_bits_fuType == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_uopIdx == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_numLsElem == '0;
            {io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_23,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_22,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_21,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_20,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_19,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_18,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_17,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_16,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_15,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_14,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_13,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_12,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_11,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_10,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_9,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_8,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_7,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_6,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_5,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_4,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_3,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_2,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_1,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_0} == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_trigger == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_fuOpType == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_flushPipe == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_lastUop == '0;
        } else {
            io_ooo_to_mem_enqLsq_needAlloc_5 inside {2'b01, 2'b10};
            (io_ooo_to_mem_enqLsq_needAlloc_5 == 2'b01) ->
                io_ooo_to_mem_enqLsq_req_5_bits_fuType == (1 << `MEMBLOCK_DUT_FUTYPE_LDU_BIT);
            (io_ooo_to_mem_enqLsq_needAlloc_5 == 2'b10) ->
                io_ooo_to_mem_enqLsq_req_5_bits_fuType == (1 << `MEMBLOCK_DUT_FUTYPE_STU_BIT);
            (io_ooo_to_mem_enqLsq_needAlloc_5 == 2'b01) ->
                io_ooo_to_mem_enqLsq_req_5_bits_fuOpType inside {`MEMBLOCK_V2_LSQ_LOAD_OR_PREFETCH_FUOPTYPE_VALUES};
            (io_ooo_to_mem_enqLsq_needAlloc_5 == 2'b10) ->
                io_ooo_to_mem_enqLsq_req_5_bits_fuOpType inside {`MEMBLOCK_V2_LSQ_STORE_FUOPTYPE_VALUES};
            io_ooo_to_mem_enqLsq_req_5_bits_uopIdx == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value inside {[0:`MEMBLOCK_DUT_ROB_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value inside {[0:`MEMBLOCK_DUT_LQ_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value inside {[0:`MEMBLOCK_DUT_SQ_SIZE-1]};
            io_ooo_to_mem_enqLsq_req_5_bits_numLsElem == 1;
            {io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_23,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_22,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_21,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_20,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_19,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_18,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_17,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_16,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_15,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_14,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_13,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_12,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_11,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_10,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_9,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_8,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_7,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_6,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_5,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_4,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_3,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_2,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_1,
             io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_0} == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_trigger == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_flushPipe == '0;
            io_ooo_to_mem_enqLsq_req_5_bits_lastUop == 1'b1;
        }
    }

    // 中文注释：V2每拍最多接收6个load element和4个store element。
    // 该batch约束覆盖随机default sequence；dispatch sequence还会按实际LSQ free count进一步截断。
    constraint c_v2_batch_enqueue_width {
        int'(io_ooo_to_mem_enqLsq_req_0_valid && io_ooo_to_mem_enqLsq_needAlloc_0 == 2'b01) +
        int'(io_ooo_to_mem_enqLsq_req_1_valid && io_ooo_to_mem_enqLsq_needAlloc_1 == 2'b01) +
        int'(io_ooo_to_mem_enqLsq_req_2_valid && io_ooo_to_mem_enqLsq_needAlloc_2 == 2'b01) +
        int'(io_ooo_to_mem_enqLsq_req_3_valid && io_ooo_to_mem_enqLsq_needAlloc_3 == 2'b01) +
        int'(io_ooo_to_mem_enqLsq_req_4_valid && io_ooo_to_mem_enqLsq_needAlloc_4 == 2'b01) +
        int'(io_ooo_to_mem_enqLsq_req_5_valid && io_ooo_to_mem_enqLsq_needAlloc_5 == 2'b01)
            <= `MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH;

        int'(io_ooo_to_mem_enqLsq_req_0_valid && io_ooo_to_mem_enqLsq_needAlloc_0 == 2'b10) +
        int'(io_ooo_to_mem_enqLsq_req_1_valid && io_ooo_to_mem_enqLsq_needAlloc_1 == 2'b10) +
        int'(io_ooo_to_mem_enqLsq_req_2_valid && io_ooo_to_mem_enqLsq_needAlloc_2 == 2'b10) +
        int'(io_ooo_to_mem_enqLsq_req_3_valid && io_ooo_to_mem_enqLsq_needAlloc_3 == 2'b10) +
        int'(io_ooo_to_mem_enqLsq_req_4_valid && io_ooo_to_mem_enqLsq_needAlloc_4 == 2'b10) +
        int'(io_ooo_to_mem_enqLsq_req_5_valid && io_ooo_to_mem_enqLsq_needAlloc_5 == 2'b10)
            <= `MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH;
    }

    typedef struct packed {
        bit [23:0] exception_vec;
        bit [3:0]  trigger;
        bit [8:0]  fu_op_type;
        bit        flush_pipe;
        bit        last_uop;
    } v2_extra_fields_t;

    extern function new(string name="lsqenq_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function v2_extra_fields_t get_v2_extra_fields(input int unsigned slot);
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(lsqenq_agent_agent_xaction)
        `uvm_field_int(memblock_dispatch_wait_can_accept, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_ready_timeout, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_aborted_by_redirect, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_request_launched, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_flush_epoch, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_needAlloc_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_needAlloc_1, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_needAlloc_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_needAlloc_3, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_needAlloc_4, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_needAlloc_5, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_uopIdx, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_numLsElem, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_uopIdx, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_numLsElem, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_uopIdx, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_numLsElem, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_uopIdx, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_numLsElem, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_uopIdx, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_numLsElem, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_uopIdx, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_numLsElem, UVM_ALL_ON);

        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_1, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_10, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_11, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_12, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_13, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_14, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_15, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_16, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_17, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_18, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_19, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_20, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_21, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_22, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_23, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_3, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_4, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_5, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_6, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_7, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_8, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_9, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_flushPipe, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_lastUop, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_0_bits_trigger, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_1, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_10, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_11, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_12, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_13, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_14, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_15, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_16, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_17, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_18, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_19, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_20, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_21, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_22, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_23, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_3, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_4, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_5, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_6, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_7, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_8, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_9, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_flushPipe, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_lastUop, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_1_bits_trigger, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_1, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_10, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_11, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_12, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_13, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_14, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_15, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_16, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_17, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_18, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_19, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_20, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_21, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_22, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_23, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_3, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_4, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_5, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_6, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_7, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_8, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_9, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_flushPipe, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_lastUop, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_2_bits_trigger, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_1, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_10, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_11, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_12, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_13, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_14, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_15, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_16, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_17, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_18, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_19, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_20, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_21, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_22, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_23, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_3, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_4, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_5, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_6, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_7, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_8, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_9, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_flushPipe, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_lastUop, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_3_bits_trigger, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_1, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_10, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_11, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_12, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_13, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_14, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_15, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_16, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_17, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_18, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_19, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_20, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_21, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_22, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_23, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_3, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_4, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_5, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_6, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_7, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_8, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_9, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_flushPipe, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_lastUop, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_4_bits_trigger, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_1, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_10, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_11, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_12, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_13, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_14, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_15, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_16, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_17, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_18, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_19, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_20, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_21, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_22, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_23, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_3, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_4, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_5, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_6, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_7, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_8, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_9, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_flushPipe, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_lastUop, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_5_bits_trigger, UVM_ALL_ON);
    `uvm_object_utils_end

endclass:lsqenq_agent_agent_xaction


constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_needAlloc_0_cons{
    io_ooo_to_mem_enqLsq_needAlloc_0 inside {2'b00, 2'b01, 2'b10};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_needAlloc_1_cons{
    io_ooo_to_mem_enqLsq_needAlloc_1 inside {2'b00, 2'b01, 2'b10};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_needAlloc_2_cons{
    io_ooo_to_mem_enqLsq_needAlloc_2 inside {2'b00, 2'b01, 2'b10};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_needAlloc_3_cons{
    io_ooo_to_mem_enqLsq_needAlloc_3 inside {2'b00, 2'b01, 2'b10};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_needAlloc_4_cons{
    io_ooo_to_mem_enqLsq_needAlloc_4 inside {2'b00, 2'b01, 2'b10};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_needAlloc_5_cons{
    io_ooo_to_mem_enqLsq_needAlloc_5 inside {2'b00, 2'b01, 2'b10};
}



constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_valid_cons{
    io_ooo_to_mem_enqLsq_req_0_valid inside {1'b0, 1'b1};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_fuType_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_fuType inside {'0,
                                                   (1 << `MEMBLOCK_DUT_FUTYPE_LDU_BIT),
                                                   (1 << `MEMBLOCK_DUT_FUTYPE_STU_BIT)};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_uopIdx_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_uopIdx inside {[0:`MEMBLOCK_DUT_MAX_UOP_SIZE-1]};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag inside {1'b0, 1'b1};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value inside {[0:`MEMBLOCK_DUT_ROB_SIZE-1]};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag inside {1'b0, 1'b1};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value inside {[0:`MEMBLOCK_DUT_LQ_SIZE-1]};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag inside {1'b0, 1'b1};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value inside {[0:`MEMBLOCK_DUT_SQ_SIZE-1]};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_numLsElem_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_numLsElem inside {[0:`MEMBLOCK_DUT_MAX_LS_ELEM]};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_1_valid_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_1_bits_fuType_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_1_bits_uopIdx_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_1_bits_numLsElem_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_2_valid_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_2_bits_fuType_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_2_bits_uopIdx_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_2_bits_numLsElem_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_3_valid_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_3_bits_fuType_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_3_bits_uopIdx_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_3_bits_numLsElem_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_4_valid_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_4_bits_fuType_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_4_bits_uopIdx_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_4_bits_numLsElem_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_5_valid_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_5_bits_fuType_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_5_bits_uopIdx_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_5_bits_numLsElem_cons{

}





















































function lsqenq_agent_agent_xaction::new(string name = "lsqenq_agent_agent_xaction");
    super.new();
    memblock_dispatch_wait_can_accept = 1'b0;
    memblock_dispatch_ready_timeout = 0;
    memblock_dispatch_aborted_by_redirect = 1'b0;
    memblock_dispatch_request_launched = 1'b0;
    memblock_dispatch_flush_epoch = 0;
endfunction:new

function void lsqenq_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void lsqenq_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void lsqenq_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void lsqenq_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function lsqenq_agent_agent_xaction::v2_extra_fields_t
lsqenq_agent_agent_xaction::get_v2_extra_fields(input int unsigned slot);
    v2_extra_fields_t fields;

    fields = '0;
    case (slot)
        0: begin
            fields.exception_vec = {io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_23,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_22,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_21,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_20,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_19,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_18,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_17,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_16,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_15,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_14,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_13,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_12,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_11,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_10,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_9,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_8,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_7,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_6,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_5,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_4,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_3,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_2,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_1,
                                    io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0};
            fields.trigger = io_ooo_to_mem_enqLsq_req_0_bits_trigger;
            fields.fu_op_type = io_ooo_to_mem_enqLsq_req_0_bits_fuOpType;
            fields.flush_pipe = io_ooo_to_mem_enqLsq_req_0_bits_flushPipe;
            fields.last_uop = io_ooo_to_mem_enqLsq_req_0_bits_lastUop;
        end
        1: begin
            fields.exception_vec = {io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_23,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_22,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_21,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_20,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_19,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_18,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_17,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_16,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_15,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_14,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_13,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_12,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_11,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_10,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_9,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_8,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_7,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_6,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_5,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_4,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_3,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_2,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_1,
                                    io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_0};
            fields.trigger = io_ooo_to_mem_enqLsq_req_1_bits_trigger;
            fields.fu_op_type = io_ooo_to_mem_enqLsq_req_1_bits_fuOpType;
            fields.flush_pipe = io_ooo_to_mem_enqLsq_req_1_bits_flushPipe;
            fields.last_uop = io_ooo_to_mem_enqLsq_req_1_bits_lastUop;
        end
        2: begin
            fields.exception_vec = {io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_23,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_22,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_21,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_20,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_19,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_18,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_17,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_16,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_15,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_14,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_13,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_12,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_11,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_10,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_9,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_8,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_7,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_6,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_5,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_4,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_3,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_2,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_1,
                                    io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_0};
            fields.trigger = io_ooo_to_mem_enqLsq_req_2_bits_trigger;
            fields.fu_op_type = io_ooo_to_mem_enqLsq_req_2_bits_fuOpType;
            fields.flush_pipe = io_ooo_to_mem_enqLsq_req_2_bits_flushPipe;
            fields.last_uop = io_ooo_to_mem_enqLsq_req_2_bits_lastUop;
        end
        3: begin
            fields.exception_vec = {io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_23,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_22,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_21,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_20,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_19,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_18,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_17,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_16,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_15,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_14,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_13,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_12,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_11,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_10,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_9,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_8,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_7,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_6,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_5,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_4,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_3,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_2,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_1,
                                    io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_0};
            fields.trigger = io_ooo_to_mem_enqLsq_req_3_bits_trigger;
            fields.fu_op_type = io_ooo_to_mem_enqLsq_req_3_bits_fuOpType;
            fields.flush_pipe = io_ooo_to_mem_enqLsq_req_3_bits_flushPipe;
            fields.last_uop = io_ooo_to_mem_enqLsq_req_3_bits_lastUop;
        end
        4: begin
            fields.exception_vec = {io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_23,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_22,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_21,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_20,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_19,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_18,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_17,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_16,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_15,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_14,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_13,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_12,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_11,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_10,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_9,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_8,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_7,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_6,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_5,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_4,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_3,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_2,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_1,
                                    io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_0};
            fields.trigger = io_ooo_to_mem_enqLsq_req_4_bits_trigger;
            fields.fu_op_type = io_ooo_to_mem_enqLsq_req_4_bits_fuOpType;
            fields.flush_pipe = io_ooo_to_mem_enqLsq_req_4_bits_flushPipe;
            fields.last_uop = io_ooo_to_mem_enqLsq_req_4_bits_lastUop;
        end
        5: begin
            fields.exception_vec = {io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_23,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_22,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_21,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_20,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_19,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_18,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_17,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_16,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_15,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_14,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_13,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_12,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_11,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_10,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_9,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_8,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_7,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_6,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_5,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_4,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_3,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_2,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_1,
                                    io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_0};
            fields.trigger = io_ooo_to_mem_enqLsq_req_5_bits_trigger;
            fields.fu_op_type = io_ooo_to_mem_enqLsq_req_5_bits_fuOpType;
            fields.flush_pipe = io_ooo_to_mem_enqLsq_req_5_bits_flushPipe;
            fields.last_uop = io_ooo_to_mem_enqLsq_req_5_bits_lastUop;
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unsupported V2 LSQ enqueue slot=%0d", slot))
        end
    endcase
    return fields;
endfunction:get_v2_extra_fields

function string lsqenq_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    v2_extra_fields_t fields;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    pkt_str = $sformatf("%spre_pkt_gap=%0d post_pkt_gap=%0d ",
                        pkt_str,
                        this.pre_pkt_gap,
                        this.post_pkt_gap);
    pkt_str = $sformatf("%swait_can_accept=%0b ready_timeout=%0d request_launched=%0b aborted_by_redirect=%0b flush_epoch=%0d ",
                        pkt_str,
                        this.memblock_dispatch_wait_can_accept,
                        this.memblock_dispatch_ready_timeout,
                        this.memblock_dispatch_request_launched,
                        this.memblock_dispatch_aborted_by_redirect,
                        this.memblock_dispatch_flush_epoch);
    for (int unsigned slot = 0; slot < `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM; slot++) begin
        fields = get_v2_extra_fields(slot);
        pkt_str = $sformatf("%sslot%0d_v2_extra={exceptionVec=0x%0h trigger=0x%0h fuOpType=0x%0h flushPipe=%0b lastUop=%0b} ",
                            pkt_str,
                            slot,
                            fields.exception_vec,
                            fields.trigger,
                            fields.fu_op_type,
                            fields.flush_pipe,
                            fields.last_uop);
    end
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_needAlloc_0 = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_needAlloc_0);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_needAlloc_1 = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_needAlloc_1);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_needAlloc_2 = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_needAlloc_2);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_needAlloc_3 = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_needAlloc_3);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_needAlloc_4 = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_needAlloc_4);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_needAlloc_5 = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_needAlloc_5);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_0_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_0_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_0_bits_fuType = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_0_bits_fuType);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_0_bits_uopIdx = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_0_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_0_bits_numLsElem = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_1_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_1_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_1_bits_fuType = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_1_bits_fuType);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_1_bits_uopIdx = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_1_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_1_bits_numLsElem = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_2_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_2_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_2_bits_fuType = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_2_bits_fuType);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_2_bits_uopIdx = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_2_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_2_bits_numLsElem = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_3_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_3_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_3_bits_fuType = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_3_bits_fuType);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_3_bits_uopIdx = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_3_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_3_bits_numLsElem = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_4_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_4_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_4_bits_fuType = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_4_bits_fuType);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_4_bits_uopIdx = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_4_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_4_bits_numLsElem = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_5_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_5_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_5_bits_fuType = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_5_bits_fuType);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_5_bits_uopIdx = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_5_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_5_bits_numLsElem = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem);

    return pkt_str;
endfunction:psdisplay

function bit lsqenq_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    lsqenq_agent_agent_xaction  rhs_;
    v2_extra_fields_t lhs_fields;
    v2_extra_fields_t rhs_fields;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a lsqenq_agent_agent_xaction or its extend"))
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

        if (this.pre_pkt_gap != rhs_.pre_pkt_gap ||
            this.post_pkt_gap != rhs_.post_pkt_gap) begin
            super_result = 0;
            `uvm_info(get_type_name(),
                      $sformatf("compare fail for streaming gap: this={pre=%0d post=%0d} rhs={pre=%0d post=%0d}",
                                this.pre_pkt_gap,
                                this.post_pkt_gap,
                                rhs_.pre_pkt_gap,
                                rhs_.post_pkt_gap),
                      UVM_NONE)
        end
        if (this.memblock_dispatch_wait_can_accept != rhs_.memblock_dispatch_wait_can_accept ||
            this.memblock_dispatch_ready_timeout != rhs_.memblock_dispatch_ready_timeout ||
            this.memblock_dispatch_request_launched != rhs_.memblock_dispatch_request_launched ||
            this.memblock_dispatch_aborted_by_redirect != rhs_.memblock_dispatch_aborted_by_redirect ||
            this.memblock_dispatch_flush_epoch != rhs_.memblock_dispatch_flush_epoch) begin
            super_result = 0;
            `uvm_info(get_type_name(),
                      $sformatf("compare fail for dispatch metadata: this={wait=%0b timeout=%0d launched=%0b aborted=%0b epoch=%0d} rhs={wait=%0b timeout=%0d launched=%0b aborted=%0b epoch=%0d}",
                                this.memblock_dispatch_wait_can_accept,
                                this.memblock_dispatch_ready_timeout,
                                this.memblock_dispatch_request_launched,
                                this.memblock_dispatch_aborted_by_redirect,
                                this.memblock_dispatch_flush_epoch,
                                rhs_.memblock_dispatch_wait_can_accept,
                                rhs_.memblock_dispatch_ready_timeout,
                                rhs_.memblock_dispatch_request_launched,
                                rhs_.memblock_dispatch_aborted_by_redirect,
                                rhs_.memblock_dispatch_flush_epoch),
                      UVM_NONE)
        end
        for (int unsigned slot = 0; slot < `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM; slot++) begin
            lhs_fields = this.get_v2_extra_fields(slot);
            rhs_fields = rhs_.get_v2_extra_fields(slot);
            if (lhs_fields != rhs_fields) begin
                super_result = 0;
                `uvm_info(get_type_name(),
                          $sformatf("compare fail for slot%0d V2 extra fields: this=0x%0h rhs=0x%0h",
                                    slot,
                                    lhs_fields,
                                    rhs_fields),
                          UVM_NONE)
            end
        end

        if(this.io_ooo_to_mem_enqLsq_needAlloc_0!=rhs_.io_ooo_to_mem_enqLsq_needAlloc_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_needAlloc_0=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_needAlloc_0=0x%0h",this.io_ooo_to_mem_enqLsq_needAlloc_0,rhs_.io_ooo_to_mem_enqLsq_needAlloc_0),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_needAlloc_1!=rhs_.io_ooo_to_mem_enqLsq_needAlloc_1) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_needAlloc_1=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_needAlloc_1=0x%0h",this.io_ooo_to_mem_enqLsq_needAlloc_1,rhs_.io_ooo_to_mem_enqLsq_needAlloc_1),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_needAlloc_2!=rhs_.io_ooo_to_mem_enqLsq_needAlloc_2) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_needAlloc_2=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_needAlloc_2=0x%0h",this.io_ooo_to_mem_enqLsq_needAlloc_2,rhs_.io_ooo_to_mem_enqLsq_needAlloc_2),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_needAlloc_3!=rhs_.io_ooo_to_mem_enqLsq_needAlloc_3) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_needAlloc_3=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_needAlloc_3=0x%0h",this.io_ooo_to_mem_enqLsq_needAlloc_3,rhs_.io_ooo_to_mem_enqLsq_needAlloc_3),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_needAlloc_4!=rhs_.io_ooo_to_mem_enqLsq_needAlloc_4) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_needAlloc_4=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_needAlloc_4=0x%0h",this.io_ooo_to_mem_enqLsq_needAlloc_4,rhs_.io_ooo_to_mem_enqLsq_needAlloc_4),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_needAlloc_5!=rhs_.io_ooo_to_mem_enqLsq_needAlloc_5) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_needAlloc_5=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_needAlloc_5=0x%0h",this.io_ooo_to_mem_enqLsq_needAlloc_5,rhs_.io_ooo_to_mem_enqLsq_needAlloc_5),UVM_NONE)
        end



        if(this.io_ooo_to_mem_enqLsq_req_0_valid!=rhs_.io_ooo_to_mem_enqLsq_req_0_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_0_valid=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_0_valid=0x%0h",this.io_ooo_to_mem_enqLsq_req_0_valid,rhs_.io_ooo_to_mem_enqLsq_req_0_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_0_bits_fuType!=rhs_.io_ooo_to_mem_enqLsq_req_0_bits_fuType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_0_bits_fuType=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_0_bits_fuType=0x%0h",this.io_ooo_to_mem_enqLsq_req_0_bits_fuType,rhs_.io_ooo_to_mem_enqLsq_req_0_bits_fuType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx!=rhs_.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx=0x%0h",this.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx,rhs_.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem!=rhs_.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem=0x%0h",this.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem,rhs_.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_1_valid!=rhs_.io_ooo_to_mem_enqLsq_req_1_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_1_valid=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_1_valid=0x%0h",this.io_ooo_to_mem_enqLsq_req_1_valid,rhs_.io_ooo_to_mem_enqLsq_req_1_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_1_bits_fuType!=rhs_.io_ooo_to_mem_enqLsq_req_1_bits_fuType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_1_bits_fuType=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_1_bits_fuType=0x%0h",this.io_ooo_to_mem_enqLsq_req_1_bits_fuType,rhs_.io_ooo_to_mem_enqLsq_req_1_bits_fuType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx!=rhs_.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx=0x%0h",this.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx,rhs_.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem!=rhs_.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem=0x%0h",this.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem,rhs_.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_2_valid!=rhs_.io_ooo_to_mem_enqLsq_req_2_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_2_valid=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_2_valid=0x%0h",this.io_ooo_to_mem_enqLsq_req_2_valid,rhs_.io_ooo_to_mem_enqLsq_req_2_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_2_bits_fuType!=rhs_.io_ooo_to_mem_enqLsq_req_2_bits_fuType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_2_bits_fuType=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_2_bits_fuType=0x%0h",this.io_ooo_to_mem_enqLsq_req_2_bits_fuType,rhs_.io_ooo_to_mem_enqLsq_req_2_bits_fuType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx!=rhs_.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx=0x%0h",this.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx,rhs_.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem!=rhs_.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem=0x%0h",this.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem,rhs_.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_3_valid!=rhs_.io_ooo_to_mem_enqLsq_req_3_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_3_valid=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_3_valid=0x%0h",this.io_ooo_to_mem_enqLsq_req_3_valid,rhs_.io_ooo_to_mem_enqLsq_req_3_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_3_bits_fuType!=rhs_.io_ooo_to_mem_enqLsq_req_3_bits_fuType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_3_bits_fuType=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_3_bits_fuType=0x%0h",this.io_ooo_to_mem_enqLsq_req_3_bits_fuType,rhs_.io_ooo_to_mem_enqLsq_req_3_bits_fuType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx!=rhs_.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx=0x%0h",this.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx,rhs_.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem!=rhs_.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem=0x%0h",this.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem,rhs_.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_4_valid!=rhs_.io_ooo_to_mem_enqLsq_req_4_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_4_valid=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_4_valid=0x%0h",this.io_ooo_to_mem_enqLsq_req_4_valid,rhs_.io_ooo_to_mem_enqLsq_req_4_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_4_bits_fuType!=rhs_.io_ooo_to_mem_enqLsq_req_4_bits_fuType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_4_bits_fuType=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_4_bits_fuType=0x%0h",this.io_ooo_to_mem_enqLsq_req_4_bits_fuType,rhs_.io_ooo_to_mem_enqLsq_req_4_bits_fuType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx!=rhs_.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx=0x%0h",this.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx,rhs_.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem!=rhs_.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem=0x%0h",this.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem,rhs_.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_5_valid!=rhs_.io_ooo_to_mem_enqLsq_req_5_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_5_valid=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_5_valid=0x%0h",this.io_ooo_to_mem_enqLsq_req_5_valid,rhs_.io_ooo_to_mem_enqLsq_req_5_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_5_bits_fuType!=rhs_.io_ooo_to_mem_enqLsq_req_5_bits_fuType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_5_bits_fuType=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_5_bits_fuType=0x%0h",this.io_ooo_to_mem_enqLsq_req_5_bits_fuType,rhs_.io_ooo_to_mem_enqLsq_req_5_bits_fuType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx!=rhs_.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx=0x%0h",this.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx,rhs_.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem!=rhs_.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem=0x%0h",this.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem,rhs_.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem),UVM_NONE)
        end





















































    end
    return super_result;
endfunction:compare

`undef MEMBLOCK_V2_LSQ_LOAD_OR_PREFETCH_FUOPTYPE_VALUES
`undef MEMBLOCK_V2_LSQ_STORE_FUOPTYPE_VALUES

`endif
