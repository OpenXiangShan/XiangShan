//=========================================================
//File name    : lsqenq_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : lsqenq_agent_agent_xaction
//Discribution : lsqenq_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef LSQENQ_AGENT_AGENT_XACTION__SV
`define LSQENQ_AGENT_AGENT_XACTION__SV

class lsqenq_agent_agent_xaction  extends tcnt_data_base;
    // Base legality follows backend dispatch->LSQ handshake:
    // needAlloc: 0=no alloc, 1=load/vload, 2=store/vstore.
    // fuType and indices must later be refined per scenario.
    bit memblock_dispatch_wait_can_accept;
    int unsigned memblock_dispatch_ready_timeout;
    bit memblock_dispatch_aborted_by_redirect;
    int unsigned memblock_dispatch_flush_epoch;
    rand bit io_ooo_to_mem_enqLsq_canAccept;
    rand bit [1:0] io_ooo_to_mem_enqLsq_needAlloc_0;
    rand bit [1:0] io_ooo_to_mem_enqLsq_needAlloc_1;
    rand bit [1:0] io_ooo_to_mem_enqLsq_needAlloc_2;
    rand bit [1:0] io_ooo_to_mem_enqLsq_needAlloc_3;
    rand bit [1:0] io_ooo_to_mem_enqLsq_needAlloc_4;
    rand bit [1:0] io_ooo_to_mem_enqLsq_needAlloc_5;
    rand bit [1:0] io_ooo_to_mem_enqLsq_needAlloc_6;
    rand bit [1:0] io_ooo_to_mem_enqLsq_needAlloc_7;
    rand bit io_ooo_to_mem_enqLsq_req_0_valid;
    rand bit [35:0] io_ooo_to_mem_enqLsq_req_0_bits_fuType;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_0_bits_uopIdx;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value;
    rand bit [4:0] io_ooo_to_mem_enqLsq_req_0_bits_numLsElem;
    rand bit io_ooo_to_mem_enqLsq_req_1_valid;
    rand bit [35:0] io_ooo_to_mem_enqLsq_req_1_bits_fuType;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_1_bits_uopIdx;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value;
    rand bit [4:0] io_ooo_to_mem_enqLsq_req_1_bits_numLsElem;
    rand bit io_ooo_to_mem_enqLsq_req_2_valid;
    rand bit [35:0] io_ooo_to_mem_enqLsq_req_2_bits_fuType;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_2_bits_uopIdx;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value;
    rand bit [4:0] io_ooo_to_mem_enqLsq_req_2_bits_numLsElem;
    rand bit io_ooo_to_mem_enqLsq_req_3_valid;
    rand bit [35:0] io_ooo_to_mem_enqLsq_req_3_bits_fuType;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_3_bits_uopIdx;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value;
    rand bit [4:0] io_ooo_to_mem_enqLsq_req_3_bits_numLsElem;
    rand bit io_ooo_to_mem_enqLsq_req_4_valid;
    rand bit [35:0] io_ooo_to_mem_enqLsq_req_4_bits_fuType;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_4_bits_uopIdx;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value;
    rand bit [4:0] io_ooo_to_mem_enqLsq_req_4_bits_numLsElem;
    rand bit io_ooo_to_mem_enqLsq_req_5_valid;
    rand bit [35:0] io_ooo_to_mem_enqLsq_req_5_bits_fuType;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_5_bits_uopIdx;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value;
    rand bit [4:0] io_ooo_to_mem_enqLsq_req_5_bits_numLsElem;
    rand bit io_ooo_to_mem_enqLsq_req_6_valid;
    rand bit [35:0] io_ooo_to_mem_enqLsq_req_6_bits_fuType;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_6_bits_uopIdx;
    rand bit io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value;
    rand bit [4:0] io_ooo_to_mem_enqLsq_req_6_bits_numLsElem;
    rand bit io_ooo_to_mem_enqLsq_req_7_valid;
    rand bit [35:0] io_ooo_to_mem_enqLsq_req_7_bits_fuType;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_7_bits_uopIdx;
    rand bit io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value;
    rand bit [4:0] io_ooo_to_mem_enqLsq_req_7_bits_numLsElem;
    rand bit io_ooo_to_mem_enqLsq_resp_0_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_resp_0_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_resp_0_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_resp_0_sqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_resp_1_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_resp_1_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_resp_1_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_resp_1_sqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_resp_2_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_resp_2_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_resp_2_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_resp_2_sqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_resp_3_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_resp_3_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_resp_3_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_resp_3_sqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_resp_4_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_resp_4_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_resp_4_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_resp_4_sqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_resp_5_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_resp_5_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_resp_5_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_resp_5_sqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_resp_6_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_resp_6_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_resp_6_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_resp_6_sqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_resp_7_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_enqLsq_resp_7_lqIdx_value;
    rand bit io_ooo_to_mem_enqLsq_resp_7_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_enqLsq_resp_7_sqIdx_value;

    extern constraint default_io_ooo_to_mem_enqLsq_canAccept_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_needAlloc_0_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_needAlloc_1_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_needAlloc_2_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_needAlloc_3_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_needAlloc_4_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_needAlloc_5_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_needAlloc_6_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_needAlloc_7_cons;
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
    extern constraint default_io_ooo_to_mem_enqLsq_req_6_valid_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_6_bits_fuType_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_6_bits_uopIdx_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_6_bits_numLsElem_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_7_valid_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_7_bits_fuType_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_7_bits_uopIdx_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_req_7_bits_numLsElem_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_0_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_0_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_0_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_0_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_1_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_1_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_1_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_1_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_2_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_2_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_2_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_2_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_3_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_3_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_3_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_3_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_4_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_4_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_4_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_4_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_5_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_5_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_5_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_5_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_6_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_6_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_6_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_6_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_7_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_7_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_7_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_enqLsq_resp_7_sqIdx_value_cons;

    extern function new(string name="lsqenq_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(lsqenq_agent_agent_xaction)
        `uvm_field_int(memblock_dispatch_wait_can_accept, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_ready_timeout, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_aborted_by_redirect, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_flush_epoch, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_canAccept, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_needAlloc_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_needAlloc_1, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_needAlloc_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_needAlloc_3, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_needAlloc_4, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_needAlloc_5, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_needAlloc_6, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_needAlloc_7, UVM_ALL_ON);
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
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_6_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_6_bits_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_6_bits_uopIdx, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_6_bits_numLsElem, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_7_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_7_bits_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_7_bits_uopIdx, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_req_7_bits_numLsElem, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_0_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_0_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_0_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_0_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_1_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_1_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_1_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_1_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_2_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_2_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_2_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_2_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_3_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_3_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_3_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_3_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_4_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_4_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_4_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_4_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_5_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_5_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_5_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_5_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_6_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_6_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_6_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_6_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_7_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_7_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_7_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_enqLsq_resp_7_sqIdx_value, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:lsqenq_agent_agent_xaction

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_canAccept_cons{
    io_ooo_to_mem_enqLsq_canAccept inside {1'b0, 1'b1};
}

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

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_needAlloc_6_cons{
    io_ooo_to_mem_enqLsq_needAlloc_6 inside {2'b00, 2'b01, 2'b10};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_needAlloc_7_cons{
    io_ooo_to_mem_enqLsq_needAlloc_7 inside {2'b00, 2'b01, 2'b10};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_valid_cons{
    io_ooo_to_mem_enqLsq_req_0_valid inside {1'b0, 1'b1};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_fuType_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_fuType inside {36'h0001_0000, 36'h0002_0000, 36'h1000_00000, 36'h2000_00000};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_uopIdx_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_uopIdx inside {[7'd0:7'd64]};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag inside {1'b0, 1'b1};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value inside {[9'd0:9'd351]};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag inside {1'b0, 1'b1};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value inside {[7'd0:7'd71]};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag inside {1'b0, 1'b1};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value inside {[6'd0:6'd55]};
}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_0_bits_numLsElem_cons{
    io_ooo_to_mem_enqLsq_req_0_bits_numLsElem inside {[5'd1:5'd16]};
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

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_6_valid_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_6_bits_fuType_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_6_bits_uopIdx_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_6_bits_numLsElem_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_7_valid_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_7_bits_fuType_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_7_bits_uopIdx_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_req_7_bits_numLsElem_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_0_lqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_0_lqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_0_sqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_0_sqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_1_lqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_1_lqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_1_sqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_1_sqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_2_lqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_2_lqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_2_sqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_2_sqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_3_lqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_3_lqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_3_sqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_3_sqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_4_lqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_4_lqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_4_sqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_4_sqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_5_lqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_5_lqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_5_sqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_5_sqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_6_lqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_6_lqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_6_sqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_6_sqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_7_lqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_7_lqIdx_value_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_7_sqIdx_flag_cons{

}

constraint lsqenq_agent_agent_xaction::default_io_ooo_to_mem_enqLsq_resp_7_sqIdx_value_cons{

}

function lsqenq_agent_agent_xaction::new(string name = "lsqenq_agent_agent_xaction");
    super.new();
    memblock_dispatch_wait_can_accept = 1'b0;
    memblock_dispatch_ready_timeout = 0;
    memblock_dispatch_aborted_by_redirect = 1'b0;
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

function string lsqenq_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_canAccept = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_canAccept);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_needAlloc_0 = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_needAlloc_0);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_needAlloc_1 = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_needAlloc_1);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_needAlloc_2 = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_needAlloc_2);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_needAlloc_3 = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_needAlloc_3);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_needAlloc_4 = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_needAlloc_4);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_needAlloc_5 = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_needAlloc_5);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_needAlloc_6 = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_needAlloc_6);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_needAlloc_7 = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_needAlloc_7);
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
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_6_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_6_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_6_bits_fuType = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_6_bits_fuType);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_6_bits_uopIdx = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_6_bits_uopIdx);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_6_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_6_bits_numLsElem = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_6_bits_numLsElem);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_7_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_7_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_7_bits_fuType = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_7_bits_fuType);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_7_bits_uopIdx = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_7_bits_uopIdx);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_7_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_req_7_bits_numLsElem = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_req_7_bits_numLsElem);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_0_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_0_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_0_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_0_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_0_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_0_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_0_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_0_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_1_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_1_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_1_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_1_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_1_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_1_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_1_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_1_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_2_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_2_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_2_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_2_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_2_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_2_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_2_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_2_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_3_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_3_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_3_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_3_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_3_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_3_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_3_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_3_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_4_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_4_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_4_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_4_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_4_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_4_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_4_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_4_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_5_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_5_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_5_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_5_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_5_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_5_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_5_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_5_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_6_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_6_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_6_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_6_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_6_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_6_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_6_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_6_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_7_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_7_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_7_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_7_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_7_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_7_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_enqLsq_resp_7_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_enqLsq_resp_7_sqIdx_value);

    return pkt_str;
endfunction:psdisplay

function bit lsqenq_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    lsqenq_agent_agent_xaction  rhs_;
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

        if(this.io_ooo_to_mem_enqLsq_canAccept!=rhs_.io_ooo_to_mem_enqLsq_canAccept) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_canAccept=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_canAccept=0x%0h",this.io_ooo_to_mem_enqLsq_canAccept,rhs_.io_ooo_to_mem_enqLsq_canAccept),UVM_NONE)
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

        if(this.io_ooo_to_mem_enqLsq_needAlloc_6!=rhs_.io_ooo_to_mem_enqLsq_needAlloc_6) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_needAlloc_6=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_needAlloc_6=0x%0h",this.io_ooo_to_mem_enqLsq_needAlloc_6,rhs_.io_ooo_to_mem_enqLsq_needAlloc_6),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_needAlloc_7!=rhs_.io_ooo_to_mem_enqLsq_needAlloc_7) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_needAlloc_7=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_needAlloc_7=0x%0h",this.io_ooo_to_mem_enqLsq_needAlloc_7,rhs_.io_ooo_to_mem_enqLsq_needAlloc_7),UVM_NONE)
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

        if(this.io_ooo_to_mem_enqLsq_req_6_valid!=rhs_.io_ooo_to_mem_enqLsq_req_6_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_6_valid=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_6_valid=0x%0h",this.io_ooo_to_mem_enqLsq_req_6_valid,rhs_.io_ooo_to_mem_enqLsq_req_6_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_6_bits_fuType!=rhs_.io_ooo_to_mem_enqLsq_req_6_bits_fuType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_6_bits_fuType=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_6_bits_fuType=0x%0h",this.io_ooo_to_mem_enqLsq_req_6_bits_fuType,rhs_.io_ooo_to_mem_enqLsq_req_6_bits_fuType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_6_bits_uopIdx!=rhs_.io_ooo_to_mem_enqLsq_req_6_bits_uopIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_6_bits_uopIdx=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_6_bits_uopIdx=0x%0h",this.io_ooo_to_mem_enqLsq_req_6_bits_uopIdx,rhs_.io_ooo_to_mem_enqLsq_req_6_bits_uopIdx),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_6_bits_numLsElem!=rhs_.io_ooo_to_mem_enqLsq_req_6_bits_numLsElem) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_6_bits_numLsElem=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_6_bits_numLsElem=0x%0h",this.io_ooo_to_mem_enqLsq_req_6_bits_numLsElem,rhs_.io_ooo_to_mem_enqLsq_req_6_bits_numLsElem),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_7_valid!=rhs_.io_ooo_to_mem_enqLsq_req_7_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_7_valid=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_7_valid=0x%0h",this.io_ooo_to_mem_enqLsq_req_7_valid,rhs_.io_ooo_to_mem_enqLsq_req_7_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_7_bits_fuType!=rhs_.io_ooo_to_mem_enqLsq_req_7_bits_fuType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_7_bits_fuType=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_7_bits_fuType=0x%0h",this.io_ooo_to_mem_enqLsq_req_7_bits_fuType,rhs_.io_ooo_to_mem_enqLsq_req_7_bits_fuType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_7_bits_uopIdx!=rhs_.io_ooo_to_mem_enqLsq_req_7_bits_uopIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_7_bits_uopIdx=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_7_bits_uopIdx=0x%0h",this.io_ooo_to_mem_enqLsq_req_7_bits_uopIdx,rhs_.io_ooo_to_mem_enqLsq_req_7_bits_uopIdx),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_req_7_bits_numLsElem!=rhs_.io_ooo_to_mem_enqLsq_req_7_bits_numLsElem) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_req_7_bits_numLsElem=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_req_7_bits_numLsElem=0x%0h",this.io_ooo_to_mem_enqLsq_req_7_bits_numLsElem,rhs_.io_ooo_to_mem_enqLsq_req_7_bits_numLsElem),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_0_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_0_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_0_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_0_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_0_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_0_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_0_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_0_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_0_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_0_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_0_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_0_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_0_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_0_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_0_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_0_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_0_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_0_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_0_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_0_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_0_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_0_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_0_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_0_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_1_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_1_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_1_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_1_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_1_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_1_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_1_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_1_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_1_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_1_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_1_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_1_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_1_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_1_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_1_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_1_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_1_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_1_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_1_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_1_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_1_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_1_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_1_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_1_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_2_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_2_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_2_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_2_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_2_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_2_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_2_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_2_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_2_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_2_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_2_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_2_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_2_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_2_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_2_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_2_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_2_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_2_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_2_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_2_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_2_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_2_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_2_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_2_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_3_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_3_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_3_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_3_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_3_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_3_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_3_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_3_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_3_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_3_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_3_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_3_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_3_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_3_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_3_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_3_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_3_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_3_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_3_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_3_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_3_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_3_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_3_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_3_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_4_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_4_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_4_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_4_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_4_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_4_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_4_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_4_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_4_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_4_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_4_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_4_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_4_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_4_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_4_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_4_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_4_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_4_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_4_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_4_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_4_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_4_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_4_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_4_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_5_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_5_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_5_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_5_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_5_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_5_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_5_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_5_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_5_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_5_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_5_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_5_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_5_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_5_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_5_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_5_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_5_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_5_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_5_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_5_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_5_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_5_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_5_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_5_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_6_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_6_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_6_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_6_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_6_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_6_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_6_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_6_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_6_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_6_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_6_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_6_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_6_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_6_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_6_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_6_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_6_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_6_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_6_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_6_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_6_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_6_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_6_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_6_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_7_lqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_7_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_7_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_7_lqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_7_lqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_7_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_7_lqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_7_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_7_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_7_lqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_7_lqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_7_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_7_sqIdx_flag!=rhs_.io_ooo_to_mem_enqLsq_resp_7_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_7_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_7_sqIdx_flag=0x%0h",this.io_ooo_to_mem_enqLsq_resp_7_sqIdx_flag,rhs_.io_ooo_to_mem_enqLsq_resp_7_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_enqLsq_resp_7_sqIdx_value!=rhs_.io_ooo_to_mem_enqLsq_resp_7_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_enqLsq_resp_7_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_enqLsq_resp_7_sqIdx_value=0x%0h",this.io_ooo_to_mem_enqLsq_resp_7_sqIdx_value,rhs_.io_ooo_to_mem_enqLsq_resp_7_sqIdx_value),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
