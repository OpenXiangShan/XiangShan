//=========================================================
//File name    : lintsissue_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : lintsissue_agent_agent_xaction
//Discribution : lintsissue_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef LINTSISSUE_AGENT_AGENT_XACTION__SV
`define LINTSISSUE_AGENT_AGENT_XACTION__SV

class lintsissue_agent_agent_xaction  extends tcnt_data_base;
    // intIssue carries the scalar LSU entry traffic. Base constraints here only
    // capture width/capacity and broad opcode legality; per-port semantics are
    // expected to be tightened by scenario sequences.
    rand bit io_ooo_to_mem_intIssue_6_0_ready;
    rand bit io_ooo_to_mem_intIssue_6_0_valid;
    rand bit [35:0] io_ooo_to_mem_intIssue_6_0_bits_fuType;
    rand bit [8:0] io_ooo_to_mem_intIssue_6_0_bits_fuOpType;
    rand bit [63:0] io_ooo_to_mem_intIssue_6_0_bits_src_0;
    rand bit io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_intIssue_6_0_bits_robIdx_value;
    rand bit io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value;
    rand bit io_ooo_to_mem_intIssue_5_0_ready;
    rand bit io_ooo_to_mem_intIssue_5_0_valid;
    rand bit [35:0] io_ooo_to_mem_intIssue_5_0_bits_fuType;
    rand bit [8:0] io_ooo_to_mem_intIssue_5_0_bits_fuOpType;
    rand bit [63:0] io_ooo_to_mem_intIssue_5_0_bits_src_0;
    rand bit io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_intIssue_5_0_bits_robIdx_value;
    rand bit io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value;
    rand bit io_ooo_to_mem_intIssue_4_0_ready;
    rand bit io_ooo_to_mem_intIssue_4_0_valid;
    rand bit [35:0] io_ooo_to_mem_intIssue_4_0_bits_fuType;
    rand bit [8:0] io_ooo_to_mem_intIssue_4_0_bits_fuOpType;
    rand bit [63:0] io_ooo_to_mem_intIssue_4_0_bits_src_0;
    rand bit [63:0] io_ooo_to_mem_intIssue_4_0_bits_imm;
    rand bit io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_intIssue_4_0_bits_robIdx_value;
    rand bit io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue;
    rand bit [7:0] io_ooo_to_mem_intIssue_4_0_bits_pdest;
    rand bit io_ooo_to_mem_intIssue_4_0_bits_isRVC;
    rand bit io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value;
    rand bit [4:0] io_ooo_to_mem_intIssue_4_0_bits_ftqOffset;
    rand bit io_ooo_to_mem_intIssue_4_0_bits_storeSetHit;
    rand bit [4:0] io_ooo_to_mem_intIssue_4_0_bits_ssid;
    rand bit io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value;
    rand bit io_ooo_to_mem_intIssue_3_0_ready;
    rand bit io_ooo_to_mem_intIssue_3_0_valid;
    rand bit [35:0] io_ooo_to_mem_intIssue_3_0_bits_fuType;
    rand bit [8:0] io_ooo_to_mem_intIssue_3_0_bits_fuOpType;
    rand bit [63:0] io_ooo_to_mem_intIssue_3_0_bits_src_0;
    rand bit [63:0] io_ooo_to_mem_intIssue_3_0_bits_imm;
    rand bit io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_intIssue_3_0_bits_robIdx_value;
    rand bit io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue;
    rand bit [7:0] io_ooo_to_mem_intIssue_3_0_bits_pdest;
    rand bit io_ooo_to_mem_intIssue_3_0_bits_isRVC;
    rand bit io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value;
    rand bit [4:0] io_ooo_to_mem_intIssue_3_0_bits_ftqOffset;
    rand bit io_ooo_to_mem_intIssue_3_0_bits_storeSetHit;
    rand bit [4:0] io_ooo_to_mem_intIssue_3_0_bits_ssid;
    rand bit io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value;
    rand bit io_ooo_to_mem_intIssue_2_0_ready;
    rand bit io_ooo_to_mem_intIssue_2_0_valid;
    rand bit [8:0] io_ooo_to_mem_intIssue_2_0_bits_fuOpType;
    rand bit [63:0] io_ooo_to_mem_intIssue_2_0_bits_src_0;
    rand bit [63:0] io_ooo_to_mem_intIssue_2_0_bits_imm;
    rand bit io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_intIssue_2_0_bits_robIdx_value;
    rand bit [7:0] io_ooo_to_mem_intIssue_2_0_bits_pdest;
    rand bit io_ooo_to_mem_intIssue_2_0_bits_rfWen;
    rand bit io_ooo_to_mem_intIssue_2_0_bits_fpWen;
    rand bit [49:0] io_ooo_to_mem_intIssue_2_0_bits_pc;
    rand bit io_ooo_to_mem_intIssue_2_0_bits_isRVC;
    rand bit io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value;
    rand bit [4:0] io_ooo_to_mem_intIssue_2_0_bits_ftqOffset;
    rand bit io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit;
    rand bit io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag;
    rand bit [8:0] io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value;
    rand bit io_ooo_to_mem_intIssue_2_0_bits_storeSetHit;
    rand bit io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict;
    rand bit io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value;
    rand bit io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value;
    rand bit io_ooo_to_mem_intIssue_1_0_ready;
    rand bit io_ooo_to_mem_intIssue_1_0_valid;
    rand bit [8:0] io_ooo_to_mem_intIssue_1_0_bits_fuOpType;
    rand bit [63:0] io_ooo_to_mem_intIssue_1_0_bits_src_0;
    rand bit [63:0] io_ooo_to_mem_intIssue_1_0_bits_imm;
    rand bit io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_intIssue_1_0_bits_robIdx_value;
    rand bit [7:0] io_ooo_to_mem_intIssue_1_0_bits_pdest;
    rand bit io_ooo_to_mem_intIssue_1_0_bits_rfWen;
    rand bit io_ooo_to_mem_intIssue_1_0_bits_fpWen;
    rand bit [49:0] io_ooo_to_mem_intIssue_1_0_bits_pc;
    rand bit io_ooo_to_mem_intIssue_1_0_bits_isRVC;
    rand bit io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value;
    rand bit [4:0] io_ooo_to_mem_intIssue_1_0_bits_ftqOffset;
    rand bit io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit;
    rand bit io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag;
    rand bit [8:0] io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value;
    rand bit io_ooo_to_mem_intIssue_1_0_bits_storeSetHit;
    rand bit io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict;
    rand bit io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value;
    rand bit io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value;
    rand bit io_ooo_to_mem_intIssue_0_0_ready;
    rand bit io_ooo_to_mem_intIssue_0_0_valid;
    rand bit [8:0] io_ooo_to_mem_intIssue_0_0_bits_fuOpType;
    rand bit [63:0] io_ooo_to_mem_intIssue_0_0_bits_src_0;
    rand bit [63:0] io_ooo_to_mem_intIssue_0_0_bits_imm;
    rand bit io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_intIssue_0_0_bits_robIdx_value;
    rand bit [7:0] io_ooo_to_mem_intIssue_0_0_bits_pdest;
    rand bit io_ooo_to_mem_intIssue_0_0_bits_rfWen;
    rand bit io_ooo_to_mem_intIssue_0_0_bits_fpWen;
    rand bit [49:0] io_ooo_to_mem_intIssue_0_0_bits_pc;
    rand bit io_ooo_to_mem_intIssue_0_0_bits_isRVC;
    rand bit io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value;
    rand bit [4:0] io_ooo_to_mem_intIssue_0_0_bits_ftqOffset;
    rand bit io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit;
    rand bit io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag;
    rand bit [8:0] io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value;
    rand bit io_ooo_to_mem_intIssue_0_0_bits_storeSetHit;
    rand bit io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict;
    rand bit io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value;
    rand bit io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value;
    bit memblock_dispatch_wait_ready;
    // 中文注释：本次 issue xaction 是否使用非阻塞 ready 采样。
    // 置位：memblock_issue_dispatch_base_sequence 从 seq_csr_common 读取开关后写入。
    // 作用：为 1 时 driver 只采样一次 valid&&ready，未 fire port 不设置 fired_mask，sequence 后续不出队这些 item。
    bit memblock_dispatch_nonblocking_issue;
    int unsigned memblock_dispatch_ready_timeout;
    bit memblock_dispatch_aborted_by_redirect;
    int unsigned memblock_dispatch_flush_epoch;
    bit [6:0] memblock_dispatch_fired_mask;

    extern constraint default_io_ooo_to_mem_intIssue_6_0_ready_cons;
    extern constraint default_io_ooo_to_mem_intIssue_6_0_valid_cons;
    extern constraint default_io_ooo_to_mem_intIssue_6_0_bits_fuType_cons;
    extern constraint default_io_ooo_to_mem_intIssue_6_0_bits_fuOpType_cons;
    extern constraint default_io_ooo_to_mem_intIssue_6_0_bits_src_0_cons;
    extern constraint default_io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_6_0_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_5_0_ready_cons;
    extern constraint default_io_ooo_to_mem_intIssue_5_0_valid_cons;
    extern constraint default_io_ooo_to_mem_intIssue_5_0_bits_fuType_cons;
    extern constraint default_io_ooo_to_mem_intIssue_5_0_bits_fuOpType_cons;
    extern constraint default_io_ooo_to_mem_intIssue_5_0_bits_src_0_cons;
    extern constraint default_io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_5_0_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_ready_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_valid_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_fuType_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_fuOpType_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_src_0_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_imm_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_pdest_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_isRVC_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_ftqOffset_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_storeSetHit_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_ssid_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_ready_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_valid_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_fuType_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_fuOpType_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_src_0_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_imm_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_pdest_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_isRVC_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_ftqOffset_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_storeSetHit_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_ssid_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_ready_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_valid_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_fuOpType_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_src_0_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_imm_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_pdest_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_rfWen_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_fpWen_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_pc_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_isRVC_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_ftqOffset_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_storeSetHit_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_ready_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_valid_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_fuOpType_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_src_0_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_imm_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_pdest_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_rfWen_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_fpWen_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_pc_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_isRVC_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_ftqOffset_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_storeSetHit_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_ready_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_valid_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_fuOpType_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_src_0_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_imm_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_pdest_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_rfWen_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_fpWen_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_pc_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_isRVC_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_ftqOffset_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_storeSetHit_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value_cons;

    extern function new(string name="lintsissue_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(lintsissue_agent_agent_xaction)
        `uvm_field_int(io_ooo_to_mem_intIssue_6_0_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_6_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_6_0_bits_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_6_0_bits_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_6_0_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_6_0_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_5_0_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_5_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_5_0_bits_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_5_0_bits_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_5_0_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_5_0_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_imm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_pdest, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_isRVC, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_ftqOffset, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_storeSetHit, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_ssid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_imm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_pdest, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_isRVC, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_ftqOffset, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_storeSetHit, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_ssid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_imm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_pdest, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_rfWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_fpWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_pc, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_isRVC, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_ftqOffset, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_storeSetHit, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_imm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_pdest, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_rfWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_fpWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_pc, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_isRVC, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_ftqOffset, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_storeSetHit, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_imm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_pdest, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_rfWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_fpWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_pc, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_isRVC, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_ftqOffset, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_storeSetHit, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_wait_ready, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_nonblocking_issue, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_ready_timeout, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_aborted_by_redirect, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_flush_epoch, UVM_ALL_ON);
        `uvm_field_int(memblock_dispatch_fired_mask, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:lintsissue_agent_agent_xaction

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_6_0_ready_cons{
    io_ooo_to_mem_intIssue_6_0_ready inside {1'b0, 1'b1};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_6_0_valid_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_6_0_bits_fuType_cons{
    io_ooo_to_mem_intIssue_6_0_bits_fuType inside {36'h0002_0000, 36'h0004_0000};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_6_0_bits_fuOpType_cons{
    io_ooo_to_mem_intIssue_6_0_bits_fuOpType inside {[9'd0:9'd47]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_6_0_bits_src_0_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_6_0_bits_robIdx_value_cons{
    io_ooo_to_mem_intIssue_6_0_bits_robIdx_value inside {[9'd0:9'd351]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value_cons{
    io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value inside {[6'd0:6'd55]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_5_0_ready_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_5_0_valid_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_5_0_bits_fuType_cons{
    io_ooo_to_mem_intIssue_5_0_bits_fuType inside {36'h0002_0000, 36'h0004_0000};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_5_0_bits_fuOpType_cons{
    io_ooo_to_mem_intIssue_5_0_bits_fuOpType inside {[9'd0:9'd47]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_5_0_bits_src_0_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_5_0_bits_robIdx_value_cons{
    io_ooo_to_mem_intIssue_5_0_bits_robIdx_value inside {[9'd0:9'd351]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value_cons{
    io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value inside {[6'd0:6'd55]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_ready_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_valid_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_fuType_cons{
    io_ooo_to_mem_intIssue_4_0_bits_fuType inside {36'h0002_0000, 36'h0004_0000};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_fuOpType_cons{
    io_ooo_to_mem_intIssue_4_0_bits_fuOpType inside {[9'd0:9'd47]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_src_0_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_imm_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_robIdx_value_cons{
    io_ooo_to_mem_intIssue_4_0_bits_robIdx_value inside {[9'd0:9'd351]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_pdest_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_isRVC_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_ftqOffset_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_storeSetHit_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_ssid_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value_cons{
    io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value inside {[6'd0:6'd55]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_ready_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_valid_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_fuType_cons{
    io_ooo_to_mem_intIssue_3_0_bits_fuType inside {36'h0002_0000, 36'h0004_0000};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_fuOpType_cons{
    io_ooo_to_mem_intIssue_3_0_bits_fuOpType inside {[9'd0:9'd47]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_src_0_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_imm_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_robIdx_value_cons{
    io_ooo_to_mem_intIssue_3_0_bits_robIdx_value inside {[9'd0:9'd351]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_pdest_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_isRVC_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_ftqOffset_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_storeSetHit_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_ssid_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value_cons{
    io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value inside {[6'd0:6'd55]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_ready_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_valid_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_fuOpType_cons{
    io_ooo_to_mem_intIssue_2_0_bits_fuOpType inside {9'd0,9'd1,9'd2,9'd3,9'd4,9'd5,9'd6,9'd16,9'd17,9'd18,9'd19,9'd20,9'd21,9'd22,9'd29,9'd30};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_src_0_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_imm_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_robIdx_value_cons{
    io_ooo_to_mem_intIssue_2_0_bits_robIdx_value inside {[9'd0:9'd351]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_pdest_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_rfWen_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_fpWen_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_pc_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_isRVC_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_ftqOffset_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_storeSetHit_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value_cons{
    io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value inside {[7'd0:7'd71]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value_cons{
    io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value inside {[6'd0:6'd55]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_ready_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_valid_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_fuOpType_cons{
    io_ooo_to_mem_intIssue_1_0_bits_fuOpType inside {9'd0,9'd1,9'd2,9'd3,9'd4,9'd5,9'd6,9'd16,9'd17,9'd18,9'd19,9'd20,9'd21,9'd22,9'd29,9'd30};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_src_0_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_imm_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_robIdx_value_cons{
    io_ooo_to_mem_intIssue_1_0_bits_robIdx_value inside {[9'd0:9'd351]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_pdest_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_rfWen_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_fpWen_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_pc_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_isRVC_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_ftqOffset_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_storeSetHit_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value_cons{
    io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value inside {[7'd0:7'd71]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value_cons{
    io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value inside {[6'd0:6'd55]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_ready_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_valid_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_fuOpType_cons{
    io_ooo_to_mem_intIssue_0_0_bits_fuOpType inside {9'd0,9'd1,9'd2,9'd3,9'd4,9'd5,9'd6,9'd16,9'd17,9'd18,9'd19,9'd20,9'd21,9'd22,9'd29,9'd30};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_src_0_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_imm_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_robIdx_value_cons{
    io_ooo_to_mem_intIssue_0_0_bits_robIdx_value inside {[9'd0:9'd351]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_pdest_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_rfWen_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_fpWen_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_pc_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_isRVC_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_ftqOffset_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_storeSetHit_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value_cons{
    io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value inside {[7'd0:7'd71]};
}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag_cons{

}

constraint lintsissue_agent_agent_xaction::default_io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value_cons{
    io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value inside {[6'd0:6'd55]};
}

function lintsissue_agent_agent_xaction::new(string name = "lintsissue_agent_agent_xaction");
    super.new();
    memblock_dispatch_wait_ready = 1'b0;
    memblock_dispatch_nonblocking_issue = 1'b0;
    memblock_dispatch_ready_timeout = 1000;
    memblock_dispatch_aborted_by_redirect = 1'b0;
    memblock_dispatch_flush_epoch = 0;
    memblock_dispatch_fired_mask = '0;
endfunction:new

function void lintsissue_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void lintsissue_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void lintsissue_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void lintsissue_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string lintsissue_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_6_0_ready = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_6_0_ready);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_6_0_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_6_0_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_6_0_bits_fuType = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_6_0_bits_fuType);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_6_0_bits_fuOpType = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_6_0_bits_fuOpType);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_6_0_bits_src_0 = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_6_0_bits_src_0);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_6_0_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_6_0_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_6_0_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_5_0_ready = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_5_0_ready);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_5_0_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_5_0_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_5_0_bits_fuType = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_5_0_bits_fuType);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_5_0_bits_fuOpType = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_5_0_bits_fuOpType);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_5_0_bits_src_0 = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_5_0_bits_src_0);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_5_0_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_5_0_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_5_0_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_ready = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_ready);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_fuType = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_fuType);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_fuOpType = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_fuOpType);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_src_0 = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_src_0);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_imm = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_imm);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_isFirstIssue = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_pdest = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_pdest);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_isRVC = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_isRVC);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_ftqOffset = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_storeSetHit = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_ssid = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_ssid);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_4_0_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_ready = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_ready);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_fuType = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_fuType);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_fuOpType = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_fuOpType);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_src_0 = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_src_0);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_imm = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_imm);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_isFirstIssue = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_pdest = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_pdest);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_isRVC = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_isRVC);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_ftqOffset = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_storeSetHit = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_ssid = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_ssid);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_3_0_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_ready = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_ready);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_fuOpType = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_fuOpType);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_src_0 = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_src_0);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_imm = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_imm);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_pdest = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_pdest);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_rfWen = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_rfWen);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_fpWen = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_fpWen);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_pc = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_pc);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_isRVC = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_isRVC);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_ftqOffset = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_loadWaitBit = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_storeSetHit = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_2_0_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_ready = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_ready);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_fuOpType = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_fuOpType);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_src_0 = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_src_0);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_imm = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_imm);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_pdest = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_pdest);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_rfWen = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_rfWen);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_fpWen = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_fpWen);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_pc = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_pc);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_isRVC = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_isRVC);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_ftqOffset = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_loadWaitBit = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_storeSetHit = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_1_0_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_ready = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_ready);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_fuOpType = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_fuOpType);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_src_0 = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_src_0);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_imm = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_imm);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_pdest = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_pdest);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_rfWen = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_rfWen);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_fpWen = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_fpWen);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_pc = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_pc);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_isRVC = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_isRVC);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_ftqOffset = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_loadWaitBit = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_storeSetHit = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_intIssue_0_0_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value);
    pkt_str = $sformatf("%smemblock_dispatch_wait_ready = 0x%0h ",pkt_str,this.memblock_dispatch_wait_ready);
    pkt_str = $sformatf("%smemblock_dispatch_nonblocking_issue = 0x%0h ",pkt_str,this.memblock_dispatch_nonblocking_issue);
    pkt_str = $sformatf("%smemblock_dispatch_ready_timeout = %0d ",pkt_str,this.memblock_dispatch_ready_timeout);

    return pkt_str;
endfunction:psdisplay

function bit lintsissue_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    lintsissue_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a lintsissue_agent_agent_xaction or its extend"))
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

        if(this.io_ooo_to_mem_intIssue_6_0_ready!=rhs_.io_ooo_to_mem_intIssue_6_0_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_6_0_ready=0x%0h while the rhs_.io_ooo_to_mem_intIssue_6_0_ready=0x%0h",this.io_ooo_to_mem_intIssue_6_0_ready,rhs_.io_ooo_to_mem_intIssue_6_0_ready),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_6_0_valid!=rhs_.io_ooo_to_mem_intIssue_6_0_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_6_0_valid=0x%0h while the rhs_.io_ooo_to_mem_intIssue_6_0_valid=0x%0h",this.io_ooo_to_mem_intIssue_6_0_valid,rhs_.io_ooo_to_mem_intIssue_6_0_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_6_0_bits_fuType!=rhs_.io_ooo_to_mem_intIssue_6_0_bits_fuType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_6_0_bits_fuType=0x%0h while the rhs_.io_ooo_to_mem_intIssue_6_0_bits_fuType=0x%0h",this.io_ooo_to_mem_intIssue_6_0_bits_fuType,rhs_.io_ooo_to_mem_intIssue_6_0_bits_fuType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_6_0_bits_fuOpType!=rhs_.io_ooo_to_mem_intIssue_6_0_bits_fuOpType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_6_0_bits_fuOpType=0x%0h while the rhs_.io_ooo_to_mem_intIssue_6_0_bits_fuOpType=0x%0h",this.io_ooo_to_mem_intIssue_6_0_bits_fuOpType,rhs_.io_ooo_to_mem_intIssue_6_0_bits_fuOpType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_6_0_bits_src_0!=rhs_.io_ooo_to_mem_intIssue_6_0_bits_src_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_6_0_bits_src_0=0x%0h while the rhs_.io_ooo_to_mem_intIssue_6_0_bits_src_0=0x%0h",this.io_ooo_to_mem_intIssue_6_0_bits_src_0,rhs_.io_ooo_to_mem_intIssue_6_0_bits_src_0),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag!=rhs_.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag,rhs_.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value!=rhs_.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value,rhs_.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag,rhs_.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value!=rhs_.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value,rhs_.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_5_0_ready!=rhs_.io_ooo_to_mem_intIssue_5_0_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_5_0_ready=0x%0h while the rhs_.io_ooo_to_mem_intIssue_5_0_ready=0x%0h",this.io_ooo_to_mem_intIssue_5_0_ready,rhs_.io_ooo_to_mem_intIssue_5_0_ready),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_5_0_valid!=rhs_.io_ooo_to_mem_intIssue_5_0_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_5_0_valid=0x%0h while the rhs_.io_ooo_to_mem_intIssue_5_0_valid=0x%0h",this.io_ooo_to_mem_intIssue_5_0_valid,rhs_.io_ooo_to_mem_intIssue_5_0_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_5_0_bits_fuType!=rhs_.io_ooo_to_mem_intIssue_5_0_bits_fuType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_5_0_bits_fuType=0x%0h while the rhs_.io_ooo_to_mem_intIssue_5_0_bits_fuType=0x%0h",this.io_ooo_to_mem_intIssue_5_0_bits_fuType,rhs_.io_ooo_to_mem_intIssue_5_0_bits_fuType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_5_0_bits_fuOpType!=rhs_.io_ooo_to_mem_intIssue_5_0_bits_fuOpType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_5_0_bits_fuOpType=0x%0h while the rhs_.io_ooo_to_mem_intIssue_5_0_bits_fuOpType=0x%0h",this.io_ooo_to_mem_intIssue_5_0_bits_fuOpType,rhs_.io_ooo_to_mem_intIssue_5_0_bits_fuOpType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_5_0_bits_src_0!=rhs_.io_ooo_to_mem_intIssue_5_0_bits_src_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_5_0_bits_src_0=0x%0h while the rhs_.io_ooo_to_mem_intIssue_5_0_bits_src_0=0x%0h",this.io_ooo_to_mem_intIssue_5_0_bits_src_0,rhs_.io_ooo_to_mem_intIssue_5_0_bits_src_0),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag!=rhs_.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag,rhs_.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value!=rhs_.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value,rhs_.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag,rhs_.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value!=rhs_.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value,rhs_.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_ready!=rhs_.io_ooo_to_mem_intIssue_4_0_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_ready=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_ready=0x%0h",this.io_ooo_to_mem_intIssue_4_0_ready,rhs_.io_ooo_to_mem_intIssue_4_0_ready),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_valid!=rhs_.io_ooo_to_mem_intIssue_4_0_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_valid=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_valid=0x%0h",this.io_ooo_to_mem_intIssue_4_0_valid,rhs_.io_ooo_to_mem_intIssue_4_0_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_fuType!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_fuType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_fuType=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_fuType=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_fuType,rhs_.io_ooo_to_mem_intIssue_4_0_bits_fuType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_fuOpType!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_fuOpType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_fuOpType=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_fuOpType=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_fuOpType,rhs_.io_ooo_to_mem_intIssue_4_0_bits_fuOpType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_src_0!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_src_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_src_0=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_src_0=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_src_0,rhs_.io_ooo_to_mem_intIssue_4_0_bits_src_0),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_imm!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_imm) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_imm=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_imm=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_imm,rhs_.io_ooo_to_mem_intIssue_4_0_bits_imm),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag,rhs_.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value,rhs_.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue,rhs_.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_pdest!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_pdest) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_pdest=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_pdest=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_pdest,rhs_.io_ooo_to_mem_intIssue_4_0_bits_pdest),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_isRVC!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_isRVC) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_isRVC=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_isRVC=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_isRVC,rhs_.io_ooo_to_mem_intIssue_4_0_bits_isRVC),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag,rhs_.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value,rhs_.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset,rhs_.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit,rhs_.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_ssid!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_ssid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_ssid=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_ssid=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_ssid,rhs_.io_ooo_to_mem_intIssue_4_0_bits_ssid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag,rhs_.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value!=rhs_.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value,rhs_.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_ready!=rhs_.io_ooo_to_mem_intIssue_3_0_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_ready=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_ready=0x%0h",this.io_ooo_to_mem_intIssue_3_0_ready,rhs_.io_ooo_to_mem_intIssue_3_0_ready),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_valid!=rhs_.io_ooo_to_mem_intIssue_3_0_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_valid=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_valid=0x%0h",this.io_ooo_to_mem_intIssue_3_0_valid,rhs_.io_ooo_to_mem_intIssue_3_0_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_fuType!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_fuType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_fuType=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_fuType=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_fuType,rhs_.io_ooo_to_mem_intIssue_3_0_bits_fuType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_fuOpType!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_fuOpType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_fuOpType=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_fuOpType=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_fuOpType,rhs_.io_ooo_to_mem_intIssue_3_0_bits_fuOpType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_src_0!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_src_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_src_0=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_src_0=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_src_0,rhs_.io_ooo_to_mem_intIssue_3_0_bits_src_0),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_imm!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_imm) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_imm=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_imm=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_imm,rhs_.io_ooo_to_mem_intIssue_3_0_bits_imm),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag,rhs_.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value,rhs_.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue,rhs_.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_pdest!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_pdest) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_pdest=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_pdest=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_pdest,rhs_.io_ooo_to_mem_intIssue_3_0_bits_pdest),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_isRVC!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_isRVC) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_isRVC=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_isRVC=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_isRVC,rhs_.io_ooo_to_mem_intIssue_3_0_bits_isRVC),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag,rhs_.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value,rhs_.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset,rhs_.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit,rhs_.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_ssid!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_ssid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_ssid=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_ssid=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_ssid,rhs_.io_ooo_to_mem_intIssue_3_0_bits_ssid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag,rhs_.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value!=rhs_.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value,rhs_.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_ready!=rhs_.io_ooo_to_mem_intIssue_2_0_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_ready=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_ready=0x%0h",this.io_ooo_to_mem_intIssue_2_0_ready,rhs_.io_ooo_to_mem_intIssue_2_0_ready),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_valid!=rhs_.io_ooo_to_mem_intIssue_2_0_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_valid=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_valid=0x%0h",this.io_ooo_to_mem_intIssue_2_0_valid,rhs_.io_ooo_to_mem_intIssue_2_0_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_fuOpType!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_fuOpType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_fuOpType=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_fuOpType=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_fuOpType,rhs_.io_ooo_to_mem_intIssue_2_0_bits_fuOpType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_src_0!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_src_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_src_0=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_src_0=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_src_0,rhs_.io_ooo_to_mem_intIssue_2_0_bits_src_0),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_imm!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_imm) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_imm=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_imm=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_imm,rhs_.io_ooo_to_mem_intIssue_2_0_bits_imm),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag,rhs_.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value,rhs_.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_pdest!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_pdest) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_pdest=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_pdest=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_pdest,rhs_.io_ooo_to_mem_intIssue_2_0_bits_pdest),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_rfWen!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_rfWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_rfWen=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_rfWen=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_rfWen,rhs_.io_ooo_to_mem_intIssue_2_0_bits_rfWen),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_fpWen!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_fpWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_fpWen=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_fpWen=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_fpWen,rhs_.io_ooo_to_mem_intIssue_2_0_bits_fpWen),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_pc!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_pc) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_pc=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_pc=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_pc,rhs_.io_ooo_to_mem_intIssue_2_0_bits_pc),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_isRVC!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_isRVC) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_isRVC=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_isRVC=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_isRVC,rhs_.io_ooo_to_mem_intIssue_2_0_bits_isRVC),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag,rhs_.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value,rhs_.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset,rhs_.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit,rhs_.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag,rhs_.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value,rhs_.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit,rhs_.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict,rhs_.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag,rhs_.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value,rhs_.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag,rhs_.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value!=rhs_.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value,rhs_.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_ready!=rhs_.io_ooo_to_mem_intIssue_1_0_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_ready=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_ready=0x%0h",this.io_ooo_to_mem_intIssue_1_0_ready,rhs_.io_ooo_to_mem_intIssue_1_0_ready),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_valid!=rhs_.io_ooo_to_mem_intIssue_1_0_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_valid=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_valid=0x%0h",this.io_ooo_to_mem_intIssue_1_0_valid,rhs_.io_ooo_to_mem_intIssue_1_0_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_fuOpType!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_fuOpType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_fuOpType=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_fuOpType=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_fuOpType,rhs_.io_ooo_to_mem_intIssue_1_0_bits_fuOpType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_src_0!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_src_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_src_0=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_src_0=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_src_0,rhs_.io_ooo_to_mem_intIssue_1_0_bits_src_0),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_imm!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_imm) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_imm=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_imm=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_imm,rhs_.io_ooo_to_mem_intIssue_1_0_bits_imm),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag,rhs_.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value,rhs_.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_pdest!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_pdest) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_pdest=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_pdest=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_pdest,rhs_.io_ooo_to_mem_intIssue_1_0_bits_pdest),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_rfWen!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_rfWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_rfWen=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_rfWen=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_rfWen,rhs_.io_ooo_to_mem_intIssue_1_0_bits_rfWen),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_fpWen!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_fpWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_fpWen=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_fpWen=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_fpWen,rhs_.io_ooo_to_mem_intIssue_1_0_bits_fpWen),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_pc!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_pc) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_pc=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_pc=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_pc,rhs_.io_ooo_to_mem_intIssue_1_0_bits_pc),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_isRVC!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_isRVC) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_isRVC=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_isRVC=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_isRVC,rhs_.io_ooo_to_mem_intIssue_1_0_bits_isRVC),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag,rhs_.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value,rhs_.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset,rhs_.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit,rhs_.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag,rhs_.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value,rhs_.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit,rhs_.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict,rhs_.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag,rhs_.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value,rhs_.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag,rhs_.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value!=rhs_.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value,rhs_.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_ready!=rhs_.io_ooo_to_mem_intIssue_0_0_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_ready=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_ready=0x%0h",this.io_ooo_to_mem_intIssue_0_0_ready,rhs_.io_ooo_to_mem_intIssue_0_0_ready),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_valid!=rhs_.io_ooo_to_mem_intIssue_0_0_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_valid=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_valid=0x%0h",this.io_ooo_to_mem_intIssue_0_0_valid,rhs_.io_ooo_to_mem_intIssue_0_0_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_fuOpType!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_fuOpType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_fuOpType=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_fuOpType=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_fuOpType,rhs_.io_ooo_to_mem_intIssue_0_0_bits_fuOpType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_src_0!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_src_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_src_0=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_src_0=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_src_0,rhs_.io_ooo_to_mem_intIssue_0_0_bits_src_0),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_imm!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_imm) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_imm=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_imm=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_imm,rhs_.io_ooo_to_mem_intIssue_0_0_bits_imm),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag,rhs_.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value,rhs_.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_pdest!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_pdest) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_pdest=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_pdest=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_pdest,rhs_.io_ooo_to_mem_intIssue_0_0_bits_pdest),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_rfWen!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_rfWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_rfWen=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_rfWen=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_rfWen,rhs_.io_ooo_to_mem_intIssue_0_0_bits_rfWen),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_fpWen!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_fpWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_fpWen=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_fpWen=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_fpWen,rhs_.io_ooo_to_mem_intIssue_0_0_bits_fpWen),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_pc!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_pc) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_pc=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_pc=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_pc,rhs_.io_ooo_to_mem_intIssue_0_0_bits_pc),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_isRVC!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_isRVC) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_isRVC=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_isRVC=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_isRVC,rhs_.io_ooo_to_mem_intIssue_0_0_bits_isRVC),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag,rhs_.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value,rhs_.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset,rhs_.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit,rhs_.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag,rhs_.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value,rhs_.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit,rhs_.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict,rhs_.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag,rhs_.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value,rhs_.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag,rhs_.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value!=rhs_.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value,rhs_.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
