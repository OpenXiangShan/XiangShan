// StoreUnit V2 分层只读覆盖；2026-11-02。
// 与DTLB参考保持相同深度：module → cg_storeunit_XX_YY → cp_XX_YY_ZZZ。
// 所有covergroup在posedge采NBA更新前值；模型_d1等历史由NBA保存到下一拍。
// b_expected_result只表示该观察关系成立；部分/缺失项的逐点边界以mapping.json及分析文档为准。
// 结果不参与iff门控；偏离与未知在相邻diagnostic covergroup中单列，weight=0。
`ifndef MEMBLOCK_STOREUNIT_CORE_FCOV__SV
`define MEMBLOCK_STOREUNIT_CORE_FCOV__SV
`include "memblock_compile_params.svh"

`ifdef MEMBLOCK_UT_FCOV
`ifdef MEMBLOCK_FCOV_STORE_UNIT_EN
module memblock_storeunit_core_fcov #(parameter integer PIPE_ID=0, parameter bit HAS_CHILD=1) (
    input wire clock,
    input wire reset,
    input wire reset_backend_done,
    input wire io_redirect_valid,
    input wire io_redirect_bits_robIdx_flag,
    input wire [7:0] io_redirect_bits_robIdx_value,
    input wire io_redirect_bits_level,
    input wire io_csrCtrl_hd_misalign_st_enable,
    input wire io_stin_ready,
    input wire io_stin_valid,
    input wire [8:0] io_stin_bits_uop_fuOpType,
    input wire [31:0] io_stin_bits_uop_imm,
    input wire io_stin_bits_uop_robIdx_flag,
    input wire [7:0] io_stin_bits_uop_robIdx_value,
    input wire io_stin_bits_uop_sqIdx_flag,
    input wire [5:0] io_stin_bits_uop_sqIdx_value,
    input wire [63:0] io_stin_bits_src_0,
    input wire io_misalign_stin_ready,
    input wire io_misalign_stin_valid,
    input wire io_misalign_stin_bits_uop_exceptionVec_0,
    input wire io_misalign_stin_bits_uop_exceptionVec_1,
    input wire io_misalign_stin_bits_uop_exceptionVec_2,
    input wire io_misalign_stin_bits_uop_exceptionVec_4,
    input wire io_misalign_stin_bits_uop_exceptionVec_5,
    input wire io_misalign_stin_bits_uop_exceptionVec_8,
    input wire io_misalign_stin_bits_uop_exceptionVec_9,
    input wire io_misalign_stin_bits_uop_exceptionVec_10,
    input wire io_misalign_stin_bits_uop_exceptionVec_11,
    input wire io_misalign_stin_bits_uop_exceptionVec_12,
    input wire io_misalign_stin_bits_uop_exceptionVec_13,
    input wire io_misalign_stin_bits_uop_exceptionVec_14,
    input wire io_misalign_stin_bits_uop_exceptionVec_16,
    input wire io_misalign_stin_bits_uop_exceptionVec_17,
    input wire io_misalign_stin_bits_uop_exceptionVec_18,
    input wire io_misalign_stin_bits_uop_exceptionVec_19,
    input wire io_misalign_stin_bits_uop_exceptionVec_20,
    input wire io_misalign_stin_bits_uop_exceptionVec_21,
    input wire io_misalign_stin_bits_uop_exceptionVec_22,
    input wire [3:0] io_misalign_stin_bits_uop_trigger,
    input wire [8:0] io_misalign_stin_bits_uop_fuOpType,
    input wire [7:0] io_misalign_stin_bits_uop_vpu_vstart,
    input wire [1:0] io_misalign_stin_bits_uop_vpu_veew,
    input wire [6:0] io_misalign_stin_bits_uop_uopIdx,
    input wire io_misalign_stin_bits_uop_robIdx_flag,
    input wire [7:0] io_misalign_stin_bits_uop_robIdx_value,
    input wire io_misalign_stin_bits_uop_sqIdx_flag,
    input wire [5:0] io_misalign_stin_bits_uop_sqIdx_value,
    input wire [49:0] io_misalign_stin_bits_vaddr,
    input wire [15:0] io_misalign_stin_bits_mask,
    input wire io_misalign_stin_bits_isvec,
    input wire io_misalign_stin_bits_is128bit,
    input wire io_misalign_stin_bits_isFinalSplit,
    input wire io_misalign_stout_valid,
    input wire io_misalign_stout_bits_uop_exceptionVec_3,
    input wire io_misalign_stout_bits_uop_exceptionVec_6,
    input wire io_misalign_stout_bits_uop_exceptionVec_7,
    input wire io_misalign_stout_bits_uop_exceptionVec_15,
    input wire io_misalign_stout_bits_uop_exceptionVec_19,
    input wire io_misalign_stout_bits_uop_exceptionVec_23,
    input wire [3:0] io_misalign_stout_bits_uop_trigger,
    input wire io_misalign_stout_bits_uop_robIdx_flag,
    input wire [7:0] io_misalign_stout_bits_uop_robIdx_value,
    input wire [49:0] io_misalign_stout_bits_vaddr,
    input wire io_misalign_stout_bits_vaNeedExt,
    input wire [47:0] io_misalign_stout_bits_paddr,
    input wire [63:0] io_misalign_stout_bits_gpaddr,
    input wire io_misalign_stout_bits_nc,
    input wire io_misalign_stout_bits_mmio,
    input wire io_misalign_stout_bits_memBackTypeMM,
    input wire io_misalign_stout_bits_isForVSnonLeafPTE,
    input wire io_misalign_stout_bits_vecActive,
    input wire io_misalign_stout_bits_need_rep,
    input wire io_tlb_req_valid,
    input wire [49:0] io_tlb_req_bits_vaddr,
    input wire [63:0] io_tlb_req_bits_fullva,
    input wire io_tlb_req_bits_checkfullva,
    input wire [2:0] io_tlb_req_bits_cmd,
    input wire io_tlb_req_bits_hyperinst,
    input wire io_tlb_req_bits_frm_mabuf,
    input wire io_tlb_req_bits_debug_robIdx_flag,
    input wire [7:0] io_tlb_req_bits_debug_robIdx_value,
    input wire io_tlb_resp_valid,
    input wire [47:0] io_tlb_resp_bits_paddr_0,
    input wire [63:0] io_tlb_resp_bits_gpaddr_0,
    input wire [63:0] io_tlb_resp_bits_fullva,
    input wire [1:0] io_tlb_resp_bits_pbmt_0,
    input wire io_tlb_resp_bits_miss,
    input wire io_tlb_resp_bits_isForVSnonLeafPTE,
    input wire io_tlb_resp_bits_excp_0_vaNeedExt,
    input wire io_tlb_resp_bits_excp_0_isHyper,
    input wire io_tlb_resp_bits_excp_0_gpf_ld,
    input wire io_tlb_resp_bits_excp_0_gpf_st,
    input wire io_tlb_resp_bits_excp_0_pf_ld,
    input wire io_tlb_resp_bits_excp_0_pf_st,
    input wire io_tlb_resp_bits_excp_0_af_ld,
    input wire io_tlb_resp_bits_excp_0_af_st,
    input wire io_pmp_ld,
    input wire io_pmp_st,
    input wire io_pmp_mmio,
    input wire io_lsq_valid,
    input wire io_lsq_bits_uop_exceptionVec_0,
    input wire io_lsq_bits_uop_exceptionVec_1,
    input wire io_lsq_bits_uop_exceptionVec_2,
    input wire io_lsq_bits_uop_exceptionVec_3,
    input wire io_lsq_bits_uop_exceptionVec_4,
    input wire io_lsq_bits_uop_exceptionVec_5,
    input wire io_lsq_bits_uop_exceptionVec_6,
    input wire io_lsq_bits_uop_exceptionVec_7,
    input wire io_lsq_bits_uop_exceptionVec_8,
    input wire io_lsq_bits_uop_exceptionVec_9,
    input wire io_lsq_bits_uop_exceptionVec_10,
    input wire io_lsq_bits_uop_exceptionVec_11,
    input wire io_lsq_bits_uop_exceptionVec_12,
    input wire io_lsq_bits_uop_exceptionVec_13,
    input wire io_lsq_bits_uop_exceptionVec_14,
    input wire io_lsq_bits_uop_exceptionVec_15,
    input wire io_lsq_bits_uop_exceptionVec_16,
    input wire io_lsq_bits_uop_exceptionVec_17,
    input wire io_lsq_bits_uop_exceptionVec_18,
    input wire io_lsq_bits_uop_exceptionVec_19,
    input wire io_lsq_bits_uop_exceptionVec_20,
    input wire io_lsq_bits_uop_exceptionVec_21,
    input wire io_lsq_bits_uop_exceptionVec_22,
    input wire io_lsq_bits_uop_exceptionVec_23,
    input wire [3:0] io_lsq_bits_uop_trigger,
    input wire [8:0] io_lsq_bits_uop_fuOpType,
    input wire [6:0] io_lsq_bits_uop_uopIdx,
    input wire io_lsq_bits_uop_robIdx_flag,
    input wire [7:0] io_lsq_bits_uop_robIdx_value,
    input wire io_lsq_bits_uop_sqIdx_flag,
    input wire [5:0] io_lsq_bits_uop_sqIdx_value,
    input wire [49:0] io_lsq_bits_vaddr,
    input wire [63:0] io_lsq_bits_fullva,
    input wire io_lsq_bits_vaNeedExt,
    input wire [47:0] io_lsq_bits_paddr,
    input wire [63:0] io_lsq_bits_gpaddr,
    input wire [15:0] io_lsq_bits_mask,
    input wire io_lsq_bits_wlineflag,
    input wire io_lsq_bits_miss,
    input wire io_lsq_bits_nc,
    input wire io_lsq_bits_isHyper,
    input wire io_lsq_bits_isForVSnonLeafPTE,
    input wire io_lsq_bits_isvec,
    input wire io_lsq_bits_isFrmMisAlignBuf,
    input wire io_lsq_bits_isMisalign,
    input wire io_lsq_bits_misalignWith16Byte,
    input wire io_lsq_bits_updateAddrValid,
    input wire io_lsq_replenish_uop_exceptionVec_3,
    input wire io_lsq_replenish_uop_exceptionVec_6,
    input wire io_lsq_replenish_uop_exceptionVec_15,
    input wire io_lsq_replenish_uop_exceptionVec_19,
    input wire io_lsq_replenish_uop_exceptionVec_23,
    input wire [6:0] io_lsq_replenish_uop_uopIdx,
    input wire io_lsq_replenish_uop_robIdx_flag,
    input wire [7:0] io_lsq_replenish_uop_robIdx_value,
    input wire [63:0] io_lsq_replenish_fullva,
    input wire io_lsq_replenish_vaNeedExt,
    input wire [63:0] io_lsq_replenish_gpaddr,
    input wire io_lsq_replenish_af,
    input wire io_lsq_replenish_mmio,
    input wire io_lsq_replenish_memBackTypeMM,
    input wire io_lsq_replenish_hasException,
    input wire io_lsq_replenish_isHyper,
    input wire io_lsq_replenish_isForVSnonLeafPTE,
    input wire io_lsq_replenish_isvec,
    input wire io_lsq_replenish_updateAddrValid,
    input wire io_feedback_slow_valid,
    input wire io_feedback_slow_bits_hit,
    input wire io_feedback_slow_bits_sqIdx_flag,
    input wire [5:0] io_feedback_slow_bits_sqIdx_value,
    input wire io_stld_nuke_query_valid,
    input wire io_stld_nuke_query_bits_robIdx_flag,
    input wire [7:0] io_stld_nuke_query_bits_robIdx_value,
    input wire [47:0] io_stld_nuke_query_bits_paddr,
    input wire [15:0] io_stld_nuke_query_bits_mask,
    input wire [1:0] io_stld_nuke_query_bits_matchType,
    input wire io_stout_valid,
    input wire io_stout_bits_uop_exceptionVec_3,
    input wire io_stout_bits_uop_exceptionVec_6,
    input wire io_stout_bits_uop_exceptionVec_7,
    input wire io_stout_bits_uop_exceptionVec_15,
    input wire io_stout_bits_uop_exceptionVec_19,
    input wire io_stout_bits_uop_exceptionVec_23,
    input wire [3:0] io_stout_bits_uop_trigger,
    input wire io_stout_bits_uop_robIdx_flag,
    input wire [7:0] io_stout_bits_uop_robIdx_value,
    input wire io_stout_bits_debug_isMMIO,
    input wire io_stout_bits_debug_isNCIO,
    input wire io_vecstout_valid,
    input wire [3:0] io_vecstout_bits_mBIndex,
    input wire io_vecstout_bits_hit,
    input wire [3:0] io_vecstout_bits_trigger,
    input wire io_vecstout_bits_exceptionVec_3,
    input wire io_vecstout_bits_exceptionVec_6,
    input wire io_vecstout_bits_exceptionVec_7,
    input wire io_vecstout_bits_exceptionVec_15,
    input wire io_vecstout_bits_exceptionVec_19,
    input wire io_vecstout_bits_exceptionVec_23,
    input wire io_vecstout_bits_hasException,
    input wire [63:0] io_vecstout_bits_vaddr,
    input wire io_vecstout_bits_vaNeedExt,
    input wire [63:0] io_vecstout_bits_gpaddr,
    input wire io_vecstout_bits_isForVSnonLeafPTE,
    input wire [7:0] io_vecstout_bits_vstart,
    input wire [7:0] io_vecstout_bits_elemIdx,
    input wire [15:0] io_vecstout_bits_mask,
    input wire [4:0] io_vecstout_bits_splitIndex,
    input wire io_st_mask_out_valid,
    input wire [5:0] io_st_mask_out_bits_sqIdx_value,
    input wire [15:0] io_st_mask_out_bits_mask,
    input wire io_vecstin_ready,
    input wire io_vecstin_valid,
    input wire [63:0] io_vecstin_bits_vaddr,
    input wire [49:0] io_vecstin_bits_basevaddr,
    input wire [15:0] io_vecstin_bits_mask,
    input wire [2:0] io_vecstin_bits_alignedType,
    input wire io_vecstin_bits_vecActive,
    input wire io_vecstin_bits_vecIsFirstActiveElement,
    input wire io_vecstin_bits_uop_exceptionVec_4,
    input wire io_vecstin_bits_uop_exceptionVec_5,
    input wire io_vecstin_bits_uop_exceptionVec_6,
    input wire io_vecstin_bits_uop_exceptionVec_13,
    input wire io_vecstin_bits_uop_exceptionVec_19,
    input wire io_vecstin_bits_uop_exceptionVec_21,
    input wire [3:0] io_vecstin_bits_uop_trigger,
    input wire [8:0] io_vecstin_bits_uop_fuOpType,
    input wire [7:0] io_vecstin_bits_uop_vpu_vstart,
    input wire [1:0] io_vecstin_bits_uop_vpu_veew,
    input wire [6:0] io_vecstin_bits_uop_uopIdx,
    input wire io_vecstin_bits_uop_robIdx_flag,
    input wire [7:0] io_vecstin_bits_uop_robIdx_value,
    input wire io_vecstin_bits_uop_sqIdx_flag,
    input wire [5:0] io_vecstin_bits_uop_sqIdx_value,
    input wire [3:0] io_vecstin_bits_mBIndex,
    input wire [7:0] io_vecstin_bits_elemIdx,
    input wire [4:0] io_vecstin_bits_splitIndex,
    input wire io_misalign_enq_req_ready,
    input wire io_misalign_enq_req_valid,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_0,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_1,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_2,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_4,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_5,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_8,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_9,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_10,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_11,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_12,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_13,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_14,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_16,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_17,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_18,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_19,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_20,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_21,
    input wire io_misalign_enq_req_bits_uop_exceptionVec_22,
    input wire [3:0] io_misalign_enq_req_bits_uop_trigger,
    input wire [8:0] io_misalign_enq_req_bits_uop_fuOpType,
    input wire [7:0] io_misalign_enq_req_bits_uop_vpu_vstart,
    input wire [1:0] io_misalign_enq_req_bits_uop_vpu_veew,
    input wire [6:0] io_misalign_enq_req_bits_uop_uopIdx,
    input wire io_misalign_enq_req_bits_uop_robIdx_flag,
    input wire [7:0] io_misalign_enq_req_bits_uop_robIdx_value,
    input wire io_misalign_enq_req_bits_uop_sqIdx_flag,
    input wire [5:0] io_misalign_enq_req_bits_uop_sqIdx_value,
    input wire [49:0] io_misalign_enq_req_bits_vaddr,
    input wire [15:0] io_misalign_enq_req_bits_mask,
    input wire io_misalign_enq_req_bits_isvec,
    input wire [7:0] io_misalign_enq_req_bits_elemIdx,
    input wire [2:0] io_misalign_enq_req_bits_alignedType,
    input wire [3:0] io_misalign_enq_req_bits_mbIndex,
    input wire [4:0] io_misalign_enq_req_bits_splitIndex,
    input wire io_misalign_enq_revoke,
    input wire [1:0] io_fromCsrTrigger_tdataVec_0_matchType,
    input wire io_fromCsrTrigger_tdataVec_0_select,
    input wire io_fromCsrTrigger_tdataVec_0_timing,
    input wire [3:0] io_fromCsrTrigger_tdataVec_0_action,
    input wire io_fromCsrTrigger_tdataVec_0_chain,
    input wire io_fromCsrTrigger_tdataVec_0_store,
    input wire [63:0] io_fromCsrTrigger_tdataVec_0_tdata2,
    input wire [1:0] io_fromCsrTrigger_tdataVec_1_matchType,
    input wire io_fromCsrTrigger_tdataVec_1_select,
    input wire io_fromCsrTrigger_tdataVec_1_timing,
    input wire [3:0] io_fromCsrTrigger_tdataVec_1_action,
    input wire io_fromCsrTrigger_tdataVec_1_chain,
    input wire io_fromCsrTrigger_tdataVec_1_store,
    input wire [63:0] io_fromCsrTrigger_tdataVec_1_tdata2,
    input wire [1:0] io_fromCsrTrigger_tdataVec_2_matchType,
    input wire io_fromCsrTrigger_tdataVec_2_select,
    input wire io_fromCsrTrigger_tdataVec_2_timing,
    input wire [3:0] io_fromCsrTrigger_tdataVec_2_action,
    input wire io_fromCsrTrigger_tdataVec_2_chain,
    input wire io_fromCsrTrigger_tdataVec_2_store,
    input wire [63:0] io_fromCsrTrigger_tdataVec_2_tdata2,
    input wire [1:0] io_fromCsrTrigger_tdataVec_3_matchType,
    input wire io_fromCsrTrigger_tdataVec_3_select,
    input wire io_fromCsrTrigger_tdataVec_3_timing,
    input wire [3:0] io_fromCsrTrigger_tdataVec_3_action,
    input wire io_fromCsrTrigger_tdataVec_3_chain,
    input wire io_fromCsrTrigger_tdataVec_3_store,
    input wire [63:0] io_fromCsrTrigger_tdataVec_3_tdata2,
    input wire io_fromCsrTrigger_tEnableVec_0,
    input wire io_fromCsrTrigger_tEnableVec_1,
    input wire io_fromCsrTrigger_tEnableVec_2,
    input wire io_fromCsrTrigger_tEnableVec_3,
    input wire io_fromCsrTrigger_debugMode,
    input wire io_fromCsrTrigger_triggerCanRaiseBpExp,
    input wire io_sqCommitPtr_flag,
    input wire [5:0] io_sqCommitPtr_value,
    input wire [6:0] io_sqCommitUopIdx,
    input wire io_sqCommitRobIdx_flag,
    input wire [7:0] io_sqCommitRobIdx_value,
    input wire io_stin_ready_0,
    input wire io_st_mask_out_valid_0,
    input wire io_misalign_enq_revoke_REG,
    input wire io_misalign_enq_revoke_0,
    input wire io_feedback_slow_valid_last_REG,
    input wire io_feedback_slow_bits_r_sqIdx_flag,
    input wire [5:0] io_feedback_slow_bits_r_sqIdx_value,
    input wire io_feedback_slow_bits_hit_r,
    input wire s0_valid,
    input wire s0_out_isvec,
    input wire [49:0] s0_vaddr,
    input wire s0_isCbo,
    input wire s0_addr_aligned,
    input wire s0_rs_cross16Bytes,
    input wire s0_out_misalignWith16Byte,
    input wire [15:0] _s0_out_mask_T_14,
    input wire _s0_fire_T_1,
    input wire s1_valid,
    input wire s1_in_uop_robIdx_flag,
    input wire [7:0] s1_in_uop_robIdx_value,
    input wire s1_in_uop_sqIdx_flag,
    input wire [5:0] s1_in_uop_sqIdx_value,
    input wire [8:0] s1_in_uop_fuOpType,
    input wire [49:0] s1_in_vaddr,
    input wire [15:0] s1_in_mask,
    input wire s1_in_is128bit,
    input wire s1_in_isFrmMisAlignBuf,
    input wire s1_in_isvec,
    input wire s1_in_isMisalign,
    input wire s1_in_isFinalSplit,
    input wire s1_in_misalignWith16Byte,
    input wire s1_misalignNeedReplay,
    input wire s1_kill,
    input wire [1:0] s1_pbmt,
    input wire s1_tlb_hit,
    input wire s1_tlb_miss,
    input wire s1_toMisalignBufferValid,
    input wire s1_toMisalignBufferValid_last_REG,
    input wire s1_frm_mab_vec,
    input wire s2_valid,
    input wire s2_in_uop_robIdx_flag,
    input wire [7:0] s2_in_uop_robIdx_value,
    input wire s2_in_isvec,
    input wire s2_in_isFrmMisAlignBuf,
    input wire s2_kill,
    input wire s2_mis_align,
    input wire s2_misalignNeedReplay,
    input wire s2_mmio,
    input wire s2_exception,
    input wire s2_out_af,
    input wire s2_exception_new_vec_6,
    input wire s3_valid,
    input wire s3_kill,
    input wire s3_in_uop_robIdx_flag,
    input wire [7:0] s3_in_uop_robIdx_value,
    input wire sx_valid_1_r,
    input wire sx_in_vec_1_r,
    input wire cur_kill,
    input wire [3:0] _storeTrigger_tdataVec_io_toLoadStore_triggerAction
);
    wire sample_enable;
    reg history_valid;
    reg vector_d1;
    reg candidate_qualified2,candidate_miss2;
    wire [8:0] req_rob;
    wire [6:0] req_sq;
    wire [8:0] red_rob;
    wire selected_scalar;
    wire selected_child;
    wire [8:0] req_op;
    wire [8:0] selected_rob;
    wire [6:0] selected_sq;
    wire flush0;
    wire exp_fire;
    wire actual_fire;
    wire basic0;
    wire hsv0;
    wire cbo_nz0;
    wire cbo0;
    wire legal0;
    wire [63:0] expected_fullva0;
    wire [49:0] expected_va0;
    wire [4:0] bytes0;
    wire aligned0;
    wire cross0;
    wire within0;
    wire [15:0] basemask0;
    wire [15:0] expected_mask0;
    wire cmd0;
    wire address_ok;
    wire request_ok;
    wire maskout_ok;
    wire selection_ok;
    wire s0_ok;
    wire sqmatch0;
    wire robmatch0;
    wire uopmatch0;
    reg expected1,expected2,expected3,expected4;
    reg [8:0] op1,op2,rob1,rob2,rob3,rob4;
    reg [6:0] sq1,sq2;
    reg [49:0] va1,va2;
    reg [15:0] mask1;
    reg child1,child2,child3,child4,final1,final2,within1,within2,cross1,cross2,unaligned1,unaligned2,cbo1,cbo2,cmd1,hsv2;
    reg sqmatch1,robmatch1,uopmatch1,sqvalue_match1,qualified1,qualified2;
    reg enable_d1,enable2,ready2,candidate2,replay2,miss2,tlbhit2,strict2;
    reg [1:0] pbmt2,pbmt3,pbmt4;
    reg [3:0] action2,action3,action4;
    reg in_ex3_2,in_ex7_2,in_ex15_2,in_ex19_2,in_ex23_2;
    reg [5:0] ex3,ex4;
    reg pmammio3,pmammio4,mmio3,mmio4;
    reg s0_ok_d1,candidate_ok_d1,replay1_ok_d1,update1_ok_d1,s1_sam_d1;
    reg trace2,trace3,trace4;
    reg io_tlb_req_bits_cmd_d1;
    reg flush1_d1;
    reg [47:0] pa2;
    reg [63:0] gpa2,fullva2;
    reg ext2,hyper2,nonleaf2;
    wire younger1;
    wire flush1;
    wire flush2;
    wire flush3;
    wire flush4;
    wire miss1;
    wire hit1;
    wire [1:0] pbmt1;
    wire fault_pf1;
    wire fault_af1;
    wire fault_gpf1;
    wire exp_sam1;
    wire exp_exception1;
    wire [5:0] exp_ex1;
    wire id1_ok;
    wire addr1_ok;
    wire s1_exception_ok;
    wire exp_fb1;
    wire admit1;
    wire exp_candidate1;
    wire candidate_ok;
    wire replay1_ok;
    wire strict1;
    wire exp_update1;
    wire update1_ok;
    wire nuke_ok;
    wire s1_ok;
    wire incoming_fault2;
    wire exp_actual_uncache2;
    wire exp_all_mmio2;
    wire exp_actual_mmio2;
    wire exp_af2;
    wire exp_non_sam2;
    wire exp_sam2;
    wire [5:0] exp_ex2;
    wire exp_exception2;
    wire exp_mmio2;
    wire exp_kill2;
    wire exp_update2;
    wire s2_ok;
    wire attributes_ok;
    wire id2_ok;
    reg fb_due,fb_expected_hit;
    reg trigger_ok_d1,s1_exception_ok_d1;
    reg [8:0] fb_rob;
    reg [6:0] fb_sq;
    wire fb_ok;
    wire strict_misalign_ok;
    wire child_ok;
    wire admit2;
    reg admission_due,admission_expected;
    wire s3_admit_ok;
    wire normal2;
    wire normal_wb;
    wire wb_ok;
    reg child128_d1;
    reg [6:0] last_req_sq,last_feedback_sq;
    reg [8:0] last_req_rob;
    reg [63:0] last_base,last_fullva;
    reg [31:0] last_imm;
    reg last_scalar,blocked_d1,feedback_history,last_feedback_miss;
    reg [4:0] normal_history;
    reg reset_d1,reset_or_redirect_seen,inflight_reset_seen;
    reg [127:0] retry_valid;
    reg [8:0] retry_rob [0:127];
    reg [7:0] retry_count [0:127];
    integer k;
    wire [3:0] trig_hit,trig_canfire,trig_bp,trig_debug;
    wire [3:0] trigger_single;
    wire [3:0] trig_action_cfg_0;
    wire trig_raw_hit_0;
    wire [3:0] trig_action_cfg_1;
    wire trig_raw_hit_1;
    wire [3:0] trig_action_cfg_2;
    wire trig_raw_hit_2;
    wire [3:0] trig_action_cfg_3;
    wire trig_raw_hit_3;
    wire [49:0] trig_addr0;
    wire [1:0] trig_mode0;
    wire [3:0] trigger_expected;
    wire trigger_ok;
    reg blocked_active,blocked_consistent;
    reg [8:0] blocked_rob;
    reg [6:0] blocked_sq;
    reg [4:0] due_history,wb_relation_history;
    reg retry1_c,retry2_c,retry3_c,retry4_c;
    reg [127:0] retry_tlb_source;
    reg tlb_retry1_c,tlb_retry2_c,tlb_retry3_c,tlb_retry4_c;
    reg [8:0] recent_rob0,recent_rob1,recent_rob2,recent_rob3;
    reg [5:0] meta_change1,meta_change2;
    reg [47:0] previous_pa;
    reg [63:0] previous_gpa,previous_fullva;
    reg previous_ext,previous_hyper,previous_nonleaf,previous_response;
    wire pipeline_wb_result;
    wire [4:0] due_sequence;
    wire [4:0] result_sequence;
    wire seq_distinct;
    reg [15:0] cbo_offsets_seen;
    reg [43:0] cbo_previous_line;
    reg cbo_previous_valid,cbo_previous_result;
    reg [8:0] cbo_previous_op;
    reg scene_miss_d1,scene_replace_d1;
    reg miss_result_d1,replace_result_d1;
    wire valid_state_relation;
    reg [4:0] close_vector_history;
    reg close_reset_inflight = 1'b0;
    reg close_reset_due;
    reg close_ghost_bad,close_reset_bad;
    wire close_next_ok;
    wire close_scalar_window;
    reg close_saved_age_1_0;
    reg close_saved_age_1_1;
    reg close_saved_age_1_2;
    reg close_saved_age_1_3;
    reg close_saved_age_1_4;
    reg close_saved_age_2_0;
    reg close_saved_age_2_1;
    reg close_saved_age_2_2;
    reg close_saved_age_2_3;
    reg close_saved_age_2_4;
    reg close_saved_age_3_0;
    reg close_saved_age_3_1;
    reg close_saved_age_3_2;
    reg close_saved_age_3_3;
    reg close_saved_age_3_4;
    reg close_saved_age_4_0;
    reg close_saved_age_4_1;
    reg close_saved_age_4_2;
    reg close_saved_age_4_3;
    reg close_saved_age_4_4;
    reg close_saved_age_5_0;
    reg close_saved_age_5_1;
    reg close_saved_age_5_2;
    reg close_saved_age_5_3;
    reg close_saved_age_5_4;
    reg close_saved_age_6_0;
    reg close_saved_age_6_1;
    reg close_saved_age_6_2;
    reg close_saved_age_6_3;
    reg close_saved_age_6_4;
    reg close_saved_age_7_0;
    reg close_saved_age_7_1;
    reg close_saved_age_7_2;
    reg close_saved_age_7_3;
    reg close_saved_age_7_4;
    reg close_saved_age_8_0;
    reg close_saved_age_8_1;
    reg close_saved_age_8_2;
    reg close_saved_age_8_3;
    reg close_saved_age_8_4;
    reg close_saved_age_9_0;
    reg close_saved_age_9_1;
    reg close_saved_age_9_2;
    reg close_saved_age_9_3;
    reg close_saved_age_9_4;
    reg close_saved_age_10_0;
    reg close_saved_age_10_1;
    reg close_saved_age_10_2;
    reg close_saved_age_10_3;
    reg close_saved_age_10_4;
    reg close_flush_result_0;
    reg close_flush_result_1;
    reg close_flush_result_2;
    reg close_flush_result_3;
    reg close_flush_result_4;

    // 参考计算职责：只读取候选输入/模型历史，形成可与实际RTL比较的地址、属性与完成预期。
    // 无类库、采样分发器或unbounded事务扫描；4级流水历史和128个完整SQ索引为固定上界。
    assign sample_enable = !reset && reset_backend_done && history_valid;



    assign req_rob = {io_stin_bits_uop_robIdx_flag,io_stin_bits_uop_robIdx_value};
    assign req_sq = {io_stin_bits_uop_sqIdx_flag,io_stin_bits_uop_sqIdx_value};
    assign red_rob = {io_redirect_bits_robIdx_flag,io_redirect_bits_robIdx_value};
    assign selected_scalar = io_stin_valid && !io_vecstin_valid && !io_misalign_stin_valid;
    assign selected_child = HAS_CHILD && io_misalign_stin_valid && !io_misalign_stin_bits_isvec;
    assign req_op = selected_scalar ? io_stin_bits_uop_fuOpType : io_misalign_stin_bits_uop_fuOpType;
    assign selected_rob = io_misalign_stin_valid ? {io_misalign_stin_bits_uop_robIdx_flag,io_misalign_stin_bits_uop_robIdx_value} : io_vecstin_valid ? {io_vecstin_bits_uop_robIdx_flag,io_vecstin_bits_uop_robIdx_value} : req_rob;
    assign selected_sq = io_misalign_stin_valid ? {io_misalign_stin_bits_uop_sqIdx_flag,io_misalign_stin_bits_uop_sqIdx_value} : io_vecstin_valid ? {io_vecstin_bits_uop_sqIdx_flag,io_vecstin_bits_uop_sqIdx_value} : req_sq;
    assign flush0 = io_redirect_valid && ((selected_rob[8]^red_rob[8]^(selected_rob[7:0]>red_rob[7:0])) || (io_redirect_bits_level && selected_rob==red_rob));
    assign exp_fire = (selected_scalar || selected_child) && !flush0;
    assign actual_fire = _s0_fire_T_1;
    assign basic0 = req_op inside {0,1,2,3};
    assign hsv0 = req_op inside {16,17,18,19};
    assign cbo_nz0 = req_op inside {12,13,14};
    assign cbo0 = cbo_nz0 || req_op==7;
    assign legal0 = basic0 || hsv0 || cbo0;
    assign expected_fullva0 = selected_scalar ? io_stin_bits_src_0 + {{52{io_stin_bits_uop_imm[11]}},io_stin_bits_uop_imm[11:0]} : {14'b0,io_misalign_stin_bits_vaddr};
    assign expected_va0 = expected_fullva0[49:0];
    assign bytes0 = 5'd1 << req_op[1:0];
    assign aligned0 = cbo0 || ((expected_va0 & (bytes0-1))==0);
    assign cross0 = ({1'b0,expected_va0[3:0]} + bytes0 > 16);
    assign within0 = !aligned0 && !cross0;
    assign basemask0 = (17'd1 << bytes0)-1;
    assign expected_mask0 = selected_child ? io_misalign_stin_bits_mask : cbo0 ? 16'hffff : cross0 && !aligned0 ? basemask0 : basemask0 << expected_va0[3:0];
    assign cmd0 = !cbo_nz0;
    assign address_ok = io_tlb_req_bits_fullva==expected_fullva0 && io_tlb_req_bits_vaddr==expected_va0;
    assign request_ok = io_tlb_req_valid && address_ok && io_tlb_req_bits_cmd=={2'b0,cmd0} && io_tlb_req_bits_checkfullva==selected_scalar && io_tlb_req_bits_frm_mabuf==selected_child && io_tlb_req_bits_hyperinst==hsv0 && {io_tlb_req_bits_debug_robIdx_flag,io_tlb_req_bits_debug_robIdx_value}==selected_rob;
    assign maskout_ok = io_st_mask_out_valid==(selected_scalar || (io_vecstin_valid && !io_misalign_stin_valid)) && (!selected_scalar || (io_st_mask_out_bits_mask==expected_mask0 && io_st_mask_out_bits_sqIdx_value==req_sq[5:0]));
    assign selection_ok = io_stin_ready==selected_scalar && (io_misalign_stin_valid ? (io_misalign_stin_ready && io_tlb_req_bits_vaddr==io_misalign_stin_bits_vaddr) : io_vecstin_valid ? (io_vecstin_ready && io_tlb_req_bits_vaddr==io_vecstin_bits_vaddr[49:0]) : request_ok);
    assign s0_ok = request_ok && maskout_ok && actual_fire==exp_fire && s0_addr_aligned==aligned0 && (!cbo0 ? s0_rs_cross16Bytes==cross0 : 1'b1) && _s0_out_mask_T_14==expected_mask0;
    assign sqmatch0 = selected_sq=={io_sqCommitPtr_flag,io_sqCommitPtr_value};
    assign robmatch0 = selected_rob=={io_sqCommitRobIdx_flag,io_sqCommitRobIdx_value};
    assign uopmatch0 = (selected_scalar ? 7'b0 : io_misalign_stin_bits_uop_uopIdx)==io_sqCommitUopIdx;





















    assign younger1 = rob1[8]^red_rob[8]^(rob1[7:0]>red_rob[7:0]);
    assign flush1 = io_redirect_valid && (younger1 || (io_redirect_bits_level && rob1==red_rob));
    assign flush2 = io_redirect_valid && ((rob2[8]^red_rob[8]^(rob2[7:0]>red_rob[7:0])) || (io_redirect_bits_level && rob2==red_rob));
    assign flush3 = io_redirect_valid && ((rob3[8]^red_rob[8]^(rob3[7:0]>red_rob[7:0])) || (io_redirect_bits_level && rob3==red_rob));
    assign flush4 = io_redirect_valid && ((rob4[8]^red_rob[8]^(rob4[7:0]>red_rob[7:0])) || (io_redirect_bits_level && rob4==red_rob));
    assign miss1 = expected1 && io_tlb_resp_valid && io_tlb_resp_bits_miss;
    assign hit1 = expected1 && io_tlb_resp_valid && !io_tlb_resp_bits_miss;
    assign pbmt1 = hit1 ? io_tlb_resp_bits_pbmt_0 : 2'b0;
    assign fault_pf1 = io_tlb_resp_bits_excp_0_pf_ld || io_tlb_resp_bits_excp_0_pf_st;
    assign fault_af1 = io_tlb_resp_bits_excp_0_af_ld || io_tlb_resp_bits_excp_0_af_st;
    assign fault_gpf1 = io_tlb_resp_bits_excp_0_gpf_ld || io_tlb_resp_bits_excp_0_gpf_st;
    assign exp_sam1 = pbmt1==2 && unaligned1;
    assign exp_exception1 = fault_pf1 || fault_af1 || fault_gpf1 || exp_sam1 || trigger_expected==0;
    assign exp_ex1 = {fault_gpf1,1'b0,fault_pf1,fault_af1,exp_sam1,trigger_expected==0};
    assign id1_ok = {io_lsq_bits_uop_robIdx_flag,io_lsq_bits_uop_robIdx_value}==rob1 && {io_lsq_bits_uop_sqIdx_flag,io_lsq_bits_uop_sqIdx_value}==sq1;
    assign addr1_ok = io_lsq_valid && io_lsq_bits_paddr==io_tlb_resp_bits_paddr_0 && io_lsq_bits_gpaddr==io_tlb_resp_bits_gpaddr_0 && io_lsq_bits_fullva==(child1 ? {14'b0,va1} : io_tlb_resp_bits_fullva) && io_lsq_bits_vaNeedExt==io_tlb_resp_bits_excp_0_vaNeedExt && io_lsq_bits_isHyper==io_tlb_resp_bits_excp_0_isHyper && io_lsq_bits_isForVSnonLeafPTE==io_tlb_resp_bits_isForVSnonLeafPTE && io_lsq_bits_vaddr==va1 && io_lsq_bits_mask==mask1 && s1_in_is128bit==(child1 ? child128_d1 : within1);
    assign s1_exception_ok = {io_lsq_bits_uop_exceptionVec_23,io_lsq_bits_uop_exceptionVec_19,io_lsq_bits_uop_exceptionVec_15,io_lsq_bits_uop_exceptionVec_7,io_lsq_bits_uop_exceptionVec_6,io_lsq_bits_uop_exceptionVec_3}==exp_ex1;
    assign exp_fb1 = expected1 && !child1 && !flush1;
    assign admit1 = expected1 && !flush1 && (!miss1 || child1);
    assign exp_candidate1 = expected1 && !child1 && !cbo1 && unaligned1 && !within1 && enable_d1;
    assign candidate_ok = s1_toMisalignBufferValid==exp_candidate1 && io_misalign_enq_req_valid==(exp_candidate1 && qualified1);
    assign replay1_ok = s1_misalignNeedReplay==(cross1 && !qualified1);
    assign strict1 = expected1 && !miss1 && !cbo1 && pbmt1==0 && enable_d1 && unaligned1 && !within1 && trigger_expected==15;
    assign exp_update1 = ((!unaligned1 || within1) && (!child1 || final1)) || exp_exception1;
    assign update1_ok = io_lsq_bits_updateAddrValid==exp_update1;
    assign nuke_ok = io_stld_nuke_query_valid==(expected1 && !miss1) && (miss1 || (io_stld_nuke_query_bits_paddr==io_tlb_resp_bits_paddr_0 && io_stld_nuke_query_bits_mask==mask1 && {io_stld_nuke_query_bits_robIdx_flag,io_stld_nuke_query_bits_robIdx_value}==rob1 && io_stld_nuke_query_bits_matchType==(cbo1 ? 2'd2 : (child1 ? child128_d1 : within1) ? 2'd1 : 2'd0)));
    assign s1_ok = s1_valid==expected1 && id1_ok && addr1_ok && s1_exception_ok && trigger_ok && nuke_ok && update1_ok && candidate_ok && replay1_ok;

    assign incoming_fault2 = in_ex7_2 || in_ex15_2 || in_ex23_2;
    assign exp_actual_uncache2 = tlbhit2 && !incoming_fault2 && ((pbmt2==0 && !io_pmp_st && io_pmp_mmio) || pbmt2==1 || pbmt2==2);
    assign exp_all_mmio2 = tlbhit2 && !incoming_fault2 && ((pbmt2==0 && !io_pmp_st && io_pmp_mmio) || pbmt2==2);
    assign exp_actual_mmio2 = tlbhit2 && !incoming_fault2 && pbmt2==0 && !io_pmp_st && io_pmp_mmio;
    assign exp_af2 = in_ex7_2 || io_pmp_st || (io_pmp_ld && op2 inside {12,13,14}) || (exp_all_mmio2 && (unaligned2 || child2)) || (exp_actual_mmio2 && cbo2);
    assign exp_non_sam2 = !miss2 && (action2==1 || in_ex3_2 || exp_af2 || in_ex15_2 || in_ex19_2 || in_ex23_2);
    assign exp_sam2 = tlbhit2 && !incoming_fault2 && pbmt2==1 && (unaligned2 || child2) && !exp_non_sam2;
    assign exp_ex2 = {in_ex23_2,in_ex19_2,in_ex15_2,exp_af2,exp_sam2,in_ex3_2};
    assign exp_exception2 = !miss2 && (action2==1 || |exp_ex2);
    assign exp_mmio2 = ((pbmt2==2) || (pbmt2==0 && io_pmp_mmio)) && !miss2;
    assign exp_kill2 = (exp_mmio2 && !exp_exception2 && !child2) || flush2;
    assign exp_update2 = (!strict2 && (!child2 || final2)) || (exp_exception2 && !replay2);
    assign s2_ok = s2_valid==expected2 && s2_out_af==exp_af2 && s2_exception_new_vec_6==exp_sam2 && s2_exception==exp_exception2 && s2_kill==exp_kill2;
    assign attributes_ok = io_lsq_replenish_af==(exp_af2 && expected2 && !exp_kill2) && io_lsq_replenish_hasException==((|exp_ex2 || action2==1 || exp_af2) && expected2 && !exp_kill2) && io_lsq_replenish_mmio==((exp_mmio2 || op2 inside {12,13,14}) && !exp_exception2) && io_lsq_replenish_memBackTypeMM==!io_pmp_mmio && io_lsq_replenish_updateAddrValid==exp_update2;
    assign id2_ok = io_lsq_replenish_fullva==fullva2 && io_lsq_replenish_gpaddr==gpa2 && io_lsq_replenish_vaNeedExt==ext2 && io_lsq_replenish_isHyper==hyper2 && io_lsq_replenish_isForVSnonLeafPTE==nonleaf2 && {io_lsq_replenish_uop_robIdx_flag,io_lsq_replenish_uop_robIdx_value}==rob2;




    assign fb_ok = io_feedback_slow_valid==fb_due && (!fb_due || (io_feedback_slow_bits_hit==(fb_expected_hit && !replay2) && {io_feedback_slow_bits_sqIdx_flag,io_feedback_slow_bits_sqIdx_value}==fb_sq));
    assign strict_misalign_ok = s2_mis_align==(expected2 && strict2);
    assign child_ok = io_misalign_stout_valid && {io_misalign_stout_bits_uop_robIdx_flag,io_misalign_stout_bits_uop_robIdx_value}==rob2 && io_misalign_stout_bits_need_rep==miss2 && io_misalign_stout_bits_paddr==pa2 && io_misalign_stout_bits_gpaddr==gpa2 && io_misalign_stout_bits_vaddr==va2 && io_misalign_stout_bits_vaNeedExt==ext2 && io_misalign_stout_bits_isForVSnonLeafPTE==nonleaf2 && io_misalign_stout_bits_nc==(pbmt2==1) && io_misalign_stout_bits_mmio==(exp_mmio2 && !exp_exception2) && {io_misalign_stout_bits_uop_exceptionVec_23,io_misalign_stout_bits_uop_exceptionVec_19,io_misalign_stout_bits_uop_exceptionVec_15,io_misalign_stout_bits_uop_exceptionVec_7,io_misalign_stout_bits_uop_exceptionVec_6,io_misalign_stout_bits_uop_exceptionVec_3}==exp_ex2;
    assign admit2 = expected2 && !exp_kill2 && ((!exp_mmio2 && !cbo2) || exp_exception2) && !replay2 && (!strict2 || exp_exception2) && !child2;

    assign s3_admit_ok = !admission_due || s3_valid==admission_expected;
    assign normal2 = expected2 && !child2 && !unaligned2 && !cbo2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_ld && !io_pmp_st && !flush2;
    assign normal_wb = expected4 && !child4 && ex4==0 && action4==15;
    assign wb_ok = io_stout_valid && !io_vecstout_valid && {io_stout_bits_uop_robIdx_flag,io_stout_bits_uop_robIdx_value}==rob4 && io_stout_bits_uop_trigger==action4 && {io_stout_bits_uop_exceptionVec_23,io_stout_bits_uop_exceptionVec_19,io_stout_bits_uop_exceptionVec_15,io_stout_bits_uop_exceptionVec_7,io_stout_bits_uop_exceptionVec_6,io_stout_bits_uop_exceptionVec_3}==ex4 && io_stout_bits_debug_isMMIO==mmio4 && io_stout_bits_debug_isNCIO==(pbmt4==1 && pmammio4);












    always @(posedge clock) begin
        reset_d1 <= reset;
        if (reset) begin
            history_valid<=0;vector_d1<=0;candidate_qualified2<=0;candidate_miss2<=0;expected1<=0;expected2<=0;expected3<=0;expected4<=0;
            fb_due<=0;admission_due<=0;last_scalar<=0;blocked_d1<=0;feedback_history<=0;normal_history<=0;
            retry_valid<=0;reset_or_redirect_seen<=1;
            inflight_reset_seen<=expected1 || expected2 || expected3 || expected4;
            for (k=0;k<128;k=k+1) begin retry_rob[k]<=0;retry_count[k]<=0;end
        end else begin
            history_valid<=1;vector_d1<=io_vecstin_valid && !io_misalign_stin_valid;
            if (io_redirect_valid) begin retry_valid<=0;reset_or_redirect_seen<=1;end
            expected1<=exp_fire;expected2<=admit1;expected3<=admit2;expected4<=expected3 && !flush3;
            if (exp_fire) begin
                op1<=req_op;rob1<=selected_rob;sq1<=selected_sq;va1<=expected_va0;mask1<=expected_mask0;
                child1<=selected_child;child128_d1<=io_misalign_stin_bits_is128bit;final1<=io_misalign_stin_bits_isFinalSplit && selected_child;
                within1<=within0;cross1<=cross0;unaligned1<=!aligned0;cbo1<=cbo0;cmd1<=cmd0;
                sqmatch1<=sqmatch0;robmatch1<=robmatch0;uopmatch1<=uopmatch0;sqvalue_match1<=selected_sq[5:0]==io_sqCommitPtr_value;
                qualified1<=sqmatch0 || (robmatch0 && uopmatch0);s0_ok_d1<=s0_ok;
            end
            enable_d1<=io_csrCtrl_hd_misalign_st_enable;
            io_tlb_req_bits_cmd_d1<=io_tlb_req_bits_cmd[0];
            trigger_ok_d1<=trigger_ok;s1_exception_ok_d1<=s1_exception_ok;candidate_ok_d1<=candidate_ok;replay1_ok_d1<=replay1_ok;update1_ok_d1<=update1_ok;s1_sam_d1<=exp_sam1;flush1_d1<=flush1;
            // 每拍保留candidate/miss历史，包含普通miss在S1被kill、没有S2项的情况。
            candidate2<=exp_candidate1;candidate_qualified2<=qualified1;candidate_miss2<=miss1;fb_due<=exp_fb1;fb_expected_hit<=!miss1;fb_sq<=sq1;fb_rob<=rob1;
            if (admit1) begin
                op2<=op1;rob2<=rob1;sq2<=sq1;va2<=va1;child2<=child1;final2<=final1;within2<=within1;cross2<=cross1;
                unaligned2<=unaligned1;cbo2<=cbo1;hsv2<=op1 inside {16,17,18,19};enable2<=enable_d1;
                ready2<=io_misalign_enq_req_ready;qualified2<=qualified1;
                replay2<=exp_candidate1 && (!io_misalign_enq_req_ready || !qualified1);
                miss2<=miss1;tlbhit2<=hit1;pbmt2<=pbmt1;strict2<=strict1;action2<=trigger_expected;
                in_ex3_2<=trigger_expected==0;in_ex7_2<=fault_af1;in_ex15_2<=fault_pf1;in_ex19_2<=0;in_ex23_2<=fault_gpf1;
                pa2<=io_tlb_resp_bits_paddr_0;gpa2<=io_tlb_resp_bits_gpaddr_0;fullva2<=child1 ? {14'b0,va1} : io_tlb_resp_bits_fullva;
                ext2<=io_tlb_resp_bits_excp_0_vaNeedExt;hyper2<=io_tlb_resp_bits_excp_0_isHyper;nonleaf2<=io_tlb_resp_bits_isForVSnonLeafPTE;
                trace2<=s0_ok_d1 && s1_ok;
            end
            admission_due<=expected2;admission_expected<=admit2;
            if (admit2) begin rob3<=rob2;child3<=child2;ex3<=exp_ex2;action3<=action2;pbmt3<=pbmt2;pmammio3<=io_pmp_mmio;mmio3<=exp_mmio2 && !exp_exception2;trace3<=trace2 && s2_ok && attributes_ok && id2_ok;end
            if (expected3 && !flush3) begin rob4<=rob3;child4<=child3;ex4<=ex3;action4<=action3;pbmt4<=pbmt3;pmammio4<=pmammio3;mmio4<=mmio3;trace4<=trace3 && s3_valid;end
            last_scalar<=exp_fire && selected_scalar;blocked_d1<=io_stin_valid && !io_stin_ready;
            if (selected_scalar) begin last_req_rob<=req_rob;last_req_sq<=req_sq;last_base<=io_stin_bits_src_0;last_imm<=io_stin_bits_uop_imm;last_fullva<=io_tlb_req_bits_fullva;end
            normal_history<={normal_history[3:0],exp_fire && selected_scalar};
            feedback_history<=fb_due;
            if (fb_due) begin last_feedback_sq<=fb_sq;last_feedback_miss<=!fb_expected_hit;end
            if (exp_fb1 && miss1) begin retry_valid[sq1]<=1;retry_rob[sq1]<=rob1;retry_count[sq1]<=1;end
            if (fb_due && (!fb_expected_hit || replay2) && !io_redirect_valid) begin retry_valid[fb_sq]<=1;retry_rob[fb_sq]<=fb_rob;retry_count[fb_sq]<=retry_count[fb_sq]==255 ? 255 : retry_count[fb_sq]+1;end
            if (exp_fire && selected_scalar && retry_valid[req_sq] && retry_rob[req_sq]!=req_rob) retry_valid[req_sq]<=0;
        end
    end


    assign trigger_single = {io_fromCsrTrigger_tEnableVec_3,io_fromCsrTrigger_tEnableVec_2,io_fromCsrTrigger_tEnableVec_1,io_fromCsrTrigger_tEnableVec_0};
    assign trig_action_cfg_0 = io_fromCsrTrigger_tdataVec_0_action;
    assign trig_raw_hit_0 = cbo1 ? (va1[49:6]==io_fromCsrTrigger_tdataVec_0_tdata2[49:6] && io_fromCsrTrigger_tdataVec_0_tdata2[63:50]==0) : io_fromCsrTrigger_tdataVec_0_matchType==0 ? va1==io_fromCsrTrigger_tdataVec_0_tdata2[49:0] : io_fromCsrTrigger_tdataVec_0_matchType==2 ? va1>=io_fromCsrTrigger_tdataVec_0_tdata2[49:0] : io_fromCsrTrigger_tdataVec_0_matchType==3 && va1<io_fromCsrTrigger_tdataVec_0_tdata2[49:0];
    assign trig_hit[0] = !io_fromCsrTrigger_tdataVec_0_select && !io_fromCsrTrigger_debugMode && io_fromCsrTrigger_tEnableVec_0 && io_fromCsrTrigger_tdataVec_0_store && trig_raw_hit_0;
    assign trig_canfire[0] = trig_hit[0] && !io_fromCsrTrigger_tdataVec_0_chain && 1'b1;
    assign trig_bp[0] = trig_canfire[0] && io_fromCsrTrigger_tdataVec_0_action==0;
    assign trig_debug[0] = trig_canfire[0] && io_fromCsrTrigger_tdataVec_0_action==1;
    assign trig_action_cfg_1 = io_fromCsrTrigger_tdataVec_1_action;
    assign trig_raw_hit_1 = cbo1 ? (va1[49:6]==io_fromCsrTrigger_tdataVec_1_tdata2[49:6] && io_fromCsrTrigger_tdataVec_1_tdata2[63:50]==0) : io_fromCsrTrigger_tdataVec_1_matchType==0 ? va1==io_fromCsrTrigger_tdataVec_1_tdata2[49:0] : io_fromCsrTrigger_tdataVec_1_matchType==2 ? va1>=io_fromCsrTrigger_tdataVec_1_tdata2[49:0] : io_fromCsrTrigger_tdataVec_1_matchType==3 && va1<io_fromCsrTrigger_tdataVec_1_tdata2[49:0];
    assign trig_hit[1] = !io_fromCsrTrigger_tdataVec_1_select && !io_fromCsrTrigger_debugMode && io_fromCsrTrigger_tEnableVec_1 && io_fromCsrTrigger_tdataVec_1_store && trig_raw_hit_1;
    assign trig_canfire[1] = trig_hit[1] && !io_fromCsrTrigger_tdataVec_1_chain && (!io_fromCsrTrigger_tdataVec_0_chain || (trig_hit[0] && io_fromCsrTrigger_tdataVec_0_timing==io_fromCsrTrigger_tdataVec_1_timing));
    assign trig_bp[1] = trig_canfire[1] && io_fromCsrTrigger_tdataVec_1_action==0;
    assign trig_debug[1] = trig_canfire[1] && io_fromCsrTrigger_tdataVec_1_action==1;
    assign trig_action_cfg_2 = io_fromCsrTrigger_tdataVec_2_action;
    assign trig_raw_hit_2 = cbo1 ? (va1[49:6]==io_fromCsrTrigger_tdataVec_2_tdata2[49:6] && io_fromCsrTrigger_tdataVec_2_tdata2[63:50]==0) : io_fromCsrTrigger_tdataVec_2_matchType==0 ? va1==io_fromCsrTrigger_tdataVec_2_tdata2[49:0] : io_fromCsrTrigger_tdataVec_2_matchType==2 ? va1>=io_fromCsrTrigger_tdataVec_2_tdata2[49:0] : io_fromCsrTrigger_tdataVec_2_matchType==3 && va1<io_fromCsrTrigger_tdataVec_2_tdata2[49:0];
    assign trig_hit[2] = !io_fromCsrTrigger_tdataVec_2_select && !io_fromCsrTrigger_debugMode && io_fromCsrTrigger_tEnableVec_2 && io_fromCsrTrigger_tdataVec_2_store && trig_raw_hit_2;
    assign trig_canfire[2] = trig_hit[2] && !io_fromCsrTrigger_tdataVec_2_chain && (!io_fromCsrTrigger_tdataVec_1_chain || (trig_hit[1] && io_fromCsrTrigger_tdataVec_1_timing==io_fromCsrTrigger_tdataVec_2_timing));
    assign trig_bp[2] = trig_canfire[2] && io_fromCsrTrigger_tdataVec_2_action==0;
    assign trig_debug[2] = trig_canfire[2] && io_fromCsrTrigger_tdataVec_2_action==1;
    assign trig_action_cfg_3 = io_fromCsrTrigger_tdataVec_3_action;
    assign trig_raw_hit_3 = cbo1 ? (va1[49:6]==io_fromCsrTrigger_tdataVec_3_tdata2[49:6] && io_fromCsrTrigger_tdataVec_3_tdata2[63:50]==0) : io_fromCsrTrigger_tdataVec_3_matchType==0 ? va1==io_fromCsrTrigger_tdataVec_3_tdata2[49:0] : io_fromCsrTrigger_tdataVec_3_matchType==2 ? va1>=io_fromCsrTrigger_tdataVec_3_tdata2[49:0] : io_fromCsrTrigger_tdataVec_3_matchType==3 && va1<io_fromCsrTrigger_tdataVec_3_tdata2[49:0];
    assign trig_hit[3] = !io_fromCsrTrigger_tdataVec_3_select && !io_fromCsrTrigger_debugMode && io_fromCsrTrigger_tEnableVec_3 && io_fromCsrTrigger_tdataVec_3_store && trig_raw_hit_3;
    assign trig_canfire[3] = trig_hit[3] && !io_fromCsrTrigger_tdataVec_3_chain && (!io_fromCsrTrigger_tdataVec_2_chain || (trig_hit[2] && io_fromCsrTrigger_tdataVec_2_timing==io_fromCsrTrigger_tdataVec_3_timing));
    assign trig_bp[3] = trig_canfire[3] && io_fromCsrTrigger_tdataVec_3_action==0;
    assign trig_debug[3] = trig_canfire[3] && io_fromCsrTrigger_tdataVec_3_action==1;
    assign trig_addr0 = io_fromCsrTrigger_tdataVec_0_tdata2[49:0];
    assign trig_mode0 = io_fromCsrTrigger_tdataVec_0_matchType;
    assign trigger_expected = |trig_debug ? 4'd1 : ((|trig_bp) && io_fromCsrTrigger_triggerCanRaiseBpExp) ? 4'd0 : 4'd15;
    assign trigger_ok = _storeTrigger_tdataVec_io_toLoadStore_triggerAction==trigger_expected && io_lsq_bits_uop_trigger==trigger_expected;

    // 跨拍闭合历史：输入场景与每拍结果分开保存，结果偏离不能消除场景。












    assign pipeline_wb_result = wb_ok && trace4;
    assign due_sequence = {due_history[3:0],expected4};
    assign result_sequence = {wb_relation_history[3:0],pipeline_wb_result};
    assign seq_distinct = rob4!=recent_rob0 && rob4!=recent_rob1 && rob4!=recent_rob2 && rob4!=recent_rob3 && recent_rob0!=recent_rob1 && recent_rob1!=recent_rob2 && recent_rob2!=recent_rob3 && recent_rob0!=recent_rob2 && recent_rob0!=recent_rob3 && recent_rob1!=recent_rob3;






    assign valid_state_relation = s1_valid==(expected1 || vector_d1) && (s2_valid==expected2 || s2_in_isvec) && (s3_valid==expected3 || sx_in_vec_1_r) && (!expected4 || io_stout_valid);
    always @(posedge clock) begin
        if (reset) begin
            blocked_active<=0;blocked_consistent<=1;due_history<=0;wb_relation_history<=0;
            retry1_c<=0;retry2_c<=0;retry3_c<=0;retry4_c<=0;retry_tlb_source<=0;tlb_retry1_c<=0;tlb_retry2_c<=0;tlb_retry3_c<=0;tlb_retry4_c<=0;
            previous_response<=0;meta_change1<=0;meta_change2<=0;cbo_previous_valid<=0;cbo_offsets_seen<=0;
            scene_miss_d1<=0;scene_replace_d1<=0;
        end else begin
            due_history<={due_history[3:0],expected4};
            wb_relation_history<={wb_relation_history[3:0],pipeline_wb_result};
            recent_rob0<=rob4;recent_rob1<=recent_rob0;recent_rob2<=recent_rob1;recent_rob3<=recent_rob2;
            if (io_redirect_valid) blocked_active<=0;
            else if (io_stin_valid && !io_stin_ready) begin
                if (!blocked_active) begin blocked_active<=1;blocked_rob<=req_rob;blocked_sq<=req_sq;blocked_consistent<=1;end
                else blocked_consistent<=blocked_consistent && req_rob==blocked_rob && req_sq==blocked_sq;
            end else if (selected_scalar) blocked_active<=0;
            if(io_redirect_valid) retry_tlb_source<=0;
            else if(exp_fb1 && miss1) retry_tlb_source[sq1]<=1;
            tlb_retry1_c<=exp_fire && selected_scalar && retry_valid[req_sq] && retry_rob[req_sq]==req_rob && retry_tlb_source[req_sq];
            tlb_retry2_c<=tlb_retry1_c && admit1;tlb_retry3_c<=tlb_retry2_c && admit2;tlb_retry4_c<=tlb_retry3_c && expected3 && !flush3;
            retry1_c<=exp_fire && selected_scalar && retry_valid[req_sq] && retry_rob[req_sq]==req_rob;
            retry2_c<=retry1_c && admit1;retry3_c<=retry2_c && admit2;retry4_c<=retry3_c && expected3 && !flush3;
            meta_change1[0]<=previous_response && io_tlb_resp_bits_paddr_0!=previous_pa;
            meta_change1[1]<=previous_response && io_tlb_resp_bits_gpaddr_0!=previous_gpa;
            meta_change1[2]<=previous_response && io_tlb_resp_bits_fullva!=previous_fullva;
            meta_change1[3]<=previous_response && (io_tlb_resp_bits_excp_0_vaNeedExt!=previous_ext || io_tlb_resp_bits_excp_0_isHyper!=previous_hyper || io_tlb_resp_bits_isForVSnonLeafPTE!=previous_nonleaf);
            if (expected1 && hit1) begin
                previous_response<=1;previous_pa<=io_tlb_resp_bits_paddr_0;previous_gpa<=io_tlb_resp_bits_gpaddr_0;previous_fullva<=io_tlb_resp_bits_fullva;
                previous_ext<=io_tlb_resp_bits_excp_0_vaNeedExt;previous_hyper<=io_tlb_resp_bits_excp_0_isHyper;previous_nonleaf<=io_tlb_resp_bits_isForVSnonLeafPTE;
            end
            meta_change2[0]<=previous_response && io_tlb_resp_bits_paddr_0!=previous_pa;
            meta_change2[1]<=previous_response && io_tlb_resp_bits_gpaddr_0!=previous_gpa;
            meta_change2[2]<=previous_response && io_tlb_resp_bits_fullva!=previous_fullva;
            meta_change2[3]<=previous_response && io_tlb_resp_bits_excp_0_vaNeedExt!=previous_ext;
            meta_change2[4]<=previous_response && io_tlb_resp_bits_excp_0_isHyper!=previous_hyper;
            meta_change2[5]<=previous_response && io_tlb_resp_bits_isForVSnonLeafPTE!=previous_nonleaf;
            if (expected1 && cbo1 && !child1) begin
                cbo_previous_valid<=1;cbo_previous_line<=va1[49:6];cbo_previous_op<=op1;cbo_previous_result<=s0_ok_d1 && addr1_ok && nuke_ok;
                if (cbo_previous_valid && cbo_previous_line==va1[49:6] && cbo_previous_op==op1) cbo_offsets_seen[va1[5:2]]<=1;
                else cbo_offsets_seen<=16'b1 << va1[5:2];
            end
            scene_miss_d1<=expected1 && !child1 && miss1 && !flush1;
            miss_result_d1<=s1_kill && io_lsq_valid && io_lsq_bits_miss;
            scene_replace_d1<=expected1 && !child1 && exp_fire && selected_scalar;
            replace_result_d1<=id1_ok && addr1_ok && s0_ok;
        end
    end





    assign close_next_ok = s1_valid==expected1 && s2_valid==expected2 && s3_valid==expected3 && sx_valid_1_r==expected4 && io_stout_valid==expected4 && (!expected1 || (id1_ok && addr1_ok)) && (!expected2 || id2_ok) && (!expected3 || {s3_in_uop_robIdx_flag,s3_in_uop_robIdx_value}==rob3) && (!expected4 || wb_ok);
    assign close_scalar_window = !io_vecstin_valid && close_vector_history==0;
    always @(posedge clock) begin
        if(reset) begin
            close_vector_history<=0;close_reset_due<=1;
            if(!reset_d1) close_reset_inflight<=((expected1 || expected2 || expected3 || expected4)===1'b1);
            close_ghost_bad<=0;close_reset_bad<=0;
        end else begin
            close_vector_history<={close_vector_history[3:0],io_vecstin_valid && !io_misalign_stin_valid};
            close_reset_due<=0;
            if(history_valid && close_scalar_window && io_stout_valid && (!expected4 || !wb_ok)) begin close_ghost_bad<=1;close_reset_bad<=1;end
        end
    end

    always @(posedge clock) begin if(reset) close_saved_age_1_0<=0;else close_saved_age_1_0<=(actual_fire);end

    always @(posedge clock) begin if(reset) close_saved_age_1_1<=0;else close_saved_age_1_1<=(!s1_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_1_2<=0;else close_saved_age_1_2<=(!s2_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_1_3<=0;else close_saved_age_1_3<=(!s3_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_1_4<=0;else close_saved_age_1_4<=(!cur_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_2_0<=0;else close_saved_age_2_0<=(actual_fire);end

    always @(posedge clock) begin if(reset) close_saved_age_2_1<=0;else close_saved_age_2_1<=(!s1_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_2_2<=0;else close_saved_age_2_2<=(!s2_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_2_3<=0;else close_saved_age_2_3<=(!s3_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_2_4<=0;else close_saved_age_2_4<=(!cur_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_3_0<=0;else close_saved_age_3_0<=(actual_fire);end

    always @(posedge clock) begin if(reset) close_saved_age_3_1<=0;else close_saved_age_3_1<=(!s1_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_3_2<=0;else close_saved_age_3_2<=(!s2_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_3_3<=0;else close_saved_age_3_3<=(!s3_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_3_4<=0;else close_saved_age_3_4<=(!cur_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_4_0<=0;else close_saved_age_4_0<=(!actual_fire);end

    always @(posedge clock) begin if(reset) close_saved_age_4_1<=0;else close_saved_age_4_1<=(s1_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_4_2<=0;else close_saved_age_4_2<=(s2_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_4_3<=0;else close_saved_age_4_3<=(s3_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_4_4<=0;else close_saved_age_4_4<=(cur_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_5_0<=0;else close_saved_age_5_0<=(!actual_fire);end

    always @(posedge clock) begin if(reset) close_saved_age_5_1<=0;else close_saved_age_5_1<=(s1_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_5_2<=0;else close_saved_age_5_2<=(s2_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_5_3<=0;else close_saved_age_5_3<=(s3_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_5_4<=0;else close_saved_age_5_4<=(cur_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_6_0<=0;else close_saved_age_6_0<=(!actual_fire);end

    always @(posedge clock) begin if(reset) close_saved_age_6_1<=0;else close_saved_age_6_1<=(s1_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_6_2<=0;else close_saved_age_6_2<=(s2_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_6_3<=0;else close_saved_age_6_3<=(s3_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_6_4<=0;else close_saved_age_6_4<=(cur_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_7_0<=0;else close_saved_age_7_0<=(!actual_fire);end

    always @(posedge clock) begin if(reset) close_saved_age_7_1<=0;else close_saved_age_7_1<=(s1_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_7_2<=0;else close_saved_age_7_2<=(s2_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_7_3<=0;else close_saved_age_7_3<=(s3_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_7_4<=0;else close_saved_age_7_4<=(cur_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_8_0<=0;else close_saved_age_8_0<=(!actual_fire);end

    always @(posedge clock) begin if(reset) close_saved_age_8_1<=0;else close_saved_age_8_1<=(s1_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_8_2<=0;else close_saved_age_8_2<=(s2_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_8_3<=0;else close_saved_age_8_3<=(s3_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_8_4<=0;else close_saved_age_8_4<=(cur_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_9_0<=0;else close_saved_age_9_0<=(actual_fire);end

    always @(posedge clock) begin if(reset) close_saved_age_9_1<=0;else close_saved_age_9_1<=(!s1_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_9_2<=0;else close_saved_age_9_2<=(!s2_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_9_3<=0;else close_saved_age_9_3<=(!s3_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_9_4<=0;else close_saved_age_9_4<=(!cur_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_10_0<=0;else close_saved_age_10_0<=(actual_fire);end

    always @(posedge clock) begin if(reset) close_saved_age_10_1<=0;else close_saved_age_10_1<=(!s1_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_10_2<=0;else close_saved_age_10_2<=(!s2_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_10_3<=0;else close_saved_age_10_3<=(!s3_kill);end

    always @(posedge clock) begin if(reset) close_saved_age_10_4<=0;else close_saved_age_10_4<=(!cur_kill);end

    always @(posedge clock) begin if(reset) close_flush_result_0<=0;else close_flush_result_0<=(!actual_fire);end

    always @(posedge clock) begin if(reset) close_flush_result_1<=0;else close_flush_result_1<=(s1_kill);end

    always @(posedge clock) begin if(reset) close_flush_result_2<=0;else close_flush_result_2<=(s2_kill);end

    always @(posedge clock) begin if(reset) close_flush_result_3<=0;else close_flush_result_3<=(s3_kill);end

    always @(posedge clock) begin if(reset) close_flush_result_4<=0;else close_flush_result_4<=(cur_kill);end

    reg [1:0] scene_04_04_001_pipe; reg [1:0] scene_04_04_001_prefix;
    always @(posedge clock) begin if(reset) begin scene_04_04_001_pipe<=0;scene_04_04_001_prefix<=0;end else begin scene_04_04_001_pipe<={scene_04_04_001_pipe[0:0],(expected2 && !child2 && in_ex15_2 && io_pmp_st)};scene_04_04_001_prefix<={scene_04_04_001_prefix[0:0],(s2_ok && exp_af2 && io_lsq_replenish_af==(!flush2) && trace2 && id2_ok)};end end
    reg [1:0] scene_04_04_003_pipe; reg [1:0] scene_04_04_003_prefix;
    always @(posedge clock) begin if(reset) begin scene_04_04_003_pipe<=0;scene_04_04_003_prefix<=0;end else begin scene_04_04_003_pipe<={scene_04_04_003_pipe[0:0],(expected2 && !child2 && in_ex23_2 && io_pmp_st)};scene_04_04_003_prefix<={scene_04_04_003_prefix[0:0],(s2_ok && exp_af2 && io_lsq_replenish_af==(!flush2) && trace2 && id2_ok)};end end
    reg [1:0] scene_04_04_005_pipe; reg [1:0] scene_04_04_005_prefix;
    always @(posedge clock) begin if(reset) begin scene_04_04_005_pipe<=0;scene_04_04_005_prefix<=0;end else begin scene_04_04_005_pipe<={scene_04_04_005_pipe[0:0],(expected2 && !child2 && in_ex7_2 && io_pmp_st)};scene_04_04_005_prefix<={scene_04_04_005_prefix[0:0],(s2_ok && exp_af2 && io_lsq_replenish_af==(!flush2) && trace2 && id2_ok)};end end
    reg [1:0] scene_04_04_008_pipe; reg [1:0] scene_04_04_008_prefix;
    always @(posedge clock) begin if(reset) begin scene_04_04_008_pipe<=0;scene_04_04_008_prefix<=0;end else begin scene_04_04_008_pipe<={scene_04_04_008_pipe[0:0],(expected2 && !child2 && action2==0 && (in_ex15_2 || in_ex23_2))};scene_04_04_008_prefix<={scene_04_04_008_prefix[0:0],(s2_ok && io_lsq_replenish_uop_exceptionVec_3 && (io_lsq_replenish_uop_exceptionVec_15 || io_lsq_replenish_uop_exceptionVec_23) && trace2 && id2_ok)};end end
    reg scene_05_01_001_d1;
    always @(posedge clock) begin if (reset) scene_05_01_001_d1<=0; else scene_05_01_001_d1<=(expected1 && !child1 && cross1 && !cbo1 && enable_d1==0); end
    reg scene_05_01_002_d1;
    always @(posedge clock) begin if (reset) scene_05_01_002_d1<=0; else scene_05_01_002_d1<=(expected1 && !child1 && cross1 && !cbo1 && enable_d1==1); end
    reg scene_05_01_003_d1;
    always @(posedge clock) begin if (reset) scene_05_01_003_d1<=0; else scene_05_01_003_d1<=(expected1 && !child1 && cross1 && sqmatch1==1 && robmatch1==0 && uopmatch1==0 && enable_d1); end
    reg scene_05_01_004_d1;
    always @(posedge clock) begin if (reset) scene_05_01_004_d1<=0; else scene_05_01_004_d1<=(expected1 && !child1 && cross1 && sqmatch1==0 && robmatch1==1 && uopmatch1==1 && enable_d1); end
    reg scene_05_01_005_d1;
    always @(posedge clock) begin if (reset) scene_05_01_005_d1<=0; else scene_05_01_005_d1<=(expected1 && !child1 && cross1 && sqmatch1==0 && robmatch1==1 && uopmatch1==0 && enable_d1); end
    reg scene_05_01_006_d1;
    always @(posedge clock) begin if (reset) scene_05_01_006_d1<=0; else scene_05_01_006_d1<=(expected1 && !child1 && cross1 && sqmatch1==0 && robmatch1==0 && uopmatch1==1 && enable_d1); end
    reg scene_05_01_007_d1;
    always @(posedge clock) begin if (reset) scene_05_01_007_d1<=0; else scene_05_01_007_d1<=(expected1 && !child1 && cross1 && sqmatch1==0 && robmatch1==0 && uopmatch1==0 && enable_d1); end
    reg scene_06_02_001_d1;
    always @(posedge clock) begin if (reset) scene_06_02_001_d1<=0; else scene_06_02_001_d1<=(hit1 && !flush1 && (expected1 && !child1 && !cbo1 && !(|trig_canfire))); end
    reg scene_06_02_002_d1;
    always @(posedge clock) begin if (reset) scene_06_02_002_d1<=0; else scene_06_02_002_d1<=(hit1 && !flush1 && (expected1 && !child1 && !cbo1 && (|trig_bp) && io_fromCsrTrigger_triggerCanRaiseBpExp && !(|trig_debug))); end
    reg scene_06_02_003_d1;
    always @(posedge clock) begin if (reset) scene_06_02_003_d1<=0; else scene_06_02_003_d1<=(hit1 && !flush1 && (expected1 && !child1 && !cbo1 && (|trig_bp) && !io_fromCsrTrigger_triggerCanRaiseBpExp && !(|trig_debug))); end
    reg scene_06_02_004_d1;
    always @(posedge clock) begin if (reset) scene_06_02_004_d1<=0; else scene_06_02_004_d1<=(hit1 && !flush1 && (expected1 && !child1 && !cbo1 && |trig_debug)); end
    reg scene_06_02_005_d1;
    always @(posedge clock) begin if (reset) scene_06_02_005_d1<=0; else scene_06_02_005_d1<=(hit1 && !flush1 && (expected1 && !child1 && !cbo1 && (|trig_debug) && !io_fromCsrTrigger_triggerCanRaiseBpExp)); end
    reg scene_06_02_006_d1;
    always @(posedge clock) begin if (reset) scene_06_02_006_d1<=0; else scene_06_02_006_d1<=(hit1 && !flush1 && (expected1 && !child1 && !cbo1 && (|trig_debug) && (|trig_bp))); end
    reg [1:0] scene_08_02_001_pipe; reg [1:0] scene_08_02_001_prefix;
    always @(posedge clock) begin if(reset) begin scene_08_02_001_pipe<=0;scene_08_02_001_prefix<=0;end else begin scene_08_02_001_pipe<={scene_08_02_001_pipe[0:0],(expected2 && !flush2 && !child2 && pbmt2==0 && !io_pmp_mmio && !cbo2 && !unaligned2 && !exp_exception2)};scene_08_02_001_prefix<={scene_08_02_001_prefix[0:0],(s2_ok && attributes_ok && trace2 && id2_ok)};end end
    reg [1:0] scene_08_02_002_pipe; reg [1:0] scene_08_02_002_prefix;
    always @(posedge clock) begin if(reset) begin scene_08_02_002_pipe<=0;scene_08_02_002_prefix<=0;end else begin scene_08_02_002_pipe<={scene_08_02_002_pipe[0:0],(expected2 && !flush2 && !child2 && hsv2 && !exp_exception2 && !exp_mmio2 && !cross2)};scene_08_02_002_prefix<={scene_08_02_002_prefix[0:0],(s2_ok && attributes_ok && trace2 && id2_ok)};end end
    reg [1:0] scene_08_02_003_pipe; reg [1:0] scene_08_02_003_prefix;
    always @(posedge clock) begin if(reset) begin scene_08_02_003_pipe<=0;scene_08_02_003_prefix<=0;end else begin scene_08_02_003_pipe<={scene_08_02_003_pipe[0:0],(expected2 && !flush2 && !child2 && pbmt2==1 && !unaligned2 && !cbo2 && !exp_exception2)};scene_08_02_003_prefix<={scene_08_02_003_prefix[0:0],(s2_ok && attributes_ok && trace2 && id2_ok)};end end
    reg [1:0] scene_08_02_006_pipe; reg [1:0] scene_08_02_006_prefix;
    always @(posedge clock) begin if(reset) begin scene_08_02_006_pipe<=0;scene_08_02_006_prefix<=0;end else begin scene_08_02_006_pipe<={scene_08_02_006_pipe[0:0],(expected2 && !flush2 && !child2 && cbo2 && exp_exception2)};scene_08_02_006_prefix<={scene_08_02_006_prefix[0:0],(s2_ok && attributes_ok && trace2 && id2_ok)};end end
    reg [1:0] scene_08_02_007_pipe; reg [1:0] scene_08_02_007_prefix;
    always @(posedge clock) begin if(reset) begin scene_08_02_007_pipe<=0;scene_08_02_007_prefix<=0;end else begin scene_08_02_007_pipe<={scene_08_02_007_pipe[0:0],(expected2 && !flush2 && !child2 && exp_mmio2 && exp_exception2)};scene_08_02_007_prefix<={scene_08_02_007_prefix[0:0],(s2_ok && attributes_ok && trace2 && id2_ok)};end end
    reg scene_09_01_001_stage0_d1;
    always @(posedge clock) begin if (reset) scene_09_01_001_stage0_d1<=0; else scene_09_01_001_stage0_d1<=(selected_scalar && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && (!(selected_rob[8]^red_rob[8]^(selected_rob[7:0]>red_rob[7:0])) && selected_rob!=red_rob && selected_rob[8]==red_rob[8])); end
    reg scene_09_01_001_stage1_d1;
    always @(posedge clock) begin if (reset) scene_09_01_001_stage1_d1<=0; else scene_09_01_001_stage1_d1<=(expected1 && !child1 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && (!(rob1[8]^red_rob[8]^(rob1[7:0]>red_rob[7:0])) && rob1!=red_rob && rob1[8]==red_rob[8]) && !miss1); end
    reg scene_09_01_001_stage2_d1;
    always @(posedge clock) begin if (reset) scene_09_01_001_stage2_d1<=0; else scene_09_01_001_stage2_d1<=(expected2 && !child2 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && (!(rob2[8]^red_rob[8]^(rob2[7:0]>red_rob[7:0])) && rob2!=red_rob && rob2[8]==red_rob[8]) && !(exp_mmio2 && !exp_exception2)); end
    reg scene_09_01_001_stage3_d1;
    always @(posedge clock) begin if (reset) scene_09_01_001_stage3_d1<=0; else scene_09_01_001_stage3_d1<=(expected3 && !child3 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && (!(rob3[8]^red_rob[8]^(rob3[7:0]>red_rob[7:0])) && rob3!=red_rob && rob3[8]==red_rob[8])); end
    reg scene_09_01_001_stage4_d1;
    always @(posedge clock) begin if (reset) scene_09_01_001_stage4_d1<=0; else scene_09_01_001_stage4_d1<=(expected4 && !child4 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && (!(rob4[8]^red_rob[8]^(rob4[7:0]>red_rob[7:0])) && rob4!=red_rob && rob4[8]==red_rob[8])); end
    reg scene_09_01_002_stage0_d1;
    always @(posedge clock) begin if (reset) scene_09_01_002_stage0_d1<=0; else scene_09_01_002_stage0_d1<=(selected_scalar && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && (!(selected_rob[8]^red_rob[8]^(selected_rob[7:0]>red_rob[7:0])) && selected_rob!=red_rob && selected_rob[8]==red_rob[8])); end
    reg scene_09_01_002_stage1_d1;
    always @(posedge clock) begin if (reset) scene_09_01_002_stage1_d1<=0; else scene_09_01_002_stage1_d1<=(expected1 && !child1 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && (!(rob1[8]^red_rob[8]^(rob1[7:0]>red_rob[7:0])) && rob1!=red_rob && rob1[8]==red_rob[8]) && !miss1); end
    reg scene_09_01_002_stage2_d1;
    always @(posedge clock) begin if (reset) scene_09_01_002_stage2_d1<=0; else scene_09_01_002_stage2_d1<=(expected2 && !child2 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && (!(rob2[8]^red_rob[8]^(rob2[7:0]>red_rob[7:0])) && rob2!=red_rob && rob2[8]==red_rob[8]) && !(exp_mmio2 && !exp_exception2)); end
    reg scene_09_01_002_stage3_d1;
    always @(posedge clock) begin if (reset) scene_09_01_002_stage3_d1<=0; else scene_09_01_002_stage3_d1<=(expected3 && !child3 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && (!(rob3[8]^red_rob[8]^(rob3[7:0]>red_rob[7:0])) && rob3!=red_rob && rob3[8]==red_rob[8])); end
    reg scene_09_01_002_stage4_d1;
    always @(posedge clock) begin if (reset) scene_09_01_002_stage4_d1<=0; else scene_09_01_002_stage4_d1<=(expected4 && !child4 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && (!(rob4[8]^red_rob[8]^(rob4[7:0]>red_rob[7:0])) && rob4!=red_rob && rob4[8]==red_rob[8])); end
    reg scene_09_01_003_stage0_d1;
    always @(posedge clock) begin if (reset) scene_09_01_003_stage0_d1<=0; else scene_09_01_003_stage0_d1<=(selected_scalar && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && (selected_rob==red_rob)); end
    reg scene_09_01_003_stage1_d1;
    always @(posedge clock) begin if (reset) scene_09_01_003_stage1_d1<=0; else scene_09_01_003_stage1_d1<=(expected1 && !child1 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && (rob1==red_rob) && !miss1); end
    reg scene_09_01_003_stage2_d1;
    always @(posedge clock) begin if (reset) scene_09_01_003_stage2_d1<=0; else scene_09_01_003_stage2_d1<=(expected2 && !child2 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && (rob2==red_rob) && !(exp_mmio2 && !exp_exception2)); end
    reg scene_09_01_003_stage3_d1;
    always @(posedge clock) begin if (reset) scene_09_01_003_stage3_d1<=0; else scene_09_01_003_stage3_d1<=(expected3 && !child3 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && (rob3==red_rob)); end
    reg scene_09_01_003_stage4_d1;
    always @(posedge clock) begin if (reset) scene_09_01_003_stage4_d1<=0; else scene_09_01_003_stage4_d1<=(expected4 && !child4 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && (rob4==red_rob)); end
    reg scene_09_01_004_stage0_d1;
    always @(posedge clock) begin if (reset) scene_09_01_004_stage0_d1<=0; else scene_09_01_004_stage0_d1<=(selected_scalar && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && (selected_rob==red_rob)); end
    reg scene_09_01_004_stage1_d1;
    always @(posedge clock) begin if (reset) scene_09_01_004_stage1_d1<=0; else scene_09_01_004_stage1_d1<=(expected1 && !child1 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && (rob1==red_rob) && !miss1); end
    reg scene_09_01_004_stage2_d1;
    always @(posedge clock) begin if (reset) scene_09_01_004_stage2_d1<=0; else scene_09_01_004_stage2_d1<=(expected2 && !child2 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && (rob2==red_rob) && !(exp_mmio2 && !exp_exception2)); end
    reg scene_09_01_004_stage3_d1;
    always @(posedge clock) begin if (reset) scene_09_01_004_stage3_d1<=0; else scene_09_01_004_stage3_d1<=(expected3 && !child3 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && (rob3==red_rob)); end
    reg scene_09_01_004_stage4_d1;
    always @(posedge clock) begin if (reset) scene_09_01_004_stage4_d1<=0; else scene_09_01_004_stage4_d1<=(expected4 && !child4 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && (rob4==red_rob)); end
    reg scene_09_01_005_stage0_d1;
    always @(posedge clock) begin if (reset) scene_09_01_005_stage0_d1<=0; else scene_09_01_005_stage0_d1<=(selected_scalar && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && ((selected_rob[8]^red_rob[8]^(selected_rob[7:0]>red_rob[7:0])) && selected_rob[8]==red_rob[8])); end
    reg scene_09_01_005_stage1_d1;
    always @(posedge clock) begin if (reset) scene_09_01_005_stage1_d1<=0; else scene_09_01_005_stage1_d1<=(expected1 && !child1 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && ((rob1[8]^red_rob[8]^(rob1[7:0]>red_rob[7:0])) && rob1[8]==red_rob[8]) && !miss1); end
    reg scene_09_01_005_stage2_d1;
    always @(posedge clock) begin if (reset) scene_09_01_005_stage2_d1<=0; else scene_09_01_005_stage2_d1<=(expected2 && !child2 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && ((rob2[8]^red_rob[8]^(rob2[7:0]>red_rob[7:0])) && rob2[8]==red_rob[8]) && !(exp_mmio2 && !exp_exception2)); end
    reg scene_09_01_005_stage3_d1;
    always @(posedge clock) begin if (reset) scene_09_01_005_stage3_d1<=0; else scene_09_01_005_stage3_d1<=(expected3 && !child3 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && ((rob3[8]^red_rob[8]^(rob3[7:0]>red_rob[7:0])) && rob3[8]==red_rob[8])); end
    reg scene_09_01_005_stage4_d1;
    always @(posedge clock) begin if (reset) scene_09_01_005_stage4_d1<=0; else scene_09_01_005_stage4_d1<=(expected4 && !child4 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && ((rob4[8]^red_rob[8]^(rob4[7:0]>red_rob[7:0])) && rob4[8]==red_rob[8])); end
    reg scene_09_01_006_stage0_d1;
    always @(posedge clock) begin if (reset) scene_09_01_006_stage0_d1<=0; else scene_09_01_006_stage0_d1<=(selected_scalar && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && ((selected_rob[8]^red_rob[8]^(selected_rob[7:0]>red_rob[7:0])) && selected_rob[8]==red_rob[8])); end
    reg scene_09_01_006_stage1_d1;
    always @(posedge clock) begin if (reset) scene_09_01_006_stage1_d1<=0; else scene_09_01_006_stage1_d1<=(expected1 && !child1 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && ((rob1[8]^red_rob[8]^(rob1[7:0]>red_rob[7:0])) && rob1[8]==red_rob[8]) && !miss1); end
    reg scene_09_01_006_stage2_d1;
    always @(posedge clock) begin if (reset) scene_09_01_006_stage2_d1<=0; else scene_09_01_006_stage2_d1<=(expected2 && !child2 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && ((rob2[8]^red_rob[8]^(rob2[7:0]>red_rob[7:0])) && rob2[8]==red_rob[8]) && !(exp_mmio2 && !exp_exception2)); end
    reg scene_09_01_006_stage3_d1;
    always @(posedge clock) begin if (reset) scene_09_01_006_stage3_d1<=0; else scene_09_01_006_stage3_d1<=(expected3 && !child3 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && ((rob3[8]^red_rob[8]^(rob3[7:0]>red_rob[7:0])) && rob3[8]==red_rob[8])); end
    reg scene_09_01_006_stage4_d1;
    always @(posedge clock) begin if (reset) scene_09_01_006_stage4_d1<=0; else scene_09_01_006_stage4_d1<=(expected4 && !child4 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && ((rob4[8]^red_rob[8]^(rob4[7:0]>red_rob[7:0])) && rob4[8]==red_rob[8])); end
    reg scene_09_01_007_stage0_d1;
    always @(posedge clock) begin if (reset) scene_09_01_007_stage0_d1<=0; else scene_09_01_007_stage0_d1<=(selected_scalar && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && ((selected_rob[8]^red_rob[8]^(selected_rob[7:0]>red_rob[7:0])) && selected_rob[8]!=red_rob[8])); end
    reg scene_09_01_007_stage1_d1;
    always @(posedge clock) begin if (reset) scene_09_01_007_stage1_d1<=0; else scene_09_01_007_stage1_d1<=(expected1 && !child1 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && ((rob1[8]^red_rob[8]^(rob1[7:0]>red_rob[7:0])) && rob1[8]!=red_rob[8]) && !miss1); end
    reg scene_09_01_007_stage2_d1;
    always @(posedge clock) begin if (reset) scene_09_01_007_stage2_d1<=0; else scene_09_01_007_stage2_d1<=(expected2 && !child2 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && ((rob2[8]^red_rob[8]^(rob2[7:0]>red_rob[7:0])) && rob2[8]!=red_rob[8]) && !(exp_mmio2 && !exp_exception2)); end
    reg scene_09_01_007_stage3_d1;
    always @(posedge clock) begin if (reset) scene_09_01_007_stage3_d1<=0; else scene_09_01_007_stage3_d1<=(expected3 && !child3 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && ((rob3[8]^red_rob[8]^(rob3[7:0]>red_rob[7:0])) && rob3[8]!=red_rob[8])); end
    reg scene_09_01_007_stage4_d1;
    always @(posedge clock) begin if (reset) scene_09_01_007_stage4_d1<=0; else scene_09_01_007_stage4_d1<=(expected4 && !child4 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && ((rob4[8]^red_rob[8]^(rob4[7:0]>red_rob[7:0])) && rob4[8]!=red_rob[8])); end
    reg scene_09_01_008_stage0_d1;
    always @(posedge clock) begin if (reset) scene_09_01_008_stage0_d1<=0; else scene_09_01_008_stage0_d1<=(selected_scalar && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && ((selected_rob[8]^red_rob[8]^(selected_rob[7:0]>red_rob[7:0])) && selected_rob[8]!=red_rob[8])); end
    reg scene_09_01_008_stage1_d1;
    always @(posedge clock) begin if (reset) scene_09_01_008_stage1_d1<=0; else scene_09_01_008_stage1_d1<=(expected1 && !child1 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && ((rob1[8]^red_rob[8]^(rob1[7:0]>red_rob[7:0])) && rob1[8]!=red_rob[8]) && !miss1); end
    reg scene_09_01_008_stage2_d1;
    always @(posedge clock) begin if (reset) scene_09_01_008_stage2_d1<=0; else scene_09_01_008_stage2_d1<=(expected2 && !child2 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && ((rob2[8]^red_rob[8]^(rob2[7:0]>red_rob[7:0])) && rob2[8]!=red_rob[8]) && !(exp_mmio2 && !exp_exception2)); end
    reg scene_09_01_008_stage3_d1;
    always @(posedge clock) begin if (reset) scene_09_01_008_stage3_d1<=0; else scene_09_01_008_stage3_d1<=(expected3 && !child3 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && ((rob3[8]^red_rob[8]^(rob3[7:0]>red_rob[7:0])) && rob3[8]!=red_rob[8])); end
    reg scene_09_01_008_stage4_d1;
    always @(posedge clock) begin if (reset) scene_09_01_008_stage4_d1<=0; else scene_09_01_008_stage4_d1<=(expected4 && !child4 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && ((rob4[8]^red_rob[8]^(rob4[7:0]>red_rob[7:0])) && rob4[8]!=red_rob[8])); end
    reg scene_09_01_009_stage0_d1;
    always @(posedge clock) begin if (reset) scene_09_01_009_stage0_d1<=0; else scene_09_01_009_stage0_d1<=(selected_scalar && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && (!(selected_rob[8]^red_rob[8]^(selected_rob[7:0]>red_rob[7:0])) && selected_rob[8]!=red_rob[8])); end
    reg scene_09_01_009_stage1_d1;
    always @(posedge clock) begin if (reset) scene_09_01_009_stage1_d1<=0; else scene_09_01_009_stage1_d1<=(expected1 && !child1 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && (!(rob1[8]^red_rob[8]^(rob1[7:0]>red_rob[7:0])) && rob1[8]!=red_rob[8]) && !miss1); end
    reg scene_09_01_009_stage2_d1;
    always @(posedge clock) begin if (reset) scene_09_01_009_stage2_d1<=0; else scene_09_01_009_stage2_d1<=(expected2 && !child2 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && (!(rob2[8]^red_rob[8]^(rob2[7:0]>red_rob[7:0])) && rob2[8]!=red_rob[8]) && !(exp_mmio2 && !exp_exception2)); end
    reg scene_09_01_009_stage3_d1;
    always @(posedge clock) begin if (reset) scene_09_01_009_stage3_d1<=0; else scene_09_01_009_stage3_d1<=(expected3 && !child3 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && (!(rob3[8]^red_rob[8]^(rob3[7:0]>red_rob[7:0])) && rob3[8]!=red_rob[8])); end
    reg scene_09_01_009_stage4_d1;
    always @(posedge clock) begin if (reset) scene_09_01_009_stage4_d1<=0; else scene_09_01_009_stage4_d1<=(expected4 && !child4 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==0 && (!(rob4[8]^red_rob[8]^(rob4[7:0]>red_rob[7:0])) && rob4[8]!=red_rob[8])); end
    reg scene_09_01_010_stage0_d1;
    always @(posedge clock) begin if (reset) scene_09_01_010_stage0_d1<=0; else scene_09_01_010_stage0_d1<=(selected_scalar && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && (!(selected_rob[8]^red_rob[8]^(selected_rob[7:0]>red_rob[7:0])) && selected_rob[8]!=red_rob[8])); end
    reg scene_09_01_010_stage1_d1;
    always @(posedge clock) begin if (reset) scene_09_01_010_stage1_d1<=0; else scene_09_01_010_stage1_d1<=(expected1 && !child1 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && (!(rob1[8]^red_rob[8]^(rob1[7:0]>red_rob[7:0])) && rob1[8]!=red_rob[8]) && !miss1); end
    reg scene_09_01_010_stage2_d1;
    always @(posedge clock) begin if (reset) scene_09_01_010_stage2_d1<=0; else scene_09_01_010_stage2_d1<=(expected2 && !child2 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && (!(rob2[8]^red_rob[8]^(rob2[7:0]>red_rob[7:0])) && rob2[8]!=red_rob[8]) && !(exp_mmio2 && !exp_exception2)); end
    reg scene_09_01_010_stage3_d1;
    always @(posedge clock) begin if (reset) scene_09_01_010_stage3_d1<=0; else scene_09_01_010_stage3_d1<=(expected3 && !child3 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && (!(rob3[8]^red_rob[8]^(rob3[7:0]>red_rob[7:0])) && rob3[8]!=red_rob[8])); end
    reg scene_09_01_010_stage4_d1;
    always @(posedge clock) begin if (reset) scene_09_01_010_stage4_d1<=0; else scene_09_01_010_stage4_d1<=(expected4 && !child4 && close_scalar_window && io_redirect_valid && io_redirect_bits_level==1 && (!(rob4[8]^red_rob[8]^(rob4[7:0]>red_rob[7:0])) && rob4[8]!=red_rob[8])); end
    reg scene_09_02_001_d1;
    always @(posedge clock) begin if (reset) scene_09_02_001_d1<=0; else scene_09_02_001_d1<=(selected_scalar && flush0 && close_scalar_window); end
    reg scene_09_02_002_d1;
    always @(posedge clock) begin if (reset) scene_09_02_002_d1<=0; else scene_09_02_002_d1<=(expected1 && !child1 && flush1 && close_scalar_window); end
    reg scene_09_02_003_d1;
    always @(posedge clock) begin if (reset) scene_09_02_003_d1<=0; else scene_09_02_003_d1<=(expected2 && !child2 && flush2 && close_scalar_window); end
    reg scene_09_02_004_d1;
    always @(posedge clock) begin if (reset) scene_09_02_004_d1<=0; else scene_09_02_004_d1<=(expected3 && !child3 && flush3 && close_scalar_window); end
    reg scene_09_02_005_d1;
    always @(posedge clock) begin if (reset) scene_09_02_005_d1<=0; else scene_09_02_005_d1<=(expected4 && !child4 && flush4 && close_scalar_window); end
    reg scene_09_02_008_stage1_d1;
    always @(posedge clock) begin if (reset) scene_09_02_008_stage1_d1<=0; else scene_09_02_008_stage1_d1<=(expected1 && !child1 && flush1 && close_scalar_window && exp_fire); end
    reg scene_09_02_008_stage2_d1;
    always @(posedge clock) begin if (reset) scene_09_02_008_stage2_d1<=0; else scene_09_02_008_stage2_d1<=(expected2 && !child2 && flush2 && close_scalar_window && admit1); end
    reg scene_09_02_008_stage3_d1;
    always @(posedge clock) begin if (reset) scene_09_02_008_stage3_d1<=0; else scene_09_02_008_stage3_d1<=(expected3 && !child3 && flush3 && close_scalar_window && admit2); end
    reg scene_09_02_008_stage4_d1;
    always @(posedge clock) begin if (reset) scene_09_02_008_stage4_d1<=0; else scene_09_02_008_stage4_d1<=(expected4 && !child4 && flush4 && close_scalar_window && expected3 && !flush3); end

    // 01.01 标量入口与共享优先级
    covergroup cg_storeunit_01_01 @(posedge clock);
        option.per_instance = 1;
        // 01.01.001 ARB_标量与MAB0向量0
        // 场景：R.valid=1，M.valid=0，V.valid=0，各来源ROB不被redirect取消；按真实连接展开；MAB优先于向量，向量优先于普通标量；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：选择标量；R.ready=1；地址、op和ROB来自选中来源，未选中的标量不建立流水token。
        // 审计结论：完全建模。
        cp_01_01_001 : coverpoint ($isunknown((selection_ok)) ? 2'd2 : ((selection_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (io_stin_valid && io_misalign_stin_valid==1'b0 && io_vecstin_valid==1'b0 && !flush0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.01.002 ARB_标量与MAB0向量1
        // 场景：R.valid=1，M.valid=0，V.valid=1，各来源ROB不被redirect取消；按真实连接展开；MAB优先于向量，向量优先于普通标量；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：选择向量；R.ready=0；地址、op和ROB来自选中来源，未选中的标量不建立流水token。
        // 审计结论：完全建模。
        cp_01_01_002 : coverpoint ($isunknown((selection_ok)) ? 2'd2 : ((selection_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (io_stin_valid && io_misalign_stin_valid==1'b0 && io_vecstin_valid==1'b1 && !flush0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.01.003 ARB_标量与MAB1向量0
        // 场景：R.valid=1，M.valid=1，V.valid=0，各来源ROB不被redirect取消；按真实连接展开；MAB优先于向量，向量优先于普通标量；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：选择MAB；R.ready=0；地址、op和ROB来自选中来源，未选中的标量不建立流水token。
        // 审计结论：完全建模。
        cp_01_01_003 : coverpoint ($isunknown((selection_ok)) ? 2'd2 : ((selection_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (io_stin_valid && io_misalign_stin_valid==1'b1 && io_vecstin_valid==1'b0 && !flush0)) {
            option.weight = HAS_CHILD ? 1 : 0;
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.01.004 ARB_标量与MAB1向量1
        // 场景：R.valid=1，M.valid=1，V.valid=1，各来源ROB不被redirect取消；按真实连接展开；MAB优先于向量，向量优先于普通标量；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：选择MAB；R.ready=0；地址、op和ROB来自选中来源，未选中的标量不建立流水token。
        // 审计结论：完全建模。
        cp_01_01_004 : coverpoint ($isunknown((selection_ok)) ? 2'd2 : ((selection_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (io_stin_valid && io_misalign_stin_valid==1'b1 && io_vecstin_valid==1'b1 && !flush0)) {
            option.weight = HAS_CHILD ? 1 : 0;
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.01.005 ARB_阻挡后标量获选
        // 场景：标量被向量或MAB阻挡一拍后竞争来源撤销，标量请求继续保持有效；按真实连接展开；MAB优先于向量，向量优先于普通标量；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：阻挡拍无标量推进；恢复拍标量被选择且只建立一次新attempt。
        // 审计结论：完全建模。
        cp_01_01_005 : coverpoint ($isunknown((blocked_consistent && req_rob==blocked_rob && req_sq==blocked_sq && selection_ok && s0_ok)) ? 2'd2 : ((blocked_consistent && req_rob==blocked_rob && req_sq==blocked_sq && selection_ok && s0_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (blocked_active && selected_scalar && exp_fire)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.01.006 ARB_无请求空闲
        // 场景：全部入口valid=0；按真实连接展开；MAB优先于向量，向量优先于普通标量；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：s0_valid/TQ.valid/st_mask_out.valid均为0；payload任意值不形成事务。
        // 审计结论：完全建模。
        cp_01_01_006 : coverpoint ($isunknown((!s0_valid && !io_tlb_req_valid && !io_st_mask_out_valid)) ? 2'd2 : ((!s0_valid && !io_tlb_req_valid && !io_st_mask_out_valid) ? 2'd1 : 2'd0))
            iff (sample_enable && (!io_stin_valid && !io_vecstin_valid && !io_misalign_stin_valid)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_01_01 cg_storeunit_01_01_inst;
    covergroup cg_storeunit_diag_01_01 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_01_01_001_result : coverpoint ($isunknown((selection_ok)) ? 2'd2 : ((selection_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (io_stin_valid && io_misalign_stin_valid==1'b0 && io_vecstin_valid==1'b0 && !flush0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_01_002_result : coverpoint ($isunknown((selection_ok)) ? 2'd2 : ((selection_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (io_stin_valid && io_misalign_stin_valid==1'b0 && io_vecstin_valid==1'b1 && !flush0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_01_003_result : coverpoint ($isunknown((selection_ok)) ? 2'd2 : ((selection_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (io_stin_valid && io_misalign_stin_valid==1'b1 && io_vecstin_valid==1'b0 && !flush0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_01_004_result : coverpoint ($isunknown((selection_ok)) ? 2'd2 : ((selection_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (io_stin_valid && io_misalign_stin_valid==1'b1 && io_vecstin_valid==1'b1 && !flush0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_01_005_result : coverpoint ($isunknown((blocked_consistent && req_rob==blocked_rob && req_sq==blocked_sq && selection_ok && s0_ok)) ? 2'd2 : ((blocked_consistent && req_rob==blocked_rob && req_sq==blocked_sq && selection_ok && s0_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (blocked_active && selected_scalar && exp_fire)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_01_006_result : coverpoint ($isunknown((!s0_valid && !io_tlb_req_valid && !io_st_mask_out_valid)) ? 2'd2 : ((!s0_valid && !io_tlb_req_valid && !io_st_mask_out_valid) ? 2'd1 : 2'd0))
            iff (sample_enable && (!io_stin_valid && !io_vecstin_valid && !io_misalign_stin_valid)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_01_01 cg_storeunit_diag_01_01_inst;

    // 01.02 连续推进与空泡
    covergroup cg_storeunit_01_02 @(posedge clock);
        option.per_instance = 1;
        // 01.02.001 PIPE_单条请求
        // 场景：以有效位模式1发送不同ROB/SQ的正常标量请求；hit/miss按叶点分别设置；无redirect、异常或MAB处理；当前RTL无stout.ready端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：每个被接受的token保持顺序；正常请求在其C1/C2/C3/C4分别出现对应阶段结果；空泡不复制前一条结果。
        // 审计结论：完全建模。
        cp_01_02_001 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && normal_history[0:0]==1'b1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.02.002 PIPE_相邻两拍
        // 场景：以有效位模式11发送不同ROB/SQ的正常标量请求；hit/miss按叶点分别设置；无redirect、异常或MAB处理；当前RTL无stout.ready端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：每个被接受的token保持顺序；正常请求在其C1/C2/C3/C4分别出现对应阶段结果；空泡不复制前一条结果。
        // 审计结论：完全建模。
        cp_01_02_002 : coverpoint ($isunknown(((result_sequence[1:0] & 2'b11)==2'b11)) ? 2'd2 : (((result_sequence[1:0] & 2'b11)==2'b11) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && due_sequence[1:0]==2'b11)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.02.003 PIPE_中间一个空泡
        // 场景：以有效位模式101发送不同ROB/SQ的正常标量请求；hit/miss按叶点分别设置；无redirect、异常或MAB处理；当前RTL无stout.ready端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：每个被接受的token保持顺序；正常请求在其C1/C2/C3/C4分别出现对应阶段结果；空泡不复制前一条结果。
        // 审计结论：完全建模。
        cp_01_02_003 : coverpoint ($isunknown(((result_sequence[2:0] & 3'b101)==3'b101)) ? 2'd2 : (((result_sequence[2:0] & 3'b101)==3'b101) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && due_sequence[2:0]==3'b101)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.02.004 PIPE_中间两个空泡
        // 场景：以有效位模式1001发送不同ROB/SQ的正常标量请求；hit/miss按叶点分别设置；无redirect、异常或MAB处理；当前RTL无stout.ready端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：每个被接受的token保持顺序；正常请求在其C1/C2/C3/C4分别出现对应阶段结果；空泡不复制前一条结果。
        // 审计结论：完全建模。
        cp_01_02_004 : coverpoint ($isunknown(((result_sequence[3:0] & 4'b1001)==4'b1001)) ? 2'd2 : (((result_sequence[3:0] & 4'b1001)==4'b1001) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && due_sequence[3:0]==4'b1001)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.02.005 PIPE_五拍连续流
        // 场景：以有效位模式11111发送不同ROB/SQ的正常标量请求；hit/miss按叶点分别设置；无redirect、异常或MAB处理；当前RTL无stout.ready端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：每个被接受的token保持顺序；正常请求在其C1/C2/C3/C4分别出现对应阶段结果；空泡不复制前一条结果。
        // 审计结论：完全建模。
        cp_01_02_005 : coverpoint ($isunknown(((result_sequence[4:0] & 5'b11111)==5'b11111)) ? 2'd2 : (((result_sequence[4:0] & 5'b11111)==5'b11111) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && due_sequence[4:0]==5'b11111)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.02.006 PIPE_新输入覆盖已完成项
        // 场景：本拍S1旧请求完成且S0新请求推进；hit/miss按叶点分别设置；无redirect、异常或MAB处理；当前RTL无stout.ready端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：下一拍S1保持有效并属于新请求；新输入优先，不误清有效位。
        // 审计结论：完全建模。
        cp_01_02_006 : coverpoint ($isunknown((replace_result_d1 && expected1 && s1_valid && id1_ok && addr1_ok)) ? 2'd2 : ((replace_result_d1 && expected1 && s1_valid && id1_ok && addr1_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_replace_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.02.007 PIPE_TLBmiss后紧接新请求
        // 场景：旧标量S1发生TLB miss，同时新请求在S0接受；hit/miss按叶点分别设置；无redirect、异常或MAB处理；当前RTL无stout.ready端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：旧项被S1 kill并生成慢反馈；新项按自己的响应推进，旧miss不污染新项。
        // 审计结论：完全建模。
        cp_01_02_007 : coverpoint ($isunknown((miss_result_d1 && fb_ok && !io_feedback_slow_bits_hit && s1_valid && id1_ok && addr1_ok)) ? 2'd2 : ((miss_result_d1 && fb_ok && !io_feedback_slow_bits_hit && s1_valid && id1_ok && addr1_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_miss_d1 && expected1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_01_02 cg_storeunit_01_02_inst;
    covergroup cg_storeunit_diag_01_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_01_02_001_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && normal_history[0:0]==1'b1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_02_002_result : coverpoint ($isunknown(((result_sequence[1:0] & 2'b11)==2'b11)) ? 2'd2 : (((result_sequence[1:0] & 2'b11)==2'b11) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && due_sequence[1:0]==2'b11)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_02_003_result : coverpoint ($isunknown(((result_sequence[2:0] & 3'b101)==3'b101)) ? 2'd2 : (((result_sequence[2:0] & 3'b101)==3'b101) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && due_sequence[2:0]==3'b101)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_02_004_result : coverpoint ($isunknown(((result_sequence[3:0] & 4'b1001)==4'b1001)) ? 2'd2 : (((result_sequence[3:0] & 4'b1001)==4'b1001) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && due_sequence[3:0]==4'b1001)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_02_005_result : coverpoint ($isunknown(((result_sequence[4:0] & 5'b11111)==5'b11111)) ? 2'd2 : (((result_sequence[4:0] & 5'b11111)==5'b11111) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && due_sequence[4:0]==5'b11111)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_02_006_result : coverpoint ($isunknown((replace_result_d1 && expected1 && s1_valid && id1_ok && addr1_ok)) ? 2'd2 : ((replace_result_d1 && expected1 && s1_valid && id1_ok && addr1_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_replace_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_02_007_result : coverpoint ($isunknown((miss_result_d1 && fb_ok && !io_feedback_slow_bits_hit && s1_valid && id1_ok && addr1_ok)) ? 2'd2 : ((miss_result_d1 && fb_ok && !io_feedback_slow_bits_hit && s1_valid && id1_ok && addr1_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_miss_d1 && expected1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_01_02 cg_storeunit_diag_01_02_inst;

    // 01.03 事务身份与指针边界
    covergroup cg_storeunit_01_03 @(posedge clock);
        option.per_instance = 1;
        // 01.03.001 ID_ROB_flag0_value0
        // 场景：标量ROB={flag=0,value=0}，其余字段合法；使用{flag,value}完整指针；同ROB可有不同attempt/child；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：DTLB debug ROB、LSQ、nuke及普通写回ROB与所选输入一致；不比较已裁剪的慢反馈ROB字段。
        // 审计结论：完全建模。
        cp_01_03_001 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && rob4==9'd0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.03.002 ID_ROB_flag0_value127
        // 场景：标量ROB={flag=0,value=127}，其余字段合法；使用{flag,value}完整指针；同ROB可有不同attempt/child；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：DTLB debug ROB、LSQ、nuke及普通写回ROB与所选输入一致；不比较已裁剪的慢反馈ROB字段。
        // 审计结论：完全建模。
        cp_01_03_002 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && rob4==9'd127)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.03.003 ID_ROB_flag0_value255
        // 场景：标量ROB={flag=0,value=255}，其余字段合法；使用{flag,value}完整指针；同ROB可有不同attempt/child；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：DTLB debug ROB、LSQ、nuke及普通写回ROB与所选输入一致；不比较已裁剪的慢反馈ROB字段。
        // 审计结论：完全建模。
        cp_01_03_003 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && rob4==9'd255)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.03.004 ID_ROB_flag1_value0
        // 场景：标量ROB={flag=1,value=0}，其余字段合法；使用{flag,value}完整指针；同ROB可有不同attempt/child；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：DTLB debug ROB、LSQ、nuke及普通写回ROB与所选输入一致；不比较已裁剪的慢反馈ROB字段。
        // 审计结论：完全建模。
        cp_01_03_004 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && rob4==9'd256)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.03.005 ID_ROB_flag1_value127
        // 场景：标量ROB={flag=1,value=127}，其余字段合法；使用{flag,value}完整指针；同ROB可有不同attempt/child；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：DTLB debug ROB、LSQ、nuke及普通写回ROB与所选输入一致；不比较已裁剪的慢反馈ROB字段。
        // 审计结论：完全建模。
        cp_01_03_005 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && rob4==9'd383)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.03.006 ID_ROB_flag1_value255
        // 场景：标量ROB={flag=1,value=255}，其余字段合法；使用{flag,value}完整指针；同ROB可有不同attempt/child；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：DTLB debug ROB、LSQ、nuke及普通写回ROB与所选输入一致；不比较已裁剪的慢反馈ROB字段。
        // 审计结论：完全建模。
        cp_01_03_006 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && rob4==9'd511)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.03.007 ID_SQ_flag0_value0
        // 场景：标量SQ={flag=0,value=0}；使用{flag,value}完整指针；同ROB可有不同attempt/child；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：S0 mask只有SQ value；LSQ和慢反馈保留实际存在的flag/value；不凭value区分跨轮次事务。
        // 审计结论：完全建模。
        cp_01_03_007 : coverpoint ($isunknown((fb_ok && trace2 && id2_ok)) ? 2'd2 : ((fb_ok && trace2 && id2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && sq2==7'd0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.03.008 ID_SQ_flag0_value63
        // 场景：标量SQ={flag=0,value=63}；使用{flag,value}完整指针；同ROB可有不同attempt/child；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：S0 mask只有SQ value；LSQ和慢反馈保留实际存在的flag/value；不凭value区分跨轮次事务。
        // 审计结论：方案需修订。当前StoreQueue物理深度56，合法value为0—55；原测试点value=63与当前集成能力冲突，仅记录模块输入诊断，不进入正向分母。
        cp_01_03_008 : coverpoint ($isunknown((fb_ok && trace2 && id2_ok)) ? 2'd2 : ((fb_ok && trace2 && id2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && sq2==7'd63)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 01.03.009 ID_SQ_flag1_value0
        // 场景：标量SQ={flag=1,value=0}；使用{flag,value}完整指针；同ROB可有不同attempt/child；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：S0 mask只有SQ value；LSQ和慢反馈保留实际存在的flag/value；不凭value区分跨轮次事务。
        // 审计结论：完全建模。
        cp_01_03_009 : coverpoint ($isunknown((fb_ok && trace2 && id2_ok)) ? 2'd2 : ((fb_ok && trace2 && id2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && sq2==7'd64)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 01.03.010 ID_SQ_flag1_value63
        // 场景：标量SQ={flag=1,value=63}；使用{flag,value}完整指针；同ROB可有不同attempt/child；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：S0 mask只有SQ value；LSQ和慢反馈保留实际存在的flag/value；不凭value区分跨轮次事务。
        // 审计结论：方案需修订。当前StoreQueue物理深度56，合法value为0—55；原测试点value=63与当前集成能力冲突，仅记录模块输入诊断，不进入正向分母。
        cp_01_03_010 : coverpoint ($isunknown((fb_ok && trace2 && id2_ok)) ? 2'd2 : ((fb_ok && trace2 && id2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && sq2==7'd127)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 01.03.011 ID_相同ROB再次发射
        // 场景：同一ROB/SQ的原始标量经TLB或MAB资格失败后再次发射；使用{flag,value}完整指针；同ROB可有不同attempt/child；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：重发建立新的attempt；不能将旧响应或旧写回算作新一次完成。
        // 审计结论：完全建模。
        cp_01_03_011 : coverpoint ($isunknown((pipeline_wb_result)) ? 2'd2 : ((pipeline_wb_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && retry4_c)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_01_03 cg_storeunit_01_03_inst;
    covergroup cg_storeunit_diag_01_03 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_01_03_001_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && rob4==9'd0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_03_002_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && rob4==9'd127)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_03_003_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && rob4==9'd255)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_03_004_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && rob4==9'd256)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_03_005_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && rob4==9'd383)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_03_006_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && rob4==9'd511)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_03_007_result : coverpoint ($isunknown((fb_ok && trace2 && id2_ok)) ? 2'd2 : ((fb_ok && trace2 && id2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && sq2==7'd0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_03_008_result : coverpoint ($isunknown((fb_ok && trace2 && id2_ok)) ? 2'd2 : ((fb_ok && trace2 && id2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && sq2==7'd63)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_03_009_result : coverpoint ($isunknown((fb_ok && trace2 && id2_ok)) ? 2'd2 : ((fb_ok && trace2 && id2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && sq2==7'd64)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_03_010_result : coverpoint ($isunknown((fb_ok && trace2 && id2_ok)) ? 2'd2 : ((fb_ok && trace2 && id2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && sq2==7'd127)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_01_03_011_result : coverpoint ($isunknown((pipeline_wb_result)) ? 2'd2 : ((pipeline_wb_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && retry4_c)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_01_03 cg_storeunit_diag_01_03_inst;

    // 02.01 操作类型与CBO语义
    covergroup cg_storeunit_02_01 @(posedge clock);
        option.per_instance = 1;
        // 02.01.001 OP_SB
        // 场景：自然对齐的SB，fuOpType=0x000；标量入口实际获选；每次只改变目标操作编码；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：非CBO，wlineflag=0；TLB写命令；hyperinst=0；按1字节生成mask。
        // 审计结论：完全建模。
        cp_02_01_001 : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.01.002 OP_SH
        // 场景：自然对齐的SH，fuOpType=0x001；标量入口实际获选；每次只改变目标操作编码；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：非CBO，wlineflag=0；TLB写命令；hyperinst=0；按2字节生成mask。
        // 审计结论：完全建模。
        cp_02_01_002 : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.01.003 OP_SW
        // 场景：自然对齐的SW，fuOpType=0x002；标量入口实际获选；每次只改变目标操作编码；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：非CBO，wlineflag=0；TLB写命令；hyperinst=0；按4字节生成mask。
        // 审计结论：完全建模。
        cp_02_01_003 : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.01.004 OP_SD
        // 场景：自然对齐的SD，fuOpType=0x003；标量入口实际获选；每次只改变目标操作编码；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：非CBO，wlineflag=0；TLB写命令；hyperinst=0；按8字节生成mask。
        // 审计结论：完全建模。
        cp_02_01_004 : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.01.005 OP_HSV.B
        // 场景：自然对齐的HSV.B，fuOpType=0x010；标量入口实际获选；每次只改变目标操作编码；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：非CBO，wlineflag=0；TLB写命令；hyperinst=1；按1字节生成mask。
        // 审计结论：完全建模。
        cp_02_01_005 : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.01.006 OP_HSV.H
        // 场景：自然对齐的HSV.H，fuOpType=0x011；标量入口实际获选；每次只改变目标操作编码；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：非CBO，wlineflag=0；TLB写命令；hyperinst=1；按2字节生成mask。
        // 审计结论：完全建模。
        cp_02_01_006 : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.01.007 OP_HSV.W
        // 场景：自然对齐的HSV.W，fuOpType=0x012；标量入口实际获选；每次只改变目标操作编码；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：非CBO，wlineflag=0；TLB写命令；hyperinst=1；按4字节生成mask。
        // 审计结论：完全建模。
        cp_02_01_007 : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.01.008 OP_HSV.D
        // 场景：自然对齐的HSV.D，fuOpType=0x013；标量入口实际获选；每次只改变目标操作编码；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：非CBO，wlineflag=0；TLB写命令；hyperinst=1；按8字节生成mask。
        // 审计结论：完全建模。
        cp_02_01_008 : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.01.009 OP_CBO.zero
        // 场景：CBO.zero指令，fuOpType=0x007，地址选不同cacheline内偏移；标量入口实际获选；每次只改变目标操作编码；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：CBO标志和wlineflag有效，mask=0xffff，视为对齐；TLB命令为write=1，nuke按CacheLine。
        // 审计结论：完全建模。
        cp_02_01_009 : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd7)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.01.010 OP_CBO.clean
        // 场景：CBO.clean指令，fuOpType=0x00c，地址选不同cacheline内偏移；标量入口实际获选；每次只改变目标操作编码；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：CBO标志和wlineflag有效，mask=0xffff，视为对齐；TLB命令为read=0，nuke按CacheLine。
        // 审计结论：完全建模。
        cp_02_01_010 : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd12)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.01.011 OP_CBO.flush
        // 场景：CBO.flush指令，fuOpType=0x00d，地址选不同cacheline内偏移；标量入口实际获选；每次只改变目标操作编码；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：CBO标志和wlineflag有效，mask=0xffff，视为对齐；TLB命令为read=0，nuke按CacheLine。
        // 审计结论：完全建模。
        cp_02_01_011 : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd13)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.01.012 OP_CBO.inval
        // 场景：CBO.inval指令，fuOpType=0x00e，地址选不同cacheline内偏移；标量入口实际获选；每次只改变目标操作编码；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：CBO标志和wlineflag有效，mask=0xffff，视为对齐；TLB命令为read=0，nuke按CacheLine。
        // 审计结论：完全建模。
        cp_02_01_012 : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd14)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_02_01 cg_storeunit_02_01_inst;
    covergroup cg_storeunit_diag_02_01 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_02_01_001_result : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_01_002_result : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_01_003_result : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_01_004_result : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_01_005_result : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_01_006_result : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_01_007_result : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_01_008_result : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_01_009_result : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd7)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_01_010_result : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd12)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_01_011_result : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd13)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_01_012_result : coverpoint ($isunknown((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok)) ? 2'd2 : ((s0_ok_d1 && s1_valid && addr1_ok && io_lsq_bits_wlineflag==cbo1 && io_tlb_req_bits_cmd_d1==cmd1 && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd14)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_02_01 cg_storeunit_diag_02_01_inst;

    // 02.02 立即数与完整地址
    covergroup cg_storeunit_02_02 @(posedge clock);
        option.per_instance = 1;
        // 02.02.001 ADDR_imm_000
        // 场景：基址可表示且imm[11:0]=0x000；只使用R.imm[11:0]；低50位用于vaddr，64位和用于fullva；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：fullva=(base+(0)) mod 2^64；vaddr取同一和的低50位。
        // 审计结论：完全建模。
        cp_02_02_001 : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_uop_imm[11:0]==12'd0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.02.002 ADDR_imm_001
        // 场景：基址可表示且imm[11:0]=0x001；只使用R.imm[11:0]；低50位用于vaddr，64位和用于fullva；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：fullva=(base+(1)) mod 2^64；vaddr取同一和的低50位。
        // 审计结论：完全建模。
        cp_02_02_002 : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_uop_imm[11:0]==12'd1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.02.003 ADDR_imm_7ff
        // 场景：基址可表示且imm[11:0]=0x7ff；只使用R.imm[11:0]；低50位用于vaddr，64位和用于fullva；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：fullva=(base+(2047)) mod 2^64；vaddr取同一和的低50位。
        // 审计结论：完全建模。
        cp_02_02_003 : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_uop_imm[11:0]==12'd2047)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.02.004 ADDR_imm_800
        // 场景：基址可表示且imm[11:0]=0x800；只使用R.imm[11:0]；低50位用于vaddr，64位和用于fullva；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：fullva=(base+(-2048)) mod 2^64；vaddr取同一和的低50位。
        // 审计结论：完全建模。
        cp_02_02_004 : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_uop_imm[11:0]==12'd2048)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.02.005 ADDR_imm_fff
        // 场景：基址可表示且imm[11:0]=0xfff；只使用R.imm[11:0]；低50位用于vaddr，64位和用于fullva；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：fullva=(base+(-1)) mod 2^64；vaddr取同一和的低50位。
        // 审计结论：完全建模。
        cp_02_02_005 : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_uop_imm[11:0]==12'd4095)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.02.006 ADDR_跨页进位
        // 场景：base=0x1fff，imm=0x001；只使用R.imm[11:0]；低50位用于vaddr，64位和用于fullva；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：fullva=0x0000000000002000，vaddr=0x0000000002000；地址合法性由DTLB后续处理，地址计算本身不截成页内偏移。
        // 审计结论：完全建模。
        cp_02_02_006 : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_src_0==64'h1fff && io_stin_bits_uop_imm[11:0]==12'd1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.02.007 ADDR_跨页借位
        // 场景：base=0x2000，imm=0xfff；只使用R.imm[11:0]；低50位用于vaddr，64位和用于fullva；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：fullva=0x0000000000001fff，vaddr=0x0000000001fff；地址合法性由DTLB后续处理，地址计算本身不截成页内偏移。
        // 审计结论：完全建模。
        cp_02_02_007 : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_src_0==64'h2000 && io_stin_bits_uop_imm[11:0]==12'd4095)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.02.008 ADDR_64位回绕
        // 场景：base=0xffffffffffffffff，imm=0x001；只使用R.imm[11:0]；低50位用于vaddr，64位和用于fullva；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：fullva=0x0000000000000000，vaddr=0x0000000000000；地址合法性由DTLB后续处理，地址计算本身不截成页内偏移。
        // 审计结论：完全建模。
        cp_02_02_008 : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_src_0==64'hffffffffffffffff && io_stin_bits_uop_imm[11:0]==12'd1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.02.009 ADDR_50位截断
        // 场景：base=0x3ffffffffffff，imm=0x001；只使用R.imm[11:0]；低50位用于vaddr，64位和用于fullva；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：fullva=0x0004000000000000，vaddr=0x0000000000000；地址合法性由DTLB后续处理，地址计算本身不截成页内偏移。
        // 审计结论：完全建模。
        cp_02_02_009 : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_src_0==64'h3ffffffffffff && io_stin_bits_uop_imm[11:0]==12'd1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.02.010 ADDR_立即数高位无影响
        // 场景：保持base和imm[11:0]相同，两次请求只改变imm[31:12]；只使用R.imm[11:0]；低50位用于vaddr，64位和用于fullva；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：两次地址相同；请求各自保留独立token。
        // 审计结论：完全建模。
        cp_02_02_010 : coverpoint ($isunknown((address_ok && io_tlb_req_bits_fullva==last_fullva)) ? 2'd2 : ((address_ok && io_tlb_req_bits_fullva==last_fullva) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && last_scalar && io_stin_bits_src_0==last_base && io_stin_bits_uop_imm[11:0]==last_imm[11:0] && io_stin_bits_uop_imm[31:12]!=last_imm[31:12])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_02_02 cg_storeunit_02_02_inst;
    covergroup cg_storeunit_diag_02_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_02_02_001_result : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_uop_imm[11:0]==12'd0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_02_002_result : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_uop_imm[11:0]==12'd1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_02_003_result : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_uop_imm[11:0]==12'd2047)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_02_004_result : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_uop_imm[11:0]==12'd2048)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_02_005_result : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_uop_imm[11:0]==12'd4095)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_02_006_result : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_src_0==64'h1fff && io_stin_bits_uop_imm[11:0]==12'd1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_02_007_result : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_src_0==64'h2000 && io_stin_bits_uop_imm[11:0]==12'd4095)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_02_008_result : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_src_0==64'hffffffffffffffff && io_stin_bits_uop_imm[11:0]==12'd1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_02_009_result : coverpoint ($isunknown((address_ok)) ? 2'd2 : ((address_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && io_stin_bits_src_0==64'h3ffffffffffff && io_stin_bits_uop_imm[11:0]==12'd1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_02_010_result : coverpoint ($isunknown((address_ok && io_tlb_req_bits_fullva==last_fullva)) ? 2'd2 : ((address_ok && io_tlb_req_bits_fullva==last_fullva) ? 2'd1 : 2'd0))
            iff (sample_enable && (exp_fire && selected_scalar && last_scalar && io_stin_bits_src_0==last_base && io_stin_bits_uop_imm[11:0]==last_imm[11:0] && io_stin_bits_uop_imm[31:12]!=last_imm[31:12])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_02_02 cg_storeunit_diag_02_02_inst;

    // 02.03 大小、低位与掩码逐值展开
    covergroup cg_storeunit_02_03 @(posedge clock);
        option.per_instance = 1;
        // 02.03.001 MASK_SB_offset00
        // 场景：SB，有效地址低4位=0（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0001；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_001 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0001 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0001 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.002 MASK_SB_offset01
        // 场景：SB，有效地址低4位=1（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0002；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_002 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0002 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0002 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.003 MASK_SB_offset02
        // 场景：SB，有效地址低4位=2（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0004；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_003 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0004 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0004 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.004 MASK_SB_offset03
        // 场景：SB，有效地址低4位=3（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0008；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_004 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0008 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0008 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd3)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.005 MASK_SB_offset04
        // 场景：SB，有效地址低4位=4（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0010；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_005 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0010 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0010 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd4)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.006 MASK_SB_offset05
        // 场景：SB，有效地址低4位=5（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0020；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_006 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0020 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0020 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd5)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.007 MASK_SB_offset06
        // 场景：SB，有效地址低4位=6（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0040；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_007 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0040 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0040 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd6)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.008 MASK_SB_offset07
        // 场景：SB，有效地址低4位=7（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0080；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_008 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0080 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0080 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd7)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.009 MASK_SB_offset08
        // 场景：SB，有效地址低4位=8（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0100；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_009 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0100 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0100 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd8)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.010 MASK_SB_offset09
        // 场景：SB，有效地址低4位=9（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0200；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_010 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0200 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0200 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd9)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.011 MASK_SB_offset10
        // 场景：SB，有效地址低4位=10（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0400；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_011 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0400 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0400 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd10)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.012 MASK_SB_offset11
        // 场景：SB，有效地址低4位=11（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0800；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_012 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0800 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0800 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd11)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.013 MASK_SB_offset12
        // 场景：SB，有效地址低4位=12（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x1000；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_013 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd12)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.014 MASK_SB_offset13
        // 场景：SB，有效地址低4位=13（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x2000；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_014 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h2000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h2000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd13)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.015 MASK_SB_offset14
        // 场景：SB，有效地址低4位=14（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x4000；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_015 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h4000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h4000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd14)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.016 MASK_SB_offset15
        // 场景：SB，有效地址低4位=15（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x8000；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_016 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h8000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h8000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd15)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.017 MASK_SH_offset00
        // 场景：SH，有效地址低4位=0（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0003；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_017 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.018 MASK_SH_offset01
        // 场景：SH，有效地址低4位=1（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0006；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_018 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0006 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0006 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.019 MASK_SH_offset02
        // 场景：SH，有效地址低4位=2（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x000c；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_019 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000c && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000c && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.020 MASK_SH_offset03
        // 场景：SH，有效地址低4位=3（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0018；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_020 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0018 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0018 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd3)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.021 MASK_SH_offset04
        // 场景：SH，有效地址低4位=4（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0030；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_021 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0030 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0030 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd4)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.022 MASK_SH_offset05
        // 场景：SH，有效地址低4位=5（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0060；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_022 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0060 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0060 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd5)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.023 MASK_SH_offset06
        // 场景：SH，有效地址低4位=6（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x00c0；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_023 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00c0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00c0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd6)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.024 MASK_SH_offset07
        // 场景：SH，有效地址低4位=7（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0180；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_024 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0180 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0180 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd7)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.025 MASK_SH_offset08
        // 场景：SH，有效地址低4位=8（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0300；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_025 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0300 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0300 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd8)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.026 MASK_SH_offset09
        // 场景：SH，有效地址低4位=9（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0600；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_026 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0600 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0600 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd9)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.027 MASK_SH_offset10
        // 场景：SH，有效地址低4位=10（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0c00；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_027 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0c00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0c00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd10)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.028 MASK_SH_offset11
        // 场景：SH，有效地址低4位=11（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x1800；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_028 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1800 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1800 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd11)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.029 MASK_SH_offset12
        // 场景：SH，有效地址低4位=12（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x3000；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_029 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd12)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.030 MASK_SH_offset13
        // 场景：SH，有效地址低4位=13（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x6000；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_030 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h6000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h6000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd13)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.031 MASK_SH_offset14
        // 场景：SH，有效地址低4位=14（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0xc000；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_031 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hc000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hc000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd14)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.032 MASK_SH_offset15
        // 场景：SH，有效地址低4位=15（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x0003；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_032 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd15)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.033 MASK_SW_offset00
        // 场景：SW，有效地址低4位=0（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x000f；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_033 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.034 MASK_SW_offset01
        // 场景：SW，有效地址低4位=1（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x001e；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_034 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h001e && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h001e && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.035 MASK_SW_offset02
        // 场景：SW，有效地址低4位=2（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x003c；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_035 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h003c && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h003c && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.036 MASK_SW_offset03
        // 场景：SW，有效地址低4位=3（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0078；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_036 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0078 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0078 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd3)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.037 MASK_SW_offset04
        // 场景：SW，有效地址低4位=4（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x00f0；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_037 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00f0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00f0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd4)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.038 MASK_SW_offset05
        // 场景：SW，有效地址低4位=5（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x01e0；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_038 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01e0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01e0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd5)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.039 MASK_SW_offset06
        // 场景：SW，有效地址低4位=6（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x03c0；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_039 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03c0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03c0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd6)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.040 MASK_SW_offset07
        // 场景：SW，有效地址低4位=7（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0780；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_040 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0780 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0780 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd7)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.041 MASK_SW_offset08
        // 场景：SW，有效地址低4位=8（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0f00；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_041 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0f00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0f00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd8)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.042 MASK_SW_offset09
        // 场景：SW，有效地址低4位=9（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x1e00；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_042 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1e00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1e00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd9)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.043 MASK_SW_offset10
        // 场景：SW，有效地址低4位=10（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x3c00；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_043 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3c00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3c00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd10)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.044 MASK_SW_offset11
        // 场景：SW，有效地址低4位=11（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x7800；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_044 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7800 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7800 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd11)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.045 MASK_SW_offset12
        // 场景：SW，有效地址低4位=12（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0xf000；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_045 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hf000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hf000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd12)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.046 MASK_SW_offset13
        // 场景：SW，有效地址低4位=13（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x000f；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_046 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd13)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.047 MASK_SW_offset14
        // 场景：SW，有效地址低4位=14（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x000f；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_047 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd14)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.048 MASK_SW_offset15
        // 场景：SW，有效地址低4位=15（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x000f；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_048 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd15)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.049 MASK_SD_offset00
        // 场景：SD，有效地址低4位=0（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_049 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.050 MASK_SD_offset01
        // 场景：SD，有效地址低4位=1（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x01fe；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_050 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01fe && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01fe && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.051 MASK_SD_offset02
        // 场景：SD，有效地址低4位=2（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x03fc；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_051 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03fc && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03fc && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.052 MASK_SD_offset03
        // 场景：SD，有效地址低4位=3（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x07f8；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_052 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h07f8 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h07f8 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd3)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.053 MASK_SD_offset04
        // 场景：SD，有效地址低4位=4（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0ff0；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_053 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0ff0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0ff0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd4)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.054 MASK_SD_offset05
        // 场景：SD，有效地址低4位=5（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x1fe0；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_054 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1fe0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1fe0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd5)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.055 MASK_SD_offset06
        // 场景：SD，有效地址低4位=6（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x3fc0；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_055 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3fc0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3fc0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd6)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.056 MASK_SD_offset07
        // 场景：SD，有效地址低4位=7（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x7f80；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_056 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7f80 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7f80 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd7)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.057 MASK_SD_offset08
        // 场景：SD，有效地址低4位=8（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0xff00；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_057 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hff00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hff00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd8)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.058 MASK_SD_offset09
        // 场景：SD，有效地址低4位=9（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_058 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd9)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.059 MASK_SD_offset10
        // 场景：SD，有效地址低4位=10（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_059 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd10)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.060 MASK_SD_offset11
        // 场景：SD，有效地址低4位=11（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_060 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd11)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.061 MASK_SD_offset12
        // 场景：SD，有效地址低4位=12（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_061 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd12)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.062 MASK_SD_offset13
        // 场景：SD，有效地址低4位=13（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_062 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd13)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.063 MASK_SD_offset14
        // 场景：SD，有效地址低4位=14（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_063 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd14)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.064 MASK_SD_offset15
        // 场景：SD，有效地址低4位=15（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_064 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd15)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.065 MASK_HSV.B_offset00
        // 场景：HSV.B，有效地址低4位=0（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0001；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_065 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0001 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0001 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.066 MASK_HSV.B_offset01
        // 场景：HSV.B，有效地址低4位=1（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0002；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_066 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0002 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0002 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.067 MASK_HSV.B_offset02
        // 场景：HSV.B，有效地址低4位=2（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0004；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_067 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0004 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0004 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.068 MASK_HSV.B_offset03
        // 场景：HSV.B，有效地址低4位=3（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0008；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_068 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0008 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0008 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd3)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.069 MASK_HSV.B_offset04
        // 场景：HSV.B，有效地址低4位=4（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0010；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_069 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0010 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0010 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd4)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.070 MASK_HSV.B_offset05
        // 场景：HSV.B，有效地址低4位=5（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0020；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_070 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0020 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0020 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd5)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.071 MASK_HSV.B_offset06
        // 场景：HSV.B，有效地址低4位=6（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0040；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_071 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0040 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0040 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd6)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.072 MASK_HSV.B_offset07
        // 场景：HSV.B，有效地址低4位=7（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0080；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_072 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0080 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0080 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd7)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.073 MASK_HSV.B_offset08
        // 场景：HSV.B，有效地址低4位=8（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0100；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_073 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0100 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0100 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd8)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.074 MASK_HSV.B_offset09
        // 场景：HSV.B，有效地址低4位=9（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0200；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_074 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0200 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0200 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd9)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.075 MASK_HSV.B_offset10
        // 场景：HSV.B，有效地址低4位=10（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0400；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_075 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0400 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0400 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd10)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.076 MASK_HSV.B_offset11
        // 场景：HSV.B，有效地址低4位=11（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0800；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_076 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0800 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0800 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd11)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.077 MASK_HSV.B_offset12
        // 场景：HSV.B，有效地址低4位=12（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x1000；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_077 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd12)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.078 MASK_HSV.B_offset13
        // 场景：HSV.B，有效地址低4位=13（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x2000；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_078 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h2000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h2000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd13)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.079 MASK_HSV.B_offset14
        // 场景：HSV.B，有效地址低4位=14（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x4000；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_079 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h4000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h4000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd14)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.080 MASK_HSV.B_offset15
        // 场景：HSV.B，有效地址低4位=15（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x8000；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_080 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h8000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h8000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd15)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.081 MASK_HSV.H_offset00
        // 场景：HSV.H，有效地址低4位=0（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0003；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_081 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.082 MASK_HSV.H_offset01
        // 场景：HSV.H，有效地址低4位=1（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0006；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_082 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0006 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0006 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.083 MASK_HSV.H_offset02
        // 场景：HSV.H，有效地址低4位=2（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x000c；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_083 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000c && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000c && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.084 MASK_HSV.H_offset03
        // 场景：HSV.H，有效地址低4位=3（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0018；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_084 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0018 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0018 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd3)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.085 MASK_HSV.H_offset04
        // 场景：HSV.H，有效地址低4位=4（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0030；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_085 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0030 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0030 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd4)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.086 MASK_HSV.H_offset05
        // 场景：HSV.H，有效地址低4位=5（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0060；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_086 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0060 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0060 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd5)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.087 MASK_HSV.H_offset06
        // 场景：HSV.H，有效地址低4位=6（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x00c0；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_087 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00c0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00c0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd6)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.088 MASK_HSV.H_offset07
        // 场景：HSV.H，有效地址低4位=7（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0180；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_088 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0180 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0180 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd7)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.089 MASK_HSV.H_offset08
        // 场景：HSV.H，有效地址低4位=8（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0300；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_089 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0300 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0300 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd8)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.090 MASK_HSV.H_offset09
        // 场景：HSV.H，有效地址低4位=9（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0600；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_090 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0600 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0600 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd9)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.091 MASK_HSV.H_offset10
        // 场景：HSV.H，有效地址低4位=10（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0c00；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_091 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0c00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0c00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd10)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.092 MASK_HSV.H_offset11
        // 场景：HSV.H，有效地址低4位=11（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x1800；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_092 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1800 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1800 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd11)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.093 MASK_HSV.H_offset12
        // 场景：HSV.H，有效地址低4位=12（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x3000；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_093 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd12)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.094 MASK_HSV.H_offset13
        // 场景：HSV.H，有效地址低4位=13（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x6000；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_094 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h6000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h6000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd13)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.095 MASK_HSV.H_offset14
        // 场景：HSV.H，有效地址低4位=14（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0xc000；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_095 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hc000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hc000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd14)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.096 MASK_HSV.H_offset15
        // 场景：HSV.H，有效地址低4位=15（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x0003；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_096 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd15)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.097 MASK_HSV.W_offset00
        // 场景：HSV.W，有效地址低4位=0（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x000f；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_097 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.098 MASK_HSV.W_offset01
        // 场景：HSV.W，有效地址低4位=1（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x001e；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_098 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h001e && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h001e && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.099 MASK_HSV.W_offset02
        // 场景：HSV.W，有效地址低4位=2（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x003c；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_099 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h003c && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h003c && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.100 MASK_HSV.W_offset03
        // 场景：HSV.W，有效地址低4位=3（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0078；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_100 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0078 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0078 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd3)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.101 MASK_HSV.W_offset04
        // 场景：HSV.W，有效地址低4位=4（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x00f0；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_101 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00f0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00f0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd4)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.102 MASK_HSV.W_offset05
        // 场景：HSV.W，有效地址低4位=5（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x01e0；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_102 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01e0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01e0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd5)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.103 MASK_HSV.W_offset06
        // 场景：HSV.W，有效地址低4位=6（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x03c0；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_103 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03c0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03c0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd6)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.104 MASK_HSV.W_offset07
        // 场景：HSV.W，有效地址低4位=7（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0780；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_104 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0780 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0780 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd7)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.105 MASK_HSV.W_offset08
        // 场景：HSV.W，有效地址低4位=8（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x0f00；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_105 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0f00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0f00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd8)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.106 MASK_HSV.W_offset09
        // 场景：HSV.W，有效地址低4位=9（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x1e00；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_106 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1e00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1e00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd9)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.107 MASK_HSV.W_offset10
        // 场景：HSV.W，有效地址低4位=10（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x3c00；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_107 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3c00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3c00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd10)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.108 MASK_HSV.W_offset11
        // 场景：HSV.W，有效地址低4位=11（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x7800；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_108 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7800 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7800 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd11)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.109 MASK_HSV.W_offset12
        // 场景：HSV.W，有效地址低4位=12（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0xf000；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_109 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hf000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hf000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd12)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.110 MASK_HSV.W_offset13
        // 场景：HSV.W，有效地址低4位=13（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x000f；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_110 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd13)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.111 MASK_HSV.W_offset14
        // 场景：HSV.W，有效地址低4位=14（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x000f；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_111 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd14)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.112 MASK_HSV.W_offset15
        // 场景：HSV.W，有效地址低4位=15（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x000f；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_112 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd15)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.113 MASK_HSV.D_offset00
        // 场景：HSV.D，有效地址低4位=0（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_113 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.114 MASK_HSV.D_offset01
        // 场景：HSV.D，有效地址低4位=1（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x01fe；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_114 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01fe && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01fe && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.115 MASK_HSV.D_offset02
        // 场景：HSV.D，有效地址低4位=2（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x03fc；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_115 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03fc && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03fc && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.116 MASK_HSV.D_offset03
        // 场景：HSV.D，有效地址低4位=3（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x07f8；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_116 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h07f8 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h07f8 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd3)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.117 MASK_HSV.D_offset04
        // 场景：HSV.D，有效地址低4位=4（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x0ff0；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_117 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0ff0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0ff0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd4)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.118 MASK_HSV.D_offset05
        // 场景：HSV.D，有效地址低4位=5（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x1fe0；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_118 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1fe0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1fe0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd5)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.119 MASK_HSV.D_offset06
        // 场景：HSV.D，有效地址低4位=6（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x3fc0；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_119 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3fc0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3fc0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd6)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.120 MASK_HSV.D_offset07
        // 场景：HSV.D，有效地址低4位=7（同16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=0，misalignWith16Byte=1；mask=0x7f80；is128bit=1。
        // 审计结论：完全建模。
        cp_02_03_120 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7f80 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7f80 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd7)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.121 MASK_HSV.D_offset08
        // 场景：HSV.D，有效地址低4位=8（自然对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=1，cross16=0，misalignWith16Byte=0；mask=0xff00；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_121 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hff00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hff00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd8)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.122 MASK_HSV.D_offset09
        // 场景：HSV.D，有效地址低4位=9（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_122 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd9)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.123 MASK_HSV.D_offset10
        // 场景：HSV.D，有效地址低4位=10（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_123 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd10)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.124 MASK_HSV.D_offset11
        // 场景：HSV.D，有效地址低4位=11（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_124 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd11)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.125 MASK_HSV.D_offset12
        // 场景：HSV.D，有效地址低4位=12（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_125 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd12)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.126 MASK_HSV.D_offset13
        // 场景：HSV.D，有效地址低4位=13（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_126 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd13)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.127 MASK_HSV.D_offset14
        // 场景：HSV.D，有效地址低4位=14（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_127 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd14)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.03.128 MASK_HSV.D_offset15
        // 场景：HSV.D，有效地址低4位=15（跨16B非对齐）；普通Store/HSV；VA低4位逐值；只展开合法op×offset组合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：aligned=0，cross16=1，misalignWith16Byte=0；mask=0x00ff；is128bit=0。
        // 审计结论：完全建模。
        cp_02_03_128 : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd15)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_02_03 cg_storeunit_02_03_inst;
    covergroup cg_storeunit_diag_02_03 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_02_03_001_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0001 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0001 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_002_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0002 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0002 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_003_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0004 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0004 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_004_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0008 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0008 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd3)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_005_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0010 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0010 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd4)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_006_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0020 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0020 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd5)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_007_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0040 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0040 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd6)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_008_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0080 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0080 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd7)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_009_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0100 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0100 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd8)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_010_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0200 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0200 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd9)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_011_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0400 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0400 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd10)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_012_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0800 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0800 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd11)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_013_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd12)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_014_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h2000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h2000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd13)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_015_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h4000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h4000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd14)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_016_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h8000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h8000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd0 && va1[3:0]==4'd15)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_017_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_018_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0006 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0006 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_019_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000c && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000c && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_020_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0018 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0018 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd3)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_021_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0030 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0030 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd4)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_022_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0060 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0060 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd5)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_023_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00c0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00c0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd6)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_024_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0180 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0180 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd7)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_025_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0300 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0300 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd8)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_026_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0600 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0600 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd9)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_027_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0c00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0c00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd10)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_028_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1800 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1800 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd11)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_029_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd12)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_030_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h6000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h6000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd13)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_031_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hc000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hc000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd14)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_032_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd1 && va1[3:0]==4'd15)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_033_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_034_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h001e && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h001e && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_035_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h003c && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h003c && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_036_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0078 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0078 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd3)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_037_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00f0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00f0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd4)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_038_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01e0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01e0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd5)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_039_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03c0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03c0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd6)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_040_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0780 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0780 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd7)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_041_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0f00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0f00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd8)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_042_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1e00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1e00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd9)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_043_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3c00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3c00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd10)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_044_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7800 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7800 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd11)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_045_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hf000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hf000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd12)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_046_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd13)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_047_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd14)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_048_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd2 && va1[3:0]==4'd15)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_049_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_050_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01fe && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01fe && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_051_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03fc && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03fc && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_052_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h07f8 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h07f8 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd3)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_053_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0ff0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0ff0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd4)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_054_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1fe0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1fe0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd5)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_055_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3fc0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3fc0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd6)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_056_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7f80 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7f80 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd7)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_057_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hff00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hff00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd8)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_058_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd9)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_059_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd10)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_060_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd11)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_061_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd12)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_062_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd13)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_063_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd14)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_064_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd3 && va1[3:0]==4'd15)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_065_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0001 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0001 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_066_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0002 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0002 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_067_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0004 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0004 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_068_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0008 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0008 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd3)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_069_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0010 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0010 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd4)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_070_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0020 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0020 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd5)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_071_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0040 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0040 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd6)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_072_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0080 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0080 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd7)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_073_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0100 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0100 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd8)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_074_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0200 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0200 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd9)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_075_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0400 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0400 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd10)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_076_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0800 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0800 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd11)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_077_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd12)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_078_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h2000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h2000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd13)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_079_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h4000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h4000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd14)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_080_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h8000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h8000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd16 && va1[3:0]==4'd15)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_081_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_082_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0006 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0006 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_083_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000c && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000c && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_084_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0018 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0018 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd3)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_085_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0030 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0030 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd4)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_086_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0060 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0060 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd5)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_087_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00c0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00c0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd6)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_088_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0180 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0180 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd7)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_089_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0300 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0300 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd8)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_090_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0600 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0600 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd9)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_091_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0c00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0c00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd10)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_092_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1800 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1800 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd11)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_093_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd12)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_094_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h6000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h6000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd13)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_095_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hc000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hc000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd14)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_096_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0003 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd17 && va1[3:0]==4'd15)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_097_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_098_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h001e && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h001e && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_099_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h003c && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h003c && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_100_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0078 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0078 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd3)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_101_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00f0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00f0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd4)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_102_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01e0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01e0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd5)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_103_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03c0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03c0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd6)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_104_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0780 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0780 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd7)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_105_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0f00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0f00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd8)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_106_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1e00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1e00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd9)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_107_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3c00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3c00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd10)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_108_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7800 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7800 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd11)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_109_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hf000 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hf000 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd12)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_110_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd13)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_111_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd14)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_112_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h000f && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd18 && va1[3:0]==4'd15)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_113_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_114_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01fe && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h01fe && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_115_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03fc && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h03fc && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_116_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h07f8 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h07f8 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd3)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_117_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0ff0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h0ff0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd4)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_118_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1fe0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h1fe0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd5)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_119_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3fc0 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h3fc0 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd6)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_120_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7f80 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h7f80 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd7)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_121_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hff00 && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'hff00 && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd8)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_122_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd9)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_123_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd10)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_124_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd11)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_125_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd12)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_126_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd13)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_127_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd14)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_03_128_result : coverpoint ($isunknown((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1)) ? 2'd2 : ((s0_ok_d1 && addr1_ok && io_lsq_bits_mask==16'h00ff && s1_in_is128bit==within1) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==9'd19 && va1[3:0]==4'd15)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_02_03 cg_storeunit_diag_02_03_inst;

    // 02.04 跨窗口与地址边界
    covergroup cg_storeunit_02_04 @(posedge clock);
        option.per_instance = 1;
        // 02.04.019 MASK_CBO_行首与行尾
        // 场景：同一cacheline的CBO分别使用偏移0和63；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：均mask=0xffff、aligned=1、CacheLine匹配；不因起始低位而缩短为普通Store mask。
        // 审计结论：完全建模。
        cp_02_04_019 : coverpoint ($isunknown((cbo_previous_result && s0_ok_d1 && addr1_ok && nuke_ok)) ? 2'd2 : ((cbo_previous_result && s0_ok_d1 && addr1_ok && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && cbo1 && cbo_previous_valid && cbo_previous_line==va1[49:6] && cbo_previous_op==op1 && ((va1[5:0]==63 && cbo_offsets_seen[0]) || (va1[5:0]==0 && cbo_offsets_seen[15])))) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_02_04 cg_storeunit_02_04_inst;
    covergroup cg_storeunit_diag_02_04 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_02_04_019_result : coverpoint ($isunknown((cbo_previous_result && s0_ok_d1 && addr1_ok && nuke_ok)) ? 2'd2 : ((cbo_previous_result && s0_ok_d1 && addr1_ok && nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && cbo1 && cbo_previous_valid && cbo_previous_line==va1[49:6] && cbo_previous_op==op1 && ((va1[5:0]==63 && cbo_offsets_seen[0]) || (va1[5:0]==0 && cbo_offsets_seen[15])))) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_02_04 cg_storeunit_diag_02_04_inst;

    // 03.01 请求字段与来源
    covergroup cg_storeunit_03_01 @(posedge clock);
        option.per_instance = 1;
        // 03.01.001 TLBREQ_普通写命令
        // 场景：普通SB/SH/SW/SD获选；TQ.valid等于s0_valid；请求valid不等于内部s0_fire；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：TQ.cmd=1、hyperinst=0、checkfullva=1、frm_mabuf=0。
        // 审计结论：完全建模。
        cp_03_01_001 : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && basic0 && exp_fire)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.01.002 TLBREQ_HSV写命令
        // 场景：HSV.B/H/W/D获选；TQ.valid等于s0_valid；请求valid不等于内部s0_fire；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：TQ.cmd=1、hyperinst=1、checkfullva=1、frm_mabuf=0。
        // 审计结论：完全建模。
        cp_03_01_002 : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && hsv0 && exp_fire)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.01.003 TLBREQ_CBO非zero读检查
        // 场景：CBO.clean/flush/inval获选；TQ.valid等于s0_valid；请求valid不等于内部s0_fire；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：TQ.cmd=0；尽管查读权限，消费者仍产生store异常。
        // 审计结论：完全建模。
        cp_03_01_003 : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && cbo_nz0 && exp_fire)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.01.004 TLBREQ_CBOzero写检查
        // 场景：CBO.zero获选；TQ.valid等于s0_valid；请求valid不等于内部s0_fire；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：TQ.cmd=1，检查写权限。
        // 审计结论：完全建模。
        cp_03_01_004 : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && req_op==7 && exp_fire)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.01.005 TLBREQ_MAB普通child
        // 场景：普通标量MAB child获选；TQ.valid等于s0_valid；请求valid不等于内部s0_fire；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：TQ.frm_mabuf=1、checkfullva=0、cmd=1、hyperinst=0。
        // 审计结论：完全建模。
        cp_03_01_005 : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_child && !hsv0 && exp_fire)) {
            option.weight = HAS_CHILD ? 1 : 0;
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.01.006 TLBREQ_MAB_HSVchild
        // 场景：HSV parent的MAB child获选；TQ.valid等于s0_valid；请求valid不等于内部s0_fire；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：TQ.frm_mabuf=1、checkfullva=0、cmd=1、hyperinst=1；HSV位由MAB恢复。
        // 审计结论：完全建模。
        cp_03_01_006 : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_child && hsv0 && exp_fire)) {
            option.weight = HAS_CHILD ? 1 : 0;
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.01.007 TLBREQ_ROB身份
        // 场景：不同ROB请求连续获选；TQ.valid等于s0_valid；请求valid不等于内部s0_fire；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：TQ.debug.robIdx使用当前选中请求的完整ROB指针。
        // 审计结论：完全建模。
        cp_03_01_007 : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && exp_fire && last_scalar && req_rob!=last_req_rob)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.01.008 TLBREQ_被冲刷仍请求valid
        // 场景：S0候选本拍needFlush为真；TQ.valid等于s0_valid；请求valid不等于内部s0_fire；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：TQ.valid仍可为1，但s0_fire=0；不得据TQ.valid建立已执行标量token。
        // 审计结论：完全建模。
        cp_03_01_008 : coverpoint ($isunknown((io_tlb_req_valid && !actual_fire && request_ok)) ? 2'd2 : ((io_tlb_req_valid && !actual_fire && request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && flush0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_03_01 cg_storeunit_03_01_inst;
    covergroup cg_storeunit_diag_03_01 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_03_01_001_result : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && basic0 && exp_fire)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_01_002_result : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && hsv0 && exp_fire)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_01_003_result : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && cbo_nz0 && exp_fire)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_01_004_result : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && req_op==7 && exp_fire)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_01_005_result : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_child && !hsv0 && exp_fire)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_01_006_result : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_child && hsv0 && exp_fire)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_01_007_result : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && exp_fire && last_scalar && req_rob!=last_req_rob)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_01_008_result : coverpoint ($isunknown((io_tlb_req_valid && !actual_fire && request_ok)) ? 2'd2 : ((io_tlb_req_valid && !actual_fire && request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && flush0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_03_01 cg_storeunit_diag_03_01_inst;

    // 03.02 hit、miss与响应缺失
    covergroup cg_storeunit_03_02 @(posedge clock);
        option.per_instance = 1;
        // 03.02.001 TLB_普通命中
        // 场景：有效普通标量S1收到TR.valid=1且miss=0；普通标量与MAB child分开；TR有效性必须独立保存；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：s1_tlb_hit=1、miss=0；无redirect时S1推进，下一拍慢反馈hit=1（MAB replay另计）。
        // 审计结论：完全建模。
        cp_03_02_001 : coverpoint ($isunknown((fb_ok && trace2 && id2_ok)) ? 2'd2 : ((fb_ok && trace2 && id2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && !flush2 && !replay2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.02.002 TLB_普通缺失
        // 场景：有效普通标量S1收到TR.valid=1且miss=1；普通标量与MAB child分开；TR有效性必须独立保存；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：s1_tlb_miss=1且s1_kill=1；LSQ仍可valid且miss=1；下一拍慢反馈hit=0；该attempt无普通S3写回。
        // 审计结论：完全建模。
        cp_03_02_002 : coverpoint ($isunknown((miss_result_d1 && fb_ok && !io_feedback_slow_bits_hit)) ? 2'd2 : ((miss_result_d1 && fb_ok && !io_feedback_slow_bits_hit) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_miss_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.02.003 TLB_MABchild缺失
        // 场景：MAB child在S1收到有效TLB miss；普通标量与MAB child分开；TR有效性必须独立保存；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：仅miss不kill该child；C2通过MO.need_rep=1返回，STA慢反馈不产生。
        // 审计结论：完全建模。
        cp_03_02_003 : coverpoint ($isunknown((child_ok && !io_feedback_slow_valid)) ? 2'd2 : ((child_ok && !io_feedback_slow_valid) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && child2 && miss2 && !flush2)) {
            option.weight = HAS_CHILD ? 1 : 0;
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.02.004 TLB_miss后重试命中
        // 场景：原始标量第一次miss，后续attempt有效hit；普通标量与MAB child分开；TR有效性必须独立保存；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：两次反馈分别block和hit，正常WB只归属后一次；不规定第几次必成功。
        // 审计结论：完全建模。
        cp_03_02_004 : coverpoint ($isunknown((pipeline_wb_result)) ? 2'd2 : ((pipeline_wb_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && tlb_retry4_c)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.02.005 TLB_缺响应_rawmiss0
        // 场景：有效S1的预计响应拍TR.valid=0，raw miss=0；普通标量与MAB child分开；TR有效性必须独立保存；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：hit与miss均为0；记录RTL仍可能推进并以!miss生成反馈的现状，结果归缺失响应，不能计为正常翻译成功。
        // 审计结论：完全建模。
        cp_03_02_005 : coverpoint ($isunknown((!s1_tlb_hit && !s1_tlb_miss)) ? 2'd2 : ((!s1_tlb_hit && !s1_tlb_miss) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !io_tlb_resp_valid && io_tlb_resp_bits_miss==0)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 03.02.006 TLB_缺响应_rawmiss1
        // 场景：有效S1的预计响应拍TR.valid=0，raw miss=1；普通标量与MAB child分开；TR有效性必须独立保存；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：hit与miss均为0；记录RTL仍可能推进并以!miss生成反馈的现状，结果归缺失响应，不能计为正常翻译成功。
        // 审计结论：完全建模。
        cp_03_02_006 : coverpoint ($isunknown((!s1_tlb_hit && !s1_tlb_miss)) ? 2'd2 : ((!s1_tlb_hit && !s1_tlb_miss) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !io_tlb_resp_valid && io_tlb_resp_bits_miss==1)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 03.02.007 TLB_无请求响应
        // 场景：S1无有效token但TR.valid=1；普通标量与MAB child分开；TR有效性必须独立保存；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：内部hit/miss均受s1_valid门控；记录游离响应，不把它计入已完成事务。
        // 审计结论：完全建模。
        cp_03_02_007 : coverpoint ($isunknown((!s1_tlb_hit && !s1_tlb_miss && !io_lsq_valid)) ? 2'd2 : ((!s1_tlb_hit && !s1_tlb_miss && !io_lsq_valid) ? 2'd1 : 2'd0))
            iff (sample_enable && (!expected1 && io_tlb_resp_valid)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_03_02 cg_storeunit_03_02_inst;
    covergroup cg_storeunit_diag_03_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_03_02_001_result : coverpoint ($isunknown((fb_ok && trace2 && id2_ok)) ? 2'd2 : ((fb_ok && trace2 && id2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && !flush2 && !replay2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_02_002_result : coverpoint ($isunknown((miss_result_d1 && fb_ok && !io_feedback_slow_bits_hit)) ? 2'd2 : ((miss_result_d1 && fb_ok && !io_feedback_slow_bits_hit) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_miss_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_02_003_result : coverpoint ($isunknown((child_ok && !io_feedback_slow_valid)) ? 2'd2 : ((child_ok && !io_feedback_slow_valid) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && child2 && miss2 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_02_004_result : coverpoint ($isunknown((pipeline_wb_result)) ? 2'd2 : ((pipeline_wb_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && tlb_retry4_c)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_02_005_result : coverpoint ($isunknown((!s1_tlb_hit && !s1_tlb_miss)) ? 2'd2 : ((!s1_tlb_hit && !s1_tlb_miss) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !io_tlb_resp_valid && io_tlb_resp_bits_miss==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_02_006_result : coverpoint ($isunknown((!s1_tlb_hit && !s1_tlb_miss)) ? 2'd2 : ((!s1_tlb_hit && !s1_tlb_miss) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !io_tlb_resp_valid && io_tlb_resp_bits_miss==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_02_007_result : coverpoint ($isunknown((!s1_tlb_hit && !s1_tlb_miss && !io_lsq_valid)) ? 2'd2 : ((!s1_tlb_hit && !s1_tlb_miss && !io_lsq_valid) ? 2'd1 : 2'd0))
            iff (sample_enable && (!expected1 && io_tlb_resp_valid)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_03_02 cg_storeunit_diag_03_02_inst;

    // 03.03 地址与故障元信息
    covergroup cg_storeunit_03_03 @(posedge clock);
        option.per_instance = 1;
        // 03.03.001 TLBRSP_paddr
        // 场景：有效hit响应携带可辨识的paddr，前后两条请求取不同值；有效响应；普通Store/HSV/zero使用st类，非zero CBO读命令使用ld类；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：paddr按真实存在字段在LSQ/S2或child返回中保持对应；普通stout已裁剪地址字段时不构造该端口。
        // 审计结论：完全建模。
        cp_03_03_001 : coverpoint ($isunknown((trace2 && id2_ok && s2_ok)) ? 2'd2 : ((trace2 && id2_ok && s2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && meta_change2[0])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.03.002 TLBRSP_gpaddr
        // 场景：有效hit响应携带可辨识的gpaddr，前后两条请求取不同值；有效响应；普通Store/HSV/zero使用st类，非zero CBO读命令使用ld类；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：gpaddr按真实存在字段在LSQ/S2或child返回中保持对应；普通stout已裁剪地址字段时不构造该端口。
        // 审计结论：完全建模。
        cp_03_03_002 : coverpoint ($isunknown((trace2 && id2_ok && s2_ok)) ? 2'd2 : ((trace2 && id2_ok && s2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && meta_change2[1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.03.003 TLBRSP_fullva
        // 场景：有效hit响应携带可辨识的fullva，前后两条请求取不同值；有效响应；普通Store/HSV/zero使用st类，非zero CBO读命令使用ld类；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：fullva按真实存在字段在LSQ/S2或child返回中保持对应；普通stout已裁剪地址字段时不构造该端口。
        // 审计结论：完全建模。
        cp_03_03_003 : coverpoint ($isunknown((trace2 && id2_ok && s2_ok)) ? 2'd2 : ((trace2 && id2_ok && s2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && meta_change2[2])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.03.004 TLBRSP_vaNeedExt
        // 场景：有效hit响应携带可辨识的vaNeedExt，前后两条请求取不同值；有效响应；普通Store/HSV/zero使用st类，非zero CBO读命令使用ld类；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：vaNeedExt按真实存在字段在LSQ/S2或child返回中保持对应；普通stout已裁剪地址字段时不构造该端口。
        // 审计结论：完全建模。
        cp_03_03_004 : coverpoint ($isunknown((trace2 && id2_ok && s2_ok)) ? 2'd2 : ((trace2 && id2_ok && s2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && meta_change2[3])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.03.005 TLBRSP_isHyper
        // 场景：有效hit响应携带可辨识的isHyper，前后两条请求取不同值；有效响应；普通Store/HSV/zero使用st类，非zero CBO读命令使用ld类；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：isHyper按真实存在字段在LSQ/S2或child返回中保持对应；普通stout已裁剪地址字段时不构造该端口。
        // 审计结论：完全建模。
        cp_03_03_005 : coverpoint ($isunknown((trace2 && id2_ok && s2_ok)) ? 2'd2 : ((trace2 && id2_ok && s2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && meta_change2[4])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.03.006 TLBRSP_isForVSnonLeafPTE
        // 场景：有效hit响应携带可辨识的isForVSnonLeafPTE，前后两条请求取不同值；有效响应；普通Store/HSV/zero使用st类，非zero CBO读命令使用ld类；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：isForVSnonLeafPTE按真实存在字段在LSQ/S2或child返回中保持对应；普通stout已裁剪地址字段时不构造该端口。
        // 审计结论：完全建模。
        cp_03_03_006 : coverpoint ($isunknown((trace2 && id2_ok && s2_ok)) ? 2'd2 : ((trace2 && id2_ok && s2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && meta_change2[5])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.03.007 TLBRSP_MAB完整VA来源
        // 场景：标量MAB child的VA高bit49为1且checkfullva=0；有效响应；普通Store/HSV/zero使用st类，非zero CBO读命令使用ld类；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：MAB路径fullva为50位vaddr零扩展到64位，按现有RTL记录；不能沿用普通请求的64位符号扩展预期。
        // 审计结论：完全建模。
        cp_03_03_007 : coverpoint ($isunknown((io_lsq_replenish_fullva=={14'b0,va2} && trace2)) ? 2'd2 : ((io_lsq_replenish_fullva=={14'b0,va2} && trace2) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && child2 && va2[49])) {
            option.weight = HAS_CHILD ? 1 : 0;
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.03.008 TLBFAULT_pf_st
        // 场景：普通Store/HSV/zero写命令有效响应仅置pf.st=1，其他fault为0且无trigger；有效响应；普通Store/HSV/zero使用st类，非zero CBO读命令使用ld类；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：对应store异常位15=1，因为pf.ld与pf.st作OR；最终组合在S2另查。
        // 审计结论：完全建模。
        cp_03_03_008 : coverpoint ($isunknown((io_lsq_bits_uop_exceptionVec_15 && s1_exception_ok)) ? 2'd2 : ((io_lsq_bits_uop_exceptionVec_15 && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && io_tlb_resp_valid && io_tlb_resp_bits_excp_0_pf_st)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.03.009 TLBFAULT_pf_ld
        // 场景：非zero CBO读命令有效响应仅置pf.ld=1，其他fault为0且无trigger；有效响应；普通Store/HSV/zero使用st类，非zero CBO读命令使用ld类；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：对应store异常位15=1，因为pf.ld与pf.st作OR；最终组合在S2另查。
        // 审计结论：完全建模。
        cp_03_03_009 : coverpoint ($isunknown((io_lsq_bits_uop_exceptionVec_15 && s1_exception_ok)) ? 2'd2 : ((io_lsq_bits_uop_exceptionVec_15 && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && io_tlb_resp_valid && io_tlb_resp_bits_excp_0_pf_ld)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.03.010 TLBFAULT_af_st
        // 场景：普通Store/HSV/zero写命令有效响应仅置af.st=1，其他fault为0且无trigger；有效响应；普通Store/HSV/zero使用st类，非zero CBO读命令使用ld类；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：对应store异常位7=1，因为af.ld与af.st作OR；最终组合在S2另查。
        // 审计结论：完全建模。
        cp_03_03_010 : coverpoint ($isunknown((io_lsq_bits_uop_exceptionVec_7 && s1_exception_ok)) ? 2'd2 : ((io_lsq_bits_uop_exceptionVec_7 && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && io_tlb_resp_valid && io_tlb_resp_bits_excp_0_af_st)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.03.011 TLBFAULT_af_ld
        // 场景：非zero CBO读命令有效响应仅置af.ld=1，其他fault为0且无trigger；有效响应；普通Store/HSV/zero使用st类，非zero CBO读命令使用ld类；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：对应store异常位7=1，因为af.ld与af.st作OR；最终组合在S2另查。
        // 审计结论：完全建模。
        cp_03_03_011 : coverpoint ($isunknown((io_lsq_bits_uop_exceptionVec_7 && s1_exception_ok)) ? 2'd2 : ((io_lsq_bits_uop_exceptionVec_7 && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && io_tlb_resp_valid && io_tlb_resp_bits_excp_0_af_ld)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.03.012 TLBFAULT_gpf_st
        // 场景：普通Store/HSV/zero写命令有效响应仅置gpf.st=1，其他fault为0且无trigger；有效响应；普通Store/HSV/zero使用st类，非zero CBO读命令使用ld类；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：对应store异常位23=1，因为gpf.ld与gpf.st作OR；最终组合在S2另查。
        // 审计结论：完全建模。
        cp_03_03_012 : coverpoint ($isunknown((io_lsq_bits_uop_exceptionVec_23 && s1_exception_ok)) ? 2'd2 : ((io_lsq_bits_uop_exceptionVec_23 && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && io_tlb_resp_valid && io_tlb_resp_bits_excp_0_gpf_st)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 03.03.013 TLBFAULT_gpf_ld
        // 场景：非zero CBO读命令有效响应仅置gpf.ld=1，其他fault为0且无trigger；有效响应；普通Store/HSV/zero使用st类，非zero CBO读命令使用ld类；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：对应store异常位23=1，因为gpf.ld与gpf.st作OR；最终组合在S2另查。
        // 审计结论：完全建模。
        cp_03_03_013 : coverpoint ($isunknown((io_lsq_bits_uop_exceptionVec_23 && s1_exception_ok)) ? 2'd2 : ((io_lsq_bits_uop_exceptionVec_23 && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && io_tlb_resp_valid && io_tlb_resp_bits_excp_0_gpf_ld)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_03_03 cg_storeunit_03_03_inst;
    covergroup cg_storeunit_diag_03_03 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_03_03_001_result : coverpoint ($isunknown((trace2 && id2_ok && s2_ok)) ? 2'd2 : ((trace2 && id2_ok && s2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && meta_change2[0])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_03_002_result : coverpoint ($isunknown((trace2 && id2_ok && s2_ok)) ? 2'd2 : ((trace2 && id2_ok && s2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && meta_change2[1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_03_003_result : coverpoint ($isunknown((trace2 && id2_ok && s2_ok)) ? 2'd2 : ((trace2 && id2_ok && s2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && meta_change2[2])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_03_004_result : coverpoint ($isunknown((trace2 && id2_ok && s2_ok)) ? 2'd2 : ((trace2 && id2_ok && s2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && meta_change2[3])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_03_005_result : coverpoint ($isunknown((trace2 && id2_ok && s2_ok)) ? 2'd2 : ((trace2 && id2_ok && s2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && meta_change2[4])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_03_006_result : coverpoint ($isunknown((trace2 && id2_ok && s2_ok)) ? 2'd2 : ((trace2 && id2_ok && s2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && meta_change2[5])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_03_007_result : coverpoint ($isunknown((io_lsq_replenish_fullva=={14'b0,va2} && trace2)) ? 2'd2 : ((io_lsq_replenish_fullva=={14'b0,va2} && trace2) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && child2 && va2[49])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_03_008_result : coverpoint ($isunknown((io_lsq_bits_uop_exceptionVec_15 && s1_exception_ok)) ? 2'd2 : ((io_lsq_bits_uop_exceptionVec_15 && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && io_tlb_resp_valid && io_tlb_resp_bits_excp_0_pf_st)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_03_009_result : coverpoint ($isunknown((io_lsq_bits_uop_exceptionVec_15 && s1_exception_ok)) ? 2'd2 : ((io_lsq_bits_uop_exceptionVec_15 && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && io_tlb_resp_valid && io_tlb_resp_bits_excp_0_pf_ld)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_03_010_result : coverpoint ($isunknown((io_lsq_bits_uop_exceptionVec_7 && s1_exception_ok)) ? 2'd2 : ((io_lsq_bits_uop_exceptionVec_7 && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && io_tlb_resp_valid && io_tlb_resp_bits_excp_0_af_st)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_03_011_result : coverpoint ($isunknown((io_lsq_bits_uop_exceptionVec_7 && s1_exception_ok)) ? 2'd2 : ((io_lsq_bits_uop_exceptionVec_7 && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && io_tlb_resp_valid && io_tlb_resp_bits_excp_0_af_ld)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_03_012_result : coverpoint ($isunknown((io_lsq_bits_uop_exceptionVec_23 && s1_exception_ok)) ? 2'd2 : ((io_lsq_bits_uop_exceptionVec_23 && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && io_tlb_resp_valid && io_tlb_resp_bits_excp_0_gpf_st)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_03_03_013_result : coverpoint ($isunknown((io_lsq_bits_uop_exceptionVec_23 && s1_exception_ok)) ? 2'd2 : ((io_lsq_bits_uop_exceptionVec_23 && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && io_tlb_resp_valid && io_tlb_resp_bits_excp_0_gpf_ld)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_03_03 cg_storeunit_diag_03_03_inst;

    // 04.01 PBMT选择与PMA覆盖
    covergroup cg_storeunit_04_01 @(posedge clock);
        option.per_instance = 1;
        // 04.01.007 ATTR_miss时PBMT清零
        // 场景：S1有效TLB miss但raw PBMT为1或2；PBMT依有效hit选择；miss分支单列，不使用其raw PBMT；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：s1_pbmt取0；raw响应残留的NC/IO不作为有效命中属性。
        // 审计结论：完全建模。
        cp_04_01_007 : coverpoint ($isunknown((s1_pbmt==0)) ? 2'd2 : ((s1_pbmt==0) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && miss1 && io_tlb_resp_bits_pbmt_0 inside {1,2})) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_04_01 cg_storeunit_04_01_inst;
    covergroup cg_storeunit_diag_04_01 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_04_01_007_result : coverpoint ($isunknown((s1_pbmt==0)) ? 2'd2 : ((s1_pbmt==0) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && miss1 && io_tlb_resp_bits_pbmt_0 inside {1,2})) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_04_01 cg_storeunit_diag_04_01_inst;

    // 04.02 PMP读写拒绝与CBO权限
    covergroup cg_storeunit_04_02 @(posedge clock);
        option.per_instance = 1;
        // 04.02.001 PMP_SD_ld0st0
        // 场景：SD在S2，P.ld=0、P.st=0；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=0；ld-only不拒绝普通Store或CBO.zero；非zero CBO无异常时转SQ特殊完成。
        // 审计结论：完全建模。
        cp_04_02_001 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==3 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==0 && !flush2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.02.002 PMP_SD_ld1st0
        // 场景：SD在S2，P.ld=1、P.st=0；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=0；ld-only不拒绝普通Store或CBO.zero；非zero CBO无异常时转SQ特殊完成。
        // 审计结论：完全建模。
        cp_04_02_002 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==3 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==0 && !flush2)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 04.02.003 PMP_SD_ld0st1
        // 场景：SD在S2，P.ld=0、P.st=1；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=1；在有效未kill的S2中RE.af/hasException体现异常。
        // 审计结论：完全建模。
        cp_04_02_003 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==3 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==1 && !flush2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.02.004 PMP_SD_ld1st1
        // 场景：SD在S2，P.ld=1、P.st=1；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=1；在有效未kill的S2中RE.af/hasException体现异常。
        // 审计结论：完全建模。
        cp_04_02_004 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==3 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 04.02.005 PMP_CBO.zero_ld0st0
        // 场景：CBO.zero在S2，P.ld=0、P.st=0；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=0；ld-only不拒绝普通Store或CBO.zero；非zero CBO无异常时转SQ特殊完成。
        // 审计结论：完全建模。
        cp_04_02_005 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==7 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==0 && !flush2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.02.006 PMP_CBO.zero_ld1st0
        // 场景：CBO.zero在S2，P.ld=1、P.st=0；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=0；ld-only不拒绝普通Store或CBO.zero；非zero CBO无异常时转SQ特殊完成。
        // 审计结论：完全建模。
        cp_04_02_006 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==7 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==0 && !flush2)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 04.02.007 PMP_CBO.zero_ld0st1
        // 场景：CBO.zero在S2，P.ld=0、P.st=1；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=1；在有效未kill的S2中RE.af/hasException体现异常。
        // 审计结论：完全建模。
        cp_04_02_007 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==7 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==1 && !flush2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.02.008 PMP_CBO.zero_ld1st1
        // 场景：CBO.zero在S2，P.ld=1、P.st=1；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=1；在有效未kill的S2中RE.af/hasException体现异常。
        // 审计结论：完全建模。
        cp_04_02_008 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==7 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 04.02.009 PMP_CBO.clean_ld0st0
        // 场景：CBO.clean在S2，P.ld=0、P.st=0；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=0；ld-only不拒绝普通Store或CBO.zero；非zero CBO无异常时转SQ特殊完成。
        // 审计结论：完全建模。
        cp_04_02_009 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==12 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==0 && !flush2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.02.010 PMP_CBO.clean_ld1st0
        // 场景：CBO.clean在S2，P.ld=1、P.st=0；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=1；在有效未kill的S2中RE.af/hasException体现异常。
        // 审计结论：完全建模。
        cp_04_02_010 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==12 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==0 && !flush2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.02.011 PMP_CBO.clean_ld0st1
        // 场景：CBO.clean在S2，P.ld=0、P.st=1；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=1；在有效未kill的S2中RE.af/hasException体现异常。
        // 审计结论：完全建模。
        cp_04_02_011 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==12 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 04.02.012 PMP_CBO.clean_ld1st1
        // 场景：CBO.clean在S2，P.ld=1、P.st=1；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=1；在有效未kill的S2中RE.af/hasException体现异常。
        // 审计结论：完全建模。
        cp_04_02_012 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==12 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 04.02.013 PMP_CBO.flush_ld0st0
        // 场景：CBO.flush在S2，P.ld=0、P.st=0；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=0；ld-only不拒绝普通Store或CBO.zero；非zero CBO无异常时转SQ特殊完成。
        // 审计结论：完全建模。
        cp_04_02_013 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==13 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==0 && !flush2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.02.014 PMP_CBO.flush_ld1st0
        // 场景：CBO.flush在S2，P.ld=1、P.st=0；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=1；在有效未kill的S2中RE.af/hasException体现异常。
        // 审计结论：完全建模。
        cp_04_02_014 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==13 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==0 && !flush2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.02.015 PMP_CBO.flush_ld0st1
        // 场景：CBO.flush在S2，P.ld=0、P.st=1；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=1；在有效未kill的S2中RE.af/hasException体现异常。
        // 审计结论：完全建模。
        cp_04_02_015 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==13 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 04.02.016 PMP_CBO.flush_ld1st1
        // 场景：CBO.flush在S2，P.ld=1、P.st=1；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=1；在有效未kill的S2中RE.af/hasException体现异常。
        // 审计结论：完全建模。
        cp_04_02_016 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==13 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 04.02.017 PMP_CBO.inval_ld0st0
        // 场景：CBO.inval在S2，P.ld=0、P.st=0；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=0；ld-only不拒绝普通Store或CBO.zero；非zero CBO无异常时转SQ特殊完成。
        // 审计结论：完全建模。
        cp_04_02_017 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==14 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==0 && !flush2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.02.018 PMP_CBO.inval_ld1st0
        // 场景：CBO.inval在S2，P.ld=1、P.st=0；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=1；在有效未kill的S2中RE.af/hasException体现异常。
        // 审计结论：完全建模。
        cp_04_02_018 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==14 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==0 && !flush2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.02.019 PMP_CBO.inval_ld0st1
        // 场景：CBO.inval在S2，P.ld=0、P.st=1；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=1；在有效未kill的S2中RE.af/hasException体现异常。
        // 审计结论：完全建模。
        cp_04_02_019 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==14 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 04.02.020 PMP_CBO.inval_ld1st1
        // 场景：CBO.inval在S2，P.ld=1、P.st=1；有效hit；PBMT=PMA且P.mmio=0；其他fault和trigger隔离；P.ld/st受实际请求cmd门控；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终storeAccessFault=1；在有效未kill的S2中RE.af/hasException体现异常。
        // 审计结论：完全建模。
        cp_04_02_020 : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==14 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_04_02 cg_storeunit_04_02_inst;
    covergroup cg_storeunit_diag_04_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_04_02_001_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==3 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==0 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_002_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==3 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==0 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_003_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==3 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_004_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==3 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_005_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==7 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==0 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_006_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==7 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==0 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_007_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==7 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_008_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==7 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_009_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==12 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==0 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_010_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==12 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==0 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_011_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==12 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_012_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==12 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_013_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==13 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==0 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_014_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==13 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==0 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_015_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==13 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_016_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==13 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_017_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==14 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==0 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_018_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==14 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==0 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_019_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==14 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==0 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_02_020_result : coverpoint ($isunknown((s2_ok && attributes_ok)) ? 2'd2 : ((s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==14 && pbmt2==0 && !io_pmp_mmio && !incoming_fault2 && action2==15 && io_pmp_ld==1 && io_pmp_st==1 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_04_02 cg_storeunit_diag_04_02_inst;

    // 04.03 非对齐NC与MMIO异常
    covergroup cg_storeunit_04_03 @(posedge clock);
        option.per_instance = 1;
        // 04.03.001 UNCACHE_NC_自然对齐
        // 场景：NC属性的自然对齐，MAB admission/commit资格按场景满足；只检查标量；本行显式属性/fault覆盖D0默认；有效物理属性须满足真实翻译条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：NC对齐Store可普通STA写回；不因NC本身触发AF或SAM。
        // 审计结论：完全建模。
        cp_04_03_001 : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==1 && !unaligned2 && !child2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.03.002 UNCACHE_PBMT_IO_自然对齐
        // 场景：PBMT_IO属性的自然对齐，MAB admission/commit资格按场景满足；只检查标量；本行显式属性/fault覆盖D0默认；有效物理属性须满足真实翻译条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：对齐MMIO无异常，S2普通标量被kill并转SQ特殊完成。
        // 审计结论：完全建模。
        cp_04_03_002 : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==2 && !unaligned2 && !child2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.03.003 UNCACHE_PMA_MMIO_自然对齐
        // 场景：PMA_MMIO属性的自然对齐，MAB admission/commit资格按场景满足；只检查标量；本行显式属性/fault覆盖D0默认；有效物理属性须满足真实翻译条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：对齐MMIO无异常，S2普通标量被kill并转SQ特殊完成。
        // 审计结论：完全建模。
        cp_04_03_003 : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==0 && io_pmp_mmio && !unaligned2 && !child2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.03.004 UNCACHE_NC_同16B非对齐
        // 场景：NC属性的同16B非对齐，MAB admission/commit资格按场景满足；只检查标量；本行显式属性/fault覆盖D0默认；有效物理属性须满足真实翻译条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：产生storeAddrMisaligned=1；其他异常会抑制SAM。
        // 审计结论：完全建模。
        cp_04_03_004 : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==1 && within2 && !child2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.03.005 UNCACHE_PBMT_IO_同16B非对齐
        // 场景：PBMT_IO属性的同16B非对齐，MAB admission/commit资格按场景满足；只检查标量；本行显式属性/fault覆盖D0默认；有效物理属性须满足真实翻译条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：产生storeAccessFault=1，S2最终mmio被异常门控清零。
        // 审计结论：完全建模。
        cp_04_03_005 : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==2 && within2 && !child2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.03.006 UNCACHE_PMA_MMIO_同16B非对齐
        // 场景：PMA_MMIO属性的同16B非对齐，MAB admission/commit资格按场景满足；只检查标量；本行显式属性/fault覆盖D0默认；有效物理属性须满足真实翻译条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：产生storeAccessFault=1，S2最终mmio被异常门控清零。
        // 审计结论：完全建模。
        cp_04_03_006 : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==0 && io_pmp_mmio && within2 && !child2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.03.007 UNCACHE_NC_跨16B原始请求
        // 场景：NC属性的跨16B原始请求，MAB admission/commit资格按场景满足；只检查标量；本行显式属性/fault覆盖D0默认；有效物理属性须满足真实翻译条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：产生storeAddrMisaligned=1；其他异常会抑制SAM。
        // 审计结论：完全建模。
        cp_04_03_007 : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==1 && unaligned2 && !within2 && !child2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.03.008 UNCACHE_PBMT_IO_跨16B原始请求
        // 场景：PBMT_IO属性的跨16B原始请求，MAB admission/commit资格按场景满足；只检查标量；本行显式属性/fault覆盖D0默认；有效物理属性须满足真实翻译条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：产生storeAccessFault=1，S2最终mmio被异常门控清零。
        // 审计结论：完全建模。
        cp_04_03_008 : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==2 && unaligned2 && !within2 && !child2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.03.009 UNCACHE_PMA_MMIO_跨16B原始请求
        // 场景：PMA_MMIO属性的跨16B原始请求，MAB admission/commit资格按场景满足；只检查标量；本行显式属性/fault覆盖D0默认；有效物理属性须满足真实翻译条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：产生storeAccessFault=1，S2最终mmio被异常门控清零。
        // 审计结论：完全建模。
        cp_04_03_009 : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==0 && io_pmp_mmio && unaligned2 && !within2 && !child2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.03.010 UNCACHE_NC_MABchild
        // 场景：NC属性的MABchild，MAB admission/commit资格按场景满足；只检查标量；本行显式属性/fault覆盖D0默认；有效物理属性须满足真实翻译条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：产生storeAddrMisaligned=1；其他异常会抑制SAM。
        // 审计结论：完全建模。
        cp_04_03_010 : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==1 && child2)) {
            option.weight = HAS_CHILD ? 1 : 0;
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.03.011 UNCACHE_PBMT_IO_MABchild
        // 场景：PBMT_IO属性的MABchild，MAB admission/commit资格按场景满足；只检查标量；本行显式属性/fault覆盖D0默认；有效物理属性须满足真实翻译条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：产生storeAccessFault=1，S2最终mmio被异常门控清零。
        // 审计结论：完全建模。
        cp_04_03_011 : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==2 && child2)) {
            option.weight = HAS_CHILD ? 1 : 0;
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.03.012 UNCACHE_PMA_MMIO_MABchild
        // 场景：PMA_MMIO属性的MABchild，MAB admission/commit资格按场景满足；只检查标量；本行显式属性/fault覆盖D0默认；有效物理属性须满足真实翻译条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：产生storeAccessFault=1，S2最终mmio被异常门控清零。
        // 审计结论：完全建模。
        cp_04_03_012 : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==0 && io_pmp_mmio && child2)) {
            option.weight = HAS_CHILD ? 1 : 0;
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.03.013 UNCACHE_翻译fault屏蔽属性可信度
        // 场景：响应含PF/GPF/AF且P.mmio=1或PBMT为NC/IO；只检查标量；本行显式属性/fault覆盖D0默认；有效物理属性须满足真实翻译条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：actually_*要求有效hit且无已有访问/页fault；不凭无效物理属性新增“实际MMIO/NC”分类。
        // 审计结论：完全建模。
        cp_04_03_013 : coverpoint ($isunknown((s2_ok && !exp_actual_uncache2)) ? 2'd2 : ((s2_ok && !exp_actual_uncache2) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && incoming_fault2 && (io_pmp_mmio || pbmt2 inside {1,2}))) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_04_03 cg_storeunit_04_03_inst;
    covergroup cg_storeunit_diag_04_03 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_04_03_001_result : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==1 && !unaligned2 && !child2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_03_002_result : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==2 && !unaligned2 && !child2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_03_003_result : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==0 && io_pmp_mmio && !unaligned2 && !child2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_03_004_result : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==1 && within2 && !child2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_03_005_result : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==2 && within2 && !child2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_03_006_result : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==0 && io_pmp_mmio && within2 && !child2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_03_007_result : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==1 && unaligned2 && !within2 && !child2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_03_008_result : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==2 && unaligned2 && !within2 && !child2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_03_009_result : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==0 && io_pmp_mmio && unaligned2 && !within2 && !child2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_03_010_result : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==1 && child2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_03_011_result : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==2 && child2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_03_012_result : coverpoint ($isunknown((s2_ok && attributes_ok && (!child2 || child_ok))) ? 2'd2 : ((s2_ok && attributes_ok && (!child2 || child_ok)) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && tlbhit2 && !incoming_fault2 && action2==15 && !io_pmp_st && !io_pmp_ld && !flush2 && pbmt2==0 && io_pmp_mmio && child2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_03_013_result : coverpoint ($isunknown((s2_ok && !exp_actual_uncache2)) ? 2'd2 : ((s2_ok && !exp_actual_uncache2) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && incoming_fault2 && (io_pmp_mmio || pbmt2 inside {1,2}))) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_04_03 cg_storeunit_diag_04_03_inst;

    // 04.04 异常组合与阶段覆盖
    covergroup cg_storeunit_04_04 @(posedge clock);
        option.per_instance = 1;
        // 04.04.001 EXC_PF_与PMP拒绝
        // 场景：S1已有PF，S2另有P.st=1；每次隔离目标组合；采样场景不以最终异常正确为门槛；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：PF位按输入保持，AF按P.st置位；本模块不替ROB只选一个最终异常码。
        // 审计结论：完全建模。
        cp_04_04_001 : coverpoint ($isunknown((scene_04_04_001_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_04_04_001_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_04_04_001_pipe[1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.04.002 EXC_PF_抑制NC非对齐SAM
        // 场景：NC非对齐访问已有PF，无其他异常原因；每次隔离目标组合；采样场景不以最终异常正确为门槛；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：PF通过非SAM异常集合抑制NC的SAM，不新增孤立SAM。
        // 审计结论：完全建模。
        cp_04_04_002 : coverpoint ($isunknown((s2_ok && !s2_exception_new_vec_6)) ? 2'd2 : ((s2_ok && !s2_exception_new_vec_6) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && in_ex15_2 && pbmt2==1 && unaligned2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.04.003 EXC_GPF_与PMP拒绝
        // 场景：S1已有GPF，S2另有P.st=1；每次隔离目标组合；采样场景不以最终异常正确为门槛；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：GPF位按输入保持，AF按P.st置位；本模块不替ROB只选一个最终异常码。
        // 审计结论：完全建模。
        cp_04_04_003 : coverpoint ($isunknown((scene_04_04_003_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_04_04_003_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_04_04_003_pipe[1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.04.004 EXC_GPF_抑制NC非对齐SAM
        // 场景：NC非对齐访问已有GPF，无其他异常原因；每次隔离目标组合；采样场景不以最终异常正确为门槛；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：GPF通过非SAM异常集合抑制NC的SAM，不新增孤立SAM。
        // 审计结论：完全建模。
        cp_04_04_004 : coverpoint ($isunknown((s2_ok && !s2_exception_new_vec_6)) ? 2'd2 : ((s2_ok && !s2_exception_new_vec_6) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && in_ex23_2 && pbmt2==1 && unaligned2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.04.005 EXC_AF_与PMP拒绝
        // 场景：S1已有AF，S2另有P.st=1；每次隔离目标组合；采样场景不以最终异常正确为门槛；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：AF位按输入保持，AF按P.st置位；本模块不替ROB只选一个最终异常码。
        // 审计结论：完全建模。
        cp_04_04_005 : coverpoint ($isunknown((scene_04_04_005_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_04_04_005_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_04_04_005_pipe[1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.04.006 EXC_AF_抑制NC非对齐SAM
        // 场景：NC非对齐访问已有AF，无其他异常原因；每次隔离目标组合；采样场景不以最终异常正确为门槛；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：AF通过非SAM异常集合抑制NC的SAM，不新增孤立SAM。
        // 审计结论：完全建模。
        cp_04_04_006 : coverpoint ($isunknown((s2_ok && !s2_exception_new_vec_6)) ? 2'd2 : ((s2_ok && !s2_exception_new_vec_6) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && in_ex7_2 && pbmt2==1 && unaligned2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.04.007 EXC_Debug动作抑制SAM
        // 场景：NC非对齐访问触发Debug动作；每次隔离目标组合；采样场景不以最终异常正确为门槛；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：Debug模式动作属于非SAM异常路径，SAM不因NC非对齐单独新增，hasException有效。
        // 审计结论：完全建模。
        cp_04_04_007 : coverpoint ($isunknown((!s2_exception_new_vec_6 && s2_ok && attributes_ok)) ? 2'd2 : ((!s2_exception_new_vec_6 && s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && pbmt2==1 && unaligned2 && action2==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.04.008 EXC_Breakpoint与翻译fault
        // 场景：同一有效标量触发Breakpoint且翻译PF或GPF；每次隔离目标组合；采样场景不以最终异常正确为门槛；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：保留breakPoint与翻译fault位图；最终架构异常优先级属下游，不能直接删去其中一位。
        // 审计结论：完全建模。
        cp_04_04_008 : coverpoint ($isunknown((scene_04_04_008_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_04_04_008_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_04_04_008_pipe[1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.04.009 EXC_S1_MMIO非对齐到S2重分类
        // 场景：PBMT_IO非对齐在S1出现SAM，S2满足实际MMIO条件；每次隔离目标组合；采样场景不以最终异常正确为门槛；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：S1可有SAM；S2 SAM公式重写为NC条件，MMIO以AF为结果；不能把S1位图直接当最终WB位图。
        // 审计结论：完全建模。
        cp_04_04_009 : coverpoint ($isunknown((s1_sam_d1 && s2_out_af && !s2_exception_new_vec_6 && s2_ok)) ? 2'd2 : ((s1_sam_d1 && s2_out_af && !s2_exception_new_vec_6 && s2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && pbmt2==2 && unaligned2 && tlbhit2 && !incoming_fault2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_04_04 cg_storeunit_04_04_inst;
    covergroup cg_storeunit_diag_04_04 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_04_04_001_result : coverpoint ($isunknown((scene_04_04_001_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_04_04_001_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_04_04_001_pipe[1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_04_002_result : coverpoint ($isunknown((s2_ok && !s2_exception_new_vec_6)) ? 2'd2 : ((s2_ok && !s2_exception_new_vec_6) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && in_ex15_2 && pbmt2==1 && unaligned2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_04_003_result : coverpoint ($isunknown((scene_04_04_003_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_04_04_003_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_04_04_003_pipe[1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_04_004_result : coverpoint ($isunknown((s2_ok && !s2_exception_new_vec_6)) ? 2'd2 : ((s2_ok && !s2_exception_new_vec_6) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && in_ex23_2 && pbmt2==1 && unaligned2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_04_005_result : coverpoint ($isunknown((scene_04_04_005_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_04_04_005_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_04_04_005_pipe[1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_04_006_result : coverpoint ($isunknown((s2_ok && !s2_exception_new_vec_6)) ? 2'd2 : ((s2_ok && !s2_exception_new_vec_6) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && in_ex7_2 && pbmt2==1 && unaligned2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_04_007_result : coverpoint ($isunknown((!s2_exception_new_vec_6 && s2_ok && attributes_ok)) ? 2'd2 : ((!s2_exception_new_vec_6 && s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && pbmt2==1 && unaligned2 && action2==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_04_008_result : coverpoint ($isunknown((scene_04_04_008_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_04_04_008_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_04_04_008_pipe[1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_04_009_result : coverpoint ($isunknown((s1_sam_d1 && s2_out_af && !s2_exception_new_vec_6 && s2_ok)) ? 2'd2 : ((s1_sam_d1 && s2_out_af && !s2_exception_new_vec_6 && s2_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && pbmt2==2 && unaligned2 && tlbhit2 && !incoming_fault2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_04_04 cg_storeunit_diag_04_04_inst;

    // 05.01 非对齐开关与commit资格
    covergroup cg_storeunit_05_01 @(posedge clock);
        option.per_instance = 1;
        // 05.01.001 MAB_开关0
        // 场景：跨16B普通标量，已稳定的hd_misalign_st_enable=0，commit资格满足且MAB可接收；跨16B标量parent；非CBO；MAB ready按条目设置；开关使用StoreUnit内部延迟一拍值；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：候选与请求均不进入MAB；按当前RTL记录后续结果，不凭设计注释补造SAM。
        // 审计结论：完全建模。
        cp_05_01_001 : coverpoint ($isunknown((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok)) ? 2'd2 : ((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_01_001_d1)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 05.01.002 MAB_开关1
        // 场景：跨16B普通标量，已稳定的hd_misalign_st_enable=1，commit资格满足且MAB可接收；跨16B标量parent；非CBO；MAB ready按条目设置；开关使用StoreUnit内部延迟一拍值；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：候选、请求valid与实际处理资格按延迟开关启用。
        // 审计结论：完全建模。
        cp_05_01_002 : coverpoint ($isunknown((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok)) ? 2'd2 : ((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_01_002_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.01.003 MAB_资格_sq1rob0uop0
        // 场景：SQ匹配=1，ROB匹配=0，uopIdx匹配=0；开关开启且ready=1；跨16B标量parent；非CBO；MAB ready按条目设置；开关使用StoreUnit内部延迟一拍值；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：s1_misalignNeedReplay=0；允许尝试MAB admission。
        // 审计结论：完全建模。
        cp_05_01_003 : coverpoint ($isunknown((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok)) ? 2'd2 : ((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_01_003_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.01.004 MAB_资格_sq0rob1uop1
        // 场景：SQ匹配=0，ROB匹配=1，uopIdx匹配=1；开关开启且ready=1；跨16B标量parent；非CBO；MAB ready按条目设置；开关使用StoreUnit内部延迟一拍值；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：s1_misalignNeedReplay=0；允许尝试MAB admission。
        // 审计结论：完全建模。
        cp_05_01_004 : coverpoint ($isunknown((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok)) ? 2'd2 : ((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_01_004_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.01.005 MAB_资格_sq0rob1uop0
        // 场景：SQ匹配=0，ROB匹配=1，uopIdx匹配=0；开关开启且ready=1；跨16B标量parent；非CBO；MAB ready按条目设置；开关使用StoreUnit内部延迟一拍值；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：s1_misalignNeedReplay=1；MR.valid被资格不足抑制，慢反馈hit=0要求原始STA重发。
        // 审计结论：完全建模。
        cp_05_01_005 : coverpoint ($isunknown((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok)) ? 2'd2 : ((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_01_005_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.01.006 MAB_资格_sq0rob0uop1
        // 场景：SQ匹配=0，ROB匹配=0，uopIdx匹配=1；开关开启且ready=1；跨16B标量parent；非CBO；MAB ready按条目设置；开关使用StoreUnit内部延迟一拍值；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：s1_misalignNeedReplay=1；MR.valid被资格不足抑制，慢反馈hit=0要求原始STA重发。
        // 审计结论：完全建模。
        cp_05_01_006 : coverpoint ($isunknown((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok)) ? 2'd2 : ((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_01_006_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.01.007 MAB_资格_sq0rob0uop0
        // 场景：SQ匹配=0，ROB匹配=0，uopIdx匹配=0；开关开启且ready=1；跨16B标量parent；非CBO；MAB ready按条目设置；开关使用StoreUnit内部延迟一拍值；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：s1_misalignNeedReplay=1；MR.valid被资格不足抑制，慢反馈hit=0要求原始STA重发。
        // 审计结论：完全建模。
        cp_05_01_007 : coverpoint ($isunknown((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok)) ? 2'd2 : ((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_01_007_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.01.008 MAB_资格比较含SQflag
        // 场景：SQ value相同但flag相反，ROB/uop替代资格不满足；跨16B标量parent；非CBO；MAB ready按条目设置；开关使用StoreUnit内部延迟一拍值；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：完整SQ指针不匹配；不得只比低6位而允许入队。
        // 审计结论：完全建模。
        cp_05_01_008 : coverpoint ($isunknown((s1_misalignNeedReplay && replay1_ok)) ? 2'd2 : ((s1_misalignNeedReplay && replay1_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && cross1 && sqvalue_match1 && !sqmatch1 && !(robmatch1 && uopmatch1))) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.01.009 MAB_CSR切换延迟
        // 场景：连续标量跨16B请求期间改变本地hd_misalign_st_enable；跨16B标量parent；非CBO；MAB ready按条目设置；开关使用StoreUnit内部延迟一拍值；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：本模块使用一拍延迟开关；顶层CSR到本地的延迟另从实际连接取，不使用顶层当拍值替代。
        // 审计结论：完全建模。
        cp_05_01_009 : coverpoint ($isunknown((s1_toMisalignBufferValid_last_REG==enable_d1 && candidate_ok)) ? 2'd2 : ((s1_toMisalignBufferValid_last_REG==enable_d1 && candidate_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (history_valid && enable_d1!=io_csrCtrl_hd_misalign_st_enable)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_05_01 cg_storeunit_05_01_inst;
    covergroup cg_storeunit_diag_05_01 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_05_01_001_result : coverpoint ($isunknown((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok)) ? 2'd2 : ((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_01_001_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_01_002_result : coverpoint ($isunknown((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok)) ? 2'd2 : ((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_01_002_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_01_003_result : coverpoint ($isunknown((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok)) ? 2'd2 : ((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_01_003_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_01_004_result : coverpoint ($isunknown((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok)) ? 2'd2 : ((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_01_004_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_01_005_result : coverpoint ($isunknown((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok)) ? 2'd2 : ((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_01_005_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_01_006_result : coverpoint ($isunknown((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok)) ? 2'd2 : ((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_01_006_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_01_007_result : coverpoint ($isunknown((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok)) ? 2'd2 : ((candidate_ok_d1 && replay1_ok_d1 && fb_ok && strict_misalign_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_01_007_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_01_008_result : coverpoint ($isunknown((s1_misalignNeedReplay && replay1_ok)) ? 2'd2 : ((s1_misalignNeedReplay && replay1_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && cross1 && sqvalue_match1 && !sqmatch1 && !(robmatch1 && uopmatch1))) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_01_009_result : coverpoint ($isunknown((s1_toMisalignBufferValid_last_REG==enable_d1 && candidate_ok)) ? 2'd2 : ((s1_toMisalignBufferValid_last_REG==enable_d1 && candidate_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (history_valid && enable_d1!=io_csrCtrl_hd_misalign_st_enable)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_05_01 cg_storeunit_diag_05_01_inst;

    // 05.04 child回流与响应
    covergroup cg_storeunit_05_04 @(posedge clock);
        option.per_instance = 1;
        option.weight = HAS_CHILD ? 1 : 0;
        // 05.04.001 CHILD_最终标志0
        // 场景：MAB child isFinalSplit=0，本身自然对齐、正常hit且无异常；仅标量parent的child；isvec=0；只接入STA0；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：正常child以MO.valid返回；其本身自然对齐时L/RE.updateAddrValid=0；无原始STA慢反馈、无StoreUnit普通stout。
        // 审计结论：完全建模。
        cp_05_04_001 : coverpoint ($isunknown((child_ok && !io_feedback_slow_valid && update1_ok_d1 && io_lsq_replenish_updateAddrValid==final2)) ? 2'd2 : ((child_ok && !io_feedback_slow_valid && update1_ok_d1 && io_lsq_replenish_updateAddrValid==final2) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && child2 && final2==0 && !unaligned2 && tlbhit2 && !incoming_fault2 && !io_pmp_st && action2==15 && pbmt2==0 && !io_pmp_mmio && !flush2)) {
            option.weight = HAS_CHILD ? 1 : 0;
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.04.002 CHILD_最终标志1
        // 场景：MAB child isFinalSplit=1，本身自然对齐、正常hit且无异常；仅标量parent的child；isvec=0；只接入STA0；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：正常child以MO.valid返回；其本身自然对齐时L/RE.updateAddrValid=1；无原始STA慢反馈、无StoreUnit普通stout。
        // 审计结论：完全建模。
        cp_05_04_002 : coverpoint ($isunknown((child_ok && !io_feedback_slow_valid && update1_ok_d1 && io_lsq_replenish_updateAddrValid==final2)) ? 2'd2 : ((child_ok && !io_feedback_slow_valid && update1_ok_d1 && io_lsq_replenish_updateAddrValid==final2) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && child2 && final2==1 && !unaligned2 && tlbhit2 && !incoming_fault2 && !io_pmp_st && action2==15 && pbmt2==0 && !io_pmp_mmio && !flush2)) {
            option.weight = HAS_CHILD ? 1 : 0;
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_05_04 cg_storeunit_05_04_inst;
    covergroup cg_storeunit_diag_05_04 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_05_04_001_result : coverpoint ($isunknown((child_ok && !io_feedback_slow_valid && update1_ok_d1 && io_lsq_replenish_updateAddrValid==final2)) ? 2'd2 : ((child_ok && !io_feedback_slow_valid && update1_ok_d1 && io_lsq_replenish_updateAddrValid==final2) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && child2 && final2==0 && !unaligned2 && tlbhit2 && !incoming_fault2 && !io_pmp_st && action2==15 && pbmt2==0 && !io_pmp_mmio && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_04_002_result : coverpoint ($isunknown((child_ok && !io_feedback_slow_valid && update1_ok_d1 && io_lsq_replenish_updateAddrValid==final2)) ? 2'd2 : ((child_ok && !io_feedback_slow_valid && update1_ok_d1 && io_lsq_replenish_updateAddrValid==final2) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && child2 && final2==1 && !unaligned2 && tlbhit2 && !incoming_fault2 && !io_pmp_st && action2==15 && pbmt2==0 && !io_pmp_mmio && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_05_04 cg_storeunit_diag_05_04_inst;

    // 06.01 Trigger使能与比较
    covergroup cg_storeunit_06_01 @(posedge clock);
        option.per_instance = 1;
        // 06.01.001 TRIG_entry0_独立命中
        // 场景：只使能trigger entry0的store地址相等比较，配置Breakpoint动作；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：有权限报告Breakpoint时action=0且exceptionVec[3]=1；各entry独立观察。
        // 审计结论：完全建模。
        cp_06_01_001 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==4'b0001 && trig_hit[0] && trig_action_cfg_0==0 && io_fromCsrTrigger_triggerCanRaiseBpExp)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.01.002 TRIG_entry1_独立命中
        // 场景：只使能trigger entry1的store地址相等比较，配置Breakpoint动作；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：有权限报告Breakpoint时action=0且exceptionVec[3]=1；各entry独立观察。
        // 审计结论：完全建模。
        cp_06_01_002 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==4'b0010 && trig_hit[1] && trig_action_cfg_1==0 && io_fromCsrTrigger_triggerCanRaiseBpExp)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.01.003 TRIG_entry2_独立命中
        // 场景：只使能trigger entry2的store地址相等比较，配置Breakpoint动作；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：有权限报告Breakpoint时action=0且exceptionVec[3]=1；各entry独立观察。
        // 审计结论：完全建模。
        cp_06_01_003 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==4'b0100 && trig_hit[2] && trig_action_cfg_2==0 && io_fromCsrTrigger_triggerCanRaiseBpExp)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.01.004 TRIG_entry3_独立命中
        // 场景：只使能trigger entry3的store地址相等比较，配置Breakpoint动作；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：有权限报告Breakpoint时action=0且exceptionVec[3]=1；各entry独立观察。
        // 审计结论：完全建模。
        cp_06_01_004 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==4'b1000 && trig_hit[3] && trig_action_cfg_3==0 && io_fromCsrTrigger_triggerCanRaiseBpExp)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.01.005 TRIG_EQ_equal
        // 场景：单entry地址比较为EQ，访问地址相对tdata2为equal；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：目标entry命中并形成所配置动作。
        // 审计结论：完全建模。
        cp_06_01_005 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==0 && va1==trig_addr0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.01.006 TRIG_EQ_different
        // 场景：单entry地址比较为EQ，访问地址相对tdata2为different；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：目标entry不命中，其他entry关闭时action=15。
        // 审计结论：完全建模。
        cp_06_01_006 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==0 && va1!=trig_addr0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.01.007 TRIG_GE_equal
        // 场景：单entry地址比较为GE，访问地址相对tdata2为equal；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：目标entry命中并形成所配置动作。
        // 审计结论：完全建模。
        cp_06_01_007 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==2 && va1==trig_addr0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.01.008 TRIG_GE_above
        // 场景：单entry地址比较为GE，访问地址相对tdata2为above；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：目标entry命中并形成所配置动作。
        // 审计结论：完全建模。
        cp_06_01_008 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==2 && va1>trig_addr0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.01.009 TRIG_GE_below
        // 场景：单entry地址比较为GE，访问地址相对tdata2为below；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：目标entry不命中，其他entry关闭时action=15。
        // 审计结论：完全建模。
        cp_06_01_009 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==2 && va1<trig_addr0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.01.010 TRIG_LT_equal
        // 场景：单entry地址比较为LT，访问地址相对tdata2为equal；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：目标entry不命中，其他entry关闭时action=15。
        // 审计结论：完全建模。
        cp_06_01_010 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==3 && va1==trig_addr0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.01.011 TRIG_LT_below
        // 场景：单entry地址比较为LT，访问地址相对tdata2为below；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：目标entry命中并形成所配置动作。
        // 审计结论：完全建模。
        cp_06_01_011 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==3 && va1<trig_addr0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.01.012 TRIG_LT_above
        // 场景：单entry地址比较为LT，访问地址相对tdata2为above；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：目标entry不命中，其他entry关闭时action=15。
        // 审计结论：完全建模。
        cp_06_01_012 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==3 && va1>trig_addr0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.01.013 TRIG_抑制_tEnable=0
        // 场景：地址本可匹配，但tEnable=0，其他entry均关闭；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：目标entry不产生动作，action=15；select=1路径仅作不支持数据触发的输入隔离观察。
        // 审计结论：完全建模。
        cp_06_01_013 : coverpoint ($isunknown((trigger_ok && trigger_expected==15)) ? 2'd2 : ((trigger_ok && trigger_expected==15) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && va1==trig_addr0 && !io_fromCsrTrigger_tEnableVec_0 && io_fromCsrTrigger_tEnableVec_1==0 && io_fromCsrTrigger_tEnableVec_2==0 && io_fromCsrTrigger_tEnableVec_3==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.01.014 TRIG_抑制_store=0
        // 场景：地址本可匹配，但store=0，其他entry均关闭；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：目标entry不产生动作，action=15；select=1路径仅作不支持数据触发的输入隔离观察。
        // 审计结论：完全建模。
        cp_06_01_014 : coverpoint ($isunknown((trigger_ok && trigger_expected==15)) ? 2'd2 : ((trigger_ok && trigger_expected==15) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && va1==trig_addr0 && !io_fromCsrTrigger_tdataVec_0_store && trigger_single==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.01.015 TRIG_抑制_select=1
        // 场景：地址本可匹配，但select=1，其他entry均关闭；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：目标entry不产生动作，action=15；select=1路径仅作不支持数据触发的输入隔离观察。
        // 审计结论：完全建模。
        cp_06_01_015 : coverpoint ($isunknown((trigger_ok && trigger_expected==15)) ? 2'd2 : ((trigger_ok && trigger_expected==15) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && va1==trig_addr0 && io_fromCsrTrigger_tdataVec_0_select && trigger_single==1)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 06.01.016 TRIG_抑制_debugMode=1
        // 场景：地址本可匹配，但debugMode=1，其他entry均关闭；普通标量；无TLB异常；未测entry关闭；trigger=None编码为15；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：目标entry不产生动作，action=15；select=1路径仅作不支持数据触发的输入隔离观察。
        // 审计结论：完全建模。
        cp_06_01_016 : coverpoint ($isunknown((trigger_ok && trigger_expected==15)) ? 2'd2 : ((trigger_ok && trigger_expected==15) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && va1==trig_addr0 && io_fromCsrTrigger_debugMode && trigger_single==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_06_01 cg_storeunit_06_01_inst;
    covergroup cg_storeunit_diag_06_01 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_06_01_001_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==4'b0001 && trig_hit[0] && trig_action_cfg_0==0 && io_fromCsrTrigger_triggerCanRaiseBpExp)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_01_002_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==4'b0010 && trig_hit[1] && trig_action_cfg_1==0 && io_fromCsrTrigger_triggerCanRaiseBpExp)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_01_003_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==4'b0100 && trig_hit[2] && trig_action_cfg_2==0 && io_fromCsrTrigger_triggerCanRaiseBpExp)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_01_004_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==4'b1000 && trig_hit[3] && trig_action_cfg_3==0 && io_fromCsrTrigger_triggerCanRaiseBpExp)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_01_005_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==0 && va1==trig_addr0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_01_006_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==0 && va1!=trig_addr0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_01_007_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==2 && va1==trig_addr0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_01_008_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==2 && va1>trig_addr0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_01_009_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==2 && va1<trig_addr0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_01_010_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==3 && va1==trig_addr0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_01_011_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==3 && va1<trig_addr0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_01_012_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && trigger_single==1 && trig_mode0==3 && va1>trig_addr0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_01_013_result : coverpoint ($isunknown((trigger_ok && trigger_expected==15)) ? 2'd2 : ((trigger_ok && trigger_expected==15) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && va1==trig_addr0 && !io_fromCsrTrigger_tEnableVec_0 && io_fromCsrTrigger_tEnableVec_1==0 && io_fromCsrTrigger_tEnableVec_2==0 && io_fromCsrTrigger_tEnableVec_3==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_01_014_result : coverpoint ($isunknown((trigger_ok && trigger_expected==15)) ? 2'd2 : ((trigger_ok && trigger_expected==15) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && va1==trig_addr0 && !io_fromCsrTrigger_tdataVec_0_store && trigger_single==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_01_015_result : coverpoint ($isunknown((trigger_ok && trigger_expected==15)) ? 2'd2 : ((trigger_ok && trigger_expected==15) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && va1==trig_addr0 && io_fromCsrTrigger_tdataVec_0_select && trigger_single==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_01_016_result : coverpoint ($isunknown((trigger_ok && trigger_expected==15)) ? 2'd2 : ((trigger_ok && trigger_expected==15) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && va1==trig_addr0 && io_fromCsrTrigger_debugMode && trigger_single==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_06_01 cg_storeunit_diag_06_01_inst;

    // 06.02 动作、chain和异常交接
    covergroup cg_storeunit_06_02 @(posedge clock);
        option.per_instance = 1;
        // 06.02.001 TRIG_无动作
        // 场景：无entry可fire；明确目标entry和地址关系；逐条配置使能、动作、许可或chain，不将未命中项上提为匹配条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：action=15、breakPoint位=0；正常标量路径向LSQ及WB传递15。
        // 审计结论：完全建模。
        cp_06_02_001 : coverpoint ($isunknown((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok)) ? 2'd2 : ((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_06_02_001_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.02.002 TRIG_断点动作
        // 场景：entry可fire且action=0，triggerCanRaiseBpExp=1；明确目标entry和地址关系；逐条配置使能、动作、许可或chain，不将未命中项上提为匹配条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：action=0、breakPoint位=1；S2按异常路径处理。
        // 审计结论：完全建模。
        cp_06_02_002 : coverpoint ($isunknown((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok)) ? 2'd2 : ((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_06_02_002_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.02.003 TRIG_断点禁止
        // 场景：entry可fire且action=0，triggerCanRaiseBpExp=0；明确目标entry和地址关系；逐条配置使能、动作、许可或chain，不将未命中项上提为匹配条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：不报告断点，action=15、breakPoint位=0。
        // 审计结论：完全建模。
        cp_06_02_003 : coverpoint ($isunknown((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok)) ? 2'd2 : ((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_06_02_003_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.02.004 TRIG_Debug动作
        // 场景：entry可fire且action=1；明确目标entry和地址关系；逐条配置使能、动作、许可或chain，不将未命中项上提为匹配条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：action=1；不是breakPoint位；S2通过trigger_debug_mode形成hasException。
        // 审计结论：完全建模。
        cp_06_02_004 : coverpoint ($isunknown((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok)) ? 2'd2 : ((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_06_02_004_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.02.005 TRIG_Debug不依赖断点许可
        // 场景：Debug命中且triggerCanRaiseBpExp=0；明确目标entry和地址关系；逐条配置使能、动作、许可或chain，不将未命中项上提为匹配条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：仍选择Debug动作=1；断点许可仅门控Breakpoint。
        // 审计结论：完全建模。
        cp_06_02_005 : coverpoint ($isunknown((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok)) ? 2'd2 : ((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_06_02_005_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.02.006 TRIG_Debug优先断点
        // 场景：不同entry分别命中Debug和Breakpoint；明确目标entry和地址关系；逐条配置使能、动作、许可或chain，不将未命中项上提为匹配条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：选择Debug=1，不能因为Breakpoint也命中而输出0。
        // 审计结论：完全建模。
        cp_06_02_006 : coverpoint ($isunknown((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok)) ? 2'd2 : ((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_06_02_006_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.02.007 TRIG_chain两项均命中
        // 场景：相邻两entry组成合法chain且均命中；明确目标entry和地址关系；逐条配置使能、动作、许可或chain，不将未命中项上提为匹配条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：末entry结束chain后才可fire并形成所配动作。
        // 审计结论：完全建模。
        cp_06_02_007_chain0_1 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_0_chain && !io_fromCsrTrigger_tdataVec_1_chain && trig_hit[0]==1 && trig_hit[1]==1 && trigger_single==3)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.02.007 TRIG_chain两项均命中
        // 场景：相邻两entry组成合法chain且均命中；明确目标entry和地址关系；逐条配置使能、动作、许可或chain，不将未命中项上提为匹配条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：末entry结束chain后才可fire并形成所配动作。
        // 审计结论：完全建模。
        cp_06_02_007_chain1_2 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_1_chain && !io_fromCsrTrigger_tdataVec_2_chain && trig_hit[1]==1 && trig_hit[2]==1 && trigger_single==6)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.02.007 TRIG_chain两项均命中
        // 场景：相邻两entry组成合法chain且均命中；明确目标entry和地址关系；逐条配置使能、动作、许可或chain，不将未命中项上提为匹配条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：末entry结束chain后才可fire并形成所配动作。
        // 审计结论：完全建模。
        cp_06_02_007_chain2_3 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_2_chain && !io_fromCsrTrigger_tdataVec_3_chain && trig_hit[2]==1 && trig_hit[3]==1 && trigger_single==12)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.02.008 TRIG_chain前项不命中
        // 场景：chain前entry不命中而末entry命中；明确目标entry和地址关系；逐条配置使能、动作、许可或chain，不将未命中项上提为匹配条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：chain不完整，不能只凭末entry命中形成动作。
        // 审计结论：完全建模。
        cp_06_02_008_chain0_1 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_0_chain && !io_fromCsrTrigger_tdataVec_1_chain && trig_hit[0]==0 && trig_hit[1]==1 && trigger_single==3)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.02.008 TRIG_chain前项不命中
        // 场景：chain前entry不命中而末entry命中；明确目标entry和地址关系；逐条配置使能、动作、许可或chain，不将未命中项上提为匹配条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：chain不完整，不能只凭末entry命中形成动作。
        // 审计结论：完全建模。
        cp_06_02_008_chain1_2 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_1_chain && !io_fromCsrTrigger_tdataVec_2_chain && trig_hit[1]==0 && trig_hit[2]==1 && trigger_single==6)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.02.008 TRIG_chain前项不命中
        // 场景：chain前entry不命中而末entry命中；明确目标entry和地址关系；逐条配置使能、动作、许可或chain，不将未命中项上提为匹配条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：chain不完整，不能只凭末entry命中形成动作。
        // 审计结论：完全建模。
        cp_06_02_008_chain2_3 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_2_chain && !io_fromCsrTrigger_tdataVec_3_chain && trig_hit[2]==0 && trig_hit[3]==1 && trigger_single==12)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.02.009 TRIG_chain末项不命中
        // 场景：chain前entry命中而末entry不命中；明确目标entry和地址关系；逐条配置使能、动作、许可或chain，不将未命中项上提为匹配条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：不产生chain动作。
        // 审计结论：完全建模。
        cp_06_02_009_chain0_1 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_0_chain && !io_fromCsrTrigger_tdataVec_1_chain && trig_hit[0]==1 && trig_hit[1]==0 && trigger_single==3)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.02.009 TRIG_chain末项不命中
        // 场景：chain前entry命中而末entry不命中；明确目标entry和地址关系；逐条配置使能、动作、许可或chain，不将未命中项上提为匹配条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：不产生chain动作。
        // 审计结论：完全建模。
        cp_06_02_009_chain1_2 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_1_chain && !io_fromCsrTrigger_tdataVec_2_chain && trig_hit[1]==1 && trig_hit[2]==0 && trigger_single==6)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.02.009 TRIG_chain末项不命中
        // 场景：chain前entry命中而末entry不命中；明确目标entry和地址关系；逐条配置使能、动作、许可或chain，不将未命中项上提为匹配条件；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：不产生chain动作。
        // 审计结论：完全建模。
        cp_06_02_009_chain2_3 : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_2_chain && !io_fromCsrTrigger_tdataVec_3_chain && trig_hit[2]==1 && trig_hit[3]==0 && trigger_single==12)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_06_02 cg_storeunit_06_02_inst;
    covergroup cg_storeunit_diag_06_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_06_02_001_result : coverpoint ($isunknown((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok)) ? 2'd2 : ((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_06_02_001_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_02_002_result : coverpoint ($isunknown((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok)) ? 2'd2 : ((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_06_02_002_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_02_003_result : coverpoint ($isunknown((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok)) ? 2'd2 : ((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_06_02_003_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_02_004_result : coverpoint ($isunknown((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok)) ? 2'd2 : ((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_06_02_004_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_02_005_result : coverpoint ($isunknown((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok)) ? 2'd2 : ((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_06_02_005_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_02_006_result : coverpoint ($isunknown((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok)) ? 2'd2 : ((trigger_ok_d1 && s1_exception_ok_d1 && s2_ok && attributes_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_06_02_006_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_02_007_chain0_1_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_0_chain && !io_fromCsrTrigger_tdataVec_1_chain && trig_hit[0]==1 && trig_hit[1]==1 && trigger_single==3)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_02_007_chain1_2_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_1_chain && !io_fromCsrTrigger_tdataVec_2_chain && trig_hit[1]==1 && trig_hit[2]==1 && trigger_single==6)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_02_007_chain2_3_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_2_chain && !io_fromCsrTrigger_tdataVec_3_chain && trig_hit[2]==1 && trig_hit[3]==1 && trigger_single==12)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_02_008_chain0_1_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_0_chain && !io_fromCsrTrigger_tdataVec_1_chain && trig_hit[0]==0 && trig_hit[1]==1 && trigger_single==3)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_02_008_chain1_2_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_1_chain && !io_fromCsrTrigger_tdataVec_2_chain && trig_hit[1]==0 && trig_hit[2]==1 && trigger_single==6)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_02_008_chain2_3_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_2_chain && !io_fromCsrTrigger_tdataVec_3_chain && trig_hit[2]==0 && trig_hit[3]==1 && trigger_single==12)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_02_009_chain0_1_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_0_chain && !io_fromCsrTrigger_tdataVec_1_chain && trig_hit[0]==1 && trig_hit[1]==0 && trigger_single==3)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_02_009_chain1_2_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_1_chain && !io_fromCsrTrigger_tdataVec_2_chain && trig_hit[1]==1 && trig_hit[2]==0 && trigger_single==6)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_02_009_chain2_3_result : coverpoint ($isunknown((trigger_ok && s1_exception_ok)) ? 2'd2 : ((trigger_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && io_fromCsrTrigger_tdataVec_2_chain && !io_fromCsrTrigger_tdataVec_3_chain && trig_hit[2]==1 && trig_hit[3]==0 && trigger_single==12)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_06_02 cg_storeunit_diag_06_02_inst;

    // 06.03 非对齐窗口及CBO触发
    covergroup cg_storeunit_06_03 @(posedge clock);
        option.per_instance = 1;
        // 06.03.001 TRIG_同16B非对齐_mask内
        // 场景：SD同16B非对齐使is128bit=1，tdata2落在同16B窗口的有效mask字节；scalar的isvec=0；目标entry开启store、select=0、debugMode=0、EQ、Breakpoint且有许可；其余entry关闭；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：is128bit不使scalar进入vector-stride比较；EQ仍要求完整VA等于tdata2，mask内但地址不同也不命中。
        // 审计结论：完全建模。
        cp_06_03_001 : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && within1 && va1[49:4]==trig_addr0[49:4] && mask1[trig_addr0[3:0]]==1 && trigger_single==1 && trig_mode0==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.03.002 TRIG_同16B非对齐_mask外
        // 场景：SD同16B非对齐使is128bit=1，tdata2落在同16B窗口的无效mask字节；scalar的isvec=0；目标entry开启store、select=0、debugMode=0、EQ、Breakpoint且有许可；其余entry关闭；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：scalar使用完整地址EQ；mask不参与独立触发资格，不因窗口相同而命中。
        // 审计结论：完全建模。
        cp_06_03_002 : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && within1 && va1[49:4]==trig_addr0[49:4] && mask1[trig_addr0[3:0]]==0 && trigger_single==1 && trig_mode0==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.03.003 TRIG_CBO.zero_同行不同偏移
        // 场景：CBO.zero地址与tdata2低6位不同但cacheline相同；scalar的isvec=0；目标entry开启store、select=0、debugMode=0、EQ、Breakpoint且有许可；其余entry关闭；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：CBO按cacheline匹配，不要求普通标量起始VA完全相同。
        // 审计结论：完全建模。
        cp_06_03_003 : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==7 && va1[49:6]==trig_addr0[49:6] && va1[5:0]!=trig_addr0[5:0] && trigger_single==1 && trig_mode0==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.03.004 TRIG_CBO.clean_同行不同偏移
        // 场景：CBO.clean地址与tdata2低6位不同但cacheline相同；scalar的isvec=0；目标entry开启store、select=0、debugMode=0、EQ、Breakpoint且有许可；其余entry关闭；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：CBO按cacheline匹配，不要求普通标量起始VA完全相同。
        // 审计结论：完全建模。
        cp_06_03_004 : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==12 && va1[49:6]==trig_addr0[49:6] && va1[5:0]!=trig_addr0[5:0] && trigger_single==1 && trig_mode0==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.03.005 TRIG_CBO.flush_同行不同偏移
        // 场景：CBO.flush地址与tdata2低6位不同但cacheline相同；scalar的isvec=0；目标entry开启store、select=0、debugMode=0、EQ、Breakpoint且有许可；其余entry关闭；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：CBO按cacheline匹配，不要求普通标量起始VA完全相同。
        // 审计结论：完全建模。
        cp_06_03_005 : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==13 && va1[49:6]==trig_addr0[49:6] && va1[5:0]!=trig_addr0[5:0] && trigger_single==1 && trig_mode0==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.03.006 TRIG_CBO.inval_同行不同偏移
        // 场景：CBO.inval地址与tdata2低6位不同但cacheline相同；scalar的isvec=0；目标entry开启store、select=0、debugMode=0、EQ、Breakpoint且有许可；其余entry关闭；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：CBO按cacheline匹配，不要求普通标量起始VA完全相同。
        // 审计结论：完全建模。
        cp_06_03_006 : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==14 && va1[49:6]==trig_addr0[49:6] && va1[5:0]!=trig_addr0[5:0] && trigger_single==1 && trig_mode0==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.03.007 TRIG_CBO不同行
        // 场景：CBO地址与tdata2处于不同cacheline；scalar的isvec=0；目标entry开启store、select=0、debugMode=0、EQ、Breakpoint且有许可；其余entry关闭；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该entry不命中；不得只比较16B窗口或低偏移。
        // 审计结论：完全建模。
        cp_06_03_007 : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && cbo1 && va1[49:6]!=trig_addr0[49:6] && trigger_single==1 && trig_mode0==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_06_03 cg_storeunit_06_03_inst;
    covergroup cg_storeunit_diag_06_03 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_06_03_001_result : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && within1 && va1[49:4]==trig_addr0[49:4] && mask1[trig_addr0[3:0]]==1 && trigger_single==1 && trig_mode0==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_03_002_result : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && within1 && va1[49:4]==trig_addr0[49:4] && mask1[trig_addr0[3:0]]==0 && trigger_single==1 && trig_mode0==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_03_003_result : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==7 && va1[49:6]==trig_addr0[49:6] && va1[5:0]!=trig_addr0[5:0] && trigger_single==1 && trig_mode0==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_03_004_result : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==12 && va1[49:6]==trig_addr0[49:6] && va1[5:0]!=trig_addr0[5:0] && trigger_single==1 && trig_mode0==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_03_005_result : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==13 && va1[49:6]==trig_addr0[49:6] && va1[5:0]!=trig_addr0[5:0] && trigger_single==1 && trig_mode0==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_03_006_result : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && op1==14 && va1[49:6]==trig_addr0[49:6] && va1[5:0]!=trig_addr0[5:0] && trigger_single==1 && trig_mode0==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_03_007_result : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && cbo1 && va1[49:6]!=trig_addr0[49:6] && trigger_single==1 && trig_mode0==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_06_03 cg_storeunit_diag_06_03_inst;

    // 06.04 Store–Load依赖查询
    covergroup cg_storeunit_06_04 @(posedge clock);
        option.per_instance = 1;
        // 06.04.001 NUKE_自然对齐普通Store
        // 场景：有效S1为自然对齐普通Store，无TLB miss；采样真实S1；nuke.valid不直接门控redirect或exception；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：N.valid=1、matchType=Normal=0；paddr/mask/完整ROB取当前S1事务。
        // 审计结论：完全建模。
        cp_06_04_001 : coverpoint ($isunknown((nuke_ok)) ? 2'd2 : ((nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && !unaligned1 && !miss1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.04.002 NUKE_同16B非对齐普通Store
        // 场景：有效S1为同16B非对齐普通Store，无TLB miss；采样真实S1；nuke.valid不直接门控redirect或exception；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：N.valid=1、matchType=QuadWord=1；paddr/mask/完整ROB取当前S1事务。
        // 审计结论：完全建模。
        cp_06_04_002 : coverpoint ($isunknown((nuke_ok)) ? 2'd2 : ((nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && within1 && !miss1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.04.003 NUKE_CBO
        // 场景：有效S1为CBO，无TLB miss；采样真实S1；nuke.valid不直接门控redirect或exception；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：N.valid=1、matchType=CacheLine=2；paddr/mask/完整ROB取当前S1事务。
        // 审计结论：完全建模。
        cp_06_04_003 : coverpoint ($isunknown((nuke_ok)) ? 2'd2 : ((nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && cbo1 && !miss1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.04.004 NUKE_TLBmiss抑制
        // 场景：有效S1普通标量TLB miss；采样真实S1；nuke.valid不直接门控redirect或exception；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：N.valid=0；不使用缺失翻译的物理地址发依赖查询。
        // 审计结论：完全建模。
        cp_06_04_004 : coverpoint ($isunknown((nuke_ok)) ? 2'd2 : ((nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && miss1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.04.005 NUKE_MABchild
        // 场景：标量MAB child在S1有效hit；采样真实S1；nuke.valid不直接门控redirect或exception；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：child仍可发nuke，匹配类型由child的is128bit/CBO标志决定，不笼统屏蔽MAB。
        // 审计结论：完全建模。
        cp_06_04_005 : coverpoint ($isunknown((nuke_ok)) ? 2'd2 : ((nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && child1 && hit1)) {
            option.weight = HAS_CHILD ? 1 : 0;
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.04.006 NUKE_异常或redirect同拍
        // 场景：S1非miss同时出现异常或needFlush；采样真实S1；nuke.valid不直接门控redirect或exception；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：当前RTL的nuke.valid仍按S1与非miss条件输出；下游按其年龄/取消协议处理，不假设本地valid即时清零。
        // 审计结论：完全建模。
        cp_06_04_006 : coverpoint ($isunknown((nuke_ok)) ? 2'd2 : ((nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !miss1 && (exp_exception1 || flush1))) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_06_04 cg_storeunit_06_04_inst;
    covergroup cg_storeunit_diag_06_04 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_06_04_001_result : coverpoint ($isunknown((nuke_ok)) ? 2'd2 : ((nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !cbo1 && !unaligned1 && !miss1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_04_002_result : coverpoint ($isunknown((nuke_ok)) ? 2'd2 : ((nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && within1 && !miss1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_04_003_result : coverpoint ($isunknown((nuke_ok)) ? 2'd2 : ((nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && cbo1 && !miss1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_04_004_result : coverpoint ($isunknown((nuke_ok)) ? 2'd2 : ((nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && miss1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_04_005_result : coverpoint ($isunknown((nuke_ok)) ? 2'd2 : ((nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && child1 && hit1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_04_006_result : coverpoint ($isunknown((nuke_ok)) ? 2'd2 : ((nuke_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && !miss1 && (exp_exception1 || flush1))) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_06_04 cg_storeunit_diag_06_04_inst;

    // 07.01 S0掩码出口
    covergroup cg_storeunit_07_01 @(posedge clock);
        option.per_instance = 1;
        // 07.01.001 SQMASK_标量选中
        // 场景：普通标量获选且无redirect；检查真实st_mask_out.valid；scalar与MAB来源分开；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：mask.valid=1，mask和sqIdx.value来自当前请求。
        // 审计结论：完全建模。
        cp_07_01_001 : coverpoint ($isunknown((maskout_ok)) ? 2'd2 : ((maskout_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && !flush0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.01.002 SQMASK_标量被向量挡住
        // 场景：R.valid=1但V获选；检查真实st_mask_out.valid；scalar与MAB来源分开；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：mask仍可valid，但属于向量，不能计为被阻挡标量的mask。
        // 审计结论：完全建模。
        cp_07_01_002 : coverpoint ($isunknown((!io_stin_ready && io_st_mask_out_valid && io_st_mask_out_bits_mask==io_vecstin_bits_mask && io_st_mask_out_bits_sqIdx_value==io_vecstin_bits_uop_sqIdx_value)) ? 2'd2 : ((!io_stin_ready && io_st_mask_out_valid && io_st_mask_out_bits_mask==io_vecstin_bits_mask && io_st_mask_out_bits_sqIdx_value==io_vecstin_bits_uop_sqIdx_value) ? 2'd1 : 2'd0))
            iff (sample_enable && (io_stin_valid && io_vecstin_valid && !io_misalign_stin_valid)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.01.003 SQMASK_标量被MAB挡住
        // 场景：MAB child获选且R仍valid；检查真实st_mask_out.valid；scalar与MAB来源分开；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：st_mask_out.valid=0，不为child重复写原始标量mask。
        // 审计结论：完全建模。
        cp_07_01_003 : coverpoint ($isunknown((maskout_ok)) ? 2'd2 : ((maskout_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_child && io_stin_valid)) {
            option.weight = HAS_CHILD ? 1 : 0;
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.01.004 SQMASK_redirect同拍
        // 场景：标量候选获选但S0 needFlush=1；检查真实st_mask_out.valid；scalar与MAB来源分开；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：mask.valid仍可能为1而s0_fire=0；后续由SQ/redirect生命周期解释，不建立正常推进token。
        // 审计结论：完全建模。
        cp_07_01_004 : coverpoint ($isunknown((maskout_ok)) ? 2'd2 : ((maskout_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && flush0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.01.005 SQMASK_SQ边界索引
        // 场景：请求使用SQ value=0或63；检查真实st_mask_out.valid；scalar与MAB来源分开；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：mask.sqIdx仅有低6位；flag从其他身份来源保存。
        // 审计结论：方案需修订。原测试点将SQ value=63视为合法边界，但当前深度56；应修订正常最大索引为55，63只作非法输入诊断。
        cp_07_01_005 : coverpoint ($isunknown((maskout_ok)) ? 2'd2 : ((maskout_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && req_sq[5:0] inside {0,63})) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_07_01 cg_storeunit_07_01_inst;
    covergroup cg_storeunit_diag_07_01 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_07_01_001_result : coverpoint ($isunknown((maskout_ok)) ? 2'd2 : ((maskout_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && !flush0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_01_002_result : coverpoint ($isunknown((!io_stin_ready && io_st_mask_out_valid && io_st_mask_out_bits_mask==io_vecstin_bits_mask && io_st_mask_out_bits_sqIdx_value==io_vecstin_bits_uop_sqIdx_value)) ? 2'd2 : ((!io_stin_ready && io_st_mask_out_valid && io_st_mask_out_bits_mask==io_vecstin_bits_mask && io_st_mask_out_bits_sqIdx_value==io_vecstin_bits_uop_sqIdx_value) ? 2'd1 : 2'd0))
            iff (sample_enable && (io_stin_valid && io_vecstin_valid && !io_misalign_stin_valid)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_01_003_result : coverpoint ($isunknown((maskout_ok)) ? 2'd2 : ((maskout_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_child && io_stin_valid)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_01_004_result : coverpoint ($isunknown((maskout_ok)) ? 2'd2 : ((maskout_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && flush0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_01_005_result : coverpoint ($isunknown((maskout_ok)) ? 2'd2 : ((maskout_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && req_sq[5:0] inside {0,63})) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_07_01 cg_storeunit_diag_07_01_inst;

    // 07.02 S1地址包与可用标志
    covergroup cg_storeunit_07_02 @(posedge clock);
        option.per_instance = 1;
        // 07.02.001 LSQ_正常对齐
        // 场景：普通对齐标量hit且无异常；L为Valid sideband无ready；SQ消费需结合miss与updateAddrValid；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：L.valid=1、miss=0、updateAddrValid=1，地址/掩码/ROB/SQ对应本token。
        // 审计结论：完全建模。
        cp_07_02_001 : coverpoint ($isunknown((update1_ok && id1_ok && addr1_ok && s1_exception_ok)) ? 2'd2 : ((update1_ok && id1_ok && addr1_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && hit1 && !unaligned1 && !exp_exception1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.02.003 LSQ_跨16B资格通过
        // 场景：cross16 parent满足commit资格且无异常；L为Valid sideband无ready；SQ消费需结合miss与updateAddrValid；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：L.isMisalign=1，updateAddrValid通常为0，等待拆分/后续交接。
        // 审计结论：完全建模。
        cp_07_02_003 : coverpoint ($isunknown((update1_ok && id1_ok && addr1_ok && s1_exception_ok)) ? 2'd2 : ((update1_ok && id1_ok && addr1_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && cross1 && qualified1 && !exp_exception1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.02.004 LSQ_跨16B资格不足
        // 场景：cross16 parent不满足commit资格；L为Valid sideband无ready；SQ消费需结合miss与updateAddrValid；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：L.isMisalign被s1_misalignNeedReplay抑制；不可将其误解为访问天然对齐。
        // 审计结论：完全建模。
        cp_07_02_004 : coverpoint ($isunknown((update1_ok && id1_ok && addr1_ok && s1_exception_ok)) ? 2'd2 : ((update1_ok && id1_ok && addr1_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && cross1 && !qualified1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.02.005 LSQ_TLBmiss
        // 场景：普通标量TLB miss；L为Valid sideband无ready；SQ消费需结合miss与updateAddrValid；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：L.valid仍为1、miss=1；SQ不得将此地址写成有效翻译。
        // 审计结论：完全建模。
        cp_07_02_005 : coverpoint ($isunknown((update1_ok && id1_ok && addr1_ok && s1_exception_ok)) ? 2'd2 : ((update1_ok && id1_ok && addr1_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && miss1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.02.006 LSQ_S1异常
        // 场景：S1出现PF/GPF/AF或Breakpoint；L为Valid sideband无ready；SQ消费需结合miss与updateAddrValid；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：updateAddrValid可由异常分支置位；异常结果不等于正常可写内存。
        // 审计结论：完全建模。
        cp_07_02_006 : coverpoint ($isunknown((update1_ok && id1_ok && addr1_ok && s1_exception_ok)) ? 2'd2 : ((update1_ok && id1_ok && addr1_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && exp_exception1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.02.009 LSQ_无有效S1
        // 场景：无S1有效token但payload保留前次地址；L为Valid sideband无ready；SQ消费需结合miss与updateAddrValid；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：L.valid=0；不能用残留地址/exception payload形成SQ更新。
        // 审计结论：完全建模。
        cp_07_02_009 : coverpoint ($isunknown((!io_lsq_valid)) ? 2'd2 : ((!io_lsq_valid) ? 2'd1 : 2'd0))
            iff (sample_enable && (!expected1 && !vector_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_07_02 cg_storeunit_07_02_inst;
    covergroup cg_storeunit_diag_07_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_07_02_001_result : coverpoint ($isunknown((update1_ok && id1_ok && addr1_ok && s1_exception_ok)) ? 2'd2 : ((update1_ok && id1_ok && addr1_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && hit1 && !unaligned1 && !exp_exception1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_02_003_result : coverpoint ($isunknown((update1_ok && id1_ok && addr1_ok && s1_exception_ok)) ? 2'd2 : ((update1_ok && id1_ok && addr1_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && cross1 && qualified1 && !exp_exception1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_02_004_result : coverpoint ($isunknown((update1_ok && id1_ok && addr1_ok && s1_exception_ok)) ? 2'd2 : ((update1_ok && id1_ok && addr1_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && cross1 && !qualified1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_02_005_result : coverpoint ($isunknown((update1_ok && id1_ok && addr1_ok && s1_exception_ok)) ? 2'd2 : ((update1_ok && id1_ok && addr1_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && miss1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_02_006_result : coverpoint ($isunknown((update1_ok && id1_ok && addr1_ok && s1_exception_ok)) ? 2'd2 : ((update1_ok && id1_ok && addr1_ok && s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && exp_exception1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_02_009_result : coverpoint ($isunknown((!io_lsq_valid)) ? 2'd2 : ((!io_lsq_valid) ? 2'd1 : 2'd0))
            iff (sample_enable && (!expected1 && !vector_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_07_02 cg_storeunit_diag_07_02_inst;

    // 08.01 STA慢反馈
    covergroup cg_storeunit_08_01 @(posedge clock);
        option.per_instance = 1;
        // 08.01.001 FEEDBACK_正常hit
        // 场景：有效TLB hit且无MAB replay；原始标量与child按叶点区分；F.valid比S1 feedback条件延迟一拍；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：C2 F.valid=1、hit=1。
        // 审计结论：完全建模。
        cp_08_01_001 : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && !replay2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.01.002 FEEDBACK_TLBmiss
        // 场景：S1有效TLB miss；原始标量与child按叶点区分；F.valid比S1 feedback条件延迟一拍；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：C2 F.valid=1、hit=0，即使该attempt已S1 kill。
        // 审计结论：完全建模。
        cp_08_01_002 : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (fb_due && !fb_expected_hit)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.01.003 FEEDBACK_MAB拒绝
        // 场景：TLB hit但MAB不ready；原始标量与child按叶点区分；F.valid比S1 feedback条件延迟一拍；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：C2 F.hit=0；不是DCache miss引起的反馈。
        // 审计结论：完全建模。
        cp_08_01_003 : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && replay2 && qualified2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.01.004 FEEDBACK_commit资格不足
        // 场景：TLB hit但跨16B资格不满足；原始标量与child按叶点区分；F.valid比S1 feedback条件延迟一拍；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：C2 F.hit=0；后续同parent可有更多attempt。
        // 审计结论：完全建模。
        cp_08_01_004 : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && !qualified2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.01.005 FEEDBACK_PMP异常
        // 场景：TLB hit且无MAB replay，S2出现PMP AF；原始标量与child按叶点区分；F.valid比S1 feedback条件延迟一拍；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：F.hit仍可为1；异常不是普通STA block反馈的同义词。
        // 审计结论：完全建模。
        cp_08_01_005 : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && io_pmp_st)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.01.006 FEEDBACK_MMIO分流
        // 场景：TLB hit、自然对齐MMIO且无MAB replay；原始标量与child按叶点区分；F.valid比S1 feedback条件延迟一拍；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：F.hit可为1但不产生普通stout；由SQ特殊路径完成。
        // 审计结论：完全建模。
        cp_08_01_006 : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && exp_mmio2 && !exp_exception2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.01.007 FEEDBACK_S1redirect
        // 场景：S1原始标量本拍needFlush；原始标量与child按叶点区分；F.valid比S1 feedback条件延迟一拍；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：对应feedback_slow_valid被抑制；不将取消当作block重试。
        // 审计结论：完全建模。
        cp_08_01_007 : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && flush1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.01.008 FEEDBACK_MABchild不反馈
        // 场景：标量MAB child有效；原始标量与child按叶点区分；F.valid比S1 feedback条件延迟一拍；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：不生成原始STA F.valid；结果走MO。
        // 审计结论：完全建模。
        cp_08_01_008 : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && child2)) {
            option.weight = HAS_CHILD ? 1 : 0;
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.01.009 FEEDBACK_反馈身份
        // 场景：两条不同SQ请求相邻发射，前条miss后条hit；原始标量与child按叶点区分；F.valid比S1 feedback条件延迟一拍；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：F.sq flag/value与各自延迟请求对应，不串线。
        // 审计结论：完全建模。
        cp_08_01_009 : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && feedback_history && sq2!=last_feedback_sq && tlbhit2 && last_feedback_miss)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_08_01 cg_storeunit_08_01_inst;
    covergroup cg_storeunit_diag_08_01 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_08_01_001_result : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && !replay2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_01_002_result : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (fb_due && !fb_expected_hit)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_01_003_result : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && replay2 && qualified2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_01_004_result : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && !qualified2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_01_005_result : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && io_pmp_st)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_01_006_result : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && tlbhit2 && exp_mmio2 && !exp_exception2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_01_007_result : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && flush1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_01_008_result : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && child2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_01_009_result : coverpoint ($isunknown((fb_ok)) ? 2'd2 : ((fb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && feedback_history && sq2!=last_feedback_sq && tlbhit2 && last_feedback_miss)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_08_01 cg_storeunit_diag_08_01_inst;

    // 08.02 S3接纳与普通写回
    covergroup cg_storeunit_08_02 @(posedge clock);
        option.per_instance = 1;
        // 08.02.001 WB_普通cacheable
        // 场景：正常cacheable SB/SH/SW/SD且无异常；无redirect；MAB replay/异常按条目设置；末级R=1；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：S3有效，C4普通stout一次；vecstout不归属该标量。
        // 审计结论：完全建模。
        cp_08_02_001 : coverpoint ($isunknown((scene_08_02_001_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_08_02_001_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_08_02_001_pipe[1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.02.002 WB_HSV
        // 场景：合法HSV标量正常完成；无redirect；MAB replay/异常按条目设置；末级R=1；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：普通标量stout输出对应ROB，非向量完成。
        // 审计结论：完全建模。
        cp_08_02_002 : coverpoint ($isunknown((scene_08_02_002_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_08_02_002_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_08_02_002_pipe[1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.02.003 WB_自然对齐NC
        // 场景：自然对齐NC普通标量且无异常；无redirect；MAB replay/异常按条目设置；末级R=1；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：可进入普通STA写回；实际NC内存事务在SQ后续处理。
        // 审计结论：完全建模。
        cp_08_02_003 : coverpoint ($isunknown((scene_08_02_003_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_08_02_003_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_08_02_003_pipe[1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.02.006 WB_CBO异常
        // 场景：CBO有有效PF/GPF/AF或trigger异常；无redirect；MAB replay/异常按条目设置；末级R=1；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：异常允许进入普通S3；不能因CBO一概等待SQ正常完成。
        // 审计结论：完全建模。
        cp_08_02_006 : coverpoint ($isunknown((scene_08_02_006_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_08_02_006_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_08_02_006_pipe[1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.02.007 WB_MMIO异常
        // 场景：MMIO访问形成有效异常；无redirect；MAB replay/异常按条目设置；末级R=1；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：异常路径允许报告普通异常结果，实际s2_mmio与exception门控分别记录。
        // 审计结论：完全建模。
        cp_08_02_007 : coverpoint ($isunknown((scene_08_02_007_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_08_02_007_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_08_02_007_pipe[1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.02.009 WB_MABchild
        // 场景：标量child进入S2；无redirect；MAB replay/异常按条目设置；末级R=1；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：通过MO返回；不接纳成普通stout。
        // 审计结论：完全建模。
        cp_08_02_009 : coverpoint ($isunknown((child_ok && !io_feedback_slow_valid && !admit2)) ? 2'd2 : ((child_ok && !io_feedback_slow_valid && !admit2) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && child2 && !flush2)) {
            option.weight = HAS_CHILD ? 1 : 0;
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_08_02 cg_storeunit_08_02_inst;
    covergroup cg_storeunit_diag_08_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_08_02_001_result : coverpoint ($isunknown((scene_08_02_001_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_08_02_001_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_08_02_001_pipe[1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_02_002_result : coverpoint ($isunknown((scene_08_02_002_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_08_02_002_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_08_02_002_pipe[1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_02_003_result : coverpoint ($isunknown((scene_08_02_003_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_08_02_003_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_08_02_003_pipe[1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_02_006_result : coverpoint ($isunknown((scene_08_02_006_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_08_02_006_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_08_02_006_pipe[1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_02_007_result : coverpoint ($isunknown((scene_08_02_007_prefix[1] && (pipeline_wb_result))) ? 2'd2 : ((scene_08_02_007_prefix[1] && (pipeline_wb_result)) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_08_02_007_pipe[1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_02_009_result : coverpoint ($isunknown((child_ok && !io_feedback_slow_valid && !admit2)) ? 2'd2 : ((child_ok && !io_feedback_slow_valid && !admit2) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && child2 && !flush2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_08_02 cg_storeunit_diag_08_02_inst;

    // 08.03 末级字段与吞吐
    covergroup cg_storeunit_08_03 @(posedge clock);
        option.per_instance = 1;
        // 08.03.001 WB_正常四拍关系
        // 场景：C0标量接受且后续hit无异常/特殊处理；正常标量或明确异常token；当前生成RTL只有sx第1个寄存级；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：在定义的组合周期中C4出现stout；不把此延迟当作含IQ等待或提交的总延迟。
        // 审计结论：完全建模。
        cp_08_03_001 : coverpoint ($isunknown((trace4 && wb_ok)) ? 2'd2 : ((trace4 && wb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.03.002 WB_连续不同ROB
        // 场景：连续五拍接受正常标量且ROB/SQ各不相同；正常标量或明确异常token；当前生成RTL只有sx第1个寄存级；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：流水填满后连续五拍按序输出对应ROB，不因后续新payload覆盖前条。
        // 审计结论：完全建模。
        cp_08_03_002 : coverpoint ($isunknown((seq_distinct && result_sequence==5'b11111)) ? 2'd2 : ((seq_distinct && result_sequence==5'b11111) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && due_sequence==5'b11111)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.03.003 WB_trigger_15
        // 场景：有效普通标量完成路径的trigger动作=15；正常标量或明确异常token；当前生成RTL只有sx第1个寄存级；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：WB.trigger=15；breakPoint位=0；其他异常另行保留。
        // 审计结论：完全建模。
        cp_08_03_003 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && action4==15)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.03.004 WB_trigger_0
        // 场景：有效普通标量完成路径的trigger动作=0；正常标量或明确异常token；当前生成RTL只有sx第1个寄存级；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：WB.trigger=0；breakPoint位=1。
        // 审计结论：完全建模。
        cp_08_03_004 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && action4==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.03.005 WB_trigger_1
        // 场景：有效普通标量完成路径的trigger动作=1；正常标量或明确异常token；当前生成RTL只有sx第1个寄存级；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：WB.trigger=1；breakPoint位=0；其他异常另行保留。
        // 审计结论：完全建模。
        cp_08_03_005 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && action4==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.03.006 WB_异常位_Breakpoint
        // 场景：有效标量异常完成中目标Breakpoint位存在；正常标量或明确异常token；当前生成RTL只有sx第1个寄存级；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：末级有效时保留bit3，同时读取整个Sta异常集合；缺失写回按缺失结果统计。
        // 审计结论：完全建模。
        cp_08_03_006 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && ex4[0])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.03.007 WB_异常位_SAM
        // 场景：有效标量异常完成中目标SAM位存在；正常标量或明确异常token；当前生成RTL只有sx第1个寄存级；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：末级有效时保留bit6，同时读取整个Sta异常集合；缺失写回按缺失结果统计。
        // 审计结论：完全建模。
        cp_08_03_007 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && ex4[1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.03.008 WB_异常位_AF
        // 场景：有效标量异常完成中目标AF位存在；正常标量或明确异常token；当前生成RTL只有sx第1个寄存级；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：末级有效时保留bit7，同时读取整个Sta异常集合；缺失写回按缺失结果统计。
        // 审计结论：完全建模。
        cp_08_03_008 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && ex4[2])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.03.009 WB_异常位_PF
        // 场景：有效标量异常完成中目标PF位存在；正常标量或明确异常token；当前生成RTL只有sx第1个寄存级；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：末级有效时保留bit15，同时读取整个Sta异常集合；缺失写回按缺失结果统计。
        // 审计结论：完全建模。
        cp_08_03_009 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && ex4[3])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.03.010 WB_异常位_GPF
        // 场景：有效标量异常完成中目标GPF位存在；正常标量或明确异常token；当前生成RTL只有sx第1个寄存级；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：末级有效时保留bit23，同时读取整个Sta异常集合；缺失写回按缺失结果统计。
        // 审计结论：完全建模。
        cp_08_03_010 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && ex4[5])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.03.011 WB_debug_PBMT0_PMA0
        // 场景：普通可写回标量PBMT=0且P.mmio=0，无其他异常；正常标量或明确异常token；当前生成RTL只有sx第1个寄存级；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：isMMIO=0；isNCIO=0，后者由nc && !memBackTypeMM决定，不能直接等同于nc。
        // 审计结论：完全建模。
        cp_08_03_011 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && pbmt4==0 && pmammio4==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.03.012 WB_debug_PBMT1_PMA0
        // 场景：普通可写回标量PBMT=1且P.mmio=0，无其他异常；正常标量或明确异常token；当前生成RTL只有sx第1个寄存级；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：isMMIO=0；isNCIO=0，后者由nc && !memBackTypeMM决定，不能直接等同于nc。
        // 审计结论：完全建模。
        cp_08_03_012 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && pbmt4==1 && pmammio4==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.03.013 WB_debug_PBMT1_PMA1
        // 场景：普通可写回标量PBMT=1且P.mmio=1，无其他异常；正常标量或明确异常token；当前生成RTL只有sx第1个寄存级；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：isMMIO=0；isNCIO=1，后者由nc && !memBackTypeMM决定，不能直接等同于nc。
        // 审计结论：完全建模。
        cp_08_03_013 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && pbmt4==1 && pmammio4==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_08_03 cg_storeunit_08_03_inst;
    covergroup cg_storeunit_diag_08_03 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_08_03_001_result : coverpoint ($isunknown((trace4 && wb_ok)) ? 2'd2 : ((trace4 && wb_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_03_002_result : coverpoint ($isunknown((seq_distinct && result_sequence==5'b11111)) ? 2'd2 : ((seq_distinct && result_sequence==5'b11111) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && due_sequence==5'b11111)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_03_003_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && action4==15)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_03_004_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && action4==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_03_005_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && action4==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_03_006_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && ex4[0])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_03_007_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && ex4[1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_03_008_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && ex4[2])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_03_009_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && ex4[3])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_03_010_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && ex4[5])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_03_011_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && pbmt4==0 && pmammio4==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_03_012_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && pbmt4==1 && pmammio4==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_03_013_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4 && pbmt4==1 && pmammio4==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_08_03 cg_storeunit_diag_08_03_inst;

    // 09.01 Redirect年龄与取消级别
    covergroup cg_storeunit_09_01 @(posedge clock);
        option.per_instance = 1;
        // 09.01.001 REDIRECT_older_level0
        // 场景：标量ROB相对redirect为older，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_001_stage0 : coverpoint ($isunknown((close_saved_age_1_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_1_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_001_stage0_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.001 REDIRECT_older_level0
        // 场景：标量ROB相对redirect为older，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_001_stage1 : coverpoint ($isunknown((close_saved_age_1_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_1_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_001_stage1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.001 REDIRECT_older_level0
        // 场景：标量ROB相对redirect为older，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_001_stage2 : coverpoint ($isunknown((close_saved_age_1_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_1_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_001_stage2_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.001 REDIRECT_older_level0
        // 场景：标量ROB相对redirect为older，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_001_stage3 : coverpoint ($isunknown((close_saved_age_1_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_1_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_001_stage3_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.001 REDIRECT_older_level0
        // 场景：标量ROB相对redirect为older，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_001_stage4 : coverpoint ($isunknown((close_saved_age_1_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_1_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_001_stage4_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.002 REDIRECT_older_level1
        // 场景：标量ROB相对redirect为older，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_002_stage0 : coverpoint ($isunknown((close_saved_age_2_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_2_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_002_stage0_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.002 REDIRECT_older_level1
        // 场景：标量ROB相对redirect为older，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_002_stage1 : coverpoint ($isunknown((close_saved_age_2_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_2_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_002_stage1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.002 REDIRECT_older_level1
        // 场景：标量ROB相对redirect为older，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_002_stage2 : coverpoint ($isunknown((close_saved_age_2_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_2_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_002_stage2_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.002 REDIRECT_older_level1
        // 场景：标量ROB相对redirect为older，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_002_stage3 : coverpoint ($isunknown((close_saved_age_2_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_2_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_002_stage3_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.002 REDIRECT_older_level1
        // 场景：标量ROB相对redirect为older，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_002_stage4 : coverpoint ($isunknown((close_saved_age_2_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_2_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_002_stage4_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.003 REDIRECT_equal_level0
        // 场景：标量ROB相对redirect为equal，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_003_stage0 : coverpoint ($isunknown((close_saved_age_3_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_3_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_003_stage0_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.003 REDIRECT_equal_level0
        // 场景：标量ROB相对redirect为equal，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_003_stage1 : coverpoint ($isunknown((close_saved_age_3_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_3_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_003_stage1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.003 REDIRECT_equal_level0
        // 场景：标量ROB相对redirect为equal，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_003_stage2 : coverpoint ($isunknown((close_saved_age_3_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_3_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_003_stage2_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.003 REDIRECT_equal_level0
        // 场景：标量ROB相对redirect为equal，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_003_stage3 : coverpoint ($isunknown((close_saved_age_3_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_3_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_003_stage3_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.003 REDIRECT_equal_level0
        // 场景：标量ROB相对redirect为equal，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_003_stage4 : coverpoint ($isunknown((close_saved_age_3_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_3_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_003_stage4_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.004 REDIRECT_equal_level1
        // 场景：标量ROB相对redirect为equal，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_004_stage0 : coverpoint ($isunknown((close_saved_age_4_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_4_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_004_stage0_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.004 REDIRECT_equal_level1
        // 场景：标量ROB相对redirect为equal，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_004_stage1 : coverpoint ($isunknown((close_saved_age_4_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_4_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_004_stage1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.004 REDIRECT_equal_level1
        // 场景：标量ROB相对redirect为equal，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_004_stage2 : coverpoint ($isunknown((close_saved_age_4_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_4_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_004_stage2_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.004 REDIRECT_equal_level1
        // 场景：标量ROB相对redirect为equal，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_004_stage3 : coverpoint ($isunknown((close_saved_age_4_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_4_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_004_stage3_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.004 REDIRECT_equal_level1
        // 场景：标量ROB相对redirect为equal，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_004_stage4 : coverpoint ($isunknown((close_saved_age_4_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_4_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_004_stage4_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.005 REDIRECT_younger_level0
        // 场景：标量ROB相对redirect为younger，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_005_stage0 : coverpoint ($isunknown((close_saved_age_5_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_5_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_005_stage0_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.005 REDIRECT_younger_level0
        // 场景：标量ROB相对redirect为younger，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_005_stage1 : coverpoint ($isunknown((close_saved_age_5_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_5_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_005_stage1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.005 REDIRECT_younger_level0
        // 场景：标量ROB相对redirect为younger，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_005_stage2 : coverpoint ($isunknown((close_saved_age_5_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_5_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_005_stage2_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.005 REDIRECT_younger_level0
        // 场景：标量ROB相对redirect为younger，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_005_stage3 : coverpoint ($isunknown((close_saved_age_5_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_5_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_005_stage3_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.005 REDIRECT_younger_level0
        // 场景：标量ROB相对redirect为younger，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_005_stage4 : coverpoint ($isunknown((close_saved_age_5_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_5_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_005_stage4_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.006 REDIRECT_younger_level1
        // 场景：标量ROB相对redirect为younger，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_006_stage0 : coverpoint ($isunknown((close_saved_age_6_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_6_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_006_stage0_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.006 REDIRECT_younger_level1
        // 场景：标量ROB相对redirect为younger，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_006_stage1 : coverpoint ($isunknown((close_saved_age_6_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_6_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_006_stage1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.006 REDIRECT_younger_level1
        // 场景：标量ROB相对redirect为younger，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_006_stage2 : coverpoint ($isunknown((close_saved_age_6_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_6_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_006_stage2_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.006 REDIRECT_younger_level1
        // 场景：标量ROB相对redirect为younger，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_006_stage3 : coverpoint ($isunknown((close_saved_age_6_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_6_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_006_stage3_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.006 REDIRECT_younger_level1
        // 场景：标量ROB相对redirect为younger，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_006_stage4 : coverpoint ($isunknown((close_saved_age_6_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_6_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_006_stage4_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.007 REDIRECT_wrap_younger_level0
        // 场景：标量ROB相对redirect为wrap_younger，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_007_stage0 : coverpoint ($isunknown((close_saved_age_7_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_7_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_007_stage0_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.007 REDIRECT_wrap_younger_level0
        // 场景：标量ROB相对redirect为wrap_younger，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_007_stage1 : coverpoint ($isunknown((close_saved_age_7_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_7_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_007_stage1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.007 REDIRECT_wrap_younger_level0
        // 场景：标量ROB相对redirect为wrap_younger，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_007_stage2 : coverpoint ($isunknown((close_saved_age_7_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_7_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_007_stage2_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.007 REDIRECT_wrap_younger_level0
        // 场景：标量ROB相对redirect为wrap_younger，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_007_stage3 : coverpoint ($isunknown((close_saved_age_7_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_7_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_007_stage3_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.007 REDIRECT_wrap_younger_level0
        // 场景：标量ROB相对redirect为wrap_younger，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_007_stage4 : coverpoint ($isunknown((close_saved_age_7_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_7_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_007_stage4_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.008 REDIRECT_wrap_younger_level1
        // 场景：标量ROB相对redirect为wrap_younger，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_008_stage0 : coverpoint ($isunknown((close_saved_age_8_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_8_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_008_stage0_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.008 REDIRECT_wrap_younger_level1
        // 场景：标量ROB相对redirect为wrap_younger，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_008_stage1 : coverpoint ($isunknown((close_saved_age_8_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_8_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_008_stage1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.008 REDIRECT_wrap_younger_level1
        // 场景：标量ROB相对redirect为wrap_younger，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_008_stage2 : coverpoint ($isunknown((close_saved_age_8_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_8_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_008_stage2_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.008 REDIRECT_wrap_younger_level1
        // 场景：标量ROB相对redirect为wrap_younger，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_008_stage3 : coverpoint ($isunknown((close_saved_age_8_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_8_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_008_stage3_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.008 REDIRECT_wrap_younger_level1
        // 场景：标量ROB相对redirect为wrap_younger，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项属于取消集合，按所在stage的清理逻辑退出。
        // 审计结论：完全建模。
        cp_09_01_008_stage4 : coverpoint ($isunknown((close_saved_age_8_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_8_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_008_stage4_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.009 REDIRECT_wrap_older_level0
        // 场景：标量ROB相对redirect为wrap_older，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_009_stage0 : coverpoint ($isunknown((close_saved_age_9_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_9_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_009_stage0_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.009 REDIRECT_wrap_older_level0
        // 场景：标量ROB相对redirect为wrap_older，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_009_stage1 : coverpoint ($isunknown((close_saved_age_9_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_9_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_009_stage1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.009 REDIRECT_wrap_older_level0
        // 场景：标量ROB相对redirect为wrap_older，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_009_stage2 : coverpoint ($isunknown((close_saved_age_9_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_9_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_009_stage2_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.009 REDIRECT_wrap_older_level0
        // 场景：标量ROB相对redirect为wrap_older，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_009_stage3 : coverpoint ($isunknown((close_saved_age_9_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_9_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_009_stage3_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.009 REDIRECT_wrap_older_level0
        // 场景：标量ROB相对redirect为wrap_older，level=0；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_009_stage4 : coverpoint ($isunknown((close_saved_age_9_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_9_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_009_stage4_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.010 REDIRECT_wrap_older_level1
        // 场景：标量ROB相对redirect为wrap_older，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_010_stage0 : coverpoint ($isunknown((close_saved_age_10_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_10_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_010_stage0_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.010 REDIRECT_wrap_older_level1
        // 场景：标量ROB相对redirect为wrap_older，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_010_stage1 : coverpoint ($isunknown((close_saved_age_10_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_10_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_010_stage1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.010 REDIRECT_wrap_older_level1
        // 场景：标量ROB相对redirect为wrap_older，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_010_stage2 : coverpoint ($isunknown((close_saved_age_10_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_10_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_010_stage2_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.010 REDIRECT_wrap_older_level1
        // 场景：标量ROB相对redirect为wrap_older，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_010_stage3 : coverpoint ($isunknown((close_saved_age_10_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_10_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_010_stage3_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.01.010 REDIRECT_wrap_older_level1
        // 场景：标量ROB相对redirect为wrap_older，level=1；Redirect使用StoreUnit实际输入延迟域；年龄关系按合法环形窗口构造；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：该项保留，不应被无关或较年轻redirect误杀。
        // 审计结论：完全建模。
        cp_09_01_010_stage4 : coverpoint ($isunknown((close_saved_age_10_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_10_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_010_stage4_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_09_01 cg_storeunit_09_01_inst;
    covergroup cg_storeunit_diag_09_01 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_09_01_001_stage0_result : coverpoint ($isunknown((close_saved_age_1_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_1_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_001_stage0_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_001_stage1_result : coverpoint ($isunknown((close_saved_age_1_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_1_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_001_stage1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_001_stage2_result : coverpoint ($isunknown((close_saved_age_1_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_1_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_001_stage2_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_001_stage3_result : coverpoint ($isunknown((close_saved_age_1_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_1_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_001_stage3_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_001_stage4_result : coverpoint ($isunknown((close_saved_age_1_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_1_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_001_stage4_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_002_stage0_result : coverpoint ($isunknown((close_saved_age_2_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_2_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_002_stage0_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_002_stage1_result : coverpoint ($isunknown((close_saved_age_2_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_2_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_002_stage1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_002_stage2_result : coverpoint ($isunknown((close_saved_age_2_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_2_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_002_stage2_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_002_stage3_result : coverpoint ($isunknown((close_saved_age_2_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_2_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_002_stage3_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_002_stage4_result : coverpoint ($isunknown((close_saved_age_2_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_2_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_002_stage4_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_003_stage0_result : coverpoint ($isunknown((close_saved_age_3_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_3_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_003_stage0_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_003_stage1_result : coverpoint ($isunknown((close_saved_age_3_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_3_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_003_stage1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_003_stage2_result : coverpoint ($isunknown((close_saved_age_3_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_3_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_003_stage2_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_003_stage3_result : coverpoint ($isunknown((close_saved_age_3_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_3_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_003_stage3_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_003_stage4_result : coverpoint ($isunknown((close_saved_age_3_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_3_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_003_stage4_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_004_stage0_result : coverpoint ($isunknown((close_saved_age_4_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_4_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_004_stage0_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_004_stage1_result : coverpoint ($isunknown((close_saved_age_4_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_4_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_004_stage1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_004_stage2_result : coverpoint ($isunknown((close_saved_age_4_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_4_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_004_stage2_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_004_stage3_result : coverpoint ($isunknown((close_saved_age_4_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_4_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_004_stage3_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_004_stage4_result : coverpoint ($isunknown((close_saved_age_4_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_4_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_004_stage4_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_005_stage0_result : coverpoint ($isunknown((close_saved_age_5_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_5_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_005_stage0_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_005_stage1_result : coverpoint ($isunknown((close_saved_age_5_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_5_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_005_stage1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_005_stage2_result : coverpoint ($isunknown((close_saved_age_5_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_5_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_005_stage2_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_005_stage3_result : coverpoint ($isunknown((close_saved_age_5_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_5_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_005_stage3_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_005_stage4_result : coverpoint ($isunknown((close_saved_age_5_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_5_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_005_stage4_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_006_stage0_result : coverpoint ($isunknown((close_saved_age_6_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_6_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_006_stage0_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_006_stage1_result : coverpoint ($isunknown((close_saved_age_6_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_6_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_006_stage1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_006_stage2_result : coverpoint ($isunknown((close_saved_age_6_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_6_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_006_stage2_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_006_stage3_result : coverpoint ($isunknown((close_saved_age_6_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_6_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_006_stage3_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_006_stage4_result : coverpoint ($isunknown((close_saved_age_6_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_6_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_006_stage4_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_007_stage0_result : coverpoint ($isunknown((close_saved_age_7_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_7_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_007_stage0_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_007_stage1_result : coverpoint ($isunknown((close_saved_age_7_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_7_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_007_stage1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_007_stage2_result : coverpoint ($isunknown((close_saved_age_7_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_7_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_007_stage2_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_007_stage3_result : coverpoint ($isunknown((close_saved_age_7_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_7_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_007_stage3_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_007_stage4_result : coverpoint ($isunknown((close_saved_age_7_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_7_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_007_stage4_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_008_stage0_result : coverpoint ($isunknown((close_saved_age_8_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_8_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_008_stage0_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_008_stage1_result : coverpoint ($isunknown((close_saved_age_8_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_8_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_008_stage1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_008_stage2_result : coverpoint ($isunknown((close_saved_age_8_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_8_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_008_stage2_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_008_stage3_result : coverpoint ($isunknown((close_saved_age_8_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_8_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_008_stage3_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_008_stage4_result : coverpoint ($isunknown((close_saved_age_8_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_8_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_008_stage4_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_009_stage0_result : coverpoint ($isunknown((close_saved_age_9_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_9_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_009_stage0_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_009_stage1_result : coverpoint ($isunknown((close_saved_age_9_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_9_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_009_stage1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_009_stage2_result : coverpoint ($isunknown((close_saved_age_9_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_9_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_009_stage2_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_009_stage3_result : coverpoint ($isunknown((close_saved_age_9_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_9_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_009_stage3_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_009_stage4_result : coverpoint ($isunknown((close_saved_age_9_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_9_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_009_stage4_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_010_stage0_result : coverpoint ($isunknown((close_saved_age_10_0 && close_next_ok)) ? 2'd2 : ((close_saved_age_10_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_010_stage0_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_010_stage1_result : coverpoint ($isunknown((close_saved_age_10_1 && close_next_ok)) ? 2'd2 : ((close_saved_age_10_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_010_stage1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_010_stage2_result : coverpoint ($isunknown((close_saved_age_10_2 && close_next_ok)) ? 2'd2 : ((close_saved_age_10_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_010_stage2_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_010_stage3_result : coverpoint ($isunknown((close_saved_age_10_3 && close_next_ok)) ? 2'd2 : ((close_saved_age_10_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_010_stage3_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_01_010_stage4_result : coverpoint ($isunknown((close_saved_age_10_4 && close_next_ok)) ? 2'd2 : ((close_saved_age_10_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_01_010_stage4_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_09_01 cg_storeunit_diag_09_01_inst;

    // 09.02 各阶段冲刷与恢复
    covergroup cg_storeunit_09_02 @(posedge clock);
        option.per_instance = 1;
        // 09.02.001 FLUSH_S0
        // 场景：标量token位于S0，redirect确实覆盖该ROB；使用明确被取消的token；后续恢复请求用新epoch；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：s0_fire=0；TQ.valid或mask.valid仍可能有效，不为该候选建立正常推进token。
        // 审计结论：完全建模。
        cp_09_02_001 : coverpoint ($isunknown((close_flush_result_0 && close_next_ok)) ? 2'd2 : ((close_flush_result_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_001_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.02.002 FLUSH_S1
        // 场景：标量token位于S1，redirect确实覆盖该ROB；使用明确被取消的token；后续恢复请求用新epoch；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：s1_kill=1；抑制该token慢反馈并清理有效位；L/nuke同拍valid仍按其独立表达式记录。
        // 审计结论：完全建模。
        cp_09_02_002 : coverpoint ($isunknown((close_flush_result_1 && close_next_ok)) ? 2'd2 : ((close_flush_result_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_002_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.02.003 FLUSH_S2
        // 场景：标量token位于S2，redirect确实覆盖该ROB；使用明确被取消的token；后续恢复请求用新epoch；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：s2_kill=1；RE.af/hasException受!kill抑制；不接纳被取消token到普通S3。
        // 审计结论：完全建模。
        cp_09_02_003 : coverpoint ($isunknown((close_flush_result_2 && close_next_ok)) ? 2'd2 : ((close_flush_result_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_003_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.02.004 FLUSH_S3
        // 场景：标量token位于S3，redirect确实覆盖该ROB；使用明确被取消的token；后续恢复请求用新epoch；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：s3_kill=1；不向延迟级推进；新输入对有效位更新的优先级按RTL核对。
        // 审计结论：完全建模。
        cp_09_02_004 : coverpoint ($isunknown((close_flush_result_3 && close_next_ok)) ? 2'd2 : ((close_flush_result_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_004_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.02.005 FLUSH_SX
        // 场景：标量token位于SX，redirect确实覆盖该ROB；使用明确被取消的token；后续恢复请求用新epoch；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cur_kill=1，末级寄存器在边沿清理；io_stout.valid本拍没有额外!cur_kill组合门控，按消费者redirect协议解释。
        // 审计结论：完全建模。
        cp_09_02_005 : coverpoint ($isunknown((close_flush_result_4 && close_next_ok)) ? 2'd2 : ((close_flush_result_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_005_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.02.007 FLUSH_恢复后正常请求
        // 场景：取消一个在途标量后发入新的正常Store；使用明确被取消的token；后续恢复请求用新epoch；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：被取消请求无后续正常完成计数；恢复请求可按正常流水前进，不继承旧异常、mask或反馈。
        // 审计结论：完全建模。
        cp_09_02_007 : coverpoint ($isunknown((pipeline_wb_result && !close_ghost_bad)) ? 2'd2 : ((pipeline_wb_result && !close_ghost_bad) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && reset_or_redirect_seen && close_scalar_window)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.02.008 FLUSH_清理与新输入同拍
        // 场景：某级旧项被取消同时前级有不被取消的新项推进；使用明确被取消的token；后续恢复请求用新epoch；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：按RTL新输入优先更新；新项有效位与新payload一致，不因旧项kill一并清空。
        // 审计结论：完全建模。
        cp_09_02_008_stage1 : coverpoint ($isunknown((close_next_ok)) ? 2'd2 : ((close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_008_stage1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.02.008 FLUSH_清理与新输入同拍
        // 场景：某级旧项被取消同时前级有不被取消的新项推进；使用明确被取消的token；后续恢复请求用新epoch；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：按RTL新输入优先更新；新项有效位与新payload一致，不因旧项kill一并清空。
        // 审计结论：完全建模。
        cp_09_02_008_stage2 : coverpoint ($isunknown((close_next_ok)) ? 2'd2 : ((close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_008_stage2_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.02.008 FLUSH_清理与新输入同拍
        // 场景：某级旧项被取消同时前级有不被取消的新项推进；使用明确被取消的token；后续恢复请求用新epoch；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：按RTL新输入优先更新；新项有效位与新payload一致，不因旧项kill一并清空。
        // 审计结论：完全建模。
        cp_09_02_008_stage3 : coverpoint ($isunknown((close_next_ok)) ? 2'd2 : ((close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_008_stage3_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.02.008 FLUSH_清理与新输入同拍
        // 场景：某级旧项被取消同时前级有不被取消的新项推进；使用明确被取消的token；后续恢复请求用新epoch；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：按RTL新输入优先更新；新项有效位与新payload一致，不因旧项kill一并清空。
        // 审计结论：完全建模。
        cp_09_02_008_stage4 : coverpoint ($isunknown((close_next_ok)) ? 2'd2 : ((close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_008_stage4_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_09_02 cg_storeunit_09_02_inst;
    covergroup cg_storeunit_diag_09_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_09_02_001_result : coverpoint ($isunknown((close_flush_result_0 && close_next_ok)) ? 2'd2 : ((close_flush_result_0 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_001_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_02_002_result : coverpoint ($isunknown((close_flush_result_1 && close_next_ok)) ? 2'd2 : ((close_flush_result_1 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_002_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_02_003_result : coverpoint ($isunknown((close_flush_result_2 && close_next_ok)) ? 2'd2 : ((close_flush_result_2 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_003_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_02_004_result : coverpoint ($isunknown((close_flush_result_3 && close_next_ok)) ? 2'd2 : ((close_flush_result_3 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_004_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_02_005_result : coverpoint ($isunknown((close_flush_result_4 && close_next_ok)) ? 2'd2 : ((close_flush_result_4 && close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_005_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_02_007_result : coverpoint ($isunknown((pipeline_wb_result && !close_ghost_bad)) ? 2'd2 : ((pipeline_wb_result && !close_ghost_bad) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && reset_or_redirect_seen && close_scalar_window)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_02_008_stage1_result : coverpoint ($isunknown((close_next_ok)) ? 2'd2 : ((close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_008_stage1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_02_008_stage2_result : coverpoint ($isunknown((close_next_ok)) ? 2'd2 : ((close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_008_stage2_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_02_008_stage3_result : coverpoint ($isunknown((close_next_ok)) ? 2'd2 : ((close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_008_stage3_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_02_008_stage4_result : coverpoint ($isunknown((close_next_ok)) ? 2'd2 : ((close_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_09_02_008_stage4_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_09_02 cg_storeunit_diag_09_02_inst;

    // 09.03 复位与必要并发组合
    covergroup cg_storeunit_09_03 @(posedge clock);
        option.per_instance = 1;
        // 09.03.001 RESET_空闲复位
        // 场景：StoreUnit复位置位并保持；复位期间不以未知payload为有效事务；运行期采样在复位解除后；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：带复位的流水与慢反馈valid清零；无复位payload不要求清零。
        // 审计结论：完全建模。
        cp_09_03_001 : coverpoint ($isunknown((!s1_valid && !s2_valid && !s3_valid && !sx_valid_1_r && !io_feedback_slow_valid)) ? 2'd2 : ((!s1_valid && !s2_valid && !s3_valid && !sx_valid_1_r && !io_feedback_slow_valid) ? 2'd1 : 2'd0))
            iff (1'b1 && (reset_d1 && !reset)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.03.002 RESET_在途清理后恢复
        // 场景：正常标量在流水中时复位，解除后发送新请求；复位期间不以未知payload为有效事务；运行期采样在复位解除后；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：复位前token结束为reset取消；解除后新请求按C0至C4关系完成，不出现旧token幽灵结果。
        // 审计结论：完全建模。
        cp_09_03_002 : coverpoint ($isunknown((pipeline_wb_result && !close_reset_bad)) ? 2'd2 : ((pipeline_wb_result && !close_reset_bad) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && close_reset_inflight && close_scalar_window)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_09_03 cg_storeunit_09_03_inst;
    covergroup cg_storeunit_diag_09_03 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_09_03_001_result : coverpoint ($isunknown((!s1_valid && !s2_valid && !s3_valid && !sx_valid_1_r && !io_feedback_slow_valid)) ? 2'd2 : ((!s1_valid && !s2_valid && !s3_valid && !sx_valid_1_r && !io_feedback_slow_valid) ? 2'd1 : 2'd0))
            iff (1'b1 && (reset_d1 && !reset)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_03_002_result : coverpoint ($isunknown((pipeline_wb_result && !close_reset_bad)) ? 2'd2 : ((pipeline_wb_result && !close_reset_bad) ? 2'd1 : 2'd0))
            iff (sample_enable && (normal_wb && close_reset_inflight && close_scalar_window)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_09_03 cg_storeunit_diag_09_03_inst;

    // 09.04 异常输入与版本差异诊断
    covergroup cg_storeunit_09_04 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 09.04.001 DIAG_PBMT保留3
        // 场景：有效响应PBMT=3；诊断项单独统计；不将异常输入或已知偏离设为必须正常命中的目标；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：记录当前RTL按isNC/isIO均不成立的处理及下游结果，不视为合法普通内存成功。
        // 审计结论：完全建模。
        cp_09_04_001 : coverpoint ($isunknown((s1_pbmt==3)) ? 2'd2 : ((s1_pbmt==3) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && io_tlb_resp_valid && io_tlb_resp_bits_pbmt_0==3)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 09.04.002 DIAG_多种raw翻译fault
        // 场景：同一有效响应PF/GPF/AF同时置位；诊断项单独统计；不将异常输入或已知偏离设为必须正常命中的目标；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：记录各OR生成位与最终消费者结果；不假定正常DTLB一定能产生所有组合。
        // 审计结论：完全建模。
        cp_09_04_002 : coverpoint ($isunknown((s1_exception_ok)) ? 2'd2 : ((s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && io_tlb_resp_valid && $countones({fault_pf1,fault_af1,fault_gpf1})>1)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 09.04.003 DIAG_非法op输入
        // 场景：对普通标量入口注入非Store/HSV/CBO编码；诊断项单独统计；不将异常输入或已知偏离设为必须正常命中的目标；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：记录解码、mask、请求与结果；不将其当普通ISA Store功能。
        // 审计结论：完全建模。
        cp_09_04_003 : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && !legal0)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 09.04.004 DIAG_NC_CBOzero基线差异
        // 场景：自然对齐CBO.zero，PBMT=NC，无其他异常；诊断项单独统计；不将异常输入或已知偏离设为必须正常命中的目标；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：生成RTL RE.mmio=(s2_mmio||nonZeroCBO)&&!exception，此场景为0；Scala新增uncacheCboZero分支为1。按RTL记录SQ交接与未完成风险。
        // 审计结论：完全建模。
        cp_09_04_004 : coverpoint ($isunknown((io_lsq_replenish_mmio==0)) ? 2'd2 : ((io_lsq_replenish_mmio==0) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==7 && pbmt2==1 && !exp_exception2)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 09.04.005 DIAG_S2更新使能基线差异
        // 场景：s2_misalignNeedReplay=1且严格s2_mis_align=0；诊断项单独统计；不将异常输入或已知偏离设为必须正常命中的目标；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：记录RTL中RE.updateAddrValid第一项未被统一!replay门控；与Scala括号表达式分别保存，并对实际可达性单列核验。
        // 审计结论：完全建模。
        cp_09_04_005 : coverpoint ($isunknown((io_lsq_replenish_updateAddrValid==exp_update2)) ? 2'd2 : ((io_lsq_replenish_updateAddrValid==exp_update2) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && replay2 && !strict2)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 09.04.007 DIAG_CBO触发比较版本差异
        // 场景：CBO使用GE/LT比较模式；诊断项单独统计；不将异常输入或已知偏离设为必须正常命中的目标；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：当前MemTrigger_3的CBO路径只比较cacheline相等且不读取matchType；Scala增加GE/LT范围比较。分别记录两种结果。
        // 审计结论：完全建模。
        cp_09_04_007 : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && cbo1 && trig_mode0 inside {2,3} && trigger_single==1)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 09.04.008 DIAG_缺失写回结果
        // 场景：正常可完成标量token达到期望末级却未出现WB；诊断项单独统计；不将异常输入或已知偏离设为必须正常命中的目标；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：记录缺失/未知/身份不符；采样由起点token决定，不等待WB出现才统计。
        // 审计结论：完全建模。
        cp_09_04_008 : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_09_04 cg_storeunit_09_04_inst;
    covergroup cg_storeunit_diag_09_04 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_09_04_001_result : coverpoint ($isunknown((s1_pbmt==3)) ? 2'd2 : ((s1_pbmt==3) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && io_tlb_resp_valid && io_tlb_resp_bits_pbmt_0==3)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_04_002_result : coverpoint ($isunknown((s1_exception_ok)) ? 2'd2 : ((s1_exception_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && io_tlb_resp_valid && $countones({fault_pf1,fault_af1,fault_gpf1})>1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_04_003_result : coverpoint ($isunknown((request_ok)) ? 2'd2 : ((request_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (selected_scalar && !legal0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_04_004_result : coverpoint ($isunknown((io_lsq_replenish_mmio==0)) ? 2'd2 : ((io_lsq_replenish_mmio==0) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && op2==7 && pbmt2==1 && !exp_exception2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_04_005_result : coverpoint ($isunknown((io_lsq_replenish_updateAddrValid==exp_update2)) ? 2'd2 : ((io_lsq_replenish_updateAddrValid==exp_update2) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected2 && !child2 && replay2 && !strict2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_04_007_result : coverpoint ($isunknown((trigger_ok)) ? 2'd2 : ((trigger_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected1 && !child1 && cbo1 && trig_mode0 inside {2,3} && trigger_single==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_04_008_result : coverpoint ($isunknown((wb_ok && trace4)) ? 2'd2 : ((wb_ok && trace4) ? 2'd1 : 2'd0))
            iff (sample_enable && (expected4 && !child4)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_09_04 cg_storeunit_diag_09_04_inst;
    initial begin
        cg_storeunit_01_01_inst = new();
        cg_storeunit_diag_01_01_inst = new();
        cg_storeunit_01_02_inst = new();
        cg_storeunit_diag_01_02_inst = new();
        cg_storeunit_01_03_inst = new();
        cg_storeunit_diag_01_03_inst = new();
        cg_storeunit_02_01_inst = new();
        cg_storeunit_diag_02_01_inst = new();
        cg_storeunit_02_02_inst = new();
        cg_storeunit_diag_02_02_inst = new();
        cg_storeunit_02_03_inst = new();
        cg_storeunit_diag_02_03_inst = new();
        cg_storeunit_02_04_inst = new();
        cg_storeunit_diag_02_04_inst = new();
        cg_storeunit_03_01_inst = new();
        cg_storeunit_diag_03_01_inst = new();
        cg_storeunit_03_02_inst = new();
        cg_storeunit_diag_03_02_inst = new();
        cg_storeunit_03_03_inst = new();
        cg_storeunit_diag_03_03_inst = new();
        cg_storeunit_04_01_inst = new();
        cg_storeunit_diag_04_01_inst = new();
        cg_storeunit_04_02_inst = new();
        cg_storeunit_diag_04_02_inst = new();
        cg_storeunit_04_03_inst = new();
        cg_storeunit_diag_04_03_inst = new();
        cg_storeunit_04_04_inst = new();
        cg_storeunit_diag_04_04_inst = new();
        cg_storeunit_05_01_inst = new();
        cg_storeunit_diag_05_01_inst = new();
        cg_storeunit_05_04_inst = new();
        cg_storeunit_diag_05_04_inst = new();
        cg_storeunit_06_01_inst = new();
        cg_storeunit_diag_06_01_inst = new();
        cg_storeunit_06_02_inst = new();
        cg_storeunit_diag_06_02_inst = new();
        cg_storeunit_06_03_inst = new();
        cg_storeunit_diag_06_03_inst = new();
        cg_storeunit_06_04_inst = new();
        cg_storeunit_diag_06_04_inst = new();
        cg_storeunit_07_01_inst = new();
        cg_storeunit_diag_07_01_inst = new();
        cg_storeunit_07_02_inst = new();
        cg_storeunit_diag_07_02_inst = new();
        cg_storeunit_08_01_inst = new();
        cg_storeunit_diag_08_01_inst = new();
        cg_storeunit_08_02_inst = new();
        cg_storeunit_diag_08_02_inst = new();
        cg_storeunit_08_03_inst = new();
        cg_storeunit_diag_08_03_inst = new();
        cg_storeunit_09_01_inst = new();
        cg_storeunit_diag_09_01_inst = new();
        cg_storeunit_09_02_inst = new();
        cg_storeunit_diag_09_02_inst = new();
        cg_storeunit_09_03_inst = new();
        cg_storeunit_diag_09_03_inst = new();
        cg_storeunit_09_04_inst = new();
        cg_storeunit_diag_09_04_inst = new();
    end
endmodule
`endif // MEMBLOCK_FCOV_STORE_UNIT_EN
`endif
`endif
