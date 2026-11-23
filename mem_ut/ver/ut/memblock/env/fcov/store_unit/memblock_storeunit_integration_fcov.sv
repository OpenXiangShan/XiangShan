// StoreUnit V2 分层只读覆盖；2026-11-02。
// 与DTLB参考保持相同深度：module → cg_storeunit_XX_YY → cp_XX_YY_ZZZ。
// 所有covergroup在posedge采NBA更新前值；模型_d1等历史由NBA保存到下一拍。
// b_expected_result只表示该观察关系成立；部分/缺失项的逐点边界以mapping.json及分析文档为准。
// 结果不参与iff门控；偏离与未知在相邻diagnostic covergroup中单列，weight=0。
`ifndef MEMBLOCK_STOREUNIT_INTEGRATION_FCOV__SV
`define MEMBLOCK_STOREUNIT_INTEGRATION_FCOV__SV
`include "memblock_compile_params.svh"

`ifdef MEMBLOCK_UT_FCOV
`ifdef MEMBLOCK_FCOV_STORE_UNIT_EN
module memblock_storeunit_integration_fcov (
    input wire clock,
    input wire reset,
    input wire reset_backend_done,
    input wire m_io_redirect_valid,
    input wire m_io_redirect_bits_robIdx_flag,
    input wire [7:0] m_io_redirect_bits_robIdx_value,
    input wire m_io_redirect_bits_level,
    input wire m_io_enq_0_req_ready,
    input wire m_io_enq_0_req_valid,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_0,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_1,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_2,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_4,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_5,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_8,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_9,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_10,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_11,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_12,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_13,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_14,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_16,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_17,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_18,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_19,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_20,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_21,
    input wire m_io_enq_0_req_bits_uop_exceptionVec_22,
    input wire [3:0] m_io_enq_0_req_bits_uop_trigger,
    input wire [8:0] m_io_enq_0_req_bits_uop_fuOpType,
    input wire [7:0] m_io_enq_0_req_bits_uop_vpu_vstart,
    input wire [1:0] m_io_enq_0_req_bits_uop_vpu_veew,
    input wire [6:0] m_io_enq_0_req_bits_uop_uopIdx,
    input wire m_io_enq_0_req_bits_uop_robIdx_flag,
    input wire [7:0] m_io_enq_0_req_bits_uop_robIdx_value,
    input wire m_io_enq_0_req_bits_uop_sqIdx_flag,
    input wire [5:0] m_io_enq_0_req_bits_uop_sqIdx_value,
    input wire [49:0] m_io_enq_0_req_bits_vaddr,
    input wire [15:0] m_io_enq_0_req_bits_mask,
    input wire m_io_enq_0_req_bits_isvec,
    input wire [7:0] m_io_enq_0_req_bits_elemIdx,
    input wire [2:0] m_io_enq_0_req_bits_alignedType,
    input wire [3:0] m_io_enq_0_req_bits_mbIndex,
    input wire [4:0] m_io_enq_0_req_bits_splitIndex,
    input wire m_io_enq_0_revoke,
    input wire m_io_enq_1_req_ready,
    input wire m_io_enq_1_req_valid,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_0,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_1,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_2,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_4,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_5,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_8,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_9,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_10,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_11,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_12,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_13,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_14,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_16,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_17,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_18,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_19,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_20,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_21,
    input wire m_io_enq_1_req_bits_uop_exceptionVec_22,
    input wire [3:0] m_io_enq_1_req_bits_uop_trigger,
    input wire [8:0] m_io_enq_1_req_bits_uop_fuOpType,
    input wire [7:0] m_io_enq_1_req_bits_uop_vpu_vstart,
    input wire [1:0] m_io_enq_1_req_bits_uop_vpu_veew,
    input wire [6:0] m_io_enq_1_req_bits_uop_uopIdx,
    input wire m_io_enq_1_req_bits_uop_robIdx_flag,
    input wire [7:0] m_io_enq_1_req_bits_uop_robIdx_value,
    input wire m_io_enq_1_req_bits_uop_sqIdx_flag,
    input wire [5:0] m_io_enq_1_req_bits_uop_sqIdx_value,
    input wire [49:0] m_io_enq_1_req_bits_vaddr,
    input wire [15:0] m_io_enq_1_req_bits_mask,
    input wire m_io_enq_1_req_bits_isvec,
    input wire [7:0] m_io_enq_1_req_bits_elemIdx,
    input wire [2:0] m_io_enq_1_req_bits_alignedType,
    input wire [3:0] m_io_enq_1_req_bits_mbIndex,
    input wire [4:0] m_io_enq_1_req_bits_splitIndex,
    input wire m_io_enq_1_revoke,
    input wire m_io_rob_pendingst,
    input wire m_io_rob_pendingPtr_flag,
    input wire [7:0] m_io_rob_pendingPtr_value,
    input wire m_io_splitStoreReq_ready,
    input wire m_io_splitStoreReq_valid,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_0,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_1,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_2,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_4,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_5,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_8,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_9,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_10,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_11,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_12,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_13,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_14,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_16,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_17,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_18,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_19,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_20,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_21,
    input wire m_io_splitStoreReq_bits_uop_exceptionVec_22,
    input wire [3:0] m_io_splitStoreReq_bits_uop_trigger,
    input wire [8:0] m_io_splitStoreReq_bits_uop_fuOpType,
    input wire [7:0] m_io_splitStoreReq_bits_uop_vpu_vstart,
    input wire [1:0] m_io_splitStoreReq_bits_uop_vpu_veew,
    input wire [6:0] m_io_splitStoreReq_bits_uop_uopIdx,
    input wire m_io_splitStoreReq_bits_uop_robIdx_flag,
    input wire [7:0] m_io_splitStoreReq_bits_uop_robIdx_value,
    input wire m_io_splitStoreReq_bits_uop_sqIdx_flag,
    input wire [5:0] m_io_splitStoreReq_bits_uop_sqIdx_value,
    input wire [49:0] m_io_splitStoreReq_bits_vaddr,
    input wire [15:0] m_io_splitStoreReq_bits_mask,
    input wire m_io_splitStoreReq_bits_isvec,
    input wire m_io_splitStoreReq_bits_is128bit,
    input wire m_io_splitStoreReq_bits_isFinalSplit,
    input wire m_io_splitStoreResp_valid,
    input wire m_io_splitStoreResp_bits_uop_exceptionVec_3,
    input wire m_io_splitStoreResp_bits_uop_exceptionVec_6,
    input wire m_io_splitStoreResp_bits_uop_exceptionVec_7,
    input wire m_io_splitStoreResp_bits_uop_exceptionVec_15,
    input wire m_io_splitStoreResp_bits_uop_exceptionVec_19,
    input wire m_io_splitStoreResp_bits_uop_exceptionVec_23,
    input wire [3:0] m_io_splitStoreResp_bits_uop_trigger,
    input wire m_io_splitStoreResp_bits_uop_robIdx_flag,
    input wire [7:0] m_io_splitStoreResp_bits_uop_robIdx_value,
    input wire [49:0] m_io_splitStoreResp_bits_vaddr,
    input wire m_io_splitStoreResp_bits_vaNeedExt,
    input wire [47:0] m_io_splitStoreResp_bits_paddr,
    input wire [63:0] m_io_splitStoreResp_bits_gpaddr,
    input wire m_io_splitStoreResp_bits_nc,
    input wire m_io_splitStoreResp_bits_mmio,
    input wire m_io_splitStoreResp_bits_memBackTypeMM,
    input wire m_io_splitStoreResp_bits_isForVSnonLeafPTE,
    input wire m_io_splitStoreResp_bits_vecActive,
    input wire m_io_splitStoreResp_bits_need_rep,
    input wire m_io_writeBack_ready,
    input wire m_io_writeBack_valid,
    input wire m_io_writeBack_bits_uop_exceptionVec_3,
    input wire m_io_writeBack_bits_uop_exceptionVec_6,
    input wire m_io_writeBack_bits_uop_exceptionVec_7,
    input wire m_io_writeBack_bits_uop_exceptionVec_15,
    input wire m_io_writeBack_bits_uop_exceptionVec_19,
    input wire m_io_writeBack_bits_uop_exceptionVec_23,
    input wire [3:0] m_io_writeBack_bits_uop_trigger,
    input wire m_io_writeBack_bits_uop_robIdx_flag,
    input wire [7:0] m_io_writeBack_bits_uop_robIdx_value,
    input wire m_io_writeBack_bits_debug_isMMIO,
    input wire m_io_writeBack_bits_debug_isNCIO,
    input wire m_io_vecWriteBack_0_valid,
    input wire [3:0] m_io_vecWriteBack_0_bits_mBIndex,
    input wire m_io_vecWriteBack_0_bits_exceptionVec_3,
    input wire m_io_vecWriteBack_0_bits_exceptionVec_6,
    input wire m_io_vecWriteBack_0_bits_exceptionVec_7,
    input wire m_io_vecWriteBack_0_bits_exceptionVec_15,
    input wire m_io_vecWriteBack_0_bits_exceptionVec_19,
    input wire m_io_vecWriteBack_0_bits_exceptionVec_23,
    input wire m_io_vecWriteBack_0_bits_hasException,
    input wire [63:0] m_io_vecWriteBack_0_bits_vaddr,
    input wire m_io_vecWriteBack_0_bits_vaNeedExt,
    input wire [63:0] m_io_vecWriteBack_0_bits_gpaddr,
    input wire m_io_vecWriteBack_0_bits_isForVSnonLeafPTE,
    input wire [7:0] m_io_vecWriteBack_0_bits_vstart,
    input wire [7:0] m_io_vecWriteBack_0_bits_elemIdx,
    input wire [15:0] m_io_vecWriteBack_0_bits_mask,
    input wire [4:0] m_io_vecWriteBack_0_bits_splitIndex,
    input wire m_io_vecWriteBack_1_valid,
    input wire [3:0] m_io_vecWriteBack_1_bits_mBIndex,
    input wire m_io_vecWriteBack_1_bits_exceptionVec_3,
    input wire m_io_vecWriteBack_1_bits_exceptionVec_6,
    input wire m_io_vecWriteBack_1_bits_exceptionVec_7,
    input wire m_io_vecWriteBack_1_bits_exceptionVec_15,
    input wire m_io_vecWriteBack_1_bits_exceptionVec_19,
    input wire m_io_vecWriteBack_1_bits_exceptionVec_23,
    input wire m_io_vecWriteBack_1_bits_hasException,
    input wire [63:0] m_io_vecWriteBack_1_bits_vaddr,
    input wire m_io_vecWriteBack_1_bits_vaNeedExt,
    input wire [63:0] m_io_vecWriteBack_1_bits_gpaddr,
    input wire m_io_vecWriteBack_1_bits_isForVSnonLeafPTE,
    input wire [7:0] m_io_vecWriteBack_1_bits_vstart,
    input wire [7:0] m_io_vecWriteBack_1_bits_elemIdx,
    input wire [15:0] m_io_vecWriteBack_1_bits_mask,
    input wire [4:0] m_io_vecWriteBack_1_bits_splitIndex,
    input wire m_io_storeOutValid,
    input wire m_io_storeVecOutValid,
    input wire m_io_sqControl_toStoreQueue_crossPageWithHit,
    input wire m_io_sqControl_toStoreQueue_crossPageCanDeq,
    input wire [47:0] m_io_sqControl_toStoreQueue_paddr,
    input wire m_io_sqControl_toStoreQueue_withSamePtr,
    input wire m_io_sqControl_toStoreMisalignBuffer_sqPtr_flag,
    input wire [5:0] m_io_sqControl_toStoreMisalignBuffer_sqPtr_value,
    input wire m_io_sqControl_toStoreMisalignBuffer_doDeq,
    input wire [6:0] m_io_sqControl_toStoreMisalignBuffer_uop_uopIdx,
    input wire m_io_sqControl_toStoreMisalignBuffer_uop_robIdx_flag,
    input wire [7:0] m_io_sqControl_toStoreMisalignBuffer_uop_robIdx_value,
    input wire m_io_toVecSplit_empty,
    input wire m_cross4KBPageBoundary,
    input wire m_req_valid,
    input wire [3:0] m_req_uop_trigger,
    input wire [8:0] m_req_uop_fuOpType,
    input wire m_req_uop_robIdx_flag,
    input wire [7:0] m_req_uop_robIdx_value,
    input wire m_req_uop_sqIdx_flag,
    input wire [5:0] m_req_uop_sqIdx_value,
    input wire [49:0] m_req_vaddr,
    input wire m_req_isvec,
    input wire [2:0] m_bufferState,
    input wire m_robMatch,
    input wire m_s2_canEnq,
    input wire m_s2_reqSelPort_next_r,
    input wire m_canEnq,
    input wire [47:0] m_splitStoreResp_1_paddr,
    input wire m_isCrossPage,
    input wire m_curPtr,
    input wire m_globalException,
    input wire m_globalUncache,
    input wire m_io_sqControl_toStoreQueue_crossPageCanDeq_0,
    input wire m_io_splitStoreReq_valid_0,
    input wire m_io_writeBack_valid_0,
    input wire m_io_vecWriteBack_0_valid_0,
    input wire [63:0] m_io_vecWriteBack_1_bits_vaddr_0,
    input wire m_io_vecWriteBack_1_valid_0,
    input wire m_s2_needRevoke,
    input wire s0_io_stin_valid,
    input wire s0_io_tlb_resp_valid,
    input wire s0_io_tlb_resp_bits_miss,
    input wire [49:0] s0_io_tlb_req_bits_vaddr,
    input wire s0_io_stout_valid,
    input wire s0_io_vecstout_valid,
    input wire s0_io_lsq_valid,
    input wire s0_io_lsq_bits_miss,
    input wire s0_io_lsq_bits_isFrmMisAlignBuf,
    input wire s0_io_lsq_bits_uop_robIdx_flag,
    input wire [7:0] s0_io_lsq_bits_uop_robIdx_value,
    input wire [5:0] s0_io_lsq_bits_uop_sqIdx_value,
    input wire [47:0] s0_io_lsq_bits_paddr,
    input wire s0_io_lsq_bits_isMisalign,
    input wire s0_io_lsq_bits_wlineflag,
    input wire [15:0] s0_io_lsq_bits_mask,
    input wire s0_io_lsq_replenish_updateAddrValid,
    input wire s0_io_lsq_replenish_mmio,
    input wire s0_io_lsq_replenish_hasException,
    input wire s0_io_lsq_replenish_memBackTypeMM,
    input wire s0_io_feedback_slow_valid,
    input wire s0_io_feedback_slow_bits_hit,
    input wire s0_io_misalign_stout_valid,
    input wire s0_io_misalign_stin_valid,
    input wire s0_io_stout_bits_uop_robIdx_flag,
    input wire [7:0] s0_io_stout_bits_uop_robIdx_value,
    input wire [3:0] s0_io_stout_bits_uop_trigger,
    input wire s0_s2_exception,
    input wire s0_s2_kill,
    input wire s0_s2_valid,
    input wire q_io_storeAddrIn_0_valid,
    input wire q_io_storeAddrIn_0_bits_miss,
    input wire [5:0] q_io_storeAddrIn_0_bits_uop_sqIdx_value,
    input wire q_io_storeAddrInRe_0_updateAddrValid,
    input wire q_io_storeAddrInRe_0_mmio,
    input wire q_io_storeAddrInRe_0_hasException,
    input wire q_io_storeAddrInRe_0_memBackTypeMM,
    input wire q_io_storeDataIn_0_valid,
    input wire [5:0] q_io_storeDataIn_0_bits_uop_sqIdx_value,
    input wire q_storeAddrInFireReg,
    input wire [5:0] q_stWbIndexReg,
    input wire s1_io_stin_valid,
    input wire s1_io_tlb_resp_valid,
    input wire s1_io_tlb_resp_bits_miss,
    input wire [49:0] s1_io_tlb_req_bits_vaddr,
    input wire s1_io_stout_valid,
    input wire s1_io_vecstout_valid,
    input wire s1_io_lsq_valid,
    input wire s1_io_lsq_bits_miss,
    input wire s1_io_lsq_bits_isFrmMisAlignBuf,
    input wire s1_io_lsq_bits_uop_robIdx_flag,
    input wire [7:0] s1_io_lsq_bits_uop_robIdx_value,
    input wire [5:0] s1_io_lsq_bits_uop_sqIdx_value,
    input wire [47:0] s1_io_lsq_bits_paddr,
    input wire s1_io_lsq_bits_isMisalign,
    input wire s1_io_lsq_bits_wlineflag,
    input wire [15:0] s1_io_lsq_bits_mask,
    input wire s1_io_lsq_replenish_updateAddrValid,
    input wire s1_io_lsq_replenish_mmio,
    input wire s1_io_lsq_replenish_hasException,
    input wire s1_io_lsq_replenish_memBackTypeMM,
    input wire s1_io_feedback_slow_valid,
    input wire s1_io_feedback_slow_bits_hit,
    input wire s1_io_misalign_stout_valid,
    input wire s1_io_misalign_stin_valid,
    input wire s1_io_stout_bits_uop_robIdx_flag,
    input wire [7:0] s1_io_stout_bits_uop_robIdx_value,
    input wire [3:0] s1_io_stout_bits_uop_trigger,
    input wire s1_s2_exception,
    input wire s1_s2_kill,
    input wire s1_s2_valid,
    input wire q_io_storeAddrIn_1_valid,
    input wire q_io_storeAddrIn_1_bits_miss,
    input wire [5:0] q_io_storeAddrIn_1_bits_uop_sqIdx_value,
    input wire q_io_storeAddrInRe_1_updateAddrValid,
    input wire q_io_storeAddrInRe_1_mmio,
    input wire q_io_storeAddrInRe_1_hasException,
    input wire q_io_storeAddrInRe_1_memBackTypeMM,
    input wire q_io_storeDataIn_1_valid,
    input wire [5:0] q_io_storeDataIn_1_bits_uop_sqIdx_value,
    input wire q_storeAddrInFireReg_1,
    input wire [5:0] q_stWbIndexReg_1,
    input wire [55:0] q_pending,
    input wire [55:0] q_mmio,
    input wire [55:0] q_hasException,
    input wire [55:0] q_addrvalid,
    input wire [55:0] q_datavalid,
    input wire [55:0] q_allocated,
    input wire _inner_otherStoutConnect_io_out_valid,
    input wire _inner_otherStoutConnect_io_out_bits_uop_robIdx_flag,
    input wire [7:0] _inner_otherStoutConnect_io_out_bits_uop_robIdx_value,
    input wire io_mem_to_ooo_writebackSta_0_valid,
    input wire io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_flag,
    input wire [7:0] io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_value,
    input wire io_mem_to_ooo_writebackSta_1_valid,
    input wire io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,
    input wire [7:0] io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value,
    input wire inner__6,
    input wire inner__7,
    input wire inner_,
    input wire m_reqSel_3_0,
    input wire [55:0] xq_committed,
    input wire [55:0] xq_memBackTypeMM,
    input wire [55:0] xq_unaligned,
    input wire [55:0][8:0] xq_rob,
    input wire [127:0] xq_io_storeDataIn_0_bits_data,
    input wire [8:0] xq_io_storeDataIn_0_bits_uop_fuOpType,
    input wire xq_io_sbuffer_0_valid,
    input wire xq_io_sbuffer_0_ready,
    input wire [127:0] xq_io_sbuffer_0_bits_data,
    input wire [15:0] xq_io_sbuffer_0_bits_mask,
    input wire [47:0] xq_io_sbuffer_0_bits_addr,
    input wire [49:0] xq_io_sbuffer_0_bits_vaddr,
    input wire xq_io_sbuffer_0_bits_wline,
    input wire xq_io_sbuffer_0_bits_vecValid,
    input wire db_io_enq_0_ready,
    input wire db_io_enq_0_valid,
    input wire [5:0] db_io_enq_0_bits_sqPtr_value,
    input wire db_io_enq_0_bits_vecValid,
    input wire [127:0] db_io_enq_0_bits_data,
    input wire [15:0] db_io_enq_0_bits_mask,
    input wire [47:0] db_io_enq_0_bits_addr,
    input wire [49:0] db_io_enq_0_bits_vaddr,
    input wire db_io_enq_0_bits_wline,
    input wire [5:0] db_io_deq_0_bits_sqPtr_value,
    input wire [49:0] x0_io_lsq_bits_vaddr,
    input wire [8:0] x0_io_lsq_bits_uop_fuOpType,
    input wire x0_io_lsq_bits_uop_sqIdx_flag,
    input wire x0_io_lsq_replenish_uop_robIdx_flag,
    input wire [7:0] x0_io_lsq_replenish_uop_robIdx_value,
    input wire [3:0] x0_io_misalign_enq_req_bits_uop_trigger,
    input wire [3:0] x0__storeTrigger_tdataVec_io_toLoadStore_triggerAction,
    input wire [127:0] xq_io_storeDataIn_1_bits_data,
    input wire [8:0] xq_io_storeDataIn_1_bits_uop_fuOpType,
    input wire xq_io_sbuffer_1_valid,
    input wire xq_io_sbuffer_1_ready,
    input wire [127:0] xq_io_sbuffer_1_bits_data,
    input wire [15:0] xq_io_sbuffer_1_bits_mask,
    input wire [47:0] xq_io_sbuffer_1_bits_addr,
    input wire [49:0] xq_io_sbuffer_1_bits_vaddr,
    input wire xq_io_sbuffer_1_bits_wline,
    input wire xq_io_sbuffer_1_bits_vecValid,
    input wire db_io_enq_1_ready,
    input wire db_io_enq_1_valid,
    input wire [5:0] db_io_enq_1_bits_sqPtr_value,
    input wire db_io_enq_1_bits_vecValid,
    input wire [127:0] db_io_enq_1_bits_data,
    input wire [15:0] db_io_enq_1_bits_mask,
    input wire [47:0] db_io_enq_1_bits_addr,
    input wire [49:0] db_io_enq_1_bits_vaddr,
    input wire db_io_enq_1_bits_wline,
    input wire [5:0] db_io_deq_1_bits_sqPtr_value,
    input wire [49:0] x1_io_lsq_bits_vaddr,
    input wire [8:0] x1_io_lsq_bits_uop_fuOpType,
    input wire x1_io_lsq_bits_uop_sqIdx_flag,
    input wire x1_io_lsq_replenish_uop_robIdx_flag,
    input wire [7:0] x1_io_lsq_replenish_uop_robIdx_value,
    input wire [3:0] x1_io_misalign_enq_req_bits_uop_trigger,
    input wire [3:0] x1__storeTrigger_tdataVec_io_toLoadStore_triggerAction,
    input wire l0_s2_valid,
    input wire l0_s2_fire,
    input wire l0_s2_kill,
    input wire [47:0] l0_s2_in_r_paddr,
    input wire [15:0] l0_s2_in_r_mask,
    input wire l0_s2_in_r_uop_robIdx_flag,
    input wire [7:0] l0_s2_in_r_uop_robIdx_value,
    input wire l0_s2_in_r_isvec,
    input wire l0_s2_in_r_misalignWith16Byte,
    input wire l0_s2_in_r_is128bit,
    input wire l0_s2_in_r_tlbMiss,
    input wire l0_s2_exception,
    input wire l0_s2_uncache,
    input wire l0_s2_nc_with_data,
    input wire l0_s2_in_r_isPrefetch,
    input wire l0_s2_out_rep_info_cause_9,
    input wire l0_s3_in_rep_info_cause_9,
    input wire l0_s3_in_uop_robIdx_flag,
    input wire [7:0] l0_s3_in_uop_robIdx_value,
    input wire l0_s3_fast_rep_REG,
    input wire l0_io_lsq_ldin_valid,
    input wire l0_io_lsq_ldin_bits_rep_info_cause_9,
    input wire l0_io_stld_nuke_query_0_valid,
    input wire l0_io_stld_nuke_query_0_bits_robIdx_flag,
    input wire [7:0] l0_io_stld_nuke_query_0_bits_robIdx_value,
    input wire [47:0] l0_io_stld_nuke_query_0_bits_paddr,
    input wire [15:0] l0_io_stld_nuke_query_0_bits_mask,
    input wire [1:0] l0_io_stld_nuke_query_0_bits_matchType,
    input wire l0_io_stld_nuke_query_1_valid,
    input wire l0_io_stld_nuke_query_1_bits_robIdx_flag,
    input wire [7:0] l0_io_stld_nuke_query_1_bits_robIdx_value,
    input wire [47:0] l0_io_stld_nuke_query_1_bits_paddr,
    input wire [15:0] l0_io_stld_nuke_query_1_bits_mask,
    input wire [1:0] l0_io_stld_nuke_query_1_bits_matchType,
    input wire l1_s2_valid,
    input wire l1_s2_fire,
    input wire l1_s2_kill,
    input wire [47:0] l1_s2_in_r_paddr,
    input wire [15:0] l1_s2_in_r_mask,
    input wire l1_s2_in_r_uop_robIdx_flag,
    input wire [7:0] l1_s2_in_r_uop_robIdx_value,
    input wire l1_s2_in_r_isvec,
    input wire l1_s2_in_r_misalignWith16Byte,
    input wire l1_s2_in_r_is128bit,
    input wire l1_s2_in_r_tlbMiss,
    input wire l1_s2_exception,
    input wire l1_s2_uncache,
    input wire l1_s2_nc_with_data,
    input wire l1_s2_in_r_isPrefetch,
    input wire l1_s2_out_rep_info_cause_9,
    input wire l1_s3_in_rep_info_cause_9,
    input wire l1_s3_in_uop_robIdx_flag,
    input wire [7:0] l1_s3_in_uop_robIdx_value,
    input wire l1_s3_fast_rep_REG,
    input wire l1_io_lsq_ldin_valid,
    input wire l1_io_lsq_ldin_bits_rep_info_cause_9,
    input wire l1_io_stld_nuke_query_0_valid,
    input wire l1_io_stld_nuke_query_0_bits_robIdx_flag,
    input wire [7:0] l1_io_stld_nuke_query_0_bits_robIdx_value,
    input wire [47:0] l1_io_stld_nuke_query_0_bits_paddr,
    input wire [15:0] l1_io_stld_nuke_query_0_bits_mask,
    input wire [1:0] l1_io_stld_nuke_query_0_bits_matchType,
    input wire l1_io_stld_nuke_query_1_valid,
    input wire l1_io_stld_nuke_query_1_bits_robIdx_flag,
    input wire [7:0] l1_io_stld_nuke_query_1_bits_robIdx_value,
    input wire [47:0] l1_io_stld_nuke_query_1_bits_paddr,
    input wire [15:0] l1_io_stld_nuke_query_1_bits_mask,
    input wire [1:0] l1_io_stld_nuke_query_1_bits_matchType,
    input wire l2_s2_valid,
    input wire l2_s2_fire,
    input wire l2_s2_kill,
    input wire [47:0] l2_s2_in_r_paddr,
    input wire [15:0] l2_s2_in_r_mask,
    input wire l2_s2_in_r_uop_robIdx_flag,
    input wire [7:0] l2_s2_in_r_uop_robIdx_value,
    input wire l2_s2_in_r_isvec,
    input wire l2_s2_in_r_misalignWith16Byte,
    input wire l2_s2_in_r_is128bit,
    input wire l2_s2_in_r_tlbMiss,
    input wire l2_s2_exception,
    input wire l2_s2_uncache,
    input wire l2_s2_nc_with_data,
    input wire l2_s2_in_r_isPrefetch,
    input wire l2_s2_out_rep_info_cause_9,
    input wire l2_s3_in_rep_info_cause_9,
    input wire l2_s3_in_uop_robIdx_flag,
    input wire [7:0] l2_s3_in_uop_robIdx_value,
    input wire l2_s3_fast_rep_REG,
    input wire l2_io_lsq_ldin_valid,
    input wire l2_io_lsq_ldin_bits_rep_info_cause_9,
    input wire l2_io_stld_nuke_query_0_valid,
    input wire l2_io_stld_nuke_query_0_bits_robIdx_flag,
    input wire [7:0] l2_io_stld_nuke_query_0_bits_robIdx_value,
    input wire [47:0] l2_io_stld_nuke_query_0_bits_paddr,
    input wire [15:0] l2_io_stld_nuke_query_0_bits_mask,
    input wire [1:0] l2_io_stld_nuke_query_0_bits_matchType,
    input wire l2_io_stld_nuke_query_1_valid,
    input wire l2_io_stld_nuke_query_1_bits_robIdx_flag,
    input wire [7:0] l2_io_stld_nuke_query_1_bits_robIdx_value,
    input wire [47:0] l2_io_stld_nuke_query_1_bits_paddr,
    input wire [15:0] l2_io_stld_nuke_query_1_bits_mask,
    input wire [1:0] l2_io_stld_nuke_query_1_bits_matchType,
    input wire sb_io_in_0_ready,
    input wire sb_io_in_0_valid,
    input wire [127:0] sb_io_in_0_bits_data,
    input wire [15:0] sb_io_in_0_bits_mask,
    input wire [47:0] sb_io_in_0_bits_addr,
    input wire [49:0] sb_io_in_0_bits_vaddr,
    input wire sb_io_in_0_bits_wline,
    input wire sb_io_in_0_bits_vecValid,
    input wire sb_io_in_1_ready,
    input wire sb_io_in_1_valid,
    input wire [127:0] sb_io_in_1_bits_data,
    input wire [15:0] sb_io_in_1_bits_mask,
    input wire [47:0] sb_io_in_1_bits_addr,
    input wire [49:0] sb_io_in_1_bits_vaddr,
    input wire sb_io_in_1_bits_wline,
    input wire sb_io_in_1_bits_vecValid,
    input wire c0_expected1,
    input wire c0_expected2,
    input wire c0_expected3,
    input wire c0_expected4,
    input wire c0_exp_fire,
    input wire c0_selected_scalar,
    input wire [8:0] c0_op1,
    input wire [8:0] c0_op2,
    input wire [8:0] c0_rob1,
    input wire [8:0] c0_rob2,
    input wire [8:0] c0_rob4,
    input wire [6:0] c0_sq1,
    input wire [6:0] c0_sq2,
    input wire [6:0] c0_req_sq,
    input wire [8:0] c0_req_rob,
    input wire [8:0] c0_req_op,
    input wire c0_child1,
    input wire c0_child2,
    input wire c0_child4,
    input wire c0_final1,
    input wire c0_hit1,
    input wire c0_exp_exception1,
    input wire [49:0] c0_va1,
    input wire [49:0] c0_va2,
    input wire [49:0] c0_expected_va0,
    input wire [15:0] c0_expected_mask0,
    input wire [1:0] c0_pbmt2,
    input wire [3:0] c0_action2,
    input wire c0_tlbhit2,
    input wire c0_incoming_fault2,
    input wire c0_exp_exception2,
    input wire c0_exp_af2,
    input wire c0_exp_mmio2,
    input wire c0_strict2,
    input wire c0_qualified2,
    input wire c0_ready2,
    input wire c0_candidate2,
    input wire c0_candidate_qualified2,
    input wire c0_candidate_miss2,
    input wire c0_replay2,
    input wire c0_fb_ok,
    input wire c0_fb_due,
    input wire [8:0] c0_fb_rob,
    input wire [6:0] c0_fb_sq,
    input wire c0_fb_expected_hit,
    input wire c0_trace2,
    input wire c0_trace4,
    input wire c0_wb_ok,
    input wire c0_s2_ok,
    input wire c0_attributes_ok,
    input wire c0_s0_ok,
    input wire c0_child_ok,
    input wire c0_admit2,
    input wire c0_in_ex15_2,
    input wire c0_in_ex23_2,
    input wire c0_in_ex7_2,
    input wire c0_miss2,
    input wire [5:0] c0_exp_ex2,
    input wire z0_s2_out_af,
    input wire z0_s2_exception,
    input wire z0_s1_in_uop_robIdx_flag,
    input wire [7:0] z0_s1_in_uop_robIdx_value,
    input wire z0_s1_kill,
    input wire z0_s2_kill,
    input wire [3:0] z0__storeTrigger_tdataVec_io_toLoadStore_triggerAction,
    input wire c1_expected1,
    input wire c1_expected2,
    input wire c1_expected3,
    input wire c1_expected4,
    input wire c1_exp_fire,
    input wire c1_selected_scalar,
    input wire [8:0] c1_op1,
    input wire [8:0] c1_op2,
    input wire [8:0] c1_rob1,
    input wire [8:0] c1_rob2,
    input wire [8:0] c1_rob4,
    input wire [6:0] c1_sq1,
    input wire [6:0] c1_sq2,
    input wire [6:0] c1_req_sq,
    input wire [8:0] c1_req_rob,
    input wire [8:0] c1_req_op,
    input wire c1_child1,
    input wire c1_child2,
    input wire c1_child4,
    input wire c1_final1,
    input wire c1_hit1,
    input wire c1_exp_exception1,
    input wire [49:0] c1_va1,
    input wire [49:0] c1_va2,
    input wire [49:0] c1_expected_va0,
    input wire [15:0] c1_expected_mask0,
    input wire [1:0] c1_pbmt2,
    input wire [3:0] c1_action2,
    input wire c1_tlbhit2,
    input wire c1_incoming_fault2,
    input wire c1_exp_exception2,
    input wire c1_exp_af2,
    input wire c1_exp_mmio2,
    input wire c1_strict2,
    input wire c1_qualified2,
    input wire c1_ready2,
    input wire c1_candidate2,
    input wire c1_candidate_qualified2,
    input wire c1_candidate_miss2,
    input wire c1_replay2,
    input wire c1_fb_ok,
    input wire c1_fb_due,
    input wire [8:0] c1_fb_rob,
    input wire [6:0] c1_fb_sq,
    input wire c1_fb_expected_hit,
    input wire c1_trace2,
    input wire c1_trace4,
    input wire c1_wb_ok,
    input wire c1_s2_ok,
    input wire c1_attributes_ok,
    input wire c1_s0_ok,
    input wire c1_child_ok,
    input wire c1_admit2,
    input wire c1_in_ex15_2,
    input wire c1_in_ex23_2,
    input wire c1_in_ex7_2,
    input wire c1_miss2,
    input wire [5:0] c1_exp_ex2,
    input wire z1_s2_out_af,
    input wire z1_s2_exception,
    input wire z1_s1_in_uop_robIdx_flag,
    input wire [7:0] z1_s1_in_uop_robIdx_value,
    input wire z1_s1_kill,
    input wire z1_s2_kill,
    input wire [3:0] z1__storeTrigger_tdataVec_io_toLoadStore_triggerAction,
    input wire [55:0] zq_completed,
    input wire [55:0] zq_nc,
    input wire [55:0] zq_cross16Byte,
    input wire zq_io_cmoOpReq_ready,
    input wire zq_io_cmoOpReq_valid,
    input wire [2:0] zq_io_cmoOpReq_bits_opcode,
    input wire [63:0] zq_io_cmoOpReq_bits_address,
    input wire zq_io_cmoOpResp_valid,
    input wire zq_io_cmoOpResp_ready,
    input wire zq_io_cmoOpResp_bits_denied,
    input wire zq_io_cmoOpResp_bits_corrupt,
    input wire zq_io_uncache_req_valid,
    input wire zq_io_uncache_req_ready,
    input wire [6:0] zq_io_uncache_req_bits_id,
    input wire zq_io_uncache_req_bits_robIdx_flag,
    input wire [7:0] zq_io_uncache_req_bits_robIdx_value,
    input wire zq_io_uncache_req_bits_nc,
    input wire [47:0] zq_io_uncache_req_bits_addr,
    input wire [7:0] zq_io_uncache_req_bits_mask,
    input wire [63:0] zq_io_uncache_req_bits_data,
    input wire zq_io_uncache_resp_valid,
    input wire zq_io_uncache_resp_bits_nc,
    input wire zq_io_uncache_resp_bits_denied,
    input wire zq_io_uncache_resp_bits_corrupt,
    input wire zq_io_mmioStout_valid,
    input wire zq_io_mmioStout_ready,
    input wire zq_io_mmioStout_bits_uop_robIdx_flag,
    input wire [7:0] zq_io_mmioStout_bits_uop_robIdx_value,
    input wire zq_io_mmioStout_bits_uop_exceptionVec_7,
    input wire zq_io_mmioStout_bits_uop_exceptionVec_19,
    input wire zq_io_cboZeroStout_valid,
    input wire zq_io_cboZeroStout_ready,
    input wire zq_io_cboZeroStout_bits_uop_robIdx_flag,
    input wire [7:0] zq_io_cboZeroStout_bits_uop_robIdx_value,
    input wire [5:0] zq_deqPtrExt_0_value,
    input wire [2:0] zq_mmioState,
    input wire [8:0] zq_uncacheUop_fuOpType,
    input wire zq_uncacheUop_robIdx_flag,
    input wire [7:0] zq_uncacheUop_robIdx_value,
    input wire zq_io_flushSbuffer_empty,
    input wire [63:0] zm_req_gpaddr,
    input wire zm_req_vaNeedExt,
    input wire zm_req_isForVSnonLeafPTE,
    input wire [1:0] zm_unSentStores,
    input wire [31:0] raw_allocated,
    input wire [31:0] raw_datavalid,
    input wire [31:0][8:0] raw_rob,
    input wire [31:0][23:0] raw_pa,
    input wire [31:0][15:0] raw_mask,
    input wire raw_rollback0_valid,
    input wire raw_rollback0_bits_robIdx_flag,
    input wire [7:0] raw_rollback0_bits_robIdx_value,
    input wire raw_rollback1_valid,
    input wire raw_rollback1_bits_robIdx_flag,
    input wire [7:0] raw_rollback1_bits_robIdx_value,
    input wire z0_io_pmp_mmio,
    input wire z1_io_pmp_mmio,
    input wire z0_io_pmp_st,
    input wire z1_io_pmp_st,
    input wire raw_io_redirect_valid,
    input wire raw_io_redirect_bits_robIdx_flag,
    input wire [7:0] raw_io_redirect_bits_robIdx_value,
    input wire raw_io_redirect_bits_level,
    input wire raw_store0_valid,
    input wire [47:0] raw_store0_bits_paddr,
    input wire [15:0] raw_store0_bits_mask,
    input wire raw_store0_bits_wlineflag,
    input wire raw_store0_bits_miss,
    input wire raw_store0_bits_uop_robIdx_flag,
    input wire [7:0] raw_store0_bits_uop_robIdx_value,
    input wire raw_store1_valid,
    input wire [47:0] raw_store1_bits_paddr,
    input wire [15:0] raw_store1_bits_mask,
    input wire raw_store1_bits_wlineflag,
    input wire raw_store1_bits_miss,
    input wire raw_store1_bits_uop_robIdx_flag,
    input wire [7:0] raw_store1_bits_uop_robIdx_value,
    input wire cam_paddrModule_wen0,
    input wire [5:0] cam_paddrModule_waddr0,
    input wire [47:0] cam_paddrModule_wdata0,
    input wire [15:0] cam_paddrModule_wmask0,
    input wire cam_paddrModule_wlineflag0,
    input wire cam_vaddrModule_wen0,
    input wire [5:0] cam_vaddrModule_waddr0,
    input wire [49:0] cam_vaddrModule_wdata0,
    input wire [15:0] cam_vaddrModule_wmask0,
    input wire cam_vaddrModule_wlineflag0,
    input wire zz0_io_lsq_bits_isFrmMisAlignBuf,
    input wire zz0_io_lsq_bits_updateAddrValid,
    input wire zz0_io_lsq_bits_misalignWith16Byte,
    input wire zz0_io_lsq_replenish_af,
    input wire zz0_io_lsq_replenish_hasException,
    input wire zz0_s2_in_isvec,
    input wire cam_paddrModule_wen1,
    input wire [5:0] cam_paddrModule_waddr1,
    input wire [47:0] cam_paddrModule_wdata1,
    input wire [15:0] cam_paddrModule_wmask1,
    input wire cam_paddrModule_wlineflag1,
    input wire cam_vaddrModule_wen1,
    input wire [5:0] cam_vaddrModule_waddr1,
    input wire [49:0] cam_vaddrModule_wdata1,
    input wire [15:0] cam_vaddrModule_wmask1,
    input wire cam_vaddrModule_wlineflag1,
    input wire zz1_io_lsq_bits_isFrmMisAlignBuf,
    input wire zz1_io_lsq_bits_updateAddrValid,
    input wire zz1_io_lsq_bits_misalignWith16Byte,
    input wire zz1_io_lsq_replenish_af,
    input wire zz1_io_lsq_replenish_hasException,
    input wire zz1_s2_in_isvec,
    input wire [6:0] z_mab_uop0,
    input wire [6:0] z_mab_uop1
);
    wire sample_enable;
    reg history_valid;
    wire [8:0] parent_rob;
    wire parent_scalar;
    wire parent_flush;
    wire m_response;
    wire response_identity;
    wire response_fault;
    wire response_retry;
    wire response_uncache;
    wire response_ok;
    reg child0_done,child1_done;
    reg [47:0] high_pa;
    reg [7:0] child_retry_count;
    reg m_response_d1,m_retry_d1,m_ptr_d1,m_fault_d1,m_uncache_d1;
    reg [8:0] parent_rob_d1;
    reg [49:0] fault_va;
    reg m_parent_wb_d1,m_cross_d1,m_deq_d1;
    reg m_parent_flush_d1;
    reg [2:0] m_state_d1;
    reg m_reqvalid_d1;
    reg [3:0] m_saved_trigger;
    wire parent_wb;
    wire m_wb_identity;
    wire [8:0] sta0_rob;
    wire [8:0] sta1_rob;
    wire [8:0] other_rob;
    wire [8:0] final0_rob;
    wire expected_mab_select;
    wire expected_other_select;
    wire expected_final_valid;
    wire final_mux_ok;
    reg m_selected_accept_d1;
    reg m_selected_port_d1;
    reg q_valid_d1_0,q_miss_d1_0,q_valid_d1_1,q_miss_d1_1;
    reg [5:0] q_index_d1_0,q_index_d1_1;
    reg q_update_d1_0,q_update_d1_1,q_expected_pending0,q_expected_pending1,q_expected_exception0,q_expected_exception1;
    reg [5:0] q_update_index0,q_update_index1;
    wire q_apply0;
    wire q_apply1;
    wire q_enable_ok0;
    wire q_enable_ok1;
    wire q_state_ok0;
    wire q_state_ok1;
    reg [55:0] data_first,addr_first;
    wire [5:0] dindex0;
    wire [5:0] dindex1;
    reg life_active,life_bad,life_wait_sq,life_wait_vec,life_wait_rob,life_child_retry;
    reg [8:0] life_rob;
    reg [49:0] life_va,life_low_va;
    reg [47:0] life_low_pa,life_high_pa;
    reg [8:0] life_op;
    reg [1:0] life_children,life_writebacks;
    reg [3:0] life_s1_trigger,life_early_trigger;
    reg life_stalled_d1;
    reg [18:0] life_payload_d1;
    wire [18:0] life_payload;
    reg life_wb_due,life_wb_good,life_release_due,life_release_good;
    reg [2:0] life_faultkind;
    reg life_release_cross;
    reg [55:0] causal_active,causal_addr_seen,causal_data_seen,causal_bad,causal_exception,causal_order_addr,causal_order_data;
    reg [8:0] causal_rob [0:55];
    reg [49:0] causal_va [0:55];
    reg [47:0] causal_pa [0:55];
    reg [15:0] causal_mask [0:55];
    reg [127:0] causal_data [0:55];
    reg [8:0] causal_op [0:55];
    reg [6:0] causal_update_sq;
    integer init_i;
    wire life_response;
    wire life_identity_ok;
    wire life_finish_context;
    wire life_finish_good;
    wire life_wait_stable;
    wire life_slot_clear;
    reg q_attr_due0,q_attr_expected_mmio0,q_attr_expected_mem0,q_attr_expected_ex0,q_attr_enable_result0;
    reg [5:0] q_attr_index0;
    reg [8:0] q_attr_rob0;
    wire q_attr_result0;
    reg q_attr_due1,q_attr_expected_mmio1,q_attr_expected_mem1,q_attr_expected_ex1,q_attr_enable_result1;
    reg [5:0] q_attr_index1;
    reg [8:0] q_attr_rob1;
    wire q_attr_result1;
    wire [8:0] c_rob0;
    wire c_address0;
    wire c_data0;
    wire [127:0] c_repeated0;
    wire [8:0] c_rob1;
    wire c_address1;
    wire c_data1;
    wire [127:0] c_repeated1;
    reg [127:0] transfer_data [0:3];
    reg [15:0] transfer_mask [0:3];
    reg [47:0] transfer_pa [0:3];
    reg [49:0] transfer_va [0:3];
    reg [5:0] transfer_sq [0:3];
    reg transfer_known [0:3];
    reg transfer_good [0:3];
    reg transfer_order_a [0:3];
    reg transfer_order_d [0:3];
    reg transfer_fault [0:3];
    reg transfer_wline [0:3];
    reg [2:0] transfer_count;
    reg [1:0] transfer_head,transfer_tail;
    wire push0;
    wire push1;
    wire pop0;
    wire pop1;
    wire [1:0] push_count;
    wire [1:0] pop_count;
    wire [1:0] tail0;
    wire [1:0] head0;
    wire [5:0] enq_index0;
    wire transfer_source0;
    wire transfer_aligned0;
    wire transfer_input_good0;
    wire buffer_link_ok0;
    wire transfer_output_good0;
    wire [1:0] tail1;
    wire [1:0] head1;
    wire [5:0] enq_index1;
    wire transfer_source1;
    wire transfer_aligned1;
    wire transfer_input_good1;
    wire buffer_link_ok1;
    wire transfer_output_good1;
    reg l0_due;
    reg [8:0] l0_rob_d1;
    reg l0_relation_d1;
    wire l0_match0;
    wire l0_match1;
    wire l0_result;
    reg l1_due;
    reg [8:0] l1_rob_d1;
    reg l1_relation_d1;
    wire l1_match0;
    wire l1_match1;
    wire l1_result;
    reg l2_due;
    reg [8:0] l2_rob_d1;
    reg l2_relation_d1;
    wire l2_match0;
    wire l2_match1;
    wire l2_result;
    wire split_pair0;
    wire [255:0] split_data0;
    wire [31:0] split_mask0;
    wire [127:0] independent_data0;
    wire [15:0] independent_mask0;
    wire [47:0] independent_pa0;
    wire [49:0] independent_va0;
    wire split_pair1;
    wire [255:0] split_data1;
    wire [31:0] split_mask1;
    wire [127:0] independent_data1;
    wire [15:0] independent_mask1;
    wire [47:0] independent_pa1;
    wire [49:0] independent_va1;
    reg [55:0] end_generation_flag;
    reg [55:0] end_active,end_bad,end_wb_seen,end_mab_seen,end_mem_seen,end_nc_done;
    reg [55:0] end_bad_work;
    reg [8:0] end_rob [0:55];
    reg [8:0] end_op [0:55];
    reg [1:0] end_pbmt [0:55];
    reg end_pma [0:55];
    reg [49:0] end_va [0:55];
    reg [47:0] end_pa [0:55];
    reg [3:0] end_action [0:55];
    reg [5:0] end_ex [0:55];
    reg [3:0] end_ready_mask [0:55];
    reg [2:0] end_enqueues [0:55];
    reg [7:0] end_max_child_retries [0:55];
    reg [47:0] end_high_pa [0:55];
    reg [55:0] end_high_known;
    reg [1:0] end_bytes_out [0:55];
    integer end_init;
    reg [6:0] normal_sq3_0,normal_sq4_0,normal_sq3_1,normal_sq4_1;
    wire [5:0] special_slot;
    wire [5:0] mab_slot;
    wire special_due;
    wire special_identity;
    wire zero_due;
    reg [5:0] zero_slot;
    reg [8:0] zero_rob;
    reg zero_pending,zero_data_ok;
    reg cmo_pending,cmo_good,cmo_resp_seen,cmo_resp_error,cmo_denied,cmo_corrupt;
    reg mmio_request_seen,mmio_response_seen,mmio_transaction_good,mmio_denied,mmio_corrupt;
    reg [8:0] mmio_owner;
    reg [6:0] mmio_id;
    reg [3:0] mmio_zero_beats;
    wire [63:0] mmio_expected_data;
    wire [7:0] mmio_expected_mask;
    wire special_is_cmo;
    wire special_response_good;
    reg [8:0] cmo_rob;
    reg [47:0] cmo_block;
    wire special_result;
    wire mab_finish_expected;
    wire end_mab_good;
    wire end_zero_good;
    reg end_mab_wrote_d1,end_mab_release_d1;
    reg [5:0] end_mab_slot_d1;
    reg [8:0] end_mab_rob_d1;
    reg end_hold_bad;
    reg [47:0] end_hold_high;
    reg end_wait_cross;
    reg [2:0] end_child_miss_run;
    wire end_mab_hold_good;
    wire end_nc_fault_complete;
    reg end_nc_fault_relation;
    reg [55:0] end_nc_fault_pulse;
    reg [55:0] end_completed_d1;
    wire end_start0;
    wire end_prepare0;
    wire end_normal_due0;
    wire [5:0] end_normal_slot0;
    wire end_normal_good0;
    reg end_prepare_seen0;
    wire end_start1;
    wire end_prepare1;
    wire end_normal_due1;
    wire [5:0] end_normal_slot1;
    wire end_normal_good1;
    reg end_prepare_seen1;
    reg rc_due,rc_good,rc_final,rc_miss,rc_fault,rc_nc,rc_mmio,rc_pf_saved,rc_gpf_saved,rc_af_saved;
    reg [8:0] rc_rob;
    reg [49:0] rc_va;
    reg [63:0] rc_fullva,rc_gpa;
    reg rc_ext,rc_nonleaf,rc_hyper;
    wire rc_next_ok;
    wire rc_fault_addr_ok;
    reg revoke_due0,revoke_due1,revoke_accepted0,revoke_accepted1;
    reg revoke_expect0,revoke_expect1;
    reg [8:0] revoke_rob0,revoke_rob1;
    reg dual_candidate_due,dual_candidate_good;
    reg [55:0] replay_feedback_bad;
    reg raw_any_pending;
    reg [7:0] revoke_kind0;
    reg revoke_check_due0;
    reg revoke_check_good0;
    reg [7:0] revoke_kind_d1_0;
    reg rcandidate0;
    reg [7:0] revoke_kind1;
    reg revoke_check_due1;
    reg revoke_check_good1;
    reg [7:0] revoke_kind_d1_1;
    reg rcandidate1;
    reg raw_match0;
    reg [2:0] raw_due0;
    wire [8:0] raw_store_rob0;
    wire raw_result0;
    reg raw_match1;
    reg [2:0] raw_due1;
    wire [8:0] raw_store_rob1;
    wire [8:0] raw_red;
    wire raw_result1;
    reg split_complete_due;
    reg [5:0] split_complete_slot;
    reg split_complete_good;
    reg [55:0] split_low_seen;
    reg [55:0] split_low_good;
    reg [55:0] end_write_bad;
    reg [7:0] end_rejects[0:55];
    reg [1:0] selected_accept_mask;
    reg [8:0] accept_rob1_0,accept_rob1_1,accept_rob2_0,accept_rob2_1;
    reg [1:0] accepted_revoke_relation;
    reg [1:0] accepted_at_s1;
    reg [7:0] origin_kind0,origin_kind1;
    reg [7:0] accepted_kind0,accepted_kind1;
    reg [1:0] accepted_revoke_expected;
    reg [1:0] accepted_revoke_due;
    reg [1:0] accepted_slot_good;
    reg [7:0] accepted_kind_d1_0,accepted_kind_d1_1;
    reg mab_cancel_due;
    reg [8:0] mab_cancel_rob;
    reg mab_cancel_good;
    reg cancelled_owner_valid,cancelled_bad;
    reg [8:0] cancelled_owner_rob;
    reg [3:0] cancelled_age;
    reg cancel_terminal;
    wire cancelled_payload_ignored;
    reg [55:0] generation_cancelled;
    reg [5:0] generation_rob_check;
    integer finish_init;
    wire transfer_split0;
    wire transfer_split1;
    wire split_second0;
    wire split_second1;
    wire final_split_good0;
    wire final_split_good1;
    wire accepted_effect_ok0;
    wire cam_ok0;
    wire kill_ex_gate_ok0;
    wire accepted_effect_ok1;
    wire cam_ok1;
    wire kill_ex_gate_ok1;
    reg [4:0] pair_active,pair_bad;
    reg [4:0] pair_done0,pair_done1;
    reg [8:0] pair_rob0[0:4],pair_rob1[0:4];
    reg [5:0] pair_sq0[0:4],pair_sq1[0:4];
    reg [4:0] pair_report,pair_result;
    reg [7:0] pair_epoch[0:4];
    integer pair_i;
    wire [4:0] pair_start;
    reg [8:0] transfer_owner[0:3],transfer_origin_op[0:3];
    reg transfer_epoch[0:3],transfer_cross[0:3],transfer_within[0:3];
    reg transfer_origin_good[0:3],transfer_origin_mab[0:3],transfer_discontinuous[0:3];
    reg [49:0] transfer_origin_va[0:3];
    reg [8:0] split_low_owner[0:55];
    reg [55:0] split_low_epoch;
    reg [8:0] split_complete_op;
    reg [49:0] split_complete_va;
    reg split_complete_origin_good,split_complete_mab,split_complete_discontinuous;
    reg split_complete_order_a,split_complete_order_d;
    wire [4:0] transfer_size0;
    wire expected_unaligned0;
    wire expected_cross0;
    wire [4:0] transfer_size1;
    wire expected_unaligned1;
    wire expected_cross1;
    wire [8:0] dual_rob0;
    wire [8:0] dual_rob1;
    wire dual_expected_port;
    reg [8:0] dual_expected_rob;
    wire dual_candidate_result;

    assign sample_enable = !reset && reset_backend_done && history_valid;

    assign parent_rob = {m_req_uop_robIdx_flag,m_req_uop_robIdx_value};
    assign parent_scalar = m_req_valid && !m_req_isvec;
    assign parent_flush = m_io_redirect_valid && ((parent_rob[8]^m_io_redirect_bits_robIdx_flag^(parent_rob[7:0]>m_io_redirect_bits_robIdx_value)) || (m_io_redirect_bits_level && parent_rob=={m_io_redirect_bits_robIdx_flag,m_io_redirect_bits_robIdx_value}));
    assign m_response = parent_scalar && m_bufferState==3 && m_io_splitStoreResp_valid;
    assign response_identity = parent_rob=={m_io_splitStoreResp_bits_uop_robIdx_flag,m_io_splitStoreResp_bits_uop_robIdx_value};
    assign response_fault = |{m_io_splitStoreResp_bits_uop_exceptionVec_3,m_io_splitStoreResp_bits_uop_exceptionVec_6,m_io_splitStoreResp_bits_uop_exceptionVec_7,m_io_splitStoreResp_bits_uop_exceptionVec_15,m_io_splitStoreResp_bits_uop_exceptionVec_19,m_io_splitStoreResp_bits_uop_exceptionVec_23};
    assign response_retry = m_io_splitStoreResp_bits_need_rep;
    assign response_uncache = !response_retry && (m_io_splitStoreResp_bits_nc || m_io_splitStoreResp_bits_mmio);
    assign response_ok = !response_retry && !response_fault && !response_uncache && m_io_splitStoreResp_bits_uop_trigger!=1;











    assign parent_wb = parent_scalar && m_io_writeBack_valid && m_io_writeBack_ready;
    assign m_wb_identity = {m_io_writeBack_bits_uop_robIdx_flag,m_io_writeBack_bits_uop_robIdx_value}==parent_rob;
    assign sta0_rob = {s0_io_stout_bits_uop_robIdx_flag,s0_io_stout_bits_uop_robIdx_value};
    assign sta1_rob = {s1_io_stout_bits_uop_robIdx_flag,s1_io_stout_bits_uop_robIdx_value};
    assign other_rob = {_inner_otherStoutConnect_io_out_bits_uop_robIdx_flag,_inner_otherStoutConnect_io_out_bits_uop_robIdx_value};
    assign final0_rob = {io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_0_bits_uop_robIdx_value};
    assign expected_mab_select = !s0_io_stout_valid && !_inner_otherStoutConnect_io_out_valid && !s0_io_vecstout_valid && !s1_io_vecstout_valid;
    assign expected_other_select = _inner_otherStoutConnect_io_out_valid && !s0_io_stout_valid;
    assign expected_final_valid = s0_io_stout_valid || expected_other_select || (expected_mab_select && m_io_writeBack_valid);
    assign final_mux_ok = io_mem_to_ooo_writebackSta_0_valid==expected_final_valid && inner__6==expected_mab_select && inner_==expected_other_select && inner__7==(expected_mab_select && m_io_writeBack_valid) && (!expected_final_valid || final0_rob==(s0_io_stout_valid ? sta0_rob : expected_other_select ? other_rob : parent_rob));






    assign q_apply0 = q_valid_d1_0 && !q_miss_d1_0 && q_io_storeAddrInRe_0_updateAddrValid;
    assign q_apply1 = q_valid_d1_1 && !q_miss_d1_1 && q_io_storeAddrInRe_1_updateAddrValid;
    assign q_enable_ok0 = q_storeAddrInFireReg==q_apply0 && q_stWbIndexReg==q_index_d1_0;
    assign q_enable_ok1 = q_storeAddrInFireReg_1==q_apply1 && q_stWbIndexReg_1==q_index_d1_1;
    assign q_state_ok0 = !q_update_d1_0 || (!q_allocated[q_update_index0] || (q_pending[q_update_index0]==q_expected_pending0 && q_hasException[q_update_index0]==q_expected_exception0));
    assign q_state_ok1 = !q_update_d1_1 || (!q_allocated[q_update_index1] || (q_pending[q_update_index1]==q_expected_pending1 && q_hasException[q_update_index1]==q_expected_exception1));

    assign dindex0 = q_io_storeDataIn_0_bits_uop_sqIdx_value;
    assign dindex1 = q_io_storeDataIn_1_bits_uop_sqIdx_value;
    always @(posedge clock) begin
        if (reset) begin
            history_valid<=0;child0_done<=0;child1_done<=0;child_retry_count<=0;
            m_response_d1<=0;m_parent_wb_d1<=0;m_parent_flush_d1<=0;m_selected_accept_d1<=0;
            q_valid_d1_0<=0;q_valid_d1_1<=0;q_update_d1_0<=0;q_update_d1_1<=0;data_first<=0;addr_first<=0;
        end else begin
            history_valid<=1;
            m_parent_flush_d1<=parent_scalar && parent_flush;
            m_response_d1<=m_response;m_retry_d1<=response_retry;m_ptr_d1<=m_curPtr;m_state_d1<=m_bufferState;m_reqvalid_d1<=m_req_valid;m_parent_wb_d1<=parent_wb;m_cross_d1<=m_isCrossPage;m_deq_d1<=m_io_sqControl_toStoreMisalignBuffer_doDeq;
            m_fault_d1<=response_fault;m_uncache_d1<=response_uncache;parent_rob_d1<=parent_rob;
            if (m_canEnq || parent_flush || m_s2_needRevoke || !m_req_valid) begin child0_done<=0;child1_done<=0;child_retry_count<=0;end
            if (m_response && response_identity && response_ok) begin
                if (!m_curPtr) child0_done<=1;
                else begin child1_done<=1;high_pa<=m_io_splitStoreResp_bits_paddr;end
                child_retry_count<=0;
            end
            if (m_response && response_identity && response_retry) child_retry_count<=child_retry_count==255 ? 255 : child_retry_count+1;
            if (m_response && response_fault && !response_retry) fault_va<=m_io_splitStoreResp_bits_vaddr;
            if (m_canEnq) m_saved_trigger<=m_reqSel_3_0 ? m_io_enq_1_req_bits_uop_trigger : m_io_enq_0_req_bits_uop_trigger;
            m_selected_accept_d1<=m_canEnq;
            m_selected_port_d1<=m_reqSel_3_0;
            q_valid_d1_0<=q_io_storeAddrIn_0_valid;q_miss_d1_0<=q_io_storeAddrIn_0_bits_miss;
            q_valid_d1_1<=q_io_storeAddrIn_1_valid;q_miss_d1_1<=q_io_storeAddrIn_1_bits_miss;
            if (q_io_storeAddrIn_0_valid) q_index_d1_0<=q_io_storeAddrIn_0_bits_uop_sqIdx_value;
            if (q_io_storeAddrIn_1_valid) q_index_d1_1<=q_io_storeAddrIn_1_bits_uop_sqIdx_value;
            q_update_d1_0<=q_apply0;q_update_d1_1<=q_apply1;
            q_update_index0<=q_index_d1_0;q_update_index1<=q_index_d1_1;
            q_expected_pending0<=q_io_storeAddrInRe_0_mmio;q_expected_pending1<=q_io_storeAddrInRe_1_mmio;
            q_expected_exception0<=q_io_storeAddrInRe_0_hasException;q_expected_exception1<=q_io_storeAddrInRe_1_hasException;
            if (m_io_redirect_valid) begin data_first<=0;addr_first<=0;end
            else begin
                if (q_io_storeDataIn_0_valid && dindex0<56 && !q_addrvalid[dindex0]) data_first[dindex0]<=1;
                if (q_io_storeDataIn_1_valid && dindex1<56 && !q_addrvalid[dindex1]) data_first[dindex1]<=1;
                if (q_io_storeAddrIn_0_valid && !q_io_storeAddrIn_0_bits_miss && !q_datavalid[q_io_storeAddrIn_0_bits_uop_sqIdx_value]) addr_first[q_io_storeAddrIn_0_bits_uop_sqIdx_value]<=1;
                if (q_io_storeAddrIn_1_valid && !q_io_storeAddrIn_1_bits_miss && !q_datavalid[q_io_storeAddrIn_1_bits_uop_sqIdx_value]) addr_first[q_io_storeAddrIn_1_bits_uop_sqIdx_value]<=1;
            end
        end
    end

    // MAB单槽生命周期：在真实accept保存ROB/原VA，child响应、等待、WB与释放累计到同一parent。









    assign life_payload = {m_io_writeBack_bits_uop_robIdx_flag,m_io_writeBack_bits_uop_robIdx_value,m_io_writeBack_bits_uop_trigger,m_io_writeBack_bits_uop_exceptionVec_3,m_io_writeBack_bits_uop_exceptionVec_6,m_io_writeBack_bits_uop_exceptionVec_7,m_io_writeBack_bits_uop_exceptionVec_15,m_io_writeBack_bits_uop_exceptionVec_19,m_io_writeBack_bits_uop_exceptionVec_23};












    assign life_response = m_response && life_active;
    assign life_identity_ok = parent_rob==life_rob && response_identity;
    assign life_finish_context = life_active && m_io_writeBack_valid && !m_req_isvec;
    assign life_finish_good = !life_bad && life_rob==parent_rob && m_wb_identity && final_mux_ok && life_writebacks==0;
    assign life_wait_stable = !life_active || life_rob==parent_rob;
    assign life_slot_clear = !m_req_valid;
    always @(posedge clock) begin
        if (reset) begin
            life_active<=0;life_bad<=0;life_children<=0;life_writebacks<=0;life_wait_sq<=0;life_wait_vec<=0;life_wait_rob<=0;life_child_retry<=0;
            life_wb_due<=0;life_release_due<=0;life_stalled_d1<=0;causal_active<=0;causal_addr_seen<=0;causal_data_seen<=0;causal_bad<=0;causal_exception<=0;causal_order_addr<=0;causal_order_data<=0;
            for(init_i=0;init_i<56;init_i=init_i+1) begin causal_rob[init_i]<=0;causal_op[init_i]<=0;causal_mask[init_i]<=0;causal_data[init_i]<=0;causal_va[init_i]<=0;causal_pa[init_i]<=0;end
        end else begin
            life_wb_due<=parent_wb;life_wb_good<=life_finish_good && (!life_stalled_d1 || life_payload==life_payload_d1);
            life_stalled_d1<=life_finish_context && !m_io_writeBack_ready;life_payload_d1<=life_payload;
            life_release_due<=m_parent_wb_d1 || (parent_scalar && m_bufferState==5 && m_io_sqControl_toStoreMisalignBuffer_doDeq);
            life_release_cross<=m_isCrossPage && !m_globalException && !m_globalUncache;
            life_release_good<=!life_bad && m_io_sqControl_toStoreQueue_paddr=={life_high_pa[47:3],3'b0};
            if (m_canEnq) begin
                life_active<=!(m_reqSel_3_0 ? m_io_enq_1_req_bits_isvec : m_io_enq_0_req_bits_isvec);
                life_bad<=0;life_children<=0;life_writebacks<=0;life_wait_sq<=0;life_wait_vec<=0;life_wait_rob<=0;life_child_retry<=0;life_faultkind<=0;
                life_rob<=m_reqSel_3_0 ? {m_io_enq_1_req_bits_uop_robIdx_flag,m_io_enq_1_req_bits_uop_robIdx_value} : {m_io_enq_0_req_bits_uop_robIdx_flag,m_io_enq_0_req_bits_uop_robIdx_value};
                life_op<=m_reqSel_3_0 ? m_io_enq_1_req_bits_uop_fuOpType : m_io_enq_0_req_bits_uop_fuOpType;
                life_va<=m_reqSel_3_0 ? m_io_enq_1_req_bits_vaddr : m_io_enq_0_req_bits_vaddr;
                life_early_trigger<=m_reqSel_3_0 ? m_io_enq_1_req_bits_uop_trigger : m_io_enq_0_req_bits_uop_trigger;
                life_s1_trigger<=m_reqSel_3_0 ? x1__storeTrigger_tdataVec_io_toLoadStore_triggerAction : x0__storeTrigger_tdataVec_io_toLoadStore_triggerAction;
            end else if (life_active) begin
                if (parent_flush || m_s2_needRevoke) life_active<=0;
                else begin
                    life_bad<=life_bad || !life_wait_stable || (life_stalled_d1 && (!m_io_writeBack_valid || life_payload!=life_payload_d1)) || (life_response && !life_identity_ok) || (parent_wb && life_writebacks!=0);
                    if (m_io_writeBack_valid && _inner_otherStoutConnect_io_out_valid) life_wait_sq<=1;
                    if (m_io_writeBack_valid && (s0_io_vecstout_valid || s1_io_vecstout_valid)) life_wait_vec<=1;
                    if (m_bufferState==0 && m_cross4KBPageBoundary && !m_robMatch) life_wait_rob<=1;
                    if (life_response && response_ok && !m_curPtr) begin life_children[0]<=1;life_low_pa<=m_io_splitStoreResp_bits_paddr;life_low_va<=m_io_splitStoreResp_bits_vaddr;end
                    if (life_response && response_ok && m_curPtr) begin life_children[1]<=1;life_high_pa<=m_io_splitStoreResp_bits_paddr;end
                    if (life_response && response_retry) life_child_retry<=1;
                    if (life_response && !response_retry && response_fault) begin
                        life_faultkind<={m_io_splitStoreResp_bits_uop_exceptionVec_23,m_io_splitStoreResp_bits_uop_exceptionVec_15,m_io_splitStoreResp_bits_uop_exceptionVec_7};
                    end
                    if (parent_wb) life_writebacks<=life_writebacks==3 ? 3 : life_writebacks+1;
                end
            end
            // 只对物理56项用位图释放；每拍只更新本拍2个地址端口、2个STD端口和2个结果端口，不遍历事务历史。
            causal_active<=causal_active & q_allocated;
            causal_addr_seen<=causal_addr_seen & q_allocated;
            causal_data_seen<=causal_data_seen & q_allocated;
            if (m_io_redirect_valid) begin causal_active<=0;causal_addr_seen<=0;causal_data_seen<=0;end else begin
        if(c_address0) begin
            causal_active[q_io_storeAddrIn_0_bits_uop_sqIdx_value]<=1;causal_addr_seen[q_io_storeAddrIn_0_bits_uop_sqIdx_value]<=1;
            causal_rob[q_io_storeAddrIn_0_bits_uop_sqIdx_value]<=c_rob0;causal_va[q_io_storeAddrIn_0_bits_uop_sqIdx_value]<=x0_io_lsq_bits_vaddr;causal_pa[q_io_storeAddrIn_0_bits_uop_sqIdx_value]<=s0_io_lsq_bits_paddr;causal_mask[q_io_storeAddrIn_0_bits_uop_sqIdx_value]<=s0_io_lsq_bits_mask;causal_op[q_io_storeAddrIn_0_bits_uop_sqIdx_value]<=x0_io_lsq_bits_uop_fuOpType;
            if(!causal_active[q_io_storeAddrIn_0_bits_uop_sqIdx_value] || causal_rob[q_io_storeAddrIn_0_bits_uop_sqIdx_value]!=c_rob0) begin causal_bad[q_io_storeAddrIn_0_bits_uop_sqIdx_value]<=0;causal_exception[q_io_storeAddrIn_0_bits_uop_sqIdx_value]<=0;causal_order_addr[q_io_storeAddrIn_0_bits_uop_sqIdx_value]<=!q_datavalid[q_io_storeAddrIn_0_bits_uop_sqIdx_value];causal_order_data[q_io_storeAddrIn_0_bits_uop_sqIdx_value]<=q_datavalid[q_io_storeAddrIn_0_bits_uop_sqIdx_value];end
        end
        if(c_data0) begin
            causal_active[dindex0]<=1;causal_data_seen[dindex0]<=1;causal_data[dindex0]<=c_repeated0;
            if(!causal_active[dindex0] || causal_rob[dindex0]!=xq_rob[dindex0]) begin causal_rob[dindex0]<=xq_rob[dindex0];causal_addr_seen[dindex0]<=(c_address0 && q_io_storeAddrIn_0_bits_uop_sqIdx_value==dindex0) || (c_address1 && q_io_storeAddrIn_1_bits_uop_sqIdx_value==dindex0);causal_bad[dindex0]<=0;causal_exception[dindex0]<=0;causal_order_data[dindex0]<=!q_addrvalid[dindex0];causal_order_addr[dindex0]<=0;end
        end
        if(q_apply0 && q_index_d1_0<56) begin
            causal_exception[q_index_d1_0]<=q_io_storeAddrInRe_0_hasException;
        end
        if(c_address1) begin
            causal_active[q_io_storeAddrIn_1_bits_uop_sqIdx_value]<=1;causal_addr_seen[q_io_storeAddrIn_1_bits_uop_sqIdx_value]<=1;
            causal_rob[q_io_storeAddrIn_1_bits_uop_sqIdx_value]<=c_rob1;causal_va[q_io_storeAddrIn_1_bits_uop_sqIdx_value]<=x1_io_lsq_bits_vaddr;causal_pa[q_io_storeAddrIn_1_bits_uop_sqIdx_value]<=s1_io_lsq_bits_paddr;causal_mask[q_io_storeAddrIn_1_bits_uop_sqIdx_value]<=s1_io_lsq_bits_mask;causal_op[q_io_storeAddrIn_1_bits_uop_sqIdx_value]<=x1_io_lsq_bits_uop_fuOpType;
            if(!causal_active[q_io_storeAddrIn_1_bits_uop_sqIdx_value] || causal_rob[q_io_storeAddrIn_1_bits_uop_sqIdx_value]!=c_rob1) begin causal_bad[q_io_storeAddrIn_1_bits_uop_sqIdx_value]<=0;causal_exception[q_io_storeAddrIn_1_bits_uop_sqIdx_value]<=0;causal_order_addr[q_io_storeAddrIn_1_bits_uop_sqIdx_value]<=!q_datavalid[q_io_storeAddrIn_1_bits_uop_sqIdx_value];causal_order_data[q_io_storeAddrIn_1_bits_uop_sqIdx_value]<=q_datavalid[q_io_storeAddrIn_1_bits_uop_sqIdx_value];end
        end
        if(c_data1) begin
            causal_active[dindex1]<=1;causal_data_seen[dindex1]<=1;causal_data[dindex1]<=c_repeated1;
            if(!causal_active[dindex1] || causal_rob[dindex1]!=xq_rob[dindex1]) begin causal_rob[dindex1]<=xq_rob[dindex1];causal_addr_seen[dindex1]<=(c_address0 && q_io_storeAddrIn_0_bits_uop_sqIdx_value==dindex1) || (c_address1 && q_io_storeAddrIn_1_bits_uop_sqIdx_value==dindex1);causal_bad[dindex1]<=0;causal_exception[dindex1]<=0;causal_order_data[dindex1]<=!q_addrvalid[dindex1];causal_order_addr[dindex1]<=0;end
        end
        if(q_apply1 && q_index_d1_1<56) begin
            causal_exception[q_index_d1_1]<=q_io_storeAddrInRe_1_hasException;
        end
            end
        end
    end




    assign q_attr_result0 = q_attr_enable_result0 && (!q_attr_due0 || (q_attr_index0<56 && xq_rob[q_attr_index0]==q_attr_rob0 && q_pending[q_attr_index0]==q_attr_expected_mmio0 && q_mmio[q_attr_index0]==q_attr_expected_mmio0 && xq_memBackTypeMM[q_attr_index0]==q_attr_expected_mem0 && q_hasException[q_attr_index0]==q_attr_expected_ex0));
    always @(posedge clock) begin
        if(reset) begin q_attr_due0<=0;q_attr_enable_result0<=1;end
        else begin
            q_attr_due0<=q_apply0;q_attr_enable_result0<=q_enable_ok0;q_attr_index0<=q_index_d1_0;q_attr_rob0<=xq_rob[q_index_d1_0];
            q_attr_expected_mmio0<=q_io_storeAddrInRe_0_mmio;q_attr_expected_mem0<=q_io_storeAddrInRe_0_memBackTypeMM;q_attr_expected_ex0<=q_io_storeAddrInRe_0_hasException;
        end
    end




    assign q_attr_result1 = q_attr_enable_result1 && (!q_attr_due1 || (q_attr_index1<56 && xq_rob[q_attr_index1]==q_attr_rob1 && q_pending[q_attr_index1]==q_attr_expected_mmio1 && q_mmio[q_attr_index1]==q_attr_expected_mmio1 && xq_memBackTypeMM[q_attr_index1]==q_attr_expected_mem1 && q_hasException[q_attr_index1]==q_attr_expected_ex1));
    always @(posedge clock) begin
        if(reset) begin q_attr_due1<=0;q_attr_enable_result1<=1;end
        else begin
            q_attr_due1<=q_apply1;q_attr_enable_result1<=q_enable_ok1;q_attr_index1<=q_index_d1_1;q_attr_rob1<=xq_rob[q_index_d1_1];
            q_attr_expected_mmio1<=q_io_storeAddrInRe_1_mmio;q_attr_expected_mem1<=q_io_storeAddrInRe_1_memBackTypeMM;q_attr_expected_ex1<=q_io_storeAddrInRe_1_hasException;
        end
    end

    assign c_rob0 = {s0_io_lsq_bits_uop_robIdx_flag,s0_io_lsq_bits_uop_robIdx_value};
    assign c_address0 = s0_io_lsq_valid && !s0_io_lsq_bits_miss && !s0_io_lsq_bits_isFrmMisAlignBuf && q_io_storeAddrIn_0_bits_uop_sqIdx_value<56 && !m_io_redirect_valid;
    assign c_data0 = q_io_storeDataIn_0_valid && dindex0<56 && q_allocated[dindex0] && !m_io_redirect_valid;
    assign c_repeated0 = xq_io_storeDataIn_0_bits_uop_fuOpType[1:0]==0 ? {16{xq_io_storeDataIn_0_bits_data[7:0]}} : xq_io_storeDataIn_0_bits_uop_fuOpType[1:0]==1 ? {8{xq_io_storeDataIn_0_bits_data[15:0]}} : xq_io_storeDataIn_0_bits_uop_fuOpType[1:0]==2 ? {4{xq_io_storeDataIn_0_bits_data[31:0]}} : {2{xq_io_storeDataIn_0_bits_data[63:0]}};


    assign c_rob1 = {s1_io_lsq_bits_uop_robIdx_flag,s1_io_lsq_bits_uop_robIdx_value};
    assign c_address1 = s1_io_lsq_valid && !s1_io_lsq_bits_miss && !s1_io_lsq_bits_isFrmMisAlignBuf && q_io_storeAddrIn_1_bits_uop_sqIdx_value<56 && !m_io_redirect_valid;
    assign c_data1 = q_io_storeDataIn_1_valid && dindex1<56 && q_allocated[dindex1] && !m_io_redirect_valid;
    assign c_repeated1 = xq_io_storeDataIn_1_bits_uop_fuOpType[1:0]==0 ? {16{xq_io_storeDataIn_1_bits_data[7:0]}} : xq_io_storeDataIn_1_bits_uop_fuOpType[1:0]==1 ? {8{xq_io_storeDataIn_1_bits_data[15:0]}} : xq_io_storeDataIn_1_bits_uop_fuOpType[1:0]==2 ? {4{xq_io_storeDataIn_1_bits_data[31:0]}} : {2{xq_io_storeDataIn_1_bits_data[63:0]}};















    assign push0 = db_io_enq_0_valid && db_io_enq_0_ready;
    assign push1 = db_io_enq_1_valid && db_io_enq_1_ready;
    assign pop0 = xq_io_sbuffer_0_valid && xq_io_sbuffer_0_ready;
    assign pop1 = xq_io_sbuffer_1_valid && xq_io_sbuffer_1_ready;
    assign push_count = {1'b0,push0}+{1'b0,push1};
    assign pop_count = {1'b0,pop0}+{1'b0,pop1};
    always @(posedge clock) begin
        if(reset) begin transfer_count<=0;transfer_head<=0;transfer_tail<=0;end
        else begin transfer_count<=transfer_count+push_count-pop_count;transfer_head<=transfer_head+pop_count;transfer_tail<=transfer_tail+push_count;end
    end

    assign tail0 = transfer_tail;
    assign head0 = transfer_head;
    assign enq_index0 = db_io_enq_0_bits_sqPtr_value;
    assign transfer_source0 = enq_index0<56 && causal_active[enq_index0] && causal_rob[enq_index0]==xq_rob[enq_index0] && causal_addr_seen[enq_index0] && causal_data_seen[enq_index0];
    assign transfer_aligned0 = transfer_source0 && !xq_unaligned[enq_index0] && !causal_exception[enq_index0];

    assign transfer_input_good0 = transfer_source0 && (causal_exception[enq_index0] || (xq_unaligned[enq_index0]==expected_unaligned0 && zq_cross16Byte[enq_index0]==expected_cross0)) && xq_committed[enq_index0] && (causal_exception[enq_index0] ? !db_io_enq_0_bits_vecValid :
       (db_io_enq_0_bits_addr==independent_pa0 && db_io_enq_0_bits_vaddr==independent_va0 && db_io_enq_0_bits_mask==independent_mask0 && (db_io_enq_0_bits_wline ? db_io_enq_0_bits_data==0 : db_io_enq_0_bits_data==independent_data0)));
    assign buffer_link_ok0 = sb_io_in_0_valid==xq_io_sbuffer_0_valid && sb_io_in_0_ready==xq_io_sbuffer_0_ready && sb_io_in_0_bits_data==xq_io_sbuffer_0_bits_data && sb_io_in_0_bits_mask==xq_io_sbuffer_0_bits_mask && sb_io_in_0_bits_addr==xq_io_sbuffer_0_bits_addr && sb_io_in_0_bits_vecValid==xq_io_sbuffer_0_bits_vecValid;
    assign transfer_output_good0 = transfer_count>=1 && transfer_good[head0] && transfer_sq[head0]==db_io_deq_0_bits_sqPtr_value && transfer_data[head0]==xq_io_sbuffer_0_bits_data && transfer_mask[head0]==xq_io_sbuffer_0_bits_mask && transfer_pa[head0]==xq_io_sbuffer_0_bits_addr && transfer_va[head0]==xq_io_sbuffer_0_bits_vaddr && transfer_wline[head0]==xq_io_sbuffer_0_bits_wline && buffer_link_ok0 && (transfer_fault[head0] ? !xq_io_sbuffer_0_bits_vecValid : xq_io_sbuffer_0_bits_vecValid);
    always @(posedge clock) if(!reset && push0) begin
        transfer_sq[tail0]<=enq_index0;transfer_known[tail0]<=transfer_source0;
        transfer_good[tail0]<=transfer_input_good0;
        transfer_data[tail0]<=db_io_enq_0_bits_data;transfer_mask[tail0]<=db_io_enq_0_bits_mask;transfer_pa[tail0]<=db_io_enq_0_bits_addr;transfer_va[tail0]<=db_io_enq_0_bits_vaddr;
        transfer_wline[tail0]<=db_io_enq_0_bits_wline;transfer_fault[tail0]<=causal_exception[enq_index0];
        transfer_order_a[tail0]<=causal_order_addr[enq_index0];transfer_order_d[tail0]<=causal_order_data[enq_index0];
    end

    assign tail1 = (transfer_tail+{1'b0,push0});
    assign head1 = (transfer_head+{1'b0,pop0});
    assign enq_index1 = db_io_enq_1_bits_sqPtr_value;
    assign transfer_source1 = enq_index1<56 && causal_active[enq_index1] && causal_rob[enq_index1]==xq_rob[enq_index1] && causal_addr_seen[enq_index1] && causal_data_seen[enq_index1];
    assign transfer_aligned1 = transfer_source1 && !xq_unaligned[enq_index1] && !causal_exception[enq_index1];

    assign transfer_input_good1 = transfer_source1 && (!split_pair1 || causal_va[enq_index1][11:4]!=8'hff || end_high_known[enq_index1]) && (causal_exception[enq_index1] || (xq_unaligned[enq_index1]==expected_unaligned1 && zq_cross16Byte[enq_index1]==expected_cross1)) && xq_committed[enq_index1] && (causal_exception[enq_index1] ? !db_io_enq_1_bits_vecValid :
       (db_io_enq_1_bits_addr==independent_pa1 && db_io_enq_1_bits_vaddr==independent_va1 && db_io_enq_1_bits_mask==independent_mask1 && (db_io_enq_1_bits_wline ? db_io_enq_1_bits_data==0 : db_io_enq_1_bits_data==independent_data1)));
    assign buffer_link_ok1 = sb_io_in_1_valid==xq_io_sbuffer_1_valid && sb_io_in_1_ready==xq_io_sbuffer_1_ready && sb_io_in_1_bits_data==xq_io_sbuffer_1_bits_data && sb_io_in_1_bits_mask==xq_io_sbuffer_1_bits_mask && sb_io_in_1_bits_addr==xq_io_sbuffer_1_bits_addr && sb_io_in_1_bits_vecValid==xq_io_sbuffer_1_bits_vecValid;
    assign transfer_output_good1 = transfer_count>=2 && transfer_good[head1] && transfer_sq[head1]==db_io_deq_1_bits_sqPtr_value && transfer_data[head1]==xq_io_sbuffer_1_bits_data && transfer_mask[head1]==xq_io_sbuffer_1_bits_mask && transfer_pa[head1]==xq_io_sbuffer_1_bits_addr && transfer_va[head1]==xq_io_sbuffer_1_bits_vaddr && transfer_wline[head1]==xq_io_sbuffer_1_bits_wline && buffer_link_ok1 && (transfer_fault[head1] ? !xq_io_sbuffer_1_bits_vecValid : xq_io_sbuffer_1_bits_vecValid);
    always @(posedge clock) if(!reset && push1) begin
        transfer_sq[tail1]<=enq_index1;transfer_known[tail1]<=transfer_source1;
        transfer_good[tail1]<=transfer_input_good1;
        transfer_data[tail1]<=db_io_enq_1_bits_data;transfer_mask[tail1]<=db_io_enq_1_bits_mask;transfer_pa[tail1]<=db_io_enq_1_bits_addr;transfer_va[tail1]<=db_io_enq_1_bits_vaddr;
        transfer_wline[tail1]<=db_io_enq_1_bits_wline;transfer_fault[tail1]<=causal_exception[enq_index1];
        transfer_order_a[tail1]<=causal_order_addr[enq_index1];transfer_order_d[tail1]<=causal_order_data[enq_index1];
    end





    assign l0_match0 = l0_s2_valid && !l0_s2_in_r_tlbMiss && !l0_s2_exception && (!l0_s2_uncache || l0_s2_nc_with_data) && !l0_s2_in_r_isPrefetch && l0_io_stld_nuke_query_0_valid &&
      (l0_s2_in_r_uop_robIdx_flag ^ l0_io_stld_nuke_query_0_bits_robIdx_flag ^ (l0_s2_in_r_uop_robIdx_value>l0_io_stld_nuke_query_0_bits_robIdx_value)) &&
      (l0_io_stld_nuke_query_0_bits_matchType==2 ? l0_s2_in_r_paddr[47:6]==l0_io_stld_nuke_query_0_bits_paddr[47:6] :
       (l0_io_stld_nuke_query_0_bits_matchType==1 || (l0_s2_in_r_isvec || l0_s2_in_r_misalignWith16Byte) && l0_s2_in_r_is128bit) ? l0_s2_in_r_paddr[47:4]==l0_io_stld_nuke_query_0_bits_paddr[47:4] : l0_s2_in_r_paddr[47:3]==l0_io_stld_nuke_query_0_bits_paddr[47:3]) && (|(l0_s2_in_r_mask & l0_io_stld_nuke_query_0_bits_mask));

    assign l0_match1 = l0_s2_valid && !l0_s2_in_r_tlbMiss && !l0_s2_exception && (!l0_s2_uncache || l0_s2_nc_with_data) && !l0_s2_in_r_isPrefetch && l0_io_stld_nuke_query_1_valid &&
      (l0_s2_in_r_uop_robIdx_flag ^ l0_io_stld_nuke_query_1_bits_robIdx_flag ^ (l0_s2_in_r_uop_robIdx_value>l0_io_stld_nuke_query_1_bits_robIdx_value)) &&
      (l0_io_stld_nuke_query_1_bits_matchType==2 ? l0_s2_in_r_paddr[47:6]==l0_io_stld_nuke_query_1_bits_paddr[47:6] :
       (l0_io_stld_nuke_query_1_bits_matchType==1 || (l0_s2_in_r_isvec || l0_s2_in_r_misalignWith16Byte) && l0_s2_in_r_is128bit) ? l0_s2_in_r_paddr[47:4]==l0_io_stld_nuke_query_1_bits_paddr[47:4] : l0_s2_in_r_paddr[47:3]==l0_io_stld_nuke_query_1_bits_paddr[47:3]) && (|(l0_s2_in_r_mask & l0_io_stld_nuke_query_1_bits_mask));

    always @(posedge clock) begin
        if(reset) l0_due<=0;
        else begin l0_due<=(l0_match0 || l0_match1) && l0_s2_fire;l0_rob_d1<={l0_s2_in_r_uop_robIdx_flag,l0_s2_in_r_uop_robIdx_value};l0_relation_d1<=l0_s2_out_rep_info_cause_9;end
    end
    assign l0_result = l0_relation_d1 && {l0_s3_in_uop_robIdx_flag,l0_s3_in_uop_robIdx_value}==l0_rob_d1 && l0_s3_in_rep_info_cause_9 && (l0_s3_fast_rep_REG || (l0_io_lsq_ldin_valid && l0_io_lsq_ldin_bits_rep_info_cause_9));





    assign l1_match0 = l1_s2_valid && !l1_s2_in_r_tlbMiss && !l1_s2_exception && (!l1_s2_uncache || l1_s2_nc_with_data) && !l1_s2_in_r_isPrefetch && l1_io_stld_nuke_query_0_valid &&
      (l1_s2_in_r_uop_robIdx_flag ^ l1_io_stld_nuke_query_0_bits_robIdx_flag ^ (l1_s2_in_r_uop_robIdx_value>l1_io_stld_nuke_query_0_bits_robIdx_value)) &&
      (l1_io_stld_nuke_query_0_bits_matchType==2 ? l1_s2_in_r_paddr[47:6]==l1_io_stld_nuke_query_0_bits_paddr[47:6] :
       (l1_io_stld_nuke_query_0_bits_matchType==1 || (l1_s2_in_r_isvec || l1_s2_in_r_misalignWith16Byte) && l1_s2_in_r_is128bit) ? l1_s2_in_r_paddr[47:4]==l1_io_stld_nuke_query_0_bits_paddr[47:4] : l1_s2_in_r_paddr[47:3]==l1_io_stld_nuke_query_0_bits_paddr[47:3]) && (|(l1_s2_in_r_mask & l1_io_stld_nuke_query_0_bits_mask));

    assign l1_match1 = l1_s2_valid && !l1_s2_in_r_tlbMiss && !l1_s2_exception && (!l1_s2_uncache || l1_s2_nc_with_data) && !l1_s2_in_r_isPrefetch && l1_io_stld_nuke_query_1_valid &&
      (l1_s2_in_r_uop_robIdx_flag ^ l1_io_stld_nuke_query_1_bits_robIdx_flag ^ (l1_s2_in_r_uop_robIdx_value>l1_io_stld_nuke_query_1_bits_robIdx_value)) &&
      (l1_io_stld_nuke_query_1_bits_matchType==2 ? l1_s2_in_r_paddr[47:6]==l1_io_stld_nuke_query_1_bits_paddr[47:6] :
       (l1_io_stld_nuke_query_1_bits_matchType==1 || (l1_s2_in_r_isvec || l1_s2_in_r_misalignWith16Byte) && l1_s2_in_r_is128bit) ? l1_s2_in_r_paddr[47:4]==l1_io_stld_nuke_query_1_bits_paddr[47:4] : l1_s2_in_r_paddr[47:3]==l1_io_stld_nuke_query_1_bits_paddr[47:3]) && (|(l1_s2_in_r_mask & l1_io_stld_nuke_query_1_bits_mask));

    always @(posedge clock) begin
        if(reset) l1_due<=0;
        else begin l1_due<=(l1_match0 || l1_match1) && l1_s2_fire;l1_rob_d1<={l1_s2_in_r_uop_robIdx_flag,l1_s2_in_r_uop_robIdx_value};l1_relation_d1<=l1_s2_out_rep_info_cause_9;end
    end
    assign l1_result = l1_relation_d1 && {l1_s3_in_uop_robIdx_flag,l1_s3_in_uop_robIdx_value}==l1_rob_d1 && l1_s3_in_rep_info_cause_9 && (l1_s3_fast_rep_REG || (l1_io_lsq_ldin_valid && l1_io_lsq_ldin_bits_rep_info_cause_9));





    assign l2_match0 = l2_s2_valid && !l2_s2_in_r_tlbMiss && !l2_s2_exception && (!l2_s2_uncache || l2_s2_nc_with_data) && !l2_s2_in_r_isPrefetch && l2_io_stld_nuke_query_0_valid &&
      (l2_s2_in_r_uop_robIdx_flag ^ l2_io_stld_nuke_query_0_bits_robIdx_flag ^ (l2_s2_in_r_uop_robIdx_value>l2_io_stld_nuke_query_0_bits_robIdx_value)) &&
      (l2_io_stld_nuke_query_0_bits_matchType==2 ? l2_s2_in_r_paddr[47:6]==l2_io_stld_nuke_query_0_bits_paddr[47:6] :
       (l2_io_stld_nuke_query_0_bits_matchType==1 || (l2_s2_in_r_isvec || l2_s2_in_r_misalignWith16Byte) && l2_s2_in_r_is128bit) ? l2_s2_in_r_paddr[47:4]==l2_io_stld_nuke_query_0_bits_paddr[47:4] : l2_s2_in_r_paddr[47:3]==l2_io_stld_nuke_query_0_bits_paddr[47:3]) && (|(l2_s2_in_r_mask & l2_io_stld_nuke_query_0_bits_mask));

    assign l2_match1 = l2_s2_valid && !l2_s2_in_r_tlbMiss && !l2_s2_exception && (!l2_s2_uncache || l2_s2_nc_with_data) && !l2_s2_in_r_isPrefetch && l2_io_stld_nuke_query_1_valid &&
      (l2_s2_in_r_uop_robIdx_flag ^ l2_io_stld_nuke_query_1_bits_robIdx_flag ^ (l2_s2_in_r_uop_robIdx_value>l2_io_stld_nuke_query_1_bits_robIdx_value)) &&
      (l2_io_stld_nuke_query_1_bits_matchType==2 ? l2_s2_in_r_paddr[47:6]==l2_io_stld_nuke_query_1_bits_paddr[47:6] :
       (l2_io_stld_nuke_query_1_bits_matchType==1 || (l2_s2_in_r_isvec || l2_s2_in_r_misalignWith16Byte) && l2_s2_in_r_is128bit) ? l2_s2_in_r_paddr[47:4]==l2_io_stld_nuke_query_1_bits_paddr[47:4] : l2_s2_in_r_paddr[47:3]==l2_io_stld_nuke_query_1_bits_paddr[47:3]) && (|(l2_s2_in_r_mask & l2_io_stld_nuke_query_1_bits_mask));

    always @(posedge clock) begin
        if(reset) l2_due<=0;
        else begin l2_due<=(l2_match0 || l2_match1) && l2_s2_fire;l2_rob_d1<={l2_s2_in_r_uop_robIdx_flag,l2_s2_in_r_uop_robIdx_value};l2_relation_d1<=l2_s2_out_rep_info_cause_9;end
    end
    assign l2_result = l2_relation_d1 && {l2_s3_in_uop_robIdx_flag,l2_s3_in_uop_robIdx_value}==l2_rob_d1 && l2_s3_in_rep_info_cause_9 && (l2_s3_fast_rep_REG || (l2_io_lsq_ldin_valid && l2_io_lsq_ldin_bits_rep_info_cause_9));

    assign split_pair0 = db_io_enq_0_bits_sqPtr_value<56 && (expected_cross0 && db_io_enq_0_bits_sqPtr_value==enq_index0);
    assign split_data0 = {128'b0,causal_data[enq_index0]} << (causal_va[enq_index0][3:0]*8);
    assign split_mask0 = {16'b0,causal_mask[enq_index0]} << causal_va[enq_index0][3:0];
    assign independent_data0 = split_pair0 ? split_data0[127:0] : expected_unaligned0 ? causal_data[enq_index0] << (causal_va[enq_index0][3:0]*8) : causal_data[enq_index0];
    assign independent_mask0 = split_pair0 ? split_mask0[15:0] : causal_mask[enq_index0];
    assign independent_pa0 = split_pair0 ? {causal_pa[enq_index0][47:3],3'b0} : expected_unaligned0 ? {causal_pa[enq_index0][47:4],4'b0} : causal_pa[enq_index0];
    assign independent_va0 = split_pair0 ? {causal_va[enq_index0][49:3],3'b0}+50'd0 : expected_unaligned0 ? {causal_va[enq_index0][49:4],4'b0} : causal_va[enq_index0];

    assign split_pair1 = db_io_enq_0_bits_sqPtr_value<56 && (expected_cross0 && db_io_enq_0_bits_sqPtr_value==enq_index1);
    assign split_data1 = {128'b0,causal_data[enq_index1]} << (causal_va[enq_index1][3:0]*8);
    assign split_mask1 = {16'b0,causal_mask[enq_index1]} << causal_va[enq_index1][3:0];
    assign independent_data1 = split_pair1 ? split_data1[255:128] : expected_unaligned1 ? causal_data[enq_index1] << (causal_va[enq_index1][3:0]*8) : causal_data[enq_index1];
    assign independent_mask1 = split_pair1 ? split_mask1[31:16] : causal_mask[enq_index1];
    assign independent_pa1 = split_pair1 ? (causal_va[enq_index1][11:4]==8'hff ? {end_high_pa[enq_index1][47:3],3'b0} : {causal_pa[enq_index1][47:3],3'b0}+48'd8) : expected_unaligned1 ? {causal_pa[enq_index1][47:4],4'b0} : causal_pa[enq_index1];
    assign independent_va1 = split_pair1 ? {causal_va[enq_index1][49:3],3'b0}+50'd8 : expected_unaligned1 ? {causal_va[enq_index1][49:4],4'b0} : causal_va[enq_index1];




















    assign special_slot = zq_deqPtrExt_0_value;
    assign mab_slot = m_req_uop_sqIdx_value;
    assign special_due = zq_mmioState==3 && special_slot<56;
    assign special_identity = {zq_io_mmioStout_bits_uop_robIdx_flag,zq_io_mmioStout_bits_uop_robIdx_value}==end_rob[special_slot];
    assign zero_due = zq_io_cboZeroStout_valid && zq_io_cboZeroStout_ready;








    assign mmio_expected_data = end_op[special_slot]==7 ? 64'b0 : causal_data[special_slot][63:0];
    assign mmio_expected_mask = end_op[special_slot]==7 ? 8'hff : causal_mask[special_slot] >> (end_pa[special_slot][3] ? 8 : 0);
    assign special_is_cmo = end_op[special_slot] inside {12,13,14} && !end_pma[special_slot];
    assign special_response_good = special_is_cmo ? (cmo_pending && cmo_resp_seen && cmo_rob==end_rob[special_slot] && cmo_good) : (mmio_request_seen && mmio_response_seen && mmio_owner==end_rob[special_slot] && mmio_transaction_good && (end_op[special_slot]!=7 || mmio_zero_beats==8 || mmio_denied || mmio_corrupt));



    assign special_result = special_due && end_active[special_slot] && !end_bad[special_slot] && special_identity && zq_io_mmioStout_valid && final_mux_ok && special_response_good && zq_io_mmioStout_bits_uop_exceptionVec_7==(special_is_cmo ? cmo_denied : mmio_denied) && zq_io_mmioStout_bits_uop_exceptionVec_19==(special_is_cmo ? cmo_corrupt && !cmo_denied : mmio_corrupt && !mmio_denied);
    assign mab_finish_expected = life_active && m_bufferState==4 && expected_mab_select && !parent_flush;
    assign end_mab_good = mab_slot<56 && end_active[mab_slot] && end_rob[mab_slot]==life_rob && !end_bad[mab_slot] && life_finish_good && end_enqueues[mab_slot]==1;
    assign end_zero_good = zero_pending && zero_slot<56 && zero_rob==end_rob[zero_slot] && !end_bad[zero_slot] && zero_data_ok && zq_io_flushSbuffer_empty && {zq_io_cboZeroStout_bits_uop_robIdx_flag,zq_io_cboZeroStout_bits_uop_robIdx_value}==zero_rob && final_mux_ok;







    assign end_mab_hold_good = !end_hold_bad && m_io_sqControl_toStoreQueue_paddr=={end_hold_high[47:3],3'b0};
    assign end_nc_fault_complete = |end_nc_fault_pulse;



    always @(posedge clock) begin
        if(reset) begin
            end_active<=0;end_generation_flag<=0;end_bad<=0;end_wb_seen<=0;end_mab_seen<=0;end_mem_seen<=0;end_high_known<=0;end_nc_done<=0;
            zero_pending<=0;cmo_pending<=0;cmo_resp_seen<=0;cmo_good<=1;mmio_request_seen<=0;mmio_response_seen<=0;mmio_transaction_good<=1;mmio_zero_beats<=0;
            end_mab_wrote_d1<=0;end_mab_release_d1<=0;end_hold_bad<=0;end_wait_cross<=0;end_child_miss_run<=0;end_nc_fault_pulse<=0;end_completed_d1<=0;end_nc_fault_relation<=1;
            for(end_init=0;end_init<56;end_init=end_init+1) begin
                end_rob[end_init]<=0;end_op[end_init]<=0;end_pbmt[end_init]<=0;end_pma[end_init]<=0;end_ready_mask[end_init]<=0;end_enqueues[end_init]<=0;end_max_child_retries[end_init]<=0;end_bytes_out[end_init]<=0;
            end
        end else begin
            end_bad_work=end_bad;
            end_completed_d1<=zq_completed;
            end_nc_fault_relation<=(((zq_completed & ~end_completed_d1) & end_active & causal_exception & zq_nc & ~xq_committed)==0) && !(zq_io_uncache_req_valid && zq_io_uncache_req_bits_nc && zq_io_uncache_req_bits_id<56 && (zq_completed[zq_io_uncache_req_bits_id] && !end_completed_d1[zq_io_uncache_req_bits_id] && end_active[zq_io_uncache_req_bits_id] && causal_exception[zq_io_uncache_req_bits_id] && zq_nc[zq_io_uncache_req_bits_id]));
            end_nc_fault_pulse<=(zq_completed & ~end_completed_d1) & end_active & causal_exception & zq_nc;
            if(m_canEnq) begin
                end_child_miss_run<=0;end_wait_cross<=0;end_hold_bad<=0;
            end
            if(life_response && response_identity) begin
                if(response_retry) begin
                    end_child_miss_run<=end_child_miss_run==7 ? 7 : end_child_miss_run+1;
                    if(mab_slot<56 && end_child_miss_run+1>end_max_child_retries[mab_slot]) end_max_child_retries[mab_slot]<=end_child_miss_run+1;
                end else begin
                    end_child_miss_run<=0;
                    if(m_curPtr && mab_slot<56) begin end_high_pa[mab_slot]<=m_io_splitStoreResp_bits_paddr;end_high_known[mab_slot]<=1;end_hold_high<=m_io_splitStoreResp_bits_paddr;end end
            end
            if(parent_wb && m_isCrossPage && !m_globalException && !m_globalUncache) end_wait_cross<=1;
            if(end_wait_cross && m_req_valid) end_hold_bad<=end_hold_bad || m_io_sqControl_toStoreQueue_paddr!={end_hold_high[47:3],3'b0};
            end_mab_wrote_d1<=mab_finish_expected;
            end_mab_release_d1<=end_wait_cross && m_io_sqControl_toStoreMisalignBuffer_doDeq;
            end_mab_slot_d1<=mab_slot;end_mab_rob_d1<=life_rob;
            if(m_canEnq) begin
                // 新接受的SQ/ROB已在原始S0索引表建立，按真实最老端口计数；撤销后重新接收需明确重置生命周期。
                if(m_reqSel_3_0 ? m_io_enq_1_req_bits_uop_sqIdx_value<56 : m_io_enq_0_req_bits_uop_sqIdx_value<56) begin
                    end_enqueues[m_reqSel_3_0 ? m_io_enq_1_req_bits_uop_sqIdx_value : m_io_enq_0_req_bits_uop_sqIdx_value] <=
                      end_enqueues[m_reqSel_3_0 ? m_io_enq_1_req_bits_uop_sqIdx_value : m_io_enq_0_req_bits_uop_sqIdx_value]+1;
                end
            end
            if(m_s2_needRevoke && mab_slot<56) end_enqueues[mab_slot]<=0;
            if(parent_wb && mab_slot<56) begin end_mab_seen[mab_slot]<=1;end_bad_work[mab_slot]=end_bad_work[mab_slot] || !end_mab_good;end
            if(zq_io_uncache_req_valid && zq_io_uncache_req_ready && !zq_io_uncache_req_bits_nc) begin
                mmio_request_seen<=1;mmio_response_seen<=0;mmio_owner<={zq_io_uncache_req_bits_robIdx_flag,zq_io_uncache_req_bits_robIdx_value};mmio_id<=zq_io_uncache_req_bits_id;
                mmio_transaction_good<=(!mmio_request_seen || (mmio_transaction_good && mmio_response_seen && !mmio_denied && !mmio_corrupt && mmio_owner==end_rob[special_slot])) && end_active[special_slot] && {zq_io_uncache_req_bits_robIdx_flag,zq_io_uncache_req_bits_robIdx_value}==end_rob[special_slot] && zq_io_uncache_req_bits_data==mmio_expected_data && zq_io_uncache_req_bits_mask==mmio_expected_mask && (end_op[special_slot]==7 ? zq_io_uncache_req_bits_addr=={end_pa[special_slot][47:6],6'b0}+({44'b0,mmio_zero_beats}<<3) : zq_io_uncache_req_bits_addr==end_pa[special_slot]);
            end
            if(zq_io_uncache_resp_valid && !zq_io_uncache_resp_bits_nc && mmio_request_seen && !mmio_response_seen) begin
                mmio_response_seen<=1;mmio_denied<=(mmio_zero_beats!=0 && mmio_denied) || zq_io_uncache_resp_bits_denied;mmio_corrupt<=(mmio_zero_beats!=0 && mmio_corrupt) || zq_io_uncache_resp_bits_corrupt;
                mmio_zero_beats<=mmio_zero_beats+1;
            end
            if(zq_io_cmoOpReq_valid && zq_io_cmoOpReq_ready) begin
                cmo_pending<=1;cmo_resp_seen<=0;cmo_rob<=end_rob[special_slot];cmo_block<={end_pa[special_slot][47:6],6'b0};
                cmo_good<=end_active[special_slot] && end_op[special_slot] inside {12,13,14} && zq_io_cmoOpReq_bits_opcode=={1'b0,end_op[special_slot][1:0]} && zq_io_cmoOpReq_bits_address=={16'b0,end_pa[special_slot][47:6],6'b0};
            end
            if(zq_io_cmoOpResp_valid && zq_io_cmoOpResp_ready) begin cmo_resp_seen<=1;cmo_resp_error<=zq_io_cmoOpResp_bits_denied || zq_io_cmoOpResp_bits_corrupt;cmo_denied<=zq_io_cmoOpResp_bits_denied;cmo_corrupt<=zq_io_cmoOpResp_bits_corrupt;end
            if(special_due && zq_io_mmioStout_valid && zq_io_mmioStout_ready) begin end_wb_seen[special_slot]<=1;end_bad_work[special_slot]=end_bad_work[special_slot] || !special_result;cmo_pending<=0;mmio_request_seen<=0;mmio_response_seen<=0;mmio_zero_beats<=0;end
            if(zero_due) begin end_wb_seen[zero_slot]<=1;end_bad_work[zero_slot]=end_bad_work[zero_slot] || !end_zero_good;zero_pending<=0;end
        if(c0_admit2) normal_sq3_0<=c0_sq2;
        if(c0_expected3) normal_sq4_0<=normal_sq3_0;
        if(end_start0) begin
            if(!end_active[c0_req_sq[5:0]] || end_rob[c0_req_sq[5:0]]!=c0_req_rob || end_generation_flag[c0_req_sq[5:0]]!=c0_req_sq[6]) begin
                end_active[c0_req_sq[5:0]]<=1;end_generation_flag[c0_req_sq[5:0]]<=c0_req_sq[6];end_rob[c0_req_sq[5:0]]<=c0_req_rob;end_op[c0_req_sq[5:0]]<=c0_req_op;end_va[c0_req_sq[5:0]]<=c0_expected_va0;
                end_bad_work[c0_req_sq[5:0]]=!c0_s0_ok;end_wb_seen[c0_req_sq[5:0]]<=0;end_mab_seen[c0_req_sq[5:0]]<=0;end_mem_seen[c0_req_sq[5:0]]<=0;end_ready_mask[c0_req_sq[5:0]]<=0;end_enqueues[c0_req_sq[5:0]]<=0;end_max_child_retries[c0_req_sq[5:0]]<=0;end_high_known[c0_req_sq[5:0]]<=0;end_bytes_out[c0_req_sq[5:0]]<=0;
            end else end_bad_work[c0_req_sq[5:0]]=end_bad_work[c0_req_sq[5:0]] || !c0_s0_ok;
        end
        if(end_prepare0) begin
            end_pbmt[c0_sq2[5:0]]<=c0_pbmt2;end_pma[c0_sq2[5:0]]<=z0_io_pmp_mmio;end_action[c0_sq2[5:0]]<=c0_action2;end_ex[c0_sq2[5:0]]<=c0_exp_ex2;end_pa[c0_sq2[5:0]]<=causal_pa[c0_sq2[5:0]];
            end_bad_work[c0_sq2[5:0]]=end_bad_work[c0_sq2[5:0]] || !c0_s2_ok || !c0_attributes_ok || !c0_trace2 || !c0_fb_ok;
            if(c0_candidate2) end_ready_mask[c0_sq2[5:0]][{c0_ready2,c0_qualified2}]<=1;
        end
        if(end_normal_due0) begin end_wb_seen[end_normal_slot0]<=1;end_bad_work[end_normal_slot0]=end_bad_work[end_normal_slot0] || !end_normal_good0;end
        if(pop0 && transfer_count>0 && transfer_known[head0] && end_rob[transfer_sq[head0]]==transfer_owner[head0] && end_generation_flag[transfer_sq[head0]]==transfer_epoch[head0]) begin
            end_mem_seen[transfer_sq[head0]]<=1;
            end_bad_work[transfer_sq[head0]]=end_bad_work[transfer_sq[head0]] || !transfer_output_good0;
            end_bytes_out[transfer_sq[head0]]<=end_bytes_out[transfer_sq[head0]]+1;
            if(transfer_wline[head0]) begin zero_pending<=1;zero_slot<=transfer_sq[head0];zero_rob<=end_rob[transfer_sq[head0]];zero_data_ok<=transfer_output_good0;end
        end
        if(c1_admit2) normal_sq3_1<=c1_sq2;
        if(c1_expected3) normal_sq4_1<=normal_sq3_1;
        if(end_start1) begin
            if(!end_active[c1_req_sq[5:0]] || end_rob[c1_req_sq[5:0]]!=c1_req_rob || end_generation_flag[c1_req_sq[5:0]]!=c1_req_sq[6]) begin
                end_active[c1_req_sq[5:0]]<=1;end_generation_flag[c1_req_sq[5:0]]<=c1_req_sq[6];end_rob[c1_req_sq[5:0]]<=c1_req_rob;end_op[c1_req_sq[5:0]]<=c1_req_op;end_va[c1_req_sq[5:0]]<=c1_expected_va0;
                end_bad_work[c1_req_sq[5:0]]=!c1_s0_ok;end_wb_seen[c1_req_sq[5:0]]<=0;end_mab_seen[c1_req_sq[5:0]]<=0;end_mem_seen[c1_req_sq[5:0]]<=0;end_ready_mask[c1_req_sq[5:0]]<=0;end_enqueues[c1_req_sq[5:0]]<=0;end_max_child_retries[c1_req_sq[5:0]]<=0;end_high_known[c1_req_sq[5:0]]<=0;end_bytes_out[c1_req_sq[5:0]]<=0;
            end else end_bad_work[c1_req_sq[5:0]]=end_bad_work[c1_req_sq[5:0]] || !c1_s0_ok;
        end
        if(end_prepare1) begin
            end_pbmt[c1_sq2[5:0]]<=c1_pbmt2;end_pma[c1_sq2[5:0]]<=z1_io_pmp_mmio;end_action[c1_sq2[5:0]]<=c1_action2;end_ex[c1_sq2[5:0]]<=c1_exp_ex2;end_pa[c1_sq2[5:0]]<=causal_pa[c1_sq2[5:0]];
            end_bad_work[c1_sq2[5:0]]=end_bad_work[c1_sq2[5:0]] || !c1_s2_ok || !c1_attributes_ok || !c1_trace2 || !c1_fb_ok;
            if(c1_candidate2) end_ready_mask[c1_sq2[5:0]][{c1_ready2,c1_qualified2}]<=1;
        end
        if(end_normal_due1) begin end_wb_seen[end_normal_slot1]<=1;end_bad_work[end_normal_slot1]=end_bad_work[end_normal_slot1] || !end_normal_good1;end
        if(pop1 && transfer_count>1 && transfer_known[head1] && end_rob[transfer_sq[head1]]==transfer_owner[head1] && end_generation_flag[transfer_sq[head1]]==transfer_epoch[head1]) begin
            end_mem_seen[transfer_sq[head1]]<=1;
            end_bad_work[transfer_sq[head1]]=end_bad_work[transfer_sq[head1]] || !transfer_output_good1;
            end_bytes_out[transfer_sq[head1]]<=end_bytes_out[transfer_sq[head1]]+1;
            if(transfer_wline[head1]) begin zero_pending<=1;zero_slot<=transfer_sq[head1];zero_rob<=end_rob[transfer_sq[head1]];zero_data_ok<=transfer_output_good1;end
        end
            end_bad<=end_bad_work;
        end
    end

    assign end_start0 = c0_exp_fire && c0_selected_scalar && c0_req_sq[5:0]<56;
    assign end_prepare0 = c0_expected2 && !c0_child2 && c0_sq2[5:0]<56;
    assign end_normal_due0 = c0_expected4 && !c0_child4 && normal_sq4_0[5:0]<56;
    assign end_normal_slot0 = normal_sq4_0[5:0];
    assign end_normal_good0 = end_active[end_normal_slot0] && end_rob[end_normal_slot0]==c0_rob4 && !end_bad[end_normal_slot0] && c0_wb_ok && c0_trace4;



    assign end_start1 = c1_exp_fire && c1_selected_scalar && c1_req_sq[5:0]<56;
    assign end_prepare1 = c1_expected2 && !c1_child2 && c1_sq2[5:0]<56;
    assign end_normal_due1 = c1_expected4 && !c1_child4 && normal_sq4_1[5:0]<56;
    assign end_normal_slot1 = normal_sq4_1[5:0];
    assign end_normal_good1 = end_active[end_normal_slot1] && end_rob[end_normal_slot1]==c1_rob4 && !end_bad[end_normal_slot1] && c1_wb_ok && c1_trace4;



    // 子请求从独立core预计的S2结果到MAB下一状态；错误valid或身份不抑制场景。





    assign rc_next_ok = rc_good && parent_rob==rc_rob && (rc_miss ? (m_bufferState==2 && m_curPtr==rc_final) : (rc_fault || rc_nc || rc_mmio) ? m_bufferState==4 : !rc_final ? (m_bufferState==2 && m_curPtr) : m_bufferState==3);
    assign rc_fault_addr_ok = m_req_vaddr==rc_va && zm_req_gpaddr==rc_gpa && zm_req_vaNeedExt==rc_ext && zm_req_isForVSnonLeafPTE==rc_nonleaf;






    always @(posedge clock) begin
        if(reset) begin rc_due<=0;revoke_due0<=0;revoke_due1<=0;dual_candidate_due<=0;replay_feedback_bad<=0;end
        else begin
            rc_due<=c0_expected2 && c0_child2 && !parent_flush && life_active;
            rc_good<=c0_child_ok && c0_s2_ok && m_io_splitStoreResp_valid && response_identity;
            rc_rob<=parent_rob;rc_final<=m_curPtr;rc_miss<=c0_miss2;rc_pf_saved<=c0_in_ex15_2;rc_gpf_saved<=c0_in_ex23_2;rc_af_saved<=c0_exp_af2;
            rc_fault<=c0_exp_exception2;rc_nc<=c0_pbmt2==1;rc_mmio<=c0_exp_mmio2;
            rc_va<=m_io_splitStoreResp_bits_vaddr;rc_fullva<={14'b0,m_io_splitStoreResp_bits_vaddr};rc_gpa<=m_io_splitStoreResp_bits_gpaddr;
            rc_ext<=m_io_splitStoreResp_bits_vaNeedExt;rc_nonleaf<=m_io_splitStoreResp_bits_isForVSnonLeafPTE;rc_hyper<=0;
            dual_candidate_due<=m_io_enq_0_req_valid && m_io_enq_1_req_valid && !m_req_valid && !m_io_redirect_valid;
            dual_candidate_good<=m_canEnq && m_reqSel_3_0==dual_expected_port && m_io_enq_0_req_ready==!dual_expected_port && m_io_enq_1_req_ready==dual_expected_port;
            if(end_start0 && (!end_active[c0_req_sq[5:0]] || end_rob[c0_req_sq[5:0]]!=c0_req_rob || end_generation_flag[c0_req_sq[5:0]]!=c0_req_sq[6])) replay_feedback_bad[c0_req_sq[5:0]]<=0;
            if(end_start1 && (!end_active[c1_req_sq[5:0]] || end_rob[c1_req_sq[5:0]]!=c1_req_rob || end_generation_flag[c1_req_sq[5:0]]!=c1_req_sq[6])) replay_feedback_bad[c1_req_sq[5:0]]<=0;
            if(c0_fb_due && c0_fb_sq[5:0]<56 && !c0_fb_ok) replay_feedback_bad[c0_fb_sq[5:0]]<=1;
            if(c1_fb_due && c1_fb_sq[5:0]<56 && !c1_fb_ok) replay_feedback_bad[c1_fb_sq[5:0]]<=1;
        end
    end






    always @(posedge clock) begin
        if(reset) begin revoke_due0<=0;revoke_check_due0<=0;end
        else begin
            revoke_due0<=c0_candidate2 && c0_candidate_qualified2;
            revoke_accepted0<=m_selected_accept_d1 && m_selected_port_d1==0;
            revoke_rob0<=c0_rob2;
            revoke_expect0<=c0_candidate_miss2 || c0_exp_exception2;
            revoke_kind0<= {c0_action2==1,c0_action2==0,c0_pbmt2==2,c0_pbmt2==1,z0_io_pmp_st,c0_in_ex15_2,c0_candidate_miss2,(c0_tlbhit2 && c0_pbmt2==0 && !c0_incoming_fault2 && c0_action2==15 && !z0_io_pmp_st)};
            revoke_check_due0<=revoke_due0;revoke_kind_d1_0<=revoke_kind0;
            revoke_check_good0<=(!revoke_accepted0 || (revoke_expect0 ? !m_req_valid || parent_rob!=revoke_rob0 : m_req_valid && parent_rob==revoke_rob0));
        end
    end






    always @(posedge clock) begin
        if(reset) begin revoke_due1<=0;revoke_check_due1<=0;end
        else begin
            revoke_due1<=c1_candidate2 && c1_candidate_qualified2;
            revoke_accepted1<=m_selected_accept_d1 && m_selected_port_d1==1;
            revoke_rob1<=c1_rob2;
            revoke_expect1<=c1_candidate_miss2 || c1_exp_exception2;
            revoke_kind1<= {c1_action2==1,c1_action2==0,c1_pbmt2==2,c1_pbmt2==1,z1_io_pmp_st,c1_in_ex15_2,c1_candidate_miss2,(c1_tlbhit2 && c1_pbmt2==0 && !c1_incoming_fault2 && c1_action2==15 && !z1_io_pmp_st)};
            revoke_check_due1<=revoke_due1;revoke_kind_d1_1<=revoke_kind1;
            revoke_check_good1<=(!revoke_accepted1 || (revoke_expect1 ? !m_req_valid || parent_rob!=revoke_rob1 : m_req_valid && parent_rob==revoke_rob1));
        end
    end

reg [8:0] raw_oldest0;integer raw_scan0;
reg [8:0] raw_rob0_pipe[0:2];reg raw_feedback0_ok[0:2];
    assign raw_store_rob0 = {raw_store0_bits_uop_robIdx_flag,raw_store0_bits_uop_robIdx_value};

    always @* begin
        raw_match0=0;raw_oldest0=0;
        if(raw_store0_valid && !raw_store0_bits_miss) begin
            for(raw_scan0=0;raw_scan0<32;raw_scan0=raw_scan0+1) begin
                if(raw_allocated[raw_scan0] && raw_datavalid[raw_scan0] &&
                   (raw_rob[raw_scan0][8]^raw_store_rob0[8]^(raw_rob[raw_scan0][7:0]>raw_store_rob0[7:0])) &&
                   !(raw_io_redirect_valid && ((raw_rob[raw_scan0][8]^raw_red[8]^(raw_rob[raw_scan0][7:0]>raw_red[7:0])) || (raw_io_redirect_bits_level && raw_rob[raw_scan0]==raw_red))) &&
                   raw_pa[raw_scan0][23:2]==raw_store0_bits_paddr[27:6] && (raw_store0_bits_wlineflag || raw_pa[raw_scan0][1:0]==raw_store0_bits_paddr[5:4]) && (|(raw_mask[raw_scan0] & raw_store0_bits_mask))) begin
                    if(!raw_match0 || (raw_oldest0[8]^raw_rob[raw_scan0][8]^(raw_oldest0[7:0]>raw_rob[raw_scan0][7:0]))) raw_oldest0=raw_rob[raw_scan0];
                    raw_match0=1;
                end
            end
        end
    end
    always @(posedge clock) begin
        if(reset) raw_due0<=0;
        else begin
            raw_due0[0]<=raw_match0;raw_rob0_pipe[0]<=raw_oldest0;raw_feedback0_ok[0]<=1;
            raw_due0[1]<=raw_due0[0] && !(raw_io_redirect_valid && ((raw_rob0_pipe[0][8]^raw_red[8]^(raw_rob0_pipe[0][7:0]>raw_red[7:0])) || (raw_io_redirect_bits_level && raw_rob0_pipe[0]==raw_red)));
            raw_due0[2]<=raw_due0[1] && !(raw_io_redirect_valid && ((raw_rob0_pipe[1][8]^raw_red[8]^(raw_rob0_pipe[1][7:0]>raw_red[7:0])) || (raw_io_redirect_bits_level && raw_rob0_pipe[1]==raw_red)));
            raw_rob0_pipe[1]<=raw_rob0_pipe[0];raw_rob0_pipe[2]<=raw_rob0_pipe[1];
            raw_feedback0_ok[1]<=c0_fb_ok;raw_feedback0_ok[2]<=raw_feedback0_ok[1];
        end
    end
    assign raw_result0 = raw_rollback0_valid && {raw_rollback0_bits_robIdx_flag,raw_rollback0_bits_robIdx_value}==raw_rob0_pipe[2] && raw_feedback0_ok[2];

reg [8:0] raw_oldest1;integer raw_scan1;
reg [8:0] raw_rob1_pipe[0:2];reg raw_feedback1_ok[0:2];
    assign raw_store_rob1 = {raw_store1_bits_uop_robIdx_flag,raw_store1_bits_uop_robIdx_value};
    assign raw_red = {raw_io_redirect_bits_robIdx_flag,raw_io_redirect_bits_robIdx_value};
    always @* begin
        raw_match1=0;raw_oldest1=0;
        if(raw_store1_valid && !raw_store1_bits_miss) begin
            for(raw_scan1=0;raw_scan1<32;raw_scan1=raw_scan1+1) begin
                if(raw_allocated[raw_scan1] && raw_datavalid[raw_scan1] &&
                   (raw_rob[raw_scan1][8]^raw_store_rob1[8]^(raw_rob[raw_scan1][7:0]>raw_store_rob1[7:0])) &&
                   !(raw_io_redirect_valid && ((raw_rob[raw_scan1][8]^raw_red[8]^(raw_rob[raw_scan1][7:0]>raw_red[7:0])) || (raw_io_redirect_bits_level && raw_rob[raw_scan1]==raw_red))) &&
                   raw_pa[raw_scan1][23:2]==raw_store1_bits_paddr[27:6] && (raw_store1_bits_wlineflag || raw_pa[raw_scan1][1:0]==raw_store1_bits_paddr[5:4]) && (|(raw_mask[raw_scan1] & raw_store1_bits_mask))) begin
                    if(!raw_match1 || (raw_oldest1[8]^raw_rob[raw_scan1][8]^(raw_oldest1[7:0]>raw_rob[raw_scan1][7:0]))) raw_oldest1=raw_rob[raw_scan1];
                    raw_match1=1;
                end
            end
        end
    end
    always @(posedge clock) begin
        if(reset) raw_due1<=0;
        else begin
            raw_due1[0]<=raw_match1;raw_rob1_pipe[0]<=raw_oldest1;raw_feedback1_ok[0]<=1;
            raw_due1[1]<=raw_due1[0] && !(raw_io_redirect_valid && ((raw_rob1_pipe[0][8]^raw_red[8]^(raw_rob1_pipe[0][7:0]>raw_red[7:0])) || (raw_io_redirect_bits_level && raw_rob1_pipe[0]==raw_red)));
            raw_due1[2]<=raw_due1[1] && !(raw_io_redirect_valid && ((raw_rob1_pipe[1][8]^raw_red[8]^(raw_rob1_pipe[1][7:0]>raw_red[7:0])) || (raw_io_redirect_bits_level && raw_rob1_pipe[1]==raw_red)));
            raw_rob1_pipe[1]<=raw_rob1_pipe[0];raw_rob1_pipe[2]<=raw_rob1_pipe[1];
            raw_feedback1_ok[1]<=c1_fb_ok;raw_feedback1_ok[2]<=raw_feedback1_ok[1];
        end
    end
    assign raw_result1 = raw_rollback1_valid && {raw_rollback1_bits_robIdx_flag,raw_rollback1_bits_robIdx_value}==raw_rob1_pipe[2] && raw_feedback1_ok[2];

























    assign cancelled_payload_ignored = !m_req_valid || parent_rob!=cancelled_owner_rob || m_canEnq;




    assign transfer_split0 = transfer_known[head0] && transfer_sq[head0]<56 && transfer_cross[head0];
    assign transfer_split1 = transfer_known[head1] && transfer_sq[head1]<56 && transfer_cross[head1];
    assign split_second0 = transfer_split0 && (split_low_seen[transfer_sq[head0]] && split_low_owner[transfer_sq[head0]]==transfer_owner[head0] && split_low_epoch[transfer_sq[head0]]==transfer_epoch[head0]);
    assign split_second1 = transfer_split1 && ((split_low_seen[transfer_sq[head1]] && split_low_owner[transfer_sq[head1]]==transfer_owner[head1] && split_low_epoch[transfer_sq[head1]]==transfer_epoch[head1]) || (pop0 && transfer_split0 && transfer_sq[head0]==transfer_sq[head1] && transfer_owner[head0]==transfer_owner[head1] && transfer_epoch[head0]==transfer_epoch[head1]));
    assign final_split_good0 = transfer_origin_good[head0] && split_low_good[transfer_sq[head0]] && transfer_output_good0;
    assign final_split_good1 = transfer_origin_good[head1] && (pop0 && transfer_split0 && transfer_sq[head0]==transfer_sq[head1] && transfer_owner[head0]==transfer_owner[head1] && transfer_epoch[head0]==transfer_epoch[head1] ? (transfer_output_good0 && transfer_origin_good[head0]) : split_low_good[transfer_sq[head1]]) && transfer_output_good1;
    always @(posedge clock) begin
        if(reset) begin
            split_complete_due<=0;split_low_seen<=0;split_low_good<=0;end_write_bad<=0;
            accepted_at_s1<=0;accepted_revoke_due<=0;selected_accept_mask<=0;mab_cancel_due<=0;generation_cancelled<=0;cancelled_owner_valid<=0;cancelled_bad<=0;cancelled_age<=0;cancel_terminal<=0;
            for(finish_init=0;finish_init<56;finish_init=finish_init+1) end_rejects[finish_init]<=0;
        end else begin
            split_complete_due<= (pop0 && split_second0) || (pop1 && split_second1);
            if(pop0 && transfer_split0) begin
                if(!split_second0) begin split_low_seen[transfer_sq[head0]]<=1;split_low_good[transfer_sq[head0]]<=transfer_output_good0 && transfer_origin_good[head0];split_low_owner[transfer_sq[head0]]<=transfer_owner[head0];split_low_epoch[transfer_sq[head0]]<=transfer_epoch[head0];end
                else begin split_low_seen[transfer_sq[head0]]<=0;split_complete_slot<=transfer_sq[head0];split_complete_good<=final_split_good0;
                    split_complete_op<=transfer_origin_op[head0];split_complete_va<=transfer_origin_va[head0];
                    split_complete_origin_good<=transfer_origin_good[head0];split_complete_mab<=transfer_origin_mab[head0];split_complete_discontinuous<=transfer_discontinuous[head0];
                    split_complete_order_a<=transfer_order_a[head0];split_complete_order_d<=transfer_order_d[head0];end
            end
            if(pop1 && transfer_split1) begin
                if(!split_second1) begin split_low_seen[transfer_sq[head1]]<=1;split_low_good[transfer_sq[head1]]<=transfer_output_good1 && transfer_origin_good[head1];split_low_owner[transfer_sq[head1]]<=transfer_owner[head1];split_low_epoch[transfer_sq[head1]]<=transfer_epoch[head1];end
                else begin split_low_seen[transfer_sq[head1]]<=0;split_complete_slot<=transfer_sq[head1];split_complete_good<=final_split_good1;
                    split_complete_op<=transfer_origin_op[head1];split_complete_va<=transfer_origin_va[head1];
                    split_complete_origin_good<=transfer_origin_good[head1];split_complete_mab<=transfer_origin_mab[head1];split_complete_discontinuous<=transfer_discontinuous[head1];
                    split_complete_order_a<=transfer_order_a[head1];split_complete_order_d<=transfer_order_d[head1];end
            end
            if(pop0 && transfer_known[head0] && end_rob[transfer_sq[head0]]==transfer_owner[head0] && end_generation_flag[transfer_sq[head0]]==transfer_epoch[head0] && !transfer_output_good0) end_write_bad[transfer_sq[head0]]<=1;
            if(pop1 && transfer_known[head1] && end_rob[transfer_sq[head1]]==transfer_owner[head1] && end_generation_flag[transfer_sq[head1]]==transfer_epoch[head1] && !transfer_output_good1) end_write_bad[transfer_sq[head1]]<=1;
            if(end_start0 && (!end_active[c0_req_sq[5:0]] || end_rob[c0_req_sq[5:0]]!=c0_req_rob || end_generation_flag[c0_req_sq[5:0]]!=c0_req_sq[6])) begin end_write_bad[c0_req_sq[5:0]]<=0;end_rejects[c0_req_sq[5:0]]<=0;generation_cancelled[c0_req_sq[5:0]]<=0;end
            if(end_start1 && (!end_active[c1_req_sq[5:0]] || end_rob[c1_req_sq[5:0]]!=c1_req_rob || end_generation_flag[c1_req_sq[5:0]]!=c1_req_sq[6])) begin end_write_bad[c1_req_sq[5:0]]<=0;end_rejects[c1_req_sq[5:0]]<=0;generation_cancelled[c1_req_sq[5:0]]<=0;end
            if(c0_expected2 && c0_candidate2 && !c0_ready2 && c0_qualified2 && c0_sq2[5:0]<56) end_rejects[c0_sq2[5:0]]<=end_rejects[c0_sq2[5:0]]==255 ? 255 : end_rejects[c0_sq2[5:0]]+1;
            if(c1_expected2 && c1_candidate2 && !c1_ready2 && c1_qualified2 && c1_sq2[5:0]<56) end_rejects[c1_sq2[5:0]]<=end_rejects[c1_sq2[5:0]]==255 ? 255 : end_rejects[c1_sq2[5:0]]+1;
            accepted_at_s1<={m_canEnq && m_reqSel_3_0,m_canEnq && !m_reqSel_3_0};
            accept_rob1_0<={m_io_enq_0_req_bits_uop_robIdx_flag,m_io_enq_0_req_bits_uop_robIdx_value};accept_rob1_1<={m_io_enq_1_req_bits_uop_robIdx_flag,m_io_enq_1_req_bits_uop_robIdx_value};
            accepted_revoke_due<=accepted_at_s1;accept_rob2_0<=accept_rob1_0;accept_rob2_1<=accept_rob1_1;
            accepted_revoke_relation<={m_io_enq_1_revoke==(c1_candidate_miss2 || c1_exp_exception2),m_io_enq_0_revoke==(c0_candidate_miss2 || c0_exp_exception2)};
            accepted_revoke_expected<={m_io_enq_1_revoke,m_io_enq_0_revoke};
            accepted_kind_d1_0<={c0_action2==1,c0_action2==0,c0_pbmt2==2,c0_pbmt2==1,z0_io_pmp_st,c0_in_ex15_2,c0_candidate_miss2,(!c0_candidate_miss2 && !c0_exp_exception2 && c0_pbmt2==0)};
            accepted_kind_d1_1<={c1_action2==1,c1_action2==0,c1_pbmt2==2,c1_pbmt2==1,z1_io_pmp_st,c1_in_ex15_2,c1_candidate_miss2,(!c1_candidate_miss2 && !c1_exp_exception2 && c1_pbmt2==0)};
            cancel_terminal<=0;
            if(parent_scalar && parent_flush) begin
                cancelled_owner_valid<=1;cancelled_owner_rob<=parent_rob;cancelled_bad<=0;cancelled_age<=0;
            end else if(cancelled_owner_valid) begin
                cancelled_age<=cancelled_age==15 ? 15 : cancelled_age+1;
                cancelled_bad<=cancelled_bad || !cancelled_payload_ignored || (inner__7 && final0_rob==cancelled_owner_rob);
                if(cancelled_age==4 || (!cancelled_payload_ignored && !cancelled_bad)) cancel_terminal<=1;
                // 新的MAB实际接受建立新代次，允许同ROB重新执行；此前旧代副作用仍归诊断。
                if(m_canEnq) begin cancelled_owner_valid<=0;cancel_terminal<=1;end
            end
            mab_cancel_due<=parent_scalar && parent_flush;
            if(parent_scalar && parent_flush) begin mab_cancel_rob<=parent_rob;generation_cancelled[mab_slot]<=1;end
            // 不把取消同拍child响应valid视为错误；消费者必须清理已取消parent，新parent必须来自新canEnq。
            mab_cancel_good<=!m_req_valid || (m_canEnq && parent_rob!=mab_cancel_rob);
        end
    end

    assign accepted_effect_ok0 = accepted_revoke_relation[0] && (accepted_revoke_expected[0] ? (!m_req_valid || parent_rob!=accept_rob2_0) : (m_req_valid && parent_rob==accept_rob2_0));
    assign cam_ok0 = cam_paddrModule_wen0==(s0_io_lsq_valid && !s0_io_lsq_bits_miss && !zz0_io_lsq_bits_isFrmMisAlignBuf) && cam_vaddrModule_wen0==cam_paddrModule_wen0 &&
       (!cam_paddrModule_wen0 || (cam_paddrModule_waddr0==s0_io_lsq_bits_uop_sqIdx_value && cam_paddrModule_wdata0==s0_io_lsq_bits_paddr && cam_paddrModule_wmask0==s0_io_lsq_bits_mask && cam_vaddrModule_wdata0==x0_io_lsq_bits_vaddr));
    assign kill_ex_gate_ok0 = zz0_io_lsq_replenish_af==(z0_s2_out_af && s0_s2_valid && !z0_s2_kill) && zz0_io_lsq_replenish_hasException==(c0_exp_exception2 && s0_s2_valid && !z0_s2_kill);

    assign accepted_effect_ok1 = accepted_revoke_relation[1] && (accepted_revoke_expected[1] ? (!m_req_valid || parent_rob!=accept_rob2_1) : (m_req_valid && parent_rob==accept_rob2_1));
    assign cam_ok1 = cam_paddrModule_wen1==(s1_io_lsq_valid && !s1_io_lsq_bits_miss && !zz1_io_lsq_bits_isFrmMisAlignBuf) && cam_vaddrModule_wen1==cam_paddrModule_wen1 &&
       (!cam_paddrModule_wen1 || (cam_paddrModule_waddr1==s1_io_lsq_bits_uop_sqIdx_value && cam_paddrModule_wdata1==s1_io_lsq_bits_paddr && cam_paddrModule_wmask1==s1_io_lsq_bits_mask && cam_vaddrModule_wdata1==x1_io_lsq_bits_vaddr));
    assign kill_ex_gate_ok1 = zz1_io_lsq_replenish_af==(z1_s2_out_af && s1_s2_valid && !z1_s2_kill) && zz1_io_lsq_replenish_hasException==(c1_exp_exception2 && s1_s2_valid && !z1_s2_kill);








    assign pair_start = {c0_expected2 && c1_expected2 && end_op[c0_sq2[5:0]] inside {7,12,13,14} && !(end_op[c1_sq2[5:0]] inside {7,12,13,14}),
       c0_expected2 && c1_expected2 && c0_exp_exception2 && !c1_exp_exception2,
       c0_expected2 && c0_child2 && c1_expected2 && !c1_child2,
       c0_expected1 && c1_expected1 && s0_io_lsq_bits_miss && !s1_io_lsq_bits_miss,
       c0_exp_fire && c1_exp_fire && c0_selected_scalar && c1_selected_scalar && c0_expected_va0!=c1_expected_va0};
    always @(posedge clock) begin
        if(reset) begin pair_active<=0;pair_bad<=0;pair_done0<=0;pair_done1<=0;pair_report<=0;pair_result<=0;end
        else begin
            pair_report<=0;
            for(pair_i=0;pair_i<5;pair_i=pair_i+1) begin
                if(!pair_active[pair_i] && pair_start[pair_i]) begin
                    pair_active[pair_i]<=1;pair_bad[pair_i]<=0;pair_done0[pair_i]<=0;pair_done1[pair_i]<=0;
                    pair_rob0[pair_i]<=pair_i==0 ? c0_req_rob : pair_i==1 ? c0_rob1 : c0_rob2;
                    pair_rob1[pair_i]<=pair_i==0 ? c1_req_rob : pair_i==1 ? c1_rob1 : c1_rob2;
                    pair_sq0[pair_i]<=pair_i==0 ? c0_req_sq[5:0] : pair_i==1 ? c0_sq1[5:0] : c0_sq2[5:0];
                    pair_sq1[pair_i]<=pair_i==0 ? c1_req_sq[5:0] : pair_i==1 ? c1_sq1[5:0] : c1_sq2[5:0];
                    if(pair_i==2) begin pair_done0[pair_i]<=1;pair_bad[pair_i]<=!c0_child_ok;end
                end else if(pair_active[pair_i]) begin
                    if((end_normal_due0 && c0_rob4==pair_rob0[pair_i]) || (special_due && end_rob[special_slot]==pair_rob0[pair_i]) || (mab_finish_expected && life_rob==pair_rob0[pair_i]) || (zero_due && zero_rob==pair_rob0[pair_i])) begin pair_done0[pair_i]<=1;pair_bad[pair_i]<=pair_bad[pair_i] || end_bad[pair_sq0[pair_i]];end
                    if((end_normal_due1 && c1_rob4==pair_rob1[pair_i]) || (special_due && end_rob[special_slot]==pair_rob1[pair_i]) || (mab_finish_expected && life_rob==pair_rob1[pair_i]) || (zero_due && zero_rob==pair_rob1[pair_i])) begin pair_done1[pair_i]<=1;pair_bad[pair_i]<=pair_bad[pair_i] || end_bad[pair_sq1[pair_i]];end
                    if(pair_done0[pair_i] && pair_done1[pair_i]) begin pair_report[pair_i]<=1;pair_result[pair_i]<=!pair_bad[pair_i] && !end_bad[pair_sq0[pair_i]] && !end_bad[pair_sq1[pair_i]];pair_active[pair_i]<=0;end
                    if(m_io_redirect_valid) pair_active[pair_i]<=0;
                end
            end
        end
    end












    always @(posedge clock) if(!reset && push0) begin
        transfer_owner[tail0]<=causal_rob[enq_index0];transfer_epoch[tail0]<=end_generation_flag[enq_index0];
        transfer_cross[tail0]<=expected_cross0;transfer_within[tail0]<=expected_unaligned0 && !expected_cross0;
        transfer_origin_op[tail0]<=end_op[enq_index0];transfer_origin_va[tail0]<=end_va[enq_index0];
        transfer_origin_good[tail0]<=end_active[enq_index0] && end_rob[enq_index0]==causal_rob[enq_index0] && !end_bad[enq_index0];
        transfer_origin_mab[tail0]<=end_mab_seen[enq_index0];
        transfer_discontinuous[tail0]<=end_high_known[enq_index0] && end_pa[enq_index0][47:12]!=end_high_pa[enq_index0][47:12];
    end

    always @(posedge clock) if(!reset && push1) begin
        transfer_owner[tail1]<=causal_rob[enq_index1];transfer_epoch[tail1]<=end_generation_flag[enq_index1];
        transfer_cross[tail1]<=expected_cross1;transfer_within[tail1]<=expected_unaligned1 && !expected_cross1;
        transfer_origin_op[tail1]<=end_op[enq_index1];transfer_origin_va[tail1]<=end_va[enq_index1];
        transfer_origin_good[tail1]<=end_active[enq_index1] && end_rob[enq_index1]==causal_rob[enq_index1] && !end_bad[enq_index1];
        transfer_origin_mab[tail1]<=end_mab_seen[enq_index1];
        transfer_discontinuous[tail1]<=end_high_known[enq_index1] && end_pa[enq_index1][47:12]!=end_high_pa[enq_index1][47:12];
    end

    assign transfer_size0 = 5'd1 << causal_op[enq_index0][1:0];
    assign expected_unaligned0 = (causal_va[enq_index0][3:0] & (transfer_size0-1))!=0 && !(causal_op[enq_index0] inside {7,12,13,14});
    assign expected_cross0 = expected_unaligned0 && ({1'b0,causal_va[enq_index0][3:0]}+transfer_size0>16);

    assign transfer_size1 = 5'd1 << causal_op[enq_index1][1:0];
    assign expected_unaligned1 = (causal_va[enq_index1][3:0] & (transfer_size1-1))!=0 && !(causal_op[enq_index1] inside {7,12,13,14});
    assign expected_cross1 = expected_unaligned1 && ({1'b0,causal_va[enq_index1][3:0]}+transfer_size1>16);

    assign dual_rob0 = {m_io_enq_0_req_bits_uop_robIdx_flag,m_io_enq_0_req_bits_uop_robIdx_value};
    assign dual_rob1 = {m_io_enq_1_req_bits_uop_robIdx_flag,m_io_enq_1_req_bits_uop_robIdx_value};
    assign dual_expected_port = (dual_rob0[8]^dual_rob1[8]^(dual_rob0[7:0]>dual_rob1[7:0])) || (dual_rob0==dual_rob1 && z_mab_uop0>z_mab_uop1);

    assign dual_candidate_result = dual_candidate_good && m_req_valid && parent_rob==dual_expected_rob && c0_fb_ok && c1_fb_ok;
    always @(posedge clock) if(!reset) dual_expected_rob<=dual_expected_port ? dual_rob1 : dual_rob0;

    reg scene_05_05_002_d1;
    always @(posedge clock) begin if (reset) scene_05_05_002_d1<=0; else scene_05_05_002_d1<=(life_active && life_wait_rob && m_bufferState==0 && m_robMatch); end
    reg scene_07_03_001_lane0_d1;
    always @(posedge clock) begin if (reset) scene_07_03_001_lane0_d1<=0; else scene_07_03_001_lane0_d1<=(q_valid_d1_0 && !q_miss_d1_0 && q_io_storeAddrInRe_0_updateAddrValid); end
    reg scene_07_03_001_lane1_d1;
    always @(posedge clock) begin if (reset) scene_07_03_001_lane1_d1<=0; else scene_07_03_001_lane1_d1<=(q_valid_d1_1 && !q_miss_d1_1 && q_io_storeAddrInRe_1_updateAddrValid); end
    reg scene_07_03_002_lane0_d1;
    always @(posedge clock) begin if (reset) scene_07_03_002_lane0_d1<=0; else scene_07_03_002_lane0_d1<=(q_valid_d1_0 && q_miss_d1_0); end
    reg scene_07_03_002_lane1_d1;
    always @(posedge clock) begin if (reset) scene_07_03_002_lane1_d1<=0; else scene_07_03_002_lane1_d1<=(q_valid_d1_1 && q_miss_d1_1); end
    reg scene_07_03_003_lane0_d1;
    always @(posedge clock) begin if (reset) scene_07_03_003_lane0_d1<=0; else scene_07_03_003_lane0_d1<=(!q_valid_d1_0); end
    reg scene_07_03_003_lane1_d1;
    always @(posedge clock) begin if (reset) scene_07_03_003_lane1_d1<=0; else scene_07_03_003_lane1_d1<=(!q_valid_d1_1); end
    reg scene_07_03_004_lane0_d1;
    always @(posedge clock) begin if (reset) scene_07_03_004_lane0_d1<=0; else scene_07_03_004_lane0_d1<=(q_valid_d1_0 && !q_miss_d1_0 && !q_io_storeAddrInRe_0_updateAddrValid); end
    reg scene_07_03_004_lane1_d1;
    always @(posedge clock) begin if (reset) scene_07_03_004_lane1_d1<=0; else scene_07_03_004_lane1_d1<=(q_valid_d1_1 && !q_miss_d1_1 && !q_io_storeAddrInRe_1_updateAddrValid); end
    reg scene_07_03_005_lane0_d1;
    always @(posedge clock) begin if (reset) scene_07_03_005_lane0_d1<=0; else scene_07_03_005_lane0_d1<=(q_valid_d1_0 && q_io_storeAddrIn_0_valid && q_index_d1_0!=q_io_storeAddrIn_0_bits_uop_sqIdx_value); end
    reg scene_07_03_005_lane1_d1;
    always @(posedge clock) begin if (reset) scene_07_03_005_lane1_d1<=0; else scene_07_03_005_lane1_d1<=(q_valid_d1_1 && q_io_storeAddrIn_1_valid && q_index_d1_1!=q_io_storeAddrIn_1_bits_uop_sqIdx_value); end

    // 02.04 跨窗口与地址边界
    covergroup cg_storeunit_02_04 @(posedge clock);
        option.per_instance = 1;
        // 02.04.001 BOUND_SH_跨16B
        // 场景：SH起始VA=0xf，有效访问末地址越过16B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x0003；两部分地址按真实拆分处理，是否跨页另判。
        // 审计结论：完全建模。
        cp_02_04_001 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==1 && split_complete_va==50'hf && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.002 BOUND_SH_跨64B
        // 场景：SH起始VA=0x3f，有效访问末地址越过64B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x0003；两部分地址按真实拆分处理，是否跨页另判。
        // 审计结论：完全建模。
        cp_02_04_002 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==1 && split_complete_va==50'h3f && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.003 BOUND_SH_跨4096B
        // 场景：SH起始VA=0xfff，有效访问末地址越过4096B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x0003；需要两页翻译，不能复用低页物理地址。
        // 审计结论：完全建模。
        cp_02_04_003 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==1 && split_complete_va==50'hfff && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.004 BOUND_SW_跨16B
        // 场景：SW起始VA=0xf，有效访问末地址越过16B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x000f；两部分地址按真实拆分处理，是否跨页另判。
        // 审计结论：完全建模。
        cp_02_04_004 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==2 && split_complete_va==50'hf && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.005 BOUND_SW_跨64B
        // 场景：SW起始VA=0x3f，有效访问末地址越过64B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x000f；两部分地址按真实拆分处理，是否跨页另判。
        // 审计结论：完全建模。
        cp_02_04_005 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==2 && split_complete_va==50'h3f && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.006 BOUND_SW_跨4096B
        // 场景：SW起始VA=0xfff，有效访问末地址越过4096B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x000f；需要两页翻译，不能复用低页物理地址。
        // 审计结论：完全建模。
        cp_02_04_006 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==2 && split_complete_va==50'hfff && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.007 BOUND_SD_跨16B
        // 场景：SD起始VA=0xf，有效访问末地址越过16B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x00ff；两部分地址按真实拆分处理，是否跨页另判。
        // 审计结论：完全建模。
        cp_02_04_007 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==3 && split_complete_va==50'hf && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.008 BOUND_SD_跨64B
        // 场景：SD起始VA=0x3f，有效访问末地址越过64B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x00ff；两部分地址按真实拆分处理，是否跨页另判。
        // 审计结论：完全建模。
        cp_02_04_008 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==3 && split_complete_va==50'h3f && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.009 BOUND_SD_跨4096B
        // 场景：SD起始VA=0xfff，有效访问末地址越过4096B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x00ff；需要两页翻译，不能复用低页物理地址。
        // 审计结论：完全建模。
        cp_02_04_009 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==3 && split_complete_va==50'hfff && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.010 BOUND_HSV.H_跨16B
        // 场景：HSV.H起始VA=0xf，有效访问末地址越过16B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x0003；两部分地址按真实拆分处理，是否跨页另判。
        // 审计结论：完全建模。
        cp_02_04_010 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==17 && split_complete_va==50'hf && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.011 BOUND_HSV.H_跨64B
        // 场景：HSV.H起始VA=0x3f，有效访问末地址越过64B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x0003；两部分地址按真实拆分处理，是否跨页另判。
        // 审计结论：完全建模。
        cp_02_04_011 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==17 && split_complete_va==50'h3f && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.012 BOUND_HSV.H_跨4096B
        // 场景：HSV.H起始VA=0xfff，有效访问末地址越过4096B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x0003；需要两页翻译，不能复用低页物理地址。
        // 审计结论：完全建模。
        cp_02_04_012 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==17 && split_complete_va==50'hfff && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.013 BOUND_HSV.W_跨16B
        // 场景：HSV.W起始VA=0xf，有效访问末地址越过16B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x000f；两部分地址按真实拆分处理，是否跨页另判。
        // 审计结论：完全建模。
        cp_02_04_013 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==18 && split_complete_va==50'hf && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.014 BOUND_HSV.W_跨64B
        // 场景：HSV.W起始VA=0x3f，有效访问末地址越过64B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x000f；两部分地址按真实拆分处理，是否跨页另判。
        // 审计结论：完全建模。
        cp_02_04_014 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==18 && split_complete_va==50'h3f && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.015 BOUND_HSV.W_跨4096B
        // 场景：HSV.W起始VA=0xfff，有效访问末地址越过4096B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x000f；需要两页翻译，不能复用低页物理地址。
        // 审计结论：完全建模。
        cp_02_04_015 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==18 && split_complete_va==50'hfff && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.016 BOUND_HSV.D_跨16B
        // 场景：HSV.D起始VA=0xf，有效访问末地址越过16B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x00ff；两部分地址按真实拆分处理，是否跨页另判。
        // 审计结论：完全建模。
        cp_02_04_016 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==19 && split_complete_va==50'hf && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.017 BOUND_HSV.D_跨64B
        // 场景：HSV.D起始VA=0x3f，有效访问末地址越过64B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x00ff；两部分地址按真实拆分处理，是否跨页另判。
        // 审计结论：完全建模。
        cp_02_04_017 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==19 && split_complete_va==50'h3f && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 02.04.018 BOUND_HSV.D_跨4096B
        // 场景：HSV.D起始VA=0xfff，有效访问末地址越过4096B边界；cross16不等于crossPage；低位和高页地址分别追踪；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：cross16=1且基础mask=0x00ff；需要两页翻译，不能复用低页物理地址。
        // 审计结论：完全建模。
        cp_02_04_018 : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==19 && split_complete_va==50'hfff && split_complete_mab)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_02_04 cg_storeunit_02_04_inst;
    covergroup cg_storeunit_diag_02_04 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_02_04_001_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==1 && split_complete_va==50'hf && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_002_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==1 && split_complete_va==50'h3f && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_003_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==1 && split_complete_va==50'hfff && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_004_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==2 && split_complete_va==50'hf && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_005_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==2 && split_complete_va==50'h3f && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_006_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==2 && split_complete_va==50'hfff && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_007_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==3 && split_complete_va==50'hf && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_008_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==3 && split_complete_va==50'h3f && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_009_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==3 && split_complete_va==50'hfff && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_010_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==17 && split_complete_va==50'hf && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_011_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==17 && split_complete_va==50'h3f && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_012_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==17 && split_complete_va==50'hfff && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_013_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==18 && split_complete_va==50'hf && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_014_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==18 && split_complete_va==50'h3f && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_015_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==18 && split_complete_va==50'hfff && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_016_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==19 && split_complete_va==50'hf && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_017_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==19 && split_complete_va==50'h3f && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_02_04_018_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_op==19 && split_complete_va==50'hfff && split_complete_mab)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_02_04 cg_storeunit_diag_02_04_inst;

    // 04.01 PBMT选择与PMA覆盖
    covergroup cg_storeunit_04_01 @(posedge clock);
        option.per_instance = 1;
        // 04.01.001 ATTR_PBMT0_PMAmmio0
        // 场景：自然对齐普通Store有效hit，无fault/trigger/PMP拒绝，PBMT=0，P.mmio=0；PBMT依有效hit选择；miss分支单列，不使用其raw PBMT；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：nc=0，s2_mmio=0，memBackTypeMM=1；无其他阻止时按普通STA路径完成。
        // 审计结论：完全建模。
        cp_04_01_001_normal0 : coverpoint ($isunknown((end_normal_good0)) ? 2'd2 : ((end_normal_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 && end_pbmt[end_normal_slot0]==0 && end_pma[end_normal_slot0]==0 && end_ex[end_normal_slot0]==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.01.001 ATTR_PBMT0_PMAmmio0
        // 场景：自然对齐普通Store有效hit，无fault/trigger/PMP拒绝，PBMT=0，P.mmio=0；PBMT依有效hit选择；miss分支单列，不使用其raw PBMT；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：nc=0，s2_mmio=0，memBackTypeMM=1；无其他阻止时按普通STA路径完成。
        // 审计结论：完全建模。
        cp_04_01_001_normal1 : coverpoint ($isunknown((end_normal_good1)) ? 2'd2 : ((end_normal_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due1 && end_pbmt[end_normal_slot1]==0 && end_pma[end_normal_slot1]==0 && end_ex[end_normal_slot1]==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.01.002 ATTR_PBMT0_PMAmmio1
        // 场景：自然对齐普通Store有效hit，无fault/trigger/PMP拒绝，PBMT=0，P.mmio=1；PBMT依有效hit选择；miss分支单列，不使用其raw PBMT；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：nc=0，s2_mmio=1，memBackTypeMM=0；普通STA出口被MMIO分流到SQ完成。
        // 审计结论：完全建模。
        cp_04_01_002_special : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_pbmt[special_slot]==0 && end_pma[special_slot]==1 && end_ex[special_slot]==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.01.003 ATTR_PBMT1_PMAmmio0
        // 场景：自然对齐普通Store有效hit，无fault/trigger/PMP拒绝，PBMT=1，P.mmio=0；PBMT依有效hit选择；miss分支单列，不使用其raw PBMT；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：nc=1，s2_mmio=0，memBackTypeMM=1；无其他阻止时按普通STA路径完成。
        // 审计结论：完全建模。
        cp_04_01_003_normal0 : coverpoint ($isunknown((end_normal_good0)) ? 2'd2 : ((end_normal_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 && end_pbmt[end_normal_slot0]==1 && end_pma[end_normal_slot0]==0 && end_ex[end_normal_slot0]==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.01.003 ATTR_PBMT1_PMAmmio0
        // 场景：自然对齐普通Store有效hit，无fault/trigger/PMP拒绝，PBMT=1，P.mmio=0；PBMT依有效hit选择；miss分支单列，不使用其raw PBMT；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：nc=1，s2_mmio=0，memBackTypeMM=1；无其他阻止时按普通STA路径完成。
        // 审计结论：完全建模。
        cp_04_01_003_normal1 : coverpoint ($isunknown((end_normal_good1)) ? 2'd2 : ((end_normal_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due1 && end_pbmt[end_normal_slot1]==1 && end_pma[end_normal_slot1]==0 && end_ex[end_normal_slot1]==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.01.004 ATTR_PBMT1_PMAmmio1
        // 场景：自然对齐普通Store有效hit，无fault/trigger/PMP拒绝，PBMT=1，P.mmio=1；PBMT依有效hit选择；miss分支单列，不使用其raw PBMT；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：nc=1，s2_mmio=0，memBackTypeMM=0；无其他阻止时按普通STA路径完成。
        // 审计结论：完全建模。
        cp_04_01_004_normal0 : coverpoint ($isunknown((end_normal_good0)) ? 2'd2 : ((end_normal_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 && end_pbmt[end_normal_slot0]==1 && end_pma[end_normal_slot0]==1 && end_ex[end_normal_slot0]==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.01.004 ATTR_PBMT1_PMAmmio1
        // 场景：自然对齐普通Store有效hit，无fault/trigger/PMP拒绝，PBMT=1，P.mmio=1；PBMT依有效hit选择；miss分支单列，不使用其raw PBMT；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：nc=1，s2_mmio=0，memBackTypeMM=0；无其他阻止时按普通STA路径完成。
        // 审计结论：完全建模。
        cp_04_01_004_normal1 : coverpoint ($isunknown((end_normal_good1)) ? 2'd2 : ((end_normal_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due1 && end_pbmt[end_normal_slot1]==1 && end_pma[end_normal_slot1]==1 && end_ex[end_normal_slot1]==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.01.005 ATTR_PBMT2_PMAmmio0
        // 场景：自然对齐普通Store有效hit，无fault/trigger/PMP拒绝，PBMT=2，P.mmio=0；PBMT依有效hit选择；miss分支单列，不使用其raw PBMT；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：nc=0，s2_mmio=1，memBackTypeMM=1；普通STA出口被MMIO分流到SQ完成。
        // 审计结论：完全建模。
        cp_04_01_005_special : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_pbmt[special_slot]==2 && end_pma[special_slot]==0 && end_ex[special_slot]==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.01.006 ATTR_PBMT2_PMAmmio1
        // 场景：自然对齐普通Store有效hit，无fault/trigger/PMP拒绝，PBMT=2，P.mmio=1；PBMT依有效hit选择；miss分支单列，不使用其raw PBMT；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：nc=0，s2_mmio=1，memBackTypeMM=0；普通STA出口被MMIO分流到SQ完成。
        // 审计结论：完全建模。
        cp_04_01_006_special : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_pbmt[special_slot]==2 && end_pma[special_slot]==1 && end_ex[special_slot]==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_04_01 cg_storeunit_04_01_inst;
    covergroup cg_storeunit_diag_04_01 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_04_01_001_normal0_result : coverpoint ($isunknown((end_normal_good0)) ? 2'd2 : ((end_normal_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 && end_pbmt[end_normal_slot0]==0 && end_pma[end_normal_slot0]==0 && end_ex[end_normal_slot0]==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_01_001_normal1_result : coverpoint ($isunknown((end_normal_good1)) ? 2'd2 : ((end_normal_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due1 && end_pbmt[end_normal_slot1]==0 && end_pma[end_normal_slot1]==0 && end_ex[end_normal_slot1]==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_01_002_special_result : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_pbmt[special_slot]==0 && end_pma[special_slot]==1 && end_ex[special_slot]==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_01_003_normal0_result : coverpoint ($isunknown((end_normal_good0)) ? 2'd2 : ((end_normal_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 && end_pbmt[end_normal_slot0]==1 && end_pma[end_normal_slot0]==0 && end_ex[end_normal_slot0]==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_01_003_normal1_result : coverpoint ($isunknown((end_normal_good1)) ? 2'd2 : ((end_normal_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due1 && end_pbmt[end_normal_slot1]==1 && end_pma[end_normal_slot1]==0 && end_ex[end_normal_slot1]==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_01_004_normal0_result : coverpoint ($isunknown((end_normal_good0)) ? 2'd2 : ((end_normal_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 && end_pbmt[end_normal_slot0]==1 && end_pma[end_normal_slot0]==1 && end_ex[end_normal_slot0]==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_01_004_normal1_result : coverpoint ($isunknown((end_normal_good1)) ? 2'd2 : ((end_normal_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due1 && end_pbmt[end_normal_slot1]==1 && end_pma[end_normal_slot1]==1 && end_ex[end_normal_slot1]==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_01_005_special_result : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_pbmt[special_slot]==2 && end_pma[special_slot]==0 && end_ex[special_slot]==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_01_006_special_result : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_pbmt[special_slot]==2 && end_pma[special_slot]==1 && end_ex[special_slot]==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_04_01 cg_storeunit_diag_04_01_inst;

    // 04.05 CBO属性与特殊完成
    covergroup cg_storeunit_04_05 @(posedge clock);
        option.per_instance = 1;
        // 04.05.001 CBO_CBO.zero_普通内存
        // 场景：CBO.zero访问普通cacheable内存且无异常；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：zero无普通StoreUnit stout；cacheable zero由SQ自己的cboZero完成来源处理。
        // 审计结论：完全建模。
        cp_04_05_001_zero : coverpoint ($isunknown((end_zero_good)) ? 2'd2 : ((end_zero_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (zero_due && end_op[zero_slot]==7 && end_pbmt[zero_slot]==0 && end_pma[zero_slot]==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.05.002 CBO_CBO.zero_PMA_MMIO
        // 场景：CBO.zero命中PMA_MMIO且P.st=0，无页表fault；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：实际PMA MMIO上的全部CBO形成AF；异常允许进入普通异常写回，RE.mmio被异常抑制。
        // 审计结论：完全建模。
        cp_04_05_002_exception0 : coverpoint ($isunknown((end_normal_good0)) ? 2'd2 : ((end_normal_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 && end_op[end_normal_slot0]==7 && end_ex[end_normal_slot0]!=0 && end_pbmt[end_normal_slot0]==0 && end_pma[end_normal_slot0]==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.05.002 CBO_CBO.zero_PMA_MMIO
        // 场景：CBO.zero命中PMA_MMIO且P.st=0，无页表fault；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：实际PMA MMIO上的全部CBO形成AF；异常允许进入普通异常写回，RE.mmio被异常抑制。
        // 审计结论：完全建模。
        cp_04_05_002_exception1 : coverpoint ($isunknown((end_normal_good1)) ? 2'd2 : ((end_normal_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due1 && end_op[end_normal_slot1]==7 && end_ex[end_normal_slot1]!=0 && end_pbmt[end_normal_slot1]==0 && end_pma[end_normal_slot1]==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.05.003 CBO_CBO.zero_PBMT_IO
        // 场景：CBO.zero命中PBMT_IO且自然对齐，无PMP拒绝/其他fault；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：PBMT_IO不等同于actually_mmio的PMA专用项；对齐且无其他异常时S2普通出口分流，RE.mmio=1。
        // 审计结论：完全建模。
        cp_04_05_003_special : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_op[special_slot]==7 && end_pbmt[special_slot]==2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.05.004 CBO_CBO.clean_普通内存
        // 场景：CBO.clean访问普通cacheable内存且无异常；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：非zero CBO通过RE.mmio=1复用SQ pending；不产生普通StoreUnit stout。
        // 审计结论：完全建模。
        cp_04_05_004_special : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_op[special_slot]==12 && end_pbmt[special_slot]==0 && end_pma[special_slot]==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.05.005 CBO_CBO.clean_PMA_MMIO
        // 场景：CBO.clean命中PMA_MMIO且P.st=0，无页表fault；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：实际PMA MMIO上的全部CBO形成AF；异常允许进入普通异常写回，RE.mmio被异常抑制。
        // 审计结论：完全建模。
        cp_04_05_005_exception0 : coverpoint ($isunknown((end_normal_good0)) ? 2'd2 : ((end_normal_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 && end_op[end_normal_slot0]==12 && end_ex[end_normal_slot0]!=0 && end_pbmt[end_normal_slot0]==0 && end_pma[end_normal_slot0]==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.05.005 CBO_CBO.clean_PMA_MMIO
        // 场景：CBO.clean命中PMA_MMIO且P.st=0，无页表fault；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：实际PMA MMIO上的全部CBO形成AF；异常允许进入普通异常写回，RE.mmio被异常抑制。
        // 审计结论：完全建模。
        cp_04_05_005_exception1 : coverpoint ($isunknown((end_normal_good1)) ? 2'd2 : ((end_normal_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due1 && end_op[end_normal_slot1]==12 && end_ex[end_normal_slot1]!=0 && end_pbmt[end_normal_slot1]==0 && end_pma[end_normal_slot1]==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.05.006 CBO_CBO.clean_PBMT_IO
        // 场景：CBO.clean命中PBMT_IO且自然对齐，无PMP拒绝/其他fault；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：PBMT_IO不等同于actually_mmio的PMA专用项；对齐且无其他异常时S2普通出口分流，RE.mmio=1。
        // 审计结论：完全建模。
        cp_04_05_006_special : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_op[special_slot]==12 && end_pbmt[special_slot]==2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.05.007 CBO_CBO.flush_普通内存
        // 场景：CBO.flush访问普通cacheable内存且无异常；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：非zero CBO通过RE.mmio=1复用SQ pending；不产生普通StoreUnit stout。
        // 审计结论：完全建模。
        cp_04_05_007_special : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_op[special_slot]==13 && end_pbmt[special_slot]==0 && end_pma[special_slot]==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.05.008 CBO_CBO.flush_PMA_MMIO
        // 场景：CBO.flush命中PMA_MMIO且P.st=0，无页表fault；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：实际PMA MMIO上的全部CBO形成AF；异常允许进入普通异常写回，RE.mmio被异常抑制。
        // 审计结论：完全建模。
        cp_04_05_008_exception0 : coverpoint ($isunknown((end_normal_good0)) ? 2'd2 : ((end_normal_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 && end_op[end_normal_slot0]==13 && end_ex[end_normal_slot0]!=0 && end_pbmt[end_normal_slot0]==0 && end_pma[end_normal_slot0]==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.05.008 CBO_CBO.flush_PMA_MMIO
        // 场景：CBO.flush命中PMA_MMIO且P.st=0，无页表fault；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：实际PMA MMIO上的全部CBO形成AF；异常允许进入普通异常写回，RE.mmio被异常抑制。
        // 审计结论：完全建模。
        cp_04_05_008_exception1 : coverpoint ($isunknown((end_normal_good1)) ? 2'd2 : ((end_normal_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due1 && end_op[end_normal_slot1]==13 && end_ex[end_normal_slot1]!=0 && end_pbmt[end_normal_slot1]==0 && end_pma[end_normal_slot1]==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.05.009 CBO_CBO.flush_PBMT_IO
        // 场景：CBO.flush命中PBMT_IO且自然对齐，无PMP拒绝/其他fault；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：PBMT_IO不等同于actually_mmio的PMA专用项；对齐且无其他异常时S2普通出口分流，RE.mmio=1。
        // 审计结论：完全建模。
        cp_04_05_009_special : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_op[special_slot]==13 && end_pbmt[special_slot]==2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.05.010 CBO_CBO.inval_普通内存
        // 场景：CBO.inval访问普通cacheable内存且无异常；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：非zero CBO通过RE.mmio=1复用SQ pending；不产生普通StoreUnit stout。
        // 审计结论：完全建模。
        cp_04_05_010_special : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_op[special_slot]==14 && end_pbmt[special_slot]==0 && end_pma[special_slot]==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.05.011 CBO_CBO.inval_PMA_MMIO
        // 场景：CBO.inval命中PMA_MMIO且P.st=0，无页表fault；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：实际PMA MMIO上的全部CBO形成AF；异常允许进入普通异常写回，RE.mmio被异常抑制。
        // 审计结论：完全建模。
        cp_04_05_011_exception0 : coverpoint ($isunknown((end_normal_good0)) ? 2'd2 : ((end_normal_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 && end_op[end_normal_slot0]==14 && end_ex[end_normal_slot0]!=0 && end_pbmt[end_normal_slot0]==0 && end_pma[end_normal_slot0]==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.05.011 CBO_CBO.inval_PMA_MMIO
        // 场景：CBO.inval命中PMA_MMIO且P.st=0，无页表fault；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：实际PMA MMIO上的全部CBO形成AF；异常允许进入普通异常写回，RE.mmio被异常抑制。
        // 审计结论：完全建模。
        cp_04_05_011_exception1 : coverpoint ($isunknown((end_normal_good1)) ? 2'd2 : ((end_normal_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due1 && end_op[end_normal_slot1]==14 && end_ex[end_normal_slot1]!=0 && end_pbmt[end_normal_slot1]==0 && end_pma[end_normal_slot1]==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 04.05.012 CBO_CBO.inval_PBMT_IO
        // 场景：CBO.inval命中PBMT_IO且自然对齐，无PMP拒绝/其他fault；CBO有效hit；页表权限、PMP和trigger按条目隔离；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：PBMT_IO不等同于actually_mmio的PMA专用项；对齐且无其他异常时S2普通出口分流，RE.mmio=1。
        // 审计结论：完全建模。
        cp_04_05_012_special : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_op[special_slot]==14 && end_pbmt[special_slot]==2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_04_05 cg_storeunit_04_05_inst;
    covergroup cg_storeunit_diag_04_05 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_04_05_001_zero_result : coverpoint ($isunknown((end_zero_good)) ? 2'd2 : ((end_zero_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (zero_due && end_op[zero_slot]==7 && end_pbmt[zero_slot]==0 && end_pma[zero_slot]==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_05_002_exception0_result : coverpoint ($isunknown((end_normal_good0)) ? 2'd2 : ((end_normal_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 && end_op[end_normal_slot0]==7 && end_ex[end_normal_slot0]!=0 && end_pbmt[end_normal_slot0]==0 && end_pma[end_normal_slot0]==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_05_002_exception1_result : coverpoint ($isunknown((end_normal_good1)) ? 2'd2 : ((end_normal_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due1 && end_op[end_normal_slot1]==7 && end_ex[end_normal_slot1]!=0 && end_pbmt[end_normal_slot1]==0 && end_pma[end_normal_slot1]==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_05_003_special_result : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_op[special_slot]==7 && end_pbmt[special_slot]==2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_05_004_special_result : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_op[special_slot]==12 && end_pbmt[special_slot]==0 && end_pma[special_slot]==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_05_005_exception0_result : coverpoint ($isunknown((end_normal_good0)) ? 2'd2 : ((end_normal_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 && end_op[end_normal_slot0]==12 && end_ex[end_normal_slot0]!=0 && end_pbmt[end_normal_slot0]==0 && end_pma[end_normal_slot0]==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_05_005_exception1_result : coverpoint ($isunknown((end_normal_good1)) ? 2'd2 : ((end_normal_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due1 && end_op[end_normal_slot1]==12 && end_ex[end_normal_slot1]!=0 && end_pbmt[end_normal_slot1]==0 && end_pma[end_normal_slot1]==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_05_006_special_result : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_op[special_slot]==12 && end_pbmt[special_slot]==2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_05_007_special_result : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_op[special_slot]==13 && end_pbmt[special_slot]==0 && end_pma[special_slot]==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_05_008_exception0_result : coverpoint ($isunknown((end_normal_good0)) ? 2'd2 : ((end_normal_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 && end_op[end_normal_slot0]==13 && end_ex[end_normal_slot0]!=0 && end_pbmt[end_normal_slot0]==0 && end_pma[end_normal_slot0]==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_05_008_exception1_result : coverpoint ($isunknown((end_normal_good1)) ? 2'd2 : ((end_normal_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due1 && end_op[end_normal_slot1]==13 && end_ex[end_normal_slot1]!=0 && end_pbmt[end_normal_slot1]==0 && end_pma[end_normal_slot1]==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_05_009_special_result : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_op[special_slot]==13 && end_pbmt[special_slot]==2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_05_010_special_result : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_op[special_slot]==14 && end_pbmt[special_slot]==0 && end_pma[special_slot]==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_05_011_exception0_result : coverpoint ($isunknown((end_normal_good0)) ? 2'd2 : ((end_normal_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 && end_op[end_normal_slot0]==14 && end_ex[end_normal_slot0]!=0 && end_pbmt[end_normal_slot0]==0 && end_pma[end_normal_slot0]==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_05_011_exception1_result : coverpoint ($isunknown((end_normal_good1)) ? 2'd2 : ((end_normal_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due1 && end_op[end_normal_slot1]==14 && end_ex[end_normal_slot1]!=0 && end_pbmt[end_normal_slot1]==0 && end_pma[end_normal_slot1]==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_04_05_012_special_result : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_op[special_slot]==14 && end_pbmt[special_slot]==2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_04_05 cg_storeunit_diag_04_05_inst;

    // 05.02 接收、仲裁与原始请求重放
    covergroup cg_storeunit_05_02 @(posedge clock);
        option.per_instance = 1;
        // 05.02.001 MAB_ready1_qualified1
        // 场景：MAB req.ready=1，commit资格满足=1；有效TLB hit；开关开启；跨16B普通parent；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：原始STA feedback.hit=1；接受parent，原始attempt不产生普通stout，最终由MAB完成。
        // 审计结论：完全建模。
        cp_05_02_001 : coverpoint ($isunknown((end_mab_good && !replay_feedback_bad[mab_slot])) ? 2'd2 : ((end_mab_good && !replay_feedback_bad[mab_slot]) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected && mab_slot<56 && end_ready_mask[mab_slot][3])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.02.002 MAB_ready0_qualified1
        // 场景：MAB req.ready=0，commit资格满足=1；有效TLB hit；开关开启；跨16B普通parent；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：原始STA feedback.hit=0；同parent允许后续attempt重试，不规定重试次数。
        // 审计结论：完全建模。
        cp_05_02_002 : coverpoint ($isunknown((end_mab_good && !replay_feedback_bad[mab_slot])) ? 2'd2 : ((end_mab_good && !replay_feedback_bad[mab_slot]) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected && mab_slot<56 && end_ready_mask[mab_slot][1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.02.003 MAB_ready1_qualified0
        // 场景：MAB req.ready=1，commit资格满足=0；有效TLB hit；开关开启；跨16B普通parent；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：原始STA feedback.hit=0；同parent允许后续attempt重试，不规定重试次数。
        // 审计结论：完全建模。
        cp_05_02_003 : coverpoint ($isunknown((end_mab_good && !replay_feedback_bad[mab_slot])) ? 2'd2 : ((end_mab_good && !replay_feedback_bad[mab_slot]) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected && mab_slot<56 && end_ready_mask[mab_slot][2])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.02.004 MAB_ready0_qualified0
        // 场景：MAB req.ready=0，commit资格满足=0；有效TLB hit；开关开启；跨16B普通parent；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：原始STA feedback.hit=0；同parent允许后续attempt重试，不规定重试次数。
        // 审计结论：完全建模。
        cp_05_02_004 : coverpoint ($isunknown((end_mab_good && !replay_feedback_bad[mab_slot])) ? 2'd2 : ((end_mab_good && !replay_feedback_bad[mab_slot]) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected && mab_slot<56 && end_ready_mask[mab_slot][0])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.02.005 MAB_双STA竞争同一parent槽
        // 场景：STA0与STA1同拍都有合法跨16B标量候选，MAB空闲；有效TLB hit；开关开启；跨16B普通parent；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：MAB按完整年龄及uop顺序选最老候选，只接收一个；未接收者慢反馈要求重试。
        // 审计结论：完全建模。
        cp_05_02_005 : coverpoint ($isunknown((dual_candidate_result)) ? 2'd2 : ((dual_candidate_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (dual_candidate_due)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.02.006 MAB_多次拒绝后恢复
        // 场景：同一parent连续多次因MAB占用被拒绝，随后ready恢复；有效TLB hit；开关开启；跨16B普通parent；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：每次拒绝均记录独立attempt；ready恢复后只接收一次parent，不能假设第三次必成功。
        // 审计结论：完全建模。
        cp_05_02_006 : coverpoint ($isunknown((end_mab_good && !replay_feedback_bad[mab_slot])) ? 2'd2 : ((end_mab_good && !replay_feedback_bad[mab_slot]) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected && mab_slot<56 && end_rejects[mab_slot]>=2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_05_02 cg_storeunit_05_02_inst;
    covergroup cg_storeunit_diag_05_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_05_02_001_result : coverpoint ($isunknown((end_mab_good && !replay_feedback_bad[mab_slot])) ? 2'd2 : ((end_mab_good && !replay_feedback_bad[mab_slot]) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected && mab_slot<56 && end_ready_mask[mab_slot][3])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_02_002_result : coverpoint ($isunknown((end_mab_good && !replay_feedback_bad[mab_slot])) ? 2'd2 : ((end_mab_good && !replay_feedback_bad[mab_slot]) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected && mab_slot<56 && end_ready_mask[mab_slot][1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_02_003_result : coverpoint ($isunknown((end_mab_good && !replay_feedback_bad[mab_slot])) ? 2'd2 : ((end_mab_good && !replay_feedback_bad[mab_slot]) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected && mab_slot<56 && end_ready_mask[mab_slot][2])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_02_004_result : coverpoint ($isunknown((end_mab_good && !replay_feedback_bad[mab_slot])) ? 2'd2 : ((end_mab_good && !replay_feedback_bad[mab_slot]) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected && mab_slot<56 && end_ready_mask[mab_slot][0])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_02_005_result : coverpoint ($isunknown((dual_candidate_result)) ? 2'd2 : ((dual_candidate_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (dual_candidate_due)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_02_006_result : coverpoint ($isunknown((end_mab_good && !replay_feedback_bad[mab_slot])) ? 2'd2 : ((end_mab_good && !replay_feedback_bad[mab_slot]) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected && mab_slot<56 && end_rejects[mab_slot]>=2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_05_02 cg_storeunit_diag_05_02_inst;

    // 05.03 候选入队与后拍撤销
    covergroup cg_storeunit_05_03 @(posedge clock);
        option.per_instance = 1;
        // 05.03.001 REVOKE_正常cacheable
        // 场景：跨16B标量的正常cacheable场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：请求可被接收，C2无异常时revoke=0。
        // 审计结论：完全建模。
        cp_05_03_001_pipe0 : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[0])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.001 REVOKE_正常cacheable
        // 场景：跨16B标量的正常cacheable场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：请求可被接收，C2无异常时revoke=0。
        // 审计结论：完全建模。
        cp_05_03_001_pipe1 : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[0])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.002 REVOKE_TLBmiss
        // 场景：跨16B标量的TLBmiss场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：C1候选仍可能入队；C2由延迟miss置revoke，原始STA走TLB重试。
        // 审计结论：完全建模。
        cp_05_03_002_pipe0 : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.002 REVOKE_TLBmiss
        // 场景：跨16B标量的TLBmiss场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：C1候选仍可能入队；C2由延迟miss置revoke，原始STA走TLB重试。
        // 审计结论：完全建模。
        cp_05_03_002_pipe1 : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.003 REVOKE_TLB页异常
        // 场景：跨16B标量的TLB页异常场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：C1候选可入队，C2由异常撤销；不把已撤销的parent再算正常拆分。
        // 审计结论：完全建模。
        cp_05_03_003_pipe0 : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[2])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.003 REVOKE_TLB页异常
        // 场景：跨16B标量的TLB页异常场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：C1候选可入队，C2由异常撤销；不把已撤销的parent再算正常拆分。
        // 审计结论：完全建模。
        cp_05_03_003_pipe1 : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[2])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.004 REVOKE_PMP拒绝
        // 场景：跨16B标量的PMP拒绝场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：C1可能接收，C2 AF形成并revoke，异常路径完成。
        // 审计结论：完全建模。
        cp_05_03_004_pipe0 : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[3])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.004 REVOKE_PMP拒绝
        // 场景：跨16B标量的PMP拒绝场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：C1可能接收，C2 AF形成并revoke，异常路径完成。
        // 审计结论：完全建模。
        cp_05_03_004_pipe1 : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[3])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.005 REVOKE_PBMT_NC
        // 场景：跨16B标量的PBMT_NC场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：admission可有效，但严格mis_align=0；C2 NC非对齐异常导致revoke。
        // 审计结论：完全建模。
        cp_05_03_005_pipe0 : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[4])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.005 REVOKE_PBMT_NC
        // 场景：跨16B标量的PBMT_NC场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：admission可有效，但严格mis_align=0；C2 NC非对齐异常导致revoke。
        // 审计结论：完全建模。
        cp_05_03_005_pipe1 : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[4])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.006 REVOKE_PBMT_IO
        // 场景：跨16B标量的PBMT_IO场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：admission可有效，但严格mis_align=0；C2 MMIO非对齐异常导致revoke。
        // 审计结论：完全建模。
        cp_05_03_006_pipe0 : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[5])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.006 REVOKE_PBMT_IO
        // 场景：跨16B标量的PBMT_IO场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：admission可有效，但严格mis_align=0；C2 MMIO非对齐异常导致revoke。
        // 审计结论：完全建模。
        cp_05_03_006_pipe1 : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[5])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.007 REVOKE_Breakpoint
        // 场景：跨16B标量的Breakpoint场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：admission不直接排除trigger；C2 breakpoint使revoke有效。
        // 审计结论：完全建模。
        cp_05_03_007_pipe0 : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[6])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.007 REVOKE_Breakpoint
        // 场景：跨16B标量的Breakpoint场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：admission不直接排除trigger；C2 breakpoint使revoke有效。
        // 审计结论：完全建模。
        cp_05_03_007_pipe1 : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[6])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.008 REVOKE_Debug动作
        // 场景：跨16B标量的Debug动作场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：admission不直接排除debug；C2 debug异常路径使revoke有效。
        // 审计结论：完全建模。
        cp_05_03_008_pipe0 : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[7])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.008 REVOKE_Debug动作
        // 场景：跨16B标量的Debug动作场景；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：admission不直接排除debug；C2 debug异常路径使revoke有效。
        // 审计结论：完全建模。
        cp_05_03_008_pipe1 : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[7])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.03.009 REVOKE_只影响前拍所选端口
        // 场景：双STA有候选，仅一端口被接收，另一端口后拍revoke；开关开启；跨16B非CBO非child；满足commit资格；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：MAB撤销须关联前拍实际选中的端口，不得由未选中端口撤掉已接收parent。
        // 审计结论：完全建模。
        cp_05_03_009 : coverpoint ($isunknown((!m_s2_needRevoke)) ? 2'd2 : ((!m_s2_needRevoke) ? 2'd1 : 2'd0))
            iff (sample_enable && (m_selected_accept_d1 && (m_selected_port_d1 ? m_io_enq_0_revoke && !m_io_enq_1_revoke : m_io_enq_1_revoke && !m_io_enq_0_revoke))) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_05_03 cg_storeunit_05_03_inst;
    covergroup cg_storeunit_diag_05_03 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_05_03_001_pipe0_result : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[0])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_001_pipe1_result : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[0])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_002_pipe0_result : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_002_pipe1_result : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_003_pipe0_result : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[2])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_003_pipe1_result : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[2])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_004_pipe0_result : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[3])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_004_pipe1_result : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[3])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_005_pipe0_result : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[4])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_005_pipe1_result : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[4])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_006_pipe0_result : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[5])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_006_pipe1_result : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[5])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_007_pipe0_result : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[6])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_007_pipe1_result : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[6])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_008_pipe0_result : coverpoint ($isunknown((accepted_effect_ok0)) ? 2'd2 : ((accepted_effect_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[0] && accepted_kind_d1_0[7])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_008_pipe1_result : coverpoint ($isunknown((accepted_effect_ok1)) ? 2'd2 : ((accepted_effect_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (accepted_revoke_due[1] && accepted_kind_d1_1[7])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_03_009_result : coverpoint ($isunknown((!m_s2_needRevoke)) ? 2'd2 : ((!m_s2_needRevoke) ? 2'd1 : 2'd0))
            iff (sample_enable && (m_selected_accept_d1 && (m_selected_port_d1 ? m_io_enq_0_revoke && !m_io_enq_1_revoke : m_io_enq_1_revoke && !m_io_enq_0_revoke))) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_05_03 cg_storeunit_diag_05_03_inst;

    // 05.04 child回流与响应
    covergroup cg_storeunit_05_04 @(posedge clock);
        option.per_instance = 1;
        // 05.04.003 CHILD_hit
        // 场景：有效标量MAB child收到hit结果；仅标量parent的child；isvec=0；只接入STA0；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：MO.need_rep=0，返回当前child的物理地址/属性。
        // 审计结论：完全建模。
        cp_05_04_003 : coverpoint ($isunknown((rc_next_ok)) ? 2'd2 : ((rc_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && !rc_miss && !rc_fault && !rc_nc && !rc_mmio)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.04.004 CHILD_miss
        // 场景：有效标量MAB child收到miss结果；仅标量parent的child；isvec=0；只接入STA0；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：MO.need_rep=1；MAB重发同一child，不推进到下一个child。
        // 审计结论：完全建模。
        cp_05_04_004 : coverpoint ($isunknown((rc_next_ok)) ? 2'd2 : ((rc_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && rc_miss)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.04.005 CHILD_PF
        // 场景：有效标量MAB child收到PF结果；仅标量parent的child；isvec=0；只接入STA0；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：MO携带storePageFault；MAB可转parent异常完成。
        // 审计结论：完全建模。
        cp_05_04_005 : coverpoint ($isunknown((rc_next_ok && rc_fault_addr_ok)) ? 2'd2 : ((rc_next_ok && rc_fault_addr_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && rc_fault && rc_pf_saved)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.04.006 CHILD_GPF
        // 场景：有效标量MAB child收到GPF结果；仅标量parent的child；isvec=0；只接入STA0；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：MO携带storeGuestPageFault及GPAddr相关元信息。
        // 审计结论：完全建模。
        cp_05_04_006 : coverpoint ($isunknown((rc_next_ok && rc_fault_addr_ok)) ? 2'd2 : ((rc_next_ok && rc_fault_addr_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && rc_fault && rc_gpf_saved)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.04.007 CHILD_PMP_AF
        // 场景：有效标量MAB child收到PMP_AF结果；仅标量parent的child；isvec=0；只接入STA0；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：MO携带storeAccessFault；属性与异常分别记录。
        // 审计结论：完全建模。
        cp_05_04_007 : coverpoint ($isunknown((rc_next_ok && rc_fault_addr_ok)) ? 2'd2 : ((rc_next_ok && rc_fault_addr_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && rc_fault && rc_af_saved)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.04.008 CHILD_NC
        // 场景：有效标量MAB child收到NC结果；仅标量parent的child；isvec=0；只接入STA0；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：child标志参与NC非对齐SAM公式；不当作正常cacheable child完成。
        // 审计结论：完全建模。
        cp_05_04_008 : coverpoint ($isunknown((rc_next_ok)) ? 2'd2 : ((rc_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && rc_nc)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.04.009 CHILD_MMIO
        // 场景：有效标量MAB child收到MMIO结果；仅标量parent的child；isvec=0；只接入STA0；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：child标志参与MMIO AF公式；MMIO结果/异常交由MAB消费。
        // 审计结论：完全建模。
        cp_05_04_009 : coverpoint ($isunknown((rc_next_ok)) ? 2'd2 : ((rc_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && rc_mmio)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.04.010 CHILD_HSV位恢复
        // 场景：HSV.H/W/D parent经过MAB拆分；仅标量parent的child；isvec=0；只接入STA0；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：MAB保留parent的HSV位并改写child大小；每个child仍以hyperinst=1翻译。
        // 审计结论：完全建模。
        cp_05_04_010 : coverpoint ($isunknown((m_io_splitStoreReq_bits_uop_fuOpType[4] && !m_io_splitStoreReq_bits_uop_fuOpType[3:2])) ? 2'd2 : ((m_io_splitStoreReq_bits_uop_fuOpType[4] && !m_io_splitStoreReq_bits_uop_fuOpType[3:2]) ? 2'd1 : 2'd0))
            iff (sample_enable && (parent_scalar && m_req_uop_fuOpType inside {16,17,18,19} && m_io_splitStoreReq_valid)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.04.011 CHILD_跨页两物理地址
        // 场景：跨4KB的parent，低/高页映射至不连续物理页；仅标量parent的child；isvec=0；只接入STA0；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：两child分别翻译；异常地址及高页PA来自相应child，不能把低页PA简单加长度作为高页PA。
        // 审计结论：完全建模。
        cp_05_04_011_normal_split : coverpoint ($isunknown((split_complete_good && split_complete_mab && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_mab && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_va[11:0]>4088 && split_complete_discontinuous)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.04.011 CHILD_跨页两物理地址
        // 场景：跨4KB的parent，低/高页映射至不连续物理页；仅标量parent的child；isvec=0；只接入STA0；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：两child分别翻译；异常地址及高页PA来自相应child，不能把低页PA简单加长度作为高页PA。
        // 审计结论：完全建模。
        cp_05_04_011_child_fault : coverpoint ($isunknown((rc_next_ok && rc_fault_addr_ok)) ? 2'd2 : ((rc_next_ok && rc_fault_addr_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && rc_fault && end_va[mab_slot][11:0]>4088)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_05_04 cg_storeunit_05_04_inst;
    covergroup cg_storeunit_diag_05_04 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_05_04_003_result : coverpoint ($isunknown((rc_next_ok)) ? 2'd2 : ((rc_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && !rc_miss && !rc_fault && !rc_nc && !rc_mmio)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_04_004_result : coverpoint ($isunknown((rc_next_ok)) ? 2'd2 : ((rc_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && rc_miss)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_04_005_result : coverpoint ($isunknown((rc_next_ok && rc_fault_addr_ok)) ? 2'd2 : ((rc_next_ok && rc_fault_addr_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && rc_fault && rc_pf_saved)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_04_006_result : coverpoint ($isunknown((rc_next_ok && rc_fault_addr_ok)) ? 2'd2 : ((rc_next_ok && rc_fault_addr_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && rc_fault && rc_gpf_saved)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_04_007_result : coverpoint ($isunknown((rc_next_ok && rc_fault_addr_ok)) ? 2'd2 : ((rc_next_ok && rc_fault_addr_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && rc_fault && rc_af_saved)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_04_008_result : coverpoint ($isunknown((rc_next_ok)) ? 2'd2 : ((rc_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && rc_nc)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_04_009_result : coverpoint ($isunknown((rc_next_ok)) ? 2'd2 : ((rc_next_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && rc_mmio)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_04_010_result : coverpoint ($isunknown((m_io_splitStoreReq_bits_uop_fuOpType[4] && !m_io_splitStoreReq_bits_uop_fuOpType[3:2])) ? 2'd2 : ((m_io_splitStoreReq_bits_uop_fuOpType[4] && !m_io_splitStoreReq_bits_uop_fuOpType[3:2]) ? 2'd1 : 2'd0))
            iff (sample_enable && (parent_scalar && m_req_uop_fuOpType inside {16,17,18,19} && m_io_splitStoreReq_valid)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_04_011_normal_split_result : coverpoint ($isunknown((split_complete_good && split_complete_mab && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_mab && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_va[11:0]>4088 && split_complete_discontinuous)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_04_011_child_fault_result : coverpoint ($isunknown((rc_next_ok && rc_fault_addr_ok)) ? 2'd2 : ((rc_next_ok && rc_fault_addr_ok) ? 2'd1 : 2'd0))
            iff (sample_enable && (rc_due && rc_fault && end_va[mab_slot][11:0]>4088)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_05_04 cg_storeunit_diag_05_04_inst;

    // 05.05 parent完成与SQ边界
    covergroup cg_storeunit_05_05 @(posedge clock);
        option.per_instance = 1;
        // 05.05.001 PARENT_两child正常完成
        // 场景：cacheable非跨页parent的两个child均成功；仅标量parent；最终完成来源由MemBlock实际mux识别；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：普通StoreUnit不为child写回；MAB在其等待/延迟结束后产生一次parent写回，写回ROB属于parent。
        // 审计结论：完全建模。
        cp_05_05_001 : coverpoint ($isunknown((life_wb_good && !m_req_valid)) ? 2'd2 : ((life_wb_good && !m_req_valid) ? 2'd1 : 2'd0))
            iff (sample_enable && (life_wb_due && life_children==3 && !m_cross_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.05.002 PARENT_跨页等待ROB队头
        // 场景：parent跨4KB且已入MAB但尚未成为pending ROB store；仅标量parent；最终完成来源由MemBlock实际mux识别；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：跨页parent在相应等待条件满足后才拆分发出child；等待不当作已执行的child。
        // 审计结论：完全建模。
        cp_05_05_002 : coverpoint ($isunknown((!life_bad && parent_rob==life_rob && m_bufferState==1 && !m_io_splitStoreReq_valid)) ? 2'd2 : ((!life_bad && parent_rob==life_rob && m_bufferState==1 && !m_io_splitStoreReq_valid) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_05_002_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.05.003 PARENT_跨页写回后保留高页
        // 场景：正常跨页parent已完成写回，SQ尚未doDeq；仅标量parent；最终完成来源由MemBlock实际mux识别；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：MAB进入block并保留高页信息，SQ dequeue后释放；WB不等于MAB立即空闲。
        // 审计结论：完全建模。
        cp_05_05_003 : coverpoint ($isunknown((!m_req_valid && end_mab_hold_good)) ? 2'd2 : ((!m_req_valid && end_mab_hold_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_mab_release_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.05.004 PARENT_高child异常
        // 场景：低child成功，高child返回PF/GPF/AF；仅标量parent；最终完成来源由MemBlock实际mux识别；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终parent异常地址来自失败child，停止按两child均正常的路径计完成。
        // 审计结论：完全建模。
        cp_05_05_004_af : coverpoint ($isunknown((life_finish_good && m_req_vaddr==fault_va)) ? 2'd2 : ((life_finish_good && m_req_vaddr==fault_va) ? 2'd1 : 2'd0))
            iff (sample_enable && (life_finish_context && life_children[0] && life_faultkind[0])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.05.004 PARENT_高child异常
        // 场景：低child成功，高child返回PF/GPF/AF；仅标量parent；最终完成来源由MemBlock实际mux识别；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终parent异常地址来自失败child，停止按两child均正常的路径计完成。
        // 审计结论：完全建模。
        cp_05_05_004_pf : coverpoint ($isunknown((life_finish_good && m_req_vaddr==fault_va)) ? 2'd2 : ((life_finish_good && m_req_vaddr==fault_va) ? 2'd1 : 2'd0))
            iff (sample_enable && (life_finish_context && life_children[0] && life_faultkind[1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.05.004 PARENT_高child异常
        // 场景：低child成功，高child返回PF/GPF/AF；仅标量parent；最终完成来源由MemBlock实际mux识别；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：最终parent异常地址来自失败child，停止按两child均正常的路径计完成。
        // 审计结论：完全建模。
        cp_05_05_004_gpf : coverpoint ($isunknown((life_finish_good && m_req_vaddr==fault_va)) ? 2'd2 : ((life_finish_good && m_req_vaddr==fault_va) ? 2'd1 : 2'd0))
            iff (sample_enable && (life_finish_context && life_children[0] && life_faultkind[2])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 05.05.005 PARENT_child多次miss
        // 场景：同一child连续miss后hit；仅标量parent；最终完成来源由MemBlock实际mux识别；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：miss只增加child attempt；不重复原始STA反馈，不为每次miss产生parent写回。
        // 审计结论：完全建模。
        cp_05_05_005 : coverpoint ($isunknown((end_mab_good && !replay_feedback_bad[mab_slot])) ? 2'd2 : ((end_mab_good && !replay_feedback_bad[mab_slot]) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected && mab_slot<56 && end_max_child_retries[mab_slot]>=2)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_05_05 cg_storeunit_05_05_inst;
    covergroup cg_storeunit_diag_05_05 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_05_05_001_result : coverpoint ($isunknown((life_wb_good && !m_req_valid)) ? 2'd2 : ((life_wb_good && !m_req_valid) ? 2'd1 : 2'd0))
            iff (sample_enable && (life_wb_due && life_children==3 && !m_cross_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_05_002_result : coverpoint ($isunknown((!life_bad && parent_rob==life_rob && m_bufferState==1 && !m_io_splitStoreReq_valid)) ? 2'd2 : ((!life_bad && parent_rob==life_rob && m_bufferState==1 && !m_io_splitStoreReq_valid) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_05_05_002_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_05_003_result : coverpoint ($isunknown((!m_req_valid && end_mab_hold_good)) ? 2'd2 : ((!m_req_valid && end_mab_hold_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_mab_release_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_05_004_af_result : coverpoint ($isunknown((life_finish_good && m_req_vaddr==fault_va)) ? 2'd2 : ((life_finish_good && m_req_vaddr==fault_va) ? 2'd1 : 2'd0))
            iff (sample_enable && (life_finish_context && life_children[0] && life_faultkind[0])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_05_004_pf_result : coverpoint ($isunknown((life_finish_good && m_req_vaddr==fault_va)) ? 2'd2 : ((life_finish_good && m_req_vaddr==fault_va) ? 2'd1 : 2'd0))
            iff (sample_enable && (life_finish_context && life_children[0] && life_faultkind[1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_05_004_gpf_result : coverpoint ($isunknown((life_finish_good && m_req_vaddr==fault_va)) ? 2'd2 : ((life_finish_good && m_req_vaddr==fault_va) ? 2'd1 : 2'd0))
            iff (sample_enable && (life_finish_context && life_children[0] && life_faultkind[2])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_05_05_005_result : coverpoint ($isunknown((end_mab_good && !replay_feedback_bad[mab_slot])) ? 2'd2 : ((end_mab_good && !replay_feedback_bad[mab_slot]) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected && mab_slot<56 && end_max_child_retries[mab_slot]>=2)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_05_05 cg_storeunit_diag_05_05_inst;

    // 06.04 Store–Load依赖查询
    covergroup cg_storeunit_06_04 @(posedge clock);
        option.per_instance = 1;
        // 06.04.007 NUKE_年轻load交接
        // 场景：older store与younger load地址重叠，观察真实下游依赖处理；采样真实S1；nuke.valid不直接门控redirect或exception；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：load相关重放不能当作older StoreUnit自己STA replay；Store反馈仍由TLB/MAB条件决定。
        // 审计结论：完全建模。
        cp_06_04_007_load0 : coverpoint ($isunknown((l0_result)) ? 2'd2 : ((l0_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (l0_due)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.04.007 NUKE_年轻load交接
        // 场景：older store与younger load地址重叠，观察真实下游依赖处理；采样真实S1；nuke.valid不直接门控redirect或exception；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：load相关重放不能当作older StoreUnit自己STA replay；Store反馈仍由TLB/MAB条件决定。
        // 审计结论：完全建模。
        cp_06_04_007_load1 : coverpoint ($isunknown((l1_result)) ? 2'd2 : ((l1_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (l1_due)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.04.007 NUKE_年轻load交接
        // 场景：older store与younger load地址重叠，观察真实下游依赖处理；采样真实S1；nuke.valid不直接门控redirect或exception；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：load相关重放不能当作older StoreUnit自己STA replay；Store反馈仍由TLB/MAB条件决定。
        // 审计结论：完全建模。
        cp_06_04_007_load2 : coverpoint ($isunknown((l2_result)) ? 2'd2 : ((l2_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (l2_due)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.04.007 NUKE_年轻load交接
        // 场景：older store与younger load地址重叠，观察真实下游依赖处理；采样真实S1；nuke.valid不直接门控redirect或exception；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：load相关重放不能当作older StoreUnit自己STA replay；Store反馈仍由TLB/MAB条件决定。
        // 审计结论：完全建模。
        cp_06_04_007_raw0 : coverpoint ($isunknown((raw_result0)) ? 2'd2 : ((raw_result0) ? 2'd1 : 2'd0))
            iff (sample_enable && (raw_due0[2])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 06.04.007 NUKE_年轻load交接
        // 场景：older store与younger load地址重叠，观察真实下游依赖处理；采样真实S1；nuke.valid不直接门控redirect或exception；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：load相关重放不能当作older StoreUnit自己STA replay；Store反馈仍由TLB/MAB条件决定。
        // 审计结论：完全建模。
        cp_06_04_007_raw1 : coverpoint ($isunknown((raw_result1)) ? 2'd2 : ((raw_result1) ? 2'd1 : 2'd0))
            iff (sample_enable && (raw_due1[2])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_06_04 cg_storeunit_06_04_inst;
    covergroup cg_storeunit_diag_06_04 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_06_04_007_load0_result : coverpoint ($isunknown((l0_result)) ? 2'd2 : ((l0_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (l0_due)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_04_007_load1_result : coverpoint ($isunknown((l1_result)) ? 2'd2 : ((l1_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (l1_due)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_04_007_load2_result : coverpoint ($isunknown((l2_result)) ? 2'd2 : ((l2_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (l2_due)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_04_007_raw0_result : coverpoint ($isunknown((raw_result0)) ? 2'd2 : ((raw_result0) ? 2'd1 : 2'd0))
            iff (sample_enable && (raw_due0[2])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_06_04_007_raw1_result : coverpoint ($isunknown((raw_result1)) ? 2'd2 : ((raw_result1) ? 2'd1 : 2'd0))
            iff (sample_enable && (raw_due1[2])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_06_04 cg_storeunit_diag_06_04_inst;

    // 07.02 S1地址包与可用标志
    covergroup cg_storeunit_07_02 @(posedge clock);
        option.per_instance = 1;
        // 07.02.002 LSQ_同16B非对齐
        // 场景：普通非对齐未跨16B且hit；L为Valid sideband无ready；SQ消费需结合miss与updateAddrValid；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：L.updateAddrValid=1、misalignWith16Byte=1；SQ保存unaligned并在数据路径移位。
        // 审计结论：完全建模。
        cp_07_02_002_lane0 : coverpoint ($isunknown((transfer_output_good0 && transfer_origin_good[head0])) ? 2'd2 : ((transfer_output_good0 && transfer_origin_good[head0]) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop0 && transfer_known[head0] && transfer_within[head0] && !transfer_cross[head0])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.02.002 LSQ_同16B非对齐
        // 场景：普通非对齐未跨16B且hit；L为Valid sideband无ready；SQ消费需结合miss与updateAddrValid；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：L.updateAddrValid=1、misalignWith16Byte=1；SQ保存unaligned并在数据路径移位。
        // 审计结论：完全建模。
        cp_07_02_002_lane1 : coverpoint ($isunknown((transfer_output_good1 && transfer_origin_good[head1])) ? 2'd2 : ((transfer_output_good1 && transfer_origin_good[head1]) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop1 && transfer_known[head1] && transfer_within[head1] && !transfer_cross[head1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.02.007 LSQ_child非最终
        // 场景：MAB非最终child正常hit；L为Valid sideband无ready；SQ消费需结合miss与updateAddrValid；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：L.isFrmMisAlignBuf=1；非最终标志抑制普通地址可用标志。
        // 审计结论：完全建模。
        cp_07_02_007_child : coverpoint ($isunknown((cam_ok0 && zz0_io_lsq_bits_updateAddrValid==0)) ? 2'd2 : ((cam_ok0 && zz0_io_lsq_bits_updateAddrValid==0) ? 2'd1 : 2'd0))
            iff (sample_enable && (c0_expected1 && c0_child1 && c0_final1==0 && c0_hit1 && !c0_exp_exception1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.02.008 LSQ_child最终
        // 场景：MAB最终child正常hit；L为Valid sideband无ready；SQ消费需结合miss与updateAddrValid；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：final参与updateAddrValid；SQ不以child重写parent的普通paddr/vaddr CAM入口。
        // 审计结论：完全建模。
        cp_07_02_008_child : coverpoint ($isunknown((cam_ok0 && zz0_io_lsq_bits_updateAddrValid==1)) ? 2'd2 : ((cam_ok0 && zz0_io_lsq_bits_updateAddrValid==1) ? 2'd1 : 2'd0))
            iff (sample_enable && (c0_expected1 && c0_child1 && c0_final1==1 && c0_hit1 && !c0_exp_exception1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_07_02 cg_storeunit_07_02_inst;
    covergroup cg_storeunit_diag_07_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_07_02_002_lane0_result : coverpoint ($isunknown((transfer_output_good0 && transfer_origin_good[head0])) ? 2'd2 : ((transfer_output_good0 && transfer_origin_good[head0]) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop0 && transfer_known[head0] && transfer_within[head0] && !transfer_cross[head0])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_02_002_lane1_result : coverpoint ($isunknown((transfer_output_good1 && transfer_origin_good[head1])) ? 2'd2 : ((transfer_output_good1 && transfer_origin_good[head1]) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop1 && transfer_known[head1] && transfer_within[head1] && !transfer_cross[head1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_02_007_child_result : coverpoint ($isunknown((cam_ok0 && zz0_io_lsq_bits_updateAddrValid==0)) ? 2'd2 : ((cam_ok0 && zz0_io_lsq_bits_updateAddrValid==0) ? 2'd1 : 2'd0))
            iff (sample_enable && (c0_expected1 && c0_child1 && c0_final1==0 && c0_hit1 && !c0_exp_exception1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_02_008_child_result : coverpoint ($isunknown((cam_ok0 && zz0_io_lsq_bits_updateAddrValid==1)) ? 2'd2 : ((cam_ok0 && zz0_io_lsq_bits_updateAddrValid==1) ? 2'd1 : 2'd0))
            iff (sample_enable && (c0_expected1 && c0_child1 && c0_final1==1 && c0_hit1 && !c0_exp_exception1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_07_02 cg_storeunit_diag_07_02_inst;

    // 07.03 S2补充属性的消费关联
    covergroup cg_storeunit_07_03 @(posedge clock);
        option.per_instance = 1;
        // 07.03.001 REPL_正常补充
        // 场景：前拍有效hit且当前RE.updateAddrValid=1；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：SQ只更新前拍锁存SQ索引的pending/mmio/memBackTypeMM/hasException等字段。
        // 审计结论：完全建模。
        cp_07_03_001_lane0 : coverpoint ($isunknown((q_attr_result0)) ? 2'd2 : ((q_attr_result0) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_001_lane0_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.001 REPL_正常补充
        // 场景：前拍有效hit且当前RE.updateAddrValid=1；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：SQ只更新前拍锁存SQ索引的pending/mmio/memBackTypeMM/hasException等字段。
        // 审计结论：完全建模。
        cp_07_03_001_lane1 : coverpoint ($isunknown((q_attr_result1)) ? 2'd2 : ((q_attr_result1) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_001_lane1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.002 REPL_前拍miss
        // 场景：前拍L.miss=1而当前RE有非零payload；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：SQ正常replenish写使能为0；不把残留属性写入miss项。
        // 审计结论：完全建模。
        cp_07_03_002_lane0 : coverpoint ($isunknown((q_attr_result0)) ? 2'd2 : ((q_attr_result0) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_002_lane0_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.002 REPL_前拍miss
        // 场景：前拍L.miss=1而当前RE有非零payload；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：SQ正常replenish写使能为0；不把残留属性写入miss项。
        // 审计结论：完全建模。
        cp_07_03_002_lane1 : coverpoint ($isunknown((q_attr_result1)) ? 2'd2 : ((q_attr_result1) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_002_lane1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.003 REPL_前拍无效
        // 场景：前拍L.valid=0而当前RE有非零payload；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：SQ不消费该RE作为独立事务。
        // 审计结论：完全建模。
        cp_07_03_003_lane0 : coverpoint ($isunknown((q_attr_result0)) ? 2'd2 : ((q_attr_result0) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_003_lane0_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.003 REPL_前拍无效
        // 场景：前拍L.valid=0而当前RE有非零payload；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：SQ不消费该RE作为独立事务。
        // 审计结论：完全建模。
        cp_07_03_003_lane1 : coverpoint ($isunknown((q_attr_result1)) ? 2'd2 : ((q_attr_result1) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_003_lane1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.004 REPL_更新门关闭
        // 场景：前拍有效hit但RE.updateAddrValid=0；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：SQ不执行该正常补充更新；需继续观察所属MAB/replay路径。
        // 审计结论：完全建模。
        cp_07_03_004_lane0 : coverpoint ($isunknown((q_attr_result0)) ? 2'd2 : ((q_attr_result0) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_004_lane0_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.004 REPL_更新门关闭
        // 场景：前拍有效hit但RE.updateAddrValid=0；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：SQ不执行该正常补充更新；需继续观察所属MAB/replay路径。
        // 审计结论：完全建模。
        cp_07_03_004_lane1 : coverpoint ($isunknown((q_attr_result1)) ? 2'd2 : ((q_attr_result1) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_004_lane1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.005 REPL_连续不同SQ
        // 场景：相邻两拍地址包属于不同SQ条目且属性不同；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：每拍RE只对应前拍地址包，不串用当前新请求的SQ索引。
        // 审计结论：完全建模。
        cp_07_03_005_lane0 : coverpoint ($isunknown((q_attr_result0)) ? 2'd2 : ((q_attr_result0) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_005_lane0_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.005 REPL_连续不同SQ
        // 场景：相邻两拍地址包属于不同SQ条目且属性不同；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：每拍RE只对应前拍地址包，不串用当前新请求的SQ索引。
        // 审计结论：完全建模。
        cp_07_03_005_lane1 : coverpoint ($isunknown((q_attr_result1)) ? 2'd2 : ((q_attr_result1) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_005_lane1_d1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.006 REPL_AF有效门控
        // 场景：S2形成AF，分别观察有效未kill和redirect kill条件；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：RE.af受s2_valid与!s2_kill门控，和组合s2_out_af分别记录。
        // 审计结论：完全建模。
        cp_07_03_006_lane0_kill0 : coverpoint ($isunknown((kill_ex_gate_ok0)) ? 2'd2 : ((kill_ex_gate_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (c0_expected2 && c0_exp_exception2 && z0_s2_kill==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.006 REPL_AF有效门控
        // 场景：S2形成AF，分别观察有效未kill和redirect kill条件；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：RE.af受s2_valid与!s2_kill门控，和组合s2_out_af分别记录。
        // 审计结论：完全建模。
        cp_07_03_006_lane0_kill1 : coverpoint ($isunknown((kill_ex_gate_ok0)) ? 2'd2 : ((kill_ex_gate_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (c0_expected2 && c0_exp_exception2 && z0_s2_kill==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.006 REPL_AF有效门控
        // 场景：S2形成AF，分别观察有效未kill和redirect kill条件；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：RE.af受s2_valid与!s2_kill门控，和组合s2_out_af分别记录。
        // 审计结论：完全建模。
        cp_07_03_006_lane1_kill0 : coverpoint ($isunknown((kill_ex_gate_ok1)) ? 2'd2 : ((kill_ex_gate_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (c1_expected2 && c1_exp_exception2 && z1_s2_kill==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.006 REPL_AF有效门控
        // 场景：S2形成AF，分别观察有效未kill和redirect kill条件；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：RE.af受s2_valid与!s2_kill门控，和组合s2_out_af分别记录。
        // 审计结论：完全建模。
        cp_07_03_006_lane1_kill1 : coverpoint ($isunknown((kill_ex_gate_ok1)) ? 2'd2 : ((kill_ex_gate_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (c1_expected2 && c1_exp_exception2 && z1_s2_kill==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.007 REPL_异常有效门控
        // 场景：S2异常位图或Debug动作有效；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：RE.hasException结合s2_valid与!s2_kill，不凭payload位图直接宣布有效异常。
        // 审计结论：完全建模。
        cp_07_03_007_lane0_kill0 : coverpoint ($isunknown((kill_ex_gate_ok0)) ? 2'd2 : ((kill_ex_gate_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (c0_expected2 && c0_exp_exception2 && z0_s2_kill==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.007 REPL_异常有效门控
        // 场景：S2异常位图或Debug动作有效；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：RE.hasException结合s2_valid与!s2_kill，不凭payload位图直接宣布有效异常。
        // 审计结论：完全建模。
        cp_07_03_007_lane0_kill1 : coverpoint ($isunknown((kill_ex_gate_ok0)) ? 2'd2 : ((kill_ex_gate_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (c0_expected2 && c0_exp_exception2 && z0_s2_kill==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.007 REPL_异常有效门控
        // 场景：S2异常位图或Debug动作有效；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：RE.hasException结合s2_valid与!s2_kill，不凭payload位图直接宣布有效异常。
        // 审计结论：完全建模。
        cp_07_03_007_lane1_kill0 : coverpoint ($isunknown((kill_ex_gate_ok1)) ? 2'd2 : ((kill_ex_gate_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (c1_expected2 && c1_exp_exception2 && z1_s2_kill==0)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.03.007 REPL_异常有效门控
        // 场景：S2异常位图或Debug动作有效；SQ实际消费条件为前拍storeAddrIn有效且非miss，再与当前RE.updateAddrValid结合；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：RE.hasException结合s2_valid与!s2_kill，不凭payload位图直接宣布有效异常。
        // 审计结论：完全建模。
        cp_07_03_007_lane1_kill1 : coverpoint ($isunknown((kill_ex_gate_ok1)) ? 2'd2 : ((kill_ex_gate_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (c1_expected2 && c1_exp_exception2 && z1_s2_kill==1)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_07_03 cg_storeunit_07_03_inst;
    covergroup cg_storeunit_diag_07_03 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_07_03_001_lane0_result : coverpoint ($isunknown((q_attr_result0)) ? 2'd2 : ((q_attr_result0) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_001_lane0_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_001_lane1_result : coverpoint ($isunknown((q_attr_result1)) ? 2'd2 : ((q_attr_result1) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_001_lane1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_002_lane0_result : coverpoint ($isunknown((q_attr_result0)) ? 2'd2 : ((q_attr_result0) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_002_lane0_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_002_lane1_result : coverpoint ($isunknown((q_attr_result1)) ? 2'd2 : ((q_attr_result1) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_002_lane1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_003_lane0_result : coverpoint ($isunknown((q_attr_result0)) ? 2'd2 : ((q_attr_result0) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_003_lane0_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_003_lane1_result : coverpoint ($isunknown((q_attr_result1)) ? 2'd2 : ((q_attr_result1) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_003_lane1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_004_lane0_result : coverpoint ($isunknown((q_attr_result0)) ? 2'd2 : ((q_attr_result0) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_004_lane0_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_004_lane1_result : coverpoint ($isunknown((q_attr_result1)) ? 2'd2 : ((q_attr_result1) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_004_lane1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_005_lane0_result : coverpoint ($isunknown((q_attr_result0)) ? 2'd2 : ((q_attr_result0) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_005_lane0_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_005_lane1_result : coverpoint ($isunknown((q_attr_result1)) ? 2'd2 : ((q_attr_result1) ? 2'd1 : 2'd0))
            iff (sample_enable && (scene_07_03_005_lane1_d1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_006_lane0_kill0_result : coverpoint ($isunknown((kill_ex_gate_ok0)) ? 2'd2 : ((kill_ex_gate_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (c0_expected2 && c0_exp_exception2 && z0_s2_kill==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_006_lane0_kill1_result : coverpoint ($isunknown((kill_ex_gate_ok0)) ? 2'd2 : ((kill_ex_gate_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (c0_expected2 && c0_exp_exception2 && z0_s2_kill==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_006_lane1_kill0_result : coverpoint ($isunknown((kill_ex_gate_ok1)) ? 2'd2 : ((kill_ex_gate_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (c1_expected2 && c1_exp_exception2 && z1_s2_kill==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_006_lane1_kill1_result : coverpoint ($isunknown((kill_ex_gate_ok1)) ? 2'd2 : ((kill_ex_gate_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (c1_expected2 && c1_exp_exception2 && z1_s2_kill==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_007_lane0_kill0_result : coverpoint ($isunknown((kill_ex_gate_ok0)) ? 2'd2 : ((kill_ex_gate_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (c0_expected2 && c0_exp_exception2 && z0_s2_kill==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_007_lane0_kill1_result : coverpoint ($isunknown((kill_ex_gate_ok0)) ? 2'd2 : ((kill_ex_gate_ok0) ? 2'd1 : 2'd0))
            iff (sample_enable && (c0_expected2 && c0_exp_exception2 && z0_s2_kill==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_007_lane1_kill0_result : coverpoint ($isunknown((kill_ex_gate_ok1)) ? 2'd2 : ((kill_ex_gate_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (c1_expected2 && c1_exp_exception2 && z1_s2_kill==0)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_03_007_lane1_kill1_result : coverpoint ($isunknown((kill_ex_gate_ok1)) ? 2'd2 : ((kill_ex_gate_ok1) ? 2'd1 : 2'd0))
            iff (sample_enable && (c1_expected2 && c1_exp_exception2 && z1_s2_kill==1)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_07_03 cg_storeunit_diag_07_03_inst;

    // 07.04 地址、数据与提交边界
    covergroup cg_storeunit_07_04 @(posedge clock);
        option.per_instance = 1;
        // 07.04.001 BOUNDARY_地址先于数据
        // 场景：同SQ地址先ready，STD数据稍后到达；仅观察与标量地址路径有关的交接；STD实际data不来自当前StoreUnit端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：StoreUnit可先完成STA；SQ必须等数据等实际条件后才能写出，STA完成不代表内存已写。
        // 审计结论：完全建模。
        cp_07_04_001_aligned_or_within_lane0 : coverpoint ($isunknown((transfer_output_good0)) ? 2'd2 : ((transfer_output_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop0 && transfer_known[head0] && !transfer_fault[head0] && !transfer_cross[head0] && transfer_order_a[head0])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.04.001 BOUNDARY_地址先于数据
        // 场景：同SQ地址先ready，STD数据稍后到达；仅观察与标量地址路径有关的交接；STD实际data不来自当前StoreUnit端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：StoreUnit可先完成STA；SQ必须等数据等实际条件后才能写出，STA完成不代表内存已写。
        // 审计结论：完全建模。
        cp_07_04_001_aligned_or_within_lane1 : coverpoint ($isunknown((transfer_output_good1)) ? 2'd2 : ((transfer_output_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop1 && transfer_known[head1] && !transfer_fault[head1] && !transfer_cross[head1] && transfer_order_a[head1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.04.001 BOUNDARY_地址先于数据
        // 场景：同SQ地址先ready，STD数据稍后到达；仅观察与标量地址路径有关的交接；STD实际data不来自当前StoreUnit端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：StoreUnit可先完成STA；SQ必须等数据等实际条件后才能写出，STA完成不代表内存已写。
        // 审计结论：完全建模。
        cp_07_04_001_cross16_pair : coverpoint ($isunknown((split_complete_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_order_a)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.04.002 BOUNDARY_数据先于地址
        // 场景：STD数据先到，标量地址稍后hit；仅观察与标量地址路径有关的交接；STD实际data不来自当前StoreUnit端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：按同SQ身份汇合；数据就绪不能替代缺失地址翻译。
        // 审计结论：完全建模。
        cp_07_04_002_aligned_or_within_lane0 : coverpoint ($isunknown((transfer_output_good0)) ? 2'd2 : ((transfer_output_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop0 && transfer_known[head0] && !transfer_fault[head0] && !transfer_cross[head0] && transfer_order_d[head0])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.04.002 BOUNDARY_数据先于地址
        // 场景：STD数据先到，标量地址稍后hit；仅观察与标量地址路径有关的交接；STD实际data不来自当前StoreUnit端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：按同SQ身份汇合；数据就绪不能替代缺失地址翻译。
        // 审计结论：完全建模。
        cp_07_04_002_aligned_or_within_lane1 : coverpoint ($isunknown((transfer_output_good1)) ? 2'd2 : ((transfer_output_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop1 && transfer_known[head1] && !transfer_fault[head1] && !transfer_cross[head1] && transfer_order_d[head1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.04.002 BOUNDARY_数据先于地址
        // 场景：STD数据先到，标量地址稍后hit；仅观察与标量地址路径有关的交接；STD实际data不来自当前StoreUnit端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：按同SQ身份汇合；数据就绪不能替代缺失地址翻译。
        // 审计结论：完全建模。
        cp_07_04_002_cross16_pair : coverpoint ($isunknown((split_complete_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_order_d)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.04.003 BOUNDARY_异常禁止正常写出
        // 场景：地址路径形成有效异常；仅观察与标量地址路径有关的交接；STD实际data不来自当前StoreUnit端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：观察SQ异常交接；不能仅因mask/address存在就将其计为正常memory write。
        // 审计结论：完全建模。
        cp_07_04_003_lane0 : coverpoint ($isunknown((transfer_output_good0)) ? 2'd2 : ((transfer_output_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop0 && transfer_known[head0] && transfer_fault[head0])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.04.003 BOUNDARY_异常禁止正常写出
        // 场景：地址路径形成有效异常；仅观察与标量地址路径有关的交接；STD实际data不来自当前StoreUnit端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：观察SQ异常交接；不能仅因mask/address存在就将其计为正常memory write。
        // 审计结论：完全建模。
        cp_07_04_003_lane1 : coverpoint ($isunknown((transfer_output_good1)) ? 2'd2 : ((transfer_output_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop1 && transfer_known[head1] && transfer_fault[head1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.04.003 BOUNDARY_异常禁止正常写出
        // 场景：地址路径形成有效异常；仅观察与标量地址路径有关的交接；STD实际data不来自当前StoreUnit端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：观察SQ异常交接；不能仅因mask/address存在就将其计为正常memory write。
        // 审计结论：完全建模。
        cp_07_04_003_nc_direct : coverpoint ($isunknown((end_nc_fault_relation)) ? 2'd2 : ((end_nc_fault_relation) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_nc_fault_complete)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.04.004 BOUNDARY_CBO块操作
        // 场景：CBO提供wlineflag和全窗口mask；仅观察与标量地址路径有关的交接；STD实际data不来自当前StoreUnit端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：块级范围由SQ/CMO/SBuffer消费；0xffff本身不是64B数据写入。
        // 审计结论：完全建模。
        cp_07_04_004_lane0 : coverpoint ($isunknown((transfer_output_good0 && xq_io_sbuffer_0_bits_data==0)) ? 2'd2 : ((transfer_output_good0 && xq_io_sbuffer_0_bits_data==0) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop0 && transfer_known[head0] && transfer_wline[head0])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.04.004 BOUNDARY_CBO块操作
        // 场景：CBO提供wlineflag和全窗口mask；仅观察与标量地址路径有关的交接；STD实际data不来自当前StoreUnit端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：块级范围由SQ/CMO/SBuffer消费；0xffff本身不是64B数据写入。
        // 审计结论：完全建模。
        cp_07_04_004_lane1 : coverpoint ($isunknown((transfer_output_good1 && xq_io_sbuffer_1_bits_data==0)) ? 2'd2 : ((transfer_output_good1 && xq_io_sbuffer_1_bits_data==0) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop1 && transfer_known[head1] && transfer_wline[head1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 07.04.004 BOUNDARY_CBO块操作
        // 场景：CBO提供wlineflag和全窗口mask；仅观察与标量地址路径有关的交接；STD实际data不来自当前StoreUnit端口；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：块级范围由SQ/CMO/SBuffer消费；0xffff本身不是64B数据写入。
        // 审计结论：完全建模。
        cp_07_04_004_CMO : coverpoint ($isunknown((special_result && cmo_good && cmo_resp_seen)) ? 2'd2 : ((special_result && cmo_good && cmo_resp_seen) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && cmo_pending)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_07_04 cg_storeunit_07_04_inst;
    covergroup cg_storeunit_diag_07_04 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_07_04_001_aligned_or_within_lane0_result : coverpoint ($isunknown((transfer_output_good0)) ? 2'd2 : ((transfer_output_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop0 && transfer_known[head0] && !transfer_fault[head0] && !transfer_cross[head0] && transfer_order_a[head0])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_04_001_aligned_or_within_lane1_result : coverpoint ($isunknown((transfer_output_good1)) ? 2'd2 : ((transfer_output_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop1 && transfer_known[head1] && !transfer_fault[head1] && !transfer_cross[head1] && transfer_order_a[head1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_04_001_cross16_pair_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_order_a)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_04_002_aligned_or_within_lane0_result : coverpoint ($isunknown((transfer_output_good0)) ? 2'd2 : ((transfer_output_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop0 && transfer_known[head0] && !transfer_fault[head0] && !transfer_cross[head0] && transfer_order_d[head0])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_04_002_aligned_or_within_lane1_result : coverpoint ($isunknown((transfer_output_good1)) ? 2'd2 : ((transfer_output_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop1 && transfer_known[head1] && !transfer_fault[head1] && !transfer_cross[head1] && transfer_order_d[head1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_04_002_cross16_pair_result : coverpoint ($isunknown((split_complete_good && split_complete_origin_good)) ? 2'd2 : ((split_complete_good && split_complete_origin_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (split_complete_due && split_complete_order_d)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_04_003_lane0_result : coverpoint ($isunknown((transfer_output_good0)) ? 2'd2 : ((transfer_output_good0) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop0 && transfer_known[head0] && transfer_fault[head0])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_04_003_lane1_result : coverpoint ($isunknown((transfer_output_good1)) ? 2'd2 : ((transfer_output_good1) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop1 && transfer_known[head1] && transfer_fault[head1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_04_003_nc_direct_result : coverpoint ($isunknown((end_nc_fault_relation)) ? 2'd2 : ((end_nc_fault_relation) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_nc_fault_complete)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_04_004_lane0_result : coverpoint ($isunknown((transfer_output_good0 && xq_io_sbuffer_0_bits_data==0)) ? 2'd2 : ((transfer_output_good0 && xq_io_sbuffer_0_bits_data==0) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop0 && transfer_known[head0] && transfer_wline[head0])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_04_004_lane1_result : coverpoint ($isunknown((transfer_output_good1 && xq_io_sbuffer_1_bits_data==0)) ? 2'd2 : ((transfer_output_good1 && xq_io_sbuffer_1_bits_data==0) ? 2'd1 : 2'd0))
            iff (sample_enable && (pop1 && transfer_known[head1] && transfer_wline[head1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_07_04_004_CMO_result : coverpoint ($isunknown((special_result && cmo_good && cmo_resp_seen)) ? 2'd2 : ((special_result && cmo_good && cmo_resp_seen) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && cmo_pending)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_07_04 cg_storeunit_diag_07_04_inst;

    // 08.02 S3接纳与普通写回
    covergroup cg_storeunit_08_02 @(posedge clock);
        option.per_instance = 1;
        // 08.02.004 WB_对齐MMIO
        // 场景：对齐MMIO普通标量无异常；无redirect；MAB replay/异常按条目设置；末级R=1；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：普通S2被kill并不接纳S3，最终由SQ完成。
        // 审计结论：完全建模。
        cp_08_02_004_special : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_ex[special_slot]==0 && end_pbmt[special_slot]!=1 && end_op[special_slot] inside {0,1,2,3,16,17,18,19})) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.02.005 WB_CBO无异常
        // 场景：无异常CBO请求；无redirect；MAB replay/异常按条目设置；末级R=1；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：抑制普通S3；zero/非zero由SQ特殊完成来源处理。
        // 审计结论：完全建模。
        cp_08_02_005_special : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_ex[special_slot]==0 && end_op[special_slot] inside {7,12,13,14})) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.02.005 WB_CBO无异常
        // 场景：无异常CBO请求；无redirect；MAB replay/异常按条目设置；末级R=1；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：抑制普通S3；zero/非zero由SQ特殊完成来源处理。
        // 审计结论：完全建模。
        cp_08_02_005_zero : coverpoint ($isunknown((end_zero_good)) ? 2'd2 : ((end_zero_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (zero_due)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.02.008 WB_MAB成功接收parent
        // 场景：合法cross16 parent已进入MAB处理；无redirect；MAB replay/异常按条目设置；末级R=1；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：不在普通STA路径重复写回parent；等待MAB结果。
        // 审计结论：完全建模。
        cp_08_02_008 : coverpoint ($isunknown((end_mab_good)) ? 2'd2 : ((end_mab_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_08_02 cg_storeunit_08_02_inst;
    covergroup cg_storeunit_diag_08_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_08_02_004_special_result : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_ex[special_slot]==0 && end_pbmt[special_slot]!=1 && end_op[special_slot] inside {0,1,2,3,16,17,18,19})) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_02_005_special_result : coverpoint ($isunknown((special_result)) ? 2'd2 : ((special_result) ? 2'd1 : 2'd0))
            iff (sample_enable && (special_due && end_ex[special_slot]==0 && end_op[special_slot] inside {7,12,13,14})) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_02_005_zero_result : coverpoint ($isunknown((end_zero_good)) ? 2'd2 : ((end_zero_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (zero_due)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_02_008_result : coverpoint ($isunknown((end_mab_good)) ? 2'd2 : ((end_mab_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_08_02 cg_storeunit_diag_08_02_inst;

    // 08.04 最终完成来源仲裁
    covergroup cg_storeunit_08_04 @(posedge clock);
        option.per_instance = 1;
        // 08.04.001 COMPLETE_普通STA0优先
        // 场景：普通STA0结果与SQ otherStout同时有效；观察真实producer；特殊完成仅跟踪到最终STA交接；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：普通StoreUnit_0输出优先，SQ结果等待；按实际producer标记。
        // 审计结论：完全建模。
        cp_08_04_001 : coverpoint ($isunknown((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob)))) ? 2'd2 : ((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob))) ? 2'd1 : 2'd0))
            iff (sample_enable && (s0_io_stout_valid && _inner_otherStoutConnect_io_out_valid)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.04.002 COMPLETE_SQ空档写回
        // 场景：STA0普通结果无效而SQ otherStout有效；观察真实producer；特殊完成仅跟踪到最终STA交接；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：STA0选择SQ特殊结果；CBO/MMIO完成不算作普通StoreUnit stout。
        // 审计结论：完全建模。
        cp_08_04_002 : coverpoint ($isunknown((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob)))) ? 2'd2 : ((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob))) ? 2'd1 : 2'd0))
            iff (sample_enable && (!s0_io_stout_valid && _inner_otherStoutConnect_io_out_valid)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.04.003 COMPLETE_MAB空档写回
        // 场景：普通STA0、SQ other和两个vector输出均无效，MAB待写回；观察真实producer；特殊完成仅跟踪到最终STA交接；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：允许MAB parent接入STA0，写回ROB来自parent。
        // 审计结论：完全建模。
        cp_08_04_003 : coverpoint ($isunknown((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob)))) ? 2'd2 : ((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob))) ? 2'd1 : 2'd0))
            iff (sample_enable && (m_io_writeBack_valid && expected_mab_select)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.04.004 COMPLETE_MAB等待SQ
        // 场景：MAB与SQ otherStout同时待完成；观察真实producer；特殊完成仅跟踪到最终STA交接；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：SQ other优先，MAB保持等待，不能丢失parent。
        // 审计结论：完全建模。
        cp_08_04_004 : coverpoint ($isunknown((life_wb_good)) ? 2'd2 : ((life_wb_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (life_wb_due && life_wait_sq)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.04.005 COMPLETE_MAB等待向量
        // 场景：MAB待写回且任一StoreUnit vecstout有效；观察真实producer；特殊完成仅跟踪到最终STA交接；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：MAB最终接入被推迟；向量仅作为标量完成的并发干扰。
        // 审计结论：完全建模。
        cp_08_04_005 : coverpoint ($isunknown((life_wb_good)) ? 2'd2 : ((life_wb_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (life_wb_due && life_wait_vec)) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 08.04.006 COMPLETE_普通STA1独立
        // 场景：STA1普通结果与STA0特殊完成同拍；观察真实producer；特殊完成仅跟踪到最终STA交接；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：两条结果按独立出口和ROB识别，不把STA1计为STA0的特殊producer。
        // 审计结论：完全建模。
        cp_08_04_006 : coverpoint ($isunknown((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob)))) ? 2'd2 : ((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob))) ? 2'd1 : 2'd0))
            iff (sample_enable && (s1_io_stout_valid && (expected_other_select || inner__7))) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_08_04 cg_storeunit_08_04_inst;
    covergroup cg_storeunit_diag_08_04 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_08_04_001_result : coverpoint ($isunknown((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob)))) ? 2'd2 : ((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob))) ? 2'd1 : 2'd0))
            iff (sample_enable && (s0_io_stout_valid && _inner_otherStoutConnect_io_out_valid)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_04_002_result : coverpoint ($isunknown((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob)))) ? 2'd2 : ((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob))) ? 2'd1 : 2'd0))
            iff (sample_enable && (!s0_io_stout_valid && _inner_otherStoutConnect_io_out_valid)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_04_003_result : coverpoint ($isunknown((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob)))) ? 2'd2 : ((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob))) ? 2'd1 : 2'd0))
            iff (sample_enable && (m_io_writeBack_valid && expected_mab_select)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_04_004_result : coverpoint ($isunknown((life_wb_good)) ? 2'd2 : ((life_wb_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (life_wb_due && life_wait_sq)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_04_005_result : coverpoint ($isunknown((life_wb_good)) ? 2'd2 : ((life_wb_good) ? 2'd1 : 2'd0))
            iff (sample_enable && (life_wb_due && life_wait_vec)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_08_04_006_result : coverpoint ($isunknown((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob)))) ? 2'd2 : ((final_mux_ok && (!s1_io_stout_valid || (io_mem_to_ooo_writebackSta_1_valid && {io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_flag,io_mem_to_ooo_writebackSta_1_bits_uop_robIdx_value}==sta1_rob))) ? 2'd1 : 2'd0))
            iff (sample_enable && (s1_io_stout_valid && (expected_other_select || inner__7))) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_08_04 cg_storeunit_diag_08_04_inst;

    // 09.02 各阶段冲刷与恢复
    covergroup cg_storeunit_09_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        // 09.02.006 FLUSH_MABparent与child
        // 场景：MAB保存parent并可能有child在StoreUnit中，redirect覆盖parent；使用明确被取消的token；后续恢复请求用新epoch；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：观察parent/MAB与在途child分别清理；redirect后的新parent使用新epoch，不能消费旧child返回。
        // 审计结论：完全建模。
        cp_09_02_006 : coverpoint ($isunknown((!cancelled_bad && cancelled_payload_ignored)) ? 2'd2 : ((!cancelled_bad && cancelled_payload_ignored) ? 2'd1 : 2'd0))
            iff (sample_enable && (cancel_terminal)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_09_02 cg_storeunit_09_02_inst;
    covergroup cg_storeunit_diag_09_02 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_09_02_006_result : coverpoint ($isunknown((!cancelled_bad && cancelled_payload_ignored)) ? 2'd2 : ((!cancelled_bad && cancelled_payload_ignored) ? 2'd1 : 2'd0))
            iff (sample_enable && (cancel_terminal)) {
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
        // 09.03.003 CONCURRENT_双STA独立翻译
        // 场景：STA0与STA1同拍发送不同VA/SQ，响应不同；复位期间不以未知payload为有效事务；运行期采样在复位解除后；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：两条端口独立关联；不能把ST DTLB共享实例误解为同一个请求token。
        // 审计结论：完全建模。
        cp_09_03_003 : coverpoint ($isunknown((pair_result[0])) ? 2'd2 : ((pair_result[0]) ? 2'd1 : 2'd0))
            iff (sample_enable && (pair_report[0])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.03.004 CONCURRENT_miss与正常并行
        // 场景：STA0本拍miss而STA1本拍hit；复位期间不以未知payload为有效事务；运行期采样在复位解除后；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：反馈、LSQ和写回按对应实例分开，miss不串到另一STA。
        // 审计结论：完全建模。
        cp_09_03_004 : coverpoint ($isunknown((pair_result[1])) ? 2'd2 : ((pair_result[1]) ? 2'd1 : 2'd0))
            iff (sample_enable && (pair_report[1])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.03.005 CONCURRENT_MABchild与另一STA
        // 场景：STA0处理标量MAB child，STA1处理普通Store；复位期间不以未知payload为有效事务；运行期采样在复位解除后；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：child结果走MO，STA1走普通反馈/写回，互不混淆。
        // 审计结论：完全建模。
        cp_09_03_005 : coverpoint ($isunknown((pair_result[2])) ? 2'd2 : ((pair_result[2]) ? 2'd1 : 2'd0))
            iff (sample_enable && (pair_report[2])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.03.006 CONCURRENT_异常与正常并行
        // 场景：两个STA同拍分别异常与正常；复位期间不以未知payload为有效事务；运行期采样在复位解除后；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：异常位与hasException保持实例隔离，正常项不被异常payload污染。
        // 审计结论：完全建模。
        cp_09_03_006 : coverpoint ($isunknown((pair_result[3])) ? 2'd2 : ((pair_result[3]) ? 2'd1 : 2'd0))
            iff (sample_enable && (pair_report[3])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
        // 09.03.007 CONCURRENT_CBO与普通并行
        // 场景：一条STA为CBO，另一条为普通Store；复位期间不以未知payload为有效事务；运行期采样在复位解除后；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：按各自wline/command/完成来源记录，不合并成同一块操作。
        // 审计结论：完全建模。
        cp_09_03_007 : coverpoint ($isunknown((pair_result[4])) ? 2'd2 : ((pair_result[4]) ? 2'd1 : 2'd0))
            iff (sample_enable && (pair_report[4])) {
            bins b_expected_result = {1};
            ignore_bins b_other_results = {0,2};
        }
    endgroup
    cg_storeunit_09_03 cg_storeunit_09_03_inst;
    covergroup cg_storeunit_diag_09_03 @(posedge clock);
        option.per_instance = 1;
        option.weight = 0;
        cp_09_03_003_result : coverpoint ($isunknown((pair_result[0])) ? 2'd2 : ((pair_result[0]) ? 2'd1 : 2'd0))
            iff (sample_enable && (pair_report[0])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_03_004_result : coverpoint ($isunknown((pair_result[1])) ? 2'd2 : ((pair_result[1]) ? 2'd1 : 2'd0))
            iff (sample_enable && (pair_report[1])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_03_005_result : coverpoint ($isunknown((pair_result[2])) ? 2'd2 : ((pair_result[2]) ? 2'd1 : 2'd0))
            iff (sample_enable && (pair_report[2])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_03_006_result : coverpoint ($isunknown((pair_result[3])) ? 2'd2 : ((pair_result[3]) ? 2'd1 : 2'd0))
            iff (sample_enable && (pair_report[3])) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_03_007_result : coverpoint ($isunknown((pair_result[4])) ? 2'd2 : ((pair_result[4]) ? 2'd1 : 2'd0))
            iff (sample_enable && (pair_report[4])) {
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
        // 09.04.006 DIAG_MABparent_trigger传播
        // 场景：无trigger命中的cacheable跨16B parent进入MAB并最终写回；诊断项单独统计；不将异常输入或已知偏离设为必须正常命中的目标；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：普通S1已算出None=15，但MAB入队复制s1_in.trigger；当前标量初值可为0，最终parent可能保留0而breakPoint位为0。
        // 审计结论：完全建模。
        cp_09_04_006 : coverpoint ($isunknown((m_io_writeBack_bits_uop_trigger==life_s1_trigger && life_rob==parent_rob)) ? 2'd2 : ((m_io_writeBack_bits_uop_trigger==life_s1_trigger && life_rob==parent_rob) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected && life_s1_trigger==15)) {
            option.weight = 0;
            bins b_relation_true = {1};
            bins b_relation_false = {0};
            bins b_unknown = {2};
        }
        // 09.04.009 DIAG_结果串线
        // 场景：相邻请求或重试attempt出现不匹配身份；诊断项单独统计；不将异常输入或已知偏离设为必须正常命中的目标；未显式改变项采用字段页D0默认条件；运行期条目在复位解除且所属token有效的对应阶段采样（复位/空闲/诊断条目按自身事件）
        // 目标：按相符/偏离/未知/参考缺失分类；不能用身份相等条件屏蔽串线样本。
        // 审计结论：完全建模。
        cp_09_04_009 : coverpoint ($isunknown(((!end_normal_due0 || end_normal_good0) && (!end_normal_due1 || end_normal_good1) && (!special_due || special_result) && (!mab_finish_expected || end_mab_good) && (!zero_due || end_zero_good))) ? 2'd2 : (((!end_normal_due0 || end_normal_good0) && (!end_normal_due1 || end_normal_good1) && (!special_due || special_result) && (!mab_finish_expected || end_mab_good) && (!zero_due || end_zero_good)) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 || end_normal_due1 || special_due || mab_finish_expected || zero_due)) {
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
        cp_09_04_006_result : coverpoint ($isunknown((m_io_writeBack_bits_uop_trigger==life_s1_trigger && life_rob==parent_rob)) ? 2'd2 : ((m_io_writeBack_bits_uop_trigger==life_s1_trigger && life_rob==parent_rob) ? 2'd1 : 2'd0))
            iff (sample_enable && (mab_finish_expected && life_s1_trigger==15)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
        cp_09_04_009_result : coverpoint ($isunknown(((!end_normal_due0 || end_normal_good0) && (!end_normal_due1 || end_normal_good1) && (!special_due || special_result) && (!mab_finish_expected || end_mab_good) && (!zero_due || end_zero_good))) ? 2'd2 : (((!end_normal_due0 || end_normal_good0) && (!end_normal_due1 || end_normal_good1) && (!special_due || special_result) && (!mab_finish_expected || end_mab_good) && (!zero_due || end_zero_good)) ? 2'd1 : 2'd0))
            iff (sample_enable && (end_normal_due0 || end_normal_due1 || special_due || mab_finish_expected || zero_due)) {
            option.weight = 0;
            bins b_deviation = {0};
            bins b_consistent = {1};
            bins b_unknown = {2};
        }
    endgroup
    cg_storeunit_diag_09_04 cg_storeunit_diag_09_04_inst;
    initial begin
        cg_storeunit_02_04_inst = new();
        cg_storeunit_diag_02_04_inst = new();
        cg_storeunit_04_01_inst = new();
        cg_storeunit_diag_04_01_inst = new();
        cg_storeunit_04_05_inst = new();
        cg_storeunit_diag_04_05_inst = new();
        cg_storeunit_05_02_inst = new();
        cg_storeunit_diag_05_02_inst = new();
        cg_storeunit_05_03_inst = new();
        cg_storeunit_diag_05_03_inst = new();
        cg_storeunit_05_04_inst = new();
        cg_storeunit_diag_05_04_inst = new();
        cg_storeunit_05_05_inst = new();
        cg_storeunit_diag_05_05_inst = new();
        cg_storeunit_06_04_inst = new();
        cg_storeunit_diag_06_04_inst = new();
        cg_storeunit_07_02_inst = new();
        cg_storeunit_diag_07_02_inst = new();
        cg_storeunit_07_03_inst = new();
        cg_storeunit_diag_07_03_inst = new();
        cg_storeunit_07_04_inst = new();
        cg_storeunit_diag_07_04_inst = new();
        cg_storeunit_08_02_inst = new();
        cg_storeunit_diag_08_02_inst = new();
        cg_storeunit_08_04_inst = new();
        cg_storeunit_diag_08_04_inst = new();
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
