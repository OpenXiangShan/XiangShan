//=========================================================
//File name    : io_mem_to_ooo_ctrl_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_ctrl_agent_connect
//Discribution : io_mem_to_ooo_ctrl_agent_connect : io_mem_to_ooo_ctrl_agent Interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_CTRL_AGENT_CONNECT__SV
`define IO_MEM_TO_OOO_CTRL_AGENT_CONNECT__SV

`define MEMBLOCK__IO_MEM_TO_OOO_CTRL_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    io_mem_to_ooo_ctrl_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    initial begin \
        uvm_config_db#(virtual io_mem_to_ooo_ctrl_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    `ifdef MEMBLOCK_UT \
    initial begin \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_hartId = RTL_PATH.io_mem_to_ooo_topToBackendBypass_hartId; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31 = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43 = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_msiInfo_valid = RTL_PATH.io_mem_to_ooo_topToBackendBypass_msiInfo_valid; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_msiInfo_bits = RTL_PATH.io_mem_to_ooo_topToBackendBypass_msiInfo_bits; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_clintTime_valid = RTL_PATH.io_mem_to_ooo_topToBackendBypass_clintTime_valid; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_clintTime_bits = RTL_PATH.io_mem_to_ooo_topToBackendBypass_clintTime_bits; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_l2FlushDone = RTL_PATH.io_mem_to_ooo_topToBackendBypass_l2FlushDone; \
        force U_IF_NAME.io_mem_to_ooo_lqCancelCnt = RTL_PATH.io_mem_to_ooo_lqCancelCnt; \
        force U_IF_NAME.io_mem_to_ooo_sqCancelCnt = RTL_PATH.io_mem_to_ooo_sqCancelCnt; \
        force U_IF_NAME.io_mem_to_ooo_sqDeq = RTL_PATH.io_mem_to_ooo_sqDeq; \
        force U_IF_NAME.io_mem_to_ooo_lqDeq = RTL_PATH.io_mem_to_ooo_lqDeq; \
        force U_IF_NAME.io_mem_to_ooo_lqDeqPtr_flag = RTL_PATH.io_mem_to_ooo_lqDeqPtr_flag; \
        force U_IF_NAME.io_mem_to_ooo_lqDeqPtr_value = RTL_PATH.io_mem_to_ooo_lqDeqPtr_value; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_valid = RTL_PATH.io_mem_to_ooo_memoryViolation_valid; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_ftqOffset = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_ftqOffset; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_isRVC = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_isRVC; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_level = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_level; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_robIdx_flag = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_robIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_robIdx_value = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_sbIsEmpty = RTL_PATH.io_mem_to_ooo_sbIsEmpty; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_vaddr = RTL_PATH.io_mem_to_ooo_lsqio_vaddr; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_gpaddr = RTL_PATH.io_mem_to_ooo_lsqio_gpaddr; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_isForVSnonLeafPTE = RTL_PATH.io_mem_to_ooo_lsqio_isForVSnonLeafPTE; \
        force U_IF_NAME.io_mem_to_ooo_ldCancel_0_ld2Cancel = RTL_PATH.io_mem_to_ooo_ldCancel_0_ld2Cancel; \
        force U_IF_NAME.io_mem_to_ooo_ldCancel_1_ld2Cancel = RTL_PATH.io_mem_to_ooo_ldCancel_1_ld2Cancel; \
        force U_IF_NAME.io_mem_to_ooo_ldCancel_2_ld2Cancel = RTL_PATH.io_mem_to_ooo_ldCancel_2_ld2Cancel; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value = RTL_PATH.io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value = RTL_PATH.io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value = RTL_PATH.io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_loadMmio_0 = RTL_PATH.io_mem_to_ooo_lsqio_loadMmio_0; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_loadMmio_1 = RTL_PATH.io_mem_to_ooo_lsqio_loadMmio_1; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_loadMmio_2 = RTL_PATH.io_mem_to_ooo_lsqio_loadMmio_2; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_storeMmio = RTL_PATH.io_mem_to_ooo_lsqio_storeMmio; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value = RTL_PATH.io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_hartId = RTL_PATH.io_mem_to_ooo_topToBackendBypass_hartId; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_debug; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31 = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43 = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_msiInfo_valid = RTL_PATH.io_mem_to_ooo_topToBackendBypass_msiInfo_valid; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_msiInfo_bits = RTL_PATH.io_mem_to_ooo_topToBackendBypass_msiInfo_bits; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_clintTime_valid = RTL_PATH.io_mem_to_ooo_topToBackendBypass_clintTime_valid; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_clintTime_bits = RTL_PATH.io_mem_to_ooo_topToBackendBypass_clintTime_bits; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_l2FlushDone = RTL_PATH.io_mem_to_ooo_topToBackendBypass_l2FlushDone; \
        force U_IF_NAME.io_mem_to_ooo_lqCancelCnt = RTL_PATH.io_mem_to_ooo_lqCancelCnt; \
        force U_IF_NAME.io_mem_to_ooo_sqCancelCnt = RTL_PATH.io_mem_to_ooo_sqCancelCnt; \
        force U_IF_NAME.io_mem_to_ooo_sqDeq = RTL_PATH.io_mem_to_ooo_sqDeq; \
        force U_IF_NAME.io_mem_to_ooo_lqDeq = RTL_PATH.io_mem_to_ooo_lqDeq; \
        force U_IF_NAME.io_mem_to_ooo_lqDeqPtr_flag = RTL_PATH.io_mem_to_ooo_lqDeqPtr_flag; \
        force U_IF_NAME.io_mem_to_ooo_lqDeqPtr_value = RTL_PATH.io_mem_to_ooo_lqDeqPtr_value; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_valid = RTL_PATH.io_mem_to_ooo_memoryViolation_valid; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_ftqOffset = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_ftqOffset; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_isRVC = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_isRVC; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_level = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_level; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_robIdx_flag = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_robIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_robIdx_value = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_sbIsEmpty = RTL_PATH.io_mem_to_ooo_sbIsEmpty; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_vaddr = RTL_PATH.io_mem_to_ooo_lsqio_vaddr; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_gpaddr = RTL_PATH.io_mem_to_ooo_lsqio_gpaddr; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_isForVSnonLeafPTE = RTL_PATH.io_mem_to_ooo_lsqio_isForVSnonLeafPTE; \
        force U_IF_NAME.io_mem_to_ooo_ldCancel_0_ld2Cancel = RTL_PATH.io_mem_to_ooo_ldCancel_0_ld2Cancel; \
        force U_IF_NAME.io_mem_to_ooo_ldCancel_1_ld2Cancel = RTL_PATH.io_mem_to_ooo_ldCancel_1_ld2Cancel; \
        force U_IF_NAME.io_mem_to_ooo_ldCancel_2_ld2Cancel = RTL_PATH.io_mem_to_ooo_ldCancel_2_ld2Cancel; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value = RTL_PATH.io_mem_to_ooo_lsqio_loadMmioUop_0_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value = RTL_PATH.io_mem_to_ooo_lsqio_loadMmioUop_1_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value = RTL_PATH.io_mem_to_ooo_lsqio_loadMmioUop_2_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_loadMmio_0 = RTL_PATH.io_mem_to_ooo_lsqio_loadMmio_0; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_loadMmio_1 = RTL_PATH.io_mem_to_ooo_lsqio_loadMmio_1; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_loadMmio_2 = RTL_PATH.io_mem_to_ooo_lsqio_loadMmio_2; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_storeMmio = RTL_PATH.io_mem_to_ooo_lsqio_storeMmio; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value = RTL_PATH.io_mem_to_ooo_lsqio_storeMmioUop_robIdx_value; \
    end \
    `endif \
    initial begin \
        force U_IF_NAME.rollback_winner_vec[0] = RTL_PATH.inner__oldestOneHot_resultOnehot_T_15; \
        force U_IF_NAME.rollback_winner_vec[1] = RTL_PATH.inner__oldestOneHot_resultOnehot_T_31; \
        force U_IF_NAME.rollback_winner_vec[2] = RTL_PATH.inner__oldestOneHot_resultOnehot_T_46; \
        force U_IF_NAME.rollback_winner_vec[3] = RTL_PATH.inner__oldestOneHot_resultOnehot_T_60; \
        force U_IF_NAME.rollback_winner_vec[4] = RTL_PATH.inner__oldestOneHot_resultOnehot_T_73; \
        force U_IF_NAME.rollback_winner_vec[5] = RTL_PATH.inner__oldestOneHot_resultOnehot_T_85; \
        force U_IF_NAME.rollback_candidate_valid[0] = RTL_PATH._inner_LoadUnit_0_io_rollback_valid; \
        force U_IF_NAME.rollback_candidate_valid[1] = RTL_PATH._inner_LoadUnit_1_io_rollback_valid; \
        force U_IF_NAME.rollback_candidate_valid[2] = RTL_PATH._inner_LoadUnit_2_io_rollback_valid; \
        force U_IF_NAME.rollback_candidate_valid[3] = RTL_PATH._inner_lsq_io_nack_rollback_0_valid; \
        force U_IF_NAME.rollback_candidate_valid[4] = RTL_PATH._inner_lsq_io_nuke_rollback_0_valid; \
        force U_IF_NAME.rollback_candidate_valid[5] = RTL_PATH._inner_lsq_io_nuke_rollback_1_valid; \
        force U_IF_NAME.rollback_candidate_rob_flag[0] = RTL_PATH._inner_LoadUnit_0_io_rollback_bits_robIdx_flag; \
        force U_IF_NAME.rollback_candidate_rob_flag[1] = RTL_PATH._inner_LoadUnit_1_io_rollback_bits_robIdx_flag; \
        force U_IF_NAME.rollback_candidate_rob_flag[2] = RTL_PATH._inner_LoadUnit_2_io_rollback_bits_robIdx_flag; \
        force U_IF_NAME.rollback_candidate_rob_flag[3] = RTL_PATH._inner_lsq_io_nack_rollback_0_bits_robIdx_flag; \
        force U_IF_NAME.rollback_candidate_rob_flag[4] = RTL_PATH._inner_lsq_io_nuke_rollback_0_bits_robIdx_flag; \
        force U_IF_NAME.rollback_candidate_rob_flag[5] = RTL_PATH._inner_lsq_io_nuke_rollback_1_bits_robIdx_flag; \
        force U_IF_NAME.rollback_candidate_rob_value[0] = RTL_PATH._inner_LoadUnit_0_io_rollback_bits_robIdx_value; \
        force U_IF_NAME.rollback_candidate_rob_value[1] = RTL_PATH._inner_LoadUnit_1_io_rollback_bits_robIdx_value; \
        force U_IF_NAME.rollback_candidate_rob_value[2] = RTL_PATH._inner_LoadUnit_2_io_rollback_bits_robIdx_value; \
        force U_IF_NAME.rollback_candidate_rob_value[3] = RTL_PATH._inner_lsq_io_nack_rollback_0_bits_robIdx_value; \
        force U_IF_NAME.rollback_candidate_rob_value[4] = RTL_PATH._inner_lsq_io_nuke_rollback_0_bits_robIdx_value; \
        force U_IF_NAME.rollback_candidate_rob_value[5] = RTL_PATH._inner_lsq_io_nuke_rollback_1_bits_robIdx_value; \
        force U_IF_NAME.rollback_candidate_level[0] = RTL_PATH._inner_LoadUnit_0_io_rollback_bits_level; \
        force U_IF_NAME.rollback_candidate_level[1] = RTL_PATH._inner_LoadUnit_1_io_rollback_bits_level; \
        force U_IF_NAME.rollback_candidate_level[2] = RTL_PATH._inner_LoadUnit_2_io_rollback_bits_level; \
        force U_IF_NAME.rollback_candidate_level[3] = RTL_PATH._inner_lsq_io_nack_rollback_0_bits_level; \
        force U_IF_NAME.rollback_candidate_level[4] = 1'b1; \
        force U_IF_NAME.rollback_candidate_level[5] = 1'b1; \
        force U_IF_NAME.rollback_lu_vp_match_fail[0] = RTL_PATH.inner_LoadUnit_0.s3_vp_match_fail; \
        force U_IF_NAME.rollback_lu_vp_match_fail[1] = RTL_PATH.inner_LoadUnit_1.s3_vp_match_fail; \
        force U_IF_NAME.rollback_lu_vp_match_fail[2] = RTL_PATH.inner_LoadUnit_2.s3_vp_match_fail; \
        force U_IF_NAME.rollback_lu_ldld_resp_valid[0] = RTL_PATH.inner_LoadUnit_0.io_lsq_ldld_nuke_query_resp_valid; \
        force U_IF_NAME.rollback_lu_ldld_resp_valid[1] = RTL_PATH.inner_LoadUnit_1.io_lsq_ldld_nuke_query_resp_valid; \
        force U_IF_NAME.rollback_lu_ldld_resp_valid[2] = RTL_PATH.inner_LoadUnit_2.io_lsq_ldld_nuke_query_resp_valid; \
        force U_IF_NAME.rollback_lu_rep_frm_fetch[0] = RTL_PATH.inner_LoadUnit_0.io_lsq_ldld_nuke_query_resp_bits_rep_frm_fetch; \
        force U_IF_NAME.rollback_lu_rep_frm_fetch[1] = RTL_PATH.inner_LoadUnit_1.io_lsq_ldld_nuke_query_resp_bits_rep_frm_fetch; \
        force U_IF_NAME.rollback_lu_rep_frm_fetch[2] = RTL_PATH.inner_LoadUnit_2.io_lsq_ldld_nuke_query_resp_bits_rep_frm_fetch; \
        force U_IF_NAME.rollback_lu_ldld_rep_inst[0] = RTL_PATH.inner_LoadUnit_0.s3_ldld_rep_inst_last_REG; \
        force U_IF_NAME.rollback_lu_ldld_rep_inst[1] = RTL_PATH.inner_LoadUnit_1.s3_ldld_rep_inst_last_REG; \
        force U_IF_NAME.rollback_lu_ldld_rep_inst[2] = RTL_PATH.inner_LoadUnit_2.s3_ldld_rep_inst_last_REG; \
        force U_IF_NAME.rollback_lu_frm_mis_flush[0] = RTL_PATH.inner_LoadUnit_0.s3_frm_mis_flush; \
        force U_IF_NAME.rollback_lu_frm_mis_flush[1] = RTL_PATH.inner_LoadUnit_1.s3_frm_mis_flush; \
        force U_IF_NAME.rollback_lu_frm_mis_flush[2] = RTL_PATH.inner_LoadUnit_2.s3_frm_mis_flush; \
        force U_IF_NAME.rollback_uncache_can_allocate[0] = RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._freeList_io_canAllocate_0; \
        force U_IF_NAME.rollback_uncache_can_allocate[1] = RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._freeList_io_canAllocate_1; \
        force U_IF_NAME.rollback_uncache_can_allocate[2] = RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._freeList_io_canAllocate_2; \
        force U_IF_NAME.rollback_raw_store_valid[0] = RTL_PATH.inner_lsq.loadQueue.loadQueueRAW.io_storeIn_0_valid; \
        force U_IF_NAME.rollback_raw_store_valid[1] = RTL_PATH.inner_lsq.loadQueue.loadQueueRAW.io_storeIn_1_valid; \
        force U_IF_NAME.rollback_raw_store_rob_flag[0] = RTL_PATH.inner_lsq.loadQueue.loadQueueRAW.io_storeIn_0_bits_uop_robIdx_flag; \
        force U_IF_NAME.rollback_raw_store_rob_flag[1] = RTL_PATH.inner_lsq.loadQueue.loadQueueRAW.io_storeIn_1_bits_uop_robIdx_flag; \
        force U_IF_NAME.rollback_raw_store_rob_value[0] = RTL_PATH.inner_lsq.loadQueue.loadQueueRAW.io_storeIn_0_bits_uop_robIdx_value; \
        force U_IF_NAME.rollback_raw_store_rob_value[1] = RTL_PATH.inner_lsq.loadQueue.loadQueueRAW.io_storeIn_1_bits_uop_robIdx_value; \
        force U_IF_NAME.rollback_raw_store_paddr[0] = RTL_PATH.inner_lsq.loadQueue.loadQueueRAW.io_storeIn_0_bits_paddr; \
        force U_IF_NAME.rollback_raw_store_paddr[1] = RTL_PATH.inner_lsq.loadQueue.loadQueueRAW.io_storeIn_1_bits_paddr; \
        force U_IF_NAME.rollback_raw_store_mask[0] = RTL_PATH.inner_lsq.loadQueue.loadQueueRAW.io_storeIn_0_bits_mask; \
        force U_IF_NAME.rollback_raw_store_mask[1] = RTL_PATH.inner_lsq.loadQueue.loadQueueRAW.io_storeIn_1_bits_mask; \
        force U_IF_NAME.uncache_lq_req_valid = { \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_15.req_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_14.req_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_13.req_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_12.req_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_11.req_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_10.req_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_9.req_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_8.req_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_7.req_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_6.req_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_5.req_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_4.req_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_3.req_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_2.req_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_1.req_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_0.req_valid}; \
        force U_IF_NAME.uncache_lq_state = { \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_15.uncacheState, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_14.uncacheState, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_13.uncacheState, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_12.uncacheState, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_11.uncacheState, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_10.uncacheState, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_9.uncacheState, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_8.uncacheState, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_7.uncacheState, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_6.uncacheState, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_5.uncacheState, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_4.uncacheState, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_3.uncacheState, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_2.uncacheState, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_1.uncacheState, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_0.uncacheState}; \
        force U_IF_NAME.uncache_lq_rob_flag = { \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_15.req_uop_robIdx_flag, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_14.req_uop_robIdx_flag, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_13.req_uop_robIdx_flag, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_12.req_uop_robIdx_flag, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_11.req_uop_robIdx_flag, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_10.req_uop_robIdx_flag, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_9.req_uop_robIdx_flag, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_8.req_uop_robIdx_flag, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_7.req_uop_robIdx_flag, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_6.req_uop_robIdx_flag, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_5.req_uop_robIdx_flag, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_4.req_uop_robIdx_flag, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_3.req_uop_robIdx_flag, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_2.req_uop_robIdx_flag, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_1.req_uop_robIdx_flag, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_0.req_uop_robIdx_flag}; \
        force U_IF_NAME.uncache_lq_rob_value = { \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_15.req_uop_robIdx_value, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_14.req_uop_robIdx_value, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_13.req_uop_robIdx_value, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_12.req_uop_robIdx_value, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_11.req_uop_robIdx_value, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_10.req_uop_robIdx_value, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_9.req_uop_robIdx_value, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_8.req_uop_robIdx_value, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_7.req_uop_robIdx_value, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_6.req_uop_robIdx_value, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_5.req_uop_robIdx_value, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_4.req_uop_robIdx_value, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_3.req_uop_robIdx_value, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_2.req_uop_robIdx_value, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_1.req_uop_robIdx_value, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_0.req_uop_robIdx_value}; \
        force U_IF_NAME.uncache_lq_slave_valid = { \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_15.slaveAccept, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_14.slaveAccept, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_13.slaveAccept, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_12.slaveAccept, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_11.slaveAccept, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_10.slaveAccept, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_9.slaveAccept, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_8.slaveAccept, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_7.slaveAccept, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_6.slaveAccept, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_5.slaveAccept, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_4.slaveAccept, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_3.slaveAccept, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_2.slaveAccept, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_1.slaveAccept, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_0.slaveAccept}; \
        force U_IF_NAME.uncache_lq_slave_id = { \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_15.slaveId, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_14.slaveId, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_13.slaveId, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_12.slaveId, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_11.slaveId, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_10.slaveId, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_9.slaveId, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_8.slaveId, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_7.slaveId, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_6.slaveId, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_5.slaveId, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_4.slaveId, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_3.slaveId, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_2.slaveId, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_1.slaveId, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_0.slaveId}; \
        force U_IF_NAME.uncache_lq_need_flush = { \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_15.needFlush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_14.needFlush, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_13.needFlush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_12.needFlush, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_11.needFlush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_10.needFlush, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_9.needFlush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_8.needFlush, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_7.needFlush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_6.needFlush, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_5.needFlush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_4.needFlush, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_3.needFlush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_2.needFlush, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_1.needFlush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_0.needFlush}; \
        force U_IF_NAME.uncache_lq_need_flush_reg = { \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_15.needFlushReg, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_14.needFlushReg, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_13.needFlushReg, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_12.needFlushReg, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_11.needFlushReg, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_10.needFlushReg, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_9.needFlushReg, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_8.needFlushReg, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_7.needFlushReg, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_6.needFlushReg, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_5.needFlushReg, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_4.needFlushReg, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_3.needFlushReg, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_2.needFlushReg, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_1.needFlushReg, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.entries_0.needFlushReg}; \
        force U_IF_NAME.uncache_lq_flush = { \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_15_io_flush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_14_io_flush, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_13_io_flush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_12_io_flush, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_11_io_flush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_10_io_flush, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_9_io_flush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_8_io_flush, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_7_io_flush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_6_io_flush, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_5_io_flush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_4_io_flush, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_3_io_flush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_2_io_flush, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_1_io_flush, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_0_io_flush}; \
        force U_IF_NAME.uncache_lq_nc_out_valid = { \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_15_io_ncOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_14_io_ncOut_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_13_io_ncOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_12_io_ncOut_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_11_io_ncOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_10_io_ncOut_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_9_io_ncOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_8_io_ncOut_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_7_io_ncOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_6_io_ncOut_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_5_io_ncOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_4_io_ncOut_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_3_io_ncOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_2_io_ncOut_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_1_io_ncOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_0_io_ncOut_valid}; \
        force U_IF_NAME.uncache_lq_mmio_out_valid = { \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_15_io_mmioOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_14_io_mmioOut_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_13_io_mmioOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_12_io_mmioOut_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_11_io_mmioOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_10_io_mmioOut_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_9_io_mmioOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_8_io_mmioOut_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_7_io_mmioOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_6_io_mmioOut_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_5_io_mmioOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_4_io_mmioOut_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_3_io_mmioOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_2_io_mmioOut_valid, \
            RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_1_io_mmioOut_valid, RTL_PATH.inner_lsq.loadQueue.uncacheBuffer._entries_0_io_mmioOut_valid}; \
        force U_IF_NAME.uncache_lq_free_mask = RTL_PATH.inner_lsq.loadQueue.uncacheBuffer.freeList.freeMask; \
        force U_IF_NAME.uncache_top_valid = {RTL_PATH.inner_uncache.states_15_valid, RTL_PATH.inner_uncache.states_14_valid, RTL_PATH.inner_uncache.states_13_valid, RTL_PATH.inner_uncache.states_12_valid, RTL_PATH.inner_uncache.states_11_valid, RTL_PATH.inner_uncache.states_10_valid, RTL_PATH.inner_uncache.states_9_valid, RTL_PATH.inner_uncache.states_8_valid, RTL_PATH.inner_uncache.states_7_valid, RTL_PATH.inner_uncache.states_6_valid, RTL_PATH.inner_uncache.states_5_valid, RTL_PATH.inner_uncache.states_4_valid, RTL_PATH.inner_uncache.states_3_valid, RTL_PATH.inner_uncache.states_2_valid, RTL_PATH.inner_uncache.states_1_valid, RTL_PATH.inner_uncache.states_0_valid}; \
        force U_IF_NAME.uncache_top_inflight = {RTL_PATH.inner_uncache.states_15_inflight, RTL_PATH.inner_uncache.states_14_inflight, RTL_PATH.inner_uncache.states_13_inflight, RTL_PATH.inner_uncache.states_12_inflight, RTL_PATH.inner_uncache.states_11_inflight, RTL_PATH.inner_uncache.states_10_inflight, RTL_PATH.inner_uncache.states_9_inflight, RTL_PATH.inner_uncache.states_8_inflight, RTL_PATH.inner_uncache.states_7_inflight, RTL_PATH.inner_uncache.states_6_inflight, RTL_PATH.inner_uncache.states_5_inflight, RTL_PATH.inner_uncache.states_4_inflight, RTL_PATH.inner_uncache.states_3_inflight, RTL_PATH.inner_uncache.states_2_inflight, RTL_PATH.inner_uncache.states_1_inflight, RTL_PATH.inner_uncache.states_0_inflight}; \
        force U_IF_NAME.uncache_top_wait_same = {RTL_PATH.inner_uncache.states_15_waitSame, RTL_PATH.inner_uncache.states_14_waitSame, RTL_PATH.inner_uncache.states_13_waitSame, RTL_PATH.inner_uncache.states_12_waitSame, RTL_PATH.inner_uncache.states_11_waitSame, RTL_PATH.inner_uncache.states_10_waitSame, RTL_PATH.inner_uncache.states_9_waitSame, RTL_PATH.inner_uncache.states_8_waitSame, RTL_PATH.inner_uncache.states_7_waitSame, RTL_PATH.inner_uncache.states_6_waitSame, RTL_PATH.inner_uncache.states_5_waitSame, RTL_PATH.inner_uncache.states_4_waitSame, RTL_PATH.inner_uncache.states_3_waitSame, RTL_PATH.inner_uncache.states_2_waitSame, RTL_PATH.inner_uncache.states_1_waitSame, RTL_PATH.inner_uncache.states_0_waitSame}; \
        force U_IF_NAME.uncache_top_wait_return = {RTL_PATH.inner_uncache.states_15_waitReturn, RTL_PATH.inner_uncache.states_14_waitReturn, RTL_PATH.inner_uncache.states_13_waitReturn, RTL_PATH.inner_uncache.states_12_waitReturn, RTL_PATH.inner_uncache.states_11_waitReturn, RTL_PATH.inner_uncache.states_10_waitReturn, RTL_PATH.inner_uncache.states_9_waitReturn, RTL_PATH.inner_uncache.states_8_waitReturn, RTL_PATH.inner_uncache.states_7_waitReturn, RTL_PATH.inner_uncache.states_6_waitReturn, RTL_PATH.inner_uncache.states_5_waitReturn, RTL_PATH.inner_uncache.states_4_waitReturn, RTL_PATH.inner_uncache.states_3_waitReturn, RTL_PATH.inner_uncache.states_2_waitReturn, RTL_PATH.inner_uncache.states_1_waitReturn, RTL_PATH.inner_uncache.states_0_waitReturn}; \
        force U_IF_NAME.uncache_top_lsq_id_resp_valid = RTL_PATH.inner_uncache.io_lsq_idResp_valid; \
        force U_IF_NAME.uncache_top_lsq_id_resp_mid = RTL_PATH.inner_uncache.io_lsq_idResp_bits_mid; \
        force U_IF_NAME.uncache_top_lsq_id_resp_sid = RTL_PATH.inner_uncache.io_lsq_idResp_bits_sid; \
        force U_IF_NAME.uncache_top_lsq_resp_valid = RTL_PATH.inner_uncache.io_lsq_resp_valid; \
        force U_IF_NAME.uncache_top_lsq_resp_ready = RTL_PATH.inner_uncache.io_lsq_resp_ready; \
        force U_IF_NAME.uncache_top_lsq_resp_id = RTL_PATH.inner_uncache.io_lsq_resp_bits_id; \
        force U_IF_NAME.uncache_top_a_valid = RTL_PATH.inner_uncache.auto_client_out_a_valid; \
        force U_IF_NAME.uncache_top_a_ready = RTL_PATH.inner_uncache.auto_client_out_a_ready; \
        force U_IF_NAME.uncache_top_a_source = RTL_PATH.inner_uncache.auto_client_out_a_bits_source; \
        force U_IF_NAME.uncache_top_d_valid = RTL_PATH.inner_uncache.auto_client_out_d_valid; \
        force U_IF_NAME.uncache_top_d_source = RTL_PATH.inner_uncache.auto_client_out_d_bits_source; \
    end

`endif
