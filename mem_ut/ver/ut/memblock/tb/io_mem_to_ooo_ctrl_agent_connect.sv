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
        force U_IF_NAME.io_mem_to_ooo_sqDeqPtr_flag = RTL_PATH.io_mem_to_ooo_sqDeqPtr_flag; \
        force U_IF_NAME.io_mem_to_ooo_sqDeqPtr_value = RTL_PATH.io_mem_to_ooo_sqDeqPtr_value; \
        force U_IF_NAME.io_mem_to_ooo_lqDeqPtr_flag = RTL_PATH.io_mem_to_ooo_lqDeqPtr_flag; \
        force U_IF_NAME.io_mem_to_ooo_lqDeqPtr_value = RTL_PATH.io_mem_to_ooo_lqDeqPtr_value; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_0_valid = RTL_PATH.io_mem_to_ooo_updateLFST_0_valid; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_0_bits_robIdx_flag = RTL_PATH.io_mem_to_ooo_updateLFST_0_bits_robIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_0_bits_robIdx_value = RTL_PATH.io_mem_to_ooo_updateLFST_0_bits_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_0_bits_ssid = RTL_PATH.io_mem_to_ooo_updateLFST_0_bits_ssid; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_0_bits_storeSetHit = RTL_PATH.io_mem_to_ooo_updateLFST_0_bits_storeSetHit; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_1_valid = RTL_PATH.io_mem_to_ooo_updateLFST_1_valid; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_1_bits_robIdx_flag = RTL_PATH.io_mem_to_ooo_updateLFST_1_bits_robIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_1_bits_robIdx_value = RTL_PATH.io_mem_to_ooo_updateLFST_1_bits_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_1_bits_ssid = RTL_PATH.io_mem_to_ooo_updateLFST_1_bits_ssid; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_1_bits_storeSetHit = RTL_PATH.io_mem_to_ooo_updateLFST_1_bits_storeSetHit; \
        force U_IF_NAME.io_mem_to_ooo_stIssuePtr_flag = RTL_PATH.io_mem_to_ooo_stIssuePtr_flag; \
        force U_IF_NAME.io_mem_to_ooo_stIssuePtr_value = RTL_PATH.io_mem_to_ooo_stIssuePtr_value; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_valid = RTL_PATH.io_mem_to_ooo_memoryViolation_valid; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_ftqOffset = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_ftqOffset; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_isRVC = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_isRVC; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_target = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_target; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_level = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_level; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_robIdx_flag = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_robIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_robIdx_value = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_stFtqOffset = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_stFtqOffset; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_stIsRVC = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_stIsRVC; \
        force U_IF_NAME.io_mem_to_ooo_sbIsEmpty = RTL_PATH.io_mem_to_ooo_sbIsEmpty; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_valid = RTL_PATH.io_mem_to_ooo_mdpTrain_valid; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_ftqIdx_value = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_ftqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_ftqOffset = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_ftqOffset; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_isRVC = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_isRVC; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_target = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_target; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_level = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_level; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_robIdx_flag = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_robIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_robIdx_value = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_stFtqOffset = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_stFtqOffset; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_stIsRVC = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_stIsRVC; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_vaddr = RTL_PATH.io_mem_to_ooo_lsqio_vaddr; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_vstart = RTL_PATH.io_mem_to_ooo_lsqio_vstart; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_vl = RTL_PATH.io_mem_to_ooo_lsqio_vl; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_gpaddr = RTL_PATH.io_mem_to_ooo_lsqio_gpaddr; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_isForVSnonLeafPTE = RTL_PATH.io_mem_to_ooo_lsqio_isForVSnonLeafPTE; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_mmioBusy = RTL_PATH.io_mem_to_ooo_lsqio_mmioBusy; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_lqCanAccept = RTL_PATH.io_mem_to_ooo_lsqio_lqCanAccept; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_sqCanAccept = RTL_PATH.io_mem_to_ooo_lsqio_sqCanAccept; \
        force U_IF_NAME.io_mem_to_ooo_ldCancel_0_ld2Cancel = RTL_PATH.io_mem_to_ooo_ldCancel_0_ld2Cancel; \
        force U_IF_NAME.io_mem_to_ooo_ldCancel_1_ld2Cancel = RTL_PATH.io_mem_to_ooo_ldCancel_1_ld2Cancel; \
        force U_IF_NAME.io_mem_to_ooo_ldCancel_2_ld2Cancel = RTL_PATH.io_mem_to_ooo_ldCancel_2_ld2Cancel; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_hartId = RTL_PATH.io_mem_to_ooo_topToBackendBypass_hartId; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip; \
        force U_IF_NAME.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip = RTL_PATH.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip; \
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
        force U_IF_NAME.io_mem_to_ooo_sqDeqPtr_flag = RTL_PATH.io_mem_to_ooo_sqDeqPtr_flag; \
        force U_IF_NAME.io_mem_to_ooo_sqDeqPtr_value = RTL_PATH.io_mem_to_ooo_sqDeqPtr_value; \
        force U_IF_NAME.io_mem_to_ooo_lqDeqPtr_flag = RTL_PATH.io_mem_to_ooo_lqDeqPtr_flag; \
        force U_IF_NAME.io_mem_to_ooo_lqDeqPtr_value = RTL_PATH.io_mem_to_ooo_lqDeqPtr_value; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_0_valid = RTL_PATH.io_mem_to_ooo_updateLFST_0_valid; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_0_bits_robIdx_flag = RTL_PATH.io_mem_to_ooo_updateLFST_0_bits_robIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_0_bits_robIdx_value = RTL_PATH.io_mem_to_ooo_updateLFST_0_bits_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_0_bits_ssid = RTL_PATH.io_mem_to_ooo_updateLFST_0_bits_ssid; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_0_bits_storeSetHit = RTL_PATH.io_mem_to_ooo_updateLFST_0_bits_storeSetHit; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_1_valid = RTL_PATH.io_mem_to_ooo_updateLFST_1_valid; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_1_bits_robIdx_flag = RTL_PATH.io_mem_to_ooo_updateLFST_1_bits_robIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_1_bits_robIdx_value = RTL_PATH.io_mem_to_ooo_updateLFST_1_bits_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_1_bits_ssid = RTL_PATH.io_mem_to_ooo_updateLFST_1_bits_ssid; \
        force U_IF_NAME.io_mem_to_ooo_updateLFST_1_bits_storeSetHit = RTL_PATH.io_mem_to_ooo_updateLFST_1_bits_storeSetHit; \
        force U_IF_NAME.io_mem_to_ooo_stIssuePtr_flag = RTL_PATH.io_mem_to_ooo_stIssuePtr_flag; \
        force U_IF_NAME.io_mem_to_ooo_stIssuePtr_value = RTL_PATH.io_mem_to_ooo_stIssuePtr_value; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_valid = RTL_PATH.io_mem_to_ooo_memoryViolation_valid; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_ftqOffset = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_ftqOffset; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_isRVC = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_isRVC; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_target = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_target; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_level = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_level; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_robIdx_flag = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_robIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_robIdx_value = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_stFtqOffset = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_stFtqOffset; \
        force U_IF_NAME.io_mem_to_ooo_memoryViolation_bits_stIsRVC = RTL_PATH.io_mem_to_ooo_memoryViolation_bits_stIsRVC; \
        force U_IF_NAME.io_mem_to_ooo_sbIsEmpty = RTL_PATH.io_mem_to_ooo_sbIsEmpty; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_valid = RTL_PATH.io_mem_to_ooo_mdpTrain_valid; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_ftqIdx_value = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_ftqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_ftqOffset = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_ftqOffset; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_isRVC = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_isRVC; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_target = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_target; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_level = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_level; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_robIdx_flag = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_robIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_robIdx_value = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_robIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_stFtqOffset = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_stFtqOffset; \
        force U_IF_NAME.io_mem_to_ooo_mdpTrain_bits_stIsRVC = RTL_PATH.io_mem_to_ooo_mdpTrain_bits_stIsRVC; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en; \
        force U_IF_NAME.io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss = RTL_PATH.io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_vaddr = RTL_PATH.io_mem_to_ooo_lsqio_vaddr; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_vstart = RTL_PATH.io_mem_to_ooo_lsqio_vstart; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_vl = RTL_PATH.io_mem_to_ooo_lsqio_vl; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_gpaddr = RTL_PATH.io_mem_to_ooo_lsqio_gpaddr; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_isForVSnonLeafPTE = RTL_PATH.io_mem_to_ooo_lsqio_isForVSnonLeafPTE; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_mmioBusy = RTL_PATH.io_mem_to_ooo_lsqio_mmioBusy; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_lqCanAccept = RTL_PATH.io_mem_to_ooo_lsqio_lqCanAccept; \
        force U_IF_NAME.io_mem_to_ooo_lsqio_sqCanAccept = RTL_PATH.io_mem_to_ooo_lsqio_sqCanAccept; \
        force U_IF_NAME.io_mem_to_ooo_ldCancel_0_ld2Cancel = RTL_PATH.io_mem_to_ooo_ldCancel_0_ld2Cancel; \
        force U_IF_NAME.io_mem_to_ooo_ldCancel_1_ld2Cancel = RTL_PATH.io_mem_to_ooo_ldCancel_1_ld2Cancel; \
        force U_IF_NAME.io_mem_to_ooo_ldCancel_2_ld2Cancel = RTL_PATH.io_mem_to_ooo_ldCancel_2_ld2Cancel; \
    end \
    `endif

`endif
