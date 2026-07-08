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
    `endif

`endif
