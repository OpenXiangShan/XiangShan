//=========================================================
//File name    : io_mem_to_ooo_ctrl_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_ctrl_agent_agent_xaction
//Discribution : io_mem_to_ooo_ctrl_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_CTRL_AGENT_AGENT_XACTION__SV
`define IO_MEM_TO_OOO_CTRL_AGENT_AGENT_XACTION__SV

class io_mem_to_ooo_ctrl_agent_agent_xaction  extends tcnt_data_base;
    // Primarily a DUT-output monitor transaction. The following base
    // constraints only bound structural ranges and are not intended as
    // random stimulus guidance.
    rand bit [5:0] io_mem_to_ooo_topToBackendBypass_hartId;
    rand bit io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip;
    rand bit io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip;
    rand bit io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip;
    rand bit io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip;
    rand bit io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31;
    rand bit io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43;
    rand bit io_mem_to_ooo_topToBackendBypass_msiInfo_valid;
    rand bit [12:0] io_mem_to_ooo_topToBackendBypass_msiInfo_bits;
    rand bit io_mem_to_ooo_topToBackendBypass_clintTime_valid;
    rand bit [63:0] io_mem_to_ooo_topToBackendBypass_clintTime_bits;
    rand bit io_mem_to_ooo_topToBackendBypass_l2FlushDone;
    rand bit [6:0] io_mem_to_ooo_lqCancelCnt;
    rand bit [5:0] io_mem_to_ooo_sqCancelCnt;
    rand bit [1:0] io_mem_to_ooo_sqDeq ;
    rand bit [3:0] io_mem_to_ooo_lqDeq ;
    rand bit io_mem_to_ooo_sqDeqPtr_flag;
    rand bit [5:0] io_mem_to_ooo_sqDeqPtr_value;
    rand bit io_mem_to_ooo_lqDeqPtr_flag;
    rand bit [6:0] io_mem_to_ooo_lqDeqPtr_value;
    rand bit io_mem_to_ooo_updateLFST_0_valid;
    rand bit io_mem_to_ooo_updateLFST_0_bits_robIdx_flag;
    rand bit [8:0] io_mem_to_ooo_updateLFST_0_bits_robIdx_value;
    rand bit [4:0] io_mem_to_ooo_updateLFST_0_bits_ssid;
    rand bit io_mem_to_ooo_updateLFST_0_bits_storeSetHit;
    rand bit io_mem_to_ooo_updateLFST_1_valid;
    rand bit io_mem_to_ooo_updateLFST_1_bits_robIdx_flag;
    rand bit [8:0] io_mem_to_ooo_updateLFST_1_bits_robIdx_value;
    rand bit [4:0] io_mem_to_ooo_updateLFST_1_bits_ssid;
    rand bit io_mem_to_ooo_updateLFST_1_bits_storeSetHit;
    rand bit io_mem_to_ooo_stIssuePtr_flag;
    rand bit [5:0] io_mem_to_ooo_stIssuePtr_value;
    rand bit io_mem_to_ooo_memoryViolation_valid;
    rand bit io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag;
    rand bit [5:0] io_mem_to_ooo_memoryViolation_bits_ftqIdx_value;
    rand bit [4:0] io_mem_to_ooo_memoryViolation_bits_ftqOffset;
    rand bit io_mem_to_ooo_memoryViolation_bits_isRVC;
    rand bit [49:0] io_mem_to_ooo_memoryViolation_bits_target;
    rand bit io_mem_to_ooo_memoryViolation_bits_level;
    rand bit io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
    rand bit [8:0] io_mem_to_ooo_memoryViolation_bits_robIdx_value;
    rand bit io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag;
    rand bit [5:0] io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value;
    rand bit [4:0] io_mem_to_ooo_memoryViolation_bits_stFtqOffset;
    rand bit io_mem_to_ooo_memoryViolation_bits_stIsRVC;
    rand bit io_mem_to_ooo_sbIsEmpty   ;
    rand bit io_mem_to_ooo_mdpTrain_valid;
    rand bit io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag;
    rand bit [5:0] io_mem_to_ooo_mdpTrain_bits_ftqIdx_value;
    rand bit [4:0] io_mem_to_ooo_mdpTrain_bits_ftqOffset;
    rand bit io_mem_to_ooo_mdpTrain_bits_isRVC;
    rand bit [49:0] io_mem_to_ooo_mdpTrain_bits_target;
    rand bit io_mem_to_ooo_mdpTrain_bits_level;
    rand bit io_mem_to_ooo_mdpTrain_bits_robIdx_flag;
    rand bit [8:0] io_mem_to_ooo_mdpTrain_bits_robIdx_value;
    rand bit io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag;
    rand bit [5:0] io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value;
    rand bit [4:0] io_mem_to_ooo_mdpTrain_bits_stFtqOffset;
    rand bit io_mem_to_ooo_mdpTrain_bits_stIsRVC;
    rand bit [8:0] io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx;
    rand bit io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid;
    rand bit [49:0] io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits;
    rand bit [8:0] io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx;
    rand bit io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid;
    rand bit [47:0] io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits;
    rand bit io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en;
    rand bit io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss;
    rand bit [8:0] io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx;
    rand bit io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid;
    rand bit [49:0] io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits;
    rand bit [8:0] io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx;
    rand bit io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid;
    rand bit [47:0] io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits;
    rand bit io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en;
    rand bit io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss;
    rand bit [8:0] io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx;
    rand bit io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid;
    rand bit [49:0] io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits;
    rand bit [8:0] io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx;
    rand bit io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid;
    rand bit [47:0] io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits;
    rand bit io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en;
    rand bit io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss;
    rand bit [63:0] io_mem_to_ooo_lsqio_vaddr;
    rand bit [7:0] io_mem_to_ooo_lsqio_vstart;
    rand bit [7:0] io_mem_to_ooo_lsqio_vl;
    rand bit [63:0] io_mem_to_ooo_lsqio_gpaddr;
    rand bit io_mem_to_ooo_lsqio_isForVSnonLeafPTE;
    rand bit io_mem_to_ooo_lsqio_mmioBusy;
    rand bit io_mem_to_ooo_lsqio_lqCanAccept;
    rand bit io_mem_to_ooo_lsqio_sqCanAccept;
    rand bit io_mem_to_ooo_ldCancel_0_ld2Cancel;
    rand bit io_mem_to_ooo_ldCancel_1_ld2Cancel;
    rand bit io_mem_to_ooo_ldCancel_2_ld2Cancel;

    extern constraint default_io_mem_to_ooo_topToBackendBypass_hartId_cons;
    extern constraint default_io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip_cons;
    extern constraint default_io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip_cons;
    extern constraint default_io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip_cons;
    extern constraint default_io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip_cons;
    extern constraint default_io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31_cons;
    extern constraint default_io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43_cons;
    extern constraint default_io_mem_to_ooo_topToBackendBypass_msiInfo_valid_cons;
    extern constraint default_io_mem_to_ooo_topToBackendBypass_msiInfo_bits_cons;
    extern constraint default_io_mem_to_ooo_topToBackendBypass_clintTime_valid_cons;
    extern constraint default_io_mem_to_ooo_topToBackendBypass_clintTime_bits_cons;
    extern constraint default_io_mem_to_ooo_topToBackendBypass_l2FlushDone_cons;
    extern constraint default_io_mem_to_ooo_lqCancelCnt_cons;
    extern constraint default_io_mem_to_ooo_sqCancelCnt_cons;
    extern constraint default_io_mem_to_ooo_sqDeq_cons;
    extern constraint default_io_mem_to_ooo_lqDeq_cons;
    extern constraint default_io_mem_to_ooo_sqDeqPtr_flag_cons;
    extern constraint default_io_mem_to_ooo_sqDeqPtr_value_cons;
    extern constraint default_io_mem_to_ooo_lqDeqPtr_flag_cons;
    extern constraint default_io_mem_to_ooo_lqDeqPtr_value_cons;
    extern constraint default_io_mem_to_ooo_updateLFST_0_valid_cons;
    extern constraint default_io_mem_to_ooo_updateLFST_0_bits_robIdx_flag_cons;
    extern constraint default_io_mem_to_ooo_updateLFST_0_bits_robIdx_value_cons;
    extern constraint default_io_mem_to_ooo_updateLFST_0_bits_ssid_cons;
    extern constraint default_io_mem_to_ooo_updateLFST_0_bits_storeSetHit_cons;
    extern constraint default_io_mem_to_ooo_updateLFST_1_valid_cons;
    extern constraint default_io_mem_to_ooo_updateLFST_1_bits_robIdx_flag_cons;
    extern constraint default_io_mem_to_ooo_updateLFST_1_bits_robIdx_value_cons;
    extern constraint default_io_mem_to_ooo_updateLFST_1_bits_ssid_cons;
    extern constraint default_io_mem_to_ooo_updateLFST_1_bits_storeSetHit_cons;
    extern constraint default_io_mem_to_ooo_stIssuePtr_flag_cons;
    extern constraint default_io_mem_to_ooo_stIssuePtr_value_cons;
    extern constraint default_io_mem_to_ooo_memoryViolation_valid_cons;
    extern constraint default_io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag_cons;
    extern constraint default_io_mem_to_ooo_memoryViolation_bits_ftqIdx_value_cons;
    extern constraint default_io_mem_to_ooo_memoryViolation_bits_ftqOffset_cons;
    extern constraint default_io_mem_to_ooo_memoryViolation_bits_isRVC_cons;
    extern constraint default_io_mem_to_ooo_memoryViolation_bits_target_cons;
    extern constraint default_io_mem_to_ooo_memoryViolation_bits_level_cons;
    extern constraint default_io_mem_to_ooo_memoryViolation_bits_robIdx_flag_cons;
    extern constraint default_io_mem_to_ooo_memoryViolation_bits_robIdx_value_cons;
    extern constraint default_io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag_cons;
    extern constraint default_io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value_cons;
    extern constraint default_io_mem_to_ooo_memoryViolation_bits_stFtqOffset_cons;
    extern constraint default_io_mem_to_ooo_memoryViolation_bits_stIsRVC_cons;
    extern constraint default_io_mem_to_ooo_sbIsEmpty_cons;
    extern constraint default_io_mem_to_ooo_mdpTrain_valid_cons;
    extern constraint default_io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag_cons;
    extern constraint default_io_mem_to_ooo_mdpTrain_bits_ftqIdx_value_cons;
    extern constraint default_io_mem_to_ooo_mdpTrain_bits_ftqOffset_cons;
    extern constraint default_io_mem_to_ooo_mdpTrain_bits_isRVC_cons;
    extern constraint default_io_mem_to_ooo_mdpTrain_bits_target_cons;
    extern constraint default_io_mem_to_ooo_mdpTrain_bits_level_cons;
    extern constraint default_io_mem_to_ooo_mdpTrain_bits_robIdx_flag_cons;
    extern constraint default_io_mem_to_ooo_mdpTrain_bits_robIdx_value_cons;
    extern constraint default_io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag_cons;
    extern constraint default_io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value_cons;
    extern constraint default_io_mem_to_ooo_mdpTrain_bits_stFtqOffset_cons;
    extern constraint default_io_mem_to_ooo_mdpTrain_bits_stIsRVC_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en_cons;
    extern constraint default_io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss_cons;
    extern constraint default_io_mem_to_ooo_lsqio_vaddr_cons;
    extern constraint default_io_mem_to_ooo_lsqio_vstart_cons;
    extern constraint default_io_mem_to_ooo_lsqio_vl_cons;
    extern constraint default_io_mem_to_ooo_lsqio_gpaddr_cons;
    extern constraint default_io_mem_to_ooo_lsqio_isForVSnonLeafPTE_cons;
    extern constraint default_io_mem_to_ooo_lsqio_mmioBusy_cons;
    extern constraint default_io_mem_to_ooo_lsqio_lqCanAccept_cons;
    extern constraint default_io_mem_to_ooo_lsqio_sqCanAccept_cons;
    extern constraint default_io_mem_to_ooo_ldCancel_0_ld2Cancel_cons;
    extern constraint default_io_mem_to_ooo_ldCancel_1_ld2Cancel_cons;
    extern constraint default_io_mem_to_ooo_ldCancel_2_ld2Cancel_cons;

    extern function new(string name="io_mem_to_ooo_ctrl_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(io_mem_to_ooo_ctrl_agent_agent_xaction)
        `uvm_field_int(io_mem_to_ooo_topToBackendBypass_hartId, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_topToBackendBypass_msiInfo_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_topToBackendBypass_msiInfo_bits, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_topToBackendBypass_clintTime_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_topToBackendBypass_clintTime_bits, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_topToBackendBypass_l2FlushDone, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lqCancelCnt, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_sqCancelCnt, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_sqDeq, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lqDeq, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_sqDeqPtr_flag, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_sqDeqPtr_value, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lqDeqPtr_flag, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lqDeqPtr_value, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_updateLFST_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_updateLFST_0_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_updateLFST_0_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_updateLFST_0_bits_ssid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_updateLFST_0_bits_storeSetHit, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_updateLFST_1_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_updateLFST_1_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_updateLFST_1_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_updateLFST_1_bits_ssid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_updateLFST_1_bits_storeSetHit, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_stIssuePtr_flag, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_stIssuePtr_value, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_memoryViolation_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_memoryViolation_bits_ftqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_memoryViolation_bits_ftqOffset, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_memoryViolation_bits_isRVC, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_memoryViolation_bits_target, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_memoryViolation_bits_level, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_memoryViolation_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_memoryViolation_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_memoryViolation_bits_stFtqOffset, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_memoryViolation_bits_stIsRVC, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_sbIsEmpty, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_mdpTrain_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_mdpTrain_bits_ftqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_mdpTrain_bits_ftqOffset, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_mdpTrain_bits_isRVC, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_mdpTrain_bits_target, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_mdpTrain_bits_level, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_mdpTrain_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_mdpTrain_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_mdpTrain_bits_stFtqOffset, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_mdpTrain_bits_stIsRVC, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsqio_vaddr, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsqio_vstart, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsqio_vl, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsqio_gpaddr, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsqio_isForVSnonLeafPTE, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsqio_mmioBusy, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsqio_lqCanAccept, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_lsqio_sqCanAccept, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_ldCancel_0_ld2Cancel, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_ldCancel_1_ld2Cancel, UVM_ALL_ON);
        `uvm_field_int(io_mem_to_ooo_ldCancel_2_ld2Cancel, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:io_mem_to_ooo_ctrl_agent_agent_xaction

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_topToBackendBypass_hartId_cons{
    io_mem_to_ooo_topToBackendBypass_hartId inside {[6'd0:6'd63]};
}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_topToBackendBypass_msiInfo_valid_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_topToBackendBypass_msiInfo_bits_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_topToBackendBypass_clintTime_valid_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_topToBackendBypass_clintTime_bits_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_topToBackendBypass_l2FlushDone_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lqCancelCnt_cons{
    io_mem_to_ooo_lqCancelCnt inside {[7'd0:7'd72]};
}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_sqCancelCnt_cons{
    io_mem_to_ooo_sqCancelCnt inside {[6'd0:6'd56]};
}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_sqDeq_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lqDeq_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_sqDeqPtr_flag_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_sqDeqPtr_value_cons{
    io_mem_to_ooo_sqDeqPtr_value inside {[6'd0:6'd55]};
}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lqDeqPtr_flag_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lqDeqPtr_value_cons{
    io_mem_to_ooo_lqDeqPtr_value inside {[7'd0:7'd71]};
}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_updateLFST_0_valid_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_updateLFST_0_bits_robIdx_flag_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_updateLFST_0_bits_robIdx_value_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_updateLFST_0_bits_ssid_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_updateLFST_0_bits_storeSetHit_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_updateLFST_1_valid_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_updateLFST_1_bits_robIdx_flag_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_updateLFST_1_bits_robIdx_value_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_updateLFST_1_bits_ssid_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_updateLFST_1_bits_storeSetHit_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_stIssuePtr_flag_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_stIssuePtr_value_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_memoryViolation_valid_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_memoryViolation_bits_ftqIdx_value_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_memoryViolation_bits_ftqOffset_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_memoryViolation_bits_isRVC_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_memoryViolation_bits_target_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_memoryViolation_bits_level_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_memoryViolation_bits_robIdx_flag_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_memoryViolation_bits_robIdx_value_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_memoryViolation_bits_stFtqOffset_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_memoryViolation_bits_stIsRVC_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_sbIsEmpty_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_mdpTrain_valid_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_mdpTrain_bits_ftqIdx_value_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_mdpTrain_bits_ftqOffset_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_mdpTrain_bits_isRVC_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_mdpTrain_bits_target_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_mdpTrain_bits_level_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_mdpTrain_bits_robIdx_flag_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_mdpTrain_bits_robIdx_value_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_mdpTrain_bits_stFtqOffset_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_mdpTrain_bits_stIsRVC_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsqio_vaddr_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsqio_vstart_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsqio_vl_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsqio_gpaddr_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsqio_isForVSnonLeafPTE_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsqio_mmioBusy_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsqio_lqCanAccept_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_lsqio_sqCanAccept_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_ldCancel_0_ld2Cancel_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_ldCancel_1_ld2Cancel_cons{

}

constraint io_mem_to_ooo_ctrl_agent_agent_xaction::default_io_mem_to_ooo_ldCancel_2_ld2Cancel_cons{

}

function io_mem_to_ooo_ctrl_agent_agent_xaction::new(string name = "io_mem_to_ooo_ctrl_agent_agent_xaction");
    super.new();
endfunction:new

function void io_mem_to_ooo_ctrl_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void io_mem_to_ooo_ctrl_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void io_mem_to_ooo_ctrl_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void io_mem_to_ooo_ctrl_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string io_mem_to_ooo_ctrl_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sio_mem_to_ooo_topToBackendBypass_hartId = 0x%0h ",pkt_str,this.io_mem_to_ooo_topToBackendBypass_hartId);
    pkt_str = $sformatf("%sio_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip = 0x%0h ",pkt_str,this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip);
    pkt_str = $sformatf("%sio_mem_to_ooo_topToBackendBypass_externalInterrupt_msip = 0x%0h ",pkt_str,this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip);
    pkt_str = $sformatf("%sio_mem_to_ooo_topToBackendBypass_externalInterrupt_meip = 0x%0h ",pkt_str,this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip);
    pkt_str = $sformatf("%sio_mem_to_ooo_topToBackendBypass_externalInterrupt_seip = 0x%0h ",pkt_str,this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip);
    pkt_str = $sformatf("%sio_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31 = 0x%0h ",pkt_str,this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31);
    pkt_str = $sformatf("%sio_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43 = 0x%0h ",pkt_str,this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43);
    pkt_str = $sformatf("%sio_mem_to_ooo_topToBackendBypass_msiInfo_valid = 0x%0h ",pkt_str,this.io_mem_to_ooo_topToBackendBypass_msiInfo_valid);
    pkt_str = $sformatf("%sio_mem_to_ooo_topToBackendBypass_msiInfo_bits = 0x%0h ",pkt_str,this.io_mem_to_ooo_topToBackendBypass_msiInfo_bits);
    pkt_str = $sformatf("%sio_mem_to_ooo_topToBackendBypass_clintTime_valid = 0x%0h ",pkt_str,this.io_mem_to_ooo_topToBackendBypass_clintTime_valid);
    pkt_str = $sformatf("%sio_mem_to_ooo_topToBackendBypass_clintTime_bits = 0x%0h ",pkt_str,this.io_mem_to_ooo_topToBackendBypass_clintTime_bits);
    pkt_str = $sformatf("%sio_mem_to_ooo_topToBackendBypass_l2FlushDone = 0x%0h ",pkt_str,this.io_mem_to_ooo_topToBackendBypass_l2FlushDone);
    pkt_str = $sformatf("%sio_mem_to_ooo_lqCancelCnt = 0x%0h ",pkt_str,this.io_mem_to_ooo_lqCancelCnt);
    pkt_str = $sformatf("%sio_mem_to_ooo_sqCancelCnt = 0x%0h ",pkt_str,this.io_mem_to_ooo_sqCancelCnt);
    pkt_str = $sformatf("%sio_mem_to_ooo_sqDeq = 0x%0h ",pkt_str,this.io_mem_to_ooo_sqDeq);
    pkt_str = $sformatf("%sio_mem_to_ooo_lqDeq = 0x%0h ",pkt_str,this.io_mem_to_ooo_lqDeq);
    pkt_str = $sformatf("%sio_mem_to_ooo_sqDeqPtr_flag = 0x%0h ",pkt_str,this.io_mem_to_ooo_sqDeqPtr_flag);
    pkt_str = $sformatf("%sio_mem_to_ooo_sqDeqPtr_value = 0x%0h ",pkt_str,this.io_mem_to_ooo_sqDeqPtr_value);
    pkt_str = $sformatf("%sio_mem_to_ooo_lqDeqPtr_flag = 0x%0h ",pkt_str,this.io_mem_to_ooo_lqDeqPtr_flag);
    pkt_str = $sformatf("%sio_mem_to_ooo_lqDeqPtr_value = 0x%0h ",pkt_str,this.io_mem_to_ooo_lqDeqPtr_value);
    pkt_str = $sformatf("%sio_mem_to_ooo_updateLFST_0_valid = 0x%0h ",pkt_str,this.io_mem_to_ooo_updateLFST_0_valid);
    pkt_str = $sformatf("%sio_mem_to_ooo_updateLFST_0_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_mem_to_ooo_updateLFST_0_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_mem_to_ooo_updateLFST_0_bits_robIdx_value = 0x%0h ",pkt_str,this.io_mem_to_ooo_updateLFST_0_bits_robIdx_value);
    pkt_str = $sformatf("%sio_mem_to_ooo_updateLFST_0_bits_ssid = 0x%0h ",pkt_str,this.io_mem_to_ooo_updateLFST_0_bits_ssid);
    pkt_str = $sformatf("%sio_mem_to_ooo_updateLFST_0_bits_storeSetHit = 0x%0h ",pkt_str,this.io_mem_to_ooo_updateLFST_0_bits_storeSetHit);
    pkt_str = $sformatf("%sio_mem_to_ooo_updateLFST_1_valid = 0x%0h ",pkt_str,this.io_mem_to_ooo_updateLFST_1_valid);
    pkt_str = $sformatf("%sio_mem_to_ooo_updateLFST_1_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_mem_to_ooo_updateLFST_1_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_mem_to_ooo_updateLFST_1_bits_robIdx_value = 0x%0h ",pkt_str,this.io_mem_to_ooo_updateLFST_1_bits_robIdx_value);
    pkt_str = $sformatf("%sio_mem_to_ooo_updateLFST_1_bits_ssid = 0x%0h ",pkt_str,this.io_mem_to_ooo_updateLFST_1_bits_ssid);
    pkt_str = $sformatf("%sio_mem_to_ooo_updateLFST_1_bits_storeSetHit = 0x%0h ",pkt_str,this.io_mem_to_ooo_updateLFST_1_bits_storeSetHit);
    pkt_str = $sformatf("%sio_mem_to_ooo_stIssuePtr_flag = 0x%0h ",pkt_str,this.io_mem_to_ooo_stIssuePtr_flag);
    pkt_str = $sformatf("%sio_mem_to_ooo_stIssuePtr_value = 0x%0h ",pkt_str,this.io_mem_to_ooo_stIssuePtr_value);
    pkt_str = $sformatf("%sio_mem_to_ooo_memoryViolation_valid = 0x%0h ",pkt_str,this.io_mem_to_ooo_memoryViolation_valid);
    pkt_str = $sformatf("%sio_mem_to_ooo_memoryViolation_bits_ftqIdx_flag = 0x%0h ",pkt_str,this.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag);
    pkt_str = $sformatf("%sio_mem_to_ooo_memoryViolation_bits_ftqIdx_value = 0x%0h ",pkt_str,this.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value);
    pkt_str = $sformatf("%sio_mem_to_ooo_memoryViolation_bits_ftqOffset = 0x%0h ",pkt_str,this.io_mem_to_ooo_memoryViolation_bits_ftqOffset);
    pkt_str = $sformatf("%sio_mem_to_ooo_memoryViolation_bits_isRVC = 0x%0h ",pkt_str,this.io_mem_to_ooo_memoryViolation_bits_isRVC);
    pkt_str = $sformatf("%sio_mem_to_ooo_memoryViolation_bits_target = 0x%0h ",pkt_str,this.io_mem_to_ooo_memoryViolation_bits_target);
    pkt_str = $sformatf("%sio_mem_to_ooo_memoryViolation_bits_level = 0x%0h ",pkt_str,this.io_mem_to_ooo_memoryViolation_bits_level);
    pkt_str = $sformatf("%sio_mem_to_ooo_memoryViolation_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_mem_to_ooo_memoryViolation_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_mem_to_ooo_memoryViolation_bits_robIdx_value = 0x%0h ",pkt_str,this.io_mem_to_ooo_memoryViolation_bits_robIdx_value);
    pkt_str = $sformatf("%sio_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag = 0x%0h ",pkt_str,this.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag);
    pkt_str = $sformatf("%sio_mem_to_ooo_memoryViolation_bits_stFtqIdx_value = 0x%0h ",pkt_str,this.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value);
    pkt_str = $sformatf("%sio_mem_to_ooo_memoryViolation_bits_stFtqOffset = 0x%0h ",pkt_str,this.io_mem_to_ooo_memoryViolation_bits_stFtqOffset);
    pkt_str = $sformatf("%sio_mem_to_ooo_memoryViolation_bits_stIsRVC = 0x%0h ",pkt_str,this.io_mem_to_ooo_memoryViolation_bits_stIsRVC);
    pkt_str = $sformatf("%sio_mem_to_ooo_sbIsEmpty = 0x%0h ",pkt_str,this.io_mem_to_ooo_sbIsEmpty);
    pkt_str = $sformatf("%sio_mem_to_ooo_mdpTrain_valid = 0x%0h ",pkt_str,this.io_mem_to_ooo_mdpTrain_valid);
    pkt_str = $sformatf("%sio_mem_to_ooo_mdpTrain_bits_ftqIdx_flag = 0x%0h ",pkt_str,this.io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag);
    pkt_str = $sformatf("%sio_mem_to_ooo_mdpTrain_bits_ftqIdx_value = 0x%0h ",pkt_str,this.io_mem_to_ooo_mdpTrain_bits_ftqIdx_value);
    pkt_str = $sformatf("%sio_mem_to_ooo_mdpTrain_bits_ftqOffset = 0x%0h ",pkt_str,this.io_mem_to_ooo_mdpTrain_bits_ftqOffset);
    pkt_str = $sformatf("%sio_mem_to_ooo_mdpTrain_bits_isRVC = 0x%0h ",pkt_str,this.io_mem_to_ooo_mdpTrain_bits_isRVC);
    pkt_str = $sformatf("%sio_mem_to_ooo_mdpTrain_bits_target = 0x%0h ",pkt_str,this.io_mem_to_ooo_mdpTrain_bits_target);
    pkt_str = $sformatf("%sio_mem_to_ooo_mdpTrain_bits_level = 0x%0h ",pkt_str,this.io_mem_to_ooo_mdpTrain_bits_level);
    pkt_str = $sformatf("%sio_mem_to_ooo_mdpTrain_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_mem_to_ooo_mdpTrain_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_mem_to_ooo_mdpTrain_bits_robIdx_value = 0x%0h ",pkt_str,this.io_mem_to_ooo_mdpTrain_bits_robIdx_value);
    pkt_str = $sformatf("%sio_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag = 0x%0h ",pkt_str,this.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag);
    pkt_str = $sformatf("%sio_mem_to_ooo_mdpTrain_bits_stFtqIdx_value = 0x%0h ",pkt_str,this.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value);
    pkt_str = $sformatf("%sio_mem_to_ooo_mdpTrain_bits_stFtqOffset = 0x%0h ",pkt_str,this.io_mem_to_ooo_mdpTrain_bits_stFtqOffset);
    pkt_str = $sformatf("%sio_mem_to_ooo_mdpTrain_bits_stIsRVC = 0x%0h ",pkt_str,this.io_mem_to_ooo_mdpTrain_bits_stIsRVC);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_0_s1_robIdx = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_0_s2_robIdx = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_1_s1_robIdx = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_1_s2_robIdx = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_2_s1_robIdx = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_2_s2_robIdx = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsqio_vaddr = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsqio_vaddr);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsqio_vstart = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsqio_vstart);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsqio_vl = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsqio_vl);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsqio_gpaddr = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsqio_gpaddr);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsqio_isForVSnonLeafPTE = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsqio_isForVSnonLeafPTE);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsqio_mmioBusy = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsqio_mmioBusy);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsqio_lqCanAccept = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsqio_lqCanAccept);
    pkt_str = $sformatf("%sio_mem_to_ooo_lsqio_sqCanAccept = 0x%0h ",pkt_str,this.io_mem_to_ooo_lsqio_sqCanAccept);
    pkt_str = $sformatf("%sio_mem_to_ooo_ldCancel_0_ld2Cancel = 0x%0h ",pkt_str,this.io_mem_to_ooo_ldCancel_0_ld2Cancel);
    pkt_str = $sformatf("%sio_mem_to_ooo_ldCancel_1_ld2Cancel = 0x%0h ",pkt_str,this.io_mem_to_ooo_ldCancel_1_ld2Cancel);
    pkt_str = $sformatf("%sio_mem_to_ooo_ldCancel_2_ld2Cancel = 0x%0h ",pkt_str,this.io_mem_to_ooo_ldCancel_2_ld2Cancel);

    return pkt_str;
endfunction:psdisplay

function bit io_mem_to_ooo_ctrl_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    io_mem_to_ooo_ctrl_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a io_mem_to_ooo_ctrl_agent_agent_xaction or its extend"))
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

        if(this.io_mem_to_ooo_topToBackendBypass_hartId!=rhs_.io_mem_to_ooo_topToBackendBypass_hartId) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_topToBackendBypass_hartId=0x%0h while the rhs_.io_mem_to_ooo_topToBackendBypass_hartId=0x%0h",this.io_mem_to_ooo_topToBackendBypass_hartId,rhs_.io_mem_to_ooo_topToBackendBypass_hartId),UVM_NONE)
        end

        if(this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip!=rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip=0x%0h while the rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip=0x%0h",this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip,rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip),UVM_NONE)
        end

        if(this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip!=rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip=0x%0h while the rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip=0x%0h",this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip,rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip),UVM_NONE)
        end

        if(this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip!=rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip=0x%0h while the rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip=0x%0h",this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip,rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip),UVM_NONE)
        end

        if(this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip!=rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip=0x%0h while the rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip=0x%0h",this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip,rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip),UVM_NONE)
        end

        if(this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31!=rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31=0x%0h while the rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31=0x%0h",this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31,rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31),UVM_NONE)
        end

        if(this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43!=rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43=0x%0h while the rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43=0x%0h",this.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43,rhs_.io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43),UVM_NONE)
        end

        if(this.io_mem_to_ooo_topToBackendBypass_msiInfo_valid!=rhs_.io_mem_to_ooo_topToBackendBypass_msiInfo_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_topToBackendBypass_msiInfo_valid=0x%0h while the rhs_.io_mem_to_ooo_topToBackendBypass_msiInfo_valid=0x%0h",this.io_mem_to_ooo_topToBackendBypass_msiInfo_valid,rhs_.io_mem_to_ooo_topToBackendBypass_msiInfo_valid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_topToBackendBypass_msiInfo_bits!=rhs_.io_mem_to_ooo_topToBackendBypass_msiInfo_bits) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_topToBackendBypass_msiInfo_bits=0x%0h while the rhs_.io_mem_to_ooo_topToBackendBypass_msiInfo_bits=0x%0h",this.io_mem_to_ooo_topToBackendBypass_msiInfo_bits,rhs_.io_mem_to_ooo_topToBackendBypass_msiInfo_bits),UVM_NONE)
        end

        if(this.io_mem_to_ooo_topToBackendBypass_clintTime_valid!=rhs_.io_mem_to_ooo_topToBackendBypass_clintTime_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_topToBackendBypass_clintTime_valid=0x%0h while the rhs_.io_mem_to_ooo_topToBackendBypass_clintTime_valid=0x%0h",this.io_mem_to_ooo_topToBackendBypass_clintTime_valid,rhs_.io_mem_to_ooo_topToBackendBypass_clintTime_valid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_topToBackendBypass_clintTime_bits!=rhs_.io_mem_to_ooo_topToBackendBypass_clintTime_bits) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_topToBackendBypass_clintTime_bits=0x%0h while the rhs_.io_mem_to_ooo_topToBackendBypass_clintTime_bits=0x%0h",this.io_mem_to_ooo_topToBackendBypass_clintTime_bits,rhs_.io_mem_to_ooo_topToBackendBypass_clintTime_bits),UVM_NONE)
        end

        if(this.io_mem_to_ooo_topToBackendBypass_l2FlushDone!=rhs_.io_mem_to_ooo_topToBackendBypass_l2FlushDone) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_topToBackendBypass_l2FlushDone=0x%0h while the rhs_.io_mem_to_ooo_topToBackendBypass_l2FlushDone=0x%0h",this.io_mem_to_ooo_topToBackendBypass_l2FlushDone,rhs_.io_mem_to_ooo_topToBackendBypass_l2FlushDone),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lqCancelCnt!=rhs_.io_mem_to_ooo_lqCancelCnt) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lqCancelCnt=0x%0h while the rhs_.io_mem_to_ooo_lqCancelCnt=0x%0h",this.io_mem_to_ooo_lqCancelCnt,rhs_.io_mem_to_ooo_lqCancelCnt),UVM_NONE)
        end

        if(this.io_mem_to_ooo_sqCancelCnt!=rhs_.io_mem_to_ooo_sqCancelCnt) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_sqCancelCnt=0x%0h while the rhs_.io_mem_to_ooo_sqCancelCnt=0x%0h",this.io_mem_to_ooo_sqCancelCnt,rhs_.io_mem_to_ooo_sqCancelCnt),UVM_NONE)
        end

        if(this.io_mem_to_ooo_sqDeq!=rhs_.io_mem_to_ooo_sqDeq) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_sqDeq=0x%0h while the rhs_.io_mem_to_ooo_sqDeq=0x%0h",this.io_mem_to_ooo_sqDeq,rhs_.io_mem_to_ooo_sqDeq),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lqDeq!=rhs_.io_mem_to_ooo_lqDeq) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lqDeq=0x%0h while the rhs_.io_mem_to_ooo_lqDeq=0x%0h",this.io_mem_to_ooo_lqDeq,rhs_.io_mem_to_ooo_lqDeq),UVM_NONE)
        end

        if(this.io_mem_to_ooo_sqDeqPtr_flag!=rhs_.io_mem_to_ooo_sqDeqPtr_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_sqDeqPtr_flag=0x%0h while the rhs_.io_mem_to_ooo_sqDeqPtr_flag=0x%0h",this.io_mem_to_ooo_sqDeqPtr_flag,rhs_.io_mem_to_ooo_sqDeqPtr_flag),UVM_NONE)
        end

        if(this.io_mem_to_ooo_sqDeqPtr_value!=rhs_.io_mem_to_ooo_sqDeqPtr_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_sqDeqPtr_value=0x%0h while the rhs_.io_mem_to_ooo_sqDeqPtr_value=0x%0h",this.io_mem_to_ooo_sqDeqPtr_value,rhs_.io_mem_to_ooo_sqDeqPtr_value),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lqDeqPtr_flag!=rhs_.io_mem_to_ooo_lqDeqPtr_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lqDeqPtr_flag=0x%0h while the rhs_.io_mem_to_ooo_lqDeqPtr_flag=0x%0h",this.io_mem_to_ooo_lqDeqPtr_flag,rhs_.io_mem_to_ooo_lqDeqPtr_flag),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lqDeqPtr_value!=rhs_.io_mem_to_ooo_lqDeqPtr_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lqDeqPtr_value=0x%0h while the rhs_.io_mem_to_ooo_lqDeqPtr_value=0x%0h",this.io_mem_to_ooo_lqDeqPtr_value,rhs_.io_mem_to_ooo_lqDeqPtr_value),UVM_NONE)
        end

        if(this.io_mem_to_ooo_updateLFST_0_valid!=rhs_.io_mem_to_ooo_updateLFST_0_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_updateLFST_0_valid=0x%0h while the rhs_.io_mem_to_ooo_updateLFST_0_valid=0x%0h",this.io_mem_to_ooo_updateLFST_0_valid,rhs_.io_mem_to_ooo_updateLFST_0_valid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_updateLFST_0_bits_robIdx_flag!=rhs_.io_mem_to_ooo_updateLFST_0_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_updateLFST_0_bits_robIdx_flag=0x%0h while the rhs_.io_mem_to_ooo_updateLFST_0_bits_robIdx_flag=0x%0h",this.io_mem_to_ooo_updateLFST_0_bits_robIdx_flag,rhs_.io_mem_to_ooo_updateLFST_0_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_mem_to_ooo_updateLFST_0_bits_robIdx_value!=rhs_.io_mem_to_ooo_updateLFST_0_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_updateLFST_0_bits_robIdx_value=0x%0h while the rhs_.io_mem_to_ooo_updateLFST_0_bits_robIdx_value=0x%0h",this.io_mem_to_ooo_updateLFST_0_bits_robIdx_value,rhs_.io_mem_to_ooo_updateLFST_0_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_mem_to_ooo_updateLFST_0_bits_ssid!=rhs_.io_mem_to_ooo_updateLFST_0_bits_ssid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_updateLFST_0_bits_ssid=0x%0h while the rhs_.io_mem_to_ooo_updateLFST_0_bits_ssid=0x%0h",this.io_mem_to_ooo_updateLFST_0_bits_ssid,rhs_.io_mem_to_ooo_updateLFST_0_bits_ssid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_updateLFST_0_bits_storeSetHit!=rhs_.io_mem_to_ooo_updateLFST_0_bits_storeSetHit) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_updateLFST_0_bits_storeSetHit=0x%0h while the rhs_.io_mem_to_ooo_updateLFST_0_bits_storeSetHit=0x%0h",this.io_mem_to_ooo_updateLFST_0_bits_storeSetHit,rhs_.io_mem_to_ooo_updateLFST_0_bits_storeSetHit),UVM_NONE)
        end

        if(this.io_mem_to_ooo_updateLFST_1_valid!=rhs_.io_mem_to_ooo_updateLFST_1_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_updateLFST_1_valid=0x%0h while the rhs_.io_mem_to_ooo_updateLFST_1_valid=0x%0h",this.io_mem_to_ooo_updateLFST_1_valid,rhs_.io_mem_to_ooo_updateLFST_1_valid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_updateLFST_1_bits_robIdx_flag!=rhs_.io_mem_to_ooo_updateLFST_1_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_updateLFST_1_bits_robIdx_flag=0x%0h while the rhs_.io_mem_to_ooo_updateLFST_1_bits_robIdx_flag=0x%0h",this.io_mem_to_ooo_updateLFST_1_bits_robIdx_flag,rhs_.io_mem_to_ooo_updateLFST_1_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_mem_to_ooo_updateLFST_1_bits_robIdx_value!=rhs_.io_mem_to_ooo_updateLFST_1_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_updateLFST_1_bits_robIdx_value=0x%0h while the rhs_.io_mem_to_ooo_updateLFST_1_bits_robIdx_value=0x%0h",this.io_mem_to_ooo_updateLFST_1_bits_robIdx_value,rhs_.io_mem_to_ooo_updateLFST_1_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_mem_to_ooo_updateLFST_1_bits_ssid!=rhs_.io_mem_to_ooo_updateLFST_1_bits_ssid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_updateLFST_1_bits_ssid=0x%0h while the rhs_.io_mem_to_ooo_updateLFST_1_bits_ssid=0x%0h",this.io_mem_to_ooo_updateLFST_1_bits_ssid,rhs_.io_mem_to_ooo_updateLFST_1_bits_ssid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_updateLFST_1_bits_storeSetHit!=rhs_.io_mem_to_ooo_updateLFST_1_bits_storeSetHit) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_updateLFST_1_bits_storeSetHit=0x%0h while the rhs_.io_mem_to_ooo_updateLFST_1_bits_storeSetHit=0x%0h",this.io_mem_to_ooo_updateLFST_1_bits_storeSetHit,rhs_.io_mem_to_ooo_updateLFST_1_bits_storeSetHit),UVM_NONE)
        end

        if(this.io_mem_to_ooo_stIssuePtr_flag!=rhs_.io_mem_to_ooo_stIssuePtr_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_stIssuePtr_flag=0x%0h while the rhs_.io_mem_to_ooo_stIssuePtr_flag=0x%0h",this.io_mem_to_ooo_stIssuePtr_flag,rhs_.io_mem_to_ooo_stIssuePtr_flag),UVM_NONE)
        end

        if(this.io_mem_to_ooo_stIssuePtr_value!=rhs_.io_mem_to_ooo_stIssuePtr_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_stIssuePtr_value=0x%0h while the rhs_.io_mem_to_ooo_stIssuePtr_value=0x%0h",this.io_mem_to_ooo_stIssuePtr_value,rhs_.io_mem_to_ooo_stIssuePtr_value),UVM_NONE)
        end

        if(this.io_mem_to_ooo_memoryViolation_valid!=rhs_.io_mem_to_ooo_memoryViolation_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_memoryViolation_valid=0x%0h while the rhs_.io_mem_to_ooo_memoryViolation_valid=0x%0h",this.io_mem_to_ooo_memoryViolation_valid,rhs_.io_mem_to_ooo_memoryViolation_valid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag!=rhs_.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag=0x%0h while the rhs_.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag=0x%0h",this.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag,rhs_.io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag),UVM_NONE)
        end

        if(this.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value!=rhs_.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value=0x%0h while the rhs_.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value=0x%0h",this.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value,rhs_.io_mem_to_ooo_memoryViolation_bits_ftqIdx_value),UVM_NONE)
        end

        if(this.io_mem_to_ooo_memoryViolation_bits_ftqOffset!=rhs_.io_mem_to_ooo_memoryViolation_bits_ftqOffset) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_memoryViolation_bits_ftqOffset=0x%0h while the rhs_.io_mem_to_ooo_memoryViolation_bits_ftqOffset=0x%0h",this.io_mem_to_ooo_memoryViolation_bits_ftqOffset,rhs_.io_mem_to_ooo_memoryViolation_bits_ftqOffset),UVM_NONE)
        end

        if(this.io_mem_to_ooo_memoryViolation_bits_isRVC!=rhs_.io_mem_to_ooo_memoryViolation_bits_isRVC) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_memoryViolation_bits_isRVC=0x%0h while the rhs_.io_mem_to_ooo_memoryViolation_bits_isRVC=0x%0h",this.io_mem_to_ooo_memoryViolation_bits_isRVC,rhs_.io_mem_to_ooo_memoryViolation_bits_isRVC),UVM_NONE)
        end

        if(this.io_mem_to_ooo_memoryViolation_bits_target!=rhs_.io_mem_to_ooo_memoryViolation_bits_target) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_memoryViolation_bits_target=0x%0h while the rhs_.io_mem_to_ooo_memoryViolation_bits_target=0x%0h",this.io_mem_to_ooo_memoryViolation_bits_target,rhs_.io_mem_to_ooo_memoryViolation_bits_target),UVM_NONE)
        end

        if(this.io_mem_to_ooo_memoryViolation_bits_level!=rhs_.io_mem_to_ooo_memoryViolation_bits_level) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_memoryViolation_bits_level=0x%0h while the rhs_.io_mem_to_ooo_memoryViolation_bits_level=0x%0h",this.io_mem_to_ooo_memoryViolation_bits_level,rhs_.io_mem_to_ooo_memoryViolation_bits_level),UVM_NONE)
        end

        if(this.io_mem_to_ooo_memoryViolation_bits_robIdx_flag!=rhs_.io_mem_to_ooo_memoryViolation_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_memoryViolation_bits_robIdx_flag=0x%0h while the rhs_.io_mem_to_ooo_memoryViolation_bits_robIdx_flag=0x%0h",this.io_mem_to_ooo_memoryViolation_bits_robIdx_flag,rhs_.io_mem_to_ooo_memoryViolation_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_mem_to_ooo_memoryViolation_bits_robIdx_value!=rhs_.io_mem_to_ooo_memoryViolation_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_memoryViolation_bits_robIdx_value=0x%0h while the rhs_.io_mem_to_ooo_memoryViolation_bits_robIdx_value=0x%0h",this.io_mem_to_ooo_memoryViolation_bits_robIdx_value,rhs_.io_mem_to_ooo_memoryViolation_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag!=rhs_.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag=0x%0h while the rhs_.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag=0x%0h",this.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag,rhs_.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag),UVM_NONE)
        end

        if(this.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value!=rhs_.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value=0x%0h while the rhs_.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value=0x%0h",this.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value,rhs_.io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value),UVM_NONE)
        end

        if(this.io_mem_to_ooo_memoryViolation_bits_stFtqOffset!=rhs_.io_mem_to_ooo_memoryViolation_bits_stFtqOffset) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_memoryViolation_bits_stFtqOffset=0x%0h while the rhs_.io_mem_to_ooo_memoryViolation_bits_stFtqOffset=0x%0h",this.io_mem_to_ooo_memoryViolation_bits_stFtqOffset,rhs_.io_mem_to_ooo_memoryViolation_bits_stFtqOffset),UVM_NONE)
        end

        if(this.io_mem_to_ooo_memoryViolation_bits_stIsRVC!=rhs_.io_mem_to_ooo_memoryViolation_bits_stIsRVC) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_memoryViolation_bits_stIsRVC=0x%0h while the rhs_.io_mem_to_ooo_memoryViolation_bits_stIsRVC=0x%0h",this.io_mem_to_ooo_memoryViolation_bits_stIsRVC,rhs_.io_mem_to_ooo_memoryViolation_bits_stIsRVC),UVM_NONE)
        end

        if(this.io_mem_to_ooo_sbIsEmpty!=rhs_.io_mem_to_ooo_sbIsEmpty) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_sbIsEmpty=0x%0h while the rhs_.io_mem_to_ooo_sbIsEmpty=0x%0h",this.io_mem_to_ooo_sbIsEmpty,rhs_.io_mem_to_ooo_sbIsEmpty),UVM_NONE)
        end

        if(this.io_mem_to_ooo_mdpTrain_valid!=rhs_.io_mem_to_ooo_mdpTrain_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_mdpTrain_valid=0x%0h while the rhs_.io_mem_to_ooo_mdpTrain_valid=0x%0h",this.io_mem_to_ooo_mdpTrain_valid,rhs_.io_mem_to_ooo_mdpTrain_valid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag!=rhs_.io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag=0x%0h while the rhs_.io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag=0x%0h",this.io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag,rhs_.io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag),UVM_NONE)
        end

        if(this.io_mem_to_ooo_mdpTrain_bits_ftqIdx_value!=rhs_.io_mem_to_ooo_mdpTrain_bits_ftqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_mdpTrain_bits_ftqIdx_value=0x%0h while the rhs_.io_mem_to_ooo_mdpTrain_bits_ftqIdx_value=0x%0h",this.io_mem_to_ooo_mdpTrain_bits_ftqIdx_value,rhs_.io_mem_to_ooo_mdpTrain_bits_ftqIdx_value),UVM_NONE)
        end

        if(this.io_mem_to_ooo_mdpTrain_bits_ftqOffset!=rhs_.io_mem_to_ooo_mdpTrain_bits_ftqOffset) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_mdpTrain_bits_ftqOffset=0x%0h while the rhs_.io_mem_to_ooo_mdpTrain_bits_ftqOffset=0x%0h",this.io_mem_to_ooo_mdpTrain_bits_ftqOffset,rhs_.io_mem_to_ooo_mdpTrain_bits_ftqOffset),UVM_NONE)
        end

        if(this.io_mem_to_ooo_mdpTrain_bits_isRVC!=rhs_.io_mem_to_ooo_mdpTrain_bits_isRVC) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_mdpTrain_bits_isRVC=0x%0h while the rhs_.io_mem_to_ooo_mdpTrain_bits_isRVC=0x%0h",this.io_mem_to_ooo_mdpTrain_bits_isRVC,rhs_.io_mem_to_ooo_mdpTrain_bits_isRVC),UVM_NONE)
        end

        if(this.io_mem_to_ooo_mdpTrain_bits_target!=rhs_.io_mem_to_ooo_mdpTrain_bits_target) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_mdpTrain_bits_target=0x%0h while the rhs_.io_mem_to_ooo_mdpTrain_bits_target=0x%0h",this.io_mem_to_ooo_mdpTrain_bits_target,rhs_.io_mem_to_ooo_mdpTrain_bits_target),UVM_NONE)
        end

        if(this.io_mem_to_ooo_mdpTrain_bits_level!=rhs_.io_mem_to_ooo_mdpTrain_bits_level) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_mdpTrain_bits_level=0x%0h while the rhs_.io_mem_to_ooo_mdpTrain_bits_level=0x%0h",this.io_mem_to_ooo_mdpTrain_bits_level,rhs_.io_mem_to_ooo_mdpTrain_bits_level),UVM_NONE)
        end

        if(this.io_mem_to_ooo_mdpTrain_bits_robIdx_flag!=rhs_.io_mem_to_ooo_mdpTrain_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_mdpTrain_bits_robIdx_flag=0x%0h while the rhs_.io_mem_to_ooo_mdpTrain_bits_robIdx_flag=0x%0h",this.io_mem_to_ooo_mdpTrain_bits_robIdx_flag,rhs_.io_mem_to_ooo_mdpTrain_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_mem_to_ooo_mdpTrain_bits_robIdx_value!=rhs_.io_mem_to_ooo_mdpTrain_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_mdpTrain_bits_robIdx_value=0x%0h while the rhs_.io_mem_to_ooo_mdpTrain_bits_robIdx_value=0x%0h",this.io_mem_to_ooo_mdpTrain_bits_robIdx_value,rhs_.io_mem_to_ooo_mdpTrain_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag!=rhs_.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag=0x%0h while the rhs_.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag=0x%0h",this.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag,rhs_.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag),UVM_NONE)
        end

        if(this.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value!=rhs_.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value=0x%0h while the rhs_.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value=0x%0h",this.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value,rhs_.io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value),UVM_NONE)
        end

        if(this.io_mem_to_ooo_mdpTrain_bits_stFtqOffset!=rhs_.io_mem_to_ooo_mdpTrain_bits_stFtqOffset) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_mdpTrain_bits_stFtqOffset=0x%0h while the rhs_.io_mem_to_ooo_mdpTrain_bits_stFtqOffset=0x%0h",this.io_mem_to_ooo_mdpTrain_bits_stFtqOffset,rhs_.io_mem_to_ooo_mdpTrain_bits_stFtqOffset),UVM_NONE)
        end

        if(this.io_mem_to_ooo_mdpTrain_bits_stIsRVC!=rhs_.io_mem_to_ooo_mdpTrain_bits_stIsRVC) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_mdpTrain_bits_stIsRVC=0x%0h while the rhs_.io_mem_to_ooo_mdpTrain_bits_stIsRVC=0x%0h",this.io_mem_to_ooo_mdpTrain_bits_stIsRVC,rhs_.io_mem_to_ooo_mdpTrain_bits_stIsRVC),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx!=rhs_.io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx,rhs_.io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid!=rhs_.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid,rhs_.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits!=rhs_.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits,rhs_.io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx!=rhs_.io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx,rhs_.io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid!=rhs_.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid,rhs_.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits!=rhs_.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits,rhs_.io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en!=rhs_.io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en,rhs_.io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss!=rhs_.io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss,rhs_.io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx!=rhs_.io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx,rhs_.io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid!=rhs_.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid,rhs_.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits!=rhs_.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits,rhs_.io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx!=rhs_.io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx,rhs_.io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid!=rhs_.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid,rhs_.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits!=rhs_.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits,rhs_.io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en!=rhs_.io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en,rhs_.io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss!=rhs_.io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss,rhs_.io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx!=rhs_.io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx,rhs_.io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid!=rhs_.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid,rhs_.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits!=rhs_.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits,rhs_.io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx!=rhs_.io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx,rhs_.io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid!=rhs_.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid,rhs_.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits!=rhs_.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits,rhs_.io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en!=rhs_.io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en,rhs_.io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss!=rhs_.io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss=0x%0h while the rhs_.io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss=0x%0h",this.io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss,rhs_.io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsqio_vaddr!=rhs_.io_mem_to_ooo_lsqio_vaddr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsqio_vaddr=0x%0h while the rhs_.io_mem_to_ooo_lsqio_vaddr=0x%0h",this.io_mem_to_ooo_lsqio_vaddr,rhs_.io_mem_to_ooo_lsqio_vaddr),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsqio_vstart!=rhs_.io_mem_to_ooo_lsqio_vstart) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsqio_vstart=0x%0h while the rhs_.io_mem_to_ooo_lsqio_vstart=0x%0h",this.io_mem_to_ooo_lsqio_vstart,rhs_.io_mem_to_ooo_lsqio_vstart),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsqio_vl!=rhs_.io_mem_to_ooo_lsqio_vl) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsqio_vl=0x%0h while the rhs_.io_mem_to_ooo_lsqio_vl=0x%0h",this.io_mem_to_ooo_lsqio_vl,rhs_.io_mem_to_ooo_lsqio_vl),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsqio_gpaddr!=rhs_.io_mem_to_ooo_lsqio_gpaddr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsqio_gpaddr=0x%0h while the rhs_.io_mem_to_ooo_lsqio_gpaddr=0x%0h",this.io_mem_to_ooo_lsqio_gpaddr,rhs_.io_mem_to_ooo_lsqio_gpaddr),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsqio_isForVSnonLeafPTE!=rhs_.io_mem_to_ooo_lsqio_isForVSnonLeafPTE) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsqio_isForVSnonLeafPTE=0x%0h while the rhs_.io_mem_to_ooo_lsqio_isForVSnonLeafPTE=0x%0h",this.io_mem_to_ooo_lsqio_isForVSnonLeafPTE,rhs_.io_mem_to_ooo_lsqio_isForVSnonLeafPTE),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsqio_mmioBusy!=rhs_.io_mem_to_ooo_lsqio_mmioBusy) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsqio_mmioBusy=0x%0h while the rhs_.io_mem_to_ooo_lsqio_mmioBusy=0x%0h",this.io_mem_to_ooo_lsqio_mmioBusy,rhs_.io_mem_to_ooo_lsqio_mmioBusy),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsqio_lqCanAccept!=rhs_.io_mem_to_ooo_lsqio_lqCanAccept) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsqio_lqCanAccept=0x%0h while the rhs_.io_mem_to_ooo_lsqio_lqCanAccept=0x%0h",this.io_mem_to_ooo_lsqio_lqCanAccept,rhs_.io_mem_to_ooo_lsqio_lqCanAccept),UVM_NONE)
        end

        if(this.io_mem_to_ooo_lsqio_sqCanAccept!=rhs_.io_mem_to_ooo_lsqio_sqCanAccept) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_lsqio_sqCanAccept=0x%0h while the rhs_.io_mem_to_ooo_lsqio_sqCanAccept=0x%0h",this.io_mem_to_ooo_lsqio_sqCanAccept,rhs_.io_mem_to_ooo_lsqio_sqCanAccept),UVM_NONE)
        end

        if(this.io_mem_to_ooo_ldCancel_0_ld2Cancel!=rhs_.io_mem_to_ooo_ldCancel_0_ld2Cancel) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_ldCancel_0_ld2Cancel=0x%0h while the rhs_.io_mem_to_ooo_ldCancel_0_ld2Cancel=0x%0h",this.io_mem_to_ooo_ldCancel_0_ld2Cancel,rhs_.io_mem_to_ooo_ldCancel_0_ld2Cancel),UVM_NONE)
        end

        if(this.io_mem_to_ooo_ldCancel_1_ld2Cancel!=rhs_.io_mem_to_ooo_ldCancel_1_ld2Cancel) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_ldCancel_1_ld2Cancel=0x%0h while the rhs_.io_mem_to_ooo_ldCancel_1_ld2Cancel=0x%0h",this.io_mem_to_ooo_ldCancel_1_ld2Cancel,rhs_.io_mem_to_ooo_ldCancel_1_ld2Cancel),UVM_NONE)
        end

        if(this.io_mem_to_ooo_ldCancel_2_ld2Cancel!=rhs_.io_mem_to_ooo_ldCancel_2_ld2Cancel) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_mem_to_ooo_ldCancel_2_ld2Cancel=0x%0h while the rhs_.io_mem_to_ooo_ldCancel_2_ld2Cancel=0x%0h",this.io_mem_to_ooo_ldCancel_2_ld2Cancel,rhs_.io_mem_to_ooo_ldCancel_2_ld2Cancel),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
