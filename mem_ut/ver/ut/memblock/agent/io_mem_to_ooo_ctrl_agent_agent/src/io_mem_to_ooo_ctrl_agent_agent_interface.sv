//=========================================================
//File name    : io_mem_to_ooo_ctrl_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_ctrl_agent_agent_interface
//Discribution : io_mem_to_ooo_ctrl_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_CTRL_AGENT_AGENT_INTERFACE__SV
`define IO_MEM_TO_OOO_CTRL_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface io_mem_to_ooo_ctrl_agent_agent_interface  (input bit clk,input bit rst_n);

    logic [5:0] io_mem_to_ooo_topToBackendBypass_hartId;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31;
    logic io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43;
    logic io_mem_to_ooo_topToBackendBypass_msiInfo_valid;
    logic [12:0] io_mem_to_ooo_topToBackendBypass_msiInfo_bits;
    logic io_mem_to_ooo_topToBackendBypass_clintTime_valid;
    logic [63:0] io_mem_to_ooo_topToBackendBypass_clintTime_bits;
    logic io_mem_to_ooo_topToBackendBypass_l2FlushDone;
    logic [6:0] io_mem_to_ooo_lqCancelCnt;
    logic [5:0] io_mem_to_ooo_sqCancelCnt;
    logic [1:0] io_mem_to_ooo_sqDeq    ;
    logic [3:0] io_mem_to_ooo_lqDeq    ;
    logic io_mem_to_ooo_sqDeqPtr_flag  ;
    logic [5:0] io_mem_to_ooo_sqDeqPtr_value;
    logic io_mem_to_ooo_lqDeqPtr_flag  ;
    logic [6:0] io_mem_to_ooo_lqDeqPtr_value;
    logic io_mem_to_ooo_updateLFST_0_valid;
    logic io_mem_to_ooo_updateLFST_0_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_updateLFST_0_bits_robIdx_value;
    logic [4:0] io_mem_to_ooo_updateLFST_0_bits_ssid;
    logic io_mem_to_ooo_updateLFST_0_bits_storeSetHit;
    logic io_mem_to_ooo_updateLFST_1_valid;
    logic io_mem_to_ooo_updateLFST_1_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_updateLFST_1_bits_robIdx_value;
    logic [4:0] io_mem_to_ooo_updateLFST_1_bits_ssid;
    logic io_mem_to_ooo_updateLFST_1_bits_storeSetHit;
    logic io_mem_to_ooo_stIssuePtr_flag;
    logic [5:0] io_mem_to_ooo_stIssuePtr_value;
    logic io_mem_to_ooo_memoryViolation_valid;
    logic io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag;
    logic [5:0] io_mem_to_ooo_memoryViolation_bits_ftqIdx_value;
    logic [4:0] io_mem_to_ooo_memoryViolation_bits_ftqOffset;
    logic io_mem_to_ooo_memoryViolation_bits_isRVC;
    logic [49:0] io_mem_to_ooo_memoryViolation_bits_target;
    logic io_mem_to_ooo_memoryViolation_bits_level;
    logic io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_memoryViolation_bits_robIdx_value;
    logic io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag;
    logic [5:0] io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value;
    logic [4:0] io_mem_to_ooo_memoryViolation_bits_stFtqOffset;
    logic io_mem_to_ooo_memoryViolation_bits_stIsRVC;
    logic io_mem_to_ooo_sbIsEmpty      ;
    logic io_mem_to_ooo_mdpTrain_valid ;
    logic io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag;
    logic [5:0] io_mem_to_ooo_mdpTrain_bits_ftqIdx_value;
    logic [4:0] io_mem_to_ooo_mdpTrain_bits_ftqOffset;
    logic io_mem_to_ooo_mdpTrain_bits_isRVC;
    logic [49:0] io_mem_to_ooo_mdpTrain_bits_target;
    logic io_mem_to_ooo_mdpTrain_bits_level;
    logic io_mem_to_ooo_mdpTrain_bits_robIdx_flag;
    logic [8:0] io_mem_to_ooo_mdpTrain_bits_robIdx_value;
    logic io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag;
    logic [5:0] io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value;
    logic [4:0] io_mem_to_ooo_mdpTrain_bits_stFtqOffset;
    logic io_mem_to_ooo_mdpTrain_bits_stIsRVC;
    logic [8:0] io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx;
    logic io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid;
    logic [49:0] io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits;
    logic [8:0] io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx;
    logic io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid;
    logic [47:0] io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits;
    logic io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en;
    logic io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss;
    logic [8:0] io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx;
    logic io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid;
    logic [49:0] io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits;
    logic [8:0] io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx;
    logic io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid;
    logic [47:0] io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits;
    logic io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en;
    logic io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss;
    logic [8:0] io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx;
    logic io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid;
    logic [49:0] io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits;
    logic [8:0] io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx;
    logic io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid;
    logic [47:0] io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits;
    logic io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en;
    logic io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss;
    logic [63:0] io_mem_to_ooo_lsqio_vaddr;
    logic [7:0] io_mem_to_ooo_lsqio_vstart;
    logic [7:0] io_mem_to_ooo_lsqio_vl ;
    logic [63:0] io_mem_to_ooo_lsqio_gpaddr;
    logic io_mem_to_ooo_lsqio_isForVSnonLeafPTE;
    logic io_mem_to_ooo_lsqio_mmioBusy ;
    logic io_mem_to_ooo_lsqio_lqCanAccept;
    logic io_mem_to_ooo_lsqio_sqCanAccept;
    logic io_mem_to_ooo_ldCancel_0_ld2Cancel;
    logic io_mem_to_ooo_ldCancel_1_ld2Cancel;
    logic io_mem_to_ooo_ldCancel_2_ld2Cancel;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_mem_to_ooo_topToBackendBypass_hartId;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43;
        input  io_mem_to_ooo_topToBackendBypass_msiInfo_valid;
        input  io_mem_to_ooo_topToBackendBypass_msiInfo_bits;
        input  io_mem_to_ooo_topToBackendBypass_clintTime_valid;
        input  io_mem_to_ooo_topToBackendBypass_clintTime_bits;
        input  io_mem_to_ooo_topToBackendBypass_l2FlushDone;
        input  io_mem_to_ooo_lqCancelCnt;
        input  io_mem_to_ooo_sqCancelCnt;
        input  io_mem_to_ooo_sqDeq;
        input  io_mem_to_ooo_lqDeq;
        input  io_mem_to_ooo_sqDeqPtr_flag;
        input  io_mem_to_ooo_sqDeqPtr_value;
        input  io_mem_to_ooo_lqDeqPtr_flag;
        input  io_mem_to_ooo_lqDeqPtr_value;
        input  io_mem_to_ooo_updateLFST_0_valid;
        input  io_mem_to_ooo_updateLFST_0_bits_robIdx_flag;
        input  io_mem_to_ooo_updateLFST_0_bits_robIdx_value;
        input  io_mem_to_ooo_updateLFST_0_bits_ssid;
        input  io_mem_to_ooo_updateLFST_0_bits_storeSetHit;
        input  io_mem_to_ooo_updateLFST_1_valid;
        input  io_mem_to_ooo_updateLFST_1_bits_robIdx_flag;
        input  io_mem_to_ooo_updateLFST_1_bits_robIdx_value;
        input  io_mem_to_ooo_updateLFST_1_bits_ssid;
        input  io_mem_to_ooo_updateLFST_1_bits_storeSetHit;
        input  io_mem_to_ooo_stIssuePtr_flag;
        input  io_mem_to_ooo_stIssuePtr_value;
        input  io_mem_to_ooo_memoryViolation_valid;
        input  io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag;
        input  io_mem_to_ooo_memoryViolation_bits_ftqIdx_value;
        input  io_mem_to_ooo_memoryViolation_bits_ftqOffset;
        input  io_mem_to_ooo_memoryViolation_bits_isRVC;
        input  io_mem_to_ooo_memoryViolation_bits_target;
        input  io_mem_to_ooo_memoryViolation_bits_level;
        input  io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
        input  io_mem_to_ooo_memoryViolation_bits_robIdx_value;
        input  io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag;
        input  io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value;
        input  io_mem_to_ooo_memoryViolation_bits_stFtqOffset;
        input  io_mem_to_ooo_memoryViolation_bits_stIsRVC;
        input  io_mem_to_ooo_sbIsEmpty;
        input  io_mem_to_ooo_mdpTrain_valid;
        input  io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag;
        input  io_mem_to_ooo_mdpTrain_bits_ftqIdx_value;
        input  io_mem_to_ooo_mdpTrain_bits_ftqOffset;
        input  io_mem_to_ooo_mdpTrain_bits_isRVC;
        input  io_mem_to_ooo_mdpTrain_bits_target;
        input  io_mem_to_ooo_mdpTrain_bits_level;
        input  io_mem_to_ooo_mdpTrain_bits_robIdx_flag;
        input  io_mem_to_ooo_mdpTrain_bits_robIdx_value;
        input  io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag;
        input  io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value;
        input  io_mem_to_ooo_mdpTrain_bits_stFtqOffset;
        input  io_mem_to_ooo_mdpTrain_bits_stIsRVC;
        input  io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx;
        input  io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid;
        input  io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits;
        input  io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx;
        input  io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid;
        input  io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits;
        input  io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en;
        input  io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss;
        input  io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx;
        input  io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid;
        input  io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits;
        input  io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx;
        input  io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid;
        input  io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits;
        input  io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en;
        input  io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss;
        input  io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx;
        input  io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid;
        input  io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits;
        input  io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx;
        input  io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid;
        input  io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits;
        input  io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en;
        input  io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss;
        input  io_mem_to_ooo_lsqio_vaddr;
        input  io_mem_to_ooo_lsqio_vstart;
        input  io_mem_to_ooo_lsqio_vl;
        input  io_mem_to_ooo_lsqio_gpaddr;
        input  io_mem_to_ooo_lsqio_isForVSnonLeafPTE;
        input  io_mem_to_ooo_lsqio_mmioBusy;
        input  io_mem_to_ooo_lsqio_lqCanAccept;
        input  io_mem_to_ooo_lsqio_sqCanAccept;
        input  io_mem_to_ooo_ldCancel_0_ld2Cancel;
        input  io_mem_to_ooo_ldCancel_1_ld2Cancel;
        input  io_mem_to_ooo_ldCancel_2_ld2Cancel;

    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_mem_to_ooo_topToBackendBypass_hartId;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_mtip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_msip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_meip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_seip;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_31;
        input  io_mem_to_ooo_topToBackendBypass_externalInterrupt_nmi_nmi_43;
        input  io_mem_to_ooo_topToBackendBypass_msiInfo_valid;
        input  io_mem_to_ooo_topToBackendBypass_msiInfo_bits;
        input  io_mem_to_ooo_topToBackendBypass_clintTime_valid;
        input  io_mem_to_ooo_topToBackendBypass_clintTime_bits;
        input  io_mem_to_ooo_topToBackendBypass_l2FlushDone;
        input  io_mem_to_ooo_lqCancelCnt;
        input  io_mem_to_ooo_sqCancelCnt;
        input  io_mem_to_ooo_sqDeq;
        input  io_mem_to_ooo_lqDeq;
        input  io_mem_to_ooo_sqDeqPtr_flag;
        input  io_mem_to_ooo_sqDeqPtr_value;
        input  io_mem_to_ooo_lqDeqPtr_flag;
        input  io_mem_to_ooo_lqDeqPtr_value;
        input  io_mem_to_ooo_updateLFST_0_valid;
        input  io_mem_to_ooo_updateLFST_0_bits_robIdx_flag;
        input  io_mem_to_ooo_updateLFST_0_bits_robIdx_value;
        input  io_mem_to_ooo_updateLFST_0_bits_ssid;
        input  io_mem_to_ooo_updateLFST_0_bits_storeSetHit;
        input  io_mem_to_ooo_updateLFST_1_valid;
        input  io_mem_to_ooo_updateLFST_1_bits_robIdx_flag;
        input  io_mem_to_ooo_updateLFST_1_bits_robIdx_value;
        input  io_mem_to_ooo_updateLFST_1_bits_ssid;
        input  io_mem_to_ooo_updateLFST_1_bits_storeSetHit;
        input  io_mem_to_ooo_stIssuePtr_flag;
        input  io_mem_to_ooo_stIssuePtr_value;
        input  io_mem_to_ooo_memoryViolation_valid;
        input  io_mem_to_ooo_memoryViolation_bits_ftqIdx_flag;
        input  io_mem_to_ooo_memoryViolation_bits_ftqIdx_value;
        input  io_mem_to_ooo_memoryViolation_bits_ftqOffset;
        input  io_mem_to_ooo_memoryViolation_bits_isRVC;
        input  io_mem_to_ooo_memoryViolation_bits_target;
        input  io_mem_to_ooo_memoryViolation_bits_level;
        input  io_mem_to_ooo_memoryViolation_bits_robIdx_flag;
        input  io_mem_to_ooo_memoryViolation_bits_robIdx_value;
        input  io_mem_to_ooo_memoryViolation_bits_stFtqIdx_flag;
        input  io_mem_to_ooo_memoryViolation_bits_stFtqIdx_value;
        input  io_mem_to_ooo_memoryViolation_bits_stFtqOffset;
        input  io_mem_to_ooo_memoryViolation_bits_stIsRVC;
        input  io_mem_to_ooo_sbIsEmpty;
        input  io_mem_to_ooo_mdpTrain_valid;
        input  io_mem_to_ooo_mdpTrain_bits_ftqIdx_flag;
        input  io_mem_to_ooo_mdpTrain_bits_ftqIdx_value;
        input  io_mem_to_ooo_mdpTrain_bits_ftqOffset;
        input  io_mem_to_ooo_mdpTrain_bits_isRVC;
        input  io_mem_to_ooo_mdpTrain_bits_target;
        input  io_mem_to_ooo_mdpTrain_bits_level;
        input  io_mem_to_ooo_mdpTrain_bits_robIdx_flag;
        input  io_mem_to_ooo_mdpTrain_bits_robIdx_value;
        input  io_mem_to_ooo_mdpTrain_bits_stFtqIdx_flag;
        input  io_mem_to_ooo_mdpTrain_bits_stFtqIdx_value;
        input  io_mem_to_ooo_mdpTrain_bits_stFtqOffset;
        input  io_mem_to_ooo_mdpTrain_bits_stIsRVC;
        input  io_mem_to_ooo_lsTopdownInfo_0_s1_robIdx;
        input  io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_valid;
        input  io_mem_to_ooo_lsTopdownInfo_0_s1_vaddr_bits;
        input  io_mem_to_ooo_lsTopdownInfo_0_s2_robIdx;
        input  io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_valid;
        input  io_mem_to_ooo_lsTopdownInfo_0_s2_paddr_bits;
        input  io_mem_to_ooo_lsTopdownInfo_0_s2_cache_miss_en;
        input  io_mem_to_ooo_lsTopdownInfo_0_s2_first_real_miss;
        input  io_mem_to_ooo_lsTopdownInfo_1_s1_robIdx;
        input  io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_valid;
        input  io_mem_to_ooo_lsTopdownInfo_1_s1_vaddr_bits;
        input  io_mem_to_ooo_lsTopdownInfo_1_s2_robIdx;
        input  io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_valid;
        input  io_mem_to_ooo_lsTopdownInfo_1_s2_paddr_bits;
        input  io_mem_to_ooo_lsTopdownInfo_1_s2_cache_miss_en;
        input  io_mem_to_ooo_lsTopdownInfo_1_s2_first_real_miss;
        input  io_mem_to_ooo_lsTopdownInfo_2_s1_robIdx;
        input  io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_valid;
        input  io_mem_to_ooo_lsTopdownInfo_2_s1_vaddr_bits;
        input  io_mem_to_ooo_lsTopdownInfo_2_s2_robIdx;
        input  io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_valid;
        input  io_mem_to_ooo_lsTopdownInfo_2_s2_paddr_bits;
        input  io_mem_to_ooo_lsTopdownInfo_2_s2_cache_miss_en;
        input  io_mem_to_ooo_lsTopdownInfo_2_s2_first_real_miss;
        input  io_mem_to_ooo_lsqio_vaddr;
        input  io_mem_to_ooo_lsqio_vstart;
        input  io_mem_to_ooo_lsqio_vl;
        input  io_mem_to_ooo_lsqio_gpaddr;
        input  io_mem_to_ooo_lsqio_isForVSnonLeafPTE;
        input  io_mem_to_ooo_lsqio_mmioBusy;
        input  io_mem_to_ooo_lsqio_lqCanAccept;
        input  io_mem_to_ooo_lsqio_sqCanAccept;
        input  io_mem_to_ooo_ldCancel_0_ld2Cancel;
        input  io_mem_to_ooo_ldCancel_1_ld2Cancel;
        input  io_mem_to_ooo_ldCancel_2_ld2Cancel;

    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:io_mem_to_ooo_ctrl_agent_agent_interface

`endif
