//=========================================================
//File name    : vecissue_agent_connect.sv
//Author       : OpenAI_Codex
//Module name  : vecissue_agent_connect
//Discribution : vecissue_agent_connect : vecissue_agent Interface connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef VECISSUE_AGENT_CONNECT__SV
`define VECISSUE_AGENT_CONNECT__SV

`define MEMBLOCK__VECISSUE_AGENT_CONNECT(U_IF_NAME,AGENT_PATH,RTL_PATH) \
    vecissue_agent_agent_interface  U_IF_NAME (clk,tc_if.rst_n); \
    initial begin \
        uvm_config_db#(virtual vecissue_agent_agent_interface)::set(null,`"*AGENT_PATH*`", "vif", U_IF_NAME); \
    end \
    `ifdef MEMBLOCK_UT \
    initial begin \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_ready = RTL_PATH.io_ooo_to_mem_issueVldu_1_ready; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_valid = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_valid; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_fuOpType; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_src_0 = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_src_0; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_src_1 = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_src_1; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_src_2 = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_src_2; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_src_3 = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_src_3; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_flag; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_value; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_pdest = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_pdest; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vecWen = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vecWen; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_v0Wen; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vlWen = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vlWen; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vma; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vta; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vm; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vstart; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vmask; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_nf; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_veew; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isVleff; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_flag; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_value; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_ftqOffset; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_flag; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_flag; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value = U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_ready = RTL_PATH.io_ooo_to_mem_issueVldu_0_ready; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_valid = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_valid; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_fuType = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_fuType; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_fuOpType; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_src_0 = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_src_0; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_src_1 = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_src_1; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_src_2 = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_src_2; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_src_3 = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_src_3; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_flag; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_value; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_pdest = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_pdest; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vecWen = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vecWen; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_v0Wen; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vlWen = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vlWen; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vma; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vta; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vm; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vstart; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vmask; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_nf; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_veew; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isVleff; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_flag; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_value; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_ftqOffset; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_flag; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_flag; \
        force RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value = U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value; \
    end \
    `else \
    initial begin \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_ready = RTL_PATH.io_ooo_to_mem_issueVldu_1_ready; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_valid = RTL_PATH.io_ooo_to_mem_issueVldu_1_valid; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_fuOpType = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_src_0 = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_src_0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_src_1 = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_src_1; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_src_2 = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_src_2; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_src_3 = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_src_3; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vl = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_flag = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_value = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_pdest = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_pdest; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_pdestVl = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vecWen = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vecWen; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_v0Wen = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vlWen = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vlWen; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vill = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vma = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vta = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVill = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVma = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVta = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVsew = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVlmul = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vm = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vstart = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_frm = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFpToVecInst = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP32Instr = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP64Instr = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isReduction = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_2 = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_4 = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_8 = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vxrm = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vmask = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_nf = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_veew = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isReverse = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isExt = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isNarrow = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDstMask = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isOpMask = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isMove = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDependOldVd = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isWritePartVd = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isVleff = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_maskVecGen = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew8 = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew16 = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew32 = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew64 = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_flag = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_value = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_ftqOffset = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_numLsElem = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_flag = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_flag = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value = RTL_PATH.io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_ready = RTL_PATH.io_ooo_to_mem_issueVldu_0_ready; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_valid = RTL_PATH.io_ooo_to_mem_issueVldu_0_valid; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_fuType = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_fuType; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_fuOpType = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_src_0 = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_src_0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_src_1 = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_src_1; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_src_2 = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_src_2; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_src_3 = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_src_3; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vl = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_flag = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_value = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_pdest = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_pdest; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_pdestVl = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vecWen = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vecWen; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_v0Wen = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vlWen = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vlWen; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vill = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vma = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vta = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVill = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVma = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVta = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVsew = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVlmul = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vm = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vstart = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_frm = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFpToVecInst = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP32Instr = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP64Instr = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isReduction = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_2 = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_4 = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_8 = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vxrm = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vmask = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_nf = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_veew = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isReverse = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isExt = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isNarrow = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDstMask = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isOpMask = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isMove = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDependOldVd = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isWritePartVd = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isVleff = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_maskVecGen = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew8 = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew16 = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew32 = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew64 = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_flag = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_value = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_ftqOffset = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_numLsElem = '0; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_flag = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_flag = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag; \
        force U_IF_NAME.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value = RTL_PATH.io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value; \
    end \
    `endif

`endif
