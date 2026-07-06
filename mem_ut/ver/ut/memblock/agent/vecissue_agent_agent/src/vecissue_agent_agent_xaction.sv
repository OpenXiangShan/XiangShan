//=========================================================
//File name    : vecissue_agent_agent_xaction.sv
//Author       : OpenAI_Codex
//Module name  : vecissue_agent_agent_xaction
//Discribution : vecissue_agent_agent_xaction : agent transaction
//Date         : 2026-04-12
//=========================================================
`ifndef VECISSUE_AGENT_AGENT_XACTION__SV
`define VECISSUE_AGENT_AGENT_XACTION__SV

class vecissue_agent_agent_xaction  extends tcnt_data_base;
    // vecissue is a highly structured interface. Base constraints keep only the
    // stable vtype/index/capacity legality; scenario templates should still
    // refine fuOpType and vpu fields together.
    rand bit io_ooo_to_mem_vecIssue_1_0_ready;
    rand bit io_ooo_to_mem_vecIssue_1_0_valid;
    rand bit [8:0] io_ooo_to_mem_vecIssue_1_0_bits_fuOpType;
    rand bit [127:0] io_ooo_to_mem_vecIssue_1_0_bits_src_0;
    rand bit [127:0] io_ooo_to_mem_vecIssue_1_0_bits_src_1;
    rand bit [127:0] io_ooo_to_mem_vecIssue_1_0_bits_src_2;
    rand bit [127:0] io_ooo_to_mem_vecIssue_1_0_bits_src_3;
    rand bit [7:0] io_ooo_to_mem_vecIssue_1_0_bits_vl;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_vecIssue_1_0_bits_robIdx_value;
    rand bit [6:0] io_ooo_to_mem_vecIssue_1_0_bits_pdest;
    rand bit [4:0] io_ooo_to_mem_vecIssue_1_0_bits_pdestVl;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vecWen;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_v0Wen;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vlWen;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_vill;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_vma;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_vta;
    rand bit [1:0] io_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew;
    rand bit [2:0] io_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVill;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVma;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVta;
    rand bit [1:0] io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVsew;
    rand bit [2:0] io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVlmul;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_vm;
    rand bit [7:0] io_ooo_to_mem_vecIssue_1_0_bits_vpu_vstart;
    rand bit [2:0] io_ooo_to_mem_vecIssue_1_0_bits_vpu_frm;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFpToVecInst;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP32Instr;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP64Instr;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isReduction;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_2;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_4;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_8;
    rand bit [1:0] io_ooo_to_mem_vecIssue_1_0_bits_vpu_vxrm;
    rand bit [6:0] io_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop;
    rand bit [127:0] io_ooo_to_mem_vecIssue_1_0_bits_vpu_vmask;
    rand bit [2:0] io_ooo_to_mem_vecIssue_1_0_bits_vpu_nf;
    rand bit [1:0] io_ooo_to_mem_vecIssue_1_0_bits_vpu_veew;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_isReverse;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_isExt;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_isNarrow;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDstMask;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_isOpMask;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_isMove;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDependOldVd;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_isWritePartVd;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_isVleff;
    rand bit [15:0] io_ooo_to_mem_vecIssue_1_0_bits_vpu_maskVecGen;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew8;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew16;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew32;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew64;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_value;
    rand bit [4:0] io_ooo_to_mem_vecIssue_1_0_bits_ftqOffset;
    rand bit [4:0] io_ooo_to_mem_vecIssue_1_0_bits_numLsElem;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value;
    rand bit io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value;
    rand bit io_ooo_to_mem_vecIssue_0_0_ready;
    rand bit io_ooo_to_mem_vecIssue_0_0_valid;
    rand bit [35:0] io_ooo_to_mem_vecIssue_0_0_bits_fuType;
    rand bit [8:0] io_ooo_to_mem_vecIssue_0_0_bits_fuOpType;
    rand bit [127:0] io_ooo_to_mem_vecIssue_0_0_bits_src_0;
    rand bit [127:0] io_ooo_to_mem_vecIssue_0_0_bits_src_1;
    rand bit [127:0] io_ooo_to_mem_vecIssue_0_0_bits_src_2;
    rand bit [127:0] io_ooo_to_mem_vecIssue_0_0_bits_src_3;
    rand bit [7:0] io_ooo_to_mem_vecIssue_0_0_bits_vl;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_robIdx_flag;
    rand bit [8:0] io_ooo_to_mem_vecIssue_0_0_bits_robIdx_value;
    rand bit [6:0] io_ooo_to_mem_vecIssue_0_0_bits_pdest;
    rand bit [4:0] io_ooo_to_mem_vecIssue_0_0_bits_pdestVl;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vecWen;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_v0Wen;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vlWen;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_vill;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_vma;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_vta;
    rand bit [1:0] io_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew;
    rand bit [2:0] io_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVill;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVma;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVta;
    rand bit [1:0] io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVsew;
    rand bit [2:0] io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVlmul;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_vm;
    rand bit [7:0] io_ooo_to_mem_vecIssue_0_0_bits_vpu_vstart;
    rand bit [2:0] io_ooo_to_mem_vecIssue_0_0_bits_vpu_frm;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFpToVecInst;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP32Instr;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP64Instr;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isReduction;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_2;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_4;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_8;
    rand bit [1:0] io_ooo_to_mem_vecIssue_0_0_bits_vpu_vxrm;
    rand bit [6:0] io_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop;
    rand bit [127:0] io_ooo_to_mem_vecIssue_0_0_bits_vpu_vmask;
    rand bit [2:0] io_ooo_to_mem_vecIssue_0_0_bits_vpu_nf;
    rand bit [1:0] io_ooo_to_mem_vecIssue_0_0_bits_vpu_veew;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_isReverse;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_isExt;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_isNarrow;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDstMask;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_isOpMask;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_isMove;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDependOldVd;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_isWritePartVd;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_isVleff;
    rand bit [15:0] io_ooo_to_mem_vecIssue_0_0_bits_vpu_maskVecGen;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew8;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew16;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew32;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew64;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_value;
    rand bit [4:0] io_ooo_to_mem_vecIssue_0_0_bits_ftqOffset;
    rand bit [4:0] io_ooo_to_mem_vecIssue_0_0_bits_numLsElem;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_flag;
    rand bit [6:0] io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value;
    rand bit io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_flag;
    rand bit [5:0] io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value;

    extern constraint default_io_ooo_to_mem_vecIssue_1_0_ready_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_valid_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_fuOpType_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_src_0_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_src_1_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_src_2_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_src_3_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vl_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_pdest_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_pdestVl_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vecWen_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_v0Wen_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vlWen_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vill_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vma_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vta_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVill_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVma_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVta_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVsew_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVlmul_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vm_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vstart_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_frm_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFpToVecInst_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP32Instr_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP64Instr_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isReduction_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_2_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_4_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_8_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vxrm_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vmask_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_nf_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_veew_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isReverse_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isExt_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isNarrow_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDstMask_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isOpMask_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isMove_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDependOldVd_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isWritePartVd_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isVleff_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_maskVecGen_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew8_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew16_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew32_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew64_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_ftqOffset_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_numLsElem_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_ready_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_valid_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_fuType_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_fuOpType_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_src_0_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_src_1_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_src_2_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_src_3_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vl_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_robIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_robIdx_value_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_pdest_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_pdestVl_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vecWen_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_v0Wen_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vlWen_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vill_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vma_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vta_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVill_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVma_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVta_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVsew_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVlmul_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vm_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vstart_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_frm_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFpToVecInst_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP32Instr_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP64Instr_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isReduction_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_2_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_4_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_8_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vxrm_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vmask_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_nf_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_veew_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isReverse_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isExt_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isNarrow_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDstMask_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isOpMask_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isMove_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDependOldVd_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isWritePartVd_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isVleff_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_maskVecGen_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew8_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew16_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew32_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew64_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_ftqOffset_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_numLsElem_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_flag_cons;
    extern constraint default_io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value_cons;

    extern function new(string name="vecissue_agent_agent_xaction");
    extern function void pack();
    extern function void unpack();
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function string psdisplay(string prefix = "");
    extern function bit compare(uvm_object rhs, uvm_comparer comparer=null);

    `uvm_object_utils_begin(vecissue_agent_agent_xaction)
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_src_1, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_src_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_src_3, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vl, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_pdest, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_pdestVl, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vecWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_v0Wen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vlWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_vill, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_vma, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_vta, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVill, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVma, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVta, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVsew, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVlmul, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_vm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_vstart, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_frm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFpToVecInst, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP32Instr, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP64Instr, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isReduction, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_4, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_8, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_vxrm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_vmask, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_nf, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_veew, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_isReverse, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_isExt, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_isNarrow, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDstMask, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_isOpMask, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_isMove, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDependOldVd, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_isWritePartVd, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_isVleff, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_maskVecGen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew8, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew16, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew32, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew64, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_ftqOffset, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_numLsElem, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_ready, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_valid, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_fuType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_fuOpType, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_src_0, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_src_1, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_src_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_src_3, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vl, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_robIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_robIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_pdest, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_pdestVl, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vecWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_v0Wen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vlWen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_vill, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_vma, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_vta, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVill, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVma, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVta, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVsew, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVlmul, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_vm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_vstart, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_frm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFpToVecInst, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP32Instr, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP64Instr, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isReduction, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_2, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_4, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_8, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_vxrm, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_vmask, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_nf, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_veew, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_isReverse, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_isExt, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_isNarrow, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDstMask, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_isOpMask, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_isMove, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDependOldVd, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_isWritePartVd, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_isVleff, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_maskVecGen, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew8, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew16, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew32, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew64, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_ftqOffset, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_numLsElem, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_flag, UVM_ALL_ON);
        `uvm_field_int(io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value, UVM_ALL_ON);

    `uvm_object_utils_end

endclass:vecissue_agent_agent_xaction

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_ready_cons{
    io_ooo_to_mem_vecIssue_1_0_ready inside {1'b0, 1'b1};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_valid_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_fuOpType_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_src_0_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_src_1_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_src_2_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_src_3_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vl_cons{
    io_ooo_to_mem_vecIssue_1_0_bits_vl inside {[8'd1:8'd16]};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_robIdx_flag_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_robIdx_value_cons{
    io_ooo_to_mem_vecIssue_1_0_bits_robIdx_value inside {[9'd0:9'd351]};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_pdest_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_pdestVl_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vecWen_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_v0Wen_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vlWen_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vill_cons{
    io_ooo_to_mem_vecIssue_1_0_bits_vpu_vill inside {1'b0, 1'b1};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vma_cons{
    io_ooo_to_mem_vecIssue_1_0_bits_vpu_vma inside {1'b0, 1'b1};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vta_cons{
    io_ooo_to_mem_vecIssue_1_0_bits_vpu_vta inside {1'b0, 1'b1};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew_cons{
    io_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew inside {2'b00, 2'b01, 2'b10, 2'b11};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul_cons{
    io_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul inside {3'b000, 3'b001, 3'b010, 3'b011, 3'b111, 3'b110, 3'b101};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVill_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVma_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVta_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVsew_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVlmul_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vm_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vstart_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_frm_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFpToVecInst_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP32Instr_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP64Instr_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isReduction_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_2_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_4_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_8_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vxrm_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx_cons{
    io_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx inside {[7'd0:7'd64]};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop_cons{
    io_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop inside {1'b0, 1'b1};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_vmask_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_nf_cons{
    io_ooo_to_mem_vecIssue_1_0_bits_vpu_nf inside {[3'd0:3'd7]};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_veew_cons{
    io_ooo_to_mem_vecIssue_1_0_bits_vpu_veew inside {[2'd0:2'd3]};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isReverse_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isExt_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isNarrow_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDstMask_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isOpMask_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isMove_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDependOldVd_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isWritePartVd_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_isVleff_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_maskVecGen_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew8_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew16_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew32_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew64_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_flag_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_value_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_ftqOffset_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_numLsElem_cons{
    io_ooo_to_mem_vecIssue_1_0_bits_numLsElem inside {[5'd1:5'd16]};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_flag_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value_cons{
    io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value inside {[7'd0:7'd71]};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_flag_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value_cons{
    io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value inside {[6'd0:6'd55]};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_ready_cons{
    io_ooo_to_mem_vecIssue_0_0_ready inside {1'b0, 1'b1};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_valid_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_fuType_cons{
    io_ooo_to_mem_vecIssue_0_0_bits_fuType inside {36'h1000_00000, 36'h2000_00000, 36'h4000_00000, 36'h8000_00000};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_fuOpType_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_src_0_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_src_1_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_src_2_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_src_3_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vl_cons{
    io_ooo_to_mem_vecIssue_0_0_bits_vl inside {[8'd1:8'd16]};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_robIdx_flag_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_robIdx_value_cons{
    io_ooo_to_mem_vecIssue_0_0_bits_robIdx_value inside {[9'd0:9'd351]};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_pdest_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_pdestVl_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vecWen_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_v0Wen_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vlWen_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vill_cons{
    io_ooo_to_mem_vecIssue_0_0_bits_vpu_vill inside {1'b0, 1'b1};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vma_cons{
    io_ooo_to_mem_vecIssue_0_0_bits_vpu_vma inside {1'b0, 1'b1};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vta_cons{
    io_ooo_to_mem_vecIssue_0_0_bits_vpu_vta inside {1'b0, 1'b1};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew_cons{
    io_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew inside {2'b00, 2'b01, 2'b10, 2'b11};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul_cons{
    io_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul inside {3'b000, 3'b001, 3'b010, 3'b011, 3'b111, 3'b110, 3'b101};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVill_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVma_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVta_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVsew_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVlmul_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vm_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vstart_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_frm_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFpToVecInst_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP32Instr_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP64Instr_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isReduction_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_2_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_4_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_8_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vxrm_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx_cons{
    io_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx inside {[7'd0:7'd64]};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop_cons{
    io_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop inside {1'b0, 1'b1};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_vmask_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_nf_cons{
    io_ooo_to_mem_vecIssue_0_0_bits_vpu_nf inside {[3'd0:3'd7]};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_veew_cons{
    io_ooo_to_mem_vecIssue_0_0_bits_vpu_veew inside {[2'd0:2'd3]};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isReverse_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isExt_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isNarrow_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDstMask_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isOpMask_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isMove_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDependOldVd_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isWritePartVd_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_isVleff_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_maskVecGen_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew8_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew16_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew32_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew64_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_flag_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_value_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_ftqOffset_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_numLsElem_cons{
    io_ooo_to_mem_vecIssue_0_0_bits_numLsElem inside {[5'd1:5'd16]};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_flag_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value_cons{
    io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value inside {[7'd0:7'd71]};
}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_flag_cons{

}

constraint vecissue_agent_agent_xaction::default_io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value_cons{
    io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value inside {[6'd0:6'd55]};
}

function vecissue_agent_agent_xaction::new(string name = "vecissue_agent_agent_xaction");
    super.new();
endfunction:new

function void vecissue_agent_agent_xaction::pack();
    super.pack();
endfunction:pack
function void vecissue_agent_agent_xaction::unpack();
    super.unpack();
endfunction:unpack
function void vecissue_agent_agent_xaction::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize
function void vecissue_agent_agent_xaction::post_randomize();
    super.post_randomize();
    //this.pack();
endfunction:post_randomize

function string vecissue_agent_agent_xaction::psdisplay(string prefix = "");
    string pkt_str;
    pkt_str = $sformatf("%s for packet[%0d] >>>>",prefix,this.pkt_index);
    pkt_str = $sformatf("%schannel_id=%0d ",pkt_str,this.channel_id);
    pkt_str = $sformatf("%sstart=%0f finish=%0f >>>>\n",pkt_str,this.start,this.finish);
    //foreach(this.pload_q[i]) begin
    //    pkt_str = $sformatf("%spload_q[%0d]=0x%2h  ",pkt_str,i,this.pload_q[i]);
    //end
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_ready = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_ready);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_fuOpType = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_fuOpType);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_src_0 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_src_0);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_src_1 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_src_1);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_src_2 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_src_2);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_src_3 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_src_3);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vl = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vl);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_pdest = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_pdest);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_pdestVl = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_pdestVl);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vecWen = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vecWen);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_v0Wen = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_v0Wen);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vlWen = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vlWen);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_vill = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vill);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_vma = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vma);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_vta = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vta);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_specVill = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVill);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_specVma = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVma);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_specVta = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVta);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_specVsew = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVsew);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_specVlmul = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVlmul);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_vm = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vm);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_vstart = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vstart);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_frm = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_frm);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFpToVecInst = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFpToVecInst);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP32Instr = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP32Instr);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP64Instr = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP64Instr);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isReduction = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isReduction);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_2 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_2);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_4 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_4);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_8 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_8);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_vxrm = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vxrm);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_vmask = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vmask);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_nf = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_nf);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_veew = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_veew);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_isReverse = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isReverse);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_isExt = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isExt);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_isNarrow = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isNarrow);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_isDstMask = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDstMask);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_isOpMask = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isOpMask);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_isMove = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isMove);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_isDependOldVd = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDependOldVd);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_isWritePartVd = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isWritePartVd);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_isVleff = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isVleff);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_maskVecGen = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_maskVecGen);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_sew8 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew8);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_sew16 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew16);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_sew32 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew32);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_vpu_sew64 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew64);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_ftqOffset = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_ftqOffset);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_numLsElem = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_numLsElem);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_ready = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_ready);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_valid = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_valid);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_fuType = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_fuType);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_fuOpType = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_fuOpType);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_src_0 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_src_0);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_src_1 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_src_1);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_src_2 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_src_2);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_src_3 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_src_3);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vl = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vl);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_robIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_robIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_pdest = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_pdest);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_pdestVl = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_pdestVl);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vecWen = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vecWen);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_v0Wen = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_v0Wen);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vlWen = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vlWen);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_vill = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vill);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_vma = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vma);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_vta = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vta);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_specVill = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVill);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_specVma = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVma);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_specVta = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVta);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_specVsew = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVsew);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_specVlmul = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVlmul);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_vm = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vm);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_vstart = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vstart);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_frm = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_frm);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFpToVecInst = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFpToVecInst);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP32Instr = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP32Instr);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP64Instr = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP64Instr);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isReduction = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isReduction);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_2 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_2);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_4 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_4);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_8 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_8);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_vxrm = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vxrm);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_vmask = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vmask);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_nf = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_nf);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_veew = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_veew);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_isReverse = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isReverse);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_isExt = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isExt);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_isNarrow = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isNarrow);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_isDstMask = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDstMask);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_isOpMask = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isOpMask);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_isMove = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isMove);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_isDependOldVd = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDependOldVd);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_isWritePartVd = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isWritePartVd);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_isVleff = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isVleff);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_maskVecGen = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_maskVecGen);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_sew8 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew8);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_sew16 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew16);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_sew32 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew32);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_vpu_sew64 = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew64);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_ftqOffset = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_ftqOffset);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_numLsElem = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_numLsElem);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_lqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_sqIdx_flag = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_flag);
    pkt_str = $sformatf("%sio_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value = 0x%0h ",pkt_str,this.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value);

    return pkt_str;
endfunction:psdisplay

function bit vecissue_agent_agent_xaction::compare(uvm_object rhs, uvm_comparer comparer=null);
    bit super_result;
    vecissue_agent_agent_xaction  rhs_;
    if(!$cast(rhs_, rhs)) begin
        `uvm_fatal(get_type_name(),$sformatf("rhs is not a vecissue_agent_agent_xaction or its extend"))
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

        if(this.io_ooo_to_mem_vecIssue_1_0_ready!=rhs_.io_ooo_to_mem_vecIssue_1_0_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_ready=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_ready=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_ready,rhs_.io_ooo_to_mem_vecIssue_1_0_ready),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_valid!=rhs_.io_ooo_to_mem_vecIssue_1_0_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_valid=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_valid=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_valid,rhs_.io_ooo_to_mem_vecIssue_1_0_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_fuOpType!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_fuOpType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_fuOpType=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_fuOpType=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_fuOpType,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_fuOpType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_src_0!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_src_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_src_0=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_src_0=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_src_0,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_src_0),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_src_1!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_src_1) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_src_1=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_src_1=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_src_1,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_src_1),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_src_2!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_src_2) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_src_2=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_src_2=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_src_2,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_src_2),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_src_3!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_src_3) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_src_3=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_src_3=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_src_3,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_src_3),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vl!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vl) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vl=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vl=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vl,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vl),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_flag!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_flag,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_value!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_value,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_pdest!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_pdest) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_pdest=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_pdest=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_pdest,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_pdest),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_pdestVl!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_pdestVl) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_pdestVl=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_pdestVl=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_pdestVl,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_pdestVl),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vecWen!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vecWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vecWen=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vecWen=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vecWen,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vecWen),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_v0Wen!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_v0Wen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_v0Wen=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_v0Wen=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_v0Wen,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_v0Wen),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vlWen!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vlWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vlWen=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vlWen=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vlWen,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vlWen),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vill!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vill) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vill=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vill=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vill,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vill),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vma!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vma) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vma=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vma=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vma,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vma),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vta!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vta) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vta=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vta=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vta,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vta),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vsew),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vlmul),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVill!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVill) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVill=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVill=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVill,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVill),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVma!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVma) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVma=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVma=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVma,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVma),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVta!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVta) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVta=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVta=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVta,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVta),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVsew!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVsew) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVsew=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVsew=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVsew,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVsew),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVlmul!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVlmul) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVlmul=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVlmul=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVlmul,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_specVlmul),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vm!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vm) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vm=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vm=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vm,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vm),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vstart!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vstart) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vstart=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vstart=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vstart,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vstart),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_frm!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_frm) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_frm=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_frm=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_frm,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_frm),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFpToVecInst!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFpToVecInst) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFpToVecInst=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFpToVecInst=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFpToVecInst,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFpToVecInst),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP32Instr!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP32Instr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP32Instr=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP32Instr=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP32Instr,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP32Instr),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP64Instr!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP64Instr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP64Instr=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP64Instr=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP64Instr,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFP64Instr),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isReduction!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isReduction) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isReduction=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isReduction=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isReduction,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isReduction),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_2!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_2) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_2=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_2=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_2,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_2),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_4!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_4) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_4=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_4=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_4,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_4),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_8!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_8) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_8=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_8=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_8,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_fpu_isFoldTo1_8),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vxrm!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vxrm) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vxrm=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vxrm=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vxrm,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vxrm),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vuopIdx),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_lastUop),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vmask!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vmask) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vmask=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vmask=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vmask,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_vmask),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_nf!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_nf) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_nf=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_nf=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_nf,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_nf),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_veew!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_veew) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_veew=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_veew=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_veew,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_veew),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isReverse!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isReverse) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isReverse=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isReverse=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isReverse,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isReverse),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isExt!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isExt) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isExt=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isExt=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isExt,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isExt),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isNarrow!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isNarrow) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isNarrow=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isNarrow=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isNarrow,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isNarrow),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDstMask!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDstMask) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDstMask=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDstMask=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDstMask,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDstMask),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isOpMask!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isOpMask) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isOpMask=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isOpMask=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isOpMask,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isOpMask),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isMove!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isMove) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isMove=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isMove=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isMove,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isMove),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDependOldVd!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDependOldVd) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDependOldVd=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDependOldVd=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDependOldVd,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isDependOldVd),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isWritePartVd!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isWritePartVd) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isWritePartVd=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isWritePartVd=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isWritePartVd,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isWritePartVd),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isVleff!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isVleff) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isVleff=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isVleff=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isVleff,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_isVleff),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_maskVecGen!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_maskVecGen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_maskVecGen=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_maskVecGen=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_maskVecGen,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_maskVecGen),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew8!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew8) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew8=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew8=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew8,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew8),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew16!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew16) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew16=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew16=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew16,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew16),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew32!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew32) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew32=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew32=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew32,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew32),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew64!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew64) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew64=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew64=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew64,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_vpu_sew64),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_flag!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_flag=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_flag,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_value!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_value=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_value,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_ftqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_ftqOffset!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_ftqOffset) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_ftqOffset=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_ftqOffset=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_ftqOffset,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_ftqOffset),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_numLsElem!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_numLsElem) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_numLsElem=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_numLsElem=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_numLsElem,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_numLsElem),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_flag!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_flag=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_flag,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_flag,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value!=rhs_.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value,rhs_.io_ooo_to_mem_vecIssue_1_0_bits_sqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_ready!=rhs_.io_ooo_to_mem_vecIssue_0_0_ready) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_ready=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_ready=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_ready,rhs_.io_ooo_to_mem_vecIssue_0_0_ready),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_valid!=rhs_.io_ooo_to_mem_vecIssue_0_0_valid) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_valid=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_valid=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_valid,rhs_.io_ooo_to_mem_vecIssue_0_0_valid),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_fuType!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_fuType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_fuType=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_fuType=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_fuType,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_fuType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_fuOpType!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_fuOpType) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_fuOpType=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_fuOpType=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_fuOpType,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_fuOpType),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_src_0!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_src_0) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_src_0=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_src_0=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_src_0,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_src_0),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_src_1!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_src_1) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_src_1=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_src_1=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_src_1,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_src_1),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_src_2!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_src_2) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_src_2=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_src_2=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_src_2,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_src_2),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_src_3!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_src_3) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_src_3=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_src_3=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_src_3,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_src_3),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vl!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vl) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vl=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vl=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vl,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vl),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_flag!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_flag=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_flag,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_value!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_value=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_value=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_value,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_robIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_pdest!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_pdest) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_pdest=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_pdest=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_pdest,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_pdest),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_pdestVl!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_pdestVl) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_pdestVl=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_pdestVl=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_pdestVl,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_pdestVl),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vecWen!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vecWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vecWen=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vecWen=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vecWen,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vecWen),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_v0Wen!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_v0Wen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_v0Wen=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_v0Wen=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_v0Wen,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_v0Wen),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vlWen!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vlWen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vlWen=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vlWen=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vlWen,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vlWen),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vill!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vill) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vill=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vill=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vill,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vill),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vma!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vma) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vma=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vma=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vma,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vma),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vta!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vta) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vta=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vta=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vta,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vta),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vsew),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vlmul),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVill!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVill) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVill=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVill=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVill,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVill),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVma!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVma) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVma=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVma=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVma,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVma),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVta!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVta) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVta=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVta=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVta,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVta),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVsew!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVsew) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVsew=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVsew=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVsew,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVsew),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVlmul!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVlmul) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVlmul=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVlmul=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVlmul,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_specVlmul),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vm!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vm) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vm=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vm=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vm,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vm),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vstart!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vstart) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vstart=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vstart=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vstart,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vstart),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_frm!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_frm) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_frm=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_frm=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_frm,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_frm),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFpToVecInst!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFpToVecInst) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFpToVecInst=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFpToVecInst=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFpToVecInst,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFpToVecInst),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP32Instr!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP32Instr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP32Instr=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP32Instr=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP32Instr,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP32Instr),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP64Instr!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP64Instr) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP64Instr=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP64Instr=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP64Instr,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFP64Instr),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isReduction!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isReduction) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isReduction=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isReduction=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isReduction,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isReduction),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_2!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_2) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_2=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_2=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_2,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_2),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_4!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_4) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_4=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_4=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_4,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_4),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_8!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_8) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_8=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_8=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_8,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_fpu_isFoldTo1_8),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vxrm!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vxrm) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vxrm=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vxrm=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vxrm,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vxrm),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vuopIdx),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_lastUop),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vmask!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vmask) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vmask=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vmask=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vmask,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_vmask),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_nf!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_nf) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_nf=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_nf=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_nf,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_nf),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_veew!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_veew) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_veew=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_veew=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_veew,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_veew),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isReverse!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isReverse) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isReverse=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isReverse=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isReverse,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isReverse),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isExt!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isExt) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isExt=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isExt=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isExt,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isExt),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isNarrow!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isNarrow) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isNarrow=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isNarrow=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isNarrow,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isNarrow),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDstMask!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDstMask) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDstMask=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDstMask=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDstMask,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDstMask),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isOpMask!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isOpMask) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isOpMask=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isOpMask=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isOpMask,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isOpMask),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isMove!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isMove) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isMove=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isMove=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isMove,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isMove),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDependOldVd!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDependOldVd) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDependOldVd=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDependOldVd=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDependOldVd,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isDependOldVd),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isWritePartVd!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isWritePartVd) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isWritePartVd=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isWritePartVd=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isWritePartVd,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isWritePartVd),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isVleff!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isVleff) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isVleff=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isVleff=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isVleff,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_isVleff),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_maskVecGen!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_maskVecGen) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_maskVecGen=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_maskVecGen=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_maskVecGen,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_maskVecGen),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew8!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew8) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew8=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew8=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew8,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew8),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew16!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew16) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew16=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew16=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew16,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew16),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew32!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew32) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew32=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew32=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew32,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew32),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew64!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew64) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew64=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew64=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew64,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_vpu_sew64),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_flag!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_flag=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_flag,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_value!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_value=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_value,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_ftqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_ftqOffset!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_ftqOffset) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_ftqOffset=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_ftqOffset=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_ftqOffset,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_ftqOffset),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_numLsElem!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_numLsElem) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_numLsElem=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_numLsElem=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_numLsElem,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_numLsElem),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_flag!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_flag=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_flag,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_lqIdx_value),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_flag!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_flag) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_flag=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_flag=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_flag,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_flag),UVM_NONE)
        end

        if(this.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value!=rhs_.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value) begin
            super_result = 0;
            `uvm_info(get_type_name(),$sformatf("compare fail for this.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value=0x%0h while the rhs_.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value=0x%0h",this.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value,rhs_.io_ooo_to_mem_vecIssue_0_0_bits_sqIdx_value),UVM_NONE)
        end

    end
    return super_result;
endfunction:compare

`endif
