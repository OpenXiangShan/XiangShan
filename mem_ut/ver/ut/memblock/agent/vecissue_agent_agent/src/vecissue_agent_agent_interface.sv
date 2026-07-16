//=========================================================
//File name    : vecissue_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : vecissue_agent_agent_interface
//Discribution : vecissue_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef VECISSUE_AGENT_AGENT_INTERFACE__SV
`define VECISSUE_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface vecissue_agent_agent_interface  (input bit clk,input bit rst_n);


    logic io_ooo_to_mem_isStoreException;
    logic [4:0] io_ooo_to_mem_issueVldu_0_bits_flowNum;
    logic io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay;
    logic [127:0] io_ooo_to_mem_issueVldu_0_bits_src_0;
    logic [127:0] io_ooo_to_mem_issueVldu_0_bits_src_1;
    logic [127:0] io_ooo_to_mem_issueVldu_0_bits_src_2;
    logic [127:0] io_ooo_to_mem_issueVldu_0_bits_src_3;
    logic [127:0] io_ooo_to_mem_issueVldu_0_bits_src_4;
    logic [`MEMBLOCK_DUT_FTQ_OFFSET_W-1:0] io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag;
    logic [`MEMBLOCK_DUT_FTQ_PTR_VALUE_W-1:0] io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value;
    logic [8:0] io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType;
    logic [`MEMBLOCK_DUT_FUTYPE_W-1:0] io_ooo_to_mem_issueVldu_0_bits_uop_fuType;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag;
    logic [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value;
    logic [7:0] io_ooo_to_mem_issueVldu_0_bits_uop_pdest;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag;
    logic [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_vecWen;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_vlWen;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop;
    logic [2:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf;
    logic [1:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew;
    logic [2:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma;
    logic [127:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask;
    logic [1:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew;
    logic [7:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta;
    logic [6:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx;
    logic [15:0] io_ooo_to_mem_issueVldu_0_bits_vecReplayMask;
    logic [3:0] io_ooo_to_mem_issueVldu_0_bits_vecReplayMbIdx;
    logic io_ooo_to_mem_issueVldu_0_ready;
    logic io_ooo_to_mem_issueVldu_0_valid;
    logic [4:0] io_ooo_to_mem_issueVldu_1_bits_flowNum;
    logic io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay;
    logic [127:0] io_ooo_to_mem_issueVldu_1_bits_src_0;
    logic [127:0] io_ooo_to_mem_issueVldu_1_bits_src_1;
    logic [127:0] io_ooo_to_mem_issueVldu_1_bits_src_2;
    logic [127:0] io_ooo_to_mem_issueVldu_1_bits_src_3;
    logic [127:0] io_ooo_to_mem_issueVldu_1_bits_src_4;
    logic [`MEMBLOCK_DUT_FTQ_OFFSET_W-1:0] io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag;
    logic [`MEMBLOCK_DUT_FTQ_PTR_VALUE_W-1:0] io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value;
    logic [8:0] io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag;
    logic [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value;
    logic [7:0] io_ooo_to_mem_issueVldu_1_bits_uop_pdest;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag;
    logic [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_vecWen;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_vlWen;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop;
    logic [2:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf;
    logic [1:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew;
    logic [2:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma;
    logic [127:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask;
    logic [1:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew;
    logic [7:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta;
    logic [6:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx;
    logic [15:0] io_ooo_to_mem_issueVldu_1_bits_vecReplayMask;
    logic [3:0] io_ooo_to_mem_issueVldu_1_bits_vecReplayMbIdx;
    logic io_ooo_to_mem_issueVldu_1_ready;
    logic io_ooo_to_mem_issueVldu_1_valid;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif

        output io_ooo_to_mem_isStoreException;
        output io_ooo_to_mem_issueVldu_0_bits_flowNum;
        output io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay;
        output io_ooo_to_mem_issueVldu_0_bits_src_0;
        output io_ooo_to_mem_issueVldu_0_bits_src_1;
        output io_ooo_to_mem_issueVldu_0_bits_src_2;
        output io_ooo_to_mem_issueVldu_0_bits_src_3;
        output io_ooo_to_mem_issueVldu_0_bits_src_4;
        output io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset;
        output io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag;
        output io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value;
        output io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType;
        output io_ooo_to_mem_issueVldu_0_bits_uop_fuType;
        output io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag;
        output io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value;
        output io_ooo_to_mem_issueVldu_0_bits_uop_pdest;
        output io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag;
        output io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value;
        output io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag;
        output io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value;
        output io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen;
        output io_ooo_to_mem_issueVldu_0_bits_uop_vecWen;
        output io_ooo_to_mem_issueVldu_0_bits_uop_vlWen;
        output io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff;
        output io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop;
        output io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf;
        output io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew;
        output io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul;
        output io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm;
        output io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma;
        output io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask;
        output io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew;
        output io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart;
        output io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta;
        output io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx;
        output io_ooo_to_mem_issueVldu_0_bits_vecReplayMask;
        output io_ooo_to_mem_issueVldu_0_bits_vecReplayMbIdx;
        input  io_ooo_to_mem_issueVldu_0_ready;
        output io_ooo_to_mem_issueVldu_0_valid;
        output io_ooo_to_mem_issueVldu_1_bits_flowNum;
        output io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay;
        output io_ooo_to_mem_issueVldu_1_bits_src_0;
        output io_ooo_to_mem_issueVldu_1_bits_src_1;
        output io_ooo_to_mem_issueVldu_1_bits_src_2;
        output io_ooo_to_mem_issueVldu_1_bits_src_3;
        output io_ooo_to_mem_issueVldu_1_bits_src_4;
        output io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset;
        output io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag;
        output io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value;
        output io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType;
        output io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag;
        output io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value;
        output io_ooo_to_mem_issueVldu_1_bits_uop_pdest;
        output io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag;
        output io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value;
        output io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag;
        output io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value;
        output io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen;
        output io_ooo_to_mem_issueVldu_1_bits_uop_vecWen;
        output io_ooo_to_mem_issueVldu_1_bits_uop_vlWen;
        output io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff;
        output io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop;
        output io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf;
        output io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew;
        output io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul;
        output io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm;
        output io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma;
        output io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask;
        output io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew;
        output io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart;
        output io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta;
        output io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx;
        output io_ooo_to_mem_issueVldu_1_bits_vecReplayMask;
        output io_ooo_to_mem_issueVldu_1_bits_vecReplayMbIdx;
        input  io_ooo_to_mem_issueVldu_1_ready;
        output io_ooo_to_mem_issueVldu_1_valid;
    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif

        input  io_ooo_to_mem_isStoreException;
        input  io_ooo_to_mem_issueVldu_0_bits_flowNum;
        input  io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay;
        input  io_ooo_to_mem_issueVldu_0_bits_src_0;
        input  io_ooo_to_mem_issueVldu_0_bits_src_1;
        input  io_ooo_to_mem_issueVldu_0_bits_src_2;
        input  io_ooo_to_mem_issueVldu_0_bits_src_3;
        input  io_ooo_to_mem_issueVldu_0_bits_src_4;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_fuType;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_pdest;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_vecWen;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_vlWen;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta;
        input  io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx;
        input  io_ooo_to_mem_issueVldu_0_bits_vecReplayMask;
        input  io_ooo_to_mem_issueVldu_0_bits_vecReplayMbIdx;
        input  io_ooo_to_mem_issueVldu_0_ready;
        input  io_ooo_to_mem_issueVldu_0_valid;
        input  io_ooo_to_mem_issueVldu_1_bits_flowNum;
        input  io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay;
        input  io_ooo_to_mem_issueVldu_1_bits_src_0;
        input  io_ooo_to_mem_issueVldu_1_bits_src_1;
        input  io_ooo_to_mem_issueVldu_1_bits_src_2;
        input  io_ooo_to_mem_issueVldu_1_bits_src_3;
        input  io_ooo_to_mem_issueVldu_1_bits_src_4;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_pdest;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_vecWen;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_vlWen;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta;
        input  io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx;
        input  io_ooo_to_mem_issueVldu_1_bits_vecReplayMask;
        input  io_ooo_to_mem_issueVldu_1_bits_vecReplayMbIdx;
        input  io_ooo_to_mem_issueVldu_1_ready;
        input  io_ooo_to_mem_issueVldu_1_valid;
    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:vecissue_agent_agent_interface

`endif
