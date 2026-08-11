//=========================================================
//File name    : io_mem_to_ooo_vec_wb_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_vec_wb_agent_agent_interface
//Discribution : io_mem_to_ooo_vec_wb_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_VEC_WB_AGENT_AGENT_INTERFACE__SV
`define IO_MEM_TO_OOO_VEC_WB_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface io_mem_to_ooo_vec_wb_agent_agent_interface  (input bit clk,input bit rst_n);


    logic [127:0] io_mem_to_ooo_writebackVldu_0_bits_data;
    logic io_mem_to_ooo_writebackVldu_0_bits_debug_isMMIO;
    logic io_mem_to_ooo_writebackVldu_0_bits_debug_isNCIO;
    logic io_mem_to_ooo_writebackVldu_0_bits_debug_isPerfCnt;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_13;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_15;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_19;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_21;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_23;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_4;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_5;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_6;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_7;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_flushPipe;
    logic [8:0] io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType;
    logic [7:0] io_mem_to_ooo_writebackVldu_0_bits_uop_pdest;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_replayInst;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_flag;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_value;
    logic [3:0] io_mem_to_ooo_writebackVldu_0_bits_uop_trigger;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_v0Wen;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_vecWen;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_vlWen;
    logic [2:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_nf;
    logic [1:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_veew;
    logic [7:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vl;
    logic [2:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vlmul;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vm;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vma;
    logic [127:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vmask;
    logic [1:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vsew;
    logic [7:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vstart;
    logic io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vta;
    logic [6:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vuopIdx;
    logic [2:0] io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField;
    logic io_mem_to_ooo_writebackVldu_0_valid;
    logic [127:0] io_mem_to_ooo_writebackVldu_1_bits_data;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_13;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_15;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_19;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_21;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_23;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_4;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_5;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_6;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_7;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_flushPipe;
    logic [8:0] io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType;
    logic [7:0] io_mem_to_ooo_writebackVldu_1_bits_uop_pdest;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_replayInst;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_flag;
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_value;
    logic [3:0] io_mem_to_ooo_writebackVldu_1_bits_uop_trigger;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_v0Wen;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_vecWen;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_vlWen;
    logic [2:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_nf;
    logic [1:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_veew;
    logic [7:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vl;
    logic [2:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vlmul;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vm;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vma;
    logic [127:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vmask;
    logic [1:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vsew;
    logic [7:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vstart;
    logic io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vta;
    logic [6:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vuopIdx;
    logic [2:0] io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField;
    logic io_mem_to_ooo_writebackVldu_1_valid;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif

        input  io_mem_to_ooo_writebackVldu_0_bits_data;
        input  io_mem_to_ooo_writebackVldu_0_bits_debug_isMMIO;
        input  io_mem_to_ooo_writebackVldu_0_bits_debug_isNCIO;
        input  io_mem_to_ooo_writebackVldu_0_bits_debug_isPerfCnt;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_13;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_15;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_19;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_21;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_23;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_4;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_5;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_6;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_7;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_flushPipe;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_pdest;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_replayInst;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_flag;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_value;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_trigger;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_v0Wen;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vecWen;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vlWen;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_nf;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_veew;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vl;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vlmul;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vm;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vma;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vmask;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vsew;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vstart;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vta;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vuopIdx;
        input  io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField;
        input  io_mem_to_ooo_writebackVldu_0_valid;
        input  io_mem_to_ooo_writebackVldu_1_bits_data;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_13;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_15;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_19;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_21;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_23;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_4;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_5;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_6;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_7;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_flushPipe;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_pdest;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_replayInst;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_flag;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_value;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_trigger;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_v0Wen;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vecWen;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vlWen;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_nf;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_veew;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vl;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vlmul;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vm;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vma;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vmask;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vsew;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vstart;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vta;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vuopIdx;
        input  io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField;
        input  io_mem_to_ooo_writebackVldu_1_valid;
    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif

        input  io_mem_to_ooo_writebackVldu_0_bits_data;
        input  io_mem_to_ooo_writebackVldu_0_bits_debug_isMMIO;
        input  io_mem_to_ooo_writebackVldu_0_bits_debug_isNCIO;
        input  io_mem_to_ooo_writebackVldu_0_bits_debug_isPerfCnt;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_13;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_15;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_19;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_21;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_23;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_3;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_4;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_5;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_6;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_exceptionVec_7;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_flushPipe;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_pdest;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_replayInst;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_flag;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_value;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_trigger;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_v0Wen;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vecWen;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vlWen;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_nf;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_veew;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vl;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vlmul;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vm;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vma;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vmask;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vsew;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vstart;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vta;
        input  io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vuopIdx;
        input  io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField;
        input  io_mem_to_ooo_writebackVldu_0_valid;
        input  io_mem_to_ooo_writebackVldu_1_bits_data;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_13;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_15;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_19;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_21;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_23;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_3;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_4;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_5;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_6;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_exceptionVec_7;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_flushPipe;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_pdest;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_replayInst;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_flag;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_value;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_trigger;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_v0Wen;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vecWen;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vlWen;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_nf;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_veew;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vl;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vlmul;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vm;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vma;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vmask;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vsew;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vstart;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vta;
        input  io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vuopIdx;
        input  io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField;
        input  io_mem_to_ooo_writebackVldu_1_valid;
    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:io_mem_to_ooo_vec_wb_agent_agent_interface

`endif
