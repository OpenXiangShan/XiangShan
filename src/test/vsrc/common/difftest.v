/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

`define DIFFTEST_DPIC_FUNC_NAME(name) \
  v_difftest_``name

`define DIFFTEST_DPIC_FUNC_DECL(name) \
  import "DPI-C" function void `DIFFTEST_DPIC_FUNC_NAME(name)

`define DIFFTEST_MOD_NAME(name)    \
  Difftest``name

`define DIFFTEST_MOD_DECL(name)    \
  module `DIFFTEST_MOD_NAME(name)

`define DIFFTEST_MOD_DPIC_CALL_BEGIN(name) \
  always @(posedge clock) begin            \
    `DIFFTEST_DPIC_FUNC_NAME(name)

`define DIFFTEST_MOD_DPIC_CALL_BEGIN_WITH_EN(enable, name) \
  always @(posedge clock) begin                            \
    if (enable) begin                                      \
      `DIFFTEST_DPIC_FUNC_NAME(name)

`define DIFFTEST_MOD_DPIC_CALL_END(name) \
  ; end

`define DIFFTEST_MOD_DPIC_CALL_END_WITH_EN(name) \
  ; end end

`define DPIC_ARG_BIT  input bit
`define DPIC_ARG_BYTE input byte
`define DPIC_ARG_INT  input int
`define DPIC_ARG_LONG input longint

// DifftestArchEvent
`DIFFTEST_DPIC_FUNC_DECL(ArchEvent) (
  `DPIC_ARG_BYTE coreid,
  `DPIC_ARG_INT  intrNo,
  `DPIC_ARG_INT  cause,
  `DPIC_ARG_LONG exceptionPC
);
`DIFFTEST_MOD_DECL(ArchEvent) (
  input        clock,
  input [ 7:0] coreid,
  input [31:0] intrNO,
  input [31:0] cause,
  input [63:0] exceptionPC
);
  `DIFFTEST_MOD_DPIC_CALL_BEGIN(ArchEvent) (
    coreid, intrNO, cause, exceptionPC
  ) `DIFFTEST_MOD_DPIC_CALL_END(ArchEvent)
endmodule

// DifftestInstrCommit
`DIFFTEST_DPIC_FUNC_DECL(InstrCommit) (
  `DPIC_ARG_BYTE coreid,
  `DPIC_ARG_BYTE index,
  `DPIC_ARG_BIT  valid,
  `DPIC_ARG_LONG pc,
  `DPIC_ARG_INT  instr,
  `DPIC_ARG_BIT  skip,
  `DPIC_ARG_BIT  isRVC,
  `DPIC_ARG_BIT  scFailed,
  `DPIC_ARG_BIT  wen,
  `DPIC_ARG_BYTE wdest,
  `DPIC_ARG_LONG wdata
);
`DIFFTEST_MOD_DECL(InstrCommit)(
  input        clock,
  input [ 7:0] coreid,
  input [ 7:0] index,
  input        valid,
  input [63:0] pc,
  input [31:0] instr,
  input        skip,
  input        isRVC,
  input        scFailed,
  input        wen,
  input [ 7:0] wdest,
  input [63:0] wdata
);
  `DIFFTEST_MOD_DPIC_CALL_BEGIN_WITH_EN(valid, InstrCommit) (
    coreid, index,
    valid, pc, instr, skip, isRVC, scFailed, wen, wdest, wdata
  ) `DIFFTEST_MOD_DPIC_CALL_END_WITH_EN(InstrCommit)
endmodule

// DifftesTrapEvent
`DIFFTEST_DPIC_FUNC_DECL(TrapEvent) (
  `DPIC_ARG_BYTE coreid,
  `DPIC_ARG_BIT  valid,
  `DPIC_ARG_BYTE code,
  `DPIC_ARG_LONG pc,
  `DPIC_ARG_LONG cycleCnt,
  `DPIC_ARG_LONG instrCnt
);
`DIFFTEST_MOD_DECL(TrapEvent)(
  input        clock,
  input [ 7:0] coreid,
  input        valid,
  input [ 2:0] code,
  input [63:0] pc,
  input [63:0] cycleCnt,
  input [63:0] instrCnt
);
  `DIFFTEST_MOD_DPIC_CALL_BEGIN(TrapEvent) (
    coreid, valid, {5'd0, code}, pc, cycleCnt, instrCnt
  ) `DIFFTEST_MOD_DPIC_CALL_END(TrapEvent)
endmodule

// DifftestCSRState
`DIFFTEST_DPIC_FUNC_DECL(CSRState) (
  `DPIC_ARG_BYTE coreid,
  `DPIC_ARG_BYTE priviledgeMode,
  `DPIC_ARG_LONG mstatus,
  `DPIC_ARG_LONG sstatus,
  `DPIC_ARG_LONG mepc,
  `DPIC_ARG_LONG sepc,
  `DPIC_ARG_LONG mtval,
  `DPIC_ARG_LONG stval,
  `DPIC_ARG_LONG mtvec,
  `DPIC_ARG_LONG stvec,
  `DPIC_ARG_LONG mcause,
  `DPIC_ARG_LONG scause,
  `DPIC_ARG_LONG satp,
  `DPIC_ARG_LONG mip,
  `DPIC_ARG_LONG mie,
  `DPIC_ARG_LONG mscratch,
  `DPIC_ARG_LONG sscratch,
  `DPIC_ARG_LONG mideleg,
  `DPIC_ARG_LONG medeleg
);
`DIFFTEST_MOD_DECL(CSRState)(
  input        clock,
  input [ 7:0] coreid,
  input [ 1:0] priviledgeMode,
  input [63:0] mstatus,
  input [63:0] sstatus,
  input [63:0] mepc,
  input [63:0] sepc,
  input [63:0] mtval,
  input [63:0] stval,
  input [63:0] mtvec,
  input [63:0] stvec,
  input [63:0] mcause,
  input [63:0] scause,
  input [63:0] satp,
  input [63:0] mip,
  input [63:0] mie,
  input [63:0] mscratch,
  input [63:0] sscratch,
  input [63:0] mideleg,
  input [63:0] medeleg
);
  `DIFFTEST_MOD_DPIC_CALL_BEGIN(CSRState) (
    coreid, {6'd0, priviledgeMode},
    mstatus, sstatus, mepc, sepc, mtval, stval, mtvec, stvec,
    mcause, scause, satp, mip, mie, mscratch, sscratch, mideleg, medeleg
  ) `DIFFTEST_MOD_DPIC_CALL_END(CSRState)
endmodule

// DifftestArchIntRegState
`DIFFTEST_DPIC_FUNC_DECL(ArchIntRegState) (
  `DPIC_ARG_BYTE coreid,
  `DPIC_ARG_LONG gpr_0,
  `DPIC_ARG_LONG gpr_1,
  `DPIC_ARG_LONG gpr_2,
  `DPIC_ARG_LONG gpr_3,
  `DPIC_ARG_LONG gpr_4,
  `DPIC_ARG_LONG gpr_5,
  `DPIC_ARG_LONG gpr_6,
  `DPIC_ARG_LONG gpr_7,
  `DPIC_ARG_LONG gpr_8,
  `DPIC_ARG_LONG gpr_9,
  `DPIC_ARG_LONG gpr_10,
  `DPIC_ARG_LONG gpr_11,
  `DPIC_ARG_LONG gpr_12,
  `DPIC_ARG_LONG gpr_13,
  `DPIC_ARG_LONG gpr_14,
  `DPIC_ARG_LONG gpr_15,
  `DPIC_ARG_LONG gpr_16,
  `DPIC_ARG_LONG gpr_17,
  `DPIC_ARG_LONG gpr_18,
  `DPIC_ARG_LONG gpr_19,
  `DPIC_ARG_LONG gpr_20,
  `DPIC_ARG_LONG gpr_21,
  `DPIC_ARG_LONG gpr_22,
  `DPIC_ARG_LONG gpr_23,
  `DPIC_ARG_LONG gpr_24,
  `DPIC_ARG_LONG gpr_25,
  `DPIC_ARG_LONG gpr_26,
  `DPIC_ARG_LONG gpr_27,
  `DPIC_ARG_LONG gpr_28,
  `DPIC_ARG_LONG gpr_29,
  `DPIC_ARG_LONG gpr_30,
  `DPIC_ARG_LONG gpr_31
);
`DIFFTEST_MOD_DECL(ArchIntRegState)(
  input         clock,
  input [ 7:0]  coreid,
  input [63:0]  gpr_0,
  input [63:0]  gpr_1,
  input [63:0]  gpr_2,
  input [63:0]  gpr_3,
  input [63:0]  gpr_4,
  input [63:0]  gpr_5,
  input [63:0]  gpr_6,
  input [63:0]  gpr_7,
  input [63:0]  gpr_8,
  input [63:0]  gpr_9,
  input [63:0]  gpr_10,
  input [63:0]  gpr_11,
  input [63:0]  gpr_12,
  input [63:0]  gpr_13,
  input [63:0]  gpr_14,
  input [63:0]  gpr_15,
  input [63:0]  gpr_16,
  input [63:0]  gpr_17,
  input [63:0]  gpr_18,
  input [63:0]  gpr_19,
  input [63:0]  gpr_20,
  input [63:0]  gpr_21,
  input [63:0]  gpr_22,
  input [63:0]  gpr_23,
  input [63:0]  gpr_24,
  input [63:0]  gpr_25,
  input [63:0]  gpr_26,
  input [63:0]  gpr_27,
  input [63:0]  gpr_28,
  input [63:0]  gpr_29,
  input [63:0]  gpr_30,
  input [63:0]  gpr_31
);
  `DIFFTEST_MOD_DPIC_CALL_BEGIN(ArchIntRegState) (
    coreid,
    gpr_0,  gpr_1,  gpr_2,  gpr_3,  gpr_4,  gpr_5,  gpr_6,  gpr_7,
    gpr_8,  gpr_9,  gpr_10, gpr_11, gpr_12, gpr_13, gpr_14, gpr_15,
    gpr_16, gpr_17, gpr_18, gpr_19, gpr_20, gpr_21, gpr_22, gpr_23,
    gpr_24, gpr_25, gpr_26, gpr_27, gpr_28, gpr_29, gpr_30, gpr_31
  ) `DIFFTEST_MOD_DPIC_CALL_END(ArchIntRegState)
endmodule

// DifftestArchFpRegState
`DIFFTEST_DPIC_FUNC_DECL(ArchFpRegState) (
  `DPIC_ARG_BYTE coreid,
  `DPIC_ARG_LONG fpr_0,
  `DPIC_ARG_LONG fpr_1,
  `DPIC_ARG_LONG fpr_2,
  `DPIC_ARG_LONG fpr_3,
  `DPIC_ARG_LONG fpr_4,
  `DPIC_ARG_LONG fpr_5,
  `DPIC_ARG_LONG fpr_6,
  `DPIC_ARG_LONG fpr_7,
  `DPIC_ARG_LONG fpr_8,
  `DPIC_ARG_LONG fpr_9,
  `DPIC_ARG_LONG fpr_10,
  `DPIC_ARG_LONG fpr_11,
  `DPIC_ARG_LONG fpr_12,
  `DPIC_ARG_LONG fpr_13,
  `DPIC_ARG_LONG fpr_14,
  `DPIC_ARG_LONG fpr_15,
  `DPIC_ARG_LONG fpr_16,
  `DPIC_ARG_LONG fpr_17,
  `DPIC_ARG_LONG fpr_18,
  `DPIC_ARG_LONG fpr_19,
  `DPIC_ARG_LONG fpr_20,
  `DPIC_ARG_LONG fpr_21,
  `DPIC_ARG_LONG fpr_22,
  `DPIC_ARG_LONG fpr_23,
  `DPIC_ARG_LONG fpr_24,
  `DPIC_ARG_LONG fpr_25,
  `DPIC_ARG_LONG fpr_26,
  `DPIC_ARG_LONG fpr_27,
  `DPIC_ARG_LONG fpr_28,
  `DPIC_ARG_LONG fpr_29,
  `DPIC_ARG_LONG fpr_30,
  `DPIC_ARG_LONG fpr_31
);
`DIFFTEST_MOD_DECL(ArchFpRegState)(
  input         clock,
  input [ 7:0]  coreid,
  input [63:0]  fpr_0,
  input [63:0]  fpr_1,
  input [63:0]  fpr_2,
  input [63:0]  fpr_3,
  input [63:0]  fpr_4,
  input [63:0]  fpr_5,
  input [63:0]  fpr_6,
  input [63:0]  fpr_7,
  input [63:0]  fpr_8,
  input [63:0]  fpr_9,
  input [63:0]  fpr_10,
  input [63:0]  fpr_11,
  input [63:0]  fpr_12,
  input [63:0]  fpr_13,
  input [63:0]  fpr_14,
  input [63:0]  fpr_15,
  input [63:0]  fpr_16,
  input [63:0]  fpr_17,
  input [63:0]  fpr_18,
  input [63:0]  fpr_19,
  input [63:0]  fpr_20,
  input [63:0]  fpr_21,
  input [63:0]  fpr_22,
  input [63:0]  fpr_23,
  input [63:0]  fpr_24,
  input [63:0]  fpr_25,
  input [63:0]  fpr_26,
  input [63:0]  fpr_27,
  input [63:0]  fpr_28,
  input [63:0]  fpr_29,
  input [63:0]  fpr_30,
  input [63:0]  fpr_31
);
  `DIFFTEST_MOD_DPIC_CALL_BEGIN(ArchFpRegState) (
    coreid,
    fpr_0,  fpr_1,  fpr_2,  fpr_3,  fpr_4,  fpr_5,  fpr_6,  fpr_7,
    fpr_8,  fpr_9,  fpr_10, fpr_11, fpr_12, fpr_13, fpr_14, fpr_15,
    fpr_16, fpr_17, fpr_18, fpr_19, fpr_20, fpr_21, fpr_22, fpr_23,
    fpr_24, fpr_25, fpr_26, fpr_27, fpr_28, fpr_29, fpr_30, fpr_31
  ) `DIFFTEST_MOD_DPIC_CALL_END(ArchFpRegState)
endmodule

// DifftestSbufferEvent
`DIFFTEST_DPIC_FUNC_DECL(SbufferEvent) (
  `DPIC_ARG_BYTE coreid,
  `DPIC_ARG_BIT  sbufferResp,
  `DPIC_ARG_LONG sbufferAddr,
  `DPIC_ARG_BYTE sbufferData_0,
  `DPIC_ARG_BYTE sbufferData_1,
  `DPIC_ARG_BYTE sbufferData_2,
  `DPIC_ARG_BYTE sbufferData_3,
  `DPIC_ARG_BYTE sbufferData_4,
  `DPIC_ARG_BYTE sbufferData_5,
  `DPIC_ARG_BYTE sbufferData_6,
  `DPIC_ARG_BYTE sbufferData_7,
  `DPIC_ARG_BYTE sbufferData_8,
  `DPIC_ARG_BYTE sbufferData_9,
  `DPIC_ARG_BYTE sbufferData_10,
  `DPIC_ARG_BYTE sbufferData_11,
  `DPIC_ARG_BYTE sbufferData_12,
  `DPIC_ARG_BYTE sbufferData_13,
  `DPIC_ARG_BYTE sbufferData_14,
  `DPIC_ARG_BYTE sbufferData_15,
  `DPIC_ARG_BYTE sbufferData_16,
  `DPIC_ARG_BYTE sbufferData_17,
  `DPIC_ARG_BYTE sbufferData_18,
  `DPIC_ARG_BYTE sbufferData_19,
  `DPIC_ARG_BYTE sbufferData_20,
  `DPIC_ARG_BYTE sbufferData_21,
  `DPIC_ARG_BYTE sbufferData_22,
  `DPIC_ARG_BYTE sbufferData_23,
  `DPIC_ARG_BYTE sbufferData_24,
  `DPIC_ARG_BYTE sbufferData_25,
  `DPIC_ARG_BYTE sbufferData_26,
  `DPIC_ARG_BYTE sbufferData_27,
  `DPIC_ARG_BYTE sbufferData_28,
  `DPIC_ARG_BYTE sbufferData_29,
  `DPIC_ARG_BYTE sbufferData_30,
  `DPIC_ARG_BYTE sbufferData_31,
  `DPIC_ARG_BYTE sbufferData_32,
  `DPIC_ARG_BYTE sbufferData_33,
  `DPIC_ARG_BYTE sbufferData_34,
  `DPIC_ARG_BYTE sbufferData_35,
  `DPIC_ARG_BYTE sbufferData_36,
  `DPIC_ARG_BYTE sbufferData_37,
  `DPIC_ARG_BYTE sbufferData_38,
  `DPIC_ARG_BYTE sbufferData_39,
  `DPIC_ARG_BYTE sbufferData_40,
  `DPIC_ARG_BYTE sbufferData_41,
  `DPIC_ARG_BYTE sbufferData_42,
  `DPIC_ARG_BYTE sbufferData_43,
  `DPIC_ARG_BYTE sbufferData_44,
  `DPIC_ARG_BYTE sbufferData_45,
  `DPIC_ARG_BYTE sbufferData_46,
  `DPIC_ARG_BYTE sbufferData_47,
  `DPIC_ARG_BYTE sbufferData_48,
  `DPIC_ARG_BYTE sbufferData_49,
  `DPIC_ARG_BYTE sbufferData_50,
  `DPIC_ARG_BYTE sbufferData_51,
  `DPIC_ARG_BYTE sbufferData_52,
  `DPIC_ARG_BYTE sbufferData_53,
  `DPIC_ARG_BYTE sbufferData_54,
  `DPIC_ARG_BYTE sbufferData_55,
  `DPIC_ARG_BYTE sbufferData_56,
  `DPIC_ARG_BYTE sbufferData_57,
  `DPIC_ARG_BYTE sbufferData_58,
  `DPIC_ARG_BYTE sbufferData_59,
  `DPIC_ARG_BYTE sbufferData_60,
  `DPIC_ARG_BYTE sbufferData_61,
  `DPIC_ARG_BYTE sbufferData_62,
  `DPIC_ARG_BYTE sbufferData_63,
  `DPIC_ARG_LONG sbufferMask
);
`DIFFTEST_MOD_DECL(SbufferEvent)(
  input        clock, 
  input [ 7:0] coreid,
  input        sbufferResp,
  input [63:0] sbufferAddr,
  input [ 7:0] sbufferData_0,
  input [ 7:0] sbufferData_1,
  input [ 7:0] sbufferData_2,
  input [ 7:0] sbufferData_3,
  input [ 7:0] sbufferData_4,
  input [ 7:0] sbufferData_5,
  input [ 7:0] sbufferData_6,
  input [ 7:0] sbufferData_7,
  input [ 7:0] sbufferData_8,
  input [ 7:0] sbufferData_9,
  input [ 7:0] sbufferData_10,
  input [ 7:0] sbufferData_11,
  input [ 7:0] sbufferData_12,
  input [ 7:0] sbufferData_13,
  input [ 7:0] sbufferData_14,
  input [ 7:0] sbufferData_15,
  input [ 7:0] sbufferData_16,
  input [ 7:0] sbufferData_17,
  input [ 7:0] sbufferData_18,
  input [ 7:0] sbufferData_19,
  input [ 7:0] sbufferData_20,
  input [ 7:0] sbufferData_21,
  input [ 7:0] sbufferData_22,
  input [ 7:0] sbufferData_23,
  input [ 7:0] sbufferData_24,
  input [ 7:0] sbufferData_25,
  input [ 7:0] sbufferData_26,
  input [ 7:0] sbufferData_27,
  input [ 7:0] sbufferData_28,
  input [ 7:0] sbufferData_29,
  input [ 7:0] sbufferData_30,
  input [ 7:0] sbufferData_31,
  input [ 7:0] sbufferData_32,
  input [ 7:0] sbufferData_33,
  input [ 7:0] sbufferData_34,
  input [ 7:0] sbufferData_35,
  input [ 7:0] sbufferData_36,
  input [ 7:0] sbufferData_37,
  input [ 7:0] sbufferData_38,
  input [ 7:0] sbufferData_39,
  input [ 7:0] sbufferData_40,
  input [ 7:0] sbufferData_41,
  input [ 7:0] sbufferData_42,
  input [ 7:0] sbufferData_43,
  input [ 7:0] sbufferData_44,
  input [ 7:0] sbufferData_45,
  input [ 7:0] sbufferData_46,
  input [ 7:0] sbufferData_47,
  input [ 7:0] sbufferData_48,
  input [ 7:0] sbufferData_49,
  input [ 7:0] sbufferData_50,
  input [ 7:0] sbufferData_51,
  input [ 7:0] sbufferData_52,
  input [ 7:0] sbufferData_53,
  input [ 7:0] sbufferData_54,
  input [ 7:0] sbufferData_55,
  input [ 7:0] sbufferData_56,
  input [ 7:0] sbufferData_57,
  input [ 7:0] sbufferData_58,
  input [ 7:0] sbufferData_59,
  input [ 7:0] sbufferData_60,
  input [ 7:0] sbufferData_61,
  input [ 7:0] sbufferData_62,
  input [ 7:0] sbufferData_63,
  input [63:0] sbufferMask
);
  `DIFFTEST_MOD_DPIC_CALL_BEGIN_WITH_EN(sbufferResp, SbufferEvent) (
    coreid, sbufferResp, sbufferAddr,
    sbufferData_0,  sbufferData_1,  sbufferData_2,  sbufferData_3,  sbufferData_4,  sbufferData_5,
    sbufferData_6,  sbufferData_7,  sbufferData_8,  sbufferData_9,  sbufferData_10, sbufferData_11,
    sbufferData_12, sbufferData_13, sbufferData_14, sbufferData_15, sbufferData_16, sbufferData_17,
    sbufferData_18, sbufferData_19, sbufferData_20, sbufferData_21, sbufferData_22, sbufferData_23,
    sbufferData_24, sbufferData_25, sbufferData_26, sbufferData_27, sbufferData_28, sbufferData_29,
    sbufferData_30, sbufferData_31, sbufferData_32, sbufferData_33, sbufferData_34, sbufferData_35,
    sbufferData_36, sbufferData_37, sbufferData_38, sbufferData_39, sbufferData_40, sbufferData_41,
    sbufferData_42, sbufferData_43, sbufferData_44, sbufferData_45, sbufferData_46, sbufferData_47,
    sbufferData_48, sbufferData_49, sbufferData_50, sbufferData_51, sbufferData_52, sbufferData_53,
    sbufferData_54, sbufferData_55, sbufferData_56, sbufferData_57, sbufferData_58, sbufferData_59,
    sbufferData_60, sbufferData_61, sbufferData_62, sbufferData_63, sbufferMask
  ) `DIFFTEST_MOD_DPIC_CALL_END_WITH_EN(SbufferEvent)
endmodule

// DifftestStoreEvent
`DIFFTEST_DPIC_FUNC_DECL(StoreEvent) (
  `DPIC_ARG_BYTE coreid,
  `DPIC_ARG_BYTE index,
  `DPIC_ARG_BIT  valid,
  `DPIC_ARG_LONG storeAddr,
  `DPIC_ARG_LONG storeData,
  `DPIC_ARG_BYTE storeMask
);
`DIFFTEST_MOD_DECL(StoreEvent)(
  input        clock,
  input [ 7:0] coreid,
  input [ 7:0] index,
  input        valid,
  input [63:0] storeAddr,
  input [63:0] storeData,
  input [ 7:0] storeMask
);
  `DIFFTEST_MOD_DPIC_CALL_BEGIN_WITH_EN(valid, StoreEvent) (
    coreid, index, valid, storeAddr, storeData, storeMask
  ) `DIFFTEST_MOD_DPIC_CALL_END_WITH_EN(StoreEvent)
endmodule

// DifftestLoadEvent
`DIFFTEST_DPIC_FUNC_DECL(LoadEvent) (
  `DPIC_ARG_BYTE coreid,
  `DPIC_ARG_BYTE index,
  `DPIC_ARG_BIT  valid,
  `DPIC_ARG_LONG paddr,
  `DPIC_ARG_BYTE opType,
  `DPIC_ARG_BYTE fuType
);
`DIFFTEST_MOD_DECL(LoadEvent)(
  input        clock,
  input [ 7:0] coreid,
  input [ 7:0] index,
  input        valid,
  input [63:0] paddr,
  input [ 7:0] opType,
  input [ 7:0] fuType
);
  `DIFFTEST_MOD_DPIC_CALL_BEGIN_WITH_EN(valid, LoadEvent) (
    coreid, index, valid, paddr, opType, fuType
  ) `DIFFTEST_MOD_DPIC_CALL_END_WITH_EN(LoadEvent)
endmodule

// DifftestAtomicEvent
`DIFFTEST_DPIC_FUNC_DECL(AtomicEvent) (
  `DPIC_ARG_BYTE coreid,
  `DPIC_ARG_BIT  atomicResp,
  `DPIC_ARG_LONG atomicAddr,
  `DPIC_ARG_LONG atomicData,
  `DPIC_ARG_BYTE atomicMask,
  `DPIC_ARG_BYTE atomicFuop,
  `DPIC_ARG_LONG atomicOut
);
`DIFFTEST_MOD_DECL(AtomicEvent)(
  input        clock,
  input [ 7:0] coreid,
  input        atomicResp,
  input [63:0] atomicAddr,
  input [63:0] atomicData,
  input [ 7:0] atomicMask,
  input [ 7:0] atomicFuop,
  input [63:0] atomicOut
);
  `DIFFTEST_MOD_DPIC_CALL_BEGIN_WITH_EN(atomicResp, AtomicEvent) (
    coreid, atomicResp, atomicAddr, atomicData, atomicMask, atomicFuop, atomicOut
  ) `DIFFTEST_MOD_DPIC_CALL_END_WITH_EN(AtomicEvent)
endmodule

// DifftestPtwEvent
`DIFFTEST_DPIC_FUNC_DECL(PtwEvent) (
  `DPIC_ARG_BYTE coreid,
  `DPIC_ARG_BIT  ptwResp,
  `DPIC_ARG_LONG ptwAddr,
  `DPIC_ARG_LONG ptwData_0,
  `DPIC_ARG_LONG ptwData_1,
  `DPIC_ARG_LONG ptwData_2,
  `DPIC_ARG_LONG ptwData_3
);
`DIFFTEST_MOD_DECL(PtwEvent)(
  input        clock,
  input [ 7:0] coreid,
  input        ptwResp,
  input [63:0] ptwAddr,
  input [63:0] ptwData_0,
  input [63:0] ptwData_1,
  input [63:0] ptwData_2,
  input [63:0] ptwData_3
);
  `DIFFTEST_MOD_DPIC_CALL_BEGIN_WITH_EN(ptwResp, PtwEvent) (
    coreid, ptwResp, ptwAddr, ptwData_0, ptwData_1, ptwData_2, ptwData_3
  ) `DIFFTEST_MOD_DPIC_CALL_END_WITH_EN(PtwEvent)
endmodule
