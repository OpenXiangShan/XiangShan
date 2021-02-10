 module xs_system_top();

wire         clock;
wire         reset;
wire  [63:0] io_difftest_r_0;
wire  [63:0] io_difftest_r_1;
wire  [63:0] io_difftest_r_2;
wire  [63:0] io_difftest_r_3;
wire  [63:0] io_difftest_r_4;
wire  [63:0] io_difftest_r_5;
wire  [63:0] io_difftest_r_6;
wire  [63:0] io_difftest_r_7;
wire  [63:0] io_difftest_r_8;
wire  [63:0] io_difftest_r_9;
wire  [63:0] io_difftest_r_10;
wire  [63:0] io_difftest_r_11;
wire  [63:0] io_difftest_r_12;
wire  [63:0] io_difftest_r_13;
wire  [63:0] io_difftest_r_14;
wire  [63:0] io_difftest_r_15;
wire  [63:0] io_difftest_r_16;
wire  [63:0] io_difftest_r_17;
wire  [63:0] io_difftest_r_18;
wire  [63:0] io_difftest_r_19;
wire  [63:0] io_difftest_r_20;
wire  [63:0] io_difftest_r_21;
wire  [63:0] io_difftest_r_22;
wire  [63:0] io_difftest_r_23;
wire  [63:0] io_difftest_r_24;
wire  [63:0] io_difftest_r_25;
wire  [63:0] io_difftest_r_26;
wire  [63:0] io_difftest_r_27;
wire  [63:0] io_difftest_r_28;
wire  [63:0] io_difftest_r_29;
wire  [63:0] io_difftest_r_30;
wire  [63:0] io_difftest_r_31;
wire  [63:0] io_difftest_r_32;
wire  [63:0] io_difftest_r_33;
wire  [63:0] io_difftest_r_34;
wire  [63:0] io_difftest_r_35;
wire  [63:0] io_difftest_r_36;
wire  [63:0] io_difftest_r_37;
wire  [63:0] io_difftest_r_38;
wire  [63:0] io_difftest_r_39;
wire  [63:0] io_difftest_r_40;
wire  [63:0] io_difftest_r_41;
wire  [63:0] io_difftest_r_42;
wire  [63:0] io_difftest_r_43;
wire  [63:0] io_difftest_r_44;
wire  [63:0] io_difftest_r_45;
wire  [63:0] io_difftest_r_46;
wire  [63:0] io_difftest_r_47;
wire  [63:0] io_difftest_r_48;
wire  [63:0] io_difftest_r_49;
wire  [63:0] io_difftest_r_50;
wire  [63:0] io_difftest_r_51;
wire  [63:0] io_difftest_r_52;
wire  [63:0] io_difftest_r_53;
wire  [63:0] io_difftest_r_54;
wire  [63:0] io_difftest_r_55;
wire  [63:0] io_difftest_r_56;
wire  [63:0] io_difftest_r_57;
wire  [63:0] io_difftest_r_58;
wire  [63:0] io_difftest_r_59;
wire  [63:0] io_difftest_r_60;
wire  [63:0] io_difftest_r_61;
wire  [63:0] io_difftest_r_62;
wire  [63:0] io_difftest_r_63;
wire  [31:0] io_difftest_commit;
wire  [63:0] io_difftest_thisPC;
wire  [31:0] io_difftest_thisINST;
wire  [31:0] io_difftest_skip;
wire  [31:0] io_difftest_wen;
wire  [63:0] io_difftest_wdata_0;
wire  [63:0] io_difftest_wdata_1;
wire  [63:0] io_difftest_wdata_2;
wire  [63:0] io_difftest_wdata_3;
wire  [63:0] io_difftest_wdata_4;
wire  [63:0] io_difftest_wdata_5;
wire  [31:0] io_difftest_wdst_0;
wire  [31:0] io_difftest_wdst_1;
wire  [31:0] io_difftest_wdst_2;
wire  [31:0] io_difftest_wdst_3;
wire  [31:0] io_difftest_wdst_4;
wire  [31:0] io_difftest_wdst_5;
wire  [63:0] io_difftest_wpc_0;
wire  [63:0] io_difftest_wpc_1;
wire  [63:0] io_difftest_wpc_2;
wire  [63:0] io_difftest_wpc_3;
wire  [63:0] io_difftest_wpc_4;
wire  [63:0] io_difftest_wpc_5;
wire  [31:0] io_difftest_isRVC;
wire  [63:0] io_difftest_intrNO;
wire  [63:0] io_difftest_cause;
wire  [1:0]  io_difftest_priviledgeMode;
wire  [63:0] io_difftest_mstatus;
wire  [63:0] io_difftest_sstatus;
wire  [63:0] io_difftest_mepc;
wire  [63:0] io_difftest_sepc;
wire  [63:0] io_difftest_mtval;
wire  [63:0] io_difftest_stval;
wire  [63:0] io_difftest_mtvec;
wire  [63:0] io_difftest_stvec;
wire  [63:0] io_difftest_mcause;
wire  [63:0] io_difftest_scause;
wire  [63:0] io_difftest_satp;
wire  [63:0] io_difftest_mip;
wire  [63:0] io_difftest_mie;
wire  [63:0] io_difftest_mscratch;
wire  [63:0] io_difftest_sscratch;
wire  [63:0] io_difftest_mideleg;
wire  [63:0] io_difftest_medeleg;
wire         io_difftest_scFailed;
wire  [1:0]  io_difftest_storeCommit;
wire  [63:0] io_difftest_storeAddr_0;
wire  [63:0] io_difftest_storeAddr_1;
wire  [63:0] io_difftest_storeData_0;
wire  [63:0] io_difftest_storeData_1;
wire  [7:0]  io_difftest_storeMask_0;
wire  [7:0]  io_difftest_storeMask_1;
wire         io_difftest_sbufferResp;
wire  [63:0] io_difftest_sbufferAddr;
wire  [7:0]  io_difftest_sbufferData_0;
wire  [7:0]  io_difftest_sbufferData_1;
wire  [7:0]  io_difftest_sbufferData_2;
wire  [7:0]  io_difftest_sbufferData_3;
wire  [7:0]  io_difftest_sbufferData_4;
wire  [7:0]  io_difftest_sbufferData_5;
wire  [7:0]  io_difftest_sbufferData_6;
wire  [7:0]  io_difftest_sbufferData_7;
wire  [7:0]  io_difftest_sbufferData_8;
wire  [7:0]  io_difftest_sbufferData_9;
wire  [7:0]  io_difftest_sbufferData_10;
wire  [7:0]  io_difftest_sbufferData_11;
wire  [7:0]  io_difftest_sbufferData_12;
wire  [7:0]  io_difftest_sbufferData_13;
wire  [7:0]  io_difftest_sbufferData_14;
wire  [7:0]  io_difftest_sbufferData_15;
wire  [7:0]  io_difftest_sbufferData_16;
wire  [7:0]  io_difftest_sbufferData_17;
wire  [7:0]  io_difftest_sbufferData_18;
wire  [7:0]  io_difftest_sbufferData_19;
wire  [7:0]  io_difftest_sbufferData_20;
wire  [7:0]  io_difftest_sbufferData_21;
wire  [7:0]  io_difftest_sbufferData_22;
wire  [7:0]  io_difftest_sbufferData_23;
wire  [7:0]  io_difftest_sbufferData_24;
wire  [7:0]  io_difftest_sbufferData_25;
wire  [7:0]  io_difftest_sbufferData_26;
wire  [7:0]  io_difftest_sbufferData_27;
wire  [7:0]  io_difftest_sbufferData_28;
wire  [7:0]  io_difftest_sbufferData_29;
wire  [7:0]  io_difftest_sbufferData_30;
wire  [7:0]  io_difftest_sbufferData_31;
wire  [7:0]  io_difftest_sbufferData_32;
wire  [7:0]  io_difftest_sbufferData_33;
wire  [7:0]  io_difftest_sbufferData_34;
wire  [7:0]  io_difftest_sbufferData_35;
wire  [7:0]  io_difftest_sbufferData_36;
wire  [7:0]  io_difftest_sbufferData_37;
wire  [7:0]  io_difftest_sbufferData_38;
wire  [7:0]  io_difftest_sbufferData_39;
wire  [7:0]  io_difftest_sbufferData_40;
wire  [7:0]  io_difftest_sbufferData_41;
wire  [7:0]  io_difftest_sbufferData_42;
wire  [7:0]  io_difftest_sbufferData_43;
wire  [7:0]  io_difftest_sbufferData_44;
wire  [7:0]  io_difftest_sbufferData_45;
wire  [7:0]  io_difftest_sbufferData_46;
wire  [7:0]  io_difftest_sbufferData_47;
wire  [7:0]  io_difftest_sbufferData_48;
wire  [7:0]  io_difftest_sbufferData_49;
wire  [7:0]  io_difftest_sbufferData_50;
wire  [7:0]  io_difftest_sbufferData_51;
wire  [7:0]  io_difftest_sbufferData_52;
wire  [7:0]  io_difftest_sbufferData_53;
wire  [7:0]  io_difftest_sbufferData_54;
wire  [7:0]  io_difftest_sbufferData_55;
wire  [7:0]  io_difftest_sbufferData_56;
wire  [7:0]  io_difftest_sbufferData_57;
wire  [7:0]  io_difftest_sbufferData_58;
wire  [7:0]  io_difftest_sbufferData_59;
wire  [7:0]  io_difftest_sbufferData_60;
wire  [7:0]  io_difftest_sbufferData_61;
wire  [7:0]  io_difftest_sbufferData_62;
wire  [7:0]  io_difftest_sbufferData_63;
wire  [63:0] io_difftest_sbufferMask;
wire  [63:0] io_logCtrl_log_begin;
wire  [63:0] io_logCtrl_log_end;
wire  [63:0] io_logCtrl_log_level;
wire         io_trap_valid;
wire  [2:0]  io_trap_code;
wire  [38:0] io_trap_pc;
wire  [63:0] io_trap_cycleCnt;
wire  [63:0] io_trap_instrCnt;
wire         io_uart_out_valid;
wire  [7:0]  io_uart_out_ch;
wire         io_uart_in_valid;
wire  [7:0]  io_uart_in_ch;

assign io_logCtrl_log_begin = 'h0;
assign io_logCtrl_log_end   = 'h0;
assign io_logCtrl_log_level = 'h0;

rcg rcg0
(
  .clock(clock),
  .reset(reset)
);

UARTHelper UARTHelper0
(
  .clock(clock),
  .reset(reset),
  .putchar_valid(io_uart_out_valid),
  .putchar_ch(io_uart_out_ch),
  .getchar_valid(io_uart_in_valid),
  .getchar_ch(io_uart_in_ch)
);

XSSimSoC XSSimSoC0
(
  .clock(clock),
  .reset(reset),
  .io_difftest_r_0(io_difftest_r_0),
  .io_difftest_r_1(io_difftest_r_1),
  .io_difftest_r_2(io_difftest_r_2),
  .io_difftest_r_3(io_difftest_r_3),
  .io_difftest_r_4(io_difftest_r_4),
  .io_difftest_r_5(io_difftest_r_5),
  .io_difftest_r_6(io_difftest_r_6),
  .io_difftest_r_7(io_difftest_r_7),
  .io_difftest_r_8(io_difftest_r_8),
  .io_difftest_r_9(io_difftest_r_9),
  .io_difftest_r_10(io_difftest_r_10),
  .io_difftest_r_11(io_difftest_r_11),
  .io_difftest_r_12(io_difftest_r_12),
  .io_difftest_r_13(io_difftest_r_13),
  .io_difftest_r_14(io_difftest_r_14),
  .io_difftest_r_15(io_difftest_r_15),
  .io_difftest_r_16(io_difftest_r_16),
  .io_difftest_r_17(io_difftest_r_17),
  .io_difftest_r_18(io_difftest_r_18),
  .io_difftest_r_19(io_difftest_r_19),
  .io_difftest_r_20(io_difftest_r_20),
  .io_difftest_r_21(io_difftest_r_21),
  .io_difftest_r_22(io_difftest_r_22),
  .io_difftest_r_23(io_difftest_r_23),
  .io_difftest_r_24(io_difftest_r_24),
  .io_difftest_r_25(io_difftest_r_25),
  .io_difftest_r_26(io_difftest_r_26),
  .io_difftest_r_27(io_difftest_r_27),
  .io_difftest_r_28(io_difftest_r_28),
  .io_difftest_r_29(io_difftest_r_29),
  .io_difftest_r_30(io_difftest_r_30),
  .io_difftest_r_31(io_difftest_r_31),
  .io_difftest_r_32(io_difftest_r_32),
  .io_difftest_r_33(io_difftest_r_33),
  .io_difftest_r_34(io_difftest_r_34),
  .io_difftest_r_35(io_difftest_r_35),
  .io_difftest_r_36(io_difftest_r_36),
  .io_difftest_r_37(io_difftest_r_37),
  .io_difftest_r_38(io_difftest_r_38),
  .io_difftest_r_39(io_difftest_r_39),
  .io_difftest_r_40(io_difftest_r_40),
  .io_difftest_r_41(io_difftest_r_41),
  .io_difftest_r_42(io_difftest_r_42),
  .io_difftest_r_43(io_difftest_r_43),
  .io_difftest_r_44(io_difftest_r_44),
  .io_difftest_r_45(io_difftest_r_45),
  .io_difftest_r_46(io_difftest_r_46),
  .io_difftest_r_47(io_difftest_r_47),
  .io_difftest_r_48(io_difftest_r_48),
  .io_difftest_r_49(io_difftest_r_49),
  .io_difftest_r_50(io_difftest_r_50),
  .io_difftest_r_51(io_difftest_r_51),
  .io_difftest_r_52(io_difftest_r_52),
  .io_difftest_r_53(io_difftest_r_53),
  .io_difftest_r_54(io_difftest_r_54),
  .io_difftest_r_55(io_difftest_r_55),
  .io_difftest_r_56(io_difftest_r_56),
  .io_difftest_r_57(io_difftest_r_57),
  .io_difftest_r_58(io_difftest_r_58),
  .io_difftest_r_59(io_difftest_r_59),
  .io_difftest_r_60(io_difftest_r_60),
  .io_difftest_r_61(io_difftest_r_61),
  .io_difftest_r_62(io_difftest_r_62),
  .io_difftest_r_63(io_difftest_r_63),
  .io_difftest_commit(io_difftest_commit),
  .io_difftest_thisPC(io_difftest_thisPC),
  .io_difftest_thisINST(io_difftest_thisINST),
  .io_difftest_skip(io_difftest_skip),
  .io_difftest_wen(io_difftest_wen),
  .io_difftest_wdata_0(io_difftest_wdata_0),
  .io_difftest_wdata_1(io_difftest_wdata_1),
  .io_difftest_wdata_2(io_difftest_wdata_2),
  .io_difftest_wdata_3(io_difftest_wdata_3),
  .io_difftest_wdata_4(io_difftest_wdata_4),
  .io_difftest_wdata_5(io_difftest_wdata_5),
  .io_difftest_wdst_0(io_difftest_wdst_0),
  .io_difftest_wdst_1(io_difftest_wdst_1),
  .io_difftest_wdst_2(io_difftest_wdst_2),
  .io_difftest_wdst_3(io_difftest_wdst_3),
  .io_difftest_wdst_4(io_difftest_wdst_4),
  .io_difftest_wdst_5(io_difftest_wdst_5),
  .io_difftest_wpc_0(io_difftest_wpc_0),
  .io_difftest_wpc_1(io_difftest_wpc_1),
  .io_difftest_wpc_2(io_difftest_wpc_2),
  .io_difftest_wpc_3(io_difftest_wpc_3),
  .io_difftest_wpc_4(io_difftest_wpc_4),
  .io_difftest_wpc_5(io_difftest_wpc_5),
  .io_difftest_isRVC(io_difftest_isRVC),
  .io_difftest_intrNO(io_difftest_intrNO),
  .io_difftest_cause(io_difftest_cause),
  .io_difftest_priviledgeMode(io_difftest_priviledgeMode),
  .io_difftest_mstatus(io_difftest_mstatus),
  .io_difftest_sstatus(io_difftest_sstatus),
  .io_difftest_mepc(io_difftest_mepc),
  .io_difftest_sepc(io_difftest_sepc),
  .io_difftest_mtval(io_difftest_mtval),
  .io_difftest_stval(io_difftest_stval),
  .io_difftest_mtvec(io_difftest_mtvec),
  .io_difftest_stvec(io_difftest_stvec),
  .io_difftest_mcause(io_difftest_mcause),
  .io_difftest_scause(io_difftest_scause),
  .io_difftest_satp(io_difftest_satp),
  .io_difftest_mip(io_difftest_mip),
  .io_difftest_mie(io_difftest_mie),
  .io_difftest_mscratch(io_difftest_mscratch),
  .io_difftest_sscratch(io_difftest_sscratch),
  .io_difftest_mideleg(io_difftest_mideleg),
  .io_difftest_medeleg(io_difftest_medeleg),
  .io_difftest_scFailed(io_difftest_scFailed),
  .io_difftest_storeCommit(io_difftest_storeCommit),
  .io_difftest_storeAddr_0(io_difftest_storeAddr_0),
  .io_difftest_storeAddr_1(io_difftest_storeAddr_1),
  .io_difftest_storeData_0(io_difftest_storeData_0),
  .io_difftest_storeData_1(io_difftest_storeData_1),
  .io_difftest_storeMask_0(io_difftest_storeMask_0),
  .io_difftest_storeMask_1(io_difftest_storeMask_1),
  .io_difftest_sbufferResp(io_difftest_sbufferResp),
  .io_difftest_sbufferAddr(io_difftest_sbufferAddr),
  .io_difftest_sbufferData_0(io_difftest_sbufferData_0),
  .io_difftest_sbufferData_1(io_difftest_sbufferData_1),
  .io_difftest_sbufferData_2(io_difftest_sbufferData_2),
  .io_difftest_sbufferData_3(io_difftest_sbufferData_3),
  .io_difftest_sbufferData_4(io_difftest_sbufferData_4),
  .io_difftest_sbufferData_5(io_difftest_sbufferData_5),
  .io_difftest_sbufferData_6(io_difftest_sbufferData_6),
  .io_difftest_sbufferData_7(io_difftest_sbufferData_7),
  .io_difftest_sbufferData_8(io_difftest_sbufferData_8),
  .io_difftest_sbufferData_9(io_difftest_sbufferData_9),
  .io_difftest_sbufferData_10(io_difftest_sbufferData_10),
  .io_difftest_sbufferData_11(io_difftest_sbufferData_11),
  .io_difftest_sbufferData_12(io_difftest_sbufferData_12),
  .io_difftest_sbufferData_13(io_difftest_sbufferData_13),
  .io_difftest_sbufferData_14(io_difftest_sbufferData_14),
  .io_difftest_sbufferData_15(io_difftest_sbufferData_15),
  .io_difftest_sbufferData_16(io_difftest_sbufferData_16),
  .io_difftest_sbufferData_17(io_difftest_sbufferData_17),
  .io_difftest_sbufferData_18(io_difftest_sbufferData_18),
  .io_difftest_sbufferData_19(io_difftest_sbufferData_19),
  .io_difftest_sbufferData_20(io_difftest_sbufferData_20),
  .io_difftest_sbufferData_21(io_difftest_sbufferData_21),
  .io_difftest_sbufferData_22(io_difftest_sbufferData_22),
  .io_difftest_sbufferData_23(io_difftest_sbufferData_23),
  .io_difftest_sbufferData_24(io_difftest_sbufferData_24),
  .io_difftest_sbufferData_25(io_difftest_sbufferData_25),
  .io_difftest_sbufferData_26(io_difftest_sbufferData_26),
  .io_difftest_sbufferData_27(io_difftest_sbufferData_27),
  .io_difftest_sbufferData_28(io_difftest_sbufferData_28),
  .io_difftest_sbufferData_29(io_difftest_sbufferData_29),
  .io_difftest_sbufferData_30(io_difftest_sbufferData_30),
  .io_difftest_sbufferData_31(io_difftest_sbufferData_31),
  .io_difftest_sbufferData_32(io_difftest_sbufferData_32),
  .io_difftest_sbufferData_33(io_difftest_sbufferData_33),
  .io_difftest_sbufferData_34(io_difftest_sbufferData_34),
  .io_difftest_sbufferData_35(io_difftest_sbufferData_35),
  .io_difftest_sbufferData_36(io_difftest_sbufferData_36),
  .io_difftest_sbufferData_37(io_difftest_sbufferData_37),
  .io_difftest_sbufferData_38(io_difftest_sbufferData_38),
  .io_difftest_sbufferData_39(io_difftest_sbufferData_39),
  .io_difftest_sbufferData_40(io_difftest_sbufferData_40),
  .io_difftest_sbufferData_41(io_difftest_sbufferData_41),
  .io_difftest_sbufferData_42(io_difftest_sbufferData_42),
  .io_difftest_sbufferData_43(io_difftest_sbufferData_43),
  .io_difftest_sbufferData_44(io_difftest_sbufferData_44),
  .io_difftest_sbufferData_45(io_difftest_sbufferData_45),
  .io_difftest_sbufferData_46(io_difftest_sbufferData_46),
  .io_difftest_sbufferData_47(io_difftest_sbufferData_47),
  .io_difftest_sbufferData_48(io_difftest_sbufferData_48),
  .io_difftest_sbufferData_49(io_difftest_sbufferData_49),
  .io_difftest_sbufferData_50(io_difftest_sbufferData_50),
  .io_difftest_sbufferData_51(io_difftest_sbufferData_51),
  .io_difftest_sbufferData_52(io_difftest_sbufferData_52),
  .io_difftest_sbufferData_53(io_difftest_sbufferData_53),
  .io_difftest_sbufferData_54(io_difftest_sbufferData_54),
  .io_difftest_sbufferData_55(io_difftest_sbufferData_55),
  .io_difftest_sbufferData_56(io_difftest_sbufferData_56),
  .io_difftest_sbufferData_57(io_difftest_sbufferData_57),
  .io_difftest_sbufferData_58(io_difftest_sbufferData_58),
  .io_difftest_sbufferData_59(io_difftest_sbufferData_59),
  .io_difftest_sbufferData_60(io_difftest_sbufferData_60),
  .io_difftest_sbufferData_61(io_difftest_sbufferData_61),
  .io_difftest_sbufferData_62(io_difftest_sbufferData_62),
  .io_difftest_sbufferData_63(io_difftest_sbufferData_63),
  .io_difftest_sbufferMask(io_difftest_sbufferMask),
  .io_logCtrl_log_begin(io_logCtrl_log_begin),
  .io_logCtrl_log_end(io_logCtrl_log_end),
  .io_logCtrl_log_level(io_logCtrl_log_level),
  .io_trap_valid(io_trap_valid),
  .io_trap_code(io_trap_code),
  .io_trap_pc(io_trap_pc),
  .io_trap_cycleCnt(io_trap_cycleCnt),
  .io_trap_instrCnt(io_trap_instrCnt),
  .io_uart_out_valid(io_uart_out_valid),
  .io_uart_out_ch(io_uart_out_ch),
  .io_uart_in_valid(io_uart_in_valid),
  .io_uart_in_ch(io_uart_in_ch)
);

endmodule

module rcg
(
  clock,
  reset
);

output clock;
output reset;

reg clock;
reg reset;

initial begin
  clock <= 0;
  reset <= 1;
  #500 reset <= 0;
end

always begin
  #1 clock <= ~clock;
end

initial begin
  $vcdpluson;
end

endmodule
