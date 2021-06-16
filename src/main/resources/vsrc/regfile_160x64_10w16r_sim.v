/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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


`timescale 1ns/1ps
//`define WITH_BYPASS

module regfile_160x64_10w16r_sim (
input clk, gpr,
input wen0, wen1, wen2, wen3, wen4, wen5, wen6, wen7, wen8, wen9,
input [ 7:0] waddr0, waddr1, waddr2, waddr3, waddr4, waddr5, waddr6, waddr7, waddr8, waddr9,
input [63:0] wdata0, wdata1, wdata2, wdata3, wdata4, wdata5, wdata6, wdata7, wdata8, wdata9,
input [ 7:0] raddr0, raddr1, raddr2, raddr3, raddr4, raddr5, raddr6, raddr7,
input [ 7:0] raddr8, raddr9, raddr10, raddr11, raddr12, raddr13, raddr14, raddr15,
output [63:0] rdata0, rdata1, rdata2, rdata3, rdata4, rdata5, rdata6, rdata7,
output [63:0] rdata8, rdata9, rdata10, rdata11, rdata12, rdata13, rdata14, rdata15);

reg reg_gpr, reg_wen0, reg_wen1, reg_wen2, reg_wen3, reg_wen4, reg_wen5, reg_wen6, reg_wen7, reg_wen8, reg_wen9;
reg [ 7:0] reg_waddr0, reg_waddr1, reg_waddr2, reg_waddr3, reg_waddr4, reg_waddr5, reg_waddr6, reg_waddr7, reg_waddr8, reg_waddr9;
reg [63:0] reg_wdata0, reg_wdata1, reg_wdata2, reg_wdata3, reg_wdata4, reg_wdata5, reg_wdata6, reg_wdata7, reg_wdata8, reg_wdata9;
reg [ 7:0] reg_raddr0, reg_raddr1, reg_raddr2, reg_raddr3, reg_raddr4, reg_raddr5, reg_raddr6, reg_raddr7;
reg [ 7:0] reg_raddr8, reg_raddr9, reg_raddr10, reg_raddr11, reg_raddr12, reg_raddr13, reg_raddr14, reg_raddr15;
always @(posedge clk) begin
  reg_gpr  <= gpr;
  reg_wen0 <= wen0;
  reg_wen1 <= wen1;
  reg_wen2 <= wen2;
  reg_wen3 <= wen3;
  reg_wen4 <= wen4;
  reg_wen5 <= wen5;
  reg_wen6 <= wen6;
  reg_wen7 <= wen7;
  reg_wen8 <= wen8;
  reg_wen9 <= wen9;
  if(wen0) begin reg_waddr0 <= waddr0; end
  if(wen1) begin reg_waddr1 <= waddr1; end
  if(wen2) begin reg_waddr2 <= waddr2; end
  if(wen3) begin reg_waddr3 <= waddr3; end
  if(wen4) begin reg_waddr4 <= waddr4; end
  if(wen5) begin reg_waddr5 <= waddr5; end
  if(wen6) begin reg_waddr6 <= waddr6; end
  if(wen7) begin reg_waddr7 <= waddr7; end
  if(wen8) begin reg_waddr8 <= waddr8; end
  if(wen9) begin reg_waddr9 <= waddr9; end
  if(wen0) begin reg_wdata0 <= wdata0; end
  if(wen1) begin reg_wdata1 <= wdata1; end
  if(wen2) begin reg_wdata2 <= wdata2; end
  if(wen3) begin reg_wdata3 <= wdata3; end
  if(wen4) begin reg_wdata4 <= wdata4; end
  if(wen5) begin reg_wdata5 <= wdata5; end
  if(wen6) begin reg_wdata6 <= wdata6; end
  if(wen7) begin reg_wdata7 <= wdata7; end
  if(wen8) begin reg_wdata8 <= wdata8; end
  if(wen9) begin reg_wdata9 <= wdata9; end
  reg_raddr0 <= raddr0;
  reg_raddr1 <= raddr1;
  reg_raddr2 <= raddr2;
  reg_raddr3 <= raddr3;
  reg_raddr4 <= raddr4;
  reg_raddr5 <= raddr5;
  reg_raddr6 <= raddr6;
  reg_raddr7 <= raddr7;
  reg_raddr8 <= raddr8;
  reg_raddr9 <= raddr9;
  reg_raddr10 <= raddr10;
  reg_raddr11 <= raddr11;
  reg_raddr12 <= raddr12;
  reg_raddr13 <= raddr13;
  reg_raddr14 <= raddr14;
  reg_raddr15 <= raddr15;
end

wire [255:0] wad0_dec, wad1_dec, wad2_dec, wad3_dec, wad4_dec, wad5_dec, wad6_dec, wad7_dec, wad8_dec, wad9_dec;
addr_dec_8x256_with_en U_wad0_dec ( .en(reg_wen0), .addr(reg_waddr0), .dec(wad0_dec) );
addr_dec_8x256_with_en U_wad1_dec ( .en(reg_wen1), .addr(reg_waddr1), .dec(wad1_dec) );
addr_dec_8x256_with_en U_wad2_dec ( .en(reg_wen2), .addr(reg_waddr2), .dec(wad2_dec) );
addr_dec_8x256_with_en U_wad3_dec ( .en(reg_wen3), .addr(reg_waddr3), .dec(wad3_dec) );
addr_dec_8x256_with_en U_wad4_dec ( .en(reg_wen4), .addr(reg_waddr4), .dec(wad4_dec) );
addr_dec_8x256_with_en U_wad5_dec ( .en(reg_wen5), .addr(reg_waddr5), .dec(wad5_dec) );
addr_dec_8x256_with_en U_wad6_dec ( .en(reg_wen6), .addr(reg_waddr6), .dec(wad6_dec) );
addr_dec_8x256_with_en U_wad7_dec ( .en(reg_wen7), .addr(reg_waddr7), .dec(wad7_dec) );
addr_dec_8x256_with_en U_wad8_dec ( .en(reg_wen8), .addr(reg_waddr8), .dec(wad8_dec) );
addr_dec_8x256_with_en U_wad9_dec ( .en(reg_wen9), .addr(reg_waddr9), .dec(wad9_dec) );

wire clk_inv = !clk_inv;
wire gpr_inv = !reg_gpr;
integer i;
reg [63:0] reg_MEM [159:0];
always @(posedge clk_inv or negedge gpr_inv) begin
  if(!gpr_inv) begin
    reg_MEM[0] <= 64'b0;
  end
  else begin
    if(wad0_dec[0]||wad1_dec[0]||wad2_dec[0]||wad3_dec[0]||wad4_dec[0]||wad5_dec[0]||wad6_dec[0]||wad7_dec[0]||wad8_dec[0]||wad9_dec[0]) begin
      reg_MEM[0] <= {64{wad0_dec[0]}}&reg_wdata0 | {64{wad1_dec[0]}}&reg_wdata1 | {64{wad2_dec[0]}}&reg_wdata2 | {64{wad3_dec[0]}}&reg_wdata3 |
                    {64{wad4_dec[0]}}&reg_wdata4 | {64{wad5_dec[0]}}&reg_wdata5 | {64{wad6_dec[0]}}&reg_wdata6 | {64{wad7_dec[0]}}&reg_wdata7 |
		    {64{wad8_dec[0]}}&reg_wdata8 | {64{wad9_dec[0]}}&reg_wdata9;
    end
  end
end
always @(posedge clk_inv) begin
  for(i=1;i<160;i=i+1) begin
    if(wad0_dec[i]||wad1_dec[i]||wad2_dec[i]||wad3_dec[i]||wad4_dec[i]||wad5_dec[i]||wad6_dec[i]||wad7_dec[i]||wad8_dec[i]||wad9_dec[i]) begin
      reg_MEM[i] <= {64{wad0_dec[i]}}&reg_wdata0 | {64{wad1_dec[i]}}&reg_wdata1 | {64{wad2_dec[i]}}&reg_wdata2 | {64{wad3_dec[i]}}&reg_wdata3 |
                    {64{wad4_dec[i]}}&reg_wdata4 | {64{wad5_dec[i]}}&reg_wdata5 | {64{wad6_dec[i]}}&reg_wdata6 | {64{wad7_dec[i]}}&reg_wdata7 |
		    {64{wad8_dec[i]}}&reg_wdata8 | {64{wad9_dec[i]}}&reg_wdata9;
    end
  end
end

wire [63:0] rdata0_0 = reg_MEM[reg_raddr0];
wire [63:0] rdata1_0 = reg_MEM[reg_raddr1];
wire [63:0] rdata2_0 = reg_MEM[reg_raddr2];
wire [63:0] rdata3_0 = reg_MEM[reg_raddr3];
wire [63:0] rdata4_0 = reg_MEM[reg_raddr4];
wire [63:0] rdata5_0 = reg_MEM[reg_raddr5];
wire [63:0] rdata6_0 = reg_MEM[reg_raddr6];
wire [63:0] rdata7_0 = reg_MEM[reg_raddr7];
wire [63:0] rdata8_0 = reg_MEM[reg_raddr8];
wire [63:0] rdata9_0 = reg_MEM[reg_raddr9];
wire [63:0] rdata10_0 = reg_MEM[reg_raddr10];
wire [63:0] rdata11_0 = reg_MEM[reg_raddr11];
wire [63:0] rdata12_0 = reg_MEM[reg_raddr12];
wire [63:0] rdata13_0 = reg_MEM[reg_raddr13];
wire [63:0] rdata14_0 = reg_MEM[reg_raddr14];
wire [63:0] rdata15_0 = reg_MEM[reg_raddr15];

`ifdef WITH_BYPASS
  wire [ 7:0] rd_by0, rd_by1, rd_by2, rd_by3, rd_by4, rd_by5, rd_by6, rd_by7, rd_by8, rd_by9, rd_by10, rd_by11, rd_by12, rd_by13, rd_by14, rd_by15;
  wire [63:0] by_data0, by_data1, by_data2, by_data3, by_data4, by_data5, by_data6, by_data7, by_data8, by_data9, by_data10, by_data11, by_data12, by_data13, by_data14, by_data15;
  addr_comp_10b U_comp_0 ( .by(rd_by0), .raddr(reg_raddr0),
    .wen0(reg_wen0), .waddr0(reg_waddr0), .wen1(reg_wen1), .waddr1(reg_waddr1), .wen2(reg_wen2), .waddr2(reg_waddr2), .wen3(reg_wen3), .waddr3(reg_waddr3),
    .wen4(reg_wen4), .waddr4(reg_waddr4), .wen5(reg_wen5), .waddr5(reg_waddr5), .wen6(reg_wen6), .waddr6(reg_waddr6), .wen7(reg_wen7), .waddr7(reg_waddr7),
    .wen8(reg_wen8), .waddr8(reg_waddr8), .wen9(reg_wen9), .waddr9(reg_waddr9) );
  addr_comp_10b U_comp_1 ( .by(rd_by1), .raddr(reg_raddr1),
    .wen0(reg_wen0), .waddr0(reg_waddr0), .wen1(reg_wen1), .waddr1(reg_waddr1), .wen2(reg_wen2), .waddr2(reg_waddr2), .wen3(reg_wen3), .waddr3(reg_waddr3),
    .wen4(reg_wen4), .waddr4(reg_waddr4), .wen5(reg_wen5), .waddr5(reg_waddr5), .wen6(reg_wen6), .waddr6(reg_waddr6), .wen7(reg_wen7), .waddr7(reg_waddr7),
    .wen8(reg_wen8), .waddr8(reg_waddr8), .wen9(reg_wen9), .waddr9(reg_waddr9) );
  addr_comp_10b U_comp_2 ( .by(rd_by2), .raddr(reg_raddr2),
    .wen0(reg_wen0), .waddr0(reg_waddr0), .wen1(reg_wen1), .waddr1(reg_waddr1), .wen2(reg_wen2), .waddr2(reg_waddr2), .wen3(reg_wen3), .waddr3(reg_waddr3),
    .wen4(reg_wen4), .waddr4(reg_waddr4), .wen5(reg_wen5), .waddr5(reg_waddr5), .wen6(reg_wen6), .waddr6(reg_waddr6), .wen7(reg_wen7), .waddr7(reg_waddr7),
    .wen8(reg_wen8), .waddr8(reg_waddr8), .wen9(reg_wen9), .waddr9(reg_waddr9) );
  addr_comp_10b U_comp_3 ( .by(rd_by3), .raddr(reg_raddr3),
    .wen0(reg_wen0), .waddr0(reg_waddr0), .wen1(reg_wen1), .waddr1(reg_waddr1), .wen2(reg_wen2), .waddr2(reg_waddr2), .wen3(reg_wen3), .waddr3(reg_waddr3),
    .wen4(reg_wen4), .waddr4(reg_waddr4), .wen5(reg_wen5), .waddr5(reg_waddr5), .wen6(reg_wen6), .waddr6(reg_waddr6), .wen7(reg_wen7), .waddr7(reg_waddr7),
    .wen8(reg_wen8), .waddr8(reg_waddr8), .wen9(reg_wen9), .waddr9(reg_waddr9) );
  addr_comp_10b U_comp_4 ( .by(rd_by4), .raddr(reg_raddr4),
    .wen0(reg_wen0), .waddr0(reg_waddr0), .wen1(reg_wen1), .waddr1(reg_waddr1), .wen2(reg_wen2), .waddr2(reg_waddr2), .wen3(reg_wen3), .waddr3(reg_waddr3),
    .wen4(reg_wen4), .waddr4(reg_waddr4), .wen5(reg_wen5), .waddr5(reg_waddr5), .wen6(reg_wen6), .waddr6(reg_waddr6), .wen7(reg_wen7), .waddr7(reg_waddr7),
    .wen8(reg_wen8), .waddr8(reg_waddr8), .wen9(reg_wen9), .waddr9(reg_waddr9) );
  addr_comp_10b U_comp_5 ( .by(rd_by5), .raddr(reg_raddr5),
    .wen0(reg_wen0), .waddr0(reg_waddr0), .wen1(reg_wen1), .waddr1(reg_waddr1), .wen2(reg_wen2), .waddr2(reg_waddr2), .wen3(reg_wen3), .waddr3(reg_waddr3),
    .wen4(reg_wen4), .waddr4(reg_waddr4), .wen5(reg_wen5), .waddr5(reg_waddr5), .wen6(reg_wen6), .waddr6(reg_waddr6), .wen7(reg_wen7), .waddr7(reg_waddr7),
    .wen8(reg_wen8), .waddr8(reg_waddr8), .wen9(reg_wen9), .waddr9(reg_waddr9) );
  addr_comp_10b U_comp_6 ( .by(rd_by6), .raddr(reg_raddr6),
    .wen0(reg_wen0), .waddr0(reg_waddr0), .wen1(reg_wen1), .waddr1(reg_waddr1), .wen2(reg_wen2), .waddr2(reg_waddr2), .wen3(reg_wen3), .waddr3(reg_waddr3),
    .wen4(reg_wen4), .waddr4(reg_waddr4), .wen5(reg_wen5), .waddr5(reg_waddr5), .wen6(reg_wen6), .waddr6(reg_waddr6), .wen7(reg_wen7), .waddr7(reg_waddr7),
    .wen8(reg_wen8), .waddr8(reg_waddr8), .wen9(reg_wen9), .waddr9(reg_waddr9) );
  addr_comp_10b U_comp_7 ( .by(rd_by7), .raddr(reg_raddr7),
    .wen0(reg_wen0), .waddr0(reg_waddr0), .wen1(reg_wen1), .waddr1(reg_waddr1), .wen2(reg_wen2), .waddr2(reg_waddr2), .wen3(reg_wen3), .waddr3(reg_waddr3),
    .wen4(reg_wen4), .waddr4(reg_waddr4), .wen5(reg_wen5), .waddr5(reg_waddr5), .wen6(reg_wen6), .waddr6(reg_waddr6), .wen7(reg_wen7), .waddr7(reg_waddr7),
    .wen8(reg_wen8), .waddr8(reg_waddr8), .wen9(reg_wen9), .waddr9(reg_waddr9) );
  addr_comp_10b U_comp_8 ( .by(rd_by8), .raddr(reg_raddr8),
    .wen0(reg_wen0), .waddr0(reg_waddr0), .wen1(reg_wen1), .waddr1(reg_waddr1), .wen2(reg_wen2), .waddr2(reg_waddr2), .wen3(reg_wen3), .waddr3(reg_waddr3),
    .wen4(reg_wen4), .waddr4(reg_waddr4), .wen5(reg_wen5), .waddr5(reg_waddr5), .wen6(reg_wen6), .waddr6(reg_waddr6), .wen7(reg_wen7), .waddr7(reg_waddr7),
    .wen8(reg_wen8), .waddr8(reg_waddr8), .wen9(reg_wen9), .waddr9(reg_waddr9) );
  addr_comp_10b U_comp_9 ( .by(rd_by9), .raddr(reg_raddr9),
    .wen0(reg_wen0), .waddr0(reg_waddr0), .wen1(reg_wen1), .waddr1(reg_waddr1), .wen2(reg_wen2), .waddr2(reg_waddr2), .wen3(reg_wen3), .waddr3(reg_waddr3),
    .wen4(reg_wen4), .waddr4(reg_waddr4), .wen5(reg_wen5), .waddr5(reg_waddr5), .wen6(reg_wen6), .waddr6(reg_waddr6), .wen7(reg_wen7), .waddr7(reg_waddr7),
    .wen8(reg_wen8), .waddr8(reg_waddr8), .wen9(reg_wen9), .waddr9(reg_waddr9) );
  addr_comp_10b U_comp_10 ( .by(rd_by10), .raddr(reg_raddr10),
    .wen0(reg_wen0), .waddr0(reg_waddr0), .wen1(reg_wen1), .waddr1(reg_waddr1), .wen2(reg_wen2), .waddr2(reg_waddr2), .wen3(reg_wen3), .waddr3(reg_waddr3),
    .wen4(reg_wen4), .waddr4(reg_waddr4), .wen5(reg_wen5), .waddr5(reg_waddr5), .wen6(reg_wen6), .waddr6(reg_waddr6), .wen7(reg_wen7), .waddr7(reg_waddr7),
    .wen8(reg_wen8), .waddr8(reg_waddr8), .wen9(reg_wen9), .waddr9(reg_waddr9) );
  addr_comp_10b U_comp_11 ( .by(rd_by11), .raddr(reg_raddr11),
    .wen0(reg_wen0), .waddr0(reg_waddr0), .wen1(reg_wen1), .waddr1(reg_waddr1), .wen2(reg_wen2), .waddr2(reg_waddr2), .wen3(reg_wen3), .waddr3(reg_waddr3),
    .wen4(reg_wen4), .waddr4(reg_waddr4), .wen5(reg_wen5), .waddr5(reg_waddr5), .wen6(reg_wen6), .waddr6(reg_waddr6), .wen7(reg_wen7), .waddr7(reg_waddr7),
    .wen8(reg_wen8), .waddr8(reg_waddr8), .wen9(reg_wen9), .waddr9(reg_waddr9) );
  addr_comp_10b U_comp_12 ( .by(rd_by12), .raddr(reg_raddr12),
    .wen0(reg_wen0), .waddr0(reg_waddr0), .wen1(reg_wen1), .waddr1(reg_waddr1), .wen2(reg_wen2), .waddr2(reg_waddr2), .wen3(reg_wen3), .waddr3(reg_waddr3),
    .wen4(reg_wen4), .waddr4(reg_waddr4), .wen5(reg_wen5), .waddr5(reg_waddr5), .wen6(reg_wen6), .waddr6(reg_waddr6), .wen7(reg_wen7), .waddr7(reg_waddr7),
    .wen8(reg_wen8), .waddr8(reg_waddr8), .wen9(reg_wen9), .waddr9(reg_waddr9) );
  addr_comp_10b U_comp_13 ( .by(rd_by13), .raddr(reg_raddr13),
    .wen0(reg_wen0), .waddr0(reg_waddr0), .wen1(reg_wen1), .waddr1(reg_waddr1), .wen2(reg_wen2), .waddr2(reg_waddr2), .wen3(reg_wen3), .waddr3(reg_waddr3),
    .wen4(reg_wen4), .waddr4(reg_waddr4), .wen5(reg_wen5), .waddr5(reg_waddr5), .wen6(reg_wen6), .waddr6(reg_waddr6), .wen7(reg_wen7), .waddr7(reg_waddr7),
    .wen8(reg_wen8), .waddr8(reg_waddr8), .wen9(reg_wen9), .waddr9(reg_waddr9) );
  
  assign rdata0  = (|rd_by0) ? by_data0 : rdata0_0;
  assign rdata1  = (|rd_by1) ? by_data1 : rdata1_0;
  assign rdata2  = (|rd_by2) ? by_data2 : rdata2_0;
  assign rdata3  = (|rd_by3) ? by_data3 : rdata3_0;
  assign rdata4  = (|rd_by4) ? by_data4 : rdata4_0;
  assign rdata5  = (|rd_by5) ? by_data5 : rdata5_0;
  assign rdata6  = (|rd_by6) ? by_data6 : rdata6_0;
  assign rdata7  = (|rd_by7) ? by_data7 : rdata7_0;
  assign rdata8  = (|rd_by8) ? by_data8 : rdata8_0;
  assign rdata9  = (|rd_by9) ? by_data9 : rdata9_0;
  assign rdata10 = (|rd_by10) ? by_data10 : rdata10_0;
  assign rdata11 = (|rd_by11) ? by_data11 : rdata11_0;
  assign rdata12 = (|rd_by12) ? by_data12 : rdata12_0;
  assign rdata13 = (|rd_by13) ? by_data13 : rdata13_0;
  assign rdata14 = (|rd_by14) ? by_data14 : rdata14_0;
  assign rdata15 = (|rd_by15) ? by_data15 : rdata15_0;
`else
  assign rdata0  = rdata0_0;
  assign rdata1  = rdata1_0;
  assign rdata2  = rdata2_0;
  assign rdata3  = rdata3_0;
  assign rdata4  = rdata4_0;
  assign rdata5  = rdata5_0;
  assign rdata6  = rdata6_0;
  assign rdata7  = rdata7_0;
  assign rdata8  = rdata8_0;
  assign rdata9  = rdata9_0;
  assign rdata10 = rdata10_0;
  assign rdata11 = rdata11_0;
  assign rdata12 = rdata12_0;
  assign rdata13 = rdata13_0;
  assign rdata14 = rdata14_0;
  assign rdata15 = rdata15_0;
`endif
endmodule

`ifdef WITH_BYPASS
module addr_comp_10b (
input [7:0] raddr,
input wen0, wen1, wen2, wen3, wen4, wen5, wen6, wen7, wen8, wen9,
input [7:0] waddr0, waddr1, waddr2, waddr3, waddr4, waddr5, waddr6, waddr7, waddr8, waddr9,
output [7:0] by);

assign by[0] = wen0 && (waddr0 == raddr);
assign by[1] = wen1 && (waddr1 == raddr);
assign by[2] = wen2 && (waddr2 == raddr);
assign by[3] = wen3 && (waddr3 == raddr);
assign by[4] = wen4 && (waddr4 == raddr);
assign by[5] = wen5 && (waddr5 == raddr);
assign by[6] = wen6 && (waddr6 == raddr);
assign by[7] = wen7 && (waddr7 == raddr);
assign by[8] = wen8 && (waddr8 == raddr);
assign by[9] = wen9 && (waddr9 == raddr);
endmodule
`endif

module addr_dec_8x256_with_en (
input en,
input [7:0] addr,
output [255:0] dec);

wire [15:0] r, c;
addr_dec_4x16 U_dec_r ( .addr(addr[3:0]), .dec(r) );
addr_dec_4x16 U_dec_c ( .addr(addr[7:4]), .dec(c) );
wire [255:0] dec_0;
addr_dec_32x256 U_dec_rc ( .r(r), .c(c), .dec(dec_0) );
assign dec = {256{en}}&dec_0;
endmodule

module addr_dec_4x16 (
input [3:0] addr,
output reg [15:0] dec);
always @(addr) begin
  case(addr) // synopsys full_case parallel_case
  4'b0000: dec = 16'b0000000000000001;
  4'b0001: dec = 16'b0000000000000010;
  4'b0010: dec = 16'b0000000000000100;
  4'b0011: dec = 16'b0000000000001000;
  4'b0100: dec = 16'b0000000000010000;
  4'b0101: dec = 16'b0000000000100000;
  4'b0110: dec = 16'b0000000001000000;
  4'b0111: dec = 16'b0000000010000000;
  4'b1000: dec = 16'b0000000100000000;
  4'b1001: dec = 16'b0000001000000000;
  4'b1010: dec = 16'b0000010000000000;
  4'b1011: dec = 16'b0000100000000000;
  4'b1100: dec = 16'b0001000000000000;
  4'b1101: dec = 16'b0010000000000000;
  4'b1110: dec = 16'b0100000000000000;
  4'b1111: dec = 16'b1000000000000000;
  default: dec = 16'b0000000000000000;
  endcase
end
endmodule

module addr_dec_32x256 (
input [15:0] r, c,
output [255:0] dec);
assign dec[  0] = c[ 0] && r[ 0];
assign dec[  1] = c[ 0] && r[ 1];
assign dec[  2] = c[ 0] && r[ 2];
assign dec[  3] = c[ 0] && r[ 3];
assign dec[  4] = c[ 0] && r[ 4];
assign dec[  5] = c[ 0] && r[ 5];
assign dec[  6] = c[ 0] && r[ 6];
assign dec[  7] = c[ 0] && r[ 7];
assign dec[  8] = c[ 0] && r[ 8];
assign dec[  9] = c[ 0] && r[ 9];
assign dec[ 10] = c[ 0] && r[10];
assign dec[ 11] = c[ 0] && r[11];
assign dec[ 12] = c[ 0] && r[12];
assign dec[ 13] = c[ 0] && r[13];
assign dec[ 14] = c[ 0] && r[14];
assign dec[ 15] = c[ 0] && r[15];

assign dec[ 16] = c[ 1] && r[ 0];
assign dec[ 17] = c[ 1] && r[ 1];
assign dec[ 18] = c[ 1] && r[ 2];
assign dec[ 19] = c[ 1] && r[ 3];
assign dec[ 20] = c[ 1] && r[ 4];
assign dec[ 21] = c[ 1] && r[ 5];
assign dec[ 22] = c[ 1] && r[ 6];
assign dec[ 23] = c[ 1] && r[ 7];
assign dec[ 24] = c[ 1] && r[ 8];
assign dec[ 25] = c[ 1] && r[ 9];
assign dec[ 26] = c[ 1] && r[10];
assign dec[ 27] = c[ 1] && r[11];
assign dec[ 28] = c[ 1] && r[12];
assign dec[ 29] = c[ 1] && r[13];
assign dec[ 30] = c[ 1] && r[14];
assign dec[ 31] = c[ 1] && r[15];

assign dec[ 32] = c[ 2] && r[ 0];
assign dec[ 33] = c[ 2] && r[ 1];
assign dec[ 34] = c[ 2] && r[ 2];
assign dec[ 35] = c[ 2] && r[ 3];
assign dec[ 36] = c[ 2] && r[ 4];
assign dec[ 37] = c[ 2] && r[ 5];
assign dec[ 38] = c[ 2] && r[ 6];
assign dec[ 39] = c[ 2] && r[ 7];
assign dec[ 40] = c[ 2] && r[ 8];
assign dec[ 41] = c[ 2] && r[ 9];
assign dec[ 42] = c[ 2] && r[10];
assign dec[ 43] = c[ 2] && r[11];
assign dec[ 44] = c[ 2] && r[12];
assign dec[ 45] = c[ 2] && r[13];
assign dec[ 46] = c[ 2] && r[14];
assign dec[ 47] = c[ 2] && r[15];

assign dec[ 48] = c[ 3] && r[ 0];
assign dec[ 49] = c[ 3] && r[ 1];
assign dec[ 50] = c[ 3] && r[ 2];
assign dec[ 51] = c[ 3] && r[ 3];
assign dec[ 52] = c[ 3] && r[ 4];
assign dec[ 53] = c[ 3] && r[ 5];
assign dec[ 54] = c[ 3] && r[ 6];
assign dec[ 55] = c[ 3] && r[ 7];
assign dec[ 56] = c[ 3] && r[ 8];
assign dec[ 57] = c[ 3] && r[ 9];
assign dec[ 58] = c[ 3] && r[10];
assign dec[ 59] = c[ 3] && r[11];
assign dec[ 60] = c[ 3] && r[12];
assign dec[ 61] = c[ 3] && r[13];
assign dec[ 62] = c[ 3] && r[14];
assign dec[ 63] = c[ 3] && r[15];

assign dec[ 64] = c[ 4] && r[ 0];
assign dec[ 65] = c[ 4] && r[ 1];
assign dec[ 66] = c[ 4] && r[ 2];
assign dec[ 67] = c[ 4] && r[ 3];
assign dec[ 68] = c[ 4] && r[ 4];
assign dec[ 69] = c[ 4] && r[ 5];
assign dec[ 70] = c[ 4] && r[ 6];
assign dec[ 71] = c[ 4] && r[ 7];
assign dec[ 72] = c[ 4] && r[ 8];
assign dec[ 73] = c[ 4] && r[ 9];
assign dec[ 74] = c[ 4] && r[10];
assign dec[ 75] = c[ 4] && r[11];
assign dec[ 76] = c[ 4] && r[12];
assign dec[ 77] = c[ 4] && r[13];
assign dec[ 78] = c[ 4] && r[14];
assign dec[ 79] = c[ 4] && r[15];

assign dec[ 80] = c[ 5] && r[ 0];
assign dec[ 81] = c[ 5] && r[ 1];
assign dec[ 82] = c[ 5] && r[ 2];
assign dec[ 83] = c[ 5] && r[ 3];
assign dec[ 84] = c[ 5] && r[ 4];
assign dec[ 85] = c[ 5] && r[ 5];
assign dec[ 86] = c[ 5] && r[ 6];
assign dec[ 87] = c[ 5] && r[ 7];
assign dec[ 88] = c[ 5] && r[ 8];
assign dec[ 89] = c[ 5] && r[ 9];
assign dec[ 90] = c[ 5] && r[10];
assign dec[ 91] = c[ 5] && r[11];
assign dec[ 92] = c[ 5] && r[12];
assign dec[ 93] = c[ 5] && r[13];
assign dec[ 94] = c[ 5] && r[14];
assign dec[ 95] = c[ 5] && r[15];

assign dec[ 96] = c[ 6] && r[ 0];
assign dec[ 97] = c[ 6] && r[ 1];
assign dec[ 98] = c[ 6] && r[ 2];
assign dec[ 99] = c[ 6] && r[ 3];
assign dec[100] = c[ 6] && r[ 4];
assign dec[101] = c[ 6] && r[ 5];
assign dec[102] = c[ 6] && r[ 6];
assign dec[103] = c[ 6] && r[ 7];
assign dec[104] = c[ 6] && r[ 8];
assign dec[105] = c[ 6] && r[ 9];
assign dec[106] = c[ 6] && r[10];
assign dec[107] = c[ 6] && r[11];
assign dec[108] = c[ 6] && r[12];
assign dec[109] = c[ 6] && r[13];
assign dec[110] = c[ 6] && r[14];
assign dec[111] = c[ 6] && r[15];

assign dec[112] = c[ 7] && r[ 0];
assign dec[113] = c[ 7] && r[ 1];
assign dec[114] = c[ 7] && r[ 2];
assign dec[115] = c[ 7] && r[ 3];
assign dec[116] = c[ 7] && r[ 4];
assign dec[117] = c[ 7] && r[ 5];
assign dec[118] = c[ 7] && r[ 6];
assign dec[119] = c[ 7] && r[ 7];
assign dec[120] = c[ 7] && r[ 8];
assign dec[121] = c[ 7] && r[ 9];
assign dec[122] = c[ 7] && r[10];
assign dec[123] = c[ 7] && r[11];
assign dec[124] = c[ 7] && r[12];
assign dec[125] = c[ 7] && r[13];
assign dec[126] = c[ 7] && r[14];
assign dec[127] = c[ 7] && r[15];

assign dec[128] = c[ 8] && r[ 0];
assign dec[129] = c[ 8] && r[ 1];
assign dec[130] = c[ 8] && r[ 2];
assign dec[131] = c[ 8] && r[ 3];
assign dec[132] = c[ 8] && r[ 4];
assign dec[133] = c[ 8] && r[ 5];
assign dec[134] = c[ 8] && r[ 6];
assign dec[135] = c[ 8] && r[ 7];
assign dec[136] = c[ 8] && r[ 8];
assign dec[137] = c[ 8] && r[ 9];
assign dec[138] = c[ 8] && r[10];
assign dec[139] = c[ 8] && r[11];
assign dec[140] = c[ 8] && r[12];
assign dec[141] = c[ 8] && r[13];
assign dec[142] = c[ 8] && r[14];
assign dec[143] = c[ 8] && r[15];

assign dec[144] = c[ 9] && r[ 0];
assign dec[145] = c[ 9] && r[ 1];
assign dec[146] = c[ 9] && r[ 2];
assign dec[147] = c[ 9] && r[ 3];
assign dec[148] = c[ 9] && r[ 4];
assign dec[149] = c[ 9] && r[ 5];
assign dec[150] = c[ 9] && r[ 6];
assign dec[151] = c[ 9] && r[ 7];
assign dec[152] = c[ 9] && r[ 8];
assign dec[153] = c[ 9] && r[ 9];
assign dec[154] = c[ 9] && r[10];
assign dec[155] = c[ 9] && r[11];
assign dec[156] = c[ 9] && r[12];
assign dec[157] = c[ 9] && r[13];
assign dec[158] = c[ 9] && r[14];
assign dec[159] = c[ 9] && r[15];

assign dec[160] = c[10] && r[ 0];
assign dec[161] = c[10] && r[ 1];
assign dec[162] = c[10] && r[ 2];
assign dec[163] = c[10] && r[ 3];
assign dec[164] = c[10] && r[ 4];
assign dec[165] = c[10] && r[ 5];
assign dec[166] = c[10] && r[ 6];
assign dec[167] = c[10] && r[ 7];
assign dec[168] = c[10] && r[ 8];
assign dec[169] = c[10] && r[ 9];
assign dec[170] = c[10] && r[10];
assign dec[171] = c[10] && r[11];
assign dec[172] = c[10] && r[12];
assign dec[173] = c[10] && r[13];
assign dec[174] = c[10] && r[14];
assign dec[175] = c[10] && r[15];

assign dec[176] = c[11] && r[ 0];
assign dec[177] = c[11] && r[ 1];
assign dec[178] = c[11] && r[ 2];
assign dec[179] = c[11] && r[ 3];
assign dec[180] = c[11] && r[ 4];
assign dec[181] = c[11] && r[ 5];
assign dec[182] = c[11] && r[ 6];
assign dec[183] = c[11] && r[ 7];
assign dec[184] = c[11] && r[ 8];
assign dec[185] = c[11] && r[ 9];
assign dec[186] = c[11] && r[10];
assign dec[187] = c[11] && r[11];
assign dec[188] = c[11] && r[12];
assign dec[189] = c[11] && r[13];
assign dec[190] = c[11] && r[14];
assign dec[191] = c[11] && r[15];

assign dec[192] = c[12] && r[ 0];
assign dec[193] = c[12] && r[ 1];
assign dec[194] = c[12] && r[ 2];
assign dec[195] = c[12] && r[ 3];
assign dec[196] = c[12] && r[ 4];
assign dec[197] = c[12] && r[ 5];
assign dec[198] = c[12] && r[ 6];
assign dec[199] = c[12] && r[ 7];
assign dec[200] = c[12] && r[ 8];
assign dec[201] = c[12] && r[ 9];
assign dec[202] = c[12] && r[10];
assign dec[203] = c[12] && r[11];
assign dec[204] = c[12] && r[12];
assign dec[205] = c[12] && r[13];
assign dec[206] = c[12] && r[14];
assign dec[207] = c[12] && r[15];

assign dec[208] = c[13] && r[ 0];
assign dec[209] = c[13] && r[ 1];
assign dec[210] = c[13] && r[ 2];
assign dec[211] = c[13] && r[ 3];
assign dec[212] = c[13] && r[ 4];
assign dec[213] = c[13] && r[ 5];
assign dec[214] = c[13] && r[ 6];
assign dec[215] = c[13] && r[ 7];
assign dec[216] = c[13] && r[ 8];
assign dec[217] = c[13] && r[ 9];
assign dec[218] = c[13] && r[10];
assign dec[219] = c[13] && r[11];
assign dec[220] = c[13] && r[12];
assign dec[221] = c[13] && r[13];
assign dec[222] = c[13] && r[14];
assign dec[223] = c[13] && r[15];

assign dec[224] = c[14] && r[ 0];
assign dec[225] = c[14] && r[ 1];
assign dec[226] = c[14] && r[ 2];
assign dec[227] = c[14] && r[ 3];
assign dec[228] = c[14] && r[ 4];
assign dec[229] = c[14] && r[ 5];
assign dec[230] = c[14] && r[ 6];
assign dec[231] = c[14] && r[ 7];
assign dec[232] = c[14] && r[ 8];
assign dec[233] = c[14] && r[ 9];
assign dec[234] = c[14] && r[10];
assign dec[235] = c[14] && r[11];
assign dec[236] = c[14] && r[12];
assign dec[237] = c[14] && r[13];
assign dec[238] = c[14] && r[14];
assign dec[239] = c[14] && r[15];

assign dec[240] = c[15] && r[ 0];
assign dec[241] = c[15] && r[ 1];
assign dec[242] = c[15] && r[ 2];
assign dec[243] = c[15] && r[ 3];
assign dec[244] = c[15] && r[ 4];
assign dec[245] = c[15] && r[ 5];
assign dec[246] = c[15] && r[ 6];
assign dec[247] = c[15] && r[ 7];
assign dec[248] = c[15] && r[ 8];
assign dec[249] = c[15] && r[ 9];
assign dec[250] = c[15] && r[10];
assign dec[251] = c[15] && r[11];
assign dec[252] = c[15] && r[12];
assign dec[253] = c[15] && r[13];
assign dec[254] = c[15] && r[14];
assign dec[255] = c[15] && r[15];
endmodule




