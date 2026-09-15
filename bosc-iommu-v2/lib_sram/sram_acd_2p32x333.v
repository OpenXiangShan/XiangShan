/*
 * Copyright (C) 2026 Beijing Institute of Open Source Chip
 *
 * Description: SRAM wrapper for XSCore
 * Author: VeriOps Group
 * Date: Mon May 19 14:52:42 2026
 */

module sram_acd_2p32x333 #(
	parameter 	AWIDTH  = 5,
	parameter 	DWIDTH  = 333
)(
	input					RW0_clk,
	input					RW0_wen,
	input  [AWIDTH-1:0]		RW0_addra,
	input					RW0_ren,
	input  [AWIDTH-1:0]		RW0_addrb,
	input  [DWIDTH-1:0]		RW0_wdata,
	output [DWIDTH-1:0]		RW0_rdata
);


`ifdef RTL_RAM_SIM
reg [DWIDTH-1:0]	mem[(1<<AWIDTH)-1:0];
reg [DWIDTH-1:0]	memreg;

assign RW0_rdata = memreg;

always@(posedge RW0_clk) begin
	if (RW0_wen) begin
        mem[RW0_addra] <= RW0_wdata;
	end
end

always@(posedge RW0_clk) begin
	if (RW0_ren) begin
        memreg <= mem[RW0_addrb];
	end
end
`endif


`ifdef FPGA_RAM_SIM
reg [DWIDTH-1:0]	mem[(1<<AWIDTH)-1:0];
reg [DWIDTH-1:0]	memreg;

assign RW0_rdata = memreg;

always@(posedge RW0_clk) begin
	if (RW0_wen) begin
        mem[RW0_addra] <= RW0_wdata;
	end
end

always@(posedge RW0_clk) begin
	if (RW0_ren) begin
        memreg <= mem[RW0_addrb];
	end
end
`endif


`ifdef ASIC_RAM_SIM
wire WEB;
assign WEB = ~RW0_wen;
wire REB;
assign REB = ~RW0_ren;
wire [168*2-1:0] rdata;
assign RW0_rdata = rdata[332:0];

TS6N12FFCLLULVTB32X168M1 u0_mem(
 .Q(rdata[168*1-1:168*0]),
 .AA(RW0_addra),
 .AB(RW0_addrb),
 .D(RW0_wdata[168*1-1:168*0]),
 .WEB(WEB),
 .REB(REB),
 .CLK(RW0_clk),
 .RTSEL(2'b01),
 .WTSEL(2'b00),
 .MTSEL(2'b01)
 );

TS6N12FFCLLULVTB32X168M1 u1_mem(
 .Q(rdata[168*2-1:168*1]),
 .AA(RW0_addra),
 .AB(RW0_addrb),
 .D({3'd0,RW0_wdata[333-1:168*1]}),
 .WEB(WEB),
 .REB(REB),
 .CLK(RW0_clk),
 .RTSEL(2'b01),
 .WTSEL(2'b00),
 .MTSEL(2'b01)
 );
`endif



endmodule
