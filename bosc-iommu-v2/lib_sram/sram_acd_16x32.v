/*
 * Copyright (C) 2026 Beijing Institute of Open Source Chip
 *
 * Description: SRAM wrapper for XSCore
 * Author: VeriOps Group
 * Date: Mon May 19 14:52:42 2026
 */

module sram_acd_16x32 #(
	parameter 	AWIDTH  = 4,
	parameter 	DWIDTH  = 32
)(
	input					RW0_clk,
	input  [AWIDTH-1:0]		RW0_addr,
	input					RW0_en,
	input					RW0_wmode,
	input  [DWIDTH-1:0]		RW0_wdata,
	output [DWIDTH-1:0]		RW0_rdata
);

wire CEB;
assign CEB = ~RW0_en;
wire WEB;
assign WEB = ~RW0_wmode;

`ifdef RTL_RAM_SIM
reg [DWIDTH-1:0]	mem[(1<<AWIDTH)-1:0];
reg [DWIDTH-1:0]	memreg;

assign RW0_rdata = memreg;

always@(posedge RW0_clk) begin
	if (RW0_wmode && RW0_en) begin
        mem[RW0_addr] <= RW0_wdata;
	end
end

always@(posedge RW0_clk) begin
	if (~RW0_wmode && RW0_en) begin
        memreg <= mem[RW0_addr];
	end
end

`endif

`ifdef FPGA_RAM_SIM
reg [DWIDTH-1:0]	mem[(1<<AWIDTH)-1:0];
reg [DWIDTH-1:0]	memreg;

assign RW0_rdata = memreg;

always@(posedge RW0_clk) begin
	if (RW0_en) 
		if (RW0_wmode)
			mem[RW0_addr] <= RW0_wdata;
		else
			memreg <= mem[RW0_addr];
end
`endif


`ifdef ASIC_RAM_SIM

TS5N12FFCLLULVTA16X32M2S u_mem(
 .Q(RW0_rdata),
 .A(RW0_addr),
 .D(RW0_wdata),
 .WEB(WEB),
 .CEB(CEB),
 .CLK(RW0_clk),
 .RTSEL(2'b01),
 .WTSEL(2'b00)
 );
`endif


endmodule
