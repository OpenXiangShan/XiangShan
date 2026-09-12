/*
 * Copyright (C) 2026 Beijing Institute of Open Source Chip
 *
 * Description: SRAM wrapper for XSCore
 * Author: VeriOps Group
 * Date: Mon May 19 14:52:42 2026
 */

module sram_acd_64x197 #(
	parameter 	AWIDTH  = 6,
	parameter 	DWIDTH  = 197
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

wire [197:0] rdata;
assign RW0_rdata = rdata[196:0]; 

TS5N12FFCLLL1ULVTA64X99M2W u0_mem(
 .Q(rdata[98:0]),
 .A(RW0_addr),
 .D(RW0_wdata[98:0]),
 .WEB(WEB),
 .CEB(CEB),
 .CLK(RW0_clk),
 .MCR(2'b00),
 .MCW(2'b00),
 .BWEB(99'd0)
);


TS5N12FFCLLL1ULVTA64X99M2W u1_mem(
 .Q(rdata[197:99]),
 .A(RW0_addr),
 .D({1'b0,RW0_wdata[196:99]}),
 .WEB(WEB),
 .CEB(CEB),
 .CLK(RW0_clk),
 .MCR(2'b00),
 .MCW(2'b00),
 .BWEB(99'd0)
);
`endif


endmodule
