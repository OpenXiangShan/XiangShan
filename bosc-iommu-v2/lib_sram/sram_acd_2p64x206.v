/*
 * Copyright (C) 2026 Beijing Institute of Open Source Chip
 *
 * Description: SRAM wrapper for XSCore
 * Author: VeriOps Group
 * Date: Mon May 19 14:52:42 2026
 */

module sram_acd_2p64x206 #(
	parameter 	AWIDTH  = 6,
	parameter 	DWIDTH  = 206
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

TS6N12FFCLLULVTB64X206M1 u_mem(
 .AA        (RW0_addra  ),
 .D         (RW0_wdata  ),
 .WEB       (WEB        ),
 .AB        (RW0_addrb  ),
 .REB       (REB        ),
 .CLK       (RW0_clk    ),
 .RTSEL     (2'b01      ),
 .WTSEL     (2'b01      ),
 .MTSEL     (2'b01      ),
 .Q         (RW0_rdata  )
 );

`endif



endmodule
