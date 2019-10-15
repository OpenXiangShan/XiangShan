`include "axi.vh"

module system_top (
  output [7:0] led,
  inout [14:0] DDR_addr,
  inout [2:0] DDR_ba,
  inout DDR_cas_n,
  inout DDR_ck_n,
  inout DDR_ck_p,
  inout DDR_cke,
  inout DDR_cs_n,
  inout [3:0] DDR_dm,
  inout [31:0] DDR_dq,
  inout [3:0] DDR_dqs_n,
  inout [3:0] DDR_dqs_p,
  inout DDR_odt,
  inout DDR_ras_n,
  inout DDR_reset_n,
  inout DDR_we_n,
  inout FIXED_IO_ddr_vrn,
  inout FIXED_IO_ddr_vrp,
  inout [53:0] FIXED_IO_mio,
  inout FIXED_IO_ps_clk,
  inout FIXED_IO_ps_porb,
  inout FIXED_IO_ps_srstb,

  output [3:0] VGA_r,
  output [3:0] VGA_g,
  output [3:0] VGA_b,
  output VGA_hsync,
  output VGA_vsync
);

  `axi_wire(AXI_MEM_MAPPED, 64, 8);
  `axi_wire(AXI_MEM, 64, 8);

  wire coreclk;
  wire corerstn;
  wire clk50;
  wire rstn50;
  wire uncoreclk;
  wire uncorerstn;

  wire noop_uart_tx;
  wire noop_uart_rx;

  zynq_soc zynq_soc_i (
    .DDR_addr(DDR_addr),
    .DDR_ba(DDR_ba),
    .DDR_cas_n(DDR_cas_n),
    .DDR_ck_n(DDR_ck_n),
    .DDR_ck_p(DDR_ck_p),
    .DDR_cke(DDR_cke),
    .DDR_cs_n(DDR_cs_n),
    .DDR_dm(DDR_dm),
    .DDR_dq(DDR_dq),
    .DDR_dqs_n(DDR_dqs_n),
    .DDR_dqs_p(DDR_dqs_p),
    .DDR_odt(DDR_odt),
    .DDR_ras_n(DDR_ras_n),
    .DDR_reset_n(DDR_reset_n),
    .DDR_we_n(DDR_we_n),
    .FIXED_IO_ddr_vrn(FIXED_IO_ddr_vrn),
    .FIXED_IO_ddr_vrp(FIXED_IO_ddr_vrp),
    .FIXED_IO_mio(FIXED_IO_mio),
    .FIXED_IO_ps_clk(FIXED_IO_ps_clk),
    .FIXED_IO_ps_porb(FIXED_IO_ps_porb),
    .FIXED_IO_ps_srstb(FIXED_IO_ps_srstb),

    `axi_connect_if(AXI_MEM, AXI_MEM_MAPPED),

    // invert connection
    .uart_txd(noop_uart_rx),
    .uart_rxd(noop_uart_tx),

    .coreclk(coreclk),
    .corerstn(corerstn),
    .clk50(clk50),
    .rstn50(rstn50),
    .uncoreclk(uncoreclk),
    .uncorerstn(uncorerstn)
  );

  addr_mapper addr_mapper_i(
    `axi_connect_if(s_axi, AXI_MEM),
    `axi_connect_if(m_axi, AXI_MEM_MAPPED)
  );

  reg corerstn_ff;
  always@(posedge uncoreclk) begin
    corerstn_ff <= corerstn;
  end

  reg corerstn_sync[1:0];
  always@(posedge coreclk) begin
    corerstn_sync[0] <= corerstn_ff;
    corerstn_sync[1] <= corerstn_sync[0];
  end

  noop noop_i(
    `axi_connect_if(AXI_MEM, AXI_MEM),

    .uart_txd(noop_uart_tx),
    .uart_rxd(noop_uart_rx),

    .VGA_b(VGA_b),
    .VGA_r(VGA_r),
    .VGA_g(VGA_g),
    .VGA_hsync(VGA_hsync),
    .VGA_vsync(VGA_vsync),

    .coreclk(coreclk),
    .corerstn(corerstn_sync[1]),
    .clk50(clk50),
    .rstn50(rstn50),
    .uncoreclk(uncoreclk),
    .uncorerstn(uncorerstn)
  );

endmodule
