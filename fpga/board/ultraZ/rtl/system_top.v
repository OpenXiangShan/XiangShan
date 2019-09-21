`include "axi.vh"

module system_top (
  output [7:0] led
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

    //.VGA_b(VGA_b),
    //.VGA_r(VGA_r),
    //.VGA_g(VGA_g),
    //.VGA_hsync(VGA_hsync),
    //.VGA_vsync(VGA_vsync),

    .coreclk(coreclk),
    .corerstn(corerstn_sync[1]),
    .clk50(clk50),
    .rstn50(rstn50),
    .uncoreclk(uncoreclk),
    .uncorerstn(uncorerstn)
  );


endmodule
