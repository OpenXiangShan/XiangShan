`include "axi.vh"

module system_top (
  //inout  hdmi_scl,
  //inout  hdmi_sda,
  //output hdmi_nreset,
  //output hdmi_clk,
  //output hdmi_hsync,
  //output hdmi_vsync,
  //output hdmi_videovalid,
  //output [23:0] hdmi_rgb
  //output [7:0] led
);

  `axi_wire(AXI_MEM_MAPPED, 64, 8);
  `axi_wire(AXI_MEM, 64, 8);

  wire coreclk;
  wire corerstn;
  wire clk50;
  wire clk27;
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
    .clk27(clk27),
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

    //.VGA_rgb(hdmi_rgb),
    //.VGA_hsync(hdmi_hsync),
    //.VGA_vsync(hdmi_vsync),
    //.VGA_videovalid(hdmi_videovalid),

    .coreclk(coreclk),
    .corerstn(corerstn_sync[1]),
    .clk50(clk50),
    .rstn50(rstn50),
    .uncoreclk(uncoreclk),
    .uncorerstn(uncorerstn)
  );

  //i2c_config hdmi_i2c_config(
  //  .rst(!uncorerstn),
  //  .clk(clk27),
  //  .i2c_scl(hdmi_scl),
  //  .i2c_sda(hdmi_sda)
  //);

  //assign hdmi_nreset = uncorerstn;
  //assign hdmi_clk = clk50;

endmodule
