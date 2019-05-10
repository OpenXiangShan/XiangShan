import "DPI-C" function void monitor
(
  input  int  trapCode
);

module Monitor(
  input         clk,
  input         isNoopTrap,
  input  [31:0] trapCode
);

  always @(posedge clk) begin
    if (isNoopTrap) monitor(trapCode);
  end

endmodule
