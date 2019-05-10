import "DPI-C" function void monitor
(
  input  int  trapCode,
  input  int  trapPC,
  input  int  cycleCnt,
  input  int  instrCnt
);

module Monitor(
  input         clk,
  input         isNoopTrap,
  input  [31:0] trapCode,
  input  [31:0] trapPC,
  input  [31:0] cycleCnt,
  input  [31:0] instrCnt
);

  always @(posedge clk) begin
    if (isNoopTrap) monitor(
      trapCode, trapPC,
      cycleCnt, instrCnt
    );
  end

endmodule
