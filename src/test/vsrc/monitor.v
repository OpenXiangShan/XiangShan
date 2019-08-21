`ifdef VERILATOR
import "DPI-C" function void monitor
(
  input  int  trapCode,
  input  int  trapPC,
  input  int  cycleCnt,
  input  int  instrCnt
);
`endif

module Monitor(
  input         clk,
  input         reset,
  input         isNoopTrap,
  input  [31:0] trapCode,
  input  [31:0] trapPC,
  input  [31:0] cycleCnt,
  input  [31:0] instrCnt
);

`ifdef VERILATOR
  always @(posedge clk) begin
    if (isNoopTrap && !reset) monitor(
      trapCode, trapPC,
      cycleCnt, instrCnt
    );
  end
`endif

endmodule
