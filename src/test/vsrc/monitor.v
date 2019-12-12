`ifdef VERILATOR
import "DPI-C" function void monitor
(
  input  int  trapCode,
  input  longint  trapPC,
  input  longint  cycleCnt,
  input  longint  instrCnt
);
`endif

module Monitor(
  input         clk,
  input         reset,
  input         isNoopTrap,
  input  [31:0] trapCode,
  input  [63:0] trapPC,
  input  [63:0] cycleCnt,
  input  [63:0] instrCnt
);

`ifdef VERILATOR
  always @(posedge clk) begin
     monitor((isNoopTrap && !reset) ? trapCode : -1,
       trapPC, cycleCnt, instrCnt);
  end
`endif

endmodule
