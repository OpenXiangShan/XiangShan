import "DPI-C" function void ram_helper
(
  input  longint    rIdx,
  output longint    rdata,
  input  longint    wIdx,
  input  longint    wdata,
  input  longint    wmask,
  input  bit    wen
);

module RAMHelper(
  input         clk,
  input  [63:0] rIdx,
  output [63:0] rdata,
  input  [63:0] wIdx,
  input  [63:0] wdata,
  input  [63:0] wmask,
  input         wen
);

  always @(posedge clk) begin
    ram_helper(rIdx, rdata, wIdx, wdata, wmask, wen);
  end

endmodule

