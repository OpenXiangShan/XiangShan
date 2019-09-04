import "DPI-C" function void ram_helper
(
  input  int    rIdx,
  output int    rdata,
  input  int    wIdx,
  input  int    wdata,
  input  int    wmask,
  input  bit    wen
);

module RAMHelper(
  input         clk,
  input  [31:0] rIdx,
  output [31:0] rdata,
  input  [31:0] wIdx,
  input  [31:0] wdata,
  input  [31:0] wmask,
  input         wen
);

  always @(posedge clk) begin
    ram_helper(rIdx, rdata, wIdx, wdata, wmask, wen);
  end

endmodule

