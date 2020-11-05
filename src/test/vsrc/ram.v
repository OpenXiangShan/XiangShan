import "DPI-C" function void ram_write_helper
(
  input  longint    wIdx,
  input  longint    wdata,
  input  longint    wmask,
  input  bit    wen
);

import "DPI-C" function longint ram_read_helper
(
  input  longint    rIdx
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

  assign rdata = ram_read_helper(rIdx);

  always @(posedge clk) begin
    ram_write_helper(wIdx, wdata, wmask, wen);
  end

endmodule

