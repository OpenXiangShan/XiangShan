import "DPI-C" function byte pte_helper (
  input  longint satp,
  input  longint vpn,
  output longint pte,
  output byte    level
);

module PTEHelper(
  input             clock,
  input             enable,
  input      [63:0] satp,
  input      [63:0] vpn,
  output reg [63:0] pte,
  output reg [ 7:0] level,
  output reg [ 7:0] pf
);
  always @(posedge clock) begin
    if (enable) begin
      pf <= pte_helper(satp, vpn, pte, level);
    end
  end
endmodule

import "DPI-C" function longint amo_helper(
  input byte    cmd,
  input longint addr,
  input longint wdata,
  input byte    mask
);

module AMOHelper(
  input             clock,
  input             enable,
  input      [ 4:0] cmd,
  input      [63:0] addr,
  input      [63:0] wdata,
  input      [ 7:0] mask,
  output reg [63:0] rdata
);

  always @(posedge clock) begin
    if (enable) begin
      rdata <= amo_helper(cmd, addr, wdata, mask);
    end
  end

endmodule
