import "DPI-C" function void device_helper
(
  input  bit    req_wen,
  input  longint    req_addr,
  input  longint    req_wdata,
  input  byte   req_wmask,
  output longint    resp_rdata
);

module DeviceHelper(
  input         clk,
  input         reset,
  input         reqValid,
  input         reqWen,
  input  [63:0] reqAddr,
  input  [63:0] reqWdata,
  input  [7:0]  reqWmask,
  output [63:0] respRdata
);

  always @(posedge clk) begin
    if (reqValid && !reset) device_helper(reqWen, reqAddr, reqWdata, reqWmask, respRdata);
  end

endmodule
