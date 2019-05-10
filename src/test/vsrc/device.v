import "DPI-C" function void device_helper
(
  input  bit    req_wen,
  input  int    req_addr,
  input  int    req_wdata,
  input  byte   req_wmask,
  output int    resp_rdata
);

module DeviceHelper(
  input         clk,
  input         reqValid,
  input         reqWen,
  input  [31:0] reqAddr,
  input  [31:0] reqWdata,
  input  [3:0]  reqWmask,
  output [31:0] respRdata
);

  always @(posedge clk) begin
    if (reqValid) device_helper(reqWen, reqAddr, reqWdata, reqWmask, respRdata);
  end

endmodule
