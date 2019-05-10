import "DPI-C" function void device_helper
(
  input  bit    req_wen,
  input  int    req_addr,
  input  int    req_wdata,
  output int    resp_rdata
);

module DeviceHelper(
  input         clk,
  input         reqValid,
  input         reqWen,
  input  [31:0] reqAddr,
  input  [31:0] reqWdata,
  output [31:0] respRdata
);

  always @(posedge clk) begin
    if (reqValid) device_helper(reqWen, reqAddr, reqWdata, respRdata);
  end

endmodule
