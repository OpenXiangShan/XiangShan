module STD_CLKGT_func (
  input  wire TE,
  input  wire E,
  input  wire CK,
  output wire Q
);

  wire clk_en;
  reg  clk_en_reg;

  assign clk_en = E | TE;

`ifdef VERILATOR_5
  always @(CK or clk_en) begin
    if (CK == 1'b0)
      clk_en_reg <= clk_en;
  end
`else
  always @(posedge CK) 
    begin
      clk_en_reg = clk_en;
    end
`endif

  assign Q = CK & clk_en_reg;

endmodule