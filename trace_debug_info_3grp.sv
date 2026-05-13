module trace_debug_info_3grp (
  input              clk,
  input              rst_n,

  input      [149:0] iaddr_i,
  input      [20:0]  iretire_i,
  input      [2:0]   ilastsize_i,
  input      [11:0]  itype_i,
  input      [2:0]   valid_i,
  /* verilator lint_off UNUSEDSIGNAL */
  input      [63:0]  cause_i,
  /* verilator lint_on UNUSEDSIGNAL */
  input      [2:0]   priv_i,
  input      [63:0]  status_i,

  output     [127:0] debug_info_o
);

  wire [49:0] iaddr_g0;
  wire [49:0] iaddr_g1;
  wire [49:0] iaddr_g2;

  wire [6:0]  iretire_g0;
  wire [6:0]  iretire_g1;
  wire [6:0]  iretire_g2;

  wire        ilast_g0;
  wire        ilast_g1;
  wire        ilast_g2;

  wire [3:0]  itype_g0;
  wire [3:0]  itype_g1;
  wire [3:0]  itype_g2;

  reg         sel_valid;
  reg  [49:0] sel_iaddr;
  reg  [6:0]  sel_iretire;
  reg         sel_ilastsize;
  reg  [3:0]  sel_itype;

  reg  [49:0] pc_calc;
  reg  [49:0] pc_q;
  reg  [3:0]  itype_q;
  reg  [6:0]  cause_q;

  assign iaddr_g0   = iaddr_i[49:0];
  assign iaddr_g1   = iaddr_i[99:50];
  assign iaddr_g2   = iaddr_i[149:100];

  assign iretire_g0 = iretire_i[6:0];
  assign iretire_g1 = iretire_i[13:7];
  assign iretire_g2 = iretire_i[20:14];

  assign ilast_g0   = ilastsize_i[0];
  assign ilast_g1   = ilastsize_i[1];
  assign ilast_g2   = ilastsize_i[2];

  assign itype_g0   = itype_i[3:0];
  assign itype_g1   = itype_i[7:4];
  assign itype_g2   = itype_i[11:8];

  always @(*) begin
    sel_valid     = 1'b0;
    sel_iaddr     = 50'd0;
    sel_iretire   = 7'd0;
    sel_ilastsize = 1'b0;
    sel_itype     = 4'd0;

    if (valid_i[2]) begin
      sel_valid     = 1'b1;
      sel_iaddr     = iaddr_g2;
      sel_iretire   = iretire_g2;
      sel_ilastsize = ilast_g2;
      sel_itype     = itype_g2;
    end
    else if (valid_i[1]) begin
      sel_valid     = 1'b1;
      sel_iaddr     = iaddr_g1;
      sel_iretire   = iretire_g1;
      sel_ilastsize = ilast_g1;
      sel_itype     = itype_g1;
    end
    else if (valid_i[0]) begin
      sel_valid     = 1'b1;
      sel_iaddr     = iaddr_g0;
      sel_iretire   = iretire_g0;
      sel_ilastsize = ilast_g0;
      sel_itype     = itype_g0;
    end
  end

  always @(*) begin
    pc_calc = sel_iaddr + ((({43'd0, sel_iretire}) - (({43'd0, 7'd1}) << sel_ilastsize)) << 1);
  end

  always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
      pc_q    <= 50'd0;
      itype_q <= 4'd0;
      cause_q <= 7'd0;
    end
    else if (sel_valid) begin
      pc_q    <= pc_calc;
      itype_q <= sel_itype;

      if ((sel_itype == 4'd1) || (sel_itype == 4'd2)) begin
        cause_q <= {cause_i[63], cause_i[5:0]};
      end
    end
  end

  assign debug_info_o = {
    status_i[63:0],
    itype_q[3:0],
    priv_i[2:0],
    cause_q[6:0],
    pc_q[49:0]
  };

endmodule
