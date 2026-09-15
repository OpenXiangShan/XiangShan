/////////////////////////////////////////////////////
// iommu_acd_mtlb_ecc_enc
/////////////////////////////////////////////////////
module iommu_acd_mtlb_ecc_enc import iommu_acd_pkg::*; #( //{{{
    parameter   DATA_WIDTH      = 128,
    parameter   ECC_WIDTH       = 8,
    parameter   SPARE_PARAM     = 0
)(
    input  logic [DATA_WIDTH-1:0]           dat_i,
    output logic [ECC_WIDTH:0]              ecc_o,

    input  logic                            spare_in
);
    localparam RIDX_WIDTH = ECC_WIDTH;
    localparam CIDX_WIDTH = DATA_WIDTH+ECC_WIDTH+2;

    logic [CIDX_WIDTH-1:0]  darray[RIDX_WIDTH-1:0];
    logic [CIDX_WIDTH-1:0]  larray;
    logic                   parity[RIDX_WIDTH-1:0];
    logic                   lparity;

genvar i,j;
generate
    for(i=0; i<RIDX_WIDTH; i++) begin: row_gen
        for(j=0; j<CIDX_WIDTH; j++) begin: col_gen
            if(j==0) begin: col0_gen                    // col0
                assign darray[i][j] = 1'b0;
            end
            else if(j==CIDX_WIDTH-1)begin: lastcol_gen
                assign darray[i][j] = 1'b0;
            end
            else if(p_col_check(j)==1) begin: p_col_gen // col1,2,4,8...2^x
                assign darray[i][j] = 1'b0;
            end
            else begin: d_col_gen                       // col data
                //logic [$clog2(DATA_WIDTH)-1:0] dbitidx;
                //assign dbitidx = d_bit_idx(j);
                localparam dbitidx = d_bit_idx(j);
                if(j[i]==1'b1) begin
                    assign darray[i][j]  = dat_i[dbitidx];
                end
                else begin
                    assign darray[i][j]  = 1'b0;
                end
            end
        end
    end
endgenerate

genvar k;
generate
//    always@(*) begin
//        for(int unsigned k=0; k<=RIDX_WIDTH-1; k++) begin: non_last_array_parity_gen
        for(k=0; k<=RIDX_WIDTH-1; k++) begin: parity_cal_gen
            assign parity[k] = ^darray[k];
        end
//    end
endgenerate

genvar m;
generate
    for(m=0; m<CIDX_WIDTH; m++) begin: lastrow_gen
        if(m==0) begin: col0_gen
            assign larray[m] = 1'b0;
        end
        else if(m==CIDX_WIDTH-1) begin: lastcol_gen
            assign larray[m] = 1'b0;
        end
        else if(p_col_check(m)==1) begin: p_col_gen
            //logic [$clog2(CIDX_WIDTH):0] pbitidx;
            //assign pbitidx = p_bit_idx(m);
            localparam pbitidx = p_bit_idx(m);
            assign larray[m] = parity[pbitidx];
        end
        else begin : d_col_gen
            //logic [$clog2(DATA_WIDTH)-1:0] dbitidx;
            //assign dbitidx = d_bit_idx(m);
            localparam dbitidx = d_bit_idx(m);
            assign larray[m] = dat_i[dbitidx];
        end
    end
endgenerate

    assign lparity = ^larray;

genvar n;
generate
//    always@(*) begin
        for(n=0; n<ECC_WIDTH; n++) begin
            assign ecc_o[n] = parity[n];
        end

        assign ecc_o[ECC_WIDTH] = lparity;
//    end
endgenerate





endmodule
//}}}



/////////////////////////////////////////////////////
// iommu_acd_mtlb_ecc_dec
/////////////////////////////////////////////////////
module iommu_acd_mtlb_ecc_dec import iommu_acd_pkg::*; #( //{{{
    parameter   DATA_WIDTH      = 128,
    parameter   ECC_WIDTH       = 8,
    parameter   SPARE_PARAM     = 0
)(
    input  logic [DATA_WIDTH-1:0]           dat_i,
    input  logic [ECC_WIDTH:0]              ecc_i,
    output logic [DATA_WIDTH-1:0]           dat_o,
    output logic [1:0]                      err_o,

    input  logic                            spare_in
);
    localparam RIDX_WIDTH = ECC_WIDTH+1;
    localparam CIDX_WIDTH = DATA_WIDTH+ECC_WIDTH+2;

    logic [CIDX_WIDTH-1:0]  darray[RIDX_WIDTH-1:0];
    logic                   parity[RIDX_WIDTH-1:0];
    logic [RIDX_WIDTH-1:0]  parity_bits;

    logic [DATA_WIDTH-1:0]  correct_bits;
    logic [RIDX_WIDTH-1:0]  dposidx[DATA_WIDTH-1:0];

    logic [ECC_WIDTH:0]     ecc_enc;
    logic [ECC_WIDTH:0]     ecc_xor;
    logic [RIDX_WIDTH-2:0]  d_pos_idx_l[DATA_WIDTH-1:0];
    logic                   d_pos_idx_h[DATA_WIDTH-1:0];
    iommu_acd_mtlb_ecc_enc  #(
    /*parameter  */ .DATA_WIDTH     (DATA_WIDTH     ), // = 128,
    /*parameter  */ .ECC_WIDTH      (ECC_WIDTH      ), // = 8,
    /*parameter  */ .SPARE_PARAM    (0              )  // = 0
    ) U_enc(
    /*input  logic [DATA_WIDTH-1:0]         */  .dat_i          (dat_i      ),
    /*output logic [ECC_WIDTH:0]            */  .ecc_o          (ecc_enc    ),
    /*input  logic                          */  .spare_in       (1'b0       )
    );

    assign ecc_xor = ecc_enc ^ ecc_i;

    always@(*) begin
        for(int unsigned n=0; n<DATA_WIDTH; n++) begin
            d_pos_idx_l[n] = (n+1)+$clog2(1+(n+1)+$clog2(1+(n+1)));
            d_pos_idx_h[n] = ~(^d_pos_idx_l[n]);
            if({d_pos_idx_h[n],d_pos_idx_l[n]}==ecc_xor)
                correct_bits[n] = 1'b1;
            else
                correct_bits[n] = 1'b0;
        end
    end
    
    assign dat_o = dat_i^correct_bits;
    assign err_o[1] = (|ecc_xor) & (~(^ecc_xor));
    assign err_o[0] = (|ecc_xor) & (  ^ecc_xor) ;

//{{{
//  genvar i,j;
//  generate
//      for(i=0; i<RIDX_WIDTH-1; i++) begin: row_gen
//          for(j=0; j<CIDX_WIDTH; j++) begin: col_gen
//              if(j==0) begin: col0_gen                       // col0
//                  assign darray[i][j] = 1'b0;
//              end
//              else if(j==CIDX_WIDTH-1) begin: lastcol_gen
//                  assign darray[i][j] = 1'b0;
//              end
//              else if(p_col_check(j)==1) begin: p_col_gen     // col1,2,4,8...2^x
//                  if(2**i==j) begin: diagonal_col_gen         // diagonal
//                      logic [$clog2(ECC_WIDTH):0] pbitidx;
//                      assign pbitidx = p_bit_idx(j);
//                      assign darray[i][j] = ecc_i[pbitidx];
//                  end
//                  else begin: nondiagonal_col_gen
//                      assign darray[i][j] = 1'b0;
//                  end
//              end
//              else begin: d_col_gen
//                  logic [$clog2(DATA_WIDTH):0] dbitidx;
//                  assign dbitidx = d_bit_idx(j);
//                  if(j[i]==1'b1) begin
//                      assign darray[i][j]  = dat_i[dbitidx];
//                  end
//                  else begin
//                      assign darray[i][j]  = 1'b0;
//                  end
//              end
//          end
//      end
//  endgenerate
//  
//      always@(*) begin
//          for(int unsigned k=0; k<RIDX_WIDTH-1; k++) begin
//              parity[k] = ^darray[k];
//          end
//      end
//  
//  genvar m;
//  generate
//      for(m=0; m<CIDX_WIDTH; m++) begin: lastrow_gen
//          if(m==0) begin: col0_gen
//              assign darray[RIDX_WIDTH-1][m] = 1'b0;
//          end
//          else if(m==CIDX_WIDTH-1) begin: lastcol_gen
//              assign darray[RIDX_WIDTH-1][m] = ecc_i[ECC_WIDTH];
//          end
//          else if(p_col_check(m)==1) begin: p_col_gen
//              logic [$clog2(ECC_WIDTH):0] pbitidx;
//              assign pbitidx = p_bit_idx(m);
//              assign darray[RIDX_WIDTH-1][m] = ecc_i[pbitidx];
//          end
//          else begin: d_col_gen
//              logic [$clog2(DATA_WIDTH):0] dbitidx;
//              assign dbitidx = d_bit_idx(m);
//              assign darray[RIDX_WIDTH-1][m] = dat_i[dbitidx];
//          end
//      end
//  endgenerate
//  
//      assign parity[RIDX_WIDTH-1] = ^darray[RIDX_WIDTH-1];
//  
//      always@(*) begin
//          for(int unsigned p=0; p<RIDX_WIDTH; p++) begin
//              parity_bits[p] = parity[p];
//          end
//      end
//  
//      always@(*) begin
//          for(int unsigned n=0; n<DATA_WIDTH; n++) begin
//              dposidx[n] = d_col_cal(n);
//              if(dposidx[n][RIDX_WIDTH-2:0]==parity_bits[RIDX_WIDTH-2:0]) begin
//                  correct_bits[n] = 1'b1;
//              end
//              else begin
//                  correct_bits[n] = 1'b0;
//              end
//          end
//      end
//      
//      assign dat_o = dat_i^correct_bits;
//      assign err_o[0] = (|parity_bits) & (~(^parity_bits));
//      assign err_o[1] = (|parity_bits) & (  ^parity_bits) ;
//}}}
endmodule
//}}}


