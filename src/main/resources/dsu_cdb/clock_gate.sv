// +FHDR------------------------------------------------------------
//                 Copyright (c) 2025 .
//                       ALL RIGHTS RESERVED
// -----------------------------------------------------------------
// Filename      : clock_gate.sv
// Author        : 
// Created On    : 2025-06-12 11:43
// Last Modified : 
// -----------------------------------------------------------------
// Description:
//
//
// -FHDR------------------------------------------------------------
`include "common_defines.sv"
`include "router_define.sv"

module clock_gate (
                input   wire    clk     ,
                input   wire    se      ,
                input   wire    clk_en  ,
                output  wire    cg
);
    reg  clk_en_lat ;
    wire enable     ;
    wire se_i       ;

    `ifdef  DSU_FPGA
        assign cg = clk;
    `else
        `ifdef DSU_DFT
            assign se_i = se;
        `else
            assign se_i = 1'b0;
        `endif
        
        assign enable = clk_en | se_i;

        `ifdef DSU_DC
            CKLNQD4BWP6T16P96CPDULVT latch (.CP(clk), .E(clk_en), .TE(se_i), .Q(cg));
        `else
            always @(clk or enable) begin: U_CLK_EN_LATCH
                if (clk == 1'b0 && 1'b1 == 1'b1)
                    clk_en_lat <= #`NOC_LAT_DELAY enable ;
            end

            assign cg = clk & clk_en_lat;
        `endif
    `endif

endmodule


