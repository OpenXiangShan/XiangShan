////////////////////////////////////////////
// iommu_atd_ptc_mtlb_idx
////////////////////////////////////////////
module iommu_atd_ptc_mtlb_idx #(
    parameter   CACHE_TYPE      = 0, // 0:S2PTC, 1:S1PTC
    parameter   IDX_WIDTH       = 7
)(
    input  logic                            gv,
    input  logic [15:0]                     gscid,
    input  logic [19:0]                     pscid,
    input  logic [63:12]                    addr,
    input  logic [2:0]                      lvl,
    output logic [IDX_WIDTH-1:0]            idx
);
    logic [96:0]                            idx_in_vec;
    logic [15:0]                            idx_raw;        // by now, only support max 16bit idx gen, enough for current design

    logic [15:0]                            gscid_masked;
    logic [19:0]                            pscid_masked;
    logic [63:12]                           addr_masked;

    assign gscid_masked = gv ? gscid : 'd0;
    assign pscid_masked = CACHE_TYPE=='d0 ? 'd0  :   // S2PTC, no pscid for idx cal
                                           pscid;
    assign addr_masked  = lvl=='d0 ? {addr[63:16], 4'd0 } : // always ignore [15:12] for svnapot
                          lvl=='d1 ? {addr[63:22], 10'd0} : // always ignore [21] for Sv32/Sv32x4
                          lvl=='d2 ? {addr[63:30], 18'd0} :
                          lvl=='d3 ? {addr[63:39], 27'd0} :
                          lvl=='d4 ? {addr[63:48], 36'd0} :
                                     {addr[63:16], 4'd0 };
generate
    if(CACHE_TYPE=='d0) begin : s2ptc_idx_gen
        assign idx_in_vec = {29'd0, addr_masked, gscid_masked};
    end
    else begin : s1ptc_idx_gen
        assign idx_in_vec = {9'd0, addr_masked, pscid_masked, gscid_masked};
    end
endgenerate

    assign idx_raw[ 0] = idx_in_vec[81] ^ idx_in_vec[78] ^idx_in_vec[76] ^idx_in_vec[75] ^idx_in_vec[70] ^idx_in_vec[69] ^idx_in_vec[68] ^idx_in_vec[64] ^idx_in_vec[63] ^idx_in_vec[59] ^idx_in_vec[56] ^idx_in_vec[50] ^idx_in_vec[49] ^idx_in_vec[47] ^idx_in_vec[45] ^idx_in_vec[44] ^idx_in_vec[42] ^idx_in_vec[40] ^idx_in_vec[37] ^idx_in_vec[34] ^idx_in_vec[32] ^idx_in_vec[31] ^idx_in_vec[29] ^idx_in_vec[25] ^idx_in_vec[22] ^idx_in_vec[20] ^idx_in_vec[19] ^idx_in_vec[18] ^idx_in_vec[17] ^idx_in_vec[16] ^idx_in_vec[11] ^idx_in_vec[ 8] ^idx_in_vec[ 3] ^idx_in_vec[ 2] ^idx_in_vec[ 1] ^idx_in_vec[ 0] ;
    assign idx_raw[ 1] = idx_in_vec[88] ^ idx_in_vec[77] ^idx_in_vec[75] ^idx_in_vec[73] ^idx_in_vec[70] ^idx_in_vec[67] ^idx_in_vec[66] ^idx_in_vec[65] ^idx_in_vec[64] ^idx_in_vec[63] ^idx_in_vec[62] ^idx_in_vec[58] ^idx_in_vec[57] ^idx_in_vec[55] ^idx_in_vec[49] ^idx_in_vec[46] ^idx_in_vec[45] ^idx_in_vec[44] ^idx_in_vec[41] ^idx_in_vec[40] ^idx_in_vec[39] ^idx_in_vec[34] ^idx_in_vec[32] ^idx_in_vec[26] ^idx_in_vec[21] ^idx_in_vec[20] ^idx_in_vec[18] ^idx_in_vec[17] ^idx_in_vec[15] ^idx_in_vec[14] ^idx_in_vec[11] ^idx_in_vec[10] ^idx_in_vec[ 9] ^idx_in_vec[ 8] ^idx_in_vec[ 7] ^idx_in_vec[ 6] ;
    assign idx_raw[ 2] = idx_in_vec[87] ^ idx_in_vec[76] ^idx_in_vec[68] ^idx_in_vec[67] ^idx_in_vec[62] ^idx_in_vec[61] ^idx_in_vec[60] ^idx_in_vec[59] ^idx_in_vec[57] ^idx_in_vec[56] ^idx_in_vec[53] ^idx_in_vec[51] ^idx_in_vec[50] ^idx_in_vec[48] ^idx_in_vec[47] ^idx_in_vec[44] ^idx_in_vec[43] ^idx_in_vec[41] ^idx_in_vec[37] ^idx_in_vec[33] ^idx_in_vec[30] ^idx_in_vec[29] ^idx_in_vec[28] ^idx_in_vec[26] ^idx_in_vec[16] ^idx_in_vec[15] ^idx_in_vec[14] ^idx_in_vec[13] ^idx_in_vec[11] ^idx_in_vec[10] ^idx_in_vec[ 9] ^idx_in_vec[ 7] ^idx_in_vec[ 6] ^idx_in_vec[ 5] ^idx_in_vec[ 4] ^idx_in_vec[ 1] ;
    assign idx_raw[ 3] = idx_in_vec[84] ^ idx_in_vec[74] ^idx_in_vec[73] ^idx_in_vec[72] ^idx_in_vec[69] ^idx_in_vec[63] ^idx_in_vec[61] ^idx_in_vec[58] ^idx_in_vec[56] ^idx_in_vec[54] ^idx_in_vec[53] ^idx_in_vec[46] ^idx_in_vec[45] ^idx_in_vec[44] ^idx_in_vec[42] ^idx_in_vec[38] ^idx_in_vec[36] ^idx_in_vec[35] ^idx_in_vec[31] ^idx_in_vec[30] ^idx_in_vec[28] ^idx_in_vec[26] ^idx_in_vec[25] ^idx_in_vec[24] ^idx_in_vec[23] ^idx_in_vec[20] ^idx_in_vec[19] ^idx_in_vec[18] ^idx_in_vec[18] ^idx_in_vec[15] ^idx_in_vec[12] ^idx_in_vec[10] ^idx_in_vec[ 7] ^idx_in_vec[ 5] ^idx_in_vec[ 3] ^idx_in_vec[ 2] ;
    assign idx_raw[ 4] = idx_in_vec[85] ^ idx_in_vec[77] ^idx_in_vec[76] ^idx_in_vec[75] ^idx_in_vec[73] ^idx_in_vec[72] ^idx_in_vec[71] ^idx_in_vec[70] ^idx_in_vec[69] ^idx_in_vec[68] ^idx_in_vec[67] ^idx_in_vec[65] ^idx_in_vec[64] ^idx_in_vec[63] ^idx_in_vec[61] ^idx_in_vec[60] ^idx_in_vec[59] ^idx_in_vec[54] ^idx_in_vec[52] ^idx_in_vec[50] ^idx_in_vec[47] ^idx_in_vec[42] ^idx_in_vec[41] ^idx_in_vec[36] ^idx_in_vec[35] ^idx_in_vec[31] ^idx_in_vec[31] ^idx_in_vec[22] ^idx_in_vec[19] ^idx_in_vec[17] ^idx_in_vec[13] ^idx_in_vec[11] ^idx_in_vec[ 9] ^idx_in_vec[ 3] ^idx_in_vec[ 2] ^idx_in_vec[ 0] ;
    assign idx_raw[ 5] = idx_in_vec[86] ^ idx_in_vec[78] ^idx_in_vec[76] ^idx_in_vec[74] ^idx_in_vec[69] ^idx_in_vec[68] ^idx_in_vec[67] ^idx_in_vec[66] ^idx_in_vec[64] ^idx_in_vec[62] ^idx_in_vec[61] ^idx_in_vec[57] ^idx_in_vec[55] ^idx_in_vec[54] ^idx_in_vec[53] ^idx_in_vec[51] ^idx_in_vec[50] ^idx_in_vec[49] ^idx_in_vec[41] ^idx_in_vec[40] ^idx_in_vec[39] ^idx_in_vec[37] ^idx_in_vec[33] ^idx_in_vec[31] ^idx_in_vec[28] ^idx_in_vec[27] ^idx_in_vec[23] ^idx_in_vec[21] ^idx_in_vec[17] ^idx_in_vec[16] ^idx_in_vec[14] ^idx_in_vec[13] ^idx_in_vec[12] ^idx_in_vec[11] ^idx_in_vec[ 9] ^idx_in_vec[ 5] ;
    assign idx_raw[ 6] = idx_in_vec[83] ^ idx_in_vec[77] ^idx_in_vec[70] ^idx_in_vec[69] ^idx_in_vec[63] ^idx_in_vec[61] ^idx_in_vec[60] ^idx_in_vec[58] ^idx_in_vec[57] ^idx_in_vec[55] ^idx_in_vec[52] ^idx_in_vec[49] ^idx_in_vec[48] ^idx_in_vec[47] ^idx_in_vec[46] ^idx_in_vec[43] ^idx_in_vec[40] ^idx_in_vec[37] ^idx_in_vec[33] ^idx_in_vec[29] ^idx_in_vec[28] ^idx_in_vec[27] ^idx_in_vec[23] ^idx_in_vec[22] ^idx_in_vec[19] ^idx_in_vec[13] ^idx_in_vec[12] ^idx_in_vec[11] ^idx_in_vec[10] ^idx_in_vec[ 8] ^idx_in_vec[ 5] ^idx_in_vec[ 4] ^idx_in_vec[ 3] ^idx_in_vec[ 2] ^idx_in_vec[ 1] ^idx_in_vec[ 0] ;
    assign idx_raw[ 7] = idx_in_vec[82] ^ idx_in_vec[79] ^idx_in_vec[75] ^idx_in_vec[73] ^idx_in_vec[69] ^idx_in_vec[68] ^idx_in_vec[66] ^idx_in_vec[63] ^idx_in_vec[62] ^idx_in_vec[55] ^idx_in_vec[52] ^idx_in_vec[45] ^idx_in_vec[43] ^idx_in_vec[39] ^idx_in_vec[37] ^idx_in_vec[36] ^idx_in_vec[35] ^idx_in_vec[34] ^idx_in_vec[32] ^idx_in_vec[30] ^idx_in_vec[28] ^idx_in_vec[27] ^idx_in_vec[26] ^idx_in_vec[23] ^idx_in_vec[22] ^idx_in_vec[18] ^idx_in_vec[15] ^idx_in_vec[13] ^idx_in_vec[ 9] ^idx_in_vec[ 8] ^idx_in_vec[ 8] ^idx_in_vec[ 6] ^idx_in_vec[ 5] ^idx_in_vec[ 4] ^idx_in_vec[ 3] ^idx_in_vec[ 2] ;
    assign idx_raw[ 8] = idx_in_vec[89] ^ idx_in_vec[79] ^idx_in_vec[77] ^idx_in_vec[75] ^idx_in_vec[73] ^idx_in_vec[71] ^idx_in_vec[67] ^idx_in_vec[65] ^idx_in_vec[57] ^idx_in_vec[56] ^idx_in_vec[53] ^idx_in_vec[52] ^idx_in_vec[48] ^idx_in_vec[44] ^idx_in_vec[42] ^idx_in_vec[40] ^idx_in_vec[39] ^idx_in_vec[36] ^idx_in_vec[34] ^idx_in_vec[32] ^idx_in_vec[31] ^idx_in_vec[28] ^idx_in_vec[27] ^idx_in_vec[24] ^idx_in_vec[22] ^idx_in_vec[19] ^idx_in_vec[17] ^idx_in_vec[13] ^idx_in_vec[11] ^idx_in_vec[10] ^idx_in_vec[ 9] ^idx_in_vec[ 5] ^idx_in_vec[ 4] ^idx_in_vec[ 2] ^idx_in_vec[ 1] ^idx_in_vec[ 0];
    assign idx_raw[ 9] = idx_in_vec[90] ^ idx_in_vec[80] ^idx_in_vec[75] ^idx_in_vec[74] ^idx_in_vec[73] ^idx_in_vec[72] ^idx_in_vec[70] ^idx_in_vec[66] ^idx_in_vec[60] ^idx_in_vec[58] ^idx_in_vec[57] ^idx_in_vec[56] ^idx_in_vec[55] ^idx_in_vec[54] ^idx_in_vec[52] ^idx_in_vec[51] ^idx_in_vec[49] ^idx_in_vec[48] ^idx_in_vec[47] ^idx_in_vec[46] ^idx_in_vec[42] ^idx_in_vec[40] ^idx_in_vec[38] ^idx_in_vec[34] ^idx_in_vec[32] ^idx_in_vec[30] ^idx_in_vec[21] ^idx_in_vec[20] ^idx_in_vec[17] ^idx_in_vec[15] ^idx_in_vec[10] ^idx_in_vec[ 8] ^idx_in_vec[ 7] ^idx_in_vec[ 4] ^idx_in_vec[ 3] ^idx_in_vec[ 1] ;
    assign idx_raw[10] = idx_in_vec[91] ^ idx_in_vec[78] ^idx_in_vec[71] ^idx_in_vec[66] ^idx_in_vec[65] ^idx_in_vec[63] ^idx_in_vec[62] ^idx_in_vec[56] ^idx_in_vec[54] ^idx_in_vec[52] ^idx_in_vec[50] ^idx_in_vec[48] ^idx_in_vec[46] ^idx_in_vec[45] ^idx_in_vec[43] ^idx_in_vec[42] ^idx_in_vec[41] ^idx_in_vec[39] ^idx_in_vec[38] ^idx_in_vec[37] ^idx_in_vec[36] ^idx_in_vec[31] ^idx_in_vec[30] ^idx_in_vec[27] ^idx_in_vec[23] ^idx_in_vec[20] ^idx_in_vec[19] ^idx_in_vec[15] ^idx_in_vec[13] ^idx_in_vec[11] ^idx_in_vec[ 8] ^idx_in_vec[ 7] ^idx_in_vec[ 6] ^idx_in_vec[ 3] ^idx_in_vec[ 1] ^idx_in_vec[ 0] ;
    assign idx_raw[11] = idx_in_vec[96] ^ idx_in_vec[80] ^idx_in_vec[73] ^idx_in_vec[71] ^idx_in_vec[70] ^idx_in_vec[69] ^idx_in_vec[65] ^idx_in_vec[63] ^idx_in_vec[62] ^idx_in_vec[60] ^idx_in_vec[59] ^idx_in_vec[58] ^idx_in_vec[57] ^idx_in_vec[56] ^idx_in_vec[53] ^idx_in_vec[50] ^idx_in_vec[49] ^idx_in_vec[47] ^idx_in_vec[44] ^idx_in_vec[43] ^idx_in_vec[37] ^idx_in_vec[34] ^idx_in_vec[29] ^idx_in_vec[27] ^idx_in_vec[26] ^idx_in_vec[25] ^idx_in_vec[22] ^idx_in_vec[19] ^idx_in_vec[16] ^idx_in_vec[15] ^idx_in_vec[14] ^idx_in_vec[13] ^idx_in_vec[ 8] ^idx_in_vec[ 5] ^idx_in_vec[ 4] ^idx_in_vec[ 3] ;
    assign idx_raw[12] = idx_in_vec[93] ^ idx_in_vec[80] ^idx_in_vec[77] ^idx_in_vec[74] ^idx_in_vec[72] ^idx_in_vec[65] ^idx_in_vec[64] ^idx_in_vec[59] ^idx_in_vec[58] ^idx_in_vec[53] ^idx_in_vec[51] ^idx_in_vec[50] ^idx_in_vec[48] ^idx_in_vec[47] ^idx_in_vec[45] ^idx_in_vec[42] ^idx_in_vec[40] ^idx_in_vec[38] ^idx_in_vec[37] ^idx_in_vec[35] ^idx_in_vec[33] ^idx_in_vec[31] ^idx_in_vec[29] ^idx_in_vec[28] ^idx_in_vec[27] ^idx_in_vec[24] ^idx_in_vec[20] ^idx_in_vec[16] ^idx_in_vec[13] ^idx_in_vec[12] ^idx_in_vec[10] ^idx_in_vec[ 9] ^idx_in_vec[ 6] ^idx_in_vec[ 5] ^idx_in_vec[ 3] ^idx_in_vec[ 2] ;
    assign idx_raw[13] = idx_in_vec[95] ^ idx_in_vec[79] ^idx_in_vec[76] ^idx_in_vec[75] ^idx_in_vec[74] ^idx_in_vec[72] ^idx_in_vec[70] ^idx_in_vec[60] ^idx_in_vec[59] ^idx_in_vec[55] ^idx_in_vec[54] ^idx_in_vec[48] ^idx_in_vec[47] ^idx_in_vec[44] ^idx_in_vec[40] ^idx_in_vec[39] ^idx_in_vec[36] ^idx_in_vec[35] ^idx_in_vec[34] ^idx_in_vec[32] ^idx_in_vec[29] ^idx_in_vec[26] ^idx_in_vec[25] ^idx_in_vec[24] ^idx_in_vec[23] ^idx_in_vec[22] ^idx_in_vec[21] ^idx_in_vec[18] ^idx_in_vec[16] ^idx_in_vec[14] ^idx_in_vec[13] ^idx_in_vec[10] ^idx_in_vec[ 6] ^idx_in_vec[ 5] ^idx_in_vec[ 4] ^idx_in_vec[ 1] ;
    assign idx_raw[14] = idx_in_vec[94] ^ idx_in_vec[78] ^idx_in_vec[76] ^idx_in_vec[75] ^idx_in_vec[71] ^idx_in_vec[69] ^idx_in_vec[68] ^idx_in_vec[66] ^idx_in_vec[63] ^idx_in_vec[61] ^idx_in_vec[58] ^idx_in_vec[55] ^idx_in_vec[54] ^idx_in_vec[51] ^idx_in_vec[46] ^idx_in_vec[41] ^idx_in_vec[40] ^idx_in_vec[39] ^idx_in_vec[38] ^idx_in_vec[37] ^idx_in_vec[35] ^idx_in_vec[28] ^idx_in_vec[27] ^idx_in_vec[26] ^idx_in_vec[22] ^idx_in_vec[23] ^idx_in_vec[21] ^idx_in_vec[19] ^idx_in_vec[12] ^idx_in_vec[11] ^idx_in_vec[10] ^idx_in_vec[ 7] ^idx_in_vec[ 6] ^idx_in_vec[ 2] ^idx_in_vec[ 1] ^idx_in_vec[ 0] ;
    assign idx_raw[15] = idx_in_vec[92] ^ idx_in_vec[77] ^idx_in_vec[73] ^idx_in_vec[72] ^idx_in_vec[71] ^idx_in_vec[68] ^idx_in_vec[65] ^idx_in_vec[57] ^idx_in_vec[55] ^idx_in_vec[54] ^idx_in_vec[53] ^idx_in_vec[51] ^idx_in_vec[49] ^idx_in_vec[46] ^idx_in_vec[43] ^idx_in_vec[41] ^idx_in_vec[38] ^idx_in_vec[37] ^idx_in_vec[35] ^idx_in_vec[33] ^idx_in_vec[30] ^idx_in_vec[25] ^idx_in_vec[24] ^idx_in_vec[22] ^idx_in_vec[20] ^idx_in_vec[19] ^idx_in_vec[19] ^idx_in_vec[14] ^idx_in_vec[12] ^idx_in_vec[11] ^idx_in_vec[10] ^idx_in_vec[ 8] ^idx_in_vec[ 7] ^idx_in_vec[ 6] ^idx_in_vec[ 4] ^idx_in_vec[ 2] ;
    
    assign idx = idx_raw[IDX_WIDTH-1:0];

endmodule

