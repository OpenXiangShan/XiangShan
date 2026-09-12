////////////////////////////////////////////////////////////////////////////////
// iommu_atd_dtc_mtlb_bank_top
////////////////////////////////////////////////////////////////////////////////
module iommu_atd_dtc_mtlb_bank_top #( //{{{
    parameter               CACHE_TYPE                                          = 0, // 0:DDTC, 1:PDTC
    parameter               BANK_TYPE                                           = 2'b00,
    parameter type          INVALID_REQ_TYPE                                    = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    parameter type          LOOKUP_REQ_TYPE                                     = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE                                     = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE                                     = iommu_atd_cache_pkg::ddtc_update_req_t,
    parameter type          BANK_LKP_REQ_TYPE                                   = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    parameter type          BANK_LKP_ACK_TYPE                                   = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    parameter type          BANK_UPD_REQ_TYPE                                   = iommu_atd_cache_pkg::ddtc_bank_upd_req_t,
    parameter type          BANK_UPD_ACK_TYPE                                   = iommu_atd_cache_pkg::ddtc_bank_upd_ack_t,
    parameter type          BANK_INV_REQ_TYPE                                   = iommu_atd_cache_pkg::ddtc_bank_inv_req_t,
    parameter type          BANK_INV_ACK_TYPE                                   = iommu_atd_cache_pkg::ddtc_bank_inv_ack_t,
    parameter               CABIN_LKP_IDX_WIDTH                                 = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    parameter               CABIN_UPD_IDX_WIDTH                                 = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH,
    parameter               CABIN_INV_IDX_WIDTH                                 = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH,
    parameter               BANK_IDX_WIDTH                                      = iommu_atd_cache_pkg::DDTC_MAX_BANK_IDX_WIDTH,
    parameter               BANK_SET_IDX_WIDTH                                  = iommu_atd_cache_pkg::DDTC_MAX_BANK_SET_IDX_WIDTH,
    parameter               BANK_WAY_IDX_WIDTH                                  = iommu_atd_cache_pkg::DDTC_MAX_BANK_WAY_IDX_WIDTH,
    parameter               CABIN_LKP_NUM                                       = 2**CABIN_LKP_IDX_WIDTH,
    parameter               CABIN_UPD_NUM                                       = 2**CABIN_UPD_IDX_WIDTH,
    parameter               CABIN_INV_NUM                                       = 2**CABIN_INV_IDX_WIDTH,
    parameter               BANK_NUM                                            = 2**BANK_IDX_WIDTH,
    parameter               BANK_SET_NUM                                        = 2**BANK_SET_IDX_WIDTH,
    parameter               BANK_WAY_NUM                                        = 2**BANK_WAY_IDX_WIDTH,
    parameter type          MTLB_TAG_TYPE                                       = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    parameter type          MTLB_ITAG_TYPE                                      = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    parameter type          MTLB_UTAG_TYPE                                      = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    parameter type          MTLB_DAT_TYPE                                       = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_l,
    parameter               TECC_WIDTH                                          = 5,
    parameter               DECC_WIDTH                                          = 9,
    parameter               TRAM_DAT_WIDTH                                      = $bits(MTLB_TAG_TYPE)+TECC_WIDTH+1,
    parameter               IRAM_DAT_WIDTH                                      = $bits(MTLB_ITAG_TYPE)+TECC_WIDTH+1,
    parameter               DRAM_DAT_WIDTH                                      = $bits(MTLB_DAT_TYPE)+DECC_WIDTH+1,
    parameter               SPARE_PARAM                                         = 0

)(
//{{{ IO
    input  logic                                                                clk,
    input  logic                                                                rstn,
    // LOOKUP INPUT                                                             
    input  logic [CABIN_LKP_NUM-1:0]                                            lkp_req_valid_i,
    output logic [CABIN_LKP_NUM-1:0]                                            lkp_req_ready_o,
    input  BANK_LKP_REQ_TYPE [CABIN_LKP_NUM-1:0]                                lkp_req_i,
    // UPDATE INPUT                                                             
    input  logic [CABIN_UPD_NUM-1:0]                                            upd_req_valid_i,
    output logic [CABIN_UPD_NUM-1:0]                                            upd_req_ready_o,
    input  BANK_UPD_REQ_TYPE [CABIN_UPD_NUM-1:0]                                upd_req_i,
    // INV INPUT                                                                
    input  logic [CABIN_INV_NUM-1:0]                                            inv_req_valid_i,
    output logic [CABIN_INV_NUM-1:0]                                            inv_req_ready_o,
    input  BANK_INV_REQ_TYPE [CABIN_INV_NUM-1:0]                                inv_req_i,
    // LOOKUP ACK                                                               
    output logic [CABIN_LKP_NUM-1:0]                                            lkp_ack_valid_o,
    output BANK_LKP_ACK_TYPE [CABIN_LKP_NUM-1:0]                                lkp_ack_o,
    // UPDATE ACK                                                               
    output logic [CABIN_UPD_NUM-1:0]                                            upd_ack_valid_o,
    output BANK_UPD_ACK_TYPE [CABIN_UPD_NUM-1:0]                                upd_ack_o,
    // INVALID ACK                                                              
    output logic [CABIN_INV_NUM-1:0]                                            inv_ack_valid_o,
    output BANK_INV_ACK_TYPE [CABIN_INV_NUM-1:0]                                inv_ack_o,
    // RAM
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                              tram_cs_o,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                              tram_wr_o,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]     tram_addr_o,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         tram_wdata_o,
    input  logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         tram_rdata_i,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                              iram_cs_o,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                              iram_wr_o,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]     iram_addr_o,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         iram_wdata_o,
    input  logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [IRAM_DAT_WIDTH-1:0]         iram_rdata_i,
    output logic [BANK_NUM-1:0]                                                 uram_cs_o,
    output logic [BANK_NUM-1:0]                                                 uram_wr_o,
    output logic [BANK_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]                        uram_addr_o,
    output MTLB_UTAG_TYPE [BANK_NUM-1:0]                                        uram_wdata_o,
    input  MTLB_UTAG_TYPE [BANK_NUM-1:0]                                        uram_rdata_i,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                              dram_cs_o,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                              dram_wr_o,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]     dram_addr_o,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         dram_wdata_o,
    input  logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         dram_rdata_i,
    // CFG                                                                      
    input  logic                                                                multi_hit_check_i,
    output logic                                                                multi_hit_fault_o,
    output logic [1:0]                                                          ecc_err_o,
    //                                                                          
    input  logic                                                                spare_in
//}}}
);
//=== Declare === {{{
    logic [BANK_NUM-1:0] [CABIN_LKP_NUM-1:0]                                    bank_lkp_req_valid_i;
    logic [BANK_NUM-1:0] [CABIN_LKP_NUM-1:0]                                    bank_lkp_req_ready_o;
    logic [CABIN_LKP_NUM-1:0] [BANK_NUM-1:0]                                    bank_lkp_req_ready_o_trs;
    BANK_LKP_REQ_TYPE [BANK_NUM-1:0] [CABIN_LKP_NUM-1:0]                        bank_lkp_req_i;
    logic [BANK_NUM-1:0] [CABIN_UPD_NUM-1:0]                                    bank_upd_req_valid_i;
    logic [BANK_NUM-1:0] [CABIN_UPD_NUM-1:0]                                    bank_upd_req_ready_o;
    logic [CABIN_UPD_NUM-1:0] [BANK_NUM-1:0]                                    bank_upd_req_ready_o_trs;
    BANK_UPD_REQ_TYPE [BANK_NUM-1:0] [CABIN_UPD_NUM-1:0]                        bank_upd_req_i;
    logic [BANK_NUM-1:0] [CABIN_INV_NUM-1:0]                                    bank_inv_req_valid_i;
    logic [BANK_NUM-1:0] [CABIN_INV_NUM-1:0]                                    bank_inv_req_ready_o;
    logic [CABIN_INV_NUM-1:0] [BANK_NUM-1:0]                                    bank_inv_req_ready_o_trs;
    BANK_INV_REQ_TYPE [BANK_NUM-1:0] [CABIN_INV_NUM-1:0]                        bank_inv_req_i;
    logic [BANK_NUM-1:0] [CABIN_LKP_NUM-1:0]                                    bank_lkp_ack_valid_o;
    BANK_LKP_ACK_TYPE [BANK_NUM-1:0] [CABIN_LKP_NUM-1:0]                        bank_lkp_ack_o;
    logic [BANK_NUM-1:0] [CABIN_UPD_NUM-1:0]                                    bank_upd_ack_valid_o;
    BANK_UPD_ACK_TYPE [BANK_NUM-1:0] [CABIN_UPD_NUM-1:0]                        bank_upd_ack_o;
    logic [BANK_NUM-1:0] [CABIN_INV_NUM-1:0]                                    bank_inv_ack_valid_o;
    BANK_INV_ACK_TYPE [BANK_NUM-1:0] [CABIN_INV_NUM-1:0]                        bank_inv_ack_o;
                                                                                
    logic [CABIN_INV_NUM-1:0]                                                   inv_req_all_valid;
    logic [CABIN_INV_NUM-1:0]                                                   inv_req_ded_valid;
    logic [CABIN_INV_NUM-1:0]                                                   inv_req_all_ready;
    logic [CABIN_INV_NUM-1:0] [BANK_NUM-1:0]                                    bank_inv_req_all_valid;
    logic [CABIN_INV_NUM-1:0] [BANK_NUM-1:0]                                    bank_inv_req_all_ready;
                                                                                
    logic [BANK_NUM-1:0]                                                        bank_multi_hit_fault_o;

    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                                     bank_tram_cs_o;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                                     bank_tram_wr_o;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]            bank_tram_addr_o;
    MTLB_TAG_TYPE [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                             bank_tram_wdata_o;
    MTLB_TAG_TYPE [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                             bank_tram_rdata_i;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                                     bank_iram_cs_o;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                                     bank_iram_wr_o;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]            bank_iram_addr_o;
    MTLB_ITAG_TYPE [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            bank_iram_wdata_o;
    MTLB_ITAG_TYPE [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                            bank_iram_rdata_i;
    logic [BANK_NUM-1:0]                                                        bank_uram_cs_o;
    logic [BANK_NUM-1:0]                                                        bank_uram_wr_o;
    logic [BANK_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]                               bank_uram_addr_o;
    MTLB_UTAG_TYPE [BANK_NUM-1:0]                                               bank_uram_wdata_o;
    MTLB_UTAG_TYPE [BANK_NUM-1:0]                                               bank_uram_rdata_i;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                                     bank_dram_cs_o;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                                     bank_dram_wr_o;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]            bank_dram_addr_o;
    MTLB_DAT_TYPE [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                             bank_dram_wdata_o;
    MTLB_DAT_TYPE [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                             bank_dram_rdata_i;

    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [TECC_WIDTH:0]                      bank_tram_wdata_ecc_o;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [TECC_WIDTH:0]                      bank_iram_wdata_ecc_o;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [DECC_WIDTH:0]                      bank_dram_wdata_ecc_o;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [TECC_WIDTH:0]                      bank_tram_rdata_ecc_i;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [TECC_WIDTH:0]                      bank_iram_rdata_ecc_i;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [DECC_WIDTH:0]                      bank_dram_rdata_ecc_i;

    logic [BANK_NUM-1:0] [1:0]                                                  bank_ecc_err_o;
    logic [1:0] [BANK_NUM-1:0]                                                  ecc_err_o_int;

//}}}

//=== MainCode === {{{
    assign multi_hit_fault_o = 'd0;
//=== LKP ACK {{{
genvar lack_i;
generate
    for(lack_i=0; lack_i<CABIN_LKP_NUM; lack_i++) begin : carbin_lkp_ack_mux_gen
        always@(*) begin
            lkp_ack_valid_o[lack_i] = 'b0;
            lkp_ack_o[lack_i]       = 'd0;
            for(int unsigned lack_b=0; lack_b<BANK_NUM; lack_b++) begin : lkp_ack_mux_gen
                if(bank_lkp_ack_valid_o[lack_b][lack_i]==1'b1) begin
                    lkp_ack_valid_o[lack_i] = 1'b1;
                    lkp_ack_o[lack_i]       = bank_lkp_ack_o[lack_b][lack_i];
                end
            end
        end
    end
endgenerate

genvar uack_i;
generate
    for(uack_i=0; uack_i<CABIN_UPD_NUM; uack_i++) begin : carbin_upd_ack_mux_gen
        always@(*) begin
            upd_ack_valid_o[uack_i] = 'b0;
            upd_ack_o[uack_i]       = 'd0;
            for(int unsigned uack_b=0; uack_b<BANK_NUM; uack_b++) begin : upd_ack_mux_gen
                if(bank_upd_ack_valid_o[uack_b][uack_i]==1'b1) begin
                    upd_ack_valid_o[uack_i] = 1'b1;
                    upd_ack_o[uack_i]       = bank_upd_ack_o[uack_b][uack_i];
                end
            end
        end
    end
endgenerate

genvar iack_i;
generate
    for(iack_i=0; iack_i<CABIN_INV_NUM; iack_i++) begin : carbin_inv_ack_mux_gen
        always@(*) begin
            inv_ack_valid_o[iack_i] = 'b0;
            inv_ack_o[iack_i]       = 'd0;
            for(int unsigned iack_b=0; iack_b<BANK_NUM; iack_b++) begin : inv_ack_mux_gen
                if(bank_inv_ack_valid_o[iack_b][iack_i]==1'b1) begin
                    inv_ack_valid_o[iack_i] = 1'b1;
                    inv_ack_o[iack_i]       = bank_inv_ack_o[iack_b][iack_i];
                end
            end
        end
    end
endgenerate

//}}}

//=== ecc_err_o {{{
    always@(*) begin
        for(int unsigned ei=0; ei<BANK_NUM; ei++) begin
            ecc_err_o_int[0][ei] = bank_ecc_err_o[ei][0];
            ecc_err_o_int[1][ei] = bank_ecc_err_o[ei][1];
        end
    end

    assign ecc_err_o[0] = |ecc_err_o_int[0];
    assign ecc_err_o[1] = |ecc_err_o_int[1];
//}}}

//}}}

//=== bank inst {{{
genvar ramb, ramc;
generate
    for(ramb=0; ramb<BANK_NUM; ramb++) begin : bank_ram_connect_gen
        assign tram_cs_o    [ramb] = bank_tram_cs_o    [ramb];
        assign tram_wr_o    [ramb] = bank_tram_wr_o    [ramb];
        assign tram_addr_o  [ramb] = bank_tram_addr_o  [ramb];
        for(ramc=0; ramc<BANK_WAY_NUM; ramc++) begin : way_tram_connect_gen
            assign tram_wdata_o         [ramb][ramc] = {bank_tram_wdata_ecc_o[ramb][ramc], bank_tram_wdata_o[ramb][ramc]};
            assign bank_tram_rdata_ecc_i[ramb][ramc] = tram_rdata_i[ramb][ramc][TRAM_DAT_WIDTH-1-:TECC_WIDTH+1];
            assign bank_tram_rdata_i    [ramb][ramc] = tram_rdata_i[ramb][ramc][TRAM_DAT_WIDTH-1-TECC_WIDTH-1:0];
        end
        assign iram_cs_o    [ramb] = bank_iram_cs_o    [ramb];
        assign iram_wr_o    [ramb] = bank_iram_wr_o    [ramb];
        assign iram_addr_o  [ramb] = bank_iram_addr_o  [ramb];
        for(ramc=0; ramc<BANK_WAY_NUM; ramc++) begin : way_iram_connect_gen
            assign iram_wdata_o         [ramb][ramc] = {bank_iram_wdata_ecc_o[ramb][ramc], bank_iram_wdata_o[ramb][ramc]};
            assign bank_iram_rdata_ecc_i[ramb][ramc] = iram_rdata_i[ramb][ramc][IRAM_DAT_WIDTH-1-:TECC_WIDTH+1];
            assign bank_iram_rdata_i    [ramb][ramc] = iram_rdata_i[ramb][ramc][IRAM_DAT_WIDTH-1-TECC_WIDTH-1:0];
        end
        assign uram_cs_o    [ramb] = bank_uram_cs_o    [ramb];
        assign uram_wr_o    [ramb] = bank_uram_wr_o    [ramb];
        assign uram_addr_o  [ramb] = bank_uram_addr_o  [ramb];
        assign uram_wdata_o [ramb] = bank_uram_wdata_o [ramb];
        assign bank_uram_rdata_i [ramb] = uram_rdata_i [ramb];
        assign dram_cs_o    [ramb] = bank_dram_cs_o    [ramb];
        assign dram_wr_o    [ramb] = bank_dram_wr_o    [ramb];
        assign dram_addr_o  [ramb] = bank_dram_addr_o  [ramb];
        for(ramc=0; ramc<BANK_WAY_NUM; ramc++) begin : way_dram_connect_gen
            assign dram_wdata_o         [ramb][ramc] = {bank_dram_wdata_ecc_o[ramb][ramc], bank_dram_wdata_o[ramb][ramc]};
            assign bank_dram_rdata_ecc_i[ramb][ramc] = dram_rdata_i[ramb][ramc][DRAM_DAT_WIDTH-1-:DECC_WIDTH+1];
            assign bank_dram_rdata_i    [ramb][ramc] = dram_rdata_i[ramb][ramc][DRAM_DAT_WIDTH-1-DECC_WIDTH-1:0];
        end
    end
endgenerate

genvar cl, bl;
generate
    for(cl=0; cl<CABIN_LKP_NUM; cl++) begin : lkp_req_ready_o_connect_gen
        for(bl=0; bl<BANK_NUM; bl++) begin : lkp_req_ready_o_connect_gen_b
//            assign bank_lkp_req_ready_o_trs[cl][bl] = bank_lkp_req_valid_i[bl][cl] & bank_lkp_req_ready_o[bl][cl];
            assign bank_lkp_req_ready_o_trs[cl][bl] = bank_lkp_req_ready_o[bl][cl];
        end
        assign lkp_req_ready_o[cl] = |bank_lkp_req_ready_o_trs[cl];
    end
endgenerate

genvar cu, bu;
generate
    for(cu=0; cu<CABIN_UPD_NUM; cu++) begin : upd_req_ready_o_connect_gen
        for(bu=0; bu<BANK_NUM; bu++) begin : upd_req_ready_o_connect_gen_b
//            assign bank_upd_req_ready_o_trs[cu][bu] = bank_upd_req_valid_i[bu][cu] & bank_upd_req_ready_o[bu][cu];
            assign bank_upd_req_ready_o_trs[cu][bu] = bank_upd_req_ready_o[bu][cu];
        end
        assign upd_req_ready_o[cu] = |bank_upd_req_ready_o_trs[cu];
    end
endgenerate

    assign bank_inv_req_all_ready = bank_inv_req_ready_o_trs;
genvar ci, bi;
generate
    for(ci=0; ci<CABIN_INV_NUM; ci++) begin : inv_req_ready_o_connect_gen
        assign inv_req_all_valid[ci] = inv_req_valid_i[ci] & ~inv_req_i[ci].bank_idx_val;
        assign inv_req_ded_valid[ci] = inv_req_valid_i[ci] &  inv_req_i[ci].bank_idx_val;
        assign inv_req_ready_o[ci]   = inv_req_all_valid[ci] ? inv_req_all_ready[ci] : (|bank_inv_req_ready_o_trs[ci]);
        iommu_acd_mtlb_ready_mux #(
        /*parameter */ .NUM    (BANK_NUM   ) // = 2
        ) u_inv_req_mux(
        /*input  logic                  */  .clk        (clk                        ),
        /*input  logic                  */  .rstn       (rstn                       ),
        /*input  logic                  */  .valid_i    (inv_req_all_valid      [ci]),
        /*output logic                  */  .ready_o    (inv_req_all_ready      [ci]),
        /*output logic [NUM-1:0]        */  .valid_o    (bank_inv_req_all_valid [ci]),
        /*input  logic [NUM-1:0]        */  .ready_i    (bank_inv_req_all_ready [ci]) 
        );

        for(bi=0; bi<BANK_NUM; bi++) begin : inv_req_ready_o_connect_gen_b
//            assign bank_inv_req_ready_o_trs[ci][bi] = bank_inv_req_valid_i[bi][ci] & bank_inv_req_ready_o[bi][ci];
            assign bank_inv_req_ready_o_trs[ci][bi] = bank_inv_req_ready_o[bi][ci];
        end
    end
endgenerate

genvar i,l,u,v;
generate
    for(i=0; i<BANK_NUM; i++) begin : bank_inst_gen
        for(l=0; l<CABIN_LKP_NUM; l++) begin
            assign bank_lkp_req_valid_i[i][l] = lkp_req_valid_i[l] & (lkp_req_i[l].bank_idx==BANK_IDX_WIDTH'(i));
            assign bank_lkp_req_i      [i][l] = lkp_req_i[l];
        end
        for(u=0; u<CABIN_UPD_NUM; u++) begin
            assign bank_upd_req_valid_i[i][u] = upd_req_valid_i[u] & (upd_req_i[u].bank_idx==BANK_IDX_WIDTH'(i));
            assign bank_upd_req_i      [i][u] = upd_req_i[u];
        end
        for(v=0; v<CABIN_INV_NUM; v++) begin
            assign bank_inv_req_valid_i[i][v] = inv_req_i[v].bank_idx_val ? (inv_req_ded_valid[v] & inv_req_i[v].bank_idx==BANK_IDX_WIDTH'(i)) : bank_inv_req_all_valid[v][i];
            assign bank_inv_req_i      [i][v] = inv_req_i[v];
        end

        iommu_atd_dtc_mtlb_bank #(
        /*parameter */ .CACHE_TYPE                         (CACHE_TYPE                 ), // = 0, // 0:DDTC, 1:PDTC
        /*parameter */ .BANK_TYPE                          (BANK_TYPE                  ), // = 2'b00,    // 00:l0  01:l1  10:l2
        /*parameter type         */ .INVALID_REQ_TYPE                   (INVALID_REQ_TYPE           ), // = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
        /*parameter type         */ .MTLB_TAG_TYPE                      (MTLB_TAG_TYPE              ), // = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
        /*parameter type         */ .MTLB_ITAG_TYPE                     (MTLB_ITAG_TYPE             ), // = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
        /*parameter type         */ .MTLB_UTAG_TYPE                     (MTLB_UTAG_TYPE             ), // = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
        /*parameter type         */ .MTLB_DAT_TYPE                      (MTLB_DAT_TYPE              ), // = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_l,
        /*parameter type         */ .LOOKUP_REQ_TYPE                    (LOOKUP_REQ_TYPE            ), // = iommu_atd_cache_pkg::lookup_req_t,
        /*parameter type         */ .LOOKUP_ACK_TYPE                    (LOOKUP_ACK_TYPE            ), // = iommu_atd_cache_pkg::lookup_ack_t,
        /*parameter type         */ .UPDATE_REQ_TYPE                    (UPDATE_REQ_TYPE            ), // = iommu_atd_cache_pkg::update_req_t,
        /*parameter type         */ .BANK_LKP_REQ_TYPE                  (BANK_LKP_REQ_TYPE          ), // = iommu_atd_cache_pkg::bank_lkp_req_t,
        /*parameter type         */ .BANK_LKP_ACK_TYPE                  (BANK_LKP_ACK_TYPE          ), // = iommu_atd_cache_pkg::bank_lkp_ack_t,
        /*parameter type         */ .BANK_UPD_REQ_TYPE                  (BANK_UPD_REQ_TYPE          ), // = iommu_atd_cache_pkg::bank_upd_req_t,
        /*parameter type         */ .BANK_UPD_ACK_TYPE                  (BANK_UPD_ACK_TYPE          ), // = iommu_atd_cache_pkg::bank_upd_ack_t,
        /*parameter type         */ .BANK_INV_REQ_TYPE                  (BANK_INV_REQ_TYPE          ), // = iommu_atd_cache_pkg::bank_inv_req_t,
        /*parameter type         */ .BANK_INV_ACK_TYPE                  (BANK_INV_ACK_TYPE          ), // = iommu_atd_cache_pkg::bank_inv_ack_t,
        /*parameter */ .CABIN_LKP_IDX_WIDTH                (CABIN_LKP_IDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
        /*parameter */ .CABIN_UPD_IDX_WIDTH                (CABIN_UPD_IDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH,
        /*parameter */ .CABIN_INV_IDX_WIDTH                (CABIN_INV_IDX_WIDTH        ), // = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH,
        /*parameter */ .BANK_IDX_WIDTH                     (BANK_IDX_WIDTH             ), // = iommu_atd_cache_pkg::DDTC_MAX_BANK_IDX_WIDTH,
        /*parameter */ .BANK_SET_IDX_WIDTH                 (BANK_SET_IDX_WIDTH         ), // = iommu_atd_cache_pkg::DDTC_MAX_BANK_SET_IDX_WIDTH,
        /*parameter */ .BANK_WAY_IDX_WIDTH                 (BANK_WAY_IDX_WIDTH         ), // = iommu_atd_cache_pkg::DDTC_MAX_BANK_WAY_IDX_WIDTH,
        /*parameter */ .TECC_WIDTH                         (TECC_WIDTH                 ), // = 5,
        /*parameter */ .DECC_WIDTH                         (DECC_WIDTH                 ), // = 9,
        /*parameter */ .SPARE_PARAM                        (1'b0                       )  // = 0
        ) U_bank(
        /*input  logic                                              */  .clk                        (clk                        ),
        /*input  logic                                              */  .rstn                       (rstn                       ),
        /*input  logic [CABIN_LKP_NUM-1:0]                          */  .lkp_req_valid_i            (bank_lkp_req_valid_i    [i]),
        /*output logic [CABIN_LKP_NUM-1:0]                          */  .lkp_req_ready_o            (bank_lkp_req_ready_o    [i]),
        /*input  BANK_LKP_REQ_TYPE [CABIN_LKP_NUM-1:0]              */  .lkp_req_i                  (bank_lkp_req_i          [i]),
        /*input  logic [CABIN_UPD_NUM-1:0]                          */  .upd_req_valid_i            (bank_upd_req_valid_i    [i]),
        /*output logic [CABIN_UPD_NUM-1:0]                          */  .upd_req_ready_o            (bank_upd_req_ready_o    [i]),
        /*input  BANK_UPD_REQ_TYPE [CABIN_UPD_NUM-1:0]              */  .upd_req_i                  (bank_upd_req_i          [i]),
        /*input  logic [CABIN_INV_NUM-1:0]                          */  .inv_req_valid_i            (bank_inv_req_valid_i    [i]),
        /*output logic [CABIN_INV_NUM-1:0]                          */  .inv_req_ready_o            (bank_inv_req_ready_o    [i]),
        /*input  BANK_INV_REQ_TYPE [CABIN_INV_NUM-1:0]              */  .inv_req_i                  (bank_inv_req_i          [i]),
        /*output logic [CABIN_LKP_NUM-1:0]                          */  .lkp_ack_valid_o            (bank_lkp_ack_valid_o    [i]),
        /*output BANK_LKP_ACK_TYPE [CABIN_LKP_NUM-1:0]              */  .lkp_ack_o                  (bank_lkp_ack_o          [i]),
        /*output logic [CABIN_UPD_NUM-1:0]                          */  .upd_ack_valid_o            (bank_upd_ack_valid_o    [i]),
        /*output BANK_UPD_ACK_TYPE [CABIN_UPD_NUM-1:0]              */  .upd_ack_o                  (bank_upd_ack_o          [i]),
        /*output logic [CABIN_INV_NUM-1:0]                          */  .inv_ack_valid_o            (bank_inv_ack_valid_o    [i]),
        /*output BANK_INV_ACK_TYPE [CABIN_INV_NUM-1:0]              */  .inv_ack_o                  (bank_inv_ack_o          [i]),
        /*output logic [BANK_WAY_NUM-1:0]                           */  .tram_cs_o                  (bank_tram_cs_o          [i]),
        /*output logic [BANK_WAY_NUM-1:0]                           */  .tram_wr_o                  (bank_tram_wr_o          [i]),
        /*output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]  */  .tram_addr_o                (bank_tram_addr_o        [i]),
        /*output MTLB_TAG_TYPE [BANK_WAY_NUM-1:0]                   */  .tram_wdata_o               (bank_tram_wdata_o       [i]),
        /*input  MTLB_TAG_TYPE  [BANK_WAY_NUM-1:0]                  */  .tram_rdata_i               (bank_tram_rdata_i       [i]),
        /*output logic [BANK_WAY_NUM-1:0]                           */  .iram_cs_o                  (bank_iram_cs_o          [i]),
        /*output logic [BANK_WAY_NUM-1:0]                           */  .iram_wr_o                  (bank_iram_wr_o          [i]),
        /*output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]  */  .iram_addr_o                (bank_iram_addr_o        [i]),
        /*output MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                  */  .iram_wdata_o               (bank_iram_wdata_o       [i]),
        /*input  MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                  */  .iram_rdata_i               (bank_iram_rdata_i       [i]),
        /*output logic                                              */  .uram_cs_o                  (bank_uram_cs_o          [i]),
        /*output logic                                              */  .uram_wr_o                  (bank_uram_wr_o          [i]),
        /*output logic [BANK_SET_IDX_WIDTH-1:0]                     */  .uram_addr_o                (bank_uram_addr_o        [i]),
        /*output MTLB_UTAG_TYPE                                     */  .uram_wdata_o               (bank_uram_wdata_o       [i]),
        /*input  MTLB_UTAG_TYPE                                     */  .uram_rdata_i               (bank_uram_rdata_i       [i]),
        /*output logic [BANK_WAY_NUM-1:0]                           */  .dram_cs_o                  (bank_dram_cs_o          [i]),
        /*output logic [BANK_WAY_NUM-1:0]                           */  .dram_wr_o                  (bank_dram_wr_o          [i]),
        /*output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]  */  .dram_addr_o                (bank_dram_addr_o        [i]),
        /*output MTLB_DAT_TYPE [BANK_WAY_NUM-1:0]                   */  .dram_wdata_o               (bank_dram_wdata_o       [i]),
        /*input  MTLB_DAT_TYPE  [BANK_WAY_NUM-1:0]                  */  .dram_rdata_i               (bank_dram_rdata_i       [i]),
        /*output logic [BANK_WAY_NUM-1:0] [TECC_WIDTH:0]            */  .tram_wdata_ecc_o           (bank_tram_wdata_ecc_o   [i]),
        /*output logic [BANK_WAY_NUM-1:0] [TECC_WIDTH:0]            */  .iram_wdata_ecc_o           (bank_iram_wdata_ecc_o   [i]),
        /*output logic [BANK_WAY_NUM-1:0] [DECC_WIDTH:0]            */  .dram_wdata_ecc_o           (bank_dram_wdata_ecc_o   [i]),
        /*input  logic [BANK_WAY_NUM-1:0] [TECC_WIDTH:0]            */  .tram_rdata_ecc_i           (bank_tram_rdata_ecc_i   [i]),
        /*input  logic [BANK_WAY_NUM-1:0] [TECC_WIDTH:0]            */  .iram_rdata_ecc_i           (bank_iram_rdata_ecc_i   [i]),
        /*input  logic [BANK_WAY_NUM-1:0] [ECC_WIDTH:0]             */  .dram_rdata_ecc_i           (bank_dram_rdata_ecc_i   [i]),
        /*input  logic                                              */  .multi_hit_check_i          (multi_hit_check_i          ),
        /*output logic                                              */  .multi_hit_fault_o          (bank_multi_hit_fault_o  [i]),
        /*output logic [1:0]                                        */  .ecc_err_o                  (bank_ecc_err_o          [i]),
        /*input  logic                                              */  .spare_in                   (1'b0                       ) 
        );
    end
endgenerate
//}}}

endmodule
//}}}



////////////////////////////////////////////////////////////////
// iommu_atd_dtc_mtlb_bank
////////////////////////////////////////////////////////////////
module iommu_atd_dtc_mtlb_bank #( //{{{
    parameter  CACHE_TYPE                          = 0, // 0:DDTC, 1:PDTC
    parameter  BANK_TYPE                           = 2'b00,    // 00:l0  01:l1  10:l2
    parameter type          INVALID_REQ_TYPE                    = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    parameter type          LOOKUP_REQ_TYPE                     = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE                     = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE                     = iommu_atd_cache_pkg::ddtc_update_req_t,
    parameter type          BANK_LKP_REQ_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    parameter type          BANK_LKP_ACK_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    parameter type          BANK_UPD_REQ_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_upd_req_t,
    parameter type          BANK_UPD_ACK_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_upd_ack_t,
    parameter type          BANK_INV_REQ_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_inv_req_t,
    parameter type          BANK_INV_ACK_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_inv_ack_t,
    parameter  CABIN_LKP_IDX_WIDTH                 = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    parameter  CABIN_UPD_IDX_WIDTH                 = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH,
    parameter  CABIN_INV_IDX_WIDTH                 = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH,
    parameter  BANK_IDX_WIDTH                      = iommu_atd_cache_pkg::DDTC_MAX_BANK_IDX_WIDTH,
    parameter  BANK_SET_IDX_WIDTH                  = iommu_atd_cache_pkg::DDTC_MAX_BANK_SET_IDX_WIDTH,
    parameter  BANK_WAY_IDX_WIDTH                  = iommu_atd_cache_pkg::DDTC_MAX_BANK_WAY_IDX_WIDTH,
    parameter  CABIN_LKP_NUM                       = 2**CABIN_LKP_IDX_WIDTH,
    parameter  CABIN_UPD_NUM                       = 2**CABIN_UPD_IDX_WIDTH,
    parameter  CABIN_INV_NUM                       = 2**CABIN_INV_IDX_WIDTH,
    parameter  BANK_NUM                            = 2**BANK_IDX_WIDTH,
    parameter  BANK_SET_NUM                        = 2**BANK_SET_IDX_WIDTH,
    parameter  BANK_WAY_NUM                        = 2**BANK_WAY_IDX_WIDTH,
    parameter type          MTLB_TAG_TYPE                       = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    parameter type          MTLB_ITAG_TYPE                      = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    parameter type          MTLB_UTAG_TYPE                      = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    parameter type          MTLB_DAT_TYPE                       = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_l,
    parameter  TECC_WIDTH                          = 5,
    parameter  DECC_WIDTH                          = 9,
    parameter  SPARE_PARAM                         = 0
)(
//{{{ IO
    input  logic                                                clk,
    input  logic                                                rstn,
    // LOOKUP INPUT                                             
    input  logic [CABIN_LKP_NUM-1:0]                            lkp_req_valid_i,
    output logic [CABIN_LKP_NUM-1:0]                            lkp_req_ready_o,
    input  BANK_LKP_REQ_TYPE [CABIN_LKP_NUM-1:0]                lkp_req_i,
    // UPDATE INPUT                                             
    input  logic [CABIN_UPD_NUM-1:0]                            upd_req_valid_i,
    output logic [CABIN_UPD_NUM-1:0]                            upd_req_ready_o,
    input  BANK_UPD_REQ_TYPE [CABIN_UPD_NUM-1:0]                upd_req_i,
    // INV INPUT                                                
    input  logic [CABIN_INV_NUM-1:0]                            inv_req_valid_i,
    output logic [CABIN_INV_NUM-1:0]                            inv_req_ready_o,
    input  BANK_INV_REQ_TYPE [CABIN_INV_NUM-1:0]                inv_req_i,
    // LOOKUP ACK                                               
    output logic [CABIN_LKP_NUM-1:0]                            lkp_ack_valid_o,
    output BANK_LKP_ACK_TYPE [CABIN_LKP_NUM-1:0]                lkp_ack_o,
    // UPDATE ACK                                               
    output logic [CABIN_UPD_NUM-1:0]                            upd_ack_valid_o,
    output BANK_UPD_ACK_TYPE [CABIN_UPD_NUM-1:0]                upd_ack_o,
    // INVALID ACK                                              
    output logic [CABIN_INV_NUM-1:0]                            inv_ack_valid_o,
    output BANK_INV_ACK_TYPE [CABIN_INV_NUM-1:0]                inv_ack_o,
    // RAM                                                      
    output logic [BANK_WAY_NUM-1:0]                             tram_cs_o   ,
    output logic [BANK_WAY_NUM-1:0]                             tram_wr_o   ,
    output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]    tram_addr_o ,
    output MTLB_TAG_TYPE [BANK_WAY_NUM-1:0]                     tram_wdata_o,
    input  MTLB_TAG_TYPE  [BANK_WAY_NUM-1:0]                    tram_rdata_i,
    output logic [BANK_WAY_NUM-1:0]                             iram_cs_o   ,
    output logic [BANK_WAY_NUM-1:0]                             iram_wr_o   ,
    output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]    iram_addr_o ,
    output MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                    iram_wdata_o,
    input  MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                    iram_rdata_i,
    output logic                                                uram_cs_o   ,
    output logic                                                uram_wr_o   ,
    output logic [BANK_SET_IDX_WIDTH-1:0]                       uram_addr_o ,
    output MTLB_UTAG_TYPE                                       uram_wdata_o,
    input  MTLB_UTAG_TYPE                                       uram_rdata_i,
    output logic [BANK_WAY_NUM-1:0]                             dram_cs_o   ,
    output logic [BANK_WAY_NUM-1:0]                             dram_wr_o   ,
    output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]    dram_addr_o ,
    output MTLB_DAT_TYPE [BANK_WAY_NUM-1:0]                     dram_wdata_o,
    input  MTLB_DAT_TYPE  [BANK_WAY_NUM-1:0]                    dram_rdata_i,
    output logic [BANK_WAY_NUM-1:0] [TECC_WIDTH:0]              tram_wdata_ecc_o,
    output logic [BANK_WAY_NUM-1:0] [TECC_WIDTH:0]              iram_wdata_ecc_o,
    output logic [BANK_WAY_NUM-1:0] [DECC_WIDTH:0]              dram_wdata_ecc_o,
    input  logic [BANK_WAY_NUM-1:0] [TECC_WIDTH:0]              tram_rdata_ecc_i,
    input  logic [BANK_WAY_NUM-1:0] [TECC_WIDTH:0]              iram_rdata_ecc_i,
    input  logic [BANK_WAY_NUM-1:0] [DECC_WIDTH:0]              dram_rdata_ecc_i,
    // CFG                                                      
    input  logic                                                multi_hit_check_i,
    output logic                                                multi_hit_fault_o,
    output logic [1:0]                                          ecc_err_o,
    //                                                          
    input  logic                                                spare_in
//}}}
);
//=== Declare === {{{
    typedef struct packed {
        logic [2:0]                                     typ; // 001: lkp, 010:upd, 100:inv
        logic [CABIN_LKP_NUM-1:0]                       lidx;
        logic [CABIN_UPD_NUM-1:0]                       uidx;
        logic [CABIN_INV_NUM-1:0]                       iidx;
        BANK_LKP_REQ_TYPE                               lkp;
        BANK_UPD_REQ_TYPE                               upd;
        BANK_INV_REQ_TYPE                               inv;
    } bank_req_t;

    logic                                               arb_lkp_req_valid;
    logic                                               arb_lkp_req_ready;
    BANK_LKP_REQ_TYPE                                   arb_lkp_req;

    logic                                               arb_upd_req_valid;
    logic                                               arb_upd_req_ready;
    BANK_UPD_REQ_TYPE                                   arb_upd_req;

    logic                                               arb_inv_req_valid;
    logic                                               arb_inv_req_ready;
    BANK_INV_REQ_TYPE                                   arb_inv_req;

    logic [3:0]                                         arb_bank_req_valid_unmux;
    logic [3:0]                                         arb_bank_req_ready_unmux;
    bank_req_t [3:0]                                    arb_bank_req_unmux;

    logic                                               arb_bank_req_valid_o;
    logic                                               arb_bank_req_ready_i;
    bank_req_t                                          arb_bank_req_o;

    logic                                               rd0_bank_req_valid_i;
    logic                                               rd0_bank_req_ready_o;
    bank_req_t                                          rd0_bank_req_i      ;
    logic                                               rd0_bank_req_valid_o;
    logic                                               rd0_bank_req_ready_i;
    bank_req_t                                          rd0_bank_req_o      ;
    logic [BANK_WAY_NUM-1:0]                            rd0_tram_cs_o       ;
    logic [BANK_WAY_NUM-1:0]                            rd0_tram_wr_o       ;
    logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]   rd0_tram_addr_o     ;
    logic [BANK_WAY_NUM-1:0]                            rd0_iram_cs_o       ;
    logic [BANK_WAY_NUM-1:0]                            rd0_iram_wr_o       ;
    logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]   rd0_iram_addr_o     ;
    logic [BANK_WAY_NUM-1:0]                            rd0_dram_cs_o       ;
    logic [BANK_WAY_NUM-1:0]                            rd0_dram_wr_o       ;
    logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]   rd0_dram_addr_o     ;
    logic                                               rd0_uram_cs_o       ;
    logic                                               rd0_uram_wr_o       ;
    logic [BANK_SET_IDX_WIDTH-1:0]                      rd0_uram_addr_o     ;

    logic                                               mt1_bank_req_valid_i;
    logic                                               mt1_bank_req_ready_o;
    bank_req_t                                          mt1_bank_req_i      ;
    logic                                               mt1_bank_req_valid_o;
    logic                                               mt1_bank_req_ready_i;
    bank_req_t                                          mt1_bank_req_o      ;
    logic [BANK_WAY_NUM-1:0]                            mt1_tag_valid_o     ;
    logic [BANK_WAY_NUM-1:0]                            mt1_tag_hit_o       ;
    MTLB_TAG_TYPE  [BANK_WAY_NUM-1:0]                   mt1_tag_o           ;
    MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                   mt1_itag_o          ;
    MTLB_UTAG_TYPE                                      mt1_utag_o          ;
    logic [BANK_WAY_NUM-1:0]                            mt1_ram_valid_i     ;
    MTLB_TAG_TYPE  [BANK_WAY_NUM-1:0]                   mt1_tram_rdata_i    ;
    MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                   mt1_iram_rdata_i    ;
    MTLB_UTAG_TYPE                                      mt1_uram_rdata_i    ;
    logic [BANK_WAY_NUM-1:0] [1:0]                      mt1_tram_rdata_err_i;
    logic [BANK_WAY_NUM-1:0] [1:0]                      mt1_iram_rdata_err_i;
    logic [BANK_WAY_NUM-1:0] [1:0]                      mt1_dram_rdata_err_i;
    logic [BANK_WAY_NUM-1:0] [1:0]                      mt1_err_o           ;

    logic                                               wr2_bank_req_valid_i;
    logic                                               wr2_bank_req_ready_o;
    bank_req_t                                          wr2_bank_req_i      ;
    logic                                               wr2_bank_req_valid_o;
    logic                                               wr2_bank_req_ready_i;
    bank_req_t                                          wr2_bank_req_o      ;
    logic [BANK_WAY_NUM-1:0]                            wr2_tag_valid_i     ;
    logic [BANK_WAY_NUM-1:0]                            wr2_tag_hit_i       ;
    MTLB_TAG_TYPE  [BANK_WAY_NUM-1:0]                   wr2_tag_i           ;
    MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                   wr2_itag_i          ;
    MTLB_UTAG_TYPE                                      wr2_utag_i          ;
    logic                                               wr2_tag_valid_o     ;
    logic                                               wr2_tag_hit_o       ;
    MTLB_TAG_TYPE                                       wr2_tag_o           ;
    MTLB_ITAG_TYPE                                      wr2_itag_o          ;
    logic [BANK_WAY_NUM-1:0]                            wr2_tram_cs_o       ;
    logic [BANK_WAY_NUM-1:0]                            wr2_tram_wr_o       ;
    logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]   wr2_tram_addr_o     ;
    MTLB_TAG_TYPE [BANK_WAY_NUM-1:0]                    wr2_tram_wdata_o    ;
    logic [BANK_WAY_NUM-1:0]                            wr2_iram_cs_o       ;
    logic [BANK_WAY_NUM-1:0]                            wr2_iram_wr_o       ;
    logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]   wr2_iram_addr_o     ;
    MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                   wr2_iram_wdata_o    ;
    logic                                               wr2_uram_cs_o       ;
    logic                                               wr2_uram_wr_o       ;
    logic [BANK_SET_IDX_WIDTH-1:0]                      wr2_uram_addr_o     ;
    MTLB_UTAG_TYPE                                      wr2_uram_wdata_o    ;
    logic [BANK_WAY_NUM-1:0]                            wr2_dram_cs_o       ;
    logic [BANK_WAY_NUM-1:0]                            wr2_dram_wr_o       ;
    logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]   wr2_dram_addr_o     ;
    MTLB_DAT_TYPE [BANK_WAY_NUM-1:0]                    wr2_dram_wdata_o    ;
    logic [BANK_WAY_NUM-1:0] [1:0]                      wr2_err_i           ;

    logic                                               ak3_bank_req_valid_i;
    logic                                               ak3_bank_req_ready_o;
    bank_req_t                                          ak3_bank_req_i      ;
    logic                                               ak3_tag_valid_i     ;
    logic                                               ak3_tag_hit_i       ;
    MTLB_TAG_TYPE                                       ak3_tag_i           ;
    MTLB_ITAG_TYPE                                      ak3_itag_i          ;
    logic [BANK_WAY_NUM-1:0]                            ak3_ram_valid_i     ;
    MTLB_DAT_TYPE  [BANK_WAY_NUM-1:0]                   ak3_dram_rdata_i    ;
    logic [CABIN_LKP_NUM-1:0]                           ak3_lkp_ack_valid_o ;
    BANK_LKP_ACK_TYPE [CABIN_LKP_NUM-1:0]               ak3_lkp_ack_o       ;
    logic [CABIN_UPD_NUM-1:0]                           ak3_upd_ack_valid_o ;
    BANK_UPD_ACK_TYPE [CABIN_UPD_NUM-1:0]               ak3_upd_ack_o       ;
    logic [CABIN_INV_NUM-1:0]                           ak3_inv_ack_valid_o ;
    BANK_INV_ACK_TYPE [CABIN_INV_NUM-1:0]               ak3_inv_ack_o       ;

    logic [BANK_WAY_NUM-1:0]                            mt1_ram_valid_i_fifo ;
    MTLB_TAG_TYPE  [BANK_WAY_NUM-1:0]                   mt1_tram_rdata_i_fifo;
    MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                   mt1_iram_rdata_i_fifo;
    MTLB_UTAG_TYPE                                      mt1_uram_rdata_i_fifo;
    logic [BANK_WAY_NUM-1:0]                            mt1_ram_valid_i_raw;

    logic [BANK_WAY_NUM-1:0] [1:0]                      tram_rdata_err;
    logic [BANK_WAY_NUM-1:0] [1:0]                      iram_rdata_err;
    logic [BANK_WAY_NUM-1:0] [1:0]                      dram_rdata_err;

    logic [BANK_WAY_NUM-1:0] [1:0]                      mt1_tram_rdata_err_fifo;
    logic [BANK_WAY_NUM-1:0] [1:0]                      mt1_iram_rdata_err_fifo;
    logic [BANK_WAY_NUM-1:0] [1:0]                      mt1_dram_rdata_err_fifo;

    logic [1:0] [BANK_WAY_NUM-1:0]                      ecc_err_o_int;
//}}}
 
//=== MainCode === {{{
//=== RAM out {{{
    assign tram_cs_o    = ~(&wr2_tram_cs_o) ? wr2_tram_cs_o     : rd0_tram_cs_o;  //rd0_tram_cs_o   & wr2_tram_cs_o;
    assign tram_wr_o    = ~(&wr2_tram_cs_o) ? wr2_tram_wr_o     : rd0_tram_wr_o;  //rd0_tram_wr_o   & wr2_tram_wr_o;
    assign tram_addr_o  = ~(&wr2_tram_cs_o) ? wr2_tram_addr_o   : rd0_tram_addr_o;//rd0_tram_addr_o | wr2_tram_addr_o;
    assign tram_wdata_o = wr2_tram_wdata_o;
    assign iram_cs_o    = ~(&wr2_iram_cs_o) ? wr2_iram_cs_o     : rd0_iram_cs_o;  //rd0_iram_cs_o   & wr2_iram_cs_o;
    assign iram_wr_o    = ~(&wr2_iram_cs_o) ? wr2_iram_wr_o     : rd0_iram_wr_o;  //rd0_iram_wr_o   & wr2_iram_wr_o;
    assign iram_addr_o  = ~(&wr2_iram_cs_o) ? wr2_iram_addr_o   : rd0_iram_addr_o;//rd0_iram_addr_o | wr2_iram_addr_o;
    assign iram_wdata_o = wr2_iram_wdata_o;
    assign uram_cs_o    = ~(&wr2_uram_cs_o) ? wr2_uram_cs_o     : rd0_uram_cs_o;  //rd0_uram_cs_o   & wr2_uram_cs_o;
    assign uram_wr_o    = ~(&wr2_uram_cs_o) ? wr2_uram_wr_o     : rd0_uram_wr_o;  //rd0_uram_wr_o   & wr2_uram_wr_o;
    assign uram_addr_o  = ~(&wr2_uram_cs_o) ? wr2_uram_addr_o   : rd0_uram_addr_o;//rd0_uram_addr_o | wr2_uram_addr_o;
    assign uram_wdata_o = wr2_uram_wdata_o;
    assign dram_cs_o    = ~(&wr2_dram_cs_o) ? wr2_dram_cs_o     : rd0_dram_cs_o;
    assign dram_wr_o    = ~(&wr2_dram_cs_o) ? wr2_dram_wr_o     : rd0_dram_wr_o;
    assign dram_addr_o  = ~(&wr2_dram_cs_o) ? wr2_dram_addr_o   : rd0_dram_addr_o;
    assign dram_wdata_o = wr2_dram_wdata_o;

genvar i,ii;
generate //{{{
    for(i=0; i<BANK_WAY_NUM; i++) begin : way_gen
        if(TECC_WIDTH==0 | DECC_WIDTH==0) begin
            assign tram_wdata_ecc_o[i] = 'd0;
            assign iram_wdata_ecc_o[i] = 'd0;
            assign dram_wdata_ecc_o[i] = 'd0;
            assign tram_rdata_err  [i] = 'd0;
            assign iram_rdata_err  [i] = 'd0;
            assign dram_rdata_err  [i] = 'd0;
        end
        else begin
            iommu_acd_mtlb_ecc_enc #(
            /*parameter */ .DATA_WIDTH         ($bits(tram_wdata_o[0])    ), // = 128,
            /*parameter */ .ECC_WIDTH          (TECC_WIDTH              ), // = 8,
            /*parameter */ .SPARE_PARAM        (                       )  // = 0
            ) U_tram_ecc_enc(
            /*input  logic [DATA_WIDTH-1:0]             */  .dat_i                  (tram_wdata_o       [i] ),
            /*output logic [ECC_WIDTH:0]                */  .ecc_o                  (tram_wdata_ecc_o   [i] ),
            /*input  logic                              */  .spare_in               (1'b0               )
            );
            
            //iommu_acd_mtlb_ecc_enc #(
            ///*parameter */ .DATA_WIDTH         ($bits(iram_wdata_o[0])    ), // = 128,
            ///*parameter */ .ECC_WIDTH          (TECC_WIDTH              ), // = 8,
            ///*parameter */ .SPARE_PARAM        (                       )  // = 0
            //) U_iram_ecc_enc(
            ///*input  logic [DATA_WIDTH-1:0]             */  .dat_i                  (iram_wdata_o       [i] ),
            ///*output logic [ECC_WIDTH:0]                */  .ecc_o                  (iram_wdata_ecc_o   [i] ),
            ///*input  logic                              */  .spare_in               (1'b0               )
            //);
            assign iram_wdata_ecc_o[i] = 'd0;
            
            iommu_acd_mtlb_ecc_enc #(
            /*parameter */ .DATA_WIDTH         ($bits(dram_wdata_o[0])    ), // = 128,
            /*parameter */ .ECC_WIDTH          (DECC_WIDTH              ), // = 8,
            /*parameter */ .SPARE_PARAM        (                       )  // = 0
            ) U_dram_ecc_enc(
            /*input  logic [DATA_WIDTH-1:0]             */  .dat_i                  (dram_wdata_o       [i] ),
            /*output logic [ECC_WIDTH:0]                */  .ecc_o                  (dram_wdata_ecc_o   [i] ),
            /*input  logic                              */  .spare_in               (1'b0               )
            );
            
            iommu_acd_mtlb_ecc_dec #(
            /*parameter */ .DATA_WIDTH         ($bits(tram_rdata_i[0])    ), // = 128,
            /*parameter */ .ECC_WIDTH          (TECC_WIDTH              ), // = 8,
            /*parameter */ .SPARE_PARAM        (1'b0                   )  // = 0
            ) U_tram_ecc_dec(
            /*input  logic [DATA_WIDTH-1:0]             */  .dat_i                  (tram_rdata_i       [i] ),
            /*input  logic [ECC_WIDTH:0]                */  .ecc_i                  (tram_rdata_ecc_i   [i] ),
            /*output logic [DATA_WIDTH-1:0]             */  .dat_o                  (),
            /*output logic [1:0]                        */  .err_o                  (tram_rdata_err     [i] ),
            /*input  logic                              */  .spare_in               (1'b0               )
            );
            
            //iommu_acd_mtlb_ecc_dec #(
            ///*parameter */ .DATA_WIDTH         ($bits(iram_rdata_i[0])    ), // = 128,
            ///*parameter */ .ECC_WIDTH          (TECC_WIDTH              ), // = 8,
            ///*parameter */ .SPARE_PARAM        (1'b0                   )  // = 0
            //) U_iram_ecc_dec(
            ///*input  logic [DATA_WIDTH-1:0]             */  .dat_i                  (iram_rdata_i       [i] ),
            ///*input  logic [ECC_WIDTH:0]                */  .ecc_i                  (iram_rdata_ecc_i   [i] ),
            ///*output logic [DATA_WIDTH-1:0]             */  .dat_o                  (),
            ///*output logic [1:0]                        */  .err_o                  (iram_rdata_err     [i] ),
            ///*input  logic                              */  .spare_in               (1'b0               )
            //);
            assign iram_rdata_err[i] = 'd0;
            
            iommu_acd_mtlb_ecc_dec #(
            /*parameter */ .DATA_WIDTH         ($bits(dram_rdata_i[0])    ), // = 128,
            /*parameter */ .ECC_WIDTH          (DECC_WIDTH              ), // = 8,
            /*parameter */ .SPARE_PARAM        (1'b0                   )  // = 0
            ) U_dram_ecc_dec(
            /*input  logic [DATA_WIDTH-1:0]             */  .dat_i                  (dram_rdata_i       [i] ),
            /*input  logic [ECC_WIDTH:0]                */  .ecc_i                  (dram_rdata_ecc_i   [i] ),
            /*output logic [DATA_WIDTH-1:0]             */  .dat_o                  (),
            /*output logic [1:0]                        */  .err_o                  (dram_rdata_err     [i] ),
            /*input  logic                              */  .spare_in               (1'b0               )
            );
        end
    end
endgenerate
//}}}

//}}}

//=== ACK {{{
    assign lkp_ack_valid_o = ak3_lkp_ack_valid_o;
    assign lkp_ack_o       = ak3_lkp_ack_o;
    assign upd_ack_valid_o = ak3_upd_ack_valid_o;
    assign upd_ack_o       = ak3_upd_ack_o;
    assign inv_ack_valid_o = ak3_inv_ack_valid_o;
    assign inv_ack_o       = ak3_inv_ack_o;

//}}}

//=== ecc_err_o {{{
    always@(*) begin
        for(int unsigned ei=0; ei<BANK_WAY_NUM; ei++) begin
            ecc_err_o_int[0][ei] = wr2_err_i[ei][0] & wr2_bank_req_valid_i & wr2_bank_req_ready_o;
            ecc_err_o_int[1][ei] = wr2_err_i[ei][1] & wr2_bank_req_valid_i & wr2_bank_req_ready_o;
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            ecc_err_o <= 2'b00;
        end
        else begin
            ecc_err_o[0] <= |ecc_err_o_int[0];
            ecc_err_o[1] <= |ecc_err_o_int[1];
        end
    end
//}}}

//}}}

//=== ARB inst {{{
//=== LKP arb {{{
    iommu_acd_bus_handler_trans_arb #(
    /*parameter */ .ARB_TYPE       (1                  ), // = 0,
    /*parameter */ .REQ_NUM        (CABIN_LKP_NUM      ), // = 2,
    /*parameter type         */ .DATA_TYPE      (BANK_LKP_REQ_TYPE  ), // = logic,
    /*parameter */ .AXIVLDRDY      (1                  )  // = 1
    ) U_lkp_arb(                                          
    /*input  logic                          */  .clk                (clk                ),
    /*input  logic                          */  .rstn               (rstn               ),
    /*input  logic [REQ_NUM-1:0]            */  .req_i              (lkp_req_valid_i    ),
    /*input  logic [REQ_NUM-1:0]            */  .req_prior_i        (CABIN_LKP_NUM'(0)  ),
    /*input  DATA_TYPE [REQ_NUM-1:0]        */  .data_i             (lkp_req_i          ),
    /*output logic [REQ_NUM-1:0]            */  .gnt_o              (lkp_req_ready_o    ),
    /*output logic                          */  .req_o              (arb_lkp_req_valid  ),
    /*output DATA_TYPE                      */  .data_o             (arb_lkp_req        ),
    /*input  logic                          */  .gnt_i              (arb_lkp_req_ready  ) 
    );
    assign arb_bank_req_valid_unmux[2]  = arb_lkp_req_valid;
    assign arb_lkp_req_ready            = arb_bank_req_ready_unmux[2];
    assign arb_bank_req_unmux[2].typ    = 3'b001;
    assign arb_bank_req_unmux[2].lidx   = lkp_req_ready_o;
    assign arb_bank_req_unmux[2].uidx   = 'd0;
    assign arb_bank_req_unmux[2].iidx   = 'd0;
    assign arb_bank_req_unmux[2].lkp    = arb_lkp_req;
    assign arb_bank_req_unmux[2].upd    = 'd0;
    assign arb_bank_req_unmux[2].inv    = 'd0;
//}}}
//=== UPD arb {{{
    iommu_acd_bus_handler_trans_arb #(
    /*parameter */ .ARB_TYPE       (1                  ), // = 0,
    /*parameter */ .REQ_NUM        (CABIN_UPD_NUM      ), // = 2,
    /*parameter type         */ .DATA_TYPE      (BANK_UPD_REQ_TYPE  ), // = logic,
    /*parameter */ .AXIVLDRDY      (1                  )  // = 1
    ) U_upd_arb(                                          
    /*input  logic                          */  .clk                (clk                ),
    /*input  logic                          */  .rstn               (rstn               ),
    /*input  logic [REQ_NUM-1:0]            */  .req_i              (upd_req_valid_i    ),
    /*input  logic [REQ_NUM-1:0]            */  .req_prior_i        (CABIN_UPD_NUM'(0)  ),
    /*input  DATA_TYPE [REQ_NUM-1:0]        */  .data_i             (upd_req_i          ),
    /*output logic [REQ_NUM-1:0]            */  .gnt_o              (upd_req_ready_o    ),
    /*output logic                          */  .req_o              (arb_upd_req_valid  ),
    /*output DATA_TYPE                      */  .data_o             (arb_upd_req        ),
    /*input  logic                          */  .gnt_i              (arb_upd_req_ready  ) 
    );
    assign arb_bank_req_valid_unmux[0]  = arb_upd_req_valid;
    assign arb_upd_req_ready            = arb_bank_req_ready_unmux[0];
    assign arb_bank_req_unmux[0].typ    = 3'b010;
    assign arb_bank_req_unmux[0].lidx   = 'd0;
    assign arb_bank_req_unmux[0].uidx   = upd_req_ready_o;
    assign arb_bank_req_unmux[0].iidx   = 'd0;
    assign arb_bank_req_unmux[0].lkp    = 'd0;
    assign arb_bank_req_unmux[0].upd    = arb_upd_req;
    assign arb_bank_req_unmux[0].inv    = 'd0;
//}}}
//=== INV arb {{{
    iommu_acd_bus_handler_trans_arb #(
    /*parameter */ .ARB_TYPE       (1                  ), // = 0,
    /*parameter */ .REQ_NUM        (CABIN_INV_NUM      ), // = 2,
    /*parameter type         */ .DATA_TYPE      (BANK_INV_REQ_TYPE  ), // = logic,
    /*parameter */ .AXIVLDRDY      (1                  )  // = 1
    ) U_inv_arb(                                          
    /*input  logic                          */  .clk                (clk                ),
    /*input  logic                          */  .rstn               (rstn               ),
    /*input  logic [REQ_NUM-1:0]            */  .req_i              (inv_req_valid_i    ),
    /*input  logic [REQ_NUM-1:0]            */  .req_prior_i        (CABIN_INV_NUM'(0)  ),
    /*input  DATA_TYPE [REQ_NUM-1:0]        */  .data_i             (inv_req_i          ),
    /*output logic [REQ_NUM-1:0]            */  .gnt_o              (inv_req_ready_o    ),
    /*output logic                          */  .req_o              (arb_inv_req_valid  ),
    /*output DATA_TYPE                      */  .data_o             (arb_inv_req        ),
    /*input  logic                          */  .gnt_i              (arb_inv_req_ready  ) 
    );
    assign arb_bank_req_valid_unmux[1]  = arb_inv_req_valid;
    assign arb_inv_req_ready            = arb_bank_req_ready_unmux[1];
    assign arb_bank_req_unmux[1].typ    = 3'b100;
    assign arb_bank_req_unmux[1].lidx   = 'd0;
    assign arb_bank_req_unmux[1].uidx   = 'd0;
    assign arb_bank_req_unmux[1].iidx   = inv_req_ready_o;
    assign arb_bank_req_unmux[1].lkp    = 'd0;
    assign arb_bank_req_unmux[1].upd    = 'd0;
    assign arb_bank_req_unmux[1].inv    = arb_inv_req;
//}}}
//=== Final arb {{{
    assign arb_bank_req_valid_unmux[3]  = 'd0;
    assign arb_bank_req_unmux[3]        = 'd0;

    iommu_acd_bus_handler_trans_arb #(
    /*parameter */ .ARB_TYPE       (0                  ), // = 0,
    /*parameter */ .REQ_NUM        (4                  ), // = 2,
    /*parameter type         */ .DATA_TYPE      (bank_req_t         ), // = logic,
    /*parameter */ .AXIVLDRDY      (1                  )  // = 1
    ) U_bank_req_arb(                                          
    /*input  logic                          */  .clk                (clk                        ),
    /*input  logic                          */  .rstn               (rstn                       ),
    /*input  logic [REQ_NUM-1:0]            */  .req_i              (arb_bank_req_valid_unmux   ),
    /*input  logic [REQ_NUM-1:0]            */  .req_prior_i        (4'(0)                      ),
    /*input  DATA_TYPE [REQ_NUM-1:0]        */  .data_i             (arb_bank_req_unmux         ),
    /*output logic [REQ_NUM-1:0]            */  .gnt_o              (arb_bank_req_ready_unmux   ),
    /*output logic                          */  .req_o              (arb_bank_req_valid_o       ),
    /*output DATA_TYPE                      */  .data_o             (arb_bank_req_o             ),
    /*input  logic                          */  .gnt_i              (arb_bank_req_ready_i       ) 
    );
//}}}

//}}}

//=== RD0 inst {{{
    assign rd0_bank_req_valid_i = arb_bank_req_valid_o;
    assign arb_bank_req_ready_i = rd0_bank_req_ready_o;
    assign rd0_bank_req_i       = arb_bank_req_o;

    iommu_atd_dtc_mtlb_bank_rd0 #(
    /*parameter */ .BANK_TYPE                          (BANK_TYPE                          ), // = 2'b00,
    /*parameter type         */ .BANK_REQ_TYPE                      (bank_req_t                         ), // = iommu_atd_cache_pkg::ddtc_bank_req_t,
    /*parameter type         */ .INVALID_REQ_TYPE                   (INVALID_REQ_TYPE                   ), // = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    /*parameter type         */ .LOOKUP_REQ_TYPE                    (LOOKUP_REQ_TYPE                    ), // = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE                    (LOOKUP_ACK_TYPE                    ), // = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE                    (UPDATE_REQ_TYPE                    ), // = iommu_atd_cache_pkg::ddtc_update_req_t,
    /*parameter type         */ .BANK_LKP_REQ_TYPE                  (BANK_LKP_REQ_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    /*parameter type         */ .BANK_LKP_ACK_TYPE                  (BANK_LKP_ACK_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    /*parameter type         */ .BANK_UPD_REQ_TYPE                  (BANK_UPD_REQ_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_upd_req_t,
    /*parameter type         */ .BANK_UPD_ACK_TYPE                  (BANK_UPD_ACK_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_upd_ack_t,
    /*parameter type         */ .BANK_INV_REQ_TYPE                  (BANK_INV_REQ_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_inv_req_t,
    /*parameter type         */ .BANK_INV_ACK_TYPE                  (BANK_INV_ACK_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_inv_ack_t,
    /*parameter */ .CABIN_LKP_IDX_WIDTH                (CABIN_LKP_IDX_WIDTH                ), // = 3,
    /*parameter */ .CABIN_UPD_IDX_WIDTH                (CABIN_UPD_IDX_WIDTH                ), // = 2,
    /*parameter */ .CABIN_INV_IDX_WIDTH                (CABIN_INV_IDX_WIDTH                ), // = 1,
    /*parameter */ .BANK_IDX_WIDTH                     (BANK_IDX_WIDTH                     ), // = 3,
    /*parameter */ .BANK_SET_IDX_WIDTH                 (BANK_SET_IDX_WIDTH                 ), // = 3,
    /*parameter */ .BANK_WAY_IDX_WIDTH                 (BANK_WAY_IDX_WIDTH                 ), // = 3,
    /*parameter */ .SPARE_PARAM                        (1'b0                               )  // = 0
    ) U_rd0(                                                              
    /*input  logic                                              */  .clk                                (clk                                ),
    /*input  logic                                              */  .rstn                               (rstn                               ),
    /*input  logic                                              */  .bank_req_valid_i                   (rd0_bank_req_valid_i               ),
    /*output logic                                              */  .bank_req_ready_o                   (rd0_bank_req_ready_o               ),
    /*input  BANK_REQ_TYPE                                      */  .bank_req_i                         (rd0_bank_req_i                     ),
    /*output logic                                              */  .bank_req_valid_o                   (rd0_bank_req_valid_o               ),
    /*input  logic                                              */  .bank_req_ready_i                   (rd0_bank_req_ready_i               ),
    /*output BANK_REQ_TYPE                                      */  .bank_req_o                         (rd0_bank_req_o                     ),
    /*output logic [BANK_WAY_NUM-1:0]                           */  .tram_cs_o                          (rd0_tram_cs_o                      ),  //active low
    /*output logic [BANK_WAY_NUM-1:0]                           */  .tram_wr_o                          (rd0_tram_wr_o                      ),  //0:W, 1:R
    /*output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]  */  .tram_addr_o                        (rd0_tram_addr_o                    ),
    /*output logic [BANK_WAY_NUM-1:0]                           */  .iram_cs_o                          (rd0_iram_cs_o                      ),
    /*output logic [BANK_WAY_NUM-1:0]                           */  .iram_wr_o                          (rd0_iram_wr_o                      ),
    /*output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]  */  .iram_addr_o                        (rd0_iram_addr_o                    ),
    /*output logic [BANK_WAY_NUM-1:0]                           */  .dram_cs_o                          (rd0_dram_cs_o                      ),
    /*output logic [BANK_WAY_NUM-1:0]                           */  .dram_wr_o                          (rd0_dram_wr_o                      ),
    /*output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]  */  .dram_addr_o                        (rd0_dram_addr_o                    ),
    /*output logic                                              */  .uram_cs_o                          (rd0_uram_cs_o                      ),
    /*output logic                                              */  .uram_wr_o                          (rd0_uram_wr_o                      ),
    /*output logic [BANK_SET_IDX_WIDTH-1:0]                     */  .uram_addr_o                        (rd0_uram_addr_o                    ),
    /*input  logic                                              */  .spare_in                           (1'b0                               ) 
    );

//}}}

//=== MATCH1 inst {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            mt1_ram_valid_i_raw <= 'd0;
        else
            mt1_ram_valid_i_raw <= ~rd0_tram_cs_o;
    end
    
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            mt1_ram_valid_i_fifo    <= 'd0;
            mt1_tram_rdata_i_fifo   <= 'd0;
            mt1_iram_rdata_i_fifo   <= 'd0;
            mt1_uram_rdata_i_fifo   <= 'd0;
            mt1_tram_rdata_err_fifo <= 'd0;
            mt1_iram_rdata_err_fifo <= 'd0;
            mt1_dram_rdata_err_fifo <= 'd0;
        end
        else begin
            if(rd0_bank_req_valid_o & ~rd0_bank_req_ready_i) begin
                mt1_ram_valid_i_fifo    <= mt1_ram_valid_i_raw;
                mt1_tram_rdata_i_fifo   <= tram_rdata_i;
                mt1_iram_rdata_i_fifo   <= iram_rdata_i;
                mt1_uram_rdata_i_fifo   <= uram_rdata_i;
                mt1_tram_rdata_err_fifo <= tram_rdata_err;
                mt1_iram_rdata_err_fifo <= iram_rdata_err;
                mt1_dram_rdata_err_fifo <= dram_rdata_err;
            end
            else if(|mt1_ram_valid_i_fifo & mt1_bank_req_valid_i & mt1_bank_req_ready_o)
                mt1_ram_valid_i_fifo  <= 'd0;
        end
    end

    assign mt1_bank_req_valid_i = rd0_bank_req_valid_o;
    assign rd0_bank_req_ready_i = mt1_bank_req_ready_o;
    assign mt1_bank_req_i       = rd0_bank_req_o;
    assign mt1_ram_valid_i      = mt1_ram_valid_i_raw | mt1_ram_valid_i_fifo;

genvar mt1ing;
generate
    for(mt1ing=0; mt1ing<BANK_WAY_NUM; mt1ing++) begin : mt1_in_gen
        assign mt1_tram_rdata_i[mt1ing]     = mt1_ram_valid_i_fifo[mt1ing] ? mt1_tram_rdata_i_fifo[mt1ing]  : tram_rdata_i[mt1ing];
        assign mt1_iram_rdata_i[mt1ing]     = mt1_ram_valid_i_fifo[mt1ing] ? mt1_iram_rdata_i_fifo[mt1ing]  : iram_rdata_i[mt1ing];
        assign mt1_tram_rdata_err_i[mt1ing] = mt1_ram_valid_i_fifo[mt1ing] ? mt1_tram_rdata_err_fifo[mt1ing]: tram_rdata_err[mt1ing];
        assign mt1_iram_rdata_err_i[mt1ing] = mt1_ram_valid_i_fifo[mt1ing] ? mt1_iram_rdata_err_fifo[mt1ing]: iram_rdata_err[mt1ing];
        assign mt1_dram_rdata_err_i[mt1ing] = mt1_ram_valid_i_fifo[mt1ing] ? mt1_dram_rdata_err_fifo[mt1ing]: dram_rdata_err[mt1ing];
    end
    assign mt1_uram_rdata_i     = (|mt1_ram_valid_i_fifo) ? mt1_uram_rdata_i_fifo  : uram_rdata_i;
endgenerate
    iommu_atd_dtc_mtlb_bank_match1 #(
    /*parameter */ .CACHE_TYPE                         (CACHE_TYPE                         ), //= 0, // 0:DDTC, 1:PDTC
    /*parameter */ .BANK_TYPE                          (BANK_TYPE                          ), // = 2'b00,
    /*parameter type         */ .INVALID_REQ_TYPE                   (INVALID_REQ_TYPE                   ), // = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    /*parameter type         */ .BANK_REQ_TYPE                      (bank_req_t                         ), // = iommu_atd_cache_pkg::ddtc_bank_req_t,
    /*parameter type         */ .LOOKUP_REQ_TYPE                    (LOOKUP_REQ_TYPE                    ), // = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE                    (LOOKUP_ACK_TYPE                    ), // = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE                    (UPDATE_REQ_TYPE                    ), // = iommu_atd_cache_pkg::ddtc_update_req_t,
    /*parameter type         */ .BANK_LKP_REQ_TYPE                  (BANK_LKP_REQ_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    /*parameter type         */ .BANK_LKP_ACK_TYPE                  (BANK_LKP_ACK_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    /*parameter type         */ .BANK_UPD_REQ_TYPE                  (BANK_UPD_REQ_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_upd_req_t,
    /*parameter type         */ .BANK_UPD_ACK_TYPE                  (BANK_UPD_ACK_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_upd_ack_t,
    /*parameter type         */ .BANK_INV_REQ_TYPE                  (BANK_INV_REQ_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_inv_req_t,
    /*parameter type         */ .BANK_INV_ACK_TYPE                  (BANK_INV_ACK_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_inv_ack_t,
    /*parameter */ .CABIN_LKP_IDX_WIDTH                (CABIN_LKP_IDX_WIDTH                ), // = 3,
    /*parameter */ .CABIN_UPD_IDX_WIDTH                (CABIN_UPD_IDX_WIDTH                ), // = 2,
    /*parameter */ .CABIN_INV_IDX_WIDTH                (CABIN_INV_IDX_WIDTH                ), // = 1,
    /*parameter */ .BANK_IDX_WIDTH                     (BANK_IDX_WIDTH                     ), // = 3,
    /*parameter */ .BANK_SET_IDX_WIDTH                 (BANK_SET_IDX_WIDTH                 ), // = 3,
    /*parameter */ .BANK_WAY_IDX_WIDTH                 (BANK_WAY_IDX_WIDTH                 ), // = 3,
    /*parameter type         */ .MTLB_TAG_TYPE                      (MTLB_TAG_TYPE                      ), // = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    /*parameter type         */ .MTLB_ITAG_TYPE                     (MTLB_ITAG_TYPE                     ), // = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    /*parameter type         */ .MTLB_UTAG_TYPE                     (MTLB_UTAG_TYPE                     ), // = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    /*parameter */ .SPARE_PARAM                        (SPARE_PARAM                        )  // = 0
    ) U_mt1(                                                              
    /*input  logic                                              */  .clk                                (clk                                ),
    /*input  logic                                              */  .rstn                               (rstn                               ),
    /*input  logic                                              */  .bank_req_valid_i                   (mt1_bank_req_valid_i               ),
    /*output logic                                              */  .bank_req_ready_o                   (mt1_bank_req_ready_o               ),
    /*input  BANK_REQ_TYPE                                      */  .bank_req_i                         (mt1_bank_req_i                     ),
    /*output logic                                              */  .bank_req_valid_o                   (mt1_bank_req_valid_o               ),
    /*input  logic                                              */  .bank_req_ready_i                   (mt1_bank_req_ready_i               ),
    /*output BANK_REQ_TYPE                                      */  .bank_req_o                         (mt1_bank_req_o                     ),
    /*output logic [BANK_WAY_NUM-1:0]                           */  .tag_valid_o                        (mt1_tag_valid_o                    ),
    /*output logic [BANK_WAY_NUM-1:0]                           */  .tag_hit_o                          (mt1_tag_hit_o                      ),
    /*output MTLB_TAG_TYPE  [BANK_WAY_NUM-1:0]                  */  .tag_o                              (mt1_tag_o                          ),
    /*output MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                  */  .itag_o                             (mt1_itag_o                         ),
    /*output MTLB_UTAG_TYPE                                     */  .utag_o                             (mt1_utag_o                         ),
    /*output logic [BANK_WAY_NUM-1:0] [1:0]                     */  .err_o                              (mt1_err_o                          ),
    /*input  logic [BANK_WAY_NUM-1:0]                           */  .ram_valid_i                        (mt1_ram_valid_i                    ),
    /*input  MTLB_TAG_TYPE  [BANK_WAY_NUM-1:0]                  */  .tram_rdata_i                       (mt1_tram_rdata_i                   ),
    /*input  MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                  */  .iram_rdata_i                       (mt1_iram_rdata_i                   ),
    /*input  MTLB_UTAG_TYPE                                     */  .uram_rdata_i                       (mt1_uram_rdata_i                   ),
    /*input  logic [BANK_WAY_NUM-1:0] [1:0]                     */  .tram_rdata_err_i                   (mt1_tram_rdata_err_i               ),
    /*input  logic [BANK_WAY_NUM-1:0] [1:0]                     */  .iram_rdata_err_i                   (mt1_iram_rdata_err_i               ),
    /*input  logic [BANK_WAY_NUM-1:0] [1:0]                     */  .dram_rdata_err_i                   (mt1_dram_rdata_err_i               ),
    /*input  logic                                              */  .multi_hit_check_i                  (multi_hit_check_i                  ),
    /*output logic                                              */  .multi_hit_fault_o                  (multi_hit_fault_o                  ),
    /*input  logic                                              */  .spare_in                           (1'b0                               ) 
    );

//}}}

//=== WR2 inst {{{
    assign wr2_bank_req_valid_i = mt1_bank_req_valid_o;
    assign mt1_bank_req_ready_i = wr2_bank_req_ready_o;
    assign wr2_bank_req_i       = mt1_bank_req_o;
    assign wr2_tag_valid_i      = mt1_tag_valid_o;
    assign wr2_tag_hit_i        = mt1_tag_hit_o;
    assign wr2_tag_i            = mt1_tag_o;
    assign wr2_itag_i           = mt1_itag_o;
    assign wr2_utag_i           = mt1_utag_o;
    assign wr2_err_i            = mt1_err_o;

    iommu_atd_dtc_mtlb_bank_wr2 #(
    /*parameter */ .CACHE_TYPE                         (CACHE_TYPE                         ), //= 0, // 0:DDTC, 1:PDTC
    /*parameter */ .BANK_TYPE                          (BANK_TYPE                          ), // = 2'b00,
    /*parameter type         */ .BANK_REQ_TYPE                      (bank_req_t                         ), // = iommu_atd_cache_pkg::ddtc_bank_req_t,
    /*parameter type         */ .INVALID_REQ_TYPE                   (INVALID_REQ_TYPE                   ), // = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    /*parameter type         */ .LOOKUP_REQ_TYPE                    (LOOKUP_REQ_TYPE                    ), // = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE                    (LOOKUP_ACK_TYPE                    ), // = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE                    (UPDATE_REQ_TYPE                    ), // = iommu_atd_cache_pkg::ddtc_update_req_t,
    /*parameter type         */ .BANK_LKP_REQ_TYPE                  (BANK_LKP_REQ_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    /*parameter type         */ .BANK_LKP_ACK_TYPE                  (BANK_LKP_ACK_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    /*parameter type         */ .BANK_UPD_REQ_TYPE                  (BANK_UPD_REQ_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_upd_req_t,
    /*parameter type         */ .BANK_UPD_ACK_TYPE                  (BANK_UPD_ACK_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_upd_ack_t,
    /*parameter type         */ .BANK_INV_REQ_TYPE                  (BANK_INV_REQ_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_inv_req_t,
    /*parameter type         */ .BANK_INV_ACK_TYPE                  (BANK_INV_ACK_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_inv_ack_t,
    /*parameter */ .CABIN_LKP_IDX_WIDTH                (CABIN_LKP_IDX_WIDTH                ), // = 3,
    /*parameter */ .CABIN_UPD_IDX_WIDTH                (CABIN_UPD_IDX_WIDTH                ), // = 2,
    /*parameter */ .CABIN_INV_IDX_WIDTH                (CABIN_INV_IDX_WIDTH                ), // = 1,
    /*parameter */ .BANK_IDX_WIDTH                     (BANK_IDX_WIDTH                     ), // = 3,
    /*parameter */ .BANK_SET_IDX_WIDTH                 (BANK_SET_IDX_WIDTH                 ), // = 3,
    /*parameter */ .BANK_WAY_IDX_WIDTH                 (BANK_WAY_IDX_WIDTH                 ), // = 3,
    /*parameter type         */ .MTLB_TAG_TYPE                      (MTLB_TAG_TYPE                      ), // = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    /*parameter type         */ .MTLB_ITAG_TYPE                     (MTLB_ITAG_TYPE                     ), // = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    /*parameter type         */ .MTLB_UTAG_TYPE                     (MTLB_UTAG_TYPE                     ), // = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    /*parameter type         */ .MTLB_DAT_TYPE                      (MTLB_DAT_TYPE                      ), // = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_l,
    /*parameter */ .SPARE_PARAM                        (1'b0                               )  // = 0
    ) U_wr2(                                                              
    /*input  logic                                              */  .clk                                (clk                                ),
    /*input  logic                                              */  .rstn                               (rstn                               ),
    /*input  logic                                              */  .bank_req_valid_i                   (wr2_bank_req_valid_i               ),
    /*output logic                                              */  .bank_req_ready_o                   (wr2_bank_req_ready_o               ),
    /*input  BANK_REQ_TYPE                                      */  .bank_req_i                         (wr2_bank_req_i                     ),
    /*output logic                                              */  .bank_req_valid_o                   (wr2_bank_req_valid_o               ),
    /*input  logic                                              */  .bank_req_ready_i                   (wr2_bank_req_ready_i               ),
    /*output BANK_REQ_TYPE                                      */  .bank_req_o                         (wr2_bank_req_o                     ),
    /*input  logic [BANK_WAY_NUM-1:0]                           */  .tag_valid_i                        (wr2_tag_valid_i                    ),
    /*input  logic [BANK_WAY_NUM-1:0]                           */  .tag_hit_i                          (wr2_tag_hit_i                      ),
    /*input  MTLB_TAG_TYPE  [BANK_WAY_NUM-1:0]                  */  .tag_i                              (wr2_tag_i                          ),
    /*input  MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                  */  .itag_i                             (wr2_itag_i                         ),
    /*input  MTLB_UTAG_TYPE                                     */  .utag_i                             (wr2_utag_i                         ),
    /*input  logic [BANK_WAY_NUM-1:0] [1:0]                     */  .err_i                              (wr2_err_i                          ),
    /*output logic                                              */  .tag_valid_o                        (wr2_tag_valid_o                    ),
    /*output logic                                              */  .tag_hit_o                          (wr2_tag_hit_o                      ),
    /*output MTLB_TAG_TYPE                                      */  .tag_o                              (wr2_tag_o                          ),
    /*output MTLB_ITAG_TYPE                                     */  .itag_o                             (wr2_itag_o                         ),
    /*output logic [BANK_WAY_NUM-1:0]                           */  .tram_cs_o                          (wr2_tram_cs_o                      ),
    /*output logic [BANK_WAY_NUM-1:0]                           */  .tram_wr_o                          (wr2_tram_wr_o                      ),
    /*output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]  */  .tram_addr_o                        (wr2_tram_addr_o                    ),
    /*output MTLB_TAG_TYPE [BANK_WAY_NUM-1:0]                   */  .tram_wdata_o                       (wr2_tram_wdata_o                   ),
    /*output logic [BANK_WAY_NUM-1:0]                           */  .iram_cs_o                          (wr2_iram_cs_o                      ),
    /*output logic [BANK_WAY_NUM-1:0]                           */  .iram_wr_o                          (wr2_iram_wr_o                      ),
    /*output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]  */  .iram_addr_o                        (wr2_iram_addr_o                    ),
    /*output MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                  */  .iram_wdata_o                       (wr2_iram_wdata_o                   ),
    /*output logic                                              */  .uram_cs_o                          (wr2_uram_cs_o                      ),
    /*output logic                                              */  .uram_wr_o                          (wr2_uram_wr_o                      ),
    /*output logic [BANK_SET_IDX_WIDTH-1:0]                     */  .uram_addr_o                        (wr2_uram_addr_o                    ),
    /*output MTLB_UTAG_TYPE                                     */  .uram_wdata_o                       (wr2_uram_wdata_o                   ),
    /*output logic [BANK_WAY_NUM-1:0]                           */  .dram_cs_o                          (wr2_dram_cs_o                      ),
    /*output logic [BANK_WAY_NUM-1:0]                           */  .dram_wr_o                          (wr2_dram_wr_o                      ),
    /*output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]  */  .dram_addr_o                        (wr2_dram_addr_o                    ),
    /*output MTLB_DAT_TYPE [BANK_WAY_NUM-1:0]                   */  .dram_wdata_o                       (wr2_dram_wdata_o                   ),
    /*input  logic                                              */  .spare_in                           (1'b0                               ) 
    );

//}}}

//=== ACK3 inst {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            ak3_ram_valid_i <= 1'b0;
        else
            ak3_ram_valid_i <= ~wr2_dram_cs_o & wr2_dram_wr_o;
    end
    assign ak3_bank_req_valid_i = wr2_bank_req_valid_o;
    assign wr2_bank_req_ready_i = ak3_bank_req_ready_o;
    assign ak3_bank_req_i       = wr2_bank_req_o;
    assign ak3_tag_valid_i      = wr2_tag_valid_o;
    assign ak3_tag_hit_i        = wr2_tag_hit_o;
    assign ak3_tag_i            = wr2_tag_o;
    assign ak3_itag_i           = wr2_itag_o;
    assign ak3_dram_rdata_i     = dram_rdata_i;

    iommu_atd_dtc_mtlb_bank_ack3 #(
    /*parameter */ .CACHE_TYPE                         (CACHE_TYPE                         ), // = 0, // 0:DDTC, 1:PDTC
    /*parameter */ .BANK_TYPE                          (BANK_TYPE                          ), // = 2'b00,
    /*parameter type         */ .BANK_REQ_TYPE                      (bank_req_t                         ), // = iommu_atd_cache_pkg::ddtc_bank_req_t,
    /*parameter type         */ .INVALID_REQ_TYPE                   (INVALID_REQ_TYPE                   ), // = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    /*parameter type         */ .LOOKUP_REQ_TYPE                    (LOOKUP_REQ_TYPE                    ), // = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    /*parameter type         */ .LOOKUP_ACK_TYPE                    (LOOKUP_ACK_TYPE                    ), // = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    /*parameter type         */ .UPDATE_REQ_TYPE                    (UPDATE_REQ_TYPE                    ), // = iommu_atd_cache_pkg::ddtc_update_req_t,
    /*parameter type         */ .BANK_LKP_REQ_TYPE                  (BANK_LKP_REQ_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    /*parameter type         */ .BANK_LKP_ACK_TYPE                  (BANK_LKP_ACK_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    /*parameter type         */ .BANK_UPD_REQ_TYPE                  (BANK_UPD_REQ_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_upd_req_t,
    /*parameter type         */ .BANK_UPD_ACK_TYPE                  (BANK_UPD_ACK_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_upd_ack_t,
    /*parameter type         */ .BANK_INV_REQ_TYPE                  (BANK_INV_REQ_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_inv_req_t,
    /*parameter type         */ .BANK_INV_ACK_TYPE                  (BANK_INV_ACK_TYPE                  ), // = iommu_atd_cache_pkg::ddtc_bank_inv_ack_t,
    /*parameter */ .CABIN_LKP_IDX_WIDTH                (CABIN_LKP_IDX_WIDTH                ), // = 3,
    /*parameter */ .CABIN_UPD_IDX_WIDTH                (CABIN_UPD_IDX_WIDTH                ), // = 2,
    /*parameter */ .CABIN_INV_IDX_WIDTH                (CABIN_INV_IDX_WIDTH                ), // = 1,
    /*parameter */ .BANK_IDX_WIDTH                     (BANK_IDX_WIDTH                     ), // = 3,
    /*parameter */ .BANK_SET_IDX_WIDTH                 (BANK_SET_IDX_WIDTH                 ), // = 3,
    /*parameter */ .BANK_WAY_IDX_WIDTH                 (BANK_WAY_IDX_WIDTH                 ), // = 3,
    /*parameter type         */ .MTLB_TAG_TYPE                      (MTLB_TAG_TYPE                      ), // = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    /*parameter type         */ .MTLB_ITAG_TYPE                     (MTLB_ITAG_TYPE                     ), // = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    /*parameter type         */ .MTLB_UTAG_TYPE                     (MTLB_UTAG_TYPE                     ), // = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    /*parameter type         */ .MTLB_DAT_TYPE                      (MTLB_DAT_TYPE                      ), // = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_l,
    /*parameter */ .SPARE_PARAM                        (1'b0                               )  // = 0
    ) U_ak3(                                                              
    /*input  logic                                              */  .clk                                (clk                                ),
    /*input  logic                                              */  .rstn                               (rstn                               ),
    /*input  logic                                              */  .bank_req_valid_i                   (ak3_bank_req_valid_i               ),
    /*output logic                                              */  .bank_req_ready_o                   (ak3_bank_req_ready_o               ),
    /*input  BANK_REQ_TYPE                                      */  .bank_req_i                         (ak3_bank_req_i                     ),
    /*input  logic                                              */  .tag_valid_i                        (ak3_tag_valid_i                    ),
    /*input  logic                                              */  .tag_hit_i                          (ak3_tag_hit_i                      ),
    /*input  MTLB_TAG_TYPE                                      */  .tag_i                              (ak3_tag_i                          ),
    /*input  MTLB_ITAG_TYPE                                     */  .itag_i                             (ak3_itag_i                         ),
    /*input  logic [BANK_WAY_NUM-1:0]                           */  .ram_valid_i                        (ak3_ram_valid_i                    ),
    /*input  MTLB_DAT_TYPE  [BANK_WAY_NUM-1:0]                  */  .dram_rdata_i                       (ak3_dram_rdata_i                   ),
    /*output logic [CABIN_LKP_NUM-1:0]                          */  .lkp_ack_valid_o                    (ak3_lkp_ack_valid_o                ),
    /*output BANK_LKP_ACK_TYPE [CABIN_LKP_NUM-1:0]              */  .lkp_ack_o                          (ak3_lkp_ack_o                      ),
    /*output logic [CABIN_UPD_NUM-1:0]                          */  .upd_ack_valid_o                    (ak3_upd_ack_valid_o                ),
    /*output BANK_UPD_ACK_TYPE [CABIN_UPD_NUM-1:0]              */  .upd_ack_o                          (ak3_upd_ack_o                      ),
    /*output logic [CABIN_INV_NUM-1:0]                          */  .inv_ack_valid_o                    (ak3_inv_ack_valid_o                ),
    /*output BANK_INV_ACK_TYPE [CABIN_INV_NUM-1:0]              */  .inv_ack_o                          (ak3_inv_ack_o                      ),
    /*input  logic                                              */  .spare_in                           (1'b0                               ) 
    );

//}}}

endmodule
//}}}



module iommu_atd_dtc_mtlb_bank_rd0 #( //{{{
    parameter  BANK_TYPE                           = 2'b00,
    parameter type          BANK_REQ_TYPE                       = iommu_atd_cache_pkg::ddtc_bank_req_t,
    parameter type          INVALID_REQ_TYPE                    = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    parameter type          LOOKUP_REQ_TYPE                     = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE                     = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE                     = iommu_atd_cache_pkg::ddtc_update_req_t,
    parameter type          BANK_LKP_REQ_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    parameter type          BANK_LKP_ACK_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    parameter type          BANK_UPD_REQ_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_upd_req_t,
    parameter type          BANK_UPD_ACK_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_upd_ack_t,
    parameter type          BANK_INV_REQ_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_inv_req_t,
    parameter type          BANK_INV_ACK_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_inv_ack_t,
    parameter  CABIN_LKP_IDX_WIDTH                 = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    parameter  CABIN_UPD_IDX_WIDTH                 = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH,
    parameter  CABIN_INV_IDX_WIDTH                 = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH,
    parameter  BANK_IDX_WIDTH                      = iommu_atd_cache_pkg::DDTC_MAX_BANK_IDX_WIDTH,
    parameter  BANK_SET_IDX_WIDTH                  = iommu_atd_cache_pkg::DDTC_MAX_BANK_SET_IDX_WIDTH,
    parameter  BANK_WAY_IDX_WIDTH                  = iommu_atd_cache_pkg::DDTC_MAX_BANK_WAY_IDX_WIDTH,
    parameter  CABIN_LKP_NUM                       = 2**CABIN_LKP_IDX_WIDTH,
    parameter  CABIN_UPD_NUM                       = 2**CABIN_UPD_IDX_WIDTH,
    parameter  CABIN_INV_NUM                       = 2**CABIN_INV_IDX_WIDTH,
    parameter  BANK_NUM                            = 2**BANK_IDX_WIDTH,
    parameter  BANK_SET_NUM                        = 2**BANK_SET_IDX_WIDTH,
    parameter  BANK_WAY_NUM                        = 2**BANK_WAY_IDX_WIDTH,
    parameter  SPARE_PARAM                         = 0
)(                                                              
//{{{ IO                                                        
    input  logic                                                clk,
    input  logic                                                rstn,
    // REQ INPUT                                                
    input  logic                                                bank_req_valid_i,
    output logic                                                bank_req_ready_o,
    input  BANK_REQ_TYPE                                        bank_req_i,
    // REQ PIPE OUT                                             
    output logic                                                bank_req_valid_o,
    input  logic                                                bank_req_ready_i,
    output BANK_REQ_TYPE                                        bank_req_o,
    // TAG_RAM RD                                               
    output logic [BANK_WAY_NUM-1:0]                             tram_cs_o,  //active low
    output logic [BANK_WAY_NUM-1:0]                             tram_wr_o,  //0:W, 1:R
    output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]    tram_addr_o,
    // ITAG_RAM RD                                              
    output logic [BANK_WAY_NUM-1:0]                             iram_cs_o,
    output logic [BANK_WAY_NUM-1:0]                             iram_wr_o,
    output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]    iram_addr_o,
    // DAT_RAM RD                                              
    output logic [BANK_WAY_NUM-1:0]                             dram_cs_o,
    output logic [BANK_WAY_NUM-1:0]                             dram_wr_o,
    output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]    dram_addr_o,
    // UTAG_RAM RD                                              
    output logic                                                uram_cs_o,
    output logic                                                uram_wr_o,
    output logic [BANK_SET_IDX_WIDTH-1:0]                       uram_addr_o,
    //
    input  logic                                                spare_in
//}}}
);
//=== Declare {{{
    logic                                                       req_is_lkp, req_is_upd, req_is_inv;

    logic                                                       req_valid;
    logic                                                       req_ready;
    BANK_REQ_TYPE                                               req;
    logic                                                       req_do;
//}}}

//=== MainCode {{{
//=== req decode {{{
    assign req_valid        = bank_req_valid_i;
    assign req              = bank_req_i;
    assign bank_req_ready_o = req_ready;
    assign req_ready        = bank_req_ready_i;

    assign req_is_lkp       = req.typ==3'b001;
    assign req_is_upd       = req.typ==3'b010;
    assign req_is_inv       = req.typ==3'b100;

    assign req_do           = req_valid & req_ready;
//}}}

//=== ram rd {{{
    assign tram_cs_o    = req_do                                ? {BANK_WAY_NUM{1'b0}} : {BANK_WAY_NUM{1'b1}};
    assign tram_wr_o    = {BANK_WAY_NUM{1'b1}};
    assign iram_cs_o    = (req_do & (req_is_lkp | req_is_inv))  ? {BANK_WAY_NUM{1'b0}} : {BANK_WAY_NUM{1'b1}};
    assign iram_wr_o    = {BANK_WAY_NUM{1'b1}};
    assign dram_cs_o    = (req_do & req_is_lkp)                 ? {BANK_WAY_NUM{1'b0}} : {BANK_WAY_NUM{1'b1}};
    assign dram_wr_o    = {BANK_WAY_NUM{1'b1}};

    assign uram_cs_o   = (req_do & req_is_upd) ? 1'b0 : 1'b1;
    assign uram_wr_o   = 1'b1;
genvar i;
generate
    for(i=0; i<BANK_WAY_NUM; i++) begin : ram_addr_gen
        assign tram_addr_o[i] = req_do ? (req_is_lkp ? req.lkp.bank_set_idx[BANK_SET_IDX_WIDTH-1:0] :
                                          req_is_upd ? req.upd.bank_set_idx[BANK_SET_IDX_WIDTH-1:0] :
                                          req_is_inv ? req.inv.bank_set_idx[BANK_SET_IDX_WIDTH-1:0] : 'd0
                                          ) : 'd0;
        assign iram_addr_o[i] = req_do ? (req_is_lkp ? req.lkp.bank_set_idx[BANK_SET_IDX_WIDTH-1:0] :
                                          req_is_inv ? req.inv.bank_set_idx[BANK_SET_IDX_WIDTH-1:0] : 'd0
                                          ) : 'd0;
        assign dram_addr_o[i] = req_do ? (req_is_lkp ? req.lkp.bank_set_idx[BANK_SET_IDX_WIDTH-1:0] : 'd0
                                          ) : 'd0;
    end
endgenerate
    assign uram_addr_o = req_do ? (req_is_upd ? req.upd.bank_set_idx[BANK_SET_IDX_WIDTH-1:0] : 'd0
                                  ) : 'd0;

//}}}

//=== req pipe out {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            bank_req_valid_o <= 1'b0;
            bank_req_o       <= 'd0;
        end
        else begin
            if(bank_req_valid_o & ~bank_req_ready_i)
                bank_req_valid_o <= 1'b1;
            else begin
                if(req_do) begin
                    bank_req_valid_o <= 1'b1;
                    bank_req_o       <= req;
                end
                else begin
                    bank_req_valid_o <= 1'b0;
                end
            end
        end
    end
//}}}

//}}}

endmodule//}}}



module iommu_atd_dtc_mtlb_bank_match1 #( //{{{
    parameter  CACHE_TYPE                          = 0, // 0:DDTC, 1:PDTC
    parameter  BANK_TYPE                           = 2'b00,
    parameter type          INVALID_REQ_TYPE                    = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    parameter type          BANK_REQ_TYPE                       = iommu_atd_cache_pkg::ddtc_bank_req_t,
    parameter type          LOOKUP_REQ_TYPE                     = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE                     = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE                     = iommu_atd_cache_pkg::ddtc_update_req_t,
    parameter type          BANK_LKP_REQ_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    parameter type          BANK_LKP_ACK_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    parameter type          BANK_UPD_REQ_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_upd_req_t,
    parameter type          BANK_UPD_ACK_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_upd_ack_t,
    parameter type          BANK_INV_REQ_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_inv_req_t,
    parameter type          BANK_INV_ACK_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_inv_ack_t,
    parameter  CABIN_LKP_IDX_WIDTH                 = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    parameter  CABIN_UPD_IDX_WIDTH                 = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH,
    parameter  CABIN_INV_IDX_WIDTH                 = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH,
    parameter  BANK_IDX_WIDTH                      = iommu_atd_cache_pkg::DDTC_MAX_BANK_IDX_WIDTH,
    parameter  BANK_SET_IDX_WIDTH                  = iommu_atd_cache_pkg::DDTC_MAX_BANK_SET_IDX_WIDTH,
    parameter  BANK_WAY_IDX_WIDTH                  = iommu_atd_cache_pkg::DDTC_MAX_BANK_WAY_IDX_WIDTH,
    parameter  CABIN_LKP_NUM                       = 2**CABIN_LKP_IDX_WIDTH,
    parameter  CABIN_UPD_NUM                       = 2**CABIN_UPD_IDX_WIDTH,
    parameter  CABIN_INV_NUM                       = 2**CABIN_INV_IDX_WIDTH,
    parameter  BANK_NUM                            = 2**BANK_IDX_WIDTH,
    parameter  BANK_SET_NUM                        = 2**BANK_SET_IDX_WIDTH,
    parameter  BANK_WAY_NUM                        = 2**BANK_WAY_IDX_WIDTH,
    parameter type          MTLB_TAG_TYPE                       = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    parameter type          MTLB_ITAG_TYPE                      = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    parameter type          MTLB_UTAG_TYPE                      = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    parameter  SPARE_PARAM                         = 0
)(                                                              
//{{{ IO                                                        
    input  logic                                                clk,
    input  logic                                                rstn,
    // REQ INPUT                                                
    input  logic                                                bank_req_valid_i,
    output logic                                                bank_req_ready_o,
    input  BANK_REQ_TYPE                                        bank_req_i,
    // REQ PIPE OUT                                             
    output logic                                                bank_req_valid_o,
    input  logic                                                bank_req_ready_i,
    output BANK_REQ_TYPE                                        bank_req_o,
    // TAG PIPE OUT
    output logic [BANK_WAY_NUM-1:0]                             tag_valid_o,
    output logic [BANK_WAY_NUM-1:0]                             tag_hit_o,
    output MTLB_TAG_TYPE  [BANK_WAY_NUM-1:0]                    tag_o,
    output MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                    itag_o,
    output MTLB_UTAG_TYPE                                       utag_o,
    output logic [BANK_WAY_NUM-1:0] [1:0]                       err_o,
    // RAM RDATA                                                
    input  logic [BANK_WAY_NUM-1:0]                             ram_valid_i,
    input  MTLB_TAG_TYPE  [BANK_WAY_NUM-1:0]                    tram_rdata_i,
    input  MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                    iram_rdata_i,
    input  MTLB_UTAG_TYPE                                       uram_rdata_i,
    input  logic [BANK_WAY_NUM-1:0] [1:0]                       tram_rdata_err_i,
    input  logic [BANK_WAY_NUM-1:0] [1:0]                       iram_rdata_err_i,
    input  logic [BANK_WAY_NUM-1:0] [1:0]                       dram_rdata_err_i,
    //
    input  logic                                                multi_hit_check_i,
    output logic                                                multi_hit_fault_o,
    //
    input  logic                                                spare_in
//}}}
);
//=== Declare === {{{
    logic                                                       req_is_lkp, req_is_upd, req_is_inv;

    logic                                                       req_valid;
    logic                                                       req_ready;
    BANK_REQ_TYPE                                               req;
    logic                                                       req_do;

    logic [BANK_WAY_NUM-1:0]                                    ram_valid;
    MTLB_TAG_TYPE  [BANK_WAY_NUM-1:0]                           tram_rdata;
    MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                           iram_rdata;
    MTLB_UTAG_TYPE                                              uram_rdata;

    logic [BANK_WAY_NUM-1:0]                                    tag_hit;
    logic [BANK_WAY_NUM-1:0]                                    inv_hit;
    logic [BANK_WAY_NUM-1:0]                                    tag_hit_err_masked;
    logic [BANK_WAY_NUM-1:0]                                    inv_hit_err_masked;

    logic [BANK_WAY_NUM-1:0] [1:0]                              err_o_nxt;
//}}}

//=== MainCode === {{{
    assign multi_hit_fault_o = 'd0;
//=== req decode {{{
    assign req_valid        = bank_req_valid_i;
    assign req              = bank_req_i;
    assign bank_req_ready_o = req_ready;
    assign req_ready        = bank_req_ready_i;

    assign req_is_lkp       = req.typ==3'b001;
    assign req_is_upd       = req.typ==3'b010;
    assign req_is_inv       = req.typ==3'b100;

    assign req_do           = req_valid & req_ready;
//}}}

//=== ram rd data in{{{
    assign ram_valid        = ram_valid_i;
    assign tram_rdata       = tram_rdata_i;
    assign iram_rdata       = iram_rdata_i;
    assign uram_rdata       = uram_rdata_i;
//}}}

//=== req pipe out {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            bank_req_valid_o <= 1'b0;
            bank_req_o       <= 'd0;
        end
        else begin
            if(bank_req_valid_o & ~bank_req_ready_i)
                bank_req_valid_o <= 1'b1;
            else begin
                if(req_do) begin
                    bank_req_valid_o <= 1'b1;
                    bank_req_o       <= req;
                end
                else begin
                    bank_req_valid_o <= 1'b0;
                end
            end
        end
    end
//}}}

//=== tag match {{{
genvar i;
generate
    for(i=0; i<BANK_WAY_NUM; i++) begin : tag_match_gen
        iommu_atd_dtc_mtlb_tag_hit #(
        /*parameter */ .CACHE_TYPE                 (CACHE_TYPE         ), // = 0, // 0:DDTC, 1:PDTC
        /*parameter */ .BANK_TYPE                  (BANK_TYPE          ), // = 2'b00,
        /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE   ), // = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
        /*parameter type         */ .BANK_REQ_TYPE              (BANK_REQ_TYPE      ), // = iommu_atd_cache_pkg::ddtc_bank_req_t.
        /*parameter type         */ .MTLB_TAG_TYPE              (MTLB_TAG_TYPE      ), // = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
        /*parameter type         */ .MTLB_ITAG_TYPE             (MTLB_ITAG_TYPE     ), // = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
        /*parameter */ .SPARE_PARAM                (1'b0               )  // = 1'b0
        ) U_tag_hit(
        /*input  BANK_REQ_TYPE                              */  .req_i              (req                ),
        /*input  MTLB_TAG_TYPE                              */  .tag_i              (tram_rdata      [i]),
        /*input  MTLB_ITAG_TYPE                             */  .itag_i             (iram_rdata      [i]),
        /*output logic                                      */  .tag_hit_o          (tag_hit         [i]),
        /*output logic                                      */  .inv_hit_o          (inv_hit         [i]),
        /*input  logic                                      */  .spare_in           (1'b0               ) 
        );
        assign tag_hit_err_masked[i] = tag_hit[i] & (err_o_nxt[i]=='d0);
        assign inv_hit_err_masked[i] = inv_hit[i] & (err_o_nxt[i]=='d0);
    end
endgenerate
//}}}

//=== tag pipe out {{{
genvar k;
generate
    for(k=0; k<BANK_WAY_NUM; k++) begin : err_merge_gen
        assign err_o_nxt[k] = req_is_lkp ? (tram_rdata_err_i[k] | iram_rdata_err_i[k] | dram_rdata_err_i[k]) :
                              req_is_inv ? (tram_rdata_err_i[k] | iram_rdata_err_i[k]                      ) :
                              req_is_upd ? (tram_rdata_err_i[k]                                            ) :
                                           err_o[k];
    end
endgenerate

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            tag_valid_o <= 'd0;
            tag_hit_o   <= 'd0;
            tag_o       <= 'd0;
            itag_o      <= 'd0;
            utag_o      <= 'd0;
            err_o       <= 'd0;
        end
        else begin
            if(bank_req_valid_o & ~bank_req_ready_i)
                tag_valid_o <= tag_valid_o;
            else begin
                if(req_do) begin
                    tag_valid_o <= ram_valid;
                    tag_hit_o   <= (req_is_lkp | req_is_upd) ? tag_hit_err_masked :
                                    req_is_inv               ? inv_hit_err_masked : 'd0;
                    tag_o       <= tram_rdata;
                    itag_o      <= iram_rdata;
                    utag_o      <= uram_rdata;
                    err_o       <= err_o_nxt;
                end
                else begin
                    tag_valid_o <= 1'b0;
                end
            end
        end
    end
//}}}

//}}}

endmodule//}}}



module iommu_atd_dtc_mtlb_bank_wr2 #( //{{{
    parameter  CACHE_TYPE                          = 0, // 0:DDTC, 1:PDTC
    parameter  BANK_TYPE                           = 2'b00,
    parameter type          BANK_REQ_TYPE                       = iommu_atd_cache_pkg::ddtc_bank_req_t,
    parameter type          INVALID_REQ_TYPE                    = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    parameter type          LOOKUP_REQ_TYPE                     = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE                     = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE                     = iommu_atd_cache_pkg::ddtc_update_req_t,
    parameter type          BANK_LKP_REQ_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    parameter type          BANK_LKP_ACK_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    parameter type          BANK_UPD_REQ_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_upd_req_t,
    parameter type          BANK_UPD_ACK_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_upd_ack_t,
    parameter type          BANK_INV_REQ_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_inv_req_t,
    parameter type          BANK_INV_ACK_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_inv_ack_t,
    parameter  CABIN_LKP_IDX_WIDTH                 = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    parameter  CABIN_UPD_IDX_WIDTH                 = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH,
    parameter  CABIN_INV_IDX_WIDTH                 = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH,
    parameter  BANK_IDX_WIDTH                      = iommu_atd_cache_pkg::DDTC_MAX_BANK_IDX_WIDTH,
    parameter  BANK_SET_IDX_WIDTH                  = iommu_atd_cache_pkg::DDTC_MAX_BANK_SET_IDX_WIDTH,
    parameter  BANK_WAY_IDX_WIDTH                  = iommu_atd_cache_pkg::DDTC_MAX_BANK_WAY_IDX_WIDTH,
    parameter  CABIN_LKP_NUM                       = 2**CABIN_LKP_IDX_WIDTH,
    parameter  CABIN_UPD_NUM                       = 2**CABIN_UPD_IDX_WIDTH,
    parameter  CABIN_INV_NUM                       = 2**CABIN_INV_IDX_WIDTH,
    parameter  BANK_NUM                            = 2**BANK_IDX_WIDTH,
    parameter  BANK_SET_NUM                        = 2**BANK_SET_IDX_WIDTH,
    parameter  BANK_WAY_NUM                        = 2**BANK_WAY_IDX_WIDTH,
    parameter type          MTLB_TAG_TYPE                       = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    parameter type          MTLB_ITAG_TYPE                      = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    parameter type          MTLB_UTAG_TYPE                      = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    parameter type          MTLB_DAT_TYPE                       = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_l,
    parameter  SPARE_PARAM                         = 0
)(                                                              
//{{{ IO
    input  logic                                                clk,
    input  logic                                                rstn,
    // REQ INPUT
    input  logic                                                bank_req_valid_i,
    output logic                                                bank_req_ready_o,
    input  BANK_REQ_TYPE                                        bank_req_i,
    // REQ PIPE OUT                                             
    output logic                                                bank_req_valid_o,
    input  logic                                                bank_req_ready_i,
    output BANK_REQ_TYPE                                        bank_req_o,
    // TAG INPUT
    input  logic [BANK_WAY_NUM-1:0]                             tag_valid_i,
    input  logic [BANK_WAY_NUM-1:0]                             tag_hit_i,
    input  MTLB_TAG_TYPE  [BANK_WAY_NUM-1:0]                    tag_i,
    input  MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                    itag_i,
    input  MTLB_UTAG_TYPE                                       utag_i,
    input  logic [BANK_WAY_NUM-1:0] [1:0]                       err_i,
    // TAG PIPE OUT for LKP_ACK
    output logic                                                tag_valid_o,
    output logic                                                tag_hit_o,
    output MTLB_TAG_TYPE                                        tag_o,
    output MTLB_ITAG_TYPE                                       itag_o,
    // TAG_RAM                                                  
    output logic [BANK_WAY_NUM-1:0]                             tram_cs_o,
    output logic [BANK_WAY_NUM-1:0]                             tram_wr_o,
    output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]    tram_addr_o,
    output MTLB_TAG_TYPE [BANK_WAY_NUM-1:0]                     tram_wdata_o,
    output logic [BANK_WAY_NUM-1:0]                             iram_cs_o,
    output logic [BANK_WAY_NUM-1:0]                             iram_wr_o,
    output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]    iram_addr_o,
    output MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                    iram_wdata_o,
    output logic                                                uram_cs_o,
    output logic                                                uram_wr_o,
    output logic [BANK_SET_IDX_WIDTH-1:0]                       uram_addr_o,
    output MTLB_UTAG_TYPE                                       uram_wdata_o,
    // DATA_RAM                                              
    output logic [BANK_WAY_NUM-1:0]                             dram_cs_o,
    output logic [BANK_WAY_NUM-1:0]                             dram_wr_o,
    output logic [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]    dram_addr_o,
    output MTLB_DAT_TYPE [BANK_WAY_NUM-1:0]                     dram_wdata_o,
    //
    input  logic                                                spare_in
//}}}
);
//=== Declare === {{{
    logic                                                       req_is_lkp, req_is_upd, req_is_inv;

    logic                                                       req_valid;
    logic                                                       req_ready;
    BANK_REQ_TYPE                                               req;
    logic [BANK_WAY_NUM-2:0]                                    req_plru_list;

    logic [BANK_WAY_NUM-1:0]                                    tag_valid;
    logic [BANK_WAY_NUM-1:0]                                    tag_hit;
    MTLB_TAG_TYPE  [BANK_WAY_NUM-1:0]                           tag;
    MTLB_ITAG_TYPE [BANK_WAY_NUM-1:0]                           itag;
    MTLB_UTAG_TYPE                                              utag;

    logic                                                       req_do;
    logic [BANK_WAY_NUM-1:0]                                    upd_sel;
    logic [BANK_WAY_NUM-1:0]                                    plru_sel;
    logic [BANK_WAY_NUM-2:0]                                    plru_list;
    logic [$bits(MTLB_UTAG_TYPE):0]                             plru_list_maxwidth;
    logic [BANK_WAY_NUM-1:0]                                    plru_tag_hit;

    logic [BANK_WAY_NUM-1:0]                                    lkp_dram_cs_muxed;

    logic                                                       tag_valid_muxed;
    logic                                                       tag_hit_muxed;
    MTLB_TAG_TYPE                                               tag_muxed;
    MTLB_ITAG_TYPE                                              itag_muxed;

    logic                                                       do_ram_write;
    logic                                                       do_ram_write_cnt;
    logic                                                       do_ram_read;
    logic                                                       do_ram_read_cnt;

    logic [BANK_WAY_NUM-1:0]                                    upd_do_write_upd_sel_ff;
    logic [BANK_WAY_NUM-2:0]                                    upd_do_write_plru_list_ff;
    logic [BANK_SET_IDX_WIDTH-1:0]                              upd_do_write_set_idx_ff;
    logic                                                       upd_do_write_ff;

    logic [BANK_WAY_NUM-1:0]                                    inv_tag_hit;
    logic [BANK_WAY_NUM-1:0]                                    prefetched_tag_hit;

    MTLB_DAT_TYPE                                               dram_wdat_int;

    logic [BANK_WAY_NUM-1:0]                                    any_ecc_err;
    logic                                                       do_ram_write_upd;
//}}}

//=== MainCode === {{{
//=== req decode {{{
    assign req_valid        = bank_req_valid_i;
    assign req              = bank_req_i;
    //assign bank_req_ready_o = req_ready;
    assign req_ready        = bank_req_ready_i;

    assign req_is_lkp       = req.typ==3'b001;
    assign req_is_upd       = req.typ==3'b010;
    assign req_is_inv       = req.typ==3'b100;

    assign req_do           = req_valid & req_ready;

    assign req_plru_list    = (upd_do_write_ff & (upd_do_write_set_idx_ff==req.upd.bank_set_idx[BANK_SET_IDX_WIDTH-1:0])) ? upd_do_write_plru_list_ff :
                                                                                                                            utag_i.plru_list[BANK_WAY_NUM-2:0];

    assign tag_valid        = tag_valid_i;
    assign tag_hit          = tag_hit_i;
    assign tag              = tag_i;
    assign itag             = itag_i;
    assign utag             = utag_i;

genvar ith;
generate
    for(ith=0; ith<BANK_WAY_NUM; ith++) begin : real_tag_hit_when_inv_gen
        assign inv_tag_hit[ith] = tag_hit[ith] & tag[ith].valid;
    end
endgenerate

genvar ecc;
generate
    for(ecc=0; ecc<BANK_WAY_NUM; ecc++) begin
        assign any_ecc_err[ecc] = |err_i[ecc];
    end
endgenerate

genvar pth;
generate
    for(pth=0; pth<BANK_WAY_NUM; pth++) begin : real_tag_hit_with_pretfetced
        assign prefetched_tag_hit[pth] = tag_hit[pth] & tag[pth].prefetched;
    end
endgenerate

    assign do_ram_write     = (req_do & req_is_inv & |inv_tag_hit       ) | // INV&hit
                              (req_do & req_is_upd & ~(|tag_hit)        ) | // UPF&!hit
                              (req_do & req_is_lkp & |prefetched_tag_hit) | // LKP&hit&is_prefetched
                              (req_do & (any_ecc_err!='d0));                // ecc err

    assign do_ram_write_upd =  req_do & req_is_upd & ~(|tag_hit);           // when req_is_upd, any_ecc_err[req.upd.bank_set_idx[BANK_SET_IDX_WIDTH-1:0]] may 1
                                                                            // means the entry use to store this new upd is broken
                                                                            // still store this new upd to this entry

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            do_ram_write_cnt <= 1'b0;
        else if(do_ram_write)
            do_ram_write_cnt <= do_ram_write_cnt + 'd1;
    end

    assign do_ram_read      = req_do & req_is_lkp & |tag_hit;
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            do_ram_read_cnt <= 1'b0;
        else if(do_ram_read)
            do_ram_read_cnt <= do_ram_read_cnt + 'd1;
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            upd_do_write_upd_sel_ff   <= 'd0;
            upd_do_write_plru_list_ff <= 'd0;
            upd_do_write_set_idx_ff   <= 'd0;
            upd_do_write_ff           <= 'b0;
        end
        else begin
            if(req_do & ~req_is_upd) begin
                upd_do_write_ff <= 1'b0;
            end
            else if(do_ram_write_upd & ~do_ram_write_cnt) begin   // if the previous is upd, store the plru for the upd immediately after this req
                upd_do_write_ff           <= 1'b1;
                upd_do_write_set_idx_ff   <= req.upd.bank_set_idx[BANK_SET_IDX_WIDTH-1:0];
                upd_do_write_plru_list_ff <= plru_list;
                upd_do_write_upd_sel_ff   <= upd_sel;
            end
        end
    end

    
//}}}

//=== TAG RAM {{{
    assign tram_cs_o = (req_do & req_is_lkp) ? ((do_ram_write & ~do_ram_write_cnt) ? {~prefetched_tag_hit & ~any_ecc_err}   :   // wr prefetech_hit or err_entries
                                                                                     {BANK_WAY_NUM{1'b1}}
                                                )                                                                               :
                       (req_do & req_is_inv) ? ((do_ram_write & ~do_ram_write_cnt) ? {~inv_tag_hit & ~any_ecc_err}          :   // wr inv_hit or err_entries
                                                                                     {BANK_WAY_NUM{1'b1}}
                                                )                                                                               :
                       (req_do & req_is_upd) ? ((do_ram_write & ~do_ram_write_cnt) ? {~upd_sel & ~any_ecc_err}              :   // wr upd_sel or err_entries
                                                                                     {BANK_WAY_NUM{1'b1}}
                                                )                                                                               :
                                               {BANK_WAY_NUM{1'b1}} ;

    assign tram_wr_o = 'd0;/*(req_do & req_is_lkp) ? ((do_ram_write & ~do_ram_write_cnt) ? {BANK_WAY_NUM{1'b0}}   :                   // wr prefetch_hit or err_entries
                                                                                     {BANK_WAY_NUM{1'b1}}
                                                )                                                                :
                       (req_do & req_is_inv) ? ((do_ram_write & ~do_ram_write_cnt) ? {BANK_WAY_NUM{1'b0}}   :                   // wr inv_hit or err_entries
                                                                                     {BANK_WAY_NUM{1'b1}}
                                                )                                                                :
                       (req_do & req_is_upd) ? ((do_ram_write & ~do_ram_write_cnt) ? {BANK_WAY_NUM{1'b0}}   :                   // wr upd_sel or err_entries
                                                                                     {BANK_WAY_NUM{1'b1}}
                                                )                                                                :
                                               {BANK_WAY_NUM{1'b1}} ;*/

genvar t;
generate
    for(t=0; t<BANK_WAY_NUM; t++) begin : tram_addr_wdata_gen
        assign tram_addr_o[t] = (req_do & req_is_lkp) ? req.lkp.bank_set_idx[BANK_SET_IDX_WIDTH-1:0]    :  // wr prefetch_hir or err_entries
                                (req_do & req_is_inv) ? req.inv.bank_set_idx[BANK_SET_IDX_WIDTH-1:0]    :  // wr inv_hit or err_entries
                                (req_do & req_is_upd) ? req.upd.bank_set_idx[BANK_SET_IDX_WIDTH-1:0]    :  // wr upd_sel or err_entries
                                                        'd0;
        if(CACHE_TYPE==0) begin : ddtc_tram_wdata_gen
            assign tram_wdata_o[t]= (req_do & req_is_lkp) ? {
                                                                any_ecc_err[t] ? 'd0 :                                          // wr err_entries
                                                                                 {tag[t].valid,     // valid                    // wr prefetcn_hit
                                                                                  1'b0,             // prefetched
                                                                                  tag[t].device_id  // device_id
                                                                                 }
                                                            }                                           :
                                    (req_do & req_is_inv) ? 'd0                                         :                       // wr inv_hit or err_entries
                                    (req_do & req_is_upd) ? {
                                                                upd_sel[t] ?     {1'b1,                     // valid            // store upd to upd_sel entry
                                                                                  req.upd.req.prefetched,   // prefetched
                                                                                  req.upd.req.device_id     // device_id
                                                                                 } :
                                                                                 'd0                                            // wr err(without upd_sel valid) entries
                                                            }                                           :
                                                            'd0 ;
        end
        else begin : pdtc_tram_wdata_gen
            assign tram_wdata_o[t]= (req_do & req_is_lkp) ? {
                                                                any_ecc_err[t] ? 'd0 :                                          // wr err_entries
                                                                                 {tag[t].valid,     // valid                    // wr prefetch_hit
                                                                                  1'b0,             // prefetched
                                                                                  tag[t].device_id, // device_id
                                                                                  tag[t].process_id // process_id
                                                                                 }
                                                            }                                           :
                                    (req_do & req_is_inv) ? 'd0                                         :                       // wr inv_hit or err_entries
                                    (req_do & req_is_upd) ? {
                                                                upd_sel[t] ?     {1'b1,                     // valid            // store upd to upd_sel entry
                                                                                  req.upd.req.prefetched,   // prefetched
                                                                                  req.upd.req.device_id,    // device_id
                                                                                  req.upd.req.process_id    // process_id
                                                                                 } :
                                                                                 'd0                                            // wr err(without upd_sel valid) entries

                                                            }                                           :
                                                            'd0 ;
        end
    end
endgenerate
//}}}

//=== ITAG RAM {{{
    assign iram_cs_o = (req_do & req_is_lkp) ? ((do_ram_write & ~do_ram_write_cnt) ? ~any_ecc_err                   :           // wr err_entries
                                                                                     {BANK_WAY_NUM{1'b1}}
                                                )                                                                        :
                       (req_do & req_is_inv) ? ((do_ram_write & ~do_ram_write_cnt) ? {inv_tag_hit & ~any_ecc_err}   :           // wr inv_hit or err_entries
                                                                                     {BANK_WAY_NUM{1'b1}}
                                                )                                                                        :
                       (req_do & req_is_upd) ? ((do_ram_write & ~do_ram_write_cnt) ? {~upd_sel    & ~any_ecc_err}   :           // wr upd_sel or err_entries
                                                                                     {BANK_WAY_NUM{1'b1}}
                                                )                                                                        :
                                               {BANK_WAY_NUM{1'b1}} ;

    assign iram_wr_o = 'd0;/*(req_do & req_is_lkp) ? {BANK_WAY_NUM{1'b1}}                                         :   // nop
                       (req_do & req_is_inv) ? {BANK_WAY_NUM{1'b1}}                                         :   // nop
                       (req_do & req_is_upd) ? ((do_ram_write & ~do_ram_write_cnt) ? {BANK_WAY_NUM{1'b0}}:
                                                                                     {BANK_WAY_NUM{1'b1}}
                                               )                                                            :   // wr
                                               {BANK_WAY_NUM{1'b1}} ;*/

genvar i;
generate
    for(i=0; i<BANK_WAY_NUM; i++) begin : iram_addr_wdata_gen
        assign iram_addr_o[i] = (req_do & req_is_lkp) ? req.lkp.bank_set_idx[BANK_SET_IDX_WIDTH-1:0]    :   // wr err_entries
                                (req_do & req_is_inv) ? req.inv.bank_set_idx[BANK_SET_IDX_WIDTH-1:0]    :   // wr inv_hit or err_entries
                                (req_do & req_is_upd) ? req.upd.bank_set_idx[BANK_SET_IDX_WIDTH-1:0]    :   // wr upd_sel or err_entries
                                                        'd0;
        
        assign iram_wdata_o[i]= (req_do & req_is_lkp) ? 'd0                                             :   // wr err_entries
                                (req_do & req_is_inv) ? 'd0                                             :   // wr inv_hit or err_entries
                                (req_do & req_is_upd) ? {                                                   // no itag now, so always write 0
                                                            upd_sel[i] ? 'd0 :
                                                                         'd0
                                                        }                                               :
                                                        'd0;
    end
endgenerate
//}}}

//=== UTAG RAM {{{
    assign uram_cs_o = (req_do & req_is_lkp) ? {BANK_WAY_NUM{1'b1}}                             :   // nop  // by now, lookup hit will not update plru, as the hitted entry will updated to MicroTLB
                       (req_do & req_is_inv) ? {BANK_WAY_NUM{1'b1}}                             :   // nop
                       (req_do & req_is_upd) ? ((do_ram_write & ~do_ram_write_cnt) ? 1'b0       :
                                                                {BANK_WAY_NUM{1'b1}}
                                               )                                                :   // wr
                                               {BANK_WAY_NUM{1'b1}} ;

    assign uram_wr_o = (req_do & req_is_lkp) ? {BANK_WAY_NUM{1'b1}} :   // nop
                       (req_do & req_is_inv) ? {BANK_WAY_NUM{1'b1}} :   // nop
                       (req_do & req_is_upd) ? ((do_ram_write & ~do_ram_write_cnt) ? 1'b0       :
                                                                {BANK_WAY_NUM{1'b1}}
                                               )                                                :   // wr
                                               {BANK_WAY_NUM{1'b1}} ;

    assign uram_addr_o = (req_do & req_is_lkp) ? 'd0                                          :  // nop
                         (req_do & req_is_inv) ? 'd0                                          :  // nop
                         (req_do & req_is_upd) ? req.upd.bank_set_idx[BANK_SET_IDX_WIDTH-1:0] :  // wr
                                                 'd0;

    assign plru_list_maxwidth = {($bits(MTLB_UTAG_TYPE)-BANK_WAY_NUM+2)'(0), plru_list};
    assign uram_wdata_o= (req_do & req_is_lkp) ? 'd0                                          :  // nop
                         (req_do & req_is_inv) ? 'd0                                          :  // nop
                         (req_do & req_is_upd) ? plru_list_maxwidth[$bits(MTLB_UTAG_TYPE)-1:0]:  // wr
                                                 'd0;
//}}}

//=== DATA RAM {{{
    always@(*) begin
        lkp_dram_cs_muxed = 'd0;
        for(int unsigned ld=0; ld<BANK_WAY_NUM; ld++) begin
            if(tag_hit[ld])
                lkp_dram_cs_muxed = {{(BANK_WAY_NUM-1){1'b0}}, 1'b1} << ld;
        end
    end
    assign dram_cs_o = (req_do & req_is_lkp) ? ((do_ram_read & ~do_ram_read_cnt)    ? {~lkp_dram_cs_muxed & ~any_ecc_err}   :       // rd tag_hit ane wr err_entries
                                                (do_ram_write & ~do_ram_write_cnt)  ? ~any_ecc_err                          :
                                                                                      {BANK_WAY_NUM{1'b1}}
                                                )                                                                               :
                       (req_do & req_is_inv) ? ((do_ram_write & ~do_ram_write_cnt)  ? {~inv_tag_hit & ~any_ecc_err}         :       // wr inv_hit or err_entries
                                                                                      {BANK_WAY_NUM{1'b1}}
                                                )                                                                               :
                       (req_do & req_is_upd) ? ((do_ram_write & ~do_ram_write_cnt)  ? {~upd_sel & ~any_ecc_err}             :       // wr upd_sel or err_entries
                                                                                      {BANK_WAY_NUM{1'b1}}
                                                )                                                                               :
                                               {BANK_WAY_NUM{1'b1}} ;

    assign dram_wr_o = (req_do & req_is_lkp) ? (lkp_dram_cs_muxed | ~any_ecc_err)   :   // rd tag_hit and wr err_entries
                       (req_do & req_is_inv) ? {~inv_tag_hit & ~any_ecc_err}        :   // wr inv_hit or err_entries
                       (req_do & req_is_upd) ? {~upd_sel & ~any_ecc_err}            :   // wr upd_sel or err_entries
                                               {BANK_WAY_NUM{1'b1}} ;

generate
    if(BANK_TYPE=='d0) begin
        if(CACHE_TYPE==0) begin
            always@(*) begin
                dram_wdat_int.msi_addr_pattern  = req.upd.req.msi_addr_pattern;
                dram_wdat_int.msi_addr_mask     = req.upd.req.msi_addr_mask;
                dram_wdat_int.msipip_mode       = req.upd.req.msipip_mode;
                dram_wdat_int.msipip_ppn        = req.upd.req.msipip_ppn;
                dram_wdat_int.fsc_mode          = req.upd.req.fsc_mode;
                dram_wdat_int.fsc_ppn           = req.upd.req.fsc_ppn;
                dram_wdat_int.PSCID             = req.upd.req.PSCID;
                dram_wdat_int.S2MODE            = req.upd.req.S2MODE;
                dram_wdat_int.GSCID             = req.upd.req.GSCID;
                dram_wdat_int.S2PPN             = req.upd.req.S2PPN;
                dram_wdat_int.SXL               = req.upd.req.SXL;
                dram_wdat_int.SBE               = req.upd.req.SBE;
                dram_wdat_int.DPE               = req.upd.req.DPE;
                dram_wdat_int.SADE              = req.upd.req.SADE;
                dram_wdat_int.GADE              = req.upd.req.GADE;
                dram_wdat_int.PRPR              = req.upd.req.PRPR;
                dram_wdat_int.PDTV              = req.upd.req.PDTV;
                dram_wdat_int.DTF               = req.upd.req.DTF;
                dram_wdat_int.T2GPA             = req.upd.req.T2GPA;
                dram_wdat_int.EN_PRI            = req.upd.req.EN_PRI;
                dram_wdat_int.EN_ATS            = req.upd.req.EN_ATS;
                dram_wdat_int.V                 = req.upd.req.V;
            end
        end
        else begin
            always@(*) begin
                dram_wdat_int.fsc_mode          = req.upd.req.fsc_mode;
                dram_wdat_int.fsc_ppn           = req.upd.req.fsc_ppn ;
                dram_wdat_int.PSCID             = req.upd.req.PSCID   ;
                dram_wdat_int.SUM               = req.upd.req.SUM     ;
                dram_wdat_int.ENS               = req.upd.req.ENS     ;
                dram_wdat_int.V                 = req.upd.req.V       ;
            end
        end
    end
    else begin
        always@(*) begin
            dram_wdat_int.fsc_ppn               = req.upd.req.fsc_ppn;
            dram_wdat_int.V                     = req.upd.req.V;
        end
    end
endgenerate

genvar d;
generate
    for(d=0; d<BANK_WAY_NUM; d++) begin : dram_addr_wdata_gen
        assign dram_addr_o[d] = (req_do & req_is_lkp) ? req.lkp.bank_set_idx[BANK_SET_IDX_WIDTH-1:0]    :  // rd tag_hit and wr err_entries
                                (req_do & req_is_inv) ? req.inv.bank_set_idx[BANK_SET_IDX_WIDTH-1:0]    :  // wr inv_hit or err_entries
                                (req_do & req_is_upd) ? req.upd.bank_set_idx[BANK_SET_IDX_WIDTH-1:0]    :  // wr upd_sel or err_entries
                                                        'd0;
        
        assign dram_wdata_o[d]= (req_do & req_is_lkp) ? 'd0                                             :  // wr err_entries with 0
                                (req_do & req_is_inv) ? 'd0                                             :  // same as above
                                (req_do & req_is_upd) ? {upd_sel[d] ? dram_wdat_int : 'd0}              :  // wr upd_sel with upd, wr err entry with 0
                                                        'd0;
    end
endgenerate

//}}}

//=== req pipe out {{{
//                        UPD/INV_HIT            LKP_HIT                 NA                LKP_HIT&ERR
//                 __    __    __    __    __    __    __    __    __    __    __    __    __    __    __    __    __    __
// clk          __|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|
//                       ___________             ___________             _____             ___________
// bank_req_valid_i ____|           |___________|           |___________|     |___________|           |____________________
//                   ......................................................................................................
// bank_req_ready_i :
//                       ___________             ___________             _____             ___________
// req_do           ____|           |___________|           |__________ |     |___________|           |____________________
//                       ___________                                                       ___________
// do_ram_write     ____|           |_____________________________________________________|           |____________________
//                             _____                                                             _____
// do_ram_write_cnt __________|     |___________________________________________________________|     |____________________
//                                               ___________                               ___________
// do_ram_read      ____________________________|           |_____________________________|           |____________________
//                                                     _____                                     _____
// do_ram_read_cnt  __________________________________|     |___________________________________|     |____________________
//                             _____                   _____                                     _____
// bank_req_valid_o __________/     \_________________/     \___________________________________/     \____________________
//                  ..........       ...........       ...................................       ..........................
// bank_req_ready_o           \_____/           \_____/                                   \ ____/
//
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            bank_req_valid_o <= 1'b0;
            bank_req_o       <= 'd0;
        end
        else begin
            if(bank_req_valid_o & ~bank_req_ready_i)
                bank_req_valid_o <= 1'b1;
            else begin
                if(req_do & ~(
                                (do_ram_write & do_ram_write_cnt) |
                                (do_ram_read  &  do_ram_read_cnt)
                              )
                    ) begin
                    bank_req_valid_o <= 1'b1;
                    bank_req_o       <= req;
                end
                else begin
                    bank_req_valid_o <= 1'b0;
                end
            end
        end
    end
//}}}

//=== tag pipe out {{{
    assign tag_valid_muxed = |tag_valid;
    assign tag_hit_muxed   = |tag_hit;
    always@(*) begin
        tag_muxed = 'd0;
        itag_muxed = 'd0;
        for(int unsigned gto=0; gto<BANK_WAY_NUM; gto++) begin
            if(tag_hit[gto]) begin
                tag_muxed  = tag[gto];
                itag_muxed = itag[gto];
            end
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            tag_valid_o <= 'd0;
            tag_hit_o   <= 'd0;
            tag_o       <= 'd0;
            itag_o      <= 'd0;
        end
        else begin
            if(bank_req_valid_o & ~bank_req_ready_i)
                tag_valid_o <= 1'b1;
            else begin
                if(req_do & (~do_ram_write | ~do_ram_write_cnt) & (~do_ram_read | ~do_ram_read_cnt)) begin
                    tag_valid_o <= tag_valid_muxed;
                    tag_hit_o   <= tag_hit_muxed;
                    tag_o       <= tag_muxed;
                    itag_o      <= itag_muxed;
                end
                else begin
                    tag_valid_o <= 1'b0;
                end
            end
        end
    end
//}}}

//=== READY GEN {{{
    assign bank_req_ready_o = (do_ram_write & ~do_ram_write_cnt) ? 1'b0 :
                              (do_ram_read  & ~do_ram_read_cnt)  ? 1'b0 :
                              req_ready;
//}}}

//=== update plru {{{
    always@(*) begin
        upd_sel = plru_sel;
        for(int unsigned pi=0; pi<BANK_WAY_NUM; pi++) begin
            if(tag[pi].valid==1'b0 & ~(upd_do_write_ff & upd_do_write_upd_sel_ff[pi]))
                upd_sel = {{(BANK_WAY_NUM-1){1'b0}}, 1'b1} << pi;
        end
    end
    assign plru_tag_hit = (|tag_hit) ? 'd0 : upd_sel;
    iommu_acd_mtlb_plru #(
    /*parameter */ .WIDTH      (BANK_WAY_NUM   )  // = 8
    ) U_plru(
    /*input  logic [WIDTH-1:0]          */  .lu_hit         (plru_tag_hit           ),
    /*input  logic                      */  .lu_hit_valid   (req_do & req_is_upd    ),
    /*input  logic [WIDTH-2:0]          */  .plru_list_ori  (req_plru_list          ),
    /*output logic [WIDTH-1:0]          */  .replace_en     (plru_sel               ),
    /*output logic [WIDTH-2:0]          */  .plru_list_new  (plru_list              )
    );

//}}}

//}}}

endmodule
//}}}



module iommu_atd_dtc_mtlb_bank_ack3 #( //{{{
    parameter  CACHE_TYPE                          = 0, // 0:DDTC, 1:PDTC
    parameter  BANK_TYPE                           = 2'b00,
    parameter type          BANK_REQ_TYPE                       = iommu_atd_cache_pkg::ddtc_bank_req_t,
    parameter type          INVALID_REQ_TYPE                    = iommu_atd_cache_pkg::INVALID_REQ_TYPE,
    parameter type          LOOKUP_REQ_TYPE                     = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE                     = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE                     = iommu_atd_cache_pkg::ddtc_update_req_t,
    parameter type          BANK_LKP_REQ_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    parameter type          BANK_LKP_ACK_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    parameter type          BANK_UPD_REQ_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_upd_req_t,
    parameter type          BANK_UPD_ACK_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_upd_ack_t,
    parameter type          BANK_INV_REQ_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_inv_req_t,
    parameter type          BANK_INV_ACK_TYPE                   = iommu_atd_cache_pkg::ddtc_bank_inv_ack_t,
    parameter  CABIN_LKP_IDX_WIDTH                 = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    parameter  CABIN_UPD_IDX_WIDTH                 = iommu_atd_cache_pkg::DDTC_CABIN_UPD_IDX_WIDTH,
    parameter  CABIN_INV_IDX_WIDTH                 = iommu_atd_cache_pkg::DDTC_CABIN_INV_IDX_WIDTH,
    parameter  BANK_IDX_WIDTH                      = iommu_atd_cache_pkg::DDTC_MAX_BANK_IDX_WIDTH,
    parameter  BANK_SET_IDX_WIDTH                  = iommu_atd_cache_pkg::DDTC_MAX_BANK_SET_IDX_WIDTH,
    parameter  BANK_WAY_IDX_WIDTH                  = iommu_atd_cache_pkg::DDTC_MAX_BANK_WAY_IDX_WIDTH,
    parameter  CABIN_LKP_NUM                       = 2**CABIN_LKP_IDX_WIDTH,
    parameter  CABIN_UPD_NUM                       = 2**CABIN_UPD_IDX_WIDTH,
    parameter  CABIN_INV_NUM                       = 2**CABIN_INV_IDX_WIDTH,
    parameter  BANK_NUM                            = 2**BANK_IDX_WIDTH,
    parameter  BANK_SET_NUM                        = 2**BANK_SET_IDX_WIDTH,
    parameter  BANK_WAY_NUM                        = 2**BANK_WAY_IDX_WIDTH,
    parameter type          MTLB_TAG_TYPE                       = iommu_atd_cache_pkg::ddtc_mtlb_tag_t,
    parameter type          MTLB_ITAG_TYPE                      = iommu_atd_cache_pkg::ddtc_mtlb_itag_t,
    parameter type          MTLB_UTAG_TYPE                      = iommu_atd_cache_pkg::ddtc_mtlb_utag_t,
    parameter type          MTLB_DAT_TYPE                       = iommu_atd_cache_pkg::ddtc_mtlb_dat_t_l,
    parameter  SPARE_PARAM                         = 0
)(                                                              
//{{{ IO
    input  logic                                                clk,
    input  logic                                                rstn,
    // REQ INPUT
    input  logic                                                bank_req_valid_i,
    output logic                                                bank_req_ready_o,
    input  BANK_REQ_TYPE                                        bank_req_i,
    // TAG INPUT
    input  logic                                                tag_valid_i,
    input  logic                                                tag_hit_i,
    input  MTLB_TAG_TYPE                                        tag_i,
    input  MTLB_ITAG_TYPE                                       itag_i,
    // RAM RDATA                                                
    input  logic [BANK_WAY_NUM-1:0]                             ram_valid_i,
    input  MTLB_DAT_TYPE  [BANK_WAY_NUM-1:0]                    dram_rdata_i,
    // LOOKUP ACK
    output logic [CABIN_LKP_NUM-1:0]                            lkp_ack_valid_o,
    output BANK_LKP_ACK_TYPE [CABIN_LKP_NUM-1:0]                lkp_ack_o,
    // UPDATE ACK                                               
    output logic [CABIN_UPD_NUM-1:0]                            upd_ack_valid_o,
    output BANK_UPD_ACK_TYPE [CABIN_UPD_NUM-1:0]                upd_ack_o,
    // INVALID ACK                                              
    output logic [CABIN_INV_NUM-1:0]                            inv_ack_valid_o,
    output BANK_INV_ACK_TYPE [CABIN_INV_NUM-1:0]                inv_ack_o,
    //
    input  logic                                                spare_in
//}}}
);
//=== Declare {{{
    logic                                                       req_is_lkp, req_is_upd, req_is_inv;

    logic                                                       req_valid;
    logic                                                       req_ready;
    BANK_REQ_TYPE                                               req;

    logic                                                       lkp_ack_valid;
    BANK_LKP_ACK_TYPE                                           lkp_ack;
    logic                                                       upd_ack_valid;
    BANK_UPD_ACK_TYPE                                           upd_ack;
    logic                                                       inv_ack_valid;
    BANK_INV_ACK_TYPE                                           inv_ack;

    MTLB_DAT_TYPE                                               dram_rdata_muxed;

    logic [CABIN_LKP_NUM-1:0]                                   req_lidx;
    logic [CABIN_UPD_NUM-1:0]                                   req_uidx;
    logic [CABIN_INV_NUM-1:0]                                   req_iidx;

//}}}



//=== MainCode {{{
//=== req decode {{{
    assign req_valid        = bank_req_valid_i;
    assign req              = bank_req_i;
    assign bank_req_ready_o = req_ready;
    assign req_ready        = 1'b1;

    assign req_is_lkp       = req.typ==3'b001;
    assign req_is_upd       = req.typ==3'b010;
    assign req_is_inv       = req.typ==3'b100;

    assign req_do           = req_valid & req_ready;

//}}}

//=== LKP ACK {{{
    always@(*) begin
        dram_rdata_muxed = 'd0;
        for(int unsigned dr=0; dr<BANK_WAY_NUM; dr++) begin
            if(ram_valid_i[dr])
                dram_rdata_muxed = dram_rdata_i[dr];
        end
    end

generate
    if(BANK_TYPE=='d0) begin: l0_lkp_ack_gen
        if(CACHE_TYPE==0) begin : ddtc_lkp_ack_gen
            always@(posedge clk or negedge rstn) begin
                if(~rstn) begin
                    lkp_ack_valid <= 1'b0;
                    lkp_ack       <= 'd0;
                    req_lidx      <= 'd0;
                end
                else begin
                    if(bank_req_valid_i & req_is_lkp) begin
                        lkp_ack_valid               <= 1'b1                             ;
                        req_lidx                    <= req.lidx                         ;
                        lkp_ack.hit                 <= tag_hit_i                        ;
                        lkp_ack.ack.idx             <= {1'b0, req.lkp.req.idx}          ;
                        lkp_ack.ack.lvl             <= 2'(BANK_TYPE)                    ;
                        lkp_ack.ack.prefetched      <= tag_i.prefetched                 ;
                        lkp_ack.ack.device_id       <= tag_i.device_id                  ;
                        lkp_ack.ack.msi_addr_pattern<= dram_rdata_muxed.msi_addr_pattern;
                        lkp_ack.ack.msi_addr_mask   <= dram_rdata_muxed.msi_addr_mask   ;
                        lkp_ack.ack.msipip_mode     <= dram_rdata_muxed.msipip_mode     ;
                        lkp_ack.ack.msipip_ppn      <= dram_rdata_muxed.msipip_ppn      ;
                        lkp_ack.ack.fsc_mode        <= dram_rdata_muxed.fsc_mode        ;
                        lkp_ack.ack.fsc_ppn         <= dram_rdata_muxed.fsc_ppn         ;
                        lkp_ack.ack.PSCID           <= dram_rdata_muxed.PSCID           ;
                        lkp_ack.ack.S2MODE          <= dram_rdata_muxed.S2MODE          ;
                        lkp_ack.ack.GSCID           <= dram_rdata_muxed.GSCID           ;
                        lkp_ack.ack.S2PPN           <= dram_rdata_muxed.S2PPN           ;
                        lkp_ack.ack.SXL             <= dram_rdata_muxed.SXL             ;
                        lkp_ack.ack.SBE             <= dram_rdata_muxed.SBE             ;
                        lkp_ack.ack.DPE             <= dram_rdata_muxed.DPE             ;
                        lkp_ack.ack.SADE            <= dram_rdata_muxed.SADE            ;
                        lkp_ack.ack.GADE            <= dram_rdata_muxed.GADE            ;
                        lkp_ack.ack.PRPR            <= dram_rdata_muxed.PRPR            ;
                        lkp_ack.ack.PDTV            <= dram_rdata_muxed.PDTV            ;
                        lkp_ack.ack.DTF             <= dram_rdata_muxed.DTF             ;
                        lkp_ack.ack.T2GPA           <= dram_rdata_muxed.T2GPA           ;
                        lkp_ack.ack.EN_PRI          <= dram_rdata_muxed.EN_PRI          ;
                        lkp_ack.ack.EN_ATS          <= dram_rdata_muxed.EN_ATS          ;
                        lkp_ack.ack.V               <= dram_rdata_muxed.V               ;
                    end
                    else begin
                        lkp_ack_valid                 <= 1'b0;
                    end
                end
            end
        end
        else begin : pdtc_lkp_ack_gen
            always@(posedge clk or negedge rstn) begin
                if(~rstn) begin
                    lkp_ack_valid <= 1'b0;
                    lkp_ack       <= 'd0;
                    req_lidx      <= 'd0;
                end
                else begin
                    if(bank_req_valid_i & req_is_lkp) begin
                        lkp_ack_valid               <= 1'b1                             ;
                        req_lidx                    <= req.lidx                         ;
                        lkp_ack.hit                 <= tag_hit_i                        ;
                        lkp_ack.ack.idx             <= {1'b0, req.lkp.req.idx}          ;
                        lkp_ack.ack.lvl             <= 2'(BANK_TYPE)                    ;
                        lkp_ack.ack.prefetched      <= tag_i.prefetched                 ;
                        lkp_ack.ack.device_id       <= tag_i.device_id                  ;
                        lkp_ack.ack.process_id      <= tag_i.process_id                 ;
                        lkp_ack.ack.fsc_mode        <= dram_rdata_muxed.fsc_mode        ;
                        lkp_ack.ack.fsc_ppn         <= dram_rdata_muxed.fsc_ppn         ;
                        lkp_ack.ack.PSCID           <= dram_rdata_muxed.PSCID           ;
                        lkp_ack.ack.SUM             <= dram_rdata_muxed.SUM             ;
                        lkp_ack.ack.ENS             <= dram_rdata_muxed.ENS             ;
                        lkp_ack.ack.V               <= dram_rdata_muxed.V               ;
                    end
                    else begin
                        lkp_ack_valid                 <= 1'b0;
                    end
                end
            end
        end
    end
    else begin : nonl0_lkp_ack_gen
        if(CACHE_TYPE==0) begin : ddtc_lkp_ack_gen
            always@(posedge clk or negedge rstn) begin
                if(~rstn) begin
                    lkp_ack_valid <= 1'b0;
                    lkp_ack       <= 'd0;
                    req_lidx      <= 'd0;
                end
                else begin
                    if(bank_req_valid_i & req_is_lkp) begin
                        lkp_ack_valid               <= 1'b1            ;
                        req_lidx                    <= req.lidx        ;
                        lkp_ack.hit                 <= tag_hit_i       ;
                        lkp_ack.ack.idx             <= {1'b0, req.lkp.req.idx} ;
                        lkp_ack.ack.lvl             <= 2'(BANK_TYPE)   ;
                        lkp_ack.ack.prefetched      <= tag_i.prefetched;
                        lkp_ack.ack.device_id       <= tag_i.device_id ;
                        lkp_ack.ack.msi_addr_pattern<= 'd0;
                        lkp_ack.ack.msi_addr_mask   <= 'd0;
                        lkp_ack.ack.msipip_mode     <= 'd0;
                        lkp_ack.ack.msipip_ppn      <= 'd0;
                        lkp_ack.ack.fsc_mode        <= 'd0;
                        lkp_ack.ack.fsc_ppn         <= dram_rdata_muxed.fsc_ppn;
                        lkp_ack.ack.PSCID           <= 'd0;
                        lkp_ack.ack.S2MODE          <= 'd0;
                        lkp_ack.ack.GSCID           <= 'd0;
                        lkp_ack.ack.S2PPN           <= 'd0;
                        lkp_ack.ack.SXL             <= 'd0;
                        lkp_ack.ack.SBE             <= 'd0;
                        lkp_ack.ack.DPE             <= 'd0;
                        lkp_ack.ack.SADE            <= 'd0;
                        lkp_ack.ack.GADE            <= 'd0;
                        lkp_ack.ack.PRPR            <= 'd0;
                        lkp_ack.ack.PDTV            <= 'd0;
                        lkp_ack.ack.DTF             <= 'd0;
                        lkp_ack.ack.T2GPA           <= 'd0;
                        lkp_ack.ack.EN_PRI          <= 'd0;
                        lkp_ack.ack.EN_ATS          <= 'd0;
                        lkp_ack.ack.V               <= dram_rdata_muxed.V;
                    end
                    else begin
                        lkp_ack_valid                 <= 1'b0;
                    end
                end
            end
        end
        else begin : pdtc_lkp_ack_gen
            always@(posedge clk or negedge rstn) begin
                if(~rstn) begin
                    lkp_ack_valid <= 1'b0;
                    lkp_ack       <= 'd0;
                    req_lidx      <= 'd0;
                end
                else begin
                    if(bank_req_valid_i & req_is_lkp) begin
                        lkp_ack_valid               <= 1'b1            ;
                        req_lidx                    <= req.lidx        ;
                        lkp_ack.hit                 <= tag_hit_i       ;
                        lkp_ack.ack.idx             <= {1'b0, req.lkp.req.idx} ;
                        lkp_ack.ack.lvl             <= 2'(BANK_TYPE)   ;
                        lkp_ack.ack.prefetched      <= tag_i.prefetched;
                        lkp_ack.ack.device_id       <= tag_i.device_id ;
                        lkp_ack.ack.process_id      <= tag_i.process_id;
                        lkp_ack.ack.fsc_mode        <= 'd0;
                        lkp_ack.ack.fsc_ppn         <= dram_rdata_muxed.fsc_ppn;
                        lkp_ack.ack.PSCID           <= 'd0;
                        lkp_ack.ack.SUM             <= 'd0;
                        lkp_ack.ack.ENS             <= 'd0;
                        lkp_ack.ack.V               <= dram_rdata_muxed.V;
                    end
                    else begin
                        lkp_ack_valid                 <= 1'b0;
                    end
                end
            end
        end
    end
endgenerate

genvar la;
generate
    for(la=0; la<CABIN_LKP_NUM; la++) begin : lkp_ack_dmux_gen
        assign lkp_ack_valid_o[la] = req_lidx[la] & lkp_ack_valid;
        assign lkp_ack_o[la]       = lkp_ack;
    end
endgenerate

//}}}

//=== UPD ACK {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            upd_ack_valid <= 1'b0;
            upd_ack       <= 'd0;
            req_uidx      <= 'd0;
        end
        else begin
            if(bank_req_valid_i & req_is_upd) begin
                upd_ack_valid <= 1'b1;
                req_uidx      <= req.uidx;
                upd_ack.ack   <= 'd0;
            end
            else begin
                upd_ack_valid <= 1'b0;
            end
        end
    end

genvar ua;
generate
    for(ua=0; ua<CABIN_UPD_NUM; ua++) begin : upd_ack_dmux_gen
        assign upd_ack_valid_o[ua] = req_uidx[ua] & upd_ack_valid;
        assign upd_ack_o[ua]       = upd_ack;
    end
endgenerate

//}}}

//=== INV ACK {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            inv_ack_valid <= 1'b0;
            inv_ack       <= 'd0;
            req_iidx      <= 'd0;
        end
        else begin
            if(bank_req_valid_i & req_is_inv) begin
                inv_ack_valid <= 1'b1;
                req_iidx      <= req.iidx;
                inv_ack.ack   <= 'd0;
            end
            else begin
                inv_ack_valid <= 1'b0;
            end
        end
    end

genvar ia;
generate
    for(ia=0; ia<CABIN_INV_NUM; ia++) begin : inv_ack_dmux_gen
        assign inv_ack_valid_o[ia] = req_iidx[ia] & inv_ack_valid;
        assign inv_ack_o[ia]       = inv_ack;
    end
endgenerate

//}}}

//}}}



endmodule
//}}}



