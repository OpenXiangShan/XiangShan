/////////////////////////////////////////////////////
// iommu_acd_mtlb_bank_top
/////////////////////////////////////////////////////
module iommu_acd_mtlb_bank_top #( //{{{
    parameter               BANK_TYPE                                           = 2'b00,    // 00:4K  01:2M  10:1G  11:512G
    parameter type          INVALID_REQ_TYPE                                    = iommu_acd_pkg::INVALID_REQ_TYPE,
    parameter type          LOOKUP_REQ_TYPE                                     = iommu_acd_pkg::lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE                                     = iommu_acd_pkg::lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE                                     = iommu_acd_pkg::update_req_t,
    parameter type          BANK_LKP_REQ_TYPE                                   = iommu_acd_pkg::bank_lkp_req_t,
    parameter type          BANK_LKP_ACK_TYPE                                   = iommu_acd_pkg::bank_lkp_ack_t,
    parameter type          BANK_UPD_REQ_TYPE                                   = iommu_acd_pkg::bank_upd_req_t,
    parameter type          BANK_UPD_ACK_TYPE                                   = iommu_acd_pkg::bank_upd_ack_t,
    parameter type          BANK_INV_REQ_TYPE                                   = iommu_acd_pkg::bank_inv_req_t,
    parameter type          BANK_INV_ACK_TYPE                                   = iommu_acd_pkg::bank_inv_ack_t,
    parameter               CABIN_LKP_IDX_WIDTH                                 = iommu_acd_pkg::CABIN_LKP_IDX_WIDTH,
    parameter               CABIN_UPD_IDX_WIDTH                                 = iommu_acd_pkg::CABIN_UPD_IDX_WIDTH,
    parameter               CABIN_INV_IDX_WIDTH                                 = iommu_acd_pkg::CABIN_INV_IDX_WIDTH,
    parameter               BANK_IDX_WIDTH                                      = iommu_acd_pkg::MAX_BANK_IDX_WIDTH,
    parameter               BANK_SET_IDX_WIDTH                                  = iommu_acd_pkg::MAX_BANK_SET_IDX_WIDTH,
    parameter               BANK_WAY_IDX_WIDTH                                  = iommu_acd_pkg::MAX_BANK_WAY_IDX_WIDTH,
    parameter               CABIN_LKP_NUM                                       = 2**CABIN_LKP_IDX_WIDTH,
    parameter               CABIN_UPD_NUM                                       = 2**CABIN_UPD_IDX_WIDTH,
    parameter               CABIN_INV_NUM                                       = 2**CABIN_INV_IDX_WIDTH,
    parameter               BANK_NUM                                            = 2**BANK_IDX_WIDTH,
    parameter               BANK_SET_NUM                                        = 2**BANK_SET_IDX_WIDTH,
    parameter               BANK_WAY_NUM                                        = 2**BANK_WAY_IDX_WIDTH,
    parameter type          MTLB_TAG_TYPE                                       = iommu_acd_pkg::mtlb_tag_t,
    parameter type          MTLB_ITAG_TYPE                                      = iommu_acd_pkg::mtlb_itag_t,
    parameter type          MTLB_UTAG_TYPE                                      = iommu_acd_pkg::mtlb_utag_t,
    parameter type          MTLB_DAT_TYPE                                       = iommu_acd_pkg::mtlb_dat_t,
    parameter               ECC_WIDTH                                           = 7,
    parameter               TRAM_DAT_WIDTH                                      = $bits(MTLB_TAG_TYPE)+ECC_WIDTH+1,
    parameter               IRAM_DAT_WIDTH                                      = $bits(MTLB_ITAG_TYPE)+ECC_WIDTH+1,
    parameter               DRAM_DAT_WIDTH                                      = $bits(MTLB_DAT_TYPE)+ECC_WIDTH+1,
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
    //output MTLB_TAG_TYPE [BANK_NUM-1:0]  [BANK_WAY_NUM-1:0]                     tram_wdata_o,
    //input  MTLB_TAG_TYPE [BANK_NUM-1:0]  [BANK_WAY_NUM-1:0]                     tram_rdata_i,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         tram_wdata_o,
    input  logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [TRAM_DAT_WIDTH-1:0]         tram_rdata_i,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                              iram_cs_o,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                              iram_wr_o,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [BANK_SET_IDX_WIDTH-1:0]     iram_addr_o,
    //output MTLB_ITAG_TYPE [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                     iram_wdata_o,
    //input  MTLB_ITAG_TYPE [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                     iram_rdata_i,
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
    //output MTLB_DAT_TYPE [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                      dram_wdata_o,
    //input  MTLB_DAT_TYPE [BANK_NUM-1:0] [BANK_WAY_NUM-1:0]                      dram_rdata_i,
    output logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         dram_wdata_o,
    input  logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [DRAM_DAT_WIDTH-1:0]         dram_rdata_i,
    // CFG                                                                      
    input  logic                                                                csr_fctl_gxl_i,
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

    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [ECC_WIDTH:0]                       bank_tram_wdata_ecc_o;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [ECC_WIDTH:0]                       bank_iram_wdata_ecc_o;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [ECC_WIDTH:0]                       bank_dram_wdata_ecc_o;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [ECC_WIDTH:0]                       bank_tram_rdata_ecc_i;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [ECC_WIDTH:0]                       bank_iram_rdata_ecc_i;
    logic [BANK_NUM-1:0] [BANK_WAY_NUM-1:0] [ECC_WIDTH:0]                       bank_dram_rdata_ecc_i;

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

//=== bank inst === {{{
genvar ramb, ramc;
generate
    for(ramb=0; ramb<BANK_NUM; ramb++) begin : bank_ram_connect_gen
        assign tram_cs_o            [ramb] = bank_tram_cs_o     [ramb];
        assign tram_wr_o            [ramb] = bank_tram_wr_o     [ramb];
        assign tram_addr_o          [ramb] = bank_tram_addr_o   [ramb];
        for(ramc=0; ramc<BANK_WAY_NUM; ramc++) begin : way_tram_connect_gen
            assign tram_wdata_o         [ramb][ramc] = {bank_tram_wdata_ecc_o[ramb][ramc], bank_tram_wdata_o[ramb][ramc]};
            assign bank_tram_rdata_ecc_i[ramb][ramc] = tram_rdata_i[ramb][ramc][TRAM_DAT_WIDTH-1-:ECC_WIDTH+1];
            assign bank_tram_rdata_i    [ramb][ramc] = tram_rdata_i[ramb][ramc][TRAM_DAT_WIDTH-1-ECC_WIDTH-1:0];
        end
        assign iram_cs_o            [ramb] = bank_iram_cs_o     [ramb];
        assign iram_wr_o            [ramb] = bank_iram_wr_o     [ramb];
        assign iram_addr_o          [ramb] = bank_iram_addr_o   [ramb];
        for(ramc=0; ramc<BANK_WAY_NUM; ramc++) begin : way_iram_connect_gen
            assign iram_wdata_o         [ramb][ramc] = {bank_iram_wdata_ecc_o[ramb][ramc], bank_iram_wdata_o[ramb][ramc]};
            assign bank_iram_rdata_ecc_i[ramb][ramc] = iram_rdata_i[ramb][ramc][IRAM_DAT_WIDTH-1-:ECC_WIDTH+1];
            assign bank_iram_rdata_i    [ramb][ramc] = iram_rdata_i[ramb][ramc][IRAM_DAT_WIDTH-1-ECC_WIDTH-1:0];
        end
        assign uram_cs_o            [ramb] = bank_uram_cs_o     [ramb];
        assign uram_wr_o            [ramb] = bank_uram_wr_o     [ramb];
        assign uram_addr_o          [ramb] = bank_uram_addr_o   [ramb];
        assign uram_wdata_o         [ramb] = bank_uram_wdata_o  [ramb];
        assign bank_uram_rdata_i    [ramb] = uram_rdata_i       [ramb];
        assign dram_cs_o            [ramb] = bank_dram_cs_o     [ramb];
        assign dram_wr_o            [ramb] = bank_dram_wr_o     [ramb];
        assign dram_addr_o          [ramb] = bank_dram_addr_o   [ramb];
        for(ramc=0; ramc<BANK_WAY_NUM; ramc++) begin : way_dram_connect_gen
            assign dram_wdata_o         [ramb][ramc] = {bank_dram_wdata_ecc_o[ramb][ramc], bank_dram_wdata_o[ramb][ramc]};
            assign bank_dram_rdata_ecc_i[ramb][ramc] = dram_rdata_i[ramb][ramc][DRAM_DAT_WIDTH-1-:ECC_WIDTH+1];
            assign bank_dram_rdata_i    [ramb][ramc] = dram_rdata_i[ramb][ramc][DRAM_DAT_WIDTH-1-ECC_WIDTH-1:0];
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

        iommu_acd_mtlb_bank #(
        /*parameter */              .BANK_TYPE                          (BANK_TYPE                  ), // = 2'b00,    // 00:4K  01:2M  10:1G  11:512G
        /*parameter type         */ .INVALID_REQ_TYPE                   (INVALID_REQ_TYPE           ), // = iommu_acd_pkg::INVALID_REQ_TYPE,
        /*parameter type         */ .LOOKUP_REQ_TYPE                    (LOOKUP_REQ_TYPE            ), // = iommu_acd_pkg::lookup_req_t,
        /*parameter type         */ .LOOKUP_ACK_TYPE                    (LOOKUP_ACK_TYPE            ), // = iommu_acd_pkg::lookup_ack_t,
        /*parameter type         */ .UPDATE_REQ_TYPE                    (UPDATE_REQ_TYPE            ), // = iommu_acd_pkg::update_req_t,
        /*parameter type         */ .BANK_LKP_REQ_TYPE                  (BANK_LKP_REQ_TYPE          ), // = iommu_acd_pkg::bank_lkp_req_t,
        /*parameter type         */ .BANK_LKP_ACK_TYPE                  (BANK_LKP_ACK_TYPE          ), // = iommu_acd_pkg::bank_lkp_ack_t,
        /*parameter type         */ .BANK_UPD_REQ_TYPE                  (BANK_UPD_REQ_TYPE          ), // = iommu_acd_pkg::bank_upd_req_t,
        /*parameter type         */ .BANK_UPD_ACK_TYPE                  (BANK_UPD_ACK_TYPE          ), // = iommu_acd_pkg::bank_upd_ack_t,
        /*parameter type         */ .BANK_INV_REQ_TYPE                  (BANK_INV_REQ_TYPE          ), // = iommu_acd_pkg::bank_inv_req_t,
        /*parameter type         */ .BANK_INV_ACK_TYPE                  (BANK_INV_ACK_TYPE          ), // = iommu_acd_pkg::bank_inv_ack_t,
        /*parameter */              .CABIN_LKP_IDX_WIDTH                (CABIN_LKP_IDX_WIDTH        ), // = 3,
        /*parameter */              .CABIN_UPD_IDX_WIDTH                (CABIN_UPD_IDX_WIDTH        ), // = 2,
        /*parameter */              .CABIN_INV_IDX_WIDTH                (CABIN_INV_IDX_WIDTH        ), // = 1,
        /*parameter */              .BANK_IDX_WIDTH                     (BANK_IDX_WIDTH             ), // = 3,
        /*parameter */              .BANK_SET_IDX_WIDTH                 (BANK_SET_IDX_WIDTH         ), // = 3,
        /*parameter */              .BANK_WAY_IDX_WIDTH                 (BANK_WAY_IDX_WIDTH         ), // = 3,
        /*parameter type         */ .MTLB_TAG_TYPE                      (MTLB_TAG_TYPE              ),
        /*parameter type         */ .MTLB_ITAG_TYPE                     (MTLB_ITAG_TYPE             ), // = iommu_acd_pkg::mtlb_itag_t,
        /*parameter type         */ .MTLB_UTAG_TYPE                     (MTLB_UTAG_TYPE             ), // = iommu_acd_pkg::mtlb_utag_t,
        /*parameter type         */ .MTLB_DAT_TYPE                      (MTLB_DAT_TYPE              ),
        /*parameter */              .ECC_WIDTH                          (ECC_WIDTH                  ), // = 7,
        /*parameter */              .SPARE_PARAM                        (1'b0                       )  // = 0
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
        /*output logic [BANK_WAY_NUM-1:0] [ECC_WIDTH:0]             */  .tram_wdata_ecc_o           (bank_tram_wdata_ecc_o   [i]),
        /*output logic [BANK_WAY_NUM-1:0] [ECC_WIDTH:0]             */  .iram_wdata_ecc_o           (bank_iram_wdata_ecc_o   [i]),
        /*output logic [BANK_WAY_NUM-1:0] [ECC_WIDTH:0]             */  .dram_wdata_ecc_o           (bank_dram_wdata_ecc_o   [i]),
        /*input  logic [BANK_WAY_NUM-1:0] [ECC_WIDTH:0]             */  .tram_rdata_ecc_i           (bank_tram_rdata_ecc_i   [i]),
        /*input  logic [BANK_WAY_NUM-1:0] [ECC_WIDTH:0]             */  .iram_rdata_ecc_i           (bank_iram_rdata_ecc_i   [i]),
        /*input  logic [BANK_WAY_NUM-1:0] [ECC_WIDTH:0]             */  .dram_rdata_ecc_i           (bank_dram_rdata_ecc_i   [i]),
        /*input  logic                                              */  .csr_fctl_gxl_i             (csr_fctl_gxl_i             ),
        /*input  logic                                              */  .multi_hit_check_i          (multi_hit_check_i          ),
        /*output logic                                              */  .multi_hit_fault_o          (bank_multi_hit_fault_o  [i]),
        /*output logic [1:0]                                        */  .ecc_err_o                  (bank_ecc_err_o          [i]),
        /*input  logic                                              */  .spare_in                   (1'b0                       ) 
        );
    end
endgenerate
//}}}

endmodule//}}}
