module iommu_atd_ptc_mtlb_lkp_cabin_wrap #(
    parameter   CACHE_TYPE                  = 0, // 0:S2PTC, 1:S1PTC
    parameter type          LOOKUP_REQ_TYPE             = iommu_atd_cache_pkg::s2ptc_lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE             = iommu_atd_cache_pkg::s2ptc_lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE             = iommu_atd_cache_pkg::s2ptc_update_req_t,
    parameter type          BANK_LKP_REQ_TYPE           = iommu_atd_cache_pkg::s2ptc_bank_lkp_req_t,
    parameter type          BANK_LKP_ACK_TYPE           = iommu_atd_cache_pkg::s2ptc_bank_lkp_ack_t,
    parameter   CABIN_IDX_WIDTH             = iommu_atd_cache_pkg::S2PTC_CABIN_LKP_IDX_WIDTH,
    parameter   BANK_L0_IDX_WIDTH           = iommu_atd_cache_pkg::S2PTC_BANK_L0_IDX_WIDTH,
    parameter   BANK_L1_IDX_WIDTH           = iommu_atd_cache_pkg::S2PTC_BANK_L1_IDX_WIDTH,
    parameter   BANK_L2_IDX_WIDTH           = iommu_atd_cache_pkg::S2PTC_BANK_L2_IDX_WIDTH,
    parameter   BANK_L3_IDX_WIDTH           = iommu_atd_cache_pkg::S2PTC_BANK_L3_IDX_WIDTH,
    parameter   BANK_L4_IDX_WIDTH           = iommu_atd_cache_pkg::S2PTC_BANK_L4_IDX_WIDTH,
    parameter   BANK_L0_SET_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L0_SET_IDX_WIDTH,
    parameter   BANK_L1_SET_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L1_SET_IDX_WIDTH,
    parameter   BANK_L2_SET_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L2_SET_IDX_WIDTH,
    parameter   BANK_L3_SET_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L3_SET_IDX_WIDTH,
    parameter   BANK_L4_SET_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L4_SET_IDX_WIDTH,
    parameter   CABIN_NUM                   = 2**CABIN_IDX_WIDTH,
    parameter   BANK_L0_NUM                 = 2**BANK_L0_IDX_WIDTH,
    parameter   BANK_L1_NUM                 = 2**BANK_L1_IDX_WIDTH,
    parameter   BANK_L2_NUM                 = 2**BANK_L2_IDX_WIDTH,
    parameter   BANK_L3_NUM                 = 2**BANK_L3_IDX_WIDTH,
    parameter   BANK_L4_NUM                 = 2**BANK_L4_IDX_WIDTH,
    parameter   BANK_L0_SET_NUM             = 2**BANK_L0_SET_IDX_WIDTH,
    parameter   BANK_L1_SET_NUM             = 2**BANK_L1_SET_IDX_WIDTH,
    parameter   BANK_L2_SET_NUM             = 2**BANK_L2_SET_IDX_WIDTH,
    parameter   BANK_L3_SET_NUM             = 2**BANK_L3_SET_IDX_WIDTH,
    parameter   BANK_L4_SET_NUM             = 2**BANK_L4_SET_IDX_WIDTH,
    parameter   SPARE_PARAM                 = 0
)(
//{{{ IO                                                
    input  logic                                        clk,
    input  logic                                        rstn,
    // LKP INPUT                                        
    input  logic                                        lookup_req_valid_i,
    output logic                                        lookup_req_ready_o,
    input  LOOKUP_REQ_TYPE                              lookup_req_i,
    // LKP OUTPUT                                       
    output logic                                        lookup_ack_valid_o,
    input  logic                                        lookup_ack_ready_i,
    output UPDATE_REQ_TYPE                              lookup_ack_o,
    output logic                                        lookup_ack_hit_o,
    // BANK REQ                                         
    output logic             [4:0] [CABIN_NUM-1:0]      lkp2bank_req_valid_o,   // [0]: BANK_L0, [1]: BANK_L1, [2]:BANK_L2
    input  logic             [4:0] [CABIN_NUM-1:0]      lkp2bank_req_ready_i,
    output BANK_LKP_REQ_TYPE [4:0] [CABIN_NUM-1:0]      lkp2bank_req_o,
    // BANK ACK                                         
    input  logic             [4:0] [CABIN_NUM-1:0]      bank2lkp_ack_valid_i,
    input  BANK_LKP_ACK_TYPE [4:0] [CABIN_NUM-1:0]      bank2lkp_ack_i,
    // BANK STATUS INPUT
    input  logic [4:0]                                  bank_valid_i,
    //
    input  logic                                        csr_fctl_gxl_i,
    //
    input  logic                                        spare_in
//}}}
);
//=== Declare {{{
    typedef struct packed {
        logic [4:0]                                     valid;
        logic [4:0]                                     ready;
        BANK_LKP_REQ_TYPE [4:0]                         req;
    } cabin_bank_lkp_req_t;

    typedef struct packed {
        logic [4:0]                                     valid;
        BANK_LKP_ACK_TYPE [4:0]                         ack;
    } cabin_bank_lkp_ack_t;

    typedef struct packed {
        UPDATE_REQ_TYPE                                 ack;
        logic                                           hit;
    } lkp_ack_out_t; //UPDATE_REQ_TYPE

    logic                [CABIN_NUM-1:0]                cabin_lkp_req_valid_i;
    LOOKUP_REQ_TYPE      [CABIN_NUM-1:0]                cabin_lkp_req_i      ;
    logic                [CABIN_NUM-1:0]                cabin_lkp_ack_valid_o;
    logic                [CABIN_NUM-1:0]                cabin_lkp_ack_ready_i;
    UPDATE_REQ_TYPE      [CABIN_NUM-1:0]                cabin_lkp_ack_o      ;
    logic                [CABIN_NUM-1:0]                cabin_lkp_ack_hit_o;
    cabin_bank_lkp_req_t [CABIN_NUM-1:0]                cabin_lkp2bank_req;
    cabin_bank_lkp_ack_t [CABIN_NUM-1:0]                cabin_bank2lkp_ack;
    logic                [CABIN_NUM-1:0]                cabin_valid_o;
                                                        
    lkp_ack_out_t        [CABIN_NUM-1:0]                cabin_lkp_ack_out_mux;
    lkp_ack_out_t                                       lkp_ack_out_muxed;
    UPDATE_REQ_TYPE                                     lkp_ack_out_ack;
    logic                                               lkp_ack_out_hit;
//}}}

//=== Main Code {{{
    assign lookup_req_ready_o = ~(&cabin_valid_o);      // ready low if all cabin occupied

//=== lkp2bank req dmux {{{
genvar k;
generate
    for(k=0; k<CABIN_NUM; k++) begin : lkp2bank_req_connect_gen
        // CABIN_k_ to BANK_l0_
        assign lkp2bank_req_valid_o[0][k]       = cabin_lkp2bank_req[k].valid[0];
        assign cabin_lkp2bank_req[k].ready[0]   = lkp2bank_req_ready_i[0][k];
        assign lkp2bank_req_o[0][k]             = cabin_lkp2bank_req[k].valid[0] ? cabin_lkp2bank_req[k].req[0] : 'd0;
        // CABIN_k_ to BANK_l1_
        assign lkp2bank_req_valid_o[1][k]       = cabin_lkp2bank_req[k].valid[1];
        assign cabin_lkp2bank_req[k].ready[1]   = lkp2bank_req_ready_i[1][k];
        assign lkp2bank_req_o[1][k]             = cabin_lkp2bank_req[k].valid[1] ? cabin_lkp2bank_req[k].req[1] : 'd0;
        // CABIN_k_ to BANK_l2_
        assign lkp2bank_req_valid_o[2][k]       = cabin_lkp2bank_req[k].valid[2];
        assign cabin_lkp2bank_req[k].ready[2]   = lkp2bank_req_ready_i[2][k];
        assign lkp2bank_req_o[2][k]             = cabin_lkp2bank_req[k].valid[2] ? cabin_lkp2bank_req[k].req[2] : 'd0;
        // CABIN_k_ to BANK_l3_
        assign lkp2bank_req_valid_o[3][k]       = cabin_lkp2bank_req[k].valid[3];
        assign cabin_lkp2bank_req[k].ready[3]   = lkp2bank_req_ready_i[3][k];
        assign lkp2bank_req_o[3][k]             = cabin_lkp2bank_req[k].valid[3] ? cabin_lkp2bank_req[k].req[3] : 'd0;
        // CABIN_k_ to BANK_l3_
        assign lkp2bank_req_valid_o[4][k]       = cabin_lkp2bank_req[k].valid[4];
        assign cabin_lkp2bank_req[k].ready[4]   = lkp2bank_req_ready_i[4][k];
        assign lkp2bank_req_o[4][k]             = cabin_lkp2bank_req[k].valid[4] ? cabin_lkp2bank_req[k].req[4] : 'd0;
    end
endgenerate
//}}}

//=== bank ack dmux {{{
genvar kk;
generate
    for(kk=0; kk<CABIN_NUM; kk++) begin : bank2lkp_ack_connect_gen
        // BANK_0_ to CABIN_kk_
        assign cabin_bank2lkp_ack[kk].valid[0]     = bank2lkp_ack_valid_i[0][kk];
        assign cabin_bank2lkp_ack[kk].ack[0]       = bank2lkp_ack_i[0][kk];
        // BANK_1_ to CABIN_kk_
        assign cabin_bank2lkp_ack[kk].valid[1]     = bank2lkp_ack_valid_i[1][kk];
        assign cabin_bank2lkp_ack[kk].ack[1]       = bank2lkp_ack_i[1][kk];
        // BANK_2_ to CABIN_kk_
        assign cabin_bank2lkp_ack[kk].valid[2]     = bank2lkp_ack_valid_i[2][kk];
        assign cabin_bank2lkp_ack[kk].ack[2]       = bank2lkp_ack_i[2][kk];
        // BANK_3_ to CABIN_kk_
        assign cabin_bank2lkp_ack[kk].valid[3]     = bank2lkp_ack_valid_i[3][kk];
        assign cabin_bank2lkp_ack[kk].ack[3]       = bank2lkp_ack_i[3][kk];
        // BANK_4_ to CABIN_kk_
        assign cabin_bank2lkp_ack[kk].valid[4]     = bank2lkp_ack_valid_i[4][kk];
        assign cabin_bank2lkp_ack[kk].ack[4]       = bank2lkp_ack_i[4][kk];
    end
endgenerate
//}}}

//=== lookup ack arbiter {{{
genvar lko;
generate
    for(lko=0; lko<CABIN_NUM; lko++) begin : lkp_ack_out_mux_gen
        assign cabin_lkp_ack_out_mux[lko].ack = cabin_lkp_ack_o[lko];
        assign cabin_lkp_ack_out_mux[lko].hit = cabin_lkp_ack_hit_o[lko];
    end
endgenerate

    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (0                      ),   //= 0,
    /*parameter     */ .REQ_NUM             (CABIN_NUM              ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (lkp_ack_out_t          ),   //= logic,
    /*parameter     */ .AXIVLDRDY           (1                      )    //= 1
    ) U_translate_ack_arb(
    /*input  logic                                  */  .clk                    (clk                        ),
    /*input  logic                                  */  .rstn                   (rstn                       ),
    /*input  logic [REQ_NUM-1:0]                    */  .req_i                  (cabin_lkp_ack_valid_o      ),
    /*input  logic [REQ_NUM-1:0]                    */  .req_prior_i            ({CABIN_NUM{1'b0}}          ),
    /*input  DATA_TYPE [REQ_NUM-1:0]                */  .data_i                 (cabin_lkp_ack_out_mux      ),
    /*output logic [REQ_NUM-1:0]                    */  .gnt_o                  (cabin_lkp_ack_ready_i      ),
    /*output logic                                  */  .req_o                  (lookup_ack_valid_o         ),
    /*output DATA_TYPE                              */  .data_o                 (lkp_ack_out_muxed          ),
    /*input  logic                                  */  .gnt_i                  (lookup_ack_ready_i         ) 
    );

//    assign lookup_ack_hit_o = |(cabin_lkp_ack_hit_o & cabin_lkp_ack_ready_i);
    assign lookup_ack_o     = lkp_ack_out_muxed.ack;
    assign lookup_ack_hit_o = lkp_ack_out_muxed.hit;

//}}}

//=== cabin inst {{{
genvar i;
generate
    for(i=0; i<CABIN_NUM; i++) begin : cabin_inst_gen
        assign cabin_lkp_req_i[i] = lookup_req_i;
        iommu_atd_ptc_mtlb_lkp_cabin #(
        /*parameter  */ .CACHE_TYPE                 (CACHE_TYPE                 ), // = 0, // 0:S2PTC, 1:S1PTC
        /*parameter type         */ .LOOKUP_REQ_TYPE            (LOOKUP_REQ_TYPE            ), // = iommu_atd_cache_pkg::s2ptc_lookup_req_t  ,
        /*parameter type         */ .LOOKUP_ACK_TYPE            (LOOKUP_ACK_TYPE            ), // = iommu_atd_cache_pkg::s2ptc_lookup_ack_t  ,
        /*parameter type         */ .UPDATE_REQ_TYPE            (UPDATE_REQ_TYPE            ), // = iommu_atd_cache_pkg::s2ptc_update_req_t,
        /*parameter type         */ .BANK_LKP_REQ_TYPE          (BANK_LKP_REQ_TYPE          ), // = iommu_atd_cache_pkg::s2ptc_bank_lkp_req_t,
        /*parameter type         */ .BANK_LKP_ACK_TYPE          (BANK_LKP_ACK_TYPE          ), // = iommu_atd_cache_pkg::s2ptc_bank_lkp_ack_t,
        /*parameter  */ .CABIN_IDX_WIDTH            (CABIN_IDX_WIDTH            ), // = iommu_atd_cache_pkg::S2PTC_CABIN_LKP_IDX_WIDTH,
        /*parameter  */ .BANK_L0_IDX_WIDTH          (BANK_L0_IDX_WIDTH          ), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_IDX_WIDTH,
        /*parameter  */ .BANK_L1_IDX_WIDTH          (BANK_L1_IDX_WIDTH          ), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_IDX_WIDTH,
        /*parameter  */ .BANK_L2_IDX_WIDTH          (BANK_L2_IDX_WIDTH          ), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_IDX_WIDTH,
        /*parameter  */ .BANK_L3_IDX_WIDTH          (BANK_L3_IDX_WIDTH          ), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_IDX_WIDTH,
        /*parameter  */ .BANK_L4_IDX_WIDTH          (BANK_L4_IDX_WIDTH          ), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_IDX_WIDTH,
        /*parameter  */ .BANK_L0_SET_IDX_WIDTH      (BANK_L0_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_BANK_L0_SET_IDX_WIDTH,
        /*parameter  */ .BANK_L1_SET_IDX_WIDTH      (BANK_L1_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_BANK_L1_SET_IDX_WIDTH,
        /*parameter  */ .BANK_L2_SET_IDX_WIDTH      (BANK_L2_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_BANK_L2_SET_IDX_WIDTH,
        /*parameter  */ .BANK_L3_SET_IDX_WIDTH      (BANK_L3_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_BANK_L3_SET_IDX_WIDTH,
        /*parameter  */ .BANK_L4_SET_IDX_WIDTH      (BANK_L4_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::S2PTC_BANK_L4_SET_IDX_WIDTH,
        /*parameter  */ .SPARE_PARAM                (1'b0                       )  // = 0
        ) U_cabin(
        /*input  logic                                      */  .clk                        (clk                            ),
        /*input  logic                                      */  .rstn                       (rstn                           ),
        /*input  logic [CABIN_IDX_WIDTH-1:0]                */  .cabin_idx                  (CABIN_IDX_WIDTH'(i)            ),
        /*input  logic                                      */  .lkp_req_valid_i            (cabin_lkp_req_valid_i       [i]),
        /*input  LOOKUP_REQ_TYPE                            */  .lkp_req_i                  (cabin_lkp_req_i             [i]),
        /*output logic                                      */  .lkp_ack_valid_o            (cabin_lkp_ack_valid_o       [i]),
        /*input  logic                                      */  .lkp_ack_ready_i            (cabin_lkp_ack_ready_i       [i]),
        /*output UPDATE_REQ_TYPE                            */  .lkp_ack_o                  (cabin_lkp_ack_o             [i]),
        /*output logic                                      */  .lkp_ack_hit_o              (cabin_lkp_ack_hit_o         [i]),
        /*output logic [4:0]                                */  .lkp2bank_req_valid_o       (cabin_lkp2bank_req[i].valid    ),   // [0]: BANK_L0, [1]: BANK_L1, [2]:BANK_L2
        /*input  logic [4:0]                                */  .lkp2bank_req_ready_i       (cabin_lkp2bank_req[i].ready    ),
        /*output BANK_LKP_REQ_TYPE [4:0]                    */  .lkp2bank_req_o             (cabin_lkp2bank_req[i].req      ),
        /*input  logic [4:0]                                */  .bank2lkp_ack_valid_i       (cabin_bank2lkp_ack[i].valid    ),
        /*input  BANK_LKP_ACK_TYPE [4:0]                    */  .bank2lkp_ack_i             (cabin_bank2lkp_ack[i].ack      ),
        /*output logic                                      */  .valid_o                    (cabin_valid_o               [i]),
        /*input  logic [4:0]                                */  .bank_valid_i               (bank_valid_i                   ),
        /*input  logic                                      */  .csr_fctl_gxl_i             (csr_fctl_gxl_i                 ),
        /*input  logic                                      */  .spare_in                   (1'b0                           ) 
        );

        iommu_acd_bus_handler_queue_entry_update #(.IDX_WIDTH(CABIN_IDX_WIDTH)) U_entry_valid_check(
            .valid_i    (cabin_valid_o              ),
            .update_i   (lookup_req_valid_i         ),
            .tag_i      (CABIN_IDX_WIDTH'(i)        ),
            .update_o   (cabin_lkp_req_valid_i[i]   )
        );
    end
endgenerate

//}}}

//}}}

endmodule



module iommu_atd_ptc_mtlb_lkp_cabin #(
    parameter   CACHE_TYPE                  = 0, // 0:S2PTC, 1:S1PTC
    parameter type          LOOKUP_REQ_TYPE             = iommu_atd_cache_pkg::s2ptc_lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE             = iommu_atd_cache_pkg::s2ptc_lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE             = iommu_atd_cache_pkg::s2ptc_update_req_t,
    parameter type          BANK_LKP_REQ_TYPE           = iommu_atd_cache_pkg::s2ptc_bank_lkp_req_t,
    parameter type          BANK_LKP_ACK_TYPE           = iommu_atd_cache_pkg::s2ptc_bank_lkp_ack_t,
    parameter   CABIN_IDX_WIDTH             = iommu_atd_cache_pkg::S2PTC_CABIN_LKP_IDX_WIDTH,
    parameter   BANK_L0_IDX_WIDTH           = iommu_atd_cache_pkg::S2PTC_BANK_L0_IDX_WIDTH,
    parameter   BANK_L1_IDX_WIDTH           = iommu_atd_cache_pkg::S2PTC_BANK_L1_IDX_WIDTH,
    parameter   BANK_L2_IDX_WIDTH           = iommu_atd_cache_pkg::S2PTC_BANK_L2_IDX_WIDTH,
    parameter   BANK_L3_IDX_WIDTH           = iommu_atd_cache_pkg::S2PTC_BANK_L3_IDX_WIDTH,
    parameter   BANK_L4_IDX_WIDTH           = iommu_atd_cache_pkg::S2PTC_BANK_L4_IDX_WIDTH,
    parameter   BANK_L0_SET_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L0_SET_IDX_WIDTH,
    parameter   BANK_L1_SET_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L1_SET_IDX_WIDTH,
    parameter   BANK_L2_SET_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L2_SET_IDX_WIDTH,
    parameter   BANK_L3_SET_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L3_SET_IDX_WIDTH,
    parameter   BANK_L4_SET_IDX_WIDTH       = iommu_atd_cache_pkg::S2PTC_BANK_L4_SET_IDX_WIDTH,
    parameter   BANK_L0_NUM                 = 2**BANK_L0_IDX_WIDTH,
    parameter   BANK_L1_NUM                 = 2**BANK_L1_IDX_WIDTH,
    parameter   BANK_L2_NUM                 = 2**BANK_L2_IDX_WIDTH,
    parameter   BANK_L3_NUM                 = 2**BANK_L3_IDX_WIDTH,
    parameter   BANK_L4_NUM                 = 2**BANK_L4_IDX_WIDTH,
    parameter   BANK_L0_SET_NUM             = 2**BANK_L0_SET_IDX_WIDTH,
    parameter   BANK_L1_SET_NUM             = 2**BANK_L1_SET_IDX_WIDTH,
    parameter   BANK_L2_SET_NUM             = 2**BANK_L2_SET_IDX_WIDTH,
    parameter   BANK_L3_SET_NUM             = 2**BANK_L3_SET_IDX_WIDTH,
    parameter   BANK_L4_SET_NUM             = 2**BANK_L4_SET_IDX_WIDTH,
    parameter   SPARE_PARAM                 = 0
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    // IDX
    input  logic [CABIN_IDX_WIDTH-1:0]                  cabin_idx,
    // LKP INPUT
    input  logic                                        lkp_req_valid_i,
    input  LOOKUP_REQ_TYPE                              lkp_req_i,
    // LKP OUTPUT
    output logic                                        lkp_ack_valid_o,
    input  logic                                        lkp_ack_ready_i,
    output UPDATE_REQ_TYPE                              lkp_ack_o,
    output logic                                        lkp_ack_hit_o,
    // BANK REQ                                         
    output logic [4:0]                                  lkp2bank_req_valid_o,   // [0]: BANK_L0, [1]: BANK_L1, [2]:BANK_L2 ..
    input  logic [4:0]                                  lkp2bank_req_ready_i,
    output BANK_LKP_REQ_TYPE [4:0]                      lkp2bank_req_o,
    // BANK ACK                                         
    input  logic [4:0]                                  bank2lkp_ack_valid_i,
    input  BANK_LKP_ACK_TYPE [4:0]                      bank2lkp_ack_i,
    // VALID OUT
    output logic                                        valid_o,
    // BANK STATUS INPUT
    input  logic [4:0]                                  bank_valid_i,
    //
    input  logic                                        csr_fctl_gxl_i,
    //
    input  logic                                        spare_in
//}}}
);
//=== Declare === {{{
    localparam MAX_BANK_IDX_WIDTH_0 = (BANK_L0_IDX_WIDTH > BANK_L1_IDX_WIDTH) ? BANK_L0_IDX_WIDTH : BANK_L1_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH_1 = (BANK_L2_IDX_WIDTH > BANK_L3_IDX_WIDTH) ? BANK_L2_IDX_WIDTH : BANK_L3_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH_2 =  BANK_L4_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH_01= (MAX_BANK_IDX_WIDTH_0 > MAX_BANK_IDX_WIDTH_1) ? MAX_BANK_IDX_WIDTH_0 : MAX_BANK_IDX_WIDTH_1;
    localparam MAX_BANK_IDX_WIDTH   = (MAX_BANK_IDX_WIDTH_2 > MAX_BANK_IDX_WIDTH_01)? MAX_BANK_IDX_WIDTH_2 : MAX_BANK_IDX_WIDTH_01;
    
    localparam MAX_BANK_SET_IDX_WIDTH_0 = (BANK_L0_SET_IDX_WIDTH > BANK_L1_SET_IDX_WIDTH) ? BANK_L0_SET_IDX_WIDTH : BANK_L1_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH_1 = (BANK_L2_SET_IDX_WIDTH > BANK_L3_SET_IDX_WIDTH) ? BANK_L2_SET_IDX_WIDTH : BANK_L3_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH_2 =  BANK_L4_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH_01= (MAX_BANK_SET_IDX_WIDTH_0 > MAX_BANK_SET_IDX_WIDTH_1) ? MAX_BANK_SET_IDX_WIDTH_0 : MAX_BANK_SET_IDX_WIDTH_1;
    localparam MAX_BANK_SET_IDX_WIDTH   = (MAX_BANK_SET_IDX_WIDTH_2 > MAX_BANK_SET_IDX_WIDTH_01)? MAX_BANK_SET_IDX_WIDTH_2 : MAX_BANK_SET_IDX_WIDTH_01;

    localparam S_IDLE = 4'b000;
//    localparam S_L0   = 4'b001;
//    localparam S_L1   = 4'b010;
//    localparam S_L2   = 4'b011;
//    localparam S_L3   = 4'b100;
//    localparam S_L4   = 4'b101;
    localparam S_ACK  = 4'b110;
    localparam S_ALL  = 4'b111;
    localparam S_WAIT_ALL_ACK = 4'b101;

    logic [3:0]                                         cs, ns;
    LOOKUP_REQ_TYPE                                     req;

    logic                                               idx_gv;
    logic [15:0]                                        idx_gscid;
    logic [19:0]                                        idx_pscid;
    logic [63:12]                                       idx_addr;
    logic [2:0]                                         idx_lvl[4:0];
    logic [MAX_BANK_IDX_WIDTH+MAX_BANK_SET_IDX_WIDTH-1:0] idx[4:0];
    logic [BANK_L0_IDX_WIDTH+BANK_L0_SET_IDX_WIDTH-1:0] idx_l0;
    logic [BANK_L1_IDX_WIDTH+BANK_L1_SET_IDX_WIDTH-1:0] idx_l1;
    logic [BANK_L2_IDX_WIDTH+BANK_L2_SET_IDX_WIDTH-1:0] idx_l2;
    logic [BANK_L3_IDX_WIDTH+BANK_L3_SET_IDX_WIDTH-1:0] idx_l3;
    logic [BANK_L4_IDX_WIDTH+BANK_L4_SET_IDX_WIDTH-1:0] idx_l4;


    BANK_LKP_ACK_TYPE                                   cur_bank2lkp_ack[4:0];
    BANK_LKP_ACK_TYPE                                   fast_bank2lkp_ack;
//    logic [2:0]                                         cur_bank2lkp_ack_pte_position;

    logic                                               lkp2bank_req_valid[4:0];
    logic                                               lkp2bank_req_ready[4:0];
    BANK_LKP_REQ_TYPE                                   lkp2bank_req[4:0];

    logic [4:0]                                         lkp_all_ack_got, lkp_fast_ack_got;


//}}}

//=== MainCode {{{
//=== FSM Stage 1 {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cs <= S_IDLE;
        else
            cs <= ns;
    end
//}}}

//=== FSM Stage 2 {{{
    always@(*) begin
        ns = cs;
        case(cs)
        S_IDLE  : begin
            if(~lkp_req_valid_i)
                ns = S_IDLE;
            else begin
                ns = S_ALL;
//                if(bank_valid_i[0])
//                    ns = S_L0;
//                else if(bank_valid_i[1])
//                    ns = S_L1;
//                else if(bank_valid_i[2])
//                    ns = S_L2;
//                else if(bank_valid_i[3])
//                    ns = S_L3;
//                else if(bank_valid_i[4])
//                    ns = S_L4;
//                else
//                    ns = S_IDLE;
            end
        end
//        S_L0    : begin
//            if(~bank2lkp_ack_valid_i[0])
//                ns = S_L0;
//            else begin
//                if(bank2lkp_ack_i[0].hit)
//                    ns = S_ACK;
//                else begin
//                    if(bank_valid_i[1])
//                        ns = S_L1;
//                    else if(bank_valid_i[2])
//                        ns = S_L2;
//                    else if(bank_valid_i[3])
//                        ns = S_L3;
//                    else if(bank_valid_i[4])
//                        ns = S_L4;
//                    else
//                        ns = S_ACK;
//                end
//            end
//        end
//        S_L1    :begin
//            if(~bank2lkp_ack_valid_i[1])
//                ns = S_L1;
//            else begin
//                if(bank2lkp_ack_i[1].hit)
//                    ns = S_ACK;
//                else begin
//                    if(bank_valid_i[2])
//                        ns = S_L2;
//                    else if(bank_valid_i[3])
//                        ns = S_L3;
//                    else if(bank_valid_i[4])
//                        ns = S_L4;
//                    else
//                        ns = S_ACK;
//                end
//            end
//        end
//        S_L2    : begin
//            if(~bank2lkp_ack_valid_i[2])
//                ns = S_L2;
//            else begin
//                if(bank2lkp_ack_i[2].hit)
//                    ns = S_ACK;
//                else begin
//                    if(bank_valid_i[3])
//                        ns = S_L3;
//                    else if(bank_valid_i[4])
//                        ns = S_L4;
//                    else
//                        ns = S_ACK;
//                end
//            end
//        end
//        S_L3    : begin
//            if(~bank2lkp_ack_valid_i[3])
//                ns = S_L3;
//            else begin
//                if(bank2lkp_ack_i[3].hit)
//                    ns = S_ACK;
//                else begin
//                    if(bank_valid_i[4])
//                        ns = S_L4;
//                    else
//                        ns = S_ACK;
//                end
//            end
//        end
//        S_L4    : begin
//            if(~bank2lkp_ack_valid_i[4])
//                ns = S_L4;
//            else begin
////                if(bank2lkp_ack_i[4].hit)
////                    ns = S_ACK;
////                else begin
////                    ns = S_ACK;
////                end
//                ns = S_ACK;
//            end
//        end
        S_ACK   : begin
            if(~lkp_ack_ready_i)
                ns = S_ACK;
            else begin
                if(&lkp_all_ack_got)
                    ns = S_IDLE;
                else
                    ns = S_WAIT_ALL_ACK;
            end
        end
        S_WAIT_ALL_ACK: begin
            if(&lkp_all_ack_got)
                ns = S_IDLE;
            else
                ns = S_WAIT_ALL_ACK;
        end
        S_ALL: begin
            if(|lkp_fast_ack_got)
                ns = S_ACK;
            else if(&lkp_all_ack_got)
                ns = S_ACK;
            else
                ns = S_ALL;
        end
        default : ns = S_IDLE;
        endcase
    end
//}}}

//=== FSM Stage 3{{{
//=== valid_o {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            valid_o <= 1'b0;
            req     <= 'd0;
        end
        else begin
            if(cs!=S_IDLE & ns==S_IDLE) begin
                valid_o <= 1'b0;
                req     <= req;
            end
            else if(cs==S_IDLE & ns!=S_IDLE) begin
                valid_o <= 1'b1;
                req     <= lkp_req_i;
            end
        end
    end
//}}}

//=== BANK_REQ {{{
//=== BANK_L0_
genvar i;
generate
    for(i=0; i<5; i++) begin : lkp2bank_req_gen
        always@(posedge clk or negedge rstn) begin
            if(~rstn) begin
                lkp2bank_req_valid  [i] <= 'd0;
                lkp2bank_req        [i] <= 'd0;
            end
            else begin
                if(lkp2bank_req_valid[i]) begin
                    if(~lkp2bank_req_ready[i])
                        lkp2bank_req_valid[i] <= 1'b1;
                    else
                        lkp2bank_req_valid[i] <= 1'b0;
                end
                else begin
                    if(cs==S_IDLE & ns==S_ALL) begin
                        lkp2bank_req_valid[i]       <= 1'b1;
                        lkp2bank_req[i].req         <= lkp_req_i;
                        lkp2bank_req[i].bank_idx    <= i==0 ? {{(MAX_BANK_IDX_WIDTH-BANK_L0_IDX_WIDTH){1'b0}},         idx_l0[BANK_L0_IDX_WIDTH+BANK_L0_SET_IDX_WIDTH-1:BANK_L0_SET_IDX_WIDTH]} :
                                                       i==1 ? {{(MAX_BANK_IDX_WIDTH-BANK_L1_IDX_WIDTH){1'b0}},         idx_l1[BANK_L1_IDX_WIDTH+BANK_L1_SET_IDX_WIDTH-1:BANK_L1_SET_IDX_WIDTH]} :
                                                       i==2 ? {{(MAX_BANK_IDX_WIDTH-BANK_L2_IDX_WIDTH){1'b0}},         idx_l2[BANK_L2_IDX_WIDTH+BANK_L2_SET_IDX_WIDTH-1:BANK_L2_SET_IDX_WIDTH]} :
                                                       i==3 ? {{(MAX_BANK_IDX_WIDTH-BANK_L3_IDX_WIDTH){1'b0}},         idx_l3[BANK_L3_IDX_WIDTH+BANK_L3_SET_IDX_WIDTH-1:BANK_L3_SET_IDX_WIDTH]} :
                                                       i==4 ? {{(MAX_BANK_IDX_WIDTH-BANK_L4_IDX_WIDTH){1'b0}},         idx_l4[BANK_L4_IDX_WIDTH+BANK_L4_SET_IDX_WIDTH-1:BANK_L4_SET_IDX_WIDTH]} :
                                                              {{(MAX_BANK_IDX_WIDTH-BANK_L0_IDX_WIDTH){1'b0}},         idx_l0[BANK_L0_IDX_WIDTH+BANK_L0_SET_IDX_WIDTH-1:BANK_L0_SET_IDX_WIDTH]};
                        lkp2bank_req[i].bank_set_idx<= i==0 ? {{(MAX_BANK_SET_IDX_WIDTH-BANK_L0_SET_IDX_WIDTH){1'b0}}, idx_l0[BANK_L0_SET_IDX_WIDTH-1:0]                                      } :
                                                       i==1 ? {{(MAX_BANK_SET_IDX_WIDTH-BANK_L1_SET_IDX_WIDTH){1'b0}}, idx_l1[BANK_L1_SET_IDX_WIDTH-1:0]                                      } :
                                                       i==2 ? {{(MAX_BANK_SET_IDX_WIDTH-BANK_L2_SET_IDX_WIDTH){1'b0}}, idx_l2[BANK_L2_SET_IDX_WIDTH-1:0]                                      } :
                                                       i==3 ? {{(MAX_BANK_SET_IDX_WIDTH-BANK_L3_SET_IDX_WIDTH){1'b0}}, idx_l3[BANK_L3_SET_IDX_WIDTH-1:0]                                      } :
                                                       i==4 ? {{(MAX_BANK_SET_IDX_WIDTH-BANK_L4_SET_IDX_WIDTH){1'b0}}, idx_l4[BANK_L4_SET_IDX_WIDTH-1:0]                                      } :
                                                              {{(MAX_BANK_SET_IDX_WIDTH-BANK_L0_SET_IDX_WIDTH){1'b0}}, idx_l0[BANK_L0_SET_IDX_WIDTH-1:0]                                      };
                    end
                end
            end
        end
    end
endgenerate
//    always@(posedge clk or negedge rstn) begin
//        if(~rstn) begin
//            lkp2bank_req_valid <= 'd0;
//            lkp2bank_req       <= 'd0;
//        end
//        else begin
//            if(lkp2bank_req_valid) begin
//                if(~lkp2bank_req_ready)
//                    lkp2bank_req_valid <= 1'b1;
//                else
//                    lkp2bank_req_valid <= 1'b0;
//            end
//            else begin
//                if(cs!=S_L0 & ns==S_L0) begin
//                    lkp2bank_req_valid        <= 1'b1;
//                    lkp2bank_req.req          <= lkp_req_i;
//                    lkp2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_L0_IDX_WIDTH){1'b0}},         idx_l0[BANK_L0_IDX_WIDTH+BANK_L0_SET_IDX_WIDTH-1:BANK_L0_SET_IDX_WIDTH]};
//                    lkp2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_L0_SET_IDX_WIDTH){1'b0}}, idx_l0[BANK_L0_SET_IDX_WIDTH-1:0]                                      };
//                end
//                else if(cs!=S_L1 & ns==S_L1) begin
//                    lkp2bank_req_valid        <= 1'b1;
//                    lkp2bank_req.req          <= req;
//                    lkp2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_L1_IDX_WIDTH){1'b0}},         idx_l1[BANK_L1_IDX_WIDTH+BANK_L1_SET_IDX_WIDTH-1:BANK_L1_SET_IDX_WIDTH]};
//                    lkp2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_L1_SET_IDX_WIDTH){1'b0}}, idx_l1[BANK_L1_SET_IDX_WIDTH-1:0]                                      };
//                end
//                else if(cs!=S_L2 & ns==S_L2) begin
//                    lkp2bank_req_valid        <= 1'b1;
//                    lkp2bank_req.req          <= req;
//                    lkp2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_L2_IDX_WIDTH){1'b0}},         idx_l2[BANK_L2_IDX_WIDTH+BANK_L2_SET_IDX_WIDTH-1:BANK_L2_SET_IDX_WIDTH]};
//                    lkp2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_L2_SET_IDX_WIDTH){1'b0}}, idx_l2[BANK_L2_SET_IDX_WIDTH-1:0]                                      };
//                end
//                else if(cs!=S_L3 & ns==S_L3) begin
//                    lkp2bank_req_valid        <= 1'b1;
//                    lkp2bank_req.req          <= req;
//                    lkp2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_L3_IDX_WIDTH){1'b0}},         idx_l3[BANK_L3_IDX_WIDTH+BANK_L3_SET_IDX_WIDTH-1:BANK_L3_SET_IDX_WIDTH]};
//                    lkp2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_L3_SET_IDX_WIDTH){1'b0}}, idx_l3[BANK_L3_SET_IDX_WIDTH-1:0]                                      };
//                end
//                else if(cs!=S_L4 & ns==S_L4) begin
//                    lkp2bank_req_valid        <= 1'b1;
//                    lkp2bank_req.req          <= req;
//                    lkp2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_L4_IDX_WIDTH){1'b0}},         idx_l4[BANK_L4_IDX_WIDTH+BANK_L4_SET_IDX_WIDTH-1:BANK_L4_SET_IDX_WIDTH]};
//                    lkp2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_L4_SET_IDX_WIDTH){1'b0}}, idx_l4[BANK_L4_SET_IDX_WIDTH-1:0]                                      };
//                end
//            end
//        end
//    end
    
    assign lkp2bank_req_valid_o[0] = lkp2bank_req_valid[0];
    assign lkp2bank_req_valid_o[1] = lkp2bank_req_valid[1];
    assign lkp2bank_req_valid_o[2] = lkp2bank_req_valid[2];
    assign lkp2bank_req_valid_o[3] = lkp2bank_req_valid[3];
    assign lkp2bank_req_valid_o[4] = lkp2bank_req_valid[4];
    assign lkp2bank_req_ready[0]   = lkp2bank_req_ready_i[0];
    assign lkp2bank_req_ready[1]   = lkp2bank_req_ready_i[1];
    assign lkp2bank_req_ready[2]   = lkp2bank_req_ready_i[2];
    assign lkp2bank_req_ready[3]   = lkp2bank_req_ready_i[3];
    assign lkp2bank_req_ready[4]   = lkp2bank_req_ready_i[4];
    assign lkp2bank_req_o[0]       = lkp2bank_req[0];
    assign lkp2bank_req_o[1]       = lkp2bank_req[1];
    assign lkp2bank_req_o[2]       = lkp2bank_req[2];
    assign lkp2bank_req_o[3]       = lkp2bank_req[3];
    assign lkp2bank_req_o[4]       = lkp2bank_req[4];


//}}}

//=== LKP ACK {{{
genvar j;
generate
    for(j=0; j<5; j++) begin : lkp_ack_got_gen
        always@(posedge clk or negedge rstn) begin
            if(~rstn) begin
                lkp_all_ack_got[j]  <= 1'b0;
                cur_bank2lkp_ack[j] <= 'd0;
            end
            else begin
                if(ns==S_IDLE) begin
                    lkp_all_ack_got[j]  <= 1'b0;
                    cur_bank2lkp_ack[j] <= 'd0;
                end
                else if(bank2lkp_ack_valid_i[j]) begin
                    lkp_all_ack_got[j]  <= 1'b1;
                    cur_bank2lkp_ack[j] <= bank2lkp_ack_i[j];
                end
            end
        end
    end
endgenerate

always@(*) begin
    lkp_fast_ack_got = 5'd0;
    fast_bank2lkp_ack= 'd0;
    if(lkp_all_ack_got[0] &  cur_bank2lkp_ack[0].hit) begin // got hit L0 translation, no wait for other bank
        lkp_fast_ack_got = 5'b00001;
        fast_bank2lkp_ack= cur_bank2lkp_ack[0];
    end
    else if(lkp_all_ack_got[1] &  cur_bank2lkp_ack[1].hit &
            lkp_all_ack_got[0] & ~cur_bank2lkp_ack[0].hit) begin// got hit L1 translation and L0 miss
        lkp_fast_ack_got = 5'b00010;
        fast_bank2lkp_ack= cur_bank2lkp_ack[1];
    end
    else if(lkp_all_ack_got[2] &  cur_bank2lkp_ack[2].hit &
            lkp_all_ack_got[1] & ~cur_bank2lkp_ack[1].hit &
            lkp_all_ack_got[0] & ~cur_bank2lkp_ack[0].hit) begin// got hit L2 translation and L1/L0 miss
        lkp_fast_ack_got = 5'b00100;
        fast_bank2lkp_ack= cur_bank2lkp_ack[2];
    end
    else if(lkp_all_ack_got[3] &  cur_bank2lkp_ack[3].hit &
            lkp_all_ack_got[2] & ~cur_bank2lkp_ack[2].hit &
            lkp_all_ack_got[1] & ~cur_bank2lkp_ack[1].hit &
            lkp_all_ack_got[0] & ~cur_bank2lkp_ack[0].hit) begin// got hit L2 translation and L1/L0 miss
        lkp_fast_ack_got = 5'b01000;
        fast_bank2lkp_ack= cur_bank2lkp_ack[3];
    end
    else if(lkp_all_ack_got[4] &  cur_bank2lkp_ack[4].hit &
            lkp_all_ack_got[3] & ~cur_bank2lkp_ack[3].hit &
            lkp_all_ack_got[2] & ~cur_bank2lkp_ack[2].hit &
            lkp_all_ack_got[1] & ~cur_bank2lkp_ack[1].hit &
            lkp_all_ack_got[0] & ~cur_bank2lkp_ack[0].hit) begin// got hit L2 translation and L1/L0 miss
        lkp_fast_ack_got = 5'b10000;
        fast_bank2lkp_ack= cur_bank2lkp_ack[4];
    end
    else begin
        lkp_fast_ack_got = 3'b000;
        fast_bank2lkp_ack= 'd0;
    end
end

generate
    if(CACHE_TYPE==0) begin : s2ptc_lkp_ack_o_gen
//        assign cur_bank2lkp_ack_pte_position =  (cur_bank2lkp_ack.ack.lvl=='d4) ? cur_bank2lkp_ack.ack.addr[50:48] :
//                                                (cur_bank2lkp_ack.ack.lvl=='d3) ? cur_bank2lkp_ack.ack.addr[41:39] :
//                                                (cur_bank2lkp_ack.ack.lvl=='d2) ? cur_bank2lkp_ack.ack.addr[32:30] :
//                                                (cur_bank2lkp_ack.ack.lvl=='d1) ? (csr_fctl_gxl_i==1'b1 ? cur_bank2lkp_ack.ack.addr[24:22] : cur_bank2lkp_ack.ack.addr[23:21]) :
//                                                (cur_bank2lkp_ack.ack.lvl=='d0) ? cur_bank2lkp_ack.ack.addr[14:12] : 3'b0;
//
        always@(posedge clk or negedge rstn) begin
            if(~rstn) begin
                lkp_ack_valid_o <= 1'b0;
                lkp_ack_o       <= 'd0;
                lkp_ack_hit_o   <= 1'b0;
            end
            else begin
                if(lkp_ack_valid_o) begin
                    if(~lkp_ack_ready_i)
                        lkp_ack_valid_o <= 1'b1;
                    else
                        lkp_ack_valid_o <= 1'b0;
                end
                else begin
                    if(cs!=S_ACK && ns==S_ACK) begin
                        lkp_ack_valid_o             <= 1'b1;
                        if(|lkp_fast_ack_got) begin
                            lkp_ack_hit_o           <= 1'b1;
                        end
                        else begin
                            lkp_ack_hit_o           <= 1'b0;
                        end
                        lkp_ack_o.idx               <= {4'd0, req.idx}                          ;
                        lkp_ack_o.gscid             <= req.gscid                        ;
                        lkp_ack_o.addr              <= req.addr                         ;
                        if(|lkp_fast_ack_got) begin
                            lkp_ack_o.lvl           <= fast_bank2lkp_ack.ack.lvl        ;
                            lkp_ack_o.prefetched    <= fast_bank2lkp_ack.ack.prefetched ;
                            lkp_ack_o.N             <= fast_bank2lkp_ack.ack.N          ;
                            lkp_ack_o.PBMT          <= fast_bank2lkp_ack.ack.PBMT       ;
                            lkp_ack_o.D             <= fast_bank2lkp_ack.ack.D          ;
                            lkp_ack_o.A             <= fast_bank2lkp_ack.ack.A          ;
                            lkp_ack_o.PPN           <= fast_bank2lkp_ack.ack.PPN        ;
                            lkp_ack_o.PERM          <= fast_bank2lkp_ack.ack.PERM       ;
                            lkp_ack_o.V             <= fast_bank2lkp_ack.ack.V          ;
                        end
                        else begin
                            lkp_ack_o.lvl           <= 'd0;
                            lkp_ack_o.prefetched    <= 'd0;
                            lkp_ack_o.N             <= 'd0;
                            lkp_ack_o.PBMT          <= 'd0;
                            lkp_ack_o.D             <= 'd0;
                            lkp_ack_o.A             <= 'd0;
                            lkp_ack_o.PPN           <= 'd0;
                            lkp_ack_o.PERM          <= 'd0;
                            lkp_ack_o.V             <= 'd0;
                        end
                    end
                end
            end
        end
    end
    else begin : s1ptc_lkp_ack_o_gen
//        assign cur_bank2lkp_ack_pte_position =  (cur_bank2lkp_ack.ack.lvl=='d4) ? cur_bank2lkp_ack.ack.addr[50:48] :
//                                                (cur_bank2lkp_ack.ack.lvl=='d3) ? cur_bank2lkp_ack.ack.addr[41:39] :
//                                                (cur_bank2lkp_ack.ack.lvl=='d2) ? cur_bank2lkp_ack.ack.addr[32:30] :
//                                                (cur_bank2lkp_ack.ack.lvl=='d1) ? (cur_bank2lkp_ack.ack.sxl==1'b1 ? cur_bank2lkp_ack.ack.addr[24:22] : cur_bank2lkp_ack.ack.addr[23:21]) :
//                                                (cur_bank2lkp_ack.ack.lvl=='d0) ? cur_bank2lkp_ack.ack.addr[14:12] : 3'b0;

        always@(posedge clk or negedge rstn) begin
            if(~rstn) begin
                lkp_ack_valid_o <= 1'b0;
                lkp_ack_o       <= 'd0;
                lkp_ack_hit_o   <= 1'b0;
            end
            else begin
                if(lkp_ack_valid_o) begin
                    if(~lkp_ack_ready_i)
                        lkp_ack_valid_o <= 1'b1;
                    else
                        lkp_ack_valid_o <= 1'b0;
                end
                else begin
                    if(cs!=S_ACK && ns==S_ACK) begin
                        lkp_ack_valid_o                 <= 1'b1;
                        if(|lkp_fast_ack_got) begin
                            lkp_ack_hit_o               <= 1'b1;
                        end
                        else begin
                            lkp_ack_hit_o               <= 1'b0;
                        end
                        lkp_ack_o.idx                   <= {4'd0, req.idx}                          ;
                        lkp_ack_o.gscid                 <= req.gscid                        ;
                        lkp_ack_o.pscid                 <= req.pscid                        ;
                        lkp_ack_o.addr                  <= req.addr                         ;
                        if(|lkp_fast_ack_got) begin
                            lkp_ack_o.gv                <= fast_bank2lkp_ack.ack.gv         ;
                            lkp_ack_o.lvl               <= fast_bank2lkp_ack.ack.lvl        ;
                            lkp_ack_o.prefetched        <= fast_bank2lkp_ack.ack.prefetched ;
                            lkp_ack_o.sxl               <= fast_bank2lkp_ack.ack.sxl        ;
                            lkp_ack_o.N                 <= fast_bank2lkp_ack.ack.N          ;
                            lkp_ack_o.PBMT              <= fast_bank2lkp_ack.ack.PBMT       ;
                            lkp_ack_o.D                 <= fast_bank2lkp_ack.ack.D          ;
                            lkp_ack_o.A                 <= fast_bank2lkp_ack.ack.A          ;
                            lkp_ack_o.PPN               <= fast_bank2lkp_ack.ack.PPN        ;
                            lkp_ack_o.PERM              <= fast_bank2lkp_ack.ack.PERM       ;
                            lkp_ack_o.V                 <= fast_bank2lkp_ack.ack.V          ;
                        end
                        else begin
                            lkp_ack_o.gv                <= 'd0;
                            lkp_ack_o.lvl               <= 'd0;
                            lkp_ack_o.prefetched        <= 'd0;
                            lkp_ack_o.sxl               <= 'd0;
                            lkp_ack_o.N                 <= 'd0;
                            lkp_ack_o.PBMT              <= 'd0;
                            lkp_ack_o.D                 <= 'd0;
                            lkp_ack_o.A                 <= 'd0;
                            lkp_ack_o.PPN               <= 'd0;
                            lkp_ack_o.PERM              <= 'd0;
                            lkp_ack_o.V                 <= 'd0;
                        end
                    end
                end
            end
        end
    end
endgenerate
//}}}

//}}}

//=== IDX inst {{{
genvar k;
generate
    if(CACHE_TYPE==0) begin : s2ptc_idx_gen
        assign idx_gv       = 1'b1;
        assign idx_gscid    = (cs==S_IDLE) ? lkp_req_i.gscid    : req.gscid;
        assign idx_pscid    = 'd0;
        assign idx_addr     = (cs==S_IDLE) ? lkp_req_i.addr     : req.addr;
//        assign idx_lvl              = ns==S_L0 ? 3'd0 :
//                                      ns==S_L1 ? 3'd1 :
//                                      ns==S_L2 ? 3'd2 :
//                                      ns==S_L3 ? 3'd3 :
//                                      ns==S_L4 ? 3'd4 :
//                                                 3'd0;
        assign idx_l0 = idx[0][BANK_L0_IDX_WIDTH+BANK_L0_SET_IDX_WIDTH-1:0];
        assign idx_l1 = idx[1][BANK_L1_IDX_WIDTH+BANK_L1_SET_IDX_WIDTH-1:0];
        assign idx_l2 = idx[2][BANK_L2_IDX_WIDTH+BANK_L2_SET_IDX_WIDTH-1:0];
        assign idx_l3 = idx[3][BANK_L3_IDX_WIDTH+BANK_L3_SET_IDX_WIDTH-1:0];
        assign idx_l4 = idx[4][BANK_L4_IDX_WIDTH+BANK_L4_SET_IDX_WIDTH-1:0];
        for(k=0; k<5; k++) begin : idx_gen
            assign idx_lvl[k] = 3'(k);
            iommu_atd_ptc_mtlb_idx #(
            /*parameter  */ .CACHE_TYPE     (CACHE_TYPE                                 ), // 0:S2PTC, 1:S1PTC
            /*parameter  */ .IDX_WIDTH      (MAX_BANK_IDX_WIDTH+MAX_BANK_SET_IDX_WIDTH  ) // = 7
            ) U_idx_gen(
            /*input  logic                          */  .gv                 (idx_gv                 ),
            /*input  logic [15:0]                   */  .gscid              (idx_gscid              ),
            /*input  logic [19:0]                   */  .pscid              (idx_pscid              ),
            /*input  logic [63:12]                  */  .addr               (idx_addr               ),
            /*input  logic [2:0]                    */  .lvl                (idx_lvl[k]             ),
            /*output logic [IDX_WIDTH-1:0]          */  .idx                (idx[k]                 ) 
            );
        end
    end
    else begin
        assign idx_gv       = (cs==S_IDLE) ? lkp_req_i.gv       : req.gv;
        assign idx_gscid    = (cs==S_IDLE) ? lkp_req_i.gscid    : req.gscid;
        assign idx_pscid    = (cs==S_IDLE) ? lkp_req_i.pscid    : req.pscid;
        assign idx_addr     = (cs==S_IDLE) ? lkp_req_i.addr     : req.addr;
//        assign idx_lvl              = ns==S_L0 ? 3'd0 :
//                                      ns==S_L1 ? 3'd1 :
//                                      ns==S_L2 ? 3'd2 :
//                                      ns==S_L3 ? 3'd3 :
//                                      ns==S_L4 ? 3'd4 :
//                                                 3'd0;
        assign idx_l0 = idx[0][BANK_L0_IDX_WIDTH+BANK_L0_SET_IDX_WIDTH-1:0];
        assign idx_l1 = idx[1][BANK_L1_IDX_WIDTH+BANK_L1_SET_IDX_WIDTH-1:0];
        assign idx_l2 = idx[2][BANK_L2_IDX_WIDTH+BANK_L2_SET_IDX_WIDTH-1:0];
        assign idx_l3 = idx[3][BANK_L3_IDX_WIDTH+BANK_L3_SET_IDX_WIDTH-1:0];
        assign idx_l4 = idx[4][BANK_L4_IDX_WIDTH+BANK_L4_SET_IDX_WIDTH-1:0];
        for(k=0; k<5; k++) begin
            assign idx_lvl[k] = 3'(k);
            iommu_atd_ptc_mtlb_idx #(
            /*parameter  */ .CACHE_TYPE     (CACHE_TYPE                                 ), // 0:S2PTC, 1:S2PTC
            /*parameter  */ .IDX_WIDTH      (MAX_BANK_IDX_WIDTH+MAX_BANK_SET_IDX_WIDTH  ) // = 7
            ) U_idx_gen(
            /*input  logic                          */  .gv                 (idx_gv                 ),
            /*input  logic [15:0]                   */  .gscid              (idx_gscid              ),
            /*input  logic [19:0]                   */  .pscid              (idx_pscid              ),
            /*input  logic [63:12]                  */  .addr               (idx_addr               ),
            /*input  logic [2:0]                    */  .lvl                (idx_lvl[k]             ),
            /*output logic [IDX_WIDTH-1:0]          */  .idx                (idx[k]                 ) 
            );
        end
    end
endgenerate
//}}}

//}}}

endmodule
