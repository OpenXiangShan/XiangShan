module iommu_atd_dtc_mtlb_lkp_cabin_wrap #(
    parameter  CACHE_TYPE                  = 0, // 0:DDTC, 1:PDTC
    parameter type          LOOKUP_REQ_TYPE             = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE             = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE             = iommu_atd_cache_pkg::ddtc_update_req_t,
    parameter type          BANK_LKP_REQ_TYPE           = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    parameter type          BANK_LKP_ACK_TYPE           = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    parameter  CABIN_IDX_WIDTH             = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    parameter  BANK_L0_IDX_WIDTH           = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH,
    parameter  BANK_L1_IDX_WIDTH           = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH,
    parameter  BANK_L2_IDX_WIDTH           = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH,
    parameter  BANK_L0_SET_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    parameter  BANK_L1_SET_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    parameter  BANK_L2_SET_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    parameter  CABIN_NUM                   = 2**CABIN_IDX_WIDTH,
    parameter  BANK_L0_NUM                 = 2**BANK_L0_IDX_WIDTH,
    parameter  BANK_L1_NUM                 = 2**BANK_L1_IDX_WIDTH,
    parameter  BANK_L2_NUM                 = 2**BANK_L2_IDX_WIDTH,
    parameter  BANK_L0_SET_NUM             = 2**BANK_L0_SET_IDX_WIDTH,
    parameter  BANK_L1_SET_NUM             = 2**BANK_L1_SET_IDX_WIDTH,
    parameter  BANK_L2_SET_NUM             = 2**BANK_L2_SET_IDX_WIDTH,
    parameter  SPARE_PARAM                 = 0
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
    output logic             [2:0] [CABIN_NUM-1:0]      lkp2bank_req_valid_o,   // [0]: BANK_L0, [1]: BANK_L1, [2]:BANK_L2
    input  logic             [2:0] [CABIN_NUM-1:0]      lkp2bank_req_ready_i,
    output BANK_LKP_REQ_TYPE [2:0] [CABIN_NUM-1:0]      lkp2bank_req_o,
    // BANK ACK                                         
    input  logic             [2:0] [CABIN_NUM-1:0]      bank2lkp_ack_valid_i,
    input  BANK_LKP_ACK_TYPE [2:0] [CABIN_NUM-1:0]      bank2lkp_ack_i,
    // BANK STATUS INPUT
    input  logic [2:0]                                  bank_valid_i,
    //
    input  logic                                        spare_in
//}}}
);
//=== Declare {{{
    typedef struct packed {
        logic [2:0]                                     valid;
        logic [2:0]                                     ready;
        BANK_LKP_REQ_TYPE [2:0]                         req;
    } cabin_bank_lkp_req_t;

    typedef struct packed {
        logic [2:0]                                     valid;
        BANK_LKP_ACK_TYPE [2:0]                         ack;
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
    /*parameter    */               .ARB_TYPE            (0                      ),   //= 0,
    /*parameter    */               .REQ_NUM             (CABIN_NUM              ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (lkp_ack_out_t          ),   //= logic,
    /*parameter    */               .AXIVLDRDY           (1                      )    //= 1
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
        iommu_atd_dtc_mtlb_lkp_cabin #(
        /*parameter */              .CACHE_TYPE                 (CACHE_TYPE                 ), // = 0, // 0:DDTC, 1:PDTC
        /*parameter type         */ .LOOKUP_REQ_TYPE            (LOOKUP_REQ_TYPE            ), // = iommu_atd_cache_pkg::ddtc_lookup_req_t  ,
        /*parameter type         */ .LOOKUP_ACK_TYPE            (LOOKUP_ACK_TYPE            ), // = iommu_atd_cache_pkg::ddtc_lookup_ack_t  ,
        /*parameter type         */ .UPDATE_REQ_TYPE            (UPDATE_REQ_TYPE            ), // = iommu_atd_cache_pkg::ddtc_update_req_t,
        /*parameter type         */ .BANK_LKP_REQ_TYPE          (BANK_LKP_REQ_TYPE          ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
        /*parameter type         */ .BANK_LKP_ACK_TYPE          (BANK_LKP_ACK_TYPE          ), // = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
        /*parameter */              .CABIN_IDX_WIDTH            (CABIN_IDX_WIDTH            ), // = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
        /*parameter */              .BANK_L0_IDX_WIDTH          (BANK_L0_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH,
        /*parameter */              .BANK_L1_IDX_WIDTH          (BANK_L1_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH,
        /*parameter */              .BANK_L2_IDX_WIDTH          (BANK_L2_IDX_WIDTH          ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH,
        /*parameter */              .BANK_L0_SET_IDX_WIDTH      (BANK_L0_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
        /*parameter */              .BANK_L1_SET_IDX_WIDTH      (BANK_L1_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
        /*parameter */              .BANK_L2_SET_IDX_WIDTH      (BANK_L2_SET_IDX_WIDTH      ), // = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
        /*parameter */              .SPARE_PARAM                (1'b0                       )  // = 0
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
        /*output logic [2:0]                                */  .lkp2bank_req_valid_o       (cabin_lkp2bank_req[i].valid    ),   // [0]: BANK_L0, [1]: BANK_L1, [2]:BANK_L2
        /*input  logic [2:0]                                */  .lkp2bank_req_ready_i       (cabin_lkp2bank_req[i].ready    ),
        /*output BANK_LKP_REQ_TYPE [2:0]                    */  .lkp2bank_req_o             (cabin_lkp2bank_req[i].req      ),
        /*input  logic [2:0]                                */  .bank2lkp_ack_valid_i       (cabin_bank2lkp_ack[i].valid    ),
        /*input  BANK_LKP_ACK_TYPE [2:0]                    */  .bank2lkp_ack_i             (cabin_bank2lkp_ack[i].ack      ),
        /*output logic                                      */  .valid_o                    (cabin_valid_o               [i]),
        /*input  logic [3:0]                                */  .bank_valid_i               (bank_valid_i                   ),
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



module iommu_atd_dtc_mtlb_lkp_cabin #(
    parameter               CACHE_TYPE                  = 0, // 0:DDTC, 1:PDTC
    parameter type          LOOKUP_REQ_TYPE             = iommu_atd_cache_pkg::ddtc_lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE             = iommu_atd_cache_pkg::ddtc_lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE             = iommu_atd_cache_pkg::ddtc_update_req_t,
    parameter type          BANK_LKP_REQ_TYPE           = iommu_atd_cache_pkg::ddtc_bank_lkp_req_t,
    parameter type          BANK_LKP_ACK_TYPE           = iommu_atd_cache_pkg::ddtc_bank_lkp_ack_t,
    parameter               CABIN_IDX_WIDTH             = iommu_atd_cache_pkg::DDTC_CABIN_LKP_IDX_WIDTH,
    parameter               BANK_L0_IDX_WIDTH           = iommu_atd_cache_pkg::DDTC_BANK_L0_IDX_WIDTH,
    parameter               BANK_L1_IDX_WIDTH           = iommu_atd_cache_pkg::DDTC_BANK_L1_IDX_WIDTH,
    parameter               BANK_L2_IDX_WIDTH           = iommu_atd_cache_pkg::DDTC_BANK_L2_IDX_WIDTH,
    parameter               BANK_L0_SET_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L0_SET_IDX_WIDTH,
    parameter               BANK_L1_SET_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L1_SET_IDX_WIDTH,
    parameter               BANK_L2_SET_IDX_WIDTH       = iommu_atd_cache_pkg::DDTC_BANK_L2_SET_IDX_WIDTH,
    parameter               BANK_L0_NUM                 = 2**BANK_L0_IDX_WIDTH,
    parameter               BANK_L1_NUM                 = 2**BANK_L1_IDX_WIDTH,
    parameter               BANK_L2_NUM                 = 2**BANK_L2_IDX_WIDTH,
    parameter               BANK_L0_SET_NUM             = 2**BANK_L0_SET_IDX_WIDTH,
    parameter               BANK_L1_SET_NUM             = 2**BANK_L1_SET_IDX_WIDTH,
    parameter               BANK_L2_SET_NUM             = 2**BANK_L2_SET_IDX_WIDTH,
    parameter               SPARE_PARAM                 = 0
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
    output logic [2:0]                                  lkp2bank_req_valid_o,   // [0]: BANK_L0, [1]: BANK_L1, [2]:BANK_L2
    input  logic [2:0]                                  lkp2bank_req_ready_i,
    output BANK_LKP_REQ_TYPE [2:0]                      lkp2bank_req_o,
    // BANK ACK                                         
    input  logic [2:0]                                  bank2lkp_ack_valid_i,
    input  BANK_LKP_ACK_TYPE [2:0]                      bank2lkp_ack_i,
    // VALID OUT
    output logic                                        valid_o,
    // BANK STATUS INPUT
    input  logic [2:0]                                  bank_valid_i,
    //
    input  logic                                        spare_in
//}}}
);
//=== Declare {{{
    localparam MAX_BANK_IDX_WIDTH_0 = (BANK_L0_IDX_WIDTH > BANK_L1_IDX_WIDTH) ? BANK_L0_IDX_WIDTH : BANK_L1_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH_1 = BANK_L2_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH   = (MAX_BANK_IDX_WIDTH_0 > MAX_BANK_IDX_WIDTH_1) ? MAX_BANK_IDX_WIDTH_0 : MAX_BANK_IDX_WIDTH_1;

    localparam MAX_BANK_SET_IDX_WIDTH_0 = (BANK_L0_SET_IDX_WIDTH > BANK_L1_SET_IDX_WIDTH) ? BANK_L0_SET_IDX_WIDTH : BANK_L1_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH_1 = BANK_L2_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH   = (MAX_BANK_SET_IDX_WIDTH_0 > MAX_BANK_SET_IDX_WIDTH_1) ? MAX_BANK_SET_IDX_WIDTH_0 : MAX_BANK_SET_IDX_WIDTH_1;

    localparam S_IDLE = 4'b000;
//    localparam S_L0   = 4'b001;
//    localparam S_L1   = 4'b010;
//    localparam S_L2   = 4'b011;
    localparam S_ACK  = 4'b110;
    localparam S_ALL  = 4'b111;
    localparam S_WAIT_ALL_ACK = 4'b101;

    logic [3:0]                                         cs, ns;
    LOOKUP_REQ_TYPE                                     req;

    logic [23:0]                                        idx_device_id;
    logic [19:0]                                        idx_process_id;
    logic [1:0]                                         idx_lvl[2:0];
    logic [MAX_BANK_IDX_WIDTH+MAX_BANK_SET_IDX_WIDTH-1:0] idx[2:0];
    logic [BANK_L0_IDX_WIDTH+BANK_L0_SET_IDX_WIDTH-1:0] idx_l0;
    logic [BANK_L1_IDX_WIDTH+BANK_L1_SET_IDX_WIDTH-1:0] idx_l1;
    logic [BANK_L2_IDX_WIDTH+BANK_L2_SET_IDX_WIDTH-1:0] idx_l2;

    BANK_LKP_ACK_TYPE                                   cur_bank2lkp_ack[2:0];
    BANK_LKP_ACK_TYPE                                   fast_bank2lkp_ack;

    logic                                               lkp2bank_req_valid[2:0];
    logic                                               lkp2bank_req_ready[2:0];
    BANK_LKP_REQ_TYPE                                   lkp2bank_req[2:0];

    logic [2:0]                                         lkp_all_ack_got, lkp_fast_ack_got;


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
//                    else
//                        ns = S_IDLE;
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
//                    else
//                        ns = S_IDLE;
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
//                    ns = S_IDLE;
//                end
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
            if(cs!=S_IDLE & ns==S_IDLE)
                valid_o <= 1'b0;
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
    for(i=0; i<3; i++) begin : lkp2bank_req_gen
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
                                                              {{(MAX_BANK_IDX_WIDTH-BANK_L0_IDX_WIDTH){1'b0}},         idx_l0[BANK_L0_IDX_WIDTH+BANK_L0_SET_IDX_WIDTH-1:BANK_L0_SET_IDX_WIDTH]};
                        lkp2bank_req[i].bank_set_idx<= i==0 ? {{(MAX_BANK_SET_IDX_WIDTH-BANK_L0_SET_IDX_WIDTH){1'b0}}, idx_l0[BANK_L0_SET_IDX_WIDTH-1:0]                                      } :
                                                       i==1 ? {{(MAX_BANK_SET_IDX_WIDTH-BANK_L1_SET_IDX_WIDTH){1'b0}}, idx_l1[BANK_L1_SET_IDX_WIDTH-1:0]                                      } :
                                                       i==2 ? {{(MAX_BANK_SET_IDX_WIDTH-BANK_L2_SET_IDX_WIDTH){1'b0}}, idx_l2[BANK_L2_SET_IDX_WIDTH-1:0]                                      } :
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
//            end
//        end
//    end
    
    assign lkp2bank_req_valid_o[0] = lkp2bank_req_valid[0];
    assign lkp2bank_req_valid_o[1] = lkp2bank_req_valid[1];
    assign lkp2bank_req_valid_o[2] = lkp2bank_req_valid[2];
    assign lkp2bank_req_ready[0]   = lkp2bank_req_ready_i[0];
    assign lkp2bank_req_ready[1]   = lkp2bank_req_ready_i[1];
    assign lkp2bank_req_ready[2]   = lkp2bank_req_ready_i[2];
    assign lkp2bank_req_o[0]       = lkp2bank_req[0];
    assign lkp2bank_req_o[1]       = lkp2bank_req[1];
    assign lkp2bank_req_o[2]       = lkp2bank_req[2];


//}}}

//=== LKP ACK {{{
genvar j;
generate
    for(j=0; j<3; j++) begin : lkp_ack_got_gen
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
    lkp_fast_ack_got = 3'd0;
    fast_bank2lkp_ack= 'd0;
    if(lkp_all_ack_got[0] &  cur_bank2lkp_ack[0].hit) begin // got hit L0 translation, no wait for other bank
        lkp_fast_ack_got = 3'b001;
        fast_bank2lkp_ack= cur_bank2lkp_ack[0];
    end
    else if(lkp_all_ack_got[1] &  cur_bank2lkp_ack[1].hit &
            lkp_all_ack_got[0] & ~cur_bank2lkp_ack[0].hit) begin// got hit L1 translation and L0 miss
        lkp_fast_ack_got = 3'b010;
        fast_bank2lkp_ack= cur_bank2lkp_ack[1];
    end
    else if(lkp_all_ack_got[2] &  cur_bank2lkp_ack[2].hit &
            lkp_all_ack_got[1] & ~cur_bank2lkp_ack[1].hit &
            lkp_all_ack_got[0] & ~cur_bank2lkp_ack[0].hit) begin// got hit L2 translation and L1/L0 miss
        lkp_fast_ack_got = 3'b100;
        fast_bank2lkp_ack= cur_bank2lkp_ack[2];
    end
    else begin
        lkp_fast_ack_got = 3'b000;
        fast_bank2lkp_ack= 'd0;
    end
end

generate
    if(CACHE_TYPE==0) begin : ddtc_lkp_ack_o_gen
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
                        lkp_ack_o.idx               <= {1'b0, req.idx}                      ;
                        lkp_ack_o.device_id         <= req.device_id                        ;
                        if(|lkp_fast_ack_got) begin
                            lkp_ack_o.lvl               <= fast_bank2lkp_ack.ack.lvl             ;
                            lkp_ack_o.prefetched        <= fast_bank2lkp_ack.ack.prefetched      ;
                            lkp_ack_o.msi_addr_pattern  <= fast_bank2lkp_ack.ack.msi_addr_pattern;
                            lkp_ack_o.msi_addr_mask     <= fast_bank2lkp_ack.ack.msi_addr_mask   ;
                            lkp_ack_o.msipip_mode       <= fast_bank2lkp_ack.ack.msipip_mode     ;
                            lkp_ack_o.msipip_ppn        <= fast_bank2lkp_ack.ack.msipip_ppn      ;
                            lkp_ack_o.fsc_mode          <= fast_bank2lkp_ack.ack.fsc_mode        ;
                            lkp_ack_o.fsc_ppn           <= fast_bank2lkp_ack.ack.fsc_ppn         ;
                            lkp_ack_o.PSCID             <= fast_bank2lkp_ack.ack.PSCID           ;
                            lkp_ack_o.S2MODE            <= fast_bank2lkp_ack.ack.S2MODE          ;
                            lkp_ack_o.GSCID             <= fast_bank2lkp_ack.ack.GSCID           ;
                            lkp_ack_o.S2PPN             <= fast_bank2lkp_ack.ack.S2PPN           ;
                            lkp_ack_o.SXL               <= fast_bank2lkp_ack.ack.SXL             ;
                            lkp_ack_o.SBE               <= fast_bank2lkp_ack.ack.SBE             ;
                            lkp_ack_o.DPE               <= fast_bank2lkp_ack.ack.DPE             ;
                            lkp_ack_o.SADE              <= fast_bank2lkp_ack.ack.SADE            ;
                            lkp_ack_o.GADE              <= fast_bank2lkp_ack.ack.GADE            ;
                            lkp_ack_o.PRPR              <= fast_bank2lkp_ack.ack.PRPR            ;
                            lkp_ack_o.PDTV              <= fast_bank2lkp_ack.ack.PDTV            ;
                            lkp_ack_o.DTF               <= fast_bank2lkp_ack.ack.DTF             ;
                            lkp_ack_o.T2GPA             <= fast_bank2lkp_ack.ack.T2GPA           ;
                            lkp_ack_o.EN_PRI            <= fast_bank2lkp_ack.ack.EN_PRI          ;
                            lkp_ack_o.EN_ATS            <= fast_bank2lkp_ack.ack.EN_ATS          ;
                            lkp_ack_o.V                 <= fast_bank2lkp_ack.ack.V               ;
                        end
                        else begin
                            lkp_ack_o.lvl               <= 'd0;
                            lkp_ack_o.prefetched        <= 'd0;
                            lkp_ack_o.msi_addr_pattern  <= 'd0;
                            lkp_ack_o.msi_addr_mask     <= 'd0;
                            lkp_ack_o.msipip_mode       <= 'd0;
                            lkp_ack_o.msipip_ppn        <= 'd0;
                            lkp_ack_o.fsc_mode          <= 'd0;
                            lkp_ack_o.fsc_ppn           <= 'd0;
                            lkp_ack_o.PSCID             <= 'd0;
                            lkp_ack_o.S2MODE            <= 'd0;
                            lkp_ack_o.GSCID             <= 'd0;
                            lkp_ack_o.S2PPN             <= 'd0;
                            lkp_ack_o.SXL               <= 'd0;
                            lkp_ack_o.SBE               <= 'd0;
                            lkp_ack_o.DPE               <= 'd0;
                            lkp_ack_o.SADE              <= 'd0;
                            lkp_ack_o.GADE              <= 'd0;
                            lkp_ack_o.PRPR              <= 'd0;
                            lkp_ack_o.PDTV              <= 'd0;
                            lkp_ack_o.DTF               <= 'd0;
                            lkp_ack_o.T2GPA             <= 'd0;
                            lkp_ack_o.EN_PRI            <= 'd0;
                            lkp_ack_o.EN_ATS            <= 'd0;
                            lkp_ack_o.V                 <= 'd0;
                        end
                    end
                end
            end
        end
    end
    else begin : pdtc_lkp_ack_o_gen
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
                        lkp_ack_o.idx               <= {1'b0, req.idx}                      ;
                        lkp_ack_o.device_id         <= req.device_id                        ;
                        lkp_ack_o.process_id        <= req.process_id                       ;
                        if(|lkp_fast_ack_got) begin
                            lkp_ack_o.lvl               <= fast_bank2lkp_ack.ack.lvl             ;
                            lkp_ack_o.prefetched        <= fast_bank2lkp_ack.ack.prefetched      ;
                            lkp_ack_o.fsc_mode          <= fast_bank2lkp_ack.ack.fsc_mode        ;
                            lkp_ack_o.fsc_ppn           <= fast_bank2lkp_ack.ack.fsc_ppn         ;
                            lkp_ack_o.PSCID             <= fast_bank2lkp_ack.ack.PSCID           ;
                            lkp_ack_o.SUM               <= fast_bank2lkp_ack.ack.SUM             ;
                            lkp_ack_o.ENS               <= fast_bank2lkp_ack.ack.ENS             ;
                            lkp_ack_o.V                 <= fast_bank2lkp_ack.ack.V               ;
                        end
                        else begin
                            lkp_ack_o.lvl               <= 'd0;
                            lkp_ack_o.prefetched        <= 'd0;
                            lkp_ack_o.fsc_mode          <= 'd0;
                            lkp_ack_o.fsc_ppn           <= 'd0;
                            lkp_ack_o.PSCID             <= 'd0;
                            lkp_ack_o.SUM               <= 'd0;
                            lkp_ack_o.ENS               <= 'd0;
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
    if(CACHE_TYPE==0) begin : ddtc_idx_gen
        assign idx_device_id        = (cs==S_IDLE) ? lkp_req_i.device_id        : req.device_id;
        assign idx_process_id       = 'd0;
//        assign idx_lvl              = ns==S_L0 ? 2'd0 :
//                                      ns==S_L1 ? 2'd1 :
//                                      ns==S_L2 ? 2'd2 : 2'd0;
        assign idx_l0 = idx[0][BANK_L0_IDX_WIDTH+BANK_L0_SET_IDX_WIDTH-1:0];
        assign idx_l1 = idx[1][BANK_L1_IDX_WIDTH+BANK_L1_SET_IDX_WIDTH-1:0];
        assign idx_l2 = idx[2][BANK_L2_IDX_WIDTH+BANK_L2_SET_IDX_WIDTH-1:0];
        for(k=0; k<3; k++) begin : idx_gen
            assign idx_lvl[k] = 2'(k);
            iommu_atd_dtc_mtlb_idx #(
            /*parameter */ .CACHE_TYPE     (CACHE_TYPE                                 ), // 0:DDTC, 1:PDTC
            /*parameter */ .IDX_WIDTH      (MAX_BANK_IDX_WIDTH+MAX_BANK_SET_IDX_WIDTH  ) // = 7
            ) U_idx_gen(
            /*input  logic [23:0]                   */  .device_id          (idx_device_id          ),
            /*input  logic [19:0]                   */  .process_id         (idx_process_id         ),
            /*input  logic [1:0]                    */  .lvl                (idx_lvl[k]             ),
            /*output logic [IDX_WIDTH-1:0]          */  .idx                (idx[k]                 ) 
            );
        end
    end
    else begin : pdtc_idx_gen
        assign idx_device_id        = (cs==S_IDLE) ? lkp_req_i.device_id        : req.device_id;
        assign idx_process_id       = (cs==S_IDLE) ? lkp_req_i.process_id       : req.process_id;
//        assign idx_lvl              = ns==S_L0 ? 2'd0 :
//                                      ns==S_L1 ? 2'd1 :
//                                      ns==S_L2 ? 2'd2 : 2'd0;
        assign idx_l0 = idx[0][BANK_L0_IDX_WIDTH+BANK_L0_SET_IDX_WIDTH-1:0];
        assign idx_l1 = idx[1][BANK_L1_IDX_WIDTH+BANK_L1_SET_IDX_WIDTH-1:0];
        assign idx_l2 = idx[2][BANK_L2_IDX_WIDTH+BANK_L2_SET_IDX_WIDTH-1:0];
        for(k=0; k<3; k++) begin : idx_gen
            assign idx_lvl[k] = 2'(k);
            iommu_atd_dtc_mtlb_idx #(
            /*parameter */ .CACHE_TYPE     (CACHE_TYPE                                 ), // 0:DDTC, 1:PDTC
            /*parameter */ .IDX_WIDTH      (MAX_BANK_IDX_WIDTH+MAX_BANK_SET_IDX_WIDTH  ) // = 7
            ) U_idx_gen(
            /*input  logic [23:0]                   */  .device_id          (idx_device_id          ),
            /*input  logic [19:0]                   */  .process_id         (idx_process_id         ),
            /*input  logic [1:0]                    */  .lvl                (idx_lvl[k]             ),
            /*output logic [IDX_WIDTH-1:0]          */  .idx                (idx[k]                 ) 
            );
        end
    end
endgenerate
//}}}

//}}}

endmodule
