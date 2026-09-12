module iommu_acd_mtlb_lkp_cabin_wrap #(
    parameter type          LOOKUP_REQ_TYPE             = iommu_acd_pkg::lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE             = iommu_acd_pkg::lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE             = iommu_acd_pkg::update_req_t,
    parameter type          BANK_LKP_REQ_TYPE           = iommu_acd_pkg::bank_lkp_req_t,
    parameter type          BANK_LKP_ACK_TYPE           = iommu_acd_pkg::bank_lkp_ack_t,
    parameter   CABIN_IDX_WIDTH             = 3,
    parameter   BANK_4K_IDX_WIDTH           = 3,
    parameter   BANK_2M_IDX_WIDTH           = 2,
    parameter   BANK_1G_IDX_WIDTH           = 2,
    parameter   BANK_0T_IDX_WIDTH           = 1,
    parameter   BANK_4K_SET_IDX_WIDTH       = 3,
    parameter   BANK_2M_SET_IDX_WIDTH       = 2,
    parameter   BANK_1G_SET_IDX_WIDTH       = 1,
    parameter   BANK_0T_SET_IDX_WIDTH       = 1,
    parameter   CABIN_NUM                   = 2**CABIN_IDX_WIDTH,
    parameter   BANK_4K_NUM                 = 2**BANK_4K_IDX_WIDTH,
    parameter   BANK_2M_NUM                 = 2**BANK_2M_IDX_WIDTH,
    parameter   BANK_1G_NUM                 = 2**BANK_1G_IDX_WIDTH,
    parameter   BANK_0T_NUM                 = 2**BANK_0T_IDX_WIDTH,
    parameter   BANK_4K_SET_NUM             = 2**BANK_4K_SET_IDX_WIDTH,
    parameter   BANK_2M_SET_NUM             = 2**BANK_2M_SET_IDX_WIDTH,
    parameter   BANK_1G_SET_NUM             = 2**BANK_1G_SET_IDX_WIDTH,
    parameter   BANK_0T_SET_NUM             = 2**BANK_0T_SET_IDX_WIDTH,
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
    output logic             [3:0] [CABIN_NUM-1:0]      lkp2bank_req_valid_o,   // [0]: BANK_4K, [1]: BANK_2M, [2]:BANK_1G, [3]:BANK_512G
    input  logic             [3:0] [CABIN_NUM-1:0]      lkp2bank_req_ready_i,
    output BANK_LKP_REQ_TYPE [3:0] [CABIN_NUM-1:0]      lkp2bank_req_o,
    // BANK ACK                                         
    input  logic             [3:0] [CABIN_NUM-1:0]      bank2lkp_ack_valid_i,
    input  BANK_LKP_ACK_TYPE [3:0] [CABIN_NUM-1:0]      bank2lkp_ack_i,
    // BANK STATUS INPUT
    input  logic [3:0]                                  bank_valid_i,
    //
    input  logic                                        spare_in
//}}}
);
//=== Declare {{{
    typedef struct packed {
        logic [3:0]                                     valid;
        logic [3:0]                                     ready;
        BANK_LKP_REQ_TYPE [3:0]                         req;
    } cabin_bank_lkp_req_t;

    typedef struct packed {
        logic [3:0]                                     valid;
        BANK_LKP_ACK_TYPE [3:0]                         ack;
    } cabin_bank_lkp_ack_t;

    typedef struct packed {
        UPDATE_REQ_TYPE                                 ack;
        logic                                           hit;
    } lkp_ack_out_t;

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
        // CABIN_k_ to BANK_4k_
        assign lkp2bank_req_valid_o[0][k]       = cabin_lkp2bank_req[k].valid[0];
        assign cabin_lkp2bank_req[k].ready[0]   = lkp2bank_req_ready_i[0][k];
        assign lkp2bank_req_o[0][k]             = cabin_lkp2bank_req[k].valid[0] ? cabin_lkp2bank_req[k].req[0] : 'd0;
        // CABIN_k_ to BANK_2m_
        assign lkp2bank_req_valid_o[1][k]       = cabin_lkp2bank_req[k].valid[1];
        assign cabin_lkp2bank_req[k].ready[1]   = lkp2bank_req_ready_i[1][k];
        assign lkp2bank_req_o[1][k]             = cabin_lkp2bank_req[k].valid[1] ? cabin_lkp2bank_req[k].req[1] : 'd0;
        // CABIN_k_ to BANK_1g_
        assign lkp2bank_req_valid_o[2][k]       = cabin_lkp2bank_req[k].valid[2];
        assign cabin_lkp2bank_req[k].ready[2]   = lkp2bank_req_ready_i[2][k];
        assign lkp2bank_req_o[2][k]             = cabin_lkp2bank_req[k].valid[2] ? cabin_lkp2bank_req[k].req[2] : 'd0;
        // CABIN_k_ to BANK_0t_
        assign lkp2bank_req_valid_o[3][k]       = cabin_lkp2bank_req[k].valid[3];
        assign cabin_lkp2bank_req[k].ready[3]   = lkp2bank_req_ready_i[3][k];
        assign lkp2bank_req_o[3][k]             = cabin_lkp2bank_req[k].valid[3] ? cabin_lkp2bank_req[k].req[3] : 'd0;
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
        iommu_acd_mtlb_lkp_cabin #(
        /*parameter type         */ .LOOKUP_REQ_TYPE            (LOOKUP_REQ_TYPE            ), // = iommu_ack_pkg::lookup_req_t  ,
        /*parameter type         */ .LOOKUP_ACK_TYPE            (LOOKUP_ACK_TYPE            ), // = iommu_ack_pkg::lookup_ack_t  ,
        /*parameter type         */ .BANK_LKP_REQ_TYPE          (BANK_LKP_REQ_TYPE          ), // = iommu_ack_pkg::bank_lkp_req_t,
        /*parameter type         */ .BANK_LKP_ACK_TYPE          (BANK_LKP_ACK_TYPE          ), // = iommu_ack_pkg::bank_lkp_ack_t,
        /*parameter  */ .CABIN_IDX_WIDTH            (CABIN_IDX_WIDTH            ), // = 2,
        /*parameter  */ .BANK_4K_IDX_WIDTH          (BANK_4K_IDX_WIDTH          ), // = 3,
        /*parameter  */ .BANK_2M_IDX_WIDTH          (BANK_2M_IDX_WIDTH          ), // = 2,
        /*parameter  */ .BANK_1G_IDX_WIDTH          (BANK_1G_IDX_WIDTH          ), // = 2,
        /*parameter  */ .BANK_0T_IDX_WIDTH          (BANK_0T_IDX_WIDTH          ), // = 2,
        /*parameter  */ .BANK_4K_SET_IDX_WIDTH      (BANK_4K_SET_IDX_WIDTH      ), // = 3,
        /*parameter  */ .BANK_2M_SET_IDX_WIDTH      (BANK_2M_SET_IDX_WIDTH      ), // = 2,
        /*parameter  */ .BANK_1G_SET_IDX_WIDTH      (BANK_1G_SET_IDX_WIDTH      ), // = 1,
        /*parameter  */ .BANK_0T_SET_IDX_WIDTH      (BANK_0T_SET_IDX_WIDTH      ), // = 1,
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
        /*output logic [3:0]                                */  .lkp2bank_req_valid_o       (cabin_lkp2bank_req[i].valid    ),   // [0]: BANK_4K, [1]: BANK_2M, [2]:BANK_1G, [3]:BANK_512G
        /*input  logic [3:0]                                */  .lkp2bank_req_ready_i       (cabin_lkp2bank_req[i].ready    ),
        /*output BANK_LKP_REQ_TYPE [3:0]                    */  .lkp2bank_req_o             (cabin_lkp2bank_req[i].req      ),
        /*input  logic [3:0]                                */  .bank2lkp_ack_valid_i       (cabin_bank2lkp_ack[i].valid    ),
        /*input  BANK_LKP_ACK_TYPE [3:0]                    */  .bank2lkp_ack_i             (cabin_bank2lkp_ack[i].ack      ),
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



module iommu_acd_mtlb_lkp_cabin #(
    parameter type          LOOKUP_REQ_TYPE             = iommu_acd_pkg::lookup_req_t,
    parameter type          LOOKUP_ACK_TYPE             = iommu_acd_pkg::lookup_ack_t,
    parameter type          UPDATE_REQ_TYPE             = iommu_acd_pkg::update_req_t,
    parameter type          BANK_LKP_REQ_TYPE           = iommu_acd_pkg::bank_lkp_req_t,
    parameter type          BANK_LKP_ACK_TYPE           = iommu_acd_pkg::bank_lkp_ack_t,
    parameter   CABIN_IDX_WIDTH             = 2,
    parameter   BANK_4K_IDX_WIDTH           = 3,
    parameter   BANK_2M_IDX_WIDTH           = 2,
    parameter   BANK_1G_IDX_WIDTH           = 2,
    parameter   BANK_0T_IDX_WIDTH           = 2,
    parameter   BANK_4K_SET_IDX_WIDTH       = 3,
    parameter   BANK_2M_SET_IDX_WIDTH       = 2,
    parameter   BANK_1G_SET_IDX_WIDTH       = 1,
    parameter   BANK_0T_SET_IDX_WIDTH       = 1,
    parameter   BANK_4K_NUM                 = 2**BANK_4K_IDX_WIDTH,
    parameter   BANK_2M_NUM                 = 2**BANK_2M_IDX_WIDTH,
    parameter   BANK_1G_NUM                 = 2**BANK_1G_IDX_WIDTH,
    parameter   BANK_0T_NUM                 = 2**BANK_0T_IDX_WIDTH,
    parameter   BANK_4K_SET_NUM             = 2**BANK_4K_SET_IDX_WIDTH,
    parameter   BANK_2M_SET_NUM             = 2**BANK_2M_SET_IDX_WIDTH,
    parameter   BANK_1G_SET_NUM             = 2**BANK_1G_SET_IDX_WIDTH,
    parameter   BANK_0T_SET_NUM             = 2**BANK_0T_SET_IDX_WIDTH,
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
    output logic [3:0]                                  lkp2bank_req_valid_o,   // [0]: BANK_4K, [1]: BANK_2M, [2]:BANK_1G, [3]:BANK_512G
    input  logic [3:0]                                  lkp2bank_req_ready_i,
    output BANK_LKP_REQ_TYPE [3:0]                      lkp2bank_req_o,
    // BANK ACK                                         
    input  logic [3:0]                                  bank2lkp_ack_valid_i,
    input  BANK_LKP_ACK_TYPE [3:0]                      bank2lkp_ack_i,
    // VALID OUT
    output logic                                        valid_o,
    // BANK STATUS INPUT
    input  logic [3:0]                                  bank_valid_i,
    //
    input  logic                                        spare_in
//}}}
);
//=== Declare {{{
    localparam MAX_BANK_IDX_WIDTH_0 = (BANK_4K_IDX_WIDTH > BANK_2M_IDX_WIDTH) ? BANK_4K_IDX_WIDTH : BANK_2M_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH_1 = (BANK_1G_IDX_WIDTH > BANK_0T_IDX_WIDTH) ? BANK_1G_IDX_WIDTH : BANK_0T_IDX_WIDTH;
    localparam MAX_BANK_IDX_WIDTH   = (MAX_BANK_IDX_WIDTH_0 > MAX_BANK_IDX_WIDTH_1) ? MAX_BANK_IDX_WIDTH_0 : MAX_BANK_IDX_WIDTH_1;

    localparam MAX_BANK_SET_IDX_WIDTH_0 = (BANK_4K_SET_IDX_WIDTH > BANK_2M_SET_IDX_WIDTH) ? BANK_4K_SET_IDX_WIDTH : BANK_2M_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH_1 = (BANK_1G_SET_IDX_WIDTH > BANK_0T_SET_IDX_WIDTH) ? BANK_1G_SET_IDX_WIDTH : BANK_0T_SET_IDX_WIDTH;
    localparam MAX_BANK_SET_IDX_WIDTH   = (MAX_BANK_SET_IDX_WIDTH_0 > MAX_BANK_SET_IDX_WIDTH_1) ? MAX_BANK_SET_IDX_WIDTH_0 : MAX_BANK_SET_IDX_WIDTH_1;

    localparam S_IDLE = 4'b000;
//    localparam S_4K   = 4'b001;
//    localparam S_2M   = 4'b010;
//    localparam S_1G   = 4'b011;
//    localparam S_0T   = 4'b100;
    localparam S_ACK  = 4'b110;
    localparam S_ALL  = 4'b111;
    localparam S_WAIT_ALL_ACK = 4'b101;

    logic [3:0]                                         cs, ns;
    LOOKUP_REQ_TYPE                                     req;

    logic [23:0]                                        idx_device_id;
    logic                                               idx_process_id_valid;
    logic [19:0]                                        idx_process_id;
    logic [63:12]                                       idx_va;
    logic [1:0]                                         idx_page_size[3:0];
    logic [MAX_BANK_IDX_WIDTH+MAX_BANK_SET_IDX_WIDTH-1:0] idx[3:0];
    logic [BANK_4K_IDX_WIDTH+BANK_4K_SET_IDX_WIDTH-1:0] idx_4k;
    logic [BANK_2M_IDX_WIDTH+BANK_2M_SET_IDX_WIDTH-1:0] idx_2m;
    logic [BANK_1G_IDX_WIDTH+BANK_1G_SET_IDX_WIDTH-1:0] idx_1g;
    logic [BANK_0T_IDX_WIDTH+BANK_0T_SET_IDX_WIDTH-1:0] idx_0t;

    BANK_LKP_ACK_TYPE                                   cur_bank2lkp_ack[3:0];
    BANK_LKP_ACK_TYPE                                   fast_bank2lkp_ack;

    logic                                               lkp2bank_req_valid[3:0];   // [0]: BANK_4K, [1]: BANK_2M, [2]:BANK_1G, [3]:BANK_512G
    logic                                               lkp2bank_req_ready[3:0];
    BANK_LKP_REQ_TYPE                                   lkp2bank_req[3:0];

    logic [3:0]                                         lkp_all_ack_got, lkp_fast_ack_got;


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
//                    ns = S_4K;
//                else if(bank_valid_i[1])
//                    ns = S_2M;
//                else if(bank_valid_i[2])
//                    ns = S_1G;
//                else if(bank_valid_i[3])
//                    ns = S_0T;
//                else
//                    ns = S_IDLE;
            end
        end
//            S_4K    : begin
//                if(~bank2lkp_ack_valid_i[0])
//                    ns = S_4K;
//                else begin
//                    if(bank2lkp_ack_i[0].hit)
//                        ns = S_ACK;
//                    else begin
//                        if(bank_valid_i[1])
//                            ns = S_2M;
//                        else if(bank_valid_i[2])
//                            ns = S_1G;
//                        else if(bank_valid_i[3])
//                            ns = S_0T;
//                        else
//                            ns = S_IDLE;
//                    end
//                end
//            end
//            S_2M    :begin
//                if(~bank2lkp_ack_valid_i[1])
//                    ns = S_2M;
//                else begin
//                    if(bank2lkp_ack_i[1].hit)
//                        ns = S_ACK;
//                    else begin
//                        if(bank_valid_i[2])
//                            ns = S_1G;
//                        else if(bank_valid_i[3])
//                            ns = S_0T;
//                        else
//                            ns = S_IDLE;
//                    end
//                end
//            end
//            S_1G    : begin
//                if(~bank2lkp_ack_valid_i[2])
//                    ns = S_1G;
//                else begin
//                    if(bank2lkp_ack_i[2].hit)
//                        ns = S_ACK;
//                    else begin
//                        if(bank_valid_i[3])
//                            ns = S_0T;
//                        else
//                            ns = S_IDLE;
//                    end
//                end
//            end
//            S_0T    : begin
//                if(~bank2lkp_ack_valid_i[3])
//                    ns = S_0T;
//                else begin
//    //                if(bank2lkp_ack_i[3].hit)
//    //                    ns = S_ACK;
//    //                else begin
//    //                    ns = S_IDLE;
//    //                end
//                    ns = S_ACK;
//                end
//            end
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
        S_ALL   : begin
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
//=== BANK_4K_
genvar i;
generate
    for(i=0; i<4; i++) begin : lkp2bank_req_gen
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
                        lkp2bank_req[i].bank_idx    <= i==0 ? {{(MAX_BANK_IDX_WIDTH-BANK_4K_IDX_WIDTH){1'b0}},         idx_4k[BANK_4K_IDX_WIDTH+BANK_4K_SET_IDX_WIDTH-1:BANK_4K_SET_IDX_WIDTH]} :
                                                       i==1 ? {{(MAX_BANK_IDX_WIDTH-BANK_2M_IDX_WIDTH){1'b0}},         idx_2m[BANK_2M_IDX_WIDTH+BANK_2M_SET_IDX_WIDTH-1:BANK_2M_SET_IDX_WIDTH]} :
                                                       i==2 ? {{(MAX_BANK_IDX_WIDTH-BANK_1G_IDX_WIDTH){1'b0}},         idx_1g[BANK_1G_IDX_WIDTH+BANK_1G_SET_IDX_WIDTH-1:BANK_1G_SET_IDX_WIDTH]} :
                                                       i==3 ? {{(MAX_BANK_IDX_WIDTH-BANK_0T_IDX_WIDTH){1'b0}},         idx_0t[BANK_0T_IDX_WIDTH+BANK_0T_SET_IDX_WIDTH-1:BANK_0T_SET_IDX_WIDTH]} :
                                                              {{(MAX_BANK_IDX_WIDTH-BANK_4K_IDX_WIDTH){1'b0}},         idx_4k[BANK_4K_IDX_WIDTH+BANK_4K_SET_IDX_WIDTH-1:BANK_4K_SET_IDX_WIDTH]} ;
                        lkp2bank_req[i].bank_set_idx<= i==0 ? {{(MAX_BANK_SET_IDX_WIDTH-BANK_4K_SET_IDX_WIDTH){1'b0}}, idx_4k[BANK_4K_SET_IDX_WIDTH-1:0]                                      } :
                                                       i==1 ? {{(MAX_BANK_SET_IDX_WIDTH-BANK_2M_SET_IDX_WIDTH){1'b0}}, idx_2m[BANK_2M_SET_IDX_WIDTH-1:0]                                      } :
                                                       i==2 ? {{(MAX_BANK_SET_IDX_WIDTH-BANK_1G_SET_IDX_WIDTH){1'b0}}, idx_1g[BANK_1G_SET_IDX_WIDTH-1:0]                                      } :
                                                       i==3 ? {{(MAX_BANK_SET_IDX_WIDTH-BANK_0T_SET_IDX_WIDTH){1'b0}}, idx_0t[BANK_0T_SET_IDX_WIDTH-1:0]                                      } :
                                                              {{(MAX_BANK_SET_IDX_WIDTH-BANK_4K_SET_IDX_WIDTH){1'b0}}, idx_4k[BANK_4K_SET_IDX_WIDTH-1:0]                                      } ;
                    end
//                    if(cs!=S_4K & ns==S_4K) begin
//                        lkp2bank_req_valid        <= 1'b1;
//                        lkp2bank_req.req          <= lkp_req_i;
//                        lkp2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_4K_IDX_WIDTH){1'b0}},         idx_4k[BANK_4K_IDX_WIDTH+BANK_4K_SET_IDX_WIDTH-1:BANK_4K_SET_IDX_WIDTH]};
//                        lkp2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_4K_SET_IDX_WIDTH){1'b0}}, idx_4k[BANK_4K_SET_IDX_WIDTH-1:0]                                      };
//                    end
//                    else if(cs!=S_2M & ns==S_2M) begin
//                        lkp2bank_req_valid        <= 1'b1;
//                        lkp2bank_req.req          <= req;
//                        lkp2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_2M_IDX_WIDTH){1'b0}},         idx_2m[BANK_2M_IDX_WIDTH+BANK_2M_SET_IDX_WIDTH-1:BANK_2M_SET_IDX_WIDTH]};
//                        lkp2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_2M_SET_IDX_WIDTH){1'b0}}, idx_2m[BANK_2M_SET_IDX_WIDTH-1:0]                                      };
//                    end
//                    else if(cs!=S_1G & ns==S_1G) begin
//                        lkp2bank_req_valid        <= 1'b1;
//                        lkp2bank_req.req          <= req;
//                        lkp2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_1G_IDX_WIDTH){1'b0}},         idx_1g[BANK_1G_IDX_WIDTH+BANK_1G_SET_IDX_WIDTH-1:BANK_1G_SET_IDX_WIDTH]};
//                        lkp2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_1G_SET_IDX_WIDTH){1'b0}}, idx_1g[BANK_1G_SET_IDX_WIDTH-1:0]                                      };
//                    end
//                    else if(cs!=S_0T & ns==S_0T) begin
//                        lkp2bank_req_valid        <= 1'b1;
//                        lkp2bank_req.req          <= req;
//                        lkp2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_0T_IDX_WIDTH){1'b0}},         idx_0t[BANK_0T_IDX_WIDTH+BANK_0T_SET_IDX_WIDTH-1:BANK_0T_SET_IDX_WIDTH]};
//                        lkp2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_0T_SET_IDX_WIDTH){1'b0}}, idx_0t[BANK_0T_SET_IDX_WIDTH-1:0]                                      };
//                    end
                end
            end
        end
    end
endgenerate

    assign lkp2bank_req_valid_o[0] = lkp2bank_req_valid[0];
    assign lkp2bank_req_valid_o[1] = lkp2bank_req_valid[1];
    assign lkp2bank_req_valid_o[2] = lkp2bank_req_valid[2];
    assign lkp2bank_req_valid_o[3] = lkp2bank_req_valid[3];
    assign lkp2bank_req_ready[0]   = lkp2bank_req_ready_i[0] ;
    assign lkp2bank_req_ready[1]   = lkp2bank_req_ready_i[1] ;
    assign lkp2bank_req_ready[2]   = lkp2bank_req_ready_i[2] ;
    assign lkp2bank_req_ready[3]   = lkp2bank_req_ready_i[3] ;
    assign lkp2bank_req_o[0]       = lkp2bank_req[0];
    assign lkp2bank_req_o[1]       = lkp2bank_req[1];
    assign lkp2bank_req_o[2]       = lkp2bank_req[2];
    assign lkp2bank_req_o[3]       = lkp2bank_req[3];


//}}}

//=== LKP ACK {{{
genvar j;
generate
    for(j=0; j<4; j++) begin : lkp_ack_got_gen
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
    lkp_fast_ack_got = 4'd0;
    fast_bank2lkp_ack= 'd0;
    if(lkp_all_ack_got[0] &  cur_bank2lkp_ack[0].hit) begin // got hit 4K translation, no wait for other bank
        lkp_fast_ack_got = 4'b0001;
        fast_bank2lkp_ack= cur_bank2lkp_ack[0];
    end
    else if(lkp_all_ack_got[1] &  cur_bank2lkp_ack[1].hit &
            lkp_all_ack_got[0] & ~cur_bank2lkp_ack[0].hit) begin// got hit 2M translation and 4K miss
        lkp_fast_ack_got = 4'b0010;
        fast_bank2lkp_ack= cur_bank2lkp_ack[1];
    end
    else if(lkp_all_ack_got[2] &  cur_bank2lkp_ack[2].hit &
            lkp_all_ack_got[1] & ~cur_bank2lkp_ack[1].hit &
            lkp_all_ack_got[0] & ~cur_bank2lkp_ack[0].hit) begin// got hit 1G translation and 4K/2M miss
        lkp_fast_ack_got = 4'b0100;
        fast_bank2lkp_ack= cur_bank2lkp_ack[2];
    end
    else if(lkp_all_ack_got[3] &  cur_bank2lkp_ack[3].hit &
            lkp_all_ack_got[2] & ~cur_bank2lkp_ack[2].hit &
            lkp_all_ack_got[1] & ~cur_bank2lkp_ack[1].hit &
            lkp_all_ack_got[0] & ~cur_bank2lkp_ack[0].hit) begin
        lkp_fast_ack_got = 4'b1000;
        fast_bank2lkp_ack= cur_bank2lkp_ack[3];
    end
    else begin
        lkp_fast_ack_got = 4'b0000;
        fast_bank2lkp_ack= 'd0;
    end
end

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
                    lkp_ack_o.idx               <= req.idx                          ;
                    lkp_ack_o.is_translated     <= req.is_translated                ;
                    lkp_ack_o.process_id_valid  <= req.process_id_valid             ;
                    if(|lkp_fast_ack_got) begin
                        lkp_ack_o.PBMT              <= fast_bank2lkp_ack.ack.PBMT   ;
                        lkp_ack_o.GPPN              <= fast_bank2lkp_ack.ack.GPPN   ;
                        lkp_ack_o.ENATS             <= fast_bank2lkp_ack.ack.ENATS  ;
                        lkp_ack_o.T2GPA             <= fast_bank2lkp_ack.ack.T2GPA  ;
                        lkp_ack_o.DTF               <= fast_bank2lkp_ack.ack.DTF    ;
                        lkp_ack_o.PDTV              <= fast_bank2lkp_ack.ack.PDTV   ;
                        lkp_ack_o.DPE               <= fast_bank2lkp_ack.ack.DPE    ;
                        lkp_ack_o.SXL               <= fast_bank2lkp_ack.ack.SXL    ;
                        lkp_ack_o.ENS               <= fast_bank2lkp_ack.ack.ENS    ;
                        lkp_ack_o.SUM               <= fast_bank2lkp_ack.ack.SUM    ;
                        lkp_ack_o.S1_D              <= fast_bank2lkp_ack.ack.S1_D   ;
                        lkp_ack_o.S2_D              <= fast_bank2lkp_ack.ack.S2_D   ;
                        lkp_ack_o.SADE              <= fast_bank2lkp_ack.ack.SADE   ;
                        lkp_ack_o.GADE              <= fast_bank2lkp_ack.ack.GADE   ;
                        lkp_ack_o.N                 <= fast_bank2lkp_ack.ack.N      ;
//                        lkp_ack_o.S1_PERM_D         <= fast_bank2lkp_ack.ack.S1_PERM_D   ;
//                        lkp_ack_o.S1_PERM_A         <= fast_bank2lkp_ack.ack.S1_PERM_A   ;
                        lkp_ack_o.S1_PERM           <= fast_bank2lkp_ack.ack.S1_PERM;
//                        lkp_ack_o.S2_PERM_D         <= fast_bank2lkp_ack.ack.S2_PERM_D   ;
//                        lkp_ack_o.S2_PERM_A         <= fast_bank2lkp_ack.ack.S2_PERM_A   ;
                        lkp_ack_o.S2_PERM           <= fast_bank2lkp_ack.ack.S2_PERM;
                        lkp_ack_o.S1SIZE            <= fast_bank2lkp_ack.ack.S1SIZE ;
                        lkp_ack_o.S2SIZE            <= fast_bank2lkp_ack.ack.S2SIZE ;
                        lkp_ack_o.S1MODE            <= fast_bank2lkp_ack.ack.S1MODE ;
                        lkp_ack_o.S2MODE            <= fast_bank2lkp_ack.ack.S2MODE ;
                        lkp_ack_o.PDTMODE           <= fast_bank2lkp_ack.ack.PDTMODE;
                        lkp_ack_o.PSCID             <= fast_bank2lkp_ack.ack.PSCID  ;
                        lkp_ack_o.GSCID             <= fast_bank2lkp_ack.ack.GSCID  ;
                        lkp_ack_o.PPN               <= fast_bank2lkp_ack.ack.PPN    ;
                    end
                    else begin
                        lkp_ack_o.PBMT              <= 'd0;
                        lkp_ack_o.GPPN              <= 'd0;
                        lkp_ack_o.ENATS             <= 'd0;
                        lkp_ack_o.T2GPA             <= 'd0;
                        lkp_ack_o.DTF               <= 'd0;
                        lkp_ack_o.PDTV              <= 'd0;
                        lkp_ack_o.DPE               <= 'd0;
                        lkp_ack_o.SXL               <= 'd0;
                        lkp_ack_o.ENS               <= 'd0;
                        lkp_ack_o.SUM               <= 'd0;
                        lkp_ack_o.S1_D              <= 'd0;
                        lkp_ack_o.S2_D              <= 'd0;
                        lkp_ack_o.SADE              <= 'd0;
                        lkp_ack_o.GADE              <= 'd0;
                        lkp_ack_o.N                 <= 'd0;
//                        lkp_ack_o.S1_PERM_D         <= 'd0;
//                        lkp_ack_o.S1_PERM_A         <= 'd0;
                        lkp_ack_o.S1_PERM           <= 'd0;
//                        lkp_ack_o.S2_PERM_D         <= 'd0;
//                        lkp_ack_o.S2_PERM_A         <= 'd0;
                        lkp_ack_o.S2_PERM           <= 'd0;
                        lkp_ack_o.S1SIZE            <= 'd0;
                        lkp_ack_o.S2SIZE            <= 'd0;
                        lkp_ack_o.S1MODE            <= 'd0;
                        lkp_ack_o.S2MODE            <= 'd0;
                        lkp_ack_o.PDTMODE           <= 'd0;
                        lkp_ack_o.PSCID             <= 'd0;
                        lkp_ack_o.GSCID             <= 'd0;
                        lkp_ack_o.PPN               <= 'd0;
                    end
                    lkp_ack_o.process_id        <= req.process_id                   ;
                    lkp_ack_o.device_id         <= req.device_id                    ;
                    lkp_ack_o.va                <= req.va                           ;
                end
            end
        end
    end
//}}}

//}}}

//=== IDX inst {{{
    assign idx_device_id        = (cs==S_IDLE) ? lkp_req_i.device_id        : req.device_id;
    assign idx_process_id_valid = (cs==S_IDLE) ? lkp_req_i.process_id_valid : req.process_id_valid;
    assign idx_process_id       = (cs==S_IDLE) ? lkp_req_i.process_id       : req.process_id;
    assign idx_va               = (cs==S_IDLE) ? lkp_req_i.va               : req.va;                
//    assign idx_page_size        = (ns==S_4K) ? 2'b00 :
//                                  (ns==S_2M) ? 2'b01 :
//                                  (ns==S_1G) ? 2'b10 :
//                                  (ns==S_0T) ? 2'b11 : 2'b00;
    assign idx_4k = idx[0][BANK_4K_IDX_WIDTH+BANK_4K_SET_IDX_WIDTH-1:0];
    assign idx_2m = idx[1][BANK_2M_IDX_WIDTH+BANK_2M_SET_IDX_WIDTH-1:0];
    assign idx_1g = idx[2][BANK_1G_IDX_WIDTH+BANK_1G_SET_IDX_WIDTH-1:0];
    assign idx_0t = idx[3][BANK_0T_IDX_WIDTH+BANK_0T_SET_IDX_WIDTH-1:0];

genvar k;
generate
    for(k=0; k<4; k++) begin : idx_gen
        assign idx_page_size[k] = 2'(k);
        iommu_acd_mtlb_idx #(
        /*parameter  */ .IDX_WIDTH      (MAX_BANK_IDX_WIDTH+MAX_BANK_SET_IDX_WIDTH  ) // = 7
        ) U_idx_gen(
        /*input  logic [23:0]                   */  .device_id          (idx_device_id          ),
        /*input  logic                          */  .process_id_valid   (idx_process_id_valid   ),
        /*input  logic [19:0]                   */  .process_id         (idx_process_id         ),
        /*input  logic [63:12]                  */  .va                 (idx_va                 ),
        /*input  logic [1:0]                    */  .page_size          (idx_page_size[k]       ),
        /*output logic [IDX_WIDTH-1:0]          */  .idx                (idx[k]                 ) 
        );
    end
endgenerate
//}}}

//}}}

endmodule
