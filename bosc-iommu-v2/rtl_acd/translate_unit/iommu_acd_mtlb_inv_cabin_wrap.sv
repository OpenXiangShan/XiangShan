module iommu_acd_mtlb_inv_cabin_wrap #(
    parameter type          INVALID_REQ_TYPE            = iommu_acd_pkg::INVALID_REQ_TYPE,
    parameter type          BANK_INV_REQ_TYPE           = iommu_acd_pkg::bank_inv_req_t,
    parameter type          BANK_INV_ACK_TYPE           = iommu_acd_pkg::bank_inv_ack_t,
    parameter   CABIN_IDX_WIDTH             = 1,
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
    // INV INPUT                                        
    input  logic                                        invalid_req_valid_i,
    output logic                                        invalid_req_ready_o,
    input  INVALID_REQ_TYPE                             invalid_req_i,
    // INV ACK OUTPUT                                       
    output logic                                        invalid_ack_valid_o,
    input  logic                                        invalid_ack_ready_i,
    output INVALID_REQ_TYPE                             invalid_ack_o,
    // BANK REQ                                         
    output logic             [3:0] [CABIN_NUM-1:0]      inv2bank_req_valid_o,   // [0]: BANK_4K, [1]: BANK_2M, [2]:BANK_1G, [3]:BANK_512G
    input  logic             [3:0] [CABIN_NUM-1:0]      inv2bank_req_ready_i,
    output BANK_INV_REQ_TYPE [3:0] [CABIN_NUM-1:0]      inv2bank_req_o,
    // BANK ACK                                         
    input  logic             [3:0] [CABIN_NUM-1:0]      bank2inv_ack_valid_i,
    input  BANK_INV_ACK_TYPE [3:0] [CABIN_NUM-1:0]      bank2inv_ack_i,
    // INV REQ TO microTLB
    output logic                                        invalid_req_valid_o,
    input  logic                                        invalid_req_ready_i,
    output INVALID_REQ_TYPE                             invalid_req_o,
    //
    input  logic                                        spare_in
//}}}
);
//=== Declare {{{
    typedef struct packed {
        logic [3:0]                                     valid;
        logic [3:0]                                     ready;
        BANK_INV_REQ_TYPE [3:0]                         req;
    } cabin_bank_inv_req_t;

    typedef struct packed {
        logic [3:0]                                     valid;
        BANK_INV_ACK_TYPE [3:0]                         ack;
    } cabin_bank_inv_ack_t;

    logic                [CABIN_NUM-1:0]                cabin_inv_req_valid_i;
    INVALID_REQ_TYPE     [CABIN_NUM-1:0]                cabin_inv_req_i      ;
    logic                [CABIN_NUM-1:0]                cabin_inv_ack_valid_o;
    logic                [CABIN_NUM-1:0]                cabin_inv_ack_ready_i;
    INVALID_REQ_TYPE     [CABIN_NUM-1:0]                cabin_inv_ack_o      ;
    logic                [CABIN_NUM-1:0]                cabin_inv_ack_hit_o;
    cabin_bank_inv_req_t [CABIN_NUM-1:0]                cabin_inv2bank_req;
    cabin_bank_inv_ack_t [CABIN_NUM-1:0]                cabin_bank2inv_ack;
    logic                [CABIN_NUM-1:0]                cabin_valid_o;
    logic                [CABIN_NUM-1:0]                cabin_inv_req_valid_o;
    logic                [CABIN_NUM-1:0]                cabin_inv_req_ready_i;
    INVALID_REQ_TYPE     [CABIN_NUM-1:0]                cabin_inv_req_o;

//}}}

//=== Main Code {{{
    assign invalid_req_ready_o = ~(&cabin_valid_o);      // ready low if all cabin occupied

//=== inv2bank req dmux {{{
genvar k;
generate
    for(k=0; k<CABIN_NUM; k++) begin : inv2bank_req_connect_gen
        // CABIN_k_ to BANK_4k_
        assign inv2bank_req_valid_o[0][k]       = cabin_inv2bank_req[k].valid[0];
        assign cabin_inv2bank_req[k].ready[0]   = inv2bank_req_ready_i[0][k];
        assign inv2bank_req_o[0][k]             = cabin_inv2bank_req[k].valid[0] ? cabin_inv2bank_req[k].req[0] : 'd0;
        // CABIN_k_ to BANK_2m_
        assign inv2bank_req_valid_o[1][k]       = cabin_inv2bank_req[k].valid[1];
        assign cabin_inv2bank_req[k].ready[1]   = inv2bank_req_ready_i[1][k];
        assign inv2bank_req_o[1][k]             = cabin_inv2bank_req[k].valid[1] ? cabin_inv2bank_req[k].req[1] : 'd0;
        // CABIN_k_ to BANK_1g_
        assign inv2bank_req_valid_o[2][k]       = cabin_inv2bank_req[k].valid[2];
        assign cabin_inv2bank_req[k].ready[2]   = inv2bank_req_ready_i[2][k];
        assign inv2bank_req_o[2][k]             = cabin_inv2bank_req[k].valid[2] ? cabin_inv2bank_req[k].req[2] : 'd0;
        // CABIN_k_ to BANK_0t_
        assign inv2bank_req_valid_o[3][k]       = cabin_inv2bank_req[k].valid[3];
        assign cabin_inv2bank_req[k].ready[3]   = inv2bank_req_ready_i[3][k];
        assign inv2bank_req_o[3][k]             = cabin_inv2bank_req[k].valid[3] ? cabin_inv2bank_req[k].req[3] : 'd0;
    end
endgenerate
//}}}

//=== bank ack dmux {{{
genvar kk;
generate
    for(kk=0; kk<CABIN_NUM; kk++) begin : bank2inv_ack_connect_gen
        // BANK_0_ to CABIN_kk_
        assign cabin_bank2inv_ack[kk].valid[0]     = bank2inv_ack_valid_i[0][kk];
        assign cabin_bank2inv_ack[kk].ack[0]       = bank2inv_ack_i[0][kk];
        // BANK_1_ to CABIN_kk_
        assign cabin_bank2inv_ack[kk].valid[1]     = bank2inv_ack_valid_i[1][kk];
        assign cabin_bank2inv_ack[kk].ack[1]       = bank2inv_ack_i[1][kk];
        // BANK_2_ to CABIN_kk_
        assign cabin_bank2inv_ack[kk].valid[2]     = bank2inv_ack_valid_i[2][kk];
        assign cabin_bank2inv_ack[kk].ack[2]       = bank2inv_ack_i[2][kk];
        // BANK_3_ to CABIN_kk_
        assign cabin_bank2inv_ack[kk].valid[3]     = bank2inv_ack_valid_i[3][kk];
        assign cabin_bank2inv_ack[kk].ack[3]       = bank2inv_ack_i[3][kk];
    end
endgenerate
//}}}

//=== invalid ack arbiter {{{
    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (0                      ),   //= 0,
    /*parameter     */ .REQ_NUM             (CABIN_NUM              ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (INVALID_REQ_TYPE       ),   //= logic,
    /*parameter     */ .AXIVLDRDY           (1                      )    //= 1
    ) U_translate_ack_arb(
    /*input  logic                                  */  .clk                    (clk                        ),
    /*input  logic                                  */  .rstn                   (rstn                       ),
    /*input  logic [REQ_NUM-1:0]                    */  .req_i                  (cabin_inv_ack_valid_o      ),
    /*input  logic [REQ_NUM-1:0]                    */  .req_prior_i            ({CABIN_NUM{1'b0}}          ),
    /*input  DATA_TYPE [REQ_NUM-1:0]                */  .data_i                 (cabin_inv_ack_o            ),
    /*output logic [REQ_NUM-1:0]                    */  .gnt_o                  (cabin_inv_ack_ready_i      ),
    /*output logic                                  */  .req_o                  (invalid_ack_valid_o        ),
    /*output DATA_TYPE                              */  .data_o                 (invalid_ack_o              ),
    /*input  logic                                  */  .gnt_i                  (invalid_ack_ready_i        ) 
    );

//}}}

//=== invalid req to microTLB arbiter {{{
    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (0                      ),   //= 0,
    /*parameter     */ .REQ_NUM             (CABIN_NUM              ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (INVALID_REQ_TYPE       ),   //= logic,
    /*parameter     */ .AXIVLDRDY           (1                      )    //= 1
    ) U_flb_req_arb(
    /*input  logic                                  */  .clk                    (clk                        ),
    /*input  logic                                  */  .rstn                   (rstn                       ),
    /*input  logic [REQ_NUM-1:0]                    */  .req_i                  (cabin_inv_req_valid_o      ),
    /*input  logic [REQ_NUM-1:0]                    */  .req_prior_i            ({CABIN_NUM{1'b0}}          ),
    /*input  DATA_TYPE [REQ_NUM-1:0]                */  .data_i                 (cabin_inv_req_o            ),
    /*output logic [REQ_NUM-1:0]                    */  .gnt_o                  (cabin_inv_req_ready_i      ),
    /*output logic                                  */  .req_o                  (invalid_req_valid_o        ),
    /*output DATA_TYPE                              */  .data_o                 (invalid_req_o              ),
    /*input  logic                                  */  .gnt_i                  (invalid_req_ready_i        ) 
    );

//}}}


//=== cabin inst {{{
genvar i;
generate
    for(i=0; i<CABIN_NUM; i++) begin : cabin_inst_gen
        assign cabin_inv_req_i[i] = invalid_req_i;
        iommu_acd_mtlb_inv_cabin #(
        /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE           ), // = iommu_ack_pkg::invalid_req_t  ,
        /*parameter type         */ .BANK_INV_REQ_TYPE          (BANK_INV_REQ_TYPE          ), // = iommu_ack_pkg::bank_inv_req_t,
        /*parameter type         */ .BANK_INV_ACK_TYPE          (BANK_INV_ACK_TYPE          ), // = iommu_ack_pkg::bank_inv_ack_t,
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
        /*input  logic                                      */  .inv_req_valid_i            (cabin_inv_req_valid_i       [i]),
        /*input  INVALID_REQ_TYPE                           */  .inv_req_i                  (cabin_inv_req_i             [i]),
        /*output logic                                      */  .inv_ack_valid_o            (cabin_inv_ack_valid_o       [i]),
        /*input  logic                                      */  .inv_ack_ready_i            (cabin_inv_ack_ready_i       [i]),
        /*output INVALID_REQ_TYPE                           */  .inv_ack_o                  (cabin_inv_ack_o             [i]),
        /*output logic [3:0]                                */  .inv2bank_req_valid_o       (cabin_inv2bank_req[i].valid    ),   // [0]: BANK_4K, [1]: BANK_2M, [2]:BANK_1G, [3]:BANK_512G
        /*input  logic [3:0]                                */  .inv2bank_req_ready_i       (cabin_inv2bank_req[i].ready    ),
        /*output BANK_INV_REQ_TYPE [3:0]                    */  .inv2bank_req_o             (cabin_inv2bank_req[i].req      ),
        /*input  logic [3:0]                                */  .bank2inv_ack_valid_i       (cabin_bank2inv_ack[i].valid    ),
        /*input  BANK_INV_ACK_TYPE [3:0]                    */  .bank2inv_ack_i             (cabin_bank2inv_ack[i].ack      ),
        /*output logic                                      */  .valid_o                    (cabin_valid_o               [i]),
        /*output logic                                      */  .inv_req_valid_o            (cabin_inv_req_valid_o       [i]),
        /*input  logic                                      */  .inv_req_ready_i            (cabin_inv_req_ready_i       [i]),
        /*output INVALID_REQ_TYPE                           */  .inv_req_o                  (cabin_inv_req_o             [i]),
        /*input  logic                                      */  .spare_in                   (1'b0                           ) 
        );

        iommu_acd_bus_handler_queue_entry_update #(.IDX_WIDTH(CABIN_IDX_WIDTH)) U_entry_valid_check(
            .valid_i    (cabin_valid_o              ),
            .update_i   (invalid_req_valid_i        ),
            .tag_i      (CABIN_IDX_WIDTH'(i)        ),
            .update_o   (cabin_inv_req_valid_i[i]   )
        );
    end
endgenerate

//}}}

//}}}

endmodule



module iommu_acd_mtlb_inv_cabin #(
    parameter type          INVALID_REQ_TYPE            = iommu_acd_pkg::INVALID_REQ_TYPE,
    parameter type          BANK_INV_REQ_TYPE           = iommu_acd_pkg::bank_inv_req_t,
    parameter type          BANK_INV_ACK_TYPE           = iommu_acd_pkg::bank_inv_ack_t,
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
    // INV INPUT
    input  logic                                        inv_req_valid_i,
    input  INVALID_REQ_TYPE                             inv_req_i,
    // INV OUTPUT
    output logic                                        inv_ack_valid_o,
    input  logic                                        inv_ack_ready_i,
    output INVALID_REQ_TYPE                             inv_ack_o,
    // BANK REQ                                         
    output logic [3:0]                                  inv2bank_req_valid_o,   // [0]: BANK_4K, [1]: BANK_2M, [2]:BANK_1G, [3]:BANK_512G
    input  logic [3:0]                                  inv2bank_req_ready_i,
    output BANK_INV_REQ_TYPE [3:0]                      inv2bank_req_o,
    // BANK ACK                                         
    input  logic [3:0]                                  bank2inv_ack_valid_i,
    input  BANK_INV_ACK_TYPE [3:0]                      bank2inv_ack_i,
    // INV TO MICRO TLB
    output logic                                        inv_req_valid_o,
    input  logic                                        inv_req_ready_i,
    output INVALID_REQ_TYPE                             inv_req_o,
    // VALID OUT
    output logic                                        valid_o,
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
    localparam S_TAG  = 4'b001;
    localparam S_ALL  = 4'b111;
    localparam S_ACK  = 4'b110;
    localparam S_FTLB = 4'b010;
    localparam S_WAIT = 4'b100;

    logic [3:0]                                         cs, ns;
    INVALID_REQ_TYPE                                    req;

    logic [23:0]                                        idx_device_id;
    logic                                               idx_process_id_valid;
    logic [19:0]                                        idx_process_id;
    logic [63:12]                                       idx_va;
    logic [3:0] [1:0]                                   idx_page_size;
    logic [3:0] [MAX_BANK_IDX_WIDTH+MAX_BANK_SET_IDX_WIDTH-1:0] idx;
    logic [BANK_4K_IDX_WIDTH+BANK_4K_SET_IDX_WIDTH-1:0] idx_4k;
    logic [BANK_2M_IDX_WIDTH+BANK_2M_SET_IDX_WIDTH-1:0] idx_2m;
    logic [BANK_1G_IDX_WIDTH+BANK_1G_SET_IDX_WIDTH-1:0] idx_1g;
    logic [BANK_0T_IDX_WIDTH+BANK_0T_SET_IDX_WIDTH-1:0] idx_0t;

    logic [3:0]                                         inv2bank_req_valid;   // [0]: BANK_4K, [1]: BANK_2M, [2]:BANK_1G, [3]:BANK_512G
    logic [3:0]                                         inv2bank_req_ready;
    BANK_INV_REQ_TYPE [3:0]                             inv2bank_req;

    logic                                               inv_in_is_vma_with_av;
    logic [1:0]                                         inv_in_size;
    logic [3:0] [MAX_BANK_SET_IDX_WIDTH:0]              inv_all_cnt;
    logic [3:0]                                         inv_all_cnt_finish;
    logic [3:0] [MAX_BANK_SET_IDX_WIDTH:0]              inv_all_cnt_thr;

    logic [3:0]                                         inv_ack_got_flag;

    logic [1:0]                                         w2c_cnt;    // wait 2 cycle before send inv_req to microTLB, cause a hit-lkp 2cycles after inv may cause a invalid entry updated to microTLB
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
    assign inv_in_is_vma_with_av = (inv_req_i.itype==2'b10) & inv_req_i.av;

    always@(*) begin
        ns = cs;
        case(cs)
        S_IDLE  : begin
            if(~inv_req_valid_i)
                ns = S_IDLE;
            else begin
                if(inv_in_is_vma_with_av) begin // VMA with VA, can cal idx
                    ns = S_TAG;
                end
                else begin
                    ns = S_ALL;
                end
            end
        end
        S_TAG: begin
            if(&inv_all_cnt_finish)
                ns = S_WAIT;
            else
                ns = S_TAG;
        end
        S_ALL: begin
            if(&inv_all_cnt_finish)
                ns = S_WAIT;
            else
                ns = S_ALL;
        end
        S_ACK   : begin
            if(~inv_ack_ready_i)
                ns = S_ACK;
            else
                ns = S_IDLE;
        end
        S_FTLB  : begin
            if(~inv_req_ready_i)
                ns = S_FTLB;
            else
                ns = S_ACK;
        end
        S_WAIT  : begin
            if(w2c_cnt=='d2)
                ns = S_FTLB;
            else
                ns = S_WAIT;
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
            if(cs==S_ACK & ns!=S_ACK) begin
                valid_o <= 1'b0;
                req     <= req;
            end
            else if(cs==S_IDLE & ns!=S_IDLE) begin
                valid_o <= 1'b1;
                req     <= inv_req_i;
            end
        end
    end
//}}}

//=== inv_all_cnt {{{
    assign inv_all_cnt_thr[0]    = (cs==S_ALL) ? {1'b1, BANK_4K_SET_IDX_WIDTH'(0)} : 'd1;
    assign inv_all_cnt_thr[1]    = (cs==S_ALL) ? {1'b1, BANK_2M_SET_IDX_WIDTH'(0)} : 'd1;
    assign inv_all_cnt_thr[2]    = (cs==S_ALL) ? {1'b1, BANK_1G_SET_IDX_WIDTH'(0)} : 'd1;
    assign inv_all_cnt_thr[3]    = (cs==S_ALL) ? {1'b1, BANK_0T_SET_IDX_WIDTH'(0)} : 'd1;

    assign inv_all_cnt_finish[0] = inv_all_cnt[0] >= inv_all_cnt_thr[0];
    assign inv_all_cnt_finish[1] = inv_all_cnt[1] >= inv_all_cnt_thr[1];
    assign inv_all_cnt_finish[2] = inv_all_cnt[2] >= inv_all_cnt_thr[2];
    assign inv_all_cnt_finish[3] = inv_all_cnt[3] >= inv_all_cnt_thr[3];

genvar ii;
generate
    for(ii=0; ii<=3; ii++) begin : inv_all_cnt_gen
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                inv_all_cnt[ii] <= 'd0;
            else begin
                if(cs!=S_ACK && ns==S_ACK)
                    inv_all_cnt[ii] <= 'd0;
                else if((ns==S_ALL | ns==S_TAG) & ~inv_all_cnt_finish[ii] & inv2bank_req_ready[ii])
                    inv_all_cnt[ii] <= inv_all_cnt[ii] + 'd1;
            end
        end
    end
endgenerate

//}}}

//=== BANK_REQ {{{
genvar kk;
generate
    for(kk=0; kk<=3; kk++) begin : inv2bank_req_valid_gen
        always@(posedge clk or negedge rstn) begin
            if(~rstn) begin
                inv2bank_req_valid[kk] <= 'd0;
            end
            else begin
                if(ns==S_ALL | ns==S_TAG) begin
                    if(inv2bank_req_valid[kk] & ~inv2bank_req_ready[kk])
                        inv2bank_req_valid[kk] <= 1'b1;
                    else if(cs==S_IDLE)
                        inv2bank_req_valid[kk] <= 1'b1;
                    else if(inv_all_cnt[kk] < inv_all_cnt_thr[kk]-'d1)
                        inv2bank_req_valid[kk] <= 1'b1;
                    else
                        inv2bank_req_valid[kk] <= 1'b0;
                end
                else begin
                    inv2bank_req_valid[kk] <= 1'b0;
                end
//                else if(ns==S_TAG) begin
//                    if(~inv2bank_req_ready[kk])
//                        inv2bank_req_valid[kk] <= 1'b1;
//                    else
//                        inv2bank_req_valid[kk] <= 1'b0;
//                end
            end
        end

        assign inv2bank_req_valid_o[kk] = inv2bank_req_valid[kk];
        assign inv2bank_req_ready[kk]   = inv2bank_req_ready_i[kk];
    end

endgenerate

    assign inv2bank_req[0].bank_set_idx = (cs==S_ALL) ? inv_all_cnt[0][MAX_BANK_SET_IDX_WIDTH-1:0] : {{(MAX_BANK_SET_IDX_WIDTH-BANK_4K_SET_IDX_WIDTH){1'b0}}, idx_4k[BANK_4K_SET_IDX_WIDTH-1:0]};
    assign inv2bank_req[1].bank_set_idx = (cs==S_ALL) ? inv_all_cnt[1][MAX_BANK_SET_IDX_WIDTH-1:0] : {{(MAX_BANK_SET_IDX_WIDTH-BANK_2M_SET_IDX_WIDTH){1'b0}}, idx_2m[BANK_2M_SET_IDX_WIDTH-1:0]};
    assign inv2bank_req[2].bank_set_idx = (cs==S_ALL) ? inv_all_cnt[2][MAX_BANK_SET_IDX_WIDTH-1:0] : {{(MAX_BANK_SET_IDX_WIDTH-BANK_1G_SET_IDX_WIDTH){1'b0}}, idx_1g[BANK_1G_SET_IDX_WIDTH-1:0]};
    assign inv2bank_req[3].bank_set_idx = (cs==S_ALL) ? inv_all_cnt[3][MAX_BANK_SET_IDX_WIDTH-1:0] : {{(MAX_BANK_SET_IDX_WIDTH-BANK_0T_SET_IDX_WIDTH){1'b0}}, idx_0t[BANK_0T_SET_IDX_WIDTH-1:0]};
    
    assign inv2bank_req[0].bank_idx_val = (cs!=S_ALL);
    assign inv2bank_req[1].bank_idx_val = (cs!=S_ALL);
    assign inv2bank_req[2].bank_idx_val = (cs!=S_ALL);
    assign inv2bank_req[3].bank_idx_val = (cs!=S_ALL);

    assign inv2bank_req[0].bank_idx     = {{(MAX_BANK_IDX_WIDTH-BANK_4K_IDX_WIDTH){1'b0}}, idx_4k[BANK_4K_IDX_WIDTH+BANK_4K_SET_IDX_WIDTH-1:BANK_4K_SET_IDX_WIDTH]};
    assign inv2bank_req[1].bank_idx     = {{(MAX_BANK_IDX_WIDTH-BANK_2M_IDX_WIDTH){1'b0}}, idx_2m[BANK_2M_IDX_WIDTH+BANK_2M_SET_IDX_WIDTH-1:BANK_2M_SET_IDX_WIDTH]};
    assign inv2bank_req[2].bank_idx     = {{(MAX_BANK_IDX_WIDTH-BANK_1G_IDX_WIDTH){1'b0}}, idx_1g[BANK_1G_IDX_WIDTH+BANK_1G_SET_IDX_WIDTH-1:BANK_1G_SET_IDX_WIDTH]};
    assign inv2bank_req[3].bank_idx     = {{(MAX_BANK_IDX_WIDTH-BANK_0T_IDX_WIDTH){1'b0}}, idx_0t[BANK_0T_IDX_WIDTH+BANK_0T_SET_IDX_WIDTH-1:BANK_0T_SET_IDX_WIDTH]};

    assign inv2bank_req[0].req          = req;
    assign inv2bank_req[1].req          = req;
    assign inv2bank_req[2].req          = req;
    assign inv2bank_req[3].req          = req;
    
    assign inv2bank_req_o[0]            = inv2bank_req[0];
    assign inv2bank_req_o[1]            = inv2bank_req[1];
    assign inv2bank_req_o[2]            = inv2bank_req[2];
    assign inv2bank_req_o[3]            = inv2bank_req[3];

//}}}

//=== INV ACK {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            inv_ack_valid_o <= 1'b0;
            inv_ack_o       <= 'd0;
        end
        else begin
            if(inv_ack_valid_o) begin
                if(~inv_ack_ready_i)
                    inv_ack_valid_o <= 1'b1;
                else
                    inv_ack_valid_o <= 1'b0;
            end
            else begin
                if(cs!=S_ACK && ns==S_ACK) begin
                    inv_ack_valid_o     <= 1'b1;
                    inv_ack_o.idx       <= req.idx;
                    inv_ack_o.itype     <= 'd0;
                    inv_ack_o.dv_gv     <= 'd0;
                    inv_ack_o.did_gscid <= 'd0;
                    inv_ack_o.pscv      <= 'd0;
                    inv_ack_o.pid_pscid <= 'd0;
                    inv_ack_o.av        <= 'd0;
                    inv_ack_o.addr      <= 'd0;
                end
            end
        end
    end

//}}}

//=== INV REQ TO Microtlb{{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            inv_req_valid_o <= 'd0;
            inv_req_o       <= 'd0;
        end
        else begin
            if(ns==S_FTLB) begin
                inv_req_valid_o <= 1'b1;
                inv_req_o       <= req;
            end
            else begin
                inv_req_valid_o <= 1'b0;
            end
        end
    end

//}}}

//=== W2C {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            inv_ack_got_flag <= 'd0;
        end
        else begin
            if(ns == S_IDLE) begin
                inv_ack_got_flag <= 'd0;
            end
            else begin
                for(int unsigned iagf=0; iagf<4; iagf++) begin
                    if(bank2inv_ack_valid_i[iagf])
                        inv_ack_got_flag[iagf] <= 1'b1;
                end
            end
        end
    end
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            w2c_cnt <= 'd0;
        else if((ns==S_WAIT) & (&inv_ack_got_flag))
            w2c_cnt <= w2c_cnt+'d1;
        else
            w2c_cnt <= 'd0;
    end
//}}}

//}}}

//=== IDX inst {{{
    assign idx_device_id        = req.did_gscid;
    assign idx_process_id_valid = req.pscv;
    assign idx_process_id       = req.pid_pscid;
    assign idx_va               = req.addr;
    assign idx_page_size[0]     = 2'b00 ;
    assign idx_page_size[1]     = 2'b01 ;
    assign idx_page_size[2]     = 2'b10 ;
    assign idx_page_size[3]     = 2'b11 ;
    assign idx_4k = idx[0][BANK_4K_IDX_WIDTH+BANK_4K_SET_IDX_WIDTH-1:0];
    assign idx_2m = idx[1][BANK_2M_IDX_WIDTH+BANK_2M_SET_IDX_WIDTH-1:0];
    assign idx_1g = idx[2][BANK_1G_IDX_WIDTH+BANK_1G_SET_IDX_WIDTH-1:0];
    assign idx_0t = idx[3][BANK_0T_IDX_WIDTH+BANK_0T_SET_IDX_WIDTH-1:0];

genvar idxg;
generate
    for(idxg=0; idxg<=3; idxg++) begin : inv_idx_gen
        iommu_acd_mtlb_idx #(
        /*parameter  */ .IDX_WIDTH      (MAX_BANK_IDX_WIDTH+MAX_BANK_SET_IDX_WIDTH  ) // = 7
        ) U_idx_gen(
        /*input  logic [23:0]                   */  .device_id          (idx_device_id          ),
        /*input  logic                          */  .process_id_valid   (idx_process_id_valid   ),
        /*input  logic [19:0]                   */  .process_id         (idx_process_id         ),
        /*input  logic [63:12]                  */  .va                 (idx_va                 ),
        /*input  logic [1:0]                    */  .page_size          (idx_page_size    [idxg]),
        /*output logic [IDX_WIDTH-1:0]          */  .idx                (idx              [idxg]) 
        );
    end
endgenerate

//}}}

//}}}

endmodule
