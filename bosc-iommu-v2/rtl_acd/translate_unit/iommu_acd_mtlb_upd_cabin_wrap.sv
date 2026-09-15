module iommu_acd_mtlb_upd_cabin_wrap #(
    parameter type          UPDATE_REQ_TYPE             = iommu_acd_pkg::update_req_t,
    parameter type          BANK_UPD_REQ_TYPE           = iommu_acd_pkg::bank_upd_req_t,
    parameter type          BANK_UPD_ACK_TYPE           = iommu_acd_pkg::bank_upd_ack_t,
    parameter               CABIN_IDX_WIDTH             = 2,
    parameter               BANK_4K_IDX_WIDTH           = 3,
    parameter               BANK_2M_IDX_WIDTH           = 2,
    parameter               BANK_1G_IDX_WIDTH           = 2,
    parameter               BANK_0T_IDX_WIDTH           = 1,
    parameter               BANK_4K_SET_IDX_WIDTH       = 3,
    parameter               BANK_2M_SET_IDX_WIDTH       = 2,
    parameter               BANK_1G_SET_IDX_WIDTH       = 1,
    parameter               BANK_0T_SET_IDX_WIDTH       = 1,
    parameter               CABIN_NUM                   = 2**CABIN_IDX_WIDTH,
    parameter               BANK_4K_NUM                 = 2**BANK_4K_IDX_WIDTH,
    parameter               BANK_2M_NUM                 = 2**BANK_2M_IDX_WIDTH,
    parameter               BANK_1G_NUM                 = 2**BANK_1G_IDX_WIDTH,
    parameter               BANK_0T_NUM                 = 2**BANK_0T_IDX_WIDTH,
    parameter               BANK_4K_SET_NUM             = 2**BANK_4K_SET_IDX_WIDTH,
    parameter               BANK_2M_SET_NUM             = 2**BANK_2M_SET_IDX_WIDTH,
    parameter               BANK_1G_SET_NUM             = 2**BANK_1G_SET_IDX_WIDTH,
    parameter               BANK_0T_SET_NUM             = 2**BANK_0T_SET_IDX_WIDTH,
    parameter               SPARE_PARAM                 = 0
)(                                                      
//{{{ IO                                                
    input  logic                                        clk,
    input  logic                                        rstn,
    // UPD INPUT                                        
    input  logic                                        update_req_valid_i,
    output logic                                        update_req_ready_o,
    input  UPDATE_REQ_TYPE                              update_req_i,
    // UPD OUTPUT                                       
    output logic                                        update_ack_valid_o,
    input  logic                                        update_ack_ready_i,
    output UPDATE_REQ_TYPE                              update_ack_o,
    // BANK REQ                                         
    output logic             [3:0] [CABIN_NUM-1:0]      upd2bank_req_valid_o,   // [0]: BANK_4K, [1]: BANK_2M, [2]:BANK_1G, [3]:BANK_512G
    input  logic             [3:0] [CABIN_NUM-1:0]      upd2bank_req_ready_i,
    output BANK_UPD_REQ_TYPE [3:0] [CABIN_NUM-1:0]      upd2bank_req_o,
    // BANK ACK                                         
    input  logic             [3:0] [CABIN_NUM-1:0]      bank2upd_ack_valid_i,
    input  BANK_UPD_ACK_TYPE [3:0] [CABIN_NUM-1:0]      bank2upd_ack_i,
    //
    input  logic                                        spare_in
//}}}
);
//=== Declare {{{
    typedef struct packed {
        logic [3:0]                                     valid;
        logic [3:0]                                     ready;
        BANK_UPD_REQ_TYPE [3:0]                         req;
    } cabin_bank_upd_req_t;

    typedef struct packed {
        logic [3:0]                                     valid;
        BANK_UPD_ACK_TYPE [3:0]                         ack;
    } cabin_bank_upd_ack_t;

    logic                [CABIN_NUM-1:0]                cabin_upd_req_valid_i;
    UPDATE_REQ_TYPE      [CABIN_NUM-1:0]                cabin_upd_req_i      ;
    logic                [CABIN_NUM-1:0]                cabin_upd_ack_valid_o;
    logic                [CABIN_NUM-1:0]                cabin_upd_ack_ready_i;
    UPDATE_REQ_TYPE      [CABIN_NUM-1:0]                cabin_upd_ack_o      ;
    logic                [CABIN_NUM-1:0]                cabin_upd_ack_hit_o;
    cabin_bank_upd_req_t [CABIN_NUM-1:0]                cabin_upd2bank_req;
    cabin_bank_upd_ack_t [CABIN_NUM-1:0]                cabin_bank2upd_ack;
    logic                [CABIN_NUM-1:0]                cabin_valid_o;
//}}}

//=== Main Code {{{
    assign update_req_ready_o = ~(&cabin_valid_o);      // ready low if all cabin occupied

//=== upd2bank req dmux {{{
genvar k;
generate
    for(k=0; k<CABIN_NUM; k++) begin : upd2bank_req_connect_gen
        // CABIN_k_ to BANK_4k_
        assign upd2bank_req_valid_o[0][k]       = cabin_upd2bank_req[k].valid[0];
        assign cabin_upd2bank_req[k].ready[0]   = upd2bank_req_ready_i[0][k];
        assign upd2bank_req_o[0][k]             = cabin_upd2bank_req[k].valid[0] ? cabin_upd2bank_req[k].req[0] : 'd0;
        // CABIN_k_ to BANK_2m_
        assign upd2bank_req_valid_o[1][k]       = cabin_upd2bank_req[k].valid[1];
        assign cabin_upd2bank_req[k].ready[1]   = upd2bank_req_ready_i[1][k];
        assign upd2bank_req_o[1][k]             = cabin_upd2bank_req[k].valid[1] ? cabin_upd2bank_req[k].req[1] : 'd0;
        // CABIN_k_ to BANK_1g_
        assign upd2bank_req_valid_o[2][k]       = cabin_upd2bank_req[k].valid[2];
        assign cabin_upd2bank_req[k].ready[2]   = upd2bank_req_ready_i[2][k];
        assign upd2bank_req_o[2][k]             = cabin_upd2bank_req[k].valid[2] ? cabin_upd2bank_req[k].req[2] : 'd0;
        // CABIN_k_ to BANK_0t_
        assign upd2bank_req_valid_o[3][k]       = cabin_upd2bank_req[k].valid[3];
        assign cabin_upd2bank_req[k].ready[3]   = upd2bank_req_ready_i[3][k];
        assign upd2bank_req_o[3][k]             = cabin_upd2bank_req[k].valid[3] ? cabin_upd2bank_req[k].req[3] : 'd0;
    end
endgenerate
//}}}

//=== bank ack dmux {{{
genvar kk;
generate
    for(kk=0; kk<CABIN_NUM; kk++) begin : bank2upd_ack_connect_gen
        // BANK_0_ to CABIN_kk_
        assign cabin_bank2upd_ack[kk].valid[0]     = bank2upd_ack_valid_i[0][kk];
        assign cabin_bank2upd_ack[kk].ack[0]       = bank2upd_ack_i[0][kk];
        // BANK_1_ to CABIN_kk_
        assign cabin_bank2upd_ack[kk].valid[1]     = bank2upd_ack_valid_i[1][kk];
        assign cabin_bank2upd_ack[kk].ack[1]       = bank2upd_ack_i[1][kk];
        // BANK_2_ to CABIN_kk_
        assign cabin_bank2upd_ack[kk].valid[2]     = bank2upd_ack_valid_i[2][kk];
        assign cabin_bank2upd_ack[kk].ack[2]       = bank2upd_ack_i[2][kk];
        // BANK_3_ to CABIN_kk_
        assign cabin_bank2upd_ack[kk].valid[3]     = bank2upd_ack_valid_i[3][kk];
        assign cabin_bank2upd_ack[kk].ack[3]       = bank2upd_ack_i[3][kk];
    end
endgenerate
//}}}

//=== update ack arbiter {{{
    iommu_acd_bus_handler_trans_arb #(
    /*parameter    */               .ARB_TYPE            (0                      ),   //= 0,
    /*parameter    */               .REQ_NUM             (CABIN_NUM              ),   //= 2,
    /*parameter type            */  .DATA_TYPE           (UPDATE_REQ_TYPE        ),   //= logic,
    /*parameter    */               .AXIVLDRDY           (1                      )    //= 1
    ) U_translate_ack_arb(
    /*input  logic                                  */  .clk                    (clk                        ),
    /*input  logic                                  */  .rstn                   (rstn                       ),
    /*input  logic [REQ_NUM-1:0]                    */  .req_i                  (cabin_upd_ack_valid_o      ),
    /*input  logic [REQ_NUM-1:0]                    */  .req_prior_i            ({CABIN_NUM{1'b0}}          ),
    /*input  DATA_TYPE [REQ_NUM-1:0]                */  .data_i                 (cabin_upd_ack_o            ),
    /*output logic [REQ_NUM-1:0]                    */  .gnt_o                  (cabin_upd_ack_ready_i      ),
    /*output logic                                  */  .req_o                  (update_ack_valid_o         ),
    /*output DATA_TYPE                              */  .data_o                 (update_ack_o               ),
    /*input  logic                                  */  .gnt_i                  (update_ack_ready_i         ) 
    );

//}}}

//=== cabin inst {{{
genvar i;
generate
    for(i=0; i<CABIN_NUM; i++) begin : cabin_inst_gen
        assign cabin_upd_req_i[i] = update_req_i;
        iommu_acd_mtlb_upd_cabin #(
        /*parameter type         */ .UPDATE_REQ_TYPE            (UPDATE_REQ_TYPE            ), // = iommu_ack_pkg::update_req_t  ,
        /*parameter type         */ .BANK_UPD_REQ_TYPE          (BANK_UPD_REQ_TYPE          ), // = iommu_ack_pkg::bank_upd_req_t,
        /*parameter type         */ .BANK_UPD_ACK_TYPE          (BANK_UPD_ACK_TYPE          ), // = iommu_ack_pkg::bank_upd_ack_t,
        /*parameter */              .CABIN_IDX_WIDTH            (CABIN_IDX_WIDTH            ), // = 2,
        /*parameter */              .BANK_4K_IDX_WIDTH          (BANK_4K_IDX_WIDTH          ), // = 3,
        /*parameter */              .BANK_2M_IDX_WIDTH          (BANK_2M_IDX_WIDTH          ), // = 2,
        /*parameter */              .BANK_1G_IDX_WIDTH          (BANK_1G_IDX_WIDTH          ), // = 2,
        /*parameter */              .BANK_0T_IDX_WIDTH          (BANK_0T_IDX_WIDTH          ), // = 2,
        /*parameter */              .BANK_4K_SET_IDX_WIDTH      (BANK_4K_SET_IDX_WIDTH      ), // = 3,
        /*parameter */              .BANK_2M_SET_IDX_WIDTH      (BANK_2M_SET_IDX_WIDTH      ), // = 2,
        /*parameter */              .BANK_1G_SET_IDX_WIDTH      (BANK_1G_SET_IDX_WIDTH      ), // = 1,
        /*parameter */              .BANK_0T_SET_IDX_WIDTH      (BANK_0T_SET_IDX_WIDTH      ), // = 1,
        /*parameter */              .SPARE_PARAM                (1'b0                       )  // = 0
        ) U_cabin(
        /*input  logic                                      */  .clk                        (clk                            ),
        /*input  logic                                      */  .rstn                       (rstn                           ),
        /*input  logic [CABIN_IDX_WIDTH-1:0]                */  .cabin_idx                  (CABIN_IDX_WIDTH'(i)            ),
        /*input  logic                                      */  .upd_req_valid_i            (cabin_upd_req_valid_i       [i]),
        /*input  UPDATE_REQ_TYPE                            */  .upd_req_i                  (cabin_upd_req_i             [i]),
        /*output logic                                      */  .upd_ack_valid_o            (cabin_upd_ack_valid_o       [i]),
        /*input  logic                                      */  .upd_ack_ready_i            (cabin_upd_ack_ready_i       [i]),
        /*output UPDATE_REQ_TYPE                            */  .upd_ack_o                  (cabin_upd_ack_o             [i]),
        /*output logic [3:0]                                */  .upd2bank_req_valid_o       (cabin_upd2bank_req[i].valid    ),   // [0]: BANK_4K, [1]: BANK_2M, [2]:BANK_1G, [3]:BANK_512G
        /*input  logic [3:0]                                */  .upd2bank_req_ready_i       (cabin_upd2bank_req[i].ready    ),
        /*output BANK_UPD_REQ_TYPE [3:0]                    */  .upd2bank_req_o             (cabin_upd2bank_req[i].req      ),
        /*input  logic [3:0]                                */  .bank2upd_ack_valid_i       (cabin_bank2upd_ack[i].valid    ),
        /*input  BANK_UPD_ACK_TYPE [3:0]                    */  .bank2upd_ack_i             (cabin_bank2upd_ack[i].ack      ),
        /*output logic                                      */  .valid_o                    (cabin_valid_o               [i]),
        /*input  logic                                      */  .spare_in                   (1'b0                           ) 
        );

        iommu_acd_bus_handler_queue_entry_update #(.IDX_WIDTH(CABIN_IDX_WIDTH)) U_entry_valid_check(
            .valid_i    (cabin_valid_o              ),
            .update_i   (update_req_valid_i         ),
            .tag_i      (CABIN_IDX_WIDTH'(i)        ),
            .update_o   (cabin_upd_req_valid_i[i]   )
        );
    end
endgenerate

//}}}

//}}}

endmodule



module iommu_acd_mtlb_upd_cabin #(
    parameter type          UPDATE_REQ_TYPE             = iommu_acd_pkg::update_req_t,
    parameter type          BANK_UPD_REQ_TYPE           = iommu_acd_pkg::bank_upd_req_t,
    parameter type          BANK_UPD_ACK_TYPE           = iommu_acd_pkg::bank_upd_ack_t,
    parameter               CABIN_IDX_WIDTH             = 2,
    parameter               BANK_4K_IDX_WIDTH           = 3,
    parameter               BANK_2M_IDX_WIDTH           = 2,
    parameter               BANK_1G_IDX_WIDTH           = 2,
    parameter               BANK_0T_IDX_WIDTH           = 2,
    parameter               BANK_4K_SET_IDX_WIDTH       = 3,
    parameter               BANK_2M_SET_IDX_WIDTH       = 2,
    parameter               BANK_1G_SET_IDX_WIDTH       = 1,
    parameter               BANK_0T_SET_IDX_WIDTH       = 1,
    parameter               BANK_4K_NUM                 = 2**BANK_4K_IDX_WIDTH,
    parameter               BANK_2M_NUM                 = 2**BANK_2M_IDX_WIDTH,
    parameter               BANK_1G_NUM                 = 2**BANK_1G_IDX_WIDTH,
    parameter               BANK_0T_NUM                 = 2**BANK_0T_IDX_WIDTH,
    parameter               BANK_4K_SET_NUM             = 2**BANK_4K_SET_IDX_WIDTH,
    parameter               BANK_2M_SET_NUM             = 2**BANK_2M_SET_IDX_WIDTH,
    parameter               BANK_1G_SET_NUM             = 2**BANK_1G_SET_IDX_WIDTH,
    parameter               BANK_0T_SET_NUM             = 2**BANK_0T_SET_IDX_WIDTH,
    parameter               SPARE_PARAM                 = 0
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    // IDX
    input  logic [CABIN_IDX_WIDTH-1:0]                  cabin_idx,
    // UPD INPUT
    input  logic                                        upd_req_valid_i,
    input  UPDATE_REQ_TYPE                              upd_req_i,
    // UPD OUTPUT
    output logic                                        upd_ack_valid_o,
    input  logic                                        upd_ack_ready_i,
    output UPDATE_REQ_TYPE                              upd_ack_o,
    // BANK REQ                                         
    output logic [3:0]                                  upd2bank_req_valid_o,   // [0]: BANK_4K, [1]: BANK_2M, [2]:BANK_1G, [3]:BANK_512G
    input  logic [3:0]                                  upd2bank_req_ready_i,
    output BANK_UPD_REQ_TYPE [3:0]                      upd2bank_req_o,
    // BANK ACK                                         
    input  logic [3:0]                                  bank2upd_ack_valid_i,
    input  BANK_UPD_ACK_TYPE [3:0]                      bank2upd_ack_i,
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
    localparam S_4K   = 4'b001;
    localparam S_2M   = 4'b010;
    localparam S_1G   = 4'b011;
    localparam S_0T   = 4'b100;
    localparam S_ACK  = 4'b110;

    logic [3:0]                                         cs, ns;
    UPDATE_REQ_TYPE                                     req;

    logic [23:0]                                        idx_device_id;
    logic                                               idx_process_id_valid;
    logic [19:0]                                        idx_process_id;
    logic [63:12]                                       idx_va;
    logic [1:0]                                         idx_page_size;
    logic [MAX_BANK_IDX_WIDTH+MAX_BANK_SET_IDX_WIDTH-1:0] idx;
    logic [BANK_4K_IDX_WIDTH+BANK_4K_SET_IDX_WIDTH-1:0] idx_4k;
    logic [BANK_2M_IDX_WIDTH+BANK_2M_SET_IDX_WIDTH-1:0] idx_2m;
    logic [BANK_1G_IDX_WIDTH+BANK_1G_SET_IDX_WIDTH-1:0] idx_1g;
    logic [BANK_0T_IDX_WIDTH+BANK_0T_SET_IDX_WIDTH-1:0] idx_0t;

    BANK_UPD_ACK_TYPE                                   cur_bank2upd_ack;

    logic                                               upd2bank_req_valid;   // [0]: BANK_4K, [1]: BANK_2M, [2]:BANK_1G, [3]:BANK_512G
    logic                                               upd2bank_req_ready;
    BANK_UPD_REQ_TYPE                                   upd2bank_req;

    logic [1:0]                                         upd_in_size;
    logic                                               s1_2m, s2_2m, is_2m;
    logic                                               s1_1g, s2_1g, is_1g;
    logic                                               s1_512g, s2_512g, is_512g;
    logic                                               s1_bare, s2_bare;
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
            if(~upd_req_valid_i)
                ns = S_IDLE;
            else begin
                if(upd_in_size==2'b00)      // 4K
                    ns = S_4K;
                else if(upd_in_size==2'b01) // 2M
                    ns = S_2M;
                else if(upd_in_size==2'b10) // 1G
                    ns = S_1G;
                else if(upd_in_size==2'b11) // 512G
                    ns = S_0T;
                else
                    ns = S_IDLE;
            end
        end
        S_4K    : begin
            if(~bank2upd_ack_valid_i[0])
                ns = S_4K;
            else
                ns = S_ACK;
        end
        S_2M    :begin
            if(~bank2upd_ack_valid_i[1])
                ns = S_2M;
            else
                ns = S_ACK;
        end
        S_1G    : begin
            if(~bank2upd_ack_valid_i[2])
                ns = S_1G;
            else
                ns = S_ACK;
        end
        S_0T    : begin
            if(~bank2upd_ack_valid_i[3])
                ns = S_0T;
             else
                ns = S_ACK;
        end
        S_ACK   : begin
            if(~upd_ack_ready_i)
                ns = S_ACK;
            else
                ns = S_IDLE;
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
                req     <= upd_req_i;
            end
        end
    end
//}}}

//=== BANK_REQ {{{
//=== BANK_4K_
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            upd2bank_req_valid <= 'd0;
            upd2bank_req       <= 'd0;
        end
        else begin
            if(upd2bank_req_valid) begin
                if(~upd2bank_req_ready)
                    upd2bank_req_valid <= 1'b1;
                else
                    upd2bank_req_valid <= 1'b0;
            end
            else begin
                if(cs!=S_4K & ns==S_4K) begin
                    upd2bank_req_valid        <= 1'b1;
                    upd2bank_req.req          <= upd_req_i;
                    upd2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_4K_IDX_WIDTH){1'b0}},         idx_4k[BANK_4K_IDX_WIDTH+BANK_4K_SET_IDX_WIDTH-1:BANK_4K_SET_IDX_WIDTH]};
                    upd2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_4K_SET_IDX_WIDTH){1'b0}}, idx_4k[BANK_4K_SET_IDX_WIDTH-1:0]                                      };
                end
                else if(cs!=S_2M & ns==S_2M) begin
                    upd2bank_req_valid        <= 1'b1;
                    upd2bank_req.req          <= upd_req_i;
                    upd2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_2M_IDX_WIDTH){1'b0}},         idx_2m[BANK_2M_IDX_WIDTH+BANK_2M_SET_IDX_WIDTH-1:BANK_2M_SET_IDX_WIDTH]};
                    upd2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_2M_SET_IDX_WIDTH){1'b0}}, idx_2m[BANK_2M_SET_IDX_WIDTH-1:0]                                      };
                end
                else if(cs!=S_1G & ns==S_1G) begin
                    upd2bank_req_valid        <= 1'b1;
                    upd2bank_req.req          <= upd_req_i;
                    upd2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_1G_IDX_WIDTH){1'b0}},         idx_1g[BANK_1G_IDX_WIDTH+BANK_1G_SET_IDX_WIDTH-1:BANK_1G_SET_IDX_WIDTH]};
                    upd2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_1G_SET_IDX_WIDTH){1'b0}}, idx_1g[BANK_1G_SET_IDX_WIDTH-1:0]                                      };
                end
                else if(cs!=S_0T & ns==S_0T) begin
                    upd2bank_req_valid        <= 1'b1;
                    upd2bank_req.req          <= upd_req_i;
                    upd2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_0T_IDX_WIDTH){1'b0}},         idx_0t[BANK_0T_IDX_WIDTH+BANK_0T_SET_IDX_WIDTH-1:BANK_0T_SET_IDX_WIDTH]};
                    upd2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_0T_SET_IDX_WIDTH){1'b0}}, idx_0t[BANK_0T_SET_IDX_WIDTH-1:0]                                      };
                end
            end
        end
    end
    
    assign upd2bank_req_valid_o[0] = (cs==S_4K) & upd2bank_req_valid;
    assign upd2bank_req_valid_o[1] = (cs==S_2M) & upd2bank_req_valid;
    assign upd2bank_req_valid_o[2] = (cs==S_1G) & upd2bank_req_valid;
    assign upd2bank_req_valid_o[3] = (cs==S_0T) & upd2bank_req_valid;
    assign upd2bank_req_ready      = (cs==S_4K) ? upd2bank_req_ready_i[0] :
                                     (cs==S_2M) ? upd2bank_req_ready_i[1] :
                                     (cs==S_1G) ? upd2bank_req_ready_i[2] :
                                     (cs==S_0T) ? upd2bank_req_ready_i[3] : 1'b0;
    assign upd2bank_req_o[0]       = (cs==S_4K) ? upd2bank_req : 'd0;
    assign upd2bank_req_o[1]       = (cs==S_2M) ? upd2bank_req : 'd0;
    assign upd2bank_req_o[2]       = (cs==S_1G) ? upd2bank_req : 'd0;
    assign upd2bank_req_o[3]       = (cs==S_0T) ? upd2bank_req : 'd0;


//}}}

//=== UPD ACK {{{
    assign cur_bank2upd_ack = (cs==S_4K) ? bank2upd_ack_i[0].ack :
                              (cs==S_2M) ? bank2upd_ack_i[1].ack :
                              (cs==S_1G) ? bank2upd_ack_i[2].ack :
                              (cs==S_0T) ? bank2upd_ack_i[3].ack : bank2upd_ack_i[0].ack;

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            upd_ack_valid_o <= 1'b0;
            upd_ack_o       <= 'd0;
        end
        else begin
            if(upd_ack_valid_o) begin
                if(~upd_ack_ready_i)
                    upd_ack_valid_o <= 1'b1;
                else
                    upd_ack_valid_o <= 1'b0;
            end
            else begin
                if(cs!=S_ACK && ns==S_ACK) begin
                    upd_ack_valid_o             <= 1'b1;
                    upd_ack_o.idx               <= req.idx;
                    upd_ack_o.is_translated     <= 'd0;
                    upd_ack_o.process_id_valid  <= 'd0;
                    upd_ack_o.PBMT              <= 'd0;
                    upd_ack_o.GPPN              <= 'd0;
                    upd_ack_o.ENATS             <= 'd0;
                    upd_ack_o.T2GPA             <= 'd0;
                    upd_ack_o.DTF               <= 'd0;
                    upd_ack_o.PDTV              <= 'd0;
                    upd_ack_o.DPE               <= 'd0;
                    upd_ack_o.SXL               <= 'd0;
                    upd_ack_o.ENS               <= 'd0;
                    upd_ack_o.SUM               <= 'd0;
                    upd_ack_o.S1_D              <= 'd0;
                    upd_ack_o.S2_D              <= 'd0;
                    upd_ack_o.SADE              <= 'd0;
                    upd_ack_o.GADE              <= 'd0;
                    upd_ack_o.N                 <= 'd0;
//                    upd_ack_o.S1_PERM_D         <= 'd0;
//                    upd_ack_o.S1_PERM_A         <= 'd0;
                    upd_ack_o.S1_PERM           <= 'd0;
//                    upd_ack_o.S2_PERM_D         <= 'd0;
//                    upd_ack_o.S2_PERM_A         <= 'd0;
                    upd_ack_o.S2_PERM           <= 'd0;
                    upd_ack_o.S1SIZE            <= 'd0;
                    upd_ack_o.S2SIZE            <= 'd0;
                    upd_ack_o.S1MODE            <= 'd0;
                    upd_ack_o.S2MODE            <= 'd0;
                    upd_ack_o.PDTMODE           <= 'd0;
                    upd_ack_o.PSCID             <= 'd0;
                    upd_ack_o.GSCID             <= 'd0;
                    upd_ack_o.PPN               <= 'd0;
                    upd_ack_o.process_id        <= 'd0;
                    upd_ack_o.device_id         <= 'd0;
                    upd_ack_o.va                <= 'd0;
                end
            end
        end
    end
//}}}

//}}}

//=== IDX inst {{{
    assign s1_bare              = upd_req_i.S1MODE=='d0;
    assign s2_bare              = upd_req_i.S2MODE=='d0;

    assign s1_2m                = upd_req_i.S1SIZE == 'b01;
    assign s2_2m                = upd_req_i.S2SIZE == 'b01;
    assign is_2m                = (~s1_bare & ~s2_bare) ? ((s2_2m & (s1_2m | s1_1g | s1_512g)) | (s1_2m & (s2_2m | s2_1g | s2_512g))) :
                                                          ((s2_2m & ~s2_bare) | (s1_2m & ~s1_bare));
    assign s1_1g                = upd_req_i.S1SIZE == 'b10;
    assign s2_1g                = upd_req_i.S2SIZE == 'b10;
    assign is_1g                = (~s1_bare & ~s2_bare) ? ((s2_1g & (s1_1g | s1_512g)) | (s1_1g & (s2_1g | s2_512g))) :
                                                          ((s2_1g & ~s2_bare) | (s1_1g & ~s1_bare));
    assign s1_512g              = upd_req_i.S1SIZE == 'b11;
    assign s2_512g              = upd_req_i.S2SIZE == 'b11;
    assign is_512g              = (~s1_bare & ~s2_bare) ? (s2_512g & s1_512g) :
                                                                  ((s2_512g & ~s2_bare) | (s1_512g & ~s1_bare));
// pagesize should concern of S1MODE and S2MODE, as when a STAGE is BARE, is SIZE value is reserved 0
//    assign upd_in_size          = (upd_req_i.S1SIZE < upd_req_i.S2SIZE) ? upd_req_i.S1SIZE : upd_req_i.S2SIZE;  // only valid when upd_req_valid_i asserted
    assign upd_in_size          = is_2m   ? 2'b01 :
                                  is_1g   ? 2'b10 :
                                  is_512g ? 2'b11 :
                                            2'b00;

    assign idx_device_id        = (cs==S_IDLE) ? upd_req_i.device_id        : req.device_id;
    assign idx_process_id_valid = (cs==S_IDLE) ? upd_req_i.process_id_valid : req.process_id_valid;
    assign idx_process_id       = (cs==S_IDLE) ? upd_req_i.process_id       : req.process_id;
    assign idx_va               = (cs==S_IDLE) ? upd_req_i.va               : req.va;
    assign idx_page_size        = (ns==S_4K) ? 2'b00 :
                                  (ns==S_2M) ? 2'b01 :
                                  (ns==S_1G) ? 2'b10 :
                                  (ns==S_0T) ? 2'b11 : 2'b00;
    assign idx_4k = idx[BANK_4K_IDX_WIDTH+BANK_4K_SET_IDX_WIDTH-1:0];
    assign idx_2m = idx[BANK_2M_IDX_WIDTH+BANK_2M_SET_IDX_WIDTH-1:0];
    assign idx_1g = idx[BANK_1G_IDX_WIDTH+BANK_1G_SET_IDX_WIDTH-1:0];
    assign idx_0t = idx[BANK_0T_IDX_WIDTH+BANK_0T_SET_IDX_WIDTH-1:0];

    iommu_acd_mtlb_idx #(
    /*parameter */ .IDX_WIDTH      (MAX_BANK_IDX_WIDTH+MAX_BANK_SET_IDX_WIDTH  ) // = 7
    ) U_idx_gen(
    /*input  logic [23:0]                   */  .device_id          (idx_device_id          ),
    /*input  logic                          */  .process_id_valid   (idx_process_id_valid   ),
    /*input  logic [19:0]                   */  .process_id         (idx_process_id         ),
    /*input  logic [63:12]                  */  .va                 (idx_va                 ),
    /*input  logic [1:0]                    */  .page_size          (idx_page_size          ),
    /*output logic [IDX_WIDTH-1:0]          */  .idx                (idx                    ) 
    );

//}}}

//}}}

endmodule
