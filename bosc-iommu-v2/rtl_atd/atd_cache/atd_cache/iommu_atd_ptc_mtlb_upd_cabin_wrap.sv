module iommu_atd_ptc_mtlb_upd_cabin_wrap #(
    parameter   CACHE_TYPE                  = 0, // 0:DDTC, 1:PDTC
    parameter type          UPDATE_REQ_TYPE             = iommu_atd_cache_pkg::s2ptc_update_req_t,
    parameter type          BANK_UPD_REQ_TYPE           = iommu_atd_cache_pkg::s2ptc_bank_upd_req_t,
    parameter type          BANK_UPD_ACK_TYPE           = iommu_atd_cache_pkg::s2ptc_bank_upd_ack_t,
    parameter   CABIN_IDX_WIDTH             = iommu_atd_cache_pkg::S2PTC_CABIN_UPD_IDX_WIDTH,
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
    // UPD INPUT                                        
    input  logic                                        update_req_valid_i,
    output logic                                        update_req_ready_o,
    input  UPDATE_REQ_TYPE                              update_req_i,
    // UPD OUTPUT                                       
    output logic                                        update_ack_valid_o,
    input  logic                                        update_ack_ready_i,
    output UPDATE_REQ_TYPE                              update_ack_o,
    // BANK REQ                                         
    output logic             [4:0] [CABIN_NUM-1:0]      upd2bank_req_valid_o,   // [0]: BANK_L0, [1]: BANK_L1, [2]:BANK_L2
    input  logic             [4:0] [CABIN_NUM-1:0]      upd2bank_req_ready_i,
    output BANK_UPD_REQ_TYPE [4:0] [CABIN_NUM-1:0]      upd2bank_req_o,
    // BANK ACK                                         
    input  logic             [4:0] [CABIN_NUM-1:0]      bank2upd_ack_valid_i,
    input  BANK_UPD_ACK_TYPE [4:0] [CABIN_NUM-1:0]      bank2upd_ack_i,
    //
    input  logic                                        spare_in
//}}}
);
//=== Declare {{{
    typedef struct packed {
        logic [4:0]                                     valid;
        logic [4:0]                                     ready;
        BANK_UPD_REQ_TYPE [4:0]                         req;
    } cabin_bank_upd_req_t;

    typedef struct packed {
        logic [4:0]                                     valid;
        BANK_UPD_ACK_TYPE [4:0]                         ack;
    } cabin_bank_upd_ack_t;

    logic                [CABIN_NUM-1:0]                cabin_upd_req_valid_i;
    UPDATE_REQ_TYPE      [CABIN_NUM-1:0]                cabin_upd_req_i      ;
    logic                [CABIN_NUM-1:0]                cabin_upd_ack_valid_o;
    logic                [CABIN_NUM-1:0]                cabin_upd_ack_ready_i;
    UPDATE_REQ_TYPE      [CABIN_NUM-1:0]                cabin_upd_ack_o      ;
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
        // CABIN_k_ to BANK_l0_
        assign upd2bank_req_valid_o[0][k]       = cabin_upd2bank_req[k].valid[0];
        assign cabin_upd2bank_req[k].ready[0]   = upd2bank_req_ready_i[0][k];
        assign upd2bank_req_o[0][k]             = cabin_upd2bank_req[k].valid[0] ? cabin_upd2bank_req[k].req[0] : 'd0;
        // CABIN_k_ to BANK_l1_
        assign upd2bank_req_valid_o[1][k]       = cabin_upd2bank_req[k].valid[1];
        assign cabin_upd2bank_req[k].ready[1]   = upd2bank_req_ready_i[1][k];
        assign upd2bank_req_o[1][k]             = cabin_upd2bank_req[k].valid[1] ? cabin_upd2bank_req[k].req[1] : 'd0;
        // CABIN_k_ to BANK_l2_
        assign upd2bank_req_valid_o[2][k]       = cabin_upd2bank_req[k].valid[2];
        assign cabin_upd2bank_req[k].ready[2]   = upd2bank_req_ready_i[2][k];
        assign upd2bank_req_o[2][k]             = cabin_upd2bank_req[k].valid[2] ? cabin_upd2bank_req[k].req[2] : 'd0;
        // CABIN_k_ to BANK_l3_
        assign upd2bank_req_valid_o[3][k]       = cabin_upd2bank_req[k].valid[3];
        assign cabin_upd2bank_req[k].ready[3]   = upd2bank_req_ready_i[3][k];
        assign upd2bank_req_o[3][k]             = cabin_upd2bank_req[k].valid[3] ? cabin_upd2bank_req[k].req[3] : 'd0;
        // CABIN_k_ to BANK_l4_
        assign upd2bank_req_valid_o[4][k]       = cabin_upd2bank_req[k].valid[4];
        assign cabin_upd2bank_req[k].ready[4]   = upd2bank_req_ready_i[4][k];
        assign upd2bank_req_o[4][k]             = cabin_upd2bank_req[k].valid[4] ? cabin_upd2bank_req[k].req[4] : 'd0;

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
        // BANK_4_ to CABIN_kk_
        assign cabin_bank2upd_ack[kk].valid[4]     = bank2upd_ack_valid_i[4][kk];
        assign cabin_bank2upd_ack[kk].ack[4]       = bank2upd_ack_i[4][kk];

    end
endgenerate
//}}}

//=== update ack arbiter {{{
    iommu_acd_bus_handler_trans_arb #(
    /*parameter     */ .ARB_TYPE            (0                      ),   //= 0,
    /*parameter     */ .REQ_NUM             (CABIN_NUM              ),   //= 2,
    /*parameter type            */ .DATA_TYPE           (UPDATE_REQ_TYPE        ),   //= logic,
    /*parameter     */ .AXIVLDRDY           (1                      )    //= 1
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
        iommu_atd_ptc_mtlb_upd_cabin #(
        /*parameter  */ .CACHE_TYPE                 (CACHE_TYPE                 ), // = 0, // 0:DDTC, 1:PDTC
        /*parameter type         */ .UPDATE_REQ_TYPE            (UPDATE_REQ_TYPE            ), // = iommu_atd_cache_pkg::update_req_t  ,
        /*parameter type         */ .BANK_UPD_REQ_TYPE          (BANK_UPD_REQ_TYPE          ), // = iommu_atd_cache_pkg::bank_upd_req_t,
        /*parameter type         */ .BANK_UPD_ACK_TYPE          (BANK_UPD_ACK_TYPE          ), // = iommu_atd_cache_pkg::bank_upd_ack_t,
        /*parameter  */ .CABIN_IDX_WIDTH            (CABIN_IDX_WIDTH            ), // = iommu_atd_cache_pkg::S2PTC_CABIN_UPD_IDX_WIDTH,
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
        /*input  logic                                      */  .upd_req_valid_i            (cabin_upd_req_valid_i       [i]),
        /*input  UPDATE_REQ_TYPE                            */  .upd_req_i                  (cabin_upd_req_i             [i]),
        /*output logic                                      */  .upd_ack_valid_o            (cabin_upd_ack_valid_o       [i]),
        /*input  logic                                      */  .upd_ack_ready_i            (cabin_upd_ack_ready_i       [i]),
        /*output UPDATE_REQ_TYPE                            */  .upd_ack_o                  (cabin_upd_ack_o             [i]),
        /*output logic [4:0]                                */  .upd2bank_req_valid_o       (cabin_upd2bank_req[i].valid    ),   // [0]: BANK_L0, [1]: BANK_L1, [2]:BANK_L2, [3]:BANK_512G
        /*input  logic [4:0]                                */  .upd2bank_req_ready_i       (cabin_upd2bank_req[i].ready    ),
        /*output BANK_UPD_REQ_TYPE [4:0]                    */  .upd2bank_req_o             (cabin_upd2bank_req[i].req      ),
        /*input  logic [4:0]                                */  .bank2upd_ack_valid_i       (cabin_bank2upd_ack[i].valid    ),
        /*input  BANK_UPD_ACK_TYPE [4:0]                    */  .bank2upd_ack_i             (cabin_bank2upd_ack[i].ack      ),
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



module iommu_atd_ptc_mtlb_upd_cabin #(
    parameter   CACHE_TYPE                  = 0, // 0:DDTC, 1:PDTC
    parameter type          UPDATE_REQ_TYPE             = iommu_atd_cache_pkg::s2ptc_update_req_t,
    parameter type          BANK_UPD_REQ_TYPE           = iommu_atd_cache_pkg::s2ptc_bank_upd_req_t,
    parameter type          BANK_UPD_ACK_TYPE           = iommu_atd_cache_pkg::s2ptc_bank_upd_ack_t,
    parameter   CABIN_IDX_WIDTH             = iommu_atd_cache_pkg::S2PTC_CABIN_UPD_IDX_WIDTH,
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
    // UPD INPUT
    input  logic                                        upd_req_valid_i,
    input  UPDATE_REQ_TYPE                              upd_req_i,
    // UPD OUTPUT
    output logic                                        upd_ack_valid_o,
    input  logic                                        upd_ack_ready_i,
    output UPDATE_REQ_TYPE                              upd_ack_o,
    // BANK REQ                                         
    output logic [4:0]                                  upd2bank_req_valid_o,   // [0]: BANK_L0, [1]: BANK_L1, [2]:BANK_L2
    input  logic [4:0]                                  upd2bank_req_ready_i,
    output BANK_UPD_REQ_TYPE [4:0]                      upd2bank_req_o,
    // BANK ACK                                         
    input  logic [4:0]                                  bank2upd_ack_valid_i,
    input  BANK_UPD_ACK_TYPE [4:0]                      bank2upd_ack_i,
    // VALID OUT
    output logic                                        valid_o,
    //
    input  logic                                        spare_in
//}}}
);
//=== Declare {{{
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
    localparam S_L0   = 4'b001;
    localparam S_L1   = 4'b010;
    localparam S_L2   = 4'b011;
    localparam S_L3   = 4'b100;
    localparam S_L4   = 4'b101;
    localparam S_ACK  = 4'b110;

    logic [3:0]                                         cs, ns;
    UPDATE_REQ_TYPE                                     req;

    logic [63:12]                                       idx_addr;
    logic                                               idx_gv;
    logic [15:0]                                        idx_gscid;
    logic [19:0]                                        idx_pscid;
    logic [2:0]                                         idx_lvl;
    logic [MAX_BANK_IDX_WIDTH+MAX_BANK_SET_IDX_WIDTH-1:0] idx;
    logic [BANK_L0_IDX_WIDTH+BANK_L0_SET_IDX_WIDTH-1:0] idx_l0;
    logic [BANK_L1_IDX_WIDTH+BANK_L1_SET_IDX_WIDTH-1:0] idx_l1;
    logic [BANK_L2_IDX_WIDTH+BANK_L2_SET_IDX_WIDTH-1:0] idx_l2;
    logic [BANK_L3_IDX_WIDTH+BANK_L3_SET_IDX_WIDTH-1:0] idx_l3;
    logic [BANK_L4_IDX_WIDTH+BANK_L4_SET_IDX_WIDTH-1:0] idx_l4;

    BANK_UPD_ACK_TYPE                                   cur_bank2upd_ack;

    logic                                               upd2bank_req_valid;
    logic                                               upd2bank_req_ready;
    BANK_UPD_REQ_TYPE                                   upd2bank_req;

    logic [2:0]                                         upd_in_lvl;
    logic                                               upd_in_leaf;
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
                if(upd_in_lvl==3'd0)      // l0
                    ns = S_L0;
                else if(upd_in_lvl==3'd1) // l1
                    ns = S_L1;
                else if(upd_in_lvl==3'd2) // l2
                    ns = S_L2;
                else if(upd_in_lvl==3'd3) // l3
                    ns = S_L3;
                else if(upd_in_lvl==3'd4) // l4
                    ns = S_L4;
                else
                    ns = S_IDLE;
            end
        end
        S_L0    : begin
            if(~bank2upd_ack_valid_i[0])
                ns = S_L0;
            else
                ns = S_ACK;
        end
        S_L1    :begin
            if(~bank2upd_ack_valid_i[1])
                ns = S_L1;
            else
                ns = S_ACK;
        end
        S_L2    : begin
            if(~bank2upd_ack_valid_i[2])
                ns = S_L2;
            else
                ns = S_ACK;
        end
        S_L3    : begin
            if(~bank2upd_ack_valid_i[3])
                ns = S_L3;
            else
                ns = S_ACK;
        end
        S_L4    : begin
            if(~bank2upd_ack_valid_i[4])
                ns = S_L4;
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
//=== BANK_L0_
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
                if(cs!=S_L0 & ns==S_L0) begin
                    upd2bank_req_valid        <= 1'b1;
                    upd2bank_req.req          <= upd_req_i;
                    upd2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_L0_IDX_WIDTH){1'b0}},         idx_l0[BANK_L0_IDX_WIDTH+BANK_L0_SET_IDX_WIDTH-1:BANK_L0_SET_IDX_WIDTH]};
                    upd2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_L0_SET_IDX_WIDTH){1'b0}}, idx_l0[BANK_L0_SET_IDX_WIDTH-1:0]                                      };
                end
                else if(cs!=S_L1 & ns==S_L1) begin
                    upd2bank_req_valid        <= 1'b1;
                    upd2bank_req.req          <= upd_req_i;
                    upd2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_L1_IDX_WIDTH){1'b0}},         idx_l1[BANK_L1_IDX_WIDTH+BANK_L1_SET_IDX_WIDTH-1:BANK_L1_SET_IDX_WIDTH]};
                    upd2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_L1_SET_IDX_WIDTH){1'b0}}, idx_l1[BANK_L1_SET_IDX_WIDTH-1:0]                                      };
                end
                else if(cs!=S_L2 & ns==S_L2) begin
                    upd2bank_req_valid        <= 1'b1;
                    upd2bank_req.req          <= upd_req_i;
                    upd2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_L2_IDX_WIDTH){1'b0}},         idx_l2[BANK_L2_IDX_WIDTH+BANK_L2_SET_IDX_WIDTH-1:BANK_L2_SET_IDX_WIDTH]};
                    upd2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_L2_SET_IDX_WIDTH){1'b0}}, idx_l2[BANK_L2_SET_IDX_WIDTH-1:0]                                      };
                end
                else if(cs!=S_L3 & ns==S_L3) begin
                    upd2bank_req_valid        <= 1'b1;
                    upd2bank_req.req          <= upd_req_i;
                    upd2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_L3_IDX_WIDTH){1'b0}},         idx_l3[BANK_L3_IDX_WIDTH+BANK_L3_SET_IDX_WIDTH-1:BANK_L3_SET_IDX_WIDTH]};
                    upd2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_L3_SET_IDX_WIDTH){1'b0}}, idx_l3[BANK_L3_SET_IDX_WIDTH-1:0]                                      };
                end
                else if(cs!=S_L4 & ns==S_L4) begin
                    upd2bank_req_valid        <= 1'b1;
                    upd2bank_req.req          <= upd_req_i;
                    upd2bank_req.bank_idx     <= {{(MAX_BANK_IDX_WIDTH-BANK_L4_IDX_WIDTH){1'b0}},         idx_l4[BANK_L4_IDX_WIDTH+BANK_L4_SET_IDX_WIDTH-1:BANK_L4_SET_IDX_WIDTH]};
                    upd2bank_req.bank_set_idx <= {{(MAX_BANK_SET_IDX_WIDTH-BANK_L4_SET_IDX_WIDTH){1'b0}}, idx_l4[BANK_L4_SET_IDX_WIDTH-1:0]                                      };
                end
            end
        end
    end
    
    assign upd2bank_req_valid_o[0] = (cs==S_L0) & upd2bank_req_valid;
    assign upd2bank_req_valid_o[1] = (cs==S_L1) & upd2bank_req_valid;
    assign upd2bank_req_valid_o[2] = (cs==S_L2) & upd2bank_req_valid;
    assign upd2bank_req_valid_o[3] = (cs==S_L3) & upd2bank_req_valid;
    assign upd2bank_req_valid_o[4] = (cs==S_L4) & upd2bank_req_valid;
    assign upd2bank_req_ready      = (cs==S_L0) ? upd2bank_req_ready_i[0] :
                                     (cs==S_L1) ? upd2bank_req_ready_i[1] :
                                     (cs==S_L2) ? upd2bank_req_ready_i[2] :
                                     (cs==S_L3) ? upd2bank_req_ready_i[3] :
                                     (cs==S_L4) ? upd2bank_req_ready_i[4] :
                                     1'b0;
    assign upd2bank_req_o[0]       = (cs==S_L0) ? upd2bank_req : 'd0;
    assign upd2bank_req_o[1]       = (cs==S_L1) ? upd2bank_req : 'd0;
    assign upd2bank_req_o[2]       = (cs==S_L2) ? upd2bank_req : 'd0;
    assign upd2bank_req_o[3]       = (cs==S_L3) ? upd2bank_req : 'd0;
    assign upd2bank_req_o[4]       = (cs==S_L4) ? upd2bank_req : 'd0;


//}}}

//=== UPD ACK {{{
    assign cur_bank2upd_ack = (cs==S_L0) ? bank2upd_ack_i[0].ack :
                              (cs==S_L1) ? bank2upd_ack_i[1].ack :
                              (cs==S_L2) ? bank2upd_ack_i[2].ack :
                              (cs==S_L3) ? bank2upd_ack_i[3].ack :
                              (cs==S_L4) ? bank2upd_ack_i[4].ack :
                                           bank2upd_ack_i[0].ack;

generate
    if(CACHE_TYPE==0) begin : s2ptc_upd_ack_o_gen
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
                        upd_ack_o.lvl               <= 'd0;
                        upd_ack_o.prefetched        <= 'd0;
                        upd_ack_o.addr              <= 'd0;
                        upd_ack_o.gscid             <= 'd0;
                        upd_ack_o.N                 <= 'd0;
                        upd_ack_o.PBMT              <= 'd0;
                        upd_ack_o.D                 <= 'd0;
                        upd_ack_o.A                 <= 'd0;
                        upd_ack_o.PPN               <= 'd0;
                        upd_ack_o.PERM              <= 'd0;
                        upd_ack_o.V                 <= 'd0;
                    end
                end
            end
        end
    end
    else begin : s1ptc_upd_ack_o_gen
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
                        upd_ack_o.lvl               <= 'd0;
                        upd_ack_o.prefetched        <= 'd0;
                        upd_ack_o.addr              <= 'd0;
                        upd_ack_o.gv                <= 'd0;
                        upd_ack_o.gscid             <= 'd0;
                        upd_ack_o.pscid             <= 'd0;
                        upd_ack_o.sxl               <= 'd0;
                        upd_ack_o.N                 <= 'd0;
                        upd_ack_o.PBMT              <= 'd0;
                        upd_ack_o.D                 <= 'd0;
                        upd_ack_o.A                 <= 'd0;
                        upd_ack_o.PPN               <= 'd0;
                        upd_ack_o.PERM              <= 'd0;
                        upd_ack_o.V                 <= 'd0;
                    end
                end
            end
        end
    end
endgenerate
//}}}

//}}}

//=== IDX inst {{{
    assign upd_in_lvl   = upd_req_i.lvl;
    assign upd_in_leaf  = upd_req_i.PERM[2] | upd_req_i.PERM[0];

generate
    if(CACHE_TYPE==0) begin : s2ptc_idx_gen
        assign idx_gv       = 1'b1;
        assign idx_gscid    = (cs==S_IDLE) ? upd_req_i.gscid    : req.gscid;
        assign idx_pscid    = 'd0;
        assign idx_addr     = (cs==S_IDLE) ? upd_req_i.addr     : req.addr;
        assign idx_lvl      = (cs==S_IDLE) ? upd_req_i.lvl      : req.lvl;
        assign idx_l0 = idx[BANK_L0_IDX_WIDTH+BANK_L0_SET_IDX_WIDTH-1:0];
        assign idx_l1 = idx[BANK_L1_IDX_WIDTH+BANK_L1_SET_IDX_WIDTH-1:0];
        assign idx_l2 = idx[BANK_L2_IDX_WIDTH+BANK_L2_SET_IDX_WIDTH-1:0];
        assign idx_l3 = idx[BANK_L3_IDX_WIDTH+BANK_L3_SET_IDX_WIDTH-1:0];
        assign idx_l4 = idx[BANK_L4_IDX_WIDTH+BANK_L4_SET_IDX_WIDTH-1:0];
    end
    else begin : s1ptc_idx_gen
        assign idx_gv       = (cs==S_IDLE) ? upd_req_i.gv       : req.gv;
        assign idx_gscid    = (cs==S_IDLE) ? upd_req_i.gscid    : req.gscid;
        assign idx_pscid    = (cs==S_IDLE) ? upd_req_i.pscid    : req.pscid;
        assign idx_addr     = (cs==S_IDLE) ? upd_req_i.addr     : req.addr;
        assign idx_lvl      = (cs==S_IDLE) ? upd_req_i.lvl      : req.lvl;
        assign idx_l0 = idx[BANK_L0_IDX_WIDTH+BANK_L0_SET_IDX_WIDTH-1:0];
        assign idx_l1 = idx[BANK_L1_IDX_WIDTH+BANK_L1_SET_IDX_WIDTH-1:0];
        assign idx_l2 = idx[BANK_L2_IDX_WIDTH+BANK_L2_SET_IDX_WIDTH-1:0];
        assign idx_l3 = idx[BANK_L3_IDX_WIDTH+BANK_L3_SET_IDX_WIDTH-1:0];
        assign idx_l4 = idx[BANK_L4_IDX_WIDTH+BANK_L4_SET_IDX_WIDTH-1:0];
    end
endgenerate

    iommu_atd_ptc_mtlb_idx #(
    /*parameter  */ .CACHE_TYPE     (CACHE_TYPE                                 ), // 0:S2PTC, 1:S1PTC
    /*parameter  */ .IDX_WIDTH      (MAX_BANK_IDX_WIDTH+MAX_BANK_SET_IDX_WIDTH  )  // = 7
    ) U_idx_gen(
    /*input  logic                          */  .gv             (idx_gv                     ),
    /*input  logic [15:0]                   */  .gscid          (idx_gscid                  ),
    /*input  logic [19:0]                   */  .pscid          (idx_pscid                  ),
    /*input  logic [63:12]                  */  .addr           (idx_addr                   ),
    /*input  logic [1:0]                    */  .lvl            (idx_lvl                    ),
    /*output logic [IDX_WIDTH-1:0]          */  .idx            (idx                        ) 
    );


//}}}

//}}}

endmodule
