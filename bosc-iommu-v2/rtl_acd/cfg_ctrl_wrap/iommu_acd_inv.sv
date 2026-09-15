/////////////////////////////////////////////////////////////////
// iommu_acd_inv
/////////////////////////////////////////////////////////////////
module iommu_acd_inv #(//{{{
    parameter   INV_IDX_WIDTH               = iommu_acd_pkg::INV_IDX_WIDTH,
    parameter   INV_INFLY_NUM               = 2**INV_IDX_WIDTH,
    parameter   INTERNAL_INV_IDX_WIDTH      = iommu_acd_pkg::INTERNAL_INV_IDX_WIDTH,
    parameter   BUS_INFLY_TOKEN_WIDTH       = iommu_acd_pkg::BUS_INFLY_TOKEN_WIDTH,
    parameter   BUS_INFLY_TOKEN_NUM         = 2**BUS_INFLY_TOKEN_WIDTH,
    parameter type          INVALID_REQ_TYPE            = iommu_acd_pkg::INVALID_REQ_TYPE,
    parameter   TLB_QIDX_WIDTH              = iommu_acd_pkg::TLB_QIDX_WIDTH,
    parameter   TLB_QUEUE_DEPTH             = 2**TLB_QIDX_WIDTH,
    parameter type          TLBQ2INV_TYPE               = iommu_acd_pkg::tlbq2inv_t,
    parameter   SPARE_PARAM                 = 0
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    // INV to cache                                     
    output logic                                        inv_tlbcache_req_valid_o,
    input  logic                                        inv_tlbcache_req_ready_i,
    output INVALID_REQ_TYPE                             inv_tlbcache_req_o,
    input  logic                                        inv_tlbcache_ack_valid_i,
    input  INVALID_REQ_TYPE                             inv_tlbcache_ack_i,
    // Info from TLBQUEUE                               
    input  TLBQ2INV_TYPE                                qinfo2_inv_i,
    // INV to ptw_ack_filter
    output logic [INV_INFLY_NUM-1:0]                    inv_ptwackfilter_req_valid_o,
    output INVALID_REQ_TYPE [INV_INFLY_NUM-1:0]         inv_ptwackfilter_req_o,
    // T2C IF                                           
    input  logic                                        msg_valid_i,
    output logic                                        msg_ready_o,
    input  logic [63:0]                                 msg_wdata_i,
    // C2T IF                                           
    output logic                                        msg_ivalid_o,
    input  logic                                        msg_iready_i,
    output iommu_acd_pkg::MSG_INV_ACK_TYPE              msg_idata_o,
    output logic                                        msg_ilast_o,
    //                                                  
    output logic                                        inv_buf_overflow_err_o,
    //
    input  logic [BUS_INFLY_TOKEN_NUM-1:0]              bh2inv_outstanding_list_i,
    input  logic [BUS_INFLY_TOKEN_NUM-1:0]              bh2inv_outstanding_rw_list_i,
    //                                                  
    input  logic                                        spare_in
//}}}                                                   
);
//=== Declare {{{                                       
    logic [INV_INFLY_NUM-1:0]                           entry_update_i                ;
    logic [INV_INFLY_NUM-1:0]                           entry_inv_req_valid_i         ;
    INVALID_REQ_TYPE                                    entry_inv_req_i               [INV_INFLY_NUM-1:0];
    logic [INV_INFLY_NUM-1:0]                           entry_inv_req_is_fence_i      ;
    logic [INV_INFLY_NUM-1:0]                           entry_msg_ivalid_o            ;
    logic [INV_INFLY_NUM-1:0]                           entry_msg_iready_i            ;
    iommu_acd_pkg::MSG_INV_ACK_TYPE [INV_INFLY_NUM-1:0] entry_msg_idata_o             ;
    logic [INV_INFLY_NUM-1:0]                           entry_inv_tlbcache_req_valid_o;
    logic [INV_INFLY_NUM-1:0]                           entry_inv_tlbcache_req_ready_i;
    INVALID_REQ_TYPE [INV_INFLY_NUM-1:0]                entry_inv_tlbcache_req_o      ;
    logic [INV_INFLY_NUM-1:0]                           entry_inv_tlbcache_ack_valid_i;
    INVALID_REQ_TYPE [INV_INFLY_NUM-1:0]                entry_inv_tlbcache_ack_i      ;
    logic [INV_INFLY_NUM-1:0]                           entry_valid_o                 ;
    logic [INV_INFLY_NUM-1:0]                           entry_valid_i                 [INV_INFLY_NUM-1:0];
    logic [INV_INFLY_NUM-1:0]                           entry_depend_bits_i           [INV_INFLY_NUM-1:0];
    logic [INV_INFLY_NUM-1:0]                           entry_depend_bit_o            ;
    logic [INV_INFLY_NUM-1:0]                           entry_is_fence_o              ;
    INVALID_REQ_TYPE [INV_INFLY_NUM-1:0]                entry_content_o               ;
    logic [BUS_INFLY_TOKEN_NUM-1:0]                     entry_bh2inv_outstanding_list_i   [INV_INFLY_NUM-1:0];
    logic [BUS_INFLY_TOKEN_NUM-1:0]                     entry_bh2inv_outstanding_rw_list_i[INV_INFLY_NUM-1:0];


    logic [1:0]                                         incnt;
    logic [INV_IDX_WIDTH:0]                             token;
    logic                                               doupdate, doupdate_ff;
    logic                                               doack;
                                                        
    INVALID_REQ_TYPE                                    inv_req;
    logic                                               inv_req_is_fence;
                                                        
    logic [6:0]                                         cmd_opcode;
    logic [2:0]                                         cmd_func3;
    logic                                               cmd_av;
    logic [19:0]                                        cmd_pscid;
    logic                                               cmd_pscv;
    logic                                               cmd_gv;
    logic [15:0]                                        cmd_gscid;
    logic [23:0]                                        cmd_did;
    logic [63:12]                                       cmd_addr;
                                                        
    logic [INV_INFLY_NUM-1:0]                           inv_tlbcache_req_priority;
                                                        
    logic                                               enqueue_fifo_push, enqueue_fifo_pop;
    logic [INV_IDX_WIDTH-1:0]                           enqueue_fifo_id_in, enqueue_fifo_id_out, enqueue_fifo_id_pop;
//}}}

//=== MainCode {{{
assign msg_ready_o = 1'b1;  // no flow_ctrl in LINK-LEVEL, cause PROTOCOL-LEVEL TOKEN can hold it

//=== incnt {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            incnt <= 'd0;
        else begin
            if(doupdate)
                incnt <= 'd0;
            else if(msg_valid_i & msg_ready_o)
                incnt <= incnt + 'd1;
        end
    end

    assign doupdate = msg_valid_i & msg_ready_o & (incnt=='d2) & (token!=0);
//}}}

//=== token {{{
    assign doack = msg_ivalid_o & msg_iready_i;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            token <= INV_INFLY_NUM;
        else begin
            case({doupdate, doack})
            2'b00: token <= token;
            2'b01: token <= token + 'd1;
            2'b10: token <= token - 'd1;
            2'b11: token <= token;
            default:token <= token;
            endcase
        end
    end

    assign inv_buf_overflow_err_o = (token=='d0) & msg_valid_i;
//}}}

//=== inv_req to entry {{{
    assign cmd_opcode = msg_wdata_i[6:0];
    assign cmd_func3  = msg_wdata_i[9:7];
    assign cmd_av     = msg_wdata_i[10];
    assign cmd_pscid  = msg_wdata_i[31:12];
    assign cmd_pscv   = msg_wdata_i[32];
    assign cmd_gv     = msg_wdata_i[33];
    assign cmd_gscid  = msg_wdata_i[59:44];
    assign cmd_did    = msg_wdata_i[63:40];
    assign cmd_addr   = msg_wdata_i[61:10];

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            inv_req         <= 'd0;
            inv_req_is_fence<= 1'b0;
        end
        else begin
            if(msg_valid_i & msg_ready_o) begin
                if(incnt=='d0) begin
                    inv_req.idx         <= {1'b0, msg_wdata_i[7:4]};
                    inv_req_is_fence    <= (msg_wdata_i[3:0]==iommu_acd_pkg::MSGCODE_FENCE_REQ) ? 1'b1 : 1'b0;
                end
                else if(incnt=='d1) begin
                    inv_req.itype       <=  (cmd_opcode==iommu_acd_pkg::INVOPCODE_IOTINVAL) ? ((cmd_func3==iommu_acd_pkg::INVFUNC3_VMA)       ? iommu_acd_pkg::INVTYPE_VMA         : iommu_acd_pkg::INVTYPE_GVMA) :
                                            (cmd_opcode==iommu_acd_pkg::INVOPCODE_IODIR   ) ? ((cmd_func3==iommu_acd_pkg::INVFUNC3_INVAL_DDT) ? iommu_acd_pkg::INVTYPE_INVALID_DDT : iommu_acd_pkg::INVTYPE_INVALID_PDT) :
                                            'b00;
                    inv_req.av          <=  cmd_av;
                    inv_req.pid_pscid   <=  cmd_pscid;
                    inv_req.pscv        <=  cmd_pscv;
                    inv_req.dv_gv       <=  cmd_gv;
                    inv_req.did_gscid   <=  (cmd_opcode==iommu_acd_pkg::INVOPCODE_IOTINVAL) ? {8'b0, cmd_gscid} :
                                            (cmd_opcode==iommu_acd_pkg::INVOPCODE_IODIR   ) ? cmd_did : 24'd0;

//                    inv_req_is_fence    <= cmd_opcode==iommu_acd_pkg::INVOPCODE_IOFENCE;
                end
                else if(incnt=='d2) begin
                    inv_req.addr        <= cmd_addr;
                end
            end
        end
    end
//}}}

//=== Update {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            doupdate_ff <= 1'b0;
        else
            doupdate_ff <= doupdate;
    end
//}}}

//=== inv_req to tlb_cache arb {{{
    iommu_acd_bus_handler_trans_arb #(
    /*parameter  */ .ARB_TYPE           (0                  ), // = 0,
    /*parameter  */ .REQ_NUM            (INV_INFLY_NUM      ), // = 2,
    /*parameter type         */ .DATA_TYPE          (INVALID_REQ_TYPE   ), //         = logic,
    /*parameter  */ .AXIVLDRDY          (1                  )  // = 1
    ) U_inv_tlbcache_req_arb(
    /*input  logic                               */ .clk                (clk                            ),
    /*input  logic                               */ .rstn               (rstn                           ),
    /*input  logic [REQ_NUM-1:0]                 */ .req_i              (entry_inv_tlbcache_req_valid_o ),
    /*input  logic [REQ_NUM-1:0]                 */ .req_prior_i        (inv_tlbcache_req_priority      ),
    /*input  DATA_TYPE [REQ_NUM-1:0]             */ .data_i             (entry_inv_tlbcache_req_o       ),
    /*output logic [REQ_NUM-1:0]                 */ .gnt_o              (entry_inv_tlbcache_req_ready_i ),
    /*output logic                               */ .req_o              (inv_tlbcache_req_valid_o       ),
    /*output DATA_TYPE                           */ .data_o             (inv_tlbcache_req_o             ),
    /*input  logic                               */ .gnt_i              (inv_tlbcache_req_ready_i       ) 
    );
//}}}

//=== update entry idx fifo {{{
        assign enqueue_fifo_push    = doupdate_ff;

        always@(*) begin
            enqueue_fifo_id_in = 'd0;
            for(int unsigned ss=0; ss<INV_INFLY_NUM; ss++) begin
                if(entry_update_i[ss]==1'b1)
                    enqueue_fifo_id_in = INV_IDX_WIDTH'(ss);
            end
        end

        assign enqueue_fifo_pop = inv_tlbcache_req_valid_o & inv_tlbcache_req_ready_i;
        
        always@(*) begin
            enqueue_fifo_id_pop = 'd0;
            for(int unsigned qq=0; qq<INV_INFLY_NUM; qq++) begin
                if(enqueue_fifo_pop & entry_inv_tlbcache_req_ready_i[qq])
                    enqueue_fifo_id_pop = INV_IDX_WIDTH'(qq);
            end
        end

        iommu_acd_bus_handler_id_queue #(
        /*parameter  */ .ID_WIDTH           (INV_IDX_WIDTH          ),
        /*parameter  */ .IDX_WIDTH          (INV_IDX_WIDTH          ), //= 3,
        /*parameter  */ .DATA_WIDTH         (1 ), //= 8,
        /*parameter  */ .SPARE_PARA         (0)  //= 0
        ) U_enqueue_idx_fifo(
        /*input  logic                            */    .clk     (clk                   ),
        /*input  logic                            */    .rstn    (rstn                  ),
        /*input  logic                            */    .push    (enqueue_fifo_push     ),
        /*input  logic [IDX_WIDTH-1:0]            */    .wid     (enqueue_fifo_id_in    ),
        /*input  logic [DATA_WIDTH-1:0]           */    .wdata   (1'b1                  ),
        /*output logic                            */    .wfull   (                      ),
        /*input  logic                            */    .pop     (enqueue_fifo_pop      ),
        /*input  logic [IDX_WIDTH-1:0]            */    .rid     (enqueue_fifo_id_pop   ),
        /*output logic [DATA_WIDTH-1:0]           */    .rdata   (                      ),
        /*output logic                            */    .rvalid  (                      ),
        /*output logic                            */    .rempty  (                      ),
        /*output logic [IDX_WIDTH-1:0]            */    .ido     (enqueue_fifo_id_out   ),
        /*output logic [DATA_WIDTH-1:0]           */    .datao   (                      ),
        /*input  logic                            */    .spare_in(1'b0                  ) 
    );

    assign inv_tlbcache_req_priority = {{(INV_INFLY_NUM-1){1'b0}}, 1'b1} << enqueue_fifo_id_out;
//}}}

//=== msg out arb {{{
    iommu_acd_bus_handler_trans_arb #(
    /*parameter  */ .ARB_TYPE           (0                  ), // = 0,
    /*parameter  */ .REQ_NUM            (INV_INFLY_NUM      ), // = 2,
    /*parameter type         */ .DATA_TYPE          (iommu_acd_pkg::MSG_INV_ACK_TYPE   ), //         = logic,
    /*parameter  */ .AXIVLDRDY          (1                  )  // = 1
    ) U_msg_out_arb(
    /*input  logic                               */ .clk                (clk                ),
    /*input  logic                               */ .rstn               (rstn               ),
    /*input  logic [REQ_NUM-1:0]                 */ .req_i              (entry_msg_ivalid_o ),
    /*input  logic [REQ_NUM-1:0]                 */ .req_prior_i        (INV_INFLY_NUM'(0)  ),
    /*input  DATA_TYPE [REQ_NUM-1:0]             */ .data_i             (entry_msg_idata_o  ),
    /*output logic [REQ_NUM-1:0]                 */ .gnt_o              (entry_msg_iready_i ),
    /*output logic                               */ .req_o              (msg_ivalid_o       ),
    /*output DATA_TYPE                           */ .data_o             (msg_idata_o        ),
    /*input  logic                               */ .gnt_i              (msg_iready_i       ) 
    );
    
    assign msg_ilast_o = msg_ivalid_o;
//}}}

//=== inv req to ptw_ack_filter {{{
genvar paf;
generate
    for(paf=0; paf<INV_INFLY_NUM; paf++) begin : paf_req_gen
        assign inv_ptwackfilter_req_valid_o[paf] = entry_valid_o[paf] & ~entry_is_fence_o[paf];
        assign inv_ptwackfilter_req_o      [paf] = entry_content_o[paf];
    end
endgenerate

//}}}

//}}}



//=== entry inst{{{
genvar i;
generate
    for(i=0; i<INV_INFLY_NUM; i++) begin : entry_gen
        assign entry_inv_req_valid_i[i]     = doupdate_ff;
        assign entry_inv_req_i[i]           = inv_req;
        assign entry_inv_req_is_fence_i[i]  = inv_req_is_fence;

        always@(*) begin                                            // dependency bit connection
            for (int unsigned j=0; j<INV_INFLY_NUM; j++) begin
                entry_depend_bits_i[i][j] = (i==j) ? 1'b0 : entry_depend_bit_o[j];
                entry_valid_i[i][j]       = (i==j) ? 1'b0 : entry_valid_o[j];
            end
        end

        assign entry_inv_tlbcache_ack_valid_i[i] = inv_tlbcache_ack_valid_i & (inv_tlbcache_ack_i.idx==INV_IDX_WIDTH'(i));
        assign entry_inv_tlbcache_ack_i[i]       = inv_tlbcache_ack_i;

        iommu_acd_bus_handler_queue_entry_update #(.IDX_WIDTH(INV_IDX_WIDTH)) U_entry_valid_check(
            .valid_i    (entry_valid_o              ),
            .update_i   (doupdate_ff                ),
            .tag_i      (INV_IDX_WIDTH'(i)          ),
            .update_o   (entry_update_i[i]          )
        );

        assign entry_bh2inv_outstanding_list_i[i]   = bh2inv_outstanding_list_i;
        assign entry_bh2inv_outstanding_rw_list_i[i]= bh2inv_outstanding_rw_list_i;

        iommu_acd_inv_entry #(
        /*parameter  */ .INV_IDX_WIDTH              (INV_IDX_WIDTH              ), // = iommu_acd_pkg::INV_IDX_WIDTH,
        /*parameter  */ .INTERNAL_INV_IDX_WIDTH     (INTERNAL_INV_IDX_WIDTH     ), // = iommu_acd_pkg::INTERNAL_INV_IDX_WIDTH,
        /*parameter  */ .BUS_INFLY_TOKEN_WIDTH      (BUS_INFLY_TOKEN_WIDTH      ), // = 6,
        /*parameter type         */ .INVALID_REQ_TYPE           (INVALID_REQ_TYPE           ), // = iommu_acd_pkg::INVALID_REQ_TYPE,
        /*parameter  */ .TLB_QIDX_WIDTH             (TLB_QIDX_WIDTH             ), // = iommu_acd_pkg::TLB_QIDX_WIDTH,
        /*parameter type         */ .TLBQ2INV_TYPE              (TLBQ2INV_TYPE              ), // = iommu_acd_pkg::tlbq2inv_t,
        /*parameter  */ .SPARE_PARAM                (0                          )  // = 0
        ) U_inv_entry(
        /*input  logic                                      */  .clk                        (clk                                ),
        /*input  logic                                      */  .rstn                       (rstn                               ),
        /*input  logic [INV_IDX_WIDTH-1:0]                  */  .idx                        (INV_IDX_WIDTH'(i)                  ),
        /*input  logic                                      */  .update_i                   (entry_update_i                  [i]),
        /*input  logic                                      */  .inv_req_valid_i            (entry_inv_req_valid_i           [i]),
        /*input  INVALID_REQ_TYPE                           */  .inv_req_i                  (entry_inv_req_i                 [i]),
        /*input  logic                                      */  .inv_req_is_fence_i         (entry_inv_req_is_fence_i        [i]),
        /*output logic                                      */  .msg_ivalid_o               (entry_msg_ivalid_o              [i]),
        /*input  logic                                      */  .msg_iready_i               (entry_msg_iready_i              [i]),
        /*output MSG_INV_ACK_TYPE                           */  .msg_idata_o                (entry_msg_idata_o               [i]),
        /*output logic                                      */  .inv_tlbcache_req_valid_o   (entry_inv_tlbcache_req_valid_o  [i]),
        /*input  logic                                      */  .inv_tlbcache_req_ready_i   (entry_inv_tlbcache_req_ready_i  [i]),
        /*output INVALID_REQ_TYPE                           */  .inv_tlbcache_req_o         (entry_inv_tlbcache_req_o        [i]),
        /*input  logic                                      */  .inv_tlbcache_ack_valid_i   (entry_inv_tlbcache_ack_valid_i  [i]),
        /*input  INVALID_REQ_TYPE                           */  .inv_tlbcache_ack_i         (entry_inv_tlbcache_ack_i        [i]),
        /*input  TLBQ2INV_TYPE                              */  .qinfo2_inv_i               (qinfo2_inv_i                       ),
        /*output logic                                      */  .valid_o                    (entry_valid_o                   [i]),
        /*input  logic [INV_INFLY_NUM-1:0]                  */  .valid_i                    (entry_valid_i                   [i]),
        /*output logic                                      */  .is_fence_o                 (entry_is_fence_o                [i]),
        /*output INVALID_REQ_TYPE                           */  .content_o                  (entry_content_o                 [i]),
        /*input  logic [INV_INFLY_NUM-1:0]                  */  .depend_bits_i              (entry_depend_bits_i             [i]),
        /*output logic                                      */  .depend_bit_o               (entry_depend_bit_o              [i]),
        /*input  logic [BUS_INFLY_TOKEN_NUM-1:0]            */  .bh2inv_outstanding_list_i   (entry_bh2inv_outstanding_list_i   [i]),
        /*input  logic [BUS_INFLY_TOKEN_NUM-1;0]            */  .bh2inv_outstanding_rw_list_i(entry_bh2inv_outstanding_rw_list_i[i]),
        /*input  logic                                      */  .spare_in                   (1'b0                               )
        );
    end
endgenerate
//}}}


endmodule
//}}}


/////////////////////////////////////////////////////////////////
// iommu_acd_inv_entry
/////////////////////////////////////////////////////////////////
module iommu_acd_inv_entry #( //{{{
    parameter   INV_IDX_WIDTH               = iommu_acd_pkg::INV_IDX_WIDTH,  // should not bigger than 4
    parameter   INV_INFLY_NUM               = 2**INV_IDX_WIDTH,
    parameter   INTERNAL_INV_IDX_WIDTH      = iommu_acd_pkg::INTERNAL_INV_IDX_WIDTH,
    parameter   BUS_INFLY_TOKEN_WIDTH       = iommu_acd_pkg::BUS_INFLY_TOKEN_WIDTH,
    parameter   BUS_INFLY_TOKEN_NUM         = 2**BUS_INFLY_TOKEN_WIDTH,
    parameter type          INVALID_REQ_TYPE            = iommu_acd_pkg::INVALID_REQ_TYPE,
    parameter   TLB_QIDX_WIDTH              = iommu_acd_pkg::TLB_QIDX_WIDTH,
    parameter   TLB_QUEUE_DEPTH             = 2**TLB_QIDX_WIDTH,
    parameter type          TLBQ2INV_TYPE               = iommu_acd_pkg::tlbq2inv_t,
    parameter   SPARE_PARAM                 = 0
)(                                                      
//{{{ IO                                                
    input  logic                                        clk,
    input  logic                                        rstn,
    input  logic [INV_IDX_WIDTH-1:0]                    idx,
    // T2C IF                                           
    input  logic                                        update_i,
    input  logic                                        inv_req_valid_i,
    input  INVALID_REQ_TYPE                             inv_req_i,
    input  logic                                        inv_req_is_fence_i,
    // C2T IF                                           
    output logic                                        msg_ivalid_o,
    input  logic                                        msg_iready_i,
    output iommu_acd_pkg::MSG_INV_ACK_TYPE              msg_idata_o,
    // TLB_CACHE                                        
    output logic                                        inv_tlbcache_req_valid_o,
    input  logic                                        inv_tlbcache_req_ready_i,
    output INVALID_REQ_TYPE                             inv_tlbcache_req_o,
    input  logic                                        inv_tlbcache_ack_valid_i,
    input  INVALID_REQ_TYPE                             inv_tlbcache_ack_i,
    // Info from TLBQUEUE                                      
    input  TLBQ2INV_TYPE                                qinfo2_inv_i,
    //                                                  
    output logic                                        valid_o,
    input  logic [INV_INFLY_NUM-1:0]                    valid_i,
    output logic                                        is_fence_o,
    output INVALID_REQ_TYPE                             content_o,
    input  logic [INV_INFLY_NUM-1:0]                    depend_bits_i,
    output logic                                        depend_bit_o,
    //
    input  logic [BUS_INFLY_TOKEN_NUM-1:0]              bh2inv_outstanding_list_i,
    input  logic [BUS_INFLY_TOKEN_NUM-1:0]              bh2inv_outstanding_rw_list_i,
    //                                                  
    input  logic                                        spare_in
//}}}
);
//=== Declare {{{
    localparam S_IDLE                                   = 3'b000;
    localparam S_WAIT_RDY                               = 3'b001;
    localparam S_WAIT_ACK                               = 3'b010;
    localparam S_ACK                                    = 3'b100;
    localparam S_WAIT_FENCE                             = 3'b101;
    localparam S_WAIT_TLBQ                              = 3'b111;
                                                        
    logic [2:0]                                         cs, ns;
                                                        
    INVALID_REQ_TYPE                                    inv_req_store;
    logic                                               inv_req_is_fence_store;
                                                        
    logic [INV_INFLY_NUM-1:0]                           depend_status, depend_bits;

//    logic [TLB_QUEUE_DEPTH-1:0]                         qdepend_bits, qdepend_status;
//    logic [TLB_QUEUE_DEPTH-1:0]                         qdepend_wr_bits;
//    logic                                               any_ongoing_trans_in_tlbq_i;

    logic [BUS_INFLY_TOKEN_NUM-1:0]                     tdepend_status, tdepend_bits;
//}}}

//=== MainCode {{{
//    assign any_ongoing_trans_in_tlbq_i = |qinfo2_inv_i.valid;


//=== FSM Stage1 {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cs <= S_IDLE;
        else
            cs <= ns;
    end
//}}}

//=== FSM Stage2 {{{
    always@(*) begin
        ns = cs;
        case(cs)
        S_IDLE      : begin
            if(update_i) begin
//                if(~any_ongoing_trans_in_tlbq_i) begin
                    if(inv_req_is_fence_i)
                        ns = S_WAIT_FENCE;
                    else
                        ns = S_WAIT_RDY;
//                end
//                else begin
//                    ns = S_WAIT_TLBQ;
//                end
            end
            else begin
                ns = S_IDLE;
            end
        end
//        S_WAIT_TLBQ : begin
//            if(qdepend_status == 'd0) begin
//                if(inv_req_is_fence_store)
//                    ns = S_WAIT_FENCE;
//                else
//                    ns = S_WAIT_RDY;
//            end
//            else begin
//                ns = S_WAIT_TLBQ;
//            end
//        end
        S_WAIT_RDY  : begin
            if(inv_tlbcache_req_ready_i)
                ns = S_WAIT_ACK;
            else
                ns = S_WAIT_RDY;
        end
        S_WAIT_ACK  : begin
            if(inv_tlbcache_ack_valid_i)
                ns = S_ACK;
            else
                ns = S_WAIT_ACK;
        end
        S_ACK       : begin
            if(msg_iready_i)
                ns = S_IDLE;
            else
                ns = S_ACK;
        end
        S_WAIT_FENCE: begin
            if((depend_status == 'd0) & (tdepend_status == 'd0))
                ns = S_ACK;
            else
                ns = S_WAIT_FENCE;
        end
        default: ns = S_IDLE;
        endcase
    end
//}}}

//=== FSM Stage3 {{{
//=== valid_o ===
    assign valid_o    = (cs != S_IDLE);
    assign is_fence_o = inv_req_is_fence_store;
    assign content_o  = inv_req_store;

//=== depend_bits {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            depend_bits <= 'b0;
        else if((ns != S_IDLE) && (cs == S_IDLE) && inv_req_is_fence_i)
            depend_bits <= depend_bits_i;
        else begin
            for(int unsigned kk=0; kk<INV_INFLY_NUM; kk++) begin
                if(depend_bits[kk] & (~valid_i[kk]))
                    depend_bits[kk] <= 1'b0;
            end
        end
    end
    //assign depend_status = depend_bits & valid_i;
    assign depend_status = depend_bits;

// remove qdepend logic, IOFENCE.C.PR/PW should monitor transaction status but not TLB_queue
//    always@(posedge clk or negedge rstn) begin
//        if(~rstn) begin
//            qdepend_bits    <= 'd0;
//            qdepend_wr_bits <= 'd0;
//        end
//        else begin
//            if((ns == S_WAIT_TLBQ) & (cs == S_IDLE)) begin
//                qdepend_bits    <= qinfo2_inv_i.valid;
//                qdepend_wr_bits <= qinfo2_inv_i.wr;
//            end
//            else begin
//                for(int unsigned q2v=0; q2v<TLB_QUEUE_DEPTH; q2v++) begin
//                    if(qinfo2_inv_i.valid[q2v]==1'b0)
//                        qdepend_bits[q2v] <= 1'b0;
//                end
//            end
//        end
//    end
//    assign qdepend_status = qdepend_bits;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            tdepend_bits <= 'b0;
        else if((ns != S_IDLE) && (cs == S_IDLE) && inv_req_is_fence_i) begin
            if(inv_req_i.pid_pscid[1:0]==2'b11)             // {PW,PR}=={1,1}
                tdepend_bits <= bh2inv_outstanding_list_i;
            else if(inv_req_i.pid_pscid[1:0]==2'b10)        // only PW
                tdepend_bits <= bh2inv_outstanding_list_i & bh2inv_outstanding_rw_list_i;
            else if(inv_req_i.pid_pscid[1:0]==2'b01)        // only PR
                tdepend_bits <= bh2inv_outstanding_list_i & (~bh2inv_outstanding_rw_list_i);
        end
        else begin
            for(int unsigned kk=0; kk<BUS_INFLY_TOKEN_NUM; kk++) begin
                if(tdepend_bits[kk] & (~bh2inv_outstanding_list_i[kk]))
                    tdepend_bits[kk] <= 1'b0;
            end
        end
    end
    //assign depend_status = depend_bits & valid_i;
    assign tdepend_status = tdepend_bits;


//=== depend_bit_o
    assign depend_bit_o = inv_req_valid_i & valid_o & inv_req_is_fence_i;
//}}}

//=== REQ store
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            inv_req_store           <= 'd0;
            inv_req_is_fence_store  <= 'd0;
        end
        else if(cs==S_IDLE & ns!=S_IDLE) begin
            inv_req_store           <= inv_req_i;
            inv_req_is_fence_store  <= inv_req_is_fence_i;
        end
    end

//=== inv_req output
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            inv_tlbcache_req_valid_o <= 1'b0;
            inv_tlbcache_req_o       <= 'd0;
        end
        else begin
            if(cs!=S_WAIT_RDY & ns==S_WAIT_RDY) begin
                inv_tlbcache_req_valid_o    <= 1'b1;
                inv_tlbcache_req_o.idx      <= {1'b0, INTERNAL_INV_IDX_WIDTH'(idx)};
                inv_tlbcache_req_o.itype    <= inv_req_i.itype    ;
                inv_tlbcache_req_o.dv_gv    <= inv_req_i.dv_gv    ;
                inv_tlbcache_req_o.did_gscid<= inv_req_i.did_gscid;
                inv_tlbcache_req_o.pscv     <= inv_req_i.pscv     ;
                inv_tlbcache_req_o.pid_pscid<= inv_req_i.pid_pscid;
                inv_tlbcache_req_o.av       <= inv_req_i.av       ;
                inv_tlbcache_req_o.addr     <= inv_req_i.addr     ;
            end
            else
                inv_tlbcache_req_valid_o    <= 1'b0;
        end
    end

//=== ACK output
    assign msg_ivalid_o = (cs==S_ACK);
    assign msg_idata_o  = inv_req_is_fence_store ?  {56'd0, inv_req_store.idx[3:0], iommu_acd_pkg::MSGCODE_FENCE_ACK} :
                                                    {56'd0, inv_req_store.idx[3:0], iommu_acd_pkg::MSGCODE_INV_ACK};
//}}}

//}}}

endmodule
//}}}




