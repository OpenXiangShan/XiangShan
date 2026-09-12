module iommu_acd_bus_handler_w_buf import iommu_acd_pkg::*; #(
    parameter  WFIFO_TYPE           = 1'b1, // 0:FF type, 1:RAM type
    parameter  WFIFO_DEPTH          = 32,
    parameter  DEQUEUE_FIFO_DEPTH   = 16,
    parameter  BUS_INFLY_TOKEN_WIDTH= 6,
    parameter  BUS_INFLY_TOKEN_NUM  = 2**BUS_INFLY_TOKEN_WIDTH,
    parameter  BUS_ID_WIDTH         = 8,
    parameter  BUS_ADDR_WIDTH       = 64,
    parameter  BUS_USER_WIDTH       = 8,
    parameter  BUS_DATA_WIDTH       = 128,
    parameter  BUS_SIZE_WIDTH       = 3,
    parameter  BUS_STRB_WIDTH       = BUS_DATA_WIDTH/8,
    parameter type         BUS_CH_AX_TYPE       = iommu_acd_pkg::ch_ax_t,
    parameter type         BUS_CH_W_TYPE        = iommu_acd_pkg::ch_w_t,
    parameter type         BUS_CH_B_TYPE        = iommu_acd_pkg::ch_b_t,
    parameter  SPARE_PARAM          = 0
)(
//{{{ IO
    input  logic                                clk             ,
    input  logic                                rstn            ,
    // AW Intf
    input  logic                                s2q_valid_i     ,
    input  logic                                s2q_ready_i     ,
    input  logic                                s2q_wr_i        ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]    s2q_token_i     ,
    input  logic [ 1:0]                         s2q_bar_i       ,
    // W Intf
    input  logic                                s2w_wvalid_i    ,
    output logic                                s2w_wready_o    ,
    input  BUS_CH_W_TYPE                        s2w_wpayld_i    ,
    // TRANS-QUEUE
    output logic [BUS_INFLY_TOKEN_NUM-1:0]      wdata_ready_o   ,
    input  logic                                qout_valid_i    ,
    output logic                                qout_ready_o    ,
    input  logic                                qout_wr_i       ,
    input  BUS_CH_AX_TYPE                       qout_axpayld_i  ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]    qout_token_i    ,
    input  logic                                qout_fault_i    ,
    input  logic                                qout_mrif_i     ,
    // MST Intf
    output logic                                w2m_awvalid_o   ,
    input  logic                                w2m_awready_i   ,
    output BUS_CH_AX_TYPE                       w2m_awpayld_o   ,
    output logic                                w2m_wvalid_o    ,
    input  logic                                w2m_wready_i    ,
    output BUS_CH_W_TYPE                        w2m_wpayld_o    ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]    w2m_token_o     ,
    output logic                                w2m_fault_o     ,
    output logic                                w2m_mrif_o      ,
    //
    output logic                                wbuf_dequeue_fifo_empty_o,
    //
    input  logic                                spare_in             
//}}}
);
//=== Declare === {{{
    typedef struct packed {
        logic [BUS_INFLY_TOKEN_WIDTH-1:0]       token   ;
    } enqueue_data_t;
    typedef struct packed {
        logic                                   mrif    ;
        logic                                   fault   ;
        BUS_CH_AX_TYPE                          axpayld ;
        logic [BUS_INFLY_TOKEN_WIDTH-1:0]       token   ;
    } dequeue_data_t;
    localparam ENQUEUE_FIFO_DATA_WIDTH          = $bits(enqueue_data_t);//BUS_INFLY_TOKEN_WIDTH;
    localparam DEQUEUE_FIFO_DATA_WIDTH          = $bits(dequeue_data_t);//BUS_INFLY_TOKEN_WIDTH + 1 + BUS_ADDR_WIDTH + BUS_ID_WIDTH + 8 + BUS_SIZE_WIDTH + 2 + 1 + 4 + 3 + 4 + BUS_USER_WIDTH + 4;
    localparam WFIFO_DATA_WIDTH                 = $bits(BUS_CH_W_TYPE);//BUS_DATA_WIDTH + BUS_STRB_WIDTH + 1 + BUS_USER_WIDTH;

    enqueue_data_t                              enqueue_token_fifo_wdata, enqueue_token_fifo_rdata;
    logic                                       enqueue_token_fifo_empty, enqueue_token_fifo_full;
    logic                                       enqueue_token_fifo_push,  enqueue_token_fifo_pop;

    dequeue_data_t                              dequeue_token_fifo_wdata, dequeue_token_fifo_rdata;
    logic                                       dequeue_token_fifo_full,  dequeue_token_fifo_empty;
    logic                                       dequeue_token_fifo_push,  dequeue_token_fifo_pop;
    
    BUS_CH_W_TYPE                               wfifo_wdata, wfifo_rdata;
    logic                                       wfifo_full,  wfifo_empty;
    logic                                       wfifo_push,  wfifo_pop;
    logic                                       wfifo_rdata_last;
    logic [BUS_INFLY_TOKEN_NUM-1:0]             wfifo_data_ready;
    logic w2m_awvalid_mask;

    logic                                       new_w_got, w_trans_ongoing;
//}}}

// === Main Code === {{{
//=== wready {{{
    assign s2w_wready_o = (~enqueue_token_fifo_empty) & (~wfifo_full);                         // wdata always waiting for enqueue_token_fifo push
//}}}

//=== enqueue_token_fifo {{{
    assign enqueue_token_fifo_wdata  = s2q_token_i;
    assign enqueue_token_fifo_push   = s2q_valid_i & s2q_ready_i & s2q_wr_i & (~s2q_bar_i[0]);
    assign enqueue_token_fifo_pop    = s2w_wvalid_i & s2w_wready_o & s2w_wpayld_i.wlast;

    iommu_acd_bus_handler_sync_fifo #(
    /*parameter */ .WIDTH (ENQUEUE_FIFO_DATA_WIDTH  ), //            = 128,
    /*parameter */ .DEPTH (BUS_INFLY_TOKEN_NUM        )  //            = 32,
    ) U_enqueue_token_fifo(
    /*input  logic                            */ .clk       (clk                            ),
    /*input  logic                            */ .rstn      (rstn                           ),
    /*input  logic                            */ .push_i    (enqueue_token_fifo_push        ),
    /*output logic                            */ .full_o    (enqueue_token_fifo_full        ),
    /*output logic                            */ .afull_o   (                               ), // almost full
    /*input  logic [WIDTH-1:0]                */ .wdata_i   (enqueue_token_fifo_wdata       ),
    /*input  logic                            */ .pop_i     (enqueue_token_fifo_pop         ),
    /*output logic                            */ .empty_o   (enqueue_token_fifo_empty       ),
    /*output logic                            */ .aempty_o  (                               ),
    /*output logic [WIDTH-1:0]                */ .rdata_o   (enqueue_token_fifo_rdata       ),
    /*input  logic                            */ .spare_in  (1'b0                           ) 
    );

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            w_trans_ongoing <= 1'b0;
        else begin
            if(s2w_wvalid_i & s2w_wready_o & s2w_wpayld_i.wlast)
                w_trans_ongoing <= 1'b0;
            else if(s2w_wvalid_i & s2w_wready_o & ~w_trans_ongoing)
                w_trans_ongoing <= 1'b1;
        end
    end

    assign new_w_got = s2w_wvalid_i & s2w_wready_o & ~w_trans_ongoing;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            wdata_ready_o <= 'd0;
        else begin
            //if(enqueue_token_fifo_pop)
            if(new_w_got)
                wdata_ready_o[enqueue_token_fifo_rdata.token] <= 1'b1;
            else if(wfifo_pop & wfifo_rdata_last)
                wdata_ready_o[dequeue_token_fifo_rdata.token] <= 1'b0;
        end
    end
//}}}

//=== wfifo {{{
    assign wfifo_wdata  = s2w_wpayld_i;
    assign wfifo_push   = s2w_wvalid_i & s2w_wready_o;

generate
    if(WFIFO_TYPE==1'b0) begin : ff_type_wfifo_pop_gen
        assign wfifo_pop    = w2m_awvalid_mask                  ? (w2m_wready_i & w2m_wvalid_o) :
                              (w2m_awvalid_o & w2m_awready_i)   ? (w2m_wready_i & w2m_wvalid_o) : 1'b0;
    end
    else begin : ram_type_wfifo_pop_gen
//        assign wfifo_pop    = w2m_awvalid_mask                  ? (w2m_wready_i & w2m_wvalid_o & ~wfifo_rdata_last) :
//                              (w2m_awvalid_o & w2m_awready_i)   ? 1'b1                                              : 1'b0;
        assign wfifo_pop    = w2m_awvalid_mask                  ? (w2m_wvalid_o ? (w2m_wready_i & ~wfifo_rdata_last) : 1'b1) :
                              (w2m_awvalid_o & w2m_awready_i)   ? 1'b1                                              : 1'b0;
    end
endgenerate

    iommu_acd_bus_handler_idx_fifo #(
    /*parameter  */ .FIFOTYPE           (1'b1                       ),
    /*parameter  */ .DEPTH              (WFIFO_DEPTH                ), //= 32,
    /*parameter  */ .WIDTH              (WFIFO_DATA_WIDTH           ), //= 64,
    /*parameter  */ .IDX_WIDTH          (BUS_INFLY_TOKEN_WIDTH      ), //= 3,
    /*parameter  */ .SPARE_PARA         (1'b0                       )  //= 0
    ) U_wfifo(
    /*input  logic                              */  .clk                        (clk                            ),
    /*input  logic                              */  .rstn                       (rstn                           ),
    /*input  logic                              */  .push_i                     (wfifo_push                     ),
    /*output logic                              */  .full_o                     (wfifo_full                     ),
    /*output logic                              */  .afull_o                    (                               ),
    /*input  logic [DATA_WIDTH-1:0]             */  .wdata_i                    (wfifo_wdata                    ),
    /*input  logic [IDX_WIDTH-1:0]              */  .widx_i                     (enqueue_token_fifo_rdata.token ),
    /*input  logic                              */  .wlast_i                    (s2w_wpayld_i.wlast             ),
    /*input  logic                              */  .pop_i                      (wfifo_pop                      ),
    /*output logic                              */  .empty_o                    (wfifo_empty                    ),
    /*output logic                              */  .aempty_o                   (                               ),
    /*output logic [DATA_WIDTH-1:0]             */  .rdata_o                    (wfifo_rdata                    ),
    /*input  logic [IDX_WIDTH-1:0]              */  .ridx_i                     (dequeue_token_fifo_rdata.token ),
    /*output logic [IDX_NUM-1:0]                */  .data_ready_o               (wfifo_data_ready               ),
    /*input  logic                              */  .spare_in                   (1'b0                           ) 
    );

generate
    if(WFIFO_TYPE==1'b0) begin : ff_type_wfifo_out_gen
        assign w2m_wvalid_o     = w2m_awvalid_mask                  ? (~dequeue_token_fifo_empty) & wfifo_data_ready[dequeue_token_fifo_rdata.token] :
                                  (w2m_awvalid_o & w2m_awready_i)   ? (~dequeue_token_fifo_empty) & wfifo_data_ready[dequeue_token_fifo_rdata.token] : 1'b0;
        assign w2m_wpayld_o     = wfifo_rdata;
        
        assign wfifo_rdata_last = (wfifo_rdata.wlast == 1'b1);
    end
    else begin : ram_type_wfifo_out_gen
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                w2m_wvalid_o    <= 1'b0;
            else begin
                if(wfifo_pop & (~wfifo_empty))
                    w2m_wvalid_o<= 1'b1;
                else if(w2m_wvalid_o & ~w2m_wready_i)
                    w2m_wvalid_o<= 1'b1;
                else
                    w2m_wvalid_o<= 1'b0;
            end
        end
        assign w2m_wpayld_o     = wfifo_rdata;
        
        assign wfifo_rdata_last = (wfifo_rdata.wlast == 1'b1);
    end
endgenerate
//}}}

//=== dequeue_token_fifo {{{
    assign dequeue_token_fifo_wdata.mrif    = qout_mrif_i   ;
    assign dequeue_token_fifo_wdata.fault   = qout_fault_i  ;
    assign dequeue_token_fifo_wdata.axpayld = qout_axpayld_i;
    assign dequeue_token_fifo_wdata.token   = qout_token_i  ;
    assign dequeue_token_fifo_push          = qout_valid_i & qout_wr_i;
generate
    if(WFIFO_TYPE==1'b0) begin : ff_type_dequeue_fifo_pop_gen
        assign dequeue_token_fifo_pop   = wfifo_pop & wfifo_rdata_last;
    end
    else begin : ram_type_dequeue_fifo_pop_gen
        assign dequeue_token_fifo_pop   = w2m_wvalid_o & w2m_wready_i & wfifo_rdata_last;
    end
endgenerate
    iommu_acd_bus_handler_sync_fifo #(
    /*parameter */  .WIDTH  (DEQUEUE_FIFO_DATA_WIDTH    ), //            = 128,
//   /*parameter */  .DEPTH  (BUS_INFLY_TOKEN_NUM        )  //            = 32,
    /*parameter */  .DEPTH  (DEQUEUE_FIFO_DEPTH         )  //            = 32,
    ) U_dequeue_token_fifo(
    /*input  logic                   */ .clk        (clk                        ),
    /*input  logic                   */ .rstn       (rstn                       ),
    /*input  logic                   */ .push_i     (dequeue_token_fifo_push    ),
    /*output logic                   */ .full_o     (dequeue_token_fifo_full    ),
    /*output logic                   */ .afull_o    (                           ), // almost full
    /*input  logic [WIDTH-1:0]       */ .wdata_i    (dequeue_token_fifo_wdata   ),
    /*input  logic                   */ .pop_i      (dequeue_token_fifo_pop     ),
    /*output logic                   */ .empty_o    (dequeue_token_fifo_empty   ),
    /*output logic                   */ .aempty_o   (                           ),
    /*output logic [WIDTH-1:0]       */ .rdata_o    (dequeue_token_fifo_rdata   ),
    /*input  logic                   */ .spare_in   (1'b0                       ) 
    );
    
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            w2m_awvalid_mask <= 1'b0;
        else begin
            if(dequeue_token_fifo_pop)
                w2m_awvalid_mask <= 1'b0;
            else if(w2m_awvalid_o & w2m_awready_i)
                w2m_awvalid_mask <= 1'b1;
        end
    end
    assign w2m_awvalid_o    = (~w2m_awvalid_mask) & (~dequeue_token_fifo_empty) & wfifo_data_ready[dequeue_token_fifo_rdata.token];
    assign w2m_awpayld_o    = dequeue_token_fifo_rdata.axpayld;

    assign w2m_token_o      = dequeue_token_fifo_rdata.token ;

    assign qout_ready_o     = ~dequeue_token_fifo_full;

    assign w2m_fault_o      = dequeue_token_fifo_rdata.fault ;
    assign w2m_mrif_o       = dequeue_token_fifo_rdata.mrif;

    assign wbuf_dequeue_fifo_empty_o = dequeue_token_fifo_empty;
//}}}

//}}}

endmodule



