module iommu_acd_bus_handler_r_buf #(
    parameter   BUS_INFLY_TOKEN_WIDTH   = 6,
    parameter   BUS_INFLY_TOKEN_NUM     = 2**BUS_INFLY_TOKEN_WIDTH,
    parameter   BUS_ID_WIDTH            = 8,
    parameter   BUS_ADDR_WIDTH          = 64,
    parameter   BUS_USER_WIDTH          = 8,
    parameter   BUS_DATA_WIDTH          = 128,
    parameter   BUS_SIZE_WIDTH          = 3,
    parameter   BUS_STRB_WIDTH          = BUS_DATA_WIDTH/8,
    parameter type          BUS_CH_AX_TYPE          = iommu_acd_pkg::ch_ax_t,
    parameter type          BUS_CH_R_TYPE           = iommu_acd_pkg::ch_r_t,
    parameter   SPARE_PARAM             = 0
)(
//{{{ IO
    input  logic                                    clk                 ,
    input  logic                                    rstn                ,
    // SLV Intf
    input  logic                                    s2q_valid_i         ,
    input  logic                                    s2q_ready_i         ,
    input  logic                                    s2q_wr_i            ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        s2q_token_i         ,

    output logic                                    r2s_rvalid_o        ,
    input  logic                                    r2s_rready_i        ,
    output BUS_CH_R_TYPE                            r2s_rpayld_o        ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        r2s_token_o         ,
    // TRANS-QUEUE
    input  logic                                    qout_valid_i        ,
    output logic                                    qout_ready_o        ,
    input  logic                                    qout_wr_i           ,
    input  BUS_CH_AX_TYPE                           qout_axpayld_i      ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        qout_token_i        ,
    input  logic                                    qout_fault_i        ,
    input  logic                                    qout_mrif_i         ,
    // MST Intf
    output logic                                    r2m_arvalid_o       ,
    input  logic                                    r2m_arready_i       ,
    output BUS_CH_AX_TYPE                           r2m_arpayld_o       ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        r2m_token_o         ,
    output logic                                    r2m_fault_o         ,
    output logic                                    r2m_mrif_o          ,
    input  logic                                    m2r_rvalid_i        ,
    output logic                                    m2r_rready_o        ,
    input  BUS_CH_R_TYPE                            m2r_rpayld_i        ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        m2r_token_i         ,
    
    input  logic                                    spare_in             
//}}}
);
//=== Declare === {{{
    typedef struct packed {
        logic [BUS_INFLY_TOKEN_WIDTH-1:0]   token;
    } enqueue_data_t;
    typedef struct packed {
        logic [BUS_ID_WIDTH-1:0]            rid  ;
        logic [BUS_DATA_WIDTH-1:0]          rdata;
        logic [ 1:0]                        rresp;
        logic                               rlast;
        logic [BUS_USER_WIDTH-1:0]          ruser;
    } rfifo_data_t;
    localparam ENQUEUE_FIFO_DATA_WIDTH      = $bits(enqueue_data_t);//BUS_INFLY_TOKEN_WIDTH;
    localparam RFIFO_DATA_WIDTH             = $bits(rfifo_data_t);

    enqueue_data_t                          enqueue_token_fifo_wdata,   enqueue_token_fifo_rdata;
    logic                                   enqueue_token_fifo_full,    enqueue_token_fifo_empty;
    logic                                   enqueue_token_fifo_push,    enqueue_token_fifo_pop;

    rfifo_data_t                            rfifo_wdata,    rfifo_rdata;
    logic                                   rfifo_full,     rfifo_empty;
    logic                                   rfifo_push,     rfifo_pop;
    logic                                   rfifo_rdata_last;
    logic [BUS_INFLY_TOKEN_NUM-1:0]         rfifo_data_ready;
//}}}

//gpf  
//gpf //=== rfifo === {{{
//gpf     assign rfifo_wdata  = {m2r_rid_i, m2r_rdata_i, m2r_rresp_i, m2r_rlast_i, m2r_ruser_i};
//gpf     assign rfifo_push   = m2r_rvalid_i & m2r_rready_o;
//gpf     assign rfifo_pop    = r2s_rvalid_o & r2s_rready_i;
//gpf 
//gpf     iommu_acd_bus_handler_idx_fifo #(
//gpf     /*parameter  */ .DEPTH            (32), //= 32,
//gpf     /*parameter  */ .WIDTH            (RFIFO_DATA_WIDTH), //= 64,
//gpf     /*parameter  */ .IDX_WIDTH        (BUS_INFLY_TOKEN_WIDTH), //= 3,
//gpf     /*parameter  */ .SPARE_PARA       (1'b0)  //= 0
//gpf     ) U_rfifo(
//gpf     /*input  logic                           */ .clk             (clk),
//gpf     /*input  logic                           */ .rstn            (rstn),
//gpf     /*input  logic                           */ .push_i          (rfifo_push),
//gpf     /*output logic                           */ .full_o          (rfifo_full),
//gpf     /*output logic                           */ .afull_o         (),
//gpf     /*input  logic [DATA_WIDTH-1:0]          */ .wdata_i         (rfifo_wdata),
//gpf     /*input  logic [IDX_WIDTH-1:0]           */ .widx_i          (m2r_token_i),
//gpf     /*input  logic                           */ .pop_i           (),
//gpf     /*output logic                           */ .empty_o         (),
//gpf     /*output logic                           */ .aempty_o        (),
//gpf     /*output logic [DATA_WIDTH-1:0]          */ .rdata_o         (rfifo_rdata),
//gpf     /*input  logic [IDX_WIDTH-1:0]           */ .ridx_i          (enqueue_token_fifo_rdata.token),
//gpf     /*output logic [IDX_NUM-1:0]             */ .data_ready_o    (rfifo_data_ready),
//gpf     /*input  logic                           */ .spare_in        (1'b0) 
//gpf     );
//gpf     assign m2r_rready_o = rfifo_full;
//gpf     assign r2s_rid_o        = rfifo_rdata.rid;
//gpf     assign r2s_rdata_o      = rfifo_rdata.rdata;
//gpf     assign r2s_rresp_o      = rfifo_rdata.rresp;
//gpf     assign r2s_rlast_o      = rfifo_rdata.rlast;
//gpf     assign r2s_ruser_o      = rfifo_rdata.ruser;
//gpf     
//gpf     assign rfifo_rdata_last = (rfifo_rdata.rlast == 1'b1);
//gpf //}}}
//gpf 
//gpf //=== enqueue_token_fifo === {{{
//gpf     assign enqueue_token_fifo_wdata = s2q_token_i;
//gpf     assign enqueue_token_fifo_push  = s2q_valid_i & s2q_ready_i & (~s2q_wr_i);
//gpf     assign enqueue_token_fifo_pop   = rfifo_pop & rfifo_rdata_last;
//gpf 
//gpf     iommu_acd_bus_handler_sync_fifo #(
//gpf     /*parameter */ .WIDTH (ENQUEUE_FIFO_DATA_WIDTH      ), //            = 128,
//gpf     /*parameter */ .DEPTH (BUS_INFLY_TOKEN_NUM          )  //            = 32,
//gpf     ) U_enqueue_token_fifo(
//gpf     /*input  logic                            */ .clk       (clk                            ),
//gpf     /*input  logic                            */ .rstn      (rstn                           ),
//gpf     /*input  logic                            */ .push_i    (enqueue_token_fifo_push         ),
//gpf     /*output logic                            */ .full_o    (enqueue_token_fifo_full         ),
//gpf     /*output logic                            */ .afull_o   (), // almost full
//gpf     /*input  logic [WIDTH-1:0]                */ .wdata_i   (enqueue_token_fifo_wdata        ),
//gpf     /*input  logic                            */ .pop_i     (enqueue_token_fifo_pop          ),
//gpf     /*output logic                            */ .empty_o   (enqueue_token_fifo_empty        ),
//gpf     /*output logic                            */ .aempty_o  (),
//gpf     /*output logic [WIDTH-1:0]                */ .rdata_o   (enqueue_token_fifo_rdata        ),
//gpf     /*input  logic                            */ .spare_in  (1'b0                           ) 
//gpf     );
//gpf     
//gpf     logic r2s_rvalid_mask;
//gpf     always@(posedge clk or negedge rstn) begin
//gpf         if(~rstn)
//gpf             r2s_rvalid_mask <= 1'b0;
//gpf         else begin
//gpf             if(enqueue_token_fifo_pop)
//gpf                 r2s_rvalid_mask <= 1'b0;
//gpf             else if(r2s_rvalid_o & r2s_rready_i)
//gpf                 r2s_rvalid_mask <= 1'b1;
//gpf         end
//gpf     end
//gpf     assign r2s_rvalid_o    = (~r2s_rvalid_mask) & (~enqueue_token_fifo_empty) & rfifo_data_ready[enqueue_token_fifo_rdata.token];
//gpf //}}}
//gpf 

//=== AW bypass ==== {{{
    assign r2m_arvalid_o= qout_valid_i & (~qout_wr_i);
    assign qout_ready_o = r2m_arready_i ;
    assign r2m_arpayld_o= qout_axpayld_i;
    assign r2m_token_o  = qout_token_i  ;
    assign r2m_fault_o  = qout_fault_i  ;
    assign r2m_mrif_o   = qout_mrif_i   ;
//}}}

//=== R bypass === {{{
    assign r2s_rvalid_o     = m2r_rvalid_i;
    assign m2r_rready_o     = r2s_rready_i;
    assign r2s_rpayld_o     = m2r_rpayld_i;
    assign r2s_token_o      = m2r_token_i ;
//}}}
endmodule



